;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'rx)
(defun string->base94 (str)
	(cl-loop for i across str
					 with acc = 0
					 do (setf acc (+ (* acc 94) (- i 33)))
					 finally (return acc)))
(defun base94->string (num)
	(if (= num 0) "!"
		(cl-loop while (> num 0) with acc
						 do (progn
									(push (+ 33 (% num 94)) acc)
									(setf num (/ num 94)))
						 finally (return (apply #'string acc)))))

(defvar-local ebangs--buffer-numbers (list))
(let (free-numbers (next 0))
	(defun ebangs-claim-number (num)
		(unless (numberp num)
			(error "Expected num to ebangs-delete-number, got %S." num))
		(if (>= num next)
				(prog1 num
					(cl-loop for i from next below num
									 do (push i free-numbers))
					(setf next (+ num 1)))
			(let ((elt (memq num free-numbers)))
				(if (memq num free-numbers) (setf free-numbers (delq num free-numbers))
					(error "Number \"%s\" already claimed." (base94->string num))))))
	(defun ebangs-get-next-number ()
		(let ((num (if (null free-numbers)
									 (prog1 next (cl-incf next))
								 (pop free-numbers))))
			(push num ebangs--buffer-numbers)
			num))
	(defun ebangs-delete-number (num)
		(unless (numberp num)
			(error "Expected num to ebangs-delete-number, got %S." num))
		(unless (memq num free-numbers) (push num free-numbers)))
	(defun ebangs--get-number-data ()
		(cons free-numbers next))
	(defun ebangs--set-number-data (d)
		(setf free-numbers (car d))
		(setf next (cdr d))))

(let ((type-index 0) (table-index 1) (nums-index 2))
	(defun ebangs-make-instance (type table &optional owned-numbers)
		(unless (cl-loop for i in owned-numbers
										 always (numberp i))
			(error "Owned numbers must be list of numbers got: %S." owned-numbers))
		(vector type table owned-numbers))
	(defun ebangs-get (name ebangs-inst)
		(cond ((eq name 'type) (aref ebangs-inst type-index))
					((eq name 'table) (aref ebangs-inst table-index))
					((eq name 'owned-numbers) (aref ebangs-inst nums-index))
					(t (gethash name (aref ebangs-inst table-index)))))
	(defun ebangs-set (name val ebangs-inst)
		(cond ((eq name 'type) (setf (aref ebangs-inst type-index) val))
					((eq name 'table) (setf (aref ebangs-inst table-index) val))
					((eq name 'owned-numbers) (setf (aref ebangs-inst nums-index) val))
					(t (puthash name val (aref ebangs-inst table-index)))))
	(defun ebangs-copy-inst (inst)
		(let ((new (copy-sequence inst)))
			(setf (aref inst table-index) (copy-hash-table (aref inst table-index)))
			new))
	(gv-define-setter ebangs-get (val name ebangs-inst)
		`(ebangs-set ,name ,val ,ebangs-inst)))
(defun ebangs-inst->list (inst)
	(nconc (ebangs-from i ((ty 'type) (nums 'owned-numbers)) `((type ,ty) (owned-numbers ,nums)))
				 (ebangs--ht-loop var val (ebangs-get 'table i)
					 collect (list var val))))
(defun ebangs-inst-delete (name ebangs-inst) (remhash name (ebangs-get 'table ebangs-inst)))
(defmacro ebangs-from (var decls &rest body)
	(declare (indent 2))
	(let* ((x (gensym))
				 (let-list (mapcar (lambda (decl) (list (car decl) (list 'ebangs-get (cadr decl) x)))
													 decls)))
		`(let ((,x ,var))
			 (let ,let-list ,@body))))
;; (map file-name [mod-date buffer])
(defvar ebangs--file-metadata (make-hash-table :test 'equal))
;; (map file-name (map ebangs-inst bool))
(defvar ebangs--files (make-hash-table :test 'equal))
;; (map key (unique (or ebangs-inst (map value (map ebangs-inst bool)))))
(defvar ebangs--indexers (make-hash-table))
(defun ebangs-index-on (key &optional unique test)
	(puthash key (cons unique (make-hash-table :test (or test 'eql))) ebangs--indexers))
(ebangs-index-on 'type)
(ebangs-index-on 'id t)
(defmacro ebangs--ht-loop (k v table &rest body)
	(declare (indent 3))
	(let ((ksym (gensym)) (vsym (gensym)))
		`(cl-loop for ,ksym being the hash-keys of ,table using (hash-values ,vsym)
							for ,k = ,ksym
							for ,v = ,vsym
							,@body)))
(defun ebangs--index-inst (inst)
	(ebangs--ht-loop key (unique . table) ebangs--indexers
		with duplicates = (list)
		for value = (ebangs-get key inst)
		if (and unique value)
		do (if (gethash value table)
					 (push (cons key value) duplicates)
				 (puthash value inst table))
		else if value
		do (puthash inst t
								(or (gethash value table)
										(puthash value (make-hash-table) table)))
		finally 
		(when duplicates
			(let ((to-unindex (ebangs-copy-inst inst)))
				(dolist (i duplicates)
					(remhash (car i) (ebags-get 'table inst)))
				(ebangs--unindex-inst to-unindex))
			(error "Duplicate unique indices: %S" duplicates))))
(defun ebangs--unindex-inst (inst)
	(ebangs--ht-loop key (unique . table) ebangs--indexers
		for value = (ebangs-get key inst)
		if (and unique value)
		do (remhash value table)
		else if value
		do (let ((x (gethash value table)))
				 (when x (remhash inst x)))))
(defun ebangs--ht-remove-if (f table)
	(ebangs--ht-loop key value table if (f value) do (remhash key table)))

(defvar ebangs-types (make-hash-table :test 'equal))
(defvar ebangs-bangs (list))
(defun ebangs-add-type (bang instance-fn)
	"INSTANCE-FN is called with the point at the start of a bang and with the length of it as an argument.
It returns an ebangs instance and leaves the point at the end of the bang's body. "
	(unless (member bang ebangs-bangs) (push bang ebangs-bangs))
	(puthash bang instance-fn ebangs-types))
(defun ebangs-instance (beg)
	(let ((bang (buffer-substring-no-properties beg (+ (point) 1))))
		(funcall (gethash bang ebangs-types) beg)))

(defun ebangs-read-number ()
	(cl-incf (point))
	(unless (looking-at (rx (+ space) (group (+? any)) (or eol (+ space))))
		(error "Malformed bang, expected number got: \n\"%s\"." (buffer-substring-no-properties (point) (line-end-position))))
	(setf (point) (- (match-end 1) 1))
	(string->base94 (match-string 1)))
(defun ebangs-read-sexp ()
	(cl-incf (point))
	(unless (looking-at (rx (+ space)))
		(error "Malformed bang, expected space got: \n\"%s\"." (buffer-substring-no-properties (point) (line-end-position))))
	(setf (point) (match-end 0))
	(let* ((beg (point))
				 (end (progn (forward-sexp)
										 (point))))
		(prog1 (read (buffer-substring-no-properties beg end))
			(setf (point) (- end 1)))))
(ebangs-add-type
 (concat "~~" "#")
 (lambda (beg)
	 (let* ((id (ebangs-read-number))
					(exp (ebangs-read-sexp))
					(items (eval exp))
					type (table (make-hash-table)))
		 (unless (and (listp items) (car items))
			 (error "Expected link expression to evaluate to list with a type, got:\n%S from \n%S" items exp))
		 (setf type (car items))
		 (dolist (i (cdr items))
			 (puthash (car i) (cadr i) table))
		 (puthash 'id id table)
		 (puthash 'position beg table)
		 (puthash 'line-number (line-number-at-pos beg) table)
		 (ebangs-make-instance type table (list id)))))

(defvar ebangs-completers (make-hash-table :test 'equal))
(defvar ebangs-completers-list (list))
(defun ebangs-add-completer (bang complete-fn)
	"INSTANCE-FN is called with the point at the start of a bang and with the length of it as an argument.
It returns an ebangs instance and leaves the point at the end of the bang's body. "
	(unless (member bang ebangs-completers-list) (push bang ebangs-completers-list))
	(puthash bang complete-fn ebangs-completers))

(defun ebangs-complete ()
	(interactive)
	(let ((end-of-bang (point))
				beg point-final)
		(unless (looking-back (regexp-opt ebangs-completers-list) nil t)
			(error "Nothing to complete."))
		(setf beg (match-beginning 0))
		(setf point-final (funcall (gethash (match-string 0) ebangs-completers) (- end-of-bang beg)))
		(ebangs-make-changed-overlay beg (point))
		(setf (point) point-final)))

(ebangs-add-completer
 (concat "~~" "#")
 (lambda (len)
	 (insert " " (base94->string (ebangs-get-next-number)) " '()")
	 (- (point) 1)))

(defvar-local ebangs--buffer-changed t)
(defvar ebangs--buffers-changed (list))
(defvar-local ebangs--buffer-overlays (list))
(defun ebangs-make-changed-overlay (beg end)
	(let ((ov (make-overlay beg end)) (buf (current-buffer)))
		(push ov ebangs--buffer-overlays)
		(overlay-put ov 'modification-hooks (list (lambda (&rest _)
																								(unless (memq buf ebangs--buffers-changed)
																									(push buf ebangs--buffers-changed))
																								(setf ebangs--buffer-changed t))))
		(overlay-put ov 'ebangs-change-overlay t)
		ov))

(defun ebangs--read-buffer-instances (file)
	(save-excursion
		(goto-char (point-min))
		(save-match-data
			(let ((rx (regexp-opt ebangs-bangs)))
				(cl-loop
				 with out = (make-hash-table)
				 while (re-search-forward rx nil t)
				 for beg = (match-beginning 0)
				 for bang = (match-string-no-properties 0)
				 do (cl-decf (point))
				 for i = (ebangs-instance beg)
				 do (setf (ebangs-get 'file i) file)
				 do (puthash i t out)
				 do (ebangs-make-changed-overlay beg (point))
				 finally (return out))))))

(defun ebangs--set-insts (file new-table)
	(let (indexed-insts (old-table (or (gethash file ebangs--files) (make-hash-table))))
		(ebangs--ht-loop i _ old-table
			do (ebangs--unindex-inst i)
			do (mapc #'ebangs-delete-number (ebangs-get 'owned-numbers i)))
		(condition-case err
				(ebangs--ht-loop i _ new-table
					do (ebangs--index-inst i)
					do (mapc #'ebangs-claim-number (ebangs-get 'owned-numbers i))
					do (push i indexed-insts))
			(error
			 (dolist (i indexed-insts)
				 (ebangs--unindex-inst i)
				 (mapc #'ebangs-delete-number (ebangs-get 'owned-numbers i)))
			 (ebangs--ht-loop i _ old-table
				 do (ebangs--index-inst i)
				 do (mapc #'ebangs-claim-number (ebangs-get 'owned-numbers i)))
			 (signal (car err) (cdr err))))
		(puthash file new-table ebangs--files)))
(defun ebangs--delete-buffer-numbers ()
	(mapc #'ebangs-delete-number ebangs--buffer-numbers)
	(setf ebangs--buffer-numbers (list)))
(defun ebangs--add-buffer-numbers (list)
	(dolist (i list)
		(ebangs-claim-number i)
		(push i ebangs--buffer-numbers)))

(defun ebangs-force-update-buffer (file)
	(let ((old-numbers ebangs--buffer-numbers)
				(old-overlays ebangs--buffer-overlays))
		(setf ebangs--buffer-overlays nil)
		(ebangs--delete-buffer-numbers)
		(condition-case err
				(ebangs--set-insts file (ebangs--read-buffer-instances))
			(error
			 (ebangs--delete-buffer-numbers)
			 (ebangs--add-buffer-numbers old-numbers)
			 (signal (car err) (cdr err))))
		(mapc #'delete-overlay old-overlays))
	;; set this last so that a partial change with error won't stop future updates
	(setf ebangs--buffer-changed nil)
	(setf ebangs--buffers-changed (delete (current-buffer) ebangs--buffers-changed)))

(defun ebangs-update-buffer (&optional file)
	(interactive)
	(if ebangs--buffer-changed
			(progn
				(when (interactive-p) (message "Buffer ebangs were out of date, updating."))
				(ebangs-force-update-buffer (or file (buffer-file-name))))
		(when (interactive-p) (message "Buffer ebangs up to date."))))

(defun ebangs-update ()
	(interactive)
	(dolist (i (copy-sequence ebangs--buffers-changed))
		(with-current-buffer i
			(when (interactive-p) (message "Updating: %S" (current-buffer)))
			(ebangs-update-buffer))))

(defvar ebangs--link-file)
;; ~~# " '(todo (text "load changed files by date"))
;; don't bother with the overlay stuff and just update all buffers that have changed
(defun ebangs-global-setup (&optional link-file)
	(add-hook 'post-gc-hook
						(lambda ()
							(ebangs--ht-loop _ (unique . table) ebangs--indexers
								unless unique do (ebangs--ht-remove-if #'hash-table-empty-p table))
							(ebangs--ht-remove-if #'hash-table-empty-p ebangs--files)))
	(add-hook 'before-save-hook #'ebangs-update-buffer)
	;; stop dead links appearing if a buffer was killed without updating
	(add-hook 'kill-buffer-hook (lambda () (setf ebangs--buffers-changed (delete (current-buffer) ebangs--buffers-changed))))
	(add-hook 'kill-emacs-hook #'ebangs-serialize)
	(advice-add 'find-file :after (lambda (&rest _) (ebangs-update-buffer)))

	(setf ebangs--buffers-changed (cl-remove-if-not #'buffer-file-name (buffer-list)))

	(setf ebangs--link-file (or link-file (file-name-concat user-emacs-directory "ebangs-linkfile")))
	(ebangs-deserialize))

(defun ebangs-serialize ()
	(with-temp-buffer
		(prin1
		 (ebangs--ht-loop
				 file table ebangs--files
			 nconc (ebangs--ht-loop i _ table collect i))
		 (current-buffer))
		(write-region nil nil ebangs--link-file)))
(defun ebangs-deserialize ()
	(with-temp-buffer
		(if (file-exists-p ebangs--link-file)
				(insert-file-contents ebangs--link-file)
			(instert "()"))
		(let ((insts (read (current-buffer))))
			(kill-region (point-min) (point-max))
			(message "%S" insts)
			(dolist (i insts)
				(mapc #'ebangs-claim-number (ebangs-get 'owned-numbers i))
				(ebangs-add-inst (ebangs-get 'file i) i)))))
;; TODO make this go from the last Begin to make the change overlay line up better
;; otherwise inserting a Begin before a block would mean no change is triggered, but if one happened it would bring the block backward
(defun ebangs-get-paragraph (name)
	(save-match-data
		(save-excursion
			(let (change-overlay-beg change-overlay-end text-beg text-end)
				(re-search-forward (rx "Begin" (+ space) (literal name) ":" (* any)))
				(setf change-overlay-beg (match-beginning 0))
				(setf text-beg (+ (match-end 0) 1))
				(re-search-forward (rx (* any) "End" (+ space) (literal name) "."))
				(setf text-end (- (match-beginning 0) 1))
				(setf change-overlay-end (match-end 0))
				(ebangs-make-changed-overlay change-overlay-beg change-overlay-end)
				(buffer-substring-no-properties text-beg text-end)))))

(defun ebangs-get-inst (key value)
	"Get the instance with key as value, given key is uniquely indexed."
	(let* ((it (gethash key ebangs--indexers))
				 (unique (car it))
				 (table (cdr it))
				 val)
		(unless it (error "Key %S is not indexed." key))
		(unless unique (error "Key %S is not unique." key))
		(gethash value table)))

(defmacro ebangs-loop (loop-keyword var &rest body)
	(declare (indent defun))
	(let* ((collection-form
					(if (eq '=> (car body))
							(prog1 (nth 1 body)
								(setf body (nthcdr 2 body)))
						var))
				 (from (when (eq (car body) :from)
								 (let ((from-arg (nth 1 body)))
									 (setf body (cddr body))
									 (if (listp from-arg) from-arg (list from-arg)))))
				 (from-key (car from))
				 (from-value (cadr from))
				 (cond (cons 'and body))
				 (_ (gensym "_"))
				 (table (gensym "table"))
				 (value-table (gensym "value-table")))
		(unless (symbolp var) (error "VAR should be a symbol."))
		`(progn
			 (ebangs-update)
			 ,(cond
				 ((and (eq from-key 'file) from-value)
					`(let ((,table (gethash ,from-value ebangs--files)))
						 (when ,table
							 (ebangs--ht-loop ,var ,_ ,table
								 if ,cond ,loop-keyword ,collection-form))))
				 ((eq from-key 'file) `(ebangs-select ,var => ,collection-form ,@body))
				 ((and from-key from-value)
					`(let ((,table (gethash ',from-key ebangs--indexers))
								 ,value-table)
						 (unless ,table (error "Key %S not indexed." ',from-key))
						 (when (car ,table) (error "Can not select value from unique key: %S." ',from-key))
						 (setf ,value-table (gethash ,from-value (cdr ,table)))
						 (when ,value-table
							 (ebangs--ht-loop ,var ,_ ,value-table
								 if ,cond ,loop-keyword ,collection-form))))
				 (from-key
					`(let ((,table (gethash ',from-key ebangs--indexers)))
						 (unless ,table (error "Key %S not indexed." ',from-key))
						 (if (car ,table) (ebangs--ht-loop ,_ ,var (cdr ,table)
																if ,cond ,loop-keyword ,collection-form)
							 (ebangs--ht-loop ,_ ,value-table (cdr ,table)
								 nconc (ebangs--ht-loop ,var ,_ ,value-table
												 if ,cond ,loop-keyword ,collection-form)))))
				 (t
					`(ebangs--ht-loop ,_ ,value-table ebangs--files
						 nconc (ebangs--ht-loop ,var ,_ ,value-table
										 if ,cond ,loop-keyword ,collection-form)))))))
(defmacro ebangs-select (var &rest body) (declare (indent defun)) `(ebangs-loop collect ,var ,@body))
(defmacro ebangs-foreach (var &rest body) (declare (indent defun)) `(ebangs-loop do ,var ,@body))

(defun ebangs-inspect ()
	(interactive)
	(let ((buf (generate-new-buffer "*ebangs-inspector*")))
		(switch-to-buffer buf)
		(ebangs-foreach i => (progn (prin1 (ebangs-inst->list i) buf) (newline)))))

(defun ebangs--test ()
	(ebangs-select i => (ebangs-get 'id i)
		:from id)
	(ebangs-select i :from (type 'todo) (= 1 (ebangs-get 'test i)))
	(ebangs-select i :from id (eq 1 (ebangs-get 'test i)))
	(ebangs-select i (eq 1 (ebangs-get 'test i)))
	)

(defun ebangs--bench (times lines count)
	(with-current-buffer (create-file-buffer "ebangs--bench")
		(setf buffer-file-name "ebangs--bench")
		(unwind-protect
				(let* ((repeat-every (/ lines count))
							 (count (/ lines repeat-every))
							 (nums (apply #'vector (cl-loop repeat count collect (ebangs-get-next-number))))
							 (nums-copy (copy-sequence nums))
							 (_ (dotimes (i lines)
										(if (/= 0 (% (+ 1 i) repeat-every))
												(insert "jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj")
											(let ((num (seq-random-elt nums)))
												(setf nums (delete num nums))
												(insert "~" "~# " (base94->string num) " '(todo (text \"test\"))")))
										(newline)))
							 (result (benchmark-run-compiled times (ebangs-force-update-buffer))))
					(ebangs--ht-loop inst _ (gethash "ebangs--bench" ebangs--files)
						do (ebangs-delete-inst inst))
					(cons (/ (car result) times) result))
			(mapc #'ebangs-delete-number ebangs--buffer-numbers)
			(set-buffer-modified-p nil)
			(kill-buffer))))

(defun show-buffer-todos ()
	(interactive)
	(let ((alist
				 (ebangs-select i => (ebangs-from i ((text 'text) (linum 'line-number) (pos 'position))
															 (cons (format "%S: %s" linum text) pos))
					 :from (file (buffer-file-name))
					 (eq (ebangs-get 'type i) 'todo))))
		(goto-char (alist-get (completing-read "Where to? " alist nil t) alist nil nil 'equal))))

(defun show-todos ()
	(interactive)
	(let* ((alist
					;; (ebangs-select i)
					(ebangs-select i => (ebangs-from i ((text 'text) (file 'file) (linum 'line-number) (pos 'position))
																(cons (format "%s.%s: %S: %s" (file-name-base file) (file-name-extension file) linum text) (cons file pos)))
						:from (type 'todo)))
				 (result (alist-get (completing-read "Where to? " alist nil t) alist nil nil 'equal)))
		(find-file (car result))
		(goto-char (cdr result))))
;; (load (buffer-file-nam))

