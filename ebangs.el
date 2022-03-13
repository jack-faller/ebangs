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

(defvar-local ebangs--unclaimed-numbers (list))
(let (free-numbers (next 0))
	(defun ebangs--claim-number (num)
		(unless (numberp num)
			(error "Expected num to ebangs--delete-number, got %S." num))
		(if (>= num next)
				(prog1 num
					(cl-loop for i from next below num
									 do (push i free-numbers))
					(setf next (+ num 1)))
			(let ((elt (memq num free-numbers)))
				(if (or (memq num free-numbers) (memq num ebangs--unclaimed-numbers))
						(setf free-numbers (delq num free-numbers)
									ebangs--unclaimed-numbers (delq num ebangs--unclaimed-numbers))
					(error "Number \"%s\" already claimed." (base94->string num))))))
	(defun ebangs-get-next-number ()
		(when ebangs--buffer-inhibit (error "Ebangs is inhibited in this buffer."))
		(let ((num (if (null free-numbers)
									 (prog1 next (cl-incf next))
								 (pop free-numbers))))
			(push num ebangs--unclaimed-numbers)
			num))
	(defun ebangs--delete-number (num)
		(unless (numberp num)
			(error "Expected num to ebangs--delete-number, got %S." num))
		(setf ebangs--unclaimed-numbers (delq num ebangs--unclaimed-numbers))
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

(defvar-local ebangs--buffer-last-change nil)
(defvar ebangs--file-update-times (make-hash-table :test 'equal))
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
					(remhash (car i) (ebangs-get 'table inst)))
				(ebangs--unindex-inst to-unindex))
			(error "Duplicate unique indices: %S" duplicates))))
(defun ebangs--index-and-claim (inst)
	(ebangs--index-inst inst)
	(let (claimed)
		(unwind-protect
				(cl-loop for i in (ebangs-get 'owned-numbers inst)
								 do (ebangs--claim-number i)
								 do (push i claimed)
								 finally (setf claimed nil))
			(mapc #'ebangs--delete-number claimed))))
(defun ebangs--unindex-inst (inst)
	(ebangs--ht-loop key (unique . table) ebangs--indexers
		for value = (ebangs-get key inst)
		if (and unique value)
		do (remhash value table)
		else if value
		do (let ((x (gethash value table)))
				 (when x (remhash inst x)))))
(defun ebangs--unindex-and-delete-nums (inst)
	(ebangs--unindex-inst inst)
	(mapc #'ebangs--delete-number (ebangs-get 'owned-numbers inst)))
(defun ebangs--ht-remove-if (f table)
	(ebangs--ht-loop key value table if (funcall f value) do (remhash key table)))

(defvar ebangs-types (make-hash-table :test 'equal))
(defvar ebangs-bangs (list))
(defun ebangs-set-type (bang instance-fn)
	"If INSTANCE-FN is nil, remove this type.
Otherwise INSTANCE-FN is called with the point on the end of a bang and with its start as the argument.
It returns an ebangs instance and leaves the point on the end of the bang's body. "
	(if instance-fn
			(progn
				(unless (member bang ebangs-bangs) (push bang ebangs-bangs))
				(puthash bang instance-fn ebangs-types))
		(setf ebangs (remove bang ebangs-bangs))
		(remhash bang ebangs-types)))
(defun ebangs--instance (beg)
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
(ebangs-set-type
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
(defun ebangs-set-completer (bang complete-fn)
	"If COMPLETE-FN is nil, remove this completer.
Otherwise COMPLETE-FN is called with the point on the end of a bang and with its start as the argument.
It should insert the body of the bang and may have any other side effects it wishes."
	(if complete-fn
			(progn
				(unless (member bang ebangs-completers-list) (push bang ebangs-completers-list))
				(puthash bang complete-fn ebangs-completers))
		(setf ebangs (remove bang ebangs-completers-list))
		(remhash bang ebangs-completers)))

(defun ebangs-complete ()
	(interactive)
	(when ebangs--buffer-inhibit (error "Ebangs is inhibited in this buffer."))
	(unless (looking-back (regexp-opt ebangs-completers-list) nil t)
		(error "Nothing to complete."))
	(funcall (gethash (match-string 0) ebangs-completers) (match-beginning 0))
	(ebangs--activate))

(ebangs-set-completer
 (concat "~~" "#")
 (lambda (start)
	 (insert " " (base94->string (ebangs-get-next-number)) " '()")
	 (cl-decf (point))))

(defvar-local ebangs--buffer-inhibit nil)
(defvar-local ebangs--buffer-active nil)
(defvar ebangs--buffers-changed (list))

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
				 for inst = (ebangs--instance beg)
				 do (setf (ebangs-get 'file inst) file)
				 do (puthash inst t out)
				 finally (return out))))))

(defun ebangs--set-insts (file new-table)
	(let (indexed-insts
				(old-table (or (gethash file ebangs--files) (make-hash-table)))
				(old-numbers (copy-sequence ebangs--unclaimed-numbers)))
		(ebangs--ht-loop i _ old-table
			do (ebangs--unindex-and-delete-nums i))
		(unwind-protect
				(ebangs--ht-loop i _ new-table
					do (ebangs--index-and-claim i)
					do (push i indexed-insts)
					finally (setf indexed-insts nil
												old-table nil))
			(mapc #'ebangs--unindex-and-delete-nums indexed-insts)
			(when old-table
				(setf ebangs--unclaimed-numbers old-numbers)
				(dolist (i ebangs--unclaimed-numbers)
					;; ensure all numbers are reset to their previous position
					(ignore-errors (ebangs--claim-number i)))
				(ebangs--ht-loop i _ old-table
					do (ebangs--index-and-claim i))))
		(puthash file new-table ebangs--files)))

(defun ebangs--update-file (&optional file buffer)
	(setf file (or file buffer-file-name))
	(if (setf buffer (or buffer (get-file-buffer file)))
			(with-current-buffer buffer
				(ebangs--set-insts file (ebangs--read-buffer-instances file)))
		(with-temp-buffer
			(ignore-errors (insert-file-contents file))
			(with-current-buffer buffer
				(ebangs--set-insts file (ebangs--read-buffer-instances file))))))

(defun ebangs--file-time (file)
	(or (file-attribute-modification-time (file-attributes file))
			;; return this to trigger an update for deleted files
			"missing file"))
(defun ebangs--determine-last-change (file)
	(let ((buf (get-file-buffer file)))
		(if (and buf (buffer-modified-p buf))
				(buffer-local-value 'ebangs--buffer-last-change buf)
			(ebangs--file-time file))))

(defun ebangs-update ()
	(ebangs--ht-loop file update-time ebangs--file-update-times
		do (let ((change-time (ebangs--determine-last-change file)))
				 (unless (equal update-time change-time)
					 (ebangs--update-file file)
					 (puthash file change-time ebangs--file-update-times))))
	;; safe to delete numbers here as all buffers have been updated
	;; any still unclaimed can be assumed not to be present
	(mapc #'ebangs--delete-number ebangs--unclaimed-numbers)
	(setf ebangs--unclaimed-numbers (list)))

(defun ebangs--activate ()
	(unless (or ebangs--buffer-active ebangs--buffer-inhibit)
		(setf ebangs--buffer-active t)
		(unless (gethash buffer-file-name ebangs--file-update-times)
			(puthash buffer-file-name "new buffer" ebangs--file-update-times))
		(setf ebangs--buffer-last-change (current-time))
		(add-hook 'after-change-functions (lambda (&rest _) (setf ebangs--buffer-last-change (current-time))))))
(defvar ebangs--link-file (file-name-concat user-emacs-directory "ebangs-linkfile"))

(defun ebangs--file-setup ()
	(if (not buffer-file-name) (setf ebangs--buffer-inhibit t)
		(when (gethash buffer-file-name ebangs--files)
			(ebangs--activate))))

(defvar ebangs-mode nil)
(define-global-minor-mode ebangs-global-minor-mode ebangs-mode ebangs--file-setup
	(add-hook 'post-gc-hook
						(lambda ()
							(ebangs--ht-loop _ (unique . table) ebangs--indexers
								unless unique do (ebangs--ht-remove-if #'hash-table-empty-p table))
							(ebangs--ht-remove-if #'hash-table-empty-p ebangs--files)))
	(add-hook 'kill-emacs-hook #'ebangs-serialize)
	(setf ebangs--buffers-changed (cl-remove-if-not #'buffer-file-name (buffer-list)))
	(ebangs-deserialize)
	;; update here to deal with files that have changed since last reading
	(ebangs-update))

(defun ebangs-serialize ()
	(with-temp-buffer
		(prin1
		 (ebangs--ht-loop
				 file table ebangs--files
			 nconc (ebangs--ht-loop i _ table collect i))
		 (current-buffer))
		(prin1 ebangs--file-update-times (current-buffer))
		(write-region nil nil ebangs--link-file)))
(defun ebangs-deserialize ()
	(with-temp-buffer
		(if (file-exists-p ebangs--link-file)
				(insert-file-contents ebangs--link-file)
			(insert "()")
			(prin1 (make-hash-table) (current-buffer)))
		(let* ((insts (read (current-buffer)))
					 (update-times (read (current-buffer))))
			(setf ebangs--file-update-times update-times)
			(kill-region (point-min) (point-max))
			(message "%S" insts)
			(dolist (i insts)
				(mapc #'ebangs--claim-number (ebangs-get 'owned-numbers i))
				(ebangs-add-inst (ebangs-get 'file i) i)))))
;; TODO make this go from the last Begin to make the change overlay line up better
;; otherwise inserting a Begin before a block would mean no change is triggered, but if one happened it would bring the block backward
(defun ebangs-get-paragraph (name)
	(save-match-data
		(save-excursion
			(let* ((text-beg (progn
												 (re-search-forward (rx "Begin" (+ space) (literal name) ":" (* any)))
												 (+ (match-end 0) 1)))
						 (text-end (progn
												 (re-search-forward (rx (* any) "End" (+ space) (literal name) "."))
												 (- (match-beginning 0) 1))))
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
	(with-temp-buffer
		(let* ((repeat-every (/ lines count))
					 (count (/ lines repeat-every))
					 (nums (apply #'vector (cl-loop repeat count collect (ebangs-get-next-number))))
					 (nums-copy (copy-sequence nums)))
			(unwind-protect
					(progn (dotimes (i lines)
									 (if (/= 0 (% (+ 1 i) repeat-every))
											 (insert "jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj")
										 (let ((num (seq-random-elt nums)))
											 (setf nums (delete num nums))
											 (insert "~" "~# " (base94->string num) " '(todo (text \"test\"))")))
									 (newline))
								 (let ((result (benchmark-run-compiled times (ebangs--update-file "ebangs--bench" (current-buffer)))))
									 (cons (/ (car result) times) result)))
				(ebangs--ht-loop inst _ (gethash "ebangs--bench" ebangs--files)
					do (ebangs--unindex-inst inst))
				(mapc #'ebangs--delete-number nums-copy)))))
;; (ebangs--bench 100 1000 30)
;; (progn (profiler-start 'cpu+mem)
;; 			  (ebangs--bench 10000 1000 30)
;; 			  (profiler-stop))
(defun show-buffer-todos ()
	(interactive)
	(let ((alist
				 (ebangs-select i => (ebangs-from i ((text 'text) (linum 'line-number) (pos 'position))
															 (cons (format "%S: %s" linum text) pos))
					 :from (file buffer-file-name)
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

