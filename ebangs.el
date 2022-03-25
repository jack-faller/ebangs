;;; ebangs.el --- Provides the command `ebangs-global-minor-mode'. -*- lexical-binding: t -*-

;;; Commentary:

;; Provides the command `ebangs-global-minor-mode'.  This mode allows bangs to
;; be entered into files and then accessed at a later date.

;;; Code:

(require 'cl-lib)
(require 'rx)
(defun base94->int (str)
  "Convert a string to an elisp number.
STR should be the string of a base 94 integer (using the char range 33 to 126)
to be converted to an elisp number."
  (cl-loop for i across str
		   with acc = 0
		   do (setf acc (+ (* acc 94) (- i 33)))
		   finally (return acc)))

(defun int->base94 (num)
  "Convert an elisp number to a string.
The reverse of `base94->int'.
NUM should be an elisp integer."
  (if (= num 0) "!"
	(cl-loop while (> num 0) with acc
			 do (progn
				  (push (+ 33 (% num 94)) acc)
				  (setf num (/ num 94)))
			 finally (return (apply #'string acc)))))

(defvar-local ebangs--buffer-inhibit nil)
(defvar ebangs--unclaimed-numbers (make-hash-table)
  "A hash map from numbers to the buffer they were gotten in.")
(defvar ebangs--next-free-number 0)
(defun ebangs--ht-first-key (ht)
  "Get the first key in hash table HT or nil if empty."
  (if (hash-table-empty-p ht)
	  nil
	(cl-block nil (maphash (lambda (a _) (cl-return a))
						   ht))))
(defvar ebangs--free-numbers (make-hash-table)
  "Free numbers below `ebangs--next-free-number'.")
(defun ebangs--claim-number (num)
  "Claim ownership of a NUM for use as an id.
Through an error if NUM is already claimed."
  (unless (numberp num)
	(error "Expected num to ebangs--claim-number, got %S" num))
  (if (>= num ebangs--next-free-number)
	  (prog1 num
		(cl-loop for i from ebangs--next-free-number below num
				 do (puthash i t ebangs--free-numbers))
		(setf ebangs--next-free-number (+ num 1)))
	(if (or (gethash num ebangs--free-numbers) (gethash num ebangs--unclaimed-numbers))
		(progn
		  (remhash num ebangs--free-numbers)
		  (remhash num ebangs--unclaimed-numbers))
	  (error "Number “%s” already claimed" (int->base94 num)))))
(defun ebangs-get-number ()
  "Get a unique number for use as an id.
Once a number has no instances that own it, and the buffer it was gotten in is
dead, it may be returned from this function again."
  (when ebangs--buffer-inhibit (error "Ebangs is inhibited in this buffer"))
  (let ((num (or (ebangs--ht-first-key ebangs--free-numbers) (prog1 ebangs--next-free-number (cl-incf ebangs--next-free-number)))))
	(remhash num ebangs--free-numbers)
	(puthash num (or buffer-file-name (error "Can not claim number in buffer without file")) ebangs--unclaimed-numbers)
	num))
(defun ebangs--delete-number (num)
  "Free NUM, allowing it to be returned from `ebangs-get-number'.
NUM should be an integer that should no longer be in use as an id, or in a file."
  (unless (numberp num)
	(error "Expected num to ebangs--delete-number, got %S" num))
  (remhash num ebangs--unclaimed-numbers)
  (puthash num t ebangs--free-numbers))

(defun ebangs--number-bench (times count)
  "Get COUNT numbers are delete/claim random ones TIMES times."
  (let (nums)
	(prog1 (benchmark-run 1
			 (setf nums (apply #'vector (cl-loop repeat count collect (ebangs-get-number))))
			 (dotimes (_ times)
			   (ignore-errors (ebangs--claim-number (seq-random-elt nums)))
			   (ebangs--delete-number (seq-random-elt nums))))
	  (mapc #'ebangs--delete-number nums))))

;; (ebangs--number-bench 10000 10000) ; => (0.108076199 0 0.0)

(defun ebangs-make-instance (type table &optional owned-numbers)
  "Make a bang instance with TYPE and OWNED-NUMBERS.
TABLE should be a hash table containing any extra keys and values for the
TABLE is altered by this function.
OWNED-NUMBERS are the numbers from `ebangs-get-number' that can be freed for use
in other instances when this one is destroyed.
instance, these can then be gotten by `ebangs-get'.
To allow them to be serialized and read from/to the linkfile, instances should
only contain objects with can be printed readably and never another instance.
To reference another instance, give it an id with `ebangs-get-number'."
  (unless (cl-loop for i in owned-numbers
				   always (numberp i))
	(error "Owned numbers must be list of numbers got: %S" owned-numbers))
  (puthash 'type type table)
  (when owned-numbers (puthash 'owned-numbers owned-numbers table))
  table)
(defun ebangs-get (name inst)
  "Get a value from the key NAME from an instance INST.
This can also be used as a gv-setter."
  (gethash name inst))
(defun ebangs-set (name val inst)
  "Set the key NAME to VAL in the instance INST."
  (puthash name val inst))
(defun ebangs-copy-inst (inst)
  "Return a new instance with the same keys and values as INST."
  (copy-hash-table inst))
(gv-define-setter ebangs-get (val name inst)
  `(puthash ,name ,val ,inst))
(defmacro ebangs-from (var decls &rest body)
  "Bind DECLS to the values of keys from VAR then evaluate BODY.
DECLS should be list of (variable-name key) pairs."
  (declare (indent 2))
  (let* ((x (gensym))
		 (let-list (mapcar (lambda (decl) (list (car decl) (list 'ebangs-get (cadr decl) x)))
						   decls)))
	`(let ((,x ,var))
	   (let ,let-list ,@body))))
(defmacro ebangs--ht-loop (k v table &rest body)
  "Loop over the keys and values from TABLE, splice BODY into `cl-loop'.
K and V should be variables to be bound destructureingly to keys and values.
BODY should be composed of valid loop clauses (see `cl-loop').
TABLE should evaluate to a hash table."
  (declare (indent 3))
  (let ((ksym (gensym)) (vsym (gensym)))
	`(cl-loop for ,ksym being the hash-keys of ,table using (hash-values ,vsym)
			  for ,k = ,ksym
			  for ,v = ,vsym
			  ,@body)))
(defun ebangs-inst->list (inst)
  "Convert an instance INST to a list of (key value) pairs."
  (ebangs--ht-loop k v inst collect (list k v)))
(defun ebangs-inst-delete (key inst)
  "Remove KEY from the instance INST.
KEY should be a valid key to INST accessible by `ebangs-get' but should not be
`owned-numbers', `table', or `type'."
  (remhash key inst))

(defvar-local ebangs--buffer-last-change nil
  "The last time this buffer was changed.")
(defvar ebangs--file-update-times (make-hash-table :test 'equal)
  "A hash map from files to the last time their contents was read.")
;; (map file-name (map ebangs-inst bool))
;; (map key (unique (or ebangs-inst (map value (map ebangs-inst ool)))))
(defvar ebangs--indexes (make-hash-table)
  "A hash map from indexed keys to (UNIQUE . UNIQUE-TABLE or TABLE).
Where:
UNIQUE is a boolean specifying if this index is unique,
UNIQUE-TABLE for unique indexes is a hash map from VALUE to instances with both
that key and that value,
TABLE for a non-unique indexes is a hash map from a value to a VALUE-TABLE,
and VALUE-TABLEs are hash maps from an instance with the related key and value
to t.")
(defun ebangs-index-on (key &optional unique test)
  "Index on KEY; must be called before `ebangs-global-minor-mode'.
When UNIQUE is non-nil, instances with KEY can not share values for KEY.
TEST should be a valid hash map test used to determine the uniqueness of values
of KEY (even if UNIQUE is nil); it is 'eql by default.
Records whose value for KEY is nil will not be indexed, all others will be
registered in `ebangs--indexes'."
  (puthash key (cons unique (make-hash-table :test (or test 'eql))) ebangs--indexes))
(ebangs-index-on 'type)
(ebangs-index-on 'id t)
(ebangs-index-on 'file nil 'equal)
(defvar ebangs--files (cdr (gethash 'file ebangs--indexes))
  "A hash map from file-names to instance-tables.
Where instance-table are maps from instances in a file to t.")
(defun ebangs--index-inst (inst)
  "Enter the keys from INST into `ebangs--indexes'.
Through an error if unique keys are shared."
  (ebangs--ht-loop key (unique . table) ebangs--indexes
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
		  (ebangs-inst-delete (car i) inst))
		(ebangs--unindex-inst to-unindex))
	  (error "Duplicate unique indices: %S" duplicates))))
(defun ebangs--index-and-claim (inst)
  "Do `ebangs--index-inst' and claim the owned-numbers from INST."
  (ebangs--index-inst inst)
  (let (claimed)
	(unwind-protect
		(cl-loop for i in (ebangs-get 'owned-numbers inst)
				 do (ebangs--claim-number i)
				 do (push i claimed)
				 finally (setf claimed nil))
	  (mapc #'ebangs--delete-number claimed))))
(defun ebangs--unindex-inst (inst)
  "Remove INST from `ebangs--indexes'."
  (ebangs--ht-loop key (unique . table) ebangs--indexes
	for value = (ebangs-get key inst)
	if (and unique value)
	do (remhash value table)
	else if value
	do (let ((x (gethash value table)))
		 (when x (remhash inst x)))))
(defun ebangs--unindex-and-delete-nums (inst)
  "Do `ebangs--unindex-inst' and delete INST's owned numbers."
  (ebangs--unindex-inst inst)
  (mapc #'ebangs--delete-number (ebangs-get 'owned-numbers inst)))
(defun ebangs--ht-remove-if (f table)
  "Call remhash on keys from the hash table TABLE that satisfy the predicate F."
  (ebangs--ht-loop key value table if (funcall f value) do (remhash key table)))

(defvar ebangs-types (make-hash-table :test 'equal)
  "A hash map from the string of bangs to instancing functions.")
(defvar ebangs-types-list (list)
  "A list of bangs which appear in `ebangs-types'.")
(defun ebangs-set-type (bang instance-fn)
  "Set the instance function to the string BANG.
If INSTANCE-FN is nil, remove BANG's function.
Otherwise INSTANCE-FN is called with the point on the end of a bang and with its
start as the argument.
It returns an ebangs instance and leaves the point on the end of the bang's
body."
  (if instance-fn
	  (progn
		(unless (member bang ebangs-types-list) (push bang ebangs-types-list))
		(puthash bang instance-fn ebangs-types))
	(setf ebangs-types-list (remove bang ebangs-types-list))
	(remhash bang ebangs-types)))
(defun ebangs--instance (beg)
  "Instance a bang by calling its instance function.
This should be called with the point on the end of the bang and with BEG as the
start of it.
\(buffer-substring beg (+ (point) 1)) should yeild the string of the bang."
  (let ((bang (buffer-substring-no-properties beg (+ (point) 1))))
	(funcall (gethash bang ebangs-types) beg)))

(defun ebangs-read-number ()
  "For use in an instance function, read a base 94 number and return it.
This should be called with the point on the end of the last item and will leave
it on the end of the number."
  (cl-incf (point))
  (unless (looking-at (rx (+ space) (group (+? any)) (or eol space)))
	(error "Expected number got: “%s”" (buffer-substring-no-properties (point) (line-end-position))))
  (setf (point) (- (match-end 1) 1))
  (base94->int (match-string 1)))
(defun ebangs-read-sexp ()
  "For use in an instance function, read one sexp number and return it.
This should be called with the point on the end of the last item and will leave
it on the end of the number."
  (cl-incf (point))
  (unless (looking-at (rx (+ space)))
	(error "Expected space got: “%s”" (buffer-substring-no-properties (point) (line-end-position))))
  (setf (point) (match-end 0))
  (let* ((beg (point))
		 (end (progn (forward-sexp)
					 (point))))
	(prog1 (read (buffer-substring-no-properties beg end))
	  (setf (point) (- end 1)))))

(defvar ebangs-completers (make-hash-table :test 'equal)
  "A hash map from bangs to functions that complete them as in `ebangs-complete'.")
(defvar ebangs-completers-list (list)
  "A list of bangs with completers.")
(defun ebangs-set-completer (bang complete-fn)
  "Set the completer for BANG to COMPLETE-FN for use in `ebangs-complete'.
If COMPLETE-FN is nil, remove BANG's completer.
Otherwise COMPLETE-FN is called with the point on the end of a bang and with its
start as the argument.
It should insert the body of the bang and may have any other side effects it
wishes."
  (if complete-fn
	  (progn
		(unless (member bang ebangs-completers-list) (push bang ebangs-completers-list))
		(puthash bang complete-fn ebangs-completers))
	(setf ebangs-completers-list (remove bang ebangs-completers-list))
	(remhash bang ebangs-completers)))

(defun ebangs-complete ()
  "Complete the body of the bang preceding the point.
This is the only valid way to enter a new bang."
  (interactive)
  (when ebangs--buffer-inhibit (error "Ebangs is inhibited in this buffer"))
  (unless (looking-back (regexp-opt ebangs-completers-list) nil t)
	(error "Nothing to complete here"))
  (funcall (gethash (match-string 0) ebangs-completers) (match-beginning 0))
  (ebangs-activate))

(defvar-local ebangs--buffer-active nil
  "Non nil once a bang has been put in a buffer.")

(defun ebangs--read-buffer-instances (file)
  "Read all bangs from the current buffer, setting their file to FILE.
Return a list of their instances."
  (save-excursion
	(goto-char (point-min))
	(save-match-data
	  (let ((rx (regexp-opt ebangs-types-list)))
		(cl-loop
		 with out = (make-hash-table)
		 while (re-search-forward rx nil t)
		 for beg = (match-beginning 0)
		 do (cl-decf (point))
		 for inst = (ebangs--instance beg)
		 do (setf (ebangs-get 'file inst) file)
		 do (puthash inst t out)
		 finally (return out))))))

(defun ebangs--set-insts (file new-table)
  "Atomically set the instances in FILE to the ones in NEW-TABLE.
NEW-TABLE should be an instance-table as seen in `ebangs--files'."
  (let* (indexed-insts
		 (it (gethash file ebangs--files))
		 (old-table (if it (copy-hash-table it) (make-hash-table)))
		 (old-numbers (copy-hash-table ebangs--unclaimed-numbers)))
	(ebangs--ht-loop i _ old-table
	  do (ebangs--unindex-and-delete-nums i))
	(unwind-protect
		(ebangs--ht-loop i _ new-table
		  do (ebangs--index-and-claim i)
		  do (push i indexed-insts)
		  finally (setf old-table nil))
	  (when old-table
		(mapc #'ebangs--unindex-and-delete-nums indexed-insts)
		(setf ebangs--unclaimed-numbers old-numbers)
		(ebangs--ht-loop i _ old-table
		  do (ebangs--index-and-claim i))))))

(defmacro ebangs--with-file-buf (file &rest body)
  "Create a temporary buffer with the file name FILE, evaluate BODY in that buffer."
  (declare (indent 1))
  `(with-temp-buffer
	 (setf buffer-file-name ,file)
	 (unwind-protect (progn ,@body)
	   ;; unset the file name to avoid a prompt for modified file
	   (setf buffer-file-name nil))))

(defun ebangs--update-file (&optional file buffer)
  "Update the instances from FILE.
If FILE is not given the current buffer is assumed.
If BUFFER isn't given, it will first look for open buffers for FILE, if none
exist it will create create a temporary one and read FILE into it."
  (setf file (or file buffer-file-name))
  (if (setf buffer (or buffer (get-file-buffer file)))
	  (with-current-buffer buffer
		(ebangs--set-insts file (ebangs--read-buffer-instances file)))
	(ebangs--with-file-buf file
	  (ignore-errors (insert-file-contents file))
	  (ebangs--set-insts file (ebangs--read-buffer-instances file)))))

(defun ebangs--file-time (file)
  "Return the most recent modification of FILE in the file system.
Or the string `missing file' if the file does not exist."
  (or (file-attribute-modification-time (file-attributes file))
	  ;; return this to trigger an update for deleted files
	  "missing file"))
(defun ebangs--determine-last-change (file)
  "Figure out the last time FILE was changed by the user."
  (let ((buf (get-file-buffer file)))
	(if (and buf (buffer-modified-p buf))
		(buffer-local-value 'ebangs--buffer-last-change buf)
	  (ebangs--file-time file))))

(defun ebangs-update ()
  "Update any changed files by reading their instances."
  (ebangs--ht-loop file update-time ebangs--file-update-times
	do (let ((change-time (ebangs--determine-last-change file)))
		 (unless (equal update-time change-time)
		   (ebangs--update-file file)
		   (puthash file change-time ebangs--file-update-times))))
  ;; only delete numbers from dead buffers as they may still be in the undo history
  (mapc #'ebangs--delete-number
		(ebangs--ht-loop num file ebangs--unclaimed-numbers
		  unless (get-file-buffer file) collect num)))

(defun ebangs-activate ()
  "Start tracking edits to the current buffer."
  (interactive)
  (unless (or ebangs--buffer-active ebangs--buffer-inhibit)
	(setf ebangs--buffer-active t)
	(unless (gethash buffer-file-name ebangs--file-update-times)
	  (puthash buffer-file-name "new buffer" ebangs--file-update-times))
	(setf ebangs--buffer-last-change (current-time))
	(add-hook 'after-change-functions (lambda (&rest _) (setf ebangs--buffer-last-change (current-time))))))
(defun ebangs-check-active ()
  "Display if the current buffer is being watched for edits."
  (interactive)
  (if (and (not ebangs--buffer-inhibit) ebangs--buffer-active)
	  (message "Buffer active.")
	(message "Buffer not active.")))

(defvar ebangs-linkfile (concat (file-name-as-directory user-emacs-directory) "ebangs-linkfile")
  "The file links are saved to.
This should be set before `ebangs-global-minor-mode' is called.")

(defun ebangs--file-setup ()
  "Called on instancing a buffer to determine how ebangs should track it."
  (if (not buffer-file-name) (setf ebangs--buffer-inhibit t)
	(when (gethash buffer-file-name ebangs--files)
	  (ebangs-activate))))

(defvar ebangs-mode nil)
(define-global-minor-mode ebangs-global-minor-mode ebangs-mode ebangs--file-setup
  (add-hook 'post-gc-hook
			(lambda ()
			  (ebangs--ht-loop _ (unique . table) ebangs--indexes
				unless unique do (ebangs--ht-remove-if #'hash-table-empty-p table))))
  (add-hook 'kill-emacs-hook #'ebangs-serialize)
  (ebangs--deserialize)
  ;; update here to deal with files that have changed since last reading
  (ebangs-update)
  ;; stop tracking files that are empty
  (ebangs--ht-remove-if (lambda (k) (not (gethash k ebangs--files))) ebangs--file-update-times))

(defun ebangs-serialize ()
  "Save all instances and metadata to `ebangs-linkfile'."
  (ebangs-update)
  (with-temp-buffer
	(prin1
	 (ebangs--ht-loop _ table ebangs--files
	   nconc (ebangs--ht-loop i _ table collect i))
	 (current-buffer))
	(newline)
	(prin1 ebangs--file-update-times (current-buffer))
	(write-region nil nil ebangs-linkfile)))

(defun ebangs--reset-all ()
  "Reset all instances to empty requiring re-read of everything."
  (let* ((old-indexes (ebangs--ht-loop key (unique . index) ebangs--indexes
						collect (list key unique (hash-table-test index)))))
	(setf ebangs--indexes (make-hash-table))
	(mapc (lambda (a) (apply #'ebangs-index-on a)) old-indexes))
  (setf ebangs--files (cdr (gethash 'file ebangs--indexes)))
  (setf ebangs--free-numbers (make-hash-table))
  (setf ebangs--next-free-number 0)
  (setf ebangs--unclaimed-numbers (make-hash-table))
  (ebangs--ht-loop k _ ebangs--file-update-times
	do (puthash k (current-time) ebangs--file-update-times)))

(defun ebangs--deserialize ()
  "Load instances and metadata from `ebangs-linkfile'.
Only call this right at the start from `ebangs-global-minor-mode'; it will
overwrite file update data and will create duplicate instances."
  (with-temp-buffer
	(if (file-exists-p ebangs-linkfile)
		(insert-file-contents ebangs-linkfile)
	  (insert "() ")
	  (prin1 (make-hash-table :test 'equal) (current-buffer)))
	(goto-char (point-min))
	(let* ((insts (read (current-buffer)))
		   (update-times (read (current-buffer))))
	  (setf ebangs--file-update-times update-times)
	  (kill-region (point-min) (point-max))
	  (message "%S" insts)
	  (dolist (i insts)
		(ebangs--index-and-claim i)))))
(defun ebangs-get-paragraph (name)
  "Get the string between the next lines with `Begin NAME:' and `End NAME.'."
  (save-match-data
	(save-excursion
	  (let* ((text-beg (progn
						 (re-search-forward (rx "Begin" (+ space) (literal name) ":" (* any)))
						 (+ (match-end 0) 1)))
			 (text-end (progn
						 (re-search-forward (rx (* any) "End" (+ space) (literal name) "."))
						 (- (match-beginning 0) 1))))
		(buffer-substring-no-properties text-beg text-end)))))

(defun ebangs-inst-with (key value &optional must-exist)
  "Get the instance with KEY set to VALUE, given key is uniquely indexed.
If MUST-EXIST, signal an error when the instance doesn't."
  (ebangs-update)
  (let* ((it (gethash key ebangs--indexes))
		 (unique (car it))
		 (table (cdr it))
		 out)
	(unless it (error "Key %S is not indexed" key))
	(unless unique (error "Key %S is not unique" key))
	(setf out (gethash value table))
	(unless (and out must-exist) (error "No instance with %s set to %s" key value))
	out))

;; potentially bad behaviour from nested selects / modifying values inside a select, oh well.
(defmacro ebangs-loop (accumulator var &rest body)
  "Iterate over and collect matching instances.
\(ebangs-loop ACCUMULATOR VAR [=> COLLECTION-FORM]
    [:from INDEX]
    or [:from (INDEX VALUE)]
    or [:from (INDEX VAR :where CONDITIONS...)]
  BODY...)
Loop with VAR bound to all bang instances, or just those with the key INDEX, set
to the value VALUE if those are provided and INDEX is an indexed key.
If all forms in BODY evaluate as non-nil, collect COLLECTION-FORM using the
`cl-loop' accumulator ACCUMULATOR."
  (declare (indent defun))
  (let* ((outer-accumulator (if (eq accumulator 'collect) 'nconc accumulator))
		 (collection-form
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
		 (from-condition
		  (cond ((null (caddr from)) nil)
				((eq :where (caddr from)) (cons 'and (cdddr from)))
				(t (error "Expected where in from conditions"))))
		 (cond (cons 'and body))
		 (outer-table (gensym "outer-table"))
		 (inner-table (gensym "inner-table"))
		 (inner-loop (lambda (keys/values)
					   `(cl-loop for ,var being the ,keys/values of ,inner-table
								 if ,cond ,accumulator ,collection-form))))
	(unless (symbolp var) (error "VAR should be a symbol"))
	`(progn
	   (ebangs-update)
	   ,(cond
		 ((and from-key from-condition)
		  `(let ((,outer-table (gethash ',from-key ebangs--indexes)))
			 (unless ,outer-table (error "Key %S not indexed" ',from-key))
			 (if (car ,outer-table)
				 (ebangs--ht-loop ,from-value ,var (cdr ,outer-table)
				   if ,from-condition if ,cond ,accumulator ,collection-form)
			   (ebangs--ht-loop ,from-value ,inner-table (cdr ,outer-table)
				 if ,from-condition
				 ,outer-accumulator ,(funcall inner-loop 'hash-keys)))))
		 ((and from-key from-value)
		  `(let ((,outer-table (gethash ',from-key ebangs--indexes))
				 ,inner-table)
			 (unless ,outer-table (error "Key %S not indexed" ',from-key))
			 (if (car ,outer-table)
				 (cl-loop repeat 1 with ,var
						  while (setf ,var (gethash ,from-value (cdr ,outer-table)))
						  ,accumulator ,collection-form)
			   (setf ,inner-table (gethash ,from-value (cdr ,outer-table)))
			   (when ,inner-table ,(funcall inner-loop 'hash-keys)))))
		 (from-key
		  `(let ((,outer-table (gethash ',from-key ebangs--indexes)))
			 (unless ,outer-table (error "Key %S not indexed" ',from-key))
			 (if (car ,outer-table)
				 (let ((,inner-table (cdr ,outer-table)))
				   ,(funcall inner-loop 'hash-values))
			   (cl-loop for ,inner-table being the hash-values of (cdr ,outer-table)
						,outer-accumulator ,(funcall inner-loop 'hash-keys)))))
		 (t
		  `(cl-loop for ,inner-table being the hash-values of ebangs--files
					,outer-accumulator ,(funcall inner-loop 'hash-keys)))))))
(defmacro ebangs-select (var &rest body)
  "`ebangs-loop' with the keyword collect."
  (declare (indent defun))
  `(ebangs-loop collect ,var ,@body))
(defmacro ebangs-foreach (var &rest body)
  "`ebangs-loop' with the keyword do."
  (declare (indent defun))
  `(ebangs-loop do ,var ,@body))

(defun ebangs-prompt-for-id ()
  "Prompt for an instance with an ID and return its ID."
  (let ((alist (ebangs-select i => (cons (prin1-to-string (ebangs-inst->list i)) (ebangs-get 'id i))
				 :from id)))
	(alist-get (completing-read "Select instance: " alist nil t) alist nil nil 'equal)))

(defun ebangs-inspect ()
  "Show a buffer with all instances."
  (interactive)
  (let ((buf (generate-new-buffer "*ebangs-inspector*")))
	(switch-to-buffer buf)
	(ebangs-foreach i => (progn (prin1 (ebangs-inst->list i) buf) (newline)))))

(defun ebangs--bench (times lines count)
  "Benchmark updating a file with LINES line and COUNT bangs TIMES times."
  (ebangs--with-file-buf "ebangs--bench"
	(let* ((repeat-every (/ lines count))
		   (count (/ lines repeat-every))
		   (nums (apply #'vector (cl-loop repeat count collect (ebangs-get-number))))
		   (nums-copy (copy-sequence nums)))
	  (unwind-protect
		  (progn (dotimes (i lines)
				   (if (/= 0 (% (+ 1 i) repeat-every))
					   (insert "jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj")
					 (let ((num (seq-random-elt nums)))
					   (setf nums (delete num nums))
					   (insert "~" "~# " (int->base94 num) " '(todo (text \"test\"))")))
				   (newline))
				 (let ((result (benchmark-run-compiled times (ebangs--update-file "ebangs--bench" (current-buffer)))))
				   (cons (/ (car result) times) result)))
		(mapc #'ebangs--delete-number nums-copy)
		(ebangs--ht-loop inst _ (copy-hash-table (or (gethash "ebangs--bench" ebangs--files) (make-hash-table)))
		  do (ebangs--unindex-inst inst))))))
;; at 2GCs, 0.0015 per update, 1GC is 0.0014
;; (ebangs--bench 1000 1000 30)
;; (unwind-protect (progn (profiler-start 'cpu+mem) (ebangs--bench 10000 1000 30)) (profiler-stop))

(ebangs-set-type
 (concat "~~" "#")
 (lambda (beg)
   (let* ((id (ebangs-read-number))
		  (exp (ebangs-read-sexp))
		  (items (eval exp t))
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
(ebangs-set-type
 (concat "~~" ">")
 (lambda (beg)
   (let* ((dest (ebangs-read-number))
		  (text (ebangs-read-sexp))
		  (table (make-hash-table)))
	 (unless (stringp text) (error "Link should have a string following it"))
	 (puthash 'dest dest table)
	 (puthash 'text text table)
	 (puthash 'position beg table)
	 (puthash 'line-number (line-number-at-pos beg) table)
	 (ebangs-make-instance 'link table (list)))))

(ebangs-set-completer
 (concat "~~" "#")
 (lambda (_)
   (insert " " (int->base94 (ebangs-get-number)) " '()")
   (cl-decf (point))))
(ebangs-set-completer
 (concat "~~" ">")
 (lambda (beg)
   ;; remove the bang as ebangs-prompt-for-id updates, which would cause an error if it
   ;; tried to read the uncompleted bang
   (backward-delete-char (- (point) beg))
   (insert "~~" "> " (int->base94 (ebangs-prompt-for-id)) " \"\"")
   (cl-decf (point))))

(defun ebangs-visit-inst (inst)
  "Go to the location of INST using its position and file attributes."
  (find-file (ebangs-get 'file inst))
  (goto-char (ebangs-get 'position inst)))
(defun ebangs-goto-inst-at-point ()
  "Go to the instance for the number at the point."
  (interactive)
  (re-search-backward (rx (or space bol)))
  (cl-decf (point))
  (ebangs-visit-inst (ebangs-inst-with 'id (ebangs-read-number) t)))

(defun ebangs-show-file-todos ()
  "Prompt with the text property and location of todos in the current file."
  (interactive)
  (let ((alist
		 (ebangs-select i => (ebangs-from i ((text 'text) (linum 'line-number) (pos 'position))
							   (cons (format "%S: %s" linum text) pos))
		   :from (file buffer-file-name)
		   (eq (ebangs-get 'type i) 'todo))))
	(goto-char (alist-get (completing-read "Where to? " alist nil t) alist nil nil 'equal))))

(defun ebangs-show-todos ()
  "Prompt with the text property and location of all todos."
  (interactive)
  (let* ((alist
		  ;; (ebangs-select i)
		  (ebangs-select
			i => (ebangs-from i ((text 'text) (file 'file) (linum 'line-number))
				   (cons
					(format "%s.%s: %S: %s" (file-name-base file) (file-name-extension file) linum text)
					i))
			:from (type 'todo)))
		 (result (alist-get (completing-read "Where to? " alist nil t) alist nil nil 'equal)))
	(ebangs-visit-inst result)))

(provide 'ebangs)
;;; ebangs.el ends here
