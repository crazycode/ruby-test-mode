;;; ruby-test.el --- Test runner for ruby unit tests.

;; Caspar Florian Ebeling <florian.ebeling@gmail.com>, 2007-12-06
;; This software can be redistributed. GPL v2 applies.

;; todo
;;   - run single test method or spec example

;;; Commentary:

;; This mode provides commands for running ruby tests. The output is
;; shown in separate buffer '*Ruby-Test*' in ruby-test
;; mode. Backtraces from failures and errors are marked, and can be
;; clicked to bring up the relevent source file, where point is moved
;; to the named line.
;;
;; The tests can be both, either rspec behaviours, or unit
;; tests. (File names are assumed to end in _spec.rb or _test.rb to
;; tell the type.)  When the command for running a test is invoked, it
;; looks at several places for an actual test to run: first, it looks
;; if the current buffer is a test (or spec), secondly, if not, it
;; checks whether one of the visible buffers is, thirdly it looks if
;; there has been a test run before (during this session), in which
;; case that test is invoked again.

;;; History:
;; - 09.02.08, Clickable backtrace added.
;; - 02.03.08, Rails support, by Roman Scherer
;; - 06.06.08, Bugfixes
;; - 09.07.08, Fix backtrace rendering
;; - 17.07.08, Fix rails support and lookup of unqualified executables

;;; Code:

(defvar ruby-test-buffer-name "*Ruby-Test*")

(defvar ruby-test-mode-hook)

(defvar ruby-test-last-run)

(defvar ruby-test-buffer)

(defvar ruby-test-ruby-executables
  '("/opt/local/bin/ruby" "/usr/bin/ruby" "ruby" "ruby1.9")
  "*A list of ruby executables to use. Non-absolute paths get
  expanded using `PATH'. The first existing will get picked. Set
  this variable to use the implementation you intend to test
  with.")

(defvar ruby-test-spec-executables
  '("/opt/local/bin/spec" "spec" "/usr/bin/spec" "/usr/local/bin/spec")
  "*A list of spec executables. If the spec does not belong to a
  rails project, then non-absolute paths get expanded using
  `PATH'; The first existing will get picked. In a rails project
  the `script/spec' script will be invoked.")

(defvar ruby-test-backtrace-key-map
  "The keymap which is bound to marked trace frames.")

;; global, since these bindings should be visible in other windows
;; operating on the file named by variable `ruby-test-last-run'.
(global-set-key (kbd "C-x t") 'ruby-test-run-file)
(global-set-key (kbd "C-x SPC") 'ruby-test-run-file)

(defun ruby-spec-p (filename)
  (and (stringp filename) (string-match "spec\.rb$" filename)))

(defun ruby-test-p (filename)
  (and (stringp filename) (string-match "test\.rb$" filename)))

(defun ruby-any-test-p (filename)
  (or (ruby-spec-p filename)
      (ruby-test-p filename)))

(defun select (fn ls)
  "Create a list from elements of list LS for which FN returns
non-nil."
  (let ((result nil))
    (dolist (item ls)
      (if (funcall fn item)
	  (setq result (cons item result))))
    (reverse result)))
(defalias 'find-all 'select)

(defun invoke-test-file (command-string category file buffer)
  (message (format "Running %s '%s'..." category file))
  (display-buffer buffer)
  (setq ruby-test-last-run file)
  (save-excursion
    (set-buffer buffer)
    (setq buffer-read-only t)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (set-auto-mode-0 'ruby-test-mode nil)
      (let ((proc (start-process "ruby-test" buffer command-string  file)))
	(set-process-sentinel proc 'ruby-test-runner-sentinel)))))

(defun rails-root (filename)
  "Returns the rails project directory for the current buffer's
filename or the filename of the optional argument."
  (interactive "f")
  (let (candidates (directory ""))
    (dolist (element (split-string filename "/"))
      (setq directory (file-name-as-directory (concat directory element "/")))
      (if (and (file-exists-p directory) (rails-root-p directory))
	  (add-to-list 'candidates directory)))
    (car candidates)))

(defun rails-root-p (directory)
  "Returns `t' if the given directory is the root of a rails
project, else `nil'."
  (let ((found t))
    (dolist (element '("config/environment.rb" "config/database.yml"))
      (setq found (and found 
		       (file-exists-p (concat 
				       (file-name-as-directory directory) 
				       element)))))
    found))

(defun ruby-test-runner-sentinel (process event)
  (save-excursion
    (set-buffer ruby-test-buffer)
    (cond
     ((string= "finished\n" event) (message "Ok"))
     ((string= "exited abnormally with code 1\n" event) (message "Failed"))
     (t (progn
	  (string-match "\\(.*\\)[^\n]" event)
	  (message "Failed: '%s'" (match-string 1 event)))))))

(defun run-spec (test-file output-buffer)
  (let ((spec "spec"))
    (invoke-test-file
     (or (ruby-test-spec-executable test-file) spec)
     spec
     test-file
     output-buffer)))

(defun run-test (test-file output-buffer)
  (invoke-test-file
   (or (ruby-test-ruby-executable) "ruby")
   "unit test"
   test-file
   output-buffer))

(defun run-test-file (file output-buffer)
  (cond
   ((ruby-spec-p file) (run-spec file output-buffer))
   ((ruby-test-p file) (run-test file output-buffer))
   (t (error "File is not a known ruby test file"))))

(defun find-ruby-test-file ()
  "Find the test file to run in number of diffeerent ways:
current buffer (if that's a test; another open buffer which is a
test; or the last run test (if there was one)."
  (let ((files))
    (if (buffer-file-name)
	(setq files (cons (buffer-file-name) files)))
    (setq files (append
		 (mapcar
		  (lambda (win-name) (buffer-file-name (window-buffer win-name)))
		  (window-list))))
    (if (boundp 'ruby-test-last-run)
	(nconc files (list ruby-test-last-run)))
    (setq ruby-test-last-run (car (select 'ruby-any-test-p (select 'identity files))))))

(defun ruby-test-run-file ()
  "Run buffer's file, first visible window file or last-run as
ruby test (or spec)."
  (interactive)
  (setq ruby-test-buffer (get-buffer-create ruby-test-buffer-name))
  (let ((test-file (find-ruby-test-file)))
    (if test-file
	(run-test-file test-file ruby-test-buffer)
      (message "No test among visible buffers or run earlier."))))

(defun ruby-test-goto-location ()
  "This command is not really meant for interactive use, but has
to be declared as such to be accessible from a key map.  It reads
the MESSAGE text property of a position, which has been placed by
the font-lock keywords."
  (interactive)
  (set-buffer ruby-test-buffer)
  (let (alist file-name line-number)
    (setq alist (get-text-property (point) 'message))
    (setq file-name (cdr (assoc 'file-name alist)))
    (setq line-number (cdr (assoc 'line-number alist)))
    (cond
     ((get-file-buffer file-name)
      (set-buffer (get-file-buffer file-name)))
     ((equal (window-buffer (selected-window)) ruby-test-buffer)
      (find-file-other-window file-name))
     (t
      (find-file file-name)))
    (goto-line line-number)))

(setq ruby-test-backtrace-key-map
      (make-sparse-keymap))
(define-key ruby-test-backtrace-key-map "\r"
  'ruby-test-goto-location)

(defvar ruby-test-mode-map nil)
(setq ruby-test-mode-map (make-sparse-keymap))
(define-key ruby-test-mode-map "\r" 'ruby-test-goto-location)
(define-key ruby-test-mode-map [mouse-2] 'ruby-test-goto-location)

(defvar ruby-test-font-lock-keywords
  (list
   '("^[[:space:]]*\\[?\\(\\([[:graph:]]*\\):\\([[:digit:]]+\\)\\):" 1 ; test/unit backtrace
     `(face font-lock-warning-face
	    message ((file-name . ,(buffer-substring-no-properties (match-beginning 2) (match-end 2)))
		     (line-number . ,(string-to-number (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
	    follow-link t
	    mouse-face highlight
	    help-echo "RET to visit location"
	    keymap ruby-test-backtrace-key-map))
   '("^[[:alnum:]_]+(.+) \\[\\(\\([[:graph:]]*\\):\\([[:digit:]]+\\)\\)\\]:" 1 ; rspec backtrace
     `(face font-lock-warning-face
	    message ((file-name . ,(buffer-substring-no-properties (match-beginning 2) (match-end 2)))
		     (line-number . ,(string-to-number (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
	    follow-link t
	    mouse-face highlight
	    help-echo "RET to visit location"
	    keymap ruby-test-backtrace-key-map))))

(defun ruby-test-mode ()
  "Major mode for running ruby tests and displaying
results. Allows to visit source file locations from backtraces."
  (interactive)
  (kill-all-local-variables)
  (use-local-map ruby-test-mode-map)
  (make-local-variable 'view-read-only)
  (set (make-local-variable 'font-lock-defaults)
       '((ruby-test-font-lock-keywords) nil nil))
  (setq major-mode 'ruby-test-mode)
  (setq mode-name "Ruby-Test")
  (run-hooks 'ruby-test-mode-hook))

(defun ruby-test-expand-executable-path (name)
  (if (file-name-absolute-p name)
      name
    (executable-find name)))

(defun ruby-test-ruby-executable ()
  "Returns the ruby binary to be used."
  (car (select 'file-readable-p 
	       (select 'identity
		       (mapcar 'ruby-test-expand-executable-path
			       ruby-test-ruby-executables)))))

(defun ruby-test-spec-executable (test-file)
  "Returns the spec executable to be used for the current buffer
test-file or the given one. If (buffer) test-file is inside of a
rails project, the project executable is returned, else the first
existing default executable. If the default executable is
relative, it is assumed to be somewhere in `PATH'."
  (interactive "b")
  (if (not (buffer-file-name (get-buffer test-file)))
      (error "%s" "Cannot find spec relative to non-file buffer"))
  (let ((executables (copy-sequence ruby-test-spec-executables)))
    (if (rails-root test-file) 
	(add-to-list 'executables (concat (rails-root test-file) 
					  "script/spec")))
    (setq executables (mapcar 'ruby-test-expand-executable-path 
			      executables))
    (let ((spec (car (select 'file-readable-p executables))))
      (message "spec found: %s" spec)
      spec)))

(provide 'ruby-test)
;;; ruby-test.el ends here
