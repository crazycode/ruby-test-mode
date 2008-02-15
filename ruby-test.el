;;; ruby-test.el --- Test runner for the ruby laguage.

;; Caspar Florian Ebeling <florian.ebeling@gmail.com>, 2007-12-06
;; This software can be redistributed. GPL v2 applies.
 
;; todo
;;   - run single test method or spec example
;;   - use small window for output
;;   - run test given in a buffer variable
;;   - set rails spec script comfortably

;;; Commentary:

;; This mode provides commands for running ruby test. The tests can be
;; both either rspec behaviours or unit tests. The output is shown in
;; separate buffer '*Ruby-Test*'. Backtrace from failures are marked
;; and can clicked to bring up the referenced source file, where the
;; cursor is moved to the named line.

;;; History:
;; - 09.02.08, Clickable backtrace added.

;;; Code:

(defvar ruby-test-buffer-name "*Ruby-Test*")

(defvar ruby-test-mode-hook)

(defvar ruby-test-last-run)

(defvar ruby-test-ruby-executable "/opt/local/bin/ruby"
  "Set the ruby binary to be used.")

(defvar ruby-test-spec-executable "/opt/local/bin/spec"
  "Set the spec exectable to be used.")

(defvar ruby-test-buffer)

(defun ruby-spec-p (filename)
  (and (stringp filename) (string-match "spec\.rb$" filename)))

(defun ruby-test-p (filename)
  (and (stringp filename) (string-match "test\.rb$" filename)))

(defun ruby-any-test-p (filename)
  (or (ruby-spec-p filename)
      (ruby-test-p filename)))

(defun odd-p (i) (= 1 (mod i 2)))

(defun even-p (i) (= 0 (mod i 2)))

(defun select (fn ls)
  "Create a list LS for which FN returns non-nil."
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

(defun ruby-test-runner-sentinel (process event)
  (save-excursion
    (set-buffer ruby-test-buffer)
    (cond
     ((string= "finished\n" event) (message "Ok"))
     ((string= "exited abnormally with code 1\n" event) (message "Failed"))
     (t (progn
	  (string-match "\\(.*\\)[^\n]" event)
	  (message "Failed: '%s'" (match-string 1 event)))))))

(defun run-spec (file buffer)
  (let ((spec "spec"))
    (invoke-test-file
     (or ruby-test-spec-executable spec)
     spec
     file
     buffer)))

(defun run-test (file buffer)
  (invoke-test-file
   (or ruby-test-ruby-executable "ruby")
   "unit test"
   file
   buffer))

(defun run-test-file (file buffer)
  (cond
   ((ruby-spec-p file) (run-spec file buffer))
   ((ruby-test-p file) (run-test file buffer))
   (t (error "File is not a known ruby test file"))))

(defun find-ruby-test-file ()
  (setq ruby-test-last-run
	(car (select 'ruby-any-test-p
		     (let ((files (nconc
				   (cons
				    (buffer-file-name)
				    (mapcar
				     (lambda (win-name) (buffer-file-name (window-buffer win-name)))
				     (window-list))))))
		       (if (boundp 'ruby-test-last-run)
			   (nconc files (list ruby-test-last-run)))
		       (mapcar 'message files)
		       (select 'identity files))))))

(defun ruby-run-buffer-file-as-test ()
  "Run buffer's file, first visible window file or last-run as ruby test (or spec)."
  (interactive)
  (setq ruby-test-buffer (get-buffer-create ruby-test-buffer-name))
  (let ((test-file (find-ruby-test-file)))
    (if test-file
	(run-test-file test-file ruby-test-buffer)
      (message "No test among visible buffers or run earlier."))))

(defvar ruby-test-backtrace-key-map
  "The keymap which is bound to marked trace frames.")

(defun ruby-test-goto-location ()
  "This command is not for interactive use.
It reads the MESSAGE text property of a position, which has
been placed by the font-lock keyswords."
  (interactive)
  (set-buffer ruby-test-buffer)
  (let (alist file-name line-number)
    (setq alist (get-text-property (point) 'message))
    (setq file-name (cdr (assoc 'file-name alist)))
    (setq line-number (cdr (assoc 'line-number alist)))
    (find-file file-name)
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
   '("^[[:space:]]*\\(\\([[:graph:]]*\\):\\([[:digit:]]+\\)\\):" 1
     `(face font-lock-warning-face
	    message ((file-name . ,(buffer-substring-no-properties (match-beginning 2) (match-end 2)))
		     (line-number . ,(string-to-number (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
	    follow-link t
	    mouse-face highlight
	    help-echo "RET to visit location"
	    keymap ruby-test-backtrace-key-map))
   '("^[[:alnum:]_]*(.+) \\[\\(\\([[:graph:]]*\\):\\([[:digit:]]+\\)\\)\\]:" 1
     `(face font-lock-warning-face
	    message ((file-name . ,(buffer-substring-no-properties (match-beginning 2) (match-end 2)))
		     (line-number . ,(string-to-number (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
	    follow-link t
	    mouse-face highlight
	    help-echo "RET to visit location"
	    keymap ruby-test-backtrace-key-map))
   ))

(defun ruby-test-mode ()
  "Major mode for running ruby tests and display results."
  (interactive)
  (kill-all-local-variables)
  (use-local-map ruby-test-mode-map)
  (make-local-variable 'view-read-only)
  (set (make-local-variable 'font-lock-defaults)
       '((ruby-test-font-lock-keywords) nil nil))
  (setq major-mode 'ruby-test-mode)
  (setq mode-name "Ruby-Test")
  (run-hooks 'ruby-test-mode-hook))

;; global, since these bindings should be visible in other windows
;; operating on the `ruby-test-last-run'.
(global-set-key (kbd "C-x t") 'ruby-run-buffer-file-as-test)
(global-set-key (kbd "C-x SPC") 'ruby-run-buffer-file-as-test)

(provide 'ruby-test)

(provide 'ruby-test)

;;; ruby-test.el ends here
