(defvar clancs-path (file-name-directory load-file-name))
(defvar clancs-buffer " *clancs output*")

(defun clancs-logger (errString)
  (with-current-buffer (get-buffer-create clancs-buffer)
    (normal-mode)
    (setq-local buffer-read-only nil)
    (buffer-disable-undo)
    (goto-char (point-max))
    (insert errString)
    (setq-local buffer-read-only t)))

(defun clancs-init ()
  (require 'deferred (concat clancs-path "emacs-deferred/deferred.el"))
  (require 'concurrent (concat clancs-path "emacs-deferred/concurrent.el"))
  (require 'ctable (concat clancs-path "emacs-ctable/ctable.el"))
  (require 'epc (concat clancs-path "emacs-epc/epc.el"))
  (require 'epcs (concat clancs-path "emacs-epc/epcs.el"))
  (require 'popup)
  ;; Initialize the client
  (defvar clancs-epc-client (epc:start-epc "python" (list (concat clancs-path "clancs.py"))))

  ;; Initialize the server
  (defvar clancs-epc-server (epcs:server-start
			       (lambda (manager)
				 (epc:define-method manager 'log 'clancs-logger "args" "Log to the *clancs* buffer")
				 )))
  ;; Communicate emacs server port to python server (and hence the client)
  (epc:call-sync clancs-epc-client 'init_client
		 (list (epcs:server-port (cdr (assoc clancs-epc-server epcs:server-processes)))))

  (message "Clancs initialized."))

(defun clancs-kill ()
  (epc:stop-epc clancs-epc-client)
  (epcs:server-stop clancs-epc-server)
  (makunbound 'clancs-epc-client)
  (makunbound 'clancs-epc-server))

;(clancs-make-item "foo" "foo(int)" "int") ; int foo(int)
(defun clancs-make-item (sym signature return-type)
  (popup-make-item sym :view signature :summary return-type))

(defun clancs-make-item-from-completion (completion)
  (let ((signature (car completion)))
    (string-match "\\([A-Za-z_]+\\).*[\t]\\(.*\\)" signature)
    (clancs-make-item (match-string 1 signature)
		      signature
		      (match-string 2 signature))))

(defun clancs-make-file-local-copy (file-or-buf)
  (if (bufferp file-or-buf)
      (with-current-buffer file-or-buf
        (let ((tempfile (make-temp-file "buffer-content-")))
          (write-region nil nil tempfile nil 'nomessage)
          tempfile))
    (file-local-copy file-or-buf)))

(defun clancs-get-project-folder ()
  (symbol-name (car (car dir-locals-class-alist))))

(defun clancs-get-compile-flags ()
  (mapcar
   (lambda (flag)
     (let ((type (substring flag 0 2))
	   (content (substring flag 2)))
       (cond ((string= type "-D")
	      (concat type content))
	     ((string= type "-I")
	      (concat type
		      (if (file-exists-p content)
			  content
			(concat (clancs-get-project-folder) content))))
	     (t
	      (concat type content)))))
   (cdr (assoc 'clancs-compile-flags
	       (cdr (assoc 'c++-mode (cdr (car dir-locals-class-alist))))))))

(defun clancs-receive-completions (completions)
  (setq-local clancs-candidates (mapcar 'clancs-make-item-from-completion completions))
  (ac-start)
  (ac-update))

(defun clancs-query-completions (&optional position buffer)
  (let ((file-name (buffer-file-name buffer))
	(clancs-compile-flags (clancs-get-compile-flags))
	(position (or position
		      (let* ((cursor-position (what-cursor-position)))
			(string-match "point=\\([0-9]+\\)" cursor-position)
			(string-to-number (match-string 1 cursor-position))))))
    (when (/= position clancs-previous-point)
      (setq-local clancs-candidates nil)
      (deferred:$
	(epc:call-deferred clancs-epc-client 'query_completions
			   (if (buffer-modified-p buffer)
			       (list file-name (- position 1) "" clancs-compile-flags
				     (clancs-make-file-local-copy (current-buffer)))
			     (list file-name (- position 1) "" clancs-compile-flags)))
	(deferred:nextc it
	  (lambda (x)
	    ;(message (concat "Found " (number-to-string (length x)) " completions"))
	    (if x
		(clancs-receive-completions x))))
	(deferred:error it
	  (lambda (err)
	    (cond
	     ((stringp err)
	      ;; application error
	      (message (concat "Application error: " err)))
	     ((eq 'epc-error (car err))
	      ;; epc error
	      (message (concat "EPC error: " (cadr err))))))))
      (setq-local clancs-previous-point position))))

(defun clancs-recompile-file (&optional position buffer)
  (let ((file-name (buffer-file-name buffer))
	(clancs-compile-flags (clancs-get-compile-flags))
	(position (or position
		      (let* ((cursor-position (what-cursor-position)))
			(string-match "point=\\([0-9]+\\)" cursor-position)
			(string-to-number (match-string 1 cursor-position))))))

    (deferred:$
      (epc:call-deferred clancs-epc-client 'recompile
			 (if (buffer-modified-p buffer)
			     (list file-name (- position 1) "" clancs-compile-flags
				   (clancs-make-file-local-copy (current-buffer)))
			   (list file-name (- position 1) "" clancs-compile-flags))))))

(defun ac-clancs-candidates ()
  ;(clancs-query-completions ac-prefix ac-point ac-buffer)
  ;; Passing prefix as empty string because ac-point seems to always point to the point (haha)
  ;; where completion was triggered in the first place.
  ;(message (concat "Called. Prefix = " (prin1-to-string ac-prefix) ", Point = " (prin1-to-string ac-point)))
  (when (not (boundp 'clancs-previous-point))
    (setq-local clancs-previous-point -1))
  (clancs-query-completions ac-point ac-buffer)
  clancs-candidates)

(defun ac-clancs-prefix ()
  (or (ac-prefix-symbol)
      (let ((c (char-before)))
	(when (or (eq ?\. c)
		  ;; ->
		  (and (eq ?> c)
		       (eq ?- (char-before (1- (point)))))
		  ;; ::
		  (and (eq ?: c)
		       (eq ?: (char-before (1- (point))))))
	  (point)))))

(ac-define-source clancs
  '((candidates . ac-clancs-candidates)
    (prefix . ac-clancs-prefix)
    (requires . 0)))

(provide 'clancs)
