(defvar clancs-path (file-name-directory load-file-name))

(defun clancs-init ()
  (require 'deferred (concat clancs-path "emacs-deferred/deferred.el"))
  (require 'concurrent (concat clancs-path "emacs-deferred/concurrent.el"))
  (require 'ctable (concat clancs-path "emacs-ctable/ctable.el"))
  (require 'epc (concat clancs-path "emacs-epc/epc.el"))
  (require 'popup)
  (setq clancs-epc (epc:start-epc "python" (list (concat clancs-path "clancs.py"))))
  (message "Clancs initialized."))

;(clancs-make-item "foo" "foo(int)" "int") ; int foo(int)
(defun clancs-make-item (sym signature return-type)
  (popup-make-item sym :view signature :summary return-type))

(defun clancs-make-item-from-completion (completion)
  (setq signature (car completion))
  (string-match "\\([A-Za-z_]+\\).*[\t]\\(.*\\)" signature)
  (setq sym (match-string 1 signature))
  (setq result-type (match-string 2 signature))
  (clancs-make-item sym signature result-type))

;; Sample input:  (("addRef()	void" "addRef()") ("draw()	void" "draw()") ("getMaterial()	gameplay::Material *" "getMaterial()") ("getMesh() const	gameplay::Mesh *" "getMesh()") ("getMeshPartCount() const	unsigned int" "getMeshPartCount()") ("getNode() const	gameplay::Node *" "getNode()") ("getRefCount() const	unsigned int" "getRefCount()") ("getSkin() const	gameplay::MeshSkin *" "getSkin()") ("hasMaterial(unsigned int partIndex) const	bool" "hasMaterial(${1:unsigned int partIndex})") ("release()	void" "release()") ("setMaterial(const char *materialPath)	gameplay::Material *" "setMaterial(${1:const char *materialPath})") ("setMaterial(const char *vshPath, const char *fshPath)	gameplay::Material *" "setMaterial(${1:const char *vshPath}, ${2:const char *fshPath})") ("setMaterial(gameplay::Material *material)	void" "setMaterial(${1:gameplay::Material *material})") ("setNode(gameplay::Node *node)	void" "setNode(${1:gameplay::Node *node})"))
(defun clancs-receive-completions (completions)
  (setq clancs-candidates (mapcar 'clancs-make-item-from-completion completions))
  (ac-start)
  (ac-update))

(defun clancs-query-completions (prefix &optional position buffer)
  (setq file-name (buffer-file-name buffer))
  (setq project-folder (car (car dir-locals-class-alist)))
  (unless position
    (setq position (let* ((cursor-position (what-cursor-position)))
		     (string-match "point=\\([0-9]+\\)" cursor-position)
		     (string-to-number (match-string 1 cursor-position)))))
  (when (/= position clancs-previous-point)
    (setq clancs-candidates nil)
    (setq clancs-compile-flags
	  (mapcar
	   (lambda (include-path)
	     (if (file-exists-p include-path)
		 (concat "-I" include-path)
	       (progn
		 (setq-local project-folder (car (car dir-locals-class-alist)))
		 (if (sequencep project-folder)
		     (setq-local project-folder (concat project-folder)))
		 (concat "-I" (symbol-name project-folder) include-path))))
	   (mapcar
	    (lambda (include-path) (substring include-path 2))
	    (cdr (assoc 'clancs-clancs-compile-flags
			(cdr (assoc 'c++-mode (cdr (car dir-locals-class-alist)))))))))
    (deferred:$
      (epc:call-deferred clancs-epc 'query_completions
			 (if (buffer-modified-p buffer)
			     (list file-name (- position 1) "" clancs-compile-flags
				   (buffer-substring-no-properties (point-min) (point-max)))
			   (list file-name (- position 1) "" clancs-compile-flags)))
      (deferred:nextc it
	(lambda (x)
	  (message (concat "Found " (number-to-string (length x)) " completions"))
	  (if x
	      (clancs-receive-completions x)))))
    (setq clancs-previous-point position)))

(defun ac-clancs-candidates ()
  ;(clancs-query-completions ac-prefix ac-point ac-buffer)
  ;; Passing prefix as empty string because ac-point seems to always point to the point (haha)
  ;; where completion was triggered in the first place.
  ;(message (concat "Called. Prefix = " (prin1-to-string ac-prefix) ", Point = " (prin1-to-string ac-point)))
  (when (not (boundp 'clancs-previous-point))
    (setq clancs-previous-point -1))
  (clancs-query-completions ac-prefix ac-point ac-buffer)
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
