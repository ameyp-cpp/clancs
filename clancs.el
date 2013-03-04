(defvar clancs-path (file-name-directory load-file-name))
(defvar clancs-candidates (list "A" "B" "C"))

(defun clancs-init ()
  (require 'deferred (concat clancs-path "emacs-deferred/deferred.el"))
  (require 'concurrent (concat clancs-path "emacs-deferred/concurrent.el"))
  (require 'ctable (concat clancs-path "emacs-ctable/ctable.el"))
  (require 'epc (concat clancs-path "emacs-epc/epc.el"))
  (setq clancs-epc (epc:start-epc "python" (list (concat clancs-path "clancs.py"))))
  (message "Clancs initialized."))

(defun clancs-receive-completions (completions)
  (insert (prin1-to-string completions)))

;; A sample output:  (("addRef()	void" "addRef()") ("draw()	void" "draw()") ("getMaterial()	gameplay::Material *" "getMaterial()") ("getMesh() const	gameplay::Mesh *" "getMesh()") ("getMeshPartCount() const	unsigned int" "getMeshPartCount()") ("getNode() const	gameplay::Node *" "getNode()") ("getRefCount() const	unsigned int" "getRefCount()") ("getSkin() const	gameplay::MeshSkin *" "getSkin()") ("hasMaterial(unsigned int partIndex) const	bool" "hasMaterial(${1:unsigned int partIndex})") ("release()	void" "release()") ("setMaterial(const char *materialPath)	gameplay::Material *" "setMaterial(${1:const char *materialPath})") ("setMaterial(const char *vshPath, const char *fshPath)	gameplay::Material *" "setMaterial(${1:const char *vshPath}, ${2:const char *fshPath})") ("setMaterial(gameplay::Material *material)	void" "setMaterial(${1:gameplay::Material *material})") ("setNode(gameplay::Node *node)	void" "setNode(${1:gameplay::Node *node})"))
(defun clancs-query-completions ()
  (deferred:$
    (setq-local file-name (show-file-name))
    (setq-local project-folder (car (car dir-locals-class-alist)))
    (setq-local position (let* ((cursor-position (what-cursor-position)))
				     (string-match "point=\\([0-9]+\\)" cursor-position)
				     (string-to-number (match-string 1 cursor-position))))
    (setq-local compile-flags (mapcar (lambda (include-path)
					(if (file-exists-p include-path)
					    (concat "-I" include-path)
					  (progn
					    (setq-local project-folder (car (car dir-locals-class-alist)))
					    (if (sequencep project-folder)
						(setq-local project-folder (concat project-folder)))
					    (concat "-I" (symbol-name project-folder) include-path))))
				   (mapcar (lambda (include-path) (substring include-path 2))
					   (cdr (assoc 'clancs-compile-flags
						       (cdr (assoc 'c++-mode (cdr (car dir-locals-class-alist)))))))))

    (epc:call-deferred clancs-epc 'query_completions (list file-name (- position 1) "" compile-flags))
    (deferred:nextc it
      (lambda (x)
	(message (prin1-to-string x))))))

(defun ac-clancs-candidates ()
  clancs-candidates)

(ac-define-source ac-clancs
  '((candidates . ac-clancs-candidates)
    (prefix . ac-clancs-prefix)
    (requires . 0)
    (cache)))

(provide 'clancs)
