(require 'deferred (file-truename "emacs-deferred/deferred.el"))
(require 'concurrent (file-truename "emacs-deferred/concurrent.el"))
(require 'async "emacs-async/async.el")

(async-start
   ;; What to do in the child process
   (lambda ()
     (message "This is a test")
     (sleep-for 3)
     222)

   ;; What to do when it finishes
   (lambda (result)
     (message "Async process done, result should be 222: %s" result)))

(defun pyclang-init ()
  (deferred:$
    (deferred:next
      (lambda ()
	(pymacs-exec "from pyclang import sublimeclang")))
    (deferred:nextc it
      (lambda ()
	(pymacs-exec "from pyclang import sublime")))
    (deferred:nextc it
      (lambda ()
	(setq pymacs-forget-mutability t)
	(setq pyclang-scaa (pymacs-eval "sublimeclang.SublimeClangAutoComplete()"))))))

(defun pyclang-receive-completions (completions)
  (insert (prin1-to-string completions)))

(defun pyclang-query-completions ()
  (let* ((view-constructor (pymacs-eval "sublime.View"))
	 (position (let* ((cursor-position (what-cursor-position)))
		     (string-match "point=\\([0-9]+\\)" cursor-position)
		     (string-to-number (match-string 1 cursor-position))))
	 (flags (list "-I/home/aparulekar/Developer/GamePlay/gameplay/src" "-I/home/aparulekar/Developer/GamePlay/external-deps/bullet/include" "-I/home/aparulekar/Developer/GamePlay/external-deps/oggvorbis/include" "-I/home/aparulekar/Developer/GamePlay/external-deps/libpng/include" "-I/home/aparulekar/Developer/GamePlay/external-deps/zlib/include" "-I/home/aparulekar/Developer/GamePlay/external-deps/lua/include" "-I/home/aparulekar/Developer/GamePlay/external-deps/glew/include"))
	 (view (funcall view-constructor (show-file-name) (- position 1) flags))
	 (query-completions (pymacs-call "getattr" pyclang-scaa "on_query_completions")))
    (funcall query-completions view "" (list (- position 1)))))

(provide 'clancs-mode)
