(require 'deferred (file-truename "emacs-deferred/deferred.el"))
(require 'concurrent (file-truename "emacs-deferred/concurrent.el"))

(pymacs-exec "from pyclang import sublimeclang")
(pymacs-exec "from pyclang import sublime")

(pymacs-exec "reload(sublimeclang)")
(pymacs-exec "reload(sublime)")

(setq pyclang-scaa (pymacs-eval "sublimeclang.SublimeClangAutoComplete()"))
(setq pymacs-forget-mutability t)

(defun pyclang-receive-completions (completions)
  (message (prin1-to-string completions)))

(defun pyclang-query-completions ()
  (let* ((view-constructor (pymacs-eval "sublime.View"))
	 (position (let* ((cursor-position (what-cursor-position)))
		     (string-match "point=\\([0-9]+\\)" cursor-position)
		     (string-to-number (match-string 1 cursor-position))))
	 (flags (list "-I/home/aparulekar/Developer/GamePlay/gameplay/src" "-I/home/aparulekar/Developer/GamePlay/external-deps/bullet/include" "-I/home/aparulekar/Developer/GamePlay/external-deps/oggvorbis/include" "-I/home/aparulekar/Developer/GamePlay/external-deps/libpng/include" "-I/home/aparulekar/Developer/GamePlay/external-deps/zlib/include" "-I/home/aparulekar/Developer/GamePlay/external-deps/lua/include" "-I/home/aparulekar/Developer/GamePlay/external-deps/glew/include"))
	 (view (funcall view-constructor (show-file-name) (- position 1) flags))
	 (query-completions (pymacs-call "getattr" pyclang-scaa "on_query_completions")))
    (funcall query-completions view "" (list (- position 1)) 'pyclang-receive-completions)))

(provide 'clancs-mode)
