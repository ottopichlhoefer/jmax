(add-to-list 'custom-theme-load-path (concat prelude-personal-dir "/themes"))
(load-theme 'my t) ; my old theme from grad school. it looks like xemacs.

(disable-theme 'zenburn)
(setq prelude-whitespace nil) ;; turns off visualizing whitespace

(setq prelude-guru nil) ; prelude-guru forces you to use shortcuts
(smartparens-mode 0)    ; turns this off. it is very slow in large org files

;;Tell the program who you are
(setq user-full-name "John Kitchin"
      user-mail-address "jkitchin@andrew.cmu.edu")

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "johnrkitchin@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(defun email-region (start end)
 (interactive "r")
 (let ((region (buffer-substring start end)))
   (mail)
   (mail-text)
   (insert region)
   (mail-to)))

(setq inhibit-startup-screen t) ;; stop showing startup screen
(tool-bar-mode 0)           ; remove the icons
(menu-bar-mode 1)           ; keep the menus
(global-visual-line-mode 1) ;; how long lines are handled.  This
                            ;; appears to wrap long lines visually,
                            ;; but not add line-returns

(global-font-lock-mode t)   ;; turn on font-lock mode everywhere

(show-paren-mode 1)         ;; highlight parentheses
(setq show-paren-style 'mixed) ;; alternative is 'expression, 'parenthesis or 'mixed

(line-number-mode 1)  ;; turn linumbers on in mode-line
;(global-linum-mode t) ;; put line numbers on left side of the screen

(setq backup-inhibited t)  ;; disable backup file creation

(fset 'yes-or-no-p 'y-or-n-p) ; answer with y/n instead of yes/no

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Disable all version control. makes startup and opening files much faster
(setq vc-handled-backends nil)

;; http://emacsredux.com/blog/2013/04/05/recently-visited-files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)
(global-set-key (kbd "<f7>") 'recentf-open-files)

;; modified from http://ergoemacs.org/emacs/emacs_hotkey_open_file_fast.html
(defvar my-filelist nil "alist for files i need to open frequently. Key is a short abbrev, Value is file path.")
(setq my-filelist
      '(
        ("master" . "~/Dropbox/org-mode/master.org")
        (".emacs.d" . "~/Dropbox/.emacs.d" )
        ("blog" . "~/Dropbox/blogofile-jkitchin.github.com/_blog/blog.org")
        ("ese" . "~/Dropbox/books/ese-book/ese.org" )
        ("pycse" . "~/Dropbox/books/pycse/pycse.org")
        ("references" . "~/Dropbox/bibliography/references.bib")
        ("notes" . "~/Dropbox/bibliography/notes.org")
        ("journal" . "~/Dropbox/org-mode/journal.org")
        ("tasks" . "~/Dropbox/org-mode/tasks.org")
        ;; more here
        ) )

(defun my-open-file-fast (openCode)
  "Prompt to open a file from a pre-defined set in `my-filelist."
  (interactive
   (list (ido-completing-read "Open:" (mapcar (lambda (x) (car x)) my-filelist))))
  (find-file (cdr (assoc openCode my-filelist))))

(global-set-key [f9] 'my-open-file-fast)

;; http://sdpconfig.wordpress.com/2011/12/21/unwrapping-paragraphs-in-emacs/
(defun unfill-paragraph ()
  "Unfill paragraph at or after point."
  (interactive "*")
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil (region-active-p))))

(defun double-space ()
  "make buffer look approximately double-spaced"
  (interactive)
  (setq line-spacing 10))

(defun single-space ()
  "make buffer single-spaced"
  (interactive)
  (setq line-spacing nil))

;http://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Expansion.html#File-Name-Expansion

(defun get-path()
  "opens dired so you can navigate to a file to insert a path to it in the current buffer"
  (interactive)
  ; store current buffer so we can change back to it
  (setq current_buffer (current-buffer))
  (setq buffer_name (buffer-file-name)) ; filename current buffer points to
  ; now call dired to navigate to the path you want
  (dired ()))

(defun insert-relative-path()
  "inserts the relative path between the original buffer and current file selected in dired"
  (interactive)
  (setq selected_file (dired-get-filename))
  (switch-to-buffer current_buffer) ; back to the original buffer
  (insert  (file-relative-name selected_file)))

(defun insert-absolute-path()
  "Inserts the absolute path to the file selected in dired"
  (interactive)
  (setq selected_file (dired-get-filename)) ; this is the file the cursor is on
  (switch-to-buffer current_buffer) ; back to the original buffer
  (insert  (expand-file-name selected_file)))

(defun insert-buffer-filename()
  "Inserts filename associated with current buffer"
  (interactive)
  (insert (buffer-file-name)))

;; These do not seem to work in prelude
(global-unset-key "\C-cg")
(global-set-key "\C-cg" 'get-path)
(global-set-key "\C-cp" 'insert-relative-path)
(global-set-key "\C-cf" 'insert-buffer-filename)

;; here I rebind Alt-c as a prefix to these functions.
(define-prefix-command 'kitchin-map)
(global-set-key (kbd "M-c") 'kitchin-map)
(define-key kitchin-map (kbd "g") 'get-path)
(define-key kitchin-map (kbd "p") 'insert-relative-path)


;; I want python to always show me the output. this advice makes that happen.
(defadvice python-shell-send-buffer (before switch-to-python-output activate)
  "Show python output in another frame after you run a script"
  (switch-to-buffer-other-frame
   (process-buffer (python-shell-get-or-create-process))))

; http://www.flannaghan.com/2013/08/29/ipython-emacs
;(setq python-shell-interpreter "ipython")
;(setq python-shell-interpreter-args "--pylab")

; http://www.emacswiki.org/emacs/PythonProgrammingInEmacs#toc5
;(setq
; python-shell-interpreter "ipython"
; python-shell-interpreter-args "--pylab"
; python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 ;python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
; python-shell-completion-setup-code
; "from IPython.core.completerlib import module_completion"
; python-shell-completion-module-string-code
; "';'.join(module_completion('''%s'''))\n"
; python-shell-completion-string-code
; "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")



(setq reftex-default-bibliography '("~/Box Sync/kitchingroup/bibliography/references.bib"))

;; see jorg-bib.el for use of these variables
(setq jorg-bib-bibliography-notes "~/Dropbox/bibliography/notes.org"
      jorg-bib-default-bibliography '("~/Dropbox/bibliography/references.bib")
      jorg-bib-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")

;;; http://www.masteringemacs.org/articles/2013/12/21/discoverel-discover-emacs-context-menus/
;(require 'makey)
;(require 'discover)
;(global-discover-mode 1)
(message "personal settings loaded!")
