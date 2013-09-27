(add-to-list 'custom-theme-load-path (concat prelude-personal-dir "/themes"))
(load-theme 'my t)
(disable-theme 'zenburn)
(setq prelude-whitespace nil)

(setq prelude-guru nil)
(smartparens-mode 0)

;;Tell the program who you are
(setq user-full-name "John Kitchin")
(setq user-mail-address "johnrkitchin@gmail.com")

(tool-bar-mode 0) ; remove the icons
(menu-bar-mode 1) ; keep the menus
(global-visual-line-mode 1) ;; how long lines are handled.
                            ;; This appears to wrap long lines

(global-font-lock-mode t)   ;; turn on font-lock mode everywhere

(show-paren-mode 1)         ;; highlight parentheses
(setq show-paren-style 'mixed) ;; alternative is 'expression, 'parenthesis or 'mixed

(line-number-mode 1)  ;; turn linumbers on in mode-line
;(global-linum-mode t) ;; put line numbers on left side of the screen

(setq backup-inhibited t)  ;; disable backup file creation

(setq inhibit-startup-screen t) ;; stop showing startup screen

(fset 'yes-or-no-p 'y-or-n-p) ; answer with y/n instead of yes/no

(global-set-key (kbd "C-x C-b") 'ibuffer)
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

;; Disable all version control. makes startup and opening files much faster
(setq vc-handled-backends nil)

;; http://emacsredux.com/blog/2013/04/05/recently-visited-files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)


; adapted from http://ergoemacs.org/emacs/emacs_hotkey_open_file_fast.html
(defun j-open-file-fast (openCode)
  "Prompt to open a file from a pre-defined set."
  (interactive "sOpen file: [m]master.org [b]blog [j]jmax.org [p]pycse.org [e]ese-book [r]eferences.bib [n]otes.org: ")
  (let (file )
    (setq file
          (cond
           ((string= openCode "m") "~/Dropbox/org-mode/master.org" )
           ((string= openCode "j") "~/Dropbox/.emacs.d/jmax.org" )
           ((string= openCode "b") "~/Dropbox/blogofile-jkitchin.github.com/_blog")
           ((string= openCode "p") "~/Dropbox/pycse/pycse.org" )
           ((string= openCode "e") "~/Dropbox/ese-book/ese.org" )
           ((string= openCode "r") "~/Dropbox/bibliography/references.bib")
           ((string= openCode "n") "~/Dropbox/bibliography/notes.org")
           (t (error "You typed %s, it doesn't associate with a file." openCode ))
           )
          )
    (find-file file ) ) )

(global-set-key [f9] 'j-open-file-fast)

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
  (setq buffer_name (buffer-file-name))
  ; now call dired to navigate to the path you want
  (dired ())
)

(defun insert-relative-path()
  "inserts the relative path between the original buffer and current file selected in dired"
  (interactive)
  (setq selected_file (dired-get-filename))
  (switch-to-buffer current_buffer) ; back to the original buffer
  (insert  (file-relative-name selected_file));inserts relative path
)

(defun insert-absolute-path()
  "Inserts the absolute path to the file selected in dired"
  (interactive)
  (setq selected_file (dired-get-filename)) ; this is the file the cursor is on
  (switch-to-buffer current_buffer) ; back to the original buffer
  (insert  (expand-file-name selected_file));inserts absolute path
)

(defun insert-buffer-filename()
  "Inserts filename associated with current buffer"
  (interactive)
  (insert (buffer-file-name))
)

(global-unset-key "\C-cg")
(global-set-key "\C-cg" 'get-path)
(global-set-key "\C-cp" 'insert-relative-path)
(global-set-key "\C-cf" 'insert-buffer-filename)

(defun MY-XYZ-HOOK ()
  "My hook for `XYZ-MODE-HOOK'."
  (local-set-key (kbd "\C-cg") 'get-path)
  (local-set-key (kbd "\C-cp") 'insert-relative-path)
  )
(add-hook 'org-mode-hook 'MY-XYZ-HOOK)

(define-prefix-command 'kitchin-map)
(global-set-key (kbd "M-c") 'kitchin-map)
(define-key kitchin-map (kbd "g") 'get-path)
(define-key kitchin-map (kbd "p") 'insert-relative-path)


;(load-file (concat prelude-personal-dir "/personal-org.el"))
;(load-file (concat prelude-personal-dir "/blogofile.el"))

(message "personal settings loaded!")
