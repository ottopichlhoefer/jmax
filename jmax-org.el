;;; jmax-org.el --- jmax customization for org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  You should not need to modify paths below here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;

(require 'ox-beamer)
(require 'ox-texinfo)
(require 'ox-org)
(require 'ox-ascii)
; (require 'ox-odt)
(require 'org-inlinetask)
(require 'org-mouse)
(require 'org-contacts)

;;; Code:

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb) ; convenient switching between open org-buffers

(global-set-key "\C-e" 'end-of-line); overwrites org-mode \C-e definition

(global-set-key "\C-cL" 'org-insert-link-global)
(global-set-key "\C-co" 'org-open-at-point-global)

(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive)
(setq org-link-search-must-match-exact-headline 'query-to-create)
(setq org-image-actual-width '(600))

;; activate single letter commands at beginning of a headline.
;; User-defined Speed commands
;; ===========================

;; Built-in Speed commands
;; =======================

;; Outline Navigation
;; ------------------
;; n   (org-speed-move-safe (quote outline-next-visible-heading))
;; p   (org-speed-move-safe (quote outline-previous-visible-heading))
;; f   (org-speed-move-safe (quote org-forward-heading-same-level))
;; b   (org-speed-move-safe (quote org-backward-heading-same-level))
;; F   org-next-block
;; B   org-previous-block
;; u   (org-speed-move-safe (quote outline-up-heading))
;; j   org-goto
;; g   (org-refile t)

;; Outline Visibility
;; ------------------
;; c   org-cycle
;; C   org-shifttab
;;     org-display-outline-path
;; s   org-narrow-to-subtree
;; =   org-columns

;; Outline Structure Editing
;; -------------------------
;; U   org-shiftmetaup
;; D   org-shiftmetadown
;; r   org-metaright
;; l   org-metaleft
;; R   org-shiftmetaright
;; L   org-shiftmetaleft
;; i   (progn (forward-char 1) (call-interactively (quote org-insert-heading-respect-content)))
;; ^   org-sort
;; w   org-refile
;; a   org-archive-subtree-default-with-confirmation
;; @   org-mark-subtree
;; #   org-toggle-comment

;; Clock Commands
;; --------------
;; I   org-clock-in
;; O   org-clock-out

;; Meta Data Editing
;; -----------------
;; t   org-todo
;; ,   (org-priority)
;; 0   (org-priority 32)
;; 1   (org-priority 65)
;; 2   (org-priority 66)
;; 3   (org-priority 67)
;; :   org-set-tags-command
;; e   org-set-effort
;; E   org-inc-effort
;; W   (lambda (m) (interactive "sMinutes before warning: ") (org-entry-put (point) "APPT_WARNTIME" m))

;; Agenda Views etc
;; ----------------
;; v   org-agenda
;; /   org-sparse-tree

;; Misc
;; ----
;; o   org-open-at-point
;; ?   org-speed-command-help
;; <   (org-agenda-set-restriction-lock (quote subtree))
;; >   (org-agenda-remove-restriction-lock)

(setq org-use-speed-commands t)

;; I like to press enter to follow a link. mouse clicks also work.
(setq org-return-follows-link t)

;; renumber footnotes when new ones are inserted
(setq org-footnote-auto-adjust t)

;; default with images open
(setq org-startup-with-inline-images "inlineimages")

;; clocking setup http://www.gnu.org/software/emacs/manual/html_node/org/Clocking-work-time.html
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; store clock entries in a drawer
(setq org-clock-into-drawer t)

;; add <p for python expansion
(add-to-list 'org-structure-template-alist
             '("p" "#+BEGIN_SRC python\n?\n#+END_SRC" "<src lang=\"python\">\n?\n</src>"))

;; add <por for python expansion with raw output
(add-to-list 'org-structure-template-alist
             '("por" "#+BEGIN_SRC python :results output raw\n?\n#+END_SRC" "<src lang=\"python\">\n?\n</src>"))

;; add <pv for python expansion with value
(add-to-list 'org-structure-template-alist
             '("pv" "#+BEGIN_SRC python :results value\n?\n#+END_SRC" "<src lang=\"python\">\n?\n</src>"))

;; add <el for emacs-lisp expansion
(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC" "<src lang=\"emacs-lisp\">\n?\n</src>"))

;; add <sh for shell
(add-to-list 'org-structure-template-alist
             '("sh" "#+BEGIN_SRC sh\n?\n#+END_SRC" "<src lang=\"shell\">\n?\n</src>"))

;; do not evaluate code on export by default
(setq org-export-babel-evaluate nil)

;; enable prompt-free code running
(setq org-confirm-babel-evaluate nil
      org-confirm-elisp-link-function nil
      org-confirm-shell-link-function nil)

;; register languages in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (sh . t)
   (matlab . t)
   (sqlite . t)
   (ruby . t)
   (perl . t)
   (org . t)
   (dot . t)
   (plantuml . t)
   (R . t)))

;; no extra indentation
(setq org-src-preserve-indentation t)

;; use syntax highlighting in org-file code blocks
(setq org-src-fontify-natively t)
;; make code blocks stand out a little from my gray80 background
;; (set-face-attribute 'org-block-background nil :background "gray")

;; give us some hint we are running. We change the background color, and print
;; some messages.
;; (defvar jmax-src-block-background
;;   nil
;;   "Stores initial background color of org source blocks.")

;; (defadvice org-babel-execute-src-block (around progress nil activate)
;;   ;; store current background first time.
;;   (unless jmax-src-block-background
;;     (setq jmax-src-block-background (face-attribute 'org-block-background :background)))
;;   ;; change color of block before running.

;;   (message "Running your code block")
;;   (set-face-attribute
;;    'org-block-background nil :background "LightSteelBlue")
;;   (message "src blocks are %s" (face-attribute 'org-block-background :background))
;;   (error "r")
;;   (unwind-protect
;;         ad-do-it
;;     ;; reset the color.
;;     (set-face-attribute
;;      'org-block-background nil :background jmax-src-block-background))
;;   (message "Done with code block"))


;; language specific headers. I think this comes before the defaults
(setq org-babel-default-header-args:emacs-lisp
      (cons '(:results . "value replace")
	    (assq-delete-all :results org-babel-default-header-args)))

;; set default :results to output
(setq org-babel-default-header-args
      (cons '(:results . "output replace")
	    (assq-delete-all :results org-babel-default-header-args)))

;; set default exports to both code and results
(setq org-babel-default-header-args
      (cons '(:exports . "both")
	    (assq-delete-all :exports org-babel-default-header-args)))

;; I do not like this mode
(auto-fill-mode -1)

;; turn off auto-fill in org-mode. It is not enough to turn it off
;; everywhere.
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;; allow lists with letters in them.
(setq org-list-allow-alphabetical t)

;;;;;; capture
(define-key global-map "\C-cc" 'org-capture)

;; setup archive location in archive directory in current folder
(setq org-archive-location "archive/%s_archive::")

; I don't want to see things that are done. turn that off here.
; http://orgmode.org/manual/Global-TODO-list.html#Global-TODO-list
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-timestamp-if-done t)
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-deadlines t)
(setq org-agenda-todo-ignore-timestamp t)
(setq org-agenda-todo-ignore-with-date t)
(setq org-agenda-start-on-weekday nil) ;; start on current day

(setq org-upcoming-deadline '(:foreground "blue" :weight bold))

;; give me some warning of upcoming deadlines
(setq org-deadline-warning-days 0)

;; record time I finished a task when I change it to DONE
(setq org-log-done 'time)

;; support for links to microsoft docx,pptx,xlsx files
;; standard org-mode opens these as zip-files
;;  http://orgmode.org/manual/Adding-hyperlink-types.html
(defun org-msx-open (path)
       "Visit the msx file on PATH.

uses the dos command:
start  empty title path"
       (cond
	((string= "windows-nt" system-type)
	 (shell-command
	  (concat "start \"title\" " (shell-quote-argument path)) t))
	((string= "darwin" system-type)
	 (shell-command
	  (concat "open " (shell-quote-argument path)) t))))

(org-add-link-type
 "msx"
 'org-msx-open)

(org-add-link-type
 "attachfile"
 (lambda (link-string) (org-open-file link-string))
 ;; formatting
 (lambda (keyword desc format)
   (cond
    ((eq format 'html) (format "")); no output for html
    ((eq format 'latex)
     ;; write out the latex command
     (format "\\attachfile{%s}" keyword)))))



;; Setup the frame configuration for following links.
(setq org-link-frame-setup (quote ((gnus . org-gnus-no-new-news)
                                   (file . find-file))))

; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)


;; use this code in emacs-lisp for folding code.
(global-set-key (kbd "C-M-]") (lambda () (interactive) (org-cycle t)))
(global-set-key (kbd "M-]") (lambda ()
                              (interactive)
                              (ignore-errors
                                (end-of-defun)
                                (beginning-of-defun))
                              (org-cycle)))

;; use ido completion wherever possible
(setq org-completion-use-ido t)

(setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t)
	("" "lmodern" nil)
	("T1" "fontenc" t)
	("" "fixltx2e" nil)
;	("" "charter" nil) ;; a decent font
;	("expert" "mathdesign" nil)
	("" "graphicx" t)
	("" "longtable" nil)
	("" "float" nil)
	("" "wrapfig" nil)
	("" "rotating" nil)
	("normalem" "ulem" t)
	("" "amsmath" t)
	("" "textcomp" t)
	("" "marvosym" t)
	("" "wasysym" t)
	("" "amssymb" t)
	("" "amsmath" t)
	("version=3" "mhchem" t)
	("numbers,super,sort&compress" "natbib" nil)
	("" "natmove" nil)
	("" "url" nil)
	("" "minted" nil)
	("" "underscore" nil)
	("linktocpage,pdfstartview=FitH,colorlinks,
linkcolor=blue,anchorcolor=blue,
citecolor=blue,filecolor=blue,menucolor=blue,urlcolor=blue"
	 "hyperref" nil)
	("" "attachfile" nil)))

;; do not put in \hypersetup use your own
;; \hypersetup{pdfkeywords={%s},\n pdfsubject={%s},\n pdfcreator={%s}
(setq org-latex-with-hyperref nil)

;; this is for code syntax highlighting in export
(setq org-latex-listings 'minted)
(setq org-latex-minted-options
           '(("frame" "lines")
             ("fontsize" "\\scriptsize")
             ("linenos" "")))

;; for minted you must run latex with -shell-escape because it calls pygmentize as an external program
;; (setq org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"
;;         "bibtex %b"
;;         "makeindex %b"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"))

;; I have not had good luck with this on windows
;(setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch"))

;; avoid getting \maketitle right after begin{document}
;; you should put \maketitle if and where you want it.
(setq org-latex-title-command "")

;; customized article. better margins
(add-to-list 'org-latex-classes
	     '("cmu-article"                          ;class-name
	       "\\documentclass{article}
\\usepackage[top=1in, bottom=1.in, left=1in, right=1in]{geometry}
 [PACKAGES]
 [EXTRA]" ;;header-string
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
	     '("article-no-defaults"                          ;class-name
	       "\\documentclass{article}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]" ;;header-string
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;;;;;; org path for loadable org-files
(defvar org-load-path
  (list (file-name-as-directory
	 (expand-file-name "org" starter-kit-dir)))
  "List of directories to find org-files that `org-babel-load-file' can load code from.")

(defun org-require (feature)
  "Load a FEATURE from an org-file.
FEATURE is a symbol, and it is loaded from an org-file by the name of FEATURE.org, that is in the `org-load-path'.  The FEATURE is loaded from `org-babel-load-file'."
  (let ((org-file (concat (symbol-name feature) ".org"))
	(path))

    ;; find the org-file
    (catch 'result
      (loop for dir in org-load-path do
	    (when (file-exists-p
		   (setq path
			 (expand-file-name
			  org-file
			  dir)))
	      (throw 'result path))))
    (let ((default-directory (file-name-directory path)))
      (org-babel-load-file path))))

(org-require 'org-show)

;; https://github.com/jkitchin/org-ref
(add-to-list 'org-load-path
	     (expand-file-name "org-ref" starter-kit-dir))

(add-to-list 'load-path
	     (expand-file-name "org-ref" starter-kit-dir))

(org-require 'org-ref)
(org-require 'doi-utils)
(org-require 'pubmed)
(require 'jmax-bibtex)



(require 'ox-cmu-qualifier)
(require 'ox-cmu-ms-report)
(require 'ox-cmu-dissertation)
(require 'ox-manuscript)
(require 'ox-archive)



(defun sa-ignore-headline (contents backend info)
  "Ignore headlines with tag `ignoreheading'.
This may mess up your labels, since the exporter still creates a label for it.
Argument CONTENTS Contents of the headline.
Argument BACKEND The backend exporter.
Argument INFO Parse-tree from org-mode."
  (when (and (org-export-derived-backend-p backend 'latex 'html 'ascii)
                  (string-match "\\`.*ignoreheading.*\n"
                                (downcase contents)))
    (replace-match "" nil nil contents)))

(defun headline-nonumber (contents backend info)
  "Make headlines with nonumber."
  (when (and (org-export-derived-backend-p backend 'latex 'html 'ascii)
                  (string-match "\\`.*nonumber.*\n"
                                (downcase contents)))
    (let ((output contents))
      (setq output (replace-regexp-in-string "section{" "section*{" contents))
      (setq output (replace-regexp-in-string "\\\\hfill{}\\\\textsc{nonumber}" "" output))
      output)))

(add-to-list 'org-export-filter-headline-functions 'sa-ignore-headline)
(add-to-list 'org-export-filter-headline-functions 'headline-nonumber)

;; refresh images after running a block
(defun org-refresh-images ()
  "Refreshes images displayed inline."
  (interactive)
  (org-remove-inline-images)
  (org-display-inline-images))

(add-hook 'org-babel-after-execute-hook
	  (lambda () (org-refresh-images)))


;; Define a new toggling function for equations.
(defun org-toggle-latex-overlays (arg)
  "Toggle LaTeX fragments.  The prefix ARG is passed to `org-preview-latex-fragment'."
  (interactive "P")
  (if org-latex-fragment-image-overlays
      (org-remove-latex-fragment-image-overlays)
    (org-preview-latex-fragment arg)))

(org-defkey org-mode-map "\C-c\C-x\C-l"    'org-toggle-latex-overlays)


(defadvice org-ctrl-c-ctrl-c (around latex-overlays nil activate)
  "Ignore latex overlays in C-cC-c."
  (let ((org-latex-fragment-image-overlays nil))
    ad-do-it))


;; setup english dictionary and spell check on windows.
(cond
 ((string= system-type "windows-nt")
  (setq-default ispell-program-name
		(expand-file-name
		 "Aspell-win32/bin/aspell.exe" starter-kit-dir))
  (flyspell-mode +1)
  (add-hook 'org-mode-hook 'turn-on-flyspell 'append)))

(message "jmax-org.el loaded")

(provide 'jmax-org)

;;; jmax-org.el ends here
