(require 'ox-beamer)
(require 'org-inlinetask)
;(require 'org-mouse)
(require 'org-id)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb) ; convenient switching between open org-buffers

(global-set-key "\C-e" 'end-of-line); overwrites org-mode \C-e definition

(global-set-key "\C-cL" 'org-insert-link-global)
(global-set-key "\C-co" 'org-open-at-point-global)

;; I like to press enter to follow a link. mouse clicks also work.
(setq org-return-follows-link t)

;; automatically create ids for links
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;; I don't know if this works. in prelude C-cg searches google
(defun MY-XYZ-HOOK ()
  "My hook for `XYZ-MODE-HOOK'."
  (local-set-key (kbd "\C-cg") 'get-path)
  (local-set-key (kbd "\C-cp") 'insert-relative-path))
(add-hook 'org-mode-hook 'MY-XYZ-HOOK)

(setq org-startup-with-inline-images "inlineimages")



; do not evaluate code on export by default
(setq org-export-babel-evaluate nil)

; enable prompt-free code running
(setq org-confirm-babel-evaluate nil)
(setq org-confirm-elisp-link-function nil)
(setq org-confirm-shell-link-function nil)

; register languages in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (sh . t)
   (matlab . t)
   (org . t)
   (dot . t)))

; no extra indentation
(setq org-src-preserve-indentation t)

;; use syntax highlighting in org-file code blocks
(setq org-src-fontify-natively t)
;; make code blocks stand out a little from my gray80 background
(set-face-attribute 'org-block-background nil :background "gray")

                                        ; set default :results to output
(setq org-babel-default-header-args
      (cons '(:results . "replace output")
	    (assq-delete-all :results org-babel-default-header-args)))

                                        ; set default exports to both code and results
(setq org-babel-default-header-args
      (cons '(:exports . "both")
	    (assq-delete-all :exports org-babel-default-header-args)))



;;;;;; capture
(setq org-default-notes-file "~/Dropbox/org-mode/notes.org")
(define-key global-map "\C-cc" 'org-capture)

;; see http://orgmode.org/manual/Template-elements.html#Template-elements
;; (add-hook 'org-capture-after-finalize-hook 'org-capture-goto-last-stored)
(setq org-capture-templates
      '(
        ("b"                                                     ; key
         "Bibtex entry"                                          ; description
         plain                                                   ; type
         (file "~/Box Sync/kitchingroup/bibliography/references.bib" "Bibtex") ; target
         "%?\n  %i\n"                                            ; template
         :empty-lines 1 :kill-buffer t)                          ; properties

        ("t"   ; key 
         "Todo"; description
         entry ; type
         (file "~/Dropbox/org-mode/tasks.org")
         "* TODO %?\n  %i\n  %a")

        ("j" "Journal" entry (file+datetree "~/Dropbox/org-mode/journal.org" "Journal")
         "* %?\nEntered on %U\n  %i\n  %a")))

;;
(setq org-archive-location "archive/%s_archive::")

(setq org-agenda-files '("~/Dropbox/org-mode"))
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

(setq org-deadline-warning-days 0)

(setq org-agenda-custom-commands
      '(("w" "Weekly Review"
         (
          ;; deadlines
          (tags-todo "+DEADLINE<=\"<today>\""
                     ((org-agenda-overriding-header "Late Deadlines")
                      ;(org-agenda-tags-todo-honor-ignore-options t)
                      ;(org-agenda-todo-ignore-scheduled t)
                      ;(org-agenda-todo-ignore-deadlines t)
		      ))

          ;; scheduled  past due
          (tags-todo "+SCHEDULED<=\"<today>\""
                     ((org-agenda-overriding-header "Late Scheduled")
                      ;(org-agenda-tags-todo-honor-ignore-options t)
                      ;(org-agenda-todo-ignore-scheduled t)
                      ;(org-agenda-todo-ignore-deadlines t)
		      ))

	  ;; now the agenda
	  (agenda ""
		  ((org-agenda-overriding-header "weekly agenda")
		   (org-agenda-ndays 7)
		   (org-agenda-tags-todo-honor-ignore-options t)
		   (org-agenda-todo-ignore-scheduled nil)
		   (org-agenda-todo-ignore-deadlines nil)
		   (org-deadline-warning-days 0)
		   ))

	  ;; and last a global todo list
          (todo "TODO"))) ;; review waiting items ...other commands
			     ;; here
        ))

(setq org-log-done 'time)




;; this is for code syntax highlighting in export
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-minted-options
           '(("frame" "lines")
             ("fontsize" "\\scriptsize")
             ("linenos" "")))

; for minted you must run latex with -shell-escape because it calls pygmentize as an external program
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "bibtex %f"
        "makeindex %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;(setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch"))

;;; support for links to microsoft docx,pptx,xlsx files
;;; standard org-mode opens these as zip-files
;;  http://orgmode.org/manual/Adding-hyperlink-types.html
(org-add-link-type "msx" 'org-msx-open)

(defun org-msx-open (path)
       "Visit the msx file on PATH.

uses the dos command:
start  empty title path
"
       (shell-command
	(concat "start \"title\" " (shell-quote-argument path)) t))


;; (require 'org-annotate-file)
;; (setq org-annotate-file-storage-file "~/Dropbox/org-mode/.org-annotate-file.org")
;; (global-set-key (kbd "C-c C-a") 'org-annotate-file)

;; (defun bookmark-show-org-annotations ()
;;   "Opens the annotations window for the currently selected bookmark file."
;;   (interactive)
;;   (bookmark-bmenu-other-window)
;;   ;; (org-annotate-file)
;;   ;; or, if you're using the http://bitbucket.org/nickdaly/org-annotate-file fork,
;;   (org-annotate-file-show-annotations)
;;   )


;; (add-hook 'bookmark-bmenu-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "a") 'bookmark-show-org-annotations)))


;; my org-feeds

;; http://www.gnu.org/software/emacs/manual/html_node/emacs/HTML-Mode.html
;; http://ergoemacs.org/emacs/emacs_html.html
;; http://ergoemacs.org/emacs/elisp_process_html.html
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-HTML_002fXML.html

;; http://stackoverflow.com/questions/4448055/download-a-file-with-emacs-lisp
;; http://stackoverflow.com/questions/11912027/emacs-lisp-search-anything-in-a-nested-list

;; (libxml-parse-html-region start end)

;; (defun ph(start end) (interactive "r")
;;   (message "%s" (libxml-parse-html-region start end)))

;; It would be really sweet to convert html to plain text, keeping links, downloading imagesso they would display here.
;; (sgml

(defun textify (s &optional chars)
  "strip control and escaped html ^M &lt; &gt; etc"
  (when s
  (let ((output s))
    (mapcar (lambda (arg)               
              (setq output (replace-regexp-in-string (nth 0 arg) (nth 1 arg) output)))
            chars)
    ;; these are like line feeds
    (setq output (replace-regexp-in-string "" " " output))
    ;; these are some escaped html codes
    (setq output (replace-regexp-in-string "&lt;" "<" output))
    (setq output (replace-regexp-in-string "&gt;" ">" output))
    (setq output (replace-regexp-in-string "&quot;" "\"" output))
    (setq output (replace-regexp-in-string "&amp;" "&" output))
    (setq output (replace-regexp-in-string "&apos;" "'" output))
    (setq output (replace-regexp-in-string "<br \>" "
" output))
    (setq output (replace-regexp-in-string "<br\>" "
" output))
    (setq output (replace-regexp-in-string "<pre>\\|</pre>" "" output))
    ;; replace <a>
    (setq output (replace-regexp-in-string "<a href=\"\\([^>]+\\)\">\\(.+?\\)</a>" "[[\\1][\\2]]" output))

    ;; get rid of images. there are a couple of styles
    (setq output (replace-regexp-in-string "<img src=\\\"\\([^\\\"].*\\)[^>]/>" " [[\\1][image]] " output))
    (setq output (replace-regexp-in-string "<img src=\\\"\\([^\\\"].*\\)></img>" " [[\\1][image]] " output))

    ;; get rid of <p></p>
    (setq output (replace-regexp-in-string "<p>\\(.*\\)</p>" "\\1" output))

    (setq output (replace-regexp-in-string "<script.*</script>" "" output))

    (setq output (replace-regexp-in-string "<span [^>]*>\\([^<]\\)?</span>" "\\1" output))

    ;; superscripts
    (setq output (replace-regexp-in-string "<sup>\\([^<>].\\)?</sup>" "^{\\1}" output))
    ;; get rid of <div> and <cite>
    (setq output (replace-regexp-in-string "<div\\([^>].*\\)?>\\|</div>" "" output))
    (setq output (replace-regexp-in-string "<cite>\\|</cite>" " " output))
    output)))

(defun my-formatter (e)
  "format for rss feed to eventually do something useful"
  (format "* TODO %S
%s

%s" (textify (plist-get e :title) '(("
" ""))) ; get rid of carriage returns in the title 
(or (and (plist-get entry :guid-permalink)
         (plist-get entry :guid))
    (plist-get entry :link))
(textify (plist-get e :description))))

(setq org-feed-alist
        '(
          ;("JEE" "http://onlinelibrary.wiley.com/rss/journal/10.1002/%28ISSN%292168-9830" "~/Dropbox/org-mode/feeds.org" "J Eng. Ed.")
          ("Chem. Mat" "http://feeds.feedburner.com/acs/cmatex" "~/Dropbox/org-mode/feeds.org" "Chem. Mat." :formatter my-formatter)
          ("JACS" "http://feeds.feedburner.com/acs/jacsat" "~/Dropbox/org-mode/feeds.org" "JACS" :formatter my-formatter)
          ("JPC"  "http://feeds.feedburner.com/acs/jpccck" "~/Dropbox/org-mode/feeds.org" "J. Phys. Chem. C" :formatter my-formatter )      
          ("JCP" "http://scitation.aip.org/rss/content/aip/journal/jcp/latestarticles;jsessionid=30u8d08ebab6g.x-aip-live-03?fmt=rss"             
	   "~/Dropbox/org-mode/feeds.org" "J. Chem. Phys."  :formatter my-formatter)   
          ("ACS Cat." "http://feeds.feedburner.com/acs/accacs" "~/Dropbox/org-mode/feeds.org" "ACS Catalysis" :formatter my-formatter)
          ("IECR" "http://feeds.feedburner.com/acs/iecred" "~/Dropbox/org-mode/feeds.org" "I&ECR" :formatter my-formatter)
	  ("ES&T" "http://feeds.feedburner.com/acs/esthag" "~/Dropbox/org-mode/feeds.org" "ES&T" :formatter my-formatter)
          ("Science" "http://www.sciencemag.org/site/rss/" "~/Dropbox/org-mode/feeds.org" "Science" :formatter my-formatter)
          ("Nature" "http://feeds.nature.com/nature/rss/current" "~/Dropbox/org-mode/feeds.org" "Nature":formatter my-formatter )
          ("surf-sci" "http://rss.sciencedirect.com/publication/science/00396028" 
	   "~/Dropbox/org-mode/feeds.org" "Surface Science" :formatter my-formatter )
          ("Nat. Mat." "http://feeds.nature.com/nmat/rss/current" "~/Dropbox/org-mode/feeds.org" "Nature Materials" :formatter my-formatter)
          ("PRL" "http://feeds.aps.org/rss/recent/prl.xml"  "~/Dropbox/org-mode/feeds.org" "PRL Entries" :formatter my-formatter)
          ("PRB" "http://feeds.aps.org/rss/recent/prb.xml"  "~/Dropbox/org-mode/feeds.org" "PRB Entries" :formatter my-formatter)
       ("planet-python" "http://planet.python.org/rss20.xml" "~/Dropbox/org-mode/feeds.org" "Planet Python" :formatter my-formatter)
       ("planet-scipy" "http://planet.scipy.org/rss20.xml" "~/Dropbox/org-mode/feeds.org" "Planet Scipy" :formatter my-formatter)
       ("planet-emacs" "http://planet.emacsen.org/atom.xml" "~/Dropbox/org-mode/feeds.org" "Planet Emacs" :formatter my-formatter)
))
 
;; convenience to delete uninteresting articles
(defun delete-feed-headline ()
  (interactive)
  (org-mark-subtree)
  (delete-forward-char 1) (org-cycle))

(global-set-key (kbd "<f5>") 'delete-feed-headline)
