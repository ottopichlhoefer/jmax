;; This is an experiment in converting an org-file to json by exporting.


(eval-when-compile (require 'cl))
(require 'ox)
(require 'ox-publish)

;;; Define Back-End

(org-export-define-backend 'json
  '((bold . org-json-bold)
;    (center-block . org-json-center-block)
;    (clock . org-json-clock)
;    (code . org-json-code)
;    (comment . (lambda (&rest args) ""))
;    (comment-block . (lambda (&rest args) ""))
;    (drawer . org-json-drawer)
;    (dynamic-block . org-json-dynamic-block)
;    (entity . org-json-entity)
;    (example-block . org-json-example-block)
;    (export-block . org-json-export-block)
;    (export-snippet . org-json-export-snippet)
;    (fixed-width . org-json-fixed-width)
;    (footnote-definition . org-json-footnote-definition)
;    (footnote-reference . org-json-footnote-reference)
    (headline . org-json-headline)
;    (horizontal-rule . org-json-horizontal-rule)
;    (inline-src-block . org-json-inline-src-block)
;    (inlinetask . org-json-inlinetask)
    (italic . org-json-italic)
;    (item . org-json-item)
;    (keyword . org-json-keyword)
;    (latex-environment . org-json-json-environment)
;    (latex-fragment . org-json-json-fragment)
;    (line-break . org-json-line-break)
    (link . org-json-link)
    (paragraph . org-json-paragraph)
;    (plain-list . org-json-plain-list)
     (plain-text . org-json-plain-text)
;    (planning . org-json-planning)
;    (property-drawer . (lambda (&rest args) ""))
 ;   (quote-block . org-json-quote-block)
;    (quote-section . org-json-quote-section)
;    (radio-target . org-json-radio-target)
    (section . org-json-section)
;    (special-block . org-json-special-block)
;    (src-block . org-json-src-block)
;    (statistics-cookie . org-json-statistics-cookie)
;    (strike-through . org-json-strike-through)
;    (subscript . org-json-subscript)
;    (superscript . org-json-superscript)
;    (table . org-json-table)
;    (table-cell . org-json-table-cell)
;    (table-row . org-json-table-row)
 ;   (target . org-json-target)
    (template . org-json-template)
;    (timestamp . org-json-timestamp)
 ;   (underline . org-json-underline)
  ;  (verbatim . org-json-verbatim)
   ; (verse-block . org-json-verse-block)
    )
  :export-block '("JSON" "JSON")
  :menu-entry
  '(?j "Export to Json"
       ((?j "As Json buffer" org-json-export-as-json)
	(?J "As Json file" org-json-export-to-json)))
  :options-alist '())

(defun org-json-template (body info)
  "this encloses the document in braces"
  (format "{%s}" body))

(defun org-json-headline (headline contents info)
  (format "{headline: %s contents:{%s}}" (org-element-property :raw-value headline) contents))

(defun org-json-section (section contents info)
  (format "{section:%s}" contents))

(defun org-json-italic (element contents info)
  (format "{italic:%s}" contents))

(defun org-json-link (link contents info)
  (format "{link:{type:%s, path:%s}" 
          (org-element-property :type link)
          (org-element-property :path link)))

(defun org-json-bold (element contents info)
  "convert bold text to a json element"
  (format "{bold:%s}" contents))

(defun org-json-plain-text (contents info)
  (format "{%s}" contents))

(defun org-json-paragraph (element contents info)
  (format "{paragraph:%s}" contents))

(defun org-json-export-as-json (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a json buffer"
  (interactive)
  (org-export-to-buffer 'json "*Org JSON Export*" async subtreep visible-only body-only ext-plist nil))

(provide 'ox-json)

;;; ox-json.el ends here

