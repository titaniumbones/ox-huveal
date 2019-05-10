;;; ox-huveal.el --- Hugo Reveal Back-End for Org Export Engine  -*- lexical-binding: t -*-

;; Authors: Kaushal Modi <kaushal.mod@gmail.com>
;;          Matt Price <moptop99@gmail.com>
;; URL: https://github.com/titaniumbones/ox-huveal
;; Package-Requires: ((emacs "24.5"))
;; Keywords: Org, markdown, docs
;; Version: 0.2

;;; Commentary:

;; This library implements an HTML back-end compatible with the
;; Reveal.js slideshow generator (https://revealjs.com/) for Org exporter.
;; It builds on the work of ox-reveal and ox-hugo, and is heavily indebted
;; to their authors.

;; See the README on https://github.com/titaniumbones/ox-huveal
;; for examples and instructions.

;;; Code:

(require 'ox-hugo)
(require 'ox-re-reveal)

;;; User-Configurable Variables
(defcustom org-huveal-root "/vendor/reveal/"
  "The root directory of reveal.js packages in your running hugo site. It is the directory
  within which js/reveal.js is."
  :group 'org-export-huveal)

(defcustom org-huveal-extra-js
  ""
  "URL to extra JS file."
  :group 'org-export-huveal
  :type 'string)

(defcustom org-huveal-extra-css
  ""
  "URL to extra css file."
  :group 'org-export-huveal
  :type 'string)

;;; Define Back-End
(org-export-define-derived-backend 'huveal 're-reveal ; huveal < re-reveal  < html
  :menu-entry
  ;; a bunch of these should be deleted probably
  '(?V "Export to Reveal HTML in the Hugo location"
       ((?H "Subtree to file"
            (lambda (a _s v _b)
              (org-huveal-export-subtree-to-html nil a v)))
        (?h "To file"
            (lambda (a s v _b)
              (org-huveal-export-to-html a s v)))
        (?B "To file and browse" org-huveal-export-to-html-and-browse)
        (?O "Subtree to file and open"
            (lambda (a _s v _b)
              (if a
                  (org-huveal-export-subtree-to-html nil :async v)
                (org-open-file (org-huveal-export-subtree-to-html nil a v)))))
        (?o "To file and open"
            (lambda (a s v _b)
              (if a (org-huveal-export-to-html t s v)
                (org-open-file (org-huveal-export-to-html nil s v)))))
        (?A "All subtrees to files"
            (lambda (a _s v _b)
              (org-huveal-export-subtree-to-html :all-subtrees a v)))
        (?t "To temporary buffer"
            (lambda (a s v _b)
              (org-huveal-export-as-md a s v)))))
  :translate-alist '((link . org-huveal-link)
                     (template . org-huveal-template)
                     )
   :filters-alist '((:filter-final-output . org-huveal-final-filter))

  ;;                KEY                       KEYWORD                    OPTION  DEFAULT                     BEHAVIOR
  :options-alist '(
                   ;; use hugo-static-images to copy images on export
                   (:hugo-static-images "HUGO_STATIC_IMAGES" nil "images/")
                   ;; override reveal path vars with huveal paths
                   ;; HUVEAL_ROOT is set to /vendor/reveal/ by default!
                   (:huveal-root "HUVEAL_ROOT" nil org-huveal-root t)
                   (:huveal-extra-css "HUVEAL_EXTRA_CSS" nil org-huveal-extra-css newline)
                   (:huveal-extra-js "HUVEAL_EXTRA_JS" nil org-huveal-extra-js nil)
                   (:hugo-base-dir "HUGO_BASE_DIR" nil nil)
                   (:hugo-section "HUGO_SECTION" nil org-hugo-default-section-directory)
          ))


;;; Transcode Functions
;;;; links (images esp)
(defun org-huveal-link (link desc info)
  "Transcode a LINK object from Org to Reveal. The result is
  identical to ox-html expect for image links. unlike `org-huveal-link, 
 `org-re-reveal-single-file' has no effect. This is because the presentations are 
intended to be viewed in the inline environment."
    (message "link type is %s" (org-element-property :type link))
  (let* ((local-image-p (and (string= "file" (org-element-property :type link))
                             (org-export-inline-image-p
                              link (plist-get info :html-inline-image-rules))))
         (raw-path (org-element-property :path link))
         ;; (images-dir (org-string-nw-p (plist-get info :hugo-static-images)))
         (type (org-element-property :type link))
         (clean-path (org-re-reveal--file-url-to-path raw-path))
         )
    (if local-image-p
        (let ((path (concat "../.."   ;; dirty hack! -- gets around hugo's absolute URL issues
                            (org-hugo--attachment-rewrite-maybe raw-path info)))) 
          ;; (message "right before put-property")

          (org-element-put-property link :path path)
          (org-html-link link desc info)
          ;; I think "file://" replacement may be unnecessary afte rewriting via ox-hugo
          ;; keeping for now though
          (replace-regexp-in-string "file://" ""
                                    (replace-regexp-in-string "<a href=\"#" "<a href=\"#/slide-"
                                                              (org-html-link link desc info))))
      (replace-regexp-in-string "<a href=\"#" "<a href=\"#/slide-"
                                (org-html-link link desc info)))
    ))



;;;; export functions
(defun org-huveal-export-to-html-and-browse
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a reveal.js and browse HTML file."
  (interactive)
  (browse-url-of-file (expand-file-name (org-huveal-export-to-html async subtreep visible-only body-only ext-plist))))




(defun org-huveal-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a reveal.js HTML file."
  (interactive)
  (let* ((extension (concat "." org-html-extension))
         (info (org-combine-plists
                (org-export--get-export-attributes
                 'hugo subtreep visible-only)
                (org-export--get-buffer-attributes)
                (org-export-get-environment 'hugo subtreep)))
         (base-dir (if (null (plist-get info :hugo-base-dir))
                       (user-error "It is mandatory to set the HUGO_BASE_DIR property")
                     (file-name-as-directory (plist-get info :hugo-base-dir))))
         (content-dir "content/")
         (org-html-container-element "div")
         ;;(content-dir "static/")
         (section-dir (if (null (plist-get info :hugo-section))
                          (user-error "It is mandatory to set the HUGO_SECTION property")
                        (file-name-as-directory (plist-get info :hugo-section))))
         (pub-dir (let ((dir (concat base-dir content-dir section-dir)))
                    (make-directory dir :parents) ;Create the directory if it does not exist
                    dir))
         
         (outfile (org-export-output-file-name extension subtreep pub-dir))
         (clientfile (org-export-output-file-name (concat "_client" extension) subtreep)))

                                        ; export filename_client HTML file if multiplexing
    (setq client-multiplex nil)
    (setq retfile (org-export-to-file 'huveal outfile
                    async subtreep visible-only body-only ext-plist))

                                        ; export the client HTML file if client-multiplex is set true
                                        ; by previous call to org-export-to-file
    (if (eq client-multiplex t)
        (org-export-to-file 'huveal clientfile
          async subtreep visible-only body-only ext-plist))
    (cond (t retfile))))

;; get subtree -- but actually for huveal we ONLY EVER want approved subtrees.
;; so we just shortcut our way to dwim behaviour of hugo
(defun org-huveal-export-subtree-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current subtree to a Reveal.js HTML file."
  (interactive)
  (save-excursion
    (org-hugo--get-valid-subtree)
    (org-narrow-to-subtree)
    (let ((ret (org-huveal-export-to-html async t visible-only body-only (plist-put ext-plist :reveal-subtree t))))
      (widen)
      ret)))


;
;; experimental filter
(defun org-huveal-final-filter (text backend info)
  "replaces org-re-reveal paths with org-huveal paths"
  ;; (message "in org-huveal-final-filter w/ info= \n%s" info)
  (let ((rev-root (plist-get info :reveal-root))
        (huv-root (plist-get info :huveal-root))
        (rev-extra-css (plist-get info :reveal-extra-css))
        (huv-extra-css (plist-get info :huveal-extra-css))
        (rev-extra-js (plist-get info :reveal-extra-js))
        (huv-extra-js (plist-get info :huveal-extra-js)))
    (replace-regexp-in-string rev-root huv-root
                              (replace-regexp-in-string rev-extra-css huv-extra-css
                                                        (replace-regexp-in-string rev-extra-js huv-extra-js
                                                                                  text)) )))



;; add a template despite code replication
(defun org-huveal-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.
INFO is a plist holding export options."
  (concat
   (format "<!DOCTYPE html>\n<html%s>\n<head>\n"
           (org-re-reveal--if-format " lang=\"%s\"" (plist-get info :language)))
   "<meta charset=\"utf-8\"/>\n"
   (org-re-reveal--if-format "<title>%s</title>\n"
                             (org-export-data (plist-get info :title) info))
   (org-re-reveal--if-format "<meta name=\"author\" content=\"%s\"/>\n"
                             (org-element-interpret-data (plist-get info :author)))
   (org-re-reveal--if-format "<meta name=\"description\" content=\"%s\"/>\n"
                             (plist-get info :description))
   (org-re-reveal--if-format "<meta name=\"keywords\" content=\"%s\"/>\n"
                             (plist-get info :keywords))
   (org-re-reveal-stylesheets info)
   (org-re-reveal-mathjax-scripts info)
   (org-re-reveal--build-pre-postamble 'head-preamble info)
   (org-element-normalize-string (plist-get info :html-head))
   (org-element-normalize-string (plist-get info :html-head-extra))
   "</head>\n<body"
   (org-re-reveal--if-format " %s" org-re-reveal-body-attrs)
   ">\n"
   (org-re-reveal--build-pre-postamble 'preamble info)
   "<div class=\"reveal\">
<div class=\"slides\">\n"
   ;; Title slides
   (let ((title-slide (plist-get info :reveal-title-slide)))
     (when title-slide ;;(and title-slide (not (plist-get info :reveal-subtree)))
       (let ((title-slide-background (plist-get info :reveal-title-slide-background))
             (title-slide-background-size (plist-get info :reveal-title-slide-background-size))
             (title-slide-background-position (plist-get info :reveal-title-slide-background-position))
             (title-slide-background-repeat (plist-get info :reveal-title-slide-background-repeat))
             (title-slide-background-transition (plist-get info :reveal-title-slide-background-transition))
             (title-slide-state (plist-get info :reveal-title-slide-state))
             (title-slide-timing (plist-get info :reveal-title-slide-timing))
             (title-slide-with-header (plist-get info :reveal-slide-global-header))
             (title-slide-with-footer (plist-get info :reveal-slide-global-footer)))
         (concat "<section id=\"sec-title-slide\""
                 (when title-slide-background
                   (concat " data-background=\"" title-slide-background "\""))
                 (when title-slide-background-size
                   (concat " data-background-size=\"" title-slide-background-size "\""))
                 (when title-slide-background-position
                   (concat " data-background-position=\"" title-slide-background-position "\""))
                 (when title-slide-background-repeat
                   (concat " data-background-repeat=\"" title-slide-background-repeat "\""))
                 (when title-slide-background-transition
                   (concat " data-background-transition=\"" title-slide-background-transition "\""))
                 (when title-slide-state
                   (concat " data-state=\"" title-slide-state "\""))
                 (when title-slide-timing
                   (concat " data-timing=\"" title-slide-timing "\""))
                 ">\n"
                 (when title-slide-with-header
                   (let ((header (plist-get info :reveal-slide-header)))
                     (when header (format org-re-reveal-slide-header-html header))))
                 (cond ((eq title-slide nil) nil)
                       ((stringp title-slide)
                        (let* ((file-contents
                                (org-re-reveal--read-file-as-string title-slide))
                               (title-string (or file-contents title-slide)))
                          (format-spec title-string
                                       (org-re-reveal-format-spec info))))
                       ((eq title-slide 'auto) (org-re-reveal--auto-title-slide-template info)))
                 "\n"
                 (when title-slide-with-footer
                   (let ((footer (plist-get info :reveal-slide-footer)))
                     (when footer (format org-re-reveal-slide-footer-html footer))))
                 "</section>\n"))))
   contents
   "</div>
</div>\n"
   (org-re-reveal--build-pre-postamble 'postamble info)
   (org-re-reveal-scripts info)
   "</body>
</html>\n"))


(provide 'ox-huveal)



;;; ox-huveal.el ends here
