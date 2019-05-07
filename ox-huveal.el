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

;; See the README on https://github.com/titaniumbones/ox-reveal
;; for examples and instructions.

;;; Code:

(require 'ox-hugo)
(require 'ox-reveal)

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
(org-export-define-derived-backend 'huveal 'reveal ; huveal < reveal  < html
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
                     ;;(template . org-huveal-template)
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
 `org-reveal-single-file' has no effect. This is because the presentations are 
intended to be viewed in the inline environment."
    (message "link type is %s" (org-element-property :type link))
  (let* ((local-image-p (and (string= "file" (org-element-property :type link))
                             (org-export-inline-image-p
                              link (plist-get info :html-inline-image-rules))))
         ;; (want-embed-image (and (plist-get info :reveal-single-file)
         ;;                        (plist-get info :html-inline-images)
         ;;                        local-image-p))
         (raw-path (org-element-property :path link))
         ;; (images-dir (org-string-nw-p (plist-get info :hugo-static-images)))
         (type (org-element-property :type link))
         (clean-path (org-reveal--file-url-to-path raw-path))
         ;; (can-embed-image (and want-embed-image
         ;;                       (file-readable-p clean-path)))
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
    ;; (if can-embed-image
    ;;     (org-reveal--format-image-data-uri link clean-path info)
    ;;   (if want-embed-image
    ;;       (error "Cannot embed image %s" raw-path)
    ;;     (if local-image-p
    ;;         (let ((path (org-hugo--attachment-rewrite-maybe
    ;;                      (if (file-name-absolute-p raw-path)
    ;;                          (expand-file-name raw-path)
    ;;                        raw-path)
    ;;                      info)))
    ;;           (org-element-put-property link :path path)
    ;;           (org-html-link link desc info)
    ;;           (replace-regexp-in-string "file://" ""
    ;;                                     (replace-regexp-in-string "<a href=\"#" "<a href=\"#/slide-"
    ;;                                                               (org-html-link link desc info))))
    ;;       (replace-regexp-in-string "<a href=\"#" "<a href=\"#/slide-"
    ;;                                 (org-html-link link desc info)))))
    ))

;;;; label css
;;;; commented out for now!
;; (defun org-huveal--css-label (in-single-file file-name style-id)
;;   "Generate an HTML label for including a CSS file, if
;;   IN-SINGLE-FILE is t, the content of FILE-NAME is embedded,
;;   otherwise, a `<link>' label is generated."

;;   (message "in org-huveal--css-label with %s" file-name)
;;   ;; (when (and file-name (not (string= file-name "")))
;;   ;;   (if in-single-file
;;   ;;       ;; Single-file
;;   ;;       (let ((local-file-name (org-reveal--file-url-to-path file-name)))
;;   ;;         (message "ox-reveal: in single file, how did that happen?")
;;   ;;         (if (file-readable-p local-file-name)
;;   ;;             (concat "<style type=\"text/css\">\n"
;;   ;;                     (org-reveal--read-file local-file-name)
;;   ;;                     "\n</style>\n")
;;   ;;           ;; But file is not readable.
;;   ;;           (error "Cannot read %s" file-name)))
;;   ;;     ;; Not in-single-file
;;   ;;     ))
;;   (concat "<link rel=\"stylesheet\" href=\"" file-name "\""
;;           (if style-id  (format " id=\"%s\"" style-id))
;;           "/>\n"))
;; ;;;; Stylesheets
;; (defun org-huveal-stylesheets (info)
;;   "Return the HTML contents for declaring reveal stylesheets
;; using custom variable `org-reveal-root'."
;;   (message "in org-huveal-style")
;;   (let* ((root-path (file-name-as-directory (plist-get info :reveal-root)))
;;          (reveal-css (concat root-path "css/reveal.css"))
;;          (theme (plist-get info :reveal-theme))
;;          (theme-css (concat root-path "css/theme/" theme ".css"))
;;          (extra-css (plist-get info :reveal-extra-css))
;;           (in-single-file (plist-get info :reveal-single-file))
;;          )
;;     (message "past initial let*")
;;     (concat
;;      ;; Default embedded style sheets
;;      "<style type=\"text/css\">
;; .underline { text-decoration: underline; }
;; </style>
;; "
;;      ;; stylesheets
;;      (mapconcat (lambda (elem) (org-huveal--css-label nil (car elem) (cdr elem)))
;;                 (append (list (cons reveal-css nil)
;;                               (cons theme-css "theme"))
;;                         (mapcar (lambda (a) (cons a nil))
;;                                 (split-string extra-css "\n")))
;;                 "\n")

;;      ;; Include CSS for highlight.js if necessary
;;      (if (org-reveal--using-highlight.js info)
;;          (format "<link rel=\"stylesheet\" href=\"%s\"/>"
;;                  (format-spec (plist-get info :reveal-highlight-css)
;;                               `((?r . ,(directory-file-name root-path))))))
;;      ;; print-pdf
;;      (if in-single-file ""
;;        (format "
;; <!-- If the query includes 'print-pdf', include the PDF print sheet -->
;; <script>
;;     if( window.location.search.match( /print-pdf/gi ) ) {
;;         var link = document.createElement( 'link' );
;;         link.rel = 'stylesheet';
;;         link.type = 'text/css';
;;         link.href = '%scss/print/pdf.css';
;;         document.getElementsByTagName( 'head' )[0].appendChild( link );
;;     }
;; </script>
;; "
;;                root-path)))))

;; ;;;;; Template
;; (defun org-huveal-template (contents info)
;;   "Return complete document string after HTML conversion.
;; contents is the transcoded contents string.
;; info is a plist holding export options."
;;   (message "in huveal-template")
;;   (concat
;;    (format "<!DOCTYPE html>\n<html%s>\n<head>\n"
;;            (if-format " lang=\"%s\"" (plist-get info :language)))
;;    "<meta charset=\"utf-8\"/>\n"
;;    (if-format "<title>%s</title>\n" (org-export-data (plist-get info :title) info))
;;    (if-format "<meta name=\"author\" content=\"%s\"/>\n" (plist-get info :author))
;;    (if-format "<meta name=\"description\" content=\"%s\"/>\n" (plist-get info :description))
;;    (if-format "<meta name=\"keywords\" content=\"%s\"/>\n" (plist-get info :keywords))
;;    (org-huveal-stylesheets info)
;;    (org-reveal-mathjax-scripts info)
;;    (org-reveal--build-pre/postamble 'head-preamble info)
;;    (org-element-normalize-string (plist-get info :html-head))
;;    (org-element-normalize-string (plist-get info :html-head-extra))
;;    "</head>
;; <body>\n"
;;    (org-reveal--build-pre/postamble 'preamble info)
;;    "<div class=\"reveal\">
;; <div class=\"slides\">\n"
;;    ;; Title slides
;;    (let ((title-slide (plist-get info :reveal-title-slide)))
;;      (when (and title-slide (not (plist-get info :reveal-subtree)))
;;        (let ((title-slide-background (plist-get info :reveal-title-slide-background))
;;              (title-slide-background-size (plist-get info :reveal-title-slide-background-size))
;;              (title-slide-background-position (plist-get info :reveal-title-slide-background-position))
;;              (title-slide-background-repeat (plist-get info :reveal-title-slide-background-repeat))
;;              (title-slide-background-transition (plist-get info :reveal-title-slide-background-transition))
;;              (title-slide-with-header (plist-get info :reveal-slide-global-header))
;;              (title-slide-with-footer (plist-get info :reveal-slide-global-footer)))
;;          (concat "<section id=\"sec-title-slide\""
;;                  (when title-slide-background
;;                    (concat " data-background=\"" title-slide-background "\""))
;;                  (when title-slide-background-size
;;                    (concat " data-background-size=\"" title-slide-background-size "\""))
;;                  (when title-slide-background-position
;;                    (concat " data-background-position=\"" title-slide-background-position "\""))
;;                  (when title-slide-background-repeat
;;                    (concat " data-background-repeat=\"" title-slide-background-repeat "\""))
;;                  (when title-slide-background-transition
;;                    (concat " data-background-transition=\"" title-slide-background-transition "\""))
;;                  ">"
;;                  (when title-slide-with-header
;;                    (let ((header (plist-get info :reveal-slide-header)))
;;                      (when header (format "<div class=\"slide-header\">%s</div>\n" header))))
;;                  (cond ((eq title-slide nil) nil)
;;                        ((stringp title-slide) (format-spec title-slide (org-html-format-spec info)))
;;                        ((eq title-slide 'auto) (org-reveal--auto-title-slide-template info)))
;;                  "\n"
;;                  (when title-slide-with-footer
;;                    (let ((footer (plist-get info :reveal-slide-footer)))
;;                      (when footer (format "<div class=\"slide-footer\">%s</div>\n" footer))))
;;                  "</section>\n"))))
;;    contents
;;    "</div>
;; </div>\n"
;;    (org-reveal--build-pre/postamble 'postamble info)
;;    (message "about to build scripts")
;;    (org-huveal-scripts info)
;;    (message "done with scripts")
;;    "</body>
;; </html>\n"))


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
         ;;(content-dir "static/")
         (section-dir (if (null (plist-get info :hugo-section))
                          (user-error "It is mandatory to set the HUGO_SECTION property")
                        (file-name-as-directory (plist-get info :hugo-section))))
         (pub-dir (let ((dir (concat base-dir content-dir section-dir)))
                    (make-directory dir :parents) ;Create the directory if it does not exist
                    dir))
         ;; (static-dir (file-truename
         ;;              (concat
         ;;               (file-name-as-directory (plist-get info :hugo-base-dir))
         ;;               "static/")))
         
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


;; need to update this to check "is lecture" or something like that
;; so I can do my lectures as I wish. Not yet integrated into workflow
(defun org-huveal--get-valid-subtree ()
  "Return the org element for a valid Hugo post subtree.
The condition to check validity is that the EXPORT_FILE_NAME
property is defined for the subtree element.

Returns nil if a valid Hugo post subtree is not found."
  (catch 'break
    (while :infinite
      (let* ((entry (org-element-at-point))
             (fname (org-element-property :EXPORT_FILE_NAME entry))
             level)
        (when fname
          (throw 'break entry))
        ;; Keep on jumping to the parent heading if the current
        ;; entry does not have an EXPORT_FILE_NAME property.
        (setq level (org-up-heading-safe))
        ;; If no more parent heading exists, break out of the loop
        ;; and return nil
        (unless level
          (throw 'break nil))))))

;; experimental filter
(defun org-huveal-final-filter (text backend info)
  "replaces org-reveal paths with org-huveal paths"
  ;; (message "in org-huveal-final-filter w/ info= \n%s" info)
  (let ((rev-root (plist-get info :reveal-root))
        (huv-root (plist-get info :huveal-root))
        (rev-extra-css (plist-get info :reveal-extra-css))
        (huv-extra-css (plist-get info :huveal-extra-css))
        (rev-extra-js (plist-get info :reveal-extra-js))
        (huv-extra-js (plist-get info :huveal-extra-js)))
    ;; (message "ox-huveal, variables:")
    ;; (message "rev-root %s" rev-root)
    ;; (message "huv-root %s" huv-root)
    ;; (message "rev-css %s" rev-extra-css)
    ;; (message "huv-css %s" huv-extra-css)
    ;; (message "rev-root %s" rev-root)
    ;; (message "rev-root %s" rev-root)
    (replace-regexp-in-string rev-root huv-root
                              (replace-regexp-in-string rev-extra-css huv-extra-css
                                                        (replace-regexp-in-string rev-extra-js huv-extra-js
                                                                                  text)) )))
  

(provide 'ox-huveal)



;;; ox-huveal.el ends here
