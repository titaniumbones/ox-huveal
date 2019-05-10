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
  

(provide 'ox-huveal)



;;; ox-huveal.el ends here
