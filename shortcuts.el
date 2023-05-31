;;; shortcuts.el --- Shortcuts -*- lexical-binding:t -*-
;;; Commentary:
;;;
;;; Allows setting Emacs commands dynamically to browse URLs or
;;; files, and optionally prompting the user for parameters to
;;; fill in those URLs/file paths.
;;;
;;; Code:

(defcustom link-string-replacement-separator "🔑"
  "Separator character for static strings in customized links.
Links can contain subparts with format <SEPARATOR>PROMPT<SEPARATOR>.
PROMPT is the message shown to the user upon entering the replacement
string."
  :type 'character :group 'shortcuts)

(defcustom link-lisp-replacement-separator "🤖"
  "Separator character for dynamic LISP forms in customized links.
Links can contain subparts with format <SEPARATOR>PROMPT<SEPARATOR>.
PROMPT is the message shown to the user upon entering the LISP form used to
determine the replacement string(s)."
  :type 'character :group 'shortcuts)

(defun parse-link (link)
  "Return a parsed LINK for use in `load-shortcuts' function.

The list contains the pair (STATIC-PART QUERYABLE-PART) or (STATIC-PART)
when there is no QUERYABLE-PART (such as leftover text at the end of the
string).

STATIC-PART is an empty string when the link begins with a QUERYABLE-PART.

The list is sorted starting from the last identified text in the string.

QUERYABLE-PART contains (TYPE PROMPT) indicating the TYPE of the requested
user input and the PROMPT to display to read this input."
  (named-let parse-link ((start-pos 0)
                         (result))
    (if (>= start-pos (length link)) result
      (if-let ((start-marker-pos
                (string-match
                 (rx (or (literal link-lisp-replacement-separator)
                         (literal link-string-replacement-separator)))
                 link start-pos)))
          (if-let ((end-marker-pos (string-match (match-string 0 link)
                                                 link (1+ start-marker-pos))))
              (parse-link
               (1+ end-marker-pos)
               (cons (list (substring link start-pos
                                      start-marker-pos)
                           (list (if (string-equal
                                      (match-string 0 link)
                                      link-string-replacement-separator)
                                     "s" "X")
                                 (substring link (1+ start-marker-pos)
                                            end-marker-pos)))
                     result))
            result)
        (cons (list (substring link start-pos))
              result)
        ))))

(defun into-interactive-args (parsed-link)
  "Form the interactive arguments needed for the PARSED-LINK.
See `parse-link' for the format of PARSED-LINK."
  (string-join (mapcar (lambda (interactive-args)
                         (let ((code (car interactive-args)))
                           (format
                            "%s%s: "
                            code
                            (cadr interactive-args))))
                       (reverse (seq-keep 'cadr parsed-link)))
               "\n"))

(defun enumerate-selections (options)
  "Return all possible selections for the given OPTIONS in reverse order.
OPTIONS is a list of lists containing the possible selections for the
nth element in a selection."
  (let* ((cur-options (mapcar (lambda (n) (mapcar #'identity n)) options))
         (cur-selection '())
         (selections '()))
    (while (not (and (seq-empty-p (car cur-options))
                     (= 0 (length cur-selection))))
      (if (= (length options) (length cur-selection))
          (setq selections (cons cur-selection selections)
                cur-selection (cdr cur-selection))
        ;; places remain to make a full selection
        (let ((options-at-pos (nth (length cur-selection) cur-options)))
          (if-let ((option (car options-at-pos)))
              (progn
                (setf (seq-elt cur-options (length cur-selection))
                      (cdr options-at-pos))
                (setq cur-selection (cons option cur-selection)))
            (when (> (length cur-selection) 0)
              (setf (seq-elt cur-options (length cur-selection))
                    (nth (length cur-selection) options))
              (setq cur-selection (cdr cur-selection)))))))
    selections))

(defun build-link (parsed-link args interactive-args-range)
  "Build PARSED-LINK given ARGS and INTERACTIVE-ARGS-RANGE.

Build PARSED-LINK against all combinations of ARGS in the selected
INTERACTIVE-ARGS-RANGE.

See `parse-link' for the format of PARSED-LINK."
  (let ((selections
         ;; take the subset of ARGS as defined in INTERACTIVE-ARGS-RANGE
         ;; to produce all combinations of args.
         (enumerate-selections (seq-drop
                                (seq-take args (cadr interactive-args-range))
                                (car interactive-args-range))))
        ;; determine if the end of PARSED-LINK is queryable or static
        (tail-fragment (when (length= (car parsed-link) 1) (caar parsed-link))))
    (if selections
        (mapcar (lambda (selection)
                  (let ((joined-link
                         (string-join
                          (reverse          ; needed for joining the strings
                           ;; there are as many selection elements as there are
                           ;; PARSED-LINKS, except when it ends with (STATIC)
                           (cl-mapcan (lambda (v1 v2)
                                        (list (format "%s" v1)
                                              (format "%s" v2)))
                                      selection
                                      (mapcar #'car (if tail-fragment
                                                        (cdr parsed-link)
                                                      parsed-link)))))))
                    ;; add the remaining (STATIC) text if it exists
                    (string-join (list joined-link tail-fragment))))
                selections)
      (car parsed-link))))

(defun load-shortcuts (shortcut-items)
  "Setup shortcuts defined in SHORTCUT-ITEMS."
  (interactive)
  (mapc
   (lambda (v)
     (let* ((name (intern (car v)))
            (function (caadr v))
            (parsed-links (mapcar 'parse-link (cadadr v)))
            (interactive-args (string-trim
                               (string-join (mapcar 'into-interactive-args
                                                    parsed-links)
                                            "\n")))
            (interactive-args-ranges
             (cl-reduce (lambda (ranges parsed-link)
                          (let ((range-len (- (length parsed-link)
                                              (if (= 1 (length (car parsed-link)))
                                                  1 0))))
                            (cons (list (or (cadar ranges) 0)
                                        (+ (or (cadar ranges) 0) range-len))
                                  ranges)))
                        parsed-links :initial-value nil)))
       (fset name `(lambda (p &rest args)
                     (interactive ,(string-join (list "p\n" interactive-args)))
                     (let* ((populated-links
                             (cl-mapcan (lambda (parsed-link range)
                                          (build-link parsed-link
                                                      (mapcar (lambda (arg)
                                                                (if (listp arg) arg
                                                                  (list arg)))
                                                              args)
                                                      range))
                                        (quote ,parsed-links)
                                        (quote ,interactive-args-ranges))))
                       (if (= p 4)
                           (let ((links (string-join populated-links "\n")))
                             (kill-new links)
                             (message "%s\n%s" "Copied!" links))
                         (mapc (quote ,function)
                               populated-links)))))))
   shortcut-items))


(defcustom shortcuts-list nil
  "List of shortcuts."
  :type '(repeat
          (list
           (string
            :tag "Command Name")
           (radio (list :tag "URL shortcuts"
                        (radio (function-item browse-url)
                               (function-item browse-chrome-url)
                               (function-item browse-url-emacs))
                        (repeat :tag "List of URLs"
                                (string :tag "URL")))
                  (list :tag "File shortcuts"
                        (radio (function-item find-file)
                               (function-item find-file-literally))
                        (repeat :tag "List of filenames"
                                (string :tag "Path")))
                  )))
  :group 'shortcuts
  :set (lambda (s v)
         (set-default-toplevel-value s v)
         (load-shortcuts v)))

(load-shortcuts shortcuts-list)

(provide 'shortcuts)

;;; End:
;;; shortcuts.el ends here
