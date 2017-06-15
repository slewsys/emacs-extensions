;;; quote-per-event.el --- quoting commands           -*- lexical-binding: t -*-
;;
;; -*- coding: utf-8 -*-
;;
;; Copyright © 2017 Andrew L. Moore

;; Author: Andrew L. Moore <alm@gnu.org>
;; Keywords: editing, languages, lisp
;; URL: https://github.com/slewsys/emacs-extensions

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; `quote-per-event-mode' is a minor mode that provides a single
;; function, `quote-per-event', for on-demand quoting and unquoting of
;; regions, things-at-point and points.
;;
;; `quote-per-event-mode' is not an `electric' mode, i.e., it's not
;; intended for rebinding self-insert characters nor modifying
;; `self-insert-command'.

;;; Installation:
;;
;;   (require 'quote-per-event)
;;
;; To enable `quote-per-event-mode' globally:
;;
;;   (quote-per-event-mode)
;;
;; To enable locally - per major mode only, .e.g, `elisp-mode' - use instead:
;;
;;   (add-hook 'elisp-mode-hook 'quote-per-event-local-mode)
;;
;; To enable globally with some major mode exceptions, e.g., `elisp-mode', use:
;;
;;   (quote-per-event-mode)
;;   (add-hook 'elisp-mode-hook
;;             (lambda ()
;;               (quote-per-event-local-mode -1)))
;;
(defgroup quote-per-event ()
  "Quotation on demand."
  :package-version '(quote-per-event . "1.0")
  :group 'editing)

(defcustom quote-per-event-quotes
  `(
   ;;; Optional-Index  Open-Char  Close-Char
    ( ,(kbd ".")      ,(kbd ".") ,(kbd "."))
    ( ,(kbd ",")      ,(kbd ",") ,(kbd ","))
    ( ,(kbd ":")      ,(kbd ":") ,(kbd ":"))
    ( ,(kbd "$")      ,(kbd "$") ,(kbd "$"))
    ( ,(kbd "%")      ,(kbd "%") ,(kbd "%"))
    ( ,(kbd "*")      ,(kbd "*") ,(kbd "*"))
    ( ,(kbd "|")      ,(kbd "|") ,(kbd "|"))
    ( ,(kbd "M-*")    ,(kbd "/") ,(kbd "/"))
    ( ,(kbd "\\")     ,(kbd "\\") ,(kbd "\\"))
    ( ,(kbd "M-[")    ,(kbd "[") ,(kbd "]"))
    ( ,(kbd "M-_")    ,(kbd "_") ,(kbd "_"))
    ( ,(kbd "M-\"")   ,(kbd "«") ,(kbd "»"))
    ( ,(kbd "C-M-?")  ,(kbd "?") ,(kbd "?"))
    ( ,(kbd "C-M-@")  ,(kbd "@") ,(kbd "@"))
    ( ,(kbd "C-M-`")  ,(kbd "`") ,(kbd "`")) ;; command substitution backquotes
    ( ,(kbd "C-M-'")  ,(kbd "‘") ,(kbd "’")) ;; typographical single quotes
    ( ,(kbd "C-M-\"") ,(kbd "“") ,(kbd "”")) ;; typographical double quotes
    )
  "Alist of quotes inserted by function `quote-per-event'.
Each element looks like (\"INDEX-CHAR\" \"OPEN-CHAR\" \"CLOSE-CHAR\"),
where INDEX-CHAR acts as an index, but is not itself inserted."
  :tag "Quote-per-Event Quotes"
  :version "25.1"
  :group 'quote-per-event
  :type '(repeat (list key-sequence key-sequence key-sequence)))

(defcustom quote-per-event-triggers
  `(
    ;;; C-c bindings.   ;;; Ctl-/Meta-bindings.
    ,(kbd "C-c '")      ,(kbd "C-'")
    ,(kbd "C-c `")      ,(kbd "C-`")
    ,(kbd "C-c \"")     ,(kbd "C-\"")
    ,(kbd "C-c \\")     ,(kbd "C-\\")
    ,(kbd "C-c :")      ,(kbd "C-:")
    ,(kbd "C-c (")      ,(kbd "C-(")
    ,(kbd "C-c <")      ,(kbd "C-<")
    ,(kbd "C-c {")      ,(kbd "C-{")
    ,(kbd "C-c $")      ,(kbd "C-$")
    ,(kbd "C-c %")      ,(kbd "C-%")
    ,(kbd "C-c *")      ,(kbd "C-*")
    ,(kbd "C-c |")      ,(kbd "C-|")
    ,(kbd "C-c [")      ,(kbd "M-[")
    ,(kbd "C-c _")      ,(kbd "M-_")
    ,(kbd "C-c C-.")    ,(kbd "C-.")
    ,(kbd "C-c C-,")    ,(kbd "C-,")
    ,(kbd "C-c M-\"")   ,(kbd "M-\"")
    ,(kbd "C-c M-*")    ,(kbd "M-*")
    ,(kbd "C-c C-M-?")  ,(kbd "C-M-?")
    ,(kbd "C-c C-M-@")  ,(kbd "C-M-@")
    ,(kbd "C-c C-M-`")  ,(kbd "C-M-`")
    ,(kbd "C-c C-M-'")  ,(kbd "C-M-'")
    ,(kbd "C-c C-M-\"") ,(kbd "C-M-\"")
    )
  "List key-sequences bound to function `quote-per-event'.
The last element of each key-sequence, with or without modifiers,
is used to index a quoting pair in `quote-per-event-alist'."
  :tag "Quote-per-Event Triggers"
  :version "25.1"
  :group 'quote-per-event
  :type '(repeat key-sequence))

(defvar quote-per-event-alist nil
 "Alist of quotes inserted by function `quote-per-event'.
Format as per variable `quote-per-event-quotes'.")

(defvar quote-per-event-closings nil
 "List of end-of-quotes characters derived from
variable `quote-per-event-alist'.")

;;; Keymap for `quote-per-event-mode'.
(defvar quote-per-event-map nil
  "Keymap for `quote-per-event-mode'.")

(defun characterize-quote-per-event-quotes (maybe-indexed-pair)
  "Convert elements of MAYBE-INDEXED-PAIR to characters."
  (mapcar
   (lambda (elmt)
     (if (arrayp elmt) (aref elmt 0)
       elmt))
   maybe-indexed-pair))

(defun init-quote-per-event-alist (init-alist-sym custom-quotes-sym)
  "Initialize variable `quote-per-event-alist'."
  (let ((custom-quotes-list
         (mapcar #'characterize-quote-per-event-quotes
                 (symbol-value custom-quotes-sym))))
    (mapc
     (lambda (pair)
       (add-to-list 'quote-per-event-alist pair))
     (append (symbol-value init-alist-sym) custom-quotes-list))))

(defun init-quote-per-event-closings (quote-per-event-alist-sym)
  "Initialize variable `quote-per-event-closings'."
  (mapc
   (lambda (close-quote)
       (add-to-list 'quote-per-event-closings close-quote))
   (mapcar (lambda (pair)
            (if (nth 2 pair) (nth 2 pair) (nth 1 pair)))
          (symbol-value quote-per-event-alist-sym))))

(defun closing-quote-p (pos)
  (memq (char-after pos) quote-per-event-closings))

(defun init-quote-per-event-map (triggers-sym)
  "Initialize keymap for `quote-per-event-mode'."
  (let ((map (make-sparse-keymap)))
    (mapc
     (lambda (binding)
       (define-key map binding 'quote-per-event))
     (symbol-value triggers-sym))
    map))

(defsubst quote-region (begin end open close count)
  "Insert COUNT pairs of quotes OPEN and CLOSE around region
delimited by BEGIN and END."
  (save-excursion
     (goto-char end)
     (dotimes (i count)
       (insert close))
     (goto-char begin)
     (dotimes (i count)
       (insert open))))

(defmacro unquote-intern (cond)
  `(progn
     (while (and ,cond (= first open) (= last close))
       (save-excursion (goto-char end) (delete-char -1)
                       (goto-char begin) (delete-char 1))
       (setq end (- end 2)
             i (1+ i)
             first (char-after begin)
             last (char-before end)))
     i))

(defmacro unquote-extern (cond)
  `(progn
     (while (and ,cond (= first open) (= last close))
       (save-excursion (goto-char end) (delete-char -1)
                       (goto-char begin) (delete-char 1))
       (setq end (1- end)
             begin (1- begin)
             i (1+ i)
             first (char-after begin)
             last (char-before end)))
     i))

(defmacro unquote-region (cond)

  ;; Look for quotes in region.
  `(let* ((first (char-after begin))
          (last (char-before end))
          (pairs-removed (unquote-intern ,cond)))
     (if (< 0 pairs-removed)
         (if at-begin (goto-char dot)
           (goto-char end))

       ;; Otherwise, look for quotes surrounding region.
       (let* ((begin (1- begin))
              (end (1+ end))
              (first (char-after begin))
              (last (char-before end))
              (pairs-removed (unquote-extern ,cond)))
         (if (< 0 pairs-removed)
             (if at-begin (goto-char (- dot i))
               (goto-char (1- end))))))))

(defun quote-per-event (&optional num)
  "Insert or delete quotes around an active region,
`thing-at-point' or `point' according to the sign of the optional
numeric prefix argument NUM. If NUM is nil or unspecified, its
value defaults to +1.

The matched pair of delimiters used for quoting/unquoting is
derived from the key sequence bound to `quote-per-event'. In
particular, it's the value of `quote-per-event-alist' indexed by
`last-command-event' of the bound key sequence, with or without
modifiers.

The absolute value of NUM, |NUM|, is interpreted according to
the target object as follows:

For an active region, a non-zero |NUM| indicates the number of
quotes to insert or delete around it, depending on whether NUM is
positive or negative, respectively. If NUM is 0, then all quotes
are deleted.

If thing-at-point (TAP) is non-nil, then a non-zero |NUM|
indicates the range of sexps forward from the current one to
quote or unquote, depending on whether NUM is positive or
negative, (respectively.)

Under the following conditions, even if TAP is non-nill,
point is quoted instead of TAP:

  * NUM is 0.
  * Point is at EOL and NUM is positive.
  * Point is at a closing quote bounding TAP and NUM is positive.

If thing-at-point is nil, then a non-zero |NUM| indicates the
number of quotes to insert or delete around point, depending upon
whether NUM is postive or negative, respecitvely. If NUM is 0,
then all quotes around point are deleted."
  (interactive "P")
  (let* ((pair (or (assq last-command-event quote-per-event-alist)
                   (assq (event-basic-type last-command-event)
                         quote-per-event-alist)))
         (open (if (nth 2 pair) (nth 1 pair) (nth 0 pair)))
         (close (if (nth 2 pair) (nth 2 pair) (nth 1 pair)))
         (dot (point))

         ;;; NB: Case `region-active-p' takes precedence over
         ;;; `thing-at-point'.
         (bounds (if (region-active-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'sexp)))
         (begin (car bounds))
         (end (cdr bounds))
         (at-begin (if begin (= dot begin) nil))
         (count (prefix-numeric-value num)))
    (cond

     ;;; Active region.
     ((region-active-p)
      (cond
       ((< 0 count)

        ;;; Insert count pairs of quotes around region
        (quote-region begin end open close count)
        (if at-begin
            (goto-char (+ dot count))
          (goto-char (+ end (1- count)))))

       ((< count 0)

        ;;; Remove |count| quotes.
        (let ((count (- count))
              (i 0))
          (unquote-region (< i count))))

       ;;; ((= count 0)
       (t

        ;;; Remove all quotes
        (let ((i 0))
          (unquote-region t)))))

     ;;; Thing-at-point and
     ;;; not at closing-quote when count > 0 and
     ;;; not at EOL unless count <= 0
     ((and bounds
           (not (and (closing-quote-p dot)
                     (< 0 count)))
           (or (not (eolp))
               (<= count 0)))
      (let ((end (save-excursion
                   (goto-char begin)
                   (forward-sexp (abs count))
                   (point))))
        (cond
         ((< 0 count)

        ;;; Enclose count things-at-point in single pair of quotes.
          (quote-region begin end open close 1)
          (goto-char (1+ dot)))

         ((< count 0)

        ;;; Remove quotes from |count| things-at-point.
          (let* ((count (- count))
                 (i 0))
            (unquote-region (< i 1))))

       ;;; ((= count 0)
         (t

        ;;; Quote point, regardless of thing-at-point.
          (insert open)
          (insert close)
          (backward-char 1)))))

     ;;; Point.
     (t
      (cond
       ((< 0 count)

        ;;; Insert |count| pairs of quotes around point.
        (dotimes (i count)
          (insert open)
          (insert close)
          (backward-char 1)))

       ((< count 0)

        ;;; Remove |count| quotes.
        (let* ((begin (1- dot))
               (end (1+ dot))
               (first (char-after begin))
               (last (char-before end))
               (count (- count))
               (i 0))
          (unquote-extern (< i count))))

       ;;; (= count 0)
       (t

        ;;; Remove all quotes.
        (let* ((begin (1- dot))
               (end (1+ dot))
               (first (char-after begin))
               (last (char-before end))
               (i 0))
          (unquote-extern t))))))))

;;;###autoload
(define-minor-mode quote-per-event-mode
  "Toggles variable `quote-per-event-mode'.

 Interactively with no argument, this command toggles the mode.
With a positive prefix argument, enables the mode. With any other
prefix argument, disables it.

From Lisp (i.e., non-interactively), argument omitted or nil
enables the mode, `toggle' toggles the state.

`quote-per-event-mode' is a minor mode that provides a single
function, `quote-per-event', for on-demand quoting and unquoting of
regions, things-at-point and points."

  ;;; No initial value.
  :init-value nil

  ;;; No mode line indicator.
  :lighter nil

  ;;; Minor mode keymap
  :keymap (init-quote-per-event-map 'quote-per-event-triggers)

  ;;; Gobal variable `quote-per-event-mode'.
  :global t

  ;;; Customization group
  :group 'quote-per-event

  ;;; Initialize quotes lists.
  (when quote-per-event-mode
    (init-quote-per-event-alist
     'insert-pair-alist
     'quote-per-event-quotes)
    (init-quote-per-event-closings
     'quote-per-event-alist)))

;;; `quote-per-event-local-mode' requires Emacs 25.1

;;;###autoload
(define-minor-mode quote-per-event-local-mode
  "Toggle `quote-per-event-mode' only in this buffer."
  :variable (buffer-local-value 'quote-per-event-mode (current-buffer))
  (cond
   ((eq quote-per-event-mode (default-value 'quote-per-event-mode))
    (kill-local-variable 'quote-per-event-mode))
   ((not (default-value 'quote-per-event-mode))

    ;;; Enable locally.
    (quote-per-event-mode)

    ;;; Disable globally elsewhere.
    (setq-default quote-per-event-mode nil)
    )))

(provide 'quote-per-event)

;; Local Variables:
;; byte-compile-dynamic: t
;; End:

;;; quote-per-event.el ends here
