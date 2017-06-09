;;; align-numbers.el --- align numbers                -*- lexical-binding: t -*-
;;
;; -*- coding: utf-8 -*-
;;
;; Copyright © 2016 Andrew L. Moore

;; Author: Andrew L. Moore <alm@gnu.org>
;; Keywords: convenience, table, formatting
;; URL: https://github.com/slewsys/emacs-extensions

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; `align-numbers' aligns tables of numbers - in integer, decimal
;; and scientific notation - on their radix (decimal) point. For example:
;;
;;     2512 -27254 +87.08 2.1438E-100
;;     +251.7 1.5889E11 416 3127.0
;;     180.93 21554 +.15535 1.5660E2
;;     .2849 14952 1.723E2 16526.
;;     27629 -297.46 24 2.5126E+5
;;     -2577.1 9919 2261.5 +3.5E05
;;
;; becomes
;;
;;      2512      -27254          +87.08        2.1438E-100
;;      +251.7         1.5889E11  416        3127.0
;;       180.93    21554            +.15535     1.5660E2
;;          .2849  14952            1.723E2 16526.
;;     27629        -297.46        24           2.5126E+5
;;     -2577.1      9919         2261.5        +3.5E05
;;
(eval-when-compile
  (require 'align))

(defgroup align-numbers nil
  "Align numbers on (implicit) radix point. See also `align-regexp'"
  :package-version '(align-numbers . "1.0")
  :group 'align)

;;;###autoload(put 'align-numbers-radix-point 'safe-local-variable 'stringp)
(defcustom align-numbers-radix-point "."
  "Radix point character for separating a number into integer and
fractional parts. Must be distinct from both
`align-numbers-radix-point-alt' and
`align-numbers-digits-separator'."
  :tag "Align-Numbers Radix Point"
  :version "25.1"
  :group 'align-numbers
  :type 'string
  :options '("." "·" ","))

;;;###autoload(put 'align-numbers-radix-point-alt 'safe-local-variable 'stringp)
(defcustom align-numbers-radix-point-alt "·"
  "Alternative radix point character for separating a number into
integer and fractional parts. Must be distinct from both
`align-numbers-radix-point' and
`align-numbers-digits-separator'."
  :tag "Align-Numbers Alternative Radix Point"
  :version "25.1"
  :group 'align-numbers
  :type 'string
  :options '("." "·" ","))

;;;###autoload(put 'align-numbers-digits-separator 'safe-local-variable 'stringp)
(defcustom align-numbers-digits-separator ""
  "Digits separator character for grouping digits in a number by
magnitude (e.g., comma in decimal representation of one thousand:
1,000). Null by default since common digits separators may occur
between numbers as well. Aligning numbers separated by the same
character as the digits separator is not supported. Digits
separator must be distinct from both `align-numbers-radix-point'
and `align-numbers-radix-point-alt'."
  :tag  "Align-Numbers Digits Separator"
  :version "25.1"
  :group 'align-numbers
  :type 'string
  :options '("." "·" ","))

(defsubst align-numbers-integer-regexp (&optional digits-separator)
  "Regular expression for integers."
  (concat "[-+]?[[:digit:]][" digits-separator "[:digit:]]*"))

(defsubst align-numbers-decimal-regexp (radix-point &optional digits-separator)
  "Regular expression for numbers in decimal notation."
  (concat "[-+]?\\([[:digit:]]["
          digits-separator
          "[:digit:]]*\\"
          radix-point
          "[[:digit:]]*\\|\\"
          radix-point
          "[[:digit:]]+\\)"))

(defsubst align-numbers-sci-notation-regexp (radix-point)
  "Regular expression for numbers in scientific notation."
  (concat "[-+]?[1-9]\\" radix-point "[[:digit:]]+[eE][-+]?[[:digit:]]+"))

(defsubst align-numbers-substitute-regexp (regexp subst)
  "Replace all occurences of pattern REGEX with substitution
SUBST in current buffer."
  (goto-char (point-min))
  (while (re-search-forward regexp nil t 1)
    (replace-match subst t)))

(defsubst align-numbers-align-on-regexp (begin end radix not-radix int-re)
  "Align numbers on fractional separator in region between BEGIN
and END."
  (let* ((regexp
          (concat "\\([[:space:]]*\\(" int-re
                  "\\|[+-]?[[:digit:]]*\\)\\)[" radix not-radix "]"))
         (rule (list
                (list
                 nil
                 (cons 'regexp regexp)
                 (cons 'group 1)
                 (cons 'justify t)
                 (cons 'spacing 1)
                 (cons 'repeat t)))))
    (align-region begin end 'entire rule nil nil)))

;;;###autoload
(defun align-numbers (begin end)
  "Align numbers in region between BEGIN and END. Numbers are
aligned on their (possibly implicit) fractional separator, as
defined by `align-numbers-radix-point'. If digits are grouped by
magnitude, the separator should be as defined by
`align-numbers-digits-separator'."
  (interactive "*r")
  (let* ((radix align-numbers-radix-point)
         (radix-alt align-numbers-radix-point-alt)
         (sep align-numbers-digits-separator)
         (re-int (align-numbers-integer-regexp sep))
         (re-dec (align-numbers-decimal-regexp radix sep))
         (re-sci (align-numbers-sci-notation-regexp radix))
         (buf (current-buffer))
         (aligned-region

          ;;; Align region in temp buffer and return aligned buffer string.
          (with-temp-buffer
            (lisp-mode)
            (insert-buffer-substring buf begin end)

            ;;; Append RADIX-ALT to non-decimal numbers.
            (if (equal sep "")
                (align-numbers-substitute-regexp
                 (concat "\\_<\\(" re-int "\\)\\_>")
                 (concat "\\1" radix-alt))
              (align-numbers-substitute-regexp
               (concat "\\_<\\(" re-int "\\)\\_>\\([^" sep "]\\)")
               (concat "\\1" radix-alt "\\2")))

            ;;; Append SPACE to decimal numbers (works around
            ;;; `align-region' adding space after integers but not
            ;;; decimals).
            (align-numbers-substitute-regexp
             (concat "\\_<\\(" re-sci "\\|" re-dec "\\)\\_>")
             "\\1 ")

            ;;; Align on both RADIX and RADIX-ALT.
            (align-numbers-align-on-regexp
             (point-min) (point-max) radix radix-alt re-int)

            ;;; Replace RADIX-ALT with SPACE.
            (align-numbers-substitute-regexp
             (concat "\\_<\\(" re-int "\\)" radix-alt)
             "\\1 ")

            ;;; Remove extraneous SPACE between number and non-number.
            (align-numbers-substitute-regexp
             (concat "\\([[:digit:]]\\)[[:space:]]\\([[:space:]]*\\)"
                     "\\([^-+" radix "[:digit:][:space:]]\\)")
             "\\1\\2\\3")

            ;;; Remove any trailing SPACEs.
            (align-numbers-substitute-regexp "[[:space:]]+$" "")

            (buffer-string))))

    ;;; Delete unaligned region.
    (delete-region begin end)

    ;;; Insert aligned region.
    (insert aligned-region)))

(provide 'align-numbers)

;; Local Variables:
;; byte-compile-dynamic: t
;; End:

;;; align-numbers.el ends here
