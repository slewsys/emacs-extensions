;;; unicode-keymap-extensions.el --- Unicode keymap   -*- lexical-binding: t -*-
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
;; `unicode-keymap-extensions-mode' is a minor mode that provides
;; extended Unicode input bindings.
;;
;;; Installation:
;;
;;   (require 'unicode-keymap-extensions)
;;
;; To enable `unicode-keymap-extensions-mode' globally:
;;
;;   (unicode-keymap-extensions-mode)
;;
;; To enable locally - per major mode only, .e.g, `elisp-mode' - use instead:
;;
;;   (add-hook 'elisp-mode-hook #'unicode-keymap-extensions-local-mode)
;;
;; To enable globally with some major mode exceptions, e.g., `elisp-mode', use:
;;
;;   (unicode-keymap-extensions-mode)
;;   (add-hook 'elisp-mode-hook
;;             (lambda ()
;;               (unicode-keymap-extensions-local-mode -1)))
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unicode Latin-1 Supplement and Latin Extended-A keymap.
;;
(defgroup unicode-keymap-extensions ()
  "Extended Unicode input bindings."
  :package-version '(unicode-keymap-extensions . "1.1")
  :group 'editing)

(defcustom unicode-keymap-extensions-bindings

   ;;; KEY SEQUENCE         UNICODE CHAR
  `((,(kbd "C-x 8 : :") . "：") ;;; FULL-WIDTH COLON
    (,(kbd "C-x 8 : /") . "／") ;;; FULL-WIDTH SOLIDUS
    (,(kbd "C-x 8 / l") . "ł") ;;; SMALL L WITH STROKE
    (,(kbd "C-x 8 / L") . "Ł") ;;; CAPITAL L WITH STROKE
    (,(kbd "C-x 8 v c") . "č") ;;; SMALL C WITH CARON
    (,(kbd "C-x 8 v C") . "Č") ;;; CAPITAL C WITH CARON
    (,(kbd "C-x 8 v e") . "ě") ;;; SMALL E WITH CARON
    (,(kbd "C-x 8 v E") . "Ě") ;;; CAPITAL E WITH CARON
    (,(kbd "C-x 8 v r") . "ř") ;;; SMALL R WITH CARON
    (,(kbd "C-x 8 v R") . "Ř") ;;; CAPITAL R WITH CARON
    (,(kbd "C-x 8 v s") . "š") ;;; SMALL S WITH CARON
    (,(kbd "C-x 8 v S") . "Š") ;;; CAPITAL S WITH CARON
    (,(kbd "C-x 8 v z") . "ž") ;;; SMALL Z WITH CARON
    (,(kbd "C-x 8 v Z") . "Ž") ;;; CAPITAL Z WITH CARON
    (,(kbd "C-x 8 . z") . "ż") ;;; SMALL Z WITH DOT ABOVE
    (,(kbd "C-x 8 . Z") . "Ż") ;;; CAPITAL Z WITH DOT ABOVE
    (,(kbd "C-x 8 , e") . "ę") ;;; SMALL E WITH OGONEK
    (,(kbd "C-x 8 , E") . "Ę") ;;; CAPITAL E WITH OGONEK
    (,(kbd "C-x 8 e a") . "æ") ;;; SMALL LATIN AE
    (,(kbd "C-x 8 E A") . "Æ") ;;; CAPITAL LATIN AE
    (,(kbd "C-x 8 e o") . "œ") ;;; SMALL LATIN OE
    (,(kbd "C-x 8 E O") . "Œ") ;;; CAPITAL LATIN OE

    (,(kbd "C-x 8 g a") . "α") ;;; GREEK SMALL LETTER ALPHA
    (,(kbd "C-x 8 g b") . "β") ;;; GREEK SMALL LETTER BETA
    (,(kbd "C-x 8 g g") . "γ") ;;; GREEK SMALL LETTER GAMMA
    (,(kbd "C-x 8 g d") . "δ") ;;; GREEK SMALL LETTER DELTA
    (,(kbd "C-x 8 g e") . "ε") ;;; GREEK SMALL LETTER EPSILON
    (,(kbd "C-x 8 g z") . "ζ") ;;; GREEK SMALL LETTER ZETA
    (,(kbd "C-x 8 g h") . "η") ;;; GREEK SMALL LETTER ETA
    (,(kbd "C-x 8 g f") . "θ") ;;; GREEK SMALL LETTER THETA
    (,(kbd "C-x 8 g i") . "ι") ;;; GREEK SMALL LETTER IOTA
    (,(kbd "C-x 8 g k") . "κ") ;;; GREEK SMALL LETTER KAPPA
    (,(kbd "C-x 8 g l") . "λ") ;;; GREEK SMALL LETTER LAMDA
    (,(kbd "C-x 8 g m") . "μ") ;;; GREEK SMALL LETTER MU
    (,(kbd "C-x 8 g n") . "ν") ;;; GREEK SMALL LETTER NU
    (,(kbd "C-x 8 g x") . "ξ") ;;; GREEK SMALL LETTER XI
    (,(kbd "C-x 8 g o") . "ο") ;;; GREEK SMALL LETTER OMICRON
    (,(kbd "C-x 8 g p") . "π") ;;; GREEK SMALL LETTER PI
    (,(kbd "C-x 8 g r") . "ρ") ;;; GREEK SMALL LETTER RHO
    (,(kbd "C-x 8 g s") . "σ") ;;; GREEK SMALL LETTER SIGMA
    (,(kbd "C-x 8 g t") . "τ") ;;; GREEK SMALL LETTER TAU
    (,(kbd "C-x 8 g u") . "υ") ;;; GREEK SMALL LETTER UPSILON
    (,(kbd "C-x 8 g v") . "φ") ;;; GREEK SMALL LETTER PHI
    (,(kbd "C-x 8 g c") . "χ") ;;; GREEK SMALL LETTER CHI
    (,(kbd "C-x 8 g y") . "ψ") ;;; GREEK SMALL LETTER PSI
    (,(kbd "C-x 8 g w") . "ω") ;;; GREEK SMALL LETTER OMEGA


    (,(kbd "C-x 8 g A") . "Α") ;;; GREEK CAPITAL LETTER ALPHA
    (,(kbd "C-x 8 g B") . "Β") ;;; Greek CAPITAL LETTER BETA
    (,(kbd "C-x 8 g G") . "Γ") ;;; GREEK CAPITAL LETTER GAMMA
    (,(kbd "C-x 8 g D") . "Δ") ;;; GREEK CAPITAL LETTER DELTA
    (,(kbd "C-x 8 g E") . "Ε") ;;; GREEK CAPITAL LETTER EPSILON
    (,(kbd "C-x 8 g Z") . "Ζ") ;;; GREEK CAPITAL LETTER ZETA
    (,(kbd "C-x 8 g H") . "Η") ;;; GREEK CAPITAL LETTER ETA
    (,(kbd "C-x 8 g F") . "Θ") ;;; GREEK CAPITAL LETTER THETA
    (,(kbd "C-x 8 g I") . "Ι") ;;; GREEK CAPITAL LETTER IOTA
    (,(kbd "C-x 8 g K") . "Κ") ;;; GREEK CAPITAL LETTER KAPPA
    (,(kbd "C-x 8 g L") . "Λ") ;;; GREEK CAPITAL LETTER LAMDA
    (,(kbd "C-x 8 g M") . "Μ") ;;; GREEK CAPITAL LETTER MU
    (,(kbd "C-x 8 g N") . "Ν") ;;; GREEK CAPITAL LETTER NU
    (,(kbd "C-x 8 g X") . "Ξ") ;;; GREEK CAPITAL LETTER XI
    (,(kbd "C-x 8 g O") . "Ο") ;;; GREEK CAPITAL LETTER OMICRON
    (,(kbd "C-x 8 g P") . "Π") ;;; GREEK CAPITAL LETTER PI
    (,(kbd "C-x 8 g R") . "Ρ") ;;; GREEK CAPITAL LETTER RHO
    (,(kbd "C-x 8 g S") . "Σ") ;;; GREEK CAPITAL LETTER SIGMA
    (,(kbd "C-x 8 g T") . "Τ") ;;; GREEK CAPITAL LETTER TAU
    (,(kbd "C-x 8 g U") . "Υ") ;;; GREEK CAPITAL LETTER UPSILON
    (,(kbd "C-x 8 g V") . "Φ") ;;; GREEK CAPITAL LETTER PHI
    (,(kbd "C-x 8 g C") . "Χ") ;;; GREEK CAPITAL LETTER CHI
    (,(kbd "C-x 8 g Y") . "Ψ") ;;; GREEK CAPITAL LETTER PSI
    (,(kbd "C-x 8 g W") . "Ω") ;;; GREEK CAPITAL LETTER OMEGA

    (,(kbd "C-x 8 h a") . "א") ;;; HEBREW LETTER ALEF
    (,(kbd "C-x 8 h b") . "ב") ;;; HEBREW LETTER BET
    (,(kbd "C-x 8 h g") . "ג") ;;; HEBREW LETTER GIMEL
    (,(kbd "C-x 8 h d") . "ד") ;;; HEBREW LETTER DALET
    (,(kbd "C-x 8 h h") . "ה") ;;; HEBREW LETTER HE
    (,(kbd "C-x 8 h v") . "ו") ;;; HEBREW LETTER VAV
    (,(kbd "C-x 8 h z") . "ז") ;;; HEBREW LETTER ZAYIN
    (,(kbd "C-x 8 h c") . "ח") ;;; HEBREW LETTER HET
    (,(kbd "C-x 8 h f") . "ט") ;;; HEBREW LETTER TET
    (,(kbd "C-x 8 h y") . "י") ;;; HEBREW LETTER YOD
    (,(kbd "C-x 8 h K") . "ך") ;;; HEBREW LETTER FINAL KAF
    (,(kbd "C-x 8 h k") . "כ") ;;; HEBREW LETTER KAF
    (,(kbd "C-x 8 h l") . "ל") ;;; HEBREW LETTER LAMED
    (,(kbd "C-x 8 h M") . "ם") ;;; HEBREW LETTER FINAL MEM
    (,(kbd "C-x 8 h m") . "מ") ;;; HEBREW LETTER MEM
    (,(kbd "C-x 8 h N") . "ן") ;;; HEBREW LETTER FINAL NUN
    (,(kbd "C-x 8 h n") . "נ") ;;; HEBREW LETTER NUN
    (,(kbd "C-x 8 h x") . "ס") ;;; HEBREW LETTER SAMEKH
    (,(kbd "C-x 8 h j") . "ע") ;;; HEBREW LETTER AYIN
    (,(kbd "C-x 8 h P") . "ף") ;;; HEBREW LETTER FINAL PE
    (,(kbd "C-x 8 h p") . "פ") ;;; HEBREW LETTER PE
    (,(kbd "C-x 8 h Z") . "ץ") ;;; HEBREW LETTER FINAL TSADI
    (,(kbd "C-x 8 h z") . "צ") ;;; HEBREW LETTER TSADI
    (,(kbd "C-x 8 h q") . "ק") ;;; HEBREW LETTER QOF
    (,(kbd "C-x 8 h r") . "ר") ;;; HEBREW LETTER RESH
    (,(kbd "C-x 8 h s") . "ש") ;;; HEBREW LETTER SHIN
    (,(kbd "C-x 8 h t") . "ת") ;;; HEBREW LETTER TAV

    (,(kbd "C-x 8 r a")   . "а") ;;; CYRILLIC SMALL LETTER A
    (,(kbd "C-x 8 r b")   . "б") ;;; CYRILLIC SMALL LETTER BE
    (,(kbd "C-x 8 r v")   . "в") ;;; CYRILLIC SMALL LETTER VE
    (,(kbd "C-x 8 r w")   . "в") ;;; CYRILLIC SMALL LETTER VE
    (,(kbd "C-x 8 r g")   . "г") ;;; CYRILLIC SMALL LETTER GHE
    (,(kbd "C-x 8 r d")   . "д") ;;; CYRILLIC SMALL LETTER DE
    (,(kbd "C-x 8 r e")   . "е") ;;; CYRILLIC SMALL LETTER IE
    (,(kbd "C-x 8 r / y") . "ё") ;;; CYRILLIC SMALL LETTER IO
    (,(kbd "C-x 8 r / x") . "ж") ;;; CYRILLIC SMALL LETTER ZHE
    (,(kbd "C-x 8 r z")   . "з") ;;; CYRILLIC SMALL LETTER ZE
    (,(kbd "C-x 8 r i")   . "и") ;;; CYRILLIC SMALL LETTER I
    (,(kbd "C-x 8 r j")   . "й") ;;; CYRILLIC SMALL LETTER SHORT I
    (,(kbd "C-x 8 r k")   . "к") ;;; CYRILLIC SMALL LETTER KA
    (,(kbd "C-x 8 r l")   . "л") ;;; CYRILLIC SMALL LETTER EL
    (,(kbd "C-x 8 r m")   . "м") ;;; CYRILLIC SMALL LETTER EM
    (,(kbd "C-x 8 r n")   . "н") ;;; CYRILLIC SMALL LETTER EN
    (,(kbd "C-x 8 r o")   . "о") ;;; CYRILLIC SMALL LETTER O
    (,(kbd "C-x 8 r p")   . "п") ;;; CYRILLIC SMALL LETTER PE
    (,(kbd "C-x 8 r r")   . "р") ;;; CYRILLIC SMALL LETTER ER
    (,(kbd "C-x 8 r s")   . "с") ;;; CYRILLIC SMALL LETTER ES
    (,(kbd "C-x 8 r t")   . "т") ;;; CYRILLIC SMALL LETTER TE
    (,(kbd "C-x 8 r u")   . "у") ;;; CYRILLIC SMALL LETTER U
    (,(kbd "C-x 8 r f")   . "ф") ;;; CYRILLIC SMALL LETTER EF
    (,(kbd "C-x 8 r h")   . "х") ;;; CYRILLIC SMALL LETTER HA
    (,(kbd "C-x 8 r x")   . "х") ;;; CYRILLIC SMALL LETTER HA
    (,(kbd "C-x 8 r c")   . "ц") ;;; CYRILLIC SMALL LETTER TSE
    (,(kbd "C-x 8 r / c") . "ч") ;;; CYRILLIC SMALL LETTER CHE
    (,(kbd "C-x 8 r / t") . "щ") ;;; CYRILLIC SMALL LETTER SHCHA
    (,(kbd "C-x 8 r ~")   . "ъ") ;;; CYRILLIC SMALL LETTER HARD SIGN
    (,(kbd "C-x 8 r y")   . "ы") ;;; CYRILLIC SMALL LETTER YERU
    (,(kbd "C-x 8 r '")   . "ь") ;;; CYRILLIC SMALL LETTER SOFT SIGN
    (,(kbd "C-x 8 r / f") . "э") ;;; CYRILLIC SMALL LETTER E
    (,(kbd "C-x 8 r / u") . "ю") ;;; CYRILLIC SMALL LETTER YU
    (,(kbd "C-x 8 r q")   . "я") ;;; CYRILLIC SMALL LETTER YA

    (,(kbd "C-x 8 r / a") . "ї") ;;; CYRILLIC SMALL LETTER YI
    (,(kbd "C-x 8 r / d") . "ђ") ;;; CYRILLIC SMALL LETTER DJE
    (,(kbd "C-x 8 r / e") . "є") ;;; CYRILLIC SMALL LETTER UKRAINIAN IE
    (,(kbd "C-x 8 r / g") . "ѓ") ;;; CYRILLIC SMALL LETTER GJE
    (,(kbd "C-x 8 r / h") . "ш") ;;; CYRILLIC SMALL LETTER SHA
    (,(kbd "C-x 8 r / i") . "і") ;;; CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
    (,(kbd "C-x 8 r / j") . "ј") ;;; CYRILLIC SMALL LETTER JE
    (,(kbd "C-x 8 r / k") . "ќ") ;;; CYRILLIC SMALL LETTER KJE
    (,(kbd "C-x 8 r / l") . "љ") ;;; CYRILLIC SMALL LETTER LJE
    (,(kbd "C-x 8 r / n") . "њ") ;;; CYRILLIC SMALL LETTER NJE
    (,(kbd "C-x 8 r / r") . "ћ") ;;; CYRILLIC SMALL LETTER TSHE
    (,(kbd "C-x 8 r / s") . "ѕ") ;;; CYRILLIC SMALL LETTER DZE
    (,(kbd "C-x 8 r / v") . "ў") ;;; CYRILLIC SMALL LETTER SHORT U
    (,(kbd "C-x 8 r / z") . "џ") ;;; CYRILLIC SMALL LETTER DZHE

    (,(kbd "C-x 8 r A")   . "А") ;;; CYRILLIC CAPITAL LETTER A
    (,(kbd "C-x 8 r B")   . "Б") ;;; CYRILLIC CAPITAL LETTER BE
    (,(kbd "C-x 8 r V")   . "В") ;;; CYRILLIC CAPITAL LETTER VE
    (,(kbd "C-x 8 r W")   . "В") ;;; CYRILLIC CAPITAL LETTER VE
    (,(kbd "C-x 8 r G")   . "Г") ;;; CYRILLIC CAPITAL LETTER GHE
    (,(kbd "C-x 8 r D")   . "Д") ;;; CYRILLIC CAPITAL LETTER DE
    (,(kbd "C-x 8 r E")   . "Е") ;;; CYRILLIC CAPITAL LETTER IE
    (,(kbd "C-x 8 r / Y") . "Ё") ;;; CYRILLIC CAPITAL LETTER IO
    (,(kbd "C-x 8 r / X") . "Ж") ;;; CYRILLIC CAPITAL LETTER ZHE
    (,(kbd "C-x 8 r Z")   . "З") ;;; CYRILLIC CAPITAL LETTER ZE
    (,(kbd "C-x 8 r I")   . "И") ;;; CYRILLIC CAPITAL LETTER I
    (,(kbd "C-x 8 r J")   . "Й") ;;; CYRILLIC CAPITAL LETTER SHORT I
    (,(kbd "C-x 8 r K")   . "К") ;;; CYRILLIC CAPITAL LETTER KA
    (,(kbd "C-x 8 r L")   . "Л") ;;; CYRILLIC CAPITAL LETTER EL
    (,(kbd "C-x 8 r M")   . "М") ;;; CYRILLIC CAPITAL LETTER EM
    (,(kbd "C-x 8 r N")   . "Н") ;;; CYRILLIC CAPITAL LETTER EN
    (,(kbd "C-x 8 r O")   . "О") ;;; CYRILLIC CAPITAL LETTER O
    (,(kbd "C-x 8 r P")   . "П") ;;; CYRILLIC CAPITAL LETTER PE
    (,(kbd "C-x 8 r R")   . "Р") ;;; CYRILLIC CAPITAL LETTER ER
    (,(kbd "C-x 8 r S")   . "С") ;;; CYRILLIC CAPITAL LETTER ES
    (,(kbd "C-x 8 r T")   . "Т") ;;; CYRILLIC CAPITAL LETTER TE
    (,(kbd "C-x 8 r U")   . "У") ;;; CYRILLIC CAPITAL LETTER U
    (,(kbd "C-x 8 r F")   . "Ф") ;;; CYRILLIC CAPITAL LETTER EF
    (,(kbd "C-x 8 r H")   . "Х") ;;; CYRILLIC CAPITAL LETTER HA
    (,(kbd "C-x 8 r X")   . "Х") ;;; CYRILLIC CAPITAL LETTER HA
    (,(kbd "C-x 8 r C")   . "Ц") ;;; CYRILLIC CAPITAL LETTER TSE
    (,(kbd "C-x 8 r / C") . "Ч") ;;; CYRILLIC CAPITAL LETTER CHE
    (,(kbd "C-x 8 r / T") . "Щ") ;;; CYRILLIC CAPITAL LETTER SHCHA
    (,(kbd "C-x 8 r C-~") . "Ъ") ;;; CYRILLIC CAPITAL LETTER HARD SIGN
    (,(kbd "C-x 8 r Y")   . "Ы") ;;; CYRILLIC CAPITAL LETTER YERU
    (,(kbd "C-x 8 r C-'") . "Ь") ;;; CYRILLIC CAPITAL LETTER SOFT SIGN
    (,(kbd "C-x 8 r / F") . "Э") ;;; CYRILLIC CAPITAL LETTER E
    (,(kbd "C-x 8 r / U") . "Ю") ;;; CYRILLIC CAPITAL LETTER YU
    (,(kbd "C-x 8 r Q")   . "Я") ;;; CYRILLIC CAPITAL LETTER YA

    (,(kbd "C-x 8 r / A") . "Ї") ;;; CYRILLIC CAPITAL LETTER YI
    (,(kbd "C-x 8 r / D") . "Ђ") ;;; CYRILLIC CAPITAL LETTER DJE
    (,(kbd "C-x 8 r / E") . "Є") ;;; CYRILLIC CAPITAL LETTER UKRAINIAN IE
    (,(kbd "C-x 8 r / G") . "Ѓ") ;;; CYRILLIC CAPITAL LETTER GJE
    (,(kbd "C-x 8 r / H") . "Ш") ;;; CYRILLIC CAPITAL LETTER SHA
    (,(kbd "C-x 8 r / I") . "І") ;;; CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
    (,(kbd "C-x 8 r / J") . "Ј") ;;; CYRILLIC CAPITAL LETTER JE
    (,(kbd "C-x 8 r / K") . "Ќ") ;;; CYRILLIC CAPITAL LETTER KJE
    (,(kbd "C-x 8 r / L") . "Љ") ;;; CYRILLIC CAPITAL LETTER LJE
    (,(kbd "C-x 8 r / N") . "Њ") ;;; CYRILLIC CAPITAL LETTER NJE
    (,(kbd "C-x 8 r / R") . "Ћ") ;;; CYRILLIC CAPITAL LETTER TSHE
    (,(kbd "C-x 8 r / S") . "Ѕ") ;;; CYRILLIC CAPITAL LETTER DZE
    (,(kbd "C-x 8 r / V") . "Ў") ;;; CYRILLIC CAPITAL LETTER SHORT U
    (,(kbd "C-x 8 r / Z") . "Џ") ;;; CYRILLIC CAPITAL LETTER DZHE
    )
   "A list of cons cells whose CAR is a key sequence and CDR a
   Unicode character. For each such cons cell, the key sequence
   is bound to insertion of the associated Unicode character when
   the minor mode `unicode-keymap-extensions-mode' is active."
  :tag "Unicode keymap-extensions bindings"
  :version "25.1"
  :group 'unicode-keymap-extensions
  :type '(repeat (cons key-sequence character)))

(defun init-extensions-keymap (bindings-sym)
  "Initialize keymap given BINDINGS-SYM, a list of (key-sequence
. char) bindings."
  (let ((map (make-sparse-keymap)))
    (mapc
     (lambda (binding)
       (define-key map (car binding) (cdr binding)))
     (symbol-value bindings-sym))
    map))

;;;###autoload
(define-minor-mode unicode-keymap-extensions-mode
  "Toggles variable `unicode-keymap-extensions-mode'.

 Interactively with no argument, this command toggles the mode.
With a positive prefix argument, enables the mode. With any other
prefix argument, disables it.

From Lisp (i.e., non-interactively), argument omitted or nil
enables the mode, `toggle' toggles the state.

`unicode-keymap-extensions-mode' is a minor mode that provides
extended Unicode input bindings, as per custom variable
`unicode-keymap-extensions-bindings'."

  ;;; No initial value.
  :init-value nil

  ;;; No mode line indicator.
  :lighter nil

  ;;; Minor mode keymap
  :keymap (init-extensions-keymap 'unicode-keymap-extensions-bindings)

  ;;; Gobal variable `unicode-keymap-extensions-mode'.
  :global t

  ;;; Customization group
  :group 'unicode-keymap-extensions
  )

;;; `unicode-keymap-extensions-local-mode' valid only for Emacs 25+.

;;;###autoload
(define-minor-mode unicode-keymap-extensions-local-mode
  "Toggle `unicode-keymap-extensions-mode' only in this buffer."
  :variable (buffer-local-value 'unicode-keymap-extensions-mode
                                  (current-buffer))
  (cond
   ((eq unicode-keymap-extensions-mode
        (default-value 'unicode-keymap-extensions-mode))
    (kill-local-variable 'unicode-keymap-extensions-mode))
   ((not (default-value 'unicode-keymap-extensions-mode))

    ;;; Enable locally.
    (unicode-keymap-extensions-mode)

    ;;; Disable globally elsewhere.
    (setq-default unicode-keymap-extensions-mode nil)
    )))

(provide 'unicode-keymap-extensions)
