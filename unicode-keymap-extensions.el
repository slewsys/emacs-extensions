;;; unicode-keymap-extensions.el --- Unicode keymap   -*- lexical-binding: t -*-
;;
;; -*- coding: utf-8 -*-
;;
;; Copyright (C) 2017 Andrew L. Moore

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unicode Latin-1 Supplement and Latin Extended-A keymap.
;;
(defgroup unicode-keymap-extensions ()
  "Extended Unicode input bindings."
  :package-version '(unicode-keymap-extensions . "1.0")
  :group 'editing)

(defcustom unicode-keymap-extensions-bindings

   ;;; KEY SEQUENCE         UNICODE CHAR
  `((,(kbd "C-x 8 : :") . ,(kbd "：")) ;;; FULL-WIDTH COLON
    (,(kbd "C-x 8 : /") . ,(kbd "／")) ;;; FULL-WIDTH SOLIDUS
    (,(kbd "C-x 8 / l") . ,(kbd "ł")) ;;; SMALL L WITH STROKE
    (,(kbd "C-x 8 / L") . ,(kbd "Ł")) ;;; CAPITAL L WITH STROKE
    (,(kbd "C-x 8 v c") . ,(kbd "č")) ;;; SMALL C WITH CARON
    (,(kbd "C-x 8 v C") . ,(kbd "Č")) ;;; CAPITAL C WITH CARON
    (,(kbd "C-x 8 v e") . ,(kbd "ě")) ;;; SMALL E WITH CARON
    (,(kbd "C-x 8 v E") . ,(kbd "Ě")) ;;; CAPITAL E WITH CARON
    (,(kbd "C-x 8 v r") . ,(kbd "ř")) ;;; SMALL R WITH CARON
    (,(kbd "C-x 8 v R") . ,(kbd "Ř")) ;;; CAPITAL R WITH CARON
    (,(kbd "C-x 8 v s") . ,(kbd "š")) ;;; SMALL S WITH CARON
    (,(kbd "C-x 8 v S") . ,(kbd "Š")) ;;; CAPITAL S WITH CARON
    (,(kbd "C-x 8 v z") . ,(kbd "ž")) ;;; SMALL Z WITH CARON
    (,(kbd "C-x 8 v Z") . ,(kbd "Ž")) ;;; CAPITAL Z WITH CARON
    (,(kbd "C-x 8 , e") . ,(kbd "ę")) ;;; SMALL E WITH OGONEK
    (,(kbd "C-x 8 , E") . ,(kbd "Ę")) ;;; CAPITAL E WITH OGONEK
    (,(kbd "C-x 8 e a") . ,(kbd "æ")) ;;; SMALL LATIN AE
    (,(kbd "C-x 8 E A") . ,(kbd "Æ")) ;;; CAPITAL LATIN AE
    (,(kbd "C-x 8 e o") . ,(kbd "œ")) ;;; SMALL LATIN OE
    (,(kbd "C-x 8 E O") . ,(kbd "Œ")) ;;; CAPITAL LATIN OE

    (,(kbd "C-x 8 g a") . ,(kbd "α")) ;;; GREEK SMALL LETTER ALPHA
    (,(kbd "C-x 8 g b") . ,(kbd "β")) ;;; GREEK SMALL LETTER BETA
    (,(kbd "C-x 8 g g") . ,(kbd "γ")) ;;; GREEK SMALL LETTER GAMMA
    (,(kbd "C-x 8 g d") . ,(kbd "δ")) ;;; GREEK SMALL LETTER DELTA
    (,(kbd "C-x 8 g e") . ,(kbd "ε")) ;;; GREEK SMALL LETTER EPSILON
    (,(kbd "C-x 8 g z") . ,(kbd "ζ")) ;;; GREEK SMALL LETTER ZETA
    (,(kbd "C-x 8 g h") . ,(kbd "η")) ;;; GREEK SMALL LETTER ETA
    (,(kbd "C-x 8 g f") . ,(kbd "θ")) ;;; GREEK SMALL LETTER THETA
    (,(kbd "C-x 8 g i") . ,(kbd "ι")) ;;; GREEK SMALL LETTER IOTA
    (,(kbd "C-x 8 g k") . ,(kbd "κ")) ;;; GREEK SMALL LETTER KAPPA
    (,(kbd "C-x 8 g l") . ,(kbd "λ")) ;;; GREEK SMALL LETTER LAMDA
    (,(kbd "C-x 8 g m") . ,(kbd "μ")) ;;; GREEK SMALL LETTER MU
    (,(kbd "C-x 8 g n") . ,(kbd "ν")) ;;; GREEK SMALL LETTER NU
    (,(kbd "C-x 8 g x") . ,(kbd "ξ")) ;;; GREEK SMALL LETTER XI
    (,(kbd "C-x 8 g o") . ,(kbd "ο")) ;;; GREEK SMALL LETTER OMICRON
    (,(kbd "C-x 8 g p") . ,(kbd "π")) ;;; GREEK SMALL LETTER PI
    (,(kbd "C-x 8 g r") . ,(kbd "ρ")) ;;; GREEK SMALL LETTER RHO
    (,(kbd "C-x 8 g s") . ,(kbd "σ")) ;;; GREEK SMALL LETTER SIGMA
    (,(kbd "C-x 8 g t") . ,(kbd "τ")) ;;; GREEK SMALL LETTER TAU
    (,(kbd "C-x 8 g u") . ,(kbd "υ")) ;;; GREEK SMALL LETTER UPSILON
    (,(kbd "C-x 8 g v") . ,(kbd "φ")) ;;; GREEK SMALL LETTER PHI
    (,(kbd "C-x 8 g c") . ,(kbd "χ")) ;;; GREEK SMALL LETTER CHI
    (,(kbd "C-x 8 g y") . ,(kbd "ψ")) ;;; GREEK SMALL LETTER PSI
    (,(kbd "C-x 8 g w") . ,(kbd "ω")) ;;; GREEK SMALL LETTER OMEGA


    (,(kbd "C-x 8 g A") . ,(kbd "Α")) ;;; GREEK CAPITAL LETTER ALPHA
    (,(kbd "C-x 8 g B") . ,(kbd "Β")) ;;; Greek CAPITAL LETTER BETA
    (,(kbd "C-x 8 g G") . ,(kbd "Γ")) ;;; GREEK CAPITAL LETTER GAMMA
    (,(kbd "C-x 8 g D") . ,(kbd "Δ")) ;;; GREEK CAPITAL LETTER DELTA
    (,(kbd "C-x 8 g E") . ,(kbd "Ε")) ;;; GREEK CAPITAL LETTER EPSILON
    (,(kbd "C-x 8 g Z") . ,(kbd "Ζ")) ;;; GREEK CAPITAL LETTER ZETA
    (,(kbd "C-x 8 g H") . ,(kbd "Η")) ;;; GREEK CAPITAL LETTER ETA
    (,(kbd "C-x 8 g F") . ,(kbd "Θ")) ;;; GREEK CAPITAL LETTER THETA
    (,(kbd "C-x 8 g I") . ,(kbd "Ι")) ;;; GREEK CAPITAL LETTER IOTA
    (,(kbd "C-x 8 g K") . ,(kbd "Κ")) ;;; GREEK CAPITAL LETTER KAPPA
    (,(kbd "C-x 8 g L") . ,(kbd "Λ")) ;;; GREEK CAPITAL LETTER LAMDA
    (,(kbd "C-x 8 g M") . ,(kbd "Μ")) ;;; GREEK CAPITAL LETTER MU
    (,(kbd "C-x 8 g N") . ,(kbd "Ν")) ;;; GREEK CAPITAL LETTER NU
    (,(kbd "C-x 8 g X") . ,(kbd "Ξ")) ;;; GREEK CAPITAL LETTER XI
    (,(kbd "C-x 8 g O") . ,(kbd "Ο")) ;;; GREEK CAPITAL LETTER OMICRON
    (,(kbd "C-x 8 g P") . ,(kbd "Π")) ;;; GREEK CAPITAL LETTER PI
    (,(kbd "C-x 8 g R") . ,(kbd "Ρ")) ;;; GREEK CAPITAL LETTER RHO
    (,(kbd "C-x 8 g S") . ,(kbd "Σ")) ;;; GREEK CAPITAL LETTER SIGMA
    (,(kbd "C-x 8 g T") . ,(kbd "Τ")) ;;; GREEK CAPITAL LETTER TAU
    (,(kbd "C-x 8 g U") . ,(kbd "Υ")) ;;; GREEK CAPITAL LETTER UPSILON
    (,(kbd "C-x 8 g V") . ,(kbd "Φ")) ;;; GREEK CAPITAL LETTER PHI
    (,(kbd "C-x 8 g C") . ,(kbd "Χ")) ;;; GREEK CAPITAL LETTER CHI
    (,(kbd "C-x 8 g Y") . ,(kbd "Ψ")) ;;; GREEK CAPITAL LETTER PSI
    (,(kbd "C-x 8 g W") . ,(kbd "Ω")) ;;; GREEK CAPITAL LETTER OMEGA

    (,(kbd "C-x 8 h a") . ,(kbd "א")) ;;; HEBREW LETTER ALEF
    (,(kbd "C-x 8 h b") . ,(kbd "ב")) ;;; HEBREW LETTER BET
    (,(kbd "C-x 8 h g") . ,(kbd "ג")) ;;; HEBREW LETTER GIMEL
    (,(kbd "C-x 8 h d") . ,(kbd "ד")) ;;; HEBREW LETTER DALET
    (,(kbd "C-x 8 h h") . ,(kbd "ה")) ;;; HEBREW LETTER HE
    (,(kbd "C-x 8 h v") . ,(kbd "ו")) ;;; HEBREW LETTER VAV
    (,(kbd "C-x 8 h z") . ,(kbd "ז")) ;;; HEBREW LETTER ZAYIN
    (,(kbd "C-x 8 h c") . ,(kbd "ח")) ;;; HEBREW LETTER HET
    (,(kbd "C-x 8 h f") . ,(kbd "ט")) ;;; HEBREW LETTER TET
    (,(kbd "C-x 8 h y") . ,(kbd "י")) ;;; HEBREW LETTER YOD
    (,(kbd "C-x 8 h K") . ,(kbd "ך")) ;;; HEBREW LETTER FINAL KAF
    (,(kbd "C-x 8 h k") . ,(kbd "כ")) ;;; HEBREW LETTER KAF
    (,(kbd "C-x 8 h l") . ,(kbd "ל")) ;;; HEBREW LETTER LAMED
    (,(kbd "C-x 8 h M") . ,(kbd "ם")) ;;; HEBREW LETTER FINAL MEM
    (,(kbd "C-x 8 h m") . ,(kbd "מ")) ;;; HEBREW LETTER MEM
    (,(kbd "C-x 8 h N") . ,(kbd "ן")) ;;; HEBREW LETTER FINAL NUN
    (,(kbd "C-x 8 h n") . ,(kbd "נ")) ;;; HEBREW LETTER NUN
    (,(kbd "C-x 8 h x") . ,(kbd "ס")) ;;; HEBREW LETTER SAMEKH
    (,(kbd "C-x 8 h j") . ,(kbd "ע")) ;;; HEBREW LETTER AYIN
    (,(kbd "C-x 8 h P") . ,(kbd "ף")) ;;; HEBREW LETTER FINAL PE
    (,(kbd "C-x 8 h p") . ,(kbd "פ")) ;;; HEBREW LETTER PE
    (,(kbd "C-x 8 h Z") . ,(kbd "ץ")) ;;; HEBREW LETTER FINAL TSADI
    (,(kbd "C-x 8 h z") . ,(kbd "צ")) ;;; HEBREW LETTER TSADI
    (,(kbd "C-x 8 h q") . ,(kbd "ק")) ;;; HEBREW LETTER QOF
    (,(kbd "C-x 8 h r") . ,(kbd "ר")) ;;; HEBREW LETTER RESH
    (,(kbd "C-x 8 h s") . ,(kbd "ש")) ;;; HEBREW LETTER SHIN
    (,(kbd "C-x 8 h t") . ,(kbd "ת")) ;;; HEBREW LETTER TAV

    (,(kbd "C-x 8 r a") . ,(kbd "а")) ;;; CYRILLIC SMALL LETTER A
    (,(kbd "C-x 8 r b") . ,(kbd "б")) ;;; CYRILLIC SMALL LETTER BE
    (,(kbd "C-x 8 r v") . ,(kbd "в")) ;;; CYRILLIC SMALL LETTER VE
    (,(kbd "C-x 8 r w") . ,(kbd "в")) ;;; CYRILLIC SMALL LETTER VE
    (,(kbd "C-x 8 r g") . ,(kbd "г")) ;;; CYRILLIC SMALL LETTER GHE
    (,(kbd "C-x 8 r d") . ,(kbd "д")) ;;; CYRILLIC SMALL LETTER DE
    (,(kbd "C-x 8 r e") . ,(kbd "е")) ;;; CYRILLIC SMALL LETTER IE
    (,(kbd "C-x 8 r / y") . ,(kbd "ё")) ;;; CYRILLIC SMALL LETTER IO
    (,(kbd "C-x 8 r / x") . ,(kbd "ж")) ;;; CYRILLIC SMALL LETTER ZHE
    (,(kbd "C-x 8 r z") . ,(kbd "з")) ;;; CYRILLIC SMALL LETTER ZE
    (,(kbd "C-x 8 r i") . ,(kbd "и")) ;;; CYRILLIC SMALL LETTER I
    (,(kbd "C-x 8 r j") . ,(kbd "й")) ;;; CYRILLIC SMALL LETTER SHORT I
    (,(kbd "C-x 8 r k") . ,(kbd "к")) ;;; CYRILLIC SMALL LETTER KA
    (,(kbd "C-x 8 r l") . ,(kbd "л")) ;;; CYRILLIC SMALL LETTER EL
    (,(kbd "C-x 8 r m") . ,(kbd "м")) ;;; CYRILLIC SMALL LETTER EM
    (,(kbd "C-x 8 r n") . ,(kbd "н")) ;;; CYRILLIC SMALL LETTER EN
    (,(kbd "C-x 8 r o") . ,(kbd "о")) ;;; CYRILLIC SMALL LETTER O
    (,(kbd "C-x 8 r p") . ,(kbd "п")) ;;; CYRILLIC SMALL LETTER PE
    (,(kbd "C-x 8 r r") . ,(kbd "р")) ;;; CYRILLIC SMALL LETTER ER
    (,(kbd "C-x 8 r s") . ,(kbd "с")) ;;; CYRILLIC SMALL LETTER ES
    (,(kbd "C-x 8 r t") . ,(kbd "т")) ;;; CYRILLIC SMALL LETTER TE
    (,(kbd "C-x 8 r u") . ,(kbd "у")) ;;; CYRILLIC SMALL LETTER U
    (,(kbd "C-x 8 r f") . ,(kbd "ф")) ;;; CYRILLIC SMALL LETTER EF
    (,(kbd "C-x 8 r h") . ,(kbd "х")) ;;; CYRILLIC SMALL LETTER HA
    (,(kbd "C-x 8 r x") . ,(kbd "х")) ;;; CYRILLIC SMALL LETTER HA
    (,(kbd "C-x 8 r c") . ,(kbd "ц")) ;;; CYRILLIC SMALL LETTER TSE
    (,(kbd "C-x 8 r / c") . ,(kbd "ч")) ;;; CYRILLIC SMALL LETTER CHE
    (,(kbd "C-x 8 r / t") . ,(kbd "щ")) ;;; CYRILLIC SMALL LETTER SHCHA
    (,(kbd "C-x 8 r ~") . ,(kbd "ъ")) ;;; CYRILLIC SMALL LETTER HARD SIGN
    (,(kbd "C-x 8 r y") . ,(kbd "ы")) ;;; CYRILLIC SMALL LETTER YERU
    (,(kbd "C-x 8 r '") . ,(kbd "ь")) ;;; CYRILLIC SMALL LETTER SOFT SIGN
    (,(kbd "C-x 8 r / f") . ,(kbd "э")) ;;; CYRILLIC SMALL LETTER E
    (,(kbd "C-x 8 r / u") . ,(kbd "ю")) ;;; CYRILLIC SMALL LETTER YU
    (,(kbd "C-x 8 r q") . ,(kbd "я")) ;;; CYRILLIC SMALL LETTER YA

    (,(kbd "C-x 8 r / a") . ,(kbd "ї")) ;;; CYRILLIC SMALL LETTER YI
    (,(kbd "C-x 8 r / d") . ,(kbd "ђ")) ;;; CYRILLIC SMALL LETTER DJE
    (,(kbd "C-x 8 r / e") . ,(kbd "є")) ;;; CYRILLIC SMALL LETTER UKRAINIAN IE
    (,(kbd "C-x 8 r / g") . ,(kbd "ѓ")) ;;; CYRILLIC SMALL LETTER GJE
    (,(kbd "C-x 8 r / h") . ,(kbd "ш")) ;;; CYRILLIC SMALL LETTER SHA
    (,(kbd "C-x 8 r / i") . ,(kbd "і")) ;;; CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
    (,(kbd "C-x 8 r / j") . ,(kbd "ј")) ;;; CYRILLIC SMALL LETTER JE
    (,(kbd "C-x 8 r / k") . ,(kbd "ќ")) ;;; CYRILLIC SMALL LETTER KJE
    (,(kbd "C-x 8 r / l") . ,(kbd "љ")) ;;; CYRILLIC SMALL LETTER LJE
    (,(kbd "C-x 8 r / n") . ,(kbd "њ")) ;;; CYRILLIC SMALL LETTER NJE
    (,(kbd "C-x 8 r / r") . ,(kbd "ћ")) ;;; CYRILLIC SMALL LETTER TSHE
    (,(kbd "C-x 8 r / s") . ,(kbd "ѕ")) ;;; CYRILLIC SMALL LETTER DZE
    (,(kbd "C-x 8 r / v") . ,(kbd "ў")) ;;; CYRILLIC SMALL LETTER SHORT U
    (,(kbd "C-x 8 r / z") . ,(kbd "џ")) ;;; CYRILLIC SMALL LETTER DZHE

    (,(kbd "C-x 8 r A") . ,(kbd "А")) ;;; CYRILLIC CAPITAL LETTER A
    (,(kbd "C-x 8 r B") . ,(kbd "Б")) ;;; CYRILLIC CAPITAL LETTER BE
    (,(kbd "C-x 8 r V") . ,(kbd "В")) ;;; CYRILLIC CAPITAL LETTER VE
    (,(kbd "C-x 8 r W") . ,(kbd "В")) ;;; CYRILLIC CAPITAL LETTER VE
    (,(kbd "C-x 8 r G") . ,(kbd "Г")) ;;; CYRILLIC CAPITAL LETTER GHE
    (,(kbd "C-x 8 r D") . ,(kbd "Д")) ;;; CYRILLIC CAPITAL LETTER DE
    (,(kbd "C-x 8 r E") . ,(kbd "Е")) ;;; CYRILLIC CAPITAL LETTER IE
    (,(kbd "C-x 8 r / Y") . ,(kbd "Ё")) ;;; CYRILLIC CAPITAL LETTER IO
    (,(kbd "C-x 8 r / X") . ,(kbd "Ж")) ;;; CYRILLIC CAPITAL LETTER ZHE
    (,(kbd "C-x 8 r Z") . ,(kbd "З")) ;;; CYRILLIC CAPITAL LETTER ZE
    (,(kbd "C-x 8 r I") . ,(kbd "И")) ;;; CYRILLIC CAPITAL LETTER I
    (,(kbd "C-x 8 r J") . ,(kbd "Й")) ;;; CYRILLIC CAPITAL LETTER SHORT I
    (,(kbd "C-x 8 r K") . ,(kbd "К")) ;;; CYRILLIC CAPITAL LETTER KA
    (,(kbd "C-x 8 r L") . ,(kbd "Л")) ;;; CYRILLIC CAPITAL LETTER EL
    (,(kbd "C-x 8 r M") . ,(kbd "М")) ;;; CYRILLIC CAPITAL LETTER EM
    (,(kbd "C-x 8 r N") . ,(kbd "Н")) ;;; CYRILLIC CAPITAL LETTER EN
    (,(kbd "C-x 8 r O") . ,(kbd "О")) ;;; CYRILLIC CAPITAL LETTER O
    (,(kbd "C-x 8 r P") . ,(kbd "П")) ;;; CYRILLIC CAPITAL LETTER PE
    (,(kbd "C-x 8 r R") . ,(kbd "Р")) ;;; CYRILLIC CAPITAL LETTER ER
    (,(kbd "C-x 8 r S") . ,(kbd "С")) ;;; CYRILLIC CAPITAL LETTER ES
    (,(kbd "C-x 8 r T") . ,(kbd "Т")) ;;; CYRILLIC CAPITAL LETTER TE
    (,(kbd "C-x 8 r U") . ,(kbd "У")) ;;; CYRILLIC CAPITAL LETTER U
    (,(kbd "C-x 8 r F") . ,(kbd "Ф")) ;;; CYRILLIC CAPITAL LETTER EF
    (,(kbd "C-x 8 r H") . ,(kbd "Х")) ;;; CYRILLIC CAPITAL LETTER HA
    (,(kbd "C-x 8 r X") . ,(kbd "Х")) ;;; CYRILLIC CAPITAL LETTER HA
    (,(kbd "C-x 8 r C") . ,(kbd "Ц")) ;;; CYRILLIC CAPITAL LETTER TSE
    (,(kbd "C-x 8 r / C") . ,(kbd "Ч")) ;;; CYRILLIC CAPITAL LETTER CHE
    (,(kbd "C-x 8 r / T") . ,(kbd "Щ")) ;;; CYRILLIC CAPITAL LETTER SHCHA
    (,(kbd "C-x 8 r C-~") . ,(kbd "Ъ")) ;;; CYRILLIC CAPITAL LETTER HARD SIGN
    (,(kbd "C-x 8 r Y") . ,(kbd "Ы")) ;;; CYRILLIC CAPITAL LETTER YERU
    (,(kbd "C-x 8 r C-'") . ,(kbd "Ь")) ;;; CYRILLIC CAPITAL LETTER SOFT SIGN
    (,(kbd "C-x 8 r / F") . ,(kbd "Э")) ;;; CYRILLIC CAPITAL LETTER E
    (,(kbd "C-x 8 r / U") . ,(kbd "Ю")) ;;; CYRILLIC CAPITAL LETTER YU
    (,(kbd "C-x 8 r Q") . ,(kbd "Я")) ;;; CYRILLIC CAPITAL LETTER YA

    (,(kbd "C-x 8 r / A") . ,(kbd "Ї")) ;;; CYRILLIC CAPITAL LETTER YI
    (,(kbd "C-x 8 r / D") . ,(kbd "Ђ")) ;;; CYRILLIC CAPITAL LETTER DJE
    (,(kbd "C-x 8 r / E") . ,(kbd "Є")) ;;; CYRILLIC CAPITAL LETTER UKRAINIAN IE
    (,(kbd "C-x 8 r / G") . ,(kbd "Ѓ")) ;;; CYRILLIC CAPITAL LETTER GJE
    (,(kbd "C-x 8 r / H") . ,(kbd "Ш")) ;;; CYRILLIC CAPITAL LETTER SHA
    (,(kbd "C-x 8 r / I") . ,(kbd "І")) ;;; CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
    (,(kbd "C-x 8 r / J") . ,(kbd "Ј")) ;;; CYRILLIC CAPITAL LETTER JE
    (,(kbd "C-x 8 r / K") . ,(kbd "Ќ")) ;;; CYRILLIC CAPITAL LETTER KJE
    (,(kbd "C-x 8 r / L") . ,(kbd "Љ")) ;;; CYRILLIC CAPITAL LETTER LJE
    (,(kbd "C-x 8 r / N") . ,(kbd "Њ")) ;;; CYRILLIC CAPITAL LETTER NJE
    (,(kbd "C-x 8 r / R") . ,(kbd "Ћ")) ;;; CYRILLIC CAPITAL LETTER TSHE
    (,(kbd "C-x 8 r / S") . ,(kbd "Ѕ")) ;;; CYRILLIC CAPITAL LETTER DZE
    (,(kbd "C-x 8 r / V") . ,(kbd "Ў")) ;;; CYRILLIC CAPITAL LETTER SHORT U
    (,(kbd "C-x 8 r / Z") . ,(kbd "Џ")) ;;; CYRILLIC CAPITAL LETTER DZHE
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
