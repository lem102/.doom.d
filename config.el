;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq-default truncate-lines nil)
(setopt word-wrap nil)

(auto-save-visited-mode 1)

(global-subword-mode 1)

(setopt xah-fly-use-control-key nil
        xah-fly-use-meta-key nil)

(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)

(defun jacob-xfk-local-key (key command)
  "Bind KEY buffer locally to COMMAND in xfk command mode."
  (let ((existing-command (keymap-lookup xah-fly-command-map key nil "NO-REMAP")))
    (unless existing-command
      (user-error "%s is not bound to a key in `xah-fly-command-map'" key))
    (keymap-local-set (format "<remap> <%s>" existing-command)
                      command)))

(load! "lemovem.el")

(defalias 'jacob-return-macro
  (kmacro "<return>"))

(keymap-global-set "<f7>" #'xah-fly-leader-key-map)

(keymap-global-set "M-SPC" #'xah-fly-command-mode-activate)

(keymap-set xah-fly-command-map "4" #'other-window-prefix)
;; (keymap-set xah-fly-command-map "9" #'jacob-swap-visible-buffers)
(keymap-set xah-fly-command-map ";" #'lemovem-end-of-line)
(keymap-set xah-fly-command-map "d" #'lemovem-backspace)
(keymap-set xah-fly-command-map "g" #'lemovem-kill-paragraph)
(keymap-set xah-fly-command-map "h" #'lemovem-beginning-of-line)
(keymap-set xah-fly-command-map "s" #'jacob-return-macro)
(keymap-set xah-fly-command-map "x" #'lemovem-kill-line)

(keymap-set xah-fly-insert-map "M-SPC" #'xah-fly-command-mode-activate)

(defmacro jacob-defhookf (hook &rest body)
  "Define function with BODY and bind it to HOOK."
  (declare (indent defun))
  (let* ((hook-name (symbol-name hook))
         (function-name (intern (concat "jacob-" hook-name "-function"))))
    `(progn
       (defun ,function-name ()
         ,(format "Auto-generated hook function for `%s'." hook-name)
         ,@body)
       (add-hook ',hook #',function-name))))

(after! dired
  (jacob-defhookf dired-mode-hook
    (dired-hide-details-mode 1)
    (jacob-xfk-local-key "s" #'dired-find-file)
    (jacob-xfk-local-key "d" #'dired-do-delete) ; we skip the "flag, delete" process as files are sent to system bin on deletion
    (jacob-xfk-local-key "q" #'quit-window)
    (jacob-xfk-local-key "i" #'dired-previous-line)
    (jacob-xfk-local-key "k" #'dired-next-line)
    (jacob-xfk-local-key "e" #'dired-mark)
    (jacob-xfk-local-key "r" #'dired-unmark)
    (jacob-xfk-local-key "g" #'revert-buffer)
    (jacob-xfk-local-key "x" #'dired-do-rename)
    (jacob-xfk-local-key "c" #'dired-do-copy)
    (jacob-xfk-local-key "u" #'dired-up-directory)
    (jacob-xfk-local-key "j" #'dired-goto-file)))

(after! helpful
  (jacob-defhookf helpful-mode-hook
    (jacob-xfk-local-key "q" #'+popup/quit-window)))

(after! magit
  (jacob-defhookf magit-mode-hook
    (jacob-xfk-local-key "q" #'+magit/quit)))

(after! corfu
  (map! :map corfu-map
        "M-SPC" nil))

(require 'expand-region)
(keymap-set xah-fly-command-map "8" #'er/expand-region)
(setopt expand-region-contract-fast-key "9")
