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

;;; Environment

(load! "environment.el" nil "NOERROR")

;;; xah fly keys

(setopt xah-fly-use-control-key nil
        xah-fly-use-meta-key nil)

(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)

;;; Editor

(auto-save-visited-mode 1)

(global-subword-mode 1)

(load! "lemovem.el")

(require 'expand-region)
(setopt expand-region-contract-fast-key "9")

;;; UI

(after! lsp-mode
  (setopt lsp-ui-doc-enable nil
          lsp-lens-enable nil
          lsp-ui-sideline-enable nil
          lsp-eldoc-enable-hover t
          lsp-eldoc-render-all t))

(after! eldoc
  (setopt eldoc-documentation-strategy 'eldoc-documentation-compose))

(setq-default truncate-lines nil)
(setopt word-wrap nil)

(defun jacob-lsp-signature-eldoc (eldoc-callback &rest _ignored)
  "Send `lsp-signature-mode' stuff to `eldoc'."
  (when lsp-mode
    (let ((f (lambda (signature)
               (let ((message
                      (if (lsp-signature-help? signature)
                          (lsp--signature->message signature)
                        (mapconcat #'lsp--signature->message signature "\n"))))
                 (when (s-present? message)
                   (funcall eldoc-callback message))))))
      (lsp-request-async "textDocument/signatureHelp"
                         (lsp--text-document-position-params)
                         f
                         :cancel-token :signature))))

;; adapted from https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
(defun jacob-flycheck-eldoc (callback &rest _ignored)
  "Print flycheck messages at point by calling CALLBACK."
  (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
    (mapc
     (lambda (err)
       (funcall callback
                (flycheck-error-message err)
                :thing (or (flycheck-error-id err)
                           (flycheck-error-group err))
                :face 'font-lock-doc-face))
     flycheck-errors)))

(defun jacob-prefer-eldoc ()
  (add-hook 'eldoc-documentation-functions #'jacob-flycheck-eldoc nil "LOCAL")
  (add-hook 'eldoc-documentation-functions #'jacob-lsp-signature-eldoc nil "LOCAL")
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (setq flycheck-display-errors-function nil)
  (setq flycheck-help-echo-function nil))

(after! (lsp-mode eldoc)
  (add-hook! 'lsp-mode-hook #'jacob-prefer-eldoc))

(after! compile
  (setopt compilation-scroll-output t))

(defun jacob-reset-cursor-advice ()
  "Advice for `wdired-exit'."
  (setq cursor-type t))

(after! (dired dirvish)
  ;; fix not restoring cursor to solid box
  (advice-add #'wdired-exit :after #'jacob-reset-cursor-advice)
  (advice-add #'wdired-finish-edit :after #'jacob-reset-cursor-advice))

;;; Completion

(setopt completion-ignore-case t)

(setq-default tab-always-indent 'complete)

(after! csharp-mode
  ;; Change from `c-indent-line' to this to allow TAB to double for completion.
  (keymap-set csharp-mode-map "TAB" #'completion-at-point))

(after! corfu
  (map! :map corfu-map
        "M-SPC" nil)
  (setopt corfu-auto nil))

(after! dabbrev
  (setopt dabbrev-case-fold-search nil
          dabbrev-case-replace nil))

(after! yasnippet
  (defun jacob-yas-camel-case (input)
    "Convert INPUT to camel case e.g. apple banana -> appleBanana.
For use in yasnippets."
    (let* ((space-at-end (if (string-match-p " $" input) " " ""))
           (words (split-string input))
           (capitalised-words (seq-reduce (lambda (previous current)
                                            (concat previous (capitalize current)))
                                          (cdr words)
                                          (car words))))
      (concat capitalised-words space-at-end)))

  (defun jacob-yas-pascal-case (input)
    "Convert INPUT to pascal case e.g. apple banana -> AppleBanana.
For use in yasnippets."
    (let ((space-at-end (if (string-match-p " $" input)
                            " "
                          "")))
      (with-temp-buffer
        (insert input)
        (goto-char (point-min))
        (subword-mode 1)
        (while (not (= (point) (point-max)))
          (call-interactively #'capitalize-word))
        (goto-char (point-min))
        (while (search-forward " " nil "NOERROR")
          (replace-match ""))
        (goto-char (point-max))
        (insert space-at-end)
        (buffer-substring-no-properties (point-min) (point-max)))))

  (defun jacob-yas-snake-case (input)
    "Convert INPUT to snake case e.g. apple banana -> apple_banana.
For use in yasnippets."
    (string-replace " " "_" input))

  (defun jacob-yas-kebab-case (input)
    "Convert INPUT to kebab case e.g. apple banana -> apple_banana.
For use in yasnippets."
    (string-replace " " "-" input)))

;;; tools

(require 'verb)
(add-hook 'org-mode-hook 'verb-mode)

(defun jacob-verb-id (response-id)
  "Get the id property from the stored verb response pertaining to RESPONSE-ID."
  (verb-json-get (oref (verb-stored-response response-id) body) "id"))

(add-to-list 'org-capture-templates '("i" "Inbox" entry (file "") "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:"))

(setopt org-agenda-start-day nil
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-custom-commands '(("a" "Morning" agenda "" ((org-agenda-tag-filter-preset '("+tickler" "+am"))
                                                               (org-agenda-span 'day)))
                                     ("p" "Evening" agenda "" ((org-agenda-tag-filter-preset '("+tickler" "+pm"))
                                                               (org-agenda-span 'day)))
                                     ("w" "Work" todo "" ((org-agenda-tag-filter-preset '("+work"))))
                                     ("x" "Stuff to do today"
                                      agenda ""
                                      ((org-agenda-time-grid '((daily today require-timed)
                                                               nil
                                                               " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
                                       (org-agenda-span 'day)
                                       (org-agenda-tag-filter-preset '("-tickler" "-work"))))))

(after! dap-mode
  (require 'dap-netcore)

  (dap-register-debug-template ".Net Core Launch (ASP.Net)"
                               (list :type "coreclr"
                                     :request "launch"
                                     :mode "launch"
                                     :name "NetCoreDbg::Launch"
                                     :dap-compilation "dotnet build"
                                     ;; TODO work out how can I populate the environemnt values from launchSettings.json
                                     :env '(("ASPNETCORE_ENVIRONMENT" . "Development"))
                                     :cwd "/home/jacobl/dev/tappitGitHub/loyalty-discount-service/Presentation/Discount.Api/")))

(after! prodigy
  (setopt prodigy-kill-process-buffer-on-stop t)

  (prodigy-define-tag
    :name 'asp.net
    :stop-signal 'kill
    :on-output (lambda (&rest args)
                 (let ((output (plist-get args :output))
                       (service (plist-get args :service)))
                   (when (string-match-p "Hosting started *$" output)
                     (prodigy-set-status service 'ready))))))

(after! sql
  (defun jacob-sql-connect ()
    "Wrapper for `sql-connect' to set postgres password.
CONNECTION is the connection settings."
    (interactive)
    (let ((connection (sql-read-connection "Connection: ")))
      (with-environment-variables
          (("PGPASSWORD" (cadr (assoc 'sql-password
                                      (assoc-string connection
                                                    sql-connection-alist
                                                    t)))))
        (sql-connect connection)))))

(jacob-defhookf sql-interactive-mode-hook
  (when (eq sql-product 'postgres)
    (setq sql-prompt-regexp "^[-[:alnum:]_]*[-=]\\*?[#>] ")
    (setq sql-prompt-cont-regexp "^\\(?:\\sw\\|\\s_\\)*[-(]\\*?[#>] "))
  (jacob-xfk-local-key "SPC , d" #'sql-send-paragraph))

(defun jacob-sqli-end-of-buffer ()
  "Move point to end of sqli buffer before sending paragraph.

Intended as before advice for `sql-send-paragraph'."
  (with-current-buffer sql-buffer
    (goto-char (point-max))))

(advice-add #'sql-send-paragraph :before #'jacob-sqli-end-of-buffer)

;;; Keybindings

;; setting `doom-leader-key' somehow allows which-key to know the names of the
;; keymaps that are “underneath” `doom-leader-map'.

;; (setq doom-leader-key "SPC SPC")
;; (setq doom-localleader-key "SPC SPC m")

;; (keymap-set xah-fly-command-map "SPC SPC" doom-leader-map)

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

(keymap-set xah-fly-command-map "8" #'er/expand-region)

(defun jacob-xfk-local-key (key command)
  "Bind KEY buffer locally to COMMAND in xfk command mode."
  (let ((existing-command (keymap-lookup xah-fly-command-map key nil "NO-REMAP")))
    (unless existing-command
      (user-error "%s is not bound to a key in `xah-fly-command-map'" key))
    (keymap-local-set (format "<remap> <%s>" existing-command)
                      command)))

(after! dired
  (jacob-defhookf dired-mode-hook
    (dired-hide-details-mode 1)
    (jacob-xfk-local-key "s" #'dired-find-file)
    (jacob-xfk-local-key "d" #'dired-do-delete) ; we skip the "flag, delete" process as files are sent to system bin on deletion
    (jacob-xfk-local-key "q" #'dirvish-quit)
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

(jacob-defhookf verb-response-body-mode-hook
  (jacob-xfk-local-key "q" #'quit-window))

(after! compile
  (jacob-defhookf compilation-mode-hook
    (jacob-xfk-local-key "g" #'recompile)))

(jacob-defhookf +doom-dashboard-mode-hook
  (jacob-xfk-local-key "k" #'+doom-dashboard/forward-button)
  (jacob-xfk-local-key "i" #'+doom-dashboard/backward-button))

(jacob-defhookf org-agenda-mode-hook
  (jacob-xfk-local-key "q" #'quit-window)
  (jacob-xfk-local-key "g" #'org-agenda-redo-all))

(after! prodigy
  (jacob-defhookf prodigy-mode-hook
    (hl-line-mode 0)
    (jacob-xfk-local-key "d" #'prodigy-stop)
    (jacob-xfk-local-key "e" #'prodigy-mark)
    (jacob-xfk-local-key "g" #'jacob-project-search)
    (jacob-xfk-local-key "f" #'project-find-file)
    (jacob-xfk-local-key "i" #'prodigy-prev)
    (jacob-xfk-local-key "k" #'prodigy-next)
    (jacob-xfk-local-key "q" #'quit-window)
    (jacob-xfk-local-key "r" #'prodigy-unmark)
    (jacob-xfk-local-key "s" #'prodigy-restart)
    (jacob-xfk-local-key "v" #'prodigy-display-process))

  (jacob-defhookf prodigy-view-mode-hook
    (compilation-minor-mode 1)
    (jacob-xfk-local-key "q" #'quit-window)
    (jacob-xfk-local-key "g" #'prodigy-restart)))

(after! sly
  (jacob-defhookf sly-mode-hook
    (jacob-xfk-local-key "SPC , m" #'sly-eval-last-expression)
    (jacob-xfk-local-key "SPC , d" #'sly-compile-defun)
    (jacob-xfk-local-key "SPC , e" #'sly-eval-buffer)
    (jacob-xfk-local-key "SPC w k" #'sly-edit-definition))

  (jacob-defhookf sly-db-hook
    (jacob-xfk-local-key "q" #'sly-db-quit)))
