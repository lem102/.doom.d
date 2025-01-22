;;; lemovem.el --- Custom movement commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom movement commands.

;;; Code:

(defvar-local lemovem-backspace-function nil
  "Called by `lemovem-backspace' if non-nil.")

(defun lemovem-backspace ()
  "DWIM backspace command.

If character to the left is a pair character as determined by
`insert-pair-alist', kill from the pair to its match.  If the
prefix argument is provided, just delete the pair characters."
  (interactive)
  (undo-boundary)
  (if (region-active-p)
      (delete-active-region)
    (when (= 1 (point))
      (user-error "Beginning of buffer"))
    (let ((char-class (char-syntax (char-before)))
          (f (if current-prefix-arg
                 #'delete-pair
               #'kill-sexp)))
      (unless (ignore-errors
                (funcall lemovem-backspace-function f))
        (cond ((= ?\" char-class)                         ; string
               (if (nth 3 (syntax-ppss))
                   (backward-char)
                 (backward-sexp))
               (funcall f))
              ((= ?\( char-class)                         ; delete from start of pair
               (backward-char)
               (funcall f))
              ((= ?\) char-class)                         ; delete from end of pair
               (backward-sexp)
               (funcall f))
              (t                                          ; delete character
               (backward-delete-char 1)))))))

(defun lemovem-beginning-of-line ()
  "Go to indentation, line start, backward paragraph."
  (interactive)
  (cond ((bolp)
         (backward-paragraph))
        ((= (save-excursion
              (back-to-indentation)
              (point))
            (point))
         (move-beginning-of-line 1))
        (t
         (back-to-indentation))))

(defun lemovem-end-of-line ()
  "Go to content end, line end, forward paragraph."
  (interactive)
  (if (eolp)
      (forward-paragraph)
    (let ((content-end (save-excursion
                         (when (comment-search-forward (line-end-position) "NOERROR")
                           (goto-char (match-beginning 0))
                           (skip-syntax-backward " " (line-beginning-position))
                           (unless (= (point) (line-beginning-position))
                             (point))))))
      (if (or (null content-end)
              (= content-end (point)))
          (move-end-of-line 1)
        (goto-char content-end)))))

(defun lemovem-kill-line ()
  "If region is active, kill it.  Otherwise:

If point is at the beginning of the line, kill the whole line.

If point is at the end of the line, kill until the beginning of the line.

Otherwise, kill from point to the end of the line."
  (interactive)
  (cond ((region-active-p)
         (call-interactively #'kill-region))
        ((bolp)
         (kill-whole-line))
        ((eolp)
         (kill-line 0))
        (t
         (kill-line))))

(defun lemovem-kill-paragraph ()
  "Move to the beginning of the paragraph, then kill it."
  (interactive)
  (forward-paragraph)
  (backward-paragraph)
  (kill-paragraph 1))

(provide 'lemovem)

;;; lemovem.el ends here
