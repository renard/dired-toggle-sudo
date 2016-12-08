;;; dired-toggle-sudo.el --- Browse directory with sudo privileges.

;; Copyright © 2011 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, dired
;; Created: 2011-07-06
;; Last changed: 2015-11-09 11:03:27
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Allow to switch from current user to sudo when browsind `dired' buffers.
;;
;; To activate and switch with "C-c C-s" just put in your .emacs:
;;
;; (require 'dired-toggle-sudo)
;; (define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)
;; (eval-after-load 'tramp
;;  '(progn
;;     ;; Allow to use: /sudo:user@host:/path/to/file
;;     (add-to-list 'tramp-default-proxies-alist
;;		  '(".*" "\\`.+\\'" "/ssh:%h:"))))

;;; Code:

(require 'files)
(require 'tramp)
(require 'dired)
(require 'cl-lib)

(defface dired-toggle-sudo-header-face
  '((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root."
  :group 'tramp)

;;;###autoload
(defun dired-toggle-sudo-set-header ()
  "*Display a warning in header line of the current buffer.
This function is suitable to add to `find-file-hook' and `dired-file-hook'."
  (when (string-equal
         (file-remote-p (or buffer-file-name default-directory) 'user)
         "root")
    (setq header-line-format
          (propertize "--- WARNING: EDITING FILE AS ROOT! %-"
                      'face 'dired-toggle-sudo-header-face))))

(add-hook 'find-file-hook 'dired-toggle-sudo-set-header)
(add-hook 'dired-mode-hook 'dired-toggle-sudo-set-header)

(defun dired-toggle-sudo--internal (path &optional sudo-user)
  "Convert PATH to its sudoed version. root is used by default
unless SUDO-USER is provided."
  (let ((path (expand-file-name path)))
    (if (not (tramp-tramp-file-p path))
        ;; local, no sudo
        (tramp-make-tramp-file-name "sudo" sudo-user nil path)
      (with-parsed-tramp-file-name path nil
        (if (not (string= method "sudo"))
            ;; add sudo
            (tramp-make-tramp-file-name
             "sudo" sudo-user host localname
             (let ((tramp-postfix-host-format tramp-postfix-hop-format)
                   (tramp-prefix-format nil))
               (tramp-make-tramp-file-name
                method user host "" hop)))
          ;; already has sudo, remove
          (if hop
              ;; remove sudo (last hop)
              (tramp-make-tramp-file-name
               nil nil nil localname (replace-regexp-in-string "|$" "" hop))
            ;; just use localname
            localname))))))

;; simple tests
(when t
  (let (orig known-good xform fail)
    (dolist (x `(("/ssh:gwuser@gateway|ssh:user@remote|sudo:root@remote:/etc/fstab"
                  . "/ssh:gwuser@gateway|ssh:user@remote:/etc/fstab")
                 ("/ssh:gwuser@gateway|ssh:user@remote:/etc/fstab"
                  . "/ssh:gwuser@gateway|ssh:user@remote|sudo:remote:/etc/fstab")
                 ("/ssh:user@remote:/etc/fstab"
                  . "/ssh:user@remote|sudo:remote:/etc/fstab")
                 ("/ssh:user@remote|sudo:root@remote:/etc/fstab"
                  . "/ssh:user@remote:/etc/fstab")
                 ("/sudo::/etc/fstab"
                  . "/etc/fstab")
                 ("/etc/fstab"
                  . "/sudo::/etc/fstab")
                 ("~/foo"
                  . ,(concat "/sudo::" (getenv "HOME") "/foo"))))
      (setq orig (car x)
            known-good (cdr x)
            xform (dired-toggle-sudo--internal orig))
        (unless (string= xform known-good)
          (message "XX %s\n-> %s\n!= %s\n" orig xform known-good)
          (setq fail t)))
    (when fail
      (message "Fail!"))))

;;;###autoload
(defun dired-toggle-sudo (&optional sudo-user)
  "Reopen current file or dired buffer with sudo.

If SUDO-USER is nil assume root.

If called with `universal-argument' (C-u), ask for username.
"
  (interactive "P")
  (let* ((fname (or buffer-file-name
                   dired-directory))
         (sudo-user (if current-prefix-arg
                        (read-string "Username: ")
                      sudo-user))
         (save-point (point))
         (save-window-start (window-start)))
    (when fname
      (setq fname (dired-toggle-sudo--internal fname sudo-user))
      (cl-letf (((symbol-function 'server-buffer-done)
                 (lambda (buffer &optional for-killing) nil))
                ((symbol-function 'server-kill-buffer-query-function)
                 (lambda () t)))
        (find-alternate-file fname))
      (goto-char save-point)
      (set-window-start (selected-window) save-window-start))))

(provide 'dired-toggle-sudo)

;;; dired-toggle-sudo.el ends here
