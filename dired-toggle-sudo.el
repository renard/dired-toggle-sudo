;;; dired-toggle-sudo.el --- Browse directory with sudo privileges.

;; Copyright © 2011 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, dired
;; Created: 2011-07-06
;; Last changed: 2011-11-23 10:27:29
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 
;; Allow to switch from current user to sudo when browsind `dired' buffers.
;;
;; To activate and swit with "C-c C-s" just put in your .emacs:
;;
;; (require 'dired-toggle-sudo)
;; (define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)

;;; Code:

(eval-when-compile (require 'files))
(eval-when-compile (require 'tramp))
(eval-when-compile (require 'dired))
(eval-when-compile (require 'advice))

(defun dired-toggle-sudo-activate ()
  "Activate some advises both `tramp-error' and
`tramp-compute-multi-hops' in order to allow sudo on remote host
using a form like:

/sudo:x@y:z ==> /multi:sshx:y:sudo:z@y:z

In order to make sudo work on a remote host,
`tramp-compute-multi-hops' should be reloaded (Why?).

So please instal emacs elisp source files.

This is based on TWB hack (http://paste.lisp.org/display/90780).
"

  (eval-after-load "tramp"
     '(progn
       (defadvice tramp-error
	 (around dired-toggle-sudo:tramp-error activate)
	 "Allow to use sudo on a remote host.
See `dired-toggle-sudo-activate' for further information."
	 (if (and (eq 'file-error signal)
		  (string= "sudo" (tramp-file-name-method vec-or-proc)))
	     (progn
	     ;;(message (format "target-alist: %s" target-alist))
	       (setq target-alist
		     (cons (vector "sshx" ""
				   (tramp-file-name-host vec-or-proc)
				   "")
			   (list (vector (tramp-file-name-method vec-or-proc)
					 (unless (string= "root" (tramp-file-name-user vec-or-proc))
					   (tramp-file-name-user vec-or-proc))
					 (tramp-file-name-host vec-or-proc)
					 (tramp-file-name-localname vec-or-proc))))))
	   ad-do-it))))
  
  (eval-after-load "tramp-sh"
    '(progn
       ;; Reload `tramp-compute-multi-hops' to make
       ;; `dired-toggle-sudo:tramp-error' advice work.
       ;; WHY ????
       (if (ignore-errors (find-library "tramp-sh"))
	   (progn
	     (find-function 'tramp-compute-multi-hops)
	     (forward-sexp)
	     (eval-last-sexp nil)
	     (kill-buffer "tramp-sh.el.gz"))
	 (warn "Could not find tramp-sh.el not found. /sudo:x@y:z forms might not work.\n"
	       "Please install emacs elisp source files.")))))

(defun dired-toggle-sudo-internal (path &optional sudo-user)
  "Convert PATH to its sudoed version. root is used by default
unless SUDO-USER is provided."
  (let* ((file-vec (or (ignore-errors (tramp-dissect-file-name
				       path))
		       (tramp-dissect-file-name
			(concat "/:" path) 1)))
	 (method  (tramp-file-name-method file-vec))
	 (user (tramp-file-name-user file-vec)) 
	 (host  (tramp-file-name-host file-vec))
	 (localname (expand-file-name 
		     (tramp-file-name-localname file-vec))))
    (when (string= system-name host)
      (setq host nil))
    (cond
     ;; remote directory -> sudo
     ((and host (string= method "scp"))
      (setq method "sudo" user sudo-user))
     ;; remote directory -> normal
     ((and host (string= method "sudo"))
      (setq method "scp" user nil))
     ;; Local directory -> normal
     ((and (not host) (string= method "scp"))
      (setq method "sudo"))
     ;; Local directory -> sudo
     ((and (not host) (string= method "sudo"))
      (setq method nil user sudo-user))
     ;; Local directory -> normal
     (t
      (setq method "sudo" user sudo-user)))
    (replace-regexp-in-string
     "^/:/" "/"
     (tramp-make-tramp-file-name method user host localname))))

(defun dired-toggle-sudo-find (fname)
  "Create a new buffer for file name FNAME."
  (let ((save-point (point)))
    (message "Buffer-file-name: %s" (buffer-file-name))
    (find-alternate-file fname)
    (goto-char save-point)))

;;;###autoload
(defun dired-toggle-sudo (&optional sudo-user)
  "Reopen current file or dired buffer with sudo.

If SUDO-USER is nil assume root.

If called with `universal-argument' (C-u), ask for username.
"
  (interactive "P")
  ;; Make sure tramp is correctly advised.
  (unless
      (assoc 'dired-toggle-sudo:tramp-error
	     (ad-get-advice-info-field 'tramp-error 'around))
    (dired-toggle-sudo-activate))
  (let* ((fname (or buffer-file-name
		    dired-directory))
	 (sudo-user (if current-prefix-arg
			(read-string "Username: ")
		      sudo-user))
	 (orig (current-buffer)))
    (when fname
      (setq fname (dired-toggle-sudo-internal fname sudo-user))
      (if (not (eq major-mode 'dired-mode))
	  (dired-toggle-sudo-find fname)
	(kill-buffer orig)
	(dired fname)))))

(provide 'dired-toggle-sudo)
