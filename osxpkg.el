;;; osxpkg.el --- Manage Mac OSX packages

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-10-18
;; Last changed: 2012-10-29 20:03:16
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup osxpkg nil "osxpkg customization group"
  :group 'convenience)

(defcustom osxpkg-pkgutil (executable-find "pkgutil")
  "Path to pkgutil."
  :group 'osxpkg
  :type 'directory)

(defcustom osxpkg-lsbom (executable-find "lsbom")
  "Path to lsbom."
  :group 'osxpkg
  :type 'directory)

(defcustom osxpkg-recipes-dir "/var/db/receipts"
  "Path to lsbom."
  :group 'osxpkg
  :type 'directory)

(defcustom osxpkg-cksum (executable-find "cksum")
  "Path to cksum."
  :group 'osxpkg
  :type 'directory)

(defvar osxpkg-mode-hook nil
  "Hooks to run after `osxpkg-mode' init.")

(defvar osxpkg-mode-map nil
  "Keymap `osxpkg-mode'.")


(defstruct osxpkg
  package-id
  version
  volume
  location
  install-time
  groups
  files)

(defstruct osxpkg-file
  name
  nmode
  mode
  uid
  nuid
  gid
  ngid
  timestamp
  size
  cksum
  )

(defun osxpkg-list-packages(&optional match)
  "Return all installed packages as string list.

Limit packages list to MATCH if provided. See pkgutil(1) for
more information about the MATCH regexp."
  (let ((match (or match ".*")))
    (split-string
     (shell-command-to-string
      (format "%s --pkgs='%s'" osxpkg-pkgutil match))
     "[\n]+" t)))

(defun osxpkg-get-pkg-content (pkg &optional args)
  "Get PKG content."
  (let ((args (or args "")))
    (split-string
     (shell-command-to-string
      (format "%s %s --files '%s'" osxpkg-pkgutil args pkg))
     "[\n]+" t)))
    
(defun osxpkg-get-pkg-dirs (pkg)
  "List all directories from PKG"
  (osxpkg-get-pkg-content pkg "--only-dirs"))

(defun osxpkg-get-pkg-files (pkg)
  "List all files from PKG"
  (osxpkg-get-pkg-content pkg "--only-files"))

(defun osxpkg-get-pkg-content-detail (pkg)
  "Get detailed file list of PKG."
  (let ((files (split-string
		 (shell-command-to-string
		  (format "%s -pfmMuUgGtsc '%s/%s.bom'"
			  osxpkg-lsbom osxpkg-recipes-dir pkg))
		 "[\n]+" t)))
    (loop for file in files
	  collect (destructuring-bind
		      (f m M u U g G ti s c)
		      (split-string file "\t" )
		    (make-osxpkg-file
		     :name f
		     :nmode (string-to-int m)
		     :mode M
		     :uid (string-to-int u)
		     :nuid U
		     :gid (string-to-int g)
		     :ngid G
		     :timestamp (string-to-int ti)
		     :size (string-to-int s)
		     :cksum (string-to-int c))))))
  
(defun osxpkg-gen-pkg-plist (pkg)
  "Generate PAK plist suitable for `make-osxpkg'."
  (append
   (loop for s in (split-string
		   (shell-command-to-string
		    (format "pkgutil --info '%s'" pkg))
		   "[\n]+" t)
	 append (destructuring-bind (k v) (split-string s ": ")
		  (let ((k (intern (format ":%s" k))))
		   (cond
		    ((eq ':install-time k)
		     (list k (string-to-int v)))
		    ((eq ':groups k)
		     (list k (split-string v " " t)))
		    ((string= "" v)
		     (list k nil))
		    (t (list k v))))))
   (list :files  (osxpkg-get-pkg-content-detail pkg))))

(defun osxpkg-get-packages-info (&rest packages)
  "Create osxpkg struct from PACKAGE."
  (loop for p in packages
	collect
	(apply #'make-osxpkg (osxpkg-gen-pkg-plist p))))

(defun osxpkg-get-cksum(filename)
  "Compute FILENAME cksum."
  (when (file-exists-p filename)
    (string-to-int
     (car
      (split-string
       (shell-command-to-string
	(format "%s '%s'" osxpkg-cksum filename)))))))

(defun osxpkg-print-file (file prefix)
  "Pretty print FILE details."
  (let* ((filename (format "%s/%s" prefix (osxpkg-file-name file)))
	 ;; (cksum (when (> (osxpkg-file-cksum file) 0)
	 ;; 	  (osxpkg-get-cksum filename)))
	 cksum
	 (face
	  (cond
	   ((and cksum (not (= cksum (osxpkg-file-cksum file)))) 'magit-diff-del)
	   ((file-directory-p filename) 'dired-directory)
	   ((file-symlink-p filename) 'dired-symlink)
	   ((not (file-exists-p filename)) 'dired-warning)
	   (t 'default))))
    (insert
     "  "
     (propertize (expand-file-name filename) 'font-lock-face face)
     "\n")))

(defun osxpkg-show-pkg (&optional pkg)
  "Show all information about PKG."
  (interactive)
  (let* ((pkg (or pkg
		  (completing-read
		   "Show package: "
		   (osxpkg-list-packages) nil  t)))
	 (buffer (format "Details of package %s" pkg))
	 (info (car (osxpkg-get-packages-info pkg)))
	 (buffer-read-only nil))
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (let* ((volume (osxpkg-volume info))
	     (location (osxpkg-location info))
	     (prefix (concat volume location)))
	(loop for f in (osxpkg-files info)
	      do (osxpkg-print-file f prefix)))
      (osxpkg-mode)
      (setq header-line-format
	    (format "Package: %s (%s) installed at %s"
		    (osxpkg-package-id info)
		    (osxpkg-version info)
		    (format-time-string "%Y-%m-%d %H:%M:%S"
					(seconds-to-time
					 (osxpkg-install-time info)))))
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(unless osxpkg-mode-map
  (setq osxpkg-mode-map (make-keymap))
  (suppress-keymap osxpkg-mode-map)
  (define-key osxpkg-mode-map "n" 'next-line)
  (define-key osxpkg-mode-map "p" 'previous-line)
  (define-key osxpkg-mode-map "v" 'osxpkg-view-thing-at-point)
  (define-key osxpkg-mode-map (kbd "<return>") 'osxpkg-view-thing-at-point)
  (define-key osxpkg-mode-map "d" 'osxpkg-mode-mark-delete)
  (define-key osxpkg-mode-map "u" 'osxpkg-mode-unmark)
  (define-key osxpkg-mode-map "q" 'quit-window))

(defun osxpkg-mode-get-name-at-line ()
  "Get file name at point."
  (save-excursion
    (goto-char (point-at-bol))
    (search-forward "/")
    (buffer-substring-no-properties
     (1- (point)) (point-at-eol))))

(defun osxpkg-view-thing-at-point ()
  "Open dired or file at point."
  (interactive)
  (let ((filename (osxpkg-mode-get-name-at-line)))
    (when filename
      (if (file-directory-p filename)
	  (dired filename)
	(view-file filename)))))

(defun osxpkg-mode-mark (what)
  (unless (eobp)
    (let ((buffer-read-only nil))
      (beginning-of-line)
      (delete-char 1)
      (insert what)
      (forward-line)
	  (setq buffer-read-only t))))

(defun osxpkg-mode-unmark ()
  (interactive)
  (osxpkg-mode-mark " "))

(defun osxpkg-mode-mark-delete ()
  (interactive)
  (let ((filename (osxpkg-mode-get-name-at-line)))
    (when filename
      (if (file-directory-p filename)
	  (osxpkg-mode-mark "d")
	(osxpkg-mode-mark "D")))))

(defun osxpkg-mode ()
  "Major mode for browsing MacOSX installed packages."
  (kill-all-local-variables)
  (use-local-map osxpkg-mode-map)
  (setq major-mode 'osxpkg-mode
	mode-name "OSX Package"
	buffer-read-only t
	truncate-lines t)
  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'osxpkg-mode-hook)
    (run-hooks 'osxpkg-mode-hook)))



(provide 'osxpkg)

;; osxpkg.el ends here
