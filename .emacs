;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; Time-stamp: <2016-10-15 23:30:21 neil>
;;;
;;; Emacs configuration file by Neil Woods <neil@netlexer.uk>.
;;; Written originally for GNU Emacs (ver 19.x), with many
;;; ideas from usenet, emacswiki, etc.  
;;; (c) Neil Woods, 1992-2016.                                  

;; Announce start of file loading...
(message "Loading Emacs personal init file...")

;; My default load dir - add to load-path - generic, local lisp.
;; Could be replaced by newer versions. Non-flavor specific (?).
;; root uses this also, thus use ~neil.
(let ((nw_dir (expand-file-name "~neil/.emacs.d/lisp")))
  (if (file-exists-p nw_dir)
      (progn (setq load-path (append (list nw_dir) load-path)))))

;; Load a sensible info search path (see also $INFOPATH)
(setq Info-directory-list '("/usr/local/share/info/" "/usr/share/info/"))
(require 'info)

;; A neat macro for X
(defmacro Xwin-code (&rest x)
  (list 'if (eq window-system 'x)(cons 'progn x)))

;; Initialise variable for, and load emacs-uptime. (M-x emacs-uptime)
(defvar *emacs-start-time* (current-time) "blink-blink yawn")
;; http://gnuvola.org/software/personal-elisp/dist/lisp/diversions/emacs-uptime.el
(require 'emacs-uptime)

(require 'package)
;; default plus add melpa for latest elisp packages
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/") t)

(unless package-archive-contents    ;; Refresh the packages descriptions
  (package-refresh-contents))
(setq package-load-list '(all))     ;; List of packages to load

(add-hook 'package-menu-mode-hook 'highline-mode)

(package-initialize)

(load-theme 'cyberpunk t)

;; Enable this to automatically save the default desktop. See EOF for
;; `desktop-read', which is enabled (if emacs finds .emacs-desktop).
;;(setq desktop-enable t)

;; Use this directory for the various utility functions/packages.
;; Note that this is specific to the flavor of Emacs running.
;; Also, it's $HOME specific
(setq my-emacs-dir "~/.emacs.d/")

;; Define a string value for myself (for use in scripts, etc)
(setq nw-identifier "Neil Woods <neil@netlexer.uk>")

;; Don't show the GNU splash screen
(setq inhibit-startup-message t)

;; Display mode-line with time & line#/col# :
; (display-time-mode t)
(setq line-number-mode t)
(setq column-number-mode t)
;; If the next line is uncommented, the position is saved in ALL files,
;; not just ones which have it set interactively on a buffer-local basis.
(setq-default save-place t)

;; Keep a list of recently opened files (kept across sessions, too!)
(require 'recentf)
(recentf-mode 1)

;; Default major mode, if not set by other means. (commented for generic-mode)
(setq default-major-mode 'text-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ASSORTED GLOBAL KEY BINDINGS
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make control+pageup/down scroll the other buffer
(global-set-key [C-next] 'scroll-other-window)
(global-set-key [C-prior] 'scroll-other-window-down)

;; Change to next/previous buffer (also, in emacs 22, bound to C-x left,right
(global-set-key [A-left] 'previous-buffer)
(global-set-key [A-right] 'next-buffer)

(global-set-key "\C-c\C-c" 'comment-region)

;; Rebind C-z to start a shell (use .emacs_shellname for the shells rc file)
;; -- which in this case is ~/.emacs_bash (= a customized ~/.bashrc).
(global-set-key "\C-z" 'shell)

(define-key global-map [(delete)]    "\C-d")
;; Maybe useful ??
(global-set-key [(meta g)] `goto-line)
(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key [(meta control ?l)] `switch-to-other-buffer) ; M-C-l
(global-set-key [(meta O) ?H] 'beginning-of-line)
(global-set-key [(meta O) ?F] 'end-of-line)
(global-set-key [end] 'end-of-line)
;; Insert date/time string, bound to Meta-f9
(defun nw-insert-time ()
  (interactive)
  ;; parameters are: FORMAT (like date(1)), TIME (ie now), and an optional
  ;; third argument, UNIVERSAL. Example: Mon, 12 Nov 2001 17:25 +0000
  ;; Example similar to "date -R" cmd. & Debian changelog time format.
  (insert (format-time-string "%a, %-d %b %Y %T %z " (current-time) t)))
(define-key global-map [(meta f9)] 'nw-insert-time)

;; Now define a function to insert the time + personal identity (eg email).
;; A small comment may be added at the end, if needed.
;; neil <neil@nova.lan> -- 11/12/01 17:51:26 : modified format (like this!),
;;  -- that is, it doesn't use nw-insert-time anymore.
(defun nw-insert-ident ()
  (interactive)
  (save-excursion)
  (beginning-of-line)
  (if (stringp comment-start)
      (if (= 1 (string-width comment-start))
	  (insert (concat comment-start comment-start " "))
	(insert comment-start)))
  (insert (concat nw-identifier " -- "
		  (format-time-string "%x %X " (current-time) t)
		  ": ")))
;; Bind it to M-F10
(define-key global-map [(meta f10)] 'nw-insert-ident)

;; Set kp-enter  to `newline-and-indent' - globally; use the vector
;; definition -- this is also bound to C-j.

(define-key global-map [kp-enter] 'newline-and-indent)

(setq next-line-add-newlines nil)

;; The only useful thing to come from PC editing ;-)
;;(pc-selection-mode)      ;; i.e. shift-up/shift-down,etc.

;; Bind the Function keys for useful shortcuts
;; Make F1 invoke help : f1 = help; shift-f1 = "man"; ctrl-f1 = info.
;; TODO: Make possibly better/useful bindings here...
(global-set-key [f1]    'help-command)
(global-set-key [S-f1]  'man)
(global-set-key [ (control f1) ] 'info)

(global-set-key [f2]    'start-kbd-macro)	; Also C-X(
(global-set-key [f3]    'end-kbd-macro)		; Also C-X)
(global-set-key [f4]    'call-last-kbd-macro)	; Also C-Xe

(global-set-key [S-f4]  'name-last-kbd-macro)	; So we can save it?

(global-set-key [f5]    'dictionary-lookup-definition)
(global-set-key [f6]    'first-error)
(global-set-key [f7]    'previous-error)
(global-set-key [f8]    'next-error)

(global-set-key [f10]   'ispell-buffer)         ; normally F10 = menu
(global-set-key [S-f10] 'delete-other-windows)
(global-set-key [f11]   'undo)
(global-set-key [f12]   'other-window)
(global-set-key [S-f12] 'delete-window)

(if (string= (getenv "TERM") "screen")
    (progn
       (global-set-key "\e[V"  'scroll-down)
       (global-set-key "\e[U"  'scroll-up)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Define the display variables(fonts/colours/frames, etc)           ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UPDATE: Feb 27,2001 - Modified for black on white, 100dpi, etc.
;; Also, try to keep resources within ~/.Xdefults.
;;

;; (This block of code is ONLY applicable under X)

;;(Xwin-code
;; Load my X specific code:
;; This sets the colour/size & other properties of the X frames.

;; Re-define the normal `save-buffers-kill-emacs' function when in X to
;; delete the current frame. Will NOT delete the LAST frame.
(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'delete-frame)

(global-set-key [(alt ?i)] 'iconify-frame)   ; Alt-i

;; Set the text for titlebar and icons - Icon only needs name of buffer -
;; the icon graphic xbm will, of course, indicate it's GNU Emacs.
(setq frame-title-format '("emacs@" system-name " [%b]" ))
(setq icon-title-format '("[%b]"))

;; these look pretty cool... (see also ~/.Xdefaults)
;;(set-face-background 'modeline "midnightblue")
;;(set-face-foreground 'modeline "goldenrod2")

;; I don't wan't the GTK+ (or any) dialog boxes on mouse clicks when invoked
;; from the menu (this is new in version 22).
;; (next line uses old gtk dialog iff next two lines are set to t.
(setq x-use-old-gtk-file-dialog t)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
;; If set to t, this autoselects windows within frames by moving mouse there
(setq mouse-autoselect-window -1.5)     ;; default setting => nil.
;;) ;;   End: X-win-code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font-lock-mode stuff (in particular)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(require 'highline)           ;; used by (at least) mldonkey, gnus.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  DIRED MODE
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dired-x)
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

(add-hook 'dired-mode-hook
          (lambda ()
            (setq dired-omit-files "^\\.[a-z|A-Z]+\\|^\\.?#\\|^\\.$")
	    ;; could add dired-omit-extentions here, also.
	    (setq dired-omit-files-p nil)   ;; disabled by default
	    (highline-mode)
	    (define-key dired-mode-map [delete] 'dired-flag-file-deletion)
	    (define-key dired-mode-map [return] 'dired-find-file-other-window)
	    (define-key dired-mode-map [C-down-mouse-1]
	      'dired-mouse-find-file-other-window)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IBUFFER 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ibuffer-formats '((mark modified read-only " " (name 16 16) " "
			      (size 6 -1 :right) " " (mode 16 16 :center)
			      " " (process 8 -1) " " filename)
                        (mark " " (name 16 -1) " " filename))
      ibuffer-elide-long-columns t
      ibuffer-eliding-string "&")

(add-hook 'ibuffer-mode-hook 'highline-mode)

(global-set-key [(hyper ?b)] 'ibuffer)

;; define the coding system
;; http://bbdb.sourceforge.net/faq.html
(setq file-coding-system-alist 
      (cons '("\\.bbdb" utf-8 . utf-8) 
        file-coding-system-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spell checking & dictionary lookup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Standard location of personal dictionary
(setq ispell-personal-dictionary "~/.flydict")

(if (file-exists-p "/usr/bin/hunspell")
    (progn
      ;; Add english-hunspell as a dictionary
      (setq-default ispell-program-name "hunspell"
                    ispell-dictionary "en_US"))
  (progn (setq-default ispell-program-name "aspell")
         (setq ispell-extra-args '("--sug-mode=normal" "--ignore=3"))))

(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))

;; flyspell
(require 'flyspell)
(define-key flyspell-mode-map (kbd "M-n") 'flyspell-goto-next-error)
(define-key flyspell-mode-map (kbd "M-.") 'ispell-word)

(load "dictionary-init")
(global-set-key "\C-cs" 'dictionary-search)
(global-set-key "\C-cm" 'dictionary-match-words)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WWW -- Read URL's with specified browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/bin/light")

(global-set-key "\C-xm" 'browse-url-at-point)

;; super-click-button-1 (Windows key + click btn 1 )  
(global-set-key [s-mouse-1] 'browse-url-at-mouse)

;; turn on ffap (emacs-goodies) (best loaded after browse-url or w3)(drazi)
(ffap-bindings)
(setq ffap-url-regexp nil)           ; disable URL features in ffap

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  A few useful functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Count words in buffer
(defun count-words-buffer ()
  "Count the number of words in current the buffer;
 print a message in the minibuffer with the result."
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(forward-word 1)
	(setq count (1+ count)))
      (message "buffer contains %d words." count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion modes: dabbrev, hippie-expand, complete, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hippie expand enables completion of filenames/dirs in buffers
(require 'hippie-exp)
(global-set-key [(hyper return)] 'hippie-expand)
(global-set-key [(control tab)] 'hippie-expand)
(setq hippie-expand-verbose t)

;; This binds word completions to Shift-Tab.
(global-set-key [S-iso-lefttab] 'dabbrev-completion)

;; Enables completion of recently used words (bound to M-RET & C-RET)
(require 'completion)
(dynamic-completion-mode)
(initialize-completions)

;; Icomplete-mode hooks - contrain minibuffer height...
(add-hook 'icomplete-minibuffer-setup-hook
	  (function
	   (lambda ()
	     (make-local-variable 'resize-minibuffer-window-max-height)
	     (setq resize-minibuffer-window-max-height 3))))


(add-hook 'message-setup-hook 'mail-abbrevs-setup)

;; make backup files in ~/.backups/ rather than scattered around all
;; over the filesystem.
(setq make-backup-files t
      version-control t
      kept-old-versions 2
      kept-new-versions 4
      delete-old-versions t
      backup-directory-alist '(("." . "~/.backups"))
      backup-by-copying nil
      backup-by-copying-when-linked t)


;; disable backups for files in /tmp or in my Mail or News directories.
(defun nw-backup-enable-predicate (filename)
  (and (not (string= "/tmp/" (substring filename 0 5)))
       (not (string-match "~/Mail/" filename))
       (not (string-match "~/News/" filename))))

(setq backup-enable-predicate 'nw-backup-enable-predicate)

(require 'minibuf-electric-gnuemacs)

;; Define function to match a parenthesis otherwise insert a % (like vi !;-P )

(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

;; Load the auto-save.el package, which lets you put all of your autosave
;; files in one place, instead of scattering them around the file system.
;;
;; (setq auto-save-directory (concat my-emacs-dir "autosave")
;;       auto-save-directory-fallback auto-save-directory
;;       auto-save-hash-p nil
;;       efs-auto-save t
;;       efs-auto-save-remotely nil
;;       auto-save-list-file-name (concat my-emacs-dir "auto-save-list")
;;       ;; now that we have auto-save-timeout, let's crank this up
;;       ;; for better interactive response. (default value is 300).
;;       ;auto-save-interval 1000
;;       )
;; We load this afterwards because it checks to make sure the
;; auto-save-directory exists (creating it if not) when it's loaded.
;; (require 'auto-save)

;; Query replace on region

(defun query-replace-from-region (&optional to)
  (interactive "sQuery replace region with: ")
  (let ((from (buffer-substring (region-beginning)
                                (region-end))))
    (save-excursion
      (goto-char (region-beginning))
      (query-replace from to))))


;; for some reason Emacs lacks delete-line, implementing it with the
;; source from kill-line is, however, trivial
(defun delete-line (&optional arg)
  "Delete the rest of the current line; if no nonblanks there, delete
thru newline. With prefix argument, delete that many lines from point.
Negative arguments delete lines backward.

When calling from a program, nil means \"no arg\", a number counts as
a prefix arg.

To delete a whole line, when point is not at the beginning, type \
\\[beginning-of-line] \\[delete-line] \\[delete-line].

If `kill-whole-line' is non-nil, then this command deletes the whole line
including its terminating newline, when used at the beginning of a line
with no argument.  As a consequence, you can always delete a whole line
by typing \\[beginning-of-line] \\[delete-line]."
  (interactive "P")
  (delete-region (point)
	       ;; It is better to move point to the other end of the
	       ;; delete before deleting. That way, in a read-only
	       ;; buffer, point moves across the text that is to be
	       ;; delete. The choice has no effect on undo now that
	       ;; undo records the value of point from before the
	       ;; command was run.
	       (progn
		 (if arg
		     (forward-visible-line (prefix-numeric-value arg))
		   (if (eobp)
		       (signal 'end-of-buffer nil))
		   (if (or (looking-at "[ \t]*$") (and kill-whole-line (bolp)))
		       (forward-visible-line 1)
		     (end-of-visible-line)))
		 (point))))

(global-set-key (kbd "C-c d") 'delete-line)


;; press HOME key to either move to bol or beginning of indentation.

(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(global-set-key [home] 'back-to-indentation-or-beginning)


;; from "rgb" <rbielaws@i1.net> in
;; <1112372995.606713.126040@g14g2000cwa.googlegroups.com>

(defun insert-sequence-key (key)
  "Inserts a keystroke suitable for use in fcns like global-set-key"
  (interactive "kInsert key chord: ")
  (insert (format "(kbd \"%s\")" (key-description key))))


;; really useful: M-. -> jump to definition of identifier at point
;;                M-, -> jump back.
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))


;; Paredit & related

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(require 'eldoc) ; if not already loaded
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
	(save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

;; Using local-set-key in a mode-hook is a better idea.
(global-set-key (kbd "RET") 'electrify-return-if-match)

(setq show-paren-mode t
      global-paren-face-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  MODES & File HOOKS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXtra modes for editing various generic UNIX specific files:
(require 'generic-x)

(autoload 'xrdb-mode "xrdb-mode" "Mode for editing X resource files" t)

;; You may also want something like:

(setq auto-mode-alist
      (append '(("\\.Xdefaults$"    . xrdb-mode)
		("\\.Xenvironment$" . xrdb-mode)
		("\\.Xresources$"   . xrdb-mode)
		("*.\\.ad$"	    . xrdb-mode)
		)
	      auto-mode-alist))

(define-generic-mode 'xmodmap-mode
  '(?!)
  '("add" "clear" "keycode" "keysym" "pointer" "remove")
  nil
  '("[xX]modmap\\(rc\\)?\\'")
  nil
  "Simple mode for xmodmap files.")


(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)


(add-hook 'find-file-hooks 'auto-insert)

;; to add to auto-insert-alist, there's already a defun... needs work?
(define-auto-insert 'sh-mode "Shell-script.inc" t)  ; in ~/emacs/insert/


(setq revert-without-query (cons "TAGS" revert-without-query))
(setq transient-mark-mode t)      ; show marked text

;; Automatic opening of zipped files.
(auto-compression-mode 1)

;; Printing (ps-print or a2ps ?)
;;
(setq ps-paper-type 'a4)
(setq ps-print-color-p nil)  ;; set this to t if printing on a colour printer

;; Add Time-stamp <> or Time-stamp " " anywhere in the top 8 lines of a
;; file to insert save date and time and user:

(add-hook 'write-file-hooks 'time-stamp)

;; Calendar, diary, and todo-modes...

(autoload 'todo-mode "todo-mode"
  "Major mode for editing TODO lists." t)
(autoload 'todo-show "todo-mode"
  "Show TODO items." t)
(autoload 'todo-insert-item "todo-mode"
  "Add TODO item." t)
(setq diary-file "~/.diary")

(global-set-key "\C-ct" 'todo-show) ;; switch to TODO buffer
(global-set-key "\C-ci" 'todo-insert-item) ;; insert new item

;; Mode for viewing FAQ's...
(autoload 'faq-mode "faq-mode"
  "Major mode for reading faq files." t)

;; and rfc's...
(setq auto-mode-alist (cons '("/rfc[0-9]+\\.txt\\'" . rfcview-mode)
			    auto-mode-alist))

(autoload 'rfcview-mode "rfcview" nil t)


;; TeX support
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  VERSION CONTROL & related modes/hooks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Support .md files, as used on github and elsewhere

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; avoid 'Symbolic link to Git-controlled source file' messages, just do it.
(setq vc-follow-symlinks t)


;; A few variables which affect the *shell* (emacs terminal) in a window
;; (also includes the general 'comint' = COMMand INTerpreter functions).
(setq shell-prompt-pattern "^[^#$%>\n]*[#$%>] *")

;; Shell-mode hooks:

(add-hook 'comint-output-filter-functions
	  'comint-strip-ctrl-m)

(add-hook 'comint-output-filter-functions
	  'comint-truncate-buffer)

(add-hook 'shell-mode-hook
	  'ansi-color-for-comint-mode-on)

(setq comint-scroll-to-bottom-on-input 't
      comint-scroll-show-maximum-output 't
      comint-scroll-to-bottom-on-output 'all
      comint-input-ignoredups 't)

(define-key comint-mode-map [up] 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-input)

;; smilies. Use F9 then a, b, c etc (more to add later...)
;; a = U+1F603 SMILING FACE WITH OPEN MOUTH
;; b = U+1F609 WINKING FACE
;; c = U+1F606 SMILING FACE WITH OPEN MOUTH AND TIGHTLY-CLOSED EYES
;;
;; (see http://ergoemacs.org/emacs/emacs_n_unicode.html)

(global-set-key (kbd "<f9> a") (lambda () (interactive) (insert "😉")))
(global-set-key (kbd "<f9> b") (lambda () (interactive) (insert "😉")))
(global-set-key (kbd "<f9> c") (lambda () (interactive) (insert "😆")))



;;; literal characters
(defun insert-literal-char (arg)
  "Insert a character into a buffer by specifying its ascii code"
  (interactive "nEnter decimal value of chracter to insert: ")
  (insert (format "%c" arg)) )

;; Support Arch Linux PKGBUILD 
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;; header creation & update (autoloaded via package-initialise)
(autoload 'auto-update-file-header "header2")
(add-hook 'write-file-hooks 'auto-update-file-header)
(autoload 'auto-make-header "header2")
(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
(add-hook 'c-mode-common-hook   'auto-make-header)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Programming: Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Make Emacs look in Cabal directory for binaries
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(setq haskell-process-auto-import-loaded-modules t
      haskell-process-log t
      haskell-process-suggest-remove-import-lines t
      haskell-process-type (quote cabal-repl)
      haskell-tags-on-save t)

; Choose indentation mode
;; Use haskell-mode indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; Use hi2
;(require 'hi2)
;(add-hook 'haskell-mode-hook 'turn-on-hi2)
;; Use structured-haskell-mode
;(add-hook 'haskell-mode-hook 'structured-haskell-mode)

; Add F8 key combination for going to imports block
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

; Add key combinations for interactive haskell-mode
(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
(eval-after-load 'haskell-cabal
  '(progn
     (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
     (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
(eval-after-load 'haskell-cabal
  '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))

;; GHC-MOD
;; -------

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Programming: C/C++ modes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use Gnu Coding Standards.
;; RET now works as C-j to re-indent & indent in C/C++ and related modes.
(require 'cc-mode)
(add-hook 'c-mode-common-hook
          (lambda ()
	    (c-set-style "bsd")
            (define-key c-mode-base-map
	      "\C-m" 'c-context-line-break)))

;; could add a (local-set-key [return] 'reindent-then-newline-and-indent) too

(setq c-default-style "bsd")

(setq cperl-hairy t)

(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
	      (ggtags-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rs-info : Enhancements to info mode (esp. with Gnus) by Reiner Steib
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'rs-info-insert-current-node "rs-info"
  "Insert reference to current Info node using STYPE in buffer." t nil)
(autoload 'rs-info-boxquote "rs-info"
  "Yank text (from an info node), box it and use current info node as title."
  t nil)
(autoload 'rs-info-reload "rs-info" "Reload current info node." t nil)
(autoload 'rs-info-insert-node-for-variable "rs-info"
  "Insert a custom style info node for the top level form at point." t nil)
(defalias 'boxquote-info 'rs-info-boxquote)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  GENERAL: Menu Interface/Mouse related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map [M-S-down-mouse-3] 'imenu)

;; Disable menubar and toolbar on the console, enable menu under X.
(menu-bar-mode 1)
(setq window-system-default-frame-alist
      '((x (menu-bar-lines . 1) (tool-bar-lines . 0))
        (nil (menu-bar-lines . 0) (tool-bar-lines . 0))))

(scroll-bar-mode nil)

;; for use in xterm
(require 'xt-mouse)
(unless window-system (xterm-mouse-mode 1))

;; Replace yes/no+enter prompts with y/n prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Emacs 22+ - revert space completing filenames in minibuffer (see FAQ)
(define-key minibuffer-local-filename-completion-map (kbd "SPC")
  'minibuffer-complete-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mule and UNICODE Support :: not needed, generally. (See earlier revs).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; but this is useful to define...
;; Next ln equiv: ISO 2022 based 8-bit encoding for Latin-1 (MIME:ISO-8859-1)
(set-language-environment "English")
(setq system-time-locale "Europe/London")
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom section (added automagically by "custom" package) ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Maybe put these in a separate file again?
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(background-color "#002b36")
 '(background-mode dark)
 '(blink-matching-paren (quote jump))
 '(canlock-password "729dd1edbb7f5765107438e259df42fe65cf1dac")
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-color "#839496")
 '(custom-enabled-themes (quote (cyberpunk)))
 '(custom-safe-themes
   (quote
    ("38e64ea9b3a5e512ae9547063ee491c20bd717fe59d9c12219a0b1050b439cdd" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e64111716b1c8c82638796667c2c03466fde37e69cada5f6b640c16f1f4e97df" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(display-time-mode t)
 '(erc-server "haxan.lan")
 '(erc-user-full-name "\"What's up doc?\"")
 '(focus-follows-mouse t)
 '(font-use-system-font nil)
 '(foreground-color "#839496")
 '(gnus-group-list-inactive-groups nil)
 '(gnus-treat-newsgroups-picon nil)
 '(haskell-process-auto-import-loaded-modules t t)
 '(haskell-process-log t t)
 '(haskell-process-suggest-remove-import-lines t t)
 '(haskell-process-type (quote cabal-repl) t)
 '(haskell-tags-on-save t t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(ibuffer-elide-long-columns t)
 '(ibuffer-eliding-string "&")
 '(ibuffer-saved-filter-groups nil)
 '(indicate-empty-lines t)
 '(magit-diff-use-overlays nil)
 '(mouse-autoselect-window -1.5)
 '(mouse-yank-at-point t)
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox")
     (:name "unread" :query "tag:unread"))))
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (spacemacs-theme magithub markdown-mode paren-face paredit-menu twittering-mode better-defaults better-shell solarized-theme pkgbuild-mode paredit elisp-slime-nav flymake-haskell-multi ghc ghc-imported-from haskell-tab-indent hindent w3m irfc tuareg header2 xkcd gist gitattributes-mode github-clone github-notifier github-search gitty yaml-mode js2-mode htmlize erc-youtube erc-tweet erc-crypt eprime-mode discord cyberpunk-theme)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(save-place-mode t)
 '(scroll-bar-mode (quote right))
 '(send-mail-function (quote sendmail-send-it))
 '(show-paren-mode t)
 '(show-paren-ring-bell-on-mismatch t)
 '(show-paren-style (quote mixed))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(starttls-extra-arguments nil)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(window-divider-default-places (quote right-only))
 '(window-divider-default-right-width 3)
 '(x-gtk-use-system-tooltips nil)
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#d3d3d3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "PfEd" :family "Inconsolata LGC"))))
 '(italic ((t (:slant italic :weight normal :height 98 :width normal :foundry "PfEd" :family "Inconsolata LGC"))))
 '(parenthesis ((t (:inherit shadow :foreground "gray45")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of custom section.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(load "/home/neil/.emacs-records")

(message "Emacs initialised and ready...")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of ~/.emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
