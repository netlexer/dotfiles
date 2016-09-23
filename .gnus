;; File: .gnus - Configuration file for Gnus.
;; Author: Neil Woods.
;; Initially created 23 Jul 04.
;;
;; $Id: .gnus,v 2.1 2013/04/23 19:54:35 neil Exp neil $
;; Time-stamp: <2016-06-03 22:10:07 neil>
;;
;; Now using Gnus CVS (as of 17-03-2005).

;; Identity :: General
;;
(setq user-full-name "Neil Woods")	     ;; C. Neil Woods ?
(setq message-user-fqdn "phun.phasmic.org")  ;; for my Message-ID's
(setq message-from-style 'angles)	     ;; angles = default (cf parens)

;; My default GnuPG ID
;;(require 'pgg)
;; my updated signing key (See ~/.gnupg/gpg.conf).
;;(setq pgg-gpg-user-id "0xEA763CB0")

;; from http://www.cs.cmu.edu/~karl/gnus/
(require 'messkeyw)			;; for keyword stuff
(add-hook 'message-send-hook 'message-keyword-insert)
(add-hook 'nnmail-prepare-incoming-message-hook 'message-auto-keyword-insert)

;; Identity :: Posting Styles
;;
;; TODO: This needs pruning somewhat!
(setq gnus-posting-styles
      '((".*"					;;default
	 (name "Neil Woods"))

	((message-news-p)			;; Usenet news?
	 ("Path" "")       ;; preload path - for leafnode.
	 ("Mail-Copies-To" "nobody")
	 (signature nw-insert-sig)           ;; see below
	 ("X-Endless-September-Date" (shell-command-to-string "sdate"))
	 ("X-Emacs-Uptime" (nw-emacs-uptime))
	 ;; (name "canaxis")
	 ;; (address "canaxis.prime@noname.invalid")

	 (address (concat "cnw+usenet-"
	 		  (format-time-string "%y%m" (current-time) t)
	 		  "@pobox.com"))     ;; eg. cnw+usenet-0601@pobox.com
	 ;; (address "nospam@phasmic.org")

	 ;;(address "elided@noname.invalid")
	 (organization "(TINO)"))

	((message-mail-p)		;; mail?
	 (name "Neil Woods")
	 (address "neil@phasmic.org")      ;; phasmic | pobox (redirected)
	 ("Reply-To" nil)
	;; (signature-file "~/.signature") )
	(signature nw-insert-sig) )
	;; Added this bit (11 Dec 05) see Info node (gnus) Posting Styles.
	;; Uses the To address as the From address in replies.
	;; This is inaccurate. But see below [message-alternative-emails].
	("nnimap.*"
	 (From (save-excursion
		 (set-buffer gnus-article-buffer)
		 (message-fetch-field "to"))))
	("nnml:.*"
	 (From (save-excursion
		 (set-buffer gnus-article-buffer)
		 (message-fetch-field "to"))))

	("^alt.horror\\|alt.vampyres\\|^rec.arts.horror.movies"
	 (name "Onryou")
	 (address "phasm@nowhere.invalid")
	 (organization "nil")
	 ;("Reply-To" "nwoods usenet <nw.public@gmail.com>")
	 (signature nil)
	 (signature-file "~/.sigs/hpl"))

	("gandi.*"
	 (address "cnw+gandi@pobox.com")
	 ("Reply-To" nil))
	 
	("^gmane"
	 ;; Need to set mail-specify-envelope-from to t for this to work.
	 ;; (eval (setq mail-specify-envelope-from t))
	 ;; (eval (setq mail-envelope-from "cnw@pobox.com"))
	 (address "cnw+gmane@pobox.com")
	 (signature-file "~/.gmane_sig")
	 ("Mail-Copies-To" "nobody")
	 ("X-Archive" "encrypt")     ;; test: see http://gmane.org/tmda.html
	 ("Reply-To" nil) )
	("nnml:mail.gmane.authorizer"
	 ;; For replies to the Gmane autoresponder
	 (address "cnw+gmane@pobox.com")
	 (signature nil))
	))
;; end of gnus-posting-styles.

;; (setq message-default-headers 
;;       ;; random black-on-white or white-on-black signature x-face:
;;       (if (= 0 (random 2))
;;        "X-Face: #GiKd+5>RME~ArGEqHB=50)RDFBW?TcLw4Le!l?hPr=9KQ3iGbB5,':&A5dZKAQbVa7H>}a\n H2]G$+dWFwZE13%f1_<,^gwmYbK`q=J9$SjNRM7Vke#95ux8mD}vVJq8lF4rbh6~e5#-s)]3C]uz2.\n 4:EFk11#8&O\\p69p\";qB;6SHHGx%pB>&8Jw_z\n"
;;      "X-Face: 9k2B3lB5IO4m*ltpmXcjd(Q0Gl5O_=[Ooyw)+%G<mKpI9C}vzR%)p&Im2=B0~A9y^Q^dg/c\n ri73GsE>VR^zolWMzX<RlIGv,E~,{~Zzozta?@RNP^j,RB/AMY1=n32\"%w3L5D;Sh^?A?8\n"))

;; X-Face - this is the Onryou one.
(setq message-default-headers "X-Face: #GiKd+5>RME~ArGEqHB=50)RDFBW?TcLw4Le!l?hPr=9KQ3iGbB5,':&A5dZKAQbVa7H>}a\n H2]G$+dWFwZE13%f1_<,^gwmYbK`q=J9$SjNRM7Vke#95ux8mD}vVJq8lF4rbh6~e5#-s)]3C]uz2.\n 4:EFk11#8&O\\p69p\";qB;6SHHGx%pB>&8Jw_z\n")


;; Regexp matching alternative email addresses.  The first address in
;; the To, Cc or From headers of the original article matching this
;; variable is used as the From field of outgoing messages, replacing
;; the default From value. This variable has precedence over posting
;; styles and anything that runs off `message-setup-hook'.

(setq message-alternative-emails
      (regexp-opt '("cnw@pobox.com" "cnw+usenet@pobox.com"
		    "cnw+amazon@pobox.com" "cnw+debian@pobox.com"
		    "cnw+dbts@pobox.com" "cnw+gmane@pobox.com"
		    "nw.public@gmail.com" "nw@immermail.com"
		    "neil@phasmic.org" "neil+amazon@phasmic.org" 
		    "cnw+gandi@pobox.com" "neil.woods@gmail.com" 
		    "phasm@fea.st" "egregore@riseup.net" "neil@netlexer.uk"
		    "@ip6ix.net"))) ; I think that's most of them.

;; Regexp of From headers that may be suppressed in favor of To headers.
(setq gnus-ignored-from-addresses
'("Neil Woods" ".*phasm@fea.st" ".*egregore@riseup.net"))


;; These are the defaults (see message.el). Use this to reset if need be.
;; (setq message-required-news-headers
;;       '(From Newsgroups Subject Date Message-ID
;;         (optional . Organization)
;;         (optional . User-Agent)))


;; insert X-Now-Playing header based on the file contents of ~/.now_playing
(defun nw-x-now-playing ()
  "Return the song now playing"
  (let ((now-playing "~/.now_playing"))
    (when (and (file-exists-p (expand-file-name now-playing))
	       (> (nth 7 (file-attributes now-playing)) 1))  ;; size > 1byte
      (save-excursion
	(beginning-of-buffer)
	(re-search-forward "^--text follows this line--")
	(beginning-of-line)
	(insert "X-Now-Playing: ")
	(insert-file (expand-file-name now-playing))))))

(add-hook 'message-setup-hook 'nw-x-now-playing)       ;; disable for now.

(require 'gnus-fun)

;; Add an X-Face header from file (see customized settings for convert func)
;; (3/11) Added new image (replaced demon-rev2.gif).
(setq message-required-news-headers
      (nconc message-required-news-headers
	     (list '(X-Face . (lambda ()
				(gnus-x-face-from-file
				 "~/emacs/faces/MeShades-th166-48x48.gif"))))))

;; Add a Face header from file
;; Create a random Face header dynamically, say using something
;; like 'ppmforge -clouds -height 48 -width 48 | ppmquant 32 | ppmtojpeg'
;; (clouds in a blue sky). Leaving out the "-clouds" produces a planet.
;; Very nice!
(defun nw-create-random-face ()
  "Create a random image suitable for use in a face header."
  (interactive)
  (shell-command
   "ppmforge -clouds -height 48 -width 48 | ppmtojpeg > /tmp/temp.face.jpg")
  (gnus-face-from-file "/tmp/temp.face.jpg"))

;;(setq message-required-news-headers
;;      (nconc message-required-news-headers
;;	     (list '(Face . (lambda ()
;;			      (nw-create-random-face))))))

;; Related to posting-styles: gnus-alias (customized with custom) should be
;; placed *after* gnus-posting-styles. (Not yet setup)
;(require 'gnus-alias)
;(gnus-alias-init)

;; Insert a signature. Includes placing the "-- ".
(defun nw-insert-sig ()
  "Insert a snappy little signature."
  (interactive)
  (insert "\n-- \nNeil.\n"
	  (shell-command-to-string "/usr/games/fortune -s")))

;; insert signature, delete previous, if existing
;; idea from http://my.gnus.org/node/315, originally.
(defun nw-change-sig ()
  "Replace signature with new one"
  (interactive)
  (save-excursion
    (when (message-goto-signature)
      (if message-signature-insert-empty-line
	  (forward-line -2) (forward-line -1)))
    (delete-region (point) (point-max))
    (nw-insert-sig)))

(define-key message-mode-map "\C-c\C-w" 'nw-change-sig)


;; Define a length, in lines of what is considered a valid signature
(setq gnus-signature-limit 100.0)      ;; anything > 100 lines != a signature

;; Miscellaneous variables
(setq gnus-novice-user 'nil)
(setq gnus-large-newsgroup 'nil) ;default 200, nil= no groups considered large

;; Don't quote XNA posts, add message instead and don't post with XNA.
(setq message-cite-articles-with-x-no-archive nil)

;; News and Mail directories
;;
(setq gnus-directory "~/News/")     ;; these are defaults
(setq message-directory "~/Mail/")

;; Backends :: News
;; 
;; Primary = nntp; local NNTP server (i.e. haxan)...
(setq gnus-select-method 
      '(nntp "news"))

;; this is my main *external* USENET news server (text only)
(add-to-list 'gnus-secondary-select-methods 
	     '(nntp "news.eternal-september.org"))

;; My other occasionally used news servers
(add-to-list 'gnus-secondary-select-methods
	     '(nntp "news.gmane.org"))
(add-to-list 'gnus-secondary-select-methods
	     '(nntp "news.gandi.net"))

;; EMAIL SERVICES / SERVERS / METHODS

;; usual (Gnus default) email select-method
(add-to-list 'gnus-secondary-select-methods
	     '(nnml ""))

;;(add-to-list 'gnus-secondary-select-methods '(nnfolder ""))

;; I setup exim4 to use maildir so set sources there for gnus to pull from
;(setq mail-sources
;      '((maildir :path "/home/neil/Maildir/"
;                   :subdirs ("cur" "new"))))

(setq mail-source-delete-incoming t)  ;; rm ~/Mail/Incoming* files immediatly

;; I use riseup.net, gandi.net (for phasmic.org) & fastmail.fm (phasm@fea.st)
;; which uses  mail.messagingengine.com as server. 

;; Backends :: Phasmic.ORG IMAP

(add-to-list 'gnus-secondary-select-methods
	     '(nnimap "Phasmic.ORG"
		      (nnimap-address "mail.gandi.net")
		      (nnimap-server-port 993)
		      (nnimap-stream tls)
		      (nnimap-record-commands t)  ;; save in "*imap log*"
		      (nnimap-expunge t)))

;; Backends :: Riseup.NET IMAP

(add-to-list 'gnus-secondary-select-methods
	     '(nnimap "Riseup.NET"
		      (nnimap-address "mail.riseup.net")
		      (nnimap-server-port 993)
		      (nnimap-stream tls)
		      (nnimap-record-commands t)  ;; save in "*imap log*"
		      (nnimap-expunge t)))


;; Backends :: FastMail.FM IMAP 

;; (add-to-list 'gnus-secondary-select-methods
;; 	     '(nnimap "FastMail.FM"
;; 		      (nnimap-address "mail.messagingengine.com")
;; 		      (nnimap-server-port 993)
;; 		      (nnimap-stream ssl)
;; 		      (nnimap-record-commands t)  ;; save in "*imap log*"
;; 		      (nnimap-expunge t)))

;; experiment with this...
(add-to-list 'gnus-secondary-select-methods '(nndiary ""))

(setq imap-log t)
(setq smtpmail-debug-info t)

;; Added Mail auth stuff (for pobox.com / gandi.net on port 587 (submission))
;; Now will connect directly (see below),  without using local exim SMTP server.
(require 'smtpmail)
(require 'starttls)

(setq message-send-mail-function 'smtpmail-send-it)
;; emacs24 - see http://stackoverflow.com/questions/12655232/how-to-ask-gnutls-to-use-client-certificate-in-emacs-24
;;(defun gnutls-available-p ()
;;  "Function redefined in order not to use built-in GnuTLS support"
;;  nil)
;;(setq starttls-gnutls-program "gnutls-cli")
;;
;; uncomment these lines and either one of the smtpmail-smtp-server lines below to
;; use external smtp servers. Default: use local exim.
;;
;;(setq starttls-use-gnutls t)
;;(setq smtpmail-stream-type 'starttls)
;;;;(setq smtpmail-smtp-server "sasl.smtp.pobox.com")
;;(setq smtpmail-smtp-server "mail.gandi.net")
;;(setq smtpmail-smtp-service 587) ;;587(starttls) or 465(tls/ssl)
;;(setq starttls-extra-arguments '("--priority" "NORMAL:%COMPAT"))


;; increase security on tls 
(setq gnutls-min-prime-bits 1024)


;; Delete duplicate mail
(setq nnmail-treat-duplicates 'delete)
(setq gnus-summary-ignore-duplicates t)
(setq gnus-suppress-duplicates t)

;; Mark as read Gcc'd messages
(setq gnus-gcc-mark-as-read t)


;; Mail Splitting    
;; TODO

;; Attempt to fetch article from another server or google if not available
;;
(setq gnus-refer-article-method
      ;; First try to load the article from your local newsserver or nnml
      '(current
	;; If it's not available, fetch it from another newsserver
	;; TODO: Add code to match group name (if ^gmane ... )
	(nntp "news.eternal-september.org")
	(nntp "news.gmane.org")
	;; At last try Google Groups
	(nnweb "google" (nnweb-type google))))

;; gnus-w3m recom. by Reiner Steib. Using nil uses system default (eg mozilla)  
(setq mm-text-html-renderer 'gnus-w3m)

(setq gnus-mime-display-multipart-related-as-mixed t)
(setq mm-inline-text-html-with-images nil) 
(setq mm-inline-text-html-with-w3m-keymap t)     ;; default value


;; MIME :: Favour display of TEXT in multipart messages
;;         also, show GnuPG/PGP signed attachments
;;
(setq gnus-buttonized-mime-types
      '("multipart/alternative" "multipart/encrypted" "multipart/signed"))
;; took "text/html" out of this list (16/9/11) to try to make images & html 
;; email display - bah!  see http://norman.walsh.name/2011/03/10/w3m.
;; (1/7/12) Now put back.
(setq mm-discouraged-alternatives
      '("image/.*" "text/html"))  

;; toggle displaying of images in the article buffer
(defun gnus-summary-w3m-toggle-inline-images (&optional arg)
  "Toggle displaying of all images in the article buffer.
If the prefix arg is given, all images are considered to be safe."
  (interactive "P")
  (save-excursion     
    (set-buffer gnus-article-buffer)
    (w3m-toggle-inline-images arg)))

(eval-after-load "gnus-sum"
  '(define-key gnus-summary-mode-map
     "\C-i" 'gnus-summary-w3m-toggle-inline-images))


;; This next variable is nil if not set. Is this correct?
(setq mm-coding-system-priorities '(utf-8 iso-8859-1 iso-8859-15))


;; Keep backup of startup file
(setq gnus-backup-startup-file t)

;; don't list groups normally with just ticked articles
(setq gnus-list-groups-with-ticked-articles nil)

;; Confirm replying to news (pressed r instead of f)
;;
(setq gnus-confirm-mail-reply-to-news t)

;;
;; Summary buffer customisations (and threading).
;;
(setq gnus-build-sparse-threads 'some)

;; changed back to by-subject (the default) as by-references doesn't work
;; in nnvirtual (mail) groups
(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)

;; my old thread-sort function list - works quite well
;;(setq gnus-thread-sort-functions
;;      '(gnus-thread-sort-by-number
;;	gnus-thread-sort-by-subject
;;	gnus-thread-sort-by-score
;;	gnus-thread-sort-by-total-score))

;; sort by score then reverse arrival
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-number)
	gnus-thread-sort-by-score))

(setq gnus-thread-hide-killed t      ;; collapse thread on kill
      gnus-fetch-old-headers 'some   ;; useful to be able to see old messages in threads
      gnus-sum-thread-tree-single-indent "  ")

;;(setq gnus-summary-line-format
;;      ":%U%R%z %(%B %s %-45=|%4L |%-20,20f|%~(ignore 1)3t%)\n")

;; this will add UTF-8 thread layout and colouration and override the defaults
(Xwin-code
 (setq gnus-sum-thread-tree-indent " "
       gnus-sum-thread-tree-root "■ "
       gnus-sum-thread-tree-false-root "□ "
       gnus-sum-thread-tree-single-indent "▣ "
       gnus-sum-thread-tree-leaf-with-other "├─▶ "
       gnus-sum-thread-tree-vertical "│"
       gnus-sum-thread-tree-single-leaf "└─▶ ")
 
 (setq gnus-summary-same-subject "")
 
 (setq gnus-user-date-format-alist
       '(((gnus-seconds-today)
	  . "Today,%k:%M")
	 ((+ 86400
	     (gnus-seconds-today))
	  . "Y'day,%k:%M")
	 (604800 . "%a %k:%M")
	 ((gnus-seconds-month)
	  . "%a %d")
	 ((gnus-seconds-year)
	  . "%b %d")
	 (t . "%b %d '%y")))
 
 (setq gnus-summary-line-format 
       (concat
	"%*%5{%U%R%z%}"
	"%4{|%}"
	"%2{%-11&user-date;%}"
	"%4{|%}"
	"%2{ %}%(%-24,24f"
	;; "%4{|%}"        ; don't really need the score,
	;; "%2{%5i%}"      ; can be got by using V S.
	"%4{|%}"
	"%2{%6k %}%)"
	"%4{|%}"
	"%2{ %}%3{%B%}%1{%s%}\n"))
 
 ;;(setq gnus-summary-display-arrow t)
 
 
 ;; added since emacs 23, and the font defaults changed to Xft.
 ;;(make-empty-face 'gnus-default)
 ;;(set-face-font 'gnus-default "-gnu-unifont-*-*-*-*-*-*-*-*-*-*-iso10646-1")  ;; or...
 ;;(set-face-font 'gnus-default "Liberation Mono 11")   ;; if using Xft.
 (copy-face 'default 'gnus-default)
 
 (copy-face 'gnus-default 'mysubject)
 (setq gnus-face-1 'mysubject)
 
 (copy-face 'gnus-default 'mytime)
 (set-face-foreground 'mytime "grey60")
 (setq gnus-face-2 'mytime) 
 
 (copy-face 'gnus-default 'mythreads)
 (set-face-foreground 'mythreads "palegreen4") 
 (setq gnus-face-3 'mythreads) 
 
 (copy-face 'gnus-default 'mygrey) 
 (set-face-foreground 'mygrey "grey") 
 (setq gnus-face-4 'mygrey) 
 
 (copy-face 'gnus-default 'myblack) 
 (set-face-foreground 'myblack "grey60") 
 (setq gnus-face-5 'myblack) 
 
 (copy-face 'gnus-default 'mybiggernumbers) 
 (set-face-foreground 'mybiggernumbers "grey60") 
 (setq gnus-face-6 'mybiggernumbers))             ;; End: (Xwin-code


;; Group Parameters
(setq gnus-parameters
      '(("^nnml:mail\\..*"
	 (gnus-use-scoring nil)
	 
	 ;;     (gnus-summary-line-format
	 ;;      "%U%R%z%I%(%[%d:%ub%-23,23f%]%) %s\n")
	 ;; sort by score and then reverse arrival order, for all
	 ;; nnml:mail.* groups. NB: Do similar for list.* groups??
	 (gnus-thread-sort-functions
	  '((lambda (t1 t2)
	      (not (gnus-thread-sort-by-number t1 t2)))
	    gnus-thread-sort-by-score))
	 (display . 200))   ;; maybe change to '[not expire]'
	
	("^nnml:mail\\.misc"
	 (expiry-wait . 14)
	 (visible . t)
	 (gnus-use-scoring  t))

	("^nnml:mail\\.admin"
	 (auto-expire . t)
	 (expiry-wait . 7)) ;; this is the default value (= nnmail-expiry-wait)
	
	("^nnml:mail\\.pobox-reports"
	 (auto-expire . t)
	 (expiry-wait . 7))
	
	("^nnml:mail\\.postmaster"
	 (auto-expire . t)
	 (expiry-wait . 28))

	("^nnml:mail\\.keep\\.*"
	 (display .  all)
	 (visible . t)
	 (gnus-use-scoring t)
	 (auto-expire . nil)
	 (total-expire . nil))

	("archive:sent-*"
	 (gnus-use-scoring nil)
	 (visible . t)
	 (display . all))

	("^nnml:list\\.debian\\.*"
	 (banner . 'signature))

	("^nnml:list\\..*"
	 ;; sort by score and then reverse arrival order, for all
	 ;; nnml:list.* groups. Similarly for mail.* groups, above.
	 (gnus-thread-sort-functions
	  '((lambda (t1 t2)
	      (not (gnus-thread-sort-by-number t1 t2)))
	    gnus-thread-sort-by-score))

	 (broken-reply-to . t)
	 (total-expire . t)
	 (expiry-wait . 14))

	("nntp.*gmane\\.debian\\.user\\.news"
	 (gnus-show-threads nil)
	 (gnus-article-sort-functions '((not gnus-article-sort-by-date)))
	 (gnus-use-adaptive-scoring nil)
	 (gnus-use-scoring nil))
	("nnrss.*debian"
	 (gnus-show-threads nil)
	 (gnus-article-sort-functions 'gnus-article-sort-by-subject)
	 (gnus-use-adaptive-scoring nil)
	 (gnus-use-scoring t)
	 (gnus-score-find-score-files-function 'gnus-score-find-single)
	 (gnus-summary-line-format "%U%R%z%d %I%(%[ %s %]%)\n"))))


;; The '(visible . t)' gnus-parameter cannot be set above. Use this:
;;(setq gnus-permanently-visible-groups 
;;       (regexp-opt 
;;        '("nnml:mail.misc"
;; 	 "nnimap+mail.messagingengine.com:INBOX"
;; 	 "nnimap+mail.riseup.net:INBOX"
;; 	 "nnimap+mail.gandi.net:INBOX"
;; 	 "nnfolder+archive:sent"
;;	 "nnvirtual:")))

;; set an expiry target (default is 'delete), except for spam - see above.
(setq nnmail-expiry-target "nnml:expired")
;;(setq nnmail-expiry-target 'delete)


;; Appearance :: Article Buffer :: Visible Headers
;;
;; make some variables summary-buffer local. see group parameters.
;; 20.12.04 - added "To:"
(setq gnus-newsgroup-variables
     '(message-use-followup-to
       (gnus-visible-headers .
	"^\\(From:\\|Subject:\\|Summary:\\|Keywords:\\|Date:\\|X-Sent:\\|To:\\|Newsgroups:\\|Followup-To:\\|Reply-To:\\|X-No-Archive:\\|X-Newsreader:\\|User-Agent:\\|X-Mailer:\\|X-Now-Playing:\\|Organization:\\)")))

;; Appearance :: Article Treatment
;;
;; Whether the X-Sent and Date headers can coexist. TODO: Finish this!
(setq gnus-article-date-lapsed-new-header t)
(setq gnus-treat-date-lapsed 'head)
;; or define gnus-article-time-format (default="%a, %d %b %Y %T %Z") and
;; uncomment next line. (lapsed after Date: like gmane would be good).
;;(setq gnus-treat-date-user-defined 'head)
(gnus-start-date-timer)


;(add-to-list 'gnus-newsgroup-variables 'message-from-style)

;; Horizontal scrolling (in Summary Buffer). This was disabled for no reason
;; maybe add this to summary-mode-hook?
(setq auto-hscroll-mode t)

;; Scoring

;; *todo* Turn on this only for newsgroups?
(setq gnus-score-interactive-default-score 400)  ;; default is 1000
; (setq gnus-use-adaptive-scoring t)      ; doesn't work well w/ auto-expire
(setq gnus-decay-scores "\\.ADAPT")       ; cf Info (gnus) Score Decays

(setq gnus-kill-files-directory "~/News/SCORES/")

;; don't show articles below this score (thus, pressing L more than once
;; is enough to not show articles (400+400 > 700)). Note: I need to
;; reset this (to say, -1800) if the default for
;; gnus-score-interactive-default-score is restored (see above).
(setq gnus-summary-expunge-below -700)

;; One score file for the 'emacs' groups, another for 'comp', and let all
;; others use their own score files (See *Info* (gnus) Home Score File):
(setq gnus-home-score-file
      ;; All groups that match the regexp `"\\.emacs"'
      '(("\\.emacs" "emacs.SCORE")
        ;; All the comp groups in one score file
        ("^comp" "comp.SCORE")))


;; Hooks :: Message
;;
;;(require 'filladapt) ; where gone?
(require 'smart-quote)
(defun my-message-mode-setup ()
  (setq fill-column 72)
  (turn-on-auto-fill)
;;  (filladapt-mode)
  (smart-quote-mode)
  (flyspell-mode))
(add-hook 'message-mode-hook 'my-message-mode-setup)

(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)

;; Hooks :: General
;;
;; Most frequently groups 'bubble' to the top, after sorting ("G S r")
;;
(add-hook 'gnus-summary-exit-hook 'gnus-summary-bubble-group)
(add-hook 'gnus-summary-mode-hook 'highline-mode)

;; Favour topics & use highline here too.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'gnus-group-mode-hook 'highline-mode)

;; Safety features -- save as much as possible...
(add-hook 'gnus-summary-exit-hook 'gnus-dribble-save)
(add-hook 'gnus-after-getting-new-news-hook 'gnus-dribble-save)
;; just remember to press 's'... this not so useful with gnus-demon.
(add-hook 'gnus-after-getting-new-news-hook 'gnus-group-save-newsrc t)
(add-hook 'gnus-group-catchup-group-hook 'gnus-dribble-save)

;; Don't query reading dribble file on startup
(setq gnus-always-read-dribble-file t)

;; this is the default (see manual - 5.4 Archived Messages)
(setq gnus-message-archive-method
      '(nnfolder "archive"
		 (nnfolder-directory   "~/Mail/archive")
		 (nnfolder-active-file "~/Mail/archive/active")
		 (nnfolder-get-new-mail nil)
		 (nnfolder-inhibit-expiry t)))

;; Save articles (use the default -- 'o' for now).
;; set the save-dir
(setq gnus-article-save-directory (concat gnus-directory "saved/"))

;; From Info node (gnus) Saving Articles:
;; If you'd like to save articles in a hierarchy that looks something
;; like a spool, you could

(setq gnus-use-long-file-name '(not-save)) ; to get a hierarchy
(setq gnus-default-article-saver
      'gnus-summary-save-in-file)       ; no encoding
;; read the above with nneething (G D in group buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Archive articles (use M-x my-archive-article or Alt-a)
;; Note: Very Useful! (TODO: Clear out/rename older groups).
;; Note2: Process marks work (#), but marking a region doesn't.
;; Changed to use 'A' instead of '1'. Bound to Alt-a.
(defun my-archive-article (&optional n)
  "Copies one or more article(s) to a corresponding `nnml:' group, e.g.
 `gnus.ding' goes to `nnml:A.gnus.ding'. And `nnml:list.bbdb' goes
 to `nnml:A.list.bbdb'.

 Use process marks or mark a region in the summary buffer to archive
 more then one article."
  (interactive "P")
  (let ((archive-name
	 (format
	  "nnml:A.%s"
	  (if (featurep 'xemacs)
	      (replace-in-string gnus-newsgroup-name "^.*:" "")
	    (replace-regexp-in-string "^.*:" "" gnus-newsgroup-name)))))
    (gnus-summary-copy-article n archive-name)))

(define-key gnus-summary-mode-map [(alt ?a)] 'my-archive-article)

;; ticked or dormant articles go into the cache.
(setq gnus-use-cache t)


;; Swap the default behavior of the 'e' and 'E' keys in the group summary
;; buffers.  Using a shifted key to expire articles is somewhat painful.
;; I expire articles often, but rarely edit the contents of email articles.
;; Inspired from <http://is.gd/bm9X8> and §3.7.5 Generic Marking Commands
;; in the Gnus Manual.
(add-hook 'gnus-summary-mode-hook 'nw-alter-summary-map)
(add-hook 'gnus-article-prepare-hook 'nw-alter-summary-map)
(defun nw-alter-summary-map ()
  (local-set-key "e"
		 (lambda (n)
		   (interactive "p")
		   (with-current-buffer gnus-summary-buffer
		     (gnus-summary-put-mark-as-expirable-next n))))
  (local-set-key "E" 'gnus-summary-edit-article))


;; charset settings - display article with e.g. 23g = gb2312, etc.

(setq gnus-summary-show-article-charset-alist
      '((1 . iso-8859-1)
	(8 . utf-8)
	(0 . iso-8859-15)
	(9 . iso-8859-15)
	(2 . iso-8859-2)
	(12 . windows-1252)
	(437 . cp437)
	(850 . cp850)
	(23 . gb2312)))


;; Additional bbdb settings (see also ~/.emacs)
(setq bbdb-north-american-phone-numbers-p nil)
(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)

(setq bbdb-auto-notes-alist (list
			     '("Organization"
			       (".*" company 0))
			     '("User-Agent"
			       (".*" mailer 0))
			     '("X-Mailer"
			       (".*" mailer 0))
			     '("X-Newsreader"
			       (".*" mailer 0))))

;; Don't ask, just do it
(setq bbdb-offer-save 'auto)

;; Planner, and related modes. (*TODO*) (cf todoo, etc)
;; (planner-insinuate-gnus)
;; Todo Lists (nntodo)
;; (require 'nntodo)


;; Gnus demon (add more here? disconnect nnimap servers after X mins?)
;; modified handler to add resetting .marks files.
(require 'gnus-demon)
(setq gnus-use-demon t)
(gnus-demon-add-handler 'nw-gnus-group-get-new-news 10 t) ;; every 10 mins.
(gnus-demon-init)
(gnus-demon-add-rescan)

;; here's the new function
(defun nw-gnus-group-get-new-news ()
  (setq nntp-marks-file-name (or nntp-marks-file-name
				 ".marks"))
  (gnus-group-get-new-news))


;; Article Citation
;;         choice: gnus original, supercite, trivial-cite, september-cite.
;;                 =============
;; Default settings, set here as an aide memoir. Changed by TC, if used [no].
;;(setq message-cite-function 'message-cite-original)
(setq message-cite-function 'message-cite-original-without-signature)

;; from message.el, modified slightly.
(defun nw-message-insert-citation-line ()
  "Insert a simple citation line, modified default."
  (when message-reply-headers
    (insert ">>>>> " (mail-header-from message-reply-headers) " writes:")
    (newline)
    (newline)))

;; normally set to message-insert-citation-line. (see message.el). (now reset)
(setq message-citation-line-function 'message-insert-citation-line)

;; Citation :: Supercite [not used at present] [ possibly remove this?]
(autoload 'sc-cite-original     "supercite" "Supercite 3.1" t)
(autoload 'sc-submit-bug-report "supercite" "Supercite 3.1" t)

;; Do not insert the original author's signature when citing with supercite
(add-hook 'sc-pre-hook
	  (lambda ()
	    (save-excursion
	      (let ((start (point))
		    (end (mark t)))
		(goto-char end)
		(when (re-search-backward "^-- $" start t)
;               (when (re-search-backward gnus-signature-separator start t)
		  (forward-line -1)
		  (while (looking-at "[ \t]*$")
		    (forward-line -1))
		  (forward-line 1)
		  (delete-region (point) end))))))

;; Attibutions to use by preference - the first non-nil string wins
(setq sc-preferred-attribution-list '("x-attribution"
				      "initials"
				      "firstname"
				      "sc-lastchoice"
				      "lastname"))

;; Use "Name>" style (the default) = nil. If t, then don't.
(setq sc-nested-citation-p t)
(setq sc-reference-tag-string ">>>>> ")   ;; default = ">>>>> "
(setq sc-electric-references-p nil)  ;; default = t
(setq sc-citation-leader "")	     ;; default = "    " (ie 4 spaces)
(setq sc-preferred-header-style 5)   ;; default = 4 (5 -> "NW" == Name Woods)
(setq sc-auto-fill-region-p nil)     ;; default = t (not always tidy refill)
(setq sc-confirm-always-p nil)       ;; default = t (may be changed below)

;; NB: uncomment to SC make active: alternative is use tc instead: see below.
;(add-hook 'mail-citation-hook 'sc-cite-original)

;; Add another hook here to ask if sc-nested-citation-p should be set
;; to nil for NW> type quoting. Called from a hook (sometimes).
(defun nw-sc-default-quoting ()
  "Ask if we want NW> type citing."
  (interactive)
  (if (y-or-n-p "Use Supercite Name> style quoting? ")
      ;; the default ("y") returns t. Set these variables up.
      (setq sc-nested-citation-p nil
	    sc-electric-references-p t
	    sc-auto-fill-region-p t       ;; occationally messes up.
	    sc-citation-leader "    "
	    sc-confirm-always-p t
	    sc-preferred-header-style 4)
    ;; otherwise set them differently.
    (setq sc-nested-citation-p t
	  sc-electric-references-p nil
	  sc-auto-fill-region-p nil
	  sc-citation-leader ""
	  sc-confirm-always-p nil
	  sc-preferred-header-style 5)))


;; hook run before selecting an attribution. I don't always need this!
;; (since I prefer to usually just use regular citing (Usenet style).
;;(add-hook 'sc-attribs-preselect-hook 'nw-sc-default-quoting)

;;(message "Gnus: Supercite initialized")

;; Citation :: Trivial Cite [not used at present]
;;
;; Do I need to use tc (only for clever XNA message?)
;;       Simply define message-citation-line-function to be something
;;       that cites like tc (it's value is now message-insert-citation-line)
;; (autoload 'trivial-cite "tc" t t)
;; (setq mail-extr-ignore-single-names nil) ; added 23.02.05
;; (setq tc-make-attribution 'tc-simple-attribution-kai)
;; (setq tc-time-format "%a, %b %d %Y")
;; (later) Use supercite, to make full use of Gnus's new defaults.
;; E.g. supercite uses mail-citation hook, rather than message-cite-function
;;(setq message-cite-function 'trivial-cite)        ; uncomment for tc

;;(message "Gnus: Trivial Cite initialized")

;; this next one is fun: eternal-september...
(defun september-citation-line ()
  (when message-reply-headers
    (insert "On "
            (int-to-string
             (-
              (time-to-days (mail-header-parse-date
                             (mail-header-date message-reply-headers)))
              (time-to-days (encode-time 0 0 0 01 09 1993))))
            " September 1993, "
            (let* ((email (mail-header-from message-reply-headers))
                   (data (mail-extract-address-components email))
                   (name (car data))
                   (net (car (cdr data))))
              (or name net email))
            " wrote:\n")))

;; make it the default citation line for Gnus' message mode (todo: News-only!)
;;(setq message-citation-line-function 'september-citation-line)


;; Article wash with "w" key
;; for those stupid newsreaders
;;
(defun bhaak-wash-this-article ()
  (interactive)
  (gnus-article-outlook-deuglify-article)
  (gnus-article-fill-cited-article nil '70)
;  (gnus-article-capitalize-sentences)
  )
(define-key gnus-summary-mode-map (kbd "C-c C-w") 'bhaak-wash-this-article)
(define-key gnus-summary-mode-map [f5] 'bhaak-wash-this-article)


;; With the code below, you can change From and Reply-To using `C-c a 1'
;; or `C-c a 2' while composing a message.
;; (from: Reiner Steib <Reiner.Steib@gmx.de>)

(defun rs-message-replace-header (header new-value)
  "Remove HEADER and insert the NEW-VALUE."
  ;; Similar to `nnheader-replace-header' but for message buffers.
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (message-remove-header header))
    (message-position-on-field header)
    (insert new-value)))

(defun rs-message-change-address (from reply-to)
  "Change FROM and REPLY-TO."
  (interactive)
  (rs-message-replace-header "Reply-To"
			     (format "%s (%s)" reply-to user-full-name))
  (rs-message-replace-header "From"
			     (format "%s (%s)" from user-full-name)))

(defun nw-message-change-address (user-full-name from)
  "Change FROM and NAME in headers."
  (interactive)
  (rs-message-replace-header "From"
			     (format "%s <%s>" user-full-name from)))

;; swap two commonly used aliases in netnews
(define-key message-mode-map (kbd "C-c a 1")
  (lambda ()
    (interactive)
    (rs-message-replace-header "Organization" "(TINO)")
    (nw-message-change-address "Neil Woods" (concat "cnw+usenet-"
			  (format-time-string "%y%m" (current-time) t)
			  "@pobox.com"))))

(define-key message-mode-map (kbd "C-c a 2")
  (lambda ()
    (interactive)
    (rs-message-replace-header "Organization" "nil")
    (nw-message-change-address "Onryou" "nw-public@gmail.invalid")))

;; A few local key re-assignments. In the summary-buffer, define 'b' to
;; go back one page (to be similar to many other  tools I use), was BS.
;; And define 'M-b' to have b's original assignment (instead of backward-word).
(define-key gnus-summary-mode-map "b" 'gnus-summary-prev-page)
(define-key gnus-summary-mode-map (kbd "M-b") 'gnus-article-view-part)


(message "Gnus: initialized.")

;; eof
