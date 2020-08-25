;;;; Netease Music :: A netease music client for Emacs.

;;; Intro
;; Copyright (C) 2020 SpringHan

;; Author: SpringHan <springchohaku@qq.com>
;; Maintainer: SpringHan

;; Version: 1.0
;; License: MIT

;; Last Change: <+++>
;; Repository: https://github.com/SpringHan/netease-cloud-music.el

(require 'cl-lib)
(require 'request)
(require 'json)

(defgroup netease-cloud-music nil
	"Netease Music group"
	:group 'applications)

(defcustom netease-cloud-music-mode-hook nil
	"The hook for Netease Music mode."
	:type 'hook
	:group 'netease-cloud-music)

(defcustom netease-cloud-music-process nil
	"The process of Netease Music."
	:group 'netease-cloud-music)

(defcustom netease-cloud-music-current-song
	:group 'netease-cloud-music)

(defconst netease-cloud-music-buffer-name "*Netease-Cloud-Music*"
	"The name of Netease Music buffer.")

(defconst netease-cloud-music-player "mplayer"
	"The player of Netease Music.")

(defconst netease-cloud-music-search-api
	"http://music.163.com/api/search/get/"
	"The search api of Netease Music.")

(defvar netease-cloud-music-mode-map
	(let ((map (make-sparse-keymap)))
		(define-key map "q" 'netease-cloud-music-close)
		(define-key map (kbd "SPC") 'netease-cloud-music-pause-or-continue)
		(define-key map "f" 'netease-cloud-music-search-song)
		(define-key map "n" 'next-line)
		(define-key map "p" 'previous-line)
		map)
	"Netease Music mode map.")

(define-derived-mode netease-cloud-music-mode nil "Netease-Cloud-Music"
	"The mode of Netease Music mode."
	:group 'netease-cloud-music
	:abbrev-table nil
	:syntax-table nil
	(linum-mode -1)
	(setq buffer-read-only t
				truncate-lines t))

;;;###autoload
(defun netease-cloud-music ()
	"Initialize the Netease Music buffer in netease-cloud-music-mode."
	(interactive)
	(unless (buffer-live-p (get-buffer netease-cloud-music-buffer-name))
		(switch-to-buffer netease-cloud-music-buffer-name))
	(netease-cloud-music-mode))

(defun netease-cloud-music-kill-process ()
	"Kill the Netease Music process."
	(when (and netease-cloud-music-process
						 (/= (process-live-p netease-cloud-music-process) 0))
		(delete-process netease-cloud-music-process)
		(setq netease-cloud-music-process nil)))

(defun netease-cloud-music-close ()
	"Close Netease Music and kill the process."
	(interactive)
	(netease-cloud-music-kill-process)
	(kill-buffer netease-cloud-music-buffer-name))

(defmacro netease-cloud-music-expand-form (&rest form)
	"Expand form in function-form."
	`(function*
		(lambda (&key data &allow-other-keys)
			,@form)))

(cl-defun netease-cloud-music-request-from-api (content &key (type 'song))
	"Request the CONTENT from Netease Music API.

CONTENT is a string.

TYPE is a symbol, its value can be song."
	(let (result search-type)
		(pcase type
			('song (setq search-type "1")))
		(request
			netease-cloud-music-search-api
			:type "POST"
			:data `(("s" . ,content)
							("limit" . "1")
							("type" . ,search-type)
							("offset" . "0"))
			:parser 'json-read
			:success (netease-cloud-music-expand-form (setq result data))
			:sync t)
		(cl-return-from netease-cloud-music-request-from-api result)))

(cl-defun netease-cloud-music-ask (type)
	"Ask user TYPE of the question.
If user reply y, return t.
Otherwise return nil."
	(let (result)
		(pcase type
			('song
			 (setq result (read-minibuffer "The info of the song is here, do you want to listen it?(y/n)"))
			 (if (string= result "y")
					 (cl-return-from netease-cloud-music-ask t))))))

(cl-defun netease-cloud-music-read-json (data &key sid sname aid aname)
	"Read the Netease Music json DATA and return the result.

SID is the song-id.

SNAME is the song-name.

AID is the artist-id.

ANAME is the artist-name."
	(let (r-sid r-sname r-aid r-aname)
		(cond (sid
					 (setq r-sid
								 (cdar (aref
												(cdr (cadar
															data)) 0))))
					(sname
					 (setq r-sname
								 (cdadr (aref
												 (cdr (cadar
															 search-result)) 0))))
					(aid
					 (setq r-aid
								 (cdar (aref
												(cdar (cddr
															 (aref (cdr
																			(cadar test)) 0))) 0))))
					(aname
					 (setq r-aid
								 (cdadr (aref
												 (cdar (cddr
																(aref (cdr
																			 (cadar data)) 0))) 0)))))
		(list r-sid r-sname r-aid r-aname)))

(defun netease-cloud-music-search-song (song-name)
	"Search SONG-NAME from Netease Music and return the song id.
SONG-NAME is a string."
	(interactive "MEnter the song name: ")
	(if (string= song-name "")
			(error "You can't enter a null string!")
		(let* ((search-result
						(netease-cloud-music-request-from-api song-name))
					 (result-song-id
						(cdar (aref
									 (cdr (cadar
												 search-result)) 0)))
					 (result-song-name
						(cdadr (aref
										(cdr (cadar
													search-result))) 0))
					 (result-artist-name
						(cdadr (aref
										(cdar (cddr
													 (aref (cdr
																	(cadar search-result)) 0))) 0))))
			(netease-cloud-music-interface-init :content (list result-song-id result-song-name result-artist-name))
			(if (netease-cloud-music-ask 'song)
					;; call the 'Play the music' function.
					(message "Now, the first song will not play.")))))

(cl-defun netease-cloud-music-interface-init (&key (content nil) (type 'song))
	"Initialize the Netease Music buffer interface.
CONTENT is a cons, its value is variable with TYPE.

TYPE is a symbol, its value can be song.

When TYPE is song, CONTENT can be:

'(music-name . artist-name)

If CONTENT is null, it will print the init content."
	(if (null content)
			(progn)))
;; (pcase type
;; 	('song))))

(provide 'netease-cloud-music)
