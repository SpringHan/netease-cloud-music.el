;;;; Netease Music :: A netease music client for Emacs.

;;; Intro
;; Copyright (C) 2020 SpringHan

;; Author: SpringHan <springchohaku@qq.com>
;; Maintainer: SpringHan

;; Version: 1.0
;; License: GPL-3.0

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

(defcustom netease-cloud-music-current-song ""
	"The current playing song."
	:type 'string
	:group 'netease-cloud-music)

(defcustom netease-cloud-music-process-status ""
	"The status of Netease Music process."
	:type 'string
	:group 'netease-cloud-music)

(defcustom netease-cloud-music-cache-directory (expand-file-name (locate-user-emacs-file "netease-cloud-music/"))
	"The cache directory of Netease Music."
	:type 'string
	:group 'netease-cloud-music)

(defconst netease-cloud-music-buffer-name "*Netease-Cloud-Music*"
	"The name of Netease Music buffer.")

(defconst netease-cloud-music-player "mplayer"
	"The player of Netease Music.")

(defconst netease-cloud-music-search-api
	"http://music.163.com/api/search/get/"
	"The search api of Netease Music.")

(defconst netease-cloud-music-song-link
	"http://music.163.com/song/media/outer/url?id="
	"The song link of Netease Music.")

(defvar netease-cloud-music-mode-map
	(let ((map (make-sparse-keymap)))
		(define-key map "q" 'netease-cloud-music-close)
		(define-key map (kbd "SPC") 'netease-cloud-music-pause-or-continue)
		(define-key map "f" 'netease-cloud-music-search-song)
		(define-key map "n" 'next-line)
		(define-key map "p" 'previous-line)
		(define-key map "x" 'netease-cloud-music-kill-current-song)
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
	(netease-cloud-music-mode)
	(netease-cloud-music-interface-init))

(defun netease-cloud-music-process-live-p ()
	"Check if the Netease Music process is live.
If it's live, return t.
Otherwise return nil."
	(if (and netease-cloud-music-process
					 (not (eq (process-live-p netease-cloud-music-process) 0)))
			t
		nil))

(defun netease-cloud-music-kill-process ()
	"Kill the Netease Music process."
	(when (netease-cloud-music-process-live-p)
		(delete-process netease-cloud-music-process)
		(setq netease-cloud-music-process nil)))

(defun netease-cloud-music-close ()
	"Close Netease Music and kill the process."
	(interactive)
	(netease-cloud-music-kill-process)
	(setq netease-cloud-music-process-status "")
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
		(when sid
			(setq r-sid
						(cdar (aref
									 (cdr (cadar
												 data)) 0))))
		(when sname
			(setq r-sname
						(cdadr (aref
										(cdr (cadar
													data)) 0))))
		(when aid
			(setq r-aid
						(cdar (aref
									 (cdar (cddr
													(aref (cdr
																 (cadar data)) 0))) 0))))
		(when aname
			(setq r-aname
						(cdadr (aref
										(cdar (cddr
													 (aref (cdr
																	(cadar data)) 0))) 0))))
		(list r-sid r-sname r-aid r-aname)))

(defun netease-cloud-music-search-song (song-name)
	"Search SONG-NAME from Netease Music and return the song id.
SONG-NAME is a string."
	(interactive "MEnter the song name: ")
	(if (string= song-name "")
			(error "You can't enter a null string!")

		;; Assigned the data from api to variables and call the functions to play the song
		(let* ((search-result
						(netease-cloud-music-read-json
						 (netease-cloud-music-request-from-api song-name)
						 :sid t :sname t :aname t))
					 (result-song-id (car search-result))
					 (result-song-name (nth 1 search-result))
					 (result-artist-name (nth 3 search-result)))
			(netease-cloud-music-interface-init
			 :content (list result-song-name result-artist-name))
			(if (netease-cloud-music-ask 'song)
					(netease-cloud-music-play result-song-id result-song-name result-artist-name)
				(message "Now, the song catched won't be played.")))))

(cl-defun netease-cloud-music-interface-init (&key (content nil) (type 'song-ask))
	"Initialize the Netease Music buffer interface.
CONTENT is a cons, its value is variable with TYPE.

TYPE is a symbol, its value can be song or song-ask.

When TYPE is song-ask, CONTENT can be:

'(music-name . artist-name)

If CONTENT is nil and TYPE is not song, it will print the init content."
	(setq buffer-read-only nil)
	(erase-buffer)
	(insert (propertize
					 "Netease Cloud Music - 网易云音乐\n"
					 'face '(:height 1.1 :foreground "Red3")))
	;; TODO Get the cache songs and insert them

	;; When the type is song, insert the current song info.
	(cond ((eq type 'song)
				 (insert "\n")
				 (insert (concat
									(propertize
									 "Current song: "
									 'face '(:height 0.9 :width bold))
									(propertize (format
															 "%s" netease-cloud-music-current-song)
															'face '(:height 1.0 :foreground "MediumSpringGreen"))
									(if content
											(propertize " [Playing]\n"
																	'face '(:foreground "OrangeRed"))
										(propertize " [Paused]\n"
																'face '(:foreground "OrangeRed"))))))
				(content
				 (insert "\n")
				 (pcase type
					 ('song-ask
						(let ((song-name (car content))
									(artist-name (nth 1 content)))
							(insert (concat
											 (propertize "Song name: "
																	 'face '(:width bold))
											 (propertize (format
																		"%s\n" song-name "")
																	 'face '(:foreground "Cyan3"))
											 (propertize "Artist name: "
																	 'face '(:width bold))
											 (propertize (format
																		"%s\n" artist-name)
																	 'face '(:foreground "Cyan3")))))))))
	(setq buffer-read-only t))

(defun netease-cloud-music-play (song-id song-name artist-name)
	"Play the song by its SONG-ID and update the interface with SONG-NAME"
	(if (null song-id)
			(error "There's no song-id!")
		(setq netease-cloud-music-process
					(start-process "netease-cloud-music-play"
												 nil
												 netease-cloud-music-player
												 "-slave"
												 (concat netease-cloud-music-song-link
																 (format "%s"
																				 song-id))))
		(set-process-sentinel netease-cloud-music-process
													'netease-cloud-music-process-sentinel)
		(setq netease-cloud-music-process-status "playing")
		(setq netease-cloud-music-current-song
					(concat song-name " - " artist-name))
		(netease-cloud-music-interface-init :content t :type 'song)))

(defun netease-cloud-music-process-sentinel (process event)
	"The sentinel of Netease Music process."
	(when (string-match "\\(finished\\|exit\\)" event)
		(message "There is a problem when playing the song.")
		(setq netease-cloud-music-process-status "")
		(netease-cloud-music-kill-process)))

(defun netease-cloud-music-pause-or-continue ()
	"Pause or continue play the current song."
	(interactive)
	(when (netease-cloud-music-process-live-p)
		(process-send-string netease-cloud-music-process "pause\n")
		(pcase netease-cloud-music-process-status
			("playing"
			 (setq netease-cloud-music-process-status "paused")
			 (netease-cloud-music-interface-init :type 'song))
			("paused"
			 (setq netease-cloud-music-process-status "playing")
			 (netease-cloud-music-interface-init :content t :type 'song)))))

(defun netease-cloud-music-kill-current-song ()
	"Kill the current song."
	(interactive)
	(if (netease-cloud-music-process-live-p)
			(progn
				(netease-cloud-music-kill-process)
				(setq netease-cloud-music-process-status "")
				(netease-cloud-music-interface-init))
		(error "There's no song playing.")))

(provide 'netease-cloud-music)
