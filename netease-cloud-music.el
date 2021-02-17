;;;; Netease Music :: A netease music client for Emacs.

;;; Intro
;; Copyright (C) 2020 SpringHan

;; Author: SpringHan <springchohaku@qq.com>
;; Maintainer: SpringHan

;; Version: 1.0
;; License: GPL-3.0

;; Last Change: 2020-09-11
;; Repository: https://github.com/SpringHan/netease-cloud-music.el

(eval-when-compile
	(require 'cl))
(require 'request)
(require 'json)
(require 'async)

(defgroup netease-cloud-music nil
	"Netease Music group"
	:group 'applications)

(defcustom netease-cloud-music-mode-hook nil
	"The hook for Netease Music mode."
	:type 'hook
	:group 'netease-cloud-music)

(defcustom netease-cloud-music-view-buffer-hook nil
	"The hook for view the Netease Music buffer."
	:type 'hook
	:group 'netease-cloud-music)

(defcustom netease-cloud-music-process nil
	"The process of Netease Music."
	:group 'netease-cloud-music)

(defcustom netease-cloud-music-current-song nil
	"The current playing song."
	:type 'list
	:group 'netease-cloud-music)

(defcustom netease-cloud-music-process-status ""
	"The status of Netease Music process."
	:type 'string
	:group 'netease-cloud-music)

(defcustom netease-cloud-music-cache-directory (expand-file-name (locate-user-emacs-file "netease-cloud-music/"))
	"The cache directory of Netease Music."
	:type 'string
	:group 'netease-cloud-music)

(defcustom netease-cloud-music-play-status ""
	"The current play status.
It can be song or playlist."
	:type 'string
	:group 'netease-cloud-music)

(defcustom netease-cloud-music-playlist nil
	"The list of the playlist songs."
	:type 'list
	:group 'netease-cloud-music)

(defcustom netease-cloud-music-playlist-song-index 0
	"The song index for the playlist song."
	:type 'number
	:group 'netease-cloud-music)

(defcustom netease-cloud-music-show-lyric t
	"Weather show lyric."
	:type 'boolean
	:group 'netease-cloud-music)

(defcustom netease-cloud-music-player-command
	'("mpv" "pause\n" "pause\n" "seek 5\n" "seek -5\n")
	"The player command for playing the online songs.
Its format is lick this:
'(command play-online-songs-arg continue-message
pause-message seek-forward-message seek-backward-message"
  :type 'list
  :group 'netease-cloud-music)

(defvar netease-cloud-music-buffer-name "*Netease-Cloud-Music*"
	"The name of Netease Music buffer.")

(defvar netease-cloud-music-lyric-timer nil
	"The timer of Netease Music lyric.")

(defvar netease-cloud-music-lyric nil
	"The Netease Music lyric.")

(defconst netease-cloud-music-search-api
	"http://music.163.com/api/search/get/"
	"The search api of Netease Music.")

(defconst netease-cloud-music-song-link
	"http://music.163.com/song/media/outer/url?id="
	"The song link of Netease Music.")

(defvar netease-cloud-music-repeat-mode ""
	"The repeat mode for Netease Cloud Music.")

(defvar netease-cloud-music-mode-map
	(let ((map (make-sparse-keymap)))
		(define-key map "q" 'netease-cloud-music-close)
		(define-key map (kbd "SPC") 'netease-cloud-music-pause-or-continue)
		(define-key map (kbd "RET") 'netease-cloud-music-play-song-at-point)
		(define-key map "f" 'netease-cloud-music-search-song)
		(define-key map "d" 'netease-cloud-music-delete-song-from-playlist)
		(define-key map "P" 'netease-cloud-music-play-playlist)
		(define-key map "p" 'netease-cloud-music-play-previous-song)
		(define-key map "n" 'netease-cloud-music-play-next-song)
		(define-key map "x" 'netease-cloud-music-kill-current-song)
		(define-key map ">" 'netease-cloud-music-seek-forward)
		(define-key map "<" 'netease-cloud-music-seek-backward)
		(define-key map "a" 'netease-cloud-music-add-to-playlist)
		(define-key map "r" 'netease-cloud-music-change-repeat-mode)
		(define-key map "?" 'describe-mode)
		(define-key map "h" 'describe-mode)
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
	(if (get-buffer netease-cloud-music-buffer-name)
			(switch-to-buffer netease-cloud-music-buffer-name)
		(unless (buffer-live-p (get-buffer netease-cloud-music-buffer-name))
			(switch-to-buffer netease-cloud-music-buffer-name))
		(netease-cloud-music-mode)
		(netease-cloud-music-interface-init)))

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
		(if netease-cloud-music-lyric-timer
			(cancel-timer netease-cloud-music-lyric-timer))
		(setq netease-cloud-music-process nil
					netease-cloud-music-current-song nil
					netease-cloud-music-play-status "")
		(when (get-buffer "*netease-cloud-music-play:process*")
			(kill-buffer "*netease-cloud-music-play:process*"))))

(defun netease-cloud-music-close ()
	"Close Netease Music and kill the process."
	(interactive)
	(netease-cloud-music-kill-process)
	(setq netease-cloud-music-process-status ""
				netease-cloud-music-repeat-mode "")
	(setq netease-cloud-music-playlist-song-index 0)
	(kill-buffer netease-cloud-music-buffer-name))

(defmacro netease-cloud-music-expand-form (&rest form)
	"Expand form in function-form."
	`(function*
		(lambda (&key data &allow-other-keys)
			,@form)))

(defun netease-cloud-music-request-from-api (content &key type)
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
		result))

(cl-defun netease-cloud-music-ask (type)
	"Ask user TYPE of the question.
If user reply y, return t.
Otherwise return nil."
	(let (result)
		(pcase type
			('song
			 (setq result (read-minibuffer "The info of the song is here, do you want to listen it?(y/n)" "y")))
			('add-song-to-playlist
			 (setq result (read-minibuffer "Do you want to add the current song into playlist?(y/n)" "y")))
			('delete-song-from-playlist
			 (setq result (read-minibuffer "Do you want to delete the song from playlist?(y/n)" "y"))))
		(when (string= result "y")
			(cl-return-from netease-cloud-music-ask t))))

(cl-defun netease-cloud-music-read-json (data &key sid sname aid aname)
	"Read the Netease Music json DATA and return the result.

SID is the song-id.

SNAME is the song-name.

AID is the artist-id.

ANAME is the artist-name."
	(let (song-json r-sid r-sname r-aid r-aname)
		(if (eq (car (cadar data)) 'queryCorrected)
				(setq song-json (aref (cdar
															 (cddar data)) 0))
			(setq song-json (aref (cdr
														 (cadar data)) 0)))
		(when sid
			(setq r-sid (cdar song-json)))
		(when sname
			(setq r-sname
						(cdadr song-json)))
		(when aid
			(setq r-aid
						(cdar (aref
									 (cdar (cddr song-json)) 0))))
		(when aname
			(setq r-aname
						(cdadr (aref
										(cdar (cddr song-json)) 0))))
		(list r-sid r-sname r-aid r-aname)))

(defun netease-cloud-music-search-song (song-name)
	"Search SONG-NAME from Netease Music and return the song id.
SONG-NAME is a string."
	(interactive "MEnter the song name: ")
	(if (string= song-name "")
			(error "[Netease-Cloud-Music]: You can't enter a null string!")

		;; Assigned the data from api to variables and call the functions to play the song
		(let* ((artist-name (read-string "Enter the artist name: "))
					 (search-result
						(netease-cloud-music-read-json
						 (netease-cloud-music-request-from-api (format
																										"%s %s"
																										song-name artist-name)
																									 :type 'song)
						 :sid t :sname t :aname t))
					 (result-song-id (car search-result))
					 (result-song-name (nth 1 search-result))
					 (result-artist-name (nth 3 search-result)))
			(if (and (not (string= artist-name "")) (not (string-match artist-name result-artist-name)))
					(message "[Netease-Cloud-Music]: Can't find the song.")
				(netease-cloud-music-interface-init
				 :content (list result-song-name result-artist-name)
				 :type 'song-ask)
				(if (netease-cloud-music-ask 'song)
						(progn
							(when (string= netease-cloud-music-repeat-mode "")
								(setq netease-cloud-music-repeat-mode "song"))
							(netease-cloud-music-play result-song-id result-song-name result-artist-name "song"))
					(message "[Netease-Cloud-Music]: Now, the song catched won't be played.")
					(netease-cloud-music-interface-init))))))

(defun netease-cloud-music-change-repeat-mode ()
	"Change the repeat mode."
	(interactive)
	(if (string= netease-cloud-music-repeat-mode "")
			(message
			 "[Netease-Cloud-Music]: The repeat mode is in the initialization state. when you start playing song, it'll be set.")
		(cond ((string= netease-cloud-music-play-status "song")
					 (if (string= netease-cloud-music-repeat-mode "song")
							 (setq netease-cloud-music-repeat-mode "off")
						 (setq netease-cloud-music-repeat-mode "song")))
					((string= netease-cloud-music-play-status "playlist")
					 (if (string= netease-cloud-music-repeat-mode "off")
							 (setq netease-cloud-music-repeat-mode "song")
						 (if (string= netease-cloud-music-repeat-mode "song")
								 (setq netease-cloud-music-repeat-mode "playlist")
							 (setq netease-cloud-music-repeat-mode "off")))))
		(netease-cloud-music-interface-init)))

(cl-defun netease-cloud-music-interface-init (&key content type)
	"Initialize the Netease Music buffer interface.
CONTENT is a cons, its value is variable with TYPE.

TYPE is a symbol, its value can be song or song-ask.

When TYPE is song-ask, CONTENT can be:

'(music-name . artist-name)

If CONTENT is nil and TYPE is not song, it will print the init content."
	(with-current-buffer netease-cloud-music-buffer-name
		(setq buffer-read-only nil)
		(erase-buffer)
		(insert (propertize
						 "Netease Cloud Music - 网易云音乐\n"
						 'face '(:height 1.1 :foreground "Red3")))
		;; Show the repeat mode status
		(insert (concat
						 (propertize "\nRepeat: "
												 'face '(:foreground "DeepSkyBlue"))
						 (propertize (if (string= netease-cloud-music-repeat-mode "song")
														 "SONG\n"
													 (if (string= netease-cloud-music-repeat-mode "playlist")
															 "PLAYLIST\n"
														 "OFF\n"))
												 'face '(:foreground "LightPink" :weight bold))))
		;; When the type is song, insert the current song info.
		(when (not (null netease-cloud-music-current-song))
			(insert "\n")
			(insert (concat
							 (propertize
								"Current song: "
								'face '(:height 0.9 :weight bold))
							 (propertize (format
														"%s - %s"
														(car netease-cloud-music-current-song)
														(nth 1 netease-cloud-music-current-song))
													 'face '(:height 1.0 :foreground "MediumSpringGreen"))
							 (if (string= netease-cloud-music-process-status "playing")
									 (propertize " [Playing]\n"
															 'face '(:foreground "OrangeRed"))
								 (propertize " [Paused]\n"
														 'face '(:foreground "OrangeRed"))))))
		;; Show the song infomation
		(when content
			(insert "\n")
			(pcase type
				('song-ask
				 (let ((song-name (car content))
							 (artist-name (nth 1 content)))
					 (insert (concat
										(propertize "Song name: "
																'face '(:weight bold))
										(propertize (format
																 "%s\n" song-name "")
																'face '(:foreground "Cyan3"))
										(propertize "Artist name: "
																'face '(:weight bold))
										(propertize (format
																 "%s\n" artist-name)
																'face '(:foreground "Cyan3"))))))))
		;; Playlist
		(let ((playlist
					 (netease-cloud-music-get-playlist 'list)))
			(when playlist
				(insert (propertize "\nPlaylist:\n"
														'face '(:height 1.05 :foreground "gold2")))
				(dolist (song playlist)
					(insert (propertize
									 (format "%s\n" song)
									 'face '(:foreground "honeydew4"))))))
		(setq buffer-read-only t)
		(goto-line 4)))

(defun netease-cloud-music-get-lyric (song-id)
	(request
	;;netease-cloud-music-lyric-api
	(format "http://music.163.com/api/song/lyric?id=%s&lv=1&kv=1&tv=-1" song-id)
	:type "GET"
	:parser 'json-read
	:success (cl-function
		(lambda (&key data &allow-other-keys)
				(setq result data)))
	:sync t)
	(let ((value (loop for i in result
					if (eq (car i) 'lrc)
					return (cdr i))))
		(loop for i in value
			if (eq (car i) 'lyric)
			return (cdr i))))

(defun netease-cloud-music-play (song-id song-name artist-name play-type)
	"Play the song by its SONG-ID and update the interface with SONG-NAME"
	(if (null song-id)
			(error "[Netease-Cloud-Music]: There's no song-id!")
		(netease-cloud-music-kill-process)
		(setq netease-cloud-music-process
					(async-start-process "netease-cloud-music-play:process"
															 (car netease-cloud-music-player-command)
															 nil
															 (nth 1 netease-cloud-music-player-command)
															 (concat netease-cloud-music-song-link
																			 (format "%s"
																							 song-id))))
		(set-process-sentinel netease-cloud-music-process
													'netease-cloud-music-process-sentinel)
		(if netease-cloud-music-show-lyric
			(setq netease-cloud-music-lyric (netease-cloud-music-get-lyric song-id))
			(setq netease-cloud-music-lyric nil))
		(if (and netease-cloud-music-lyric netease-cloud-music-show-lyric)
			(setq netease-cloud-music-lyric (split-string netease-cloud-music-lyric "\n")
				netease-cloud-music-lyric-timer (run-with-timer  0.5 1 (lambda ()
																			(if (and (get-buffer "*netease-cloud-music-play:process*")
																					(length netease-cloud-music-lyric))
																				(with-current-buffer "*netease-cloud-music-play:process*"
																					(goto-char (point-max))
																					(if (search-backward "[KA: " nil t)
																						(let ((current-lyric (car netease-cloud-music-lyric))
																							(current-song-time (buffer-substring-no-properties (+ 8 (point))
																																				(+ 13 (point)))))
																						(unless (string> (substring current-lyric 1 6) current-song-time)
																							(message (substring current-lyric 11))
																							(setq netease-cloud-music-lyric (cdr netease-cloud-music-lyric))))))
																				(cancel-timer netease-cloud-music-lyric-timer))))))
		(setq netease-cloud-music-process-status "playing")
		(setq netease-cloud-music-current-song
					`(,song-name ,artist-name ,song-id))
		(setq netease-cloud-music-play-status play-type)
		(netease-cloud-music-interface-init)))

(defun netease-cloud-music-playlist-play ()
	"The function for playlist play mode to play song in the playlist."
	(let ((result
				 (netease-cloud-music-read-json
					(netease-cloud-music-request-from-api
					 (nth netease-cloud-music-playlist-song-index
								netease-cloud-music-playlist)
					 :type 'song)
					:sid t :sname t :aname t)))
		(when (string= netease-cloud-music-repeat-mode "")
			(setq netease-cloud-music-repeat-mode "playlist"))
		(netease-cloud-music-play (car result)
															(nth 1 result)
															(nth 3 result)
															"playlist")))

(defun netease-cloud-music-playlist-include-song (song-name)
	"Check if the SONG-NAME is in the playlist.
If the song is included, return t.
Otherwise return nil."
	(let ((include nil)
				(playlist (with-temp-buffer
										(insert-file-contents (concat
																					 netease-cloud-music-cache-directory
																					 "playlist.txt"))
										(split-string (buffer-string) "\n" t))))
		(dolist (song playlist)
			(when (string= song-name (concat song "\n"))
				(setq include t)))
		include))

(defun netease-cloud-music-playlist-get-index (song-name)
	"Get the song index in playlist by its SONG-NAME."
	(let (song-index)
		(dolist (song netease-cloud-music-playlist)
			(when (string= song-name (concat song "\n"))
				(setq song-index (cl-position song netease-cloud-music-playlist))))
		song-index))

(defun netease-cloud-music-play-playlist ()
	"Play all of the song in playlist."
	(interactive)
	(let ((song-at-point (thing-at-point 'line t)))
		(setq netease-cloud-music-playlist
					(netease-cloud-music-get-playlist 'list))
		(if (null netease-cloud-music-playlist)
				(error "[Netease-Cloud-Music]: There's no song in the playlist.")
			(if (or (null song-at-point) (string-match "Playlist:"
																								 song-at-point)
							(null (netease-cloud-music-playlist-include-song
										 song-at-point)))
					;; Set the playlist song index when there's no song name at point
					(setq netease-cloud-music-playlist-song-index 0)
				;; Set the playlist song index as the song at point
				(setq netease-cloud-music-playlist-song-index
							(netease-cloud-music-playlist-get-index song-at-point))))
		(netease-cloud-music-playlist-play)))

(defun netease-cloud-music-play-song-at-point ()
	"Play the song at the point."
	(interactive)
	(let ((song-info (thing-at-point 'line t)))
		(if (not song-info)
				(error "[Netease-Cloud-Music]: There's no song at point.")
			(setq search-result (netease-cloud-music-read-json
													 (netease-cloud-music-request-from-api
														song-info :type 'song)
													 :sid t :sname t :aname t))
			(when (string= netease-cloud-music-repeat-mode "")
				(setq netease-cloud-music-repeat-mode "song"))
			(netease-cloud-music-play (car search-result)
																(nth 1 search-result)
																(nth 3 search-result)
																"song"))))

(defun netease-cloud-music-process-sentinel (process event)
	"The sentinel of Netease Music process."
	(when (string-match "\\(exit\\|finished\\)" event)
		(cond ((string= netease-cloud-music-repeat-mode "off")
					 (netease-cloud-music-kill-current-song))
					((string= netease-cloud-music-play-status "song")
					 (setq netease-cloud-music-process-status "")
					 (if (string= netease-cloud-music-repeat-mode "off")
							 (netease-cloud-music-kill-process)
						 (netease-cloud-music-play
							(nth 2 netease-cloud-music-current-song)
							(car netease-cloud-music-current-song)
							(nth 1 netease-cloud-music-current-song)
							"song"))
					 (netease-cloud-music-interface-init)
					 (cond ((and (string-match "\\(finished\\)" event) (string= netease-cloud-music-repeat-mode "off"))
									(message "[Netease-Cloud-Music]: The current song is over."))
								 ((string-match "\\(exit\\)" event)
									(message "[Netease-Cloud-Music]: There is a problem when playing the current song."))))
					((string= netease-cloud-music-play-status "playlist")
					 (if (string= netease-cloud-music-repeat-mode "playlist")
							 (netease-cloud-music-play-next-song)
						 (if (string= netease-cloud-music-repeat-mode "off")
								 (progn
									 (netease-cloud-music-kill-current-song)
									 (message "[Netease-Cloud-Music]: The current song is over."))
							 (netease-cloud-music-play
								(nth 2 netease-cloud-music-current-song)
								(car netease-cloud-music-current-song)
								(nth 1 netease-cloud-music-current-song)
								"playlist")))))))

(defun netease-cloud-music-play-previous-song ()
	"Play the previous song in the playlist."
	(interactive)
	(if (not (string= netease-cloud-music-play-status "playlist"))
			(previous-line)
		(let ((previous-song-index
					 (- netease-cloud-music-playlist-song-index 1)))
			(if (or (null (nth previous-song-index netease-cloud-music-playlist))
							(< previous-song-index 0))
					(if (null netease-cloud-music-repeat-mode)
							(error "[Netease-Cloud-Music]: There's no song previous.")
						(setq netease-cloud-music-playlist-song-index
									(- (length netease-cloud-music-playlist) 1)))
				(setq netease-cloud-music-playlist-song-index previous-song-index))
			(netease-cloud-music-playlist-play))))

(defun netease-cloud-music-play-next-song ()
	"Play the next song in the playlist."
	(interactive)
	(if (not (string= netease-cloud-music-play-status "playlist"))
			(next-line)
		(let ((next-song-index
					 (+ netease-cloud-music-playlist-song-index 1)))
			(if (null (nth next-song-index netease-cloud-music-playlist))
					(if (string= netease-cloud-music-repeat-mode "off")
							(progn
								(netease-cloud-music-kill-current-song)
								(message "[Netease-Cloud-Music]: The playlist songs over."))
						(setq netease-cloud-music-playlist-song-index 0)
						(netease-cloud-music-playlist-play))
				(setq netease-cloud-music-playlist-song-index next-song-index)
				(netease-cloud-music-playlist-play)))))

(defun netease-cloud-music-pause-or-continue ()
	"Pause or continue play the current song."
	(interactive)
	(when (netease-cloud-music-process-live-p)
		(pcase netease-cloud-music-process-status
			("playing"
			 (process-send-string netease-cloud-music-process
														(nth 3 netease-cloud-music-player-command))
			 (setq netease-cloud-music-process-status "paused"))
			("paused"
			 (process-send-string netease-cloud-music-process
														(nth 2 netease-cloud-music-player-command))
			 (setq netease-cloud-music-process-status "playing")
			 (netease-cloud-music-interface-init)))
		(netease-cloud-music-interface-init)))

(defun netease-cloud-music-kill-current-song ()
	"Kill the current song."
	(interactive)
	(if (not (netease-cloud-music-process-live-p))
			(error "[Netease-Cloud-Music]: There's no song playing.")
		(netease-cloud-music-kill-process)
		(setq netease-cloud-music-process-status "")
		(netease-cloud-music-interface-init)))

(defun netease-cloud-music-seek-forward ()
	"Seek forward the current song."
	(interactive)
	(when (netease-cloud-music-process-live-p)
		(process-send-string netease-cloud-music-process
												 (nth 4 netease-cloud-music-player-command))))

(defun netease-cloud-music-seek-backward ()
	"Seek backward the current song."
	(interactive)
	(when (netease-cloud-music-process-live-p)
		(process-send-string netease-cloud-music-process
												 (nth 5 netease-cloud-music-player-command))))

(defun netease-cloud-music-add-to-playlist ()
	"Add the current playing song to playlist."
	(interactive)
	(let ((playlist-file
				 (concat netease-cloud-music-cache-directory "playlist.txt")))
		(if (null netease-cloud-music-current-song)
				(error "[Netease-Cloud-Music]: There's no song is playing.")
			(unless (file-exists-p playlist-file)
				(make-empty-file playlist-file))
			(when (netease-cloud-music-ask 'add-song-to-playlist)
				(with-temp-file playlist-file
					(insert (concat
									 (format "%s  %s\n"
													 (car netease-cloud-music-current-song)
													 (nth 1 netease-cloud-music-current-song))
									 (netease-cloud-music-get-playlist 'string))))
				(message "[Netease-Cloud-Music]: %s is added to playlist."
								 (car netease-cloud-music-current-song)))
			(netease-cloud-music-interface-init))))

(defun netease-cloud-music-delete-song-from-playlist ()
	"Delete the song at point from playlist."
	(interactive)
	(let ((song-name (thing-at-point 'line t))
				(playlist-songs (with-temp-buffer
													(insert-file-contents (concat
																								 netease-cloud-music-cache-directory
																								 "playlist.txt"))
													(split-string (buffer-string) "\n" t))))
		(when (netease-cloud-music-ask 'delete-song-from-playlist)
			(if (null (netease-cloud-music-playlist-include-song song-name))
					(error "[Netease-Cloud-Music]: The song at point is not included in playlist.")
				(with-temp-file (concat netease-cloud-music-cache-directory
																"playlist.txt")
					(dolist (song playlist-songs)
						(unless (string= song-name (concat song "\n"))
							(insert (format "%s\n" song)))))
				(message "[Netease-Cloud-Music]: Delete the song from playlist."))
			(netease-cloud-music-interface-init))))

(defun netease-cloud-music-get-playlist (type)
	"Get the playlist and return it.

TYPE is the playlist type.If it's list, return the playlist as list.
Else if it's string, return the playlist as string."
	(let ((playlist-file (concat
												netease-cloud-music-cache-directory
												"playlist.txt"))
				(playlist-contents))
		(if (not (file-exists-p playlist-file))
				nil
			(with-temp-buffer
				(insert-file-contents playlist-file)
				(pcase type
					('list
					 (split-string (buffer-string) "\n" t))
					('string
					 (buffer-string)))))))

(provide 'netease-cloud-music)
