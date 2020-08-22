;;;; Netease Music :: A netease music client in Emacs.

;;; Intro
;; Copyright (C) 2020 SpringHan

;; Author: SpringHan <springchohaku@qq.com>
;; Maintainer: SpringHan

;; Version: 1.0
;; License: MIT

;; Last Change: <+++>
;; Repository: https://github.com/SpringHan/netease-music.el

(require 'cl-lib)
(require 'request)
(require 'json)

(defgroup netease-music nil
	"Netease Music group"
	:group 'applications)

(defcustom netease-music-mode-hook nil
	"The hook for Netease Music mode."
	:type 'hook
	:group 'netease-music)

(defcustom netease-music-process nil
	"The process of Netease Music."
	:group 'netease-music)

(defcustom netease-music-current-song nil
	:type 'string
	:group 'netease-music)

(defconst netease-music-buffer-name "*Netease-Music*"
	"The name of Netease Music buffer.")

(defconst netease-music-player "mplayer"
	"The player of Netease Music.")

(defconst netease-music-search-api
	"http://music.163.com/api/search/get/"
	"The search api of Netease Music.")

(defvar netease-music-mode-map
	(let ((map (make-sparse-keymap)))
		(define-key map "q" 'netease-music-close)
		(define-key map (kbd "SPC") 'netease-music-pause-or-continue)
		(define-key map "f" 'netease-music-search-song)
		(define-key map "n" 'next-line)
		(define-key map "p" 'previous-line)
		map)
	"Netease Music mode map.")

(define-derived-mode netease-music-mode nil "Netease-Music"
	"The mode of Netease Music mode."
	:group 'netease-music
	:abbrev-table nil
	:syntax-table nil
	(linum-mode -1)
	(setq buffer-read-only t
				truncate-lines t))

;;;###autoload
(defun netease-music ()
	"Initialize the Netease Music buffer in netease-music-mode."
	(interactive)
	(unless (buffer-live-p (get-buffer netease-music-buffer-name))
		(switch-to-buffer netease-music-buffer-name))
	(netease-music-mode))

(defun netease-music-kill-process ()
	"Kill the Netease Music process."
	(when (and netease-music-process
						 (/= (process-live-p netease-music-process) 0))
		(delete-process netease-music-process)
		(setq netease-music-process nil)))

(defun netease-music-close ()
	"Close Netease Music and kill the process."
	(interactive)
	(netease-music-kill-process)
	(kill-buffer netease-music-buffer-name))

(cl-defun netease-music-request-from-api (content &key (type 'song))
	"Request the CONTENT from Netease Music API."
	(let (result)
		(pcase type
			(song
			 (request
				 netease-music-search-api
				 :auth "digest"
				 :data '(("s" . content)
								 ("limit" . "1")
								 ("type" . "1"))
				 :parser 'json-read
				 :success (cl-function
									 (lambda (&key data &allow-other-keys)
										 (setq result data))))))
		(cl-return-from netease-music-request-from-api result)))

(defun netease-music-search-song (song-name)
	"Search SONG-NAME from Netease Music."
	(interactive "MEnter the song name: ")
	(if (string= song-name "")
			(error "You can't enter a null string!")
		(let ((song-result (netease-music-request-from-api song-name))))))

(provide 'netease-music)
