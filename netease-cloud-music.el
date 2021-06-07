;;; netease-cloud-music.el --- Netease Cloud Music client for Emacs -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 2.0
;; Package-Requires: ((cl-lib "1.0") (request) (json "1.4"))
;; Homepage: https://github.com/SpringHan/netease-cloud-music.git
;; Keywords: Player


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Netease Cloud Music Client for Emacs.

;;; Tips: The basic functions are in `netease-cloud-music-functions.el',
;;; Commands & functions about UI are in `netease-cloud-music-ui.el'.
;;; If you want to use the TUI, you should add

;;; (require 'netease-cloud-music-ui)

;;; In your configuration.

;;; Code:

(require 'cl-lib)
(require 'request)
(require 'json)

(require 'netease-cloud-music-functions)

(defgroup netease-cloud-music nil
  "Netease Music group"
  :group 'applications)

(defcustom netease-cloud-music-mode-hook nil
  "The hook for Netease Music mode."
  :type 'hook
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-switch-mode-hook nil
  "The hook for Netease Cloud Music Switch mode."
  :type 'hook
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-process nil
  "The process of Netease Music."
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-api-process nil
  "The third-party api process."
  :type 'process
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-last-buffer nil
  "The buffer before calling `netease-cloud-music' funciton."
  :type 'buffer
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-api-buffer "*Netease-API*"
  "The third-party api process buffer."
  :type 'buffer
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-api-port "3000"
  "The port for the API process."
  :type 'string
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-current-song nil
  "The current playing song."
  :type 'list
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-process-status ""
  "The status of Netease Music process."
  :type 'string
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-cache-directory
  (locate-user-emacs-file "netease-cloud-music")
  "The cache directory."
  :type 'string
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-playlist-file
  (expand-file-name "ncm-playlist" netease-cloud-music-cache-directory)
  "The cache directory of Netease Music."
  :type 'string
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-playlist nil
  "The list of the playlist songs."
  :type 'list
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-backup-playlist nil
  "The backup playlist."
  :type 'list
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-playlists nil
  "The playlists of the user."
  :type 'list
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-playlist-song-index 0
  "The song index for the playlist song."
  :type 'number
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-show-lyric t
  "Wether show lyric."
  :type 'boolean
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-showed-lyric-buffer nil
  "The list of buffers showed lyrics."
  :type 'list
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-player-command
  '("mpv" "pause\n" "seek 5" "seek -5")
  "The player command for playing the online songs.
Its format is lick this:
'(command play-online-songs-arg continue-message
pause-message seek-forward-message seek-backward-message"
  :type 'list
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-search-page nil
  "The search page."
  :type 'number
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-search-alist nil
  "List for the songs by searching."
  :type 'list
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-search-playlists nil
  "The alist for the playlists by searching."
  :type 'list
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-login-timer nil
  "The timer for login."
  :type 'timer
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-use-local-playlist t
  "The type of the playlist now used.
If it's t, meaning to use the local playlist."
  :type 'boolean
  :group 'netease-cloud-music)

(defvar netease-cloud-music-lyric-timer nil
  "The timer of Netease Music lyric.")

(defvar netease-cloud-music-update-lyric-timer nil
  "The timer for updating lyrics.")

(defvar netease-cloud-music-lyric nil
  "The Netease Music lyric.")

(defvar netease-cloud-music-lyrics nil
  "The lyrics that will not be changed when playing.")

(defvar netease-cloud-music-current-lyric nil
  "Current lyric.")

(defvar netease-cloud-music-lyric-song nil
  "The song of current lyrics.")

(defconst netease-cloud-music-search-api
  "http://music.163.com/api/search/get/"
  "The search api of Netease Music.")

(defconst netease-cloud-music-song-link
  "http://music.163.com/song/media/outer/url?id="
  "The song link of Netease Music.")

(defconst netease-cloud-music-api-dir
  (expand-file-name "api/" netease-cloud-music-cache-directory)
  "The directory for the third-party api.")

(defconst netease-cloud-music-user-loginfo-file
  (expand-file-name "log-info" netease-cloud-music-cache-directory)
  "The login info file for the user.")

(defvar netease-cloud-music-repeat-mode ""
  "The repeat mode for Netease Cloud Music.")

(defvar netease-cloud-music-search-limit 10
  "The search limit for Netease Cloud Music.")

(defvar netease-cloud-music-phone nil
  "Phone number.")

(defvar netease-cloud-music-username nil
  "Username.")

(defvar netease-cloud-music-user-password nil
  "Password")

(defvar netease-cloud-music-user-id nil
  "User ID.")

(defalias 'na-defun 'netease-api-defun)

(defalias 'na-error 'netease-cloud-music-error
  "To be same as the `na-defun', so use this name.")

(defun netease-cloud-music-download-api ()
  "Download the third-party API."
  (interactive)
  (if (netease-cloud-music--api-downloaded)
      (na-error "The third-party API has been downloaded!")
    (async-shell-command
     (format "git clone https://github.com/SpringHan/NeteaseCloudMusicApi.git %s --depth=1 && cd %s && npm install"
             netease-cloud-music-api-dir
             netease-cloud-music-api-dir)
     (get-buffer-create "*Netease-Cloud-Music-Api-Preperation*"))))

(defun netease-cloud-music-process-live-p ()
  "Check if the Netease Music process is live.
If it's live, return t.
Otherwise return nil."
  (if (and netease-cloud-music-process
           (process-live-p netease-cloud-music-process))
      t
    nil))

(defun netease-cloud-music-kill-process ()
  "Kill the Netease Music process."
  (when (netease-cloud-music-process-live-p)
    (set-process-sentinel netease-cloud-music-process nil)
    (if (get-buffer " *netease-cloud-music-play:process*")
        (progn
          (delete-process " *netease-cloud-music-play:process*")
          (kill-buffer " *netease-cloud-music-play:process*"))
      (delete-process netease-cloud-music-process))
    (when netease-cloud-music-lyric-timer
      (netease-cloud-music-cancel-timer))
    (setq netease-cloud-music-process nil
          netease-cloud-music-current-song nil)))

(defun netease-cloud-music-quit ()      ;This command is just used for initialize the vars when exiting.
  "Quit the music client."
  (interactive)
  (netease-cloud-music-kill-process)
  (setq netease-cloud-music-process-status "")
  (setq netease-cloud-music-search-page nil
        netease-cloud-music-lyric nil
        netease-cloud-music-lyrics nil
        netease-cloud-music-update-lyric-timer nil
        netease-cloud-music-lyric-song nil
        netease-cloud-music-last-buffer nil)
  (netease-cloud-music-cancel-timer t)
  (netease-cloud-music-save-playlist)
  (netease-cloud-music-stop-api t)
  (when (get-buffer " *Request*")
    (kill-buffer " *Request*")))

(defmacro netease-cloud-music-expand-form (&rest form)
  "Expand form in function-form."
  `(cl-function
    (lambda (&key data &allow-other-keys)
      ,@form)))

(defun netease-cloud-music-search-song (song-name)
  "Search SONG-NAME from Netease Music and return the song id.
SONG-NAME is a string."
  (interactive "MEnter the song name: ")
  (let* ((artist-name (read-string "Enter the artist name(can be null): "))
         (search-content (format "%s %s" song-name artist-name))
         (search-result
          (netease-cloud-music-get-song
           (netease-cloud-music-request-from-api search-content nil
                                                 netease-cloud-music-search-limit))))
    (setq netease-cloud-music-search-alist
          (cons search-content search-result)
          netease-cloud-music-search-page 1)
    (netease-cloud-music-search-song--open-switch search-result)))

(defun netease-cloud-music-change-repeat-mode ()
  "Change the repeat mode."
  (interactive)
  (if (string= netease-cloud-music-repeat-mode "")
      (na-error
       "The repeat mode is in the initialization state. when you start playing song, it'll be set!")
    (setq netease-cloud-music-repeat-mode
          (pcase netease-cloud-music-repeat-mode
            ("off" "song")
            ("song" "playlist")
            ("playlist" "random")
            ("random" "off")))
    (netease-cloud-music-interface-init)))

;; TODO: Add translated lyric
(defun netease-cloud-music-get-lyric (song-id)
  (request
    ;;netease-cloud-music-lyric-api
    (format "http://music.163.com/api/song/lyric?id=%s&lv=1&kv=1&tv=-1" song-id)
    :type "GET"
    :parser 'json-read
    :success (netease-cloud-music-expand-form
              (setq result data))
    :sync t)
  (let ((value (loop for i in result
                     if (eq (car i) 'lrc)
                     return (cdr i))))
    (loop for i in value
          if (eq (car i) 'lyric)
          return (cdr i))))

(defun netease-cloud-music-play (song-id song-name artist-name)
  "Play the song by its SONG-ID and update the interface with SONG-NAME"
  (if (null song-id)
      (na-error "There's no song-id!")
    (netease-cloud-music-kill-process)
    (setq netease-cloud-music-process
          (start-process "netease-cloud-music-play:process"
                         " *netease-cloud-music-play:process*"
                         (car netease-cloud-music-player-command)
                         (if netease-cloud-music-user-id
                             (netease-cloud-music--song-url-by-user song-id)
                           (concat netease-cloud-music-song-link
                                   (number-to-string song-id)))
                         (if (string= (car netease-cloud-music-player-command)
                                      "mpv")
                             "--input-ipc-server=/tmp/mpvserver"
                           "")))
    (set-process-sentinel netease-cloud-music-process
                          'netease-cloud-music-process-sentinel)
    (if netease-cloud-music-show-lyric
        (progn
          (setq netease-cloud-music-lyric (netease-cloud-music-get-lyric song-id))
          (with-current-buffer " *netease-cloud-music-play:process*"
            (erase-buffer))
          (with-current-buffer netease-cloud-music-buffer-name
            (netease-cloud-music-add-header-lyrics))
          (setq netease-cloud-music-current-lyric nil))
      (setq netease-cloud-music-lyric nil))
    (setq netease-cloud-music-process-status "playing")
    (setq netease-cloud-music-current-song
          `(,song-name ,artist-name ,song-id))
    (netease-cloud-music-adjust-song-index)
    (if (and netease-cloud-music-lyric netease-cloud-music-show-lyric)
        (setq netease-cloud-music-lyric (split-string netease-cloud-music-lyric "\n")
              netease-cloud-music-lyrics netease-cloud-music-lyric
              netease-cloud-music-lyric-song song-id
              netease-cloud-music-lyric-timer
              (run-with-timer
               0.5 1
               (lambda ()
                 (if (and (get-buffer " *netease-cloud-music-play:process*")
                          (length netease-cloud-music-lyric)
                          (eq netease-cloud-music-lyric-song
                              (nth 2 netease-cloud-music-current-song)))
                     (with-current-buffer " *netease-cloud-music-play:process*"
                       (goto-char (point-max))
                       (when (and (search-backward "[K[0mA" nil t)
                                  (not (string= netease-cloud-music-process-status
                                                "paused"))
                                  (not (string= netease-cloud-music-process-status
                                                "")))
                         (let* ((current-lyric (car netease-cloud-music-lyric))
                                (current-song-time (buffer-substring-no-properties (+ 12 (point))
                                                                                   (+ 17 (point))))
                                (current-lyric-time (ignore-errors
                                                      (substring current-lyric 1 6))))
                           (when (and (stringp current-lyric-time)
                                      (not
                                       (netease-cloud-music--string>
                                        current-lyric-time current-song-time)))
                             (setq netease-cloud-music-current-lyric
                                   (netease-cloud-music--current-lyric current-lyric))
                             (setq netease-cloud-music-lyric (cdr netease-cloud-music-lyric))))))
                   (netease-cloud-music-cancel-timer))))))
    (netease-cloud-music-interface-init)))

(defun netease-cloud-music-playlist-play ()
  "Play the playlist songs."
  (interactive)
  (when (string= netease-cloud-music-repeat-mode "")
    (setq netease-cloud-music-repeat-mode "playlist"))
  (let (current-song song)
    (when (and (ignore-errors
                 (eq (key-binding (read-kbd-macro (char-to-string last-input-event)))
                     'netease-cloud-music-playlist-play))
               (string= (buffer-name (current-buffer))
                        netease-cloud-music-buffer-name))
      (setq current-song (netease-cloud-music--current-song)))
    (when current-song
      (setq netease-cloud-music-playlist-song-index current-song))
    (setq song
          (nth netease-cloud-music-playlist-song-index netease-cloud-music-playlist))
    (netease-cloud-music-play (car song)
                              (nth 1 song)
                              (nth 3 song))))

(defun netease-cloud-music-process-sentinel (process event)
  "The sentinel of Netease Music process."
  (when (string-match "\\(finished\\|Exiting\\|killed\\|exited\\)" event)
    (pcase netease-cloud-music-repeat-mode
      ("off" (netease-cloud-music-kill-current-song))
      ("song"
       (setq netease-cloud-music-process-status "")
       (netease-cloud-music-play
        (nth 2 netease-cloud-music-current-song)
        (car netease-cloud-music-current-song)
        (nth 1 netease-cloud-music-current-song))
       (netease-cloud-music-interface-init))
      ("playlist"
       (netease-cloud-music-play-next-song))
      ("random"
       (netease-cloud-music-random-play)))))

(defun netease-cloud-music-play-previous-song ()
  "Play the previous song in the playlist."
  (interactive)
  (if (string= netease-cloud-music-process-status "")
      (previous-line)
    (let ((previous-song-index
           (- netease-cloud-music-playlist-song-index 1)))
      (if (or (null (nth previous-song-index netease-cloud-music-playlist))
              (< previous-song-index 0))
          (if (string= netease-cloud-music-repeat-mode "off")
              (na-error "There's no song previous.")
            (setq netease-cloud-music-playlist-song-index
                  (- (length netease-cloud-music-playlist) 1)))
        (setq netease-cloud-music-playlist-song-index previous-song-index))
      (netease-cloud-music-playlist-play))))

(defun netease-cloud-music-play-next-song ()
  "Play the next song in the playlist."
  (interactive)
  (if (string= netease-cloud-music-process-status "")
      (next-line)
    (let ((next-song-index
           (+ netease-cloud-music-playlist-song-index 1)))
      (if (null (nth next-song-index netease-cloud-music-playlist))
          (if (string= netease-cloud-music-repeat-mode "off")
              (progn
                (netease-cloud-music-kill-current-song)
                (na-error "The playlist songs over!"))
            (setq netease-cloud-music-playlist-song-index 0)
            (netease-cloud-music-playlist-play))
        (setq netease-cloud-music-playlist-song-index next-song-index)
        (netease-cloud-music-playlist-play)))))

(defun netease-cloud-music-pause-or-continue ()
  "Pause or continue play the current song."
  (interactive)
  (when (netease-cloud-music-process-live-p)
    (process-send-string netease-cloud-music-process
                         (nth 1 netease-cloud-music-player-command))
    (pcase netease-cloud-music-process-status
      ("playing"
       (setq netease-cloud-music-process-status "paused"))
      ("paused"
       (setq netease-cloud-music-process-status "playing")))
    (netease-cloud-music-interface-init)))

(defun netease-cloud-music-kill-current-song ()
  "Kill the current song."
  (interactive)
  (when (netease-cloud-music-process-live-p)
    (netease-cloud-music-kill-process)
    (setq netease-cloud-music-process-status "")
    (netease-cloud-music-interface-init)))

(defun netease-cloud-music-seek-forward ()
  "Seek forward the current song."
  (interactive)
  (when (netease-cloud-music-process-live-p)
    (if (string= "mpv" (car netease-cloud-music-player-command))
        (progn
          (shell-command (concat "echo '"
                                 (nth 2 netease-cloud-music-player-command)
                                 "' | socat - /tmp/mpvserver")
                         "*shell-output*" nil)
          (when (get-buffer "*shell-output*")
            (kill-buffer "*shell-output*")))
      (process-send-string netease-cloud-music-process
                           (nth 2 netease-cloud-music-player-command)))
    (netease-cloud-music-update-lyrics)))

(defun netease-cloud-music-seek-backward ()
  "Seek backward the current song."
  (interactive)
  (when (netease-cloud-music-process-live-p)
    (if (string= "mpv" (car netease-cloud-music-player-command))
        (progn
          (shell-command (concat "echo '"
                                 (nth 3 netease-cloud-music-player-command)
                                 "' | socat - /tmp/mpvserver")
                         "*shell-output*" nil)
          (when (get-buffer "*shell-output*")
            (kill-buffer "*shell-output*")))
      (process-send-string netease-cloud-music-process
                           (nth 3 netease-cloud-music-player-command)))
    (when netease-cloud-music-update-lyric-timer
      (cancel-timer netease-cloud-music-update-lyric-timer))
    (setq netease-cloud-music-update-lyric-timer
          (run-with-timer
           1.1 nil
           (lambda ()
             (netease-cloud-music-update-lyrics t))))))

(defun netease-cloud-music-delete-song-from-playlist ()
  "Delete current song from playlist."
  (interactive)
  (let ((song (netease-cloud-music--current-song))
        (current-line (line-number-at-pos)))
    (when song
      (setq netease-cloud-music-playlist
            (delete (nth song netease-cloud-music-playlist)
                    netease-cloud-music-playlist))
      (netease-cloud-music-save-playlist)
      (netease-cloud-music-interface-init)
      (goto-char (point-min))
      (forward-line (1- current-line)))))

(defun netease-cloud-music-clear-playlist ()
  "Clear the playlist and kill current song."
  (interactive)
  (setq netease-cloud-music-playlist nil)
  (netease-cloud-music-kill-current-song))

(defun netease-cloud-music-add-header-lyrics ()
  "Add lyrics in current header."
  (interactive)
  (unless (netease-cloud-music--memeq '(:eval (netease-cloud-music-show-lyrics))
                                      header-line-format)
    (setq header-line-format (append header-line-format
                                     (list '(:eval (netease-cloud-music-show-lyrics)))))
    (add-to-list 'netease-cloud-music-showed-lyric-buffer
                 (buffer-name (current-buffer)))))

(defun netease-cloud-music-delete-header-lyrics (&optional all)
  "Delete lyrics in header line.
ALL means eval it in all of the `netease-cloud-music-showed-lyric-buffer'."
  (interactive)
  (let ((buffer (if all
                    netease-cloud-music-showed-lyric-buffer
                  (list (buffer-name (current-buffer))))))
    (mapc #'(lambda (b)
              (when (get-buffer b)
                (with-current-buffer b
                  (setq header-line-format
                        (delete '(:eval (netease-cloud-music-show-lyrics))
                                header-line-format)))))
          buffer)
    (if all
        (setq netease-cloud-music-showed-lyric-buffer nil)
      (setq netease-cloud-music-showed-lyric-buffer
            (delete (buffer-name (current-buffer))
                    netease-cloud-music-showed-lyric-buffer)))))

(defun netease-cloud-music-cancel-timer (&optional delete-lyrics)
  "Cancel timer and kill the lyrics."
  (when (timerp netease-cloud-music-lyric-timer)
    (cancel-timer netease-cloud-music-lyric-timer))
  (setq netease-cloud-music-lyric-timer nil)
  (setq netease-cloud-music-current-lyric nil)
  (when delete-lyrics
    (netease-cloud-music-delete-header-lyrics t)))

(defun netease-cloud-music-show-lyrics ()
  "Show lyrics."
  (concat (propertize (car netease-cloud-music-current-song)
                      'face 'font-lock-keyword-face)
          " - "
          (propertize (nth 1 netease-cloud-music-current-song)
                      'face 'font-lock-function-name-face)
          ": "
          (when (stringp netease-cloud-music-current-lyric)
            netease-cloud-music-current-lyric)))

(defun netease-cloud-music--string> (string1 string2)
  "Check if STRING1 is bigger than STRING2.
Mainly used to check the time."
  (ignore-errors
    (let (string1-prefix string1-end
                         string2-prefix string2-end)
      (progn
        (string-match "\\(.*\\):\\(.*\\)" string1)
        (setq string1-prefix (string-to-number (match-string 1 string1))
              string1-end (string-to-number (match-string 2 string1)))

        (string-match "\\(.*\\):\\(.*\\)" string2)
        (setq string2-prefix (string-to-number (match-string 1 string2))
              string2-end (string-to-number (match-string 2 string2))))
      (if (> string1-prefix string2-prefix)
          t
        (when (= string1-prefix string2-prefix)
          (when (> string1-end string2-end)
            t))))))

(defun netease-cloud-music-update-lyrics (&optional init)
  "Force Update the lyrics.
Optional argument means init the lyrics list."
  (when netease-cloud-music-show-lyric
    (when init
      (setq netease-cloud-music-lyric netease-cloud-music-lyrics))
    (with-current-buffer " *netease-cloud-music-play:process*"
      (when (search-backward "[K[0mA" nil t)
        (let ((current-song-time (buffer-substring-no-properties (+ 12 (point))
                                                                 (+ 17 (point))))
              (current-time (ignore-errors
                              (substring (car netease-cloud-music-lyric) 1 6)))
              current-lyric)
          (while (not
                  (netease-cloud-music--string> current-time current-song-time))
            (setq current-time
                  (ignore-errors
                    (substring (car netease-cloud-music-lyric) 1 6)))
            (setq current-lyric (car netease-cloud-music-lyric)
                  netease-cloud-music-lyric (cdr netease-cloud-music-lyric)))
          (setq netease-cloud-music-current-lyric
                (netease-cloud-music--current-lyric current-lyric)))))))

(defun netease-cloud-music-change-order (song num)
  "Change the song's to NUM."
  (interactive (let ((current-song (netease-cloud-music--current-song)))
                 (list current-song
                       (read-number (format "Enter the number for the order(current one is %S): "
                                            (1+ current-song))))))
  (if (null song)
      (na-error "The song is not exists!")
    (setq num (1- num))
    (let ((original-song (nth num netease-cloud-music-playlist))
          (song-info (nth song netease-cloud-music-playlist)))
      (cond ((> num (1- (length netease-cloud-music-playlist)))
             (setq netease-cloud-music-playlist
                   (append (list song-info)
                           (delete song-info netease-cloud-music-playlist))))
            ((< num 0)
             (setq netease-cloud-music-playlist
                   (append (delete song-info netease-cloud-music-playlist)
                           (list song-info))))
            (t (setf (nth num netease-cloud-music-playlist) song-info
                     (nth song netease-cloud-music-playlist) original-song)))
      (netease-cloud-music-save-playlist)
      (netease-cloud-music-interface-init)
      (netease-cloud-music-adjust-song-index))))

(defun netease-cloud-music-adjust-song-index ()
  "Adjust the current song's index."
  (let ((index (netease-cloud-music--current-song netease-cloud-music-current-song)))
    (when index
      (setq netease-cloud-music-playlist-song-index index))))

(defun netease-cloud-music-ask-play (song)
  "Play song by asking."
  (interactive
   (list
    (let (song-list)
      (dolist (song netease-cloud-music-playlist)
        (setq song-list (append song-list
                                (list (format "%s - %s"
                                              (nth 1 song)
                                              (nth 3 song))))))
      (completing-read "Enter the song: " song-list))))
  (let ((song-info (netease-cloud-music--current-song song)))
    (when song-info
      (setq song-info (nth song-info netease-cloud-music-playlist))
      (when (string= netease-cloud-music-repeat-mode "")
        (setq netease-cloud-music-repeat-mode "song"))
      (netease-cloud-music-play (car song-info)
                                (nth 1 song-info)
                                (nth 3 song-info)))))

(defun netease-cloud-music-save-playlist (&optional hint)
  "Save playlist to the cache file."
  (interactive (list t))
  (unless (file-exists-p netease-cloud-music-playlist-file)
    (make-empty-file netease-cloud-music-playlist-file))
  (with-temp-file netease-cloud-music-playlist-file
    (erase-buffer)
    (insert (format "%S" netease-cloud-music-playlist)))
  (when hint
    (message "[Netease-Cloud-Music]: The playlist has been saved into the cache file.")))

(defun netease-cloud-music-get-playlist ()
  "Set the playlist to the one which saved in the cache file."
  (let ((playlist (with-temp-buffer
                    (insert-file-contents netease-cloud-music-playlist-file)
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (setq netease-cloud-music-playlist (car (read-from-string playlist)))))

(defun netease-cloud-music-search-playlist (name)
  "Fetch the playlist by INFO.
INFO can be the id of playlist or its name."
  (interactive "MEnter the name of the playlist: ")
  (if (string= name "")
      (na-error "The name of playlist can not be null!")
    (let ((playlists
           (netease-cloud-music-get-playlists
            (netease-cloud-music-request-from-api
             name 'playlist netease-cloud-music-search-limit))))
      (setq netease-cloud-music-search-playlists
            (cons name playlists)
            netease-cloud-music-search-page 1)
      (netease-cloud-music-playlist--open-switch playlists))))

(defun netease-cloud-music-get-playlist-by-uid (uid)
  "Fetch playlist by UID."
  (interactive (list (read-string "Enter the uid: "
                                  (when netease-cloud-music-user-id
                                    (number-to-string netease-cloud-music-user-id)))))
  (let ((playlists (netease-cloud-music-get-user-playlist uid)))
    (netease-cloud-music-playlist--open-switch playlists)
    (setq netease-cloud-music-search-playlists playlists)))

(defun netease-cloud-music-playlist-enter ()
  "Add the playlist under the cursor."
  (interactive)
  (let ((playlist (alist-get
                   (substring (thing-at-point 'line) 0 -1)
                   netease-cloud-music-search-playlists nil nil 'string-equal)))
    (if (null playlist)
        (na-error "The playlist can not found!")
      (netease-cloud-music--append (netease-cloud-music-get-playlist-songs playlist))
      (netease-cloud-music-save-playlist)
      (netease-cloud-music-switch-close)
      (netease-cloud-music-interface-init))))

(defun netease-cloud-music-get-playlist-songs (pid)
  "Get the songs in the playlist whose if is PID."
  (let (songs song artist result)
    (request "https://music.163.com/api/playlist/detail"
      :type "POST"
      :data `(("id" . ,pid))
      :parser 'json-read
      :success (netease-cloud-music-expand-form (setq songs data))
      :sync t)
    (if (or (null songs) (/= 200 (alist-get 'code songs)))
        (na-error "The pid can not fount!")
      (setq songs (alist-get 'tracks (alist-get 'result songs)))
      (dotimes (n (length songs))
        (setq song (aref songs n)
              artist (aref (alist-get 'artists song) 0))
        (setq result (append result
                             (list (list (alist-get 'id song)
                                         (alist-get 'name song)
                                         (alist-get 'id artist)
                                         (alist-get 'name artist))))))
      result)))

(defun netease-cloud-music-get-user-playlist (uid)
  "Get the playlists of the user whose user id is UID."
  (let (playlist-json playlist result)
    (request "https://music.163.com/api/user/playlist/"
      :type "POST"
      :data `(("offset" . "0")
              ("uid" . ,uid)
              ("limit" . "1000"))
      :parser 'json-read
      :success (netease-cloud-music-expand-form (setq playlist-json data))
      :sync t)
    (if (/= 200 (alist-get 'code playlist-json))
        (na-error "The uid can not found!")
      (setq playlist-json (alist-get 'playlist playlist-json))
      (dotimes (n (length playlist-json))
        (setq playlist (aref playlist-json n))
        (setq result (append result
                             (list (cons (alist-get 'name playlist)
                                         (alist-get 'id playlist))))))
      result)))

(defun netease-cloud-music-random-play ()
  "Return the random number for the playlist as an index."
  (interactive)
  (setq netease-cloud-music-playlist-song-index
        (random (1- (length netease-cloud-music-playlist))))
  (netease-cloud-music-playlist-play))

(defun netease-cloud-music-delete-playing-song ()
  "Delete playing song."
  (interactive)
  (setq netease-cloud-music-playlist
        (delete (nth
                 (netease-cloud-music--current-song
                  (format "%s - %s"
                          (car netease-cloud-music-current-song)
                          (nth 1 netease-cloud-music-current-song)))
                 netease-cloud-music-playlist)
                netease-cloud-music-playlist))
  (netease-cloud-music-save-playlist)
  (netease-cloud-music-process-sentinel nil "killed"))

(na-defun netease-cloud-music-start-api ()
  "Start third-party API."
  (if (netease-cloud-music-api-process-live-p)
      (na-error "API process is running.")
    (setq netease-cloud-music-api-process
          (start-process "netease-api"
                         netease-cloud-music-api-buffer
                         "node"
                         (expand-file-name "app.js" netease-cloud-music-api-dir))))
  (message "[Netease-Cloud-Music]: API process started."))

(na-defun netease-cloud-music-stop-api (&optional no-error)
  "Stop third-party api.
NO-ERROR means to close error signal."
  (interactive)
  (if (not (netease-cloud-music-api-process-live-p))
      (unless no-error
        (na-error "API process is not exists."))
    (if (not (get-buffer netease-cloud-music-api-buffer))
        (delete-process netease-cloud-music-api-process)
      (delete-process netease-cloud-music-api-buffer)
      (kill-buffer netease-cloud-music-api-buffer))
    (setq netease-cloud-music-api-process nil)
    (message "[Netease-Cloud-Music]: API process stoped.")))

(na-defun netease-cloud-music-restart-api ()
  "Restart the third-party API."
  (interactive)
  (netease-cloud-music-stop-api)
  (netease-cloud-music-start-api))

(na-defun netease-cloud-music-login (phone password)
  "Login with PHONE number and PASSWORD."
  (interactive (list (read-string "Phone number(Countrycode[Space]number): " "+86 ")
                     (md5 (read-passwd "Password: "))))
  (if (not (netease-cloud-music-api-process-live-p))
      (na-error "API process is null!")
    (let ((countrycode (prog2 (string-match "\\(.*\\) \\(.*\\)" phone)
                           (substring (match-string 1 phone) 1)
                         (setq phone (match-string 2 phone))))
          login-result)
      (request (format "http://localhost:%s/login/cellphone?phone=%s&md5_password=%s&countrycode=%s"
                       netease-cloud-music-api-port phone password countrycode)
        :parser 'json-read
        :success (netease-cloud-music-expand-form
                  (setq login-result data))
        :sync t)
      (if (/= 200 (alist-get 'code login-result))
          (na-error "Phone number or password is error!")
        (setq netease-cloud-music-user-id (alist-get 'id (alist-get 'account login-result))
              netease-cloud-music-username (alist-get 'nickname (alist-get 'profile login-result))
              netease-cloud-music-user-password password
              netease-cloud-music-phone (cons countrycode phone))
        (netease-cloud-music-save-loginfo
         (cons netease-cloud-music-phone
               netease-cloud-music-user-password))
        (message "[Netease-Cloud-Music]: Login successfully!")
        (netease-cloud-music-interface-init)))))

(na-defun netease-cloud-music--song-url-by-user (id)
  "Get the song's url by user.
ID is the song's id."
  (let ((song-info (netease-api-request
                    (format "song/url?id=%d" id))))
    (if (null song-info)
        (na-error "The API can't be used, maybe it's starting.")
      (if (/= 200 (alist-get 'code song-info))
          (na-error "The song whose id is %d cannot found!" id)
        (alist-get 'url (aref (alist-get 'data song-info) 0))))))

(na-defun netease-cloud-music--get-user-info ()
  "Get user's info automatically and then login."
  (let ((info (cdr (car (ignore-errors
                          (netease-api-request "login/status"))))))
    (when info
      (if (/= 200 (alist-get 'code info))
          (na-error "Phone number or password is error!")
        (setq netease-cloud-music-username (alist-get 'nickname (alist-get 'profile info))
              netease-cloud-music-user-id (alist-get 'id (alist-get 'account info)))
        (message "[Netease-Cloud-Music]: Login successfully!")
        (netease-cloud-music-interface-init)))))

(defun netease-cloud-music--refersh-playlists ()
  "Refersh the user's playlists."
  (unless netease-cloud-music-use-local-playlist
    (setq netease-cloud-music-playlists
          (netease-cloud-music-get-user-playlist netease-cloud-music-user-id))))

(na-defun netease-cloud-music-create-playlist (name)
  "Create a new playlist named NAME."
  (interactive "sEnter the playlist's name: ")
  (let ((new-playlist (netease-api-request
                       (concat "playlist/create?name=" name))))
    (if (or (null new-playlist) (/= 200 (alist-get 'code new-playlist)))
        (na-error "Failed to create new playlist!")
      (netease-cloud-music--refersh-playlists)
      (message "[Netease-Cloud-Music]: Created the playlist named %s!" name))))

(na-defun netease-cloud-music-delete-playlist (name)
  "Delete the user's playlist which named NAME."
  (interactive (list (completing-read "Enter the playlist you want to delete: "
                                      (netease-cloud-music-delete--list-playlist))))
  (let ((id (progn
              (string-match "^\\(.*\\) - \\(.*\\)" name)
              (match-string 2 name)))
        playlist-name result)
    (if (not (stringp id))
        (na-error "The playlist is not exists!")
      (setq result (netease-api-request (concat "playlist/delete?id=" id)))
      (if (or (null result) (/= 200 (alist-get 'code result)))
          (na-error "Failed to delete the playlist!")
        (netease-cloud-music--refersh-playlists)
        (message "[Netease-Cloud-Music]: Deleted the playlist successfully!")))))

(na-defun netease-cloud-music-delete--list-playlist ()
  "List the playlist then retunr it."
  (let ((user-playlists (netease-cloud-music-get-user-playlist
                         netease-cloud-music-user-id))
        result)
    (if (null user-playlists)
        (na-error "Your playlist is null!")
      (dolist (playlist user-playlists)
        (setq result (append result
                             (list (format "%s - %s"
                                           (car playlist) (cdr playlist))))))
      result)))

(na-defun netease-cloud-music-update-playlist-name (id name)
  "Replace the name of playlist whose id is equal to ID with NAME."
  (let (result)
    (if (null (netease-cloud-music-alist-cdr
               id (netease-cloud-music-get-user-playlist
                   netease-cloud-music-user-id)))
        (na-error "The playlist is not exists!")
      (setq result (netease-api-request
                    (format "playlist/name/update?id=%d&name=%s"
                            id (netease-cloud-music-encode-url name))))
      (if (or (null result) (/= 200 (alist-get 'code result)))
          (na-error "Failed to update the name of the playlist!")
        (netease-cloud-music--refersh-playlists)
        (message "[Netease-Cloud-Music]: Updated playlist's name successfully!")))))

(provide 'netease-cloud-music)

;;; netease-cloud-music.el end here
