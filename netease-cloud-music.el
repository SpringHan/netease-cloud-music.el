;;; netease-cloud-music.el --- Netease Cloud Music client -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 2.0
;; Package-Requires: ((emacs "27.1") (request "0.3.3"))
;; Homepage: https://github.com/SpringHan/netease-cloud-music.git
;; Keywords: multimedia


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

(declare-function netease-cloud-music-interface-init "netease-cloud-music-ui")
(declare-function netease-cloud-music-mode "netease-cloud-music-ui")
(declare-function netease-cloud-music-search-song--open-switch "netease-cloud-music-ui")
(declare-function netease-cloud-music-change-playlist-mode "netease-cloud-music-ui")
(declare-function netease-cloud-music-playlist--open-switch "netease-cloud-music-ui")
(declare-function netease-cloud-music-switch-close "netease-cloud-music-ui")

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
  :type 'process
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

(defcustom netease-cloud-music-api-repo
  "https://github.com/SpringHan/NeteaseCloudMusicApi.git"
  "The git repo for ncm api.
It will be cloned into `netease-cloud-music-api-dir' if `netease-cloud-music-api-type' is npm"
  :type 'string
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-api-docker-image
  "docker.io/binaryify/netease_cloud_music_api:latest"
  "The register for docker image of netease-cloud-music-api."
  :group 'netease-cloud-music
  :type 'string)

(defcustom netease-cloud-music-docker-exec (or (executable-find "docker")
                                               (executable-find "podman"))
  "The docker executable path."
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

(defcustom netease-cloud-music-playlists-songs nil
  "The songs of user's playlist."
  :type 'list
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-playlist-id nil
  "The current playlist id of the user."
  :type 'number
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-playlists nil
  "The playlists of the user."
  :type 'list
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-storage nil
  "Storage songs."
  :type 'list
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-playlist-refresh-timer nil
  "The timer to refresh playlist."
  :type 'timer
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-playlist-song-index 0
  "The song index for the playlist song."
  :type 'number
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-show-lyric t
  "Wether show lyric.
If it's t, show the original lyric.
If it's 'all, show the original lyric and the translated lyric.
If it's nil, don't show the lyric."
  :type 'symbol
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

(defcustom netease-cloud-music-search-type nil
  "The search type.  (For EAF)."
  :type 'symbol
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-login-timer nil
  "The timer for login."
  :type 'timer
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-timer-protect nil
  "Prevent other timer from breaking current timer."
  :type 'boolean
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-use-local-playlist t
  "The type of the playlist now used.
If it's t, meaning to use the local playlist."
  :type 'boolean
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-write-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'netease-cloud-music-write-finish)
    (define-key map (kbd "C-c C-k") 'netease-cloud-music-write-cancel)
    map)
  "The map for write mode."
  :type 'keymap
  :group 'netease-cloud-music)

(defface netease-cloud-music-song-face
  '((t :inherit font-lock-keyword-face))
  "The song face."
  :group 'netease-cloud-music)

(defface netease-cloud-music-artist-face
  '((t :inherit font-lock-function-name-face))
  "The artist face."
  :group 'netease-cloud-music)

(defvar netease-cloud-music-buffer-name "*Netease-Cloud-Music*"
  "The name of Netease Music buffer.")

(defvar netease-cloud-music-lyric-timer nil
  "The timer of Netease Music lyric.")

(defvar netease-cloud-music-update-lyric-timer nil
  "The timer for updating lyrics.")

(defvar netease-cloud-music-lyric nil
  "The Netease Music lyric.")

(defvar netease-cloud-music-tlyric nil
  "Translated lyric.")

(defvar netease-cloud-music-lyrics nil
  "The lyrics that will not be changed when playing.")

(defvar netease-cloud-music-current-lyric nil
  "Current lyric.")

(defvar netease-cloud-music-translated-lyric nil
  "Translated lyric.")

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

(defvar netease-cloud-music-username nil
  "Username.")

(defvar netease-cloud-music-user-id nil
  "User ID.")

(defvar eaf-netease-cloud-music-user+list)

;;;###autoload
(define-derived-mode netease-cloud-music-write-mode text-mode "Netease-Cloud-Music:Write"
  "The write mode for `netease-cloud-music'."
  :group 'netease-cloud-music
  :syntax-table nil
  :abbrev-table nil
  (if (get-buffer netease-cloud-music-buffer-name)
      (progn
        (netease-cloud-music-interface-init)
        (setq buffer-read-only nil)
        (if netease-cloud-music-use-local-playlist
            (progn
              (goto-char (point-max))
              (search-backward "Local Playlist:")
              (forward-line)
              (narrow-to-region (point) (point-max)))

          (goto-char (point-min))             ;Get the position of the playlist
          (search-forward
           (car (netease-cloud-music-alist-cdr
                 netease-cloud-music-playlist-id netease-cloud-music-playlists))
           nil t)
          (forward-line)
          (let ((start (line-beginning-position)))
            (while (eq (get-text-property (point) 'face) 'font-lock-keyword-face)
              (forward-line))
            (forward-line -1)
            (narrow-to-region start (line-end-position))
            (goto-char (point-min))))
        (when netease-cloud-music-show-lyric
          (with-current-buffer netease-cloud-music-buffer-name
            (netease-cloud-music-add-header-lyrics))))
    (netease-cloud-music-for-eaf
     (with-current-buffer "eaf-netease-cloud-music-write"
       (let ((playlist-songs (if netease-cloud-music-use-local-playlist
                                 netease-cloud-music-playlist
                               netease-cloud-music-playlists-songs)))
         (mapc (lambda (s)
                 (insert (nth 1 s) " - " (nth 3 s) "\n"))
               playlist-songs)
         (goto-char (point-min)))))))

(defun netease-cloud-music--deleted-item (list)
  "Check the delete item in LIST.  For playlist."
  (let (result)
    (dolist (song netease-cloud-music-playlists-songs)
      (unless (memq (car song) list)
        (setq result (append result (list (car song))))))
    result))

(defun netease-cloud-music-write-cancel ()
  "Cancel and turn to normal mode."
  (interactive)
  (if (get-buffer netease-cloud-music-buffer-name)
      (progn
        (netease-cloud-music-mode)
        (netease-cloud-music-interface-init)
        (when netease-cloud-music-show-lyric
          (with-current-buffer netease-cloud-music-buffer-name
            (netease-cloud-music-add-header-lyrics))))
    (netease-cloud-music-for-eaf
     (kill-buffer "eaf-netease-cloud-music-write")
     (switch-to-buffer "eaf-netease-cloud-music"))))

(defun netease-cloud-music-write-finish ()
  "Modify by the modified contents and turn to normal mode."
  (interactive)
  (let (current-song copy deleted)
    (with-current-buffer (if (get-buffer netease-cloud-music-buffer-name)
                             netease-cloud-music-buffer-name
                           "eaf-netease-cloud-music-write")
      (goto-char (point-min))
      (while (not (eobp))
        (setq current-song (netease-cloud-music--current-song))
        (when current-song
          (setq current-song (nth current-song
                                  (if netease-cloud-music-use-local-playlist
                                      netease-cloud-music-playlist
                                    netease-cloud-music-playlists-songs)))
          (setq copy
                (append copy (list (if netease-cloud-music-use-local-playlist
                                       current-song
                                     (car current-song))))))
        (forward-line))
      (if (not (eobp))
          (progn
            (netease-cloud-music-write-cancel)
            (netease-cloud-music-error "There's an error when save the result!"))
        (if netease-cloud-music-use-local-playlist
            (progn
              (setq netease-cloud-music-playlist copy)
              (netease-cloud-music-save-playlist))
          (netease-cloud-music-update-songs-order-in-playlist
           netease-cloud-music-playlist-id copy)

          (when (setq deleted (netease-cloud-music--deleted-item copy))
            (netease-cloud-music--track nil netease-cloud-music-playlist-id deleted)
            (setq netease-cloud-music-timer-protect 'track)))

        (netease-cloud-music-write-cancel)
        (netease-cloud-music-adjust-song-index)))))

(defun netease-cloud-music-download-api ()
  "Download the third-party API."
  (interactive)
  (if (netease-cloud-music--api-downloaded)
      (netease-cloud-music-error "The third-party API has been downloaded!")
    (async-shell-command
     (format "git clone %s %s --depth=1 && cd %s && npm install"
             netease-cloud-music-api-repo
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

(defun netease-cloud-music-tui-init ()
  "Init the TUI, if it's exists."
  (when (and netease-cloud-music-buffer-name
             (get-buffer netease-cloud-music-buffer-name))
    (netease-cloud-music-interface-init)))

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
      (netease-cloud-music-cancel-timer)))
  (setq netease-cloud-music-process nil
        netease-cloud-music-current-song nil)
  (netease-cloud-music-for-eaf
   :eaf-buffer
   (netease-cloud-music-call-js "set_panel_song_info" (json-encode (list "" "")))
   (eaf--netease-cloud-music-change-play-status)
   (netease-cloud-music-call-js "reset_song_style")))

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
  (when netease-cloud-music-playlist-refresh-timer
    (cancel-timer netease-cloud-music-playlist-refresh-timer)
    (setq netease-cloud-music-playlist-refresh-timer nil))
  (netease-cloud-music-cancel-timer t)
  (netease-cloud-music-save-playlist)
  (when (not (eq netease-cloud-music-api-type 'remote))
    (netease-cloud-music-stop-api t))
  (when (get-buffer " *Request*")
    (kill-buffer " *Request*"))
  (netease-cloud-music-for-eaf
   (kill-buffer "eaf-netease-cloud-music")))

(defun netease-cloud-music-back ()
  "Back to the `netease-cloud-music-last-buffer'."
  (interactive)
  (if (or (null netease-cloud-music-last-buffer)
          (null (buffer-name (get-buffer netease-cloud-music-last-buffer))))
      (previous-buffer)
    (switch-to-buffer netease-cloud-music-last-buffer)
    (setq netease-cloud-music-last-buffer nil)))

(defun netease-cloud-music-search-song (&optional song-name)
  "Search SONG-NAME from Netease Music and return the song id.
SONG-NAME is a string."
  (interactive)
  (unless song-name
    (setq song-name (read-string "Enter the song name: ")))
  (let* ((artist-name (read-string "Enter the artist name(can be null): "))
         (search-content (format "%s %s" song-name artist-name))
         (search-result
          (netease-cloud-music-get-song
           (netease-cloud-music-request-from-api search-content nil
                                                 netease-cloud-music-search-limit))))
    (setq netease-cloud-music-search-alist
          (cons search-content search-result)
          netease-cloud-music-search-page 1)
    (if (get-buffer netease-cloud-music-buffer-name)
        (netease-cloud-music-search-song--open-switch search-result)
      (setq netease-cloud-music-search-type 'song)
      (netease-cloud-music-for-eaf
       :eaf-buffer
       (netease-cloud-music-call-js "reset_song_style")
       (netease-cloud-music-call-js "set_playlist" (json-encode-array
                                                    search-result))
       (netease-cloud-music-call-js "change_playlist_mode" (json-encode t))))))

(defun netease-cloud-music-change-repeat-mode (&optional repeat-mode)
  "Change the repeat mode.
When REPEAT-MODE is non-nil, set current repeat mode to it."
  (interactive)
  (if (and (string= netease-cloud-music-repeat-mode "")
           (null repeat-mode))
      (netease-cloud-music-error
       "The repeat mode is in the initialization state.  when you start playing song, it'll be set!")
    (setq netease-cloud-music-repeat-mode
          (if repeat-mode
              repeat-mode
            (pcase netease-cloud-music-repeat-mode
              ("off" "song")
              ("song" "playlist")
              ("playlist" "random")
              ("random" "off"))))
    (netease-cloud-music-tui-init)
    (netease-cloud-music-for-eaf
     :eaf-buffer
     (netease-cloud-music-call-js "set_repeat_mode" (json-encode netease-cloud-music-repeat-mode)))))

(defun netease-cloud-music-get-lyric (song-id)
  "Get the lyrics of the song whose id is SONG-ID."
  (let (result lyric tlyric)
    (request
      ;;netease-cloud-music-lyric-api
      (format "http://music.163.com/api/song/lyric?id=%s&lv=1&kv=1&tv=-1" song-id)
      :type "GET"
      :parser 'json-read
      :success (netease-cloud-music-expand-form
                (setq result data))
      :sync t)
    (if (or (null result) (/= 200 (alist-get 'code result)))
        (netease-cloud-music-error "Failed to get the lyric!")
      (setq lyric (alist-get 'lyric (alist-get 'lrc result)))
      (when (eq netease-cloud-music-show-lyric 'all)
        (setq tlyric (alist-get 'lyric (alist-get 'tlyric result))))
      (setq result (if (and tlyric
                            (not (string= tlyric "")))
                       (cons lyric tlyric)
                     lyric)))))

(defun netease-cloud-music-change-lyric-type (&optional type)
  "Change the lyric's TYPE."
  (interactive)
  (pcase (if type
             type
           netease-cloud-music-show-lyric)
    ('nil (setq netease-cloud-music-show-lyric t)
          (netease-cloud-music-lyric-init (nth 2 netease-cloud-music-current-song))
          (netease-cloud-music-start-lyric)
          (netease-cloud-music-update-lyrics))
    ('t (setq netease-cloud-music-show-lyric 'all)
        (setq netease-cloud-music-lyrics
              (netease-cloud-music-get-lyric
               (nth 2 netease-cloud-music-current-song))
              netease-cloud-music-tlyric
              (split-string (cdr netease-cloud-music-lyrics) "\n"))
        (netease-cloud-music-update-lyrics))
    ('all (setq netease-cloud-music-show-lyric nil)
          (setq netease-cloud-music-lyric nil
                netease-cloud-music-lyrics nil
                netease-cloud-music-tlyric nil
                netease-cloud-music-lyric-song nil)
          (netease-cloud-music-cancel-timer t))))

(defun netease-cloud-music-lyric-init (song-id)
  "Initialize the lyric.
SONG-ID is the song's id for current lyric."
  (if netease-cloud-music-show-lyric
      (progn
        (setq netease-cloud-music-lyric (netease-cloud-music-get-lyric song-id))
        (with-current-buffer " *netease-cloud-music-play:process*"
          (erase-buffer))
        (when (and netease-cloud-music-buffer-name
                   (get-buffer netease-cloud-music-buffer-name))
          (with-current-buffer netease-cloud-music-buffer-name
            (netease-cloud-music-add-header-lyrics)))
        (setq netease-cloud-music-current-lyric nil
              netease-cloud-music-translated-lyric nil
              netease-cloud-music-tlyric nil))
    (setq netease-cloud-music-lyric nil)))

(defun netease-cloud-music-start-lyric ()
  "Start the lyric."
  (when (and netease-cloud-music-lyric netease-cloud-music-show-lyric)
    
    (setq netease-cloud-music-lyrics netease-cloud-music-lyric
          netease-cloud-music-lyric
          (if (consp netease-cloud-music-lyrics)
              (progn
                (setq netease-cloud-music-tlyric
                      (split-string (cdr netease-cloud-music-lyrics)
                                    "\n"))
                (split-string (car netease-cloud-music-lyrics) "\n"))
            (split-string netease-cloud-music-lyrics "\n"))
          netease-cloud-music-lyric-song (nth 2 netease-cloud-music-current-song)
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
                         (setq current-lyric (netease-cloud-music--current-lyric current-lyric))
                         (unless (string= current-lyric "")
                           (setq netease-cloud-music-current-lyric current-lyric))
                         (setq netease-cloud-music-lyric (cdr netease-cloud-music-lyric)))

                       (when (and (eq netease-cloud-music-show-lyric ;To sync translated lyric
                                      'all)
                                  (listp netease-cloud-music-tlyric)
                                  (stringp current-lyric-time)
                                  (not
                                   (netease-cloud-music--string>
                                    (ignore-errors
                                      (substring (car netease-cloud-music-tlyric)
                                                 1 6))
                                    current-song-time)))
                         (setq netease-cloud-music-translated-lyric (netease-cloud-music--current-lyric
                                                                     (car netease-cloud-music-tlyric)))
                         (setq netease-cloud-music-tlyric (cdr netease-cloud-music-tlyric))))))
               (netease-cloud-music-cancel-timer)))))

    (run-with-idle-timer
     1.5 nil
     (lambda ()
       (let ((origin-lyrics (split-string (if (listp netease-cloud-music-lyrics)
                                              (car netease-cloud-music-lyrics)
                                            netease-cloud-music-lyrics)
                                          "\n" t))
             min-time last-time current-time minus-time)
         (when (> (length origin-lyrics) 4)
           (dotimes (i 4)           ;Delete the lyrics about copyright
             (setq origin-lyrics (delete (nth i origin-lyrics)
                                         origin-lyrics))))

         (dolist (lyric origin-lyrics)
           (setq current-time (netease-cloud-music--get-lyric-time
                               lyric))
           (when last-time
             (setq minus-time (netease-cloud-music--format-lyric-time
                               (- current-time last-time)))
             (when (> minus-time 0)
               (when (or (and min-time
                              (< minus-time min-time))
                         (null min-time))
                 (setq min-time minus-time))))
           (setq last-time current-time))

         (when (and (numberp min-time)
                    (timerp netease-cloud-music-lyric-timer)
                    (< min-time 1))
           (timer-set-time netease-cloud-music-lyric-timer
                           0 min-time)))))))

(defun netease-cloud-music-play (song-id song-name artist-name)
  "Play the song by its SONG-ID and update the interface with SONG-NAME & ARTIST-NAME."
  (catch 'stop
    (if (null song-id)
        (netease-cloud-music-error "There's no song-id!")
      (when (stringp song-id)
        (setq song-id (string-to-number song-id)))
      (netease-cloud-music-kill-process)
      (let (tmp)
        (setq netease-cloud-music-process
              (start-process "netease-cloud-music-play:process"
                             " *netease-cloud-music-play:process*"
                             (car netease-cloud-music-player-command)
                             (if netease-cloud-music-user-id
                                 (if (setq tmp
                                           (netease-cloud-music--song-url-by-user
                                            song-id))
                                     tmp
                                   (message
                                    "[Netease-Cloud-Music]: Cannot get the song's info %s: %s, now play the next."
                                    song-id song-name)
                                   (if (string= netease-cloud-music-repeat-mode "random")
                                       (netease-cloud-music-random-play)
                                     (netease-cloud-music-play-next-song
                                      (netease-cloud-music-adjust-song-index
                                       (list song-name artist-name song-id))))
                                   (throw 'stop nil))
                               (concat netease-cloud-music-song-link
                                       (number-to-string song-id)))
                             (if (string=
                                  (car netease-cloud-music-player-command)
                                  "mpv")
                                 "--input-ipc-server=/tmp/mpvserver"
                               ""))))
      (set-process-sentinel netease-cloud-music-process
                            'netease-cloud-music-process-sentinel)
      (netease-cloud-music-lyric-init song-id)
      (setq netease-cloud-music-process-status "playing")
      (setq netease-cloud-music-current-song
            `(,song-name ,artist-name ,song-id))
      (when (string= netease-cloud-music-repeat-mode "")
        (netease-cloud-music-change-repeat-mode "song"))
      (netease-cloud-music-start-lyric)
      (netease-cloud-music-tui-init)
      (netease-cloud-music-adjust-song-index)
      (netease-cloud-music-for-eaf
       :eaf-buffer
       (netease-cloud-music-call-js "set_panel_song_info" (json-encode (list song-name
                                                                             artist-name)))
       (eaf--netease-cloud-music-change-play-status)
       (eaf--netease-cloud-music--update-song-style)))))

(defun netease-cloud-music-playlist-play ()
  "Play the playlist songs."
  (interactive)
  (when (string= netease-cloud-music-repeat-mode "")
    (netease-cloud-music-change-repeat-mode "playlist"))
  (let (current-song song)
    (when (and (ignore-errors
                 (eq (key-binding (read-kbd-macro (char-to-string last-input-event)))
                     'netease-cloud-music-playlist-play))
               (string= (buffer-name (current-buffer))
                        netease-cloud-music-buffer-name))
      (setq current-song (netease-cloud-music--current-song))
      (netease-cloud-music-change-playlist-mode))
    (when current-song
      (setq netease-cloud-music-playlist-song-index current-song))
    (when (> netease-cloud-music-playlist-song-index
             (length (if netease-cloud-music-use-local-playlist
                         netease-cloud-music-playlist
                       netease-cloud-music-playlists-songs)))
      (setq netease-cloud-music-playlist-song-index 0))
    (setq song
          (nth netease-cloud-music-playlist-song-index
               (if netease-cloud-music-use-local-playlist
                   netease-cloud-music-playlist
                 netease-cloud-music-playlists-songs)))
    (netease-cloud-music-play (car song)
                              (nth 1 song)
                              (nth 3 song))))

(defun netease-cloud-music-process-sentinel (_ event)
  "The sentinel of Netease Music process.
EVENT is the event trigger sentinel."
  (when (string-match "\\(finished\\|Exiting\\|killed\\|exited\\)" event)
    (pcase netease-cloud-music-repeat-mode
      ("off" (netease-cloud-music-kill-current-song t))
      ("song"
       (setq netease-cloud-music-process-status "")
       (netease-cloud-music-play
        (nth 2 netease-cloud-music-current-song)
        (car netease-cloud-music-current-song)
        (nth 1 netease-cloud-music-current-song))
       (netease-cloud-music-tui-init))
      ("playlist"
       (netease-cloud-music-play-next-song))
      ("random"
       (netease-cloud-music-random-play)))))

(defun netease-cloud-music-play-previous-song ()
  "Play the previous song in the playlist."
  (interactive)
  (if (string= netease-cloud-music-process-status "")
      (forward-line -1)
    (let ((previous-song-index
           (- netease-cloud-music-playlist-song-index 1)))
      (if (or (null (nth previous-song-index
                         (if netease-cloud-music-use-local-playlist
                             netease-cloud-music-playlist
                           netease-cloud-music-playlists-songs)))
              (< previous-song-index 0))
          (if (string= netease-cloud-music-repeat-mode "off")
              (netease-cloud-music-error "There's no song previous!")
            (setq netease-cloud-music-playlist-song-index
                  (- (length (if netease-cloud-music-use-local-playlist
                                 netease-cloud-music-playlist
                               netease-cloud-music-playlists-songs))
                     1)))
        (setq netease-cloud-music-playlist-song-index previous-song-index))
      (netease-cloud-music-playlist-play))))

(defun netease-cloud-music-play-next-song (&optional force)
  "Play the next song in the playlist.
If FORCE is non-nil, forcly play the next song.
And it's must be a song's index."
  (interactive)
  (if (and (string= netease-cloud-music-process-status "")
           (null force))
      (forward-line)
    (let ((next-song-index
           (+ (if force
                  force
                netease-cloud-music-playlist-song-index)
              1)))
      (if (null (nth next-song-index (if netease-cloud-music-use-local-playlist
                                         netease-cloud-music-playlist
                                       netease-cloud-music-playlists-songs)))
          (if (string= netease-cloud-music-repeat-mode "off")
              (progn
                (netease-cloud-music-kill-current-song)
                (netease-cloud-music-error "The playlist songs over!"))
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
    (netease-cloud-music-tui-init)
    (netease-cloud-music-for-eaf
     :eaf-buffer
     (eaf--netease-cloud-music-change-play-status))))

(defun netease-cloud-music-kill-current-song (&optional force)
  "Kill the current song.
FORCE means to forcely kill it."
  (interactive)
  (when (or (netease-cloud-music-process-live-p)
            force)
    (setq netease-cloud-music-process-status "")
    (netease-cloud-music-kill-process)
    (netease-cloud-music-tui-init)))

(defun netease-cloud-music-seek-forward ()
  "Seek forward the current song."
  (interactive)
  (when (netease-cloud-music-process-live-p)
    (if (string= "mpv" (car netease-cloud-music-player-command))
        (progn
          ;; (shell-command (format
          ;;                 "echo '%s' | socat - %smpvserver"
          ;;                 (nth 2 netease-cloud-music-player-command)
          ;;                 (netease-cloud-music--format-process-file))
          ;;                "*shell-output*" nil)
          (shell-command (format
                          "echo '%s' | socat - /tmp/mpvserver"
                          (nth 2 netease-cloud-music-player-command))
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
          ;; (shell-command (format
          ;;                 "echo '%s' | socat%s - %smpvserver"
          ;;                 (nth 3 netease-cloud-music-player-command)
          ;;                 (netease-cloud-music--format-process-file))
          ;;                "*shell-output*" nil)
          (shell-command (format
                          "echo '%s' | socat - /tmp/mpvserver"
                          (nth 3 netease-cloud-music-player-command))
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

(defun netease-cloud-music-clear-playlist ()
  "Clear the playlist and kill current song."
  (interactive)
  (when (yes-or-no-p "Do you really want to clear playlist?")
    (netease-cloud-music-kill-current-song)
    (if netease-cloud-music-use-local-playlist
        (progn
          (setq netease-cloud-music-playlist nil)
          (if (get-buffer netease-cloud-music-buffer-name)
              (netease-cloud-music-interface-init)
            (netease-cloud-music-for-eaf
             :eaf-buffer
             (netease-cloud-music-call-js "set_playlist" (json-encode-array
                                                          nil)))))
      (let (ids)
        (dolist (song netease-cloud-music-playlists-songs)
          (setq ids (append ids (list (car song)))))
        (netease-cloud-music--track
         nil netease-cloud-music-playlist-id ids)))))

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
    (mapc (lambda (b)
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
  "Cancel timer and kill the lyrics.
DELETE-LYRICS means delete all the lyric headers."
  (when (timerp netease-cloud-music-lyric-timer)
    (cancel-timer netease-cloud-music-lyric-timer))
  (setq netease-cloud-music-lyric-timer nil)
  (setq netease-cloud-music-current-lyric nil)
  (when delete-lyrics
    (netease-cloud-music-delete-header-lyrics t)))

(defun netease-cloud-music-show-lyrics ()
  "Show lyrics."
  (ignore-errors
    (concat (propertize (car netease-cloud-music-current-song)
                        'face 'netease-cloud-music-song-face)
            " - "
            (propertize (nth 1 netease-cloud-music-current-song)
                        'face 'netease-cloud-music-artist-face)
            ": "
            (when (stringp netease-cloud-music-current-lyric)
              netease-cloud-music-current-lyric)
            (when (stringp netease-cloud-music-translated-lyric)
              (concat
               "   "
               (propertize netease-cloud-music-translated-lyric
                           'face '(:height 0.9)))))))

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
Optional argument INIT means init the lyrics list."
  (when netease-cloud-music-show-lyric
    (when init
      (setq netease-cloud-music-lyric
            (if (eq netease-cloud-music-show-lyric 'all)
                (progn
                  (setq netease-cloud-music-tlyric
                        (split-string
                         (cdr netease-cloud-music-lyrics) "\n"))
                  (split-string
                   (car netease-cloud-music-lyrics) "\n"))
              (split-string netease-cloud-music-lyrics "\n"))))
    (with-current-buffer " *netease-cloud-music-play:process*"
      (when (search-backward "[K[0mA" nil t)
        (let ((current-song-time (buffer-substring-no-properties (+ 12 (point))
                                                                 (+ 17 (point))))
              (current-time (ignore-errors
                              (substring (car netease-cloud-music-lyric) 1 6)))
              current-lyric
              current-tlyric)
          (while (not
                  (netease-cloud-music--string> current-time current-song-time))
            (setq current-time
                  (ignore-errors
                    (substring (car netease-cloud-music-lyric) 1 6)))
            (setq current-lyric (pop netease-cloud-music-lyric))
            (when (eq netease-cloud-music-show-lyric 'all)
              (setq current-tlyric (pop netease-cloud-music-tlyric))))

          (setq netease-cloud-music-current-lyric
                (netease-cloud-music--current-lyric current-lyric))
          (when (eq netease-cloud-music-show-lyric 'all)
            (netease-cloud-music--current-lyric
             current-tlyric)))))))

(defun netease-cloud-music-move-down (&optional index)
  "Move the current song down.
INDEX is the songs index."
  (interactive)
  (let ((current (if index
                     index
                   (netease-cloud-music--current-song)))
        (current-line (when (get-buffer netease-cloud-music-buffer-name)
                        (line-number-at-pos))))
    (when current
      (netease-cloud-music-change-order current (+ current 2))
      (when (get-buffer netease-cloud-music-buffer-name)
        (goto-char (point-min))
        (forward-line current-line)))))

(defun netease-cloud-music-move-up (&optional index)
  "Move the current song up.
INDEX is the song's index."
  (interactive)
  (let ((current (if index
                     index
                   (netease-cloud-music--current-song)))
        (current-line (when (get-buffer netease-cloud-music-buffer-name)
                        (line-number-at-pos))))
    (when current
      (netease-cloud-music-change-order current current)
      (when (get-buffer netease-cloud-music-buffer-name)
        (goto-char (point-min))
        (forward-line (- current-line 2))))))

(defun netease-cloud-music-change-order (song num)
  "Change the SONG's order to NUM."
  (interactive (let ((current-song (netease-cloud-music--current-song)))
                 (list current-song
                       (read-number (format "Enter the number for the order(current one is %S): "
                                            (1+ current-song))))))
  (if (null song)
      (netease-cloud-music-error "The song is not exists!")
    (setq num (1- num))
    (let* ((playlist (if netease-cloud-music-use-local-playlist
                         'netease-cloud-music-playlist
                       'netease-cloud-music-playlists-songs))
           (playlist-value (if netease-cloud-music-use-local-playlist
                               netease-cloud-music-playlist
                             netease-cloud-music-playlists-songs))
           (original-song (nth num playlist-value))
           (song-info (nth song playlist-value))
           (backup (unless netease-cloud-music-use-local-playlist
                     (format "%S" netease-cloud-music-playlists-songs)))
           tmp)

      (cond ((> num (1- (length playlist-value)))
             (eval `(setq ,playlist
                          (append (list ',song-info)
                                  (delete ',song-info ,playlist)))))
            ((< num 0)
             (eval `(setq ,playlist
                          (append (delete ',song-info ,playlist)
                                  (list ',song-info)))))
            (t
             (eval `(setf (nth ,num ,playlist) ',song-info
                          (nth ,song ,playlist) ',original-song))))

      (if netease-cloud-music-use-local-playlist
          (progn
            (netease-cloud-music-save-playlist)
            (netease-cloud-music-adjust-song-index))
        (dolist (song netease-cloud-music-playlists-songs)
          (setq tmp (append tmp (list (car song)))))
        (setq netease-cloud-music-playlists-songs
              (car (read-from-string backup)))
        (netease-cloud-music-update-songs-order-in-playlist
         netease-cloud-music-playlist-id tmp))
      (netease-cloud-music-tui-init))))

(defun netease-cloud-music-adjust-song-index (&optional get)
  "Adjust the current song's index.
If GET is non-nil, it just return the index.
The format of GET is same as `netease-cloud-music-current-song'."
  (let* ((song-info (cond (get get)
                          (netease-cloud-music-current-song
                           netease-cloud-music-current-song)
                          (t nil)))
         (index (when song-info
                  (netease-cloud-music--current-song song-info))))
    (if (and index
             (null get))
        (progn
          (setq netease-cloud-music-playlist-song-index index)
          (netease-cloud-music-for-eaf
           :eaf-buffer
           (netease-cloud-music-call-js "change_song_style" netease-cloud-music-playlist-song-index)))
      index)))

(defun netease-cloud-music-ask-play (song)
  "Play the SONG by asking."
  (interactive
   (list
    (let (song-list)
      (dolist (song (if netease-cloud-music-use-local-playlist
                        netease-cloud-music-playlist
                      netease-cloud-music-playlists-songs))
        (setq song-list (append song-list
                                (list (format "%s - %s"
                                              (nth 1 song)
                                              (nth 3 song))))))
      (completing-read "Enter the song: " song-list))))
  (let ((song-info (netease-cloud-music--current-song song)))
    (when song-info
      (setq song-info (nth song-info (if netease-cloud-music-use-local-playlist
                                         netease-cloud-music-playlist
                                       netease-cloud-music-playlists-songs)))
      (netease-cloud-music-play (car song-info)
                                (nth 1 song-info)
                                (nth 3 song-info)))))

(defun netease-cloud-music-save-playlist (&optional hint)
  "Save playlist to the cache file.
If HINT is not non-nil, show the hint message."
  (interactive (list t))
  (unless (file-exists-p netease-cloud-music-playlist-file)
    (make-empty-file netease-cloud-music-playlist-file))
  (with-temp-file netease-cloud-music-playlist-file
    (erase-buffer)
    (insert (format "%S" netease-cloud-music-playlist)))
  (when hint
    (message "[Netease-Cloud-Music]: The playlist has been saved into the cache file."))
  (netease-cloud-music-for-eaf
   :eaf-buffer
   (when netease-cloud-music-use-local-playlist
     (netease-cloud-music-call-js "set_playlist" (json-encode-array
                                                  netease-cloud-music-playlist)))))

(defun netease-cloud-music-get-playlist ()
  "Set the playlist to the one which saved in the cache file."
  (when (file-exists-p netease-cloud-music-playlist-file)
    (let ((playlist (with-temp-buffer
                      (insert-file-contents netease-cloud-music-playlist-file)
                      (buffer-substring-no-properties (point-min) (point-max)))))
      (setq netease-cloud-music-playlist (car (read-from-string playlist)))
      (netease-cloud-music-for-eaf
       :eaf-buffer
       (when netease-cloud-music-use-local-playlist
         (netease-cloud-music-call-js "set_playlist" (json-encode-array
                                                      netease-cloud-music-playlist)))))))

(defun netease-cloud-music-search-playlist (&optional name)
  "Fetch the playlist by NAME."
  (interactive)
  (unless name
    (setq name (read-string "Enter the name of the playlist: ")))
  (if (string= name "")
      (netease-cloud-music-error "The name of playlist can not be null!")
    (let ((playlists
           (netease-cloud-music-get-playlists
            (netease-cloud-music-request-from-api
             name 'playlist netease-cloud-music-search-limit))))
      (setq netease-cloud-music-search-playlists
            (cons name playlists)
            netease-cloud-music-search-page 1)
      (if (get-buffer netease-cloud-music-buffer-name)
          (netease-cloud-music-playlist--open-switch playlists)
        (setq netease-cloud-music-search-type 'playlist)
        (netease-cloud-music-for-eaf
         :eaf-buffer
         (netease-cloud-music-call-js "reset_song_style")
         (netease-cloud-music-call-js "set_playlist" (json-encode-array
                                                      (netease-cloud-music--playlist-search-format
                                                       playlists)))
         (netease-cloud-music-call-js "change_playlist_mode" (json-encode t)))))))

(defun netease-cloud-music--playlist-search-format (playlists)
  "Format the PLAYLISTS from search to EAF."
  (let (list)
    (mapc (lambda (p)
            (setq list
                  (append list
                          (list
                           (list 0 (car p) 0 "")))))
          playlists)
    list))

(defun netease-cloud-music-get-playlist-by-uid (uid)
  "Fetch playlist by UID."
  (interactive (list (read-string "Enter the uid: "
                                  (when netease-cloud-music-user-id
                                    (number-to-string netease-cloud-music-user-id)))))
  (let ((playlists (netease-cloud-music-get-user-playlist uid)))
    (netease-cloud-music-playlist--open-switch playlists)
    (setq netease-cloud-music-search-playlists playlists)))

(defun netease-cloud-music-get-playlist-songs (pid)
  "Get the songs in the playlist whose if is PID."
  (let (songs song artist result ids)
    (if (and netease-cloud-music-playlists
             (netease-cloud-music-alist-cdr pid netease-cloud-music-playlists))
        (setq songs (netease-cloud-music-api-request (format "playlist/detail?id=%d" pid)))
      (request (format "https://music.163.com/api/v6/playlist/detail?id=%d"
                       pid)
        :parser 'json-read
        :success (netease-cloud-music-expand-form (setq songs data))
        :sync t))
    (if (or (null songs) (/= 200 (alist-get 'code songs)))
        (netease-cloud-music-error "The pid can not fount!")
      (if (netease-cloud-music-api-process-live-p)
          (progn
            (setq songs (alist-get 'trackIds (alist-get 'playlist songs)))
            (dotimes (n (length songs))
              (setq ids (append ids
                                (list (alist-get 'id (aref songs n))))))
            (setq result (netease-cloud-music-get-song-detail
                          ids)))
        (setq songs (alist-get 'tracks (alist-get 'playlist songs)))
        (dotimes (n (length songs))
          (setq song (aref songs n)
                artist (aref (alist-get 'ar song) 0))
          (setq result (append result
                               (list (list (alist-get 'id song)
                                           (alist-get 'name song)
                                           (alist-get 'id artist)
                                           (alist-get 'name artist)))))))
      result)))

(defun netease-cloud-music--list-to-batch-list (input size)
  "Split INPUT list into a batches (i.e. sublists) of maximum SIZE."
  (when (< size 1)
    (netease-cloud-music-error "SIZE of the batches must be at least 1"))
  (unless (seqp input)
    (netease-cloud-music-error "INPUT must be a sequence or list"))
  (cl-loop with tail = input
           while tail
           collect (cl-loop for ptr on tail
                            for i upfrom 0
                            while (< i size)
                            collect (car ptr)
                            finally (setf tail ptr))))

(defun netease-cloud-music-get-song-detail (ids)
  "Get the songs' info by their IDS."
  (when ids
    (let ((batch-ids (netease-cloud-music--list-to-batch-list ids 500))
          result)
      (dotimes (n (length batch-ids))
        (setq ids (nth n batch-ids))
        (let ((song-info (netease-cloud-music-api-request
                          (format "song/detail?ids=%s"
                                  (netease-cloud-music--list-to-splited-string
                                   ids))))
              songs song artist)
          (if (or (null song-info)
                  (/= 200 (alist-get 'code song-info)))
              (netease-cloud-music-error "To get songs info failed!")
            (setq songs (alist-get 'songs song-info))
            (dotimes (n (length songs))
              (setq song (aref songs n)
                    artist (aref (alist-get 'ar song) 0))
              (setq result (append result
                                   (list (list (alist-get 'id song)
                                               (alist-get 'name song)
                                               (alist-get 'id artist)
                                               (alist-get 'name artist)))))))))
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
        (netease-cloud-music-error "The uid can not found!")
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
        (random (1- (length (if netease-cloud-music-use-local-playlist
                                netease-cloud-music-playlist
                              netease-cloud-music-playlists-songs)))))
  (netease-cloud-music-playlist-play))

(defun netease-cloud-music--start-api-npm ()
  "Start API by using npm."
  (start-process "netease-api"
                 netease-cloud-music-api-buffer
                 "node"
                 (expand-file-name "app.js" netease-cloud-music-api-dir)))

(defun netease-cloud-music--start-api-docker ()
  "Start API by using docker."
  (let ((container-name "netease_cloud_music_api")
        (containers
         (with-temp-buffer
           (call-process netease-cloud-music-docker-exec
                         nil t nil "container" "ls" "--all")
           (buffer-string))))
    (if (string-match container-name containers)
        (start-process "netease-api" netease-cloud-music-api-buffer
                       netease-cloud-music-docker-exec
                       "start" "-a" container-name)
      (start-process "netease-api"
                     netease-cloud-music-api-buffer
                     netease-cloud-music-docker-exec
                     "run" "--name" container-name
                     "-p" (format "%s:3000" netease-cloud-music-api-port)
                     netease-cloud-music-api-docker-image))))

(defun netease-cloud-music--stop-api-npm ()
  "Stop API launched by npm."
  (if (not (get-buffer netease-cloud-music-api-buffer))
      (delete-process netease-cloud-music-api-process)
    (delete-process netease-cloud-music-api-buffer)))

(defun netease-cloud-music--stop-api-docker ()
  "Stop API launched by docker."
  (if (not (get-buffer netease-cloud-music-api-buffer))
      (delete-process netease-cloud-music-api-process)
    (delete-process netease-cloud-music-api-buffer))
  (start-process "netease-api-stop"
                 netease-cloud-music-api-buffer
                 netease-cloud-music-docker-exec
                 "stop" "netease_cloud_music_api"))

(netease-cloud-music-api-defun netease-cloud-music-start-api ()
  "Start third-party API."
  (if (netease-cloud-music-api-process-live-p)
      (netease-cloud-music-error "API process is running!")
    (setq netease-cloud-music-api-process
          (when netease-cloud-music-api-type
            (netease-cloud-music--api-func start))))
  (message "[Netease-Cloud-Music]: API process started."))

(netease-cloud-music-api-defun netease-cloud-music-stop-api (&optional no-error)
  "Stop third-party api.
NO-ERROR means to close error signal."
  (interactive)
  (if (not (netease-cloud-music-api-process-live-p))
      (unless no-error
        (netease-cloud-music-error "API process is not exists!"))
    (netease-cloud-music--api-func stop)
    (kill-buffer netease-cloud-music-api-buffer))
  (setq netease-cloud-music-api-process nil)
  (message "[Netease-Cloud-Music]: API process stoped."))

(netease-cloud-music-api-defun netease-cloud-music-restart-api ()
  "Restart the third-party API."
  (interactive)
  (if (eq netease-cloud-music-api-type 'remote)
      (message "[Netease-Cloud-Music]: We are using remote API.")
    (netease-cloud-music-stop-api)
    (netease-cloud-music-start-api)))

(netease-cloud-music-api-defun netease-cloud-music-login (&optional phone password)
  "Login with PHONE number and PASSWORD."
  (interactive)
  (unless phone
    (setq phone (read-string "Phone number(Countrycode[Space]number): " "+86 ")
          password (md5 (read-passwd "Password: "))))
  (if (not (netease-cloud-music-api-process-live-p))
      (netease-cloud-music-error "API process is null!")
    (let ((countrycode (prog2 (string-match "\\(.*\\) \\(.*\\)" phone)
                           (substring (match-string 1 phone) 1)
                         (setq phone (match-string 2 phone))))
          login-result other-info)
      (request (format "%s:%s/login/cellphone?phone=%s&md5_password=%s&countrycode=%s"
		       netease-cloud-music-api-address
                       netease-cloud-music-api-port
		       phone
		       password
		       countrycode)
        :parser 'json-read
        :success (netease-cloud-music-expand-form
                  (setq login-result data))
        :sync t)
      (if (/= 200 (alist-get 'code login-result))
          (netease-cloud-music-error "Phone number or password is error!")
        (setq netease-cloud-music-user-id (alist-get 'id (alist-get 'account login-result))
              netease-cloud-music-username (alist-get 'nickname (alist-get 'profile login-result))
              netease-cloud-music-user-password password
              netease-cloud-music-phone (cons countrycode phone))
        (netease-cloud-music-save-loginfo
         (cons netease-cloud-music-phone
               netease-cloud-music-user-password))
        (message "[Netease-Cloud-Music]: Login successfully!")
        (netease-cloud-music-tui-init)
        (netease-cloud-music-for-eaf
         :eaf-buffer
         (setq other-info (list netease-cloud-music-username
                                (alist-get 'avatarUrl
                                           (alist-get 'profile login-result))))
         (setq eaf-netease-cloud-music-user+list other-info)
         (netease-cloud-music-call-js "update_user_info"
                                      (json-encode other-info)))
        (netease-cloud-music--refresh-playlists)))))

(netease-cloud-music-api-defun netease-cloud-music--song-url-by-user (id)
  "Get the song's url by user.
ID is the song's id."
  (let ((song-info (netease-cloud-music-api-request
                    (format "song/url?id=%d" id))))
    (if (null song-info)
        (netease-cloud-music-error "The API can't be used, maybe it's starting!")
      (if (/= 200 (alist-get 'code song-info))
          (netease-cloud-music-error "The song whose id is %d cannot found!" id)
        (alist-get 'url (aref (alist-get 'data song-info) 0))))))

(netease-cloud-music-api-defun netease-cloud-music--get-user-info ()
  "Get user's info automatically and then login."
  (let ((info (cdr (car (ignore-errors
                          (netease-cloud-music-api-request "login/status")))))
        other-info)
    (when info
      (if (/= 200 (alist-get 'code info))
          (netease-cloud-music-error "Phone number or password is error!")
        (setq netease-cloud-music-username (alist-get 'nickname (alist-get 'profile info))
              netease-cloud-music-user-id (alist-get 'id (alist-get 'account info)))
        (netease-cloud-music-for-eaf
         :eaf-buffer
         (setq other-info
               (list netease-cloud-music-username
                     (alist-get 'avatarUrl
                                (alist-get 'profile info))))
         (setq eaf-netease-cloud-music-user+list
               other-info)
         (netease-cloud-music-call-js "update_user_info"
                                      (json-encode other-info)))
        (message "[Netease-Cloud-Music]: Login successfully!")))))

(defun netease-cloud-music--refresh-playlists ()
  "Refresh the user's playlists."
  (setq netease-cloud-music-playlists
        (netease-cloud-music-get-user-playlist netease-cloud-music-user-id))
  (netease-cloud-music-for-eaf
   :eaf-buffer
   (netease-cloud-music-call-js "set_user_playlists"
                                (json-encode
                                 (netease-cloud-music--cons-to-list
                                  netease-cloud-music-playlists)))))

(netease-cloud-music-api-defun netease-cloud-music-create-playlist (&optional name)
  "Create a new playlist named NAME."
  (interactive)
  (unless name
    (setq name (read-string "Enter the playlist's name: ")))
  (let ((new-playlist (netease-cloud-music-api-request
                       (concat "playlist/create?name="
                               (url-encode-url name)))))
    (if (or (null new-playlist) (/= 200 (alist-get 'code new-playlist)))
        (netease-cloud-music-error "Failed to create new playlist!")
      (run-with-timer
       2 nil #'netease-cloud-music--refresh-playlists)
      (message "[Netease-Cloud-Music]: Created the playlist named %s!" name)
      (netease-cloud-music-tui-init))))

(netease-cloud-music-api-defun netease-cloud-music-delete-playlist (&optional name)
  "Delete the user's playlist which named NAME."
  (interactive)
  (unless name
    (setq name (if (get-buffer netease-cloud-music-buffer-name)
                   (completing-read "Enter the playlist you want to delete: "
                                    (netease-cloud-music-delete--list-playlist))
                 (netease-cloud-music-for-eaf
                  :eaf-buffer
                  (prog2 (netease-cloud-music-call-js "set_index_style" (json-encode t))
                      (- (read-number "Enter the playlist's index: ") 2)
                    (netease-cloud-music-call-js "set_index_style" (json-encode nil)))))))
  (let ((id (if (get-buffer netease-cloud-music-buffer-name)
                (progn
                  (string-match "^\\(.*\\) - \\(.*\\)" name)
                  (match-string 2 name))
              (number-to-string (cdr (nth name netease-cloud-music-playlists)))))
        result)
    (if (not (stringp id))
        (netease-cloud-music-error "The playlist is not exists!")
      (setq result (netease-cloud-music-api-request (concat "playlist/delete?id=" id)))
      (if (or (null result) (/= 200 (alist-get 'code result)))
          (netease-cloud-music-error "Failed to delete the playlist!")
        (run-with-timer
         2 nil #'netease-cloud-music--refresh-playlists)
        (when (and netease-cloud-music-playlist-id
                   (= netease-cloud-music-playlist-id id))
          (setq netease-cloud-music-use-local-playlist t
                netease-cloud-music-playlist-id nil))
        (message "[Netease-Cloud-Music]: Deleted the playlist successfully!")
        (netease-cloud-music-tui-init)))))

(netease-cloud-music-api-defun netease-cloud-music-delete--list-playlist ()
  "List the playlist then retunr it."
  (let ((user-playlists (netease-cloud-music-get-user-playlist
                         netease-cloud-music-user-id))
        result)
    (if (null user-playlists)
        (netease-cloud-music-error "Your playlist is null!")
      (dolist (playlist user-playlists)
        (setq result (append result
                             (list (format "%s - %s"
                                           (car playlist) (cdr playlist))))))
      result)))

(defun netease-cloud-music--playlist-exists (pid)
  "Check if the playlist whose id is PID exists."
  (netease-cloud-music-alist-cdr
   pid (netease-cloud-music-get-user-playlist
        netease-cloud-music-user-id)))

(netease-cloud-music-api-defun netease-cloud-music-update-playlist-name (id name)
  "Replace the name of playlist whose id is equal to ID with NAME."
  (let (result)
    (if (null (netease-cloud-music--playlist-exists id))
        (netease-cloud-music-error "The playlist is not exists!")
      (setq result (netease-cloud-music-api-request
                    (format "playlist/name/update?id=%d&name=%s"
                            id
                            (url-encode-url name))))
      (if (or (null result) (/= 200 (alist-get 'code result)))
          (netease-cloud-music-error "Failed to update the name of the playlist!")
        (run-with-timer
         2 nil #'netease-cloud-music--refresh-playlists)
        (message "[Netease-Cloud-Music]: Updated playlist's name successfully!")))))

(defun netease-cloud-music-change-playlist-name (&optional pid name)
  "Change the playlist's NAME under cursor.
PID is its id.
NAME is its name."
  (interactive)
  (unless pid
    (if (get-buffer netease-cloud-music-buffer-name)
        (let ((playlist (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))
              id)
          (if (setq id (alist-get playlist
                                  netease-cloud-music-playlists
                                  nil nil 'string-equal))
              (setq pid id)
            (netease-cloud-music-error "The playlist is not exists!")))
      (netease-cloud-music-for-eaf
       :eaf-buffer
       (netease-cloud-music-call-js "set_index_style" (json-encode t))
       (let* ((index (- (read-number "Enter the playlist index: ") 2)) ;The index include local playlist, so minus 2.
              (playlist (nth index netease-cloud-music-playlists)))
         (setq pid (cdr-safe playlist)))
       (netease-cloud-music-call-js "set_index_style" (json-encode nil))))
    (setq name (read-string "Enter the new name: ")))
  (netease-cloud-music-update-playlist-name pid name)
  (netease-cloud-music-tui-init))

(defun netease-cloud-music--list-to-splited-string (list)
  "Convert the LIST to string splited with comma."
  (let (result)
    (if (listp list)
        (dolist (id list)
          (setq result
                (concat result
                        (if (null result)
                            (number-to-string id)
                          (concat "," (number-to-string id))))))
      (setq result (if (numberp list)
                       (number-to-string list)
                     list)))
    result))

(netease-cloud-music-api-defun netease-cloud-music-update-songs-order-in-playlist (pid song-ids)
  "Update the songs' order in the playlist.
PID is the id of the playlist, SONG-IDS is the list of songs' ids."
  (let (result)
    (if (null (netease-cloud-music--playlist-exists pid))
        (netease-cloud-music-error "The playlist is not exists!")
      (setq song-ids (netease-cloud-music--list-to-splited-string song-ids))
      (setq result (netease-cloud-music-api-request
                    (format "song/order/update?pid=%s&ids=\\[%s\\]"
                            pid song-ids)))
      (if (or (null result) (/= 200 (alist-get 'code result)))
          (netease-cloud-music-error "Failed to update the songs' order in the playlist!")
        (message "[Netease-Cloud-Music]: Updated songs order successfully!")
        (when (and netease-cloud-music-playlist-id
                   (= netease-cloud-music-playlist-id pid))
          (when (or (null netease-cloud-music-timer-protect)
                    (eq netease-cloud-music-timer-protect 'order))
            (run-with-timer
             1 nil (lambda ()
                     (message "[Netease-Cloud-Music]: Syncing the playlist...")))
            (netease-cloud-music--delete-other-timer)
            (setq netease-cloud-music-playlist-refresh-timer
                  (run-with-timer
                   1 1.5 #'netease-cloud-music--refresh-playlist-songs))))))))

(netease-cloud-music-api-defun netease-cloud-music--track (add pid tracks)
  "Add or delete TRACKS with playlist whose id is PID.
If ADD is t, add songs.Otherwise delete songs."
  (let ((option (if add
                    "add"
                  "del"))
        result)
    (if (null (netease-cloud-music--playlist-exists pid))
        (netease-cloud-music-error "The playlist is not exists!")
      (setq tracks (netease-cloud-music--list-to-splited-string tracks))
      (setq result (netease-cloud-music-api-request
                    (format "playlist/tracks/?op=%s&pid=%d&tracks=%s"
                            option pid tracks)))
      (if (or (null result) (/= 200 (alist-get 'code (alist-get 'body result))))
          (netease-cloud-music-error "Failed to %s the songs with playlist!" option)
        (message "[Netease-Cloud-Music]: %s the songs with playlist successfully!"
                 (if add
                     "Added"
                   "Deleted"))
        (when (and netease-cloud-music-playlist-id
                   (= netease-cloud-music-playlist-id pid))
          (when (or (null netease-cloud-music-timer-protect)
                    (eq netease-cloud-music-timer-protect 'track))
            (run-with-timer
             1 nil (lambda ()
                     (message "[Netease-Cloud-Music]: Syncing the playlist...")))
            (netease-cloud-music--delete-other-timer)
            (setq netease-cloud-music-playlist-refresh-timer
                  (run-with-timer
                   1 1.5 #'netease-cloud-music--refresh-playlist-songs))))))))

(netease-cloud-music-api-defun netease-cloud-music-like-song (id)
  "Like or dislike song, ID is its id."
  (let (option like-playlist result tmp)
    (if (null netease-cloud-music-user-id)
        (netease-cloud-music-error "You should login first!")

      ;; To check if the song has already been liked.
      (setq tmp
            (netease-cloud-music-get-user-playlist netease-cloud-music-user-id))
      (catch 'stop
        (dolist (playlist tmp)
          (when (string-match-p "\\(.*\\)喜欢的音乐" (car playlist))
            (setq like-playlist (cdr playlist))
            (setq tmp (netease-cloud-music-get-playlist-songs like-playlist))
            (throw 'stop t))))          ;To let the liked songs to `tmp'.
      (setq option (if (alist-get id tmp)
                       "false"
                     "true"))

      (setq result (netease-cloud-music-api-request (format "like?like=%s&id=%s"
                                                            option id)))
      (if (or (null result) (/= 200 (alist-get 'code result)))
          (netease-cloud-music-error "Failed to like/dislike the song!")
        (message "[Netease-Cloud-Music]: %s the song successfully!"
                 (if (string-equal option "true")
                     "Liked "
                   "Disliked"))
        (when (and netease-cloud-music-playlist-id
                   (= netease-cloud-music-playlist-id like-playlist))
          (when (or (null netease-cloud-music-timer-protect)
                    (eq netease-cloud-music-timer-protect 'like))
            (run-with-timer
             1 nil (lambda ()
                     (message "[Netease-Cloud-Music]: Syncing the playlist...")))
            (netease-cloud-music--delete-other-timer)
            (setq netease-cloud-music-playlist-refresh-timer
                  (run-with-timer
                   1 1.5 #'netease-cloud-music--refresh-playlist-songs))))
        (netease-cloud-music-tui-init)))))

(netease-cloud-music-api-defun netease-cloud-music-get-recommend-songs ()
  "Get the recommend songs."
  (interactive)
  (let ((songs (netease-cloud-music-api-request "recommend/songs"))
        result song artist)
    (if (or (null songs)
            (/= 200 (alist-get 'code songs)))
        (netease-cloud-music-error "Cannot get recomend songs!")
      (setq songs (alist-get 'dailySongs (alist-get 'data songs)))
      (dotimes (i (length songs))
        (setq song (aref songs i)
              artist (aref (alist-get 'ar song) 0))
        (setq result
              (append result
                      (list (list (alist-get 'id song)
                                  (alist-get 'name song)
                                  (alist-get 'id artist)
                                  (alist-get 'name artist))))))
      (setq netease-cloud-music-search-alist
            (append '("Recommend songs") result))
      (if (get-buffer netease-cloud-music-buffer-name)
          (netease-cloud-music-search-song--open-switch
           result)
        (setq netease-cloud-music-search-type 'song)
        (netease-cloud-music-for-eaf
         :eaf-buffer
         (netease-cloud-music-call-js "reset_song_style")
         (netease-cloud-music-call-js "set_playlist" (json-encode-array
                                                      result))
         (netease-cloud-music-call-js "change_playlist_mode" (json-encode t)))))))

(netease-cloud-music-api-defun netease-cloud-music-get-recommend-playlists ()
  "Get recommend playlists."
  (interactive)
  (let ((playlists (netease-cloud-music-api-request "recommend/resource"))
        result playlist)
    (if (or (null playlists)
            (/= 200 (alist-get 'code playlists)))
        (netease-cloud-music-error "Cannot get recommend playlists!")
      (setq playlists (alist-get 'recommend playlists))
      (dotimes (i (length playlists))
        (setq playlist (aref playlists i))
        (setq result
              (append result
                      (list (cons (alist-get 'name playlist)
                                  (alist-get 'id playlist))))))
      (setq netease-cloud-music-search-playlists
            (append '("Recomend playlists") result))
      (if (get-buffer netease-cloud-music-buffer-name)
          (netease-cloud-music-playlist--open-switch result)
        (setq netease-cloud-music-search-type 'playlist)
        (netease-cloud-music-for-eaf
         :eaf-buffer
         (netease-cloud-music-call-js "reset_song_style")
         (netease-cloud-music-call-js "set_playlist" (json-encode-array
                                                      (netease-cloud-music--playlist-search-format
                                                       result)))
         (netease-cloud-music-call-js "change_playlist_mode" (json-encode t)))))))

(defun netease-cloud-music--refresh-playlist-songs ()
  "Refresh the playlist songs."
  (if netease-cloud-music-use-local-playlist
      (when netease-cloud-music-playlist-refresh-timer
        (cancel-timer netease-cloud-music-playlist-refresh-timer)
        (setq netease-cloud-music-playlist-refresh-timer nil))
    (let ((songs (netease-cloud-music-get-playlist-songs
                  netease-cloud-music-playlist-id)))
      (unless (equal songs netease-cloud-music-playlists-songs)
        (setq netease-cloud-music-playlists-songs songs)
        (when netease-cloud-music-playlist-refresh-timer
          (message "[Netease-Cloud-Music]: Syncing the playlist... Done")
          (cancel-timer netease-cloud-music-playlist-refresh-timer)
          (setq netease-cloud-music-playlist-refresh-timer nil)
          (when netease-cloud-music-timer-protect
            (setq netease-cloud-music-timer-protect nil)))
        (netease-cloud-music-for-eaf
         :eaf-buffer
         (netease-cloud-music-call-js "set_playlist" (json-encode-array
                                                      netease-cloud-music-playlists-songs)))
        (netease-cloud-music-adjust-song-index)
        (netease-cloud-music-tui-init)))))

(defun netease-cloud-music-delete-song-from-playlist (&optional index)
  "Delete current song from playlist.
INDEX is the song's index in playlist."
  (interactive)
  (when (or index
            (yes-or-no-p "Do you really want to delete the song? "))
    (let ((song (if index
                    index
                  (netease-cloud-music--current-song)))
          (current-line (line-number-at-pos)))
      (when song
        (if netease-cloud-music-use-local-playlist
            (progn
              (setq netease-cloud-music-playlist
                    (delete (nth song netease-cloud-music-playlist)
                            netease-cloud-music-playlist))
              (netease-cloud-music-save-playlist)
              (netease-cloud-music-adjust-song-index))
          (netease-cloud-music--track
           nil netease-cloud-music-playlist-id
           (car (nth song netease-cloud-music-playlists-songs))))
        (netease-cloud-music-tui-init)
        (when (get-buffer netease-cloud-music-buffer-name)
          (with-current-buffer netease-cloud-music-buffer-name
            (goto-char (point-min))
            (forward-line (1- current-line))))))))

(defun netease-cloud-music-delete-playing-song ()
  "Delete playing song."
  (interactive)
  (let ((current-song netease-cloud-music-current-song))
    (if (null current-song)
        (netease-cloud-music-error "You're playing nothing!")
      (netease-cloud-music-process-sentinel nil "killed")
      (if netease-cloud-music-use-local-playlist
          (progn
            (setq netease-cloud-music-playlist
                  (delete (nth
                           (netease-cloud-music--current-song
                            (format "%s - %s"
                                    (car current-song)
                                    (nth 1 current-song)))
                           netease-cloud-music-playlist)
                          netease-cloud-music-playlist))
            (netease-cloud-music-save-playlist)
            (netease-cloud-music-adjust-song-index))
        (netease-cloud-music--track
         nil netease-cloud-music-playlist-id
         (nth 2 current-song))))))

(defun netease-cloud-music--delete-other-timer ()
  "Delete other playlist refresh timer."
  (when netease-cloud-music-playlist-refresh-timer
    (cancel-timer netease-cloud-music-playlist-refresh-timer)
    (setq netease-cloud-music-playlist-refresh-timer
          nil)))

;;; Switch-mode functions

(defun netease-cloud-music-switch-enter (&optional index)
  "The enter action in `netease-cloud-music-switch-mode'.
INDEX is the index of the song in search list."
  (interactive)
  (with-current-buffer (current-buffer)
    (let (song-list)
      (if (get-buffer netease-cloud-music-buffer-name)
          (let* ((content (substring-no-properties (thing-at-point 'line) 0 -1))
                 (song-info (progn
                              (string-match "^<<\\(.*\\)>> - \\(.*\\)" content)
                              (cons (match-string 1 content) (match-string 2 content))))
                 (song (car-safe song-info))
                 (artist (cdr-safe song-info)))
            (if (not (and song artist))
                (netease-cloud-music-error "The song info of the song under cursor is error!")
              (setq song-list (netease-cloud-music--get-song-list song artist))
              (netease-cloud-music-switch-close)))
        (if (null index)
            (netease-cloud-music-error "The index argument is needed!")
          (setq song-list (nth index (cdr netease-cloud-music-search-alist)))))

      (if netease-cloud-music-use-local-playlist
          (progn
            (add-to-list 'netease-cloud-music-playlist song-list)
            (netease-cloud-music-save-playlist))
        (netease-cloud-music--track t netease-cloud-music-playlist-id
                                    (car song-list)))
      (netease-cloud-music-play
       (car-safe song-list)
       (nth 1 song-list) (nth 3 song-list))
      (netease-cloud-music-for-eaf
       :eaf-buffer
       (netease-cloud-music-call-js "change_playlist_mode" (json-encode nil))
       (netease-cloud-music-call-js "set_playlist" (json-encode-array
                                                    (if netease-cloud-music-use-local-playlist
                                                        netease-cloud-music-playlist
                                                      netease-cloud-music-playlists-songs)))
       (netease-cloud-music-adjust-song-index)))))

(defun netease-cloud-music-switch-next-page ()
  "Next page in switch mode."
  (interactive)
  (netease-cloud-music--page (1+ netease-cloud-music-search-page)))

(defun netease-cloud-music-switch-prev-page ()
  "Previous page in switch mode."
  (interactive)
  (unless (< netease-cloud-music-search-page 2)
    (netease-cloud-music--page (1- netease-cloud-music-search-page))))

(defun netease-cloud-music--page (page)
  "Goto the PAGE."
  (when netease-cloud-music-search-alist
    (let* ((search-content (car netease-cloud-music-search-alist))
           (limit (* netease-cloud-music-search-limit page))
           (mode (if (or (eq major-mode 'netease-cloud-music-switch-song-mode)
                         (eq netease-cloud-music-search-type 'song))
                     (cons nil 'netease-cloud-music-get-song)
                   (cons 'playlist 'netease-cloud-music-get-playlists)))
           (data (netease-cloud-music-request-from-api
                  search-content
                  (car mode)
                  limit))
           (search-results
            (netease-cloud-music--catch
             limit
             (funcall (cdr mode) data))))
      (if (get-buffer netease-cloud-music-buffer-name)
          (if (eq (car mode) 'playlist)
              (netease-cloud-music-playlist--open-switch search-results)
            (netease-cloud-music-search-song--open-switch
             search-results))
        (netease-cloud-music-for-eaf
         :eaf-buffer
         (netease-cloud-music-call-js "change_playlist_mode" (json-encode nil))
         (netease-cloud-music-call-js "set_playlist" (json-encode-array
                                                      (if (eq (car mode) 'playlist)
                                                          (netease-cloud-music--playlist-search-format
                                                           search-results)
                                                        search-results)))))
      (setq netease-cloud-music-search-page page)

      (if (eq (car mode) 'playlist)
          (setq netease-cloud-music-search-playlists
                (cons search-content search-results))
        (setq netease-cloud-music-search-alist
              (cons search-content search-results))))))

(defun netease-cloud-music-switch-add-to-playlist ()
  "Add the songs in current page to playlist."
  (interactive)
  (if netease-cloud-music-use-local-playlist
      (progn
        (netease-cloud-music--append (cdr netease-cloud-music-search-alist))
        (netease-cloud-music-save-playlist))
    (let (ids)
      (dolist (song (cdr netease-cloud-music-search-alist))
        (unless (alist-get (car song) netease-cloud-music-playlists-songs)
          (setq ids (append (list (car song)) ids))))
      (netease-cloud-music--track t netease-cloud-music-playlist-id ids)))
  (if (get-buffer netease-cloud-music-buffer-name)
      (progn
        (netease-cloud-music-switch-close)
        (netease-cloud-music-interface-init))
    (netease-cloud-music-for-eaf
     :eaf-buffer
     (netease-cloud-music-call-js "change_playlist_mode" (json-encode nil))
     (netease-cloud-music-call-js "set_playlist" (json-encode-array
                                                  (if netease-cloud-music-use-local-playlist
                                                      netease-cloud-music-playlist
                                                    netease-cloud-music-playlists-songs)))
     (netease-cloud-music-adjust-song-index))))

(defun netease-cloud-music-switch-add-page (&optional page)
  "Add the pages to playlist.
PAGE is the pages info."
  (interactive)
  (when (and (get-buffer "eaf-netease-cloud-music")
             (eq netease-cloud-music-search-type 'playlist))
    (netease-cloud-music-error "This function can only used for song!"))
  (unless page
    (setq page (read-string "Enter the page[n-n]: "
                            (concat (number-to-string netease-cloud-music-search-page)
                                    "-"))))
  (if netease-cloud-music-use-local-playlist
      (progn
        (netease-cloud-music--append (netease-cloud-music--songs-by-page page))
        (netease-cloud-music-save-playlist))
    (let (ids)
      (dolist (song (netease-cloud-music--songs-by-page page))
        (unless (alist-get (car song) netease-cloud-music-playlists-songs)
          (setq ids (append (list (car song)) ids))))
      (netease-cloud-music--track t netease-cloud-music-playlist-id ids)))
  (if (get-buffer netease-cloud-music-buffer-name)
      (progn
        (netease-cloud-music-switch-close)
        (netease-cloud-music-interface-init))
    (netease-cloud-music-for-eaf
     :eaf-buffer
     (netease-cloud-music-call-js "change_playlist_mode" (json-encode nil))
     (netease-cloud-music-call-js "set_playlist" (json-encode-array
                                                  (if netease-cloud-music-use-local-playlist
                                                      netease-cloud-music-playlist
                                                    netease-cloud-music-playlists-songs)))
     (netease-cloud-music-adjust-song-index))))

(defun netease-cloud-music-playlist-enter (&optional index)
  "Add the playlist under the cursor.
INDEX is the index of the playlist in search list."
  (interactive)
  (let ((playlist (if (get-buffer netease-cloud-music-buffer-name)
                      (alist-get
                       (substring (thing-at-point 'line) 0 -1)
                       netease-cloud-music-search-playlists nil nil 'string-equal)
                    (if (null index)
                        (netease-cloud-music-error "The index is error!")
                      (cdr (nth index (cdr netease-cloud-music-search-playlists))))))
        ids)
    (if (null playlist)
        (netease-cloud-music-error "The playlist can not found!")
      (if netease-cloud-music-use-local-playlist
          (progn
            (netease-cloud-music--append (netease-cloud-music-get-playlist-songs playlist))
            (netease-cloud-music-save-playlist))
        (dolist (song (netease-cloud-music-get-playlist-songs playlist))
          (unless (alist-get (car song) netease-cloud-music-playlists-songs)
            (setq ids (append (list (car song)) ids))))
        (netease-cloud-music--track t netease-cloud-music-playlist-id ids))
      (when (get-buffer netease-cloud-music-buffer-name)
        (netease-cloud-music-switch-close)
        (netease-cloud-music-interface-init))
      (netease-cloud-music-for-eaf
       :eaf-buffer
       (netease-cloud-music-call-js "change_playlist_mode" (json-encode nil))
       (netease-cloud-music-call-js "set_playlist" (json-encode-array
                                                    (if netease-cloud-music-use-local-playlist
                                                        netease-cloud-music-playlist
                                                      netease-cloud-music-playlists-songs)))
       (when netease-cloud-music-process
         (netease-cloud-music-call-js "change_song_style" netease-cloud-music-playlist-song-index))))))

(defun netease-cloud-music-playlist-tab (&optional index)
  "Toggle the songs of playlist under cursor in switch playlist buffer.
INDEX is the playlist's index."
  (interactive)
  (let ((playlist (if (get-buffer netease-cloud-music-buffer-name)
                      (alist-get
                       (substring (thing-at-point 'line) 0 -1)
                       netease-cloud-music-search-playlists nil nil 'string-equal)
                    (if (null index)
                        (netease-cloud-music-error "The index is error!")
                      (cdr (nth index (cdr netease-cloud-music-search-playlists)))))))

    (if (null playlist)
        (netease-cloud-music-error "The playlist can not found!")
      (with-current-buffer "*Netease-Cloud-Music:Switch->Playlist*"
        (setq-local buffer-read-only nil)
        (save-mark-and-excursion
          (forward-line)                    ;To check the next line if it's a null string or songs' info.
          (if (not (or (equal (get-text-property (point) 'face)
                              'netease-cloud-music-playlist-face)
                       (string-empty-p (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position)))))
              (progn
                (while (eq (get-text-property (point) 'face)
                           'netease-cloud-music-song-face)
                  (delete-region (line-beginning-position) (line-end-position))
                  (delete-char 1)))
            (forward-line -1)
            (goto-char (line-end-position))
            (insert "\n")
            (let ((songs (netease-cloud-music-get-playlist-songs playlist)))
              (if (null songs)
                  (message "[Netease-Cloud-Music]: The playlist is empty.")
                (dolist (song songs)
                  (insert (format "%s - %s\n"
                                  (propertize
                                   (nth 1 song)
                                   'face 'netease-cloud-music-song-face)
                                  (propertize
                                   (if (nth 3 song) (nth 3 song) "nil")
                                   'face 'netease-cloud-music-artist-face))))))
            (delete-char -1)))
        (setq-local buffer-read-only t)))))

(defun netease-cloud-music-playlist-add-all ()
  "Add all the searched playlists to the playlist."
  (interactive)
  (if (null netease-cloud-music-search-playlists)
      (netease-cloud-music-error "The playlist can not found!")
    (let (ids)                          ;For user's playlist
      (dolist (playlist (cdr netease-cloud-music-search-playlists))
        (if netease-cloud-music-use-local-playlist
            (progn
              (netease-cloud-music--append
               (netease-cloud-music-get-playlist-songs (cdr playlist)))
              (netease-cloud-music-save-playlist))
          (dolist (song (netease-cloud-music-get-playlist-songs (cdr playlist)))
            (unless (alist-get (car song) netease-cloud-music-playlists-songs)
              (setq ids (append (list (car song)) ids))))))
      (when ids
        (netease-cloud-music--track t netease-cloud-music-playlist-id ids)))
    (if (get-buffer netease-cloud-music-buffer-name)
        (progn
          (netease-cloud-music-switch-close)
          (netease-cloud-music-interface-init))
      (netease-cloud-music-for-eaf
       :eaf-buffer
       (netease-cloud-music-call-js "change_playlist_mode" (json-encode nil))
       (netease-cloud-music-call-js "set_playlist" (json-encode-array
                                                    (if netease-cloud-music-use-local-playlist
                                                        netease-cloud-music-playlist
                                                      netease-cloud-music-playlists-songs)))
       (netease-cloud-music-adjust-song-index)))))

(defun netease-cloud-music-storage-song (&optional song)
  "Storage the SONG under cursor or with its index."
  (interactive)
  (unless song
    (let (tmp)
      (setq song (catch 'result
                   (if (get-buffer netease-cloud-music-buffer-name)
                       (nth (if (setq tmp (netease-cloud-music--current-song))
                                tmp
                              (throw 'result
                                     (if (string= (completing-read "Do you want to add: "
                                                                   '("current song" "all"))
                                                  "all")
                                         'all
                                       nil)))
                            (if netease-cloud-music-use-local-playlist
                                netease-cloud-music-playlist
                              netease-cloud-music-playlists-songs))
                     (nth (1- (cond ((string=
                                      (setq tmp
                                            (read-string "Enter the song's index(or input all): "))
                                      "")
                                     (throw 'result nil))
                                    ((string= tmp "all")
                                     (throw 'result 'all))
                                    (t (string-to-number tmp))))
                          (if netease-cloud-music-use-local-playlist
                              netease-cloud-music-playlist
                            netease-cloud-music-playlists-songs)))))))
  (if (null song)
      (netease-cloud-music-storage-current-song)
    (if (eq song 'all)
        (netease-cloud-music-storage-current-playlist)
      (unless (netease-cloud-music--memeq song netease-cloud-music-storage)
        (setq netease-cloud-music-storage
              (append netease-cloud-music-storage
                      (list song)))
        (message "[Netease-Cloud-Music]: Added the song into storage.")))))

(defun netease-cloud-music-storage-current-playlist ()
  "Storage current playlist."
  (dolist (song (if netease-cloud-music-use-local-playlist
                    netease-cloud-music-playlist
                  netease-cloud-music-playlists-songs))
    (unless (netease-cloud-music--memeq song netease-cloud-music-storage)
      (setq netease-cloud-music-storage
            (append netease-cloud-music-storage
                    (list song)))))
  (message "[Netease-Cloud-Music]: Added current playlist to storage."))

(defun netease-cloud-music-storage-current-song ()
  "Storage current playing song."
  (interactive)
  (if (null netease-cloud-music-process)
      (netease-cloud-music-error "You're not playing any song!")
    (let ((song (nth netease-cloud-music-playlist-song-index
                     (if netease-cloud-music-use-local-playlist
                         netease-cloud-music-playlist
                       netease-cloud-music-playlists-songs))))
      (unless (netease-cloud-music--memeq song netease-cloud-music-storage)
        (setq netease-cloud-music-storage
              (append netease-cloud-music-storage
                      (list song)))
        (message "[Netease-Cloud-Music]: Added current playing song into storage.")))))

(defun netease-cloud-music-add-storage-to-current-playlist ()
  "Add the songs in storage into current playlist."
  (interactive)
  (if (null netease-cloud-music-storage)
      (netease-cloud-music-error "The storage is empty!")
    (if netease-cloud-music-use-local-playlist
        (progn
          (netease-cloud-music--append netease-cloud-music-storage)
          (netease-cloud-music-save-playlist))
      (let (ids)
        (dolist (song netease-cloud-music-storage)
          (setq ids (append (list (car song)) ids)))
        (netease-cloud-music--track t netease-cloud-music-playlist-id ids)))
    (if (get-buffer netease-cloud-music-buffer-name)
        (progn
          (netease-cloud-music-interface-init))
      (netease-cloud-music-for-eaf
       :eaf-buffer
       (netease-cloud-music-call-js "set_playlist" (json-encode-array
                                                    (if netease-cloud-music-use-local-playlist
                                                        netease-cloud-music-playlist
                                                      netease-cloud-music-playlists-songs)))
       (netease-cloud-music-adjust-song-index)))))

(defun netease-cloud-music-show-storage (&optional refresh)
  "Show the songs in storage.
REFRESH means to refresh the storage."
  (interactive)
  (if (null netease-cloud-music-storage)
      (message "[Netease-Cloud-Music]: Storage is empty.")
    (if (get-buffer netease-cloud-music-buffer-name)
        (netease-cloud-music-search-song--open-switch
         netease-cloud-music-storage)
      (netease-cloud-music-for-eaf
       :eaf-buffer
       (when refresh
         (netease-cloud-music-call-js "change_playlist_mode" (json-encode nil)))
       (netease-cloud-music-call-js "reset_song_style")
       (netease-cloud-music-call-js "set_playlist" (json-encode-array
                                                    netease-cloud-music-storage))
       (netease-cloud-music-call-js "change_playlist_mode" (json-encode t))))))

(defun netease-cloud-music-delete-song-from-storage (&optional song)
  "Delete SONG from storage."
  (interactive)
  (setq song (nth (if (get-buffer netease-cloud-music-buffer-name)
                      (with-current-buffer "*Netease-Cloud-Music:Switch->Songs*"
                        (1- (line-number-at-pos)))
                    (1- (read-number "Enter the song's index: ")))
                  netease-cloud-music-storage))
  (if (null song)
      (netease-cloud-music-error "The song is not exists!")
    (setq netease-cloud-music-storage (delete song netease-cloud-music-storage))
    (message "[Netease-Cloud-Music]: Deleted song from storage.")
    (if (null netease-cloud-music-storage)
        (if (get-buffer netease-cloud-music-buffer-name)
            (netease-cloud-music-switch-close)
          (netease-cloud-music-for-eaf
           :eaf-buffer
           (netease-cloud-music-call-js "change_playlist_mode" (json-encode nil))
           (netease-cloud-music-call-js "set_playlist" (json-encode-array
                                                        (if netease-cloud-music-use-local-playlist
                                                            netease-cloud-music-playlist
                                                          netease-cloud-music-playlists-songs)))
           (netease-cloud-music-adjust-song-index)))
      (netease-cloud-music-show-storage t))))

(defun netease-cloud-music-clear-storage ()
  "Clear the storage."
  (interactive)
  (when (yes-or-no-p "Do you really want to clear storage?")
    (setq netease-cloud-music-storage nil)))

;;; Basic functions

(defun netease-cloud-music-get-loginfo ()
  "Get login info."
  (when (file-exists-p netease-cloud-music-user-loginfo-file)
    (let* ((content (split-string
                     (with-temp-buffer
                       (insert-file-contents netease-cloud-music-user-loginfo-file)
                       (buffer-string))
                     "\n" t))
           (phone (cons (car content) (nth 1 content)))
           (password (nth 2 content)))
      (cons phone password))))

(defun netease-cloud-music-save-loginfo (loginfo)
  "Save login info.  LOGINFO is the info."
  (unless (file-exists-p netease-cloud-music-user-loginfo-file)
    (make-empty-file netease-cloud-music-user-loginfo-file))
  (with-temp-file netease-cloud-music-user-loginfo-file
    (erase-buffer)
    (insert (car (car loginfo)) ;Countrycode
            "\n"
            (cdr (car loginfo)) ;Phone number
            "\n"
            (cdr loginfo))))          ;Password

(defun netease-cloud-music-request-from-api (content &optional type limit)
  "Request the CONTENT from Netease Music API.

CONTENT is a string.

TYPE is a symbol, its value can be song.

LIMIT is the limit for the search result, it's a number."
  (when (null type) (setq type 'song))
  (when (null limit) (setq limit "1"))
  (let (result search-type)
    ;; result type
    (pcase type
      ('song (setq search-type "1"))
      ('playlist (setq search-type "1000")))
    (when (numberp limit)
      (setq limit (number-to-string limit)))
    (request
      netease-cloud-music-search-api
      :type "POST"
      :data `(("s" . ,content)
              ("limit" . ,limit)
              ("type" . ,search-type)
              ("offset" . "0"))
      :parser 'json-read
      :success (netease-cloud-music-expand-form (setq result data))
      :sync t)
    result))

(defun netease-cloud-music--songs-by-page (page-string)
  "Get the songs list by the page limit.
PAGE-STRING is the string that includes the two pages."
  (let (start end search-result)
    (progn
      (string-match "\\(.*\\)-\\(.*\\)" page-string)
      (setq start (match-string 1 page-string)
            end (match-string 2 page-string)))
    (when (and start end)
      (setq start (* netease-cloud-music-search-limit (1- (string-to-number start)))
            end (* netease-cloud-music-search-limit (string-to-number end)))
      (setq search-result
            (netease-cloud-music-get-song
             (netease-cloud-music-request-from-api
              (car netease-cloud-music-search-alist)
              nil end)))
      (when search-result
        (setq search-result
              (netease-cloud-music--catch nil search-result start t))))))

(defun netease-cloud-music--get-song-list (name artist)
  "Get the song-info list by its NAME and ARTIST."
  (when (cdr-safe netease-cloud-music-search-alist)
    (catch 'result
      (dolist (song-list (cdr netease-cloud-music-search-alist))
        (when (and (string= name (nth 1 song-list))
                   (string= artist (nth 3 song-list)))
          (throw 'result song-list))))))

(defun netease-cloud-music--catch (all list &optional index-limit no-result-limit)
  "Catch the song list or playlists by ALL length in LIST.
INDEX-LIMIT is the start of the song-list or playlists.
NO-RESULT-LIMIT means do not limit the catch."
  (let ((index (if index-limit
                   index-limit
                 (- all netease-cloud-music-search-limit)))
        result)
    (if no-result-limit
        (progn
          (setq result list)
          (dotimes (_ index)
            (pop result)))
      (dotimes (_ netease-cloud-music-search-limit)
        (setq result (append result (list (nth index list))))
        (setq index (1+ index))))
    result))

(defun netease-cloud-music--current-song (&optional song-info)
  "Get the current song at point.
If SONG-INFO is non-nil, get its song info."
  (let ((song (cond ((stringp song-info)
                     song-info)
                    (song-info nil)
                    (t (substring (thing-at-point 'line) 0 -1))))
        (index 0)
        name artist)
    (if (or (null song-info)
            (stringp song-info))
        (ignore-errors
          (string-match "\\(.*\\) - \\(.*\\)" song)
          (setq name (match-string 1 song)
                artist (match-string 2 song)))
      (setq name (car song-info)
            artist (nth 1 song-info)))
    (when (and name artist)
      (catch 'song-list
        (dolist (song-info (if netease-cloud-music-use-local-playlist
                               netease-cloud-music-playlist
                             netease-cloud-music-playlists-songs))
          (when (and (string= name (nth 1 song-info))
                     (string= artist (nth 3 song-info)))
            (throw 'song-list index))
          (setq index (1+ index)))))))

(defun netease-cloud-music--append (ele)
  "Append ELE to `netease-cloud-music-playlist'.
ELE is a list."
  (dolist (item ele)
    (add-to-list 'netease-cloud-music-playlist item t 'equal)))

(defun netease-cloud-music--api-downloaded ()
  "Check if the third-party API has been downloaded."
  (and (file-exists-p netease-cloud-music-api-dir)
       (> (length (directory-files netease-cloud-music-api-dir)) 2)))

(defun netease-cloud-music-api-process-live-p ()
  "Check if the third-party API process is live.

When using remote API, we just assume it is live."
  (if (eq netease-cloud-music-api-type 'remote) t
    (and netease-cloud-music-api-process
	 (process-live-p netease-cloud-music-api-process))))

(provide 'netease-cloud-music)

;;; netease-cloud-music.el ends here
