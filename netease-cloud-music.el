;;; netease-cloud-music.el --- Netease Cloud Music client for Emacs -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 1.5
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

(defcustom netease-cloud-music-playlist-file
  (expand-file-name (locate-user-emacs-file "ncm-playlist"))
  "The cache directory of Netease Music."
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

(defvar netease-cloud-music-buffer-name "*Netease-Cloud-Music*"
  "The name of Netease Music buffer.")

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

(defvar netease-cloud-music-repeat-mode ""
  "The repeat mode for Netease Cloud Music.")

(defvar netease-cloud-music-search-limit 10
  "The search limit for Netease Cloud Music.")

(defvar netease-cloud-music-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'netease-cloud-music-interface-init)
    (define-key map "q" 'netease-cloud-music-close)
    (define-key map (kbd "SPC") 'netease-cloud-music-pause-or-continue)
    (define-key map (kbd "RET") 'netease-cloud-music-play-song-at-point)
    (define-key map "s" 'netease-cloud-music-save-playlist)
    (define-key map "u" 'netease-cloud-music-get-playlist-by-uid)
    (define-key map "f" 'netease-cloud-music-search-song)
    (define-key map "F" 'netease-cloud-music-search-playlist)
    (define-key map "d" 'netease-cloud-music-delete-song-from-playlist)
    (define-key map "P" 'netease-cloud-music-playlist-play)
    (define-key map "p" 'netease-cloud-music-play-previous-song)
    (define-key map "n" 'netease-cloud-music-play-next-song)
    (define-key map "x" 'netease-cloud-music-kill-current-song)
    (define-key map ">" 'netease-cloud-music-seek-forward)
    (define-key map "<" 'netease-cloud-music-seek-backward)
    (define-key map "r" 'netease-cloud-music-change-repeat-mode)
    (define-key map "k" 'netease-cloud-music-clear-playlist)
    (define-key map "R" 'netease-cloud-music-change-order)
    (define-key map "w" 'netease-cloud-music-write-mode)
    (define-key map "/" 'netease-cloud-music-ask-play)
    (define-key map (kbd "<down>") 'netease-cloud-music-move-down)
    (define-key map (kbd "<up>") 'netease-cloud-music-move-up)
    (define-key map "?" 'describe-mode)
    (define-key map "h" 'describe-mode)
    map)
  "Netease Music mode map.")

(defcustom netease-cloud-music-switch-song-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'netease-cloud-music-switch-close)
    (define-key map "n" 'next-line)
    (define-key map "p" 'netease-cloud-music-switch-previous)
    (define-key map "f" 'netease-cloud-music-switch-next-page)
    (define-key map "b" 'netease-cloud-music-switch-prev-page)
    (define-key map (kbd "RET") 'netease-cloud-music-switch-enter)
    (define-key map "P" 'netease-cloud-music-switch-play-page)
    (define-key map "a" 'netease-cloud-music-switch-add-to-playlist)
    (define-key map "A" 'netease-cloud-music-switch-add-page)
    map)
  "The Netease Cloud Music Songs Switch mode map."
  :type 'keymap
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-switch-playlist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'netease-cloud-music-switch-close)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "a" 'netease-cloud-music-playlist-add-all)
    (define-key map "f" 'netease-cloud-music-switch-next-page)
    (define-key map "b" 'netease-cloud-music-switch-prev-page)
    (define-key map (kbd "RET") 'netease-cloud-music-playlist-enter)
    map)
  "The Playlist switch mode map."
  :type 'keymap
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-write-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'netease-cloud-music-write-finish)
    (define-key map (kbd "C-c C-k") 'netease-cloud-music-write-cancel)
    map)
  "The map for write mode."
  :type 'keymap
  :group 'netease-cloud-music)

;;;###autoload
(define-derived-mode netease-cloud-music-mode nil "Netease-Cloud-Music"
  "The mode of Netease Music mode."
  :group 'netease-cloud-music
  :abbrev-table nil
  :syntax-table nil
  (linum-mode -1)
  (setq buffer-read-only t
        truncate-lines t))

;;;###autoload
(define-derived-mode netease-cloud-music-switch-mode netease-cloud-music-mode "Netease-Cloud-Music:Switch"
  "The child mode for `netease-cloud-music-mode' to do switch action."
  :group 'netease-cloud-music
  :abbrev-table nil
  :syntax-table nil)

(defun netease-cloud-music-open-switch (buffer-name)
  "Open the switch buffer."
  (split-window nil nil 'above)
  (switch-to-buffer (format "*Netease-Cloud-Music:Switch->%s*"
                            buffer-name))
  (netease-cloud-music-switch-mode))

(defun netease-cloud-music-switch-close ()
  "Close the switch buffer."
  (interactive)
  (kill-buffer-and-window)) ; NOTE: Maybe will add new features.

(defun netease-cloud-music-switch-previous ()
  "Previous line or play all the songs current page."
  (interactive)
  (if (= 1 (line-number-at-pos))
      (netease-cloud-music-switch-play-all)
    (forward-line -1)))

(defun netease-cloud-music-switch-enter ()
  "The enter action in `netease-cloud-music-switch-mode'."
  (interactive)
  (with-current-buffer (current-buffer)
    (let* ((content (substring-no-properties (thing-at-point 'line) 0 -1))
           (song-info (progn
                        (string-match "^<<\\(.*\\)>> - \\(.*\\)" content)
                        (cons (match-string 1 content) (match-string 2 content))))
           (song (car-safe song-info))
           (artist (cdr-safe song-info))
           song-list)
      (if (not (and song artist))
          (user-error "[Netease-Cloud-Music]: The song info of the song under cursor is error!")
        (setq song-list (netease-cloud-music--get-song-list song artist))
        (netease-cloud-music-switch-close)
        (add-to-list 'netease-cloud-music-playlist song-list)
        (when (string= netease-cloud-music-repeat-mode "")
          (setq netease-cloud-music-repeat-mode "song"))
        (netease-cloud-music-play
         (car-safe song-list)
         song artist)
        (netease-cloud-music-save-playlist)))))

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
           (mode (if (eq major-mode 'netease-cloud-music-switch-song-mode)
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
      (if (eq (car mode) 'playlist)
          (netease-cloud-music-playlist--open-switch search-results)
        (netease-cloud-music-search-song--open-switch
         search-results))
      (setq netease-cloud-music-search-page page
            netease-cloud-music-search-alist
            (cons search-content search-results)))))

(defun netease-cloud-music-switch-play-all ()
  "Play all the songs in the search list."
  (interactive)
  (when netease-cloud-music-search-alist
    (setq netease-cloud-music-playlist (cdr netease-cloud-music-search-alist))
    (cond ((/= (line-number-at-pos) 1)
           (setq netease-cloud-music-playlist-song-index (1- (line-number-at-pos))))
          (t
           (setq netease-cloud-music-playlist-song-index 0)))
    (netease-cloud-music-playlist-play)
    (netease-cloud-music-switch-close)))

(defun netease-cloud-music-switch-play-page (page)
  "Play songs by page."
  (interactive (list (read-string "Enter the page[n-n]: " "1-")))
  (setq netease-cloud-music-playlist (netease-cloud-music--songs-by-page page))
  (unless (= 0 netease-cloud-music-playlist-song-index)
    (setq netease-cloud-music-playlist-song-index 0))
  (netease-cloud-music-playlist-play)
  (netease-cloud-music-save-playlist)
  (netease-cloud-music-switch-close))

(defun netease-cloud-music-switch-add-to-playlist ()
  "Add the songs in current page to playlist."
  (interactive)
  (netease-cloud-music--append (cdr netease-cloud-music-search-alist))
  (netease-cloud-music-save-playlist)
  (netease-cloud-music-switch-close)
  (netease-cloud-music-interface-init))

(defun netease-cloud-music-switch-add-page (page)
  "Add the pages to playlist."
  (interactive
   (list
    (read-string "Enter the page[n-n]: "
                 (concat (number-to-string netease-cloud-music-search-page)
                         "-"))))
  (netease-cloud-music--append (netease-cloud-music--songs-by-page page))
  (netease-cloud-music-save-playlist)
  (netease-cloud-music-switch-close)
  (netease-cloud-music-interface-init))

;;;###autoload
(define-derived-mode netease-cloud-music-write-mode text-mode "Netease-Cloud-Music:Write"
  "The write mode for `netease-cloud-music'."
  :group 'netease-cloud-music
  :syntax-table nil
  :abbrev-table nil
  (netease-cloud-music-interface-init)
  (setq buffer-read-only nil)
  (goto-char (point-min))
  (forward-line 7)
  (narrow-to-region (point) (point-max))
  (when netease-cloud-music-show-lyric
    (with-current-buffer netease-cloud-music-buffer-name
      (netease-cloud-music-add-header-lyrics))))

(defun netease-cloud-music-write-cancel ()
  "Cancel and turn to normal mode."
  (interactive)
  (netease-cloud-music-mode)
  (netease-cloud-music-interface-init)
  (when netease-cloud-music-show-lyric
    (with-current-buffer netease-cloud-music-buffer-name
      (netease-cloud-music-add-header-lyrics))))

(defun netease-cloud-music-write-finish ()
  "Modify by the modified contents and turn to normal mode."
  (interactive)
  (let (current-song-index song-name copy)
    (with-current-buffer netease-cloud-music-buffer-name
      (goto-char (point-min))
      (while (not (eobp))
        (setq current-song-index (netease-cloud-music--current-song))
        (when current-song-index
          (setq copy
                (append copy (list (nth current-song-index netease-cloud-music-playlist)))))
        (forward-line))
      (if (not (eobp))
          (progn
            (netease-cloud-music-write-cancel)
            (user-error "[Netease-Cloud-Music]: There's an error when save the result!"))
        (setq netease-cloud-music-playlist copy)
        (netease-cloud-music-write-cancel)
        (netease-cloud-music-save-playlist)
        (netease-cloud-music-adjust-song-index)))))

;;;###autoload
(defun netease-cloud-music ()
  "Initialize the Netease Music buffer in netease-cloud-music-mode."
  (interactive)
  (if (get-buffer netease-cloud-music-buffer-name)
      (switch-to-buffer netease-cloud-music-buffer-name)
    (unless (buffer-live-p (get-buffer netease-cloud-music-buffer-name))
      (switch-to-buffer netease-cloud-music-buffer-name))
    (netease-cloud-music-mode)
    (netease-cloud-music-get-playlist)
    (netease-cloud-music-interface-init)))

(defun netease-cloud-music-play-song-at-point ()
  "Play the song at point."
  (interactive)
  (let ((song (nth (netease-cloud-music--current-song)
                   netease-cloud-music-playlist)))
    (when song
      (when (string= netease-cloud-music-repeat-mode "")
        (setq netease-cloud-music-repeat-mode "song"))
      (netease-cloud-music-play
       (car song)
       (nth 1 song)
       (nth 3 song)))))

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

(defun netease-cloud-music-close ()
  "Close Netease Music and kill the process."
  (interactive)
  (netease-cloud-music-kill-process)
  (setq netease-cloud-music-process-status "")
  (setq netease-cloud-music-search-page nil
        netease-cloud-music-lyric nil
        netease-cloud-music-lyrics nil
        netease-cloud-music-update-lyric-timer nil
        netease-cloud-music-lyric-song nil)
  (netease-cloud-music-cancel-timer t)
  (netease-cloud-music-save-playlist)
  (kill-buffer netease-cloud-music-buffer-name))

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

(defun netease-cloud-music-search-song--open-switch (songs-info)
  "Enter the `netease-cloud-music-switch-mode' to switch song from searched."
  (unless (eq major-mode 'netease-cloud-music-switch-song-mode)
    (netease-cloud-music-open-switch "Songs"))
  (with-current-buffer "*Netease-Cloud-Music:Switch->Songs*"
    (use-local-map netease-cloud-music-switch-song-mode-map)
    (setq-local major-mode 'netease-cloud-music-switch-song-mode)
    (setq-local buffer-read-only nil)
    (erase-buffer)
    (let ((prefix (propertize "<<" 'face 'font-lock-comment-face))
          (end (propertize ">>" 'face 'font-lock-comment-face)))
      (mapc #'(lambda (s)
                (insert prefix
                        (propertize (nth 1 s) 'face 'font-lock-keyword-face)
                        end " - "
                        (propertize (nth 3 s) 'face 'font-lock-function-name-face)
                        "\n"))
            (if (listp (car-safe songs-info))
                songs-info
              (list songs-info))))
    (setq-local buffer-read-only t)
    (goto-char (point-min))))

(defun netease-cloud-music-change-repeat-mode ()
  "Change the repeat mode."
  (interactive)
  (if (string= netease-cloud-music-repeat-mode "")
      (message
       "[Netease-Cloud-Music]: The repeat mode is in the initialization state. when you start playing song, it'll be set.")
    (setq netease-cloud-music-repeat-mode
          (pcase netease-cloud-music-repeat-mode
            ("off" "song")
            ("song" "playlist")
            ("playlist" "random")
            ("random" "off")))
    (netease-cloud-music-interface-init)))

(defun netease-cloud-music-interface-init (&optional content type)
  "Initialize the Netease Music buffer interface.
CONTENT is a cons, its value is variable with TYPE.

TYPE is a symbol, its value can be song or song-ask.

When TYPE is song-ask, CONTENT can be:

'(music-name . artist-name)

If CONTENT is nil and TYPE is not song, it will print the init content."
  (interactive)
  (with-current-buffer netease-cloud-music-buffer-name
    (unless (eq major-mode 'netease-cloud-music-write-mode)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (propertize
               "Netease Cloud Music - 网易云音乐\n"
               'face '(:height 1.1 :foreground "Red3")))
      ;; Show the repeat mode status
      (insert (concat
               (propertize "\nRepeat: "
                           'face '(:foreground "DeepSkyBlue"))
               (propertize (concat (upcase netease-cloud-music-repeat-mode) "\n")
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
      (when netease-cloud-music-playlist
        (insert (propertize "\nPlaylist:\n"
                            'face '(:height 1.05 :foreground "gold2")))
        (mapc #'(lambda (s)
                  (insert (format "%s - %s\n"
                                  (propertize
                                   (nth 1 s)
                                   'face 'font-lock-keyword-face)
                                  (propertize
                                   (nth 3 s)
                                   'face 'font-lock-function-name-face))))
              netease-cloud-music-playlist))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (forward-line 3))))

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

(defun netease-cloud-music-play (song-id song-name artist-name)
  "Play the song by its SONG-ID and update the interface with SONG-NAME"
  (if (null song-id)
      (user-error "[Netease-Cloud-Music]: There's no song-id!")
    (netease-cloud-music-kill-process)
    (setq netease-cloud-music-process
          (start-process "netease-cloud-music-play:process"
                         " *netease-cloud-music-play:process*"
                         (car netease-cloud-music-player-command)
                         (concat netease-cloud-music-song-link
                                 (number-to-string song-id))
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
              (user-error "[Netease-Cloud-Music]: There's no song previous.")
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
                (message "[Netease-Cloud-Music]: The playlist songs over."))
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
      (user-error "[Netease-Cloud-Music]: The song is not exists!")
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

(defun netease-cloud-music-move-down ()
  "Move the current song down."
  (interactive)
  (let ((current (netease-cloud-music--current-song))
        (current-line (line-number-at-pos)))
    (when current
      (netease-cloud-music-change-order current (+ current 2))
      (goto-char (point-min))
      (forward-line current-line))))

(defun netease-cloud-music-move-up ()
  "Move the current song up."
  (interactive)
  (let ((current (netease-cloud-music--current-song))
        (current-line (line-number-at-pos)))
    (when current
      (netease-cloud-music-change-order current current)
      (goto-char (point-min))
      (forward-line (- current-line 2)))))

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
      (user-error "[Netease-Cloud-Music]: The name of playlist can not be null!")
    (let ((playlists
           (netease-cloud-music-get-playlists
            (netease-cloud-music-request-from-api
             name 'playlist netease-cloud-music-search-limit))))
      (setq netease-cloud-music-search-alist
            (cons name playlists)
            netease-cloud-music-search-page 1)
      (netease-cloud-music-playlist--open-switch playlists))))

(defun netease-cloud-music-playlist--open-switch (playlists)
  "Open switch buffer for playlist."
  (unless (eq major-mode 'netease-cloud-music-switch-playlist-mode)
    (netease-cloud-music-open-switch "Playlist"))
  (with-current-buffer "*Netease-Cloud-Music:Switch->Playlist*"
    (use-local-map netease-cloud-music-switch-playlist-mode-map)
    (setq-local major-mode 'netease-cloud-music-switch-playlist-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (dolist (playlist playlists)
      (insert (format "%s\n" (car playlist))))
    (setq buffer-read-only t)
    (goto-char (point-min))))

(defun netease-cloud-music-get-playlist-by-uid (uid)
  "Fetch playlist by UID."
  (interactive "sEnter the uid: ")
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
        (user-error "[Netease-Cloud-Music]: The playlist can not found!")
      (netease-cloud-music--append (netease-cloud-music-get-playlist-songs playlist))
      (netease-cloud-music-save-playlist)
      (netease-cloud-music-switch-close)
      (netease-cloud-music-interface-init))))

(defun netease-cloud-music-playlist-add-all ()
  "Add all the searched playlists to the playlist"
  (interactive)
  (if (null netease-cloud-music-search-playlists)
      (user-error "[Netease-Cloud-Music]: The playlist can not found!")
    (dolist (playlist netease-cloud-music-search-playlists)
      (netease-cloud-music--append
       (netease-cloud-music-get-playlist-songs (cdr playlist))))
    (netease-cloud-music-save-playlist)
    (netease-cloud-music-switch-close)
    (netease-cloud-music-interface-init)))

(defun netease-cloud-music-get-playlist-songs (pid)
  "Get the songs in the playlist whose if is PID."
  (let (songs song artist result)
    (request "https://music.163.com/api/playlist/detail"
         :type "POST"
         :data `(("id" . ,pid))
         :parser 'json-read
         :success (netease-cloud-music-expand-form (setq songs data))
         :sync t)
    (if (/= 200 (alist-get 'code songs))
        (user-error "[Netease-Cloud-Music]: The pid can not fount!")
      (setq songs (alist-get 'tracks (alist-get 'result songs)))
      ;; (print (alist-get 'artists (aref songs 0)))
      (dotimes (n (length songs))
        (setq song (aref songs n)
              artist (aref (alist-get 'artists song) 0))
        (add-to-list 'result
                     (list (alist-get 'id song)
                           (alist-get 'name song)
                           (alist-get 'id artist)
                           (alist-get 'name artist))
                     t))
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
        (user-error "[Netease-Cloud-Music]: The uid can not found!")
      (setq playlist-json (alist-get 'playlist playlist-json))
      (dotimes (n (length playlist-json))
        (setq playlist (aref playlist-json n))
        (add-to-list 'result
                     (cons (alist-get 'name playlist)
                           (alist-get 'id playlist))
                     t))
      result)))

(defun netease-cloud-music-random-play ()
  "Return the random number for the playlist as an index."
  (setq netease-cloud-music-playlist-song-index
        (random (1- (length netease-cloud-music-playlist))))
  (netease-cloud-music-playlist-play))

(provide 'netease-cloud-music)
