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

(defcustom netease-cloud-music-cache-directory
  (expand-file-name (locate-user-emacs-file "netease-cloud-music/"))
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

(defvar netease-cloud-music-buffer-name "*Netease-Cloud-Music*"
  "The name of Netease Music buffer.")

(defvar netease-cloud-music-lyric-timer nil
  "The timer of Netease Music lyric.")

(defvar netease-cloud-music-lyric nil
  "The Netease Music lyric.")

(defvar netease-cloud-music-lyrics nil
  "The lyrics that will not be changed when playing.")

(defvar netease-cloud-music-current-lyric nil
  "Current lyric.")

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
    (define-key map "f" 'netease-cloud-music-search-song)
    (define-key map "d" 'netease-cloud-music-delete-song-from-playlist)
    (define-key map "P" 'netease-cloud-music-playlist-play)
    (define-key map "p" 'netease-cloud-music-play-previous-song)
    (define-key map "n" 'netease-cloud-music-play-next-song)
    (define-key map "x" 'netease-cloud-music-kill-current-song)
    (define-key map ">" 'netease-cloud-music-seek-forward)
    (define-key map "<" 'netease-cloud-music-seek-backward)
    (define-key map "r" 'netease-cloud-music-change-repeat-mode)
    (define-key map "k" 'netease-cloud-music-clear-playlist)
    (define-key map "?" 'describe-mode)
    (define-key map "h" 'describe-mode)
    map)
  "Netease Music mode map.")

(defvar netease-cloud-music-switch-mode-map
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
  "The Netease Cloud Music Switch mode map.")

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
        (netease-cloud-music-play
         (car-safe song-list)
         song artist "song")))))

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
  (let* ((search-content (car netease-cloud-music-search-alist))
         (limit (* netease-cloud-music-search-limit page))
         (search-results
          (netease-cloud-music--catch-songs
           limit
           (netease-cloud-music-read-json
            (netease-cloud-music-request-from-api
             search-content nil limit)
            t t t t limit))))
    (netease-cloud-music-search-song--open-switch
     search-results)
    (setq netease-cloud-music-search-page page
          netease-cloud-music-search-alist
          (cons search-content search-results))))

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
  (netease-cloud-music-switch-close))

(defun netease-cloud-music-switch-add-to-playlist ()
  "Add the songs in current page to playlist."
  (interactive)
  (setq netease-cloud-music-playlist
        (append netease-cloud-music-playlist (cdr netease-cloud-music-search-alist)))
  (netease-cloud-music-switch-close)
  (netease-cloud-music-interface-init))

(defun netease-cloud-music-switch-add-page (page)
  "Add the pages to playlist."
  (interactive
   (list
    (read-string "Enter the page[n-n]: "
                 (concat (number-to-string netease-cloud-music-search-page)
                         "-"))))
  (setq netease-cloud-music-playlist
        (append netease-cloud-music-playlist
                (netease-cloud-music--songs-by-page page)))
  (netease-cloud-music-switch-close)
  (netease-cloud-music-interface-init))

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

(defun netease-cloud-music-play-song-at-point ()
  "Play the song at point."
  (interactive)
  (let ((song (netease-cloud-music--current-song)))
    (when song
      (netease-cloud-music-play
       (car song)
       (nth 1 song)
       (nth 3 song)
       "song"))))

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
    (when netease-cloud-music-lyric-timer
      (netease-cloud-music-cancel-timer netease-cloud-music-lyric-timer))
    (setq netease-cloud-music-process nil
          netease-cloud-music-current-song nil
          netease-cloud-music-play-status "")
    (when (get-buffer " *netease-cloud-music-play:process*")
      (kill-buffer " *netease-cloud-music-play:process*"))))

(defun netease-cloud-music-close ()
  "Close Netease Music and kill the process."
  (interactive)
  (netease-cloud-music-kill-process)
  (setq netease-cloud-music-process-status ""
        netease-cloud-music-repeat-mode "")
  (setq netease-cloud-music-playlist-song-index 0
        netease-cloud-music-search-page nil)
  (netease-cloud-music-cancel-timer nil t)
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
          (netease-cloud-music-read-json
           (netease-cloud-music-request-from-api search-content nil
                                                 netease-cloud-music-search-limit)
           t t t t netease-cloud-music-search-limit)))
    (setq netease-cloud-music-search-alist
          (cons search-content search-result)
          netease-cloud-music-search-page 1)
    (netease-cloud-music-search-song--open-switch search-result)))

(defun netease-cloud-music-search-song--open-switch (songs-info)
  "Enter the `netease-cloud-music-switch-mode' to switch song from searched."
  (unless (derived-mode-p 'netease-cloud-music-switch-mode)
    (netease-cloud-music-open-switch "Songs"))
  (with-current-buffer "*Netease-Cloud-Music:Switch->Songs*"
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

(defun netease-cloud-music-interface-init (&optional content type)
  "Initialize the Netease Music buffer interface.
CONTENT is a cons, its value is variable with TYPE.

TYPE is a symbol, its value can be song or song-ask.

When TYPE is song-ask, CONTENT can be:

'(music-name . artist-name)

If CONTENT is nil and TYPE is not song, it will print the init content."
  (interactive)
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
    ;; (let ((playlist
    ;;        (netease-cloud-music-get-playlist 'list)))
    ;;   (when playlist
    ;;     (insert (propertize "\nPlaylist:\n"
    ;;                         'face '(:height 1.05 :foreground "gold2")))
    ;;     (dolist (song playlist)
    ;;       (insert (propertize
    ;;                (format "%s\n" song)
    ;;                'face '(:foreground "honeydew4"))))))
    (setq buffer-read-only t)
    (goto-char (point-min))
    (forward-line 3)))

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
      (user-error "[Netease-Cloud-Music]: There's no song-id!")
    (netease-cloud-music-kill-process)
    (setq netease-cloud-music-process
          (start-process "netease-cloud-music-play:process"
                         " *netease-cloud-music-play:process*"
                         (car netease-cloud-music-player-command)
                         (concat netease-cloud-music-song-link
                                 (number-to-string song-id))
                         (when (string= (car netease-cloud-music-player-command)
                                        "mpv")
                           "--input-ipc-server=/tmp/mpvserver")))
    (set-process-sentinel netease-cloud-music-process
                          'netease-cloud-music-process-sentinel)
    (if netease-cloud-music-show-lyric
        (progn
          (setq netease-cloud-music-lyric (netease-cloud-music-get-lyric song-id))
          (with-current-buffer netease-cloud-music-buffer-name
            (netease-cloud-music-add-header-lyrics)))
      (setq netease-cloud-music-lyric nil))
    (if (and netease-cloud-music-lyric netease-cloud-music-show-lyric)
        (setq netease-cloud-music-lyric (split-string netease-cloud-music-lyric "\n")
              netease-cloud-music-lyrics netease-cloud-music-lyric
              netease-cloud-music-lyric-timer
              (run-with-timer
               0.5 1
               (lambda ()
                 (if (and (get-buffer " *netease-cloud-music-play:process*")
                          (length netease-cloud-music-lyric))
                     (with-current-buffer " *netease-cloud-music-play:process*"
                       (goto-char (point-max))
                       (when (and (search-backward "[K[0mA" nil t)
                                  (not (string= netease-cloud-music-play-status
                                                "paused")))
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
                                   (ignore-errors
                                     (match-string
                                      (if (string-match "\\[\\(.*\\):\\(.*\\)\\.\\(.*\\)\\]\\(.*\\)" current-lyric)
                                          4
                                        (when (string-match
                                               "\\[\\(.*\\):\\(.*\\)\\]\\(.*\\)"
                                               current-lyric)
                                          3))
                                      current-lyric)))
                             (setq netease-cloud-music-lyric (cdr netease-cloud-music-lyric))))))
                   (netease-cloud-cancel-timer netease-cloud-music-lyric-timer))))))
    (setq netease-cloud-music-process-status "playing")
    (setq netease-cloud-music-current-song
          `(,song-name ,artist-name ,song-id))
    (setq netease-cloud-music-play-status play-type)
    (netease-cloud-music-interface-init)))

(defun netease-cloud-music-playlist-play ()
  "Play the playlist songs."
  (interactive)
  (when (string= netease-cloud-music-repeat-mode "")
    (setq netease-cloud-music-repeat-mode "playlist"))
  (let ((current-song (netease-cloud-music--current-song))
        song)
    (when current-song
      (setq netease-cloud-music-playlist-song-index current-song))
    (setq song
          (nth netease-cloud-music-playlist-song-index netease-cloud-music-playlist))
    (netease-cloud-music-play (car song)
                              (nth 1 song)
                              (nth 3 song)
                              "playlist")))

(defun netease-cloud-music-process-sentinel (process event)
  "The sentinel of Netease Music process."
  (when (string-match "\\(finished\\|Exiting\\)" event)
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
           (netease-cloud-music-interface-init))
          ((string= netease-cloud-music-play-status "playlist")
           (if (string= netease-cloud-music-repeat-mode "playlist")
               (netease-cloud-music-play-next-song)
             (if (string= netease-cloud-music-repeat-mode "off")
                 (netease-cloud-music-kill-current-song)
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
              (user-error "[Netease-Cloud-Music]: There's no song previous.")
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
  (if (not (netease-cloud-music-process-live-p))
      (user-error "[Netease-Cloud-Music]: There's no song playing.")
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
                           (nth 2 netease-cloud-music-player-command)))))

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
    (when netease-cloud-music-show-lyric
      (setq netease-cloud-music-lyric netease-cloud-music-lyrics))))

(defun netease-cloud-music-delete-song-from-playlist ()
  "Delete current song from playlist."
  (interactive)
  (let ((song (netease-cloud-music--current-song))
        (current-line (line-number-at-pos)))
    (when song
      (setq netease-cloud-music-playlist
            (delete (nth song netease-cloud-music-playlist)
                    netease-cloud-music-playlist))
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

(defun netease-cloud-music-cancel-timer (&optional timer delete-lyrics)
  "Cancel timer and kill the lyrics."
  (when timer
    (cancel-timer timer))
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
          netease-cloud-music-current-lyric))

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

;;; For old user
(defun netease-cloud-music-insert-playlist (file-path)
  "Convert the playlist file into playlist type and insert it."
  (interactive "fEnter the file path: ")
  (when (file-exists-p file-path)
    (let ((file-contents
           (with-temp-buffer
             (insert-file-contents file-path)
             (split-string (buffer-string)
                           "\n" t)))
          result)
      (when file-contents
        (mapc #'(lambda (s)
                  (setq result (append result
                                       (list (netease-cloud-music-read-json
                                              (netease-cloud-music-request-from-api
                                               s)
                                              t t t t)))))
              file-contents)
        (insert "'" (format "%S" result))))))

(provide 'netease-cloud-music)
