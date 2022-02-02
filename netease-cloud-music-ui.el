;;; netease-cloud-music-ui.el --- Netease Cloud Music client for Emacs -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 2.1

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

;;; This file is the TUI for client.

(require 'netease-cloud-music)

(defcustom netease-cloud-music-jump-function
  'netease-cloud-music-jump-read-line
  "FUnction for jumpping."
  :type 'symbol
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-line-number-relative nil
  "If the line number is relative."
  :type 'boolean
  :group 'netease-cloud-music)

(defface netease-cloud-music-head-title-face
  '((t :height 1.1 :foreground "Red3"))
  "The head title face."
  :group 'netease-cloud-music)

(defface netease-cloud-music-user-name-face
  '((t :inherit font-lock-constant-face :weight bold))
  "The username face."
  :group 'netease-cloud-music)

(defface netease-cloud-music-repeat-face
  '((t :foreground "DeepSkyBlue"))
  "The repeat title face."
  :group 'netease-cloud-music)

(defface netease-cloud-music-repeat-mode-face
  '((t :foreground "LightPink" :weight bold))
  "The repeat mode face."
  :group 'netease-cloud-music)

(defface netease-cloud-music-current-song-title-face
  '((t :height 0.9 :weight bold))
  "The current song title face."
  :group 'netease-cloud-music)

(defface netease-cloud-music-playing-song-face
  '((t :height 1.0 :foreground "MediumSpringGreen"))
  "The playing song face."
  :group 'netease-cloud-music)

(defface netease-cloud-music-play-status-face
  '((t :foreground "OrangeRed"))
  "The play status face."
  :group 'netease-cloud-music)

(defface netease-cloud-music-playlists-face
  '((t :height 1.05 :foreground "gold2"))
  "The playlists face."
  :group 'netease-cloud-music)

(defface netease-cloud-music-playlist-face
  '((t :inherit font-lock-warning-face :weight bold))
  "The playlist face."
  :group 'netease-cloud-music)

(defvar netease-cloud-music-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'netease-cloud-music-goto)
    (define-key map "Q" 'netease-cloud-music-close)
    (define-key map "q" 'netease-cloud-music-back)
    (define-key map (kbd "SPC") 'netease-cloud-music-pause-or-continue)
    (define-key map (kbd "RET") 'netease-cloud-music-play-song-at-point)
    (define-key map (kbd "TAB") 'netease-cloud-music-toggle-playlist-songs)
    (define-key map "c" 'netease-cloud-music-change-lyric-type)
    (define-key map "M" 'netease-cloud-music-delete-playlist)
    (define-key map "m" 'netease-cloud-music-change-playlist-name)
    (define-key map "C" 'netease-cloud-music-create-playlist)
    (define-key map "s" 'netease-cloud-music-save-playlist)
    (define-key map "u" 'netease-cloud-music-get-playlist-by-uid)
    (define-key map "f" 'netease-cloud-music-search-song)
    (define-key map "F" 'netease-cloud-music-search-playlist)
    (define-key map "d" 'netease-cloud-music-delete-song-from-playlist)
    (define-key map "D" 'netease-cloud-music-delete-playing-song)
    (define-key map "P" 'netease-cloud-music-playlist-play)
    (define-key map "p" 'netease-cloud-music-play-previous-song)
    (define-key map "n" 'netease-cloud-music-play-next-song)
    (define-key map "N" 'netease-cloud-music-random-play)
    (define-key map "x" 'netease-cloud-music-kill-current-song)
    (define-key map ">" 'netease-cloud-music-seek-forward)
    (define-key map "<" 'netease-cloud-music-seek-backward)
    (define-key map "r" 'netease-cloud-music-change-repeat-mode)
    (define-key map "k" 'netease-cloud-music-clear-playlist)
    (define-key map "R" 'netease-cloud-music-change-order)
    (define-key map "w" 'netease-cloud-music-write-mode)
    (define-key map "l" 'netease-cloud-music-login)
    (define-key map "e" 'netease-cloud-music-get-recommend-songs)
    (define-key map "E" 'netease-cloud-music-get-recommend-playlists)
    (define-key map "j" 'netease-cloud-music-storage-song)
    (define-key map "J" 'netease-cloud-music-add-storage-to-current-playlist)
    (define-key map "o" 'netease-cloud-music-show-storage)
    (define-key map "K" 'netease-cloud-music-clear-storage)
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
    (define-key map "d" 'netease-cloud-music-delete-song-from-storage)
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
    (define-key map (kbd "TAB") 'netease-cloud-music-playlist-tab)
    map)
  "The Playlist switch mode map."
  :type 'keymap
  :group 'netease-cloud-music)

(defun netease-cloud-music-switch-previous ()
  "Previous line or play all the songs current page."
  (interactive)
  (if (= 1 (line-number-at-pos))
      (netease-cloud-music-switch-play-all)
    (forward-line -1)))

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
  "Open the switch buffer by its BUFFER-NAME."
  (split-window nil nil 'above)
  (switch-to-buffer (format "*Netease-Cloud-Music:Switch->%s*"
                            buffer-name))
  (netease-cloud-music-switch-mode))

(defun netease-cloud-music-switch-close ()
  "Close the switch buffer."
  (interactive)
  (kill-buffer-and-window)) ; NOTE: Maybe will add new features.

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
  "Play songs by PAGE."
  (interactive (list (read-string "Enter the page[n-n]: " "1-")))
  (when (yes-or-no-p "If you really want to do this? It'll replace all your songs!")
    (if netease-cloud-music-use-local-playlist
        (setq netease-cloud-music-playlist (netease-cloud-music--songs-by-page page))
      (let (ids)
        (dolist (song (netease-cloud-music--songs-by-page page))
          (setq ids (append ids (list (car song)))))
        (netease-cloud-music-update-songs-order-in-playlist
         netease-cloud-music-playlist-id ids)))
    (unless (= 0 netease-cloud-music-playlist-song-index)
      (setq netease-cloud-music-playlist-song-index 0))
    (netease-cloud-music-playlist-play)
    (netease-cloud-music-save-playlist)
    (netease-cloud-music-switch-close)))

(defun netease-cloud-music-play-song-at-point ()
  "Play the song at point."
  (interactive)
  (when
      (catch 'stop                      ;When the user only changed the playing mode, don't play any song.
        (netease-cloud-music-change-playlist-mode)
        (let ((song (nth (netease-cloud-music--current-song)
                         (if netease-cloud-music-use-local-playlist
                             netease-cloud-music-playlist
                           netease-cloud-music-playlists-songs))))
          (when song
            (when (string= netease-cloud-music-repeat-mode "")
              (setq netease-cloud-music-repeat-mode "song"))
            (netease-cloud-music-play
             (car song)
             (nth 1 song)
             (nth 3 song)))))
    (netease-cloud-music-interface-init)))

;;;###autoload
(defun netease-cloud-music ()
  "Initialize the Netease Music buffer in `netease-cloud-music-mode'."
  (interactive)
  (setq netease-cloud-music-last-buffer (current-buffer)) ;Record the buffer which called this function
  
  (if (get-buffer netease-cloud-music-buffer-name)
      (switch-to-buffer netease-cloud-music-buffer-name)
    (unless (buffer-live-p (get-buffer netease-cloud-music-buffer-name))
      (switch-to-buffer netease-cloud-music-buffer-name))
    (netease-cloud-music-mode)
    (netease-cloud-music-get-playlist)
    (netease-cloud-music-interface-init)

    (unless (file-exists-p netease-cloud-music-cache-directory)
      (make-directory netease-cloud-music-cache-directory))
    (when (and (or (not (netease-cloud-music--api-need-downloaded))
                   (netease-cloud-music--api-downloaded))
               (not (netease-cloud-music-api-process-live-p)))
      (netease-cloud-music-start-api)   ;Start third-party API

      (when (file-exists-p netease-cloud-music-user-loginfo-file)
        (let ((loginfo (netease-cloud-music-get-loginfo)))
          (when loginfo
            (setq netease-cloud-music-phone (car loginfo)
                  netease-cloud-music-user-password (cdr loginfo)
                  netease-cloud-music-login-timer
                  (run-with-timer
                   1 2
                   (lambda ()
                     (if (and netease-cloud-music-user-id
                              netease-cloud-music-username)
                         (progn
                           (cancel-timer netease-cloud-music-login-timer)
                           (setq netease-cloud-music-login-timer nil))
                       (netease-cloud-music--get-user-info)
                       (ignore-errors
                         (setq netease-cloud-music-playlists
                               (netease-cloud-music-get-user-playlist
                                netease-cloud-music-user-id)))
                       (netease-cloud-music-interface-init)))))))))))

(defun netease-cloud-music-close ()
  "Close Netease Music and kill the process."
  (interactive)
  (netease-cloud-music-quit)
  (kill-buffer netease-cloud-music-buffer-name))

(defun netease-cloud-music-search-song--open-switch (songs-info)
  "Enter the `netease-cloud-music-switch-mode' to switch song from searched.
SONGS-INFO is the infos of the songs want to show."
  (unless (eq major-mode 'netease-cloud-music-switch-song-mode)
    (netease-cloud-music-open-switch "Songs"))
  (with-current-buffer "*Netease-Cloud-Music:Switch->Songs*"
    (use-local-map netease-cloud-music-switch-song-mode-map)
    (setq-local major-mode 'netease-cloud-music-switch-song-mode)
    (setq-local buffer-read-only nil)
    (erase-buffer)
    (let ((prefix (propertize "<<" 'face 'font-lock-comment-face))
          (end (propertize ">>" 'face 'font-lock-comment-face)))
      (mapc (lambda (s)
              (insert prefix
                      (propertize (nth 1 s) 'face 'netease-cloud-music-song-face)
                      end " - "
                      (propertize (nth 3 s) 'face 'netease-cloud-music-artist-face)
                      "\n"))
            (if (listp (car-safe songs-info))
                songs-info
              (list songs-info))))
    (setq-local buffer-read-only t)
    (goto-char (point-min))))

(defun netease-cloud-music-playlist--open-switch (playlists)
  "Open switch buffer for PLAYLISTS."
  (unless (eq major-mode 'netease-cloud-music-switch-playlist-mode)
    (netease-cloud-music-open-switch "Playlist"))
  (with-current-buffer "*Netease-Cloud-Music:Switch->Playlist*"
    (use-local-map netease-cloud-music-switch-playlist-mode-map)
    (setq-local major-mode 'netease-cloud-music-switch-playlist-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (dolist (playlist playlists)
      (insert (propertize (car playlist)
                          'face 'netease-cloud-music-playlist-face)
              "\n"))
    (setq buffer-read-only t)
    (goto-char (point-min))))

(defun netease-cloud-music-interface-init ()
  "Initialize the Netease Music buffer interface."
  (interactive)
  (with-current-buffer netease-cloud-music-buffer-name
    (unless (eq major-mode 'netease-cloud-music-write-mode)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (propertize
               "Netease Cloud Music - 网易云音乐\n"
               'face 'netease-cloud-music-head-title-face))
      (when netease-cloud-music-username ;Show user name
        (insert "你好，"
                (propertize netease-cloud-music-username
                            'face 'netease-cloud-music-user-name-face) "\n"))
      ;; Show the repeat mode status
      (insert (propertize "Repeat: "
                          'face 'netease-cloud-music-repeat-face)
              (propertize (concat (upcase netease-cloud-music-repeat-mode) "\n")
                          'face 'netease-cloud-music-repeat-mode-face))
      ;; When the type is song, insert the current song info.
      (when netease-cloud-music-current-song
        (insert "\n")
        (insert (propertize
                 "Current song: "
                 'face 'netease-cloud-music-current-song-title-face)
                (propertize (format
                             "%s - %s"
                             (car netease-cloud-music-current-song)
                             (nth 1 netease-cloud-music-current-song))
                            'face 'netease-cloud-music-playing-song-face)
                (if (string= netease-cloud-music-process-status "playing")
                    (propertize " [Playing]\n"
                                'face 'netease-cloud-music-play-status-face)
                  (propertize " [Paused]\n"
                              'face 'netease-cloud-music-play-status-face))))

      ;; User's Playlist
      (when netease-cloud-music-playlists
        (insert (propertize "\nUser's Playlists:\n"
                            'face 'netease-cloud-music-playlists-face))
        (dolist (playlist netease-cloud-music-playlists)
          (insert (propertize (car playlist)
                              'face 'netease-cloud-music-playlist-face)
                  "\n")
          (when (and netease-cloud-music-playlist-id
                     (= netease-cloud-music-playlist-id (cdr playlist)))
            (mapc (lambda (s)
                    (insert (number-to-string (car s)))
                    (overlay-put (make-overlay (line-beginning-position) (line-end-position))
                                 'display
                                 (format "%s - %s"
                                         (propertize
                                          (nth 1 s)
                                          'face 'netease-cloud-music-song-face)
                                         (propertize
                                          (if (nth 3 s) (nth 3 s) "nil")
                                          'face 'netease-cloud-music-artist-face)))
                    (insert "\n"))
                  netease-cloud-music-playlists-songs))))

      ;; Local Playlist
      (when netease-cloud-music-playlist
        (insert (propertize "\nLocal Playlist:\n"
                            'face 'netease-cloud-music-playlists-face))
        (mapc (lambda (s)
                (insert (number-to-string (car s)))
                (overlay-put (make-overlay (line-beginning-position) (line-end-position))
                             'display
                             (format "%s - %s"
                                     (propertize
                                      (nth 1 s)
                                      'face 'netease-cloud-music-song-face)
                                     (propertize
                                      (nth 3 s)
                                      'face 'netease-cloud-music-artist-face)))
                (insert "\n"))
              netease-cloud-music-playlist))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (forward-line 3))))

(defun netease-cloud-music-toggle-playlist-songs (pid)
  "Toggle the songs of playlist under cursor.
PID is the playlist's id."
  (interactive (list
                (let ((playlist-name (substring-no-properties
                                      (thing-at-point 'line) 0 -1))
                      playlist)
                  (if (null
                       (setq playlist
                             (alist-get playlist-name netease-cloud-music-playlists
                                        nil nil 'string-equal)))
                      (netease-cloud-music-error "The playlist is not exists!")
                    playlist))))
  (with-current-buffer netease-cloud-music-buffer-name
    (setq-local buffer-read-only nil)
    (save-mark-and-excursion
      (forward-line)                    ;To check the next line if it's a null string or songs' info.
      (if (not (or (equal (get-text-property (point) 'face)
                          'netease-cloud-music-playlist-face)
                   (string-empty-p (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position)))))
          (progn
            (while (overlays-at (point))
              (delete-region (line-beginning-position) (line-end-position))
              (delete-char 1)))
        (forward-line -1)
        (goto-char (line-end-position))
        (insert "\n")
        (let ((songs (netease-cloud-music-get-playlist-songs pid)))
          (if (null songs)
              (message "[Netease-Cloud-Music]: The playlist is empty.")
            (dolist (song songs)
              (insert (number-to-string (car song)))
              (overlay-put (make-overlay (line-beginning-position) (line-end-position))
                           'display
                           (format "%s - %s"
                                   (propertize
                                    (nth 1 song)
                                    'face 'netease-cloud-music-song-face)
                                   (propertize
                                    (if (nth 3 song) (nth 3 song) "nil")
                                    'face 'netease-cloud-music-artist-face)))
              (insert "\n"))))
        (delete-char -1)))
    (setq-local buffer-read-only t)))

(defun netease-cloud-music--get-current-playlist (&optional move)
  "Get the current playlist.
MOVE means do not care about the cursor's position."
  (let ((get (lambda ()
               (with-current-buffer netease-cloud-music-buffer-name
                 (while (overlays-at (point)) ;To get to the playlist's name
                   (forward-line -1))
                 (alist-get (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position))
                            netease-cloud-music-playlists nil nil 'string-equal)))))
    (if move
        (funcall get)
      (save-mark-and-excursion
        (funcall get)))))

(defun netease-cloud-music-change-playlist-mode ()
  "Scan the current situation and change the playlist mode accroding to it."
  (with-current-buffer netease-cloud-music-buffer-name
    (cond ((and (string-equal "Local Playlist:\n" (thing-at-point 'line))
                (equal (get-text-property (point) 'face)
                       'netease-cloud-music-playlists-face))
           (setq netease-cloud-music-use-local-playlist t
                 netease-cloud-music-playlists-songs nil
                 netease-cloud-music-playlist-id nil)
           (when netease-cloud-music-playlist-refresh-timer
             (cancel-timer netease-cloud-music-playlist-refresh-timer)
             (setq netease-cloud-music-playlist-refresh-timer nil))
           (throw 'stop t))
          
          ((equal (get-text-property (point) 'face)
                  'netease-cloud-music-playlist-face)
           (let ((playlist (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position))))
             (netease-cloud-music--switch-playlist playlist)
             (throw 'stop t)))
          
          ((and (overlays-at (point))
                (eq major-mode 'netease-cloud-music-mode))
           (save-mark-and-excursion
             (let* ((current-line (line-number-at-pos))
                    (current-mode ;If it's t, means that now is playing local playlist.Otherwise user's playlist.
                     (progn
                       (goto-char (point-max))
                       (search-backward "Local Playlist:")
                       (if (> current-line (line-number-at-pos))
                           t
                         nil)))
                    current-playlist)
               (when (or (not (eq netease-cloud-music-use-local-playlist current-mode))
                         (and (null current-mode)
                              (not
                               (eq (progn
                                     (goto-char (point-min))
                                     (forward-line (1- current-line))
                                     (setq current-playlist
                                           (netease-cloud-music--get-current-playlist)))
                                   netease-cloud-music-playlist-id))))
                 (setq netease-cloud-music-use-local-playlist current-mode)
                 (if current-mode
                     (setq netease-cloud-music-playlists-songs nil
                           netease-cloud-music-playlist-id nil)
                   
                   (setq netease-cloud-music-playlist-id
                         (if current-playlist
                             current-playlist
                           (progn
                             (goto-char (point-min))
                             (forward-line (1- current-line))
                             (netease-cloud-music--get-current-playlist)))
                         netease-cloud-music-playlists-songs
                         (netease-cloud-music-get-playlist-songs
                          netease-cloud-music-playlist-id))))))))))

(defun netease-cloud-music--switch-playlist (name)
  "Switch user playlist by its NAME."
  (setq netease-cloud-music-use-local-playlist nil
        netease-cloud-music-playlist-id
        (alist-get name netease-cloud-music-playlists
                   nil nil 'string-equal)
        netease-cloud-music-playlists-songs
        (netease-cloud-music-get-playlist-songs
         netease-cloud-music-playlist-id))
  (when netease-cloud-music-process
    (netease-cloud-music-kill-current-song)))

;;; Jump

(defun netease-cloud-music--keep-cursor-visible ()
  "Keep cursor visible."
  (set-window-point (get-buffer-window netease-cloud-music-buffer-name) (point)))

(defun netease-cloud-music-goto (position)
  "Goto the POSITION."
  (interactive (list (let ((face '((:inherit font-lock-warning-face :underline t))))
                       (read-char
                        (concat "Enter the position("
                                (propertize "g" 'face face)
                                " refresh, "
                                (propertize "u" 'face face)
                                "ser playlist, "
                                (propertize "l" 'face face)
                                "ocal playlist, "
                                (propertize "c" 'face face)
                                "urrent song, "
                                (propertize "s" 'face face)
                                "witch playlist, "
                                (propertize "p" 'face face)
                                "lay song, "
                                (propertize "f" 'face face)
                                " play at the first"
                                "):")))))
  (with-current-buffer netease-cloud-music-buffer-name
    (pcase position
      (?g (netease-cloud-music-interface-init))
      (?u
       (goto-char (point-min))
       (let ((case-fold-search t))
         (search-forward "User's Playlists:\n"))
       (netease-cloud-music--keep-cursor-visible))
      (?l
       (goto-char (point-min))
       (let ((case-fold-search t))
         (search-forward "Local Playlist:\n"))
       (netease-cloud-music--keep-cursor-visible))
      (?c
       (let ((playlist-index (unless netease-cloud-music-use-local-playlist
                               (netease-cloud-music--cdr-index
                                netease-cloud-music-playlist-id
                                netease-cloud-music-playlists)))
             (case-fold-search t))
         (goto-char (point-min))
         (if playlist-index
             (progn
               (search-forward "User's playlists:\n")
               (forward-line (1+ playlist-index)))
           (search-forward "Local Playlist:\n"))
         (forward-line netease-cloud-music-playlist-song-index)
         (netease-cloud-music--keep-cursor-visible)))
      (?s
       (let ((playlist (netease-cloud-music--jump)))
         (when playlist
           (netease-cloud-music--switch-playlist playlist))
         (netease-cloud-music-interface-init)
         (netease-cloud-music--keep-cursor-visible)))
      (?p
       (let ((song (netease-cloud-music--jump t)))
         (when (/= (string-to-number (substring (car song) 1)) 0)
           (goto-char (point-min))
           (forward-line (1- (cdr song)))
           (netease-cloud-music-play-song-at-point))))
      (?f (setq netease-cloud-music-playlist-song-index 0)
          (netease-cloud-music-playlist-play)))))

(defun netease-cloud-music--cdr-index (ele list)
  "Get the index of item in LIST which cdr is equal to ELE."
  (catch 'stop
    (dotimes (i (length list))
      (when (eq (cdr (nth i list)) ele)
        (throw 'stop i)))))

(defun netease-cloud-music-jump-read-line ()
  "The function for jump by reading line number."
  (read-number "Enter the line number(negetive number means up): "))

(defun netease-cloud-music--jump (&optional with-line)
  "Get the text from line number.
WHen WITH-LINE is non-nil, return the (content . line-number)."
  (if (not (fboundp netease-cloud-music-jump-function))
      (netease-cloud-music-error "The jump function cannot be found!")
    (let ((line (funcall netease-cloud-music-jump-function))
          result)
      (save-excursion
        (if netease-cloud-music-line-number-relative
            (forward-line line)
          (goto-char (point-min))
          (forward-line (1- line)))
        (setq result (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
      (if with-line
          (cons result (if netease-cloud-music-line-number-relative
                           (+ (line-number-at-pos) line)
                         line))
        result))))

(provide 'netease-cloud-music-ui)

;;; netease-cloud-music-ui.el ends here
