;;; netease-cloud-music-ui.el --- Netease Cloud Music client for Emacs -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 2.0

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

(defvar netease-cloud-music-buffer-name "*Netease-Cloud-Music*"
  "The name of Netease Music buffer.")

(defvar netease-cloud-music-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'netease-cloud-music-interface-init)
    (define-key map "Q" 'netease-cloud-music-close)
    (define-key map "q" 'netease-cloud-music-back)
    (define-key map (kbd "SPC") 'netease-cloud-music-pause-or-continue)
    (define-key map (kbd "RET") 'netease-cloud-music-play-song-at-point)
    (define-key map (kbd "TAB") 'netease-cloud-music-toggle-playlist-songs)
    (define-key map "c" 'netease-cloud-music-change-lyric-type)
    (define-key map "C" 'netease-cloud-music-change-playlist-name)
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
    (define-key map "L" 'netease-cloud-music-like-song)
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
          (na-error "The song info of the song under cursor is error!")
        (setq song-list (netease-cloud-music--get-song-list song artist))
        (netease-cloud-music-switch-close)
        (if netease-cloud-music-use-local-playlist
            (progn
              (add-to-list 'netease-cloud-music-playlist song-list)
              (netease-cloud-music-save-playlist))
          (netease-cloud-music--track t netease-cloud-music-playlist-id
                                      (car song-list)))
        (when (string= netease-cloud-music-repeat-mode "")
          (setq netease-cloud-music-repeat-mode "song"))
        (netease-cloud-music-play
         (car-safe song-list)
         song artist)))))

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
          (setq ids (append ids (list (car song))))))
      (netease-cloud-music--track t netease-cloud-music-playlist-id ids)))
  (netease-cloud-music-switch-close)
  (netease-cloud-music-interface-init))

(defun netease-cloud-music-switch-add-page (page)
  "Add the pages to playlist."
  (interactive
   (list
    (read-string "Enter the page[n-n]: "
                 (concat (number-to-string netease-cloud-music-search-page)
                         "-"))))
  (if netease-cloud-music-use-local-playlist
      (progn
        (netease-cloud-music--append (netease-cloud-music--songs-by-page page))
        (netease-cloud-music-save-playlist))
    (let (ids)
      (dolist (song (netease-cloud-music--songs-by-page page))
        (unless (alist-get (car song) netease-cloud-music-playlists-songs)
          (setq ids (append ids (list (car song))))))
      (netease-cloud-music--track t netease-cloud-music-playlist-id ids)))
  (netease-cloud-music-switch-close)
  (netease-cloud-music-interface-init))

(defun netease-cloud-music-playlist-enter ()
  "Add the playlist under the cursor."
  (interactive)
  (let ((playlist (alist-get
                   (substring (thing-at-point 'line) 0 -1)
                   netease-cloud-music-search-playlists nil nil 'string-equal))
        ids)
    (if (null playlist)
        (na-error "The playlist can not found!")
      (if netease-cloud-music-use-local-playlist
          (progn
            (netease-cloud-music--append (netease-cloud-music-get-playlist-songs playlist))
            (netease-cloud-music-save-playlist))
        (dolist (song (netease-cloud-music-get-playlist-songs playlist))
          (unless (alist-get (car song) netease-cloud-music-playlists-songs)
            (setq ids (append ids (car song)))))
        (netease-cloud-music--track t netease-cloud-music-playlist-id ids))
      (netease-cloud-music-switch-close)
      (netease-cloud-music-interface-init))))

;;;###autoload
(define-derived-mode netease-cloud-music-write-mode text-mode "Netease-Cloud-Music:Write"
  "The write mode for `netease-cloud-music'."
  :group 'netease-cloud-music
  :syntax-table nil
  :abbrev-table nil
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

(defun netease-cloud-music-write-cancel ()
  "Cancel and turn to normal mode."
  (interactive)
  (netease-cloud-music-mode)
  (netease-cloud-music-interface-init)
  (when netease-cloud-music-show-lyric
    (with-current-buffer netease-cloud-music-buffer-name
      (netease-cloud-music-add-header-lyrics))))

(defun netease-cloud-music--deleted-item (list)
  "Check the delete item in LIST. For playlist."
  (let (result)
    (dolist (song netease-cloud-music-playlists-songs)
      (unless (memq (car song) list)
        (setq result (append result (list (car song))))))
    result))

(defun netease-cloud-music-write-finish ()
  "Modify by the modified contents and turn to normal mode."
  (interactive)
  (let (current-song song-name copy deleted)
    (with-current-buffer netease-cloud-music-buffer-name
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
            (na-error "There's an error when save the result!"))
        (if netease-cloud-music-use-local-playlist
            (progn
              (setq netease-cloud-music-playlist copy)
              (netease-cloud-music-save-playlist))
          (netease-cloud-music-update-songs-order-in-playlist
           netease-cloud-music-playlist-id copy)

          (when (setq deleted (netease-cloud-music--deleted-item copy))
            (netease-cloud-music--track nil netease-cloud-music-playlist-id deleted)))

        (netease-cloud-music-write-cancel)
        (netease-cloud-music-adjust-song-index)))))

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
  "Initialize the Netease Music buffer in netease-cloud-music-mode."
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
    (when (and (netease-cloud-music--api-downloaded)
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

(defun netease-cloud-music-playlist-add-all ()
  "Add all the searched playlists to the playlist"
  (interactive)
  (if (null netease-cloud-music-search-playlists)
      (na-error "The playlist can not found!")
    (let (ids)                          ;For user's playlist
      (dolist (playlist netease-cloud-music-search-playlists)
        (if netease-cloud-music-use-local-playlist
            (progn
              (netease-cloud-music--append
               (netease-cloud-music-get-playlist-songs (cdr playlist)))
              (netease-cloud-music-save-playlist))
          (dolist (song (netease-cloud-music-get-playlist-songs (cdr playlist)))
            (unless (alist-get (car song) netease-cloud-music-playlists-songs)
              (setq ids (append ids (car song)))))))
      (when ids
        (netease-cloud-music--track t netease-cloud-music-playlist-id ids)))
    (netease-cloud-music-switch-close)
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
      (when netease-cloud-music-username ;Show user name
        (insert "你好，"
                (propertize netease-cloud-music-username
                            'face '(:inherit font-lock-constant-face :weight bold)) "\n"))
      ;; Show the repeat mode status
      (insert (concat
               (propertize "Repeat: "
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

      ;; User's Playlist
      (when netease-cloud-music-playlists
        (insert (propertize "\nUser's Playlists:\n"
                            'face '(:height 1.05 :foreground "gold2")))
        (dolist (playlist netease-cloud-music-playlists)
          (insert (propertize (car playlist)
                              'face '(:inherit font-lock-warning-face :weight bold))
                  "\n")
          (when (and netease-cloud-music-playlist-id
                     (= netease-cloud-music-playlist-id (cdr playlist)))
            (mapc #'(lambda (s)
                      (insert (format "%s - %s\n"
                                      (propertize
                                       (nth 1 s)
                                       'face 'font-lock-keyword-face)
                                      (propertize
                                       (nth 3 s)
                                       'face 'font-lock-function-name-face))))
                  netease-cloud-music-playlists-songs))))

      ;; Local Playlist
      (when netease-cloud-music-playlist
        (insert (propertize "\nLocal Playlist:\n"
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

(defun netease-cloud-music-toggle-playlist-songs (pid)
  "Toggle the songs of playlist under cursor."
  (interactive (list
                (let ((playlist-name (substring-no-properties
                                      (thing-at-point 'line) 0 -1))
                      playlist)
                  (if (null
                       (setq playlist
                             (alist-get playlist-name netease-cloud-music-playlists
                                        nil nil 'string-equal)))
                      (na-error "The playlist is not exists!")
                    playlist))))
  (with-current-buffer netease-cloud-music-buffer-name
    (setq-local buffer-read-only nil)
    (save-mark-and-excursion
      (forward-line)                    ;To check the next line if it's a null string or songs' info.
      (if (not (or (equal (get-text-property (point) 'face)
                          '(:inherit font-lock-warning-face :weight bold))
                   (string-empty-p (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position)))))
          (progn
            (while (eq (get-text-property (point) 'face) 'font-lock-keyword-face)
              (delete-region (line-beginning-position) (line-end-position))
              (delete-char 1)))
        (forward-line -1)
        (goto-char (line-end-position))
        (insert "\n")
        (let ((songs (netease-cloud-music-get-playlist-songs pid)))
          (if (null songs)
              (message "[Netease-Cloud-Music]: The playlist is empty.")
            (dolist (song songs)
                   (insert (format "%s - %s\n"
                                   (propertize
                                    (nth 1 song)
                                    'face 'font-lock-keyword-face)
                                   (propertize
                                    (nth 3 song)
                                    'face 'font-lock-function-name-face))))))
        (delete-char -1)))
    (setq-local buffer-read-only t)))

(defun netease-cloud-music--get-current-playlist (&optional move)
  "Get the current playlist.
MOVE means do not care about the cursor's position."
  (let ((get (lambda ()
               (with-current-buffer netease-cloud-music-buffer-name
                 (while (eq (get-text-property (point) 'face) 'font-lock-keyword-face) ;To get to the playlist's name
                   (forward-line -1))
                 (alist-get (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position))
                            netease-cloud-music-playlists nil nil 'string-equal))))
        playlist)
    (if move
        (funcall get)
      (save-mark-and-excursion
        (funcall get)))))

(defun netease-cloud-music-change-playlist-mode ()
  "Scan the current situation and change the playlist mode accroding to it."
  (with-current-buffer netease-cloud-music-buffer-name
    (cond ((and (string-equal "Local Playlist:\n" (thing-at-point 'line))
                (equal (get-text-property (point) 'face)
                       '(:height 1.05 :foreground "gold2")))
           (setq netease-cloud-music-use-local-playlist t
                 netease-cloud-music-playlists-songs nil
                 netease-cloud-music-playlist-id nil)
           (when netease-cloud-music-playlist-refresh-timer
             (cancel-timer netease-cloud-music-playlist-refresh-timer)
             (setq netease-cloud-music-playlist-refresh-timer nil))
           (throw 'stop t))
          
          ((equal (get-text-property (point) 'face)
                  '(:inherit font-lock-warning-face :weight bold))
           (let ((playlist (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position))))
             (setq netease-cloud-music-use-local-playlist nil
                   netease-cloud-music-playlist-id
                   (alist-get playlist netease-cloud-music-playlists
                              nil nil 'string-equal)
                   netease-cloud-music-playlists-songs
                   (netease-cloud-music-get-playlist-songs
                    netease-cloud-music-playlist-id))
             (when netease-cloud-music-process
               (netease-cloud-music-kill-current-song))
             (throw 'stop t)))
          
          ((and (string-match-p "^\\(.*\\) - \\(.*\\)" (thing-at-point 'line))
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

(defun netease-cloud-music-change-playlist-name (pid name)
  "Change the playlist's name under cursor."
  (interactive
   (let ((playlist (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))
         id)
     (if (setq id (alist-get playlist
                             netease-cloud-music-playlists
                             nil nil 'string-equal))
         (list id (read-string "Enter the new name: "))
       (na-error "The playlist is not exists!"))))
  (netease-cloud-music-update-playlist-name pid name)
  (netease-cloud-music-interface-init))

(provide 'netease-cloud-music-ui)

;;; netease-cloud-music-ui.el ends here
