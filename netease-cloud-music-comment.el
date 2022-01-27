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

;;; This file is the comment extension for `netease-cloud-music'.
;;; If you want to enable it, add the code below in your configuration:
;;; (require 'netease-cloud-music-comment)

(require 'svg)
(require 'netease-cloud-music)

(when (featurep 'netease-cloud-music-ui)
  (define-key netease-cloud-music-mode-map "R" #'netease-cloud-music-comment))

(defcustom netease-cloud-music-comments nil
  "The list of comments."
  :type 'list
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-comment-buffers nil
  "All the comment buffers."
  :type 'list
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-edit-buffers nil
  "All the edit buffers."
  :type 'list
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'netease-cloud-music-comment-exit)
    (define-key map "x" #'bury-buffer)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "c" #'netease-cloud-music-copy-content)
    (define-key map "x" #'bury-buffer)
    (define-key map "R" #'netease-cloud-music-reply)
    (define-key map "g" #'beginning-of-buffer)
    (define-key map "G" #'end-of-buffer)
    map)
  "The keymap for netease-cloud-music-comment-mode."
  :type 'keymap
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'netease-cloud-music-edit-finish)
    (define-key map (kbd "C-c C-k") #'netease-cloud-music-edit-cancel)
    map)
  "The keymap for `netease-cloud-music-edit-mode'."
  :type 'keymap
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-comment-buffer-template
  "*Netease-Cloud-Music-Comment:%s*"
  "The template for comment buffer."
  :type 'string
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-buffer-song-id nil
  "The song-id of current comment buffer."
  :type 'number
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-target-info nil
  "The info about the target which will be sent comment.
Format: (song-id . comment-id)."
  :type 'list
  :group 'netease-cloud-music)

;;;###autoload
(define-derived-mode netease-cloud-music-comment-mode nil "Netease-Cloud-Music-Comment"
  "The comment mode for `netease-cloud-music'."
  :group 'netease-cloud-music
  :abbrev-table nil
  :syntax-table nil
  (setq buffer-read-only t)
  (when netease-cloud-music-show-lyric
    (netease-cloud-music-add-header-lyrics)))

(defun netease-cloud-music-comment (song-id song-name)
  "Open the comment buffer of song with SONG-ID & SONG-NAME.
If there's a song like 'xxx - xxx' under cursor, 
then get its id.
Otherwise get the playing song's id."
  (interactive (let ((song (netease-cloud-music--current-song)))
                 (if song
                     (progn
                       (setq song
                             (nth song (if netease-cloud-music-use-local-playlist
                                           netease-cloud-music-playlist
                                         netease-cloud-music-playlists-songs)))
                       (list (car song) (nth 1 song)))
                   (if netease-cloud-music-current-song
                       (list (nth 2 netease-cloud-music-current-song)
                             (car netease-cloud-music-current-song))
                     (netease-cloud-music-error "Can't get any song!")))))
  (let ((buf-name (format netease-cloud-music-comment-buffer-template
                          song-name))
        (comments (cons song-id
                        (netease-cloud-music-get-comment
                         song-id))))
    (with-current-buffer (get-buffer-create buf-name)
      (setq netease-cloud-music-comments
            (append netease-cloud-music-comments (list comments)))
      (mapc #'netease-cloud-music--content-build (cdr comments))
      ;; TODO: Add '[Press for more]' button.
      ;; TODO: Improve the speed for image insert (Like asynchronous)
      (goto-char (point-min))
      (netease-cloud-music-comment-mode)
      (setq-local netease-cloud-music-buffer-song-id song-id))
    (add-to-list 'netease-cloud-music-comment-buffers (get-buffer buf-name))
    (switch-to-buffer buf-name)))

;;;###autoload
(define-derived-mode netease-cloud-music-edit-mode text-mode "Netease-Cloud-Music-Edit"
  "The edit mode for editing comment content."
  :group 'netease-cloud-music
  :abbrev-table nil
  :syntax-table nil
  (setq header-line-format
        (substitute-command-keys
         (concat "\\<netease-cloud-music-edit-mode-map>"
                 "NCM-Edit: "
                 "Confirm with '\\[netease-cloud-music-edit-finish]', "
                 "Cancel with '\\[netease-cloud-music-edit-cancel]'"))))

(defun netease-cloud-music-edit-finish ()
  "Finish the comment editing and send it."
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((target-info netease-cloud-music-target-info)
          (content (buffer-string)))
      (netease-cloud-music-edit-cancel)
      (netease-cloud-music-comment-or-reply
       (car target-info) content (cdr target-info)))))

(defun netease-cloud-music-edit-cancel ()
  "Cancel the editing."
  (interactive)
  (unless (eq major-mode 'netease-cloud-music-edit-mode)
    (netease-cloud-music-error "You're not in the edit buffer!"))
  (let ((buf (current-buffer)))
    (delete-window)
    (kill-buffer buf)
    (setq netease-cloud-music-edit-buffers
          (remq buf netease-cloud-music-edit-buffers))))

(defun netease-cloud-music-reply ()
  "Open the scratch buffer for comment."
  (interactive)
  (with-current-buffer (current-buffer)
    (netease-cloud-music--comment-check)
    (let* ((song-id netease-cloud-music-buffer-song-id)
           (height (* 0.45 (window-body-height nil t)))
           (cid (pcase (read-char "[r]Comment, [R]eply")
                  (?r nil)
                  (?R (netease-cloud-music--get-current-comment-id))))
           (song-name (progn
                        (string-match
                         "\\*Netease-Cloud-Music-Comment:\\(.*\\)\\*"
                         (buffer-name))
                        (match-string 1 (buffer-name))))
           (buf (get-buffer-create (concat "*NCM-Edit: " song-name))))
      (split-window nil height 'above t)
      (with-current-buffer buf
        (netease-cloud-music-edit-mode)
        (setq-local netease-cloud-music-target-info
                    (cons song-id cid)))
      (add-to-list 'netease-cloud-music-edit-buffers buf)
      (switch-to-buffer buf))))

(defun netease-cloud-music-comment-exit ()
  "Exit current comment buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (netease-cloud-music--comment-check)
    (setq netease-cloud-music-comments
          (remq (netease-cloud-music--car-eq
                 netease-cloud-music-buffer-song-id
                 netease-cloud-music-comments)
                netease-cloud-music-comments)))
  (setq netease-cloud-music-comment-buffers
        (remq (current-buffer) netease-cloud-music-comment-buffers))
  (kill-current-buffer))

(defun netease-cloud-music-copy-content ()
  "Copy the content of comment under cursor."
  (interactive)
  (netease-cloud-music--comment-check)
  (save-excursion
    (goto-char (line-beginning-position))
    (let (start tmp)
      (if (and (not (eolp))
               (setq tmp (overlays-at (point)))
               (null (overlay-get (car tmp) 'display)))
          (forward-line)
        (when (or (eolp)
                  (overlays-at (point)))
          (forward-line -1)
          (while (or (eolp)
                     (overlays-at (point)))
            (forward-line -1))))
      (setq start (+ 2 (point)))
      (while (not (overlays-at (point)))
        (forward-line))
      (kill-ring-save start (1- (point))))))

(defun netease-cloud-music--throw-mass-suffix (content)
  "Throw the mass suffix like '^M' of CONTENT."
  (let ((string-list (split-string content "\n" t))
        result)
    (if (= (length string-list) 1)
        content
      (dolist (line string-list)
        (setq result
              (concat result
                      (if (string-suffix-p "" line)
                          (substring line 0 -1)
                        line)
                      "\n")))
      (substring result 0 -1))))

(defun netease-cloud-music--comment-check ()
  "Check if current buffer is comment mode.
If not, throw an error."
  (unless (eq major-mode 'netease-cloud-music-comment-mode)
    (netease-cloud-music-error "You're not in a comment buffer!")))

;;; UI building Functions
(defun netease-cloud-music--content-build (info)
  "Build comment content by its INFO.
 Tips: the last but one line is not null.
Content's id is here."
  (let ((cid (car info))
        (content (nth 1 info))
        (user-name (nth 2 info))
        (avatar (nth 3 info))
        start-point)
    (with-current-buffer (current-buffer)
      (goto-char (setq start-point (point-max)))
      (netease-cloud-music--insert-avatar avatar)
      (insert " " (propertize user-name 'face '((t :weight bold :height 1.2)))
              "\n")
      (overlay-put (make-overlay start-point (1+ (point))) 'face
                   '((t :inherit header-line :extend t)))
      (insert "\t\t" content "\n" (number-to-string cid))
      ;; NOTE: Hide the content id, and you can get it by `overlays-at'
      (overlay-put (make-overlay (line-beginning-position) (line-end-position))
                   'display "\n")
      (insert "\n"))))

(defun netease-cloud-music--insert-avatar (url)
  "The function to insert user's avatar.
URL is avatar's url."
  (let ((buf (url-retrieve-synchronously url)))
    (unwind-protect
        (let ((data (with-current-buffer buf
                      (goto-char (point-min))
                      (search-forward "\n\n")
                      (buffer-substring (point) (point-max)))))
          (let* ((width (round (* (frame-width) 0.35)))
                 (svg (svg-create width width))
                 (cpath (svg-clip-path svg :id "clip")))
            (svg-rectangle cpath 0 0 width width
                           :rx (* 0.5 width))
            (svg-embed svg data "image/jpeg" t
                       :width (format "%dpx" width)
                       :height (format "%dpx" width)
                       :clip-path "url(#clip)")
            (svg-insert-image svg)))
      (kill-buffer buf))))

;;; Comment Network API
(defun netease-cloud-music-get-comment (id)
  "Get the song's comment by its ID and return it.
Warning: This function doesn't have side-effect."
  (let* ((get-page (1+ (/ (length (alist-get id netease-cloud-music-comments)) 10)))
         (data (netease-cloud-music--request
                (format
                 "http://localhost:%s/comment/new?type=0&id=%d&pageSize=10&sortType=1&pageNo="
                 netease-cloud-music-api-port id get-page)))
         result comment)
    (if (/= (alist-get 'code data) 200)
        (netease-cloud-music-error "Failed to get comment of %d." id)
      (setq data (alist-get 'comments (alist-get 'data data)))
      (dotimes (i (length data))
        (setq comment (aref data i))
        (setq result (append result
                             (list
                              (list (alist-get 'commentId comment)
                                    (netease-cloud-music--throw-mass-suffix
                                     (alist-get 'content comment))
                                    (alist-get 'nickname (car comment))
                                    (alist-get 'avatarUrl (car comment)))))))
      result)))

(defun netease-cloud-music-comment-or-reply (sid content &optional cid)
  "The function to comment or reply CONTENT to a comment.
SID is the song's id.
When CID is non-nil, means to reply comment with cid(its id)."
  (let ((send (netease-cloud-music-api-request
               (format "comment?t=%s&type=0&id=%d&content=%s"
                       (if cid
                           (format "2&commentID=%d" cid)
                         "1")
                       sid (url-encode-url content)))))
    (if (/= (alist-get 'code send) 200)
        (netease-cloud-music-error "Failed to comment!")
      (message "[Netease-Cloud-Music]: Successfully sent a comment!")
      send)))

(defun netease-cloud-music-delete-comment (sid cid)
  "The function to delete comment.
SID is the song's id.
CID is the comment's id."
  (let ((del (netease-cloud-music-api-request
              (format "comment?t=0&type=0&id=%d&commentId=%d"
                      sid cid))))
    (if (/= (alist-get 'code del) 200)
        (netease-cloud-music-error "Failed to delete the comment!")
      (message "[Netease-Cloud-Music]: Successfully deleted a comment!")
      del)))

;;; Comment UI API
(defun netease-cloud-music--get-current-comment-id ()
  "The function to get comment id under cursor."
  (save-excursion
    (with-current-buffer (current-buffer)
      (let (ov move-arg)
        (setq move-arg
              (cond ((and (= (line-beginning-position) (line-end-position))
                          (null (overlays-at (point))))
                     -1)
                    ((/= (line-beginning-position) (line-end-position))
                     1)))
        (when move-arg
          (forward-line move-arg)
          (while (null (setq ov (overlays-at (point))))
            (forward-line move-arg)))
        (setq ov (if ov
                     (car ov)
                   (car (overlays-at (point)))))
        (string-to-number
         (buffer-substring (overlay-start ov) (overlay-end ov)))))))

;;; Advice
(advice-add 'netease-cloud-music-quit :after
            (lambda ()
              (setq netease-cloud-music-comments nil)
              (mapc #'kill-buffer
                    (append netease-cloud-music-comment-buffers
                            netease-cloud-music-edit-buffers))
              (setq netease-cloud-music-comment-buffers nil
                    netease-cloud-music-edit-buffers nil)))

;;; Debug
(defun spring/ncm-build-comment ()
  "Debug function."
  (netease-cloud-music--content-build
   (car (netease-cloud-music-get-comment
         (nth 2 netease-cloud-music-current-song)))))

(provide 'netease-cloud-music-comment)

;;; netease-cloud-music-comment.el ends here
