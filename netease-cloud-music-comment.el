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

(defcustom netease-cloud-music-comments nil
  "The list of comments."
  :type 'list
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "x" #'bury-buffer)
    map)
  "The keymap for netease-cloud-music-comment-mode."
  :type 'keymap
  :group 'netease-cloud-music)

(defcustom netease-cloud-music-comment-buffer-template
  "*Netease-Cloud-Music-Comment:%s*"
  "The template for comment buffer."
  :type 'string
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

(defun netease-cloud-music-goto-top-or-refresh ()
  "Goto the top of comment buffer or refresh it."
  (interactive)
  (pcase (read-char "[g]oto top  [G]refresh")
    (?g (goto-char (point-min)))
    (?G
     ;; TODO: Add refresh function
     )))

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
                                    (alist-get 'content comment)
                                    (alist-get 'nickname (car comment))
                                    (alist-get 'avatarUrl (car comment)))))))
      result)))

;; TODO: To test this function, (haven't been tested).
(defun netease-cloud-music-comment-or-reply (sid content &optional cid)
  "The function to comment or reply CONTENT to a comment.
SID is the song's id.
When CID is non-nil, means to reply comment with cid(its id)."
  (let ((send (netease-cloud-music-api-request
               (format "comment?t=%s&type=0&id=%d&content=%s"
                       (if cid
                           (format "2&%d" cid)
                         "1")
                       sid (url-encode-url content)))))
    (if (/= (alist-get 'code send) 200)
        (netease-cloud-music-error "Failed to comment!")
      send)))
;; TODO: Add a function to delete the comment

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

;;; Debug
(defun spring/ncm-build-comment ()
  "Debug function."
  (netease-cloud-music--content-build
   (car (netease-cloud-music-get-comment
         (nth 2 netease-cloud-music-current-song)))))

(provide 'netease-cloud-music-comment)

;;; netease-cloud-music-comment.el ends here
