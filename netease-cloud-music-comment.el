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

;;; This file is the comment extension for `netease-cloud-music'.
;;; If you want to enable it, add the code below in your configuration:
;;; (require 'netease-cloud-music-comment)

(require 'url)                          ;Used to get the data of user avatar
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
  (setq buffer-read-only t))

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
  "Build comment content by its INFO."
  (let ((cid (car info))
        (content (nth 1 info))
        (user-name (nth 2 info))
        (avatar (nth 3 info)))
    ;; Debug
    (netease-cloud-music--insert-avatar avatar (current-buffer))
    ;; TODO: To find the way to keep user-name in the center vertically
    ;; Study the code in 'vertical-center.txt'
    (insert (propertize user-name 'face '((t :weight bold :height 1.5)))
            "\n\t\t" content "\n\n")
    ))

;;; Comment Network API
(defun netease-cloud-music-get-comment (id)
  "Get the song's comment by its ID."
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

(defun netease-cloud-music--insert-avatar (url buffer)
  "The function to insert user's avatar.
URL is avatar's url.
BUFFER is the comment buffer."
  (let ((buf (url-retrieve-synchronously url)))
    (unwind-protect
        (let ((data (with-current-buffer buf
                      (goto-char (point-min))
                      (search-forward "\n\n")
                      (buffer-substring (point) (point-max)))))
          (with-current-buffer buffer
            (insert-image (create-image data nil t :scale 0.05))))
      (kill-buffer buf))))

(provide 'netease-cloud-music-comment)

;;; netease-cloud-music-comment.el ends here
