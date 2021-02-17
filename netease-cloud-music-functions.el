;;; netease-cloud-music-functions.el --- Netease Cloud Music client for Emacs -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 1.5

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

(defun netease-cloud-music-get-loginfo ()
  "Get login info."
  (when (netease-cloud-music-exists-p "log-info")
    (let* ((file (split-string
                  (with-temp-buffer
                    (insert-file-contents (concat netease-cloud-music-cache-directory
                                                  "log-info"))
                    (buffer-string))
                  "\n" t))
           (name (netease-cloud-music-list-string-to-string
                  (split-string (car file) " " t)))
           (password (netease-cloud-music-list-string-to-string
                      (split-string (nth 1 file) " " t))))
      (cons name password))))

(defun netease-cloud-music-save-loginfo (loginfo)
  "Save login info."
  (netease-cloud-music-exists-p "log-info" t)
  (with-temp-file (concat netease-cloud-music-cache-directory "log-info")
    (erase-buffer)
    (insert (netease-cloud-music-string-to-char-string (car loginfo))
            "\n"
            (netease-cloud-music-string-to-char-string (cdr loginfo)))))

(defun netease-cloud-music-string-to-char-string (string)
  "Convert string to string that includes the last one's ASCII."
  (let ((result "")
        (string-list (string-to-list string)))
    (mapc #'(lambda (c)
              (setq result (concat result (if (string= result "")
                                              ""
                                            " ")
                                   (number-to-string (string-to-char c)))))
          string-list)
    result))

(defun netease-cloud-music-list-string-to-string (list-string)
  "Convert the list includes ASCII strings to a string."
  (let ((result ""))
    (mapc #'(lambda (s) (setq result (concat result
                                             (char-to-string (string-to-number s)))))
          list-string)
    result))

(defun netease-cloud-music-exists-p (file &optional create)
  "Check if FILE is exists. When CREATE is t, it'll create a empty file named FILE if it's not exists."
  (let* ((file-path (concat netease-cloud-music-cache-directory file))
         (exists-p (file-exists-p file-path)))
    (when (and create (null exists-p))
      (make-empty-file file-path))
    exists-p))

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
      ('song (setq search-type "1")))
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

(defun netease-cloud-music-ask (type)
  "Ask user TYPE of the question.
If user reply y, return t.
Otherwise return nil."
  (let ((ask (pcase type
               ('song "The info of the song is here, do you want to listen it?")
               ('add-song-to-playlist "Do you want to add the current song into playlist?")
               ('delete-song-from-playlist "Do you want to delete the song from playlist?")))
        result)
    (setq result (read-char (concat ask "(y/n)")))
    (when (= result 121)
      t)))

(defun netease-cloud-music-read-json (data &optional sid sname aid aname limit)
  "Read the Netease Music json DATA and return the result.

SID is the song-id.

SNAME is the song-name.

AID is the artist-id.

ANAME is the artist-name.

LIMIT is the limit for the results, it's a number."
  (when (symbolp limit)
    (setq limit 1))
  (let (result song-json r-sid r-sname r-aid r-aname to-get-json)
    (if (eq (car (cadar data)) 'queryCorrected)
        (if (> limit 1)
            (setq song-json (cdar (cddar data)))
          (setq song-json (aref (cdar (cddar data)) 0)))
      (if (> limit 1)
          (setq song-json (cdr (cadar data)))
        (setq song-json (aref (cdr (cadar data)) 0))))
    (dotimes (song-time limit)
      ;; Set the json which is contented the songs info
      (if (= limit 1)
          (setq to-get-json song-json)
        (setq to-get-json (aref song-json song-time)))

      (when sid
        (setq r-sid (cdar to-get-json)))
      (when sname
        (setq r-sname
              (cdadr to-get-json)))
      (when aid
        (setq r-aid
              (cdar (aref
                     (cdar (cddr to-get-json)) 0))))
      (when aname
        (setq r-aname
              (cdadr (aref
                      (cdar (cddr to-get-json)) 0))))
      (if (not (null result))
          (setf result (append result (list (list r-sid r-sname r-aid r-aname))))
        (if (= limit 1)
            (setf result (list r-sid r-sname r-aid r-aname))
          (setf result (list (list r-sid r-sname r-aid r-aname))))))
    result))

(defun netease-cloud-music--songs-by-page (page-string)
  "Get the songs list by the page limit."
  (let (start end search-result)
    (progn
      (string-match "\\(.*\\)-\\(.*\\)" page-string)
      (setq start (match-string 1 page-string)
            end (match-string 2 page-string)))
    (when (and start end)
      (setq start (* netease-cloud-music-search-limit (1- (string-to-number start)))
            end (* netease-cloud-music-search-limit (string-to-number end)))
      (setq search-result
            (netease-cloud-music-read-json
             (netease-cloud-music-request-from-api
              (car netease-cloud-music-search-alist)
              nil end)
             t t t t end))
      (when search-result
        (setq search-result
              (netease-cloud-music--catch-songs nil search-result start t))))))

(defun netease-cloud-music--get-song-list (name artist)
  "Get the song-info list by its NAME and ARTIST."
  (when (cdr-safe netease-cloud-music-search-alist)
    (catch 'result
      (dolist (song-list (cdr netease-cloud-music-search-alist))
        (print song-list)
        (when (and (string= name (nth 1 song-list))
                   (string= artist (nth 3 song-list)))
          (throw 'result song-list))))))

(defun netease-cloud-music--catch-songs (all song-list &optional index-limit no-result-limit)
  "Catch the song list by ALL length in SONG-LIST.
INDEX-LIMIT is the start of the song-list.
NO-RESULT-LIMIT means do not limit the songs catch."
  (let ((index (if index-limit
                   index-limit
                 (- all netease-cloud-music-search-limit)))
        result)
    (if no-result-limit
        (progn
          (setq result song-list)
          (dotimes (_ index)
            (pop result)))
      (dotimes (_ netease-cloud-music-search-limit)
        (setq result (append result (list (nth index song-list))))
        (setq index (1+ index))))
    result))

(defun netease-cloud-music--index (ele list)
  "Get the index of ELE in LIST. Use `equal' to check."
  (let ((index 0))
    (catch 'stop
      (dolist (item list)
        (when (equal ele item)
          (throw 'stop t))
        (setq index (1+ index))))
    index))

(defun netease-cloud-music--memeq (ele list)
  "Check if ELE is in LIST.
Like `memq', but use `equal'."
  (catch 'result
    (dolist (item list)
      (when (equal ele item)
        (throw 'result t)))))

(provide 'netease-cloud-music-functions)

;;; netease-cloud-music-functions.el ends here
