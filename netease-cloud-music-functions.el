;;; netease-cloud-music-functions.el --- Netease Cloud Music client for Emacs -*- lexical-binding: t -*-

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
  "Save login info. LOGINFO is the info."
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

(defun netease-cloud-music-get-song (data)
  "Read the Netease Music json DATA and return the result."
  (let (song artist result)
    (if (/= 200 (alist-get 'code data))
        (user-error "[Netease-Cloud-Music]: The song you search is error!")
      (setq data (alist-get 'songs (alist-get 'result data)))
      (dotimes (n (length data))
        (setq song (aref data n)
              artist (aref (alist-get 'artists song) 0))
        (add-to-list 'result
                     (list (alist-get 'id song)
                           (alist-get 'name song)
                           (alist-get 'id artist)
                           (alist-get 'name artist))
                     t))
      result)))

(defun netease-cloud-music-get-playlists (data)
  "Read the playlist json DATA searched from API."
  (let (playlist result)
    (if (/= 200 (alist-get 'code data))
        (user-error "[Netease-Cloud-Music]: The playlist you search is error!")
      (setq data (alist-get 'playlists (alist-get 'result data)))
      (dotimes (n (length data))
        (setq playlist (aref data n))
        (add-to-list 'result
                     (cons (alist-get 'name playlist)
                           (alist-get 'id playlist))
                     t))
      result)))

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
  "Get the current song at point."
  (let ((song (cond ((stringp song-info)
                     song-info)
                    (song-info nil)
                    (t (substring (thing-at-point 'line) 0 -1))))
        (index 0)
        name artist)
    (if (or (null song-info)
            (stringp song-info))
        (ignore-errors
          (progn
            (string-match "\\(.*\\) - \\(.*\\)" song)
            (setq name (match-string 1 song)
                  artist (match-string 2 song))))
      (setq name (car song-info)
            artist (nth 1 song-info)))
    (when (and name artist)
      (catch 'song-list
        (dolist (song-info netease-cloud-music-playlist)
          (when (and (string= name (nth 1 song-info))
                     (string= artist (nth 3 song-info)))
            (throw 'song-list index))
          (setq index (1+ index)))))))

(defun netease-cloud-music--current-lyric (string)
  "Get the lyric from STRING."
  (ignore-errors
    (match-string
     (if (string-match "\\[\\(.*\\):\\(.*\\)\\.\\(.*\\)\\]\\(.*\\)" string)
         4
       (when (string-match
              "\\[\\(.*\\):\\(.*\\)\\]\\(.*\\)"
              string)
         3))
     string)))

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
    (let ((index 0))
      (dolist (item list)
        (when (equal ele item)
          (throw 'result index))
        (setq index (1+ index)))
      nil)))

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
  "Check if the third-party API process is live."
  (and netease-cloud-music-api-process
       (process-live-p netease-cloud-music-api-process)))

(defmacro netease-api-defun (name arg &optional docstring &rest body)
  "Like `defun', but it will check the third-party api's status first.
NAME is the function's name.
ARG is the arguments for function.
DOCSTRING is the doc-string for the function.
BODY is the main codes for the function."
  (declare (indent defun)
           (debug defun)
           (doc-string 3))
  `(defun ,name ,arg
     ,(when docstring
        docstring)
     ,(when (equal (car (car body)) 'interactive)
        (prog1 (car body)
          (pop body)))
     (if (not (netease-cloud-music--api-downloaded))
         (user-error "[Netease-Cloud-Music]: The third-party API has not been donwloaded!")
       ,@body)))

(defun netease-api-request (url)
  "Request with the user info.
URL is the url to request."
  (let (result)
    (request (format "http://localhost:%s/login/cellphone?phone=%s&md5_password=%s&countrycode=%s"
                     netease-cloud-music-api-port
                     (cdr netease-cloud-music-phone)
                     netease-cloud-music-user-password
                     (car netease-cloud-music-phone))
      :success (netease-cloud-music-expand-form
                (request url
                  :parser 'buffer-string
                  :success (netease-cloud-music-expand-form
                            (with-current-buffer (get-buffer-create " *Request*")
                              (erase-buffer)
                              (insert data)))
                  :sync t))
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (when (string-match-p "^exited abnormally with code \\(.*\\)"
                                                  (cdr error-thrown))
                              (message nil))))
      :sync t)
    (when (get-buffer " *Request*")
      (with-current-buffer " *Request*"
        (setq result (json-read-from-string (buffer-string)))))
    result))

(provide 'netease-cloud-music-functions)

;;; netease-cloud-music-functions.el ends here
