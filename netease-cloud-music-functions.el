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

(require 'request)

(defcustom netease-cloud-music-api-type (cond ((executable-find "npm") 'npm)
                                              ((or (executable-find "docker")
                                                   (executable-find "podman"))
                                               'docker))
  "How to manage the api.

Its value can be as following:
npm: download api project into `netease-cloud-music-api-dir' and run npm
locally.
docker: use docker to start the api."
  :group 'netease-cloud-music
  :type '(choice (const :tag "Native" native)
                 (const :tag "Docker" docker)))

(defcustom netease-cloud-music-api-port "3000"
  "The port for the API process."
  :type 'string
  :group 'netease-cloud-music)

(defvar netease-cloud-music-phone nil
  "Phone number.")

(defvar netease-cloud-music-user-password nil
  "Password.")

(eval-and-compile
  (defmacro netease-cloud-music-api-defun (name arg &optional docstring &rest body)
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
       (if (not (or (not (netease-cloud-music--api-need-downloaded))
                    (netease-cloud-music--api-downloaded))) ; = !(x -> y)
           (netease-cloud-music-error "The third-party API has not been donwloaded!")
         ,@body)))

  (defmacro netease-cloud-music-for-eaf (&rest body)
    "The macro for eaf.  BODY is the Lisp you want to execute."
    (let ((with-eaf-buffer (eq (car body) :eaf-buffer)))
      (when with-eaf-buffer
        (pop body))
      `(when (get-buffer "eaf-netease-cloud-music")
         ,(if with-eaf-buffer
              `(with-current-buffer "eaf-netease-cloud-music"
                 ,@body)
            `(eval ,@body)))))

  (defmacro netease-cloud-music-eaf-defun (name args &rest body)
    "If the NAME function is not exists, define it.
ARGS is the arguments.
BODY is the body of the function."
    (declare (indent defun))
    (unless (or (functionp (symbol-function name))
                (macrop (symbol-function name)))
      `(defun ,name ,args
         ,@body)))

  (defmacro netease-cloud-music-expand-form (&rest form)
    "Expand FORM in function-form."
    `(cl-function
      (lambda (&key data &allow-other-keys)
        ,@form)))
  
  (defmacro netease-cloud-music--api-func (action)
    "Call the function which is matched with `netease-cloud-music--.*-api-.*'.
ACTION is its function."
    (when netease-cloud-music-api-type
      (let* ((sfunc (format "netease-cloud-music--%s-api-%s"
                            (symbol-name action)
                            (symbol-name netease-cloud-music-api-type)))
             (func (intern sfunc)))
        `(,func)))))

(defun netease-cloud-music-error (&rest seq)
  "Print the error message, it's SEQ."
  (let ((main (pop seq)))
    (eval `(user-error (concat "[Netease-Cloud-Music]: " ,main)
                       ,@seq))))

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
        (netease-cloud-music-error "The song you search is error!")
      (setq data (alist-get 'songs (alist-get 'result data)))
      (dotimes (n (length data))
        (setq song (aref data n)
              artist (aref (alist-get 'artists song) 0))
        (setq result (append result
                             (list
                              (list (alist-get 'id song)
                                    (alist-get 'name song)
                                    (alist-get 'id artist)
                                    (alist-get 'name artist))))))
      result)))

(defun netease-cloud-music-get-playlists (data)
  "Read the playlist json DATA searched from API."
  (let (playlist result)
    (if (/= 200 (alist-get 'code data))
        (netease-cloud-music-error "The playlist you search is error!")
      (setq data (alist-get 'playlists (alist-get 'result data)))
      (dotimes (n (length data))
        (setq playlist (aref data n))
        (setq result (append result
                             (list
                              (cons (alist-get 'name playlist)
                                    (alist-get 'id playlist))))))
      result)))

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
  "Get the index of ELE in LIST.  Use `equal' to check."
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

(defun netease-cloud-music--api-need-downloaded ()
  "Check if `netease-cloud-music-api-type' is depended on downloaded repo."
  (memq netease-cloud-music-api-type '(npm)))

(defun netease-cloud-music-api-request (url)
  "Request with the user info.
URL is the url to request."
  (let (result)
    (setq url (format "http://localhost:%s/%s"
                      netease-cloud-music-api-port url))
    (request (format "http://localhost:%s/login/cellphone?phone=%s&md5_password=%s&countrycode=%s"
                     netease-cloud-music-api-port
                     (cdr netease-cloud-music-phone)
                     netease-cloud-music-user-password
                     (car netease-cloud-music-phone))
      :success (netease-cloud-music-expand-form
                data                    ;PlaceHolder
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
                              (message nil)) ;Ignore the warning when API has not finished starting.
                            (when (get-buffer " *Request")
                              (with-current-buffer " *Request*"
                                (erase-buffer)))))
      :sync t)
    (when (get-buffer " *Request*")
      (with-current-buffer " *Request*"
        (unless (string-empty-p (buffer-string))
          (setq result (json-read-from-string (buffer-string))))))
    result))

(defun netease-cloud-music-alist-cdr (key list)
  "Find the first item in LIST which `cdr' is equal to KEY.
Use `equal' to compare.
If the item is exists, return the cons."
  (if (not (listp list))
      (user-error "The %S is not a list!" list)
    (let (item)
      (setq item
            (catch 'stop
              (dolist (ele list)
                (when (equal key (cdr ele))
                  (throw 'stop ele)))))
      item)))

(defun netease-cloud-music-encode-url (text)
  "Change the special chars in the TEXT to the escape character."
  (let ((chars (string-to-list text))
        tmp)
    (setq text nil)
    (dolist (char chars)
      (setq tmp (pcase char
                  (32 "%20")
                  (43 "%2B")
                  (47 "%2F")
                  (63 "%3F")
                  (37 "%25")
                  (35 "%23")
                  (38 "%26")
                  (61 "%3D")
                  (_ (char-to-string char))))
      (setq text (concat text tmp)))
    text))

(defun netease-cloud-music--get-lyric-time (lyric)
  "Get the LYRIC's time."
  (let (min sec)
    (progn
      (string-match "\\[\\(.*\\):\\(.*\\)\\.\\(.*\\)\\]\\(.*\\)"
                    lyric)
      (setq min (match-string 2 lyric)
            sec (match-string 3 lyric)))
    (string-to-number (concat min "." sec))))

(defun netease-cloud-music--format-lyric-time (time)
  "Format lyric TIME."
  (if (or (< time 0)
          (< (length (number-to-string time)) 5))
      time
    (let ((time-string (number-to-string time))
          sec msec)
      (progn
        (string-match "\\(.*\\)\\.\\(.*\\)" time-string)
        (setq sec (match-string 1 time-string)
              msec (match-string 2 time-string)))
      (string-to-number
       (concat sec "." (substring msec
                                  0 2))))))

(netease-cloud-music-eaf-defun eaf-call-async (&rest args)
  args)

(netease-cloud-music-eaf-defun eaf-call-sync (&rest args)
  args)

(netease-cloud-music-eaf-defun eaf--netease-cloud-music--update-song-style ())

(provide 'netease-cloud-music-functions)

;;; netease-cloud-music-functions.el ends here
