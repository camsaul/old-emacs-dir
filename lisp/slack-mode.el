;; -*- lexical-binding: t -*-
;;; slack-mode --- Work-in-progress implementation of a Slack client for Emacs
;;; Commentary:
;;; Code:

(require 'cam-macros)
'
;; need to install http-post-simple package (!)
(cam/setup-autoloads ("http-post-simple" #'http-post-simple-internal))

;; (require 'http-post-simple)

;; HARASS COWORKERS BY AUTO-POSTING TO SLACK

(defvar slack-channel
  "data"
  "The channel to post Slack messages to.")

(defun slack-url ()
  "The URL for posting Slack messages."
  (concat "https://expa.slack.com/services/hooks/slackbot?token=FQ2iLGcb6m0dcByOTuSfnoZQ&channel=" slack-channel))

(defun set-slack-user (username)
  "Send future Slack messages to USERNAME."
  (interactive "sSend Slack messages to user: ")
  (setq slack-channel (concat "%40" username)))

(defun set-slack-channel (channel)
  "Send future Slack messages to CHANNEL."
  (interactive "sSend Slack messages to channel: ")
  (setq slack-channel (concat "%23" channel)))

(defun post-to-slack (message)
  "Post MESSAGE to Slack."
  (interactive "sMessage: ")
  (http-post-simple-internal
   (slack-url)
   message
   'utf-8
   nil)
  message)

(defun angry-police-captain-to-slack ()
  "Post angry police caption quote to slack."
  (interactive)
  (angry-police-captain)
  (sleep-for 1) ; wait for HTTP request to finish
  (->> (current-message)
    (s-replace "The Angry Police Captain" "Hobbs") ; actually, let's do angry Hobbs quotes
    post-to-slack
    message))

(cam/define-keys nil
  "H-M-p" #'post-to-slack
  "H-M-P" #'angry-police-captain-to-slack
  "H-M-u" #'set-slack-user
  "H-M-c" #'set-slack-channel)

;; (require 'json)
;; (-let (((response-json . response-info)
;;         (http-post-simple-internal
;;          "https://slack.com/api/auth.test" ; chat.postMessage
;;          (json-encode '(:token "xoxp-2170866713-2715807697-3066816744-33b4bb"
;;                                :username "hobbs"
;;                                :channel "data"
;;                                :text "HELLO"
;;                                :icon_url "https://s3-us-west-2.amazonaws.com/slack-files2/avatars/2014-11-14/3015677761_9539e16a0d353e4f17c4_48.jpg"))
;;          'utf-8
;;          `(("Content-Type"
;;             .
;;             ,(http-post-content-type
;;               "application/json"
;;               'utf-8))))))
;;   (->> response-json
;;     json-read-from-string)
;;   ;; response-info
;;   )

;; (defun json-encode-dict (&rest kwargs)
;;   "Encode KWARGS :key value into a JSON dict."
;;   (->> kwargs
;;     (-partition 2)
;;     (mapcar (-lambda ((kar kdr)) ; convert '(k v) -> '(k . v)
;;               (cons kar kdr)))
;;     json-encode))

;; (json-encode-dict
;;  :a "COOL"
;;  :b "VERY COOL"
;;  :c "SICK")

(provide 'slack-mode)
;;; slack-mode.el ends here
