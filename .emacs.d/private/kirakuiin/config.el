;;; config.el --- kirakuiin layer config file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: kirakuiin <wang.zhuowei@foxmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; General

(setq-default evil-escape-key-sequence "jk")
(setq-default lsp-restart 'auto-restart)
(setq-default gc-cons-threshold 10000000);
(setq-default calendar-longitude 113.51221908543394)
(setq-default calendar-latitude 23.15287104087724)

(defvar kirakuiin/holidays
  '(;;公历节日
    (holiday-fixed 1 1 "元旦")
    (holiday-fixed 2 14 "情人节")
    (holiday-fixed 3 8 "妇女节")
    (holiday-fixed 3 14 "白色情人节")
    (holiday-fixed 4 1 "愚人节")
    (holiday-fixed 5 1 "劳动节")
    (holiday-float 5 0 2 "母亲节")
    (holiday-fixed 6 1 "儿童节")
    (holiday-float 6 0 3 "父亲节")
    (holiday-fixed 9 10 "教师节")
    (holiday-fixed 10 1 "国庆节")
    (holiday-fixed 12 25 "圣诞节")
    ;; 农历节日
    (holiday-lunar 1 1 "春节" 0)
    (holiday-lunar 1 2 "春节" 0)
    (holiday-lunar 1 3 "春节" 0)
    (holiday-lunar 1 15 "元宵节" 0)
    (holiday-solar-term "清明" "清明节")
    (holiday-lunar 5 5 "端午节" 0)
    (holiday-lunar 8 15 "中秋节" 0)
    ;; 生日 -- 家人,朋友
    (holiday-lunar 12 12 "生日" 0)
    (holiday-lunar 11 17 "母亲生日" 0))
  "kirakuiin's holiday"
  )
(setq-default calendar-holidays kirakuiin/holidays) ;; block all other holidays

;; Python

(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent-offset 4
                  python-indent 4)))

;; Org

;; Set org todo keyword face
(setq org-todo-keyword-faces
      '(("TODO" . "#dc752f") ("WAIT" . "#FFFF60")
                             ("SCH" . "#4f97d7")
                             ("DONE" . "#86dc2f")
                             ("CANCELED" . "#f2241f")))

;;;; Org Hook
(add-hook 'org-mode-hook
          (lambda ()
            (message "org-mode-hook...")
            ;; Modeline setting
            (spacemacs/toggle-mode-line-org-clock-on)
            (setq org-archive-location (concat "archive/archive-"
                                               (format-time-string "%Y" (current-time))
                                               ".org_archive::")

                  org-agenda-diary-file (concat (kirakuiin/get-layer-path) "res/diary")
                  diary-file (concat (kirakuiin/get-layer-path) "res/diary")
                  ;; org-agenda-category-icon-alist `(("work" ,(concat (kirakuiin/get-layer-path) "img/work_icon.png")
                  ;;                                   nil nil :ascent center :height 15 :width 15)
                  ;;                                  ("live" ,(concat (kirakuiin/get-layer-path) "img/live_icon.png")
                  ;;                                   nil nil :ascent center :height 15 :width 15))
                  org-capture-templates '(("e" "Eureka" entry (file+headline "~/org/capture/eureka.org" "Inbox")
                                           (function
                                             (lambda () (kirakuiin/org-capture-templates)))
                                           :clock-resume t :kill-buffer t :clock-keep nil)
                                          ("w" "Work")
                                          ("we" "Eureka" entry (file+headline "~/org/gtd/work.org" "Inbox")
                                           (function
                                             (lambda () (kirakuiin/org-capture-templates 'task)))
                                           :clock-resume t :kill-buffer t :clock-keep nil)
                                          ("wt" "Task" entry (file+headline "~/org/gtd/work.org" "Next Action")
                                           (function
                                             (lambda () (kirakuiin/org-capture-templates 'schedule)))
                                           :clock-resume t :kill-buffer t :clock-keep nil)
                                          ("wa" "Appointment" entry (file+headline "~/org/gtd/work.org" "Appointment")
                                           (function
                                             (lambda () (kirakuiin/org-capture-templates 'appointment)))
                                           :clock-resume t :kill-buffer t :clock-keep nil)
                                          ("wp" "Project" entry (file+headline "~/org/gtd/work.org" "Project")
                                           (function
                                             (lambda () (kirakuiin/org-capture-templates 'project)))
                                           :clock-resume t :kill-buffer t :clock-keep nil)
                                          ("l" "Live")
                                          ("le" "Eureka" entry (file+headline "~/org/gtd/live.org" "Inbox")
                                           (function
                                             (lambda () (kirakuiin/org-capture-templates 'task)))
                                           :clock-resume t :kill-buffer t :clock-keep nil)
                                          ("lt" "Task" entry (file+headline "~/org/gtd/live.org" "Next Action")
                                           (function
                                             (lambda () (kirakuiin/org-capture-templates 'schedule)))
                                           :clock-resume t :kill-buffer t :clock-keep nil)
                                          ("la" "Appointment" entry (file+headline "~/org/gtd/live.org" "Appointment")
                                           (function
                                             (lambda () (kirakuiin/org-capture-templates 'appointment)))
                                           :clock-resume t :kill-buffer t :clock-keep nil)
                                          ("lp" "Project" entry (file+headline "~/org/gtd/live.org" "Project")
                                           (function
                                             (lambda () (kirakuiin/org-capture-templates 'project)))
                                           :clock-resume t :kill-buffer t :clock-keep nil)
                                          )
                  )
            (kirakuiin/org-custom-varibles)
            (kirakuiin/org-pomodoro-hooks-on-winnt)
            (require 'org-habit)
            ))

;; C++

(message "kirakuiin config.el loaded")
