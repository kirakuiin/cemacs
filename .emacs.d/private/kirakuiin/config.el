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

;; Fix when evil-search broken cause evil-key-binding exception
(add-hook 'minibuffer-exit-hook #'evil-ex-search-stop-session)

;; Python

(setq-default python-indent-offset 4)
(setq-default python-indent 4)

;; Org

;; Set org todo keyword face
(setq org-todo-keyword-faces
      '(("TODO" . "#dc752f") ("WAIT" . "#2d9574")
                             ("SCH" . "#4f97d7")
                             ("DONE" . "#86dc2f")
                             ("CANCELED" . "#f2241f")))

;;;; Org Hook
(add-hook 'org-mode-hook
          (lambda ()
            (message "org-mode-hook...")
            ;; Modeline setting
            (spacemacs/toggle-mode-line-org-clock-on)
            (setq org-link-search-must-match-exact-headline nil ;; org文件链接跳转不再仅匹配标题
                  ;; org-enforce-todo-dependencies t ;; 已用edna代替
                  org-enforce-todo-checkbox-dependencies t ;; 子选框未完成任务无法完成
                  org-pomodoro-length 25
                  org-pomodoro-short-break-length 1
                  org-pomodoro-long-break-length 10
                  )
            (kirakuiin/org-custom-varibles)
            (kirakuiin/org-pomodoro-hooks-on-winnt)
            (require 'org-habit)
            ))

;; C++

(message "kirakuiin config.el loaded")
