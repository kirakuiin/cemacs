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

(setq-default evil-escape-key-sequence "jk")
(setq-default lsp-restart 'auto-restart)
(setq-default gc-cons-threshold 10000000);
(setq-default python-indent-offset 4)
(setq-default python-indent 4)

;; Org

;;;; Org Hook
(add-hook 'org-mode-hook
          (lambda ()
            (message "org-mode-hook...")
            (setq org-link-search-must-match-exact-headline nil ;; org文件链接跳转不再仅匹配标题
                  org-enforce-todo-dependencies t ;; 子任务未完成父任务无法完成
                  org-enforce-todo-checkbox-dependencies t ;; 子选框未完成任务无法完成
                  )))

;; C++

(message "kirakuiin config.el loaded")
