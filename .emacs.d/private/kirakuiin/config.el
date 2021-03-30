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
;; Python Hook
(add-hook 'python-mode-hook
          (message "python-mode-hook...")
          (lambda ()
            (setq indent-tabs-mode nil
                  tab-width 4)))
;; Org Hook
(add-hook 'org-mode-hook
          (lambda ()
            (message "org-mode-hook...")
            (setq org-link-search-must-match-exact-headline nil ;; org文件链接跳转不再仅匹配标题
                  )))

;; setting of c++ indent
;; (setq-default indent-tabs-mode nil)
;; (setq-default c-basic-offset 4)

(message "kirakuiin config.el loaded")
