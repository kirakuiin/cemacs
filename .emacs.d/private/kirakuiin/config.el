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
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 4))))
;; setting of c++ indent
;; (setq-default indent-tabs-mode nil)
;; (setq-default c-basic-offset 4)
