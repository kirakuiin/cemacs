;;; keybindings.el --- kirakuiin layer keybindings file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: kirakuiin <wang.zhuowei@foxmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; org-mode映射
(spacemacs/declare-prefix-for-mode 'org-mode "mk" "kiramap")

(spacemacs/set-leader-keys-for-major-mode 'org-mode
                                          "kd" 'evil-insert-digraph
                                          "kr" 'org-refile
                                          "kc" 'org-columns
                                          "ks" 'org-sort
                                          )

(message "kirakuiin keybindings.el loaded")
