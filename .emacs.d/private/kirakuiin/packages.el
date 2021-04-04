;;; packages.el --- kirakuiin layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: kirakuiin <wang.zhuowei@foxmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst kirakuiin-packages
  '(
    org-edna ;; org todo高级依赖库
    )
  )

(defun kirakuiin/init-org-depend ()
  (use-package org-edna
    :defer t
    :init
    ))

;;; packages.el ends here
