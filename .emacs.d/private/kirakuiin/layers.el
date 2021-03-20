;;; layers.el --- kirakuiin layer layers file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: kirakuiin <wang.zhuowei@foxmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(configuration-layer/declare-layers
  '(
    markdown
    cmake
    multiple-cursors
    treemacs
    vimscript
    javascript
    (lua :variables
         lua-backend nil
         ;;  lua-lsp-emmy-jar-path "~/.emacs.d/EmmyLua-LS-all.jar" ; default path
         ;;  lua-lsp-emmy-java-path "java"                         ; default path
         ;;  lua-lsp-emmy-enable-file-watchers t                   ; enabled default
         )
    (org :variables
         org-enable-github-support t)
    (c-c++ :variables
           c-c++-default-mode-for-headers 'c++-mode
           c-c++-adopt-subprojects t
           c-c++-enable-clang-support t
           c-c++-enable-clang-format-on-save t
           c-c++-enable-google-newline t
           c-c++-enable-google-style t
           c-c++-backend 'lsp-clangd
           c-c++-lsp-enable-semantic-highlight 'rainbow
           lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")
    (lsp :variables
         lsp-enable-indentation nil)
    )
  )

;;; packages.el ends here
