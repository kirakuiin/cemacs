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
    (ipython-notebook :variables
                      ein-backend 'jupyter)
    (python :variables
            python-backend 'lsp
            python-lsp-server 'pyright
            python-test-runner 'pytest
            python-formatter 'yapf
            python-format-on-save t
            python-save-before-test nil
            python-fill-column 80
            python-sort-imports-on-save t
            )
    (lua :variables
         lua-backend 'lsp
         lua-lsp-server 'lua-language-server
         lsp-clients-lua-language-server-bin "/Users/lambda/lua-language-server/bin/macOS/lua-language-server"
         lsp-clients-lua-language-server-main-location "/Users/lambda/lua-language-server/main.lua")
    (org :variables
         org-enable-github-support t
         org-habit-graph-column 1
         org-habit-preceding-days 6
         org-habit-following-days 2
         org-habit-show-all-today t
         org-directory "~/org"
         org-default-notes-file (concat org-directory "/eureka.org")
         org-attach-method 'ln
         org-attach-store-link-p 2 ;; 0.don't store 1.origin link, 2.attachmentlink
         org-agenda-include-diary t
         org-agenda-start-with-follow-mode nil ;; 关闭自动跟随, F开启
         org-link-search-must-match-exact-headline nil ;; org文件链接跳转不再仅匹配标题
         ;; org-enforce-todo-dependencies t ;; 已用edna代替
         org-enforce-todo-checkbox-dependencies t ;; 子选框未完成任务无法完成
         org-pomodoro-length 25
         org-pomodoro-short-break-length 5
         org-pomodoro-long-break-length 10
         org-pretty-entities t  ;; 默认显示上下标
         org-pretty-entities-include-sub-superscripts t
         org-use-sub-superscripts '{} ;; 强制上下标语法加大括号
         )
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
         lsp-enable-indentation nil
         lsp-enable-file-watchers nil
         )
    )
  )

(message "kirakuiin layers.el loaded")
;;; packages.el ends here
