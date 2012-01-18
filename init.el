;;; ライブラリロード
(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
   (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
       (normal-top-level-add-subdirs-to-load-path)))

;;; indent 2012/1/17
(setq tab-width 2
      indent-tabs-mode nil)

;;; meta key 2012/1/17
; http://homepage.mac.com/zenitani/emacs-j.html
; for mac only
(setq mac-option-modifier 'meta)

;;; js2-mode 2012/1/17
; http://code.google.com/p/js2-mode/wiki/InstallationInstructions

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq-default c-basic-offset 2) ; for js2


;;; ruby-mode ロード
; http://jutememo.blogspot.com/2008/06/meadow-ruby-mode.html
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
 (setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                                     interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
            (inf-ruby-keys)))

;;; 色分けの設定
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq fast-lock nil)
(setq lazy-lock nil)
(setq jit-lock t)

;;; C-Perl モード使用
;(defalias 'perl-mode 'cperl-mode)

;;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)

;;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)

;;; UTF-8
;(require 'un-define)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(prefer-coding-system 'utf-8)

(setq default-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

;;ホイルマウスの設定
(mouse-wheel-mode t)
(setq mouse-wheel-follow-mouse t)

;; リージョンに色をつける
(setq transient-mark-mode t)

;; emacs git
;(require 'egg)

(put 'upcase-region 'disabled nil)
