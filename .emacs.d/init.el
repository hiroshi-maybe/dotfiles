;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; author: Hiroshi Kori
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load Path
(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
   (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
       (normal-top-level-add-subdirs-to-load-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indent 2012/1/17
(setq tab-width 2
      indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; key map
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-d" 'delete-char)
(global-set-key [(meta D)] 'backward-kill-word) ;; (meta d) is opposite

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; major mode

;;; js2-mode 2012/1/17
; http://code.google.com/p/js2-mode/wiki/InstallationInstructions
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq-default c-basic-offset 2) ; for js2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PHP-mode 2012/7/4
; http://blog.asial.co.jp/190
(autoload 'php-mode "php-mode")
(setq auto-mode-alist
      (cons '("\\.php\\'" . php-mode) auto-mode-alist))
(setq php-mode-force-pear t)
;(add-hook 'php-mode-user-hook
;  '(lambda ()
;     (setq php-manual-path "/usr/local/share/php/doc/html")
;     (setq php-manual-url "http://www.phppro.jp/phpmanual/")))

;;; ruby-mode
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Haskell-mode 2012/11/23
;;; http://projects.haskell.org/haskellmode-emacs/
(autoload 'haskell-mode "haskell-mode")
(setq auto-mode-alist
      (append '(("\\.hs$" . haskell-mode)) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backup
;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)
;;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text Encoding (UTF-8)
;(require 'un-define)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input Device
;; meta key 2012/1/17
;; http://homepage.mac.com/zenitani/emacs-j.html
;; for mac only
(setq mac-option-modifier 'meta)
;;ホイルマウスの設定
(mouse-wheel-mode t)
(setq mouse-wheel-follow-mouse t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Appearance 2012/1/18
;; リージョンに色をつける
(setq transient-mark-mode t)
;; スタートアップメッセージを非表示
(setq inhibit-startup-screen t)
;; tool-barを非表示。コンソールでは不要
(tool-bar-mode 0)
;; scroll-barを非表示。コンソールでは不要
(scroll-bar-mode 0)
;; menu-barを非表示
(menu-bar-mode 0)
;; 色分けの設定
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq fast-lock nil)
(setq lazy-lock nil)
(setq jit-lock t)
