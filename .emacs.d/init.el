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
(setq tab-width 4
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
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(setq-default c-basic-offset 2) ; for js2

;;; PHP-mode 2012/7/4
; http://blog.asial.co.jp/190
(autoload 'php-mode "php-mode")
(setq auto-mode-alist
      (cons '("\\.php\\'" . php-mode) auto-mode-alist))
(setq php-mode-force-pear t)

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

;;; Haskell-mode 2012/11/23
;;; http://projects.haskell.org/haskellmode-emacs/
(autoload 'haskell-mode "haskell-mode")
(setq auto-mode-alist
      (append '(("\\.hs$" . haskell-mode)) auto-mode-alist))

;;; Scala-mode 2013/02/23
;;; http://blog.iss.ms/2012/06/02/101357
(autoload 'scala-mode "scala-mode")
(setq auto-mode-alist
      (append '(("\\.scala$" . scala-mode)) auto-mode-alist))


;;; web-mode.el for jsx
;;; CAUTION: web-mode.el is not compatible with emacs 22.x. Load by emacs 24.x.
(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backup
;; Prevent auto creation of *.~ backup
(setq make-backup-files nil)
;; Prevent auto creation of .#* backup
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
;; mouse wheel
(mouse-wheel-mode t)
(setq mouse-wheel-follow-mouse t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Appearance 2012/1/18
;; Coloring region
(setq transient-mark-mode t)
;; Hide startup message
(setq inhibit-startup-screen t)
;; Hide tool-bar
(tool-bar-mode 0)
;; Hide scroll-bar
(scroll-bar-mode 0)
;; Hide menu-bar
(menu-bar-mode 0)
;; More coloring
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq fast-lock nil)
(setq lazy-lock nil)
(setq jit-lock t)

(put 'upcase-region 'disabled nil)
