(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;;(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("eecacf3fb8efc90e6f7478f6143fd168342bbfa261654a754c7d47761cec07c8" "a4d03266add9a1c8f12b5309612cbbf96e1291773c7bc4fb685bfdaf83b721c6" default)))
 '(delimit-columns-end 100)
 '(package-selected-packages
   (quote
    (company cider clojure-mode flymake-css yafolding dracula-theme darktooth-theme ac-emacs-eclim markdown-mode eclim scss-mode helm-spotify rjsx-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Load Dracula theme on start)
(load-theme 'dracula t)

;; Auto-revert-mode (auto refresh document when the file changes in the disk, git checkout or reset for example)
(global-auto-revert-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           FRONT-END PACKAGES AND CONFIGURATIONS                          ;;
;;                                                                                          ;;
;;    jsx - syntatic sugar for JavaScript with React                                        ;;
;;   scss - CSS extension language. "CSS with superpowers"                                  ;;
;;                                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rjsx-mode
(add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))

;; scss-mode                          !make sure to set the correct for the package!
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/scss-mode-20180123.908"))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; flymake-css
(require 'flymake-css)
(add-hook 'css-mode-hook 'flymake-css-load)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           LANGUAGE PACKAGES AND CONFIGURATIONS                           ;;
;;                                                                                          ;;
;;    tex - typesetting system "LaTeX"                                                      ;;
;;                                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LaTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq Tex-save-query nil)
(setq Tex-PDF-mode t)

;; Java
(require 'eclim)
(add-hook 'java-mode-hook 'eclim-mode)
(require 'eclimd)

;; Clojure
;; Enter cider mode when entering the clojure major mode
(add-hook 'clojure-mode-hook 'cider-mode)

;; Turn on auto-completion with Company-Mode
(global-company-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

;; Replace return key with newline-and-indent when in cider mode.
(add-hook 'cider-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

;; Show parenthesis mode
(show-paren-mode 1)

;; rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; set F9 to run Cider with a single click
(global-set-key [f9] 'cider-jack-in)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               DEFAULT Emacs CONFIGURATIONS                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set list-buffers to use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; To make tab complete, without losing the ability to manually indent
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

;; Default tab (spaces) size
(setq-default tab-width 2)
(setq tab-width 2)

(setq js2-basic-offset 2)

;; Set backtab to remove 2 spaces
(global-set-key (kbd "<backtab>") 'un-indent-by-removing-2-spaces)
(defun un-indent-by-removing-2-spaces ()
  "remove 2 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^  ")
        (replace-match "")))))

;; Delete highlited
(delete-selection-mode 1)

;; Auto-complete mode of operation
(setq ac-auto-start 3)

;; Stop auto-indent
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;; Buffer size adjustment
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; Open frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Improve frame separator line
(set-face-background 'vertical-border "gray")
(set-face-foreground 'vertical-border (face-background 'vertical-border))

(defvar yafolding-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
    (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
    (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
    map))

