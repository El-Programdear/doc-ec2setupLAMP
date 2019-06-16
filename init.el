;;
;; パッケージ読み込み
;;

(require 'package)

;; HTTPS (EC2 ではエラー)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)

;; HTTP
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)

;; marmalade は HTTP アクセスすると証明書エラーでフリーズするので注意
;; (add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

;;
;; Mac OS のクリップボードと同期する
;; ⌘V を使えば、どの環境にも貼り付けられる
;;

(defun copy-from-osx ()
(shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
(let ((process-connection-type nil))
     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;; (EC2 ではコピペできなくなることがあるので無効化したほうがいい)
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; 時間も表示
(display-time)

;; emacs テーマ選択
(load-theme 'manoj-dark t)

;; "yes or no" の選択を "y or n" にする
(fset 'yes-or-no-p 'y-or-n-p)

;; alpha
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 95)))

;; font
(add-to-list 'default-frame-alist '(font . "ricty-12"))

;; line number の表示
(require 'linum)
(global-linum-mode 1)

;; line number を分かりやすくする
(set-face-attribute 'linum nil
            :foreground "#a9a9a9"
            :background "#404040"
            :height 0.9)

;; メニューバーを非表示
(menu-bar-mode 0)

;; 現在ポイントがある関数名をモードラインに表示
(which-function-mode 1)

;; 対応する括弧をハイライト
(show-paren-mode 1)

;; リージョンのハイライト
(transient-mark-mode 1)

;; current directory 表示
(let ((ls (member 'mode-line-buffer-identification
                  mode-line-format)))
  (setcdr ls
    (cons '(:eval (concat " ("
            (abbreviate-file-name default-directory)
            ")"))
          (cdr ls))))

;; ターミナルで起動したときにメニューを表示しない
(if (eq window-system 'x)
    (menu-bar-mode 1) (menu-bar-mode 0))
(menu-bar-mode nil)

;; bufferの最後でカーソルを動かそうとしても音をならなくする
(defun next-line (arg)
  (interactive "p")
  (condition-case nil
      (line-move arg)
    (end-of-buffer)))

;; active window move
(global-set-key (kbd "<C-left>")  'windmove-left)
(global-set-key (kbd "<C-down>")  'windmove-down)
(global-set-key (kbd "<C-up>")    'windmove-up)
(global-set-key (kbd "<C-right>") 'windmove-right)

;; rgrepのheader messageを消去
(defun delete-grep-header ()
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 5)
      (narrow-to-region (point) (point-max)))))
(defadvice grep (after delete-grep-header activate) (delete-grep-header))
(defadvice rgrep (after delete-grep-header activate) (delete-grep-header))

;; "grepバッファに切り替える"
(defun my-switch-grep-buffer()
  (interactive)
    (if (get-buffer "*grep*")
            (pop-to-buffer "*grep*")
      (message "No grep buffer")))
(global-set-key (kbd "s-e") 'my-switch-grep-buffer)

;; 履歴参照
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

;; Terminal 化
(setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
(global-set-key (kbd "C-c o") 'shell-pop)

;;
;; setq
;;

;; cmdキーを superとして割り当てる
(setq mac-command-modifier 'super)
;; クリップボードへのコピー
(setq x-select-enable-clipboard t)
;; C-kで行全体を削除する
(setq kill-whole-line t)
;; スタートアップメッセージを表示させない
(setq inhibit-startup-message t)
;; エラー音をならなくする
(setq ring-bell-function 'ignore)
;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)
;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)
;; tabサイズ
(setq default-tab-width 4)
;; タイトルにフルパス表示
(setq frame-title-format "%f")
;; スタートアップメッセージを表示させない
(setq inhibit-startup-message 1)
;; scratchの初期メッセージ消去
(setq initial-scratch-message "")
;; スクロールは1行ごとに
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))
;; スクロールの加速をやめる
(setq mouse-wheel-progressive-speed nil)
;; 大文字・小文字を区別しない
(setq case-fold-search t)
;; rgrep時などに，新規にwindowを立ち上げる
(setq special-display-buffer-names '("*Help*" "*compilation*" "*interpretation*" "*grep*" ))

;;
;; define-key
;;

;; Contol H で 1文字削除
(define-key global-map "\C-h" 'delete-backward-char)
;; ヘルプの表示はこっちに変更
(define-key global-map "\M-?" 'help-for-help)
;; ファイル名検索
(define-key global-map [(super i)] 'find-name-dired)
;; ファイル内検索（いらないメッセージは消去）
(define-key global-map [(super f)] 'rgrep)

;;
;; put
;;

;; リージョンの大文字小文字変換
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;
;; 以下、個別パッケージ設定
;;

;; 使っているパッケージが自動で追加される
(custom-set-variables
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
'(helm-gtags-auto-update t)
'(helm-gtags-ignore-case t)
'(package-selected-packages
  (quote
    (flycheck-credo recentf-ext golden-ratio neotree elscreen hiwin helm-gtags use-package helm auto-complete magit))))

;;
;; helm
;;

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                  (let ((bg-color (face-background 'default nil)))
                    `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)

;; helm: helm-M-x 有効
(global-set-key (kbd "M-x") 'helm-M-x)
;; helm: あいまい検索有効
(setq helm-M-x-fuzzy-match t)
;; helm: persistent action リバインド
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; helm: make TAB works in terminal リバインド
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;; helm: list actions リバインド
(define-key helm-map (kbd "C-z")  'helm-select-action)
;; helm: バッファのサイズを候補の数に応じて自動的にリサイズ
(helm-autoresize-mode t)
;; helm: キルリングの表示
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; helm: いろいろなソースを包括的に扱えるようにする
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
;; helm: ファイル あいまい検索
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; helm: Semantic と Imenuのあいまい検索
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
;; helm: マニュアルページに素早く飛ぶ。ミニバッファにpatternを入力可能、カーソルの指すシンボルをそのまま検索可能
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
;; helm: helm-locate あいまい検索
(setq helm-locate-fuzzy-match t)
;; helm: helm-occur のリバインド
(global-set-key (kbd "C-c h o") 'helm-occur)
;; helm: apropos あいまい検索
(setq helm-apropos-fuzzy-match t)
;; helm: Emacs Lisp の関数あいまい検索
(setq helm-lisp-fuzzy-completion t)
;; helm: helm-all-mark-rings リバインド
;; (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
;; helm: Web 検索を有効
(setq helm-surfraw-default-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")
;; helm: Google 検索のサジェスト有効
(global-set-key (kbd "C-c h g") 'helm-google-suggest)

(custom-set-faces
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
)

;;
;; Auto Complete
;;

(require 'auto-complete-config)
(ac-config-default)

(add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
(add-to-list 'ac-modes 'fundamental-mode)  ;; fundamental-mode
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'yatex-mode)

(ac-set-trigger-key "TAB")

(setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)          ;; 曖昧マッチ

(global-auto-complete-mode 0.5)

;;
;; helm-gtags
;;

(require 'helm-gtags)

(add-hook 'helm-gtags-mode-hook
          '(lambda ()
            ;; do what i mean
            (local-set-key (kbd "M-.") 'helm-gtags-dwim)
            ;;入力されたタグの定義元へジャンプ
            (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
            ;;入力タグを参照する場所へジャンプ
            (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
            ;;入力したシンボルを参照する場所へジャンプ
            (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
            ;;タグ一覧からタグを選択し, その定義元にジャンプする
            (local-set-key (kbd "M-l") 'helm-gtags-select)
            ;;ジャンプ前の場所に戻る
            (local-set-key (kbd "M-,") 'helm-gtags-pop-stack)
            (local-set-key (kbd "M-g M-p") 'helm-gtags-parse-file)
            (local-set-key (kbd "C-c <") 'helm-gtags-previous-history)
            (local-set-key (kbd "C-c >") 'helm-gtags-next-history)))

;;
;; hiwin: 非アクティブウィンドウの背景色を設定
;;

(require 'hiwin)
(hiwin-activate)
(set-face-background 'hiwin-face "gray30")

;;
;; neotree（サイドバー）
;;

(require 'neotree)
(global-set-key "\C-o" 'neotree-toggle)

;;
;; golden ratio
;;

(golden-ratio-mode 1)
(add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")

;;
;; recentf (標準ライブラリなのでパッケージインストールは不要)
;;

(require 'recentf)
(setq recentf-save-file "~/.emacs.d/.recentf")
(setq recentf-max-saved-items 1000)            ;; recentf に保存するファイルの数
(setq recentf-exclude '(".recentf"))           ;; .recentf自体は含まない
(setq recentf-auto-cleanup 'never)             ;; 保存する内容を整理
(run-with-idle-timer 30 t '(lambda ()          ;; 30秒ごとに .recentf を保存
                            (with-suppressed-message (recentf-save-list))))

;;
;; recentf-ext (パッケージインストール必要)
;;

(require 'recentf-ext)
(define-key global-map [(super r)] 'recentf-open-files)

;; コメントアウト
;; 選択範囲
(global-set-key (kbd "s-;") 'comment-region)

;; コメントアウト
;; 一行
(defun one-line-comment ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (comment-or-uncomment-region (region-beginning) (region-end))))
(global-set-key (kbd "s-/") 'one-line-comment)

;; 直前のバッファに戻る
(global-set-key (kbd "s-[") 'switch-to-prev-buffer)

;; 次のバッファに進む
(global-set-key (kbd "s-]") 'switch-to-next-buffer)

;;
;; magit (パッケージインストール不要)
;;

(global-set-key (kbd "C-c C-g") 'magit-diff-working-tree)

;; ファイル編集時に，bufferを再読込
(global-auto-revert-mode 1)

;;
;; elscreen（上部タブ）
;;

(require 'elscreen)

(elscreen-start)
(global-set-key (kbd "s-t") 'elscreen-create)
(global-set-key "\C-l" 'elscreen-next)
(global-set-key "\C-r" 'elscreen-previous)
(global-set-key (kbd "s-d") 'elscreen-kill)
(set-face-attribute 'elscreen-tab-background-face nil
                    :background "grey10"
                    :foreground "grey90")
(set-face-attribute 'elscreen-tab-control-face nil
                    :background "grey20"
                    :foreground "grey90")
(set-face-attribute 'elscreen-tab-current-screen-face nil
                    :background "grey20"
                    :foreground "grey90")
(set-face-attribute 'elscreen-tab-other-screen-face nil
                    :background "grey30"
                    :foreground "grey60")
;;; [X]を表示しない
(setq elscreen-tab-display-kill-screen nil)
;;; [<->]を表示しない
(setq elscreen-tab-display-control nil)
;;; タブに表示させる内容を決定
(setq elscreen-buffer-to-nickname-alist
      '(("^dired-mode$" .
        (lambda ()
          (format "Dired(%s)" dired-directory)))
        ("^Info-mode$" .
        (lambda ()
          (format "Info(%s)" (file-name-nondirectory Info-current-file))))
        ("^mew-draft-mode$" .
        (lambda ()
          (format "Mew(%s)" (buffer-name (current-buffer)))))
        ("^mew-" . "Mew")
        ("^irchat-" . "IRChat")
        ("^liece-" . "Liece")
        ("^lookup-" . "Lookup")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")
        ("-telnet" . "telnet")
        ("dict" . "OnlineDict")
        ("*WL:Message*" . "Wanderlust")))
