# EC2 Setup LAMP

## EC2

### Elastic IP

### セキュリティグループ

```
HTTP: 80
SSL: 22
```

## アクセス

### pem ファイル

```sh
chmod 400 hoge.pem
```

### ~/.ssh/config

```
ServerAliveInterval 15

Host [ホスト名]
    HostName [IPアドレス]
    User ec2-user
    IdentityFile ~/.ssh/[pem ファイル]
    RemoteForward 52698 127.0.0.1:52698
```

## サーバー設定

```sh
# 最新の EPEL リポジトリ
sudo amazon-linux-extras install epel
```

### root でログインし直して、パスワードを設定する

```sh
sudo su -
passwd
# パスワードを設定
```

### ミドルウェア

#### ZSH

##### インストール

```sh
sudo yum install zsh git

# /usr/bin/zsh を確認
cat /etc/shell
sudo emacs /etc/passwd
```

```
# 以下に変更。間違えたらログインできなくなるので注意。
# 間違えてもやり直せるよう、セッションを切らずに ZSH の確認を行う
[username]:x :1634231:100 :[Your Name]:/home/[username]:/bin/zsh
```

###### ~/.zshenv

```sh
# oh-my-zsh

# 1. Mac OS 環境
# export ZSH="/Users/[ユーザ名]/.oh-my-zsh"

# 2. Ubuntu 環境
# export ZSH=$HOME/.oh-my-zsh

# 3.1. CentOS (EC2) 環境
# export ZSH="/home/[ユーザ名]/.oh-my-zsh"

# 3.2. 文字化けするので EC2 では言語設定
# export LC_CTYPE=$LANG

# 追加したソフトやパッケージを扱うためにパスを通す
export PATH=/usr/local/bin:$PATH

# デフォルトのエディターを emacs に設定
export EDITOR=emacs

# デフォルトの pager を lv に設定
export PAGER='lv -c'

# 文字コードの指定
export LANG=ja_JP.UTF-8
```

###### ~/.zshrc

```sh
# oh-my-zsh で利用できるテーマを指定
ZSH_THEME="candy"

# oh my zsh で利用できるプラグインを指定
plugins=(git brew brew-cask cdd gem git rbenv vagrant zsh-syntax-highlighting zsh-completions)

# oh-my-zsh に変更を適用
source $ZSH/oh-my-zsh.sh

# zsh-completions の設定。コマンド補完機能
autoload -U compinit && compinit -u

# プロンプト表示設定
PROMPT='%~ %# '

# git のカラー表示
git config --global color.ui auto

# エイリアス
alias his='history'
alias ...='cd ../..'
alias ....='cd ../../..'
alias e="emacs"
alias v='vim'
alias vi='vim'
alias r='ruby'
alias mss='mysql.server start'
alias so='source'
alias be='bundle exec'
alias ber='bundle exec ruby'
alias zshrc='emacs ~/.zshrc'
alias zshenv='emacs ~/.zshenv'
alias initel='emacs ~/.emacs.d/init.el'

# エイリアス: git 系
alias g='git'
alias gs='git status'
alias gb='git branch'
alias gc='git checkout'
alias gct='git commit'
alias gg='git grep'
alias ga='git add'
alias gd='git diff'
alias gl='git log'
alias gcma='git checkout master'
alias gfu='git fetch upstream'
alias gfo='git fetch origin'
alias gmod='git merge origin/develop'
alias gmud='git merge upstream/develop'
alias gmom='git merge origin/master'
alias gcm='git commit -m'
alias gpo='git push origin'
alias gpom='git push origin master'
alias gst='git stash'
alias gsl='git stash list'
alias gsu='git stash -u'
alias gsp='git stash pop'

# 色を使用出来るようにする
autoload -Uz colors
colors

# 日本語ファイル名を表示可能にする
setopt print_eight_bit

# cd なしでもディレクトリ移動
setopt auto_cd

# ビープ音の停止
setopt no_beep

# ビープ音の停止(補完時)
setopt nolistbeep

# cd -<tab>で以前移動したディレクトリを表示
setopt auto_pushd

# ヒストリ(履歴)を保存、数を増やす
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

# 同時に起動した zsh の間でヒストリを共有する
setopt share_history

# 直前と同じコマンドの場合は履歴に追加しない
setopt hist_ignore_dups

# 同じコマンドをヒストリに残さない
setopt hist_ignore_all_dups

# スペースから始まるコマンド行はヒストリに残さない
setopt hist_ignore_space

# ヒストリに保存するときに余分なスペースを削除する
setopt hist_reduce_blanks

# キーバインディングを emacs 風にする
bindkey -d
bindkey -e

# 補完で小文字でも大文字にマッチさせる
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# <Tab> でパス名の補完候補を表示したあと、
# 続けて <Tab> を押すと候補からパス名を選択できるようになる
# 候補を選ぶには <Tab> か Ctrl-N,B,F,P
zstyle ':completion:*:default' menu select=1

# コマンドのスペルを訂正する
setopt correct

# cd した先のディレクトリをディレクトリスタックに追加する
# ディレクトリスタックとは今までに行ったディレクトリの履歴のこと
# `cd +<Tab>` でディレクトリの履歴が表示され、そこに移動できる
setopt auto_pushd

# pushd したとき、ディレクトリがすでにスタックに含まれていればスタックに追加しない
setopt pushd_ignore_dups

# 拡張 glob を有効にする。glob とはパス名にマッチするワイルドカードパターンのこと (たとえば `mv hoge.* ~/dir` における "*")
# 拡張 glob を有効にすると # ~ ^ もパターンとして扱われる
setopt extended_glob

# 単語の一部として扱われる文字のセットを指定する
# ここではデフォルトのセットから / を抜いたものとする (Ctrl-W でカーソル前の1単語を削除したとき、 / までで削除が止まる)
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
```

#### Emacs

##### インストール

```sh
sudo yum -y install gcc make ncurses-devel
cd /usr/local/src
sudo wget http://ftp.gnu.org/pub/gnu/emacs/emacs-26.1.tar.gz
sudo tar zxvf emacs-26.1.tar.gz
cd emacs-26.1
sudo ./configure --without-x --with-gnutls=no
sudo make
sudo make install
sudo chmod 777 /usr/local/bin/emacs-26.1
```

###### ~/.emacs.d/init.el

```
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
```

#### ripgrep

```sh
sudo yum-config-manager --add-repo=https://copr.fedorainfracloud.org/coprs/carlwgeorge/ripgrep/repo/epel-7/carlwgeorge-ripgrep-epel-7.repo
sudo yum install ripgrep
```

#### peco

```sh
cd
sudo wget https://github.com/peco/peco/releases/download/v0.5.3/peco_linux_386.tar.gz
sudo tar xzvf peco_linux_386.tar.gz
sudo rm peco_linux_386.tar.gz
cd peco_linux_386
sudo chmod +x peco
sudo cp peco /usr/local/bin
cd ..
sudo rm -r peco_linux_386/
```

##### ~/.zshrc

```sh
# fc コマンドでカレントディレクトリ以下のディレクトリを絞り込んだ後に移動する
function find_cd() {
    cd "$(find . -type d | peco)"
}
alias fc="find_cd"

# fe コマンドでカレントディレクトリ以下のファイルを絞り込んだ後に emacs で開く
function find_emacs() {
    emacs "$(find . -type f | peco)"
}
alias fe="find_emacs"

# ctrl-x で過去移動したディレクトリに移動
function peco-get-destination-from-cdr() {
  cdr -l | \
  sed -e 's/^[[:digit:]]*[[:blank:]]*//' | \
  peco --query "$LBUFFER"
}

function peco-cdr() {
  local destination="$(peco-get-destination-from-cdr)"
  if [ -n "$destination" ]; then
    BUFFER="cd $destination"
    zle accept-line
  else
    zle reset-prompt
  fi
}
zle -N peco-cdr
bindkey '^x' peco-cdr

# pk で実行中のプロセスを選択して kill
function peco-pkill() {
  for pid in `ps aux | peco | awk '{ print $2 }'`
  do
    kill $pid
    echo "Killed ${pid}"
  done
}
alias pk="peco-pkill"
```

- oh my zsh を使っていたら

```sh
# ctrl-r で peco で history 検索
function peco-select-history() {
    local tac
    if which tac > /dev/null; then
        tac="tac"
    else
        tac="tail -r"
    fi

    BUFFER=$(\history -n 1 | \
        eval $tac | \
        peco --query "$LBUFFER")
    CURSOR=$#BUFFER
    zle clear-screen
}
zle -N peco-select-history
bindkey '^r' peco-select-history
```

#### lv

```sh
sudo yum install lv -y
```

#### tig

```sh
sudo yum install tig -y
```

### ドキュメントルート

```sh
sudo chmod 775 /var/www/html
sudo chown ec2-user:ec2-user /var/www/html
```

#### フレームワークを使う場合

##### /etc/httpd/conf/httpd.conf

- symfony3 の場合

```
# 指定したディレクトリの中で探すファイル
DirectoryIndex index.html

# 119 (昔だと 290) 行目あたりで参照するドキュメントルート設定がある
DocumentRoot "/var/www/html/eglab/web"

#
# AllowOverride controls what directives may be placed in .htaccess files.
# It can be "All", "None", or any combination of the keywords:
#   Options FileInfo AuthConfig Limit
#
# AllowOverride None (.htaccess による書き換えの許可。151行目)
AllowOverride All
```

### LAMP

#### PHP

```
sudo amazon-linux-extras install -y lamp-mariadb10.2-php7.2 php7.2
sudo yum install php-common php-gd php-mysqlnd php-mbstring php-pdo php-xml
```

#### Apache

```sh
sudo yum update -y

# パッケージ自動更新
sudo yum install yum-cron -y
sudo sed -i "s/^apply\_updates.*$/apply\_updates = yes/g" /etc/yum/yum-cron.conf
sudo systemctl status yum-cron
sudo systemctl start yum-cron
sudo systemctl enable yum-cron

# タイムゾーン変更
timedatectl status
sudo timedatectl set-timezone Asia/Tokyo

# 日本語ロケール追加
localectl status
sudo localectl set-locale LANG=ja_JP.UTF-8
sudo localectl set-keymap jp106

# appache インストール
sudo yum install -y httpd mariadb-server

# Webサーバ起動
sudo service httpd start

# サーバを停止・再起動した時に httpd は止まるので、サーバ起動時は httpd も自動起動するように設定 (check config)
sudo chkconfig httpd on
```
