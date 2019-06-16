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

# oh-my-zsh にしか対応していない
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
