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
