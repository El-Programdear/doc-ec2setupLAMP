# AWS Setup LAMP

## VPC

### リージョン

- **東京リージョン** になっていることを確認する

### VPC / サブネット作成手順

デフオルトの VPC が既に起動しているが無視する

1. **VPC ウィザードの起動** (VPC + サブネットを作成)
2. 左サイドメニューから VPC 設定の選択を選ぶ
   2.1. 今回は「1 個のパブリックサブネットを持つ VPC」
3. IPv4 CIDR ブロック を `10.0.0.0/16` に設定 (デフォルトのまま)
4. VPC 名: 任意の名前
5. パブリックサブネットの IPv4 CIDR: `10.0.11.0/24`
   5.1. AZ: `1a` を選択する
   5.2. サブネット名: 任意の名前 -> ex) `[VPC 名]-public-subnet-1a`
6. **サービスエンドポイント**
   VPC の中にエンドポイントを置いて、そこにアクセスすれば S3 に繋がるみたいな設定ができる
7. DNS ホスト名を有効化: 「はい」を選択
8. ハードウェアのテナンシー: AWS のハードウェアを自分で貸し切ってもらうかどうか。「デフォルト」を選択

-> VPC を作成: Internet Gateway も付与される

画面下半分にあるメニューから**ルートテーブル**を選択できる

#### 左サイドバー > サブネット

- 先程作った VPC から追加でパブリックサブネット `[VPC 名]-public-subnet-1c` を作成する
- これはロードバランサーで使うパブリックサブネット

  - 名前タグ: `[VPC 名]-public-subnet-1c`
  - **VPC の項目はさっき作った VPC をセレクトボックスから選ぶようにする**
  - AZ: `1c` を選択
  - CIDR: `10.0.12.0/24`

- **2 つ目に追加したサブネットには Internet Gateway が自動付与されていないので注意**
  - `ルートテーブルの関連付けの編集` ボタン
  - ルートテーブルの VPC ID を先ほどの AZ:1a のもので選択して、保存する
    - 1a が利用しているルートテーブル設定が 1c にも適用される

| 送信先      | ターゲット            |                                          |
| ----------- | --------------------- | ---------------------------------------- |
| 10.0.0.0/16 | local                 | VPC の中への接続はローカルから接続可能   |
| 0.0.0.0/0   | igw-0ededde892df2a789 | それ以外は Internet Gateway から接続可能 |

#### 左サイドバー > ルートテーブル

名前を `[VPC 名]-public-rt` などと明示的にしておくとわかりやすい

##### 左サイドバー > インターネットゲートウェイ

VPC 作成後、すでにインターネットゲートウェイが 1 つできあがっている

### EC2: パブリックサブネット

#### EC2 インスタンスの作成

EC2 マネジメントコンソールでも**東京リージョン**になっていることを確認する

- インスタンス作成: 「手順 3: インスタンスの詳細の設定」

  - **ネットワーク**: 作成した VPC を選択する
  - パブリックサブネット: **1a** の方を選択する
  - 自動割り当てパブリック IP: **有効化** (固定されないが、グローバル IP が付与されるようにする)
  - **高度な詳細** をクリックする
    - **ユーザーデータ**
    - 「テキストで」にチェックを入れる
    - スクリプトを貼ることができる

- 「手順 5: タグの追加」
  - `Name` キー: `Web-1a` など分かりやすいものにしておく
- セキュリティグループの設定
  - セキュリティグループ名: Web-sg
  - 説明: Web-sg
  - HTTP はよいが **SSH はフルオープンにしない**こと
  - 自宅の IP アドレスなどを指定しておく
  - パブリックサブネットはグローバル SSH できる

#### セキュリティグループ

- Web-sg: Web セキュリティグループ名で作成
  - SSH(22) は特定の IP
    - [What Is My IP Address? - ifconfig.me](https://ifconfig.me/)
  - HTTP(80) は FULL オープン

##### セキュリティグループ名を変更したいとき

- **グループ名** は後から変更できないので、「アクション > コピーして新規作成」でリネームする
- 「アクション > ネットワーキング > セキュリティグループの変更」が可能
  - その VPC に紐づくセキュリティグループしか表示されない点に注意

#### Elastic IP

#### ログイン

```sh
chmod 400 [pem ファイル]
ssh -i [pem ファイル] ec2-user@[IPv4 パブリック IP]
```

##### ~/.ssh/config

```
ServerAliveInterval 15

Host [ホスト名]
    HostName [IPアドレス]
    User ec2-user
    IdentityFile ~/.ssh/[pem ファイル]
    RemoteForward 52698 127.0.0.1:52698
```

## サーバー設定

### root でログインし直して、パスワードを設定する

```sh
sudo su -
passwd
# パスワードを設定
```

### ミドルウェア (1)

```sh
# 最新の EPEL リポジトリ
sudo amazon-linux-extras install epel -y
# ripgrep リポジトリ
sudo yum-config-manager --add-repo=https://copr.fedorainfracloud.org/coprs/carlwgeorge/ripgrep/repo/epel-7/carlwgeorge-ripgrep-epel-7.repo

sudo yum install -y zsh git lv tig ripgrep
```

#### ZSH

##### インストール

```sh
# /usr/bin/zsh を確認
cat /etc/shells
sudo emacs /etc/passwd
```

```sh:/etc/passwd
# 以下に変更する。間違えたらログインできなくなるので注意
# 間違えてもやり直せるよう、セッションを切らずに ZSH の変更確認を行う
[username]:x :1634231:100 :[Your Name]:/home/[username]:/usr/bin/zsh
```

###### oh-my-zsh 導入

```sh
cd ~/.ssh
ssh-keygen
# GitHub に鍵登録
cat id_rsa.pub
cd
git clone git@github.com:El-Programdear/doc-ec2setupLAMP.git
chmod 777 install_oh_my_zsh.sh
./install_oh_my_zsh.sh
mv .zshenv ~
mv .zsherc ~
cd
# 編集
emacs ~/.zshenv
emacs ~/.zshrc
```

### ミドルウェア (2)

```sh
chmod 777 install_middleware.sh
./install_middleware.sh
```

#### Emacs

##### ~/.emacs.d/init.el

`~/.emacs.d` に `init.el` を配置する

##### sudo emacs を使えるようにする

```sh
sudo visudo
```

`:/usr/local/bin` を追加する

```
Defaults    secure_path = /sbin:/bin:/usr/sbin:/usr/bin:/usr/local/bin
```

#### サーバ設定

```sh
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
```

##### うるさいのでビープ音を消す

```sh:/etc/inputrc
set bell-style none
```

##### ホスト名の変更

`sudo emacs /etc/sysconfig/network`

```sh
# HOSTNAME=localhost.localdomain
HOSTNAME=udemy-aws-14days-web-1a
```

`sudo emacs /etc/hosts`

```sh
# 先頭行に追加する
127.0.0.1   udemy-aws-14days-web-1a localhost localhost.localdomain localhost4 localhost4.localdomain4
```

```sh
# EC2 再起動
sudo reboot
```

```sh
# ホスト名が変更されていれば OK
[ec2-user@udemy-aws-14days-web-1a ~]$
```

##### 言語、時刻設定

`sudo emacs /etc/sysconfig/i18n`

```
# LANG=en_US.UTF-8
LANG=ja_JP.UTF-8
```

```sh
sudo cp /usr/share/zoneinfo/Japan /etc/localtime
```

`sudo emacs /etc/sysconfig/clock`

```
# ZONE="UTC"
ZONE="Asia/Tokyo"
UTC=true
```

```sh
date

2019年  6月  1日 土曜日 16:32:08 JST
```

### パッケージ導入、LAMP 環境作成

#### 自分で用意する場合

- httpd24
  - apache
- php70
- php70-mbstring
  - 日本語を扱うためのパッケージ
- ph70-pdo
  - DB 接続クラス
- php70-mysqlnd
  - mysql 関連のファイル
- mysql
- git

```sh
sudo yum update
sudo yum install -y httpd24 php70 php70-mbstring php70-mysqlnd mysql
```

#### amazon-linux-extras を使う場合

- サンプル 1
  - PHP7.3
  - mysql

```sh
sudo amazon-linux-extras install php7.3 -y
sudo yum install mysql php-common php-gd php-mysqlnd php-mbstring php-pdo php-xml php-opacache php-apcu php-fpm -y
sudo systemctl start php-fpm
sudo systemctl enable php-fpm
```

- サンプル 2
  - PHP7.2
  - mariadb10.2

```sh
sudo amazon-linux-extras install -y lamp-mariadb10.2-php7.2 php7.2
sudo yum install php-common php-gd php-mysqlnd php-mbstring php-pdo php-xml php-opacache php-apcu php-fpm -y
```

##### nginx を使う場合

```sh
sudo yum update -y
sudo amazon-linux-extras install nginx1.12 -y
sudo systemctl start nginx
sudo systemctl enable nginx
```

```sh:/usr/share/nginx/html/phpinfo.php
<?php phpinfo(); ?>
```

```sh
sudo systemctl restart php-fpm
sudo systemctl restart nginx
```

`http://[IP]/phpinfo.php`

```sh:/etc/nginx/nginx.conf
location / {
    root /var/www/html/wordpress;
    index index.php;
}
```

```sh
sudo sed -i "s/user = apache/user = nginx/" /etc/php-fpm.d/www.conf
sudo sed -i "s/group = apache/group = nginx/" /etc/php-fpm.d/www.conf

# 文法チェック
sudo nginx -t
```

##### Apache を使う場合

```sh
sudo yum update -y
sudo amazon-linux-extras install httpd24  -y

# もしくは
sudo yum install httpd -y
```

```sh
# apache インストール
sudo yum install -y httpd mariadb-server

# Webサーバ起動
sudo service httpd start

# サーバを停止・再起動した時に httpd は止まるので、サーバ起動時は httpd も自動起動するように設定 (check config)
sudo chkconfig httpd on
```

###### PHP フレームワークを使う場合

- symfony3 の場合

```:/etc/httpd/conf/httpd.conf
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

###### 素の PHP

ドキュメントルートの変更

```sh
sudo chmod 775 /var/www/html
sudo chown ec2-user:ec2-user /var/www/html
```

- 下記ファイルで `DirectoryIndex` で検索する

```:/etc/httpd/conf/httpd.conf
<IfModule dir_module>
    # index.php を追加して優先順位を上げる
    DirectoryIndex index.php index.html
</IfModule>
```

- `ServerName` で検索して `ServerName [VPN 名]-web-1a` を記述する

```:/etc/httpd/conf/httpd.conf
#
# ServerName gives the name and port that the server uses to identify itself.
# This can often be determined automatically, but we recommend you specify
# it explicitly to prevent problems during startup.
#
# If your host doesn't have a registered DNS name, enter its IP address here.
#
#ServerName www.example.com:80
ServerName udemy-aws-14days-web-1a
```

- 文法チェック

```sh
sudo /etc/init.d/httpd configtest
Syntax OK
```

- インスタンス起動時に Apache が自動的に起動するようにする

```sh
sudo chkconfig httpd on

# 初回起動
sudo service httpd start
Starting httpd:                                            [  OK  ]
```

- 下記ファイルで `Dynamic` で検索する
  - `extension=mbstring` を追加

```:/ect/php.ini
;;;;;;;;;;;;;;;;;;;;;;
; Dynamic Extensions ;
;;;;;;;;;;;;;;;;;;;;;;

; If you wish to have an extension loaded automatically, use the following
; syntax:
;
;   extension=modulename.extension
;
; For example, on Windows:
;
;   extension=msql.dll
;
; ... or under UNIX:
;
;   extension=msql.so
;
; ... or with a path:
;
;   extension=/path/to/extension/msql.so
;
; If you only provide the name of the extension, PHP will look for it in its
; default extension directory.

extension=mbstring
```

- `mbstring.language` で検索する

```:/ect/php.ini
[mbstring]
; language for internal character representation.
; This affects mb_send_mail() and mbstring.detect_order.
; http://php.net/mbstring.language
mbstring.language = Japanese # ここのコメントアウトを外す

; Use of this INI entry is deprecated, use global internal_encoding instead.
; internal/script encoding.
; Some encoding cannot work as internal encoding. (e.g. SJIS, BIG5, ISO-2022-*)
; If empty, default_charset or internal_encoding or iconv.internal_encoding is used.
; The precedence is: default_charset < internal_encoding < iconv.internal_encoding
mbstring.internal_encoding = UTF-8 # ここのコメントアウトを外して、UTF-8 を設定する
```
