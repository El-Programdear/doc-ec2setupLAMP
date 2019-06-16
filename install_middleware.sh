#!/usr/bin/zsh

# Emacs
cd
sudo yum -y install gcc make ncurses-devel
cd /usr/local/src
sudo wget http://ftp.gnu.org/pub/gnu/emacs/emacs-26.1.tar.gz
sudo tar zxvf emacs-26.1.tar.gz
cd emacs-26.1
sudo ./configure --without-x --with-gnutls=no
sudo make
sudo make install
sudo chmod 777 /usr/local/bin/emacs-26.1
cd

# peco
cd
sudo wget https://github.com/peco/peco/releases/download/v0.5.3/peco_linux_386.tar.gz
sudo tar xzvf peco_linux_386.tar.gz
sudo rm peco_linux_386.tar.gz
cd peco_linux_386
sudo chmod +x peco
sudo cp peco /usr/local/bin
cd ..
sudo rm -r peco_linux_386/
cd
