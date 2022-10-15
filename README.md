# dotfiles
- To build vivid environment without efforts

## Attention!
- Sorry, Under Construction

## Steps
- for Ubuntu 18 2020/04/19

```sh
# Add usual user
sudo adduser <username>
sudo gpasswd -a <username> sudo
sudo su - <username>

# SSH settings for usual user
mkdir .ssh
vi .ssh/authorized_keys
chmod 700 .ssh/
chmod 600 .ssh/authorized_keys 


# Update
sudo apt update -y
sudo apt upgrade -y

# Install ubuntu desktop
sudo apt -y install ubuntu-desktop xrdp emacs git wget curl zsh zplugin build-essential
xrdp -v
sudo sed -e 's/^new_cursors=true/new_cursors=false/g' -i /etc/xrdp/xrdp.ini
sudo systemctl restart xrdp
sudo systemctl enable xrdp-sesman.service 
systemctl list-unit-files -t service | grep xrdp
DESKTOP=/usr/share/ubuntu:/usr/local/share:/usr/share:/var/lib/snapd/desktop
cat <<EOF > ~/.xsessionrc
export GNOME_SHELL_SESSION_MODE=ubuntu
export XDG_CURRENT_DESKTOP=ubuntu:GNOME
export XDG_DATA_DIRS=${DESKTOP}
export XDG_CONFIG_DIRS=/etc/xdg/xdg-ubuntu:/etc/xdg
EOF

cat <<EOF | sudo tee /etc/polkit-1/localauthority/50-local.d/xrdp-color-manager.pkla
[Netowrkmanager]
Identity=unix-user:*
Action=org.freedesktop.color-manager.create-device
ResultAny=no
ResultInactive=no
ResultActive=yes
EOF

sudo systemctl restart polkit

# change hostname
sudo emacs /etc/hostname
sudo reboot

# dotfile Install
cd .ssh/
ssh-keygen
# Resister key to GitHub
git clone git@github.com:lilybrevec/dotfiles.git
ln -s $HOME/dotfiles/config/.emacs.d/elisp $HOME/.emacs.d/elisp
ln -s $HOME/dotfiles/config/.emacs.d/init.el $HOME/.emacs.d/init.el
git config --global user.name lilybrevec
git config --global user.mail allabrevec@gmail.com

# Setting for zsh

ln -s $HOME/.zplugin/plugins/sorin-ionescu---prezto/ .zprezto
setopt EXTENDED_GLOB
for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
  ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done

# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
cargo install exa

# Install anyenv
git clone https://github.com/anyenv/anyenv ~/.anyenv
anyenv install --init
```

## Acknowledgments
- Thanks b4b4r07
  - https://qiita.com/b4b4r07/items/b70178e021bef12cd4a2
- Thanks vintersnow
  - https://github.com/vintersnow/dotfiles
  - すぺしゃりてのお味。