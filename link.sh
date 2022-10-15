ln -s $HOME/dotfiles/config/.emacs.d/elisp $HOME/.emacs.d/elisp
ln -s $HOME/dotfiles/config/.emacs.d/init.el $HOME/.emacs.d/init.el
ln -s $HOME/dotfiles/config/vscode/settings.json $HOME/Library/Application\ Support/Code/User/settings.json

zsh
ln -s $HOME/.zplugin/plugins/sorin-ionescu---prezto/ .zprezto
setopt EXTENDED_GLOB
for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
  ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done
