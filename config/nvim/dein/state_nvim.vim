if g:dein#_cache_version !=# 420 || g:dein#_init_runtimepath !=# '/Users/yuri/.config/nvim,/opt/homebrew/etc/xdg/nvim,/etc/xdg/nvim,/Users/yuri/.local/share/nvim/site,/opt/homebrew/share/nvim/site,/usr/local/share/nvim/site,/usr/share/nvim/site,/opt/homebrew/Cellar/neovim/0.8.0/share/nvim/runtime,/opt/homebrew/Cellar/neovim/0.8.0/lib/nvim,/usr/share/nvim/site/after,/usr/local/share/nvim/site/after,/opt/homebrew/share/nvim/site/after,/Users/yuri/.local/share/nvim/site/after,/etc/xdg/nvim/after,/opt/homebrew/etc/xdg/nvim/after,/Users/yuri/.config/nvim/after,/Users/yuri/.config/nvim/dein/repos/github.com/Shougo/dein.vim' | throw 'Cache loading error' | endif
let [s:plugins, s:ftplugin] = dein#min#_load_cache_raw(['/Users/yuri/dotfiles/config/nvim/init.vim', '/Users/yuri/.config/nvim/toml/dein.toml', '/Users/yuri/.config/nvim/toml/dein_lazy.toml'])
if empty(s:plugins) | throw 'Cache loading error' | endif
let g:dein#_plugins = s:plugins
let g:dein#ftplugin = s:ftplugin
let g:dein#_base_path = '/Users/yuri/.config/nvim/dein'
let g:dein#_runtime_path = '/Users/yuri/.config/nvim/dein/.cache/init.vim/.dein'
let g:dein#_cache_path = '/Users/yuri/.config/nvim/dein/.cache/init.vim'
let g:dein#_on_lua_plugins = {}
let &runtimepath = '/Users/yuri/.config/nvim,/opt/homebrew/etc/xdg/nvim,/etc/xdg/nvim,/Users/yuri/.local/share/nvim/site,/opt/homebrew/share/nvim/site,/usr/local/share/nvim/site,/usr/share/nvim/site,/Users/yuri/.config/nvim/dein/repos/github.com/Shougo/dein.vim,/Users/yuri/.config/nvim/dein/.cache/init.vim/.dein,/opt/homebrew/Cellar/neovim/0.8.0/share/nvim/runtime,/Users/yuri/.config/nvim/dein/.cache/init.vim/.dein/after,/opt/homebrew/Cellar/neovim/0.8.0/lib/nvim,/usr/share/nvim/site/after,/usr/local/share/nvim/site/after,/opt/homebrew/share/nvim/site/after,/Users/yuri/.local/share/nvim/site/after,/etc/xdg/nvim/after,/opt/homebrew/etc/xdg/nvim/after,/Users/yuri/.config/nvim/after'
filetype off
