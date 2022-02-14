local Plug = vim.fn['plug#']
vim.call('plug#begin', '~/.config/nvim/plugged')
Plug( 'junegunn/fzf', { ['do'] = vim.fn['fzf#install'] } )
Plug( 'junegunn/fzf.vim')
Plug( 'mbbill/undotree')
Plug( 'lervag/vimtex')
Plug( 'neovimhaskell/haskell-vim' )
Plug( 'neovim/nvim-lspconfig' )
Plug( 'hrsh7th/cmp-nvim-lsp')
Plug( 'hrsh7th/cmp-buffer')
Plug( 'hrsh7th/cmp-path')
Plug( 'hrsh7th/cmp-cmdline')
Plug( 'hrsh7th/nvim-cmp')
Plug( 'hrsh7th/cmp-vsnip')
Plug( 'hrsh7th/vim-vsnip')
Plug( 'vim-airline/vim-airline' )
Plug( 'vim-airline/vim-airline-themes' )
vim.call('plug#end')

require('nvim-lspconfig')
require('keymappings')
require('nvim-cmp')

vim.cmd([[
set undofile
set undodir=~/.config/nvim/undo

set termguicolors   

colorscheme morning

highlight Cursor guifg=white guibg=black
highlight iCursor guifg=white guibg=orange
highlight rCursor guifg=orange guibg=magenta
set guicursor=n-v-c:block-Cursor
set guicursor+=i:ver100-iCursor
set guicursor+=n-v-c:blinkon100
set guicursor+=i:blinkwait1

let g:airline_theme='one'
let g:airline#extensions#tabline#enabled = 1

set clipboard=unnamedplus

noremap <F5> :UndotreeT<return>

set mouse=a
set wrap linebreak
set breakindent
]])
