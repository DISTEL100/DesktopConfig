local Plug = vim.fn['plug#']
vim.call('plug#begin', '~/.config/nvim/plugged')
Plug( 'junegunn/fzf', { ['do'] = vim.fn['fzf#install'] } )
Plug( 'junegunn/fzf.vim')
Plug( 'xolox/vim-misc')
Plug( 'xolox/vim-colorscheme-switcher')
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
Plug( 'rafamadriz/friendly-snippets' )
Plug( 'vim-airline/vim-airline' )
Plug( 'vim-airline/vim-airline-themes' )
Plug( 'nvim-lua/plenary.nvim' )
Plug( 'lewis6991/gitsigns.nvim' )
Plug( 'uiiaoo/java-syntax.vim' )
vim.call('plug#end')

require('nvim-lspconfig')
require('keymappings')
require('nvim-cmp')
require('gitsigns').setup()

vim.cmd([[
set undofile
set undodir=$HOME/.config/nvim/undo//
set backupdir=$HOME/.config/nvim/backup//
set directory=$HOME/.config/nvim/swap//
set backup
set writebackup
set backupcopy=yes
au BufWritePre * let &bex = '@' . strftime("%F.%H")

au TextYankPost * silent! lua vim.highlight.on_yank()

set number
set termguicolors   
colorscheme peachpuff
let g:airline_theme='tomorrow'
let g:airline#extensions#tabline#enabled = 1

highlight Cursor guifg=white guibg=black
highlight iCursor guifg=white guibg=orange
highlight rCursor guifg=orange guibg=magenta
set guicursor=n-v-c:block-Cursor
set guicursor+=i:ver100-iCursor
set guicursor+=n-v-c:blinkon100
set guicursor+=i:blinkwait1

set clipboard=unnamedplus

set expandtab
set softtabstop=4
set shiftwidth=4
set tabstop=4

set mouse=a
set wrap linebreak
set breakindent

let g:netrw_liststyle=0
let g:netrw_banner=0
let g:netrw_winsize=80
let g:netrw_browse_split=4
let g:netrw_preview=1
let g:netrw_altv =1
let g:netrw_sizestyle='H'
let g:netrw_fastbrowse=0
]])
