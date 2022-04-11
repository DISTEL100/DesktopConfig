set undofile
set undodir=$HOME/.config/nvim/undo//
set backupdir=$HOME/.config/nvim/backup//
set directory=$HOME/.config/nvim/swap//
set backup
set writebackup
set backupcopy=yes
au BufWritePre * let &bex = '@' . strftime("%F.%H")

au TextYankPost * silent! lua vim.highlight.on_yank()

set title
set number
set nornu
set cursorline
set termguicolors   
colorscheme desert
let g:airline_theme='tomorrow'

highlight Cursor guifg=white guibg=black
highlight iCursor guifg=white guibg=orange
highlight rCursor guifg=orange guibg=magenta
set guicursor=n-v-c:block-Cursor
set guicursor+=i:ver100-iCursor
set guicursor+=n-v-c:blinkon100
set guicursor+=i:blinkwait1

set clipboard=unnamed

set mps+=<:>
set mouse=a
set wrap linebreak
set breakindent

set tabstop=4
set shiftwidth=4
set smarttab

let g:netrw_liststyle=0
let g:netrw_banner=0
let g:netrw_winsize=80
let g:netrw_browse_split=0
let g:netrw_preview=1

let g:netrw_altv =1
let g:netrw_sizestyle='H'
let g:netrw_fastbrowse=0
