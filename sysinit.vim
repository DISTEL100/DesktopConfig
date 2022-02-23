inoremap kj <esc>l
nnoremap ö :w<return>
nnoremap Ö :
nnoremap <tab><tab> :bn<return>

set termguicolors   

let $FZF_DEFAULT_COMMAND = 'ag --hidden --ignore .git -l -g ""'

colorscheme morning

highlight Cursor guifg=white guibg=black
highlight iCursor guifg=white guibg=orange
highlight rCursor guifg=orange guibg=magenta
set guicursor=n-v-c:block-Cursor
set guicursor+=i:ver100-iCursor
set guicursor+=n-v-c:blinkon100
set guicursor+=i:blinkwait1

let g:netrw_liststyle=3
let g:netrw_banner=0
command! Ll Lexplore | vert res 30
let g:netrw_browse_split = 4
let g:netrw_altv = 1

nmap <C-S> <Plug>(scnvim-send-line)
set confirm
set clipboard=unnamedplus

noremap <F5> :UndotreeT<return>

filetype plugin on
filetype indent on

set number relativenumber
set mouse=a
set wrap linebreak
set breakindent
set nostartofline
set wildmenu

nnoremap k gk
noremap j gj

set undofile
set undodir=$HOME/.config/nvim/undo//
set backupdir=$HOME/.config/nvim/backup//
set directory=$HOME/.config/nvim/swap//
set backup
set writebackup
set backupcopy=yes
"Meaningful backup name, ex: filename@2015-04-05.14:59
au BufWritePre * let &bex = '@' . strftime("%F.%H")

" vimtex
"
let g:vimtex_fold_enabled=1
let g:vimtex_complete_enabled=1
let g:vimtex_indent_enabled=1
set fillchars=fold:\ 
let g:vimtex_view_general_viewer = 'evince'
    let g:vimtex_compiler_latexmk = {
\ 'build_dir' : '.tex_build',
        \ 'callback' : 1,
        \ 'continuous' : 1,
        \ 'executable' : 'latexmk',
        \ 'hooks' : [],
        \ 'options' : [
        \   '-verbose',
        \   '-file-line-error',
        \   '-synctex=1',
        \   '-interaction=nonstopmode',
\ ],
        \}
<
