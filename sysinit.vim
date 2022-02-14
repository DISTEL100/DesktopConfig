call plug#begin(stdpath('data') . '/plugged')
Plug 'davidgranstrom/scnvim', { 'do': {-> scnvim#install() } }
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'mbbill/undotree'
Plug 'lervag/vimtex'
Plug 'neovimhaskell/haskell-vim' 
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/nvim-cmp'
Plug 'quangnguyen30192/cmp-nvim-ultisnips'
Plug 'rakr/vim-one'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
call plug#end()

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

let g:airline_theme='one'
let g:airline#extensions#tabline#enabled = 1

let g:UltiSnipsSnippetDirectories = ['UltiSnips', 'scnvim-data']
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

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

set undofile
set undodir=$HOME/.config/nvim/undo//
set backupdir=$HOME/.config/nvim/backup//
set directory=$HOME/.config/nvim/swap//
set backup
set writebackup
set backupcopy=yes
"Meaningful backup name, ex: filename@2015-04-05.14:59
au BufWritePre * let &bex = '@' . strftime("%F.%H")


set completeopt=menu,menuone,noselect

lua <<EOF
  -- Setup nvim-cmp.
  local cmp = require'cmp'

  cmp.setup({
    snippet = {
      expand = function(args)
        vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
      end,
    },
    mapping = {
     ["<C-p>"] = cmp.mapping.select_prev_item(),
      ["<C-n>"] = cmp.mapping.select_next_item(), 
      ['<C-d>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
      ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
      ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
      ['<C-y>'] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
      ['<C-e>'] = cmp.mapping({
        i = cmp.mapping.abort(),
        c = cmp.mapping.close(),
      }),
      ['<TAB>'] = cmp.mapping.confirm({ select = true }),
    },
    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
      { name = 'ultisnips' }, -- For ultisnips users.
    }, {
      { name = 'buffer' },
    })
  })

  -- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline('/', {
    sources = {
      { name = 'buffer' }
    }
  })

  -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline(':', {
    sources = cmp.config.sources({
      { name = 'path' }
    }, {
      { name = 'cmdline' }
    })
  })

  -- Setup lspconfig.
  local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
  require('lspconfig')['texlab'].setup {
    capabilities = capabilities
  }
    require('lspconfig')['hls'].setup {
    capabilities = capabilities
  }
EOF


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
" utilsnip
"
" Trigger configuration. You need to change this to something other than <tab> if you use one of the following:
" - https://github.com/Valloric/YouCompleteMe
" - https://github.com/nvim-lua/completion-nvim
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"
