tnoremap <Esc> <C-\><C-n>
inoremap kj <esc>l
nnoremap ö :w<return>
nnoremap Ö :
noremap <F5> :UndotreeT<return>

nnoremap k gk
noremap j gj

inoremap <C-l> <esc>ea

noremap s cl

set langmap=ü{,Ü[,ä},Ä],#`
set langremap

nmap <C-ü> <C-]>

nnoremap <silent> <Leader>r :Rg <C-R><C-W><CR>
nnoremap <silent> <Leader>b :Buffers <CR>
nnoremap <silent> <Leader>f :Files <CR>
nnoremap <silent> <Leader>m :Marks <CR>
nnoremap <silent> <Leader>e :E <CR>

map [[ ?{<CR>w99[{
map ][ /}<CR>b99]}
map ]] j0[[%/{<CR>
map [] k$][%?}<CR>

inoremap { {}<Esc>ha
inoremap ( ()<Esc>ha
inoremap [ []<Esc>ha
inoremap " ""<Esc>ha
inoremap ' ''<Esc>ha
inoremap ` ``<Esc>ha
