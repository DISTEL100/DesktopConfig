tnoremap <Esc> <C-\><C-n>
inoremap kj <esc>l
nnoremap ö :w<return>
nnoremap Ö :
nnoremap <tab><tab> :bn<return>
map <C-B> <Esc>:Buffers<return>
noremap <F5> :UndotreeT<return>

nnoremap k gk
noremap j gj

inoremap <C-l> <esc>ea

noremap s cl

set langmap=ü{,Ü[,ä},Ä],#`
set langremap

nmap <C-ü> <C-]>

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
