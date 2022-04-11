tnoremap <Esc> <C-\><C-n>
inoremap kj <esc>l
nnoremap ö :w<return>
nnoremap Ö :
nnoremap <tab><tab> :bn<return>
map <C-B> <Esc>:Buffers<return>
noremap <F5> :UndotreeT<return>

nnoremap k gk
noremap j gj

set langmap=ü{,Ü[,ä},Ä],#`

map <C-p> :tjump<Return>

map [[ ?{<CR>w99[{
map ][ /}<CR>b99]}
map ]] j0[[%/{<CR>
map [] k$][%?}<CR>
