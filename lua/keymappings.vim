tnoremap <Esc> <C-\><C-n>
inoremap kj <esc>l
nnoremap ö :w<return>
nnoremap Ö :

nnoremap k gk
noremap j gj

inoremap <C-l> <esc>la

noremap s cl

set langmap=ü{,Ü[,ä},Ä],#`
set langremap

nmap <C-ü> <C-]>

nnoremap <silent> <Leader>+ :RG <C-R><C-W><CR>
nnoremap <silent> <Leader>r :RG <CR>
nnoremap <silent> <Leader>b :Buffers <CR>
nnoremap <silent> <Leader>h :History <CR>
nnoremap <silent> <Leader>f :Files <CR>
nnoremap <silent> <Leader>m :Marks <CR>
nnoremap <silent> <Leader>e :E <CR>
nnoremap <silent> <Leader>u :UndotreeT<return>

map [[ ?{<CR>w99[{
map ][ /}<CR>b99]}
map ]] j0[[%/{<CR>
map [] k$][%?}<CR>

