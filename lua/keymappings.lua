vim.cmd([[
tnoremap <Esc> <C-\><C-n>
inoremap kj <esc>l
nnoremap ö :w<return>
nnoremap Ö :
nnoremap <tab><tab> :bn<return>
map <C-B> <Esc>:Buffers<return>
noremap <F5> :UndotreeT<return>

nnoremap k gk
noremap j gj

set langmap=ä{,Ä[,ü},Ü]
imap ä {
imap Ä [
imap ü }
imap Ü ]
imap # `
nmap <C-p> :tjump<Return>

]])
