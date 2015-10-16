" Vundle
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'
Bundle 'scrooloose/nerdtree'
Bundle 'mileszs/ack.vim'
Bundle 'Align'
Bundle 'bronson/vim-trailing-whitespace'
Bundle 'renamer.vim'
Bundle 'leafgarland/typescript-vim'
Bundle 'kien/ctrlp.vim'
filetype plugin indent on

" swp files to ~/.vim/backups
set directory=~/.vim/backup
set backupdir=~/.vim/backup

" Global indenting options
set autoindent
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab

" Line and column number
set ruler

" Fold current line to 80 characters (or 72 if in mutt)
set textwidth=80

" Draw a subtle line at 80 columns
set colorcolumn=80
highlight ColorColumn ctermbg=6

" Nerd tree shortcut, two times enter
" so we don't have to confirm
noremap <C-t> :NERDTreeToggle<CR><CR>
let NERDTreeIgnore=[
    \ ".*\\.class$",
    \ ".*\\.o$",
    \ ".*\\.hi$",
    \ ".*\\.cmi$",
    \ ".*\\.cmx$",
    \ ]

" Colorscheme used.
syntax on
set background=light

" Show line numbers
set number
set numberwidth=4

" Show matches
set showmatch

" Change buffers without saving.
set hidden

" Enable mouse
set mouse=a

" Options for haskell
autocmd FileType haskell call HaskellHook()
autocmd BufRead,BufNewFile *.lhs call HaskellHook()
autocmd BufRead,BufNewFile *.hsc set filetype=haskell
function HaskellHook()
    noremap <C-i> :!ghci -Wall '%'<CR>
    noremap <C-c> :%!stylish-haskell<CR>
    setlocal makeprg=cabal-make
endfunction

" Options for ruby
autocmd FileType ruby call RubyHook()
function RubyHook()
    setlocal tabstop=2
    setlocal shiftwidth=2
    noremap <C-i> :!irb -Ilib -r ./%<CR>
endfunction

" Options for mails
autocmd BufRead,BufNewFile mutt-* call MailHook()
function MailHook()
    setlocal textwidth=72
    setlocal colorcolumn=72
endfunction

" Options for JavaScript/json
au BufRead,BufNewFile *.json set filetype=javascript

" Options for ludwig
au BufRead,BufNewFile *.lw set filetype=yaml
