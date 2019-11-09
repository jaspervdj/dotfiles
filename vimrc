" Vundle
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/vundle'
Plugin 'scrooloose/nerdtree'
Plugin 'mileszs/ack.vim'
Plugin 'Align'
Plugin 'bronson/vim-trailing-whitespace'
Plugin 'renamer.vim'
Plugin 'leafgarland/typescript-vim'
Plugin 'kien/ctrlp.vim'
call vundle#end()
filetype plugin indent on

" vim does not support fish (yet)
set shell=/bin/bash

" swp files to ~/.vim/backups
set directory=~/.vim/backup
set backupdir=~/.vim/backup

" Global indenting options
set autoindent
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
if exists('+breakindent')
  set breakindent
endif

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
autocmd BufRead,BufNewFile *.purs set filetype=haskell
function HaskellHook()
    noremap <C-i> :!ghci -Wall '%'<CR>
    noremap <C-c> :%!stylish-haskell<CR>
    setlocal makeprg=cabal-make
endfunction

" Options for go
autocmd FileType go call GoHook()
function GoHook()
    noremap <C-c> :%!gofmt<CR>
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

" Cut/paste lines with xsel
:map <leader>p o<Esc>:.!xsel -o<Cr>
:map <leader>c :!xsel -i<Cr>

" grep using git
set grepprg=git\ grep\ --line-number\ --column
set grepformat^=%f:%l:%c:%m
