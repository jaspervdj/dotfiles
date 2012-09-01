" Vundle
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'
Bundle 'scrooloose/nerdtree'
Bundle 'mileszs/ack.vim'
Bundle 'Align'
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

" Fold current line to 80 characters
set formatprg=par\ -w80
set formatoptions+=t
set textwidth=80

" Nerd tree shortcut, two times enter
" so we don't have to confirm
noremap <C-t> :NERDTreeToggle<CR><CR>
let NERDTreeIgnore=[
    \ ".*\\.class$",
    \ ".*\\.o$",
    \ ".*\\.hi$"
    \ ]

" Colorscheme used.
syntax on
set background=dark

" Show line numbers
set number
set numberwidth=4

" Show matches
set showmatch

" Change buffers without saving.
set hidden

" Options for haskell
autocmd FileType haskell call HaskellHook()
autocmd BufRead,BufNewFile *.lhs call HaskellHook()
autocmd BufRead,BufNewFile *.hsc set filetype=haskell
function HaskellHook()
    noremap <C-i> :!ghci -Wall '%'<CR>
    noremap <C-c> :%!stylish-haskell<CR>
    setlocal makeprg=cabal\ build
endfunction

" Options for ruby
autocmd FileType ruby call RubyHook()
function RubyHook()
    setlocal tabstop=2
    setlocal shiftwidth=2
    noremap <C-i> :!irb -Ilib -r ./%<CR>
endfunction

" Options for lua
autocmd FileType lua call LuaHook()
function LuaHook()
    setlocal tabstop=2
    setlocal shiftwidth=2
    noremap <C-i> :!lua -l $(echo % \| sed 's/\//./' \| sed 's/\.[^\.]*$//')<CR>
endfunction

" Options for python
autocmd FileType python call PythonHook()
function PythonHook()
    noremap <C-i> :!python2.7 -i %<CR>
endfunction

" Options for java
autocmd Filetype java call JavaHook()
function JavaHook()
endfunction

" Options for OCaml
autocmd Filetype ocaml call OCamlHook()
function OCamlHook()
    setlocal tabstop=2
    setlocal shiftwidth=2
endfunction

" Options for Clojure
autocmd Filetype clojure call ClojureHook()
function ClojureHook()
    setlocal tabstop=2
    setlocal shiftwidth=2
endfunction

" Options for JavaScript/json
au BufRead,BufNewFile *.json set filetype=javascript

" Options for Prolog
au BufRead,BufNewFile *.pl set filetype=prolog
autocmd Filetype prolog call PrologHook()
function PrologHook()
    noremap <C-i> :!swipl -s '%'<CR>
endfunction

" Options for Coffee
au BufRead,BufNewFile *.coffee set filetype=coffee
autocmd Filetype coffee call CoffeeHook()
function CoffeeHook()
    setlocal tabstop=2
    setlocal shiftwidth=2
endfunction
