set nocompatible
syntax enable
set encoding=utf-8
set showcmd ruler number numberwidth=6
set history=1000
set scrolloff=3
set wildignore+=*~,*.tar.*,*.tgz
set listchars=tab:▸\ ,eol:$

let mapleader = ","
nnoremap <C-s> :w<CR>
inoremap <C-s> <Esc>:w<CR>a
nnoremap <C-u> viwUw
inoremap <C-u> <Esc>viwUwa
noremap  <leader>l :set list!<CR>
nnoremap <leader>f :set fullscreen!<CR>
nnoremap <leader>s :call <SID>StripTrailingSpaces()<CR>
nnoremap <leader>ev :vsplit $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>
" make it harder to do bad habits
inoremap jk <esc>
inoremap <esc> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

call pathogen#infect()

"" Whitespace
set tabstop=2 softtabstop=2 shiftwidth=2 expandtab
set backspace=indent,eol,start
set autoindent shiftround

"" Searching
set hlsearch incsearch ignorecase smartcase

"" Wildmenu
set wildmode=longest,list
set wildmenu

"" Buffers
set hidden

"" Windows
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

"" Mouse
set mouse=a

"" 256 Color
set t_Co=256
colorscheme xoria256

"" Invisible character colors
highlight NonText guifg=#4a4a59
highlight SpecialKey guifg=#4a4a59

if has("autocmd")
  filetype plugin indent on
  autocmd FileType python setlocal tabstop=4 softtabstop=4 shiftwidth=4
  autocmd FileType snippet,snippets setlocal noexpandtab
  autocmd BufNewFile,BufRead *.rss,*.atom setfiletype xml
  autocmd BufWritePre *.py,*.js,*.rb,*.lisp :call <SID>StripTrailingSpaces()
  autocmd InsertEnter * set cursorline
  autocmd InsertLeave * set nocursorline
endif

"" Javascript
set wildignore+=node_modules/*,*.min.js

"" Python
set wildignore+=dist/*,build/*,*.egg-info,*.egg

"" GUI
set visualbell guioptions-=T guioptions-=L guioptions-=r
set guifont=Monaco:h12

"" Functions
function! <SID>StripTrailingSpaces()
  " save last search, and cursor position
  let _s=@/
  let l = line(".")
  let c = col(".")
  %s/\s\+$//e
  let @/=_s
  call cursor(l, c)
endfunction

command! -nargs=* Stab call Stab()
function! Stab()
  let l:tabstop = 1 * input('set ts = sts = sw = ')
  if l:tabstop > 0
    let &l:sts = l:tabstop
    let &l:ts = l:tabstop
    let &l:sw = l:tabstop
  endif
  set et
endfunction

"" snipMate
let g:snipMate = {}
let g:snipMate.scope_aliases = {}
let g:snipMate.scope_aliases.dustjs = 'dustjs,html'
