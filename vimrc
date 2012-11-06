" Basic Settings {{{
set nocompatible
syntax enable
set encoding=utf-8
set showcmd ruler number numberwidth=6
set history=1000
set scrolloff=3
set wildignore+=*~,*.tar.*,*.tgz
set listchars=tab:▸\ ,eol:$

call pathogen#infect()
call pathogen#helptags()
" }}}

" Status Line {{{
set laststatus=2
set statusline=%n\       " Buffer number
set statusline+=%f\      " Path to the file
set statusline+=%m\      " Modified flag 
set statusline+=%r\      " Read-Only flag
set statusline+=%=       " Switch to the right side
set statusline+=%B\      " Hex-value of a current char
set statusline+=%l:%c    " Current line and col
set statusline+=/        " Seperator
set statusline+=%L\      " Total lines
set statusline+=%y       " File type
" }}}

" Keymappings {{{
let mapleader = ","
let maplocalleader = ","
nnoremap <C-l> viwUw
inoremap <C-l> <Esc>viwUwa
nnoremap <C-s> :w<CR>
inoremap <C-s> <Esc>:w<CR>a
nnoremap <leader>b :CtrlPBuffer<CR>
nnoremap <leader>d :CtrlPBookmarkDir<CR>
nnoremap <leader>ev :split $MYVIMRC<CR>
nnoremap <leader>f :set fullscreen!<CR>
nnoremap <leader>g :Ack<CR>
nnoremap <leader>h :nohlsearch<CR>
nnoremap <leader>j :lnext<CR>
nnoremap <leader>k :lprev<CR>
nnoremap <leader>l :set list!<CR>
nnoremap <leader>m :CtrlPMixed<CR>
nnoremap <leader>s :call <SID>StripTrailingSpaces()<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>
nnoremap <leader><leader> <C-^>
let g:ctrlp_map = '<leader>t'

" make it harder to do bad habits
inoremap jk <esc>
inoremap <esc> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

" Window navigation
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

" start new undo seq before some kill keys
inoremap <C-u> <C-g>u<C-u>
inoremap <C-w> <C-g>u<C-w>
" }}}

" Abbrevs {{{
iabbrev <// </<C-X><C-O>
" }}}

" Whitespace & Searching {{{
set tabstop=4 softtabstop=4 shiftwidth=4 expandtab
set backspace=indent,eol,start
set autoindent shiftround
set hlsearch incsearch ignorecase smartcase
" }}}

" User Interface {{{
set wildignore+=node_modules/*,*.min.js               " Javascript
set wildignore+=*.pyc,dist/*,build/*,*.egg-info,*.egg " Python
set wildmode=longest,list
set wildmenu
set hidden                                        " Permit hidden buffers
set t_Co=256                                      " 256 colors
set background=dark
colorscheme candycode

" GUI specific options
set visualbell guioptions-=T guioptions-=L guioptions-=r
set guifont=Monaco:h12
set linespace=1

if has('mouse')
  set mouse=a
endif
" }}}

" Functions {{{
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
  setlocal expandtab
endfunction
" }}}

" snipMate {{{
let g:snipMate = {}
let g:snipMate.scope_aliases = {}
let g:snipMate.scope_aliases.dustjs = 'dustjs,html'
" }}}

if has("autocmd")
  filetype plugin indent on

  " General programming autocmds {{{
  augroup programming_au
    autocmd!
    autocmd FileType ruby,vim,jade,stylus,javascript,html setlocal ts=2 sts=2 sw=2
    autocmd FileType html setlocal nowrap
    autocmd FileType snippet,snippets setlocal noexpandtab
    autocmd BufEnter *.rss,*.atom,*.odrl setfiletype xml
    autocmd BufEnter *.md setfiletype markdown
    autocmd BufEnter *.arc setfiletype arc
    autocmd BufEnter volofile setfiletype javascript
    autocmd BufWritePre *.py,*.js,*.rb,*.lisp :call <SID>StripTrailingSpaces()
    " Jump to last cursor position
    autocmd BufReadPost *
      \ if line("'\"") > 1 && line("'\"") <= line("$") |
      \   exe "normal! g`\"" |
      \ endif
  augroup END
  " }}}

  " Vim autocmds  {{{
  augroup vim_au
    autocmd!
    autocmd InsertEnter * set cursorline
    autocmd InsertLeave * set nocursorline
  augroup END
  " }}}

  " Vimscript file settings {{{
  augroup filetype_vim
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
  augroup END
  " }}}
endif
