" Basic Settings {{{
set nocompatible
runtime bundle/pathogen/autoload/pathogen.vim
call pathogen#infect()
call pathogen#helptags()
runtime macros/matchit.vim

syntax enable
set encoding=utf-8
set showcmd ruler number numberwidth=6
set history=1000
set scrolloff=3
set listchars=tab:▸\ ,eol:$
set display=lastline
" }}}

" Status Line {{{
set laststatus=2
set statusline=%n\       " Buffer number
set statusline+=%f\      " Path to the file
set statusline+=%m\      " Modified flag 
set statusline+=%r\      " Read-Only flag
set statusline+=%=       " Switch to the right side
set statusline+=%c:%l\   " Current col and line
set statusline+=[%L]\    " Total lines
set statusline+=%B\      " Hex-value of a current char
set statusline+=%y       " File type
" }}}

" Keymappings {{{
let mapleader = ","
let maplocalleader = '\'
cnoremap %% <C-R>=expand('%:p:h').'/'<CR>
nnoremap <C-l> viwUw
inoremap <C-l> <Esc>viwUwa
 noremap <leader>ae :Tabularize /=<CR>
 noremap <leader>ac :Tabularize /:<CR>
nnoremap <leader>db :CtrlPBuffer<CR>
nnoremap <leader>dd :CtrlPBookmarkDir<CR>
nnoremap <leader>da :CtrlPBookmarkDirAdd<CR>
nnoremap <leader>dm :CtrlPMixed<CR>
    nmap <leader>ew :e %%
    nmap <leader>es :sp %%
    nmap <leader>ev :vsp %%
    nmap <leader>et :tabe %%
nnoremap <leader>f :set fullscreen!<CR>
nnoremap <leader>g :Ack<CR>
nnoremap <leader>h :nohlsearch<CR>
nnoremap <leader>l :set list!<CR>
nnoremap <leader>o :only<CR>
nnoremap <leader>s :call <SID>StripTrailingSpaces()<CR>
nnoremap <leader>v :split $MYVIMRC<CR>
nnoremap <leader>w :w<CR>
nnoremap <leader>z :match Error /\s\+$/<CR>
nnoremap <leader>Z :match Error //<CR>
nnoremap <leader>/ /\v
nnoremap <leader><leader> <C-^>
let g:ctrlp_map = '<leader>t'
nnoremap <F5> :GundoToggle<CR>

" Bubble single lines
nmap <C-up> [e
nmap <C-down> ]e
" Bubble multiple lines
vmap <C-up> [egv
vmap <C-down> ]egv

" Visually select last edited/pasted text
nnoremap gV `[v`]

" make it harder to do bad habits
inoremap jk <esc>
inoremap <esc> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
inoremap <C-c> <nop>

" Window navigation
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

" start new undo seq before some kill keys
inoremap <C-u> <C-g>u<C-u>
inoremap <C-w> <C-g>u<C-w>

" move inside visible line
noremap <D-j> gj
noremap <D-k> gk
noremap <D-$> g$
noremap <D-7> g0
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
set wildmode=longest,list
set wildmenu
set hidden                                        " Permit hidden buffers
set t_Co=16                                       " 16 colors
set background=dark
colorscheme solarized

" GUI specific options
set visualbell guioptions-=T guioptions-=L guioptions-=r
set guifont=Monaco:h12
set linespace=1

if has('mouse')
  set mouse=a
endif
" }}}

" Wildignore {{{
set wildignore+=*~,*.sw?
set wildignore+=*.tar.*,*.tgz
set wildignore+=.DS_Store
set wildignore+=node_modules/*,*.min.js               " Javascript
set wildignore+=*.pyc,dist/*,build/*,*.egg-info,*.egg " Python
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
let g:snipMate.scope_aliases.htmldjango = 'django,html'
" }}}

if has("autocmd")
  filetype plugin indent on

  " General programming autocmds {{{
  augroup programming_au
    autocmd!
    autocmd FileType ruby,vim,jade,stylus,javascript,html setl ts=2 sts=2 sw=2
    autocmd FileType html setl nowrap
    autocmd FileType snippet,snippets setlocal noexpandtab
    autocmd BufEnter *.rss,*.atom,*.odrl setf xml
    autocmd BufEnter *.md setf markdown
    autocmd BufEnter *.arc setf arc
    autocmd BufEnter *.go setl noet ts=4 sts=4 sw=4
    autocmd BufEnter *.sml setl et ts=4 sts=4 sw=4 commentstring=\(*\ %s\ *\)
    autocmd BufEnter volofile setf javascript
    autocmd BufWritePre *.py,*.js,*.rb,*.lisp,*.css :call <SID>StripTrailingSpaces()
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
    autocmd FileType vim setl foldmethod=marker
    autocmd BufWritePost .vimrc source $MYVIMRC
  augroup END
  " }}}

  augroup paste
    autocmd!
    autocmd InsertLeave * set nopaste
  augroup END
endif

" Clojure {{{
let vimclojure#ParenRainbow = 1
" }}}

" CtrlP {{{
let g:ctrlp_prompt_mappings = {
    \ 'PrtSelectMove("j")':   ['<c-n>', '<down>'],
    \ 'PrtSelectMove("k")':   ['<c-p>', '<up>'],
    \ 'PrtHistory(-1)':       ['<c-k>'],
    \ 'PrtHistory(1)':        ['<c-j>'],
    \ }
" }}}
