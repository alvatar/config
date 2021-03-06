"
" General configuration
set nocompatible
syntax enable
set foldmethod=syntax
set nofoldenable

filetype on
filetype plugin on
filetype indent on

set shiftwidth=2
set tabstop=2
set expandtab
set showmatch

set path+=.**
set sessionoptions=blank,buffers,curdir,globals,folds,help,localoptions,options,tabpages,winsize
set showcmd
set number
set cursorline
set spelllang=es,en
set wildmenu
set hidden
map K <Nop>

" Don't do any kind of backup or swap
set nobackup
set nowritebackup
set noswapfile

" S substitutes word with last yanked text (without overwritting yank buffer)
nnoremap S "_diwP
vnoremap S "_d"0P

"
" Status line
:set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ [POS=%04l,%04v][%p%%]\ [LEN=%L]
:set laststatus=1

"
" Colors, syntax and GUI
set background=light
if has("gui_running")
    set guioptions=ac
    set guiheadroom=0
    set guifont=MonteCarlo
    "set guifont=Monofur\ 11
    "set guifont=Envy\ Code\ R\ 9
    colo solarized
elseif &term =~ "xterm\\|rxvt\\|screen-bce"
  " set t_Co=256
    " colo silent
endif

if &term =~ "xterm\\|rxvt\\|screen-bce"
    :silent !echo -ne "\033]12;red\007"
    let &t_SI = "\033]12;orange\007"
    let &t_EI = "\033]12;red\007"
    autocmd VimLeave * :!echo -ne "\033]12;red\007"
    highlight Visual ctermbg=3
endif
highlight CursorLine guibg=#eeeeee

"
" Internet searches
nnoremap <F2> :!firefox "http://www.google.es/search?q="<C-R><C-W>"&ie=utf-8&oe=utf-8&aq=t"<CR>
nnoremap <F2><F2> :!firefox <C-R><C-A><CR>

"
" To support filenames with spaces when using gf: set isfname+=32
map gf :edit <cfile><CR>

" Match tabs that are used inside character strings
match ErrorMsg /[^\t]\zs\t\+/

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" FILE TYPES AND LANGUAGES 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd BufRead,BufNewFile *.e{build,class} let is_bash=1|setfiletype sh
autocmd BufRead,BufNewFile *.e{build,class} set ts=4 sw=4 noexpandtab

autocmd BufRead,BufNewFile *.{scm,ss,sls} set ts=2 sw=2 expandtab showmatch filetype=scheme

autocmd BufRead,BufNewFile *.{rb} set ts=2 sw=2 expandtab filetype=ruby

autocmd BufRead,BufNewFile *.{js} set ts=2 sw=2 expandtab filetype=javascript

autocmd BufRead,BufNewFile *.{html,php} set ts=2 sw=2 expandtab showmatch

autocmd BufRead,BufNewFile *.7 nmap <F4> :w<CR>:!groff -Tascii -man % \| less<CR>

autocmd BufRead,BufNewFile *.{c,cpp,cc,cxx,c++,C,h,hpp,hxx,h++,H,cu} set ts=2 sw=2 expandtab
autocmd BufRead,BufNewFile *.{c,cpp,cc,cxx,c++,C,h,hpp,hxx,h++,H,cu} set makeprg=make\ -j10
autocmd BufRead,BufNewFile *.{c,cpp,cc,cxx,c++,C,h,hpp,hxx,h++,H,cu} noremap <silent> <F10> <ESC>:wa<CR>:make<CR>
autocmd BufRead,BufNewFile *.{c,cpp,cc,cxx,c++,C,h,hpp,hxx,h++,H,cu} inoremap <silent> <F10> <ESC>:wa<CR>:make<CR>
autocmd BufRead,BufNewFile *.{c,cpp,cc,cxx,c++,C,h,hpp,hxx,h++,H,cu} noremap <silent> <F11> <ESC>:wa<CR>:make!<CR>
autocmd BufRead,BufNewFile *.{c,cpp,cc,cxx,c++,C,h,hpp,hxx,h++,H,cu} inoremap <silent> <F11> <ESC>:wa<CR>:make!<CR>
autocmd BufNewFile,BufRead *.{c,C,cpp,cxx,cc,h,hpp} set equalprg=astyle\ -s2\ --style=gnu\ --pad-header\ --align-pointer=name\ --indent-col1-comments\ --pad-first-paren-out

autocmd BufRead,BufNewFile *.{markdown,md} set syntax=off

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" PLUGINS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"
" Exhuberant CTags and CScope
function! BuildCTagsAndCSCopeDatabase( filetype )
    if exists( "g:TagsArgs" )
        let l:input_arg = g:TagsArgs
      else
        let l:input_arg = expand('%:p')
        echo "--> Set Variable TagsArgs to set cTags and cScope input files/dirs"
    endif
    if !exists( "g:TagsTopDir" )
        let g:TagsTopDir = "."
        echo "--> Set Variable TagsTopDir to set cTags and cScope database location"
    endif
    execute "cd ".g:TagsTopDir
    if a:filetype =~# '^\(c\|cpp\)$'
        execute "!ctags --langmap=c++:+.cu,d:+.di --c-kinds=+p --c++-kinds=+p --d-kinds=+p  --fields=+afiklmsSt --extra=+q -R ".l:input_arg
        execute "!find ".g:TagsArgs." -type f -regex '.*\.\(cpp\|h\)' | xargs cscope -Rqv"
        cscope reset
        cscope add cscope.out
    endif
    if a:filetype =~# '^\(d\)$'
        execute "!ctags --langmap=c++:+.cu,d:+.di --c-kinds=+p --c++-kinds=+p --d-kinds=+p  --fields=+afiklmsSt --extra=+q -R ".l:input_arg
    endif
    if a:filetype =~# '^\(scheme\)$'
        execute "!ctags ".l:input_arg
        " ctags --langmap=scheme: --langdef=scm --langmap=scm:.ss.scm --regex-scm="/^\(def[a-zA-Z0-9\-_\?\/\\]+[ \t]+([a-zA-Z0-9\-_\/\\\?]+)/\1/d,definition/" --regex-scm="/^\(define\-syntax(\-rule)?[ \t]+([a-zA-Z0-9\-_\/\\\?]+)/\2/m,macro/" --regex-scm="/^\(define?[ \t]+(([a-zA-Z0-9\-_\/\\\?]+)[ \t]+\(lambda|\(([a-zA-Z0-9\-_\/\\\?]+))/\2\3/f,function/
    endif
    execute "cd -"
endfunction
nnoremap <silent> <F12> :call BuildCTagsAndCSCopeDatabase(&filetype)<CR>

"
" CScope
if has("cscope")
  " use both cscope and ctag for 'ctrl-]', ':ta', and 'vim -t'
  set cscopetag
  " check cscope for definition of a symbol before checking ctags: set to 1
  " if you want the reverse search order.
  set csto=0
  " add any cscope database in current directory
  if filereadable("cscope.out")
    cs add cscope.out  
    " else add the database pointed to by environment variable 
  elseif $CSCOPE_DB != ""
    cs add $CSCOPE_DB
  endif
  " show msg when any other cscope db added
  set cscopeverbose  
  " The following maps all invoke one of the following cscope search types:
  "
  "   's'   symbol: find all references to the token under cursor
  "   'g'   global: find global definition(s) of the token under cursor
  "   'c'   calls:  find all calls to the function name under cursor
  "   't'   text:   find all instances of the text under cursor
  "   'e'   egrep:  egrep search for the word under cursor
  "   'f'   file:   open the filename under cursor
  "   'i'   includes: find files that include the filename under cursor
  "   'd'   called: find functions that function under cursor calls
  nmap <C-Space>s :cs find s <C-R><C-W><CR>
  nmap <C-Space>g :cs find g <C-R><C-W><CR>
  nmap <C-Space>c :cs find c <C-R><C-W><CR>
  nmap <C-Space>t :cs find t <C-R><C-W><CR>
  nmap <C-Space>e :cs find e <C-R><C-W><CR>
  nmap <C-Space>f :cs find f <C-R><C-F><CR>
  nmap <C-Space>i :cs find i <C-R><C-F><CR>
  nmap <C-Space>d :cs find d <C-R><C-W><CR>

  cnoremap <C-Space> cscope find

  nmap <C-Space><C-Space>s :vert scs find s <C-R><C-W><CR>
  nmap <C-Space><C-Space>g :vert scs find g <C-R><C-W><CR>
  nmap <C-Space><C-Space>c :vert scs find c <C-R><C-W><CR>
  nmap <C-Space><C-Space>t :vert scs find t <C-R><C-W><CR>
  nmap <C-Space><C-Space>e :vert scs find e <C-R><C-W><CR>
  nmap <C-Space><C-Space>f :vert scs find f <C-R><C-F><CR>
  nmap <C-Space><C-Space>i :vert scs find i <C-R><C-F><CR>
  nmap <C-Space><C-Space>d :vert scs find d <C-R><C-W><CR>

  cnoremap <C-Space><C-Space> vert scscope find
endif

