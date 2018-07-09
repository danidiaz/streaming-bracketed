:set path=$PWD,$PWD/library
:set suffixesadd=.hs
:set includeexpr=substitute(v:fname,'\\.','/','g')
:set wildignore+=dist-newstyle/*

:set include=^import\\\s*\\\(qualified\\\)\\\?
