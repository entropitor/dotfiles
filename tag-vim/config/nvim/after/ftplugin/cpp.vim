if !filereadable(expand("%:p:h")."/Makefile")
    setlocal makeprg=g++\ -Wall\ -Wextra\ -o\ %<\ -std=c++17\ %
endif

ALEDisable
