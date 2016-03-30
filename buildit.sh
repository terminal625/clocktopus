sbcl --no-userinit --no-sysinit --non-interactive \
     --load ~/quicklisp/setup.lisp \
     --eval '(ql:quickload "cl-opengl")' \
     --eval '(ql:quickload "cl-glu")' \
     --eval '(ql:quickload "lispbuilder-sdl")' \
     --eval '(ql:write-asdf-manifest-file "quicklisp-manifest.txt")'

buildapp \
	--manifest-file quicklisp-manifest.txt \
    --load-system cl-opengl \
    --load-system cl-glu \
    --load-system lispbuilder-sdl \
    --output clocktopus \
	--asdf-path ~/src/lisp/cocktus/ \
	--load-system cocktus \
    --entry cocktus:arise