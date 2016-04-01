#!/bin/bash
buildapp \
	--manifest-file quicklisp-manifest.txt \
    --load-system cl-opengl \
    --load-system cl-glu \
    --load-system lispbuilder-sdl \
    --output macclocktopus \
	--asdf-path ~/src/lisp/cocktus/ \
	--load-system cocktus \
    --entry cocktus:arise