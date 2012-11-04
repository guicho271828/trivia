#!/bin/sh

sbcl --quit \
     --eval '(ql:quickload :optima)' \
     --eval '(ql:quickload :optima.contrib)' \
     --eval '(ql:quickload :parendoc)' \
     --eval '(parendoc:render-markdown-to-file
	               (list (parendoc.generate:generate (asdf:find-system :optima))
	                     (parendoc:page-feed)
	                     (parendoc.generate:generate (asdf:find-system :optima.contrib)))
	               "README.md" :if-exists :supersede)'
