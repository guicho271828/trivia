.PHONY: doc test

doc:
	sbcl --quit \
	     --eval '(ql:quickload :optima)' \
	     --eval '(ql:quickload :optima.ppcre)' \
	     --eval '(ql:quickload :parendoc)' \
	     --eval "(parendoc:render-markdown-to-file \
	              (list (parendoc.generate:generate (asdf:find-system :optima)) \
	                    (parendoc:page-feed) \
	                    (parendoc.generate:generate (asdf:find-system :optima.ppcre))) \
	              \"README.md\" :if-exists :supersede)"

test:
	sbcl --quit \
	     --eval '(asdf:test-system :optima)'
