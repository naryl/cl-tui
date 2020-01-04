#!/bin/sh

sbcl --eval '(ql:quickload "cl-tui")' \
	--eval '(ql:quickload "swank")' \
	--eval '(swank:create-server :dont-close t)' \
	--load $1 \
	--eval '(cl-tui.examples::start)' \
	--no-linedit
