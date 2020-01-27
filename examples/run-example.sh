#!/bin/sh

sbcl --eval '(ql:quickload "cl-tui")' \
	--load $1 \
	--eval '(cl-tui.examples::start)' \
	--no-linedit
