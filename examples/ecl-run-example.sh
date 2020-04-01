#!/bin/sh

ecl \
	--eval '(ql:quickload "swank")' \
	--eval '(swank:create-server :dont-close t)' \
	--eval '(ql:quickload "cl-tui")' \
	--load $1 \
	--eval '(cl-tui.examples::start)'
