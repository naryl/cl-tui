#!/bin/sh
sbcl --eval "(ql:quickload 'cl-tui)" --eval "(cl-tui:init-screen)" --eval "(loop while cl-tui::*running* do (sleep 1) finally (sb-ext:exit))"
