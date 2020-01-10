[![Build Status](https://travis-ci.com/naryl/cl-tui.svg?branch=master)](https://travis-ci.com/naryl/cl-tui)

# cl-tui - Text User Interface for Common Lisp

cl-tui is a library for text user interfaces like that of
[ncmpcpp](http://screenshots.debian.net/screenshots/000/012/942/large.png)
or [Dungeon Crawl](http://screenshots.debian.net/screenshots/000/001/023/large.png)

It's intended to be available on Linux and Windows but the latter is rarely
tested. Please, report bugs either on
[github's](https://github.com/naryl/cl-tui/issues) or
[SourceForge's](https://sourceforge.net/p/cl-tui/tickets/) issue tracker.

Supported implementations are SBCL, CCL. I want to also support ECL and CLISP
but ECL currently can't build osicat (investigated by osicat developers) and
CLISP had a lot of issues with ncurses. Any help with CLISP is appreciated.

`cl-tui` is supposed to be a complete abstraction so if you have to use
`cl-charms` directly for some reason please submit an issue describing your use-case.

# Documentation

Follow the very well-commented examples in the examples directory. Each new
feature gets an example and all of them are checked to be in working order on
every commit.
