# cl-tui - Text User Interface for Common Lisp

Cl-tui is a library for text user interfaces like that of [ncmpcpp](https://screenshots.debian.net/screenshots/n/ncmpcpp/4889_large.png) or [Dungeon Crawl](http://screenshots.debian.net/screenshots/c/crawl/1023_large.png)

It's currently in development and not intended for general use.

# Documentation

### (defun init-screen (&rest arguments))

Initializes the ncurses screen with the specified attributes.

* :echo/:noecho - Echo input characters to screen
* :raw/:noraw - Send keypresses without any processing
* :cbreak/:nocbreak - Disable or enable line buffering (disabled automatically if :raw)
* :cursor/:nocursor - Show or hide input cursor
* :delay/:nodelay - Blocking or non-blocking input
* :keypad/:nokeypad - Detect system keys and report them using keywords instead of :ESC-sequences
* :meta/:nometa - Detect and report alt with read-key's third value. (NOT RECOMMENDED, NOT PORTABLE)
* :colors - Initialize colors system

Note that calling init-screen or with-screen several times in the same image may or may not break something.

### (defun destroy-screen ())

Destroys the ncurses screen. Note that you'll probably have a lot of weird behaviour currently if you try to start it back again.

### (defmacro with-screen ((&body arguments) &body body))

Runs the body between init-screen and destroy-screen

Note that calling init-screen or with-screen several times in the same image may or may not break something.

### (defun display (&optional (frame :root)))

Display frame and its children on screen. Displays the root frame by default.

### (defun refresh (&optional (frame *display*)))

Refreshes the specified frame and its children. By default refreshes all currently displayed frames.

### (defun frame-size (&optional frame))

Returns the list (y x) containing the frame size in screen characters.

### define-frame

### destroy-frame

### (defmacro with-attributes ((&body attributes) frame &body body))

Runs the code with specified attributes set. Attributes are:

* :standout - draw highlighted text
* :underline - draw underlined text
* :reverse - draw text with reversed colors
* :blink - make text blinking
* :dim - less bright colors
* :bold - more bright colors or bold text
* :protect - protected mode
* :invis - invisible mode
* :altcharset - alternate character set
* (:color color-pair) - set text color

### (defun make-color (r g b))

Make a color and return the color object. Note that ncurses have very limited pool of color objects. Should be around 85.

### (defun free-color (color))

Free the color object making it usable for other colors.

### (defun make-color-pair (fg bg))

Make a color pair from foreground and background colors. ncurses can have no more than 64 color pairs at any time.

### (defun free-color-pair (pair))

Free the color pair object making it usable for other colors.

### #:retained-frame

### #:callback-frame

### #:put-char

### #:put-text


### #:text-frame

### #:append-line

### #:append-text

### #:clear



### #:read-key

## Frame types

* retained-mode frame
* callback frame
* grid frame
* Scrolling log frame

# TODO
See the [TODO list](https://bitbucket.org/naryl/cl-tui/src/default/TODO.wiki).
