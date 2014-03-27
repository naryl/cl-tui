cl-tui - Text User Interface for Common Lisp
==================================================
Cl-tui is a library for text user interfaces like that of [ncmpcpp](https://screenshots.debian.net/screenshots/n/ncmpcpp/4889_large.png) or [Dungeon Crawl](http://screenshots.debian.net/screenshots/c/crawl/1023_large.png)

It's currently in development and not intended for general use.

### Documentation
    :::lisp
    (defun init-screen (&rest arguments))

    Initializes the ncurses screen with the specified attributes.
    :echo/:noecho - echo input characters to screen
    :raw/:noraw - send keypresses without any processing
    :cbreak/:nocbreak - disable line buffering (disabled automatically if :raw)
    :cursor/:nocursor - show input cursor
    :colors - initialize colors system
    
    :::lisp
    (defun destroy-screen ())

    Destroys the ncurses screen. Note that you'll probably have a lot of weird behaviour currently if you try to start it back again.

    :::lisp
    (defmacro with-screen ((&body arguments) &body body))

    Runs the body between init-screen and destroy-screen

    :::lisp
    (defun display (&optional (frame :root)))

    Display frame and its children on screen. Displays the root frame by default.

    :::lisp
    (defun refresh (&optional (frame *display*)))
    
    Refreshes the specified frame and its children. By default refreshes all currently displayed frames.

    :::lisp
    (defun frame-size (&optional frame))

    Returns the list (y x) containing the frame size in screen characters.

    :::lisp
    define-frame
    :::lisp
    #:destroy-frame

    :::lisp
    (defmacro with-attributes ((&body attributes) frame &body body))

    Runs the code with specified attributes set. Attributes are:
    :standout - draw highlighted text
    :underline - draw underlined text
    :reverse - draw text with reversed colors
    :blink - make text blinking
    :dim - less bright colors
    :bold - more bright colors or bold text
    :protect - protected mode
    :invis - invisible mode
    :altcharset - alternate character set
    (:color color-pair) - set text color

    :::lisp
    (defun make-color (r g b))

    Make a color and return the color object. Note that ncurses have very limited pool of color objects. Should be around 85.

    :::lisp
    (defun free-color (color))

    Free the color object making it usable for other colors.

    :::lisp
    (defun make-color-pair (fg bg))
    
    Make a color pair from foreground and background colors. ncurses can have no more than 64 color pairs at any time.

    :::lisp
    (defun free-color-pair (pair))

    Free the color pair object making it usable for other colors.

    :::lisp #:retained-frame

    :::lisp #:callback-frame

    :::lisp #:put-char

    :::lisp #:put-text


    :::lisp #:text-frame

    :::lisp #:append-line

    :::lisp #:append-text

    :::lisp #:clear



    :::lisp #:read-key

   ))

### TODO
See the [TODO list](https://bitbucket.org/naryl/cl-tui/src/default/TODO.wiki).
