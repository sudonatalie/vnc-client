Haskell VNC Client
==================

Installation
------------

**OS X**:
    
1. Install [XQuartz](http://xquartz.macosforge.org/landing/).
2. `LIBRARY_PATH=/opt/X11/lib:$LIBRARY_PATH CPPFLAGS="-I/opt/X11/include" LDFLAGS="-L/opt/X11/lib" cabal install X11`
3. `cabal install gtk2hs-buildtools`
4. `brew install gtk+`
5. `brew install libglade`
6. `brew tap homebrew/versions`
7. `brew install gcc49`
8. `PKG_CONFIG_PATH=/opt/X11/lib/pkgconfig cabal install glade --with-gcc=gcc-4.9`
9. `cd vnc-client`
10. `cabal install`

**Ubuntu**:

1. Install X11.
2. `sudo cabal install gtk2hs-buildtools --global`
3. `sudo apt-get install libghc6-zlib-dev`
4. `sudo apt-get install libgtk2.0-dev`
5. `sudo apt-get install libgtksourceview2.0-dev`
8. `sudo apt-get install libglade2-dev`
9. `sudo cabal install glade --global`
10. `cd vnc-client`
11. `sudo cabal install`

Usage
-----

    ~ $ vnc-client --help
    Usage: vnc-client [OPTION...] host
      -?         --help           print usage instructions
      -v         --verbose        verbose mode for more information output
      -g         --gui            configure client via graphical UI
      -n         --no-auth        connect without authentication (default: VNC password authentication)
      -p PORT    --port=PORT      port number (default: 5900)
      -t TOP     --top=TOP        top position (default: 0)
      -l LEFT    --left=LEFT      left position (default: 0)
      -w WIDTH   --width=WIDTH    width (default: entire framebuffer)
      -h HEIGHT  --height=HEIGHT  height (default: entire framebuffer)

Documentation
-------------

Source code documentation can be generated from the literate source files with:

1. `lhs2TeX doc.lhs > doc.tex`
2. `pdflatex doc.tex`
