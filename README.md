Haskell VNC Client
==================

Installation
------------

    cabal install

Troubleshooting
---------------

X11 on OSX:

1. Install [XQuartz](http://xquartz.macosforge.org/)
2. `LIBRARY_PATH=/opt/X11/lib:$LIBRARY_PATH CPPFLAGS="-I/opt/X11/include" LDFLAGS="-L/opt/X11/lib" cabal install X11`

GTK on OSX:

1. `PKG_CONFIG_PATH=/opt/X11/lib/pkgconfig cabal install glade --with-gcc=gcc-4.9`
 
GTK on Ubuntu:

1. `sudo apt-get install libghc6-zlib-dev`
2. `sudo apt-get install libgtk2.0-dev`
3. `sudo apt-get install libgtksourceview2.0-dev`
4. `sudo cabal install gtk2hs-buildtools --global`
5. `sudo apt-get install libglade2-dev`
6. `sudo cabal install glade --global`

Documentation
-------------

Source code documentation can be generated from the literate source files with:

1. lhs2TeX doc.lhs > doc.tex
2. pdflatex doc.tex