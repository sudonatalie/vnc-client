Haskell VNC Client
==================

Notice
------

This is an experimental alpha project which is no longer under active development. 

Installation
------------
This installation guide needs to be tested again for accuracy.

**OS X**:
    
1. Install [XQuartz](http://xquartz.macosforge.org/landing/).
2. `LIBRARY_PATH=/opt/X11/lib:$LIBRARY_PATH CPPFLAGS="-I/opt/X11/include" LDFLAGS="-L/opt/X11/lib" cabal install X11`
3. `brew install gtk+`
4. `brew tap homebrew/versions`
5. `cd vnc-client`
6. `stack build`

**Ubuntu**:

1. Install X11.
2. `sudo apt-get install libghc6-zlib-dev`
3. `sudo apt-get install libgtk2.0-dev`
4. `sudo apt-get install libgtksourceview2.0-dev`
5. `cd vnc-client`
6. `stack build`

Usage
-----

    ~ $ stack exec vnc-client --help
    Usage: stack exec vnc-client [OPTION...] host
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

License
-------

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
