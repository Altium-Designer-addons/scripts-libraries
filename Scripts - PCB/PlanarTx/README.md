# PlanarTx
This script will draw a trace around a user defined rectangle.


## Usage
User enters the size of the rectangle to draw tracks around, the clearance, track width, track to track clearance and number of turns to draw.


## Credits
Copyright (c) 2008 Mooretronics Pty Ltd\
Darren Moore\
Spartak Timchev (sparky@omegatim.com)


## Changelog
Metric support added
Mouse select start point added
Improve dialog, DXP style
get current layer
much faster calls now
grouped undo
removed Str2Int functions so it can handle non integer values
fixed metric issue, force change to imperial before placing objects
make some cosmetic changes
set dialog to open in middle of screen
add layer names to Layers list again

Version 0.7, May 2005 (Darren Moore)

Version 0.7b, August 2012 (Spartak Timchev)
* Center of winding is selected instead of start of winding
* Windings start at 9 o'clock
* Proper single winding

Version 0.7c, August 22 2012 (Spartak Timchev)
* Option to select winding start position
* Radius cannot be larger than maximum possible
* Radius optimisation on "left center" windings
* Arc optimisation on single winding and large radius
* Option to create as component or as free primitives
* Layers Combo works now
* Several interface enhancements
