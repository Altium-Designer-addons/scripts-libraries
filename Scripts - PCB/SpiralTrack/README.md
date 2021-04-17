# SpiralTrack
This script will draw a trace around a user defined Via, based on Via size.


## Usage
The via and Gap set the size of the first two Arc's, then the Gap and width set the following Arc's.\
Each Arc gets a little bigger, so that after 4 Arc's it has grown enough to maintain the Gap.

The resolution could be improved by using smaller Arc's (less than 90°), but four seems to work well.


## Changelog
Version 0.8, Jun 2005
Would like to integrate with planar script, and maybe others

Improve dialog, DXP style
Metric support added
Fixed Via start/finish layer bug also
Mouse select start point added
Change the way it draws objects, much faster now, and its a grouped undo now also.
Gets current layer now also


## Credits
Darren Moore\
Copyright (c) 2005 Mooretronics Pty Ltd\
Melbourne Australia
