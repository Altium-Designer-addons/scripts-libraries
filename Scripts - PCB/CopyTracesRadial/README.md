# CopyTracesRadial
Script for the cloning and placement of traces and vias radially.

*This script may probably be replaced by the "Paste Array" function provided by Altium Designer.*


## Usage
1. Lay the initial traces and vias.
2. Select them on the PCB and run the script.
3. Select the centre around which the objects will be rotated (i.e. the rotation center).
4. Enter the start suffix and end suffix of the net names (the number of steps is derived from this so it must be entered even if the net names will not be automatically incremented. For example if your template traces have the net names "netLED5" and you want these replicated and rotated 10 more times then 5 would go in the start suffix and 15 would go in the end suffix).
5. Select the angle step between each iteration and then tick the "Auto increment net name" box and the "Flip layer" box if desired.


## Changelog
Version : 0.1


## Credits
Edward Catley