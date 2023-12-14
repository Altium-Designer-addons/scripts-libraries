# PCBScale Script

## What is it
This script is a utility tool to scale selected objects in a .PcbDoc

## How to install and use
_Step 1_: [DOWNLOAD](https://altium-designer-addons.github.io/DownGit/#/home?url=https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20PCB/PCBScale) script

_Step 2_: integrate the script into Altium Designer and execute it.\
If you are a newcomer to Altium scripts, [please read the "how to" wiki page](https://github.com/Altium-Designer-addons/scripts-libraries/wiki/HowTo_execute_scripts).

## Usage guide
From within an active .PcbDoc, select one or more objects and run the _Start_ script procedure.

## Known issues
- Does not work for dimensions and coordinates at the moment
- scales only extruded 3D models (I do not know how to get radius)

## Credits
Created by: Petar Perisin

## Changelog
v1.0 - Initial Release\
2023-06-11 by Ryan Rutledge : v1.1 - some formatting cleanup; fixed text scaling issue with AD19+\
2023-06-12 by Ryan Rutledge : v2.1 - merged in V2 posted on Altium forums; changed anchor selection style and added more options; streamlined some operations to refocus on scale input
