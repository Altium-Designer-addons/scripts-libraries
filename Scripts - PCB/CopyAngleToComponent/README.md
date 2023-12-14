# CopyAngleToComponent Script

## What is it
This script is a utility tool to rotate components to align with a track or tangent to the midpoint of an arc in a .PcbDoc

## How to install and use
_Step 1_: [DOWNLOAD](https://altium-designer-addons.github.io/DownGit/#/home?url=https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20PCB/CopyAngleToComponent) script

_Step 2_: integrate the script into Altium Designer and execute it.\
If you are a newcomer to Altium scripts, [please read the "how to" wiki page](https://github.com/Altium-Designer-addons/scripts-libraries/wiki/HowTo_execute_scripts).

## Usage guide
* From within an active .PcbDoc, run the _CopyAngleToComponent_ or _CopyAngleToComponentSilent_ script procedures
* Select a track or arc, then select a component. Status bar shows angle component will be rotated to.

## Credits
Created by: unknown

## Changelog
v1.0 - Initial Release\
2023-06-12 by Ryan Rutledge : v1.1 - some translation and UX tweaking; don't show angles as negative numbers\
2023-06-12 by Ryan Rutledge : v1.2 - added support for arcs (output angle is tangent to arc midpoint)\
2023-06-12 by Ryan Rutledge : v1.3 - added silent option with _CopyAngleToComponentSilent_ procedure; made script repeat\
2023-06-12 by Ryan Rutledge : v1.4 - fixed type error with horizontal and vertical tracks
