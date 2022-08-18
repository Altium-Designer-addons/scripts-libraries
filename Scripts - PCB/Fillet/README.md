# Fillet Script

## What is it
This Script creates fillet(s) on selected tracks either as a percentage of the shortest connected track or with fixed radius.


## Credits
Created by: Petar Perisin\
Edited by: Ryan Rutledge, Brett Miller\
Edited 2018-09-15 to support any-angle tracks and fixed radius\
Edited 2022-08-01 to add 8 configurable preset radius values\
Edited 2022-08-06 to cleanup TStringList object methods etc.


## Usage guide
_Eligible objects_: two or more tracks with _exactly_ joined vertices
_Ineligible objects_: any non-track objects will be deselected when script is launched
_Invoke script_ and choose relevant operation from GUI.
### Fixed Radius
_"mils" or "mm"_ will fillet selected tracks with an arc of the specified radius. Can exceed limit and reverse direction of track(s).
### Percentage
_"%"_ will create a radius that will shorten the shorter of two joined tracks by the indicated percentage. May have unintended behavior when multiple tracks in series are selected.
### Changing Units
Default setting is fixed radius in mils. Value presets are unitless.


## How to install and use
_Step 1_: [DOWNLOAD the script](https://minhaskamal.github.io/DownGit/#/home?url=https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20PCB/Fillet)

_Step 2_: integrate the script into Altium Designer and execute it.\
If you are a newcomer to Altium scripts, [please read the "how to" wiki page](https://github.com/Altium-Designer-addons/scripts-libraries/wiki/HowTo_execute_scripts).
