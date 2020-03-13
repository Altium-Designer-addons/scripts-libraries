# Distribute Script

## What is it
This script can set the clearance or center-to-center distance between two or more tracks, and in the case of three or more tracks, can distribute the track centers or clearances evenly between the outer pair of tracks.

## How to install and use
_Step 1_: [DOWNLOAD the script](https://minhaskamal.github.io/DownGit/#/home?url=https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Distribute)

_Step 2_: integrate the script into Altium Designer and execute it.\
If you are a newcomer to Altium scripts, [please read the "how to" wiki page](https://github.com/Altium-Designer-addons/scripts-libraries/wiki/HowTo_execute_scripts).

## Usage guide
_Eligible objects_: Select two or more parallel tracks.
_Invoke script_ and choose relevant operation from GUI.
### Distribute by Clearance
_Distribute by Clearance_ will make the clearance distance between all tracks equal, without moving the outer 2 tracks. Requires 3 or more parallel tracks.
### Distribute by Centerlines
_Distribute by Centerlines_ will make pitch of all tracks equal, without moving the outer 2 tracks. If tracks are different widths, they will necessarily have different gaps. Requires 3 or more parallel tracks.
### Distribute Clearances by value
_Distribute Clearances by value_ will set the clearance of tracks to the user-specified value, without moving the first track.
### Distribute Centers by value
_Distribute Centers by value_ will set the centerlines of tracks to the user-specified pitch, without moving the first track.
### Which one is the first track?
_First track_ depends on the direction the tracks are running. 
For horizontal tracks, the lowest track is first.
For vertical tracks, the leftmost track is first.
For tracks rotated anti-clockwise from vertical, the leftmost track is first.
For tracks rotated clockwise from vertical, the rightmost track is first.
### Changing Units
When one of the by-value options is active, clicking the "mil" label next to the input value will change it to "MM" and vice versa.
