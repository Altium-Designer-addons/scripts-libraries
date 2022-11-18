# Distribute Script

## What is it
This script can set the clearance or center-to-center distance between two or more tracks, and in the case of three or more tracks, can distribute the track centers or clearances evenly between the outer pair of tracks.

## How to install and use
_Step 1_: [DOWNLOAD](https://minhaskamal.github.io/DownGit/#/home?url=https://github.com/RnDMonkey/scripts-libraries/new/Update-Distribute-script/Distribute) script

_Step 2_: integrate the script into Altium Designer and execute it.\
If you are a newcomer to Altium scripts, [please read the "how to" wiki page](https://github.com/Altium-Designer-addons/scripts-libraries/wiki/HowTo_execute_scripts).

## Usage guide
_Eligible objects_: Select two or more parallel tracks.\
_Invoke script_ and choose relevant operation from GUI.
### Distribute by Clearance
_Distribute by Clearance_ will make the clearance distance between all tracks equal, without moving the outer 2 tracks. Requires 3 or more parallel tracks.
### Distribute by Centerlines
_Distribute by Centerlines_ will make pitch of all tracks equal, without moving the outer 2 tracks. If tracks are different widths, they will necessarily have different gaps. Requires 3 or more parallel tracks.
### Distribute Clearances by value
_Distribute Clearances by value_ will set the clearance of tracks to the user-specified value, without moving the first track.
### Distribute Centers by value
_Distribute Centers by value_ will set the centerlines of tracks to the user-specified pitch, without moving the first track.
### Which one is the First Track?
For the _FWD_ Direction mode, _First Track_ depends on the direction the tracks are running:\
* For horizontal tracks, the lowest track is first.
* For vertical tracks, the leftmost track is first.
* For tracks rotated anti-clockwise from vertical, the leftmost track is first.
* For tracks rotated clockwise from vertical, the rightmost track is first.

For the _REV_ Direction mode, the order above is reversed.

For the _CEN_ Direction mode, _First Track_ is either the middle track if there are an odd number of tracks, or it is the ***(N/2)***_-th_ track.

## Features

### Direction of distribution
_FWD_ behaves as described above, which is the same behavior as previous versions of the script.\
_CEN_ redistributes tracks from the center of extents. For example, a pair of tracks will move symmetrically. The center line is halfway between the outer pair of tracks.\
_REV_ will reverse the direction of distribution i.e. what would normally be the last track is instead the first track.
### Changing Units
When one of the by-value options is active, clicking the "mil" label next to the input value will change it to "MM" and vice versa.
### Presets
When one of the by-value options is active, 8 user preset values are available. Editing a preset value and pressing Enter, or pressing the corresponding button will run the distribute command with that value.
### Trim Unconnected Track Ends
If checked, any unconnected track ends will be trimmed to be perpendicular with the respective end of the _First Track_.
### Debug Logging
If script is launched using the _StartWithDebug_ procedure, it will save before-and-after data for each track that is distributed.\
Debug file is saved in **`%appdata%\Altium\Altium Designer {installation ID}\DistributeScriptDebug.csv`**

## Known Issues
### Rounding errors with tracks that aren't at 90/45 angles
Sometimes, particularly when distributing tracks that are not routed at 90° or 45° angles, rounding errors can occur due to precision of double data type and Altium TCoord unit resolution. Errors up to 0.002mil have been observed.
### Tracks that are nearly, but not quite, vertical
Tracks with a slope greater than 20 (90° > angle > ~87.137°) will be coerced to be perfectly vertical. This is a limitation of the Double number type. In short, angles that are too steep will have Y-intercept values that are too large to handle.

## Change log
2022-11-02 by Ryan Rutledge : v1.3 - Added support for up to 8 user presets for by-value inputs; remembers last-used by-value input

2022-11-06 by Ryan Rutledge : v1.4 - Remembers last-used distribute mode if valid; added support for distributing tracks by-value in different working directions

2022-11-16 by Ryan Rutledge : v1.41 - Added ability to trim dangling track ends perpendicular to _First Track_

2022-11-17 by Ryan Rutledge : v1.42 - added version to form

2022-11-17 by Ryan Rutledge : v1.43 - added debugging tools; fixed TargetSlope getting reset for each track instead of set once by _First Track_

2022-11-18 by Ryan Rutledge : v1.44 - fixed bug where near-vertical tracks wouldn't trim properly if ends were normalized; fixed overflow error for steep angles; fixed invalid distribution if tracks are near vertical and are coerced to vertical
