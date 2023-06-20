# MoveAPdesignators2
This script will change auto-positioned designators to manual and move them by a user-defined amount. Will operate on all or selected components.\
The direction of the movement depends on the current autoposition status.

*Taken from original MoveAPdesignators script*

- Added ability to set justification anchor so that changes to designator size or length grow in the same direction AutoPosition would grow (manual but smarter)
- Added 8 configurable preset values
- Remembers last-used settings
- Negative inputs move designators farther away from component instead of closer
- Works in AD 22.8.2+, not sure exactly what version introduced text justification for single strings

## How to install and use
_Step 1_: [DOWNLOAD](https://minhaskamal.github.io/DownGit/#/home?url=https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20PCB/MoveAPdesignators2) script

_Step 2_: integrate the script into Altium Designer and execute it.\
If you are a newcomer to Altium scripts, [please read the "how to" wiki page](https://github.com/Altium-Designer-addons/scripts-libraries/wiki/HowTo_execute_scripts).

## Known Issues
- none known at this time

# Changelog
- 2022-11-28 - Ver 2.0 : Initial release based on MoveAPdesignators scrip Ver 1.2; uses new string justification settings and fixed bug with designator not actually moving coords; added presets and restore last used values
- 2022-11-28 - Ver 2.01 : fixed justification for rotated strings (approximated to nearest 90° rotation)
- 2022-11-28 - Ver 2.02 : *actually* fixed justification for rotated strings (I think)
- 2022-11-28 - Ver 2.03 : ***actually*** *actually* fixed justification and movement for rotated strings (my test PcbDoc was glitched where bottom side designators were mirrored in place or something)
- 2023-02-15 - Ver 2.04 : fixed support for moving designators away with negative input value; maybe, *possibly*, ***actually*** fixed justification and movement for the last time (*found a bug in Altium where components flipped to the other side of the board have the wrong autoposition behavior until the PcbDoc is closed and re-opened.*)
- 2023-06-20 - Ver 2.05 : added support for initially selecting a mix of designators and components, rather than only components; added support for AD19+ AdvanceSnapping text property (might actually fix previous justification bug); better memory safety

## Credits
  - Credit to Mattias Ericson & Tony Chilco for the MoveAPdesignators script I started from
  - Modified by Ryan Rutledge
