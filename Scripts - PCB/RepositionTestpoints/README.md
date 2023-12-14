# Reposition Testpoints
Place testpoints in bulk without having to scroll over and manually select each testpoint.

## Usage
Select an object with a net

The corresponding testpoint with a matching net is selected

Component will be moved to your cursor for placement

### Notes
Testpoints that are locked or already placed on their net will not be selected\
Hold Shift to select multiple nets before placement\
Hold Ctrl to allow selecting testpoints which are already placed

### Assumptions
* It is assumed that all testpoints contain a Pad with an assigned net and use the designator prefix 'TP'

### Known Issues
* When a net has multiple testpoints the script may not select the desired testpoint, as it just grabs the first free testpoint it finds with the net.
* When checking if testpoints are already placed, checking against polygons is not perfect. Irregularly shaped polygon areas are natively converted to a virtual rectangle prior to checking for overlap with the testpoint pad. Along with this, polygons are assumed to have copper in the entire virtual area. Thus, the script can inaccurately assume testpoints are placed when they may not actually be in contact with copper.
* Testpoints connected with only a via will be considered unconnected.

## How to install and use
_Step 1_: [DOWNLOAD the script](https://altium-designer-addons.github.io/DownGit/#/home?url=https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20PCB/RepositionTestpoints)

_Step 2_: integrate the script into Altium Designer and execute it.\
If you are a newcomer to Altium scripts, [please read the "how to" wiki page](https://github.com/Altium-Designer-addons/scripts-libraries/wiki/HowTo_execute_scripts).

## Changelog
Version 1.0 - Corey Beyer - Initial Release\
Version 1.1 - Corey Beyer - Added dummy args to hide private procedures
