# Deprecated. See [AssemblyTextPrep](https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20PCB/AssemblyTextPrep) script instead
-
-
-
-
-
-
### Select Assembly Designators Script

## What is it
This script is a utility tool to quickly switch between components and a *.Designator* special string belonging to their footprint.

## How to install and use
_Step 1_: [DOWNLOAD](https://altium-designer-addons.github.io/DownGit/#/home?url=https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20PCB/SelectAssyDesignators) script

_Step 2_: integrate the script into Altium Designer and execute it.\
If you are a newcomer to Altium scripts, [please read the "how to" wiki page](https://github.com/Altium-Designer-addons/scripts-libraries/wiki/HowTo_execute_scripts).

## Usage guide
### **ENABLE BOTH COMPONENTS AND TEXTS IN SELECTION FILTER**
### to Select Assembly Designators
Select one or more components then run the _SelectDesignators_ script procedure to switch your selection to the first .Designator special string in each component.
### to Select Components
Select one or more .Designator special strings belonging to components then run the _SelectComponents_ script procedure to switch your selection to the components to which they belong.
### to Select Both
Select one or more components and/or .Designator special strings then run the _SelectBoth_ script procedure to select all components and .Designator special strings associated with your selection.\
* **Tip:** other objects are ignored/deselected in each case, so you can use _SelectBoth_ followed by one of the other procedures to go from a mixed selection to selecting only one type.
### to Reset Designator Positions
Select one or more components and/or .Designator special strings then run _ResetDesignatorPositions_ to reset all .Designator special strings associated with the selected items to center on their parent components and exactly match their rotation value.
### to Reset Designator Positions (Normalized)
Select one or more components and/or .Designator special strings then run _ResetDesignatorPositionsNorm_ to reset all .Designator special strings associated with the selected items to center on their parent components and align to their rotation value but normalized to be right-reading.
### to Reset Designator Positions (Normalized and orthogonally oriented)
Select one or more components and/or .Designator special strings then run _ResetDesignatorPositionsNormOrtho_ to reset all .Designator special strings associated with the selected items to center on their parent components and align to their rotation value (rotated 90 deg. CW) but normalized to be right-reading.
### to Reset Designator Positions (Orthogonally oriented)
Select one or more components and/or .Designator special strings then run _ResetDesignatorPositionsOrtho_ to reset all .Designator special strings associated with the selected items to center on their parent components and align to their rotation value (rotated 90 deg. CW).
### to Resize Designators
Select one or more components and/or .Designator special strings then run _DesignatorResize_ to resize all .Designator special strings associated with the selected items (based on the current value) to to fit within the boundary of the component pads, then reset their positions.
### to Resize Designators (Orthogonally oriented)
Select one or more components and/or .Designator special strings then run _DesignatorOrthoResize_ to resize all .Designator special strings associated with the selected items (based on the current value) to to fit within the boundary of the component pads (rotated 90 deg. CW), then reset their positions.
### FUTURE PLANS
Have a GUI that remembers last-used settings

## Features

### Eligible Objects
The script should only do *useful* work when selecting components and/or .Designator special strings associated with components. Other object types can be in the selection, however, but they will be deselected if any valid objects were selected.

## Known Issues
### Not happy with the name
I'm open to suggestions. While I'm using these for assembly documentation, I'm not sure calling them "Assembly Designators" is the best way to refer to them, but I can't think of anything better or more concise at the moment.
### Silently ignores components without a .Designator special string
There might be some value in adding a function to check that all components *have* a .Designator special string, but for now, attempting to select it will just deselect the component and nothing else.\
* **Tip:** Select components then run _SelectDesignators_ followed by _SelectComponents_ to reduce your selection to only components with the special string.

## Change log
2023-05-17 by Ryan Rutledge : v0.1 - Initial version of script. Provides basic selection manipulation for PCB components and associated .Designator special strings

2023-05-18 by Ryan Rutledge : v0.2 - added _About_ command

2023-05-18 by Ryan Rutledge : v0.3 - fix to refresh properties panel after command is executed

2023-05-18 by Ryan Rutledge : v1.0 - added commands to reset .Designator special strings to center on component and match alignment (with or without rotation normalization)

2023-05-22 by Ryan Rutledge : v1.1 - added commands for orthogonal rotation and resizing .Designator special strings to fit component pads' bounding box

2023-05-23 by Ryan Rutledge : v1.2 - added IPCB_Text inspection function for debugging

2023-05-24 by Ryan Rutledge : v1.3 - fixed bug with debugging flag set to false caused by Delphi short-circuit logic

2023-06-07 by Ryan Rutledge : v1.4 - fixed bug with justification not being applied to text that hadn't had it manually changed before (thanks, Brett Miller!); updated message box styles

2023-06-07 by Ryan Rutledge : v1.5 - streamlined script to use SnapPointX, SnapPointY properties directly to position text, rather than calculating offsets and using MoveByXY

2023-06-09 by Ryan Rutledge : v1.6 - added error message if the _SelectBoth_ procedure is attempted without both Components and Texts enabled in the selection filter

2023-06-21 by Ryan Rutledge : v1.7 - refactored rotation into its own function to make the code more modular and to steal the function for other scripts

2023-07-05 by Ryan Rutledge : v1.8 - added command to normalize any selected text while preserving justification
