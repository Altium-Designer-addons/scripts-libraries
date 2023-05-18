# Select Assembly Designators Script

## What is it
This script is a utility tool to quickly switch between components and a *.Designator* special string belonging to their footprint.

## How to install and use
_Step 1_: [DOWNLOAD](https://minhaskamal.github.io/DownGit/#/home?url=https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20PCB/SelectAssyDesignators) script

_Step 2_: integrate the script into Altium Designer and execute it.\
If you are a newcomer to Altium scripts, [please read the "how to" wiki page](https://github.com/Altium-Designer-addons/scripts-libraries/wiki/HowTo_execute_scripts).

## Usage guide
### Select Assembly Designators
Select one or more components then run the _SelectDesignators_ script procedure to switch your selection to the first .Designator special string in each component.
### Select Components
Select one or more .Designator special strings belonging to components then run the _SelectComponents_ script procedure to switch your selection to the components to which they belong.
### Select Both
Select one or more components and/or .Designator special strings then run the _SelectBoth_ script procedure to select all components and .Designator special strings associated with your selection.\
* **Tip:** other objects are ignored/deselected in each case, so you can use _SelectBoth_ followed by one of the other procedures to go from a mixed selection to selecting only one type.

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
2023-05-17 by Ryan Rutledge : v0.1 - Initial version of script. Provides basic selection manipulation for PCB components and associated .Designator special strings.
