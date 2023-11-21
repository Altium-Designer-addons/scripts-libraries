# ReturnViaCheck Script

# DISCLAIMER
This script is provided "AS IS" in the hopes that it will be useful, but comes with no guarantees or warranties. Use of this script is conditional on accepting it as-is, and the user is responsible for any issues that may arise from its use, including failure to detect a critical problem that results in scrap boards. Please thoroughly verify its fitness for your particular use case.

## [DOWNLOAD](https://altium-designer-addons.github.io/DownGit/#/home?url=https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts+-+PCB/ReturnViaCheck)

# INITIAL REQUIREMENTS SPEC
## Non-Modal GUI
- Non-modal so that findings can be addressed without restarting the script (better UX)
## Configurable check distance
- If any eligible return via exists within range, via passes check
- If no eligible return via exists within range, add signal via to TInterfaceList of vias that failed check
- Support either metric or imperial units
- Check distance will set size of SpatialIterator but final determination will be based on radial distance between via centers
  - verify that SpatialIterator search area only needs to touch via centers but need not enclose entire via
## Drop-Down to filter signal vias by Specific Net, Net Class, or All Nets
- Multiselection would be really ugly in GUI because of number of nets that can exist
- If signal via selection filter is "All Nets" then how to determine which nets to ignore? 
- Obviously selected "Return Via" filter should be ignored
- Ignore any vias without a trace wired up to them?
  - return vias can have traces wired up to them
  - vias of both types could have vias in pad without any traces wired
## Drop-Down to filter return vias by Specific Net or by Net Class
- How to avoid treating return vias outside selected net or net class as signal vias?
- Should through-hole pads of a suitable net be considered? I'm leaning toward no. User can ignore if desired.
- Return vias need to *actually* span the relevant reference plane layers
  - Without getting into actual impedance structure definition, there are some corner cases to watch out for
  - For an internal signal trace, return via should span layers adjacent to signal layers
  - If reference planes are not on adjacent layers it may accept a via that doesn't span both reference planes
  - contrived example: for a SIG1 <> SIG3 route in a SIG1-GND1-SIG2-SIG3-GND2-SIG4 stackup, a SIG1-SIG3 GND via is insufficient because it does not connect GND1 to GND2
  - another scenario: stackup above has GND1-GND2 buried via
## Do we need to enumerate layers and allow tagging layers as GND references?
- Much simpler implementation would be to only consider full-stack vias as eligible return vias
- For complex stackups, only considering full-stack vias could raise false positives (reinforces need for ability to ignore/waive detected failures)
## Dedicated list of nets to exclude?
- Same potential multiselection problem as above
## Save states between runs
- Lists of nets/classes could be challenging
- Needs to not break if netlist changes (eg. with different board or just a netlist change)
- Would be nice to save state for a few "recent" boards, but probably not worth implementation effort
## Buttons to Check All, Recheck, and Clear Results
- Check All button: checks all vias. After clicking, selected filter settings are locked until clear results button is used
- Recheck button: (for performance reasons) only repeats check on vias in failed via list. If any now pass, list will be updated
- Clear Results button: clears failed via list and unlocks selection filters again
## Zoom to failing vias button?
- Clears selection, selects all failing vias, zooms to selection, then clears selection again
## "Next" and "Previous" buttons to step through the list of failed vias
- Only active while list of fails is not empty
- There should be a button to "Ignore Selected" that removes via from list of fails as if it were rechecked and passed
- There should also be a button to "Center" on the current via from the list in case user pans away
- Would be really handy to display a tooltip by the current via (like the dimensions after a measure command)



# What This Script Is
This script is a utility tool to help detect non-GND signal vias without a nearby GND return via.

## Why?
When a high-speed (or RF) signal's route changes layers, the reference plane for the signal also changes. Just as the incident signal requires a via to change layers, so too does the "return" signal need a path between reference planes.

## How to install and use
_Step 1_: [DOWNLOAD](https://altium-designer-addons.github.io/DownGit/#/home?url=https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts+-+PCB/ReturnViaCheck) script

_Step 2_: integrate the script into Altium Designer and execute it.\
If you are a newcomer to Altium scripts, [please read the "how to" wiki page](https://github.com/Altium-Designer-addons/scripts-libraries/wiki/HowTo_execute_scripts).

## Usage guide
### USE THE GUI
- Accessed by launching `_GUI` script procedure

### Configuration/Editing GUI
![GUI Screenshot](ReturnViaCheck_GUI.png)

### **ENABLE VIAS IN SELECTION FILTER**
### Operation 1
How to execute operation 1

## Features

### Eligible Objects
Vias

## Known Issues
### <Known Issue #1>
Description of known issue #1 and any relevant workaround

## Change log
- 2023-11-20 by Ryan Rutledge : v0.01 - Initial draft of script documentation and requirements spec
