### [DOWNLOAD](https://altium-designer-addons.github.io/DownGit/#/home?url=https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20PCB/QuickSilk)

# QuickSilk
### (formerly MoveAPdesignators2)
This script is a tool to quickly place silkscreen designators and comments around components.\
Click on the component, then click in the octants around the component to place the designator or comment on that side of the part, following autoposition rules.\
Designator or Comment will be automatically moved closer to the parent component according to configured clearance settings.

Script can also change existing auto-positioned designators (original MoveAPdesignators function with some enhancements) to manual and move them by a user-defined or automatic amount toward their owner component. Will operate on all or selected components.\
The direction of the movement depends on the current autoposition status.

### Configuration/Mass Editing GUI
![GUI Screenshot](QuickSilk_GUI.png)

### Interactive Placement Demo
![Interactive Placement Demo](QuickSilk_Demo.gif)

## Credits
- by Ryan Rutledge
- Credit to Mattias Ericson & Tony Chilco for the original *MoveAPdesignators* script I started from
- Credit to Stephen Thompson (@coffeenmusic) for the object overlap code I took from the AutoPlaceSilkscreen script to get AutoMove working

## How to install and use
_Step 1_: [DOWNLOAD](https://altium-designer-addons.github.io/DownGit/#/home?url=https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20PCB/QuickSilk) script

_Step 2_: integrate the script into Altium Designer and execute it.\
If you are a newcomer to Altium scripts, [please read the "how to" wiki page](https://github.com/Altium-Designer-addons/scripts-libraries/wiki/HowTo_execute_scripts).

## Known Issues
- **ALTIUM BUG**: When a component is flipped to the other side of the board, autopositioning initially still orients as if it is on the original side. Closing and re-opening the PcbDoc will make the autoposition work properly.
- An unlocked component on a different board layer will be picked over a locked component on the current layer due to priority order.
- SpaceNavigator will not work during interactive placement mode if it was started from the GUI.
- Clicking too quickly can register as double-click, script is not mouse-gesture responsive

# Features
## Interactive Designator and Comment placement (no GUI, works in 2D and 3D mode)
- Accessed by launching `_QuickSilk` or `InteractivelyAutoposition` script procedure
- Shows instructions in status bar
- After launching, prompts user to click on a component
    - Default behavior is to place Designator
    - Holding CTRL while clicking on the component will place the Comment instead of Designator
- After choosing a component, click in one of the octants around the component to autoposition ithe Designator/Comment n that octant.\
For example: 
    - clicking in the upper-right octant (~45°) will set autoposition to "Right-Above"
    - clicking in the bottom octant (~270°) will set autoposition to "Center-Below"
- After clicking the position location, script will automatically move the Designator/Comment toward the component (within 120 mil) as close as it can while clearing silkscreen, pads, component bodies, etc.
    - Default behavior is to place Designator/Comment with 0° rotation *relative to the component orientation*, only taking objects in the parent component into account. Free silkscreen and other components' designators and pads will be ignored.
    - **CLEARANCE MODE:** Hold CTRL while clicking the location to NOT ignore objects outside the parent component
        - In this mode, AutoMove will attempt up to 30 mils (in 5mil steps) of nudging in each perpendicular direction to try to find a passing solution
        - Enable modifier key or lock behavior through GUI (will persist even when not launched from GUI)
    - Hold ALT to place the Designator/Comment at 90° rotation instead.
    - CTRL and ALT can be combined
## Any-Angle Autopositioning
- Enable or Disable through GUI (will persist even when not launched from GUI)
- Only applies to Interactive placement command
- Basically gives you autopositioning of Designators and Comments that aren't limited to angle multiples of 90°
- "Normal" placement will orient the text at 0° relative to the component's zero orientation
- "Orthogonal" placement (ALT key) will orient the text at 90° relative to the component's zero orientation
- Designator/Comment will be normalized to be right-reading after it is manipulated
## Mass modification of Autopositioned Designators and Configuration of AutoMove Clearance (GUI)
- Accessed by launching `_GUI` or `TweakDesignators` script procedure
- Moves by a fixed or automatically-determined movement amount toward the owner component. Automatic movement works by moving it in the autoposition-derived direction until it hits (with some hard-coded clearance constants) a pad, silkscreen line, component body, etc.

# Summary of changes since MoveAPdesignators
- Added ability to set justification anchor so that changes to designator size or length grow in the same direction AutoPosition would grow (manual but smarter)
- Added 8 configurable preset values
- Remembers last-used settings
- Negative inputs move designators farther away from component instead of closer
- Works in AD 19+
- Added Automatic movement amount detection
- Added Interactive designator and comment placement tool

# Changelog
- 2022-11-28 - MoveAPdesignators2 Ver 2.0 : Initial release based on MoveAPdesignators scrip Ver 1.2; uses new string justification settings and fixed bug with designator not actually moving coords; added presets and restore last used values
- 2022-11-28 - MoveAPdesignators2 Ver 2.01 : fixed justification for rotated strings (approximated to nearest 90° rotation)
- 2022-11-28 - MoveAPdesignators2 Ver 2.02 : *actually* fixed justification for rotated strings (I think)
- 2022-11-28 - MoveAPdesignators2 Ver 2.03 : ***actually*** *actually* fixed justification and movement for rotated strings (my test PcbDoc was glitched where bottom side designators were mirrored in place or something)
- 2023-02-15 - MoveAPdesignators2 Ver 2.04 : fixed support for moving designators away with negative input value; maybe, *possibly*, ***actually*** fixed justification and movement for the last time (*found a bug in Altium where components flipped to the other side of the board have the wrong autoposition behavior until the PcbDoc is closed and re-opened.*)
- 2023-06-20 - MoveAPdesignators2 Ver 2.05 : added support for initially selecting a mix of designators and components, rather than only components; added support for AD19+ AdvanceSnapping text property (might actually fix previous justification bug); better memory safety
- 2023-07-04 - MoveAPdesignators2 Ver 2.06 : 
    - added automatic movement amount support
    - added command (no GUI) to interactively adjust Designator Autoposition setting for components (with automove)
    - improved clearance detection to use object outlines instead of bounding boxes for most things (better support for objects that aren't at 0/90)
    - reworked configuration file to use .ini file instead, for better forward compatibility
- 2023-07-05 - MoveAPdesignators2 Ver 2.07 : added button to GUI to start interactive placement tool; added redundant `Start` procedure for users that are accustomed to using that
- 2023-07-05 - MoveAPdesignators2 Ver 2.08 : fixed issue with interactive placement tool being flipped horizontally for bottom side components
- 2023-07-06 - MoveAPdesignators2 Ver 2.09 : (Interactive placement) added ability to target locked components
- 2023-07-11 - MoveAPdesignators2 Ver 2.10 : (Interactive placement) reworked text rotation to be 0° or 90° relative to part rotation (i.e. normal or orthogonal to part, not board); **autoposition now supports any placement angle**
- 2023-07-13 - MoveAPdesignators2 Ver 2.11 : 
    - (interactive placement, when other components NOT ignored) will try up to 30 mils (in 5mil steps) of nudging in each perpendicular direction to try to find a passing solution
    - Cursor will change when picking component, picking location, and when AutoMove is processing
    - Visible grid will be set to 5mil or 0.1mm at start of interactive placement as large grid can mess things up
- 2023-07-14 - QuickSilk Ver 1.00 : Renamed to "QuickSilk"
    - fixed issue where component could be picked if some of its layers were visible (eg silkscreen) even though its copper layer was hidden. Intended behavior is that only components whose COPPER layer is enabled will be selectable.
    - clearance constraints are now configurable through GUI
    - main GUI now includes configuration values
    - improved cursor lag issue
- 2023-07-16 - QuickSilk Ver 1.01 : optimization and polish pass
    - added quicker nudging routine when avoiding all objects (exhaustive search fallback is available as "Try Extra Offsets" config option)
    - added separate clearance mode option for mass AutoMove
    - GUI polish