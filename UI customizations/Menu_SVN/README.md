# A dedicated SVN menu for Altium Designer (Menu_SVN)

![Screen capture AD19](https://github.com/Altium-Designer-addons/scripts-libraries/raw/master/UI%20customizations/Menu_SVN/Menu_SVN_AD19.png "Screen capture AD19")

![Screen capture AD17](https://github.com/Altium-Designer-addons/scripts-libraries/raw/master/UI%20customizations/Menu_SVN/Menu_SVN_AD17.png "Screen capture AD17")


## Usage

### Contexts

All menus in Altium Designer are "contextual", i.e. depend on the file being edited. The proposed menu and shortcuts are only available in the following contexts:

|File type (example)|Context|
|:---|:---|
|.SchDoc|Schematics editor|
|.PcbDoc|PCB editor|
|.MbsDoc|Multi-board schematics editor|
|.MbaDoc|Multi-board assembly editor|
|.BomDoc|BOM editor|
|.SchLib|Symbol library editor|
|.PcbLib|Footprint library editor|
|.PvLib |Pads & Vias library editor|
|.PCBDwf|Draftsman editor|
|.OutJob|Output job manager|

### Actions

Below is the list of available actions and associated shortcuts present in the menu.\
Each function can work **only if** you have the **focus** on a document. A menu items is greyed out if the function cannot apply on the current file.

|Action|Keyboard shortcut|Special instructions|
|:---|:---|:---|
|Lock|CTRL + ALT + K|
|Unlock|CTRL + ALT + N|
|Refresh|CTRL + ALT + F|
|TSVN Commit folder (2)|CTRL + ALT + PgUp|Needs TortoiseSVN installed + [this script](https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20Misc/TortoiseSVN), see the prerequisites below|
|TSVN Update folder (2)|CTRL + ALT + PgDn|Needs TortoiseSVN installed + [this script](https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20Misc/TortoiseSVN), see the prerequisites below|
|Commit whole project (1)|CTRL + ALT + M|
|Update whole project (1)|CTRL + ALT + U|
|Revert local modifications|CTRL + ALT + V|Needs [this script](https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20Misc/BetterRevertVCS), see the prerequisites below|
|Add to version control|CTRL + ALT + I|"Insert"|
|Remove from version control|CTRL + ALT + D|"Delete"

**Warning 1**: The functions « Commit whole project » and « Update whole project » are built-in features of Altium Designer and apply to the currently active project. If the currently open document is not inside a project, the functions cannot be used.  
**Warning 2**: The functions « TSVN Commit folder » and « TSVN Update folder » are add-ons and apply to the whole content of the directory containing the current file or project. If the current file has never been saved on the disk, the functions cannot be used.


## Installation

### Create the folder structure

Before the creation of the menu you need to follow those instructions:
1. Create a folder named exactly "AltiumDesigner_AddOns" on your root drive C:\
2. Create two other folders into "C:\AltiumDesigner_AddOns\": "MenuIcons" and "Scripts"
3. Download the content of https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/UI%20customizations/Menu_SVN/Icons and put it into the directory "C:\AltiumDesigner_AddOns\MenuIcons\Menu_SVN"
4. Download the content of https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20Misc/BetterRevertVCS and put it into the directory "C:\AltiumDesigner_AddOns\Scripts\BetterRevertVCS"
5. Download the content of https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20Misc/TortoiseSVN and put it into the directory "C:\AltiumDesigner_AddOns\Scripts\TortoiseSVN"
6. Install [TortoiseSVN](https://tortoisesvn.net) (free and open source) to get the full potential of the TortoiseSVN script and menu

You should now have the following folder structure:  
> C:\AltiumDesigner_AddOns\

>> MenuIcons
>>> Menu_SVN
>>>> [icon files]

>> Scripts
>>> BetterRevertVCS
>>>> [script files]

>>> TortoiseSVN
>>>> [script files]

### Import the menu

Now that everything is ready:
1. Load the [pre-made preferences file](https://github.com/Altium-Designer-addons/scripts-libraries/raw/master/UI%20customizations/Menu_SVN/Menu_SVN.DXPPrf) into the Altium Designer preferences.
2. Deselect all options but "System > Customizations" like in the screen capture below and click OK.
3. Restart Altium Designer.

![how to import a custom menu](https://github.com/Altium-Designer-addons/scripts-libraries/raw/master/UI%20customizations/Menu_SVN/Load_Custom_Menus_Altium.png)


## Improvements and modifications

### How to edit

All customizations are stored in a `DXP.RCS` file.
1. Edit the `DXP.RCS` file,  which describes the menu structure and corresponding actions
2. For test purposes only: copy the file to `%AppData%\Altium\Altium Designer <GUID>\`

Or you can do it the hard way by re-creating the menu yourself. Use "DXP > Customize" or right-click on any menu item and choose "Customize".\
Do not forget that you probably need to create the same menu many times if you want it to be accessible in several contexts (e.g. Schematic editor, PCB editor, SCH library editor, etc.). If you do not have specific needs you are encouraged to import the preference file.

### How to deploy

1. Make a ZIP archive file containing the files `DXP.RCS` and `IncludedFiles.ini`
2. Rename the ZIP file in `Menu_SVN.DXPPrf`
3. Open Altium Designer and load the file `Menu_SVN.DXPPrf` from the preferences panel


## More help

Customizing Altium Designer: https://www.altium.com/documentation/altium-designer/client-cmd-customizeresourcescustomizeresources-ad

Resources file: https://www.altium.com/documentation/altium-dxp-developer/server-configuration-files#!resources-file

Resources file and its sections: https://www.altium.com/documentation/altium-dxp-developer/integrating-an-extension-in-altium-designer#!resources-file-and-its-sections
