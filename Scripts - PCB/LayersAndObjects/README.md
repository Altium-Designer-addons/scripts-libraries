# LayersAndObjects
This scripts can be used to show or hide layers and objects.

It shows all layers in a form and by clicking you turn them on or off. This is easier to use than "Board Layers and colors" dialog because:
- View is automatically updated and user can see the view when clicking on checkboxes, so if something also is in a way it can be turned off.
- This is not dialog so user can control PCB while the form is shown.

This one was made from scratch, because I wanted to remove tabs from "ShowHideLayers" script. It shows layers in blocks that can be shown or hidden. It has couple of more enhancements than "ShowHidelayers" script:
- Support for all 48 copper layers (you will never manage to fit them on screen)
- You can disable group of layers by clicking in the upper right corner of the group. If you disable the group you still see "All" and "Visible" checkboxes.
- "Visible" checkbox allows you to disable the group from view without changing checkbox values.
- Click on Layer Color makes that Layer Current Layer.
- Current layer has bolded checkbox.

Script was updated by allowing user to modify objects view in Final, Draft and hidden forms.

Also, Script was modified to enable it to work when documents are switched, just like a panel works. So you can on-the-fly change between PCB documents, or other documents too (other documents will have empty panel, but problems should not appear) and panel will refresh.

Finally, Script UI will now show board database internal layer names (MidLayer2, Mechanical13, etc)

## How to install and use
_Step 1_: [DOWNLOAD](https://altium-designer-addons.github.io/DownGit/#/home?url=https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20PCB/LayersAndObjects) script

_Step 2_: integrate the script into Altium Designer and execute it.\
If you are a newcomer to Altium scripts, [please read the "how to" wiki page](https://github.com/Altium-Designer-addons/scripts-libraries/wiki/HowTo_execute_scripts).

## Changelog
2011/12/14: Save position to ini file (Tony Chilco)\
2011/12/14: Tab object resize with main dialogue (Tony Chilco)\
Ver 2.6: Modified to Work with AD14 (Randy Clemmons)\
2022/11/23: v2.7 - added internal database layer names to dialogue; added version to UI (Ryan Rutledge)


## Credits
Created by: Petar Perisin

Improved by:\
Tony Chilco\
Randy Clemmons\
Ryan Rutledge
