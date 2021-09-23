*Source: https://github.com/Altium-Designer-addons/scripting-reference/blob/master/HowTo_execute_scripts.md*

# How to execute/run a script in Altium Designer

As an alternative to the official ["running scripts" help page](https://techdocs.altium.com/display/SCRT/Running+Scripts+in+Altium+Designer), we would try to guide you as simply a we can through the process of executing scripts in Altium Designer.
There are (at least) 3 ways of doing this:
1. The "Run script" command
2. A custom menu item
3. A custom toolbar


# Understanding Altium script files

There are three types of files used by Altium Designer in the scripting system:
* The script project file (`*.PrjScr`). It works as any other project files: it "groups" files together. You can define some options for the project.
* The script "unit". It contains some programming code in one of the recognized languages (accepted languages are listed on [the Altium documentation page](http://techdocs.altium.com/display/SCRT/Scripting)).
* The script "form". If your script has graphical capabilities, it probably has a "form" file having the same name as the "unit": this is the definition for a window.


# In video

Thanks to Petar PERISIN, you will be able to see most of the points covered in this tutorial in a [single and short video](https://www.youtube.com/watch?v=w62Hg0EqTVs).


# Solution 1: "Run script"

<a href="https://camo.githubusercontent.com/6ba0aef6ea37acd2eabf74bac4de63ca01ba6435/68747470733a2f2f676f746f6d6174696f6e2e696e666f2f77702d636f6e74656e742f75706c6f6164732f323031382f30342f46696c655f52756e5363726970745f414431392e706e67"><img src="https://camo.githubusercontent.com/6ba0aef6ea37acd2eabf74bac4de63ca01ba6435/68747470733a2f2f676f746f6d6174696f6e2e696e666f2f77702d636f6e74656e742f75706c6f6164732f323031382f30342f46696c655f52756e5363726970745f414431392e706e67" alt="Running a script with AD18" data-canonical-src="https://gotomation.info/wp-content/uploads/2018/04/File_RunScript_AD19.png" height="300"></a>
<a href="https://camo.githubusercontent.com/80f44d6b5d72d5e35364c2556fdf0499948eb923/68747470733a2f2f676f746f6d6174696f6e2e696e666f2f77702d636f6e74656e742f75706c6f6164732f323031382f30342f4458505f52756e5363726970742e706e67"><img src="https://camo.githubusercontent.com/80f44d6b5d72d5e35364c2556fdf0499948eb923/68747470733a2f2f676f746f6d6174696f6e2e696e666f2f77702d636f6e74656e742f75706c6f6164732f323031382f30342f4458505f52756e5363726970742e706e67" alt="Running a script with AD17" data-canonical-src="https://gotomation.info/wp-content/uploads/2018/04/DXP_RunScript.png" height="300"></a>

AD18 and newer: From the "File" menu, clock on "Run Script".\
AD17 and older: From the "DXP" menu, click on "Run script" in the right column.

A new window appears showing a list of available script, that is:
* Scripts made globally available in the preferences (Preferences > Scripting system > Global projects). This requires to have a project script file (`*.PrjScr`).
* Scripts available in the Projects panel, either because they are part of a project or because they are open as free documents.

The list is actually a tree of this form:
* Script project (`*.PrjScr` or Free documents)  
  * Script file (accepted languages are listed on [the Altium documentation page](http://techdocs.altium.com/display/SCRT/Scripting))    
    * Function inside the script (if any)

Choose either a script file or a particular function and click "OK" to run it.

_The script itself is visible but some functions are missing in the list ? Do these functions require some input parameters? Well, the "Run script" dialog is not able to let you start this type of functions because you cannot input anything from there, sorry._


# Solution 2: New menu item

Want to have access to a script by a menu entry?

You have two possible starting points:
* DXP > Customize (only with AD17 and older)
* Right-click on an existing menu > Customize

Then in the section `[Scripts]` you get a list similar to the one from the "Run script" dialog. Drag and drop the requested script file or function in the menu bar, then customize the menu entry with a name, description, image, shortcuts, etc.
When you have closed the "Customize" dialog, your menu entry becomes fully functional and must launch your script properly.


# Solution 3: New toolbar

If you prefer to not put your dedicated script launchers inside the main menu, you can create your own toolbar:
1. "DXP > Customize" or "Right-click on an existing menu > Customize"
2. Switch to the "Toolbars" tab
3. Click on "New", give your toolbar a name

Now that you can find your new (but empty) toolbar in the Altium Designer interface, you can follow the steps of Solution 2 to add a link to a script.

_Cannot locate your freshly created toolbar? Disable then re-enable it, you would probably see it as a "flash" appearing between existing menus._
