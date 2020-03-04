# Menu_SVN, dedicated SVN menu for Altium Designer

![Screen capture AD19](https://github.com/Altium-Designer-addons/scripts-libraries/raw/master/Menu_SVN/Menu_SVN_AD19.png "Screen capture AD19")

![Screen capture AD17](https://github.com/Altium-Designer-addons/scripts-libraries/raw/master/Menu_SVN/Menu_SVN_AD17.png "Screen capture AD17")


## How to use

See [the wiki page](https://github.com/Altium-Designer-addons/scripts-libraries/wiki/Menu_SVN)


## How to edit

All customizations are stored in a `DXP.RCS` file.
1) Edit the `DXP.RCS` file
2) For test purposes only: copy the file to `%AppData%\Altium\Altium Designer <GUID>\`


## How to deploy

1) Make a ZIP archive file containing the files `DXP.RCS` and `IncludedFiles.ini`
2) Rename the ZIP file in `Menu_SVN.DXPPrf`
3) Open Altium Designer and load the file `Menu_SVN.DXPPrf` from the preferences panel ; see the below capture for an example

![Load customizations](https://github.com/Altium-Designer-addons/scripts-libraries/raw/master/Menu_SVN/Load_Custom_Menus_Altium.png "Load customizations")


## More help

Customizing Altium Designer: https://www.altium.com/documentation/altium-designer/client-cmd-customizeresourcescustomizeresources-ad

Resources file: https://www.altium.com/documentation/altium-dxp-developer/server-configuration-files#!resources-file

Resources file and its sections: https://www.altium.com/documentation/altium-dxp-developer/integrating-an-extension-in-altium-designer#!resources-file-and-its-sections
