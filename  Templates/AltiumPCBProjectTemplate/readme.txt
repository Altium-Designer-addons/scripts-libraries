= Altium PCB Project Template =
PCB Project Template is sample project template for Altium Designer with all necessary settings of source documents and settings of output generation process. It should make starting of a new project easier. Please read Terms of use at the end of this file.
The template was created and is using features of Altium Designer 13.3.
Design rules in this template are set according to Seeed Studio Fusion service, what is international and I think affordable service which I successfully used for my hobby projects. The difference of 2 layer and 4 layer board template is in the PCB file (number of layers, documentation details) and in the setup for generation output files.

= How use this template for your project =
Detailed description of template files can be found in attached file "Altium PCB Project Template.pdf" or at Altium Designer Addons wiki page AltiumPCBProjectTemplate.
https://code.google.com/p/altium-designer-addons/wiki/AltiumPCBProjectTemplate
Note: All file extensions must remain untouched during renaming operations.

  * Project
    # Unzip the package folder with desired number of layers
    # Rename main folder to your project name
    # Rename file "Project Name.PrjPcb" to your project name
    # Rename PCB file (PcbDoc) in folder PCB\ to your project name
    # Rename schematic file (SchDoc) in Source\ folder to the name of your schematic sheet number 1
    # Copy SchDoc file as many time as you need to reach final number of schematic sheets in your project
    # Load project file (PrjPcb) into Altium Designer, error message about missing file can be ignored
    # Use menu Project » Add Existing to Project and add all PcbDoc and SchDoc files from PCB and Source folder (you would need to call the command twice to add two folders)
    # Go into Project » Project Options » Parameters tab and set the values of user global/project parameters according to your project 
  * Schematics
    # Go into each schematic sheet and set required local/document parameters in Design » Document Options » Parameters tab
    # Draw your schematics, components can be included from Libraries panel
    # Run logical annotation (component numbering and document numbering in source documents) by Tools » Annotate Schematics and Tools » Number Schematic Sheets 
    # Run physical annotation (component numbering and document numbering as it will be displayed on PCB and in all outputs) by Tools » Board Level Annotations and Tools » Annotate Compiled Sheets
      * see http://wiki.altium.com/display/ADOH/Understanding+Design+Annotation for details
    # Use Project » Compile PCB Project to run Electrical Rule Check (ERC) and solve the reported issues - some of them by NoERC directive probably
  * PCB
    # Import information from your schematic sheet into PCB document by Design » Update PCB document in schematic editor or Design » Import Changes from Project in PCB editor
    # Check settings in Design » Layer Stack Manager according to your board stackup. Defauls stack is set for 2 or 4 layers as it is defined by Seeed Studio Fusion service
    # Check settings in Design » Rules to comply with parameters of your board manufacturer. Design rules in this template are set according to Seeed Studio Fusion service
    # Place your components and route the board
    # If polygon pours are used for GND signal you can add them into GNDpolygons class in Design » Classes » Polygon Classes what causes they will be handled with differen clearance by ClearanceGNDpolygons rule
    # Run Tools » Design Rule Check » Run Design Rule Check and solve all violations through PCB Rules and Violations panel by PCB modifications and DRC exceptions
  * Outputs
    # Open Outputs.OutJob file and click on Generate content in each Output container what will produce all data outputs into the separate folders
    # Check all generated files, Gerber files will be automatically open in CAMtastic (build-in gerber editor)
    # Backup all project data prior sending to a manufacturer by Project » Project Packager (turn on timestamp option)
    # Send data - Gerber and NC drill folder - to the boar manufacturer, do the assembly, testing ...
  
= Terms of use = 
The template is provided free of charge with no warranty or liability.
Feel free to modify the template by your own needs. I would like to encourage you to share your modifications with others.
Please let me know your feedback on this template. Any comments are welcome. You can contact me via email PetrTos (a) gmail.com .

Petr Tosovsky
2013-07-29
Altium Designer Addons page