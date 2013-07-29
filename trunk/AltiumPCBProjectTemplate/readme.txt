Download [addons Download Altium PCB Project Template]

= Altium PCB Project Template =
PCB Project Template is sample project template for Altium Designer with all necessary settings of source documents and settings of output generation process. It should make starting of a new project easier. Please read [#Terms_of_use Terms of use] at the end of this page.
Difference of 2 layer and 4 layer board is in the PcbDoc file (number of layers, documentation details) and in setup fog generation output files.

= How to start my project using this template =

Detailed description of template files can be found in section [PCBProjectTemplate#Conten_of_the_template].

  # Project
    # Unzip the package folder with desired numbers of layers
    # Rename main folder to your project name
    # Rename file "Project Name.PrjPcb" to your project name
    # Rename PCB file (PcbDoc) in folder PCB\ to your project name
    # Rename schematic file (SchDoc) in Source\ folder to the name of your schematic sheet number 1
    # Copy SchDoc file as many time as you need to reach final number of schematic sheets in your ptoject
    # Load project file (PrjPcb) into Altium Designer
    # Use menu Project � Add Existing to Project and add all files from PCB and Source folder (you would need to call the command twice to add twoo folders)
    # Go into Project � Project Options � Parameters tab and change values of user parameters 
  # Schematics
    # Go into each schematic sheet and set required parameters in Design � Document Options � Parameters tab
    # Draw your schematics, components can be included from Libraries panel
    # Run logical annotation (component numbering and document numbering in source documents) by Tools � Annotate Schematics and Tools � Number Schematic Sheets 
    # Run physical annotation (component numbering and document numbering as it will be displayed on PCB) by Tools � Board Level Annotations and Tools � Annotate Compiled Sheets
    # Use Project � Compile PCB Project to run Electrical Rule Check (ERC) and solve the reported issues - some of them by NoERC directive probably
  # PCB
    # Import information from your schematic sheet into PCB document by Design � Update PCB document in schematic editor or Design � Import Changes from Project in PCB editor
    # Check settings in Design � Layer Stack Manager according to your board stackup. Defauls stack is set for 2 or 4 layers as it is defined by Seeed Studio Fusion service
    # Check settings in Design � Rules to comply with parameters of your board manufacturer. Design rules in this template are set according to Seeed Studio Fusion service
    # Place your components and route the board
    # If polygon pours are used for GND signal you can add them into GNDpolygons class in Design � Classes � Polygon Classes what causes they will be handled with differen clearance by ClearanceGNDpolygons rule
    # Run Tools � Design Rule Check � Run Design Rule Check and solve all violations through PCB Rules and Violations panel by PCB modifications and DRC exceptions
  # Outputs
    # Open Outputs.OutJob file and click on Generate content in each Output container what will produce all data outputs into the separate folders described in [#Template_Folder_Structure Template Folder Structure] section
    # Check all generated files, Gerber files will be automatically open in CAMtastic 
	# Backup all project data prior sending to a manufacturer by Project � Project Packager (turn on timestamp option)
    # Send data to the boar manufacturer, do the assembly, testing ...
  
Note: All file extensions must remain untouched during renaming operations.

= Content of the template =
== Template Folder Structure ==
This template is using fixed folder structure. The folders contain following files (typical file extension):
  * BOM\ - bill of material for purchasing purposes (XLS,XLSX)
  * Documentation\ - documentation for printing (PDF)
  * Export 3D\ - model of PCB in 3D STEP format (STEP)
  * Fabrication\ - all data for board manufacturing and assembling
    * BOM\ - assembly bill of material for manual assembling (XLS,XLSX)
    * Gerber\ - PCB export in gerber format for PCB manufacturing (G??)
    * NC Drill\ - program for CNC drill machine (TXT)
    * Pick Place\ - assembly data for machine assembly (TXT)
  * History\ - automatically managed history of project available in Storage Manager panel
  * Checks\ - results of validation outputs
    * DiffReport\ - results of consistency check between schematics and PCB (HTML)
    * DRC\ - results of Design Rule Check of PCB(HTML)
    * ERC\ - results of Electrical Rule Check od schematics (HTML)
  * Libraries\ - project related libraries, usually not necessary, DXP � Preferences � Data Management � Installed Libraries used instead (SchLib, PcbLib, IntLib), template contains Stamp.SchLib (schematic library with tile block for schematic files) and Fiducial.PcbLib (footprint/package library with fiducial markers)
  * PCB\ - PCB file (PcbDoc)
  * Settings\ - settings of batch output generation (OutJob)
  * Source\ - schematic files/sheets (SchDoc)

== Project ==
  * PCB project in this template is set in default settings what is usually ok. 
  * Global Parameters - parameters which has the same value for all project documents. These parameters can be set on Project � Project Options � Parameters tab. The parameter values can be displayed anywhere in the project by =parameter_name in schematic documents or .parameter_name in PCB document (don't use spaces or special characters in parameter name). Predefined parameters are:
    * GlobalProjectLink - short web link to the project or company web page for future updates
    * GlobalProjectName - project name withou revision number
    * GlobalProjectRevision - revision number, e.g. v1.3c
    * GlobalProjectTeam - team, company or department name
    * GlobalRevisionDate - release date of the revision

== Schematic ==
  * Title block from library - contains actual date and time which will produce timestamp during document printing. The title block (stamp) in the template comes from Stamp.SchLib from Libraries\ folder. It's included as a component into the schematic file and it's locked to prevent unwanted moving with the stamp. Component type is Graphic so the component won't be synchonized with PCB and won't be in BOM reports. Values are loaded from Global/Project parameters and Local/Document parameters.  
  * Size and orientation can be set by Design � Document Options � Sheet Options tab
  * Local parameters - parameters which has different value for each project document. These parameters can be set on Design � Document Options � Parameters tab. The parameter values can be displayed anywhere in the document by =parameter_name (don't use spaces or special characters in parameter name). Used parameters are predefined by Altium. Template stamp is using parameters:
    * Author - author name and contact info (email)
    * Title - descriptive name of the schematic sheet 
    * There are also SheetNumber, SheetTotal parameters used in the document parameters. They are filled by annotation process described previously.

Hint: Document Parameters of all schematic documents can be modified together through Tools � Parameter Manager set to Documents options only. Keep option Exclude System Parameters unchecked.

== PCB ==
PCB file contains settings of layer stackup, naming of mechanical layers, layer pairs, layer sets (predefined sets of displayed layers) and basic set of rules usually given by PCB manufacturing technology. PCB used in this template is using clearance 0.2mm, minimum track width 0.16mm, via diameter 0.3mm and via annular ring 0.2mm what comply (wit some margin) with Fusion service by Seeed Studio and other manufacturing services. 
  * Technology Design Rules - defines technology limits mentioned above
  * Additional Design rules
    * ClearanceGNDpolygons - exception of general clearance rule for polygons which are members of GNDpolygons class in Design � Classes � Polygon Classes to be able define setback for polygons independently.
	* ClearancePadVia - additional clearance in between component pads and vias in general to keep vias in distance from pads
	* Note: All design rules are exported to RUL files in PCB\ folder. You can import the rules by Design � Rules dialog to another PCB files.
  * PCB file contains simple tile block/stamp drawn directly in the PCB file. Values are loaded from Global/Project parameters and system parameters. Primitives of the stamp are locked to prevent unwanted moving.
  * Mechanical layers usage
	* Outlines (Mech 1) - board outline, use Design � Board Shape � Define from selected primitives to update actual Board Shape from this layer
	* Dimensions (Mech 2) - dimensions of board shape and important objects (holes)
	* Ref (Mech 5) - auxiliary reference data in components (e.g. important coordinates in connectors) 
	* Assembly Text Top (Mech 6) - text reference for assembly of components on top side (typically ".designator" strings)
	* Assembly Text Bot (Mech 7) - as above for the bottom side of the board
	* Assembly Top (Mech 8) - outline and pin 1 marking for components on top side (similar to component silkscreen but without clipping by exposed copper)
	* Assembly Bot (Mech 9) - as above for the bottom side of the board
	* 3D Top (Mech 13) - 3D models for components
	* 3D Bot (Mech 14) - as above for the bottom side of the board
	* Courtyard Top (Mech 15) - 
	* Courtyard Bot (Mech 16) - as above for the bottom side of the board
	  * Note: Top and Bot layers are linked by layer pairs settings in the PCB file, the data will be distributed into the layers automatically
      * Hint: settings of mechanical layers can be imported/exported by [https://code.google.com/p/altium-designer-addons/downloads/detail?name=MechLayerNames_v2.0.zip&can=2&q= MechLayerNames script]
  
== Outputs ==
Outputs from the PCB projects has many options so the setup is fixed in OutputJob file which is able to run batch generation of outputs. The Outputs.OutJob is split into several containers (output folders according to the output type). You should click on Generate content in all containers separately to get complete set of updated output date. Predefined containers:
  * Checks - validation process, you should check all outputs in Checks folder befor you create final data
  * Documentation - PDF file container with schematic and PCB printouts
    * Schematic Prints - schematic sheets printout
    * PCB View - one page PCB composite view
    * Dimensions - board dimensions
    * Layout - routing of each copper layer seprately (visual routing check)
    * Layout test footprints - top and bottom copper layers with only pads displayed for testing printouts on paper printout before boar manufacturing
    * Assembly - assembly drawing from top and bottom side
    * PCB 3D Print Top - PCB 3D view from top
    * PCB 3D Print Bottom - PCB 3D view from bottom
	  * Hint: 3D views can be set by positioning the 3D view directly in PcbDoc an using "Take current camera position" button in the output properties
  * Fabrication - Gerber files, drill files for CNC drill machine and assembly bill of material and pick and place data for assembly machine
  * BOM - purchase bill of material
  * Export 3D - export od PCB 3D model into 3D STEP file

= What's next =
Please check all settings and in case of doubt discuss the settings with your manufacturer specifically. 

If you find any state of document typical for your future designs you can set the document into DXP � Preferences � System � New Document Defaults (the source document must be stored in location where it stays untouched by any modifications).
 
Other advanced techniques of setting up Altium Designer according to your needs can be found at [http://wiki.altium.com/display/ADOH/The+Altium+Designer+Environment wiki.altium.com].

= Terms of use = 
The template is provided free of charge with no warranty or liability.
Feel free to modify the template by your own needs. I would like to encourage you to share your modifications with others.
Please let me know your feedback on this template. Any comments are welcome. You can contact me via email PetrTos (a) gmail.com .

Petr Tosovsky
2013-07-29