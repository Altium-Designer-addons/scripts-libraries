<h1>Altium PCB Project Template</h1>
PCB Project Template is sample project template for Altium Designer with all necessary settings of source documents and settings of output generation process for 2 layer and 4 layer boards. It should make starting of a new project easier. Please read <a href='#terms-of-use'>Terms of use</a> at the end of this page.<br>
<br>
The template was created and is using features of Altium Designer 13.3.<br>
<br>
Design rules in this template are set according to Seeed Studio Fusion service, what is international and I think affordable service which I successfully used for my hobby projects. The difference of 2 layer and 4 layer board template is in the PCB file (number of layers, documentation details) and in the setup for generation output files.<br>
<br>
<h1>How use this template for your project</h1>

Detailed description of template files can be found in section <a href='#content-of-the-template'>Content of the template</a>.<br>
<br>
Note: All file extensions must remain untouched during renaming operations.<br>
<br>
<ul><li><b>Project</b>
<ol><li>Unzip the package folder with desired number of layers<br>
</li><li>Rename main folder to your project name<br>
</li><li>Rename file "Project Name.PrjPcb" to your project name<br>
</li><li>Rename PCB file (PcbDoc) in folder PCB\ to your project name<br>
</li><li>Rename schematic file (SchDoc) in Source\ folder to the name of your schematic sheet number 1<br>
</li><li>Copy SchDoc file as many time as you need to reach final number of schematic sheets in your project<br>
</li><li>Load project file (PrjPcb) into Altium Designer, error message about missing file can be ignored<br>
</li><li>Use menu Project » Add Existing to Project and add all PcbDoc and SchDoc files from PCB and Source folder (you would need to call the command twice to add two folders)<br>
</li><li>Go into Project » Project Options » Parameters tab and set the values of user global/project parameters according to your project<br>
</li></ol></li><li><b>Schematics</b>
<ol><li>Go into each schematic sheet and set required local/document parameters in Design » Document Options » Parameters tab<br>
</li><li>Draw your schematics, components can be included from Libraries panel<br>
</li><li>Run logical annotation (component numbering and document numbering in source documents) by Tools » Annotate Schematics and Tools » Number Schematic Sheets<br>
</li><li>Run physical annotation (component numbering and document numbering as it will be displayed on PCB and in all outputs) by Tools » Board Level Annotations and Tools » Annotate Compiled Sheets<br>
<ul><li>see <a href='http://wiki.altium.com/display/ADOH/Understanding+Design+Annotation'>Design Annotation</a> for details<br>
</li></ul></li><li>Use Project » Compile PCB Project to run Electrical Rule Check (ERC) and solve the reported issues - some of them by NoERC directive probably<br>
</li></ol></li><li><b>PCB</b>
<ol><li>Import information from your schematic sheet into PCB document by Design » Update PCB document in schematic editor or Design » Import Changes from Project in PCB editor<br>
</li><li>Check settings in Design » Layer Stack Manager according to your board stackup. Defauls stack is set for 2 or 4 layers<br>
</li><li>Check settings in Design » Rules to comply with parameters of your board manufacturer.<br>
</li><li>Place your components and route the board<br>
</li><li>If polygon pours are used for GND signal you can add them into GNDpolygons class in Design » Classes » Polygon Classes what causes they will be handled with differen clearance by ClearanceGNDpolygons rule<br>
</li><li>Run Tools » Design Rule Check » Run Design Rule Check and solve all violations through PCB Rules and Violations panel by PCB modifications and DRC exceptions<br>
</li></ol></li><li><b>Outputs</b>
<ol><li>Open Outputs.OutJob file and click on Generate content in each Output container what will produce all data outputs into the separate folders described in <a href='#template-folder-structure'>Template Folder Structure</a> section<br>
</li><li>Check all generated files, Gerber files will be automatically open in CAMtastic (build-in gerber editor)<br>
</li><li>Backup all project data prior sending to a manufacturer by Project » Project Packager (turn on timestamp option)<br>
</li><li>Send data - Gerber and NC drill folder - to the board manufacturer, do the assembly, testing ...</li></ol></li></ul>

<h1>Content of the template</h1>
<h2>Template Folder Structure</h2>
This template is using fixed folder structure. The folders contain following files (typical file extension):<br>
<ul><li><b>BOM\</b> - bill of material for component purchasing purposes (XLS, XLSX)<br>
</li><li><b>Documentation\</b> - documentation for printing (PDF)<br>
</li><li><b>Export 3D\</b> - model of PCB in 3D STEP format (STEP)<br>
</li><li><b>Fabrication\</b> - all data for board manufacturing and assembling<br>
<ul><li><b>BOM\</b> - assembly bill of material as additional document to the assembly view in printed documentation from Documentation\ folder for manual assembling (XLS,XLSX)<br>
</li><li><b>Gerber\</b> - PCB export in gerber format for PCB manufacturing (G??)<br>
</li><li><b>NC Drill\</b> - program for CNC drill machine (TXT)<br>
</li><li><b>Pick Place\</b> - assembly data for machine assembly (TXT)<br>
</li></ul></li><li><b>History\</b> - automatically managed history of project available in Storage Manager panel, versions are created when a document is saved, the folder can be deleted when project history is not needed<br>
</li><li><b>Checks\</b> - results of validation outputs<br>
<ul><li><b>DiffReport\</b> - results of consistency check between schematics and PCB (HTML)<br>
</li><li><b>DRC\</b> - results of Design Rule Check of PCB (HTML)<br>
</li><li><b>ERC\</b> - results of Electrical Rule Check of schematics (HTML)<br>
</li></ul></li><li><b>Libraries\</b> - project related libraries, usually not necessary, DXP » Preferences » Data Management » Installed Libraries used instead (SchLib, PcbLib, IntLib), template contains Stamp.SchLib (schematic library with title block for schematic files) and Fiducial.PcbLib (footprint/package library with fiducial marker)<br>
</li><li><b>PCB\</b> - PCB file (PcbDoc)<br>
<ul><li><b>additional</b> - script and configuration files for mechanical layers naming, exported DRC settings, screenshots of some settings in the PCB (INI, RUL. PNG)<br>
</li></ul></li><li><b>Settings\</b> - settings of batch output generation (OutJob)<br>
</li><li><b>Source\</b> - schematic files/sheets (SchDoc)</li></ul>

<h2>Project</h2>
<ul><li>PCB project in this template is set in default settings what is usually ok.<br>
</li><li><b>Global Parameters</b> - parameters which has the same value for all project documents. These parameters can be set on Project » Project Options » Parameters tab. The parameter values can be displayed anywhere in the project by =parameter_name in schematic documents or by .parameter_name in PCB document. Don't use spaces or special characters in parameter name. Template predefined parameters are:<br>
<ul><li><b>GlobalProjectLink</b> - short web link to the project or company web page with project details<br>
</li><li><b>GlobalProjectName</b> - full project name without revision number<br>
</li><li><b>GlobalProjectRevision</b> - revision number, e.g. v1.3<br>
</li><li><b>GlobalProjectTeam</b> - team, company or department name<br>
</li><li><b>GlobalRevisionDate</b> - release date of the revision</li></ul></li></ul>

<h2>Schematic</h2>
<ul><li><b>Title block</b> from library - The title block (stamp) in the template comes from Stamp.SchLib from Libraries\ folder. It's included as a component into the schematic file and it's locked to prevent unwanted moving of the stamp. Component type is Graphic so the component won't be synchronized with PCB and won't be in BOM reports. Values are loaded from Global/Project parameters and Local/Document parameters. The stamp contains actual date and time from system parameters what will produce timestamp during document printing.<br>
</li><li><b>Size and orientation</b> can be set by Design » Document Options » Sheet Options tab<br>
</li><li><b>Units</b> - set to DXP units, 1 DXP unit = 10 mils, imperial units are recommended because most of available libraries are using imperial grid<br>
</li><li><b>Grid</b> - 5 DXP units<br>
</li><li><b>Local parameters</b> - parameters which has different value for each project document. These parameters can be set on Design » Document Options » Parameters tab. The parameter values can be displayed anywhere in the document by =parameter_name. Don't use spaces or special characters in parameter name. Used parameters are predefined by Altium. Template stamp using these parameters:<br>
<ul><li><b>Author</b> - author name and contact info (email)<br>
</li><li><b>Title</b> - descriptive name of the schematic sheet, 1 DXP unit = 10 mils,<br>
</li><li>There are also <b>SheetNumber</b>, <b>SheetTotal</b> parameters used in the document parameters. They are filled by annotation process described previously.</li></ul></li></ul>

Hint: Document Parameters of all schematic documents can be modified together through Tools » Parameter Manager set to Documents options only. Keep option Exclude System Parameters unchecked.<br>
<br>
<h2>PCB</h2>
PCB file contains settings of layer stackup, naming of mechanical layers, layer pairs, layer sets (predefined sets of displayed layers) and basic set of rules usually given by PCB manufacturing technology. PCB used in this template is using clearance 0.2mm, minimum track width 0.16mm, via diameter 0.3mm and via annular ring 0.2mm what comply (including some margin) with Fusion service by Seeed Studio and other manufacturing services.<br>
<ul><li><b>Default Grid</b> - 0.1mm, it will not produce any big issue when you change grid or even units used in the PCB during layout<br>
</li><li><b>Technology Design Rules</b> - basic rules with highest priority number defines technology limits mentioned above, other rules (exceptions) has to have lower number of priority and can't break the values given by technology limits to keep board manufacturability<br>
</li><li><b>Additional Design rules</b>
<ul><li><b>ClearanceGNDpolygons</b> - exception of general clearance rule for polygons which are members of GNDpolygons class in Design » Classes » Polygon Classes to be able define setback for polygons independently.<br>
</li><li><b>ClearancePadVia</b> - additional clearance between component pads and vias to keep vias in distance from pads (because of assembly issues)<br>
</li><li>Note: All design rules are exported to RUL files in PCB\ folder. You can import the rules by Design » Rules dialog to another PCB files.<br>
</li></ul></li><li>PCB file contains simple <b>title block</b>/stamp drawn directly in the PCB file. Values are loaded from Global/Project parameters and system parameters. Primitives of the stamp are locked to prevent unwanted moving.<br>
</li><li><b>Copper Layers</b>
<ul><li>2 layer PCB <img src='https://github.com/Altium-Designer-addons/scripts-libraries/raw/master/Document%20templates/AltiumPCBProjectTemplate/2%20Layer%20PCB%20template/PCB/additional/2%20Layer%20PCB%20template.png' />
</li><li>4 layer PCB - planes are inverse layers (line drawn in this layer mean there will be a gap in copper) used for power supply distribution, they can be split into smaller regions with different net attached by closed shapes drawn by lines, see Altium wiki page <a href='http://wiki.altium.com/display/ADOH/Internal+Power+and+Split+Planes'>Split Planes</a> <img src='https://github.com/Altium-Designer-addons/scripts-libraries/raw/master/Document%20templates/AltiumPCBProjectTemplate/4%20Layer%20PCB%20template/PCB/additional/4%20Layer%20PCB%20template.png' />
</li></ul></li><li><b>Mechanical layers</b> usage<br>
<ul><li><b>Outlines</b> (Mech 1) - board outline, use Design » Board Shape » Define from selected primitives to update actual Board Shape from this layer<br>
</li><li><b>Dimensions</b> (Mech 2) - dimensions of board shape and important objects (mounting holes)<br>
</li><li><b>Ref</b> (Mech 5) - auxiliary reference data in components (e.g. important spots in connectors)<br>
</li><li><b>Assembly Text Top</b> (Mech 6) - text reference for assembly of components on top side (typically ".designator" strings)<br>
</li><li><b>Assembly Text Bot</b> (Mech 7) - as above for the bottom side of the board<br>
</li><li><b>Assembly Top</b> (Mech 8) - outline and pin 1 marking for components on top side (similar to component silkscreen but without clipping by exposed copper)<br>
</li><li><b>Assembly Bot</b> (Mech 9) - as above for the bottom side of the board<br>
</li><li><b>3D Top</b> (Mech 13) - 3D models for top side components<br>
</li><li><b>3D Bot</b> (Mech 14) - as above for the bottom side components<br>
</li><li><b>Courtyard Top</b> (Mech 15) - outline of the component including space needed for assembly purposes where no other components should be placed<br>
</li><li><b>Courtyard Bot</b> (Mech 16) - as above for the bottom side components on the board<br>
Note: Top and Bot layers are linked by layer pairs settings in the PCB file, the data will be distributed into the layers automatically when the component is flipped to other board side.</li></ul></li></ul>

~~Hint: settings of mechanical layers can be imported/exported by <a href='https://code.google.com/p/altium-designer-addons/downloads/detail?name=MechLayerNames_v2.0.zip&can=2&q='>MechLayerNames</a> script.~~ Settings from the template are stored in PCB\additional\ folder.<br>
<br>
<h2>Outputs</h2>
Outputs from the PCB projects has many options so the setup is fixed in OutputJob file which is able to run batch generation of outputs. The Outputs.OutJob is split into several containers (output folders according to the output type). You should click on Generate content in all containers separately to get complete set of updated output data. Predefined containers:<br>
<ul><li><b>Checks</b> - validation process, you should check all outputs in Checks folder before you create final data<br>
</li><li><b>Documentation</b> - PDF file container with schematic and PCB printouts including 3D board views<br>
<ul><li><b>Schematic Prints</b> - schematic sheets printout<br>
</li><li><b>PCB View</b> - one page PCB composite view<br>
</li><li><b>Dimensions</b> - board dimensions drawing<br>
</li><li><b>Layout</b> - routing of each copper layer separately (visual routing check)<br>
</li><li><b>Layout test footprints</b> - top and bottom copper layers with only pads displayed for testing footprints/packages on paper printout before board manufacturing<br>
</li><li><b>Assembly</b> - assembly drawing from top and bottom side<br>
</li><li><b>PCB 3D Print Top</b> - PCB 3D view from top<br>
</li><li><b>PCB 3D Print Bottom</b> - PCB 3D view from bottom<br>
</li></ul><ul><li>Hint: 3D views can be set by positioning the 3D view directly in PCB file and using "Take current camera position" button in the output properties in Output.OutJob file<br>
</li></ul></li><li><b>Fabrication</b> - Gerber files (RS-274X, 2:5, imperial), drill files for CNC drill machine (2:5, imperial), assembly bill of material and pick and place data for assembly machine<br>
</li><li><b>BOM</b> - bill of material for component purchasing purposes<br>
</li><li><b>Export 3D</b> - export of PCB 3D model into 3D STEP file</li></ul>

<h1>What's next</h1>
Please check all settings and in case of doubt discuss the settings with your manufacturer specifically.<br>
<br>
If you find any state of document typical for your future designs you can set the document into DXP » Preferences » System » New Document Defaults (the source document must be stored in location where it stays untouched by any modifications).<br>
<br>
Other advanced techniques of setting up Altium Designer according to your needs can be found at Altium wiki page <a href='http://wiki.altium.com/display/ADOH/The+Altium+Designer+Environment'>Design Environment</a>.<br>
<br>
<h1>Terms of use</h1>
<b>The template is provided free of charge with no warranty or liability.</b>

Feel free to modify the template by your own needs. I would like to encourage you to share your modifications with others.<br>
<br>
Please let me know your feedback on this template. Any comments are welcome. You can contact me via email PetrTos (a) gmail.com .<br>
<br>
<br>
Petr Tosovsky<br>
2013-11-10