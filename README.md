<h1>Altium Designer addons</h1>
Set of addons for Altium Designer unified design environment for electronics development.
Moved from code.google.com/p/altium-designer-addons

This project contains set of scripts, examples and other content which is developed to provide extended features for Altium Designer  unified design environment for electronics development.<br>
The project is in Czech and English language. <br> 
Distributed "as is" with no warranty. In case of difficulties please contact leaders of this project or create a record in Issues part of this page.<br>
Genuine Altium Script Gallery (source used for API usage examples) can be found in <a href="http://techdocs.altium.com">Altium Techdocs</a> in <a href="http://techdocs.altium.com/display/SCRT/Script+Examples+Reference">Script Examples Reference</a> page.
<br><br>
*Contributors* (sorted chronologically): Petr Tosovsky, Petar Perisin, John Go-Soco, Mattias Ericson, Darren Moore, Colby Meyer, Juan Martinez, Rob Sterling, Jeff Collins, Ran Shahar, Tony Chilco, Ryan Rutledge, Erick Albach, Matija Markovic, Cyril Andreatta, Randy Clemmons, Miklós Zsikla, Justin Massiot
<br><br>
<hr>
Easy download of scripts can be done through Google drive link here 

=<a href="https://drive.google.com/folderview?id=0B9wYgjewDMDFYzBTU1pMR0xaSHM&usp=sharing#list">Download</a>=

<hr>
*If you are interested to be committer, please send an email to retry.var (a) gmail.com. It is needed to have GitHub account (gmail) to access repository with write permission.*<br>
You can read more about commiting your content here [AltiumAddonsCommitersRecomendation]
<hr>
<h2>Altium Designer LIBRARIES available at Addons page:</h2>
 * *Libs_NetTieLib* - set of components for joining and splitting signals on PCB. Details here [NetTieLib] 
 * *Libs_CAndreatta* - set of individual schematic symbols and footprints Cyril Andreatta. Plain SchLib and PcbLib files. Details here [Libs_CAndreatta] 
 * *Libs_RRutledge* - General Library by Ryan Rutledge. Plain SchLib and PcbLib files. Details here [Libs_RRutledge] 

*Thank you Ryan for this kick off in the Libraries section.* 

<h2>Altium Designer templates available at Addons page:</h2>
 * *AltiumPCBProjectTemplate* - Sample project template for 2 and 4 layer board design in Altium Designer. Details can be found here [AltiumPCBProjectTemplate] 

<h2>Scripts available at our Addons page:</h2>
<ul>
<li>*TortoiseSVN* script - script is using TortoiseSVN client to process SVN operations what brings standard TortoiseSVN dialogs into AD to have more features while working with SVN
<li>*BetterRevertVCS* script - script reverts active document from repository and reopens working copy to be sure user is working with reverted data
<li>*OpenComponentPCBLib* script - opens the component footprint library of a selected component on PCB
<li>*BoardAutoSizer* script - Redefine Board Shape based on Embedded Board Arrays
<li>*Fix Connections* script - Fix Connections is a modified version of Fix Overlaps v2.2 created by Petar Perisin, there are some features and UI improvements for better usability. 
<li>*Via Soldermask Barrel Relief* script - the script searches for vias on board with higher then defined hole size and opens soldermask for the vias by given value from the edge of hole to prevent possible mask damage. 
<li>*Calculator for converting units* [mil_mm_conv] script - script allows easily convert entered value to all other units interpretations
<li>*ShowHideDesignators* script - script allows easily show or hide Designators and Comments in PcbDoc file
<li>*RotationStep-Toggle-Modify* script - easier way to modify PCB editor rotation step settings (when Spacebar is used to rotate an object)
<li>*AutoHotkey Enhancement* script - script provides several enhancements to Altium Designer, such as limited autocomplete for filter expressions, improved mouse wheel support, and various tweaks to save time on frequently-used tasks
<li>*Distribute* script - script that distributes distance between selected lines.
<li>*Run_exe* script - script that can open exe file from Altium. Currently supports PCB Toolkit, TraceSim and TX-Line.
<li>*FilletWithRadius* script - script that rounds connection on horizontaland vertical lines by fixed radius value.
<li>*Arc8* script - script that connects arc tangent and via/Pad, or places tangent to connect two arcs.
<li>*FixOverlaps* script - script that will clean nets in chosen net class of overlaps.
<li>*OpenSchDocs* script - script will open all sch documents in current PCB project.
<li>*UpdateNetOnclick* script - script will update net of all selected objects to a net user chooses with a mouse click.
<li>*LengthTuningHelper* script - script that helps in length tuning of DDR3-FPGA interfaces, where you need to include length of lines inside FPGA. This one also helps in length tuning when you need to include via stack size to length of a net. 
<li>*LayerStackExporter* script - script that exports layer stack to a CSV file.
<li>*UpdateFootprintHeightFrom3dModelHeight* script - script that updates Footprint height based on height of it's 3D model (in PCB library).
<li>*SelectViaAntennas* script - This script selects unnecessary vias on PCB (vias connected only on one layer).
<li>*ReAnnotateSelection* script - This can be used to re-annotate selected components on PCB.
<li>*ZoomAndCenter* script - This script invokes zoom and centers view on zoomed area. similar how PCAD worked with zoom.
<li>*PlaceRectangle* script - This script places a rectangle on the PCB.
<li>*AutoRouter_Interface* script - This script creates an interface to auto routers. It fixes some problems with rte importer and dsn exporter, and gives user the ability to import ses file. 
<li>*CalculateCopperArea* script - This script calculates area of selected poly or region.
<li>*LayersAndObjects* script - This script creates a form similar to a panel, from which you can control layer and object display. Originated from LayersPanel script. 
<li>*SelectBadConnections* script - This script checks weather Tracks and arcs on signal connect totaly on some other object. Center-to-center check is done. If not, it is selected. Tolerance (and zero tolerance) is supported.
<li>*WheelSelector* script - This script allows selection and scrolling through insight form using wheel. 
<li>*AddDatumPointsToArcs* script - This script will add tracks to selected arcs. This tracks end in arc center, so after that you can easily move objects to arc center. 
<li>*AutoSTEPplacer* script - This script will place STEP files on a footprint in a library if they have same name. STEP file must be in the same directory as library, or it's subfolder. 
<li>*PCBScale* script - This script allows you to scale selected objects on a PCB by amount. 
<li>*LayersPanel* script - This script creates a form similar to a panel, from which you can control layer display. Originated from ShowHideLayers script. 
<li>*Man2APDesignators* script - This script will change manual designators to auto-positioned. Will operate on all or selected components. 
<li>*ShowHideLayers* script - This script allows very easy turning layers on/off.
<li>*SelectConnectedOnLayer* script - This script is similar to SelectConnectedTracks, only it works with arcs, it's faster and objects do not have to have common point.
<li>*ComponentPlacement* script - This script allows user to equalize relative component placement between two groups of components.
<li>*MoveAPdesignators* script - This script allows user to modify distance between auto-positioned designators and component.
<li>*ParamsToPCB* script - This script allows user to transfer component parameters to the PCB.
<li>*CreateTableOfContents* script - Script that can be used to create Table Of Contents on newly created top level sheet in PCB Project.
<li>*MultiPCBProject* script - This Script enables Project to be used with multiple PCB documents, so that project is split among multiple PCB Documents. It is workaround solution that uses blankets to point which PCB document gets the circuit under it. 
<li>*XIA_Update_From_Database* script -  This is the script in use by XIA LCC to update schematic designs with respect to the company components database.  This includes synchronization of user parameters (MFGNAME, Description, etc.), synchronization of footprints, and setting the Comment parameter.
<li>*XIA_Release_Manager* script - This is the script in use at XIA LLC to do all Altium output generation, packaging (zipping), and release-tag.  Everything is done within Subversion (SVN).
<li>*FilterObjects* script - Script that filters objects based on type, layer and parent.
<li>*LockNetRouting* script - Script which locks or unlocks routing tracks, vias and components (possible to exclude components) on selected net. That aims to prevent some unwanted changes on PCB during routing.
<li>*SingleLayerModeWithConnectionLines* script - This script enables user to see all connection lines in a display similar to single layer mode.
<li>*VendorTools* script - Script that can be used to exchange info between different FPGA vendors and AD. Currently supports Altera
<li>*ConnectionLinesOnSelection* script - script that enables user to show/hide connection lines on selected objects
<li>*TrimExtend* script - Script that trims or extends tracks up to a point where first selected track is. This function is available in all 3D tools
<li>*TestpointMaker* script - This script creates test points for a net class.
<li>*StitchingVias* script - Script that generates stitching vias on a PCB. Vias are added to graphical component for easier use.
<li>*ThievingPads* script - Script that adds thieving pads to a PCB Document. Pads are added in a dummy component, that allows easier manipulation
<li>*LR_Justify* script - Script that switches selected text between left and right justification on a sch.
<li>*AddWireStubsSch* script - it search for unconected pins of components in schematic sheet and draw small segment of a wire on it equipped by net label according to the name or designator of the pin
<li>*Adjust Designators 2 script* - This script modifies designator position. It is based on AdjustDesignator script from Mattias Ericson, only user gets the form in which he can enable/disable and modify certain options, like layers, height etc...
<li>*MechLayerNames script* - Script that saves mech layer names to txt file. This names can then be imported it to another PCB Document.
<li>*DeleteInvalidPCBObjects script* - Cleans a PCB documents of some current invalid objects. Checks for invalid regions or polygons and deletes them.
<li>*DrcOnOff script* - This script toggles Online DRC check box in DXP->Preferences->PCB editor->General what enables/disables online DRC checks. 
<li>*LockMultiPartComponents script* - This script can be used to lock parts in multi-part components. It searches for "Group" parameter in all components and assigns a value in it, based on current component designator.
<li>*PrintAllvariants script* - This Script saves variant information to CSV file, which can then be easily opened in excel.
<li>*SheetParameters script* - script that modifies document parameters in all SCH documents of focused project. 
<li>*FlipComponents script* - Script flips selected components. If there are no selected objects it asks user to click on components that will be flipped. It uses smart mechanism for flipping, which tries to keep pads position. 
<li>*MoveToLayer script* - This script moves selected copper tracks to selected signal layer, while maintaining connectivity with Vias that are automatically placed. 
<li>*IsPadCenterConnected script* - This script checks weather pads have track in their center. if not, pad is selected.
<li>*DesignReuse_v3.0 script* - Script that simplifies true design reuse - route circuit once and reuse same routing in your next design. This automated design reuse is using Device Sheets or SCH snippets in SCH, and PCB Snippets in PCB. For more info read "How to use this script.odt" document (in OpenOffice or M$ Word) form the zip file
<li>*SpiralTrackVer0[1].8 script* - Spiral Generator script made by Darren Moore.
<li>*PlanarTXv0.7 script* - Script to create planar transformer made by Darren Moore.
<li>*Fillet script* - Script that places arcs to corners of selected tracks (Fillet command).
<li>*DeleteSelectedSplit script* - Script that can be used to delete selected split plane. It actually creates region based on selected split plane, with holes inside, so no copper will appear on that place.
<li>*DeleteAllSelectedItemsInPCBLIB script* - This script can be used to delete selected objects in PCB Library. Currently you can only delete selected objects that are part of currently visible footprint, but this script deletes selected objects that are in other footprints.
<li>*FormatPaintBrush script* - This script is used to copy formattings from one object to the others. Currently it works on dimensions, coordinate, String and poly in PCB and all sch objects.
<li>*Current Calculator script* - PCB script that gives the user a dialog box with current (Amperes) handling calculations for a selected track. The script determines if the track is on an internal or external layer, and provides current calculations for 1, 5, and 10°C rise above ambient.
<li>*Hyperlynx Exporter script* - Script for PCB export to hyp file. It adds fills, regions, polygons and split planes in hyp file.
<li>*RenumberPads script* - Script helps with changing order of pads mainly in Altium PCBLIBs. You just start script, select start index and increment and you create new designators of pads by clicking on them in the new order.
<li>*RoomFromPoly script* - Script to create room from selected objects or from selected polygon.
<li>*AdjustDesignators script* - it will center the designator in top and bottom overlay.
<li>*ZoomComponent script* - PCB function similar to Altium's Jump Component, but it also zoom, mask and/or select component.
<li>*SCHSelectionFilter script* - Script that uses select Touching trectangle, but user can choose object types that will be selected.
<li>*SCH-SelectTouchingRectangle script* - Script made because some people wanted select touching rectangle feature in Sch.
<li>*IncrementingDesignators script* - Script that enables user to set designators with mouse. Works on Components (in SchDoc and PCBDoc), pins (SCHLIB) and pads (PCBLIB). Designators can be swapped too. When working with pins it can also move pin names.
<li>*IBIS Editor script* - Script that overrides [Model Selector] and [Submodel] keywords in IBIS File.
<li>*EagleToAD conversion package* - package of scripts for conversion of PCB project from Cadsoft Eagle to Altium Designer (manual available only in Czech)
<li>*CopyAngleToComponent script* - script for copying angle of track to a component
<li>*SelectConnectedTrack script* - script for selection of connected NoNet track on Mech layers
<li>*Custom Pick&Place report script* - script for generating user defined P&P for SMD components only 
