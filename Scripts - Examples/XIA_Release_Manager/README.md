## XIA_Release_Manager script package

The `XIA_Release_Manager` targets a specific organisation and its internal processes, that's why it might not be appropriate to run this script as is.\
However it could be a goldmine for any developer looking for real use case examples of advanced scripting. Credits go to Jeff Collins, the initial author of this script package.

**IMPORTANT NOTICE**: the code (more than 16k lines of code) is **old** and unmaintained. Many features may be broken with newer versions of Altium Designer.


### What's inside

1. This release manager script is intended to be an interface to a set of 6 OutJob files that were copied from company templates and somewhat customized for the project in question. The 6 OutJob files exist to break up the task of output generation into 6 pieces, so that all output files need not be generated each time. (This was XIA's previous setup, and wasted lots of time.)

2. This script will perform some of the same checks as the Altium builtin release manager: checking for source files that are saved and checked in.

3. This script does a variety of additional sanity checks on the design, including checking version numbers between schematic and layout, checking for certain design elements required by company design rules, checking for existing releases of the current version number, etc. It will also check that all output files that had been previously generated were, in fact, also generated this time around.

4. ~~In the future, this script will likely also check that the schematic is in sync with the company components database, so that all schematic parameters (supplier part numbers, RoHS status, soldering info, etc.) are updated prior to generating a BOM.~~

5. Roughly speaking, this script is setup such that the user is allowed to run OutJobs 1; 1,2; 1,2,3; 1,2,3,4; 1,2,3,4,5; or 1,2,3,4,5,6. In other words, running later OutJobs require the earlier ones. So you can stop at a certain point, but you are not allowed to just run the later ones. This limitation is by design, to make sure that the earlier OutJobs (which generate various outputs for design reviews, etc.) must be run prior to the later ones (which actually generate fab & assembly outputs).

6. This script will perform certain additional operations on certain output files, after Altium generates them.  
  6a. This script will sort a multi-wire netlist, so that a designer may use this sorted, human-readable, netlist to track connectivity changes as the design progresses.  
  6b. This script will (if necessary) fix Altium-generated IPC356 netlists in order to address an Altium bug that we've been hitting.  
  6c. This script will perform some additional handling in order to get an svn rev number inserted into Excel BOMs.  
  6d. This script will "mark" Excel BOMs when we're doing an assembly packaging operation. This currently takes the form of replacing a special string within the xls file with a new string of the exact same length. This operation is done with sed, outside of Excel.

7. This script will checkin all generated output files (including additional ones like sorted multiwire netlist and fixed IPC356 netlist) to svn, in trunk/.

8. If requested, this script will package (zip up) fabrication and/or assembly files and checkin those zipfiles to trunk/.  
  8a. If additional files (eg. additional documentation, etc.) in the fabrication and/or assembly include directories, these additional files will be added to the relevant zipfile(s).

9. If requested, this script will create new subdirs in releases/ and tags/, corresponding to the version and svn rev number of the current release(s). It will then copy the zipfile(s) to the new subdir(s) in releases/. Finally, it will make a project snapshot in the new subdir(s) in tags/.

![XIA_Release_Manager welcome screen](https://github.com/Altium-Designer-addons/scripts-libraries/raw/master/Scripts%20-%20Examples/XIA_Release_Manager/docs/v1.8.10_screen_capture1.jpg "XIA_Release_Manager welcome screen")

![XIA_Release_Manager successful](https://github.com/Altium-Designer-addons/scripts-libraries/raw/master/Scripts%20-%20Examples/XIA_Release_Manager/docs/v1.8.10_screen_capture2.jpg "XIA_Release_Manager successful")


### Covered topics (incomplete list of)

Feature/Topic | Script file
--- | ---
Walk through schematics and electrical connectivity | XIA_Generate_Sorted_Multiwire_Netlist.pas
Sync parameters with the components database (Update from database) | XIA_Update_From_Database.pas
Sync footprints with the components database (Update from database) | XIA_Update_From_Database.pas
Run output job files from a script | XIA_Release_Manager.pas
Execute SVN commands | XIA_Release_Manager.pas<br>XIA_Utils.pas
Many more things ... | XIA_Release_Manager.pas


### In case you want to try to execute the script

```
Notes:
1. Requires Altium 10.
2. Tested with Windows 7 x64.
3. Old versions of svn.exe will fail when trying to svn add files currently open with MS Excel. Svn version 1.6.3 is known to NOT work. Svn versions 1.6.11 and 1.6.16 are known to work.
4. When multiple people are working on a given project (and making releases), this script will not necessarily pull in all the svn updates to releases/ and tags/ for the project to a given user's working copy. Rather, it will only svn update empty directories, in order to try to minimize disk usage within a particular user's working copy. If you wish to have releases/ and tags/ all the way updated in your working copy, you will need to manually issue a command like: svn update --set-depth=infinity h:/projects/foo/releases h:/projects/foo/tags
5. This file should no longer have TAB characters in it.
```

```
External dependencies:
1. This script requires functions and constants defined in XIA_Utils.pas. This script also requires one function from XIA_Generate_Sorted_Multiwire_Netlist.pas. Both of these scripts must be included in the script project along with this file.
2. svn.exe that is in the DOS path. Tested with SlikSvn v1.6.16 x64. See http://www.sliksvn.com/en/download . **This script has NOT BEEN TESTED with svn v1.7!! Stick with v1.6 for now!!**
3. UnxUtils from http://sourceforge.net/projects/unxutils/ . It's not exactly clear what version of this I have, but I downloaded mine on 2011/08/30. Create "Altium_scripts"\dosutils\UnxUtils. Put UnxUtils.zip in that directory, then unzip it. Files end up in "Altium_scripts"\dosutils\UnxUtils\usr\local\wbin\sed.exe, etc.
```

```
 CAD SETUP REQUIREMENTS:
 *  1.  Subversion (svn) server that is accessible to all Altium users in your company.
 *  It must also be accessible to purchasing and operations personnel who interface
 *  with printed circuit board fabrication and/or assembly shops.
 *  The point is also for all users to be able to review other people's designs.
 *  In addition, an svn checkin to a common server will always result in a monotonically
 *  increasing svn rev number.  This svn rev number is used as a backup to script-
 *  maintained human readable version numbers to try to make absolutely sure that
 *  two different releases NEVER HAVE THE SAME (VERSION NUMBER + SVN REV NUMBER).
 *  This script makes no provision for moving to a new svn server and resetting
 *  svn rev numbers, etc.  It assumes that there is one svn server and monotonically
 *  increasing svn rev numbers.  Period.  Full stop.
 *
 *  2.  All users must have SlikSvn (or equivalent) installed on their PCs.  It must
 *  be setup such that command line svn commits will succeed without prompting the
 *  user for a password.  This is required because this script will perform a large
 *  number of svn commands that run non-interactively in DOS windows with all of
 *  the output text redirected to files and not shown on the screen.
 *
 *  3.  All users should have an SVN GUI frontend installed to enable easy tracking
 *  of changes, diffing of changes, prompting user to enter checkin comments, etc.
 *  Most users at my company use TortoiseSVN and like its integration with windows
 *  explorer.  I personally detest windows explorer and don't see that as a feature.
 *  I much prefer SmartSVN (which was an approximately USD$80 single license), plus
 *  Araxis Merge for graphical file diffing (approximately a USD$100 single license).
 *  
 *  4.  Ability to access certain CAD files via a global absolute path, which is
 *  identical for all Altium users at your company.  For XIA, the CAD files in
 *  question are schematic and footprint libraries, the company DBLib database link
 *  file, and the company Excel BOM template file.  I am not aware of any specific
 *  parts of this script that will fail if this global path is not setup properly.
 *  But I can say for certain that all my testing has been done with such a setup
 *  and I make no guarantees that it will work in the absence of such a setup.
 *  There are 2 obvious ways to do this on windows (though it should be possible
 *  to have some users use each method):
 *
 *  4a.  Company fileserver and all users agreeing to mount this as the same drive
 *  letter (eg. R:).  Fileserver is assumed to (a) be always up and available, and
 *  (b) always updated with the latest and greatest libraries, etc.  Fileserver
 *  should presumably only be writable by responsible persons (eg. database librarians).
 *
 *  4b.  Each user checks out a working copy of the company Altium libraries
 *  directory to a location of their choosing.  Then each user has a custom
 *  script to mount their working copy as an agreed-upon drive letter (eg. R:).
 *  My custom batch file sub.bat reads:
 *  subst r: "c:\XIA\XIA_components_database_ro"
 *  This mounts the directory c:/XIA/XIA_components_database_ro (which is my local
 *  working copy of the company Altium libraries, etc.) as my R: drive.
 *  I added a link to sub.bat to my windows startup folder, so that my R: drive mounts
 *  automagically on initial login to my PC.
 *  This setup is tolerant of the svn server being occasionally down or unavailable,
 *  since it is only needed when I want to "svn update c:/XIA/XIA_components_database_ro",
 *  diff or checkin changed source files, or run this release manager script.
 *  However, the catch is that I need to explicitly do that svn update operation
 *  to pull down updated libraries, etc. from the svn server to my local working copy.
 *
 *  5.  Scripts directory location.  XIA recently moved the Altium_scripts directory
 *  to be a subdirectory of R: (specifically, R:\ALTIUM_LIBRARIES\Altium_scripts).
 *  If you're setting up your CAD environment to be compatible with the requirements
 *  of this script, I highly recommend you have Altium_scripts live within your
 *  global path (eg. R:\something).  It makes much more sense this way anyway.
 *
 *  5a.  There is vestigial support for an "Altium_scripts" directory than can
 *  exist in any location on a given user's PC.  So there is existing code that
 *  was written to determine that location and use that information as needed.
 *  While I have no immediate plans to remove the code that supports migratory
 *  Altium_scripts, I am no longer testing such code.  Old description:
 *  
 *      Assumes that there is an "Altium_scripts" directory in which this
 *      script and several helper applicatons will live.
 *      My "Altium_scripts" directory is H:\projects\Altium_scripts.
 *      Do NOT put spaces in the directory name for your Altium_scripts,
 *      eg. do NOT do "D:\my stuff\Altium_scripts".
 *
 *  6.  Assumes that there is a script project named "XIA_Altium_scripts.PrjScr"
 *  that lives in "Altium_scripts".  This script must be part of that script project.
 *  
 *  7.  Assumes scripts in Altium will be run by having this script project open,
 *  as well as the PCB project in question open.  PCB project must be focused
 *  before running this script.
 *
 *  8.  There is currently an Altium bug where the paper size of pdf files generated
 *  by Altium is limited to the size of paper supported by the windows default
 *  printer at the time Altium started.
 *  
 *  This problem and my workaround are discussed in http://forum.live.altium.com/#posts/187015 .
 *  But essentially, if you want to have Altium generate outputs (eg. assembly
 *  drawings) that are larger than letter size, you MUST have a windows default printer
 *  that is capable of handling the desired paper size.
 *  
 *  My workaround is to identify a large format plotter made by the same company
 *  (eg. HP) as an existing color network letter size printer at your company.
 *  The large format plotter and your existing color network printer must both
 *  have a language in common (eg. PostScript or PCL).  Install the driver for
 *  the large format plotter (even though you don't have such a plotter).  Then
 *  point it to the IP address for your existing color network printer.  If it works,
 *  set this large format plotter as your windows default printer.  This should result in:
 *  (a) A print driver that is generally usable for printing letter size pages from
 *  Firefox, Excel, Word, etc.
 *  (b) A print driver that reports to Altium that it can handle ANSI A,B,C,D,E size
 *  paper.  Thus, Altium will allow you to generate pdf files in any of those sizes.
 *
 *  I personally have an HP DesignJet T2300ps PS3 (a color plotter with a 42" paper roll)
 *  printer driver set as my windows default printer.  This printer driver is configured
 *  to point to our HP Color Laserjet 3800 (postscript) printer.  So I can print A size
 *  pages to this color laserjet without changing my default windows printer.  And from
 *  Altium, I can generate pdf files in ANSI A,B,C,D,E sizes.
```

```
 PROJECT SETUP REQUIREMENTS:
 *  1.  A new project must be allocated a subdir on the svn server.
 *  The project hierarchy on the svn server MUST contain a trunk/ subdir.
 *  Examples:  projects/spiffy_widget/trunk/schem/spiffy_widget
 *  Note:  The schem/ subdir is not strictly required, but I strongly recommend creating
 *  schem/, schematic/, sch/, or similar to separate your PCB schematic/layout working
 *  area from documentation, FPGA firmware, microcontroller firmware, application code, etc.
 *  
 *  2.  I highly recommend always making a subdir of schem/, even when there is only
 *  one board planned.  In my experience, it is common to subsequently need an
 *  associated test board, extender board, etc.  In other words, the Altium project
 *  root of a new project
 *  should be in projects/spiffy_widget/trunk/schem/spiffy_widget,
 *  NOT in       projects/spiffy_widget/trunk/schem.
 *
 *  3.  The project working copy on a user's PC should be a FULL working copy of
 *  the project, including trunk/, releases/, and tags/.
 *  In this example, it should be h:/projects/spiffy_widget/trunk/schem/spiffy_widget,
 *  NOT                           h:/projects/spiffy_widget_abbrev/schem/spiffy_widget, 
```

```
 DESIGN AND USAGE REQUIREMENTS: 
 *  1.  This script assumes that the windows default printer when Altium started
 *  is capable of generating the size assembly drawing desired (eg. ANSI D).
 *  This script makes no attempt to check which printer is the windows default,
 *  nor the exact capabilities of any windows printer drivers.  
 *
 *  2.  You must have exactly 1 PcbDoc file in your project.  However, this script will
 *  allow you to proceed with 0 PcbDoc files if you are not requesting to do any operation
 *  that requires a PcbDoc file.
 *  
 *  3.  There must be exactly 6 OutJobs in the project, whose names are enumerated
 *  in the global constants section below.  XIA has templates for all 6 of these
 *  OutJob files (to setup what files each one generates) and wiki instructions
 *  on how to customize each one to fit the exact needs of a particular project.
 *  These details are not explained in this script, but are assumed.
 *  
 *  4.  There must be 2 project level properties that specify the target (eg. next)
 *  release version for fabrication and assembly releases.  The names of these
 *  properties are enumerated in the global constants section below.  The last
 *  part of the fab and assembly version numbers must end in ".[integer]",
 *  eg. "ASY1.0.0".  Examples that won't work:  "ASY1-0", "ASY1", "ASY1.A".
 *
 *  5.  There must be an APCB1 component and a gerber title block component in
 *  the top level schematic file.  See section below for more details.
 *
 *  6.  You must have updated your schematic design with respect to database prior
 *  to running this script.
 *
 *  7.  That the schematic design and layout (PcbDoc file) are synchronized.  This
 *  script will fail in strange ways if refdes'es differ between schematic and layout!
 *
 *  8.  If you intend to package (zip) outputs, the various PCB and PCBA version
 *  numbers in schematic, project properties, and PcbDoc file MUST ALL MATCH!
 *
 *  9.  If you intend to do rel-and-tag, you must be running from a "full" working
 *  copy, containing trunk/, releases/, and tags/
 *  (eg. h:/projects/nifty_widget/trunk/schem/nifty_widget_main,
 *  NOT  h:/projects/nifty_widget_abbrev/schem/nifty_widget_main).
 *
 *  10.  That all source files (including the project file) are saved and checked into
 *  svn prior to running this release manager script.
```

```
 XIA-ism's: (Assumptions / constraints / weirdness / etc. that may be very specific to my company)
 *  1.  XIA takes the somewhat unusual step of trying to get the blank PCB fab
 *  (eg. unstuffed PCB) to appear in the BOM for the project in question.
 *  The idea behind this is to have BOMs that call out all the parts and instructions
 *  needed to build the project in question, including blank PCB fab, all parts
 *  to be soldered onto the board during assembly, all 3rd op parts to be manually
 *  installed after assembly (eg. screws, standoffs, jumper blocks, etc.), and
 *  all assembly instructions (eg. Trim excess pin length for J5, Install standoff
 *  from top side and screw from bottom side at MH7, etc.).  Our intention is to
 *  have the cost for the blank PCB fab populated and added to the total BOM
 *  cost for the project.  The idea of including the assembly instructions in the
 *  BOM is to provide more complete project documentation when sending out BOMs and
 *  requesting PCB assembly quotes.
 *  
 *  1a.  The refdes for this "blank PCB fab" component shall be APCB1.
 *  (The "A" part dates from a time when XIA BOMs were sorted by refdes, in
 *  order to try to get this item to appear at the top of the BOM.)
 *  
 *  1b.  In layout, the APCB1 footprint consists of a line of silkscreen text which reads:
 *  "PCB PART #: .Comment         Rev .VersionControl_RevNumber",
 *  where .Comment is the special string that becomes the comment that comes through from schematic,
 *  and .VersionControl_RevNumber is the special string that gets replaced by
 *  a numeric subversion (SVN) revision number upon checkin of the PcbDoc file.
 *  So upon final checkin of the PcbDoc file, this text will be something like
 *  "PCB PART #: PCB-MICROCAL_MAIN-1.13 Rev 12345".
 *  It will be such when examining the silkscreen gerber file and the actual PCB back from fab.
 *  This APCB1 component footprint as described must be placed within the
 *  board outline.
 *  
 *  1c.  In schematic, the APCB1 component is a small rectangle with fixed
 *  text inside that says "PCB Fab Part #".  The refdes and comment fields are
 *  displayed.
 *  More specifically, this is a database part whose :
 *  VALUE field will be set to the PCB-part-number-and-version (eg. "PCB-MICROCAL_MAIN-1.13");
 *  MFGNUMBER field will be set to the PCB-part-number-and-version + svn rev number
 *  (which isn't known until PcbDoc file checkin), eg. "PCB-MICROCAL_MAIN-1.13 Rev 12345";
 *  Comment field set to "=VALUE".
 *  The Comment/VALUE field propagates from schematic to layout.
 *  The MFGNUMBER field does not do so.
 *  Warning:  The PCB-part-number-and-version is REQUIRED to begin with "PCB-".
 *  
 *  1d.  In schematic, there must be a gerber title block (GTB) component,
 *  with refdes XX something (eg. XX5).
 *  This is just a small rectangle with fixed text inside that says "Layout component".
 *  The refdes, Comment, and VALUE fields are displayed.
 *  More specifically, this is a database part whose :
 *  VALUE field will describe which GTB component this is (eg. the one with 3 internal
 *  routing layers and 3 internal plane layers);
 *  Comment field set to the PCB-part-number-and-version (eg. "PCB-MICROCAL_MAIN-1.13").
 *  The Comment field propagates from schematic to layout.
 *  The VALUE field does not do so.
 *  NOTE:  Because the GTB component is not restricted to a particular refdes (eg. XX1),
 *  this script looks for a very specific, unique to XIA, property to be present in this
 *  component!  You may need to visit this issue for your company.
 *
 *  1e.  In layout, the GTB component consists of a title block with boxes and
 *  text on the global layer.  Included in this text is the .Comment special
 *  string, so that the PCB-part-number-and-version (eg. "PCB-MICROCAL_MAIN-1.13") appears
 *  on the global layer.
 *  The GTB footprint also labels all board layers for ease of examining gerbers.
 *  This GTB component footprint must be placed outside the board outline,
 *  so that it appears on all gerbers, but doesn't actually appear on the board itself.
 *
 *  1f.  In schematic, the APCB1 component and GTB component must both live
 *  within the top level schematic file!
 *
 *  2.  How to use APCB1 & GTB components.
 *
 *  2a.  New design.  Simultaneous release of fab and assembly.
 *  -Manually add new project properties:
 *    XIA_fabrication_pcb_part_number_and_version_target to be "PCB-SPIFFY_WIDGET-1.0.0" (no quotes).
 *    XIA_assembly_pcba_version_target to be "ASY1.0.0" (no quotes).
 *  -Have database librarian create new APCB1 component placeholder.
 *    VALUE field will be set to "PCB-SPIFFY_WIDGET-1.0.0";
 *    MFGNUMBER field will be set to "PCB-SPIFFY_WIDGET-1.0.0 Rev ?????".
 *  -Place new APCB1 component and appropriate GTB components in top level sch file.
 *  -Do schematic design and layout.
 *  -Release purchasing BOM(s) as needed.  Placeholder APCB1 comp cannot be purchased.
 *  -Finish schematic design and layout.
 *  -Upon checkin of final changes to PcbDoc file, note svn rev # of checkin (eg. 12345).
 *  -Have database librarian update APCB1 component placeholder.
 *    MFGNUMBER field will now be set to "PCB-SPIFFY_WIDGET-1.0.0 Rev 12345".
 *  -Push updates from database to schematic.
 *  -Run this release manager script to package and rel-and-tag fab and assembly.
 *  -Issue purchase orders for PCB fabrication and PCB assembly.
 *  
 *  2b.  New design.  Separate releases for fab and assembly.
 *  -Manually add new project properties:
 *    XIA_fabrication_pcb_part_number_and_version_target to be "PCB-SPIFFY_WIDGET-1.0.0" (no quotes).
 *    XIA_assembly_pcba_version_target to be "ASY1.0.0" (no quotes).
 *  -Have database librarian create new APCB1 component placeholder.
 *    VALUE field will be set to "PCB-SPIFFY_WIDGET-1.0.0";
 *    MFGNUMBER field will be set to "PCB-SPIFFY_WIDGET-1.0.0 Rev ?????".
 *  -Place new APCB1 component and appropriate GTB components in top level sch file.
 *  -Do schematic design and layout.
 *  -Release purchasing BOM(s) as needed.  Placeholder APCB1 comp cannot be purchased.
 *  -Finish schematic design and layout.
 *  -Upon checkin of final changes to PcbDoc file, note svn rev # of checkin (eg. 12345).
 *  -Run this release manager script to package and rel-and-tag fab only.
 *  -Issue purchase order for PCB fabrication.
 *  -Have database librarian update APCB1 component placeholder.
 *    MFGNUMBER field will now be set to "PCB-SPIFFY_WIDGET-1.0.0 Rev 12345".
 *  -Push updates from database to schematic.
 *  -Run this release manager script to package and rel-and-tag assembly only.
 *  -Issue purchase order for PCB assembly.
 *  
 *  2c.  Existing design.  Changing some resistor values and replacing obsolete parts.  No new fab.
 *  -Assume previous fab version was "PCB-SPIFFY_WIDGET-1.0.0".
 *  -Assume previous pcba version was "ASY1.0.0".
 *  -Examine project properties:
 *    XIA_fabrication_pcb_part_number_and_version_target is already "PCB-SPIFFY_WIDGET-1.0.0" (no quotes).
 *    XIA_assembly_pcba_version_target is already "ASY1.0.1" (no quotes).
 *  -No need to change project properties.
 *  -No need to have database libarian update APCB1 component.
 *  -Revise schematic design and layout.
 *  -Release purchasing BOM(s) as needed.  Existing, valid APCB1 comp will be purchased!
 *  -Finish schematic design and layout.
 *  -Run this release manager script to package and rel-and-tag assembly only.
 *  -Issue purchase orders for PCB fabrication and PCB assembly.
 *  
 *  2d.  Existing design.  Creating new minor version of fab to add small new feature and roll up blue wires.  Simultaneous release of fab and assembly.
 *  -Manually update project properties:
 *    XIA_fabrication_pcb_part_number_and_version_target to be "PCB-SPIFFY_WIDGET-1.1.0" (no quotes).
 *    XIA_assembly_pcba_version_target to be "ASY1.1.0" (no quotes).
 *  -Have database librarian create new APCB1 component placeholder.
 *    VALUE field will be set to "PCB-SPIFFY_WIDGET-1.1.0";
 *    MFGNUMBER field will be set to "PCB-SPIFFY_WIDGET-1.1.0 Rev ?????".
 *  -Update APCB1 component in top level sch file to point to new placeholder APCB1 comp.
 *  -Do schematic design updates and layout updates.
 *  -Release purchasing BOM(s) as needed.  Placeholder APCB1 comp cannot be purchased.
 *  -Finish schematic design and layout.
 *  -Upon checkin of final changes to PcbDoc file, note svn rev # of checkin (eg. 13333).
 *  -Have database librarian update APCB1 component placeholder.
 *    MFGNUMBER field will now be set to "PCB-SPIFFY_WIDGET-1.1.0 Rev 13333".
 *  -Push updates from database to schematic.
 *  -Run this release manager script to package and rel-and-tag fab and assembly.
 *  -Issue purchase orders for PCB fabrication and PCB assembly.
 *
 *  3.  PCBA part numbers.  For printed circuit board assembly (stuffed PCB) part
 *  numbers, this script will currently append an "A" to the PCB part number.
 *  So PCB part number "PCB-SPIFFY_WIDGET" becomes PCBA part number "PCBA-SPIFFY_WIDGET".
 *
 *  4.  Unified BOM.  We have previously suffered with a painfully slow database
 *  interface, which resulted in very slow generation of BOMs.  Partly as a result
 *  of this, we still generate only a single unified BOM, to be used for
 *  review, purchasing, and assembly.  We currently assume that users will delete
 *  or hide columns in the Excel BOM file that are not relevant for their purposes.
 *  This approach may be revisited at some point, in order to have different BOM
 *  templates for purchasing BOMs vs. assembly BOMs, etc.  And then we'd generate
 *  another BOM during Generate Assembly outputs.
 *
 *  5.  Excel BOMs tagged with svn rev #.  There are several reasons for this:
 *  (a) To serve as a backup for this script failing to advance the human readable
 *  PCBA version number.  We want to make absolutely sure that no two distinct
 *  board releases get out the door with the same (version + rev) number!
 *  (b) To be the only versioning number present in BOMs that are not released
 *  assembly BOMs (eg. review BOMs, purchasing BOMs, etc.).
 *  (c) To be consistent with XIA release procedures that pre-date the initial
 *  creation of this release manager script.
 *
 *  Thus, XIA marks all released assembly BOMs not only with a human readable
 *  PCBA version number, but also with the svn rev number.
 *  This is done with the use of svn:keywords properties and
 *  "$""Rev::               $:" text in the XIA BOM template.
 *  The way this works is that this script will create the Excel BOM, turn on
 *  the svn:keywords properties, and check in all generated files (including said BOM).
 *  During checkin, svn.exe will replace "$""Rev::               $:" with the actual
 *  svn rev number of that checkin, eg   "$""Rev::               $:".
 *  Following that initial checkin, this script will turn off the svn:keywords
 *  property and re-checkin all BOM files.  That way, the svn rev number becomes
 *  static and keyed only to that initial checkin.  That way, it will not change
 *  if you svn move the Excel BOM to a new location, etc.
 *  Note that we MUST use the fixed width form of $""Rev:       $: as shown above, so that
 *  svn.exe does not change the length of this string and corrupt the Excel file!
 *  Note that the svn rev number of the Excel BOM is NOT KNOWN prior to that
 *  initial svn checkin of all generated files.
 *
 *  6.  Sorted multi-wire netlist.  XIA will now directly generate a sorted version
 *  of a Multiwire netlist as part of our design review process.  We use this as a
 *  human readable netlist to allow us to track and audit changes to a design's netlist
 *  as the design progresses.
 *
 *  Previously we relied on the Altium builtin Multiwire netlist generator and then
 *  explicitly sorted that output.
 *
 *  There were 2 drawbacks to the previous approach:
 *  a.  A designer wishing to simply manually run the OutJob in question would only
 *  get the unsorted Multiwire netlist.
 *  b.  The Altium builtin Multiwire netlist generator truncates all netnames at
 *  ~12 characters.  And it does so in such a way that the truncated netnames are
 *  not necessarily unique.
 *
 *  The new approach solves the 2 drawbacks from the previous approach.
 *
 *  There is 1 drawback to the new approach:
 *  c.  Altium is limited to executing only one script at a time.  The sorted
 *  Multiwire netlist generator is implemented as a separate script called
 *  XIA_Generate_Sorted_Multiwire_Netlist that can be called from the XRM1 OutJob
 *  file.  So when we're running this release manager script, we cannot call the
 *  XRM1 OutJob, and then have that call the separate script.  So what we're doing
 *  is to have the XRM1 OutJob have a separate generator section just for that
 *  XIA_Generate_Sorted_Multiwire_Netlist script.  This separate generator section
 *  will not be batch run by this script, so it won't generate the "Sorry, only
 *  one script at a time" error message.  Rather, we will magically detect that
 *  we're running the XRM1 OutJob and we will just call the specific function
 *  from XIA_Generate_Sorted_Multiwire_Netlist to generate and sort this netlist.
 *  
 *  7.  Fixing IPC356 netlist.  Currently XIA is hitting an Altium bug where
 *  Altium generates an incorrect IPC356 netlist for certain oversize component
 *  pads (ESD discharge strips for PXI boards).  So this script contains
 *  code to check for this error in the Altium generated IPC356 netlist.
 *  If it is present, a fixed IPC356 netlist will be generated.
 *  This check for IPC356 netlist problems should be harmless.
 *  However, the check for IPC356 netlist problems can be disabled in PopulateStringLists().
 *
 *  8.  Fixing BOM filtering.  Currently XIA is hitting an Altium bug where
 *  the standard filtering that we setup for Excel BOMs breaks frequently.
 *  This filtering is used to suppress certain design items (eg. test points,
 *  PCB fab notes, etc.) from the Excel BOM.  The problem is that Altium
 *  stores this filtering in the project file by the index of the parameter
 *  that we want to examine.  If the project adds or deletes schematic parameters,
 *  then this filtering often breaks and must be manually fixed.  Currently,
 *  there is no way for this script to detect whether the BOM filtering is
 *  "happy" or not.  So we pop up a dialog box and ask the user to tell us.
```

```
 THIS SCRIPT WILL SILENTLY DO THESE OPERATIONS WITHOUT ASKING THE USER:
 *  1.  Add all ECO log files in ProjectLogs/ directory to svn and check them in.
```

```
 NOTES RE/ SCRIPT PROBLEMS:
 *  1.  This script will always generate both _Debug.txt and _Summary.txt files.
 *  The _Summary.txt file contains only what's shown in the last dialog box on
 *  screen.  However, the _Debug.txt file contains lots of debugging information.
 *  If this script ever aborts due to some unexpected and/or unexplained-on-screen
 *  error, be sure to check the _Debug.txt file and try to figure out what
 *  happened.  If you had a previous version of the _Debug.txt file open, be
 *  sure to close the file and re-open it.  Some text editors will not detect
 *  that this file has changed underneath it.
 *  
 *  2.  This script is designed to clean up after itself.  It goes to some effort
 *  to recover from prior failed script runs every time it starts.
 *  
 *  3.  If you are only generating (but not packaging or rel-and-tagging) files,
 *  and you have a script abort, then solve the underlying problem (eg. svn cleanup)
 *  and re-run this release manager script.  There should be nothing else you have
 *  to do.
 *  
 *  4.  The worst case for this script aborting is for it to happen after packaging
 *  zipfiles, bumping PCBA version number in project properties, checking those in,
 *  and then aborting before or during rel-and-tag.  In this case, you may need
 *  to svn delete recent fab and/or assembly zipfiles from ProjectOutputs/XRM5
 *  and/or ProjectOutputs/XRM6.  You may also need to manually roll back the PCBA
 *  version number in the project properties.  Use svn to examine recent changes
 *  to the project file.
```