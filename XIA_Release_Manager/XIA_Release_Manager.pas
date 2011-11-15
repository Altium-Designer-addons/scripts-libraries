{***************************************************************************
 XIA_Release_Manager.pas
    Altium DelphiScript (basically Pascal) that will generate all OutJob
 outputs, do some fixes and sanity checks, check them in, and then
 optionally package them up, and finally optionally do release-and-tag.
 ***************************************************************************}

{***************************************************************************
 * Copyright (c) 2009-2011 XIA LLC.
 *  (Some code stolen from Altium examples and forum posts)
 *  Author:        Jeff Collins, jcollins@xia.com
 *  Author:        $Author$
 *  Check-in Date: $Date$ 
 *  Version #:     $Revision$
 *  
 * Redistribution and use in source and binary forms, 
 * with or without modification, are permitted provided 
 * that the following conditions are met:
 *
 *   * Redistributions of source code must retain the above 
 *     copyright notice, this list of conditions and the 
 *     following disclaimer.
 *   * Redistributions in binary form must reproduce the 
 *     above copyright notice, this list of conditions and the 
 *     following disclaimer in the documentation and/or other 
 *     materials provided with the distribution.
 *   * Neither the name of XIA LLC nor the names of its
 *     contributors may be used to endorse or promote
 *     products derived from this software without 
 *     specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF 
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE 
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR 
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF 
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
 * SUCH DAMAGE.
 ***************************************************************************}

{***************************************************************************
 * External dependencies:
 *  1.  svn.exe that is in the DOS path.  Tested with SlikSvn v1.6.16 x64.
 *      See http://www.sliksvn.com/en/download .
 *      **This script has NOT BEEN TESTED with svn v1.7!!  Stick with v1.6 for now!!**
 *  2.  UnxUtils from http://sourceforge.net/projects/unxutils/ .
 *      It's not exactly clear what version of this I have, but I downloaded
 *      mine on 2011/08/30.
 *      Create "Altium_scripts"\dosutils\UnxUtils.
 *      Put UnxUtils.zip in that directory, then unzip it.
 *      Files end up in "Altium_scripts"\dosutils\UnxUtils\usr\local\wbin\sed.exe, etc.
 *  
 * Notes:
 *  1.  Requires Altium 10.
 *  2.  Tested with Windows 7 x64.
 *  3.  Old versions of svn.exe will fail when trying to svn add files currently
 *      open with MS Excel.  Svn version 1.6.3 is known to NOT work.
 *      Svn versions 1.6.11 and 1.6.16 are known to work.
 *  4.  When multiple people are working on a given project (and making releases),
 *      this script will not necessarily pull in all the svn updates to
 *      releases/ and tags/ for the project to a given user's working copy.
 *      Rather, it will only svn update empty directories, in order to try to
 *      minimize disk usage within a particular user's working copy.
 *      If you wish to have releases/ and tags/ all the way updated in your working
 *      copy, you will need to manually issue a command like:
 *      svn update --set-depth=infinity h:/projects/foo/releases h:/projects/foo/tags
 *  5.  This file should no longer have TAB characters in it.
 *
 * TODO:
 *  1.  Implement non-release mode, where we do not require svn checkins,
 *  and we do not do svn checkins here in this script.
 *  2.  Handle the case where user wants to RelAndTag already-generated zipfiles (??).
 *
 * WHAT THIS SCRIPT WILL DO:
 *  1.  This release manager script is intended to be an interface to a set of 6
 *  OutJob files that were copied from company templates and somewhat customized
 *  for the project in question.  The 6 OutJob files exist to break up the task
 *  of output generation into 6 pieces, so that all output files need not be
 *  generated each time.  (This was XIA's previous setup, and wasted lots of time.)
 *
 *  2.  This script will perform some of the same checks as the Altium builtin release
 *  manager:  checking for source files that are saved and checked in.
 *
 *  3.  This script does a variety of additional sanity checks on the design,
 *  including checking version numbers between schematic and layout, checking for
 *  certain design elements required by company design rules, checking for
 *  existing releases of the current version number, etc.  It will also check
 *  that all output files that had been previously generated were, in fact,
 *  also generated this time around.
 *
 *  4.  In the future, this script will likely also check that the schematic is
 *  in sync with the company components database, so that all schematic parameters
 *  (supplier part numbers, RoHS status, soldering info, etc.) are updated prior
 *  to generating a BOM.
 *  
 *  5.  Roughly speaking, this script is setup such that the user is allowed to
 *  run OutJobs 1; 1,2; 1,2,3; 1,2,3,4; 1,2,3,4,5; or 1,2,3,4,5,6.
 *  In other words, running later OutJobs require the earlier ones.
 *  So you can stop at a certain point, but you are not allowed to just run the
 *  later ones.  This limitation is by design, to make sure that the earlier
 *  OutJobs (which generate various outputs for design reviews, etc.) must be
 *  run prior to the later ones (which actually generate fab & assembly outputs).
 *
 *  6.  This script will perform certain additional operations on certain output
 *  files, after Altium generates them.
 *  6a.  This script will sort a multi-wire netlist, so that a designer may use
 *  this sorted, human-readable, netlist to track connectivity changes as the
 *  design progresses.
 *  6b.  This script will (if necessary) fix Altium-generated IPC356 netlists
 *  in order to address an Altium bug that we've been hitting.
 *  6c.  This script will perform some additional handling in order to get an
 *  svn rev number inserted into Excel BOMs.
 *  6d.  This script will "mark" Excel BOMs when we're doing an assembly packaging
 *  operation.  This currently takes the form of replacing a special string within
 *  the xls file with a new string of the exact same length.  This operation is
 *  done with sed, outside of Excel.
 *
 *  7.  This script will checkin all generated output files (including additional
 *  ones like sorted multiwire netlist and fixed IPC356 netlist) to svn, in trunk/.
 *
 *  8.  If requested, this script will package (zip up) fabrication and/or assembly
 *  files and checkin those zipfiles to trunk/.
 *  8a.  If additional files (eg. additional documentation, etc.) in the fabrication
 *  and/or assembly include directories, these additional files will be added to
 *  the relevant zipfile(s).
 *
 *  9.  If requested, this script will create new subdirs in releases/ and tags/,
 *  corresponding to the version and svn rev number of the current release(s).
 *  It will then copy the zipfile(s) to the new subdir(s) in releases/.
 *  Finally, it will make a project snapshot in the new subdir(s) in tags/.
 *
 * WHAT THIS SCRIPT WILL *NOT* DO:
 *  1.  Generate and then gather up all the generated output files and then throw
 *  them over the wall into some other, unrelated document management system,
 *  as the Altium 10 builtin release manager does with their strange "Vault" concept.
 *  We have a company svn server.  Everyone here has access to it.  Everyone knows
 *  how to use it.  Everyone has already been using it for Altium projects.
 *  Why on earth would we want to throw releases over the wall into some unrelated
 *  document management system??  We want to use releases/ and tags/ in svn!!
 *
 *  2.  Generate release output in some other sandbox directory, other than the
 *  project's usual ProjectOutputs/ directory.
 *  
 * CAD SETUP REQUIREMENTS:
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
 *  
 * PROJECT SETUP REQUIREMENTS:
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
 *  
 * DESIGN AND USAGE REQUIREMENTS: 
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
 *
 * XIA-ism's: (Assumptions / constraints / weirdness / etc. that may be very specific to my company)
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
 *  6.  Sorted multi-wire netlist.  XIA generates a multi-wire netlist as part
 *  of our design review process.  We then sort this multi-wire netlist so that
 *  we can use svn to audit connectivity changes as the design progresses, make
 *  sure we don't inadvertently change connectivity when cleaning up a schematic
 *  page, etc.
 *  Note that a sorted, human readable netlist (of whatever format) could be
 *  directly produced by this script.  But we've been using a generated multi-wire
 *  netlist with subsequent sorting for a while now, and we have lots of designs
 *  with existing sorted multi-wire netlists checked into svn.  And so far I
 *  haven't sat down and written script code to directly generate a sorted
 *  multi-wire netlist that's compatible with all of our existing netlists.
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
 *
 * THIS SCRIPT WILL SILENTLY DO THESE OPERATIONS WITHOUT ASKING THE USER:
 *  1.  Add all ECO log files in ProjectLogs/ directory to svn and check them in.
 *
 * NOTES RE/ SCRIPT PROBLEMS:
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
 *  
 ***************************************************************************}


uses
SysUtils;

{***************************************************************************
 * Forward declarations for form objects.
 ***************************************************************************}
Interface

type
   TGenerateAndPackageOutputsForm = class(TForm)
   formText01 : TLabel;           
   formText02 : TLabel;           
   formText03 : TLabel;           
   formText04 : TLabel;           
   formText05 : TLabel;           
   formText06 : TLabel;           
   formText07 : TLabel;           
   formText08 : TLabel;           
   formText09 : TLabel;           
   formText10 : TLabel;           
   formText11 : TLabel;           
   formText12 : TLabel;           
   formText13 : TLabel;           
   formText14 : TLabel;           
   formText15 : TLabel;           
   formText16 : TLabel;           
   formText17 : TLabel;           
   formText18 : TLabel;           
   formText19 : TLabel;           
   formText20 : TLabel;           
   formText21 : TLabel;           
   formText22 : TLabel;           
   formText23 : TLabel;           
   formText24 : TLabel;           
   formText25 : TLabel;           
   formText26 : TLabel;           
   formText27 : TLabel;           
   formText28 : TLabel;           
   formText29 : TLabel;           
   formText30 : TLabel;           
   formText31 : TLabel;           
   formText32 : TLabel;           
   formText33 : TLabel;           
   formText34 : TLabel;           

   formCbFlagOp0_1 : TCheckBox;

   formCbFlagOp1_1 : TCheckBox;
   formCbFlagOp1_2 : TCheckBox;
   formCbFlagOp1_3 : TCheckBox;
   formCbFlagOp1_4 : TCheckBox;
   formCbFlagOp1_5 : TCheckBox;
   formCbFlagOp1_6 : TCheckBox;

   formCbFlagOp2_1 : TCheckBox;
   formCbFlagOp2_2 : TCheckBox;
   formCbFlagOp2_3 : TCheckBox;
   formCbFlagOp2_4 : TCheckBox;
   formCbFlagOp2_5 : TCheckBox;
   formCbFlagOp2_6 : TCheckBox;

   formCbFlagOp3_1 : TCheckBox;
   formCbFlagOp3_2 : TCheckBox;
   formCbFlagOp3_3 : TCheckBox;
   formCbFlagOp3_4 : TCheckBox;
   formCbFlagOp3_5 : TCheckBox;
   formCbFlagOp3_6 : TCheckBox;

   formButtonsLabel1 :TLabel;         
   formButtonOk: TButton;                 
   formButtonCancel: TButton;             
   formStatusBar1: TXStatusBar;   
   procedure TGenerateAndPackageOutputsForm.clickedOk(Sender : TPanel);
   procedure TGenerateAndPackageOutputsForm.clickedCancel(Sender : TPanel);
   procedure TGenerateAndPackageOutputsForm.bCheck0_1(Sender : TPanel);
   procedure TGenerateAndPackageOutputsForm.bCheck1_1(Sender : TPanel);
   procedure TGenerateAndPackageOutputsForm.bCheck1_2(Sender : TPanel);
   procedure TGenerateAndPackageOutputsForm.bCheck1_3(Sender : TPanel);
   procedure TGenerateAndPackageOutputsForm.bCheck1_4(Sender : TPanel);
   procedure TGenerateAndPackageOutputsForm.bCheck1_5(Sender : TPanel);
   procedure TGenerateAndPackageOutputsForm.bCheck1_6(Sender : TPanel);
   procedure TGenerateAndPackageOutputsForm.bCheck2_4(Sender : TPanel);
   procedure TGenerateAndPackageOutputsForm.bCheck2_5(Sender : TPanel);
   procedure TGenerateAndPackageOutputsForm.bCheck2_6(Sender : TPanel);
   procedure TGenerateAndPackageOutputsForm.bCheck3_4(Sender : TPanel);
   procedure TGenerateAndPackageOutputsForm.bCheck3_5(Sender : TPanel);
   procedure TGenerateAndPackageOutputsForm.bCheck3_6(Sender : TPanel);
//   procedure TGenerateAndPackageOutputsForm.AtExit(foo    : Integer);
end;                                                

   
{***************************************************************************
 * Function forward declarations.
 ***************************************************************************}
procedure WriteToDebugFile(msg : TDynamicString); forward;
procedure MyAbort(msg : TDynamicString); forward;
procedure GuiEnablePackageFabricationPreconditions(foo : Integer); forward;
procedure GuiDisablePackageFabricationPreconditions(foo : Integer); forward;
procedure GuiEnablePackageAssemblyPreconditions(foo : Integer); forward;
procedure GuiDisablePackageAssemblyPreconditions(foo : Integer); forward;
function GetFabIndex(    projOutSubDirs     : TStringList;
                         )                  : Integer; forward;
function GetAssyIndex(    projOutSubDirs    : TStringList;
                          )                 : Integer; forward;
function CreateZipFileName(    projectName  : TDynamicString;
                               pcbPartNum   : TDynamicString;
                               pcbVersion   : TDynamicString;
                               pcbDocRevNum : TDynamicString;
                               pcbaPartNum  : TDynamicString;
                               pcbaVersion  : TDynamicString;
                               bomRevNum    : TDynamicString;
                           var zipFileName  : TDynamicString;
                               )            : Integer; forward;


{***************************************************************************
 * Global constants.
 ***************************************************************************}
const
{* Declare the version and name of this script. *}
   constScriptVersion            = 'v1.8.13_gc $Revision$';
   constThisScriptNameNoExt      = 'XIA_Release_Manager';
   constThisScriptName           = constThisScriptNameNoExt + '.pas';

{* Declare acronyms for the various scripts that could be running and using some of our code. *}
   constWhichScriptXrm           = 'XRM';     { XIA_Release_Manager.pas }
   constWhichScriptUfd           = 'UFD';     { XIA_Update_From_Database.pas }

{* Declare the name of the script project of which this script must be part. }
   constScriptProjectName        = 'XIA_Altium_scripts';

{* Declare the global working copy (with scripts and libraries) that must be up-to-date before we allow this script to actually run. *}
   constGlobalScriptsAndLibsWc   = 'R:\ALTIUM_LIBRARIES\'; { Leave the trailing "\" char there! }
   
{* Declare file and path names to the required DBLib file and the approved DBLink file. *}
   { Note:  These are only needed by the XIA_Update_From_Database script. }
   { Note:  If there is no old DBLib file to upgrade, set constOldDbLibFileName to ''. }
   constOldDbLibFileName         = 'Old_database_library_file.DBLib';
   constRequiredDbLibFileName    = 'Current_database_library_file.DBLib';
   constApprovedDblinkFileName   = 'Database_link_file_no_symbol_sync.DBLink';
   constRequiredDbLibFilePath    = constGlobalScriptsAndLibsWc + constRequiredDbLibFileName;
   constApprovedDblinkFilePath   = constGlobalScriptsAndLibsWc + constApprovedDblinkFileName;

{* Declare the names of all ProjectOutputs/ subdirs. *}
   { We are also using these as the names of the OutJob files (don't include ".OutJob" extension here). }
   constEngineersReviewSubDir    = 'XRM1_XIA_engineers_review';
   constSchematicReviewSubDir    = 'XRM2_XIA_schematic_review';
   constLayoutReviewSubDir       = 'XRM3_XIA_layout_review';
   constPurchasingSubDir         = 'XRM4_Purchasing';
   constFabricationSubDir        = 'XRM5_Fabrication';
   constAssemblySubDir           = 'XRM6_Assembly';

{* Declare the names of all ProjectOutputs/ include directories. *}
   constFabricationIncludeSubDir = 'XRM5_Fabrication_include';
   constAssemblyIncludeSubDir    = 'XRM6_Assembly_include';

{* Setup standard timeout limits for svn operations. *}
   constStandardTimeoutLimit     = 1000;                { Each unit here means one Sleep(100) call. }
   constCommitTimeoutLimit       = 10000000;            { Each unit here means one Sleep(100) call. }

{* Various boring constants. *}
   constLineBreak                = #13 + #10;           { Windows CR/LF pair }
   MaxInt                        = 1000000;             { Large positive integer. }
   constStringDelimiter          = '|';                 { Standard delimiter used to split delimited string into a stringList. }
   constStringQuote              = '"';                 { Standard quote char used to protect strings containing spaces when splitting delimited string into a stringList. }
   constStringEquals             = '=';                 { Standard char to use for NAME=VALUE pairs. }
   constStringFpNameVersionDelim = '/';                 { The character that appears exactly once in footprint names to separate FP name on left from version number on right. }

{* Constants related to dialog boxes. *}
   constMaxLinesInShowMessage    = 55;                  { Maximum lines that will fit in ShowMessage() in a horizontally oriented monitor. }

{* Declare names of the project level parameters that we care about *}
   constPcbVersionParm           = 'XIA_fabrication_pcb_part_number_and_version_target';
   constPcbVersionLastParm       = 'XIA_fabrication_pcb_part_number_and_version_last';
   constPcbaVersionParm          = 'XIA_assembly_pcba_version_target';
   constPcbaVersionLastParm      = 'XIA_assembly_pcba_version_last';

{* Constants related to XIA-specific database fields and associated values. *}
   { Note:  If there is no old/interim database key, then set constDbParmDbKeyInterim to ''. }
   constDbParmMfgNumber          = 'MFGNUMBER';         { Name of "MFGNUMBER" parameter. }
   constDbParmValue              = 'VALUE';             { Name of "VALUE" parameter. }
   constDbParmCategory           = 'CATEGORY';          { Name of "CATEGORY" parameter. }
   constDbParmComment            = 'Comment';           { Name of "Comment" parameter. }
   constDbParmDescription        = 'Description';       { Name of "Description" parameter. }
   constDbParmDbKeyInterim       = 'OLD_DB_KEY';        { Name of former database key, that we will use on an interim basis until we find the real db key. }
   constDbParmDbKey              = 'DB_KEY';            { Name of real database key. }
   constDbValCommentStd          = '=VALUE';            { "Standard" setting for Comment value. }
   constDbValValueFreeform       = 'ALLOW_COMMENT_TO_DIFFER_FROM_THIS_VALUE';  { Value of the VALUE field used to flag that Comment field for this component is freeform. }
   constDbValCategoryFreeform    = 'ALLOW_COMMENT_TO_DIFFER_FROM_VALUE_FIELD'; { Value of the CATEGORY field used to flag that Comment field for this component is freeform. }

{* Constants related to processing database info as seen by Altium. *}
   { NOTE:  There must be no actual database parameters with names that conflict with these!! }
   constDbParmModelType          = 'ModelType';         { Text that identifies ModelType in database record string. }
   constDbParmModelName          = 'ModelName';         { Text that identifies ModelName in database record string. }
   constDbParmCurrentModelType   = 'CurrentModelType';  { We will create fake entry named this to mark current model. }
   constDbParmCurrentModelName   = 'CurrentModelName';  { We will create fake entry named this to mark current model. }
   constDbParmParameterName      = 'ParameterName';     { Text that identifies ParameterName in database record string. }
   constDbParmParameterValue     = 'ParameterValue';    { Text that identifies ParameterValue in database record string. }
   constDbParmParameterVisible   = 'ParameterVisible';  { Text that identifies ParameterVisible in database record string. } 
   constDbParmParmVisibleVisible = 'True';              { Text in the value field for ParameterVisible that means "Yes, it's visible.". }

{* Constants related to required schematic components. *}
   constApcb1RefDes              = 'APCB1';
   constGtbCategoryValue         = constDbValCategoryFreeform;  { Value of the CATEGORY field used to recognize Gerber Title Block (gtb) component. }

{* Prefixes for the PCB (printed circuit board fabrication) and PCBA (printed circuit board assembly) part numbers. *}
   constPartNumPrefixPcb         = 'PCB-';
   constPartNumPrefixPcba        = 'PCBA-';

{* Constants related to changes that we will brutally make to xls BOM files. *}
   {Note:  Don't use any regex or sed special chars, aside from "!" (eg. "(", "/", "\", "[", "+", "*", etc.).  Don't use any more "$" chars than the ones already there. }
   {Note:  We MUST do a concatenation to prevent svn from doing a keyword substitution to this script on script checkin! }
   constXlsBomPcbaVerAndRevStr   = 'NOT-AN-ASSEMBLY-RELEASE! DO-NOT-BUILD-OR-PROGRAM-TO-THIS-BOM!  $'+'Rev::               $:'; { Field from xls BOM template that we will replace with PCBA version if we are doing assembly release. }
   constXlsBomRevStr             = '$'+'Rev::               $:';{ Just the svn rev # part of the above string. }
   
{* Setup constants for some common file extensions. *}
   constExtOutJob                = '.OutJob';     { Altium OutJob file. }
   constExtXls                   = '.xls';        { Excel BOM file. }
   constExtXlsUpper              = '.XLS';        { Excel BOM file, forced to all upper case. }
   constExtXlsTemp               = '_TEMP.XLS';   { Temporary Excel BOM file. }
   constExtCsv                   = '.csv';        { Comma separated values BOM file. }
   constExtPdf                   = '.pdf';        { Adobe pdf document. }
   constExtMultiWireNetlist      = '.NET';        { Altium generated Multiwire netlist. }
   constExtMultiWireNetlistSorted= '_1.NET';      { Script generated sorted Multiwire netlist. }
   constExtIpc356Netlist         = '.IPC';        { Altium generated IPC-356 netlist. }
   constExtIpc356NetlistFixed    = '_fixed.ipc';  { Script generated fixed IPC-356 netlist. }
   constExtBatScript             = '.bat';        { DOS/Windows batch script. }
   constExtBatScriptRcFile       = '_rc.txt';     { File to store return code generated by external bat script. }
   constExtBatScriptOutFile      = '_out.txt';    { File to store output generated by external bat script. }
   
{* Setup constants for some common file "kind"'s within Altium. *}
   constKindSch                  = 'SCH';         { Altium schematic document.  Must be all UPPER case. }
   constKindPcbLib               = 'PCBLIB';      { Altium PCB library.  Must be all UPPER case. }
   constKindDbLib                = 'DATABASELIB'; { Altium database library.  Must be all UPPER case. }
   constKindDbLink               = 'DATABASELINK';{ Altium database link.  Must be all UPPER case. }
   constKindPcb                  = 'PCB';
   constKindOutJob               = 'OUTPUTJOB';
   constKindHarness              = 'Harness';
   constKindSchLib               = 'SCHLIB';
   
{* Setup constants for some of the svn commands that we will issue, to avoid having the code full of magic numbers (er, magic strings). *}
   constSvnCmdPropSetKeywords    = 'propset svn:keywords "Date Rev Author Id"';
   constSvnCmdPropDelKeywords    = 'propdel svn:keywords';
   constSvnCmdUpdateDepthEmpty   = 'update --depth=empty';
   constSvnCmdCommit             = 'commit';
   constSvnCmdAdd                = 'add';
   constSvnCmdRevert             = 'revert';
   constSvnCmdMove               = 'move';
   constSvnCmdCopy               = 'copy';
   constSvnCmdInfo               = 'info';
   constSvnCmdStatus             = 'status';
   constSvnCmdStatusWrtServer    = 'status -u';

{* Setup constants for parsing some results from svn commands. *}
   constSvnRepStatusMissing      = '!';
   constSvnRepStatusNotInSvn     = '?';
   constSvnRepStatusModified     = 'M';
   constSvnRepInfoUrl            = 'URL: ';
   constSvnRepInfoLastChangedRev = 'Last Changed Rev: ';
   constSvnRepCommitCommittedRev = 'Committed revision ';
   constSvnRepStatusStatAgainstRev='Status against revision:';
   
{* Setup some standard directory names we'll be expecting to use with svn. *}
   constSvnDirTrunk              = 'trunk';
   constSvnDirReleases           = 'releases';
   constSvnDirTags               = 'tags';

{* Names of our external batch files (without .bat extension). *}
   constBatFileSvn               = 'svn_cmd';
   constBatFileStandardSed       = 'sed_cmd';
   constBatFilePatchingSed       = 'patch_with_sed';
   constBatFileSortMulti         = 'sort_multi';

{* Constants related to part number, version number, svn rev number substitutions. *}
   constSubUninitMarker          = '$ERROR_UNINITIALIZED_VARIABLE$';

{* These are the valid variable substitutions that can be used in zipFileNames and relAndTagSubDir. *}
   { We aren't going to create constants for them at this time, in order to try to keep code readable. 
    $projectName$',     
    $pcbPartNum$',  
    $pcbVersion$',  
    $pcbDocRevNum$',     
    $pcbaPartNum$',     
    $pcbaVersion$',     
    $bomRevNum$',   
   }

{* Note that there are also a number of hardcoded constants in function PopulateStringLists(), 
 immediately below here. *}

{***************************************************************************
 * Global variables.  Highly evil.  Ick ick.
 ***************************************************************************}
var
   whichScriptIsThis             : TString;
   GenerateAndPackageOutputsForm : TGenerateAndPackageOutputsForm;
   DebugFile                     : TextFile;
   SummaryFile                   : TextFile;
   SummaryMessages               : TStringList;
   projectPath                   : TDynamicString;
   enableGenerateOutputs         : Boolean;
   enableSvnCommits              : Boolean;



{***************************************************************************
 * function PopulateStringLists()
 *  Perform init step by populating several string lists.
 *  
 *  What we're doing here is telling the script some detailed information
 *  about each of our 6 OutJob files, as well as flagging some additional
 *  steps that need to be performed after running certain OutJob files.
 *
 *  Note:  Unusually for this script, this function will create all
 *  the var string lists.  They will need to be Free()'ed later.
 *
 *  Returns created and populated string lists in all of these var parms.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function PopulateStringLists(var projOutSubDirs             : TStringList;
                             var projOutIncludes            : TStringList;
                             var outJobFiles                : TStringList;
                             var outJobPdfContainers        : TStringList;
                             var outJobPdfEnableNets        : TStringList;
                             var outJobGenContainers        : TStringList;
                             var outJobStatusMsg            : TStringList;
                             var outJobDoSortMultiNetlist   : TStringList;
                             var outJobSetSvnKeywordsOnBoms : TStringList;
                             var outJobDoFixIpc356Netlist   : TStringList;
                             var deleteExcludes             : TStringList;
                             var zipDoCheckForExisting      : TStringList;
                             var zipExcludes                : TStringList;
                             var zipFindAddlFiles           : TStringList;
                             var zipFileNames               : TStringList;
                             var relAndTagSubDir            : TStringList;
                                 )                          : Integer;
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

//   ShowMessage('Hello world from PopulateStringLists()');
   
   { Create all these string lists before we try to populate them. }
   projOutSubDirs := TStringList.Create;
   projOutIncludes := TStringList.Create;
   outJobFiles := TStringList.Create;
   outJobPdfContainers := TStringList.Create;
   outJobPdfEnableNets := TStringList.Create;
   outJobGenContainers := TStringList.Create;
   outJobStatusMsg := TStringList.Create;
   outJobDoSortMultiNetlist := TStringList.Create;
   outJobSetSvnKeywordsOnBoms := TStringList.Create;
   outJobDoFixIpc356Netlist := TStringList.Create;
   deleteExcludes := TStringList.Create;
   zipDoCheckForExisting := TStringList.Create;
   zipExcludes := TStringList.Create;
   zipFindAddlFiles := TStringList.Create;
   zipFileNames := TStringList.Create;
   relAndTagSubDir := TStringList.Create;
   
   { Populate everything associated with OutJob #1:  XIA_engineers_review_outputs. }
   projOutSubDirs.Add(constEngineersReviewSubDir);
   projOutIncludes.Add('');
   outJobFiles.Add(constEngineersReviewSubDir + constExtOutJob);
   outJobPdfContainers.Add('');
   outJobPdfEnableNets.Add('False');
   outJobGenContainers.Add('Generate_files_XIA_engineers_review');
   outJobStatusMsg.Add('Generating files for XIA_engineers_review.');
   outJobDoSortMultiNetlist.Add(BoolToStr(True));               { Flag that we should sort the multiwire netlist after running this OutJob file. }
   outJobSetSvnKeywordsOnBoms.Add(BoolToStr(False));
   outJobDoFixIpc356Netlist.Add(BoolToStr(False));
   deleteExcludes.Add('*.zip');                                 { Exclude previously created zipfiles. }
   zipDoCheckForExisting.Add(BoolToStr(False));
   zipExcludes.Add('*.zip');
   zipFindAddlFiles.Add('');                                    { No additional outputs to find to include in this zipfile. }
   zipFileNames.Add('');                                        { Create no zipfile for this OutJob. }
   relAndTagSubDir.Add('');                                     { Create no subdirs in releases/ and tags/ to hold the zipfile for this OutJob. }

   { Populate everything associated with OutJob #2:  XIA_schematic_review_outputs. }
   projOutSubDirs.Add(constSchematicReviewSubDir);
   projOutIncludes.Add('');
   outJobFiles.Add(constSchematicReviewSubDir + constExtOutJob);
   outJobPdfContainers.Add('Publish_To_PDF_XIA_schematic_review');
   outJobPdfEnableNets.Add('True');                             { Flag that we are allowed to add net info to this PDF file. }
   outJobGenContainers.Add('');
   outJobStatusMsg.Add('Generating files for XIA_schematic_review.');
   outJobDoSortMultiNetlist.Add(BoolToStr(False));
   outJobSetSvnKeywordsOnBoms.Add(BoolToStr(False));
   outJobDoFixIpc356Netlist.Add(BoolToStr(False));
   deleteExcludes.Add('*.zip');                                 { Exclude previously created zipfiles. }
   zipDoCheckForExisting.Add(BoolToStr(False));
   zipExcludes.Add('*.zip');
   zipFindAddlFiles.Add('');                                    { No additional outputs to find to include in this zipfile. }
   zipFileNames.Add('');                                        { Create no zipfile for this OutJob. }
   relAndTagSubDir.Add('');                                     { Create no subdirs in releases/ and tags/ to hold the zipfile for this OutJob. }

   { Populate everything associated with OutJob #3:  XIA_layout_review_outputs. }
   projOutSubDirs.Add(constLayoutReviewSubDir);
   projOutIncludes.Add('');
   outJobFiles.Add(constLayoutReviewSubDir + constExtOutJob);
   outJobPdfContainers.Add('Publish_To_PDF_XIA_layout_review');
   outJobPdfEnableNets.Add('True');                             { Flag that we are allowed to add net info to this PDF file. }
   outJobGenContainers.Add('Generate_files_XIA_layout_review');
   outJobStatusMsg.Add('Generating files for XIA_layout_review.');
   outJobDoSortMultiNetlist.Add(BoolToStr(False));
   outJobSetSvnKeywordsOnBoms.Add(BoolToStr(False));
   outJobDoFixIpc356Netlist.Add(BoolToStr(False));
   deleteExcludes.Add('*.zip');                                 { Exclude previously created zipfiles. }
   zipDoCheckForExisting.Add(BoolToStr(False));
   zipExcludes.Add('*.zip');
   zipFindAddlFiles.Add('');                                    { No additional outputs to find to include in this zipfile. }
   zipFileNames.Add('');                                        { Create no zipfile for this OutJob. }
   relAndTagSubDir.Add('');                                     { Create no subdirs in releases/ and tags/ to hold the zipfile for this OutJob. }

   { Populate everything associated with OutJob #4:  purchasing outputs. }
   projOutSubDirs.Add(constPurchasingSubDir);
   projOutIncludes.Add('');
   outJobFiles.Add(constPurchasingSubDir + constExtOutJob);
   outJobPdfContainers.Add('');
   outJobPdfEnableNets.Add('False');
   outJobGenContainers.Add('Generate_files_purchasing');
   outJobStatusMsg.Add('Generating files for purchasing.');
   outJobDoSortMultiNetlist.Add(BoolToStr(False));
   outJobSetSvnKeywordsOnBoms.Add(BoolToStr(True));             { Flag that we should set svn prop keywords on generated BOM files in this subdir. }
   outJobDoFixIpc356Netlist.Add(BoolToStr(False));
   deleteExcludes.Add('*.zip');                                 { Exclude previously created zipfiles. }
   zipDoCheckForExisting.Add(BoolToStr(False));
   zipExcludes.Add('"Status Report.Txt"|.ZIP');                 { Specific filenames and/or specific file extensions to exclude from zipfile.  Case insensitive.  Must quote protect anything containing a space char!}
   zipFindAddlFiles.Add('');                                    { No additional outputs to find to include in this zipfile. }
   zipFileNames.Add   ('$projectName$_purch_rev_$bomRevNum$.zip'); { Variables in $dollarsigns$ will be substituted in later.  Don't attempt to use "$" char otherwise. }
   relAndTagSubDir.Add('$projectName$_purch_rev_$bomRevNum$');  { Variables in $dollarsigns$ will be substituted in later.  Don't attempt to use "$" char otherwise. }

   { Populate everything associated with OutJob #5:  fabrication outputs. }
   { NOTE:  For zipFileName, it MUST end with "_$pcbDocRevNum$.zip" unless you rewrite CheckForExistingZipFile()! }
   projOutSubDirs.Add(constFabricationSubDir);
   projOutIncludes.Add(constFabricationIncludeSubDir);          { Specify that there is an include subdirectory for this OutJob. }
   outJobFiles.Add(constFabricationSubDir + constExtOutJob);
   outJobPdfContainers.Add('');
   outJobPdfEnableNets.Add('False');
   outJobGenContainers.Add('Generate_files_fabrication');
   outJobStatusMsg.Add('Generating files for fabrication.');
   outJobDoSortMultiNetlist.Add(BoolToStr(False));
   outJobSetSvnKeywordsOnBoms.Add(BoolToStr(False));
   outJobDoFixIpc356Netlist.Add(BoolToStr(True));               { Flag that we should fixup the IPC-356 netlist after running this OutJob file. }
   deleteExcludes.Add('*.zip');                                 { Exclude previously created zipfiles. }
   zipDoCheckForExisting.Add(BoolToStr(True));                  { Check for existing zipfiles with same version number (though possibly different svn rev #). }
   zipExcludes.Add('"Status Report.Txt"|.REP|.APR_LIB|.RUL|.ZIP');{ Specific filenames and/or specific file extensions to exclude from zipfile.  Case insensitive.  Must quote protect anything containing a space char!}
   zipFindAddlFiles.Add('');                                    { No additional outputs to find to include in this zipfile. }
   zipFileNames.Add   ('$pcbPartNum$-$pcbVersion$_fab_rev_$pcbDocRevNum$.zip'); { Variables in $dollarsigns$ will be substituted in later.  Don't attempt to use "$" char otherwise. }
   relAndTagSubDir.Add('$projectName$-$pcbVersion$_fab_rev_$pcbDocRevNum$'); { Variables in $dollarsigns$ will be substituted in later.  Don't attempt to use "$" char otherwise. }

   { Populate everything associated with OutJob #6:  assembly outputs. }
   { NOTE:  For zipFileName, it MUST end with "_$bomRevNum$.zip" unless you rewrite CheckForExistingZipFile()! }
   projOutSubDirs.Add(constAssemblySubDir);
   projOutIncludes.Add(constAssemblyIncludeSubDir);             { Specify that there is an include subdirectory for this OutJob. }
   outJobFiles.Add(constAssemblySubDir + constExtOutJob);
   outJobPdfContainers.Add('Publish_To_PDF_assembly');
   outJobPdfEnableNets.Add('False');
   outJobGenContainers.Add('Generate_files_assembly');
   outJobStatusMsg.Add('Generating files for assembly.');
   outJobDoSortMultiNetlist.Add(BoolToStr(False));
   outJobSetSvnKeywordsOnBoms.Add(BoolToStr(False));
   outJobDoFixIpc356Netlist.Add(BoolToStr(False));
   deleteExcludes.Add('*.zip');                                 { Exclude previously created zipfiles. }
   zipDoCheckForExisting.Add(BoolToStr(True));                  { Check for existing zipfiles with same version number (though possibly different svn rev #). }
   zipExcludes.Add('"Status Report.Txt"|.REP|.APR_LIB|.RUL|.ZIP');{ Specific filenames and/or specific file extensions to exclude from zipfile.  Case insensitive.  Must quote protect anything containing a space char!}
   zipFindAddlFiles.Add(constPurchasingSubDir + '|' + '*.xls'); { Specify that we should also include all *.xls files in purchasing/ subdir. }
   zipFileNames.Add   ('$pcbaPartNum$-$pcbaVersion$_assy_rev_$bomRevNum$.zip'); { Variables in $dollarsigns$ will be substituted in later.  Don't attempt to use "$" char otherwise. }
   relAndTagSubDir.Add('$projectName$-$pcbaVersion$_assy_rev_$bomRevNum$'); { Variables in $dollarsigns$ will be substituted in later.  Don't attempt to use "$" char otherwise. }

end; { end PopulateStringLists() }


{***************************************************************************
 * function FreeStringLists()
 *  Perform cleanup step by freeing several string lists.
 *  
 *  Returns freed string lists in all of these var parms.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function FreeStringLists(var projOutSubDirs             : TStringList;
                         var projOutIncludes            : TStringList;
                         var outJobFiles                : TStringList;
                         var outJobPdfContainers        : TStringList;
                         var outJobPdfEnableNets        : TStringList;
                         var outJobGenContainers        : TStringList;
                         var outJobStatusMsg            : TStringList;
                         var outJobDoSortMultiNetlist   : TStringList;
                         var outJobSetSvnKeywordsOnBoms : TStringList;
                         var outJobDoFixIpc356Netlist   : TStringList;
                         var deleteExcludes             : TStringList;
                         var zipDoCheckForExisting      : TStringList;
                         var zipExcludes                : TStringList;
                         var zipFindAddlFiles           : TStringList;
                         var zipFileNames               : TStringList;
                         var relAndTagSubDir            : TStringList;
                             )                          : Integer;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

//   ShowMessage('Hello world from FreeStringLists()');
   
   { Free all these string lists. }
   projOutSubDirs.Free;
   projOutIncludes.Free;
   outJobFiles.Free;
   outJobPdfContainers.Free;
   outJobPdfEnableNets.Free;
   outJobGenContainers.Free;
   outJobStatusMsg.Free;
   outJobDoSortMultiNetlist.Free;
   outJobSetSvnKeywordsOnBoms.Free;
   outJobDoFixIpc356Netlist.Free;
   deleteExcludes.Free;
   zipDoCheckForExisting.Free;
   zipExcludes.Free;
   zipFindAddlFiles.Free;
   zipFileNames.Free;
   relAndTagSubDir.Free;
   
end; { end FreeStringLists() }


{***************************************************************************
 * procedure XrmAtExit()
 *  Attempt to close all project documents and XRM primary dialog box at exit.
 ***************************************************************************}
procedure XrmAtExit(foo : Integer);
begin

//   ShowMessage('Hello world from XrmAtExit()');
   
   { Try to close all project documents. }
   ResetParameters;
   AddStringParameter('ObjectKind', 'FocusedProjectDocuments');
   RunProcess('WorkspaceManager:CloseObject');
   
   { Close primary dialog box. }
   { TODO:  Figure out why this doesn't actually work. }
   ModalResult := mrOK;
   GenerateAndPackageOutputsForm.Close;
   
//   GenerateAndPackageOutputsForm.Free;

end; { end XrmAtExit() }


{***************************************************************************
 * function StepPlusPlus()
 *  Post increment step number.
 *
 *  Returns incremented value of step as var parm (wait for it, what could it be, oh yeah) step.
 *  Returns original value of step (before increment) as function return value.
 ***************************************************************************}
function StepPlusPlus(var step : Integer;
                          )    : Integer;
   
begin

   { Return original value of step as function return value. }
   result := step;
   
   { Increment step # }
   step := step + 1;

end; { end StepPlusPlus() }


{***************************************************************************
 * function StripTrailingBackslash()
 *  Strip any trailing backslashes from a file path.
 *
 *  Returns stripped string as function return value.
 ***************************************************************************}
function StripTrailingBackslash(filePath : TDynamicString;
                                )        : TDynamicString;
var
   len      : Integer;
   position : Integer;
   temp     : TDynamicString;
   
begin

   { Copy input to temp. }
   temp := filePath;

   //   ShowMessage('String is "' + temp + '".');

   { Repeat until we no longer have a '\' char as the last char in this string. }
   repeat
   begin
      
      { Determine string length and position of last '\' char in string. }
      len := Length(temp);
      position := LastDelimiter('\', temp);
      
      { If the position of the last '\' char is at the end of the string, then strip it off. }
      if ( (position = len) and (len > 0) ) then
      begin
         SetLength(temp, (position-1));
      end;
      
   end;
   until ( (Length(temp) = 0) or (LastDelimiter('\', temp) <> (Length(temp))) );
   
   //   ShowMessage('String is now "' + temp + '".');

   //   ShowMessage('String is "' + temp + '".');
   //   ShowMessage('Strlen is ' + IntToStr(Length(temp)));
   //   ShowMessage('LastDelimiter returns ' + IntToStr(LastDelimiter('\', temp)));

   { Return temp as function return value. }
   result := temp;

end; { end StripTrailingBackslash() }


{***************************************************************************
 * function SplitDelimitedStringIntoStringList()
 *  Splits a delimited string (eg. 'foo|bar|bin|bat|"gee whiz"') into a list containing
 *  eg. 'foo', 'bar', 'bin', 'bat', and 'gee whiz'.
 *  
 *  Note:  Assumes that stringList has already been created.
 *  Note:  Assumes that any field containing spaces (eg. "gee whiz" above) is quoted!
 *
 *  Returns populated string list in var parameter stringList.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function SplitDelimitedStringIntoStringList(    delimitedString : TDynamicString;
                                                delimiter       : TDynamicString;
                                            var stringList      : TStringList;
                                                )               : Integer;
var
   i        : Integer;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Sanity check on delimiter. }
   if (Length(delimiter) <> 1) then
      MyAbort('In SplitDelimitedStringIntoStringList(), delimiter is required to be of length 1!');

   { Specify delimiter. }
   stringList.Delimiter := delimiter;

   { Specify quote character. }
   stringList.QuoteChar := constStringQuote;

   { Import delimitedString into list. }
   stringList.DelimitedText := delimitedString;

end; { end SplitDelimitedStringIntoStringList() }


{***************************************************************************
 * function SplitStringIntoLeftAndRight()
 *  Split a string with a single delimiter character into "left" and "right"
 *  halves.
 *  
 *  Returns left half in var parm leftStr.
 *  Returns right half in var parm rightStr.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function SplitStringIntoLeftAndRight(    splitMe   : TDynamicString;
                                         delimiter : TString;
                                     var leftStr   : TDynamicString;
                                     var rightStr  : TDynamicString;
                                         )         : Integer;
var
   position : Integer;
                                         
begin

   { Assume success. }
   result := 0;
   leftStr := '';
   rightStr := '';

   { This entry will look something like "foo=bar".
    So split it into a left string (before '=' char) and a right string (after '=' char). }

   { Find the position of the next delimiter character. }
   position := AnsiPos(delimiter, splitMe);

   { Sanity check. }
   if (position <= 0) then
   begin
      result := 1;  { Flag that we had an error. }
      WriteToDebugFile('Unable to find delimiter "' + delimiter + '" in string "' + splitMe + '"!');
   end

   { Else we passed the sanity check.  Proceed. }
   else
   begin

      { The left string is everything up until the char before the delimiter. }
      leftStr := Copy(splitMe, 0, (position-1));
      //            WriteToDebugFile('db leftStr is "' + leftStr + '".');

      { The right string is everything after the delimiter char. }
      rightStr := Copy(splitMe, (position+1), MaxInt);
      //            WriteToDebugFile('db rightStr is "' + rightStr + '".');

   end; { endelse }

end; { end SplitStringIntoLeftAndRight() }


{***************************************************************************
 * function SplitStringIntoLeftAndRightWithAbort()
 *  Split a string with a single delimiter character into "left" and "right"
 *  halves.
 *  
 *  Returns left half in var parm leftStr.
 *  Returns right half in var parm rightStr.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function SplitStringIntoLeftAndRightWithAbort(    splitMe   : TDynamicString;
                                                  delimiter : TString;
                                              var leftStr   : TDynamicString;
                                              var rightStr  : TDynamicString;
                                                  )         : Integer;
var
   rc : Integer;
                                         
begin

   { Assume success. }
   result := 0;

   { Call SplitStringIntoLeftAndRight() to do all the real work. }
   rc := SplitStringIntoLeftAndRight(splitMe,
                                     delimiter,
                                     leftStr,
                                     rightStr);

   { Sanity check. }
   if (rc <> 0) then
   begin
      MyAbort('Unable to find delimiter "' + delimiter + '" in string "' + splitMe + '"!');
   end

end; { end SplitStringIntoLeftAndRightWithAbort() }


{***************************************************************************
 * function DeleteFileWithVerify()
 *  Try to delete a file and make sure we succeeded.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function DeleteFileWithVerify(filePath : TDynamicString;
                              )        : Integer;
var
   i      : Integer;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { See if the file even exists. }
   if (FileExists(filePath)) then
   begin
      
      { Attempt to delete this file. }
      if (not DeleteFile(filePath)) then
      begin
         ShowError('Unable to delete file ' + filePath + '.' + constLineBreak + constLineBreak +
                   'Please close whatever program has this file open (eg. Excel or Acroread or Altium).' + constLineBreak +
                   'Then click OK, and I will try one more time.' + constLineBreak + constLineBreak +
                   'Note:  If I''m complaining about an html file, it''s probably something that Altium itself has open.' + constLineBreak +
                   'In this case, you won''t be able to close it since this script is running.' + constLineBreak +
                   'So after script aborts, close all documents, and then re-start this script.');
         
         { Attempt to delete it one more time. }
         if (not DeleteFile(filePath)) then
         begin
            MyAbort('Still unable to delete file ' + filePath + '.');

            { Report error. }
            result := 1;

         end { endif attempt to delete one more time }
         
      end { endif attempt to delete }

   end; { endif file exists }

end; { end DeleteFileWithVerify() }


{***************************************************************************
 * function MyFindFilesSpecifyRecursion()
 *  Search a given directory (either recursively or non-recursively)
 *  for files matching specified mask.
 *
 *  Note:  FilesOnly string list is assumed to have already been created by caller.
 *  
 *  Returns:  String list of generated files in var parameter FilesOnly.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function MyFindFilesSpecifyRecursion(    projOutPath : TString;
                                         subDir      : TString;
                                         mask        : TString;
                                         recursive   : Boolean;
                                     var FilesOnly   : TStringList);
var
   FilesAndDirs : TStringList;
   attrs        : Integer;
   i            : Integer;
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Initialize lists of found files. }
   FilesAndDirs := TStringList.Create;
   
   { Fetch a list of all files in this subdirectory.  Doesn't seem to work with the ( - faDirectory ) added in, so
    we'll have to weed out directory results later. }
   { This will be either a recursive or non-recursive search, depending on value of parm "recursive". }
   FindFiles(projOutPath + '\' + subDir + '\', mask, (faAnyFile) { - faDirectory)}, recursive, FilesAndDirs);

   { Loop over all the files and directories returned to us. }
   for i := 0 to FilesAndDirs.Count - 1 do
   begin

      //       ShowMessage(FilesAndDirs.Strings[i]);

      { Unfortunately, the FindFiles() call will also return directories.  So attempt to weed these out. }
      attrs := FileGetAttr(FilesAndDirs.Strings[i]);

      { if the directory bit is not set, then proceed to attempt to delete file. }
      if (attrs and faDirectory = 0) then
      begin
         //          ShowMessage('Found non-directory file ' + FilesAndDirs.Strings[i] + '.');

         { Add this entry to list FilesOnly. }
         FilesOnly.Add(FilesAndDirs.Strings[i]);
         
      end { is not directory }

   end; { endfor }

   { Free up FilesAndDirs list. }
   FilesAndDirs.Free;

end; { end MyFindFilesSpecifyRecursion() }


{***************************************************************************
 * function MyFindFiles()
 *  Search a given directory for files matching specified mask.
 *  
 *  Note:  Assumes that list FilesOnly has already been created!
 *
 *  Returns:  String list of generated files in var parameter FilesOnly.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function MyFindFiles(    projOutPath : TString;
                         subDir      : TString;
                         mask        : TString;
                     var FilesOnly   : TStringList);
begin

   { Call MyFindFilesSpecifyRecursion() to do all the real work. }
   result := MyFindFilesSpecifyRecursion(projOutPath,
                                         subDir,
                                         mask,
                                         False,     { Specify no recursive searching. }
                                         {var} FilesOnly);

end; { end MyFindFiles() }


{***************************************************************************
 * function MyFindFilesRecursive()
 *  Search a given directory recursively for files matching specified mask.
 *  
 *  Note:  Assumes that list FilesOnly has already been created!
 *
 *  Returns:  String list of generated files in var parameter FilesOnly.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function MyFindFilesRecursive(    projOutPath : TString;
                                  subDir      : TString;
                                  mask        : TString;
                              var FilesOnly   : TStringList);
begin

   { Call MyFindFilesSpecifyRecursion() to do all the real work. }
   result := MyFindFilesSpecifyRecursion(projOutPath,
                                         subDir,
                                         mask,
                                         True,      { Specify to do recursive searching. }
                                         {var} FilesOnly);

end; { end MyFindFilesRecursive() }


{***************************************************************************
 * function IsFileZeroLength()
 *  Determine if a given file is 0 length.
 *
 *  Returns:  True if file is 0 length, False if non-existent or non-zero length.
 ***************************************************************************}
function IsFileZeroLength(filePath : TDynamicString;
                          )        : Boolean;
var
   fileHandle : TextFile;
   
begin

//   ShowMessage('In IsFileZeroLength(), filePath is "' + filePath + '".');

   { Assume failure until we know otherwise. }
   result := False;

   { See if the file even exists. }
   if (FileExists(filePath)) then
   begin
      
      { Try to open file for reading. }
      AssignFile(fileHandle, filePath);
      Reset(fileHandle);

      { See if we are already at EOF. }
//    ShowMessage('In IsFileZeroLength(), EOF is ' + BoolToStr(EOF(fileHandle)) + '.');

      { Return this information to caller. }
      result := EOF(fileHandle);

      { Close file. }
      CloseFile(fileHandle);

   end; { endif file exists }

end; { end IsFileZeroLength() }


{***************************************************************************
 * function IsFileWriteable()
 *  Determine if a given file is writeable (eg. not flocked by Excel or Acroread)..
 *
 *  Returns:  True if file is writeable, False if non-existent or non-writeable.
 ***************************************************************************}
function IsFileWriteable(filePath : TDynamicString;
                         )        : Boolean;
var
   age        : Integer;
   newAge     : Integer;
   dir        : TDynamicString;
   name       : TDynamicString;
   rc         : Integer;
   
begin

   WriteToDebugFile('*In IsFileWriteable(), filePath is "' + filePath + '".');
   WriteToDebugFile('IsFileWriteable(), before doing anything, GetCurrentDir() reports "' + GetCurrentDir + '".');

   { Assume failure until we know otherwise. }
   result := False;

   { See if the file even exists. }
   if (FileExists(filePath)) then
   begin

      age := FileAge(filePath);
//    WriteToDebugFile('IsFileWriteable(), age before is ' + IntToStr(age) + '.');

      { The set age command requires us to operate in the current directory only. }
      dir := ExtractFileDir(filePath);
      name := ExtractFileName(filePath);
      ChDir(dir);

//    WriteToDebugFile('IsFileWriteable(), dir is "' + dir + '", name is "' + name + '".');
//    WriteToDebugFile('IsFileWriteable(), GetCurrentDir() reports "' + GetCurrentDir + '".');

      {* Attempt to set the age of this file to 1/1/2000. *}
      rc := FileSetDate(name, DateTimeToFileDate(StrToDateTime('01/01/2000 12:34:56')));

      { Report success if warranted. }
      if (rc = 0) then
         result := True;

      WriteToDebugFile('In IsFileWriteable(), rc from FileSetDate() was ' + IntToStr(rc) + '.');

      newAge := FileAge(filePath);
//    WriteToDebugFile('IsFileWriteable(), newAge after initial mod is ' + IntToStr(newAge) + '.');

      {* Attempt to set the age of this file back to what it was. *}
      rc := FileSetDate(name, age);

      newAge := FileAge(filePath);
//    WriteToDebugFile('IsFileWriteable(), newAge after final mod is ' + IntToStr(newAge) + '.');

      { Report success if warranted. }
      if ( (rc = 0) and (age = FileAge(filePath)) and (result = True) ) then
         result := True
      else
         result := False;
      
      WriteToDebugFile('IsFileWriteable(), reporting result as ' + BoolToStr(result) + '.');

   end; { endif file exists }

end; { end IsFileWriteable() }


{***************************************************************************
 * function VerifyFileIsWriteable()
 *  Verify that a given file is writeable (eg. not flocked by Excel or Acroread).
 *  If it is flocked, warn the user to close it, then check one more time.
 *  If on the 2nd check the file is still flocked, then abort script with error.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function VerifyFileIsWriteable(filePath : TDynamicString;
                               )        : Boolean;
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Make sure the original xls BOM file is writeable (eg. has been closed by Excel). }
   if (not IsFileWriteable(filePath)) then
   begin

      { Display warning dialog box to user. }
      ShowWarning('File "' + filePath + '" is not writeable (probably due to being flocked).' + constLineBreak + constLineBreak +
                  'Probably is it still open in Excel or Acroread.' + constLineBreak + constLineBreak +
                  'Please close this file and then click Ok.' + constLineBreak + constLineBreak +
                  'I will try one more time.');

      { Check one more time. }
      if (not IsFileWriteable(filePath)) then
         MyAbort('File "' + filePath + '" is still not writeable.');
      

   end; { endif }

end; { end VerifyFileIsWriteable() }


{***************************************************************************
 * function TruncateFile()
 *  Try to truncate a file to zero length.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function TruncateFile(filePath : TDynamicString;
                      )      : Integer;
var
   i          : Integer;
   fileHandle : TextFile;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { See if the file even exists. }
   if (FileExists(filePath)) then
   begin
      
      {* Attempt to truncate this file. *}
      { Try to open file for writing. }
      AssignFile(fileHandle, filePath);
      ReWrite(fileHandle);
      
      { Close file. }
      CloseFile(fileHandle);

   end { endif file exists }

   { Else report failure.  File does not already exist. }
   else
   begin

      { Report failure. }
      result := 1;

   end; { endelse }

end; { end TruncateFile() }


{***************************************************************************
 * function TruncateFileWithVerify()
 *  Try to truncate a file to zero length and make sure we succeeded.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function TruncateFileWithVerify(filePath : TDynamicString;
                                )        : Integer;
var
   i          : Integer;
   size       : Int64;
   tempFile   : TextFile;
   fileHandle : THandle;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { See if the file even exists. }
   if (FileExists(filePath)) then
   begin
      
      { Attempt to truncate this file to 0 length. }
      TruncateFile(filePath);
      
      { If file size is not 0, then panic. }
      if (not IsFileZeroLength(filePath)) then
      begin
         ShowError('Unable to truncate file ' + filePath + '.' + constLineBreak + constLineBreak +
                   'Please close whatever program has this file open (eg. Excel or Acroread or Altium).' + constLineBreak +
                   'Then click OK, and I will try one more time.' + constLineBreak + constLineBreak +
                   'Note:  If I''m complaining about an html file, it''s probably something that Altium itself has open.' + constLineBreak +
                   'In this case, you won''t be able to close it since this script is running.' + constLineBreak +
                   'So after script aborts, close all documents, and then re-start this script.');
         
         {* Attempt to truncate this file one more time. *}
         TruncateFile(filePath);

         { Make sure we succeeded. }
         if (not IsFileZeroLength(filePath)) then
         begin

            MyAbort('Still unable to truncate file ' + filePath + '.');

            { Report error. }
            result := 1;

         end { endif attempt to delete one more time }
         
      end { endif attempt to truncate }

   end; { endif file exists }

end; { end TruncateFileWithVerify() }


{***************************************************************************
 * procedure OpenDebugFile()
 *  Open debug file for the first time.
 ***************************************************************************}
procedure OpenDebugFile(fileName :  TDynamicString);
begin

//   ShowMessage('In OpenDebugFile(), fileName is ' + fileName);
   
   { Truncate old version of debug file and verify that we succeeded. }
   TruncateFileWithVerify(fileName);
   
   { Try to open debug file for writing. }
   AssignFile(DebugFile, fileName);
   
end; { end OpenDebugFile() }


{***************************************************************************
 * procedure WriteToDebugFile()
 *  Write a new line of text to the debug file.
 *  
 *  Note that we will be operating in append-write-close mode in order to
 *  prevent Altium from flocking this file in the event of an unhandled
 *  Altium script crash.
 ***************************************************************************}
procedure WriteToDebugFile(msg : TDynamicString);
begin

//   ShowMessage('Writing this to DebugFile: "' + msg + '"');

   { Reopen file in append mode. }
   Append(DebugFile);
   
   { Write new line of text to debug file. }
   WriteLn(DebugFile, msg);

   { Close debug file. }
   CloseFile(DebugFile);

end; { end WriteToDebugFile() }


{***************************************************************************
 * procedure CloseDebugFile()
 *  Close debug file.
 *
 *  Note:  Since we're now operating in append-write-close mode, there's
 *  no longer anything we actually have to do here.
 *  
 *  Note:  foo is a dummy parameter that exists only to keep CloseDebugFile()
 *  from being offered as the script entry point in Altium.
 ***************************************************************************}
procedure CloseDebugFile(foo : Integer);
begin

//   ShowMessage('In CloseDebugFile()');
   
end; { end CloseDebugFile() }


{***************************************************************************
 * procedure OpenSummaryFile()
 *  Open summary file for the first time.
 ***************************************************************************}
procedure OpenSummaryFile(fileName :  TDynamicString);
begin

   { Truncate old version of summary file and verify that we succeeded. }
   TruncateFileWithVerify(fileName);
   
   { Try to open summary file for writing. }
   AssignFile(SummaryFile, fileName);

   { Create string lists that will later be written as the summary file. }
   SummaryMessages := TStringList.Create;
   
end; { end OpenSummaryFile() }


{***************************************************************************
 * procedure WriteToSummaryFile()
 *  Write a new line of text to the summary file.
 *  
 *  Note that we will be operating in append-write-close mode in order to
 *  prevent Altium from flocking this file in the event of an unhandled
 *  Altium script crash.
 ***************************************************************************}
procedure WriteToSummaryFile(msg : TDynamicString);
begin

   { Reopen file in append mode. }
   Append(SummaryFile);
   
   { Write new line of text to summary file. }
   WriteLn(SummaryFile, msg);

   { Close summary file. }
   CloseFile(SummaryFile);

   { Add this line of text to string list that we are keeping resident in memory. }
   SummaryMessages.Add(msg);   
   
end; { end WriteToSummaryFile() }


{***************************************************************************
 * procedure CloseSummaryFile()
 *  Close summary file.
 *
 *  Note:  Since we're now operating in append-write-close mode, there's
 *  very little that we actually have to do here.
 *  
 *  Note:  foo is a dummy parameter that exists only to keep CloseDebugFile()
 *  from being offered as the script entry point in Altium.
 ***************************************************************************}
procedure CloseSummaryFile(foo : Integer);
begin

   { Free string list. }
   SummaryMessages.Free;
   
end; { end CloseSummaryFile() }


{***************************************************************************
 * procedure UpdateGuiStatusMessage()
 *  Update status message in dialog box and write said status to the debug file.
 ***************************************************************************}
procedure UpdateGuiStatusMessage(msg :  TDynamicString);
begin

   { Change text in GUI status line. }
   formStatusBar1.SimpleText := msg;

   { Force a screen refresh of GUI status line. }
   formStatusBar1.Update;

   { Copy this message to debug file. }
   WriteToDebugFile('');
   WriteToDebugFile('* ' + msg);

end; { end UpdateGuiStatusMessage() }


{***************************************************************************
 * procedure AtExit()
 *  Call cleanup routines.
 ***************************************************************************}
procedure AtExit(rc : Integer);
var
   FileName : TDynamicString;
   msg      : TDynamicString;
   i        : Integer;

begin 

   { If we have less than or equal to 55 lines in the summary messages list,
    or we're not being called from the UFD script, then just use ShowMessage().}
   if ( (SummaryMessages.Count <= constMaxLinesInShowMessage) or
       (whichScriptIsThis <> constWhichScriptUfd) ) then
   begin
      
      { Loop over all the lines in summary. }
      msg := '';

      { Loop over all the summary messages. }
      for i := 0 to SummaryMessages.Count - 1 do
      begin

         { Append next line to running message. }
         msg := msg + SummaryMessages.Strings[i] + constLineBreak;
         
      end; { endfor }

      { If we were given a sucess error code, meaning we're exiting successfully, report that. }
      if ( (rc = 0) and (enableGenerateOutputs = True) and (enableSvnCommits = True) ) then
      begin
         msg := msg + constLineBreak + constLineBreak +
         'Script is exiting successfully.  Click Ok to finish.';

         { Display summary to screen. }
         ShowMessage(msg);
         
      end

      { Else report error exit condition. }
      else
      begin

         { Report if either enableGenerateOutputs or enableSvnCommits were disabled. }
         if ( (enableGenerateOutputs = False) or (enableSvnCommits = False) ) then
         begin
            
            { Report if enableGenerateOutputs was disabled. }
            if (enableGenerateOutputs = False) then
            begin
               msg := msg + constLineBreak + constLineBreak +
               'ERROR ERROR ERROR Script was running with enableGenerateOutputs set to False, meaning that I generated no outputs!!!';
            end;

            { Report if enableSvnCommits was disabled. }
            if (enableSvnCommits = False) then
            begin
               msg := msg + constLineBreak + constLineBreak +
               'ERROR ERROR ERROR Script was running with enableSvnCommits set to False, meaning that I did no svn commits!!!';
            end;

            { Report if we have an error code from elsewhere in this script. }
            if (rc <> 0) then
            begin
               msg := msg + constLineBreak + constLineBreak +
               'Failed while running this operation: ' + formStatusBar1.SimpleText + constLineBreak + constLineBreak +
               'ERROR:  Script is exiting prematurely due to error!';
            end;
            
         end;
         
         { Display summary to screen. }
         ShowError(msg);
         
      end; { endelse }

   end { endif }

   { Else we need to call a custom dialog box in the UFD script so that the user can
    scroll through all the summary messages. }
   else
   begin

      { Call UfdAtExit() to do all the real work. }
      UfdAtExit(rc);

   end; { endelse }
   
   { Close debug file. }
   CloseDebugFile(0);

   { Close summary file. }
   CloseSummaryFile(0);

   { Attempt to call script-specific AtExit() procedure. }
   if (whichScriptIsThis = constWhichScriptXrm) then
      XrmAtExit(1);
   
end; { end AtExit() }
   

{***************************************************************************
 * procedure MyAbort()
 *  Call cleanup routines and then abort script.
 ***************************************************************************}
procedure MyAbort(msg : TDynamicString);
begin

   { Save abort message to debug file. }
   WriteToDebugFile('');
   WriteToDebugFile('**In MyAbort()!!!');
   WriteToDebugFile(msg);
   
   { Give error message to user. }
   ShowError(msg + constLineBreak + constLineBreak +
               'Aborting script!!!' + constLineBreak + constLineBreak +
               'Afterwards, hit Control-F3 (or go to Run->Stop) to shut down script execution.' + constLineBreak +
               'Then, click on a file in your PCB project to reset focus.');
   
   { Call AtExit() procedure to write debug outputs to file. }
   AtExit(1);                   { Report error at exit }

   { Now do the real abort. }
   Abort;
end; { end MyAbort() }


{***************************************************************************
 * procedure IssueDialogBoxWithOkOrCancel()
 *  Present the user with a dialog box of the specified type, with a specified message in it.
 *  If user clicks OK, optionally display "ok" message and return to caller.
 *  If user clicks Cancel, then abort script with "cancel" message.
 ***************************************************************************}
procedure IssueDialogBoxWithOkOrCancel(dialogType : TMsgDlgType;
                                       msg        : TDynamicString;
                                       okMsg      : TDynamicString;
                                       cancelMsg  : TDynamicString);
var
   button : Integer;

begin

   { Ask the user what we should do. }
   { Specify the dialog box type (mtWarning, mtError, mtInformation, mtConfirmation, or mtCustom). }
   { See http://www.delphibasics.co.uk/RTL.asp?Name=MessageDlg }
   button := MessageDlg(msg, dialogType, mbOKCancel, 0);

   { Now we've gotten either an OK or a Cancel click. }
   if ( (button = mrOk) and (okMsg <> '') ) then
      ShowMessage(okMsg);
   
   if (button = mrCancel) then
      MyAbort(cancelMsg);

end; { end IssueDialogBoxWithOkOrCancel() }

                                    
{***************************************************************************
 * procedure IssueConfirmationWithOkOrCancel()
 *  Present the user with a confirmation dialog box with a specified message in it.
 *  If user clicks OK, optionally display "ok" message and return to caller.
 *  If user clicks Cancel, then abort script with "cancel" message.
 ***************************************************************************}
procedure IssueConfirmationWithOkOrCancel(msg       : TDynamicString;
                                          okMsg     : TDynamicString;
                                          cancelMsg : TDynamicString);
begin

   { Call IssueDialogBoxWithOkOrCancel() to do all the real work. }
   { Specify a confirmation dialog box. }
   IssueDialogBoxWithOkOrCancel(mtConfirmation,
                                msg,
                                okMsg,
                                cancelMsg);

end; { end IssueConfirmationWithOkOrCancel() }

                                    
{***************************************************************************
 * procedure IssueWarningWithOkOrCancel()
 *  Present the user with a warning dialog box with a specified message in it.
 *  If user clicks OK, optionally display "ok" message and return to caller.
 *  If user clicks Cancel, then abort script with "cancel" message.
 ***************************************************************************}
procedure IssueWarningWithOkOrCancel(msg       : TDynamicString;
                                     okMsg     : TDynamicString;
                                     cancelMsg : TDynamicString);
begin

   { Call IssueDialogBoxWithOkOrCancel() to do all the real work. }
   { Specify a warning dialog box. }
   IssueDialogBoxWithOkOrCancel(mtWarning,
                                msg,
                                okMsg,
                                cancelMsg);

end; { end IssueWarningWithOkOrCancel() }

                                    
{***************************************************************************
 * function CleanupSvnRcFile()
 *  Try to delete rc file from external svn command.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CleanupSvnRcFile(svnRcPath : TDynamicString;
                          )         : Integer;
var
   i      : Integer;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Init loop counter. }
   i := 0;

   { Delay for 100 us before we make the first attempt to delete file. }
   Sleep(100);

   { Loop and repeatedly try to delete this file. }
   repeat
   begin
      
      { Attempt to delete the file from disk, now that we've read it in. }
      DeleteFile(svnRcPath);

      { Delay for 100 us. }
      Sleep(100);

      { Increment loop counter. }
      i := i + 1;
   end;
   until ( (not FileExists(svnRcPath)) or (i > constStandardTimeoutLimit) );

   { Make sure we were actually able to delete the file. }
   if (FileExists(svnRcPath)) then
   begin
      MyAbort('Unable to delete file ' + svnRcPath + '!');
   end;
   
end; { end CleanupSvnRcFile() }


{***************************************************************************
 * function AwaitSvnCompletion()
 *  Wait for the external svn command to complete.
 *
 *  Returns first line in svn return code file in var parm svnRc.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function AwaitSvnCompletion(    svnRcPath    : TDynamicString;
                                timeoutLimit : Integer;
                            var svnRc        : TDynamicString;
                                )            : Integer;
var
   buf    : TStringList;
   rc     : Integer;
   i      : Integer;
   done   : Integer;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Repeat the loop as desired by user. }
   repeat
   begin
      
      { Wait in a loop with timeout for the svn return code file to appear.
       The issue is that the external .bat script is started, but we keep running in parallel.
       So we have to wait for it to finish and write its output file before we proceed. }
      i := 0;
      repeat
      begin
         { Delay for 100 us. }
         Sleep(100);
         
         i := i + 1;
      end;
      until ( (FileExists(svnRcPath)) or (i > constStandardTimeoutLimit) );
      
      { See if the file was actually created by the external .bat script. }
      if (FileExists(svnRcPath)) then
      begin
         done := 1;     { Flag that we're done. }
      end

      { Else svn return code file does not exist.  Ask user if he/she wants to keep waiting. }
      else
      begin

         { Issue warning modal dialog box with specified warning message,
          specified reply after clicking Ok, and specified reply after clicking Cancel. }
         IssueWarningWithOkOrCancel('Warning:  Timed out waiting for external svn command to complete.' + constLineBreak +
                                    'Timeout limit was ' + IntToStr(timeoutLimit) + '.' + constLineBreak +
                                    'Sometimes commits may take a while if the server or network is slow.' + constLineBreak +
                                    'Shall I keep waiting (OK) or shall I Cancel running this script?',
                                    'Ok.  Click OK to keep waiting.',
                                    'Timed out waiting for external svn command to complete.  User asked to abort script rather than keep waiting.');
      end; { endelse }

   end; { end repeat }
   until (done = 1);
      
   { Delay for 100 us. }
   Sleep(100);

   { Read the result of the svn command. }
   { There is apparently no way to pass this via return code from the .bat file.
    Return code from RunApplication is 0 if it's able to execute the .bat file.
    It does not give us the return code from .bat file itself. }
   buf := TStringList.Create;
   buf.LoadFromFile(svnRcPath);

   { See if we were able to read from file. }
   if (buf.Count > 0) then
   begin
//    ShowMessage('Reading svn return code file.  First line is ' + buf.Strings[0]);

      { Replace all ' ' (space) chars with null strings. }
      svnRc := StringReplace(buf.Strings[0], ' ', '', MkSet(rfReplaceAll));

   end

   else
   begin
      MyAbort('Unable to read line from svn return code file ' + svnRcPath + '!');
   end;

   { Free string buffer. }
   buf.Free;

   { Cleanup (aka delete) rc file from svn command. }
   CleanupSvnRcFile(svnRcPath);
   
end; { end AwaitSvnCompletion() }
   

{***************************************************************************
 * function IssueSvnCommandGetOutput()
 *  Shell out and call bat file to issue a generic svn command.
 *  Check its return code, and return all output generated by svn.exe.
 *
 *  Returns all svn output in var parm svnOut.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function IssueSvnCommandGetOutput(    scriptsPath : TDynamicString;
                                      projectPath : TDynamicString;
                                      command     : TDynamicString;
                                      parms       : TStringList;
                                  var svnOut      : TStringList;
                                      )           : Integer;
var
   svnRcPath    : TDynamicString;
   svnOutPath   : TDynamicString;
   projectDrive : TDynamicString;
   rc           : Integer;
   svnRc        : TDynamicString;
   i            : Integer;
   timeoutLimit : Integer;
   msg          : TDynamicString;
   cmdLine      : TDynamicString;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Use (projectPath + "svn_rc.txt") as the file with which the external script will
    communicate back to us the return code from svn.exe. }
   svnRcPath := (projectPath + '\svn' + constExtBatScriptRcFile);

   { Attempt to delete any old version of this file from disk. }
   DeleteFileWithVerify(svnRcPath);
   
   { Use (projectPath + "svn_out.txt") as the file with which the external script will
    communicate back to us the output from svn.exe. }
   svnOutPath := (projectPath + '\svn' + constExtBatScriptOutFile);

   { Attempt to delete any old version of this file from disk. }
   DeleteFileWithVerify(svnOutPath);
   
   { Extract the DOS drive (ie. C:) on which the project working directory exists. }
   projectDrive := ExtractFileDrive(projectPath);
   
   { Shell out to run svn_cmd.bat. }
   { When calling svn_cmd.bat, it needs the following command line parameters:
    %1 : Drive on which Altium working directory is located.
    %2 : Path to Altium working directory.
    %3 : The file to which svn_cmd.bat should write its return code.
    %4 : The file to which svn_cmd.bat should write its output.
    %5 : The svn command we wish to run.
    %6 .. : Parameters to supply to svn.
    }
   { Use /k to keep window open after running.  Use /c to close after running. }
   { Add one layer of double quotes around everything after /c. }
   { Add double quote protection to pathSubDir since this is assumed to contain spaces in it. }
   cmdLine := 'cmd.exe /c "' + scriptsPath + '\' + constBatFileSvn + constExtBatScript + ' ' + projectDrive + ' "' + projectPath + '" "' + svnRcPath + '" "' + svnOutPath + '" ' + command + ' ';

   { Add all parms to cmdLine. }
   for i := 0 to parms.Count - 1 do
   begin

      { Add this parameter to cmdLine with double quote protection. }
      { First, though, run parameter through StripTrailingBackslash() to remove any '\' chars at end of filename.
       The DOS version of svn.exe that we're using will barf with a "Cannot determine case of blah blah"
       error when presented with trailing backslashes in filenames.  This is very annoying. }
      cmdLine := cmdLine + '"' + StripTrailingBackslash(parms.Strings[i]) + '" ';
   end; { endfor }

   { Add final close double quote to cmdLine. }
   cmdLine := cmdLine + ' "';

   { Now that the cmdLine is ready, actually shell out and run it. }
   WriteToDebugFile('');
   WriteToDebugFile('*About to call ' + constBatFileSvn + ' as: ' + cmdLine);
   rc := RunApplication(cmdLine);

   { Decide which timeout limit to use for this command. }
   if (command = constSvnCmdCommit) then
   begin
      timeoutLimit := constCommitTimeoutLimit;      { Larger timeout limit for commits. }
   end
   
   else
   begin
      timeoutLimit := constStandardTimeoutLimit;    { Standard timeout limit for svn adds, etc. }
   end;
         
   { Wait for the external svn command to complete and get its return code. }
   AwaitSvnCompletion(svnRcPath,
                      timeoutLimit,
                      {var} svnRc);

   { Read output from svn command into svnOut. }
   svnOut.LoadFromFile(svnOutPath);

   { Copy all lines from svnOut to DebugFile. }
   for i := 0 to svnOut.Count - 1 do
   begin

      { Copy this line to debug file. }
      WriteToDebugFile(svnOut.Strings[i]);
   end; { endfor }

   { Cleanup (aka delete) out file from svn command. }
   CleanupSvnRcFile(svnOutPath);

   { Check return code from external svn command. }
   WriteToDebugFile('*Return code from svn.exe was ' + svnRc);
   if (StrToInt(svnRc) <> 0) then
   begin

      { Abort on error for all svn commands except 'move' and 'revert'. }
      { TODO:  Decide if we want to handle this in some more elegant way, like having a parameter
       that specifies whether we should ignore an error return code from svn.exe. }
      if ( (command <> constSvnCmdMove) and (command <> constSvnCmdRevert) ) then
      begin

         { Abort script with error message. }
         MyAbort('External svn command reported error.  cmdLine was "' + cmdLine + '".  Return code was ' + svnRc + '!');
      end

      { Else we're doing an svn move (currently only performed by script MigrateOutputsForAltium10.pas),
       or an svn revert (which will complain if directories we're trying to revert don't already exist in svn),
       so ignore error return from svn.exe.  Report errors to user dialog box, but continue running. }
      else
      begin

         { Setup beginning of message to user. }
         msg := 'WARNING:  Svn reports errors as seen in this output below.' + constLineBreak +
         'Since this was an svn ' + command + ' command, this may be OK.' + constLineBreak +
         'I will not abort script and will keep running after you click OK.' + constLineBreak + constLineBreak;

         { Copy all lines from svnOut to DebugFile. }
         for i := 0 to svnOut.Count - 1 do
         begin

            { Add this line to dialog box message. }
            msg := msg + svnOut.Strings[i] + constLineBreak;

         end; { endfor }

         { Display warning dialog box to user. }
         ShowWarning(msg);

      end; { endelse }

   end; { endif }

end; { end IssueSvnCommandGetOutput() }


{***************************************************************************
 * function IssueSvnCommandLookForOutputLine()
 *  Shell out and call bat file to issue a generic svn command.
 *  Check its return code, and look for a specific line in its output.
 *
 *  Returns the line of svn output containing "lookForMe" as var parm foundInLine.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function IssueSvnCommandLookForOutputLine(    scriptsPath : TDynamicString;
                                              projectPath : TDynamicString;
                                              command     : TDynamicString;
                                              parms       : TStringList;
                                              lookForMe   : TDynamicString;
                                          var foundInLine : TDynamicString;
                                              )           : Integer;
var
   rc           : Integer;
   svnOut       : TStringList;
   i            : Integer;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Clear var parm output foundInLine. }
   foundInLine := '';

   { Initialize svnOut string list. }
   svnOut := TStringList.Create;

   { Call IssueSvnCommandGetOutput() to do all the real work. }
   IssueSvnCommandGetOutput(scriptsPath,
                            projectPath,
                            command,
                            parms,
                            {var} svnOut);

   { Look for the desired line of output in all the output generated by svn.exe. }
   for i := 0 to svnOut.Count - 1 do
   begin

      { Look for a line in the output file containing a specific string we were given. }
      if (AnsiPos(lookForMe, svnOut.Strings[i]) <> 0) then
         foundInLine := svnOut.Strings[i];
      
   end; { endfor }

   { Free svnOut list. }
   svnOut.Free;
      
end; { end IssueSvnCommandLookForOutputLine() }


{***************************************************************************
 * function IssueSvnCommand()
 *  Shell out and call bat file to issue a generic svn command.
 *  Check its return code, but ignore its output.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function IssueSvnCommand(scriptsPath : TDynamicString;
                         projectPath : TDynamicString;
                         command     : TDynamicString;
                         parms       : TStringList;
                         )           : Integer;
var
   foo    : TDynamicString;

begin

   { Call IssueSvnCommandLookForOutputLine() to do the real work. }
   result := IssueSvnCommandLookForOutputLine(scriptsPath,
                                          projectPath,
                                          command,
                                          parms,
                                          'foo',
                                          {var} foo);

end; { end IssueSvnCommand() }
   

{***************************************************************************
 * function DoSvnRevert()
 *  Do a recursive svn revert on 1 or 2 specified directories.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function DoSvnRevert(scriptsPath : TDynamicString;
                     projectPath : TString;
                     dir1        : TDynamicString;
                     dir2        : TDynamicString;
                     )           : Integer;
var
   rc    : Integer;
   parms : TStringList;

begin

   { Create list of parameters for svn command. }
   parms := TStringList.Create;
   parms.Add('--depth=infinity');
   parms.Add(dir1);
   parms.Add(dir2);
   
   { Issue command to svn revert. }
   result := IssueSvnCommand(scriptsPath,
                             projectPath,
                             constSvnCmdRevert,
                             parms);
   
   { Free list of parameters. }
   parms.Free;
      
end; { end DoSvnRevert() }


{***************************************************************************
 * function DoSvnInfoAndGetOneLine()
 *  Query via svn info and look for a line of text starting with "findLine".
 *
 *  Returns line we found in svn reply, not counting the header we were looking for.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function DoSvnInfoAndGetOneLine(    scriptsPath : TDynamicString;
                                    projectPath : TString;
                                    fileOrDir   : TString;
                                    findLine    : TString;
                                var foundLine   : TDynamicString;
                                    )           : Integer;
var
   rc    : Integer;
   parms : TStringList;

begin

   { Initialize list of new subdirs that get created and need to be checked into svn. }
   parms := TStringList.Create;

   { Specify fileOrDir as the file/directory we want svn info about. }
   parms.Add(fileOrDir);
   
   { Issue svn info command to and look for 1 specific line in the reply from svn.exe. }
   rc := IssueSvnCommandLookForOutputLine(scriptsPath,
                                          projectPath,
                                          constSvnCmdInfo,
                                          parms,
                                          findLine,
                                          {var} foundLine);
   
   { Free list of parms to svn copy. }
   parms.Free;

   { Make sure we found the desired line of output from svn.exe. }
   if (foundLine = '') then
   begin
      MyAbort('Did not find file containing "' + findLine + '" in output from svn.exe!!');
   end;
   
   { Strip off leading heading from string to give us our foundLine. }
   WriteToDebugFile('*Got this line from svn info command: ' + foundLine);

   foundLine := StringReplace(foundLine, findLine, '', '');
   WriteToDebugFile('*foundLine is: ' + foundLine);

   { Return code to caller. }
   result := rc;
   
end; { end DoSvnInfoAndGetOneLine() }


{***************************************************************************
 * function GetFileOrDirSvnServerUrl()
 *  Query to get the svn server side URL for a specified file or directory.
 *
 *  Returns svn server side URL in var parm fileOrDirUrl.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function GetFileOrDirSvnServerUrl(    scriptsPath : TDynamicString;
                                      projectPath  : TString;
                                      fileOrDir    : TString;
                                  var fileOrDirUrl : TDynamicString;
                                      )            : Integer;
begin

   { Call DoSvnInfoAndGetOneLine() to do the real work. }
   result := DoSvnInfoAndGetOneLine(scriptsPath,
                                    projectPath,
                                    fileOrDir,
                                    constSvnRepInfoUrl,
                                    {var} fileOrDirUrl);

end; { end GetFileOrDirSvnServerUrl() }


{***************************************************************************
 * function GetFileSvnRevNum()
 *  Query to get the latest svn rev number for a specified file.
 *
 *  Returns svn rev number (in string form) in var parm fileRevNum.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function GetFileSvnRevNum(    scriptsPath : TDynamicString;
                              projectPath  : TDynamicString;
                              filePath     : TDynamicString;
                          var fileRevNum   : TDynamicString;
                              )            : Integer;
begin

   { Call DoSvnInfoAndGetOneLine() to do the real work. }
   result := DoSvnInfoAndGetOneLine(scriptsPath,
                                    projectPath,
                                    filePath,
                                    constSvnRepInfoLastChangedRev,
                                    {var} fileRevNum);

end; { end GetFileSvnRevNum() }


{***************************************************************************
 * function RunSomeSedBatFile()
 *  Shell out and call specified batFile to run some sed-related command.
 *  Check its return code.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function RunSomeSedBatFile(scriptsPath : TDynamicString;
                           projectPath : TDynamicString;
                           inputPath   : TDynamicString;
                           outputPath  : TDynamicString;
                           batFile     : TDynamicString;
                           command     : TDynamicString;
                           )           : Integer;
var
   sedRcPath : TDynamicString;
   rc        : Integer;
   sedRc     : TDynamicString;
   i         : Integer;
   cmdLine   : TDynamicString;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Use (projectPath + "sed_rc.txt") as the file with which the external script will
    communicate back to us the return code from sed.exe. }
   sedRcPath := (projectPath + '\' + batFile + constExtBatScriptRcFile);

   { Attempt to delete any old version of this file from disk. }
   DeleteFileWithVerify(sedRcPath);
   
   { Attempt to delete any old version of output file from disk. }
   DeleteFileWithVerify(outputPath);
   
   { Shell out to run batFile.bat. }
   { When calling batFile.bat, it needs the following command line parameters:
    %1 : Path to Altium_scripts directory.
    %2 : The file to pipe into sed.
    %3 : The file to which batFile.bat should write its return code.
    %4 : The file to which batFile.bat should write its output.
    %6 : Sed command(s).
    }
   { Use /k to keep window open after running.  Use /c to close after running. }
   { Add one layer of double quotes around everything after /c. }
   { Add double quote protection to pathSubDir since this is assumed to contain spaces in it. }
   cmdLine := 'cmd.exe /c "' + scriptsPath + '\' + batFile + constExtBatScript + ' ' + scriptsPath + ' "' + inputPath + '" "' + sedRcPath + '" "' + outputPath + '" ' + command + ' ';

   { Add final close double quote to cmdLine. }
   cmdLine := cmdLine + ' "';
   
   { Now that the cmdLine is ready, actually shell out and run it. }
   WriteToDebugFile('');
   WriteToDebugFile('*About to call ' + batFile + ' as: ' + cmdLine);
   rc := RunApplication(cmdLine);

   { Wait for the external sed command to complete and get its return code. }
   AwaitSvnCompletion(sedRcPath,
                      constStandardTimeoutLimit,
                      {var} sedRc);

   { Check return code from external sed command. }
   WriteToDebugFile('*Return code from sed.exe was ' + sedRc);
   if (StrToInt(sedRc) <> 0) then
   begin
      MyAbort('External sed command reported error.  cmdLine was "' + cmdLine + '".  Return code was ' + sedRc + '!');
   end;
   
end; { end RunSomeSedBatFile() }


{***************************************************************************
 * function RunSed()
 *  Shell out and call bat file to run standard sed command.
 *  Check its return code.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function RunSed(scriptsPath : TDynamicString;
                projectPath  : TDynamicString;
                inputPath    : TDynamicString;
                outputPath   : TDynamicString;
                command      : TDynamicString;
                )            : Integer;

begin

   { Call RunSomeSedBatFile() to do all the real work. }
   result := RunSomeSedBatFile(scriptsPath,
                               projectPath,
                               inputPath,
                               outputPath,
                               constBatFileStandardSed,
                               command);

end; { end RunSed() }


{***************************************************************************
 * function RunPatchWithSed()
 *  Shell out and call bat file to run patch sed command.
 *  Check its return code.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function RunPatchWithSed(scriptsPath : TDynamicString;
                         projectPath : TDynamicString;
                         inputPath   : TDynamicString;
                         outputPath  : TDynamicString;
                         command     : TDynamicString;
                         )           : Integer;

begin

   { Call RunSomeSedBatFile() to do all the real work. }
   result := RunSomeSedBatFile(scriptsPath,
                               projectPath,
                               inputPath,
                               outputPath,
                               constBatFilePatchingSed,
                               command);

end; { end RunPatchWithSed() }


{***************************************************************************
 * function RunSortMulti()
 *  Shell out and call bat file to sort Multiwire netlist.
 *  Check its return code.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function RunSortMulti(scriptsPath : TDynamicString;
                      projectPath  : TDynamicString;
                      inputPath    : TDynamicString;
                      outputPath   : TDynamicString;
                      )            : Integer;
var
   sortMultiRcPath : TDynamicString;
   rc              : Integer;
   sortMultiRc     : TDynamicString;
   i               : Integer;
   cmdLine         : TDynamicString;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Use (projectPath + "sort_multi_rc.txt") as the file with which the external script will
    communicate back to us the return code from sort_multi.bat. }
   sortMultiRcPath := (projectPath + '\' + constBatFileSortMulti + constExtBatScriptRcFile);

   { Attempt to delete any old version of this file from disk. }
   DeleteFileWithVerify(sortMultiRcPath);
   
   { Attempt to delete any old version of output file from disk. }
   DeleteFileWithVerify(outputPath);
   
   { Shell out to run sort_multi.bat. }
   { When calling sort_multi.bat, it needs the following command line parameters:
    %1 : Path to Altium_scripts directory.
    %2 : The file to operate upon.
    %3 : The file to which sort_multi.bat should write its output.
    %4 : The file to which sort_multi.bat should write its return code.
    }
   { Use /k to keep window open after running.  Use /c to close after running. }
   { Add one layer of double quotes around everything after /c. }
   { Add double quote protection to pathSubDir since this is assumed to contain spaces in it. }
   cmdLine := 'cmd.exe /c "' + scriptsPath + '\' + constBatFileSortMulti + constExtBatScript + ' ' + scriptsPath + ' "' + inputPath + '" "' + outputPath + '" "' + sortMultiRcPath + '" ';

   { Add final close double quote to cmdLine. }
   cmdLine := cmdLine + ' "';
   
   { Now that the cmdLine is ready, actually shell out and run it. }
   WriteToDebugFile('');
   WriteToDebugFile('*About to call sort_multi as: ' + cmdLine);
   rc := RunApplication(cmdLine);

   { Wait for the external sort_multi command to complete and get its return code. }
   AwaitSvnCompletion(sortMultiRcPath,
                      constStandardTimeoutLimit,
                      {var} sortMultiRc);

   { Check return code from external sort_multi command. }
   WriteToDebugFile('*Return code from ' + constBatFileSortMulti + constExtBatScript + ' was ' + sortMultiRc);
   if (StrToInt(sortMultiRc) <> 0) then
   begin
      MyAbort('External ' + constBatFileSortMulti + ' command reported error.  cmdLine was "' + cmdLine + '".  Return code was ' + sortMultiRc + '!');
   end;
   
end; { end RunSortMulti() }


{***************************************************************************
 * function CreateSubDir()
 *  Ensures that a given subdirectory exists.  If not, create it.
 *  Return a list of directories created.
 *
 *  Note:  Previously this code would also attempt to restore a locally
 *  deleted directory by running svn update.  However, we now handle that
 *  operation by doing an svn revert of the directory tree long before we get here.
 *
 *  Note:  Assumes that string list newSubDirs has already been created.
 *
 *  Adds current subDir to var parm subDirs.
 *  If new subDir is created, it is added to var parm newSubDirs
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CreateSubDir(    scriptsPath : TDynamicString;
                          projectPath : TDynamicString;
                          subDir      : TString;
                      var newSubDirs  : TStringList;
                          )           : Integer;
var
   i            : Integer;
   rc           : Integer;
                  
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { See if it exists and is a directory. }
   if (DirectoryExists(subDir)) then
   begin
      WriteToDebugFile('SubDir ' + subDir + ' exists.  No worries.');
   end

   { Else it doesn't exist. }
   else
   begin
      WriteToDebugFile('*SubDir ' + subDir + ' does not exist!  Will create it.');

      { Attempt to create it. }
      rc := MkDir(subDir);
      
      { Verify success. }
      if (not DirectoryExists(subDir)) then
      begin
         MyAbort('Unable to create SubDir ' + subDir + '!');
      end; { endif }
      
      { Add this directory to a running list of created directories. }
      newSubDirs.Add(subDir);
      
      { Note that adding this directory to svn will be taken care of in a later step. }

   end; { endelse }

end; { end CreateSubDir() }


{***************************************************************************
 * function CreateOutputSubDir()
 *  Ensures that a given output subdirectory exists.  If not, create it.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CreateOutputSubDir(scriptsPath : TDynamicString;
                            projectPath : TDynamicString;
                            projOutPath : TString;
                            subDir      : TString;
                            )           : Integer;
var
   pathSubDir : TString;
   rc         : Integer;
   newSubDirs : TStringList;
                  
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Create full path and name of subdirectory. }
   pathSubDir := projOutPath + '\' + subDir;

   { Create useless list. }
   newSubDirs := TStringList.Create;
   
   { Call CreateSubDir() to do the actual work. }
   { Note that we won't use the var parm newSubDirs returned by this function. }
   CreateSubDir(scriptsPath,
                projectPath,
                pathSubDir,
                {var} newSubDirs);
   
   { Free useless list. }
   newSubDirs.Free;
   
end; { end CreateOutputSubDir() }


{***************************************************************************
 * function CreateAllOutputSubDirs()
 *  Ensures that all output subdirectories exist.  If any do not, create them.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CreateAllOutputSubDirs(scriptsPath     : TDynamicString;
                                projectPath     : TDynamicString;
                                projOutPath     : TString;
                                projOutSubDirs  : TStringList;
                                projOutIncludes : TStringList;
                                )               : Integer;
var
   rc     : Integer;
   i      : Integer;
   subDir : TDynamicString;
                  
begin

   { For now, assume/hope/pray that we will succeed. until we find out otherwise. }
   rc := 0;

   { Make sure that all output subdirectories of ProjectOutputs exist.  If not, create them. }
   for i := 0 to projOutSubDirs.Count - 1 do
   begin

      { Extract name of desired subdir. }
      subDir := projOutSubDirs.Strings[i];

      WriteToDebugFile('*About to verify existence of ProjectOutputs subdir ' + subDir);
      
      { Create this output subdir if needed. }
      rc := rc | CreateOutputSubDir(scriptsPath,
                                    projectPath,
                                    projOutPath,
                                    subDir);

   end; { endfor }
   
   { Make sure that all include subdirectories exist.  If not, create them. }
   for i := 0 to projOutIncludes.Count - 1 do
   begin

      { Extract name of desired subdir. }
      subDir := projOutIncludes.Strings[i];

      { Create this output subdir if needed. }
      if (subDir <> '') then
      begin
         WriteToDebugFile('*About to verify existence of ProjectOutputs include ' + subDir);
         rc := rc | CreateOutputSubDir(scriptsPath,
                                       projectPath,
                                       projOutPath,
                                       subDir);
      end;

   end; { endfor }
   
   { Return code to caller. }
   result := rc;
   
end; { end CreateAllOutputSubDirs() }


{***************************************************************************
 * function ExcludeFilesFromList()
 *  Process a list of files.  For each file, see if the filename or the fileext
 *  matches a list of excludes.  If so, remove it from the list.
 *
 *  Note:  Assumes that fileList stringlist has already been created.
 *
 *  Returns modified string list as var parm fileList.
 *  Returns:  0 on success, 1 if unable to delete one or more files
 ***************************************************************************}
function ExcludeFilesFromList(var fileList : TStringList;
                                  excludes : TStringList;
                                  )        : Integer;
var
   i              : Integer;
   fileName       : TDynamicString;
   fileExt        : TDynamicString;
   starDotFileExt : TDynamicString;
   deletions      : TStringList;
   idx            : Integer;
                  
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Create deletions list. }
   deletions := TStringList.Create;
   
   { Set fileList to operate case-insensitively. }
   fileList.CaseSensitive := False;

   { Loop over all the files. }
   for i := 0 to fileList.Count - 1 do
   begin

      { Extract just the filename for this file (eg. strip off leading path). }
      fileName := ExtractFileName(fileList.Strings[i]);
      
      { Extract just the extension for this file (eg. ".TXT", ".XLS", etc.). }
      fileExt := ExtractFileExt(fileList.Strings[i]);
      //                 ShowMessage('Extracted filename is ' + fileName + '.');

      { Extend the file extension to "star dot fileExt" (eg. "*.zip"). }
      starDotFileExt := '*' + fileExt;

      { Look for this filename in the excludes list.
       Look for this file extension (eg. ".zip") in the excludes list.
       Look for *.this file extension (eg. "*.zip") in the excludes list. }
      if ( (excludes.IndexOf(fileName) >= 0) or
          (excludes.IndexOf(fileExt) >= 0) or
          (excludes.IndexOf(starDotFileExt) >= 0) ) then
      begin
         WriteToDebugFile('*Found file ' + fileList.Strings[i] + ' that I will remove from the list.');

         { Record the index of the file we will delete from the list.
          We can't do this right now, since we would screw up the for loop end condition. }
         deletions.Add(IntToStr(i));

      end; { endif }

   end; { endfor i }

   { Loop over the list of files we wish to delete from the original list.
    Note that we must loop backwards so that we delete the later files first.
    This way, we don't change the indices of things that we have yet to delete. }
   for i := deletions.Count - 1 downto 0 do
   begin

      { Retrieve the index into fileList from deletions list. }
      idx := strToInt(deletions.Strings[i]);
      
      WriteToDebugFile('*Found file ' + fileList.Strings[idx] + ' that I am removing from the list.');

      { Do the deletion. }
      fileList.Delete(idx);

   end; {endfor i}

   { Free deletions list. }
   deletions.Free;

end; { end ExcludeFilesFromList() }


{***************************************************************************
 * function DeleteOutputFiles()
 *  Attempt to delete all files in a specified subdirectory of ProjectOutputs.
 *  Take a list of files to exclude from deletion.
 *
 *  Returns:  0 on success, 1 if unable to delete one or more files
 ***************************************************************************}
function DeleteOutputFiles(projOutPath : TString;
                           subDir      : TString;
                           excludes    : TStringList;
                           )           : Integer;
var
   generatedFiles : TStringList;
   i              : Integer;
                  
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

//   ShowMessage('Hello World from DeleteOutputFiles.  excludes.Count is ' + IntToStr(excludes.Count));

   { Initialize list of generated files. }
   generatedFiles := TStringList.Create;

   { Fetch a list of all files in this subdirectory. }
   MyFindFiles(projOutPath,
               subDir,
               '*.*',
               {var} generatedFiles);

   { Exclude specified files from this list. }
   ExcludeFilesFromList({var} generatedFiles,
                        excludes);
   
   { Loop over all the files. }
   for i := 0 to generatedFiles.Count - 1 do
   begin

      //      ShowMessage(generatedFiles.Strings[i]);

      { Attempt to delete this file. }
      DeleteFileWithVerify(generatedFiles.Strings[i]);
      
   end; { endfor }

   { Free list of generated files. }
   generatedFiles.Free;

end; { end DeleteOutputFiles() }


{***************************************************************************
 * function DeleteAllOutputFiles()
 *  Attempt to delete all files in all output subdirs of ProjectOutputs.
 *  Take a list of output subdirs and a list of files to exclude from
 *  deletion for each subdir.
 *
 *  Returns:  0 on success, 1 if unsuccessful.
 ***************************************************************************}
function DeleteAllOutputFiles(projOutPath    : TString;
                              projOutSubDirs : TStringList;
                              deleteExcludes : TStringList;
                              runOutJobs     : TStringList;
                              )              : Integer;
var
   rc       : Integer;
   i        : Integer;
   excludes : TStringList;
   subDir   : TDynamicString;
            
begin 

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

//   ShowMessage('Hello World from DeleteAllOutputFiles.');

   { Delete all old output files from all output subdirectories of ProjectOutputs. }
   for i := 0 to projOutSubDirs.Count - 1 do
   begin

      { Only delete files if we have been enabled to run this OutJob. }
      if (StrToBool(runOutJobs.Strings[i])) then
      begin

         { Extract name of desired subdir. }
         subDir := projOutSubDirs.Strings[i];

         { Extract list of excludes for this subdir. }
         excludes := TStringList.Create;
         SplitDelimitedStringIntoStringList(deleteExcludes.Strings[i],
                                            constStringDelimiter,
                                            {var} excludes);

         WriteToDebugFile('*About to delete old files from enabled ProjectOutputs subdir ' + subDir);
         
         { Delete files in this output subdir, subject to some exclusions. }
         rc := rc | DeleteOutputFiles(projOutPath, subDir, excludes);

         { Free list of excludes. }
         excludes.Free;

      end; { endif }

   end; { endfor }

   { Verify success. }
   if (rc = 0) then
   begin
      WriteToDebugFile('*Succeeded in deleting all old output files!');
   end
   
   else
   begin
      MyAbort('Unable to delete all output files!');
   end;

end; { end DeleteAllOutputFiles() }


{***************************************************************************
 * function CheckThatSvnScriptsWorkingCopyIsUpdated()
 *  Make sure that the working copy containing our scripts and libraries
 *  is up-to-date with respect to the svn server.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CheckThatSvnScriptsWorkingCopyIsUpdated(scriptsPath    : TDynamicString;
                                                 thisScriptName : TString;
                                                 )              : Integer;
var
   rc        : Integer;
   svnOut    : TStringList;
   i         : Integer;
   mustAbort : Boolean;
   parms     : TStringList;
   msg       : TDynamicString;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Clear msg string. }
   msg := 'Svn reports that these script and library files from global working copy are out-of-date ("*") or modified ("M") with respect to svn server:' + constLineBreak + constLineBreak ;

   { Initialize svnOut string list. }
   svnOut := TStringList.Create;

   { Initialize parms string list. }
   parms := TStringList.Create;

   { Tell svn which root directory to check. }
   parms.Add(constGlobalScriptsAndLibsWc);
   
   { Call IssueSvnCommandGetOutput() to do all the real work. }
   IssueSvnCommandGetOutput(scriptsPath,
                            constGlobalScriptsAndLibsWc,
                            constSvnCmdStatusWrtServer,
                            parms,
                            {var} svnOut);

   { Free parms list. }
   parms.Free;

   { Flag that we don't yet have reason to abort. }
   mustAbort := False;

   { Loop over all the output produced by svn.exe (files and directories that differ from svn server). }
   for i := 0 to (svnOut.Count - 1) do
   begin

      { Check for a special case of this script being modified (eg. during script development). }
      if ( (Copy(svnOut.Strings[i],1,1) = constSvnRepStatusModified) and
          (AnsiPos(thisScriptName, svnOut.Strings[i]) <> 0) and
         (not mustAbort) ) then
      begin

         { Issue warning modal dialog box with specified warning message,
          no reply after clicking Ok, and specified reply after clicking Cancel. }
         IssueWarningWithOkOrCancel('It appears that this script itself has been modified with respect to svn server.' + constLineBreak +
                                    'If you are actively developing this script, then click Ok.  Otherwise, click Cancel.' + constLineBreak + constLineBreak +
                                    'Output line from svn.exe was:' + constLineBreak +
                                    svnOut.Strings[i],
                                    '',
                                    'Aborting script at user request due to modifications to this script file.');
         
      end { endif }

      { If we're not otherwise going to abort, then we must ignore any line that
       begins "Status against revision:". }
      else if ( (Copy(svnOut.Strings[i],1,Length(constSvnRepStatusStatAgainstRev)) = constSvnRepStatusStatAgainstRev) and
               (not mustAbort) ) then
      begin

         { Do nothing. }
         
      end { endif }
      
      { Ignore lines that have a '?' char as the first char in a line of svn output.
       This indicates a file that exists locally, but not in the svn repo.  We ignore these. }
      else if (Copy(svnOut.Strings[i],1,1) <> constSvnRepStatusNotInSvn) then
      begin
         
         { Add this line of svn output to the abort message that we will display to screen. }
         msg := msg + svnOut.Strings[i] + constLineBreak;
         
         { Flag that we found differences between local working copy and svn server, and thus we will be aborting the script. }
         mustAbort := True;

      end; { endif }
         
   end; { endfor }

   { Free svnOut list. }
   svnOut.Free;

   { If we were flagged to abort, then proceed to do so. }
   if (mustAbort = True) then
   begin
      MyAbort(msg);
   end;
      
end; { end CheckThatSvnScriptsWorkingCopyIsUpdated() }


{***************************************************************************
 * function CheckForUnCheckedInFiles()
 *  Do an svn status command in project home to look for files
 *  that have not been checked into svn.
 *
 *  Returns:  0 on success, 1 if unsuccessful.
 ***************************************************************************}
function CheckForUnCheckedInFiles(scriptsPath : TDynamicString;
                                  projectPath : TString;
                                  filePaths   : TStringList;
                                  msg         : TDynamicString;
                                  )           : Integer;
var
   i         : Integer;
   rc        : Integer;
   parms     : TStringList;
   svnOut    : TStringList;
   mustAbort : Boolean;
          
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   //   ShowMessage('Hello World from CheckForUnCheckedInFiles.');

   { Initialize svnOut string list. }
   svnOut := TStringList.Create;

   { Do an svn status command and retrieve all its output. }
   rc := IssueSvnCommandGetOutput(scriptsPath,
                                  projectPath,
                                  constSvnCmdStatus,
                                  filePaths,
                                  {var} svnOut);

   { Flag that we don't yet have reason to abort. }
   mustAbort := False;

   { Loop over all the files and directories returned to us. }
   for i := 0 to svnOut.Count - 1 do
   begin

      //      ShowMessage(svnOut.Strings[i]);

      { Add this line of svn output to the abort message that we will display to screen. }
      msg := msg + svnOut.Strings[i] + constLineBreak;
      
      { Flag that we found missing output files and thus we will be aborting the script. }
      mustAbort := True;
      
   end; { endfor }

   { Free svnOut list. }
   svnOut.Free;

   { If we were flagged to abort, then proceed to do so. }
   if (mustAbort = True) then
   begin
      MyAbort(msg);
   end;

end; { end CheckForUnCheckedInFiles() }


{***************************************************************************
 * function CheckForUnCheckedInSourceFiles()
 *  Do an svn status command in project home to look for source files
 *  that have not been checked into svn.
 *
 *  Returns:  0 on success, 1 if unsuccessful.
 ***************************************************************************}
function CheckForUnCheckedInSourceFiles(scriptsPath     : TDynamicString;
                                        projectPath     : TString;
                                        sourceFilePaths : TStringList;
                                        )               : Integer;
var
   i         : Integer;
   rc        : Integer;
   parms     : TStringList;
   svnOut    : TStringList;
   msg       : TDynamicString;
   mustAbort : Boolean;
          
begin

   { Call CheckForUnCheckedInFiles() to do all the real work. }
   result := CheckForUnCheckedInFiles(scriptsPath,
                                      projectPath,
                                      sourceFilePaths,
                                      'Svn reports that these project source files have local modifications that have not been checked in.' + constLineBreak +
                                      'Please check in these files before re-running this script.' + constLineBreak + constLineBreak);

end; { end CheckForUnCheckedInSourceFiles() }


{***************************************************************************
 * function CheckForAllMissingOutputFiles()
 *  Do an svn status command in ProjectOutputs SubDirs to look for files
 *  that exist in svn repo but do not exist in working copy.
 *
 *  This situation occurs when an output file was previously generated and
 *  checked into svn, but then the user disabled that output in OutJob
 *  files before running this script.
 *
 *  Of course, it can also occur when a legitimate change (reducing number
 *  of PcbDoc layers, reducing number of variants, etc.) results in fewer
 *  output files being generated compared to last time this script was run.
 *  In this event, the user must manually svn delete the file(s) in question
 *  and re-run this release manager script.
 *
 *  Returns:  0 on success, 1 if unsuccessful.
 ***************************************************************************}
function CheckForAllMissingOutputFiles(scriptsPath    : TDynamicString;
                                       projOutPath    : TString;
                                       projOutSubDirs : TStringList;
                                       )              : Integer;
var
   i         : Integer;
   rc        : Integer;
   svnOut    : TStringList;
   msg       : TDynamicString;
   mustAbort : Boolean;
          
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   //   ShowMessage('Hello World from CheckForAllMissingOutputFiles.');

   { Initialize svnOut string list. }
   svnOut := TStringList.Create;

   {** Do an svn status in all ProjectOutputs SubDirs to look for files that are missing from working copy. **}
   { Do an svn status command and retrieve all its output. }
   rc := IssueSvnCommandGetOutput(scriptsPath,
                                  projOutPath,
                                  constSvnCmdStatusWrtServer,
                                  projOutSubDirs,
                                  {var} svnOut);

   { Initialize error message in case we need it. }
   mustAbort := False;
   msg := 'Svn reports that these files in ProjectOutputs subdirs previously existed and were checked in, but were not generated this time around.' + constLineBreak +
   'It is likely that these outputs got disabled in the various OutJob files before you ran this script.' + constLineBreak +
   'In this case, you want to re-enable these outputs in OutJob files and then re-run this script.' + constLineBreak + constLineBreak +
   'However, it is also possible that these are gerbers from PCB layers that no longer exist, BOMs from variants that no longer exist, etc.' + constLineBreak +
   'If this is the case, you must manually svn delete these missing files, and then re-run this script.' + constLineBreak + constLineBreak;

   { Loop over all the files and directories returned to us. }
   for i := 0 to svnOut.Count - 1 do
   begin

      { Look for a '!' char in the first char of this svn output line.
       This indicates a file that exists in the svn repo, but is missing from the working copy. }
      if (Copy(svnOut.Strings[i],1,1) = constSvnRepStatusMissing) then
      begin

         { Add this line of svn output to the abort message that we will display to screen. }
         msg := msg + svnOut.Strings[i] + constLineBreak;

         { Flag that we found missing output files and thus we will be aborting the script. }
         mustAbort := True;
         
      end; { endif }

   end; { endfor }

   { Free svnOut list. }
   svnOut.Free;

   { If we were flagged to abort, then proceed to do so. }
   if (mustAbort = True) then
   begin
      MyAbort(msg);
   end;

end; { end CheckForAllMissingOutputFiles() }


{***************************************************************************
 * function FindProjectBomFile()
 *  Find the Bom file associated with this project.
 *  Here we assume that there may be more than 1 BOM file.
 *  We further assume that all BOM files are created at the same time and
 *  checked in at the same time.  Therefore, just return the name of the
 *  arbitrary first one that we find.
 *
 *  NOTE:  This function is not currently being used and will need improvements
 *  in order to be put back in service!
 *  
 *  Returns full path to this project's Bom file in var parm bomPath.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function FindProjectBomFile(    projOutPath : TString;
                                subDir      : TString;
                            var bomPath     : TDynamicString; 
                                )           : Integer;
var
   bomFiles : TStringList;
   
begin 

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Init list of BOM files. }
   bomFiles := TStringList.Create;
   
   { Find all BOM .xls files in the purchasing/ subdirectory and add them to the assembly zipfile. }
   MyFindFiles(projOutPath,
               subDir,
               ('*' + constExtXls),
               {var} bomFiles);

   { See if we found at least one BOM file.  If so, record name of first BOM file. }
   if (bomFiles.Count > 0) then
   begin

      { Record name of this BOM file to return to caller. }
      bomPath := bomFiles.Strings[0];
      WriteToDebugFile('*Found BOM file with full path ' + bomPath);
   end

   { Else we failed sanity check. }
   else
   begin
      MyAbort('Found 0 BOM files in your project.  This number should have been at least 1!');
   end;

//   ShowMessage('About to leave FindProjectBomFile(), bomPath is ' + bomPath);

   { Free list of BOM files. }
   bomFiles.Free;
   
end; {end FindProjectBomFile() }


{***************************************************************************
 * function CheckForUnsavedSource()
 *  Iterate over all the source documents in the project.
 *  Make sure that they've all been saved to disk.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CheckForUnsavedSource(sourceFilePaths : TStringList;
                               )               : Integer;
var
   AView           : IServerDocumentView;
   AServerDocument : IServerDocument;
   i               : Integer;
   kind            : TDynamicString;
   sourceFilePath  : TDynamicString;
   msg             : TDynamicString;
   allClean        : Boolean;
   modified        : LongBool;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Assume all source files are clean until we find out otherwise. }
   allClean := True;

//   ShowMessage('In CheckForUnsavedSource, docPath is ' + docPath);

   { Sanity check that we can access the Client object. }
   if Client = Nil then
      MyAbort('Unable to get client view.');

   { Setup beginning of message to user. }
   msg := 'ERROR:  These source documents are unclean (need to be saved):' + constLineBreak + constLineBreak;

   { Loop over all source documents. }
   for i := 0 to sourceFilePaths.Count - 1 do
   begin

      { Get a reference to the current source file. }
      sourceFilePath := sourceFilePaths.Strings[i];

      { Retrieve server interface for this document from document's path. }
      AServerDocument := Client.GetDocumentByPath(sourceFilePath);

      { Retrieve document kind for this document. }
      kind := Client.GetDocumentKindFromDocumentPath(sourceFilePath);

      { Exclude certain files from this check.  They do not have the "Modified" bit. }
      { TODO:  We should probably invert this and check for the file types
       that actually do have the "Modified" bit. }
      if ( (kind <> constKindDbLib) and (kind <> constKindPcb) and
          (kind <> constKindOutJob) and (kind <> constKindHarness) and
          (kind <> constKindDbLink) and (kind <> constKindSchLib) and
          (kind <> constKindPcbLib) ) then 
      begin

         //          { Set  the  document  dirty. }
         //          AServerDocument.Modified := True;   

         WriteToDebugFile('*Checking cleanliness of file at path ' + sourceFilePath + ' of kind ' + kind);

         { Get cleanliness status of this document. }
         modified := AServerDocument.GetModified;
         
         //          WriteToDebugFile('*File at path ' + sourceFilePath + ' reports modified as ' + BoolToStr(modified));

         { Examine cleanliness of this document. }
         { It's not at all clear why "if (modified = True) then" doesn't work, but it doesn't.  JWC 2011/09/02. }
         if (BoolToStr(modified) <> '0') then
         begin

            WriteToDebugFile('Found unclean document ' + sourceFilePath + '!!');

            { Flag that we have at least one unclean source document. }
            allClean := False;

            { Add this source file to the list of files that are unclean. }
            msg := msg + sourceFilePath + constLineBreak;
            
         end; { endif }

      end { endif }

      { Else we were not able to check this file.  Note that. }
      else
      begin
         WriteToDebugFile('*UNABLE to check cleanliness of file at path ' + sourceFilePath + ' of kind ' + kind + '!');
      end;
      
      
   end; { endfor }

   { If we were flagged that we had unclean files, then abort script with error message detailing those files. }
   if (allClean = False) then
   begin
      MyAbort(msg);
   end;

   { Since we weren't able to check that all files are clean, we're going to do the next best thing and
    brutally close all files in the project.  This should force popup windows warning of unsaved changes
    (if any).  If saved, those changes will still fail the next test, checking for svn cleanliness. }
   { TODO:  We may still have issues if there are generated files open that Altium doesn't think are
    part of the project, but actually are.  Unfortunately, however, we cannot close All documents here. }
   ResetParameters;
   AddStringParameter('ObjectKind', 'FocusedProjectDocuments');
   RunProcess('WorkspaceManager:CloseObject');

//   { Attempt to close all documents. }
//   ResetParameters;
//   AddStringParameter('ObjectKind', 'WorkspaceDocuments');
//   RunProcess('WorkspaceManager:CloseObject');
   

   
   {   IOutputer interface
    Overview
    The IOutputer interface represents the one of the outputs of an output job within a design project.
    Interface Methods
Function DM_ViewName : WideString
Function DM_EditProperties : Boolean;
Function DM_Generate_OutputFilesTo (OutputDirectory : WideString; ParameterOverrides : PChar) : Boolean;
Function DM_Generate_OutputFiles (AGeneratedFilename : PChar) : Boolean;
Procedure DM_SetPrintScale (APrintScale : Double);
Procedure DM_SetPrintMode (AFitPrintToPage : Boolean);
Procedure DM_SetDocumentPath (ADocPath : WideString);
    See also
    Workspace Manager Interfaces
    IProject interface
    IOutputJob interface
    IWSM_OutputJobDocument interface}

end; {end CheckForUnsavedSource() }


{***************************************************************************
 * function GetSourceFilesAndFindTopLevelSchDoc()
 *  Iterate over all the source documents in the project.
 *  Return a list of the paths to all source documents.
 *
 *  While we're at it, also identify the top level schematic document.
 *  This will be used later to find the pcb version number.
 *
 *  Also while we're at it, identify the project file name.
 *  This will be used later to increment the pcba version number.
 *
 *  Returns source files in var parm sourceFilePaths.
 *  Returns top level schematic as var parm topLevelSchDoc.
 *  Returns project file name as var parm projFilePath.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function GetSourceFilesAndFindTopLevelSchDoc(    Project         : IProject;
                                             var sourceFilePaths : TStringList;
                                             var topLevelSchDoc  : IDocument;
                                             var projFilePath    : TDynamicString;
                                                 )               : Integer;
var
   Document      : IDocument;
   k             : Integer;
   docPath       : TDynamicString;
   foundTopSch   : Boolean;
                  
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Flag that we haven't yet found top level sch page. }
   foundTopSch := False;
   
   { Compile project before proceeding. }
   Project.DM_Compile;

   { Retrieve the name of the project file. }
   docPath := Project.DM_ProjectFullPath;
   WriteToDebugFile('*Found project file at path ' + docPath);
   projFilePath := docPath;

   { Add the path to this document to the list of sourceFilePaths. }
   sourceFilePaths.Add(docPath);

   { Loop over all logical documents in the project.... }
   for k := 0 to Project.DM_LogicalDocumentCount - 1 do
   begin

      { Get reference to this document. }
      Document := Project.DM_LogicalDocuments(k);

      { Get the path to this document and report this to debug file. }
      docPath := Document.DM_FullPath;
      WriteToDebugFile('*Found source document of kind ' + Document.DM_DocumentKind + ' at path ' + docPath);
      
      { Add the path to this document to the list of sourceFilePaths. }
      sourceFilePaths.Add(docPath);

      { See if this document is a schematic page and is the top level schematic in the project. }
      if ( (Document.DM_DocumentKind = constKindSch) and (Document.DM_IndentLevel = 1) ) then
      begin
         WriteToDebugFile('*Found SCH document with full path ' + docPath + ', indent level ' + IntToStr(Document.DM_IndentLevel));

         { Flag that we found top level schematic page. }
         foundTopSch := True;
         
         { Record that this is the top level schematic page.
          This info will be used later on to find the PCB version number from APCB1 component. }
         topLevelSchDoc := Document;

      end;

   end;

   { Make sure we found the top level schematic page. }
   if (foundTopSch = False) then
   begin
      MyAbort('Unable to find top level schematic page!');
   end;

end; {end GetSourceFilesAndFindTopLevelSchDoc() }


{***************************************************************************
 * function GetPcbAndPcbaVersions()
 *  Get the PCB (pcb fabrication) project properties.
 *  Get the PCBA (pcb assembly) project properties.
 *
 *  If the user wishes to package fabrication, then require presence of PCB parms.
 *  If the user wishes to package assembly, then require presence of PCB parms.
 *
 *  Returns modified project parms list as var parm projectParms.
 *  Returns PCB part number and version as string in var parm pcbPartNumAndVersion.
 *  Returns PCBA version as string in var parm pcbaVersion.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function GetPcbAndPcbaVersions(    Project              : IProject;
                                   projOutSubDirs       : TStringList;
                                   runPackager          : TStringList;
                               var projectParms         : TStringList;
                               var pcbPartNumAndVersion : TDynamicString; 
                               var pcbaVersion          : TDynamicString; 
                                   )                    : Integer;
var
   fabIndex  : Integer;
   assyIndex : Integer;
   ParmCount : Integer;             { The number of parameters}
   i         : Integer;             { An index for the current parameter}
   CurrParm  : IParameter;          { The current parameter}
   position  : Integer;
   name      : TDynamicString;
   value     : TDynamicString;
                  
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   {*** Get indices into primary string lists for fabrication and assembly packaging ops, for later use. ***}
   {* Get index of fabrication ProjectOutputs/ subdir. *}
   fabIndex := GetFabIndex(projOutSubDirs);
   
   {* Get index of assembly ProjectOutputs/ subdir. *}
   assyIndex := GetAssyIndex(projOutSubDirs);


   { Flag that we have not yet found what we're looking for. }
   pcbPartNumAndVersion := '';
   pcbaVersion := '';


   {* Examine project level parameters and try to find the 2 that we care about. *}
   { Extract all project parameters from the project file. }
   ParmCount := Project.DM_ParameterCount;

   WriteToDebugFile('*Found ' + IntToStr(ParmCount) + ' project parameters.');
   
   { Loop over all project parameters }
   for i := 0 to ParmCount - 1 do
   begin
      
      { Get current parameter. }
      CurrParm := Project.DM_Parameters(i);

      WriteToDebugFile('Examining project parameter index ' + IntToStr(i) + ' named "' + CurrParm.DM_Name + '", value is "' + CurrParm.DM_Value + '".');

      { Make sure there are not any delimiter ('=') characters in parm name or parm value. }
      if (AnsiPos(constStringEquals, CurrParm.DM_Name) <> 0) then
         MyAbort('Found illegal "=" char in project parameter named "' + CurrParm.DM_Name + '".');
      
      if (AnsiPos(constStringEquals, CurrParm.DM_Value) <> 0) then
         MyAbort('Found illegal "=" char in project parameter valued "' + CurrParm.DM_Value + '".');
      

      { Look for an existing project parameter with this same name. }
      position := projectParms.IndexOfName(CurrParm.DM_Name);

      { See if we're seeing a duplicate parameter, as can happen
       (a) when re-running this release manager script without closing and re-opening a project, or
       (b) in degenerate cases with screwed up project files (including those generated by really old versions of this script!). }
      if (position >= 0) then
      begin
         
         { Our current dispute resolution policy is to use the stringily "greater" value of the old value and the new value. }

         { Split each project parameter into name and value. }
         { Use our home brewed function, so that we don't split on spaces as well. }
         SplitStringIntoLeftAndRight(projectParms.Strings[position],
                                     constStringEquals,
                                     {var} name,
                                     {var} value);

         { Compare value of the current parameter to previous value of this parameter. }
         if (CurrParm.DM_Value > value) then
         begin

            { Overwrite the previous value with the new value. }
            projectParms.Strings[position] := (CurrParm.DM_Name + constStringEquals + CurrParm.DM_Value);
            WriteToDebugFile('WARNING:  Found duplicate project parameter named "' + CurrParm.DM_Name + '".  Old value was "' + value +
                             '", new value is "' + CurrParm.DM_Value + '".  Using new (greater)!!');

         end

         { Else the old value was greater or equal.  Leave it alone and keep using the old value. }
         else
         begin
         
            WriteToDebugFile('WARNING:  Found duplicate project parameter named "' + CurrParm.DM_Name + '".  Old value was "' + value +
                             '", new value is "' + CurrParm.DM_Value + '".  Keeping old value!!');

         end;
         
      end { endif }

      { Else we haven't seen this one before.  Add it to our list of project parameters. }
      else
      begin

         WriteToDebugFile('Found new project parameter named "' + CurrParm.DM_Name + '".  New value is "' + CurrParm.DM_Value + '".');

         { Store current parameter in our list of project level parameters. }
         projectParms.Add(CurrParm.DM_Name + constStringEquals + CurrParm.DM_Value);
      
      end;
      
   end; { endfor }

   
   {* Look for the PCB version number parameter. *}
   position := projectParms.IndexOfName(constPcbVersionParm);
      
   { Make sure we found the project level parameter we're looking for. }
   if (position >= 0) then
   begin

      { Make sure there are no space characters in parameter value. }
      if (AnsiPos(' ', projectParms.Strings(position)) <> 0) then
         MyAbort('Found illegal " " char in project parameter "' + projectParms.Strings(position) + '".');

      { Extract value of this parameter. }
      pcbPartNumAndVersion := projectParms.ValueFromIndex(position);
      WriteToDebugFile('*Found pcbPartNumAndVersion as ' + pcbPartNumAndVersion);

   end

   { Else it was not found. }
   else
   begin
   
      { Determine if this is a fatal error or just a warning. }
      if (StrToBool(runPackager.Strings[fabIndex])) then
         MyAbort('Unable to find (new) required project level parameter named ' + constPcbVersionParm)
         
      else
         { Issue warning modal dialog box with specified warning message,
          no reply after clicking Ok, and specified reply after clicking Cancel. }
         IssueWarningWithOkOrCancel('Unable to find (new) required project level parameter named ' + constPcbVersionParm + '.' + constLineBreak +
                                    'You need to fix this prior to packaging a fabrication zipfile, but I will proceed to generate OutJob outputs if you click OK.',
                                    '',
                                    'Aborting script at user request due to missing project level parameter ' + constPcbVersionParm + '.');

   end; { endelse }
   
   
   {* Look for the PCBA version number parameter. *}
   position := projectParms.IndexOfName(constPcbaVersionParm);
      
   { Make sure we found the project level parameter we're looking for. }
   if (position >= 0) then
   begin

      { Make sure there are no space characters in parameter value. }
      if (AnsiPos(' ', projectParms.Strings(position)) <> 0) then
         MyAbort('Found illegal " " char in project parameter "' + projectParms.Strings(position) + '".');

      { Extract value of this parameter. }
      pcbaVersion := projectParms.ValueFromIndex(position);
      WriteToDebugFile('*Found pcbaVersion as ' + pcbaVersion);

   end

   { Else it was not found. }
   else
   begin
   
      { Determine if this is a fatal error or just a warning. }
      if (StrToBool(runPackager.Strings[assyIndex])) then
         MyAbort('Unable to find (new) required project level parameter named ' + constPcbaVersionParm)

      else
         { Issue warning modal dialog box with specified warning message,
          no reply after clicking Ok, and specified reply after clicking Cancel. }
         IssueWarningWithOkOrCancel('Unable to find (new) required project level parameter named ' + constPcbaVersionParm + '.' + constLineBreak +
                                    'You need to fix this prior to packaging an assembly zipfile, but I will proceed to generate OutJob outputs if you click OK.',
                                    '',
                                    'Aborting script at user request due to missing project level parameter ' + constPcbaVersionParm + '.');

   end; { endelse }

end; { end GetPcbAndPcbaVersions() }


{***************************************************************************
 * function SplitPcbPartNumAndVersion()
 *  Split a string that contains both the PCB part number (eg. "MICROCAL-MAIN")
 *  and the PCB version number (eg. "1.13") into its component parts.
 *
 *  Returns PCB part number in var parm pcbPartNum.
 *  Returns PCB version number in var parm pcbVersion.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function SplitPcbPartNumAndVersion(    pcbPartNumAndVersion : TDynamicString;
                                   var pcbPartNum           : TDynamicString;
                                   var pcbVersion           : TDynamicString; 
                                   )                        : Integer;

var
   position  : Integer;
   position2 : Integer;
   foo       : TDynamicString;
   foo2      : TDynamicString;
   
begin
   
   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   {** Extract PCB part number portion of pcbPartNumAndVersion. **}
   { Make sure there are not more than 2 '-' chars in VALUE field. }
//   ShowMessage('pcbPartNumAndVersion is "' + pcbPartNumAndVersion + '"');
   position := AnsiPos('-', pcbPartNumAndVersion);
   if (position <> 0) then
   begin

      { Copy the part of pcbPartNumAndVersion after '-' to temp string foo. }
      foo := Copy(pcbPartNumAndVersion, (position+1), MaxInt);
//    ShowMessage('foo is "' + foo + '"');

      { Find the position of the 2nd '-' char. }
      position2 := AnsiPos('-', foo);

      { Make sure we found that second '-' char in foo. }
      if (position2 = 0) then
         MyAbort('In GetPcbVersionNumberFromTopLevelSchDoc(), fewer than 2 "-" chars in APCB1 VALUE field.  This is not supported by the current version of this script.  Please speak to Jeff.');

      { Copy the part of pcbPartNumAndVersion after '-' to temp string foo2. }
      foo2 := Copy(foo, (position2+1), MaxInt);
//    ShowMessage('foo2 is "' + foo2 + '"');

      { Make sure there are no more '-' chars in foo2. }
      if (AnsiPos('-', foo2) <> 0) then
         MyAbort('In GetPcbVersionNumberFromTopLevelSchDoc(), found more than 2 "-" chars in APCB1 VALUE field.  This is not supported by the current version of this script.  Please speak to Jeff.');

   end; { endif }
   
   { Extract from the beginning until the character before the 2nd '-' char. }
   pcbPartNum := Copy(pcbPartNumAndVersion, 1, (position+(position2-1)));
//   ShowMessage('pcbPartNum is "' + pcbPartNum + '"');

   {** Extract PCB version number portion of pcbPartNumAndVersion. **}
   pcbVersion := Copy(pcbPartNumAndVersion, (position+(position2+1)), MaxInt);

end; { end SplitPcbPartNumAndVersion() }


{***************************************************************************
 * function GetPcbVersionNumbersFromTopLevelSchDoc()
 *  
 *  In top level schematic document, find the schematic component with refdes "APCB1".
 *  Also find the Gerber Title Block component that will have some refdes "XX*".
 *  
 *  APCB1 will have a MFGNUMBER parameter that looks something like
 *  "PCB-MICROCAL_MAIN-1.13 Build Rev 19894".
 *  Return everything up to the last char before the first space (eg. "PCB-MICROCAL_MAIN-1.13") as var parm schApcb1MnoPcbPartNum.
 *  Return everyting after the last space char (eg. "19894") as var parm schApcb1MnoPcbRevNum.
 *
 *  APCB1 will have a VALUE parameter that looks something like
 *  "PCB-MICROCAL_MAIN-1.13".
 *  Return this as var parm schApcb1ValPcbPartNum.
 *
 *  APCB1 will have a Comment parameter that looks something like
 *  "PCB-MICROCAL_MAIN-1.13" or "=VALUE".
 *  Return this as var parm schApcb1ComPcbPartNum.
 *
 *  GerberTitleBlock component (XX something) with have a Comment parameter that looks something like
 *  "PCB-MICROCAL_MAIN-1.13".
 *  Return this as var parm schGtbPcbPartNum.
 *
 *  NOTE:  This entire function is highly specific to the required design
 *  elements at XIA LLC.  It would need to be generalized or modified to
 *  support another company's requirements!
 *  
 *  Returns : 0 on success, 1 if not successful.
 ***************************************************************************}
function GetPcbVersionNumbersFromTopLevelSchDoc(    topLevelSchDoc                  : IDocument,
                                                var schApcb1MnoPcbPartNumAndVersion : TDynamicString;   { PCB part number and version as reported by SCH component APCB1, MFGNUMBER field. }
                                                var schApcb1ValPcbPartNumAndVersion : TDynamicString;   { PCB part number and version as reported by SCH component APCB1, VALUE field. }
                                                var schApcb1ComPcbPartNumAndVersion : TDynamicString;   { PCB part number and version as reported by SCH component APCB1, Comment field. }
                                                var schApcb1MnoPcbRevNum            : TDynamicString;   { PcbDoc file svn rev number as reported by SCH component APCB1, MFGNUMBER field. }
                                                var schGtbPcbPartNumAndVersion      : TDynamicString;   { PCB part number and version as reported by SCH component gerber title block, Comment field. }
                                                var gtbRefDes                       : TDynamicString;
                                               )                                    : Integer;
var
   CurrentSch     : ISch_Sheet;
   iterator       : ISch_Iterator;
   PIterator      : ISch_Iterator;
   AComponent     : ISch_Component;
   i              : Integer;
   Parameter      : ISch_Parameter;
   apcb1MfgNumber : TDynamicString;
   position       : Integer;
   foundGtbComp   : Boolean;
   
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

//   ShowMessage('Hello world from GetPcbVersionNumbersFromTopLevelSchDoc(), sch full path is ' + topLevelSchDoc.DM_FullPath);

   { Clear the contents of various strings we haven't found yet. }
   apcb1MfgNumber                  := '';
   schApcb1MnoPcbPartNumAndVersion := '';
   schApcb1ValPcbPartNumAndVersion := '';
   schApcb1ComPcbPartNumAndVersion := '';
   schApcb1MnoPcbRevNum            := '';
   schGtbPcbPartNumAndVersion      := '';
   gtbRefDes                       := '';

   { Sanity check. }
   if SchServer = nil then
      MyAbort('In GetPcbVersionNumbersFromTopLevelSchDoc(), could not find SchServer!');

   { Initialize the robots in Schematic editor. }
   SchServer.ProcessControl.PreProcess(topLevelSchDoc, '');

//   ShowMessage('Full path to top level schdoc is "' + topLevelSchDoc.DM_FullPath + '"');

   { Manually re-open top level schdoc file, since now we're making sure to close all project
    documents before we get to this point. }
   ResetParameters;
   AddStringParameter('ObjectKind', 'Document');
   AddStringParameter('FileName', topLevelSchDoc.DM_FullPath);
   RunProcess('WorkspaceManager:OpenObject');
   
   { Open schematic document. }
   CurrentSch := SchServer.GetSchDocumentByPath(topLevelSchDoc.DM_FullPath);

   { Sanity check. }
   if CurrentSch = nil then
      MyAbort('In GetPcbVersionNumbersFromTopLevelSchDoc(), could not find CurrentSch!');

   { Look for components only }
   iterator := CurrentSch.SchIterator_Create;
   iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

   { Get a reference to the first object in this schematic page. }
   AComponent := iterator.FirstSchObject;

   { Get a reference to first object in this schematic page. }
   AComponent := iterator.FirstSchObject;

   { Loop over all objects in this schematic page. }
   while (AComponent <> nil) do
   begin
      
      { See if we found "APCB1". }
      if (AComponent.Designator.Text = constApcb1RefDes) then
      begin

         { Extract Comment field from this component. }
         schApcb1ComPcbPartNumAndVersion := AComponent.Comment.Text;
         
         { Setup to iterate over all the parameters in this component. }
         PIterator := AComponent.SchIterator_Create;
         PIterator.AddFilter_ObjectSet(MkSet(eParameter));

         { Retrieve reference to first parameter. }
         Parameter := PIterator.FirstSchObject;
         
         { Loop over all parameters currently attached to this component. }
         while (Parameter <> nil) do
         begin

            { Look for parameter named "MFGNUMBER". }
            if (Parameter.Name = constDbParmMfgNumber) then
            begin

               { Record this text to return it to caller. }
               apcb1MfgNumber := Parameter.Text;

               WriteToDebugFile('*Found APCB1, MFGNUMBER is ' + apcb1MfgNumber);
            end; { endif }

            { Look for parameter named "VALUE". }
            if (Parameter.Name = constDbParmValue) then
            begin

               { Record this text to return it to caller. }
               schApcb1ValPcbPartNumAndVersion := Parameter.Text;

               WriteToDebugFile('*Found APCB1, VALUE is ' + schApcb1ValPcbPartNumAndVersion);
            end; { endif }

            { Advance to next parameter in this component. }
            Parameter := PIterator.NextSchObject;

         end; { endwhile }

         { Free parameter iterator. }
         AComponent.SchIterator_Destroy(PIterator);
         
      end; { endif }

      { See if we found an "XX*" component that could be our gerber title block component. }
      if (Copy(AComponent.Designator.Text,1,2) = 'XX') then
      begin
         
         WriteToDebugFile('*Found XX* component ' + AComponent.Designator.Text);         

         { Setup to iterate over all the parameters in this component. }
         PIterator := AComponent.SchIterator_Create;
         PIterator.AddFilter_ObjectSet(MkSet(eParameter));

         { Retrieve reference to first parameter. }
         Parameter := PIterator.FirstSchObject;
         
         { Loop over all parameters currently attached to this component. }
         while (Parameter <> nil) do
         begin

            //          ShowMessage(Parameter.Name + constStringEquals + Parameter.Text);
            
            { Look for parameter named "CATEGORY". }
            if (Parameter.Name = 'CATEGORY') then
            begin

               { See if we can now prove that this component is the gerber title block component. }
               if (Parameter.Text = constGtbCategoryValue) then
               begin

                  WriteToDebugFile('*Found Gerber Title Block, CATEGORY is ' + Parameter.Text);

                  { Extract Comment field from this component. }
                  schGtbPcbPartNumAndVersion := AComponent.Comment.Text;

                  { Record the refdes of this Gerber Title Block component in the top level sch file. }
                  gtbRefDes := AComponent.Designator.Text;

               end; { endif }
               
            end; { endif }
            
            { Advance to next parameter in this component. }
            Parameter := PIterator.NextSchObject;

         end; { endwhile }

         { Free parameter iterator. }
         AComponent.SchIterator_Destroy(PIterator);
   
      end; { endif found XX* component. }
      
      { Advance to next component in this schematic page. }
      AComponent := iterator.NextSchObject;

   end;  { endwhile loop over all sch components in this schematic page. }

   { Free sch iterator. }
   CurrentSch.SchIterator_Destroy(iterator);
   
   { Clean up the robots in Schematic editor. }
   SchServer.ProcessControl.PostProcess(topLevelSchDoc, '');
   
   { Refresh the screen. }
   CurrentSch.GraphicallyInvalidate;

   { Manually close top level schdoc file, since we're done with it. }
   ResetParameters;
   AddStringParameter('ObjectKind', 'Document');
   AddStringParameter('FileName', topLevelSchDoc.DM_FullPath);
   RunProcess('WorkspaceManager:CloseObject');
      
   { See if we failed. }
   if (apcb1MfgNumber = '') then
      MyAbort('Unable to find refdes APCB1, parameter MFGNUMBER in top level schematic!');

   { See if we failed. }
   if (schApcb1ValPcbPartNumAndVersion = '') then
      MyAbort('Unable to find refdes APCB1, parameter VALUE in top level schematic!');
   
   { See if we failed to find Gerber Title Block component at all.
    We'll check it's Comment later. }
   if (gtbRefDes = '') then
      MyAbort('Unable to find Gerber Title Block component (refdes XX*) in top level schematic!');

   
   {** Extract PCB part number (aka pcb version number) from APCB1 MFGNUMBER field **}
   {  APCB1 will have a MFGNUMBER parameter that looks something like
    "PCB-MICROCAL_MAIN-1.13 Build Rev 19894".
    We want to extract the "PCB-MICROCAL_MAIN-1.13" from this example.
    Return everything up to the last char before the first space var parm schApcb1MnoPcbPartNumAndVersion. }
   
   { Find the position of the first ' ' character. }
   position := AnsiPos(' ', apcb1MfgNumber);

   { Copy everything to the left of that first space as schApcb1MnoPcbPartNumAndVersion. }
   schApcb1MnoPcbPartNumAndVersion := Copy(apcb1MfgNumber, 1, (position-1));

   {** Extract svn rev of PcbDoc file from APCB1 MFGNUMBER field **}
   {  APCB1 will have a MFGNUMBER parameter that looks something like
    "PCB-MICROCAL_MAIN-1.13 Build Rev 19894".
    We want to extract the "19894" from this example.
    Return everyting after the last space char as var parm schApcb1MnoPcbRevNum. }

   { Find the position of the last ' ' character. }
   position := LastDelimiter(' ', apcb1MfgNumber);

   { Copy everything to the right of that last space as schApcb1MnoPcbRevNum. }
   schApcb1MnoPcbRevNum := Copy(apcb1MfgNumber, (position+1), MaxInt);

   {** Make sure that the schApcb1ValPcbPartNumAndVersion contains the string "PCB-". **}
   if (AnsiPos(constPartNumPrefixPcb, schApcb1ValPcbPartNumAndVersion) = 0) then
      MyAbort('In GetPcbVersionNumbersFromTopLevelSchDoc(), could not find required substring "PCB-" in APCB1 VALUE field.');
   
end; {end GetPcbVersionNumbersFromTopLevelSchDoc() }


{***************************************************************************
 * function FindProjectPcbDocFile()
 *  Find the PcbDoc file associated with this project.
 *  Panic if we find any number not equal to 1 (eg 0 or 2).
 *  
 *  Returns full path to this project's PcbDoc file in var parm pcbDocPath.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function FindProjectPcbDocFile(    Project               : IProject;
                                   flagRequirePcbDocFile : Boolean;
                               var pcbDocPath            : TDynamicString; 
                               )                         : Integer;
var
   Document   : IDocument;
   k          : Integer;
   numPcbDocs : Integer;
                  
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Flag that we haven't yet found the PcbDoc file. }
   pcbDocPath := '';
   
   { Init number of PcbDoc files found to be 0. }
   numPcbDocs := 0;
   
   {*** Find name of this project's .PcbDoc file. ***}
   { Loop over all logical documents in the project.... }
   for k := 0 to Project.DM_LogicalDocumentCount - 1 do
   begin
      Document := Project.DM_LogicalDocuments(k);
      
      { See if this document is a PcbDoc file. }
      if (Document.DM_DocumentKind = constKindPcb) then
      begin

         { Increment number of PcbDoc files that we've found. }
         numPcbDocs := numPcbDocs + 1;

         { Record name of this PcbDoc file to return to caller. }
         pcbDocPath := Document.DM_FullPath;
//       ShowMessage('Found PCB document with full path ' + pcbDocPath);
      end;

   end; { endfor loop over all logical documents in project }


   { Make sure there is at least one PcbDoc file. }
   if (numPcbDocs < 1) then
   begin

      { See if the user has requested operations that require a PcbDoc file. }
      if (flagRequirePcbDocFile) then
      begin
         MyAbort('Found ' + IntToStr(numPcbDocs) + ' PcbDoc files in your project.  This number should have been exactly 1!');
      end

      { Else just issue a warning. }
      else
      begin
         { Issue warning modal dialog box with specified warning message,
          no reply after clicking Ok, and specified reply after clicking Cancel. }
         IssueWarningWithOkOrCancel('Unable to find a PcbDoc file within this project.' + constLineBreak +
                                    'However, since you have not requested operations that require a PcbDoc file, I will proceed to generate other OutJob outputs if you click OK.',
                                    '',
                                    'Aborting script at user request due to missing PcbDoc file ' + constPcbVersionParm + '.');
      end; { endelse }
      
   end; { endif }

   { Make sure there is no more than 1 PcbDoc file. }
   if (numPcbDocs > 1) then
   begin
      MyAbort('Found ' + IntToStr(numPcbDocs) + ' PcbDoc files in your project.  This script currently only supports having 1 PcbDoc file per project!');
   end;

//   ShowMessage('About to leave FindProjectPcbDocFile(), pcbDocPath is ' + pcbDocPath);
   
end; {end FindProjectPcbDocFile() }


{***************************************************************************
 * function GetPcbVersionNumbersFromPcbDoc()
 *  
 *  Find the PCB component with refdes "APCB1".
 *  
 *  APCB1 will have a Comment parameter that looks something like
 *  "PCB-MICROCAL_MAIN-1.13" or "=VALUE".
 *  Return this as var parm pcbApcb1PcbPartNumAndVersion.
 *
 *  GerberTitleBlock component (XX something) with have a Comment parameter that looks something like
 *  "PCB-MICROCAL_MAIN-1.13".
 *  Return this as var parm pcbGtbPcbPartNumAndVersion.
 *
 *  NOTE:  This entire function is highly specific to the required design
 *  elements at XIA LLC.  It would need to be generalized or modified to
 *  support another company's requirements!
 *  
 *  Returns : 0 on success, 1 if not successful.
 ***************************************************************************}
function GetPcbVersionNumbersFromPcbDoc(    pcbDocPath                   : TDynamicString;
                                            gtbRefDes                    : TDynamicString;
                                        var pcbApcb1PcbPartNumAndVersion : TDynamicString;  { PCB part number and version as reported by PCB component APCB1. }
                                        var pcbGtbPcbPartNumAndVersion   : TDynamicString;  { PCB part number and version as reported by PCB component gerber title block. }
                                        var step                         : Integer;
                                            )                            : Integer;
var
   board      : IPCB_Board;
   iterator   : IPCB_BoardIterator;
   component  : IPCB_Component;
   
   
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

//   ShowMessage('Hello world from GetPcbVersionNumbersFromPcbDoc(), pcbDocPath is ' + pcbDocPath + '.');

   { Clear the contents of various strings we haven't found yet. }
   pcbApcb1PcbPartNumAndVersion   := '';
   pcbGtbPcbPartNumAndVersion     := '';

   { See if we have a valid PcbDoc file. }
   if (pcbDocPath <> '') then
   begin
   
      UpdateGuiStatusMessage('Status:  Starting step 0-' + IntToStr(step) + ':  Extracting PCB version numbers from PcbDoc file.');

      { Attempt to start PCB server. }
      Client.StartServer(constKindPcb);

      { Check if PCB server is alive. }
      if (PCBServer = Nil) then
         MyAbort('PCBServer is Nil.  D''oh.');
      
      { Initialize the PCB editor. }
      PCBServer.PreProcess;

      { I cannot get PCBServer.GetPCBBoardByPath(pcbDocPath) to work, so I'm going
       to manually open the PCB document and then use PCBServer.GetCurrentPCBBoard. }
      ResetParameters;
      AddStringParameter('ObjectKind', 'Document');
      AddStringParameter('FileName', pcbDocPath);
      RunProcess('WorkspaceManager:OpenObject');
      
      { Attempt to open the project's PcbDoc file. }
      { FIXME:  Why does this not work?? GetPCBBoardByPath(pcbDocPath); }
      board := PCBServer.GetCurrentPCBBoard;

      { Sanity check }
      if (board = Nil) then
         MyAbort('Unable to open PcbDoc file ' + pcbDocPath);

      { Setup an iterator so that we can iterate over all PCB components. }
      iterator        := Board.BoardIterator_Create;
      iterator.AddFilter_ObjectSet(MkSet(eComponentObject));
      iterator.AddFilter_LayerSet(AllLayers);
      iterator.AddFilter_Method(eProcessAll);

      { Get a reference to the first PCB object. }
      component := iterator.FirstPCBObject;

      { Panic if we can't find any components. }
      if (component = Nil) then
      begin
         board.BoardIterator_Destroy(iterator);
         MyAbort('Unable to find any PCB components!');
      end;

      { Loop over all objects in this PcbDoc file. }
      while (component <> nil) do
      begin
         
         WriteToDebugFile('*Found component ' + component.SourceDesignator);

         { See if we found "APCB1". }
         if (component.SourceDesignator = constApcb1RefDes) then
         begin

            { Record the comment text from APCB1. }
            pcbApcb1PcbPartNumAndVersion := component.Comment.Text;
            
            WriteToDebugFile('*APCB1 comment is ' + pcbApcb1PcbPartNumAndVersion);
            
         end; { endif }
         
         { See if we found GTB (gerber title block) component. }
         if (component.SourceDesignator = gtbRefDes) then
         begin

            { Record the comment text from GTB. }
            pcbGtbPcbPartNumAndVersion := component.Comment.Text;
            
            WriteToDebugFile('*GTB comment is ' + pcbGtbPcbPartNumAndVersion);
            
         end; { endif }

         { Advance to next component in this PcbDoc file. }
         component := iterator.NextPCBObject;

      end;  { endwhile loop over all PCB components in this PcbDoc file. }

      { Free PCB component iterator. }
      board.BoardIterator_Destroy(iterator);

      { Close the PCB document. }
      ResetParameters;
      AddStringParameter('ObjectKind', 'Document');
      AddStringParameter('FileName', pcbDocPath);
      RunProcess('WorkspaceManager:CloseObject');

      
      WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Extracted PCB version numbers from PcbDoc file.');

      { Report various version numbers that we know have to debug file. }
      WriteToDebugFile('* pcbApcb1PcbPartNumAndVersion is    "' + pcbApcb1PcbPartNumAndVersion + '"');
      WriteToDebugFile('* pcbGtbPcbPartNumAndVersion is      "' + pcbGtbPcbPartNumAndVersion + '"');
   
   end { endif }

   { Else we don't have a PcbDoc file.  So don't actually do anything. }
   else
   begin

      WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  SKIPPED Extracting PCB version numbers from PcbDoc file.');
   end; { endelse }
   
end; {end GetPcbVersionNumbersFromPcbDoc() }


{***************************************************************************
 * function GetPcbDocFileSvnRevNum()
 *  Get the latest svn rev number for the PcbDoc file.
 *
 *  Returns svn rev number (in string form) in var parm pcbDocRevNum.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function GetPcbDocFileSvnRevNum(    scriptsPath  : TDynamicString;
                                    projectPath  : TDynamicString;
                                    pcbDocPath   : TDynamicString;
                                var pcbDocRevNum : TDynamicString;
                                var step         : Integer;
                                    )            : Integer;
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

//   ShowMessage('Hello world from GetPcbVersionNumbersFromPcbDoc(), pcbDocPath is ' + pcbDocPath + '.');

   { Clear the contents of various strings we haven't found yet. }
   pcbDocRevNum := '';

   { See if we have a valid PcbDoc file. }
   if (pcbDocPath <> '') then
   begin
   
      UpdateGuiStatusMessage('Status:  Starting step 0-' + IntToStr(step) + ':  Extracting PcbDoc file svn rev number.');

      { Call GetFileSvnRevNum() to do all the real work. }
      GetFileSvnRevNum(scriptsPath,
                       projectPath,
                       pcbDocPath,
                       {var} pcbDocRevNum);
      
      WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Extracted PcbDoc file svn rev number.');
      WriteToDebugFile('*PcbDoc file rev number is "' + pcbDocRevNum + '"');
   end { endif }

   { Else we don't have a PcbDoc file.  So don't actually do anything. }
   else
   begin
      WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  SKIPPED Extracting PcbDoc file svn rev number.');
   end; { endelse }

end; { end GetPcbDocFileSvnRevNum() }


{***************************************************************************
 * function GetPcbAndPcbaPartNumsVersionsAndRevs()
 *  Get pcbPartNum and pcbVersion information from several sources:
 *  (a) Project level properties
 *  (b) Top Level schematic document
 *  (c) PcbDoc file
 *  and make sure that these all agree.
 *
 *  Get pcbaPartNum and pcbaVersion information from project level properties.
 *
 *  NOTE:  This entire function is highly specific to the required design
 *  elements at XIA LLC.  It would need to be generalized or modified to
 *  support another company's requirements!
 *  
 *  TODO:  Handle future use case where we are doing only release and tag,
 *  and not packaging files this time around. (??)
 *
 *  Returns list of all project level paramters as var parm projectParms.
 *  Returns pcb part number as var parm pcbPartNum.
 *  Returns pcb version number as var parm pcbVersion.
 *  Returns svn rev number of PcbDoc file as var parm pcbDocRevNum.
 *  Returns pcba part number as var parm pcbaPartNum.
 *  Returns pcba version number of project as var parm pcbaVersion.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function GetPcbAndPcbaPartNumsVersionsAndRevs(    Project               : IProject;
                                                  projectName           : TDynamicString;
                                                  scriptsPath           : TDynamicString;
                                                  projectPath           : TDynamicString;
                                                  projOutPath           : TDynamicString;
                                                  projOutSubDirs        : TStringList;
                                                  zipFileNames          : TStringList;
                                                  flagRequirePcbDocFile : Boolean;
                                                  runPackager           : TStringList;
                                                  topLevelSchDoc        : IDocument;
                                              var projectParms          : TStringList;
                                              var pcbPartNum            : TDynamicString;
                                              var pcbVersion            : TDynamicString;
                                              var pcbDocRevNum          : TDynamicString;
                                              var pcbaPartNum           : TDynamicString;
                                              var pcbaVersion           : TDynamicString;
                                              var step                  : Integer;
                                                  )                     : Integer;
var
   rc                              : Integer;
   pcbDocPath                      : TDynamicString;
   bomPath                         : TDynamicString;
   pcbPartNumAndVersion            : TDynamicString;
   schApcb1MnoPcbPartNumAndVersion : TDynamicString;    { PCB part number and version as reported by SCH component APCB1, MFGNUMBER field. }
   schApcb1ValPcbPartNumAndVersion : TDynamicString;    { PCB part number and version as reported by SCH component APCB1, VALUE field. }
   schApcb1ComPcbPartNumAndVersion : TDynamicString;    { PCB part number and version as reported by SCH component APCB1, Comment field. }
   schApcb1MnoPcbRevNum            : TDynamicString;    { PcbDoc file svn rev number as reported by SCH component APCB1, MFGNUMBER field. }
   schGtbPcbPartNumAndVersion      : TDynamicString;    { PCB part number and version as reported by SCH component gerber title block. }
   gtbRefDes                       : TDynamicString;
   pcbApcb1PcbPartNumAndVersion    : TDynamicString;    { PCB part number and version as reported by PCB component APCB1. }
   pcbGtbPcbPartNumAndVersion      : TDynamicString;    { PCB part number and version as reported by PCB component gerber title block. }
   schApcb1MnoPcbPartNum           : TDynamicString;
   schApcb1MnoPcbVersion           : TDynamicString;
   zipFileName                     : TDynamicString;
   filePath                        : TDynamicString;
   i                               : Integer;
   fabIndex                        : Integer;
   assyIndex                       : Integer;
   bomRevNum                       : TDynamicString;    { Placeholder only.  We don't know this information yet. }

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

//   ShowMessage('Hello world from GetPcbAndPcbaPartNumsVersionsAndRevs()');

   {*** Initialize all version and rev number variables to be our super duper "uninitialized variable" marker. ***}
   pcbPartNum := constSubUninitMarker;
   pcbVersion := constSubUninitMarker;
   pcbDocRevNum := constSubUninitMarker;
   pcbaPartNum := constSubUninitMarker;
   pcbaVersion := constSubUninitMarker;
   bomRevNum := constSubUninitMarker;
   
   
   {*** Get indices into primary string lists for fabrication and assembly packaging ops, for later use. ***}
   
   {* Get index of fabrication ProjectOutputs/ subdir. *}
   fabIndex := GetFabIndex(projOutSubDirs);
   
   {* Get index of assembly ProjectOutputs/ subdir. *}
   assyIndex := GetAssyIndex(projOutSubDirs);

   
   {*** Get PCB & PCBA version numbers, extracted from project level parameter. ***}
   UpdateGuiStatusMessage('Status:  Starting step 0-' + IntToStr(step) + ':  Extracting PCB and PCBA version numbers from project properties.');
   GetPcbAndPcbaVersions(Project,
                         projOutSubDirs,
                         runPackager,
                         {var} projectParms,
                         {var} pcbPartNumAndVersion,
                         {var} pcbaVersion);
   WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Extracted PCB and PCBA version numbers from project properties.');

   { Split combined pcbPartNumAndVersion into separate pcbPartNum and pcbVersion. }
   SplitPcbPartNumAndVersion(pcbPartNumAndVersion,
                             {var} pcbPartNum,
                             {var} pcbVersion);

   { Replace "PCB-" with "PCBA-" to convert pcbPartNum into pcbaPartNum. }
   pcbaPartNum := StringReplace(pcbPartNum, constPartNumPrefixPcb, constPartNumPrefixPcba, '');

   
   {*** Get pcb version numbers from top level schematic file. ***}
   UpdateGuiStatusMessage('Status:  Starting step 0-' + IntToStr(step) + ':  Extracting PCB version numbers from top level schematic file.');
   GetPcbVersionNumbersFromTopLevelSchDoc(topLevelSchDoc,
                                          {var} schApcb1MnoPcbPartNumAndVersion,
                                          {var} schApcb1ValPcbPartNumAndVersion,
                                          {var} schApcb1ComPcbPartNumAndVersion,
                                          {var} schApcb1MnoPcbRevNum,
                                          {var} schGtbPcbPartNumAndVersion,
                                          {var} gtbRefDes);
   WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Extracted PCB version numbers from top level schematic file.');

   { Report various version numbers that we know have to debug file. }
   WriteToDebugFile('* pcbPartNumAndVersion is            "' + pcbPartNumAndVersion + '"');
   WriteToDebugFile('* pcbaVersion is                     "' + pcbaVersion + '"');
   WriteToDebugFile('* schApcb1MnoPcbPartNumAndVersion is "' + schApcb1MnoPcbPartNumAndVersion + '"');
   WriteToDebugFile('* schApcb1ValPcbPartNumAndVersion is "' + schApcb1ValPcbPartNumAndVersion + '"');
   WriteToDebugFile('* schApcb1ComPcbPartNumAndVersion is "' + schApcb1ComPcbPartNumAndVersion + '"');
   WriteToDebugFile('* schApcb1MnoPcbRevNum is            "' + schApcb1MnoPcbRevNum + '"');
   WriteToDebugFile('* schGtbPcbPartNumAndVersion is      "' + schGtbPcbPartNumAndVersion + '"');

   
   {*** Find this project's PcbDoc file. ***}
   { Find the full path to the 1 and only 1 PcbDoc file that should be part of this project. }
   UpdateGuiStatusMessage('Status:  Starting step 0-' + IntToStr(step) + ':  Identifying project''s PcbDoc file.');
   FindProjectPcbDocFile(Project,
                         flagRequirePcbDocFile,
                         {var} pcbDocPath);
   WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Identified project''s PcbDoc file.');

   
   {*** Get pcb version numbers from PcbDoc file. ***}
   GetPcbVersionNumbersFromPcbDoc(pcbDocPath,
                                  gtbRefDes,
                                  {var} pcbApcb1PcbPartNumAndVersion,
                                  {var} pcbGtbPcbPartNumAndVersion,
                                  {var} step);
   
   
   {*** Get svn rev number for this project's PcbDoc file. ***}
   { Get the svn rev number for the PcbDoc file. }
   GetPcbDocFileSvnRevNum(scriptsPath,
                          projectPath,
                          pcbDocPath,
                          {var} pcbDocRevNum,
                          {var} step);

   
   {*** Make sure version numbers and svn rev numbers match. ***}
   { See if we are flagged to package fabrication zipfile. }
   if (StrToBool(runPackager.Strings[fabIndex])) then
   begin

      { Make a substitution where will will replace the sch component Comment field "=VALUE"
       with the contents of the VALUE field itself.  This mostly serves to make the error
       message when things don't match slightly less confusing. }
      schApcb1ComPcbPartNumAndVersion := StringReplace(schApcb1ComPcbPartNumAndVersion, '=VALUE', schApcb1ValPcbPartNumAndVersion, '');
      
      { Make sure that all SCH and PCB components that specify the PCB version number match. }
      { Make sure that pcbPartNumAndVersion matches schApcb1MnoPcbPartNumAndVersion and schApcb1ValPcbPartNumAndVersion and schApcb1ComPcbPartNumAndVersion and
       schApcb1MnoPcbRevNum and schGtbPcbPartNumAndVersion and pcbApcb1PcbPartNumAndVersion and pcbGtbPcbPartNumAndVersion. }
      { Note that schApcb1ComPcbPartNumAndVersion is allowed to be '=VALUE'. }
      if (  (pcbPartNumAndVersion <> schApcb1MnoPcbPartNumAndVersion) or
          (pcbPartNumAndVersion <> schApcb1ValPcbPartNumAndVersion) or
          (pcbPartNumAndVersion <> schApcb1ComPcbPartNumAndVersion) or
          (pcbPartNumAndVersion <> schGtbPcbPartNumAndVersion) or
          (pcbPartNumAndVersion <> pcbApcb1PcbPartNumAndVersion) or
          (pcbPartNumAndVersion <> pcbGtbPcbPartNumAndVersion)  ) then
      begin

         UpdateGuiStatusMessage('Status:  Starting step 0-' + IntToStr(step) + ':  Checking agreement of version and rev numbers.');

         WriteToDebugFile('**Mismatch when checking PCB version numbers.  About to abort script.');
         WriteToDebugFile('* pcbPartNumAndVersion is            "' + pcbPartNumAndVersion + '"');
         WriteToDebugFile('* schApcb1MnoPcbPartNumAndVersion is "' + schApcb1MnoPcbPartNumAndVersion + '"');
         WriteToDebugFile('* schApcb1ValPcbPartNumAndVersion is "' + schApcb1ValPcbPartNumAndVersion + '"');
         WriteToDebugFile('* schApcb1ComPcbPartNumAndVersion is "' + schApcb1ComPcbPartNumAndVersion + '"');
         WriteToDebugFile('* schGtbPcbPartNumAndVersion is      "' + schGtbPcbPartNumAndVersion + '"');
         WriteToDebugFile('* pcbApcb1PcbPartNumAndVersion is    "' + pcbApcb1PcbPartNumAndVersion + '"');
         WriteToDebugFile('* pcbGtbPcbPartNumAndVersion is      "' + pcbGtbPcbPartNumAndVersion + '"');

         MyAbort('You are not ready to package files for fabrication.' + constLineBreak +
                 'These values must all agree as the same (human readable) PCB version number, and they do not.  Please fix.' + constLineBreak + constLineBreak + 
                 'pcbPartNumAndVersion            (from project level property) is                 "' + pcbPartNumAndVersion + '"' + constLineBreak +
                 'schApcb1MnoPcbPartNumAndVersion (from SCH APCB1 MFGNUMBER database parameter) is "' + schApcb1MnoPcbPartNumAndVersion + '"' + constLineBreak +
                 'schApcb1ValPcbPartNumAndVersion (from SCH APCB1 VALUE database parameter) is     "' + schApcb1ValPcbPartNumAndVersion + '"' + constLineBreak +
                 'schApcb1ComPcbPartNumAndVersion (from SCH APCB1 Comment parameter) is            "' + schApcb1ComPcbPartNumAndVersion + '"' + constLineBreak +
                 'schGtbPcbPartNumAndVersion      (from SCH XX* Comment parameter) is              "' + schGtbPcbPartNumAndVersion + '"' + constLineBreak +
                 'pcbApcb1PcbPartNumAndVersion    (from PCB APCB1 Comment parameter) is            "' + pcbApcb1PcbPartNumAndVersion + '"' + constLineBreak +
                 'pcbGtbPcbPartNumAndVersion      (from PCB XX* Comment parameter) is              "' + pcbGtbPcbPartNumAndVersion + '"' + constLineBreak);

         WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Checked agreement of version and rev numbers.');
         
      end; { endif }

   end { endif are we flagged to package fabrication zipfile }

   { Else we are not flagged to package fabrication zipfile. }
   else
   begin
      WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  SKIPPED Checking agreement of version and rev numbers.');
   end;

   { See if we are flagged to package assembly zipfile. }
   if (StrToBool(runPackager.Strings[assyIndex])) then
   begin

      { See if we're doing a simultaneous package fabrication and package assembly. }
      if (StrToBool(runPackager.Strings[fabIndex])) then
      begin

         WriteToDebugFile('*Doing simultaneous package fabrication and package assembly.  About to verify PcbDoc svn rev number.');

         { Make sure that the PcbDoc svn rev num called out in APCB1 MFGNUMBER field matches actual PcbDoc svn rev num. }
         if (schApcb1MnoPcbRevNum <> pcbDocRevNum) then
         begin

            MyAbort('I see that you are trying to do a simultaneous fabrication and assembly packaging.' + constLineBreak +
                    'However, PcbDoc svn rev num specified in MFGNUMBER field of APCB1 component, "' + schApcb1MnoPcbRevNum + '",' + constLineBreak +
                    'does not match svn rev num of PcbDoc file, "' + pcbDocRevNum + '"!!');

         end;

         WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Checked PcbDoc file svn rev number.');
      end { endif }

      { Else we're just doing a package assembly operation.
       Verify that a previously-created fabrication zipfile has the version number and svn rev number called out in APCB1 component. }
      else
      begin

         WriteToDebugFile('*Doing package assembly only.  About to verify existence of referenced fabrication zipfile.');

         { Split combined schApcb1MnoPcbPartNumAndVersion into separate schApcb1MnoPcbPartNum and schApcb1MnoPcbVersion. }
         SplitPcbPartNumAndVersion(schApcb1MnoPcbPartNumAndVersion,
                                   {var} schApcb1MnoPcbPartNum,
                                   {var} schApcb1MnoPcbVersion);


         { Retrieve existing string for fab zipFileName that contains a bunch of placeholders in need of substitution. }
         zipFileName := zipFileNames.Strings[fabIndex];

         { Determine what would be the name of this already-existing fabrication zipfile. }
         { TODO:  Clean up this mess.  It should be true that pcbPartNum = schApcb1MnoPcbPartNum, etc., right?? }
         CreateZipFileName(projectName,
                           {pcbPartNum} schApcb1MnoPcbPartNum, 
                           {pcbVersion} schApcb1MnoPcbVersion,
                           {pcbDocRevNum} schApcb1MnoPcbRevNum,
                           pcbaPartNum,
                           pcbaVersion,
                           bomRevNum,
                           {var} zipFileName);
         
         { Construct full path to this alleged zipfile. }
         filePath := (projOutPath + '\' + projOutSubDirs.Strings[fabIndex] + '\' + zipFileName);

         { Make sure that this fabrication zipfile in fact does already exist in trunk/ProjectOutputs/fabrication/. }
         if (not FileExists(filePath)) then
            MyAbort('I see that you are trying to do just assembly packaging.' + constLineBreak +
                    'However, I could not find an existing fabrication zipfile named ' + constLineBreak +
                    '"' + filePath + '"!' + constLineBreak);
         
         WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Checked existence of referenced fabrication zipfile.');
      end; { endelse }

   end { endif are we flagged to package assembly zipfile }
   
   { Else we are not flagged to package assembly zipfile. }
   else
   begin
      WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  SKIPPED Checking naming/existence of fabrication zipfile.');
   end;

end; { end GetPcbAndPcbaPartNumsVersionsAndRevs() }


{***************************************************************************
 * function SanityCheckRelAndTag()
 *  Perform sanity checks before starting release & tag operation.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function SanityCheckRelAndTag(scriptsPath : TDynamicString;
                              projectPath : TDynamicString;
                              )           : Integer;
var
   rc               : Integer;

begin 

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*In SanityCheckRelAndTag().');
   
   { First make sure there is a "\trunk\" in the project path.
    If not, it means that the user is running from an incomplete working copy of the project.
    This means that we won't be able to do release and tag, so abort now. }
   if (AnsiPos('\' + constSvnDirTrunk + '\', projectPath) = 0) then
   begin
      MyAbort('Could not find "\'+ constSvnDirTrunk + '\" in project path "' + projectPath + '".  Thus, it appears that you are working from an incomplete working copy of this project.  I will not be able to do release and tag operations because of this.');
   end; { endif }

end; { end SanityCheckRelAndTag() }


{***************************************************************************
 * function ComputeIncrementedVersion()
 *  Extract the build number ("z" of "foo-bar-x.y.z" or "x.y.z"), increment
 *  it, and then recombine it with the rest of the version string.
 *
 *  Returns incremented version string as var parm newVersion.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function ComputeIncrementedVersion(    version    : TDynamicString; 
                                   var newVersion : TDynamicString; 
                                   )              : Integer;

var
   majorMinor  : TDynamicString;
   position    : Integer;
   buildNum    : TDynamicString;
   buildNumInt : Integer;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   {* Split version number into "foo-bar-x.y" and "z" portions. *}
   WriteToDebugFile('*version is "' + version + '"');
   majorMinor := version;

   { Find the positon of the last "." char. }
   position := LastDelimiter('.', version);
   //   ShowMessage('Found last . at pos ' + IntToStr(position));

   { Truncate majorMinor. }
   if (position > 0) then
   begin
      
      { Chop off string at the start of last '.' delimiter. }
      SetLength(majorMinor, position-1);

      //      ShowMessage('majorMinor is now "' + majorMinor + '"');
   end;

   { Extract the build number "z". }
   buildNum := Copy(version, position+1, MaxInt);
   //   ShowMessage('buildNum is "' + buildNum + '"');

   { Increment buildNum. }
   buildNumInt := StrToInt(buildNum);
   buildNumInt := buildNumInt + 1;
   buildNum := IntToStr(buildNumInt);
   //   ShowMessage('buildNum is now "' + buildNum + '"');

   { Construct new PCBA version number. }
   newVersion := majorMinor + '.' + buildNum;
   //   ShowMessage('version is now "' + newVersion + '"');
   WriteToDebugFile('*version is now "' + newVersion + '"');

end; { end ComputeIncrementedVersion() }
   

{***************************************************************************
 * function ModifyProjectFileWithNewParms()
 *  Implement kludge to modify the contents of the Altium project file, to
 *  update/add project level parameters.
 *  
 *  FIXME:  We're currently doing this in a very kludgey way, by brutally
 *  re-writing the .PrjPcb project file itself.  This is because I've been
 *  unable to figure out how to modify and/or add project level parameters
 *  within Altium DelphiScript.
 *
 *  WARNING:  After this script finishes running, Altium will have a duplicate
 *  set of all project parameters in memory.  This is because it will read
 *  in the new values of all parameters after we re-write the project file.
 *  But it will not erase the old values of all the parameters.
 *  So the second time that a user runs this script (without shutting down
 *  and re-opening the project), GetPcbAndPcbaVersions() will find two
 *  values for each project parameter, one from before the first run of this
 *  script, and one from after the first run of this script.
 *  Currently we are relying on GetPcbAndPcbaVersions() to compare the two
 *  values for each parameter and use the "greater" one.
 *  If this should prove unreliable, then we could get more aggressive and
 *  force Altium to close the project after this script finishes running.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function ModifyProjectFileWithNewParms(Project      : IProject;
                                       projFilePath : TDynamicString;
                                       projectParms : TStringList;
                                       )            : Integer;

var
   i               : Integer;
   k               : Integer;
   projFileStrs    : TStringList;
   state           : Integer;
   projFile        : TextFile;
   name            : TDynamicString;
   value           : TDynamicString;
   AServerDocument : IServerDocument;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*In ModifyProjectFileWithNewParms()...');

   {** Proceed with the kludge to read in, modify, and re-write the project file itself. **}
   { Init string list to read in project file. }
   projFileStrs := TStringList.Create;

   { Read existing project file into projFileStrs. }
   projFileStrs.LoadFromFile(projFilePath);

   { Try to re-open project file for writing. }
   AssignFile(projFile, projFilePath);
   ReWrite(projFile);
   
   {* Copy existing project file to new project file, except completely rewrite the project parameters section. *}
   { Loop over all existing lines in the project file. }
   state := 0;
   for i := 0 to projFileStrs.Count - 1 do
   begin

      { Examine current state and act appropriately. }
      case state of

        { State 0:  Project file before the project parameters section. }
        { In state 0, unconditionally copy line from input to output.
         Look for a line that looks like "[Parameter". When we find it, advance state. }
        0 :
           begin

              { Look for the line that signifies the start of the project parameters section. }
              if (AnsiPos('[Parameter', projFileStrs.Strings[i]) <> 0) then
              begin

                 { Advance state. }
                 state := 1;
                 WriteToDebugFile('*Found "[Parameter" marker.  Advancing state.');

                 {* Output all project parameters to output file now. *}
                 { Loop over all the parameters backwards to preserve order in PrjPcb file. }
                 for k := (projectParms.Count - 1) downto 0 do
                 begin

                    { Split each project parameter into name and value. }
                    { Use our home brewed function, so that we don't split on spaces as well. }
                    SplitStringIntoLeftAndRight(projectParms.Strings[k],
                                                constStringEquals,
                                                {var} name,
                                                {var} value);
                    
                    { For each parameter, write a 4 line sequence of
                     [ParameterX]
                     Name=foo
                     Value=bar
                     (blank line).
                     }
                    WriteLn(projFile, '[Parameter' + IntToStr(projectParms.Count - k) + ']');
                    WriteLn(projFile, 'Name=' + name);
                    WriteLn(projFile, 'Value=' + value);
                    WriteLn(projFile, '');
                    
                 end; { endfor }

              end { endif }

              { Else we didn't find "[Parameter".  Just copy this line from input to output. }
              else
              begin

                 { Unconditionally copy this line of the original project file back out to the new project file. }
                 WriteLn(projFile, projFileStrs.Strings[i]);

              end; { endelse }

           end; { endcase 0 }

        { State 1:  Project file during the project parameters section. }
        { In state 1, ignore all lines.  Look for a line that starts with "[" and does not
         contain "[Parameter".  When we find it, output current line and advance state. }
        1 :
           begin

              { Look for a line that begins with "[" and does not contain "[Parameter" }
              if ( (Copy(projFileStrs.Strings[i], 1, 1) = '[') and (AnsiPos('[Parameter', projFileStrs.Strings[i]) = 0) ) then
              begin

                 { Copy this line of the original project file back out to the new project file. }
                 WriteLn(projFile, projFileStrs.Strings[i]);

                 { Advance state. }
                 state := 2;
                 WriteToDebugFile('*Found "[" with no "[Parameter".  Advancing state.');

              end; { endif }

           end; { endcase 1 }

        { State 2:  Project file after the project parameters section. }
        { In state 2, copy all lines from input to output and remain in this state forever. }
        2 :
           begin

              { Unconditionally copy this line of the original project file back out to the new project file. }
              WriteLn(projFile, projFileStrs.Strings[i]);

           end; { endcase 2 }

      else MyAbort('Unknown state ' + IntToStr(state));
      end; { endcase }          
      
   end; { endfor }

   { Close project file. }
   CloseFile(projFile);

   { Free projFileStrs. }
   projFileStrs.Free;

   { Sanity check. }
   if (state <> 2) then
      MyAbort('Encountered problem modifying project file.  Project file may be corrupted!');
   
   { Notify Altium that we've changed the project file contents out from under it. }
   { TODO:  Is this really doing anything?  Is there a better way to do this? }
   AServerDocument := Client.GetDocumentByPath(projFilePath);
   AServerDocument.DoFileLoad;
   
end; { end ModifyProjectFileWithNewParms() }


{***************************************************************************
 * function ModifyProjectParm()
 *  Modify a project level parameter in memory.
 *  We will handle writing it to disk later.
 *  
 *  When we find a project level parameter named versionParm, we replace
 *  its value with newVersion.
 *  When we find a project level parameter named verionLastParm, we replace
 *  its value with version.
 *  
 *  Returns modified project parms list as var parm projectParms.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function ModifyProjectParm(    version         : TDynamicString; 
                               newVersion      : TDynamicString; 
                               versionParm     : TDynamicString; 
                               versionLastParm : TDynamicString;
                           var projectParms    : TStringList;
                               )               : Integer;

var
   i                : Integer;
   position         : Integer;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   {* Attempt to modify parameter named versionParm. *}
   { Try to find the requested parameter in the parameter list. }
   position := projectParms.IndexOfName(versionParm);

   { We should have already verified the existence of this parameter, so
    this sanity check _should_ be redundant. }
   if (position < 0) then
      MyAbort('In ModifyProjectParm(), unable to find requested parm named "' + versionParm + '".');

   { Modify the value of this parameter. }
   projectParms.Strings[position] := (versionParm + constStringEquals + newVersion);
   WriteToDebugFile('*Set value of "' + versionParm + '" parameter to be "' + newVersion + '".');


   {* Attempt to modify parameter named versionLastParm. *}
   { Try to find the requested parameter in the parameter list. }
   position := projectParms.IndexOfName(versionLastParm);

   { This parameter is allowed to not yet exist.  We'll add it as needed. }
   if (position < 0) then
   begin

      { Add this parameter to the list. }
      projectParms.Add(versionLastParm + constStringEquals + version);
      WriteToDebugFile('*Added parameter "' + versionLastParm + '" with value "' + version + '".');

   end

   { Else it already exists.  Modify it. }
   else
   begin

      { Modify the value of this parameter. }
      projectParms.Strings[position] := (versionLastParm + constStringEquals + version);
      WriteToDebugFile('*Set value of "' + versionLastParm + '" parameter to be "' + version + '".');

   end; { endelse }
   
end; { end ModifyProjectParm() }


{***************************************************************************
 * function IncrementPcbAndPcbaVersions()
 *  If needed, increment the build number ("z" of the foo-bar-x.y.z version number)
 *  for the PCB (pcb fabrication) project property.
 *
 *  If needed, increment the build number ("z" of the x.y.z version number)
 *  for the PCBA (pcb assembly) project property.
 *
 *  If either of these parameters updated, then modify these parameters permanently
 *  by modifying the project file.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function IncrementPcbAndPcbaVersions(    Project              : IProject;
                                         projOutSubDirs       : TStringList;
                                         runPackager          : TStringList;
                                         projFilePath         : TDynamicString;
                                     var projectParms         : TStringList;
                                         pcbPartNumAndVersion : TDynamicString; 
                                         pcbaVersion          : TDynamicString; 
                                     var step                 : Integer;
                                         )                    : Integer;
var
   newPcbPartNumAndVersion : TDynamicString;
   newPcbaVersion          : TDynamicString;
   fabIndex                : Integer;
   assyIndex               : Integer;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   {*** Get indices into primary string lists for fabrication and assembly packaging ops, for later use. ***}
   {* Get index of fabrication ProjectOutputs/ subdir. *}
   fabIndex := GetFabIndex(projOutSubDirs);
   
   {* Get index of assembly ProjectOutputs/ subdir. *}
   assyIndex := GetAssyIndex(projOutSubDirs);


   {*** Proceed to increment fab and/or assembly versions as needed. ***}
   { We only need to do this operation at all if we were flagged to package
    fabrication and/or assembly files.  If not, we have nothing to do, so just return. }
   if ( (StrToBool(runPackager.Strings[fabIndex])) or (StrToBool(runPackager.Strings[assyIndex])) ) then
   begin
      
      { See if we need to increment PCB version number. }
      if (StrToBool(runPackager.Strings[fabIndex])) then
      begin
         
         UpdateGuiStatusMessage('Status:  Starting step 2-' + IntToStr(step) + ':  Incrementing PCB version number by modifying project parameter in project file.');

         { Compute incremented PCB version number. }
         ComputeIncrementedVersion(pcbPartNumAndVersion,
                                   {var} newPcbPartNumAndVersion);

         { Modify contents of project file string list with new PCB version number. }
         ModifyProjectParm(pcbPartNumAndVersion,
                           newPcbPartNumAndVersion,
                           constPcbVersionParm,
                           constPcbVersionLastParm,
                           {var} projectParms);
            
      end; { endif }

      { See if we need to increment PCBA version number. }
      if (StrToBool(runPackager.Strings[assyIndex])) then
      begin
         
         UpdateGuiStatusMessage('Status:  Starting step 2-' + IntToStr(step) + ':  Incrementing PCBA version number by modifying project parameter in project file.');

         { Compute incremented PCBA version number. }
         ComputeIncrementedVersion(pcbaVersion,
                                   {var} newPcbaVersion);

         { Modify contents of project file string list with new PCBA version number. }
         ModifyProjectParm(pcbaVersion,
                           newPcbaVersion,
                           constPcbaVersionParm,
                           constPcbaVersionLastParm,
                           {var} projectParms);
            
      end; { endif }

      { Modify the project file to update/add project parameters. }
      ModifyProjectFileWithNewParms(Project,
                                    projFilePath,
                                    projectParms);

      { Report to summary file what we did. }
      if ( (StrToBool(runPackager.Strings[fabIndex])) and (StrToBool(runPackager.Strings[assyIndex])) ) then
         WriteToSummaryFile('2-' + IntToStr(StepPlusPlus(step)) + '.  Incremented PCB and PCBA version numbers by modifying project parameters in project file.') { no semi }
      else if (StrToBool(runPackager.Strings[fabIndex])) then
         WriteToSummaryFile('2-' + IntToStr(StepPlusPlus(step)) + '.  Incremented PCB version number by modifying project parameter in project file.') { no semi }
      else
         WriteToSummaryFile('2-' + IntToStr(StepPlusPlus(step)) + '.  Incremented PCBA version number by modifying project parameter in project file.');
  
   end { endif }

   { Else we had nothing to do. }
   else
   begin
      WriteToSummaryFile('2-' + IntToStr(StepPlusPlus(step)) + '.  SKIPPED Incrementing PCB and/or PCBA version numbers by modifying project parameter(s) in project file.');

   end; { endelse }
   
end; { end IncrementPcbAndPcbaVersions() }
   

{***************************************************************************
 * function DiffFiles()
 *  Diff 2 text files on the disk and say whether or not they are exactly the same.
 *
 *  Returns:  0 when the files are the same, 1 if they are different.
 ***************************************************************************}
function DiffFiles(filePathA : TString;
                   filePathB : TString;
                   )         : Integer;
var                                                        
   i     : Integer;
   rc    : Integer;
   fileA : TStringList;
   fileB : TStringList;
                  
begin

   { Assume the files are the same until we find out otherwise. }
   rc := 0;

   { Create string lists to hold contents of both files. }
   fileA := TStringList.Create;
   fileB := TStringList.Create;

   { Load both files into string lists. }
   fileA.LoadFromFile(filePathA);
   fileB.LoadFromFile(filePathB);

   { If they have different numbers of lines, then the files differ. }
   if (fileA.Count <> fileB.Count) then
   begin
      rc := 1;
   end

   { Else they have the same number of lines.  Proceed to check them line-by-line. }
   else
   begin

      { Loop over all the files. }
      for i := 0 to fileA.Count - 1 do
      begin

         { Compare the contents of this line from each file. }
         if (fileA.Strings[i] <> fileB.Strings[i]) then
         begin

            { Now we know that the files differ. }
            { TODO:  Abort out of loop to speed things up. }
            rc := 1;
            
         end; { endif }
         
      end; { endfor }

   end; { endelse }
      
   { Free fileA & fileB lists.}
   fileA.Free;
   fileB.Free;
   
   { Return result to caller. }
   result := rc;

end; { end DiffFiles() }


{***************************************************************************
 * function AddGeneratedFileToProject()
 *  If the specified generated file is not already part of the project,
 *  then add it to the project.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function AddGeneratedFileToProject(var Project  : IProject;
                                       filePath : TString;
                                   var step     : Integer;
                                       )        : Integer;
var                                                        
   i       : Integer;
   rc      : Integer;
   genFile : TDynamicString;
   found   : Boolean;
                  
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

//   ShowMessage('Hello world from AddGeneratedFileToProject()');
   WriteToDebugFile('*Hello world from AddGeneratedFileToProject().  filePath is "' + filePath + '".');

   { Flag that we have not yet found our file in the list of known generated files. }
   found := False;

   {* Loop over all known generated files. *}
   for i := 0 to (Project.DM_GeneratedDocumentCount - 1) do
   begin

      { Extract the full filename of this generated file. }
      genFile := Project.DM_GeneratedDocuments(i).DM_FullPath;
      
      WriteToDebugFile('*Examining generated file "' + genFile + '".');

      { See if we have now found our generated file among the list of known generated files. }
      { Note:  It's likely that the file we're looking for (filePath) will be in all
       upper case.  The known generated files are likely not so.  Convert both
       to all upper case before comparing. }
      if (AnsiUpperCase(genFile) = AnsiUpperCase(filePath)) then
      begin

         WriteToDebugFile('*This is a match to filePath!');

         { Flag that we have found our file in the list of known generated files. }
         found := True;
         
      end; { endif }
      
   end; { endfor }

   {* See if we need to add this possibly new generated file to the project. *}
   if (not found) then
   begin

      UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  Adding new generated file to project.');

      { Attempt to add sorted Multiwire netlist to project. }
      Project.DM_AddGeneratedDocument(filePath);

      { Attempt to save modified project file. }
      ResetParameters;
      AddStringParameter('SaveMode', 'Standard');
      AddStringParameter('ObjectKind', 'Project');
      RunProcess('WorkspaceManager:SaveObject');      
      
      WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + ':  Added new generated file to project.');

   end { endif }

   { Else there's nothing we need to do.  This generated file is already part of the project. }
   else
   begin

      WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + ':  SKIPPED Adding new generated file to project.');

   end; { endelse }

end; { end AddGeneratedFileToProject() }


{***************************************************************************
 * function SortMultiwireNetlist()
 *  Sort the generated Multiwire netlist to make it human-readable and
 *  human-auditable.
 *
 *  TODO:  This whole operation could be done internally in this script,
 *  rather than shelling out to call awk, sed, sort, etc.
 *
 *  Note:  Due to limitations of FindFiles(), the sorted Multiwire netlist file will
 *  be named in ALL CAPS.  This is annoying, but not a show stopper.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function SortMultiwireNetlist(var Project     : IProject;
                                  scriptsPath : TDynamicString;
                                  projectPath : TString;
                                  projOutPath : TString;
                                  subDir      : TString;
                              var step        : Integer;
                                  )           : Integer;
var                                                        
   i            : Integer;
   fileList     : TStringList;
   unsortedPath : TDynamicString;
   sortedPath   : TDynamicString;
   fooStr       : WideString;
   rc           : Integer;
                  
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

//   ShowMessage('Hello world from SortMultiwireNetlist()');
   WriteToDebugFile('*Hello world from SortMultiwireNetlist()');

   UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  Sorting Multiwire netlist.');

   { Create list of files. }
   fileList := TStringList.Create;

   { Look for one or more files ending in ".NET".
    These are the Multiwire netlist files produced by Altium. }
   MyFindFiles(projOutPath,
               subDir,
               ('*' + constExtMultiWireNetlist),
               {var} fileList);

   { See if we found any Multiwire netlist files. }
   for i := 0 to fileList.Count - 1 do
   begin

      { Extract the filename of the unsorted Multiwire netlist file. }
      unsortedPath := fileList.Strings[i];
      
      WriteToDebugFile('*Examining net file ' + unsortedPath);

      { Exclude any files that already have "_1.NET" in their filename. }
      { Here we have to be aware that all files returned by MyFindFiles() are in all upper case,
       and AnsiPos() is case sensitive. }
      if (AnsiPos(constExtMultiWireNetlistSorted, unsortedPath) = 0) then
      begin

         { Construct the filename for the sorted version of this file that we will create. }
         sortedPath := StringReplace(unsortedPath, constExtMultiWireNetlist, constExtMultiWireNetlistSorted, '');

         WriteToDebugFile('*About to try to sort Multiwire netlist file ' + unsortedPath + ' as ' + sortedPath + '.');

         { Call external bat script to sort the Multiwire netlist. }
         RunSortMulti(scriptsPath,
                      projectPath,
                      unsortedPath,
                      sortedPath);

         WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  Sorted Multiwire netlist.');
         
         { If needed, add sorted Multiwire netlist to project. }
         AddGeneratedFileToProject({var} Project,
                                   sortedPath, {filePath,}
                                   {var} step);
         
      end; { endif }
      
   end; { endfor }

   { Free the list of files. }
   fileList.Free;   

end; { end SortMultiwireNetlist() }


{***************************************************************************
 * function FixIpc356Netlist()
 *  Correct bug with Altium generation of IPC-356 netlist files.
 *
 * The problem is that the IPC356 file format is a fixed column format that only allocates 4 digits
 * to the X and Y sizes of any pad (where a digit is 1/10 of a mil).  So the max size of a pad is
 * 9999 * 10^-4 inch = 999.9 mil = 0.9999 inch.
 *
 * Altium does not respect this limitation.  When it is presented with a pad with X or Y dimensions that 
 * exceed 999.9 mil, it will use another digit, and thus generate an improper IPC356 netlist line.
 * This comes up with ESD pads that are part of the PXI spec.
 *
 * A valid IPC356 netlist line looks like:
 *327V_ESD1           R7    -2         PA01X 012563Y 001812X0591Y0512R270 S0
 *
 * An invalid one that uses one too many digits for an X or Y pad size looks like:
 *327V_ESD1           ESD1  -1         PA01X 011750Y 000625X0700Y15500R090 S0
 *
 * We will fix this to look like:
 *327V_ESD1           ESD1  -1         PA01X 011750Y 000625X0700Y9999R090 S0
 *
 * The fix will result in the PCB test group thinking that the pad is smaller in X or Y than it actually is.
 * But this is not a big deal, and the pad will still be tested.
 *
 *  Note:  Due to limitations of FindFiles(), the fixed IPC356 netlist file will
 *  be named in ALL CAPS.  This is annoying, but inconsequential.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function FixIpc356Netlist(var Project     : IProject;
                              scriptsPath : TDynamicString;
                              projectPath : TString;
                              projOutPath : TString;
                              subDir      : TString;
                          var step        : Integer;
                          )               : Integer;

var                                                        
   i           : Integer;
   fileList    : TStringList;
   unfixedPath : TDynamicString;
   fixedPath   : TDynamicString;
   rc          : Integer;
                  
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

//   ShowMessage('Hello world from FixIpc356Netlist()');
   WriteToDebugFile('*Hello world from FixIpc356Netlist()');

   UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  Fixing IPC-356 netlist if needed.');
            
   { Create list of files. }
   fileList := TStringList.Create;

   { Look for one or more files ending in ".ipc".
    These are the IPC-356 netlist files produced by Altium. }
   MyFindFiles(projOutPath,
               subDir,
               ('*' + constExtIpc356Netlist),
               {var} fileList);

   { See if we found any IPC-356 netlist files. }
   for i := 0 to fileList.Count - 1 do
   begin

      { Extract the filename of the unfixed IPC-356 netlist file. }
      unfixedPath := fileList.Strings[i];
      
      WriteToDebugFile('*Examining ipc file ' + unfixedPath);

      { Exclude any files that already have "_fixed.ipc" in their filename. }
      { Here we have to be aware that all files returned by MyFindFiles() are in all upper case,
       and AnsiPos() is case sensitive. }
      if (AnsiPos(AnsiUpperCase(constExtIpc356NetlistFixed), unfixedPath) = 0) then
      begin

         { Construct the filename for the fixed version of this file that we will create. }
         fixedPath := StringReplace(unfixedPath, constExtIpc356Netlist, constExtIpc356NetlistFixed, '');

         WriteToDebugFile('*About to try to fix IPC356 file ' + unfixedPath + ' as ' + fixedPath + '.');

         { Call external sed script to fix the IPC-356 netlist file if needed. }
         RunSed(scriptsPath,
                projectPath,
                unfixedPath,
                fixedPath,
                '"s/\([0-9][0-9][0-9][0-9][0-9][0-9]\)X\([0-9][0-9][0-9][0-9][0-9]\)Y\([0-9][0-9][0-9][0-9]\)R\([0-9][0-9][0-9]\) S\([0-9]\)/\1X9999Y\3R\4 S\5/; s/\([0-9][0-9][0-9][0-9][0-9][0-9]\)X\([0-9][0-9][0-9][0-9]\)Y\([0-9][0-9][0-9][0-9][0-9]\)R\([0-9][0-9][0-9]\) S\([0-9]\)/\1X\2Y9999R\4 S\5/; "');

         { Examine unfixed file and fixed file and see if they are different. }
         rc := DiffFiles(unfixedPath, fixedPath);

         { If they're the same, then delete the fixed file, since we didn't actually need to fix anything. }
         if (rc = 0) then
         begin
            WriteToDebugFile('*These files ended up the same: ' + unfixedPath + ' ' + fixedPath + '.  I will delete the fixed file');

            WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  SKIPPED Fixing generated IPC-356 netlist file (not broken).');

            { Attempt to delete the fixed file from disk. }
            DeleteFileWithVerify(fixedPath);
            
         end
         
         { Else they were different.  Keep the fixed version. }
         else
         begin
            WriteToDebugFile('*These files are different: ' + unfixedPath + ' ' + fixedPath + '.  Thus, keeping fixed file.');
            
            WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  Fixed generated IPC-356 netlist file.');

            { If needed, add fixed IPC-356 netlist file to project. }
            AddGeneratedFileToProject({var} Project,
                                      fixedPath, {filePath,}
                                      {var} step);
            
         end; { endelse }

      end; { endif did this file not have "fixed.ipc" in the filename? }
      
   end; { endfor }

   { Free the list of files. }
   fileList.Free;   

end; { end FixIpc356Netlist() }


{***************************************************************************
 * function GenerateAllOutputs()
 *  Open all OutJob files in turn and generate all output files.
 *  Along the way, do a few cleanup steps as needed.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function GenerateAllOutputs(var Project                    : IProject;
                                projectName                : TDynamicString;
                                scriptsPath                : TDynamicString;
                                projectPath                : TString;
                                projOutPath                : TString;
                                projOutSubDirs             : TStringList;
                                outJobFiles                : TStringList;
                                outJobPdfContainers        : TStringList;
                                outJobPdfEnableNets        : TStringList;
                                outJobGenContainers        : TStringList;
                                outJobStatusMsg            : TStringList;
                                outJobDoSortMultiNetlist   : TStringList;
                                outJobSetSvnKeywordsOnBoms : TStringList;
                                outJobDoFixIpc356Netlist   : TStringList;
                                runOutJobs                 : TStringList;
                            var step                       : Integer;
                                )                          : Integer;
var                                                        
   i    : Integer;
                  
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;
   
   WriteToDebugFile('*In GenerateAllOutputs().');

   {** Loop over all ProjectOutputs subdirs and run the specified OutJob container(s) for each. **}
   { Note:  Here we assume that all these string lists are of equal size! }
   for i := 0 to projOutSubDirs.Count - 1 do
   begin

      WriteToDebugFile('*Starting to generate outputs for projOutSubDir ' + IntToStr(i));

      { See if we have been enabled to run this OutJob file at all. }
      if (StrToBool(runOutJobs.Strings[i])) then
      begin

         { Open the desired OutJob file. }
         ResetParameters;
         AddStringParameter('ObjectKind'              ,'Document');
         AddStringParameter('FileName', projectPath + outJobFiles.Strings[i]);
         RunProcess('WorkspaceManager:OpenObject');

         UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  ' + outJobStatusMsg.Strings[i]);

         {* See if we have been enabled to run the PDF output container within this OutJob. *}
         if (outJobPdfContainers.Strings[i] <> '') then
         begin

            { Prepare to run specific outputs that are setup under the "Publish_To_PDF" Container in this OutJob script. }
            { For now we have to trust that the user has enabled all desired outputs and associated
             them with this output container.  In other words, each output must still have a green dot
             and green arrow by it.  I can't figure out how to forcibly enable these in script land. }
            ResetParameters;
            AddStringParameter ('Action'                 ,'PublishToPDF');
            AddStringParameter ('OutputMedium'           ,outJobPdfContainers.Strings[i]);
            AddStringParameter ('ObjectKind'             ,'OutputBatch');
            AddStringParameter ('OutputFilePath'         ,projOutPath); { This may or may not have any effect in OutputBatch mode. }
            AddIntegerParameter('ReleaseManaged'         ,'False');
            AddStringParameter ('OutputBasePath'         ,'foo38'); { This has no effect in OutputBatch mode.  Assume the user set this up in the Altium OutJob GUI.  //Project Outputs for MICROCAL_MAIN'); }
            AddStringParameter ('OutputPathMedia'        ,'');
            AddStringParameter ('OutputPathOutputer'     ,'[Output Type]');
            AddStringParameter ('OutputFileName'         ,'foo37.pdf'); { This has no effect in OutputBatch mode.  Assume the user set this up in the Altium OutJob GUI.  //projectName + '_FOO_Schematic_and_Gerber_plots.PDF');}
            AddStringParameter ('OpenOutput'             ,'True');
            AddStringParameter ('PromptOverwrite'        ,'False');
            AddIntegerParameter('PublishMethod'          ,0);
            AddIntegerParameter('ZoomLevel'              ,90);
            AddStringParameter ('FitSCHPrintSizeToDoc'   ,'False'); { Set to False to follow the Page Setup settings under the output. }
            AddStringParameter ('FitPCBPrintSizeToDoc'   ,'False'); { Set to False to follow the Page Setup settings under the output. }
            AddStringParameter ('GenerateNetsInfo'       ,outJobPdfEnableNets.Strings[i]);
            AddStringParameter ('MarkPins'               ,'True');
            AddStringParameter ('MarkNetLabels'          ,'True');
            AddStringParameter ('MarkPortsId'            ,'True');
            AddStringParameter ('GenerateTOC'            ,'True');
            AddStringParameter ('DisableDialog'          ,'True'); { Set to False to get confirmation dialog box to pop up prior to PDF file generation. }

            { See if we are globally allowed to generate outputs. }
            if (enableGenerateOutputs = True) then
            begin
               WriteToDebugFile('*Running pdf container....');
               RunProcess('WorkspaceManager:Print');
            end;

         end; { endif enabled to run PDF output container. }

         
         {* See if we have been enabled to run the GEN output container within this OutJob. *}
         if (outJobGenContainers.Strings[i] <> '') then
         begin

            { Attempt to run "Generate_files_XIA_reviews" as specified in the OutJob script. }
            { For now we have to trust that the user has enabled all desired outputs and associated
             them with this output container.  In other words, each output must still have a green dot
             and green arrow by it.  I can't figure out how to forcibly enable these in script land. }
            ResetParameters;
            AddStringParameter ('Action'                 ,'Run');
            AddStringParameter ('OutputMedium'           ,outJobGenContainers.Strings[i]);
            AddStringParameter ('ObjectKind'             ,'OutputBatch');

            { See if we are allowed to generate outputs. }
            if (enableGenerateOutputs = True) then
            begin
               WriteToDebugFile('*Running generate outputs container....');
               RunProcess('WorkspaceManager:GenerateReport');
            end

         end; { endif enabled to run generate outputs output container. }

         
         {* Write appropriate summary message. *}
         if (enableGenerateOutputs) then
         begin
            WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + ':  ' + StringReplace(outJobStatusMsg.Strings[i], 'Generating', 'Generated', ''));
         end

         else
         begin
            WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  ERROR!  In debug mode!  SKIPPED ' + outJobStatusMsg.Strings[i]);
         end;
         

         {* See if we've been flagged to sort the multiwire netlist at this time. *}
         if (StrToBool(outJobDoSortMultiNetlist.Strings[i])) then
         begin
            
            { Sort Multiwire netlist. }
            SortMultiwireNetlist({var} Project,
                                 scriptsPath,
                                 projectPath,
                                 projOutPath,
                                 projOutSubDirs.Strings[i],
                                 {var} step);
            
         end; { endif enabled to sort multiwire netlist. }

         
         {* See if we've been flagged to set svn prop keywords on BOM outputs from this OutJob. *}
         { This code has been moved to after we do the svn add. }
         
         {* See if we've been flagged to fixup the IPC-356 netlist at this time. *}
         if (StrToBool(outJobDoFixIpc356Netlist.Strings[i])) then
         begin
            
            { Fix up erroneous IPC-356 netlist file if needed. }
            FixIpc356Netlist({var} Project,
                             scriptsPath,
                             projectPath,
                             projOutPath,
                             projOutSubDirs.Strings[i], {subDir,}
                             {var} step);

         end; { endif enabled to fixup IPC-356 netlist. }

//       ShowMessage('About to close OutJob file "' + projectPath + outJobFiles.Strings[i] + '".');
         
         { Close the desired OutJob file. }
//       ResetParameters;
//       AddStringParameter('ObjectKind'              ,'Document');
//       AddStringParameter('FileName', projectPath + outJobFiles.Strings[i]);
//       RunProcess('WorkspaceManager:CloseObject');

         { The above doesn't work for some reason.  So try to close all project documents instead. }
         ResetParameters;
         AddStringParameter('ObjectKind', 'FocusedProjectDocuments');
         RunProcess('WorkspaceManager:CloseObject');
         
         { The above still doesn't close everything.  So try to close focused document. }
         { NOTE:  This assumes that there is only one document that remains open! }
         { FIXME:  Is this really safe to do in cases where there are 0 documents open?? }
         ResetParameters;
         AddStringParameter('ObjectKind', 'FocusedDocument');
         RunProcess('WorkspaceManager:CloseObject');
         
      end { endif enabled to run this OutJob }

      { Else we're not enabled to run this OutJob.  But increment step number anyway. }
      else
      begin

         { Disclose what steps we skipped. }
         WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  SKIPPED ' + outJobStatusMsg.Strings[i]);

      end; { endelse }
      
   end; { endfor }

end; { end GenerateAllOutputs() }


{***************************************************************************
 * function VerifyBomFilteringIsHappy()
 *  Ask the user if the filtering in the BOM file(s) turned out ok.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function VerifyBomFilteringIsHappy(var step : Integer;
                                       )    : Integer;

var
   button      : Integer;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { See if we are allowed to generate outputs. }
   if (enableGenerateOutputs = True) then
   begin

      UpdateGuiStatusMessage('Status:  Waiting for user response.');

      { Ask the user what we should do. }
      { See http://www.delphibasics.co.uk/RTL.asp?Name=MessageDlg }
      button := MessageDlg('Question:  Please look at the generated .xls BOM file and tell me if the filtering (suppressing ZF refdes fab notes) is correct.' + constLineBreak + constLineBreak +
                           'Is the filtering OK or shall I Cancel running this script?', mtConfirmation, mbOKCancel, 0);
      //   if (button = mrOk) then ShowMessage('Ok.  Click OK to proceed.');
      if (button = mrCancel) then MyAbort('User indicated that BOM filtering is not happy.');

      WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  Asked user to verify that xls BOM filtering was happy before proceeding.');
   
   end; { endif }

end; { end VerifyBomFilteringIsHappy() }
      
      
{***************************************************************************
 * function VerifyBomsAndPdfsAreNotFlocked()
 *  Ask the user to close all .pdf and .xls files before proceeding.
 *  Then check all files that are likely to be flocked to make sure they
 *  are not flocked, before proceeding.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function VerifyBomsAndPdfsAreNotFlocked(    scriptsPath    : TDynamicString;
                                            projectPath    : TDynamicString;
                                            projOutPath    : TString;
                                            projOutSubDirs : TStringList;
                                        var step           : Integer;
                                            )              : Integer;
var
   possiblyFlockedFiles : TStringList;
   i                    : Integer;
   k                    : Integer;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   UpdateGuiStatusMessage('Status:  Waiting for user response.');

   { Ask user to close all .pdf and .xls files before proceeding. }
   ShowWarning('Proceeding.  Please close all open files in Excel or Acroread and then click OK.');

   UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  Verifying that csv, xls, and pdf files are not flocked (eg. still open in Excel or Acroread) before proceeding.');
   
   {** Find all files that are likely to be flocked due to being open in Excel or Acroread. **}
   { Initialize lists of possiblyFlockedFiles. }
   possiblyFlockedFiles := TStringList.Create;

   { Loop over all ProjectOutputs output subdirs. }
   for i := 0 to projOutSubDirs.Count - 1 do
   begin

      { Find any csv BOM file(s) in this ProjectOutputs/ subdir. }
      MyFindFiles(projOutPath,
                  projOutSubDirs.Strings[i],
                  ('*' + constExtCsv),
                  {var} possiblyFlockedFiles);

      { Find any xls BOM file(s) in this ProjectOutputs/ subdir. }
      MyFindFiles(projOutPath,
                  projOutSubDirs.Strings[i],
                  ('*' + constExtXls),
                  {var} possiblyFlockedFiles);

      { Find any pdf documentation file(s) in this ProjectOutputs/ subdir. }
      MyFindFiles(projOutPath,
                  projOutSubDirs.Strings[i],
                  ('*' + constExtPdf),
                  {var} possiblyFlockedFiles);

   end; { endfor i }

   
   {** Check each file that may be flocked to be sure that it is not. **}
   { Loop over all possibly flocked files found in all ProjectOutputs output subdirs. }
   for k := 0 to possiblyFlockedFiles.Count - 1 do
   begin

      { Verify that this particular file is writeable (eg. not flocked). }
      WriteToDebugFile('*Verifying that file "' + possiblyFlockedFiles.Strings[k] + '" is not flocked.');
      VerifyFileIsWriteable(possiblyFlockedFiles.Strings[k]);

   end; { endfor k }

   
   WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  Verified that csv, xls, and pdf files are not flocked (eg. still open in Excel or Acroread) before proceeding.');
   
   { Free lists of possiblyFlockedFiles. }
   possiblyFlockedFiles.Free;
      
end; { end VerifyBomsAndPdfsAreNotFlocked() }
      
      
{***************************************************************************
 * function MarkXlsBomFileForAssemblyRelease()
 *  Modify the xls BOM file in question so that it is "marked" as being
 *  part of a valid assembly release.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function MarkXlsBomFileForAssemblyRelease(scriptsPath : TDynamicString;
                                          projectPath : TDynamicString;
                                          pcbaVersion : TDynamicString; 
                                          xlsBomPath  : TString;
                                          )           : Integer;
var
   i                   : Integer;
   len                 : Integer;
   xlsBomTemp          : TDynamicString;
   oldPcbaVerAndRevStr : TDynamicString;
   newPcbaVerAndRevStr : TDynamicString;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

//   ShowMessage('Hello world from MarkXlsBomFileForAssemblyRelease().');
   
   { Create the name of the temp xls BOM file. }
   { Here we must be mindful that xlsBomPath was originally created by FindFiles(),
    which has the nasty habit of converting everything to all upper case. }
   xlsBomTemp := StringReplace(xlsBomPath, constExtXlsUpper, constExtXlsTemp, '');

   { Explicitly delete the temp xls BOM file, and make sure it has been deleted from filesystem. }
   DeleteFileWithVerify(xlsBomTemp);

   WriteToDebugFile('*About to mark xls BOM file for assembly release.  BOM file is "' + xlsBomPath + '", temp BOM file will be "' +xlsBomTemp + '".');

   { Contruct replacement PCBA version + svn rev # string to put into modified xls BOM file. }
   newPcbaVerAndRevStr  := pcbaVersion + ' ' + constXlsBomRevStr;

   { Make sure the replacement PCBA version + svn rev # string is shorter than the original one. }
   len := Length(newPcbaVerAndRevStr);
   if (len > Length(constXlsBomPcbaVerAndRevStr)) then
      MyAbort('The replacement PCBA version + svn rev # string is longer than the original one!  Probably the PCBA version string is ridiculously long.');
   
   { Append enough ' ' (space) chars to newPcbaVerAndRevStr to equal the original length of constXlsBomPcbaVerAndRevStr
    (which is intentionally quite long). }
   for i:=1 to (Length(constXlsBomPcbaVerAndRevStr) - len) do
   begin
      newPcbaVerAndRevStr := newPcbaVerAndRevStr + ' ';
   end;

   WriteToDebugFile('*Original PCBA version + svn rev # string was "' + constXlsBomPcbaVerAndRevStr + '".');
   WriteToDebugFile('*New PCBA version + svn rev # string is       "' + newPcbaVerAndRevStr + '".');

   { Add escapes to any "!" chars in the old PCBA version + svn rev # string. }
   oldPcbaVerAndRevStr := StringReplace(constXlsBomPcbaVerAndRevStr, '!', '\!', MkSet(rfReplaceAll));

   { Call external sed script to copy xls BOM file to temp file and then modify it and overwrite original xls BOM file. }
   { Note:  Because we're using this with a DOS cmd.exe shell, we must use double quotes around
    our s/ expression below.  However, if you want to test this from within a cygwin or real
    unix shell, you want to use single quotes instead.  Otherwise, the shell will want
    to interpret "$Rev" as a shell variable. }
   RunPatchWithSed(scriptsPath,
                   projectPath,
                   xlsBomPath,
                   xlsBomTemp,
                   '"s/' + oldPcbaVerAndRevStr + '/' + newPcbaVerAndRevStr + '/; "');

   { Explicitly delete the temp xls BOM file, and make sure it has been deleted from filesystem. }
   DeleteFileWithVerify(xlsBomTemp);
   
end; { end MarkXlsBomFileForAssemblyRelease() }
      
      
{***************************************************************************
 * function MarkXlsBomFilesForAssemblyRelease()
 *  Mark all xls BOM files if we're doing an assembly release.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function MarkXlsBomFilesForAssemblyRelease(    scriptsPath    : TDynamicString;
                                               projectPath    : TDynamicString;
                                               projOutPath    : TString;
                                               projOutSubDirs : TStringList;
                                               runPackager    : TStringList;
                                               pcbaVersion    : TDynamicString; 
                                           var step           : Integer;
                                               )              : Integer;
var
   xlsBomFiles : TStringList;
   i           : Integer;
   k           : Integer;
   assyIndex   : Integer;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   {* Get index of assembly subdir. *}
   assyIndex := GetAssyIndex(projOutSubDirs);


   {** Find .xls BOM files. **}
   { See if we are allowed to generate outputs. }
   if (enableGenerateOutputs = True) then
   begin

      { Initialize lists of xlsBomFiles. }
      xlsBomFiles := TStringList.Create;

      {** Look for .xls BOM files that we need to modify to report that they are part of a valid assembly release. **}
      { Loop over all ProjectOutputs output subdirs. }
      for i := 0 to projOutSubDirs.Count - 1 do
      begin

         { Only modify xls BOM file(s) if we are flagged to package assembly files.
          Otherwise, leave them alone, so that they end up reporting PCBA version as
          "NOT-AN-ASSEMBLY-RELEASE! DO-NOT-BUILD-OR-PROGRAM-TO-THIS-BOM!". }
         if (StrToBool(runPackager.Strings[assyIndex])) then
         begin

            { Find any xls BOM file(s) in this ProjectOutputs/ subdir. }
            MyFindFiles(projOutPath,
                        projOutSubDirs.Strings[i],
                        ('*' + constExtXls),
                        {var} xlsBomFiles);

         end; { endif }
               

      end; { endfor i }

      
      {** See if there are any .xls BOM files that we need to modify. **}
      if (xlsBomFiles.Count > 0) then
      begin
      
         UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  Marking xls BOM files that they are part of a valid assembly release.');
      
         { Loop over all xls BOM files found in all ProjectOutputs output subdirs. }
         for k := 0 to xlsBomFiles.Count - 1 do
         begin

            { Call MarkXlsBomFileForAssemblyRelease() to do the actual work of "marking" this xls BOM file. }
            WriteToDebugFile('*About to mark xls BOM file "' + xlsBomFiles.Strings[k] + '" for assembly release "');
            MarkXlsBomFileForAssemblyRelease(scriptsPath,
                                             projectPath,
                                             pcbaVersion,
                                             xlsBomFiles.Strings[k]);

         end; { endfor k }

         WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  Marked xls BOM files that they are part of a valid assembly release.');
         
      end { endif }

      { Else we have no xls BOM files to mark (probably because we're not doing an assembly release). }
      else
      begin
          
         WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  SKIPPED Marking xls BOM files that they are part of a valid assembly release.');

      end; { endelse }
      
      { Free lists of xlsBomFiles. }
      xlsBomFiles.Free;
      
   end; { endif (enableGenerateOutputs = True) }

end; { end MarkXlsBomFilesForAssemblyRelease() }
      
      
{***************************************************************************
 * function AlterSvnPropsForXlsBomFiles()
 *  Alter svn:keywords properties on all Excel (.xls) BOM files that were
 *  generated in all ProjectOutputs/ output subdirs.  This can be called
 *  to either set or delete these svn:keywords properties.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function AlterSvnPropsForXlsBomFiles(    scriptsPath                : TDynamicString;
                                         projOutPath                : TString;
                                         projOutSubDirs             : TStringList;
                                         outJobSetSvnKeywordsOnBoms : TStringList;
                                         runOutJobs                 : TStringList;
                                         svnCmd                     : TString;
                                         alterVerbProgressive       : TString;
                                         alterVerbPast              : TString;
                                     var xlsBomFiles                : TStringList;
                                     var step                       : Integer;
                                         )                          : Integer;
var
   i           : Integer;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   {** Look for .xls BOM files for which we need to turn on/turn off svn properties. **}
   { Loop over all ProjectOutputs output subdirs. }
   for i := 0 to projOutSubDirs.Count - 1 do
   begin

      WriteToDebugFile('*Examining projOutSubDir ' + IntToStr(i) + ' to look for xls BOM files that we need to alter svn props on.');

      { See if we have been enabled to run this OutJob file at all. }
      if (StrToBool(runOutJobs.Strings[i])) then
      begin

         { See if we've been flagged to set svn keywords on BOM outputs from this OutJob. }
         if (StrToBool(outJobSetSvnKeywordsOnBoms.Strings[i])) then
         begin
            
            { Find any xls BOM file(s) in this ProjectOutputs/ subdir. }
            { Note:  Here we assume that the only files for which we want to alter
             svn properties are Excel (.xls) files. }
            MyFindFiles(projOutPath,
                        projOutSubDirs.Strings[i],
                        ('*' + constExtXls),
                        {var} xlsBomFiles);

         end; { endif }
            
      end; { endif }
      
   end; { endfor i }

   
   {** See if there are any .xls BOM files that we need to modify. **}
   if (xlsBomFiles.Count > 0) then
   begin
      
      UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  ' + alterVerbProgressive + ' svn property svn:keywords on xls BOM file(s).');

      {* Get ready to do the actual svn propset or propdel command. *}
   
      { Issue command to add svn:keywords properties to generated .xls BOM files. }
      IssueSvnCommand(scriptsPath,
                      projOutPath,
                      svnCmd,
                      xlsBomFiles);
   
      WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  ' + alterVerbPast + ' svn property svn:keywords on xls BOM file(s).');

   end { endif }

   { Else we have no xls BOM files to alter. }
   else
   begin
      
      WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  SKIPPED ' + alterVerbProgressive + ' svn property svn:keywords on xls BOM file(s).');

   end; { endelse }
   
end; { end AlterSvnPropsForXlsBomFiles() }
      
      
{***************************************************************************
 * function SetSvnPropsForXlsBomFiles()
 *  Set svn:keywords properties on all Excel (.xls) BOM files that were
 *  generated in all ProjectOutputs/ output subdirs.  
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function SetSvnPropsForXlsBomFiles(    scriptsPath                : TDynamicString;
                                       projOutPath                : TString;
                                       projOutSubDirs             : TStringList;
                                       outJobSetSvnKeywordsOnBoms : TStringList;
                                       runOutJobs                 : TStringList;
                                   var step                       : Integer;
                                       )                          : Integer;
var
   xlsBomFiles : TStringList;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Initialize lists of xlsBomFiles. }
   xlsBomFiles := TStringList.Create;

   { Call AlterSvnPropsForXlsBomFiles() to do all the real work. }
   result := AlterSvnPropsForXlsBomFiles(scriptsPath,
                                         projOutPath,
                                         projOutSubDirs,
                                         outJobSetSvnKeywordsOnBoms,
                                         runOutJobs,
                                         constSvnCmdPropSetKeywords,
                                         'Setting',
                                         'Set',
                                         {var} xlsBomFiles,
                                         {var} step);
  
   { Free lists of xlsBomFiles. }
   xlsBomFiles.Free;
   
end; { end SetSvnPropsForXlsBomFiles() }
      
      
{***************************************************************************
 * function DelSvnPropsForXlsBomFiles()
 *  Del svn:keywords properties on all Excel (.xls) BOM files that were
 *  generated in all ProjectOutputs/ output subdirs.
 *
 *  We will also use sed to replace "$Rev::               $:" with "Rev 12345              ".
 *  This is purely a cosmetic change to make the text less cryptic for techs
 *  at the board assembly house trying to make labels for our boards, etc.
 *  Of course, the obvious thing is to have Excel do this substitution as part
 *  of a formula.  The problem is that Excel will not re-evaluate expressions
 *  whose dependencies change outside of Excel's purview.  So when we replace
 *  the contents of cell D5 with Excel closed, Excel doesn't know that it needs
 *  to re-evaluate all expressions that depend on cell D5.
 *
 *  Another way to accomplish this would be to figure out how to do the equivalent
 *  of Control-Alt-Shift-F9 (re-evauluate ALL expressions) upon opening our Excel
 *  BOM file(s).  If anyone knows VB script, maybe that's not so difficult.
 *
 *  Report whether any changes were made to svn controlled files as var parm changesMade.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function DelSvnPropsForXlsBomFiles(    scriptsPath                : TDynamicString;
                                       projOutPath                : TString;
                                       projOutSubDirs             : TStringList;
                                       outJobSetSvnKeywordsOnBoms : TStringList;
                                       runOutJobs                 : TStringList;
                                   var changesMade                : Boolean;
                                   var step                       : Integer;
                                       )                          : Integer;
var
   xlsBomFiles : TStringList;
   xlsBomTemp  : TDynamicString;
   i           : Integer;
   
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Initialize lists of xlsBomFiles. }
   xlsBomFiles := TStringList.Create;

   { Call AlterSvnPropsForXlsBomFiles() to do the real work of deleting svn properties. }
   result := AlterSvnPropsForXlsBomFiles(scriptsPath,
                                         projOutPath,
                                         projOutSubDirs,
                                         outJobSetSvnKeywordsOnBoms,
                                         runOutJobs,
                                         constSvnCmdPropDelKeywords,
                                         'Deleting',
                                         'Deleted',
                                         {var} xlsBomFiles,
                                         {var} step);
  
   {** See if there are any .xls BOM files that we need to modify with sed. **}
   if (xlsBomFiles.Count > 0) then
   begin
      
      UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  Tweaking $' + 'Rev$ text in .xls BOM file(s).');

      { Loop over all .xls BOM files }
      for i := 0 to (xlsBomFiles.Count - 1) do
      begin
         
         { Create the name of the temp xls BOM file. }
         { Here we must be mindful that xlsBomFiles was originally created by FindFiles(),
          which has the nasty habit of converting everything to all upper case. }
         xlsBomTemp := StringReplace(xlsBomFiles.Strings[i], constExtXlsUpper, constExtXlsTemp, '');

         { Explicitly delete the temp xls BOM file, and make sure it has been deleted from filesystem. }
         DeleteFileWithVerify(xlsBomTemp);

         { Call external sed script to copy xls BOM file to temp file and then modify it and overwrite original xls BOM file. }
         { Note:  Because we're using this with a DOS cmd.exe shell, we must use double quotes around
          our s/ expression below.  However, if you want to test this from within a cygwin or real
          unix shell, you want to use single quotes instead.  Otherwise, the shell will want
          to interpret "$Rev" as a shell variable. }
         { The regex in sed will replace "$Rev::" with "Rev" and the ending "$:" with "".  Plus it will carry
          through the digits and spaces following them, and then add in exactly 5 spaces to make up for the
          3 chars lost around "$Rev::" and the 2 chars lost with "$:". }
         { We split this string into 2 delphi strings to prevent svn from doing a replacement around $Rev upon
          checkin of this script file. }
         RunPatchWithSed(scriptsPath,
                         projOutPath,
                         xlsBomFiles.Strings[i],
                         xlsBomTemp,
                         '"s/\$' + 'Rev:: \([0-9]\+\)\([ ]\+\)\$:/Rev \1\2     /g; "');

         { Explicitly delete the temp xls BOM file, and make sure it has been deleted from filesystem. }
         DeleteFileWithVerify(xlsBomTemp);

         { Flag that changes were made to xls BOM files. }
         changesMade := True;

      end; { endfor }
      
      WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  Tweaked $' + 'Rev$ text in .xls BOM file(s).');

   end { endif }

   { Else we have no xls BOM files to alter. }
   else
   begin
      
      WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  SKIPPED Tweaking $' + 'Rev$ text in .xls BOM file(s).');

      { Flag that no changes were made to xls BOM files. }
      changesMade := False;
      
   end; { endelse }
   
   { Free lists of xlsBomFiles. }
   xlsBomFiles.Free;
   
end; { end DelSvnPropsForXlsBomFiles() }
      
      
{***************************************************************************
 * function ExcludeUnfixedIpc356()
 *  Look in specified ProjectOutputs/ subdir for an .IPC file containing "_fixed."
 *  in the filename.  When this occurs, it means that we earlier found a
 *  flaw in the raw Altium-generated IPC-356 netlist file and corrected it,
 *  by creating another IPC-356 netlist with "_fixed" in the name.
 *  
 *  When this happens, we want to add an exclusion entry for the "unfixed" version
 *  of the file.  This way, the board fab house only sees the fixed version in
 *  the zipfile that we generate.
 *
 *  Note:  Assumes that excludes stringlist has already been created.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function ExcludeUnfixedIpc356(projOutPath : TString;
                              subDir      : TString;
                              excludes    : TStringList;
                              )           : Integer;
var
   fileList    : TStringList;
   i           : Integer;
   fileExt     : TString;
   unfixedFile : TDynamicString;
   position    : Integer;
                  
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Create list of files. }
   fileList := TStringList.Create;
   
   { Look for a file containing "_fixed.ipc".  If found, this indicates that one of my other scripts had
    to patch the IPC-356 netlist file.  If that happens, we want to include the "_fixed." file and
    exclude the "unfixed" file of the similar name. }
   MyFindFiles(projOutPath,
               subDir,
               ('*' + constExtIpc356NetlistFixed),
               {var} fileList);

   { See if we found any files. }
   for i := 0 to fileList.Count - 1 do
   begin
      
      WriteToDebugFile('*Found a filename containing "' + constExtIpc356NetlistFixed + '"! ' + fileList.Strings[i]);

      { Extract just the extension for future use. }
      fileExt := ExtractFileExt(fileList.Strings[i]);

      { Find the position of "_fixed.ipc" within the filename. }
      { Note that AnsiPos() is case sensitive, and thus we must be mindful of the fact that
       all results from MyFindFiles() are in all upper case. }
      position := AnsiPos(AnsiUpperCase(constExtIpc356NetlistFixed), fileList.Strings[i]);

      if (position > 0) then
      begin

         { Replace the fixed version of the file extension with the unfixed version of the file extension. }
         { Also, we want just the final filename, without the leading drive and path. }
         unfixedFile := ExtractFileName(StringReplace(fileList.Strings[i], AnsiUpperCase(constExtIpc356NetlistFixed), AnsiUpperCase(constExtIpc356Netlist), ''));

      end

      { This else condition should never happen. }
      else
      begin
         MyAbort('Problem finding "' + constExtIpc356NetlistFixed + '" in filename!!!');

         { Report error. }
         result := 1;
      end;
      
      WriteToDebugFile('*About to add this to list of excludes: ' + unfixedFile);
      
      { Add the "unfixed" version of the file to our excludes list. }
      excludes.Add(unfixedFile);

   end;

   { Free fileList list.}
   fileList.Free;

end; { end ExcludeUnfixedIpc356() }
   

{***************************************************************************
 * function AddAllOutputsToSvn()
 *  Silently add all ECO log files in ProjectLogs/ directory to svn.
 *  Add all generated output files and all zipfiles we just created to svn.
 *  Don't check them in yet.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function AddAllOutputsToSvn(scriptsPath     : TDynamicString;
                            projectPath     : TDynamicString;
                            projOutPath     : TString;
                            projLogPath     : TString;
                            projOutSubDirs  : TStringList;
                            projOutIncludes : TStringList;
                            )               : Integer;
var
   rc          : Integer;
   i           : Integer;
   subDirs     : TStringList;
      
begin 

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

//   ShowMessage('Hello World from AddAllOutputsToSvn.');

   { Create list of subdirectories that we need to add to svn. }
   subDirs := TStringList.Create;

   { Attempt to add ProjectLogs directory to svn. }
   subDirs.Add(projLogPath);
   subDirs.Add(projLogPath + '\*.*');
   
   { Attempt to add ProjectOutputs directory itself to svn. }
   subDirs.Add(projOutPath);

   { Attempt to add all ProjectOutputs output subdirs to svn.}
   for i := 0 to projOutSubDirs.Count - 1 do
   begin
      subDirs.Add(projOutPath + '\' + projOutSubDirs.Strings[i]);
      subDirs.Add(projOutPath + '\' + projOutSubDirs.Strings[i] + '\*.*');
   end; { endfor }
   
   { Attempt to add all ProjectOutputs include subdirs to svn.}
   for i := 0 to projOutIncludes.Count - 1 do
   begin

      { Make sure we filter out null entries in projOutIncludes list. }
      if (projOutIncludes.Strings[i] <> '') then
      begin
         subDirs.Add(projOutPath + '\' + projOutIncludes.Strings[i]);
         subDirs.Add(projOutPath + '\' + projOutIncludes.Strings[i] + '\*.*');
      end;

   end; { endfor }

   { Try to add all these subdirectories to svn.
    Note:  It will report lots of lines like "xyz is already under version control". }
   rc := IssueSvnCommand(scriptsPath,
                         projOutPath,
                         constSvnCmdAdd,
                         subDirs);
   
   { Free list of subdirs. }
   subDirs.Free;
   
end; { end AddAllOutputsToSvn() }


{***************************************************************************
 * function CheckinGeneratedFiles()
 *  Checkin all files that have already been "svn add'ed" in ProjectOutputs/
 *  subdirectories.
 *
 *  Returns svn rev number from this checkin as var parm newRevNum.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CheckinGeneratedFiles(    Project     : IProject;
                                   scriptsPath : TDynamicString;
                                   projectPath : TDynamicString;
                                   projLogPath : TString;
                                   projOutPath : TString;
                                   commitMsg   : TString;
                               var newRevNum   : TDynamicString;
                                   )           : Integer;
var
   rc          : Integer;
   projectFile : TString;
   parms       : TStringList;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Get the name of this project's project file, with extension. }
   projectFile := Project.DM_ProjectFileName;
   
   { Create list of parameters for svn command. }
   parms := TStringList.Create;

   { Set commit message. }
   parms.Add('-m');
   parms.Add(commitMsg);
      
   { Attempt to checkin project file, since this script may have made changes to it. }
   parms.Add(projectFile);
   
   { Attempt to checkin all new / changed files in ProjectLogs directory. }
   parms.Add(projLogPath);
   
   { Attempt to checkin all new / changed files in ProjectOutputs directory. }
   parms.Add(projOutPath);

   { Issue command to checkin generated files, and ask to look for "Committed revision" in svn output. }
   IssueSvnCommandLookForOutputLine(scriptsPath,
                                    projectPath,
                                    constSvnCmdCommit,
                                    parms,
                                    constSvnRepCommitCommittedRev,
                                    {var} newRevNum);

   { Free list of parameters. }
   parms.Free;
      
   { Make sure we found the desired line of output from svn.exe. }
   if (newRevNum = '') then
   begin
      MyAbort('Did not find file containing "' + constSvnRepCommitCommittedRev + '" in output from svn.exe!!');
   end;
   
   { Strip off "Committed revision " from start of newRevNum. }
   newRevNum := StringReplace(newRevNum, constSvnRepCommitCommittedRev, '', '');
   
   { Strip off "." from end of newRevNum. }
   newRevNum := StringReplace(newRevNum, '.', '', '');

//   ShowMessage('New rev num of generated outputs is "' + newRevNum + '".');
   
end; { end CheckinGeneratedFiles() }



{***************************************************************************
 * function GetFabIndex()
 *  Get index into various string lists for Fabrication subDir.
 *
 *  Returns index for Fabrication subDir.
 ***************************************************************************}
function GetFabIndex(projOutSubDirs : TStringList;
                     )              : Integer;
begin

   {* Get index of fabrication ProjectOutputs/ subdir. *}
   result := projOutSubDirs.IndexOf(constFabricationSubDir);

   { Sanity check.  IndexOf method fails with -1 return code. }
   if (result = -1) then
      MyAbort('Unable to find fabrication subdir!');

   WriteToDebugFile('*GetFabIndex() is returning ' + IntToStr(result) + '.');

end; { end GetFabIndex() }


{***************************************************************************
 * function GetAssyIndex()
 *  Get index into various string lists for Assembly subDir.
 *
 *  Returns index for Assembly subDir.
 ***************************************************************************}
function GetAssyIndex(projOutSubDirs : TStringList;
                      )              : Integer;
begin
   
   {* Get index of assembly ProjectOutputs/ subdir. *}
   result := projOutSubDirs.IndexOf(constAssemblySubDir);

   { Sanity check.  IndexOf method fails with -1 return code. }
   if (result = -1) then
      MyAbort('Unable to find assembly subdir!');

   WriteToDebugFile('*GetAssyIndex() is returning ' + IntToStr(result) + '.');

end; { end GetAssyIndex() }


{***************************************************************************
 * function CreateZipFileName()
 *  Construct the name of a zipFile.
 *  This will be accomplished by replacing placeholder strings (eg. "$bomRevNum$")
 *  with the actual value of such, now that these are all known.
 *
 *  Returns zipFile name into var parm zipFileName.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CreateZipFileName(    projectName  : TDynamicString;
                               pcbPartNum   : TDynamicString;
                               pcbVersion   : TDynamicString;
                               pcbDocRevNum : TDynamicString;
                               pcbaPartNum  : TDynamicString;
                               pcbaVersion  : TDynamicString;
                               bomRevNum    : TDynamicString;
                           var zipFileName  : TDynamicString;
                               )            : Integer;
begin 
   
   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*Hello world from CreateZipFileName(), existing zipFile/rel-and-tag-subdir name is "' + zipFileName + '".');

   { Replace all known placeholder strings with their now-known values. }
   zipFileName := StringReplace(zipFileName, '$projectName$', projectName, MkSet(rfReplaceAll));
   zipFileName := StringReplace(zipFileName, '$pcbPartNum$', pcbPartNum, MkSet(rfReplaceAll));
   zipFileName := StringReplace(zipFileName, '$pcbVersion$', pcbVersion, MkSet(rfReplaceAll));
   zipFileName := StringReplace(zipFileName, '$pcbDocRevNum$', pcbDocRevNum, MkSet(rfReplaceAll));
   zipFileName := StringReplace(zipFileName, '$pcbaPartNum$', pcbaPartNum, MkSet(rfReplaceAll));
   zipFileName := StringReplace(zipFileName, '$pcbaVersion$', pcbaVersion, MkSet(rfReplaceAll));
   zipFileName := StringReplace(zipFileName, '$bomRevNum$', bomRevNum, MkSet(rfReplaceAll));

   { Do a sanity check to make sure there are no error markers indicating uninitialized
    variables that made it into the final zipFileName. }
   if (AnsiPos(constSubUninitMarker, zipFileName) > 0) then
      MyAbort('Found uninitialized variable marker "' + constSubUninitMarker + '" in proposed zipFile / rel-and-tag-subdir name "' + zipFileName + '"!');
   
   WriteToDebugFile('*New zipFile/rel-and-tag-subdir name is "' + zipFileName + '".');

end; { end CreateZipFileName() }


{***************************************************************************
 * function CreateAndStoreZipFileName()
 *  Construct the name of a zipFile and store at the specified index.
 *  This will be accomplished by replacing placeholder strings (eg. "$bomRevNum$")
 *  with the actual value of such, now that these are all known.
 *
 *  Returns zipFile name into var parm zipFileNames.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CreateAndStoreZipFileName(    projectName  : TDynamicString;
                                       pcbPartNum   : TDynamicString;
                                       pcbVersion   : TDynamicString;
                                       pcbDocRevNum : TDynamicString;
                                       pcbaPartNum  : TDynamicString;
                                       pcbaVersion  : TDynamicString;
                                       bomRevNum    : TDynamicString;
                                       currIndex    : Integer;
                                   var zipFileNames : TStringList;
                                       )            : Integer;
var
   zipFileName : TDynamicString;

begin
   
   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*Hello world from CreateAndStoreZipFileName()');

   { Retrieve existing string that contains a bunch of placeholders. }
   zipFileName := zipFileNames.Strings[currIndex];

   { Call CreateZipFileName() to do all the various substitutions. }
   result := CreateZipFileName(projectName,
                               pcbPartNum,
                               pcbVersion,
                               pcbDocRevNum,
                               pcbaPartNum,
                               pcbaVersion,
                               bomRevNum,
                               {var} zipFileName);
   
   { Store newly constructed zipFileName back to zipFileNames string list for later use. }
   zipFileNames.Strings[currIndex] := zipFileName;
   
end; { end CreateAndStoreZipFileName() }


{***************************************************************************
 * function CreateAndStoreRelTagSubDirName()
 *  Construct the name of a rel-and-tag subdir and store at the specified index.
 *  This will be accomplished by replacing placeholder strings (eg. "$bomRevNum$")
 *  with the actual value of such, now that these are all known.
 *
 *  Returns zipFile name into var parm relAndTagSubDir.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CreateAndStoreRelTagSubDirName(    projectName     : TDynamicString;
                                            pcbPartNum      : TDynamicString;
                                            pcbVersion      : TDynamicString;
                                            pcbDocRevNum    : TDynamicString;
                                            pcbaPartNum     : TDynamicString;
                                            pcbaVersion     : TDynamicString;
                                            bomRevNum       : TDynamicString;
                                            currIndex       : Integer;
                                        var relAndTagSubDir : TStringList;
                                            )               : Integer;
begin

   { Cheat and use CreateAndStoreZipFileName() to do the real work.
    All we need to do is feed it relAndTagSubDir instead of zipFileNames. }
   result := CreateAndStoreZipFileName(projectName,
                                       pcbPartNum,
                                       pcbVersion,
                                       pcbDocRevNum,
                                       pcbaPartNum,
                                       pcbaVersion,
                                       bomRevNum,
                                       currIndex,
                                       {var} {zipFileNames} relAndTagSubDir);
   
end; { end CreateAndStoreRelTagSubDirName() }


{***************************************************************************
 * function CreateAllZipFileNames()
 *  Using various version and svn rev information previously retrieved,
 *  construct names for all the zipfiles and rel-and-tag subdirs.
 *
 *  As of 2011/08/30, bomRevNum is extracted from the original BOM file checkin,
 *  and is given to this function as an input.
 *
 *  Returns modified zipfile names in var parm zipFileNames.
 *  Returns modified rel-and-tag subdir names in var parm relAndTagSubDir.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CreateAllZipFileNames(    projectName     : TDynamicString;
                                   projOutPath     : TString;
                                   projOutSubDirs  : TStringList;
                               var zipFileNames    : TStringList;
                               var relAndTagSubDir : TStringList;
                                   runPackager     : TStringList;
                                   pcbPartNum      : TDynamicString;
                                   pcbVersion      : TDynamicString;
                                   pcbDocRevNum    : TDynamicString;
                                   pcbaPartNum     : TDynamicString;
                                   pcbaVersion     : TDynamicString;
                                   bomRevNum       : TDynamicString;
                                   )               : Integer;
var
   rc        : Integer;
   currIndex : Integer;
   assyIndex : Integer;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*Hello world from CreateAllZipFileNames()');
   
   {* Get index of assembly ProjectOutputs/ subdir, for later use. *}
   assyIndex := GetAssyIndex(projOutSubDirs);

   
   {*** Get svn rev number for this project's BOM file. ***}
   { Find the full path to one of the xls BOM files that should be part of this project. }
   { The svn rev number for the BOM file was already obtained and given to us. }
   { TODO:  If we ever want to be able to package files, but not re-generate them, then
    we would again need to get the BOM rev number.  Except we actually want the rev number
    of the next-to-last checkin, unlike what this function currently does. }
   { TODO:  If we do resurrect this, we should not hardcode for constPurchasingSubDir! }
//   UpdateGuiStatusMessage('Status:  Starting step 0-' + IntToStr(step) + ':  Extracting xls BOM file svn rev number.');
//   FindProjectBomFile(projOutPath,
//                      constPurchasingSubDir,
//                      {var} bomPath);
//   WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + ':  Extracted xls BOM file svn rev number.');

   
   {* Construct the names of all zipfiles, and store in zipFileNames. *}
   for currIndex := 0 to zipFileNames.Count - 1 do
   begin

      { Suppress doing this for assembly operation if the user didn't check that
       he/she wanted to do package assembly. }
      { TODO:  Is this really an issue any more?? }
      if ( (currIndex <> assyIndex) or (StrToBool(runPackager.Strings[assyIndex])) ) then
      begin
      
         { Construct and store the name of this particular zipFile. }
         CreateAndStoreZipFileName(projectName,
                                   pcbPartNum,
                                   pcbVersion,
                                   pcbDocRevNum,
                                   pcbaPartNum,
                                   pcbaVersion,
                                   bomRevNum,
                                   currIndex,
                                   {var} zipFileNames);

      end; { endif }
      
      WriteToDebugFile('* zipFileNames[' + IntToStr(currIndex) + '] is "' + zipFileNames.Strings[currIndex] + '"');
   end;

   {* Construct the names of all releases/ and tags/ subdirs, and store in relAndTagSubDir. *}
   for currIndex := 0 to relAndTagSubDir.Count - 1 do
   begin

      { Suppress doing this for assembly operation if the user didn't check that
       he/she wanted to do package assembly. }
      { TODO:  Is this really an issue any more?? }
      if ( (currIndex <> assyIndex) or (StrToBool(runPackager.Strings[assyIndex])) ) then
      begin
         
         { Construct and store the name of this particular rel-and-tag-subdir. }
         CreateAndStoreRelTagSubDirName(projectName,
                                        pcbPartNum,
                                        pcbVersion,
                                        pcbDocRevNum,
                                        pcbaPartNum,
                                        pcbaVersion,
                                        bomRevNum,
                                        currIndex,
                                        {var} relAndTagSubDir);

      end; { endif }
      
      WriteToDebugFile('* relAndTagSubDir[' + IntToStr(currIndex) + '] is "' + relAndTagSubDir.Strings[currIndex] + '"');
   end;
   
end; { end CreateAllZipFileNames() }


{***************************************************************************
 * function CheckForExistingZipFile()
 *  Check that a zipfile with the same version number (though possibly
 *  different svn rev number) does not already exist in this directory.
 *
 *  Note:  Don't call this function on any subDir whose zipfiles do
 *  not incorporate a "version" number, such as the purchasing subdir!
 *
 *  Note:  This code assumes/requires that all zipFile names end in "_$svnRevNum$.zip"!
 *  
 *  Returns:  0 on success, 1 if unable to delete one or more files
 ***************************************************************************}
function CheckForExistingZipFile(projOutPath : TString;
                                 subDir      : TString;
                                 zipFileName : TString;
                                 )           : Integer;
var
   fileList        : TStringList;
   strippedName    : TDynamicString;
   position        : Integer;
   i               : Integer;
   msg             : TDynamicString;
                   
begin
   
   { For now, assume/hope/pray that we will succeed. }
   result := 0;

//   ShowMessage('Hello World from CheckForExistingZipFile.');

   { Make sure that an existing zipfile with the same version number
    (but possibly different svn rev number) does not already exist. }
   strippedName := zipFileName;

   { Find position of last '_' char.  Example zipname is "PCBA-MICROCAL_MAIN_1.1.10_assy_rev_511.zip".}
   position := LastDelimiter('_', strippedName);
   SetLength(strippedName, position);

   { Append "*.zip" to strippedName.  Example strippedName is "PCBA-MICROCAL_MAIN_1.1.10_assy_rev_*.zip".}
   strippedName := (strippedName + '*.zip');

   WriteToDebugFile('*In CheckForExistingZipFile(), subDir is "' + subDir + '", zipFileName is "' + zipFileName + '", strippedName is "' + strippedName + '"');

   { Initialize list of files. }
   fileList := TStringList.Create;

   { Look for a zipfile with the same version number, but possibly a different svn rev number. }
   MyFindFiles(projOutPath,
               subDir,
               strippedName,
               {var} fileList);

   { Init error message in case we need it. }
   msg := 'Before creating zipfile "' + subDir + '\' + zipFileName + '",' + constLineBreak +
   'I did a check to be sure that there were not existing zipfiles with this version number (though possibly with different svn rev number).' + constLineBreak +
   'Unfortunately, I found one or more such files.' + constLineBreak +
   'This means that you need to bump the version number before re-running this script!' + constLineBreak;

   { See if we found any offending existing zipfiles. }
   if (fileList.Count > 0) then
   begin
      
      { Loop over all the files. }
      for i := 0 to fileList.Count - 1 do
      begin

         { Add the offending existing zipfile name to error message. }
         msg := msg + '   Existing file: "' + fileList.Strings[i] + '"' + constLineBreak;
            
      end; { endfor i }

      { Abort with error message we just created. }
      MyAbort(msg);

   end; { endif }
   
   { Free the file list. }
   fileList.Free;
      
end; { end CheckForExistingZipFile() }


{***************************************************************************
 * function CreateZipFile()
 *  Create a zipfile in a particular subdirectory of ProjectOutputs.
 *
 *  Allow caller to specify zipfile name, a list of files/extensions to exclude
 *  from zipfile, and a list of additional files to add to the zipfile.
 *  All files will be stored in zipfile without path information.
 *
 *  Returns name of new zipfile as var parm zipPathFileName.
 *  Returns:  0 on success, 1 if unable to delete one or more files
 ***************************************************************************}
function CreateZipFile(    projOutPath     : TString;
                           subDir          : TString;
                           zipFileName     : TString;
                           excludes        : TStringList;
                           addlIncludes    : TStringList;
                       var zipPathFileName : TDynamicString;
                           )               : Integer;
var
   Zip             : TXceedZip;
   fileList        : TStringList;
   i               : Integer;
   msg             : TDynamicString;
                   
begin
   
   { For now, assume/hope/pray that we will succeed. }
   result := 0;

//   ShowMessage('Hello World from CreateZipFile.');

   { Prepend the path to the zipfile name to yield a path + filename. }
   zipPathFileName := projOutPath + '\' + subDir + '\' + zipFileName;

   Try // begin { Emacs pascal mode doesn't understand Try construct.  Add fake "begin" keyword. }

      { Create zipfile }
      Zip := TXCeedZip.Create(zipPathFileName);

      { Setup Zipper so that we don't generate a temporary folder/file. }
      Zip.UseTempFile := False;

      { It's a non-issue for this application, but flag to not keep paths for archived files. }
      Zip.PreservePaths := False;

      { Initialize list of files. }
      fileList := TStringList.Create;

      { Fetch a list of all files in this subdirectory. }
      MyFindFiles(projOutPath,
                  subDir,
                  '*.*',
                  {var} fileList);

      { Exclude specified files from this list. }
      ExcludeFilesFromList({var} fileList,
                           excludes);
      
      { Loop over the non-excluded files in this ProjectOutputs/ subdir. }
      for i := 0 to fileList.Count - 1 do
      begin

         WriteToDebugFile('*Attempting to add file ' + fileList.Strings[i] + ' to zipfile....');
         
         { Add this file to the pending zip file that we will create. }
         Zip.AddFilesToProcess(fileList.Strings[i]);
         
      end; { endfor i }
      
      { See if we have any additional files we're supposed to add to the zipfile. }
      for i := 0 to addlIncludes.Count - 1 do
      begin

         WriteToDebugFile('*Attempting to add additional file ' + addlIncludes.Strings[i] + ' to zipfile....');
         
         { Add this file to the pending zip file that we will create. }
         Zip.AddFilesToProcess(addlIncludes.Strings[i]);
         
      end; { endfor i }
      
      { Create the zip file, now that we've identified all the files we want added. }
      WriteToDebugFile('*About to create zipfile ' + zipPathFileName);
      Zip.Zip;
      
      Finally
      Zip.Free;
      
   end; { endtry }

   { Free the file list. }
   fileList.Free;
      
end; { end CreateZipFile() }


{***************************************************************************
 * function CreateAllZipFiles()
 *  Create zipfiles in all desired ProjectOutputs/ subdirs.
 *
 *  Note:  Assumes that newZipFiles has already been Created.
 *
 *  Returns list of new zipfiles created as var parm newZipFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CreateAllZipFiles(    scriptsPath              : TDynamicString;
                               projOutPath              : TString;
                               projOutSubDirs           : TStringList;
                               projOutIncludes          : TStringList;
                               outJobDoFixIpc356Netlist : TStringList;
                               zipDoCheckForExisting    : TStringList;
                               zipExcludes              : TStringList;
                               zipFindAddlFiles         : TStringList;
                               zipFileNames             : TStringList;
                               runPackager              : TStringList;
                           var newZipFiles              : TStringList;
                               )                        : Integer;
var
   rc              : Integer;
   excludes        : TStringList;
   addlIncludes    : TStringList;
   findParms       : TStringList;
   pcbDocPath      : TDynamicString;
   bomPath         : TDynamicString;
   zipPathFileName : TDynamicString;
   i               : Integer;

begin
   { Assume that we'll succeed in creating zipfiles. }
   rc := 0;

   {* Proceed to create all zip files. *}
   { Loop over all ProjectOutputs output subdirs. }
   for i := 0 to projOutSubDirs.Count - 1 do
   begin

      { Only create a zipfile if the zipFileName is non-NULL,
       and if we've been flagged to do so from GUI form. }
      if ( (zipFileNames.Strings[i] <> '') and
          (StrToBool(runPackager.Strings[i])) )then
      begin
         
         { Initialize lists of excludes and additional includes. }
         excludes := TStringList.Create;
         addlIncludes := TStringList.Create;
         findParms := TStringList.Create;

         { Split this entry in zipExcludes into a string list named "excludes". }
         SplitDelimitedStringIntoStringList(zipExcludes.Strings[i],
                                            constStringDelimiter,
                                            {var} excludes);

         { If we had been instructed to fixup the IPC-356 netlist in this subdir,
          then look for the unfixed file to exclude from the zipfile. }
         if (StrToBool(outJobDoFixIpc356Netlist.Strings[i])) then
         begin
            
            { Look for a file containing "_fixed.".  If found, this indicates that earlier on we had
             to patch the IPC-356 netlist file.  If that happens, we want to include the "_fixed." file and
             then add the "unfixed" file of the similar name to the excludes list. }
            ExcludeUnfixedIpc356(projOutPath, projOutSubDirs.Strings[i], excludes);

         end;

         { See if this ProjectOutputs subdir has an associated include subdir. }
         if (projOutIncludes.Strings[i] <> '') then
         begin
            
            { Find any documentation, etc. files in include_whatever/ that we should be adding to our zipfile. }
            MyFindFiles(projOutPath,
                        projOutIncludes.Strings[i],
                        '*.*',
                        {var} addlIncludes);
            
         end;

         { See if we've been asked to find files in other ProjectOutputs/ subdirs that we should include in this zipfile. }
         if (zipFindAddlFiles.Strings[i] <> '') then
         begin

            { Split this entry in zipFindAddlFiles into a string list named "findParms". }
            SplitDelimitedStringIntoStringList(zipFindAddlFiles.Strings[i],
                                               constStringDelimiter,
                                               {var} findParms);

            { Make sure we have both an output subdir and a file mask. }
            if (findParms.Count <> 2) then
               MyAbort('In CreateAllZipFiles(), was expecting exactly 2 strings after splitting entry in zipFindAddlFiles.  Found ' + IntToStr(findParms.Count) + ' parms.');

            { Launch a find files with the specified ProjectOutputs output subdir and file mask. }
            MyFindFiles(projOutPath,
                        findParms.Strings[0],
                        findParms.Strings[1],
                        {var} addlIncludes);
            
         end; { endif }

         { See if we need to check for presence of existing zipfile in this subDir. }
         if (StrToBool(zipDoCheckForExisting.Strings[i])) then
         begin
            
            { Check for presence of an existing zipfile with the same version number
             (though possibly different svn rev #) in this subDir. }
            CheckForExistingZipFile(projOutPath,
                                    projOutSubDirs.Strings[i],
                                    zipFileNames.Strings[i])

         end; { endif }
         
         { Attempt to zip up the files in this ProjectOutputs/ subdir. }
         rc := rc | CreateZipFile(projOutPath,
                                  projOutSubDirs.Strings[i],
                                  zipFileNames.Strings[i],
                                  excludes,
                                  addlIncludes,
                                  {var} zipPathFileName);

         { Add this file to the list of new zipfiles created. }
         newZipFiles.Add(zipPathFileName);           

         { Free lists of excludes and additional includes. }
         excludes.Free;
         addlIncludes.Free;
         findParms.Free;

      end; { endif }

   end; { endfor }
   
   { Give return code to caller. }
   result := rc;

end; { end CreateAllZipFiles() }


{***************************************************************************
 * function CreateRelOrTagSubDirs()
 *  Iteratively create all needed subdirectories for a given OutJob in
 *  either releases/ or tags/.
 *
 *  Note:  Assumes that finalSubDir includes exactly one trailing '\' char.
 *
 *  Returns a list of all subDirs created in var parm newSubDirs.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CreateRelOrTagSubDirs(    scriptsPath      : TDynamicString;
                                   projectPath      : TDynamicString;
                                   finalSubDir      : TDynamicString;
                                   relOrTag         : TDynamicString;
                               var newSubDirs       : TStringList;
                                   )                : Integer;
var
   rc                   : Integer;
   basePath             : TDynamicString;
   strippedRelOrTagPath : TDynamicString;
   projRelOrTagPath     : TDynamicString;
   position             : Integer;
   subDir               : TDynamicString;
   subDirPath           : TDynamicString;
   subDirs              : TStringList;
   i                    : Integer;
   oldNumNewSubDirs     : Integer;

begin 

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Init subDirs string list. }
   subDirs := TStringList.Create;

   { Add leading and trailing path separators to relOrTag for ease of use. }
   relOrTag := ('\' + relOrTag + '\');
   
   { First, add final subdir (eg. "MICROCAL_MAIN_v1.13_fab_rev_19894") to projectPath. }
   { Next, translate the extended project path into a release path or a tag path. }
   projRelOrTagPath := StringReplace( (projectPath + finalSubDir), '\' + constSvnDirTrunk + '\', relOrTag, '');
//   ShowMessage(projRelOrTagPath);
   
   { Strip off everything before "\releases\" or "\tags\". }
   { Here again we're hobbled by the limited string handling in the subset of Delphi that Altium
    gives us.  So go through some contortions. }
   { TODO:  Revisit this code to replace kludgy SetLength() operation with Copy(). }   
   basePath := projRelOrTagPath;
   position := AnsiPos(relOrTag, projRelOrTagPath);
   SetLength(basePath, position);

   { Replace everything preceding the "\releases\" or "\tags\" string with a null string. }
   strippedRelOrTagPath := StringReplace(projRelOrTagPath, basePath, '', MkSet(rfReplaceAll));
//   ShowMessage(strippedRelOrTagPath);

   { Record the number of "new" subdirs that are already in the list. }
   oldNumNewSubDirs := newSubDirs.Count;
   
   { Now enumerate all the subdirectories (eg, "releases", "schem", "MicroCal_main", "MICROCAL_MAIN_v1.13_fab_rev_19894_assy_rev_19901" ). }
   { Loop until we run out of path separators. }
   { Note:  Here we assume that there is a useless path separator on the end to make our lives easier. }

   { Once again work around the fact of limited string handling capability in Altium's subset
    of Delphi.  So this is more convoluted than it should be. }
   { TODO:  Revisit this code to replace kludgy StringReplace() operation with Copy(). }   
   i := 0;
   repeat
   begin
      
      { Copy running strippedRelOrTagPath to subDir. }
      subDir := strippedRelOrTagPath;
      position := (AnsiPos('\', strippedRelOrTagPath) - 1);
      
      { There are remaining path separator(s).  Truncate and get ready for next time. }
      { Truncate end of subDir name. }
      setLength(subDir, position);

      { Enumerate full path to this subDir }
      subDirPath := (basePath + '\' + subDir);
//    WriteToDebugFile('Need to make this subDir: "' + subDir + '", as "' + subDirPath + '"');

      { Add this subdir to the running list of subDirs to verify / create. }
      subDirs.Add(subDirPath);
      
      { Strip this subDir off of running strippedRelOrTagPath. }
      strippedRelOrTagPath := StringReplace(strippedRelOrTagPath, (subDir + '\'), '', '');

      { Keep track of the subDir we just enumerated by adding to the base path for next time around. }
      basePath := (basePath + '\' + subDir);

      { Increment loop counter. }
      i := i+1;
      
   end;
   until (AnsiPos('\', strippedRelOrTagPath) = 0);

   { Attempt to restore / update some or all of these subDirs from svn repo. }
   { WARNING:  This is funky usage of svn update and will result in the working
    copy having an incomplete copy of either releases/ or tags/!  This is
    being done in order to minimize disk space used by a user's working copy,
    particularly in a situation where 2 or more users are working on (and
    making releases of) a given project.
    Moreover, this "depth=empty" property will stick around and an ordinary
    svn update command will NOT result in pulling down the missing subdirs and
    files to the working copy.
    If you wish to have releases/ and tags/ all the way updated in your working
    copy, you will need to manually issue a command like:
    svn update --set-depth=infinity h:/projects/spiffy_widget/releases h:/projects/spiffy_widget/tags
    }
   IssueSvnCommand(scriptsPath,
                   projectPath,
                   constSvnCmdUpdateDepthEmpty,
                   subDirs);
   
   { Loop over all the subDirs that we need to verify / create. }
   { If any of these didn't already exist or were not restored by the above
    svn update command, then we'll create them now. }
   for i := 0 to subDirs.Count - 1 do
   begin

      { Call CreateSubDir() to do the actual verify / create of this subDir. }
      CreateSubDir(scriptsPath,
                   projectPath,
                   subDirs.Strings[i],
                   {var} newSubDirs);

   end; { endfor }
   
   { Free list of subDirs to verify / create. }
   subDirs.Free;

   { Sanity check to make sure that we created at least the final leaf subdir. }
   if (oldNumNewSubDirs = newSubDirs.Count) then
      MyAbort('In CreateRelOrTagSubDirs(), did not create any new subdirs in ' + relOrTag + ', finalSubDir is ' + finalSubDir + '.' + constLineBreak +
              'This probably means that you are trying to do a release and tag operation with stale (unbumped) version numbers.');
            
end; { end CreateRelOrTagSubDirs() }


{***************************************************************************
 * function CleanupRelAndTag()
 *  Cleanup (svn revert) in top level releases/ and tags/.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CleanupRelAndTag(scriptsPath : TDynamicString;
                          projectPath  : TDynamicString;
                          )            : Integer;
var
   rc               : Integer;
   projTopRelPath   : TDynamicString;
   projTopTagPath   : TDynamicString;

begin 

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*In CleanupRelAndTag().');
   
   { Construct top level paths in releases/ and tags/, based on the full project path. }
   projTopRelPath := StringReplace(projectPath, ('\' + constSvnDirTrunk + '\'), ('\' + constSvnDirReleases + '\'), '');
   projTopTagPath := StringReplace(projectPath, ('\' + constSvnDirTrunk + '\'), ('\' + constSvnDirTags + '\'), '');

   {*** Attempt to revert any svn adds in releases/ or tags/ that may have been left behind by a crashed script run. ***}
   DoSvnRevert(scriptsPath,
               projectPath,
               projTopRelPath,
               projTopTagPath);

end; { end CleanupRelAndTag() }


{***************************************************************************
 * function CreateAllRelAndTagSubDirs()
 *  For each OutJob, create desired subdirs in both releases/ and tags/.
 *
 *  Returns list of new subdirs created in var parm newSubDirs.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CreateAllRelAndTagSubDirs(    scriptsPath     : TDynamicString;
                                       projectPath     : TDynamicString;
                                       relAndTagSubDir : TStringList;
                                       runRelAndTag    : TStringList;
                                   var newSubDirs      : TStringList;
                                       )               : Integer;
var
   rc               : Integer;
   i                : Integer;
   finalSubDir      : TDynamicString;

begin 

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*In CreateAllRelAndTagSubDirs().');
   
   {** For each OutJob, determine if we're supposed to create new subdirs in releases/ and tags/. **}
   { Loop over all OutJobs. }
   for i := 0 to relAndTagSubDir.Count - 1 do
   begin

      { Extract name of desired final rel and tag subdir for this OutJob. }
      finalSubDir := relAndTagSubDir.Strings[i];

      { Only create rel and tag subdirs for this OutJob if the finalSubDir is non-NULL,
       and if we've been flagged to do so from GUI form. }
      if ( (finalSubDir <> '') and 
          (StrToBool(runRelAndTag[i])) ) then
      begin

         WriteToDebugFile('*About to create final relAndTagSubDir ' + finalSubDir + ' in both ' + constSvnDirReleases + '/ and ' + constSvnDirTags + '/.');

         { We must append a '\' to finalSubDir so that simplistic parsing code actually works. }
         finalSubDir := (finalSubDir + '\');
         
         {** Make subdirectories in releases/. **}
         CreateRelOrTagSubDirs(scriptsPath,
                               projectPath,
                               finalSubDir,
                               constSvnDirReleases,
                               {var} newSubDirs);

         
         {** Make subdirectories in tags/. **}
         CreateRelOrTagSubDirs(scriptsPath,
                               projectPath,
                               finalSubDir,
                               constSvnDirTags,
                               {var} newSubDirs);

      end; { endif }

   end; { endfor }
   
end; { end CreateAllRelAndTagSubDirs() }


{***************************************************************************
 * function PopulateAllSubDirsInReleases()
 *  For each OutJob, copy release zipfile to new subdir in releases/.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function PopulateAllSubDirsInReleases(scriptsPath     : TDynamicString;
                                      projectPath     : TDynamicString;
                                      projOutPath     : TString;
                                      projOutSubDirs  : TStringList;
                                      zipFileNames    : TStringList;
                                      relAndTagSubDir : TStringList;
                                      runRelAndTag    : TStringList;
                                      )               : Integer;

var
   rc             : Integer;
   i              : Integer;
   subDir         : TDynamicString;
   finalSubDir    : TDynamicString;
   zipName        : TDynamicString;
   projTopRelPath : TDynamicString;
   parms          : TStringList;

begin 

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*In PopulateAllSubDirsInReleases().');

   { Construct top level path in releases/, based on the full project path. }
   projTopRelPath := StringReplace(projectPath, ('\' + constSvnDirTrunk + '\'), ('\' + constSvnDirReleases + '\'), '');

   {** For each OutJob, determine if we're supposed to copy zipfile to that OutJob's new subdir in releases/. **}
   { Loop over all OutJobs. }
   for i := 0 to relAndTagSubDir.Count - 1 do
   begin

      { Extract name of ProjectOutputs subdir for this OutJob. }
      subDir := projOutSubDirs.Strings[i];
      
      { Extract name of zipfile for this OutJob. }
      zipName := zipFileNames.Strings[i];
      
      { Extract name of desired final rel and tag subdir for this OutJob. }
      finalSubDir := relAndTagSubDir.Strings[i];

      { Only copy zipfile for this OutJob if the finalSubDir is non-NULL,
       and if we've been flagged to do so from GUI form. }
      if ( (finalSubDir <> '') and 
          (StrToBool(runRelAndTag[i])) ) then
      begin

         WriteToDebugFile('*About to copy zipfile ' + zipName + ' to relAndTagSubDir ' + finalSubDir + ' in ' + constSvnDirReleases + '/.');

         { Initialize list of svn parms. }
         parms := TStringList.Create;

         { Specify zipfile as the source for this svn copy. }
         { Note that this will be a relative path with respect to ProjectOutputs\, but this is ok,
          since we will specify projOutPath as the working directory when doing the svn copy command. }
         parms.Add(subDir + '\' + zipName);

         { Specify new subdir in releases/ as the destination for this svn copy. }
         parms.Add(projTopRelPath + '\' + finalSubDir);
         
         { Issue svn copy command to copy new zipfiles to releases directory. }
         IssueSvnCommand(scriptsPath,
                         projOutPath,
                         constSvnCmdCopy,
                         parms);

         { Free list of parms to svn copy. }
         parms.Free;

      end; { endif }

   end; { endfor }

end; { end PopulateAllSubDirsInReleases() }
   

{***************************************************************************
 * function PopulateAllSubDirsInTags()
 *  For each OutJob, perform svn server side copy-with-commit to copy snapshot
 *  of project to new subdir in tags/.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function PopulateAllSubDirsInTags(scriptsPath     : TDynamicString;
                                  projectPath     : TDynamicString;
                                  relAndTagSubDir : TStringList;
                                  runRelAndTag    : TStringList;
                                  )               : Integer;
var
   rc             : Integer;
   i              : Integer;
   projTopTagPath : TDynamicString;
   projTagPath    : TDynamicString;
   finalSubDir    : TDynamicString;
   parms          : TStringList;
   projectPathUrl : TDynamicString;
   projTagPathUrl : TDynamicString;

begin 

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*In PopulateAllSubDirsInTags().');

   { Construct top level path in, based on the full project path. }
   projTopTagPath := StringReplace(projectPath, ('\' + constSvnDirTrunk + '\'), ('\' + constSvnDirTags + '\'), '');

      
   {** Determine svn server URL for trunk/ working directory. **}
   { Issue svn info command to query for svn server URL for project directory. }
   GetFileOrDirSvnServerUrl(scriptsPath,
                            projectPath,
                            projectPath,
                            {var} projectPathUrl);
   

   {** For each OutJob, determine if we're supposed to copy project snapshot to that OutJob's new subdir in tags/. **}
   { Loop over all OutJobs. }
   for i := 0 to relAndTagSubDir.Count - 1 do
   begin

      { Extract name of desired final tag subdir for this OutJob. }
      finalSubDir := relAndTagSubDir.Strings[i];

      { Only copy zipfile for this OutJob if the finalSubDir is non-NULL,
       and if we've been flagged to do so from GUI form. }
      if ( (finalSubDir <> '') and 
          (StrToBool(runRelAndTag[i])) ) then
      begin

         WriteToDebugFile('*About to copy project snapshot to relAndTagSubDir ' + finalSubDir + ' in ' + constSvnDirTags + '/.');

         { Construct full path of final tag subdir for this OutJob. }
         projTagPath := (projTopTagPath + '\' + finalSubDir);
         
         {** Determine svn server URL for new directory we just created in tags/. **}
         { Issue svn info command to query for svn server URL for project directory. }
         GetFileOrDirSvnServerUrl(scriptsPath,
                                  projectPath,
                                  projTagPath,
                                  {var} projTagPathUrl);
         
         {** Prepare to do server side svn-copy-with-commit. **}
         { Initialize list of svn parms. }
         parms := TStringList.Create;

         { Specify URL of project trunk/ directory as the source for this svn copy. }
         parms.Add(projectPathUrl);

         { Specify new subdir in tags/ as the destination for this svn copy. }
         parms.Add(projTagPathUrl);
         
         { Set commit message. }
         parms.Add('-m');
         parms.Add('Performing server side copy-with-commit to copy snapshot of ' + constSvnDirTrunk + '/ working directory to new subdir in ' + constSvnDirTags + '/.  This is an automated checkin performed by script ' + constThisScriptName + '.');
         
         {** Issue svn server side copy-with-commit command to copy project snapshot to new subdir in tags/. **}
         { Make sure we're not in a debugging mode where we aren't allowed to do the svn commits. }
         { Note that since this is a server side copy, it includes the commit operation as well. }
         if (enableSvnCommits = True) then
         begin
            IssueSvnCommand(scriptsPath,
                            projectPath,
                            constSvnCmdCopy,
                            parms);
         end;
         
         { Free list of parms to svn copy. }
         parms.Free;
         
      end; { endif }

   end; { endfor }

end; { end PopulateAllSubDirsInTags() }


{***************************************************************************
 * function InitScript()
 *  Do a bunch of standard initialization that is needed by all scripts.
 *
 *  Note:  You MUST examine return code from this function and do "Exit;" if
 *  this function returns non-zero!
 *
 *  Returns various objects and pathnames in var parms listed below.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function InitScript(var Workspace   : IWorkspace;
                    var Project     : IProject;
                    var scriptsPath : TDynamicString;
                    var projectName : TDynamicString;
                    var projectPath : TDynamicString;
                    var projOutPath : TDynamicString;
                    var projLogPath : TDynamicString;
                        )           : Integer;

var
   projectCount : Integer;
   i            : Integer;
   rc           : Integer;

begin 

   { For now, assume/hope/pray that we will succeed. }
   rc := 0;

   { Attempt to get reference to current workspace. }
   Workspace  := GetWorkspace;
   if (Workspace = nil) then
   begin
      ShowError('Unable to find current workspace.');

      { Set error return code. }
      rc := 1;
      result := rc;
      Exit;
   end;
      
   { Attempt to determine which is the currently focused project. }
   Project := Workspace.DM_FocusedProject;
   if (Project = nil) then
   begin
      ShowError('Unable to find current project.');

      { Set error return code. }
      rc := 1;
      result := rc;
      Exit;
   end;

   { Get a count of the number of currently opened projects.  The script project
    from which this script runs must be one of these. }
   projectCount := Workspace.DM_ProjectCount();

   { Loop over all the open projects.  We're looking for the XIA_Altium_scripts
    project (of which we are a part).  Once we find this, we want to record the
    path to the Altium_scripts/ directory on this PC. }
   for i:=0 to projectCount-1 do
   begin

      { Get reference to project # i. }
      Project := Workspace.DM_Projects(i);

      { See if we found our script project. }
      if (AnsiPos(constScriptProjectName, Project.DM_ProjectFullPath) > 0) then
      begin

         { See if the script project was the focused project.
          If so, we will Exit right now, since we can't know which PCB project the user wanted
          to operate on (if there were more than one open). }
         if (Project = Workspace.DM_FocusedProject) then
         begin
            ShowError('Script project was focused project.  You must focus on the desired PCB project before running this script.  Exiting now!!!');
            
            { Set error return code. }
            rc := 1;
            result := rc;
            Exit;
         end;
         
         { ShowMessage('Found my script home!'); }
         scriptsPath := ChangeFileExt(Project.DM_ProjectFullPath,'');

         { Strip off project name to give us just the path. }
         scriptsPath := StringReplace(scriptsPath, '\' + constScriptProjectName,'', MkSet(rfReplaceAll));

      end;
      
   end;

   { Return to focused project. }
   Project := Workspace.DM_FocusedProject;

   { Retrieve the name of the project. }
   { Strip off the extension (eg. ".PRJPCB") }
   projectName := Project.DM_ProjectFileName;
   projectName := ChangeFileExt(projectName,'');

   { Retrieve the project working directory. }
   projectPath := ExtractFilePath(Project.DM_ProjectFullPath);
   
   { Retrieve the name of the ProjectOutputs directory for this project. }
   projOutPath := Project.DM_GetOutputPath;

   { I can't find the proper way to get the path to the ProjectLogs directory.
    So for now, I'm going to kludge it and assume that it's the same as the ProjectOutputs
    directory, with substituting "Logs" for "Outputs".  We'll need to verify this. }
   { TODO:  Figure out proper way to get the name of the ProjectLogs directory for this project! }
   projLogPath := StringReplace(projOutPath, 'Outputs', 'Logs', MkSet(rfReplaceAll));

   { Check for existence of this directory. }
   if (not DirectoryExists(projLogPath)) then
   begin
      ShowError('ProjectLogs directory ' + projLogPath + ' does not exist!');
      
      { Set error return code. }
      rc := 1;
   end;

   { Give return code to caller. }
   result := rc;
   
end; { end InitScript() }


{***************************************************************************
 * procedure DoGenerateAndPackageOutputs()
 *  Now that we have presented the user with a dialog box full of checkboxes,
 *  we know what operations he/she wishes us to perform.  Proceed to do so.
 ***************************************************************************}
procedure DoGenerateAndPackageOutputs(runOutJobs            : TStringList;
                                      runPackager           : TStringList;
                                      runRelAndTag          : TStringList;
                                      flagRequirePcbDocFile : Boolean;
                                      flagCreateZipFiles    : Boolean;
                                      flagDoReleaseAndTag   : Boolean;
                                      );
                                      
var
   Workspace                  : IWorkspace;
   Project                    : IProject;
   Document                   : IDocument;
   projectName                : TDynamicString;
//   projectPath              : TDynamicString;
   projOutPath                : TDynamicString;
   projLogPath                : TDynamicString;
   projFilePath               : TDynamicString;
   projOutSubDirs             : TStringList;
   projOutIncludes            : TStringList;
   outJobFiles                : TStringList;
   outJobPdfContainers        : TStringList;
   outJobPdfEnableNets        : TStringList;
   outJobGenContainers        : TStringList;
   outJobStatusMsg            : TStringList;
   outJobDoSortMultiNetlist   : TStringList;
   outJobSetSvnKeywordsOnBoms : TStringList;
   outJobDoFixIpc356Netlist   : TStringList;
   deleteExcludes             : TStringList;
   zipDoCheckForExisting      : TStringList;
   zipExcludes                : TStringList;
   zipFindAddlFiles           : TStringList;
   zipFileNames               : TStringList;
   relAndTagSubDir            : TStringList;
   sourceFilePaths            : TStringList;
   topLevelSchDoc             : IDocument;
   i                          : Integer;
   scriptsPath                : TDynamicString;
   rc                         : Integer;
   foo                        : TDynamicString;
   bomPath                    : TDynamicString;
   pcbDocPath                 : TDynamicString;
   pcbPartNumAndVersion       : TDynamicString; 
   pcbPartNum                 : TDynamicString; 
   pcbVersion                 : TDynamicString; 
   pcbDocRevNum               : TDynamicString;
   pcbaPartNum                : TDynamicString;
   pcbaVersion                : TDynamicString; 
   bomRevNum                  : TDynamicString;
   newZipFiles                : TStringList;
   newSubDirs                 : TStringList;
   step                       : Integer;
   changesMade                : Boolean;
   projectParms               : TStringList;

begin

   {****** Initialize script. ******}
   { Set to True to actually produce Altium outputs.  Set it to True before checking in this script! }
   { Set to False to test the script without actually producing most outputs. }
   enableGenerateOutputs := True; //False;

   { Set to True to actually do svn commits.  Set it to True before checking in this script! }
   { Set to False to test the script without doing svn commits. }
   enableSvnCommits      := True; //False;

   { Specify that we are running the XIA_Release_Manager script. }
   whichScriptIsThis     := constWhichScriptXrm;
   

   {*** Run standard script initialization routine. ***}
   rc := InitScript({var} Workspace,
                    {var} Project,
                    {var} scriptsPath,
                    {var} projectName,
                    {var} projectPath,
                    {var} projOutPath,
                    {var} projLogPath);

   { Make sure init function succeeded.  If not, we have a _serious_ problem and we need to Exit; now. }
   if (rc <> 0) then
      Exit;

   { Open debug file. }
   OpenDebugFile((projectPath + constThisScriptNameNoExt + '_Debug.txt'));
   WriteToDebugFile('**Script ' + constThisScriptName + ' started at ' + DateTimeToStr(Date) + ' ' + TimeToStr(Now));

   { Open summary file. }
   OpenSummaryFile((projectPath + constThisScriptNameNoExt + '_Summary.txt'));
   WriteToSummaryFile('Actions performed by this script:');
   WriteToSummaryFile('');

   { Populate the strings lists that tell us what OutJob files to run, what subdirs to create, etc. }
   PopulateStringLists({var} projOutSubDirs,
                       {var} projOutIncludes,
                       {var} outJobFiles,
                       {var} outJobPdfContainers,
                       {var} outJobPdfEnableNets,
                       {var} outJobGenContainers,
                       {var} outJobStatusMsg,
                       {var} outJobDoSortMultiNetlist,
                       {var} outJobSetSvnKeywordsOnBoms,
                       {var} outJobDoFixIpc356Netlist,
                       {var} deleteExcludes,
                       {var} zipDoCheckForExisting,
                       {var} zipExcludes,
                       {var} zipFindAddlFiles,
                       {var} zipFileNames,
                       {var} relAndTagSubDir);

   { Perform basic sanity check to make sure that there are the same number of entries in
    all our string lists. }
   if (
       (projOutSubDirs.Count <> projOutIncludes.Count) or
       (projOutSubDirs.Count <> outJobFiles.Count) or
       (projOutSubDirs.Count <> outJobPdfContainers.Count) or
       (projOutSubDirs.Count <> outJobPdfEnableNets.Count) or
       (projOutSubDirs.Count <> outJobGenContainers.Count) or
       (projOutSubDirs.Count <> outJobStatusMsg.Count) or
       (projOutSubDirs.Count <> outJobDoSortMultiNetlist.Count) or
       (projOutSubDirs.Count <> outJobSetSvnKeywordsOnBoms.Count) or
       (projOutSubDirs.Count <> outJobDoFixIpc356Netlist.Count) or
       (projOutSubDirs.Count <> deleteExcludes.Count) or
       (projOutSubDirs.Count <> zipDoCheckForExisting.Count) or      
       (projOutSubDirs.Count <> zipExcludes.Count) or      
       (projOutSubDirs.Count <> zipFindAddlFiles.Count) or     
       (projOutSubDirs.Count <> zipFileNames.Count) or     
       (projOutSubDirs.Count <> relAndTagSubDir.Count) or      
       (projOutSubDirs.Count <> runOutJobs.Count) or
       (projOutSubDirs.Count <> runPackager.Count) or
       (projOutSubDirs.Count <> runRelAndTag.Count) ) then
      MyAbort('Unequal number of entries in the various string lists!');
       
   { Tell debug file what operations we've been requested to do. }
   for i := 0 to runOutJobs.Count - 1 do
   begin
      WriteToDebugFile('* runOutJobs[' + IntToStr(i) + '] is ' + runOutJobs.Strings[i]);
   end;

   for i := 0 to runPackager.Count - 1 do
   begin
      WriteToDebugFile('* runPackager[' + IntToStr(i) + '] is ' + runPackager.Strings[i]);
   end;

   for i := 0 to runRelAndTag.Count - 1 do
   begin
      WriteToDebugFile('* runRelAndTag[' + IntToStr(i) + '] is ' + runRelAndTag.Strings[i]);
   end;
   
   WriteToDebugFile('* flagRequirePcbDocFile is ' + BoolToStr(flagRequirePcbDocFile));
   WriteToDebugFile('* flagCreateZipFiles is ' + BoolToStr(flagCreateZipFiles));
   WriteToDebugFile('* flagDoReleaseAndTag is ' + BoolToStr(flagDoReleaseAndTag));

   
   {****** STEP 0.  Perform sanity checks to make sure project is ready to generate outputs. ******}
   WriteToDebugFile('');
   WriteToDebugFile('**About to perform step 0 sanity checks...');

   { Set to start out at step 0-1. }
   step := 1;


   {*** Verify that scripts and libraries global working copy is up-to-date. ***}
   UpdateGuiStatusMessage('Status:  Starting step 0-' + IntToStr(step) + ':  Checking that scripts and libraries global working copy is up-to-date.');
   CheckThatSvnScriptsWorkingCopyIsUpdated(scriptsPath,
                                           constThisScriptName);
   WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Checked that scripts and libraries global working copy is up-to-date.');
   
   
   {*** Analyze project hierarchy and find top level schematic page. ***}
   sourceFilePaths := TStringList.Create;
   UpdateGuiStatusMessage('Status:  Starting step 0-' + IntToStr(step) + ':  Analyzing project hierarchy.');
   GetSourceFilesAndFindTopLevelSchDoc(Project,
                                       {var} sourceFilePaths,
                                       {var} topLevelSchDoc,
                                       {var} projFilePath);
   WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Analyzed project hierarchy.');

   
   {*** Check all source documents for cleanliness. ***}
   UpdateGuiStatusMessage('Status:  Starting step 0-' + IntToStr(step) + ':  Checking for un-saved source files and closing project documents.');
   CheckForUnsavedSource(sourceFilePaths);
   WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Checked for un-saved source files and closing project documents.');

   
   {*** Check that all source documents are checked in. ***}
   UpdateGuiStatusMessage('Status:  Starting step 0-' + IntToStr(step) + ':  Checking for un-checked-in source files.');
   CheckForUnCheckedInSourceFiles(scriptsPath,
                                  projectPath,
                                  sourceFilePaths);
   WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Checked for un-checked-in source files.');

   
   {*** Get all version numbers and svn revs.  These will later be used to construct zipfile names, etc. ***}
   projectParms := TStringList.Create;
   projectParms.Delimiter := constStringEquals;
   GetPcbAndPcbaPartNumsVersionsAndRevs(Project,
                                        projectName,
                                        scriptsPath,
                                        projectPath,
                                        projOutPath,
                                        projOutSubDirs,
                                        zipFileNames,
                                        flagRequirePcbDocFile,
                                        runPackager,
                                        topLevelSchDoc,
                                        {var} projectParms,
                                        {var} pcbPartNum,
                                        {var} pcbVersion,
                                        {var} pcbDocRevNum,
                                        {var} pcbaPartNum,
                                        {var} pcbaVersion,
                                        {var} step);
   

   { Only do these next steps if the user has requested release-and-tag operations. }
   if (flagDoReleaseAndTag = True) then
   begin
      
      {*** Perform any sanity checks for release and tag operations that we can in advance, so that we fail now rather than later. ***}
      UpdateGuiStatusMessage('Status:  Starting step 0-' + IntToStr(step) + ':  Performing sanity checks for release-and-tag.');
      SanityCheckRelAndTag(scriptsPath,
                           projectPath);
      WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Performed sanity checks for release-and-tag.');

   end; { endif }
   

   {****** STEP 1.  Set things up, generate all output files, and then check them in automagically. ******}
   WriteToDebugFile('');
   WriteToDebugFile('**About to start step 1:  Generate output files...');

   { Set to start out at step 1-1. }
   step := 1;
   
   
   {*** Attempt to revert any svn adds in ProjectOutputs that may have been left behind by a crashed script run. ***}
   UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  Cleaning up ProjectOutputs if needed.');
   DoSvnRevert(scriptsPath,
               projOutPath,
               projOutPath,
               '');   
   WriteToSummaryFile('');
   WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  Cleaned up ProjectOutputs if needed.');

   //   ShowMessage('After step 1-' + IntToStr(step) + ':  Reverting in ProjectOutputs.  If you want to delete some output files to test script''s ability to detect this, do so now.');
   
   
   {*** Attempt to create all output subdirectories (if needed). ***}
   UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  Create or verify existence of all enabled ProjectOutputs subdirectories.');
   CreateAllOutputSubDirs(scriptsPath,
                          projectPath,
                          projOutPath,
                          projOutSubDirs,
                          projOutIncludes);
   WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  Created or verified existence of all enabled ProjectOutputs subdirectories.');
   
   
   {*** Attempt to delete all old output files in enabled ProjectOutputs subdirectories ***}
   if (enableGenerateOutputs = True) then
   begin
      UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  Deleting pre-existing output files in enabled ProjectOutputs subdirectories.');
      DeleteAllOutputFiles(projOutPath,
                           projOutSubDirs,
                           deleteExcludes,
                           runOutJobs);
      WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  Deleted pre-existing output files in enabled ProjectOutputs subdirectories.');
   end

   { Else we're not running for real.  Leave existing outputs alone. }
   else
   begin
      WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  ERROR!  In debug mode!  SKIPPED Deleting pre-existing output files in enabled ProjectOutputs subdirectories.');
   end;

   
   {*** Run all OutJobs scripts to generate all output files. ***}
   GenerateAllOutputs({var} Project,
                      projectName,
                      scriptsPath,
                      projectPath,
                      projOutPath,
                      projOutSubDirs,
                      outJobFiles,
                      outJobPdfContainers,
                      outJobPdfEnableNets,
                      outJobGenContainers,
                      outJobStatusMsg,
                      outJobDoSortMultiNetlist,
                      outJobSetSvnKeywordsOnBoms,
                      outJobDoFixIpc356Netlist,
                      runOutJobs,
                      {var} step);

//   ShowMessage('Finished generating all outputs.  If you want to delete something to test this script, do so now.');

   
   {*** Now that we've generated all our outputs, check for missing outputs. ***}
   UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  Checking for missing outputs in enabled ProjectOutputs subdirectories.');
   CheckForAllMissingOutputFiles(scriptsPath,
                                 projOutPath,
                                 projOutSubDirs);
   WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  Checked for missing outputs in enabled ProjectOutputs subdirectories.');

   
   {*** Add all generated output files to svn ***}
   UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  Adding all outputs in ProjectOutputs subdirectories to svn.');
   AddAllOutputsToSvn(scriptsPath,
                      projectPath,
                      projOutPath,
                      projLogPath,
                      projOutSubDirs,
                      projOutIncludes);
   WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  Added all outputs in ProjectOutputs subdirectories to svn.');
   
   
   {*** Perform last minute sanity checks and tweaks to generated files, prior to svn commit. ***}

   { Ask user to verify that filtering in BOM file is happy. }
   WriteToDebugFile('');
   WriteToDebugFile('**About to ask user to verify that BOM filtering is happy...');
   VerifyBomFilteringIsHappy({var} step);
   
   { Ask user to close .pdf and .xls files, and then verify that this was actually done, before proceeding. }
   WriteToDebugFile('');
   WriteToDebugFile('**About to ask user to close files open in Excel and Acroread, and then verify that this was done...');
   VerifyBomsAndPdfsAreNotFlocked(scriptsPath,
                                  projectPath,
                                  projOutPath,
                                  projOutSubDirs,
                                  {var} step);
   
   { Mark all xls BOM files if we're doing an assembly release. }
   WriteToDebugFile('');
   WriteToDebugFile('**About to mark xls BOM files if we''re doing an assembly release...');
   MarkXlsBomFilesForAssemblyRelease(scriptsPath,
                                     projectPath,
                                     projOutPath,
                                     projOutSubDirs,
                                     runPackager,
                                     pcbaVersion,
                                     {var} step);

   { Set svn property svn:keywords on all xls BOM files. }
   SetSvnPropsForXlsBomFiles(scriptsPath,
                             projOutPath,
                             projOutSubDirs,
                             outJobSetSvnKeywordsOnBoms,
                             runOutJobs,
                             {var} step);
   
   {*** Commit all generated files to svn. ***}

   { Checking in generated files.  When doing so, extract new svn rev number from commit message and save in bomRevNum. }
   WriteToDebugFile('');
   WriteToDebugFile('**About to commit generated files to svn...');

   { Make sure we're not in a debugging mode where we aren't allowed to do the svn commits. }
   bomRevNum := constSubUninitMarker;   { Set BOM svn rev number to be our uninitialized variable marker. }
   if (enableSvnCommits = True) then
   begin
      UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  Committing all outputs in ProjectOutputs subdirectories to svn.');
      CheckinGeneratedFiles(Project,
                            scriptsPath,
                            projectPath,
                            projLogPath,
                            projOutPath,
                            'Automated checkin of Altium generated output files, performed by script ' + constThisScriptName + '.',
                            {var} bomRevNum);
      WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  Committed all outputs in ProjectOutputs subdirectories to svn.');
   end

   else
      WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  ERROR!  In debug mode!  SKIPPED Committing all outputs in ProjectOutputs subdirectories to svn.');

   
   {*** Turn off svn properties for all generated .xls BOM files so that svn rev num embedded in generated xls BOM(s) will no longer change. }
   DelSvnPropsForXlsBomFiles(scriptsPath,
                             projOutPath,
                             projOutSubDirs,
                             outJobSetSvnKeywordsOnBoms,
                             runOutJobs,
                             {var} changesMade,
                             {var} step);
   
   
   {*** Re-commit generated .xls BOM files to svn. ***}
   { Make sure we're not in a debugging mode where we aren't allowed to do the svn commits. }
   { If there were no BOM file(s) generated this time, then we cannot do an empty svn commit. }
   if ( (enableSvnCommits = True) and (changesMade = True) ) then
   begin
      
      UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  Re-committing .xls BOM file(s) to svn.');
      CheckinGeneratedFiles(Project,
                            scriptsPath,
                            projectPath,
                            projLogPath,
                            projOutPath,
                            'Automated checkin of .xls BOM files with properties disabled, performed by script ' + constThisScriptName + '.',
                            {var} foo);
      WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  Re-committed .xls BOM file(s) to svn.');
   end;

   if (changesMade = False) then
   begin
      WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  SKIPPED Re-committing .xls BOM file(s) to svn.');
   end;
      
   if (enableSvnCommits = False) then
   begin
      WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  ERROR!  In debug mode!  SKIPPED Re-committing .xls BOM file(s) to svn.');
   end;


   {****** STEP 2.  Create fabrication and assembly release zipfiles, and then check them in automagically. ******}
   if (flagCreateZipFiles = True) then
   begin

      { Set to start out at step 2-1. }
      step := 1;
   
      {*** Create names for all zipfiles ***}
      UpdateGuiStatusMessage('Status:  Starting step 2-' + IntToStr(step) + ':  Creating names for selected zipfiles.');
      CreateAllZipFileNames(projectName,
                            projOutPath,
                            projOutSubDirs,
                            {var} zipFileNames,
                            {var} relAndTagSubDir,
                            runPackager,
                            pcbPartNum,
                            pcbVersion,
                            pcbDocRevNum,
                            pcbaPartNum,
                            pcbaVersion,
                            bomRevNum);
      WriteToSummaryFile('');
      WriteToSummaryFile('2-' + IntToStr(StepPlusPlus(step)) + '.  Created names for selected zipfiles.');

      
      {*** Attempt to create all zipfiles ***}
      newZipFiles := TStringList.Create;
      UpdateGuiStatusMessage('Status:  Starting step 2-' + IntToStr(step) + ':  Creating selected zipfiles.');
      CreateAllZipFiles(scriptsPath,
                        projOutPath,
                        projOutSubDirs,
                        projOutIncludes,
                        outJobDoFixIpc356Netlist,
                        zipDoCheckForExisting,
                        zipExcludes,
                        zipFindAddlFiles,
                        zipFileNames,
                        runPackager,
                        {var} newZipFiles);
      WriteToSummaryFile('2-' + IntToStr(StepPlusPlus(step)) + '.  Created selected zipfiles.');


      {*** Add all zipfiles to svn ***}
      if (newZipFiles.Count > 0) then
      begin
         UpdateGuiStatusMessage('Status:  Starting step 2-' + IntToStr(step) + ':  Adding zipfiles to svn.');
         IssueSvnCommand(scriptsPath,
                         projOutPath,
                         constSvnCmdAdd,
                         newZipFiles);

         WriteToSummaryFile('2-' + IntToStr(StepPlusPlus(step)) + '.  Added zipfiles to svn.');

      end;

      {*** If needed, bump PCB and PCBA version numbers by modifying project parameter in project file. ***}
      { Make sure we're not in a debugging mode where we aren't allowed to do the svn commits. }
      if (enableSvnCommits = True) then
      begin
         { Combine PCB part number with PCB version number. }
         pcbPartNumAndVersion := (pcbPartNum + '-' + pcbVersion);

         { Call IncrementPcbAndPcbaVersions() to do the real work. }
         IncrementPcbAndPcbaVersions(Project,
                                     projOutSubDirs,
                                     runPackager,
                                     projFilePath,
                                     {var} projectParms,
                                     pcbPartNumAndVersion,
                                     pcbaVersion,
                                     {var} step);
      end { endif }   
      
      else
         WriteToSummaryFile('2-' + IntToStr(StepPlusPlus(step)) + '.  ERROR!  In debug mode!  SKIPPED Incrementing PCB and/or PCBA version numbers by modifying project parameter(s) in project file.');
      

      {*** Commit all generated files to svn. ***}
      { Make sure we're not in a debugging mode where we aren't allowed to do the svn commits. }
      if ( (enableSvnCommits = True) and (newZipFiles.Count > 0) ) then
      begin

         { Add project file to list of files to commit. }
         newZipFiles.Add(projFilePath);

         { Set commit message. }
         newZipFiles.Add('-m');
         newZipFiles.Add('Automated checkin of release zipfiles that we just created (and possibly project file with modified parameters), performed by script ' + constThisScriptName + '.');
         
         UpdateGuiStatusMessage('Status:  Starting step 2-' + IntToStr(step) + ':  Committing zipfiles to svn (in ' + constSvnDirTrunk + '/).');
         IssueSvnCommand(scriptsPath,
                         projOutPath,
                         constSvnCmdCommit,
                         newZipFiles);
         WriteToSummaryFile('2-' + IntToStr(StepPlusPlus(step)) + '.  Committed zipfiles to svn (in ' + constSvnDirTrunk + '/).');

      end { endif }

      else
         WriteToSummaryFile('2-' + IntToStr(StepPlusPlus(step)) + '.  ERROR!  In debug mode!  SKIPPED Committing zipfiles to svn (in ' + constSvnDirTrunk + '/).');

      { Free newZipFiles string list. }
      newZipFiles.Free;

   end; { endif if (flagCreateZipFiles = True) }
   

   {****** STEP 3.  Create directories in tags/ and releases/, copy zipfiles to new dir in releases/,
    and perform svn server side copy to populate new dir in tags/. ******}
   if (flagDoReleaseAndTag = True) then
   begin
      
      { Set to start out at step 3-1. }
      step := 1;
   
      {*** Perform sanity checks and cleanup top level releases/ and tags/ directories. ***}
      UpdateGuiStatusMessage('Status:  Starting step 3-' + IntToStr(step) + ':  Cleaning up ' + constSvnDirReleases + '/ and ' + constSvnDirTags + '/ if needed.');
      CleanupRelAndTag(scriptsPath,
                       projectPath);
      WriteToSummaryFile('');
      WriteToSummaryFile('3-' + IntToStr(StepPlusPlus(step)) + '.  Cleaned up ' + constSvnDirReleases + '/ and ' + constSvnDirTags + '/ if needed.');


      {*** For each OutJob, create desired subdirs in releases/ and tags/. ***}
      { Initialize list of new subdirs that get created and need to be checked into svn. }
      newSubDirs := TStringList.Create;

      UpdateGuiStatusMessage('Status:  Starting step 3-' + IntToStr(step) + ':  Creating new subdirs in ' + constSvnDirReleases + '/ and ' + constSvnDirTags + '/.');
      CreateAllRelAndTagSubDirs(scriptsPath,
                                projectPath,
                                relAndTagSubDir,
                                runRelAndTag,
                                {var} newSubDirs);
      WriteToSummaryFile('3-' + IntToStr(StepPlusPlus(step)) + '.  Created new subdirs in ' + constSvnDirReleases + '/ and ' + constSvnDirTags + '/.');
      

      {*** Add new subdirs in releases/ and tags/ to svn. ***}
      UpdateGuiStatusMessage('Status:  Starting step 3-' + IntToStr(step) + ':  Adding these subdirs to svn.');
      IssueSvnCommand(scriptsPath,
                      projectPath,
                      constSvnCmdAdd,
                      newSubDirs);
      WriteToSummaryFile('3-' + IntToStr(StepPlusPlus(step)) + '.  Added these subdirs to svn.');
                   

      {*** For each OutJob, copy release zipfile to new subdir in releases/. ***}
      UpdateGuiStatusMessage('Status:  Starting step 3-' + IntToStr(step) + ':  Copying release zipfile(s) to new subdir(s) in ' + constSvnDirReleases + '/.');
      PopulateAllSubDirsInReleases(scriptsPath,
                                   projectPath,
                                   projOutPath,
                                   projOutSubDirs,
                                   zipFileNames,
                                   relAndTagSubDir,
                                   runRelAndTag);
      WriteToSummaryFile('3-' + IntToStr(StepPlusPlus(step)) + '.  Copied release zipfile(s) to new subdir(s) in ' + constSvnDirReleases + '/.');


      {*** Commit new subdirs in releases/ and tags/, as well as any copied zipfiles, to svn. ***}
      { Set commit message. }
      newSubDirs.Add('-m');
      newSubDirs.Add('Preparing new subdirs in ' + constSvnDirReleases + '/ and ' + constSvnDirTags + '/ and copying zipfile(s) to said new subdir(s) in ' + constSvnDirReleases +'/.  This is an automated checkin performed by script ' + constThisScriptName + '.');
      
      { Make sure we're not in a debugging mode where we aren't allowed to do the svn commits. }
      if (enableSvnCommits = True) then
      begin
         UpdateGuiStatusMessage('Status:  Starting step 3-' + IntToStr(step) + ':  Committing new subdirs in ' + constSvnDirReleases + '/ and ' + constSvnDirTags + '/ and copies of zipfile(s).');
         IssueSvnCommand(scriptsPath,
                         projectPath,
                         constSvnCmdCommit,
                         newSubDirs);
         WriteToSummaryFile('3-' + IntToStr(StepPlusPlus(step)) + '.  Committed new subdirs in ' + constSvnDirReleases + '/ and ' + constSvnDirTags + '/ and copies of zipfile(s).');
      end

      else
         WriteToSummaryFile('3-' + IntToStr(StepPlusPlus(step)) + '.  ERROR!  In debug mode!  SKIPPED Committing new subdirs in ' + constSvnDirReleases + '/ and ' + constSvnDirTags + '/ and copies of zipfile(s).');
      
      { Free list of subdirectories that have been created. }
      newSubDirs.Free;


      {*** For each OutJob, copy snapshot of project working directory to new subdir in tags/. ***}
      { Make sure we're not in a debugging mode where we aren't allowed to do the svn commits. }
      { Note that since this is a server side copy, it includes the commit operation as well. }
      if (enableSvnCommits = True) then
         UpdateGuiStatusMessage('Status:  Starting step 3-' + IntToStr(step) + ':  Performing server side copy-with-commit to copy snapshot(s) of ' + constSvnDirTrunk + '/ working directory to new subdir(s) in ' + constSvnDirTags + '/.');

      { This function will gate itself against enableSvnCommits, so just always call it. }
      PopulateAllSubDirsInTags(scriptsPath,
                               projectPath,
                               relAndTagSubDir,
                               runRelAndTag);
      
      if (enableSvnCommits = True) then
         WriteToSummaryFile('3-' + IntToStr(StepPlusPlus(step)) + '.  Performed server side copy-with-commit to copy snapshot(s) of ' + constSvnDirTrunk + '/ working directory to new subdir(s) in ' + constSvnDirTags + '/.') { no semicolon }
            
      else
         WriteToSummaryFile('3-' + IntToStr(StepPlusPlus(step)) + '.  ERROR!  In debug mode!  SKIPPED Performing server side copy-with-commit to copy snapshot(s) of ' + constSvnDirTrunk + '/ working directory to new subdir(s) in ' + constSvnDirTags + '/.');

      { Write blank line to end of summary file. }
      WriteToSummaryFile('');
      
   end; { endif if (flagDoReleaseAndTag = True) }

   
   {****** Wrap things up ******}
   { Free the strings lists that were created by PopulateStringLists(). }
   FreeStringLists({var} projOutSubDirs,
                   {var} projOutIncludes,
                   {var} outJobFiles,
                   {var} outJobPdfContainers,
                   {var} outJobPdfEnableNets,
                   {var} outJobGenContainers,
                   {var} outJobStatusMsg,
                   {var} outJobDoSortMultiNetlist,
                   {var} outJobSetSvnKeywordsOnBoms,
                   {var} outJobDoFixIpc356Netlist,
                   {var} deleteExcludes,
                   {var} zipDoCheckForExisting,
                   {var} zipExcludes,
                   {var} zipFindAddlFiles,
                   {var} zipFileNames,
                   {var} relAndTagSubDir);
   
   { Free remaining string lists that we created in this function. }
   sourceFilePaths.Free;
   projectParms.Free;

   { Call AtExit() procedure to write debug outputs to file. }
   WriteToDebugFile('**About to exit script.');
   AtExit(0);                   { Report success at exit }

end; { end DoGenerateAndPackageOutputs() }


{***************************************************************************
 * procedure TGenerateAndPackageOutputsForm.clickedOk()
 *  This is the handler for primary dialog box "OK" click.
 ***************************************************************************}
procedure TGenerateAndPackageOutputsForm.clickedOk(Sender : TPanel);

var
   runOutJobs            : TStringList;
   runPackager           : TStringList;
   runRelAndTag          : TStringList;
   flagRequirePcbDocFile : Boolean;
   flagCreateZipFiles    : Boolean;
   flagDoReleaseAndTag   : Boolean;

begin

//   ShowMessage('Hello world from TGenerateAndPackageOutputsForm.clickedOk()');
   
   { Disable (grey out) OK and Cancel buttons on primary form. }
   formButtonsLabel1.Caption := 'OK.  Proceeding to run script.';
   formButtonsLabel1.Update;
   
   formButtonOk.Enabled := False;
   formButtonOk.Update;

   formButtonCancel.Enabled := False;
   formButtonCancel.Update;

   { For now, assume that our dependencies were checked in real time as the user checked/unchecked
    boxes before clicking on OK. JWC 2011/09/08. }

   
   {** Setup which OutJob files to run. **}
   { Create a string list which describes which OutJob files we want to run. }
   runOutJobs := TStringList.Create;

   { Populate this string list with the checkboxes given to us by the GUI form. }
   runOutJobs.Add(BoolToStr(formCbFlagOp1_1.Checked));
   runOutJobs.Add(BoolToStr(formCbFlagOp1_2.Checked));
   runOutJobs.Add(BoolToStr(formCbFlagOp1_3.Checked));
   runOutJobs.Add(BoolToStr(formCbFlagOp1_4.Checked));
   runOutJobs.Add(BoolToStr(formCbFlagOp1_5.Checked));
   runOutJobs.Add(BoolToStr(formCbFlagOp1_6.Checked));
   

   {** Setup which packaging jobs to run. **}
   { Create a string list which describes which packaging jobs we want to run. }
   runPackager := TStringList.Create;

   { Populate this string list with the checkboxes given to us by the GUI form. }
   runPackager.Add(BoolToStr(False)); //formCbFlagOp2_1.Checked;
   runPackager.Add(BoolToStr(False)); //formCbFlagOp2_2.Checked;
   runPackager.Add(BoolToStr(False)); //formCbFlagOp2_3.Checked;
   runPackager.Add(BoolToStr(formCbFlagOp2_4.Checked));
   runPackager.Add(BoolToStr(formCbFlagOp2_5.Checked));
   runPackager.Add(BoolToStr(formCbFlagOp2_6.Checked));
   
   
   {** Setup which release & tag jobs to run. **}
   { Create a string list which describes which release & tag jobs we want to run. }
   runRelAndTag := TStringList.Create;

   { Populate this string list with the checkboxes given to us by the GUI form. }
   runRelAndTag.Add(BoolToStr(False)); //formCbFlagOp3_1.Checked;
   runRelAndTag.Add(BoolToStr(False)); //formCbFlagOp3_2.Checked;
   runRelAndTag.Add(BoolToStr(False)); //formCbFlagOp3_3.Checked;
   runRelAndTag.Add(BoolToStr(formCbFlagOp3_4.Checked));
   runRelAndTag.Add(BoolToStr(formCbFlagOp3_5.Checked));
   runRelAndTag.Add(BoolToStr(formCbFlagOp3_6.Checked));


   { See if the user has selected to do any steps that require the existence of a PcbDoc (layout) file. }
   flagRequirePcbDocFile  := (formCbFlagOp1_3.Checked);     { "Generate XIA_Layout_Review outputs" }

   
   { See if the user has selected to do any Packaging at all. }
   flagCreateZipFiles  := (
                           False or  {formCbFlagOp2_1.Checked}
                           False or  {formCbFlagOp2_2.Checked}
                           False or  {formCbFlagOp2_3.Checked}
                           formCbFlagOp2_4.Checked or 
                           formCbFlagOp2_5.Checked or 
                           formCbFlagOp2_6.Checked);

   
   { See if the user has selected to do any RelAndTag at all. }
   flagDoReleaseAndTag := (
                           False or  {formCbFlagOp3_1.Checked}
                           False or  {formCbFlagOp3_2.Checked}
                           False or  {formCbFlagOp3_3.Checked}
                           formCbFlagOp3_4.Checked or 
                           formCbFlagOp3_5.Checked or 
                           formCbFlagOp3_6.Checked);
   
   
   { Call DoGenerateAndPackageOutputs to do all the actual work. }
   DoGenerateAndPackageOutputs(runOutJobs,
                               runPackager,
                               runRelAndTag,
                               flagRequirePcbDocFile,
                               flagCreateZipFiles,
                               flagDoReleaseAndTag);

//   ShowMessage('Back from DoGenerateAndPackageOutputs()!');
   
   { Close primary dialog box. }
   Close;

   { Free string lists. }
   runOutJobs.Free;
   runPackager.Free;
   runRelAndTag.Free;

   { Exit script now. }
   Exit;

end; { end TGenerateAndPackageOutputsForm.clickedOk() }


{***************************************************************************
 * procedure TGenerateAndPackageOutputsForm.clickedCancel()
 *  This is the handler for primary dialog box "Cancel" click.
 ***************************************************************************}
procedure TGenerateAndPackageOutputsForm.clickedCancel(Sender : TPanel);

var
   Action : TCloseAction;
        
begin

   { Close dialog box. }
   { TODO:  Figure out why this doesn't actually work! }
   GenerateAndPackageOutputsForm.Close;

   ShowError('Script canceled at User request.');

   { Exit script now. }
   Exit;
   
end; { end TGenerateAndPackageOutputsForm.clickedCancel() }


{***************************************************************************
 * procedure TGenerateAndPackageOutputsForm.bCheck0_1()
 *  This is the handler for unchecking "Run in release mode".
 ***************************************************************************}
procedure TGenerateAndPackageOutputsForm.bCheck0_1(Sender : TPanel);

var
   Action : TCloseAction;
        
begin

   { See if the user just unchecked this box. }
   if (formCbFlagOp0_1.Checked = False) then
      begin
         
         ShowError('Sorry, but for now, only Release Mode is supported by this script.' + constLineBreak +
             'Speak to Jeff to bug him to improve this script.');
         
         { Re-check the checkbox. }
         formCbFlagOp0_1.Checked := True;

      end; { endif }
      
end; { end TGenerateAndPackageOutputsForm.bCheck0_1() }


{***************************************************************************
 ****  DEPENDENCY LIST for STEP 1 (Generate Outputs): ****
 Generate Assembly outputs depends on:
 1.  Generate XIA_Engineers_Review outputs
 2.  Generate XIA_Schematic_Review outputs
 3.  Generate XIA_Layout_Review outputs
 4.  Generate Purchasing outputs

 Generate Fabrication outputs depends on:
 1.  Generate XIA_Engineers_Review outputs
 2.  Generate XIA_Schematic_Review outputs
 3.  Generate XIA_Layout_Review outputs
 
 Generate Purchasing outputs depends on:
 1.  Generate XIA_Engineers_Review outputs
 2.  Generate XIA_Schematic_Review outputs
 
 Generate XIA_Layout_Review outputs depends on:
 1.  Generate XIA_Engineers_Review outputs
 2.  Generate XIA_Schematic_Review outputs
 
 Generate XIA_Schematic_Review outputs depends on:
 1.  Generate XIA_Engineers_Review outputs
 
 Generate XIA_Engineers_Review outputs depends on:
 (nothing)

 ****  DEPENDENCY LIST for STEP 2 (Package Outputs): ****
 Package Assembly outputs depends on:
 1.  Generate Assembly outputs
 2.  Absence of Package Purchasing outputs

 Package Fabrication outputs depends on:
 1.  Generate Fabrication outputs
 
 Package Purchasing outputs depends on:
 1.  Generate Purchasing outputs
 2.  Absence of Package Assembly outputs
 
 ****  DEPENDENCY LIST for STEP 3 (RelAndTag Outputs): ****
 RelAndTag Assembly outputs depends on:
 1.  Package Assembly outputs
 2.  Absence of RelAndTag Purchasing outputs

 RelAndTag Fabrication outputs depends on:
 1.  Package Fabrication outputs
 
 RelAndTag Purchasing outputs depends on:
 1.  Package Purchasing outputs
 2.  Absence of RelAndTag Assembly outputs
 
 ***************************************************************************}

{***************************************************************************
 * procedure GuiHandleUncheckOfRelAndTagPurchasingPrereq()
 *  This is the handler for unchecking a prerequisite of "RelAndTag Purchasing outputs".
 ***************************************************************************}
procedure GuiHandleUncheckOfRelAndTagPurchasingPrereq(foo : Integer);
begin

   { Force RelAndTag Purchasing checkbox to be unchecked. }
   formCbFlagOp3_4.Checked := False;
end;


{***************************************************************************
 * procedure GuiHandleUncheckOfRelAndTagFabricationPrereq()
 *  This is the handler for unchecking a prerequisite of "RelAndTag Fabrication outputs".
 ***************************************************************************}
procedure GuiHandleUncheckOfRelAndTagFabricationPrereq(foo : Integer);
begin

   { Force RelAndTag Fabrication checkbox to be unchecked. }
   formCbFlagOp3_5.Checked := False;
end;


{***************************************************************************
 * procedure GuiHandleUncheckOfRelAndTagAssemblyPrereq()
 *  This is the handler for unchecking a prerequisite of "RelAndTag Assembly outputs".
 ***************************************************************************}
procedure GuiHandleUncheckOfRelAndTagAssemblyPrereq(foo : Integer);
begin

   { Force RelAndTag Assembly checkbox to be unchecked. }
   formCbFlagOp3_6.Checked := False;
end;


{***************************************************************************
 * procedure GuiHandleUncheckOfPackagePurchasingPrereq()
 *  This is the handler for unchecking a prerequisite of "Package Purchasing outputs".
 ***************************************************************************}
procedure GuiHandleUncheckOfPackagePurchasingPrereq(foo : Integer);
begin

   { Force Package Purchasing checkbox to be unchecked. }
   formCbFlagOp2_4.Checked := False;

   { Call existing procedure to handle the rest. }
   GuiHandleUncheckOfRelAndTagPurchasingPrereq(0);

end;


{***************************************************************************
 * procedure GuiHandleUncheckOfPackageFabricationPrereq()
 *  This is the handler for unchecking a prerequisite of "Package Fabrication outputs".
 ***************************************************************************}
procedure GuiHandleUncheckOfPackageFabricationPrereq(foo : Integer);
begin

   { Force Package Fabrication checkbox to be unchecked. }
   formCbFlagOp2_5.Checked := False;

   { Call existing procedure to handle the rest. }
   GuiHandleUncheckOfRelAndTagFabricationPrereq(0);

end;


{***************************************************************************
 * procedure GuiHandleUncheckOfPackageAssemblyPrereq()
 *  This is the handler for unchecking a prerequisite of "Package Assembly outputs".
 ***************************************************************************}
procedure GuiHandleUncheckOfPackageAssemblyPrereq(foo : Integer);
begin

   { Force Package Assembly checkbox to be unchecked. }
   formCbFlagOp2_6.Checked := False;

   { Call existing procedure to handle the rest. }
   GuiHandleUncheckOfRelAndTagAssemblyPrereq(0);   
end;


{***************************************************************************
 * procedure GuiHandleUncheckOfGenerateSchematicReviewPrereq()
 *  This is the handler for unchecking a prerequisite of "Generate SchematicReview outputs".
 ***************************************************************************}
procedure GuiHandleUncheckOfGenerateSchematicReviewPrereq(foo : Integer);
begin

   { Force Generate SchematicReview checkbox to be unchecked. }
   formCbFlagOp1_2.Checked := False;
end;


{***************************************************************************
 * procedure GuiHandleUncheckOfGenerateLayoutReviewPrereq()
 *  This is the handler for unchecking a prerequisite of "Generate LayoutReview outputs".
 ***************************************************************************}
procedure GuiHandleUncheckOfGenerateLayoutReviewPrereq(foo : Integer);
begin

   { Force Generate LayoutReview checkbox to be unchecked. }
   formCbFlagOp1_3.Checked := False;
end;


{***************************************************************************
 * procedure GuiHandleUncheckOfGeneratePurchasingPrereq()
 *  This is the handler for unchecking a prerequisite of "Generate Purchasing outputs".
 ***************************************************************************}
procedure GuiHandleUncheckOfGeneratePurchasingPrereq(foo : Integer);
begin

   { Force Generate Purchasing checkbox to be unchecked. }
   formCbFlagOp1_4.Checked := False;
end;


{***************************************************************************
 * procedure GuiHandleUncheckOfGenerateFabricationPrereq()
 *  This is the handler for unchecking a prerequisite of "Generate Fabrication outputs".
 ***************************************************************************}
procedure GuiHandleUncheckOfGenerateFabricationPrereq(foo : Integer);
begin

   { Force Generate Fabrication checkbox to be unchecked. }
   formCbFlagOp1_5.Checked := False;

   { Call existing procedure to handle the rest. }
   GuiHandleUncheckOfPackageFabricationPrereq(0);
end;


{***************************************************************************
 * procedure GuiHandleUncheckOfGenerateAssemblyPrereq()
 *  This is the handler for unchecking a prerequisite of "Generate Assembly outputs".
 ***************************************************************************}
procedure GuiHandleUncheckOfGenerateAssemblyPrereq(foo : Integer);
begin

   { Force Generate Assembly checkbox to be unchecked. }
   formCbFlagOp1_6.Checked := False;

   { Call existing procedure to handle the rest. }
   GuiHandleUncheckOfPackageAssemblyPrereq(0);
end;


{***************************************************************************
 * procedure TGenerateAndPackageOutputsForm.bCheck1_1()
 *  This is the handler for checking/unchecking 'Generate XIA_Engineers_Review outputs'.
 ***************************************************************************}
procedure TGenerateAndPackageOutputsForm.bCheck1_1(Sender : TPanel);
begin

   { See if the user just checked this box. }
   if (formCbFlagOp1_1.Checked = True) then
   begin
      { Force pre-requisites to be checked. }
   end
   
   { Else the user just unchecked this box. }
   else
   begin
      { Handle dependencies that depend on this operation. }
      GuiHandleUncheckOfGenerateSchematicReviewPrereq(0);
      GuiHandleUncheckOfGenerateLayoutReviewPrereq(0);
      GuiHandleUncheckOfGeneratePurchasingPrereq(0);
      GuiHandleUncheckOfGenerateFabricationPrereq(0);
      GuiHandleUncheckOfGenerateAssemblyPrereq(0);

   end; { endelse }
   
end; { end TGenerateAndPackageOutputsForm.bCheck1_1() }


{***************************************************************************
 * procedure TGenerateAndPackageOutputsForm.bCheck1_2()
 *  This is the handler for checking/unchecking 'Generate XIA_Schematic_Review outputs'.
 ***************************************************************************}
procedure TGenerateAndPackageOutputsForm.bCheck1_2(Sender : TPanel);
begin

   { See if the user just checked this box. }
   if (formCbFlagOp1_2.Checked = True) then
   begin
      { Force pre-requisites to be checked. }
      formCbFlagOp1_1.Checked := True;
   end
   
   { Else the user just unchecked this box. }
   else
   begin
      { Handle dependencies that depend on this operation. }
      GuiHandleUncheckOfGenerateLayoutReviewPrereq(0);
      GuiHandleUncheckOfGeneratePurchasingPrereq(0);
      GuiHandleUncheckOfGenerateFabricationPrereq(0);
      GuiHandleUncheckOfGenerateAssemblyPrereq(0);

   end; { endelse }
      
end; { end TGenerateAndPackageOutputsForm.bCheck1_2() }


{***************************************************************************
 * procedure TGenerateAndPackageOutputsForm.bCheck1_3()
 *  This is the handler for checking/unchecking 'Generate XIA_Layout_Review outputs'.
 ***************************************************************************}
procedure TGenerateAndPackageOutputsForm.bCheck1_3(Sender : TPanel);
begin

   { See if the user just checked this box. }
   if (formCbFlagOp1_3.Checked = True) then
   begin
      { Force pre-requisites to be checked. }
      formCbFlagOp1_1.Checked := True;
      formCbFlagOp1_2.Checked := True;
   end
   
   { Else the user just unchecked this box. }
   else
   begin
      
      { Handle dependencies that depend on this operation. }
      GuiHandleUncheckOfGenerateFabricationPrereq(0);
      GuiHandleUncheckOfGenerateAssemblyPrereq(0);
      
   end; { endelse }
      
end; { end TGenerateAndPackageOutputsForm.bCheck1_3() }


{***************************************************************************
 * procedure TGenerateAndPackageOutputsForm.bCheck1_4()
 *  This is the handler for checking/unchecking 'Generate Purchasing outputs'.
 ***************************************************************************}
procedure TGenerateAndPackageOutputsForm.bCheck1_4(Sender : TPanel);
begin

   { See if the user just checked this box. }
   if (formCbFlagOp1_4.Checked = True) then
   begin
      { Force pre-requisites to be checked. }
      formCbFlagOp1_1.Checked := True;
      formCbFlagOp1_2.Checked := True;
   end
   
   { Else the user just unchecked this box. }
   else
   begin
      
      { Handle dependencies that depend on this operation. }
      GuiHandleUncheckOfGenerateAssemblyPrereq(0);
      GuiHandleUncheckOfPackagePurchasingPrereq(0);
      
   end; { endelse }
      
end; { end TGenerateAndPackageOutputsForm.bCheck1_4() }


{***************************************************************************
 * procedure TGenerateAndPackageOutputsForm.bCheck1_5()
 *  This is the handler for checking/unchecking "Generate Fabrication outputs".
 ***************************************************************************}
procedure TGenerateAndPackageOutputsForm.bCheck1_5(Sender : TPanel);
begin

   { See if the user just checked this box. }
   if (formCbFlagOp1_5.Checked = True) then
   begin
      { Force pre-requisites to be checked. }
      formCbFlagOp1_1.Checked := True;
      formCbFlagOp1_2.Checked := True;
      formCbFlagOp1_3.Checked := True;
   end
   
   { Else the user just unchecked this box. }
   else
   begin
      
      { Handle dependencies that depend on this operation. }
      GuiHandleUncheckOfPackageFabricationPrereq(0);
   end; { endelse }
      
end; { end TGenerateAndPackageOutputsForm.bCheck1_5() }


{***************************************************************************
 * procedure TGenerateAndPackageOutputsForm.bCheck1_6()
 *  This is the handler for checking/unchecking "Generate Assembly outputs".
 ***************************************************************************}
procedure TGenerateAndPackageOutputsForm.bCheck1_6(Sender : TPanel);
begin

   { See if the user just checked this box. }
   if (formCbFlagOp1_6.Checked = True) then
   begin
      { Force pre-requisites to be checked. }
      formCbFlagOp1_1.Checked := True;
      formCbFlagOp1_2.Checked := True;
      formCbFlagOp1_3.Checked := True;
      formCbFlagOp1_4.Checked := True;
   end
   
   { Else the user just unchecked this box. }
   else
   begin
      
      { Handle dependencies that depend on this operation. }
      GuiHandleUncheckOfPackageAssemblyPrereq(0);
   end; { endelse }
      
end; { end TGenerateAndPackageOutputsForm.bCheck1_6() }


{***************************************************************************
 * procedure TGenerateAndPackageOutputsForm.bCheck2_4()
 *  This is the handler for checking/unchecking "Package Purchasing outputs".
 ***************************************************************************}
procedure TGenerateAndPackageOutputsForm.bCheck2_4(Sender : TPanel);
begin

   { See if the user just checked this box. }
   if (formCbFlagOp2_4.Checked = True) then
   begin
      { Force pre-requisites to be checked. }
      formCbFlagOp1_4.Checked := True;

      { Force conflicting operations to be un-checked. }
      GuiHandleUncheckOfPackageAssemblyPrereq(0);
   end
   
   { Else the user just unchecked this box. }
   else
   begin
      { Handle dependencies that depend on this operation. }
      GuiHandleUncheckOfRelAndTagPurchasingPrereq(0);

   end; { endelse }
   
end; { end TGenerateAndPackageOutputsForm.bCheck2_4() }


{***************************************************************************
 * procedure TGenerateAndPackageOutputsForm.bCheck2_5()
 *  This is the handler for checking/unchecking "Package Fabrication outputs".
 ***************************************************************************}
procedure TGenerateAndPackageOutputsForm.bCheck2_5(Sender : TPanel);
begin

   { See if the user just checked this box. }
   if (formCbFlagOp2_5.Checked = True) then
   begin
      { Force pre-requisites to be checked. }
      formCbFlagOp1_5.Checked := True;

      { Turn on text describing preconditions for running package fabrication. }
      GuiEnablePackageFabricationPreconditions(0);
      
   end
   
   { Else the user just unchecked this box. }
   else
   begin
      { Handle dependencies that depend on this operation. }
      GuiHandleUncheckOfRelAndTagFabricationPrereq(0);

      { Turn off text describing preconditions for running package fabrication. }
      GuiDisablePackageFabricationPreconditions(0);
      
   end; { endelse }
   
end; { end TGenerateAndPackageOutputsForm.bCheck2_5() }


{***************************************************************************
 * procedure TGenerateAndPackageOutputsForm.bCheck2_6()
 *  This is the handler for checking/unchecking "Package Assembly outputs".
 ***************************************************************************}
procedure TGenerateAndPackageOutputsForm.bCheck2_6(Sender : TPanel);
begin

   { See if the user just checked this box. }
   if (formCbFlagOp2_6.Checked = True) then
   begin
      { Force pre-requisites to be checked. }
      formCbFlagOp1_6.Checked := True;

      { Force conflicting operations to be un-checked. }
      GuiHandleUncheckOfPackagePurchasingPrereq(0);

      { Turn on text describing preconditions for running package assembly. }
      GuiEnablePackageAssemblyPreconditions(0);
      
   end
   
   { Else the user just unchecked this box. }
   else
   begin
      { Handle dependencies that depend on this operation. }
      GuiHandleUncheckOfRelAndTagAssemblyPrereq(0);

      { Turn off text describing preconditions for running package assembly. }
      GuiDisablePackageAssemblyPreconditions(0);
      
   end; { endelse }
   
end; { end TGenerateAndPackageOutputsForm.bCheck2_6() }


{***************************************************************************
 * procedure TGenerateAndPackageOutputsForm.bCheck3_4()
 *  This is the handler for checking/unchecking "RelAndTag Purchasing outputs".
 ***************************************************************************}
procedure TGenerateAndPackageOutputsForm.bCheck3_4(Sender : TPanel);
begin

   { See if the user just checked this box. }
   if (formCbFlagOp3_4.Checked = True) then
   begin
      { Force pre-requisites to be checked. }
      formCbFlagOp2_4.Checked := True;

      { Force conflicting operations to be un-checked. }
      GuiHandleUncheckOfPackageAssemblyPrereq(0);
   end
   
   { Else the user just unchecked this box. }
   else
   begin
      { Handle dependencies that depend on this operation. }
   end; { endelse }
   
end; { end TGenerateAndPackageOutputsForm.bCheck3_4() }


{***************************************************************************
 * procedure TGenerateAndPackageOutputsForm.bCheck3_5()
 *  This is the handler for checking/unchecking "RelAndTag Fabrication outputs".
 ***************************************************************************}
procedure TGenerateAndPackageOutputsForm.bCheck3_5(Sender : TPanel);
begin

   { See if the user just checked this box. }
   if (formCbFlagOp3_5.Checked = True) then
   begin
      { Force pre-requisites to be checked. }
      formCbFlagOp2_5.Checked := True;
   end
   
   { Else the user just unchecked this box. }
   else
   begin
      { Handle dependencies that depend on this operation. }
   end; { endelse }
   
end; { end TGenerateAndPackageOutputsForm.bCheck3_5() }


{***************************************************************************
 * procedure TGenerateAndPackageOutputsForm.bCheck3_6()
 *  This is the handler for checking/unchecking "RelAndTag Assembly outputs".
 ***************************************************************************}
procedure TGenerateAndPackageOutputsForm.bCheck3_6(Sender : TPanel);
begin

   { See if the user just checked this box. }
   if (formCbFlagOp3_6.Checked = True) then
   begin
      { Force pre-requisites to be checked. }
      formCbFlagOp2_6.Checked := True;

      { Force conflicting operations to be un-checked. }
      GuiHandleUncheckOfPackagePurchasingPrereq(0);   
   end
   
   { Else the user just unchecked this box. }
   else
   begin
      { Handle dependencies that depend on this operation. }
   end; { endelse }
   
end; { end TGenerateAndPackageOutputsForm.bCheck3_6() }


{***************************************************************************
 * procedure GuiEnablePackageFabricationPreconditions()
 *  This procedure will enable (un-gray-out) the text describing the Package Fabrication
 *  preconditions.
 ***************************************************************************}
procedure GuiEnablePackageFabricationPreconditions(foo : Integer);
begin

   { Turn on text describing preconditions for running package fabrication. }
   formText18.Enabled := True;
   formText19.Enabled := True;
   formText20.Enabled := True;
   formText21.Enabled := True;
   formText22.Enabled := True;
   formText23.Enabled := True;
   formText24.Enabled := True;
   
end; 
   

{***************************************************************************
 * procedure GuiDisablePackageFabricationPreconditions()
 *  This procedure will disable (gray-out) the text describing the Package Fabrication
 *  preconditions.
 ***************************************************************************}
procedure GuiDisablePackageFabricationPreconditions(foo : Integer);
begin

   { Turn off text describing preconditions for running package fabrication. }
   formText18.Enabled := False;
   formText19.Enabled := False;
   formText20.Enabled := False;
   formText21.Enabled := False;
   formText22.Enabled := False;
   formText23.Enabled := False;
   formText24.Enabled := False;
   
end; 


{***************************************************************************
 * procedure GuiEnablePackageAssemblyPreconditions()
 *  This procedure will enable (un-gray-out) the text describing the Package Assembly
 *  preconditions.
 ***************************************************************************}
procedure GuiEnablePackageAssemblyPreconditions(foo : Integer);
begin

   { Turn on text describing preconditions for running package assembly. }
   formText25.Enabled := True;
   formText26.Enabled := True;
   formText27.Enabled := True;
   formText28.Enabled := True;
   formText29.Enabled := True;
   formText30.Enabled := True;
   formText31.Enabled := True;
   formText32.Enabled := True;
   formText33.Enabled := True;
   
end; 
   

{***************************************************************************
 * procedure GuiDisablePackageAssemblyPreconditions()
 *  This procedure will disable (gray-out) the text describing the Package Assembly
 *  preconditions.
 ***************************************************************************}
procedure GuiDisablePackageAssemblyPreconditions(foo : Integer);
begin

   { Turn off text describing preconditions for running package assembly. }
   formText25.Enabled := False;
   formText26.Enabled := False;
   formText27.Enabled := False;
   formText28.Enabled := False;
   formText29.Enabled := False;
   formText30.Enabled := False;
   formText31.Enabled := False;
   formText32.Enabled := False;
   formText33.Enabled := False;
   
end; 


{***************************************************************************
 * procedure XIA_Release_Manager()
 *  This is the entry point for this script file.
 ***************************************************************************}
procedure XIA_Release_Manager;

//var

begin

   { Override GUI text entries. }
   { Note:  Don't condense this onto fewer lines or you will have svn replacing $Revision.*$
    with svn rev number on checkin! }
   GenerateAndPackageOutputsForm.Caption := 'Welcome to ' + constThisScriptNameNoExt + ' ' +
   StringReplace(StringReplace(constScriptVersion, '$Revision:', ', svn rev', ''),
                 ' $', '', '') + ' script main menu!';
   
   { Override GUI text entries. }
   formText01.Caption := 'This script will perform many of the generate output related steps that you previously had to do manually.  Requires Altium 10.';
   formText01.Font.Style := MkSet(fsBold);
   formText02.Caption := '';

   formText03.Caption := 'Pre-conditions that must be satisfied before running this script (that you''ve seen before):';
   formText03.Font.Style := MkSet(fsBold);
   formText04.Caption := '1.  You must have customized your OutJob scripts to reflect the layers actually used in your PcbDoc file.';
   formText05.Caption := '2.  You must have customized your OutJob scripts to reflect the variants that exist in your design.';
   formText06.Caption := '3.  You must have fixed the filtering of the .xls BOM file(s) if/when it breaks.';
   formText07.Caption := '';

   formText08.Caption := 'Pre-conditions that must be satisfied before running this script (that are new for this script and Altium 10):';
   formText08.Font.Style := MkSet(fsBold);
   formText09.Caption := '4.  You must have installed the HP Designjet T2300ps PS3 printer driver and set it as your Windows default printer prior to starting Altium.';
   formText10.Caption := '5.  You must be using the 6 OutJob scripts now required for use with this script (customized for your project from template OutJobs).';
   formText11.Caption := '6.  You must have created new project level parameters to store target (human readable) version numbers for fabrication and assembly releases.';
   formText12.Caption := '7.  You must have updated and moved Gerber Title Block component to top level sch file and deleted former Global Note comp from top level sch file.';
   formText13.Caption := '';
   
   formText14.Caption := 'Additional pre-Conditions to generate files for XIA review and/or purchasing:';
   formText14.Font.Style := MkSet(fsBold);
   formText15.Caption := '1.  You must have recently run XIA_Update_From_Database script (formerly 3 separate update steps) to update schematic design with respect to database.';
   formText16.Caption := '2.  You must have checked in all source documents (.SchDoc, .PcbDoc, .OutJob, .PRJPCB, etc.).';
   formText17.Caption := '';

   { Note:  Be sure these lines of text agree with code to enable/disable them, above. }
   formText18.Caption := 'Additional pre-Conditions to package fabrication outputs (Check by "Package (zip) Fabrication outputs"):';
   formText18.Font.Style := MkSet(fsBold);
   formText19.Caption := '1.  Update APCB1 and Gerber Title Block comp in top level sch file to match target PCB version number specified in project level parameters.';
   formText20.Caption := '2.  If desired, update text in silkscreen note in top level sch file to update project text.';
   formText21.Caption := '3.  Update PCB design (forward annotate)';
   formText22.Caption := '4.  Save your project .PcbDoc layout file.';
   formText23.Caption := '5.  Checkin .PcbDoc file, and any schematic changes.  Note resulting SVN rev #.';
   formText24.Caption := '';

   { Turn off text describing preconditions for running package fabrication. }
   GuiDisablePackageFabricationPreconditions(0);

   { Note:  Be sure these lines of text agree with code to enable/disable them, above. }
   formText25.Caption := 'Additional pre-Conditions to package assembly outputs (Check by "Package (zip) Assembly outputs"):';
   formText25.Font.Style := MkSet(fsBold);
   formText26.Caption := '1.  Get Database Librarian (Jeff) to update the database component corresponding to APCB1 to reflect human readable PCB version number and SVN rev # of the PCB fab for this project.';
   formText27.Caption := '2.  Close project and/or shut down Altium.';
   formText28.Caption := '3.  Run SVN update against the XIA_Components_Database/ALTIUM_LIBRARIES working copy.';
   formText29.Caption := '4.  Re-open project and/or restart Altium.';
   formText30.Caption := '5.  Run XIA_Update_From_Database script (formerly "Update Parameters from Database..." operation) to get this last update into your schematic.  Follow wiki instructions for this process.';
   formText31.Caption := '6.  Save schematic changes (to TOP LEVEL schematic).  Do NOT run "Update PCB Document" to propagate changes to .PcbDoc file!!  Otherwise we will go around and around!';
   formText32.Caption := '7.  Checkin saved schematic file (eg. TOP_LEVEL.SchDoc).';
   formText33.Caption := '';

   { Turn off text describing preconditions for running package assembly. }
   GuiDisablePackageAssemblyPreconditions(0);

   formText34.Caption := 'Choose the actions you wish to perform automagically:';
   formText34.Font.Style := MkSet(fsBold);

   { Override GUI text entries. }
   formButtonsLabel1.Caption := 'Is it OK to proceed with Altium output file generation, or shall I cancel?';
   formStatusBar1.SimpleText := 'Status:  Awaiting user selections prior to starting script.';
   formStatusBar1.Update;
   
   { Override GUI check box labels. }
   formCbFlagOp0_1.Caption := 'Run in Release Mode';

   { Override GUI check box labels. }
   formCbFlagOp1_1.Caption := 'Generate XIA_Engineers_Review outputs';
   formCbFlagOp1_2.Caption := 'Generate XIA_Schematic_Review outputs';
   formCbFlagOp1_3.Caption := 'Generate XIA_Layout_Review outputs';
   formCbFlagOp1_4.Caption := 'Generate Purchasing outputs';
   formCbFlagOp1_5.Caption := 'Generate Fabrication outputs';
   formCbFlagOp1_6.Caption := 'Generate Assembly outputs';

   { Override GUI check box labels. }
   formCbFlagOp2_1.Caption := 'Package (zip) XIA_Engineers_Review outputs';
   formCbFlagOp2_2.Caption := 'Package (zip) XIA_Schematic_Review outputs';
   formCbFlagOp2_3.Caption := 'Package (zip) XIA_Layout_Review outputs';
   formCbFlagOp2_4.Caption := 'Package (zip) Purchasing outputs';
   formCbFlagOp2_5.Caption := 'Package (zip) Fabrication outputs';
   formCbFlagOp2_6.Caption := 'Package (zip) Assembly outputs';

   { Override GUI check box labels. }
   formCbFlagOp3_1.Caption := 'Do Release and Tag for XIA_Engineers_Review zipfile';
   formCbFlagOp3_2.Caption := 'Do Release and Tag for XIA_Schematic_Review zipfile';
   formCbFlagOp3_3.Caption := 'Do Release and Tag for XIA_Layout_Review zipfile';
   formCbFlagOp3_4.Caption := 'Do Release and Tag for Purchasing zipfile';
   formCbFlagOp3_5.Caption := 'Do Release and Tag for Fabrication zipfile';
   formCbFlagOp3_6.Caption := 'Do Release and Tag for Assembly zipfile';

   { Delete permanently unavailable (won't be implemented) checkboxes in GUI dialog box. }
   formCbFlagOp2_1.Free; { 'Package (zip) XIA_Engineers_Review outputs' }
   formCbFlagOp2_2.Free; { 'Package (zip) XIA_Schematic_Review outputs' }
   formCbFlagOp2_3.Free; { 'Package (zip) XIA_Layout_Review outputs' }

   formCbFlagOp3_1.Free; { 'Do Release and Tag for XIA_Engineers_Review zipfile' }
   formCbFlagOp3_2.Free; { 'Do Release and Tag for XIA_Schematic_Review zipfile' }
   formCbFlagOp3_3.Free; { 'Do Release and Tag for XIA_Layout_Review zipfile' }

   { Choose which checkboxes are checked on startup. }
   formCbFlagOp0_1.Checked := True; { 'Run in Release Mode' }
   formCbFlagOp1_1.Checked := True; { 'Generate XIA_Engineers_Review outputs' }
   formCbFlagOp1_2.Checked := True; { 'Generate XIA_Schematic_Review outputs' }
   formCbFlagOp1_3.Checked := True; { 'Generate XIA_Layout_Review outputs' }

   
   { Run GUI dialog box asking what processes the user wants to run. }
   GenerateAndPackageOutputsForm.ShowModal;

   { Note:  Control now passes to one of the handler functions above. }
   
end; { end XIA_Release_Manager() }
