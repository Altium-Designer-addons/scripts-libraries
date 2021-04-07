{***************************************************************************
 SPI_Commit_Release_Tag.pas
    Altium DelphiScript (basically Pascal) that will step in after we have
 finished generating all fabrication or assembly OutJob outputs via Release View.
 We will copy the generated outputs to their normal home in Project_Outputs/,
 create the subdirs in releases/ and tags/, create a zipfile in releases/, and
 take a project snapshot in tags/.
 ***************************************************************************}

{***************************************************************************
 * Sierra Photonics Inc. has derived this file from XIA_Release_Manager.pas.
 *  Original / modified / updated code is subject to:
 *
 * The Sierra Photonics, Inc. Software License, Version 1.0:
 *  
 * Copyright (c) 2012 by Sierra Photonics Inc.  All rights reserved.
 *  Author:        Jeff Collins, jcollins@sierraphotonics.com
 *  Author:        $Author$
 *  Check-in Date: $Date$ 
 *  Version #:     $Revision$
 *  
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met and the person seeking to use or redistribute such software hereby
 * agrees to and abides by the terms and conditions below:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in
 * the documentation and/or other materials provided with the
 * distribution.
 *
 * 3. The end-user documentation included with the redistribution,
 * if any, must include the following acknowledgment:
 * "This product includes software developed by Sierra Photonics Inc." 
 * Alternately, this acknowledgment may appear in the software itself,
 * if and wherever such third-party acknowledgments normally appear.
 *
 * 4. The Sierra Photonics Inc. names or marks must
 * not be used to endorse or promote products derived from this
 * software without prior written permission. For written
 * permission, please contact:
 *  
 *  Sierra Photonics Inc.
 *  attn:  Legal Department
 *  7563 Southfront Rd.
 *  Livermore, CA  94551  USA
 * 
 * IN ALL CASES AND TO THE FULLEST EXTENT PERMITTED UNDER APPLICABLE LAW,
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL SIERRA PHOTONICS INC. OR 
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Altium Community Software.
 *
 * See also included file SPI_License.txt.
 ***************************************************************************}


{***************************************************************************
 * Copyright (c) 2009-2012 XIA LLC.
 *  (Some code stolen from Altium examples and forum posts)
 *  (Specifically, some code borrowed from forum post at http://forum.live.altium.com/#posts/189386.
 *   This code is Copyright (c) 2011 Sencore Inc.)
 *  Author:        Jeff Collins, jcollins@xia.com
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
 *  1.  This script requires functions and constants defined in XIA_Utils.pas.
 *  	This script also requires one function from XIA_Generate_Sorted_Multiwire_Netlist.pas.
 *  	Both of these scripts must be included in the script project along with this file.
 *  2.  svn.exe that is in the DOS path.  Tested with SlikSvn v1.7.6 x64.
 *      See http://www.sliksvn.com/en/download .
 *      **This script has NOT BEEN TESTED with svn v1.6.x!!  It will likely not work! **
 *      Please upgrade to v1.7.x!!
 *  3.  UnxUtils from http://sourceforge.net/projects/unxutils/ .
 *      It's not exactly clear what version of this I have, but I downloaded
 *      mine on 2011/08/30.
 *      Create "Altium_scripts"\dosutils\UnxUtils.
 *      Put UnxUtils.zip in that directory, then unzip it.
 *      Files end up in "Altium_scripts"\dosutils\UnxUtils\usr\local\wbin\sed.exe, etc.
 *  
 * Notes:
 *  1.  Requires Altium 10.
 *  2.  Tested with Windows 7 x64.
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
 *  WHAT THE VAULT RELEASE PROCESS GETS RIGHT:
 *  1.  Enforcing monotonically-increasing, non-repeating release numbers for fabrication
 *  and assembly.
 *  2.  Ease of marking schematics, gerbers, and other generated files with fab or assembly
 *  ItemRevNumber.
 *  3.  Separation of fabrication and assembly release numbers.
 *  4.  Separation of fabrication and assembly release generation.
 *  5.  The concept that a PCB assembly (board + components soldered to it) is a superset
 *  of a PCB fabrication (blank board).
 *  6.  Enforcing that entire project (including dependencies) is svn clean prior to allowing
 *  a release to run.
 *  
 *  MY COMPLAINTS/ISSUES WITH RELEASING DESIGNS TO VAULT:
 *  1.  For design mode, creating outputs in a directory named by the selected configuration
 *  (rather than the standard ProjectOutputs/ directory) results in multiple places for project
 *  outputs to live.  This makes it more difficult to track changes to certain key project
 *  outputs in svn, such as human readable netlist and csv BOM.
 *  2.  For release mode, doing a checkout into a temporary snapshot directory is unnecessary
 *  and creates yet another place for project outputs to be generated.
 *  3.  For a release, the fact that files are generated saying "This file is not in version control"
 *  is not acceptable.  The files are in svn, and were verified to be svn clean, prior to
 *  running the release!
 *  4.  Project files (source + outputs) are thrown over the wall into an outside-subversion
 *  document control system.
 *  4a.  What gets released to Vault server is a galactic zipfile containing all project files
 *  (source + outputs).  This is my idea of what an svn tags/ entry should be (a snapshot in time
 *  of the project).  It is not my idea of what an svn releases/ entry should be (targeted project
 *  outputs suitable for release to outside vendors).
 *  4b.  There is no way to customize what files should be sent to PCB fab vendor, PCB assembly
 *  vendor, etc., short of downloading release zipfile from Vault server and manually selecting
 *  files.
 *  5.  Design releases should be recorded with an entry in releases/ and tags/ in svn.
 *  6.  PCB fabrication items in Vault are treated differently from components.  Specifically,
 *  they cannot be added to a schematic and thus to BOM.  Thus, there is not an easy way to
 *  have a schematic design & BOM call out the PCB fabrication item.
 *
 * WHAT THIS SCRIPT WILL DO:
 *  When run following a PCB Release View "design mode" run:
 *  1.  Copy generated outputs from the configuration-named directory back to the standard
 *  project Project_Outputs/ subdir.
 *  2.  Offer to checkin project source and project output files (???).
 *  
 *  When run following a PCB Release View "release mode" run:
 *  	(Note that this script can be run against a "real" release mode temp dir, which will
 *  hold BOM, Checkout, Output, and Snapshot subdirs.  This script can also be run against
 *  an "unzipped" temp dir, resulting from unpacking the zipfile stored in the Vault.
 *  We would use this option if the real release mode temp dir had been deleted already.)
 *  	
 *  1.  Copy generated outputs from the release snapshot directory back to the standard
 *  project Project_Outputs/ subdir.
 *  2.  Checkin said files to Project_Outputs/.
 *  3.  Create subdirs in releases/ and tags/.
 *  4.  Package up most of the generated files into an appropriate fabrication or assembly
 *  zipfile in releases/.
 *  5.  Take a project snapshot in tags/.
 *
 * WHAT THIS SCRIPT WILL *NOT* DO:
 *  1.  Run the release-to-Vault from the PCB Release View screen in Altium.
 *  2.  Currently this script cannot be run from an OutJob via the PCB Release View.
 *  Thus, this script needs to be run manually immediately following a release.
 *  
 * CAD SETUP REQUIREMENTS:
 *  1.  Subversion (svn) server that is accessible to all Altium users in your company.
 *  It must also be accessible to purchasing and operations personnel who interface
 *  with printed circuit board fabrication and/or assembly shops.
 *  The point is also for all users to be able to review other people's designs.
 *  In addition, an svn checkin to a common server will always result in a monotonically
 *  increasing svn rev number.  
 *
 *  2.  All users must have SlikSvn (or equivalent) installed on their PCs.  Installing
 *  TortoiseSVN with adding the optional command line client also works.  Svn client must
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
 *  identical for all Altium users at your company.  For SPI, the CAD files in
 *  question are Altium scripts (such as this one), schematic and footprint libraries,
 *  OutJob scripts, and Excel BOM template files.  I am not aware of any specific parts
 *  of this script that will fail if this global path is not setup properly.
 *  But I can say for certain that all my testing has been done with such a setup
 *  and I make no guarantees that it will work in the absence of such a setup.
 *  There are 2 obvious ways to do this on windows (though it should be possible
 *  to have some users use each method):
 *
 *  4a.  Each user checks out a working copy of the company Altium libraries
 *  directory to a location of their choosing.  Then each user has a custom
 *  script to mount their working copy as an agreed-upon drive letter (eg. R:).
 *  My custom batch file sub.bat reads:
 *  subst r: "c:\SPI\SPI_components_database_ro"
 *  This mounts the directory c:/SPI/SPI_components_database_ro (which is my local
 *  working copy of the company Altium libraries, etc.) as my R: drive.
 *  I added a link to sub.bat to my windows startup folder, so that my R: drive mounts
 *  automagically on initial login to my PC.
 *  This setup is tolerant of the svn server being occasionally down or unavailable.
 *  However, the catch is that I need to explicitly do that svn update operation
 *  to pull down updated libraries, etc. from the svn server to my local working copy.
 *  (This is the method we use and what all the script testing has been against.)
 *
 *  4b.  Company fileserver and all users agreeing to mount this as the same drive
 *  letter (eg. R:).  Fileserver is assumed to (a) be always up and available, and
 *  (b) always updated with the latest and greatest libraries, etc.  Fileserver
 *  should presumably only be writable by responsible persons (eg. database librarians).
 *
 *  5.  Scripts directory location.  
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
 *  6.  Assumes that there is a script project named "SPI_Altium_scripts.PrjScr"
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
 *  1.  If you intend to commit/release/tag following a release-to-Vault, you MUST
 *  be running from a "full" working copy, containing trunk/, releases/, and tags/
 *  (eg. h:/projects/nifty_widget/trunk/schem/nifty_widget_main,
 *  NOT  h:/projects/nifty_widget_abbrev/schem/nifty_widget_main).
 *
 * SPI-ism's: (Assumptions / constraints / weirdness / etc. that may be very specific to my company)
 *  1.  SPI takes the somewhat unusual step of trying to get the blank PCB fab
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
 *  TODO:
 *  1.  Handle situations where files generated by PCB Release View applet were configured
 *  for a specific variant.  In such cases, copy varied files to Project_Outputs/,
 *  but with appropriate suffixes to describe the variant.
 *  5.  How to get ItemRevNumber into assembly BOM?
 *  6.  How to get svn rev number into purchasing BOM?
 *
 *  ASSUMPTIONS / THINGS NOT TO DO:
 *  1.  Project subdirs, source files, etc. should NOT contain any of the following reserved words:
 *      "trunk"
 *      "releases"
 *      "tags"
 *      "branches"
 *      "Dependencies"
 *      "[" + FullItemId + "]"
 *  2.  Assumes that svn server is https type or at least uses Unix ("/") style path separators.
 *  3.  Don't have any file that has different casing in Windows compared to casing on svn server
 *  	(eg. FOO.SchDoc vs. Foo.SchDoc).
 *  4.  Don't have multiple source or dependency files in a project with the same name but different
 *  	paths (eg. r:\trunk\foo1.OutJob and r:\trunk\bar\foo1.OutJob).
 *
 *  NOTES:
 *  1.  If there are pending svn adds to the basePathRel (eg. "H:\projects\MMVOA\releases\"),
 *  the commit will fail.  This is because we only do reverts on
 *  releases/ and tags/ starting at the level of the given project
 *  (eg. "H:\projects\MMVOA\releases\ee\schem\RapidPrototypeCircuits\GroundPlaneRestrictionDemo\").
 *  We don't want to do reverts outside of the given project hierarchy!
 *  Workaround:  If safe to do so, manually do svn revert of basePathRel/basePathTag.
 ***************************************************************************}


uses
SysUtils;

{***************************************************************************
 * Forward declarations for form objects.
 ***************************************************************************}
Interface

type
   TCommitReleaseTagForm = class(TForm)
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

   formButtonOk: TButton;                 
   formButtonCancel: TButton;             
   formButtonsLabel1 :TLabel;         
   formStatusBar1: TXStatusBar;   
   Image1        : TImage;
   procedure TCommitReleaseTagForm.clickedOk(Sender : TPanel);
   procedure TCommitReleaseTagForm.clickedCancel(Sender : TPanel);
   procedure TCommitReleaseTagForm.bCheck0_1(Sender : TPanel);
end;

   
{***************************************************************************
 * Function forward declarations.
 ***************************************************************************}


{***************************************************************************
 * Global constants.
 ***************************************************************************}
const
{* Declare the version and name of this script. *}
   constScriptVersion            = 'v0.4.0 $Revision$';
   constThisScriptNameNoExt      = 'SPI_Commit_Release_Tag';
   constThisScriptName           = constThisScriptNameNoExt + '.pas';
{}
   {* Declare the global working copy (with scripts and libraries) that must be up-to-date before we allow this script to actually run. *}
   cCRT_GlobalScriptsAndLibsWc   = 'R:\trunk\'; { Leave the trailing "\" char there! }
{}
   {* Declare the names of all ProjectOutputs/ subdirs. *}
   { We are also using these as the names of the OutJob files (don't include ".OutJob" extension here). }
   cCRT_FabricationSubDir        = 'SPI05_Fabrication';
   cCRT_AssemblySubDir           = 'SPI06_Assembly';
{}
   { Constants related to info directory created by Altium PCB Release View applet. }
   cCRT_ReleaseInfoDir           = 'ReleaseInfo';	{ Dir under project path created by Altium PCB Release View applet }
   cCRT_ReleaseLogFileName       = '_Release.Log';	{ Logfile created by Altium PCB Release View applet }
{}
   { Constants related to the contents of _Release.Log file. }
   cCRT_ConfigurationName        = 'ConfigurationName';
   cCRT_FullItemId               = 'FullItemId';
{}
   { Constants related to the contents of *.ReleaseInfo file. }
   cCRT_ReleaseInfoFileName      = '$fullItemId$[$projectName$].ReleaseInfo';	{ Release info file created by Altium PCB Release View applet }
   cCRT_FullItemIdPrefixFormat   = '[$fullItemId$]';	{ Format of FullItemId prefix, not counting trailing space char. }
   cCRT_ReleaseModePrefixSep     = ' ';					{ Separator char between FullItemId prefix and the rest of the file name. }
{}
   { Constants related to how Altium PCB Release View applet stores things in the galactic zipfile sent to Vault. }
   cCRT_ZippedBomSubDir          = 'BOM';					{ Location of useless psuedo-BOM files. }
   cCRT_ZippedCheckoutSubDir     = '';						{ This subdir does not exist. }
   cCRT_ZippedSnapshotSubDir     = 'Design';				{ Location of snapshot subdir. }
   cCRT_ZippedDepsSubDir         = 'Design\Dependencies'	{ Location of external project dependencies. }
   cCRT_ZippedOutputSubDir       = 'Released';				{ Subdir under which release-generated outputs are stored. }
{}
   { Constants related to how Altium PCB Release View applet sets up temporary release directory. }
   cCRT_ReleasedBomSubDir        = 'BOM';					{ Location of useless psuedo-BOM files. }
   cCRT_ReleasedCheckoutSubDir   = 'Checkout';				{ Location of checkout subdir. }
   cCRT_ReleasedSnapshotSubDir   = 'Snapshot';				{ Location of snapshot subdir. }
   cCRT_ReleasedDepsSubDir       = 'Snapshot\Dependencies'	{ Location of external project dependencies. }
   cCRT_ReleasedOutputSubDir     = 'Output';				{ Subdir under which release-generated outputs are stored. }
   
{}
   { Constants related to how we will setup our tags/ subdirs. }
   cCRT_TagsDesignSubDir         = 'Design';	{ Name of project snapshot directory in tags/ subdir. }
   cCRT_TagsDependenciesSubDir   = 'Dependencies';	{ Subdir under tags/ snapshot dir to hold scripts, templates, and other project dependencies. }
{}
   cCRT_CopyFileFailRc           = 0;			{ Return code if CopyFile() call fails. }

{***************************************************************************
 * Global variables.  Highly evil.  Ick ick.
 ***************************************************************************}
var
   CommitReleaseTagForm : TCommitReleaseTagForm;
//   step                    : Integer;


{***************************************************************************
 * procedure CRT_UpdateGuiStatusMessage()
 *  Update status message in dialog box and write said status to the debug file.
 ***************************************************************************}
procedure CRT_UpdateGuiStatusMessage(msg : TString);
begin

   { Change text in GUI status line. }
   formStatusBar1.SimpleText := msg;

   { Force a screen refresh of GUI status line. }
   formStatusBar1.Update;
   CommitReleaseTagForm.Update;

   { Copy this message to debug file. }
   WriteToDebugFile('');
   WriteToDebugFile('* ' + msg);

end; { end CRT_UpdateGuiStatusMessage() }


{***************************************************************************
 * procedure CRT_AtExit()
 *  Put results in our dialog box list and return to AtExit() for the rest of the cleanup routines.
 ***************************************************************************}
procedure CRT_AtExit(rc : Integer);
var
   i        : Integer;

begin 

//   ShowMessage('In CRT_AtExit()!');
   
   {* Transform existing GUI dialog box so that there is a big list box available. *}

   { Nuke most text fields to make room for the big list box. }
   CommitReleaseTagForm.formText03.Free;
   CommitReleaseTagForm.formText04.Free;
   CommitReleaseTagForm.formText05.Free;
   CommitReleaseTagForm.formText06.Free;
   CommitReleaseTagForm.formText07.Free;
   CommitReleaseTagForm.formText08.Free;
   CommitReleaseTagForm.formText09.Free;
   CommitReleaseTagForm.formText10.Free;
   CommitReleaseTagForm.formText11.Free;
   CommitReleaseTagForm.formText12.Free;
   CommitReleaseTagForm.formText13.Free;
   CommitReleaseTagForm.formText14.Free;
   CommitReleaseTagForm.formText15.Free;
   CommitReleaseTagForm.formText16.Free;
   CommitReleaseTagForm.formText17.Free;
   CommitReleaseTagForm.formText18.Free;
   CommitReleaseTagForm.formText19.Free;
   CommitReleaseTagForm.formText20.Free;
   CommitReleaseTagForm.formText21.Free;
   CommitReleaseTagForm.formText22.Free;
   CommitReleaseTagForm.formText23.Free;
   CommitReleaseTagForm.formText24.Free;
   CommitReleaseTagForm.formText25.Free;
   CommitReleaseTagForm.formText26.Free;
   CommitReleaseTagForm.formText27.Free;
   CommitReleaseTagForm.formText28.Free;
   CommitReleaseTagForm.formText29.Free;
   CommitReleaseTagForm.formText30.Free;

   { Transform existing GUI dialog box so that there is a big list box available. }
   CommitReleaseTagForm.listBox1.Left := 14;
   CommitReleaseTagForm.listBox1.Top := 40;
   CommitReleaseTagForm.listBox1.Width := 972;
   CommitReleaseTagForm.listBox1.Height := 640;

   { Move Ok button to center. }
   CommitReleaseTagForm.formButtonOk.Left := 450;
   CommitReleaseTagForm.formButtonOk.Enabled := True;
   CommitReleaseTagForm.formButtonOk.Update;

   { Nuke Cancel button. }
   CommitReleaseTagForm.formButtonCancel.Free;

   { Run GUI dialog box to display the summary messages. }
   //        ShowMessage('About to display modal dialog');
   CommitReleaseTagForm.formText01.Caption := SummaryMessages.Strings[0];

   CommitReleaseTagForm.listBox1.Clear;


   {* Proceed to output summary messages in the list box. *}
   
   { Loop over all the summary messages. }
   for i := 1 to SummaryMessages.Count - 1 do
   begin

      { Add this line of message to the list box on screen. }
      CommitReleaseTagForm.listBox1.Items.Insert((i-1), SummaryMessages.Strings[i]);

      { Update the size of the horizontal scroll bars if needed. }
      { Code stolen from http://www.delphipages.com/forum/showthread.php?t=203460 }
//    if (CommitReleaseTagForm.listBox1.Canvas.TextWidth(SummaryMessages.Strings[i] + 'W') > CommitReleaseTagForm.listBox1.ScrollWidth) then
//    begin
//       CommitReleaseTagForm.listBox1.ScrollWidth := CommitReleaseTagForm.listBox1.Canvas.TextWidth(SummaryMessages.Strings[i] + 'W');
//    end;

      { For some reason, that's not enough.  Double it instead of adding the width of a 'W' char. }
      if (CommitReleaseTagForm.listBox1.Canvas.TextWidth(SummaryMessages.Strings[i] + SummaryMessages.Strings[i]) > CommitReleaseTagForm.listBox1.ScrollWidth) then
      begin
         CommitReleaseTagForm.listBox1.ScrollWidth := CommitReleaseTagForm.listBox1.Canvas.TextWidth(SummaryMessages.Strings[i] + SummaryMessages.Strings[i]);
      end;    

   end;

   { Report if enableSvnCommits was disabled. }
   if (enableSvnCommits = False) then
   begin
      CommitReleaseTagForm.formButtonsLabel1.Caption := 'ERROR ERROR ERROR Script was running with enableSvnCommits set to False, meaning that I did no svn commits!!!';
   end
   
   { If we were given a sucess error code, meaning we're exiting successfully, report that. }
   else if (rc = 0) then
   begin
      CommitReleaseTagForm.formButtonsLabel1.Caption := 'Script is exiting successfully.  Click Ok to finish.';
   end

   { Else report error exit condition. }
   else
   begin

      CommitReleaseTagForm.formButtonsLabel1.Caption := 'Failed while running this operation: ' + CommitReleaseTagForm.formStatusBar1.SimpleText + constLineBreak + constLineBreak +
      'ERROR:  Script is exiting prematurely due to error!';

   end;

   { Since that form is already modal, we simply exit and rely on the click handler
    to do the remaining cleanup. }   


end; { end CRT_AtExit() }
   

{***************************************************************************
 * BEGIN Support functions.
 ***************************************************************************}

{***************************************************************************
 * function CRT_GetScriptVersionAndRev()
 *  Return the script version and rev in a more readable format.
 *  
 *  Returns:  Reformatted version-and-rev string.
 ***************************************************************************}
function CRT_GetScriptVersionAndRev(versionAndRevString : TString;
                                    )                   : TString;
var
   k             : Integer;
                  
begin

   { Note:  Don't combine the string below or you will have svn replacing $Revision.*$
    with svn rev number on checkin! }
   result := StringReplace(StringReplace(versionAndRevString, ('$' + 'Revision:'), 'svn rev', ''), ' $', '', '');
   
end; {end CRT_GetScriptVersionAndRev() }


{***************************************************************************
 * function CRT_RunUnixFindCommand()
 *  Shell out and run find.exe so that we may find files without the results
 *  being forced to all upper case, as with Delphi FindFiles().
 *  This annoys me in general and offends the unix hacker in me.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_RunUnixFindCommand(    scriptsPath : TDynamicString;
                                    findPath    : TString;
                                    findParms   : TString;
                                var findResults : TStringList;
                                    )           : Integer;
var
   rc           : Integer;
   parms        : TStringList;
   
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('Hello from CRT_RunUnixFindCommand()');

   { Create necessary string lists. }
   parms             := TStringList.Create();
   //   findResults       := TStringList.Create();	// We now require that parent has already done this step.

   { Pass along our command line parameters. }
//   parms.Add('.');
//   parms.Add(findParms);

   { Run generic external command, only in this case it will be "find.exe". }
   CLF_RunGenericExternalCommand(scriptsPath,
                                 findPath,
                                 {batFile} 'generic_cmd',
                                 {command} 'find' + ' ' + findParms,
                                 parms,
                                 {var} {genOut} findResults);

   { Free string lists. }
   parms.Free();

end; { end CRT_RunUnixFindCommand() }


{***************************************************************************
 * function CRT_CopyFilePreserveModTime()
 *  Copy a file, while preserving the modification time across the copy.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_CopyFilePreserveModTime(srcPathAndName : TString;
                                     dstPathAndName : TString;
                                     )              : Integer;
var
   i       : Integer;
   rc      : Integer;
   srcAge  : Integer;
   dstPath : TString;
   dstName : TString;
                  
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*Hello from CRT_CopyFilePreserveModTime()!');

   { Sanity check that source file exists. }
   if (not FileExists(srcPathAndName)) then
      MyAbort('Source file "' + srcPathAndName + '" does not exist!');

   { Retrieve age of source file. }
   srcAge := FileAge(srcPathAndName);
//    WriteToDebugFile('CRT_CopyFilePreserveModTime(), srcAge before is ' + IntToStr(srcAge) + '.');

   { Proceed to do the file copy. }
   rc := CopyFile(srcPathAndName, dstPathAndName, False);
   
   { Sanity check to be sure we succeeded. }
   if (rc = cCRT_CopyFileFailRc) then
      MyAbort('CopyFile() reported error!');
  
   { The set age command requires us to operate in the current directory only. }
   dstPath := ExtractFileDir(srcPathAndName);
   dstName := ExtractFileName(srcPathAndName);
   ChDir(dstPath);

//    WriteToDebugFile('CRT_CopyFilePreserveModTime(), dstPath is "' + dstPath + '", dstName is "' + dstName + '".');
//    WriteToDebugFile('CRT_CopyFilePreserveModTime(), GetCurrentDir() reports "' + GetCurrentDir + '".');

   { Attempt to preserve age of file across copy. }
   rc := FileSetDate(dstName, srcAge);
   
   { Sanity check to be sure we succeeded. }
   if (rc <> 0) then
      MyAbort('Unable to set destination file age!');

end; { end CRT_CopyFilePreserveModTime() }


{***************************************************************************
 * function CRT_ProjectAddRemoveGenerated()
 *  Add or remove a specified generated file to/from the project.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_ProjectAddRemoveGenerated(Project                : IProject;
                                       addRemoveMePathAndName : IDocument;
                                       action                 : TString;
                                       )                      : Integer;

var
   i        : Integer;
   k        : Integer;
   document : IDocument;
   found    : Boolean;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CRT_ProjectAddRemoveGenerated()');

   { Flag that we have not yet found the document we're looking for. }
   found := False;

   { Look over all project documents looking for PcbDoc files. }
   {** Loop over all logical documents in the project (counting backwards). **}
   for k := (Project.DM_GeneratedDocumentCount - 1) downto 0 do
   begin

      { Retrieve reference to kth document in project. }
      document := Project.DM_GeneratedDocuments(k);
      WriteToDebugFile(' Examining project generated file ' + document.DM_FullPath);

      { See if this is a match for the one we're looking for. }
      if (addRemoveMePathAndName = document.DM_FullPath) then
      begin

         { Flag that we've found the document we're looking for. }
         WriteToDebugFile(' Found a match!');
         found := True;

         { If we're supposed to be removing the document from the project,
          then proceed to do so. }
         if (action = 'remove') then
         begin
            MyAbort('Ack!  Currently no support for removing a generated file!');
            project.DM_RemoveSourceDocument(addRemoveMePathAndName);
            WriteToDebugFile(' Removing generated file "' + addRemoveMePathAndName + '" from project.');
         end;

      end;

   end; { endfor }

   { If we're supposed to be adding a document to the project and said
    document isn't already part of the project, then proceed to add it. }
   if ( (action = 'add') and (not found) ) then
   begin
      project.DM_AddGeneratedDocument(addRemoveMePathAndName);
      WriteToDebugFile(' Adding generated file "' + addRemoveMePathAndName + '" to project.');
   end;

end; { end CRT_ProjectAddRemoveGenerated() }


{***************************************************************************
 * function CRT_ProjectAddGenerated()
 *  Add a specified source document to the project, but only if it's not
 *  already part of the project.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_ProjectAddGenerated(Project          : IProject;
                                 addMePathAndName : IDocument;
                                 )                : Integer;

begin

   { Call CRT_ProjectAddRemoveGenerated() to do all the real work. }
   result := CRT_ProjectAddRemoveGenerated(Project,
                                           addMePathAndName,
                                           'add');
   
end; { end CRT_ProjectAddGenerated() }


{***************************************************************************
 * function CRT_ProjectRemoveGenerated()
 *  Remove a specified source document from the project.  Remove multiple
 *  instances of it, if needed.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_ProjectRemoveGenerated(Project             : IProject;
                                    removeMePathAndName : IDocument;
                                    )                   : Integer;

begin

   { Call CRT_ProjectAddRemoveGenerated() to do all the real work. }
   result := CRT_ProjectAddRemoveGenerated(Project,
                                           removeMePathAndName,
                                           'remove');

end; { end CRT_ProjectRemoveGenerated() }

{***************************************************************************
 * END Support functions.
 ***************************************************************************}


{***************************************************************************
 * function CRT_PopulateStringLists()
 *  Perform init step by populating galactic string list.
 *
 *  Configure some additional steps, files to include, files to exclude, etc.
 *  
 *  Returns created and populated galactic string list.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_PopulateStringLists(var CRT_GalacticStringList : TStringList;
                                     )                      : Integer;

var
   zipFileNum    : Integer;
   zipFileNumStr : TString;
   
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

//   ShowMessage('Hello world from CRT_PopulateStringLists()');
   
   { Create all these string lists before we try to populate them. }
   CRT_GalacticStringList := TStringList.Create;

   { Keywords that will be substituted later:
    1.  $fullItemIdPrefix$ for fullItemIdPrefix that PCB Release View applet (running in release mode)
    	prepends to all files, eg "[14100-001-00020-01]".
    }

   { Configure a zipfile to hold fabrication outputs (gerbers, drill, IPC356 only). }
   zipFileNum := 1;
   zipFileNumStr := IntToStr(zipFileNum);
   CRT_GalacticStringList.Add('Config_zipfile'+zipFileNumStr + constStringEquals + 'Fabrication_gerbers_only.zip');
   CRT_GalacticStringList.Add('Config_zipfile'+zipFileNumStr+'_subdir' + constStringEquals + cCRT_FabricationSubDir);
   CRT_GalacticStringList.Add('Config_zipfile'+zipFileNumStr+'_excludes' + constStringEquals + '$fullItemIdPrefix$ odb\*.*' + '|*.ZIP|*.REP|*.APR_LIB|*.RUL');

   { Configure a zipfile to hold fabrication outputs (gerbers, drill, IPC356, odb zipfile). }
   zipFileNum := zipFileNum + 1;
   zipFileNumStr := IntToStr(zipFileNum);
   CRT_GalacticStringList.Add('Config_zipfile'+zipFileNumStr + constStringEquals + 'Fabrication_gerbers_odb.zip');
   CRT_GalacticStringList.Add('Config_zipfile'+zipFileNumStr+'_subdir' + constStringEquals + cCRT_FabricationSubDir);
   CRT_GalacticStringList.Add('Config_zipfile'+zipFileNumStr+'_excludes' + constStringEquals + '$fullItemIdPrefix$ odb\*.*' + '|*.REP|*.APR_LIB|*.RUL');

   { Configure a zipfile to hold minimal fabrication outputs (gerbers, drill only) for fab at AdvPcb. }
   zipFileNum := zipFileNum + 1;
   zipFileNumStr := IntToStr(zipFileNum);
   CRT_GalacticStringList.Add('Config_zipfile'+zipFileNumStr + constStringEquals + 'Fabrication_gerbers_minimal_AdvPcb.zip');
   CRT_GalacticStringList.Add('Config_zipfile'+zipFileNumStr+'_subdir' + constStringEquals + cCRT_FabricationSubDir);
   CRT_GalacticStringList.Add('Config_zipfile'+zipFileNumStr+'_excludes' + constStringEquals + '$fullItemIdPrefix$ odb\*.*' + '|*.EXTREP|*.ZIP|*.APR|*.LDP|*.DRR|*.REP|*.APR_LIB|*.RUL');

end; { end CRT_PopulateStringLists() }


{***************************************************************************
 * function CRT_GetSourceFiles()
 *  Iterate over all the source documents in the project.
 *  Return a list of the paths to all source documents.
 *
 *  Returns source files in var parm sourceFilePathsAndNames.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_GetSourceFiles(    Project                 : IProject;
                                scriptsPath             : TDynamicString;
                                projectPath             : TDynamicString;
                            var sourceFilePathsAndNames : TStringList;
                            var projectPathUrl          : TString;
                                )                       : Integer;
var
   Document      : IDocument;
   k             : Integer;
   docPath       : TDynamicString;
                  
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Compile project before proceeding. }
   Project.DM_Compile;

   { Retrieve the name of the project file. }
   docPath := Project.DM_ProjectFullPath;
   WriteToDebugFile('*Found project file at path ' + docPath);

   { Add the path to this document to the list of sourceFilePathsAndNames. }
   sourceFilePathsAndNames.Add(docPath);

   { Loop over all logical documents in the project.... }
   for k := 0 to (Project.DM_LogicalDocumentCount - 1) do
   begin

      { Get reference to this document. }
      Document := Project.DM_LogicalDocuments(k);

      { Get the path to this document and report this to debug file. }
      docPath := Document.DM_FullPath;
      WriteToDebugFile('*Found source document of kind ' + Document.DM_DocumentKind + ' at path ' + docPath);
      
      { Add the path to this document to the list of sourceFilePathsAndNames. }
      sourceFilePathsAndNames.Add(docPath);

   end;

   
   {** Determine svn server URL for project trunk/ working directory. **}
   { Issue svn info command to query for svn server URL for project directory. }
   GetFileOrDirSvnServerUrl(scriptsPath,
                            projectPath,
                            {fileOrDir} '.',
                            {var} {fileOrDirUrl} projectPathUrl);
   WriteToDebugFile('projectPathUrl is "' + projectPathUrl + '".');

end; {end CRT_GetSourceFiles() }


{***************************************************************************
 * function CRT_FindReleaseTempDir()
 *  Have the user show us the location of the temp dir where the PCB Release View
 *  applet just ran.  Alternatively, show us the location of a temp dir in
 *  which the full release zipfile generated by PCB Release View applet and
 *  sent to Vault server has been unpacked.
 *
 *  Following this, proceed to retrieve information about this directory and
 *  about the release.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_FindReleaseTempDir(    scriptsPath             : TDynamicString;
                                    projectName             : TDynamicString;
                                    projectPath             : TDynamicString;
                                    projectPathUrl          : TString;
                                var releaseTempPath         : TString;
                                var releaseTempPathBom      : TString;
                                var releaseTempPathCheckout : TString;
                                var releaseTempPathSnapshot : TString;
                                var releaseTempPathDeps     : TString;
                                var releaseTempPathOutput   : TString;
                                var releaseMode             : Boolean;
                                var releaseViewDeps         : TStringList;
                                var configurationName       : TString;
                                var fullItemId              : TString;
                                var fullItemIdPrefix        : TString;
                                var releaseInfoSources      : TStringList;
                                var releaseInfoDeps         : TStringList;
                                var releaseInfoGenerated    : TStringList;
                                    )                       : Integer;
var
   i                      : Integer;
   fileOpenDialog         : TFileOpenDialog;
   findPath               : TString;
   findParms              : TString;
   releaseLogPathAndName  : TString;
   releaseLog             : TStringList;
   thisReleaseStr         : TString;
   thisReleaseInfo        : TStringList;
   releaseInfoPathAndName : TString;
   releaseInfo            : TStringList;
   thisFileStr            : TString;
   leftStr                : TString;
   rightStr               : TString;
   foo                    : TString;
   urlAndRevStr           : TString;
   
begin
   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   fileOpenDialog := TFileOpenDialog.Create(nil);
   fileOpenDialog.Options := MkSet(fdoPickFolders);
   fileOpenDialog.Title := 'Select release temp directory (or unzipped temp dir) or configuration-named subdir where Altium PCB Release View applet just ran.';

   { Attempt to execute fileOpenDialog. }
   if fileOpenDialog.Execute then
   begin

      { We succeeded.  Store releaseTempPath. }
      releaseTempPath := fileOpenDialog.FileName;
//      ShowMessage('File : '+fileOpenDialog.FileName)

      WriteToDebugFile('*User identified Release temp dir as "' + releaseTempPath + '".');
   end
   
   else
      MyAbort('FileOpen file was cancelled');

   
   { Create a stringlist to hold any dependencies declared in a release-mode temp dir. }
   releaseViewDeps := TStringList.Create;

   { Create stringlists to hold sources, deps, and generated files as declared in ReleaseInfo/ dir. }
   releaseInfoSources   := TStringList.Create;
   releaseInfoDeps      := TStringList.Create;
   releaseInfoGenerated := TStringList.Create;

   { Set output strings to null values, until overridden later. }
   configurationName    := '';
   fullItemId           := '';
   fullItemIdPrefix     := '';
   
   {** Determine whether this is a release-mode temporary directory (indicating
    a release mode run) or a Vault unzipped temp dir (also indicating a release
    mode run) or a configuration-named outputs subdir (indicating a design mode run). **}

   {
    A release-mode temp dir will have a structure that looks like:
    BOM/
    Checkout/
    Snapshot/
      Dependencies/
    Outputs/
	  (OutJob subdirs)

    A Vault unzipped temp dir will have a structure that looks like:
    BOM/
    Design/
      Dependencies/
    Released/
	  (OutJob subdirs)

    A design mode configuration-named outputs subdir has a structure like:
    (OutJob subdirs)
    SystemBOM.xml
    SystemBOM.xsl

    We will test for the presence of SystemBOM.xsl in the chosen directory.
    }

   { Look for SystemBOM.xsl in the chosen directory. }
   { FIXME:  Magic strings! }
   if (FileExists(releaseTempPath + '\' + 'SystemBOM.xsl')) then
   begin

      { Configure that we're just copying files following a PCB Release View applet design mode run. }
      releaseMode            := False;
      WriteToDebugFile('*This appears to be a configuration-named outputs subdir.  Setting releaseMode to False!');
      
   end

   { Else we presume that this is either a real release mode temp dir or an unzipped temp dir. }
   else
   begin
      
      { Look for markers that this is a real release mode temp dir. }
      if (DirectoryExists(releaseTempPath + '\' + cCRT_ReleasedCheckoutSubDir)) then
      begin

         { Configure that we're committing, releasing, and tagging following a PCB Release View applet release mode run. }
         releaseMode            := True;
         WriteToDebugFile('*This appears to be a release-mode temp dir.  Setting releaseMode to True!');

         { Configure the various release temp paths to match what we expect for an unzipped temp dir. }
         releaseTempPathBom      := releaseTempPath + '\' + cCRT_ReleasedBomSubDir;
         releaseTempPathCheckout := releaseTempPath + '\' + cCRT_ReleasedCheckoutSubDir;
         releaseTempPathSnapshot := releaseTempPath + '\' + cCRT_ReleasedSnapshotSubDir;
         releaseTempPathDeps     := releaseTempPath + '\' + cCRT_ReleasedDepsSubDir;
         releaseTempPathOutput   := releaseTempPath + '\' + cCRT_ReleasedOutputSubDir;

      end { endif }
   
      { Else we assume that we're dealing with an unzipped temp dir. }
      else
      begin

         { Configure that we're committing, releasing, and tagging following a PCB Release View applet release mode run. }
         releaseMode            := True;
         WriteToDebugFile('*This appears to be an unzipped temp dir.  Setting releaseMode to True!');

         { Configure the various release temp paths to match what we expect for an unzipped temp dir. }
         releaseTempPathBom      := releaseTempPath + '\' + cCRT_ZippedBomSubDir;
         releaseTempPathCheckout := releaseTempPath + '\' + cCRT_ZippedCheckoutSubDir;
         releaseTempPathSnapshot := releaseTempPath + '\' + cCRT_ZippedSnapshotSubDir;
         releaseTempPathDeps     := releaseTempPath + '\' + cCRT_ZippedDepsSubDir;
         releaseTempPathOutput   := releaseTempPath + '\' + cCRT_ZippedOutputSubDir;

      end; { endelse }
   
      { Verify the presence of BOM, Checkout, Snapshot, and Output subdirs (no Deps). }
      if (not DirectoryExists(releaseTempPathBom)) then
         MyAbort('Did not find expected subdir "' + releaseTempPathBom + '"!');

      if (not DirectoryExists(releaseTempPathCheckout)) then
         MyAbort('Did not find expected subdir "' + releaseTempPathCheckout + '"!');

      if (not DirectoryExists(releaseTempPathSnapshot)) then
         MyAbort('Did not find expected subdir "' + releaseTempPathSnapshot + '"!');

      if (not DirectoryExists(releaseTempPathOutput)) then
         MyAbort('Did not find expected subdir "' + releaseTempPathOutput + '"!');

      
      {* Get a list of external file dependencies as identified by Altium PCB Release View applet. *}

      { Only do this operation if we actually have a "Dependencies" subdir! }
      if (DirectoryExists(releaseTempPathDeps)) then
      begin
      
         { Use external unix find command so that we get true casing of all files in question. }
         findPath       := releaseTempPathDeps;
         findParms      := '-type f';
         CRT_RunUnixFindCommand(scriptsPath,
                                findPath,
                                findParms,
                                {var findResults} releaseViewDeps);

      end; { endif have releaseTempPathDeps subdir }

      { Make sure we have a "ReleaseInfo" dir in project path. }
      if (not DirectoryExists(projectPath + cCRT_ReleaseInfoDir)) then
         MyAbort('Did not find "' + cCRT_ReleaseInfoDir + '" dir in project path!');

      {* Get info from "_Release.Log" file in ReleaseInfo/ dir. }
      { Make sure we have a "_Release.Log" file in ReleaseInfo/ dir. }
      releaseLogPathAndName := projectPath + cCRT_ReleaseInfoDir + '\' + cCRT_ReleaseLogFileName;
      if (not FileExists(releaseLogPathAndName)) then
         MyAbort('Did not find release log file at "' + releaseLogPathAndName + '"!');

      { Proceed to read full contents of _Release.Log file. }
      releaseLog     := TStringList.Create;
      releaseLog.LoadFromFile(releaseLogPathAndName);

      { Remove empty lines from file. }
      for i := (releaseLog.Count - 1) downto 0 do
      begin

         if (releaseLog.Strings[i] = '') then
            releaseLog.Delete(i);

      end; { endfor }
      
      { Output useful contents of this file to debug log. }
      for i := 0 to (releaseLog.Count - 1) do
      begin
         WriteToDebugFile('* releaseLog line: "' + releaseLog.Strings[i] + '".');

         { Retrieve info about release. }
         thisReleaseStr := releaseLog.Strings[i];

      end; { endfor }

      { TODO:  Check file header. }

      { FIXME:  Does this file get appended to bottom with next release???  Assuming so for now. }

      { Split the line describing this release into a list of name=value pairs. }
      thisReleaseInfo := TStringList.Create;
      CLF_SplitDelimitedUnquotedStringIntoStringList(thisReleaseStr,
                                                     constStringDelimiter,
                                                     {var} thisReleaseInfo);
      
      { Output contents of this file to debug log. }
      for i := 0 to (thisReleaseInfo.Count - 1) do
      begin
         WriteToDebugFile('* thisReleaseInfo line: "' + thisReleaseInfo.Strings[i] + '".');
      end; { endfor }

      { Sanity check that we have configuration name info. }
      if (not CLF_IsNameInStringList(cCRT_ConfigurationName, thisReleaseInfo)) then
         MyAbort('Did not find configuration name info!');
      
      { Extract the configuration name info. }
      configurationName := thisReleaseInfo.ValueFromIndex(thisReleaseInfo.IndexOfName(cCRT_ConfigurationName));
      WriteToDebugFile('*configurationName: "' +configurationName + '".');

      { Sanity check that we have full item ID info. }
      if (not CLF_IsNameInStringList(cCRT_FullItemId, thisReleaseInfo)) then
         MyAbort('Did not find full item ID info!');
      
      { Extract the full item ID info. }
      fullItemId := thisReleaseInfo.ValueFromIndex(thisReleaseInfo.IndexOfName(cCRT_FullItemId));
      WriteToDebugFile('*fullItemId: "' +fullItemId + '".');

      { Use full item ID info to create fullItemIdPrefix. }
      fullItemIdPrefix     := StringReplace(cCRT_FullItemIdPrefixFormat, '$fullItemId$', fullItemId, MkSet(rfReplaceAll));
      WriteToDebugFile('*fullItemIdPrefix: "' +fullItemIdPrefix + '".');

      
      {* Get info from "*.ReleaseInfo" file in ReleaseInfo/ dir. }

      { Make sure we have a "*.ReleaseInfo" file in ReleaseInfo/ dir. }
      releaseInfoPathAndName := projectPath + cCRT_ReleaseInfoDir + '\' + cCRT_ReleaseInfoFileName;

      { Perform keyword substitition on releaseInfoPathAndName. }
      releaseInfoPathAndName := StringReplace(releaseInfoPathAndName, '$fullItemId$', fullItemId, MkSet(rfReplaceAll));
      releaseInfoPathAndName := StringReplace(releaseInfoPathAndName, '$projectName$', projectName, MkSet(rfReplaceAll));

      if (not FileExists(releaseInfoPathAndName)) then
         MyAbort('Did not find release info file at "' + releaseInfoPathAndName + '"!');

      { Proceed to read full contents of *.ReleaseInfo file. }
      releaseInfo     := TStringList.Create;
      releaseInfo.LoadFromFile(releaseInfoPathAndName);

      { TODO:  Check file header. }

      { Remove empty lines from file. }
      for i := (releaseInfo.Count - 1) downto 0 do
      begin

         if (releaseInfo.Strings[i] = '') then
            releaseInfo.Delete(i);

      end; { endfor }
      
      { Output useful contents of this file to debug log. }
      for i := 0 to (releaseInfo.Count - 1) do
      begin
         WriteToDebugFile('* releaseInfo line: "' + releaseInfo.Strings[i] + '".');
         thisFileStr            := releaseInfo.Strings[i];

         { Line looks something like:
          "Record=SourceFile|URL=https://192.168.10.247:9880/spi/projects/common/altium_libraries/trunk/templates/OutJobs/SPI01-0_Schematic_Review_netlist_csv_bom_sch_pdf.OutJob|Revision=1863" }
         
         { Proceed to break the string down.  We want the URL name=value pair. }
         SplitStringIntoLeftAndRightWithAbort(thisFileStr,
                                              constStringDelimiter,
                                              {var} leftStr,
                                              {var} rightStr);
         
         { Break the URL name=value pair (now in rightStr) into name and value. }
         SplitStringIntoLeftAndRightWithAbort(rightStr,
                                              constStringEquals,
                                              {var leftStr} foo,
                                              {var rightStr} urlAndRevStr);
         
         WriteToDebugFile('*  URL and revision value: "' + urlAndRevStr + '".');

         { See if this is what Altium calls a source file, which we will call either
          a source or a dependency. }
         { FIXME:  Magic string! }
         if (leftStr = 'Record=SourceFile') then
         begin
            WriteToDebugFile('*  this is an Altium source file: "' + thisFileStr + '".');

            { See if this file's URL is a child of the project's URL, meaning it is what I will
             call it a source file, as opposed to a dependency file. }
            if (CLF_DoesStringStartWith(urlAndRevStr, projectPathUrl)) then
            begin
               releaseInfoSources.Add(urlAndRevStr);
               WriteToDebugFile('*   I will call this a source file.');
            end

            { Else it's an external dependency file (script, OutJob, Excel template, etc.). }
            else
            begin
               releaseInfoDeps.Add(urlAndRevStr);
               WriteToDebugFile('*   I will call this a dependency file.');
            end;
           
         end { endif is it what Altium calls source file }

         { Else see if it is a generated file. }
         { FIXME:  Magic string! }
         else if (leftStr = 'Record=GeneratedFile') then
         begin
            releaseInfoGenerated.Add(urlAndRevStr);
            WriteToDebugFile('*  this is a generated file: "' + thisFileStr + '".');

         end { end elsif }
         
      end; { endfor }

      
      { Free local string lists. }
      releaseLog.Free;
      releaseInfo.Free;
      thisReleaseInfo.Free;
      
   end; { endif release-mode temp dir. }

end; {end CRT_FindReleaseTempDir() }


{***************************************************************************
 * function CRT_CleanupReleaseTempPath()
 *  Cleanup (filesystem delete) any zipfiles created by this script
 *  (from a previous crashed script run) sitting around in the releaseTempPathOutput.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_CleanupReleaseTempPath(projectName           : TDynamicString;
                                    releaseTempPathOutput : TString;
                                    )                     : Integer;
var
   rc        : Integer;
   FilesOnly : TStringList;
   subDir    : TString;
   mask      : TSTring;
   recursive : Boolean;
   i         : Integer;

begin 

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*In CRT_CleanupReleaseTempPath().');

   { Initialize list of files. }
   FilesOnly := TStringList.Create;

   { Find any files matching "*_ProjectName_*.zip". }
   subDir    := '';
   mask      := '*_' + projectName + '_*.zip';
   recursive := True;
   MyFindFilesSpecifyRecursion({projOutPath} releaseTempPathOutput,
                               subDir,
                               mask,
                               recursive,
                               {var} FilesOnly);
   
   { Loop over all such files. }
   for i := 0 to (FilesOnly.Count - 1) do
   begin

      { Attempt to delete this zipfile left over from a previous, crashed script run. }
      WriteToDebugFile('* Attempting to delete leftover zipfile "' + FilesOnly.Strings(i) + '".');
      DeleteFileWithVerify(FilesOnly.Strings(i));

   end; { endfor }
   
end; { end CRT_CleanupReleaseTempPath() }

                                            
{***************************************************************************
 * function CRT_CleanupProjectOutputs()
 *  Cleanup (svn revert) any modifications to Project_Outputs/ dir.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_CleanupProjectOutputs(scriptsPath : TDynamicString;
                                   projectPath : TDynamicString;
                                   projOutPath : TString;
                                   )           : Integer;
var
   rc               : Integer;

begin 

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*In CRT_CleanupProjectOutputs().');
   
   {*** Attempt to revert any modifications to Project_Outputs/ due to files that were previously created but not
    checked in, due to crashed script run, etc. ***}
   DoSvnRevert(scriptsPath,
               projectPath,
               projOutPath,
               '');

end; { end CRT_CleanupProjectOutputs() }


{***************************************************************************
 * function CRT_SanityCheckRelAndTag()
 *  Perform sanity checks before starting release & tag operation.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_SanityCheckRelAndTag(scriptsPath : TDynamicString;
                                  projectPath : TDynamicString;
                                  )           : Integer;
var
   rc               : Integer;

begin 

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*In CRT_SanityCheckRelAndTag().');
   
   { First make sure there is a "\trunk\" in the project path.
    If not, it means that the user is running from an incomplete working copy of the project.
    This means that we won't be able to do release and tag, so abort now. }
   if (AnsiPos('\' + constSvnDirTrunk + '\', projectPath) = 0) then
   begin
      MyAbort('Could not find "\'+ constSvnDirTrunk + '\" in project path "' + projectPath + '".  Thus, it appears that you are working from an incomplete working copy of this project.  I will not be able to do release and tag operations because of this.');
   end; { endif }

end; { end CRT_SanityCheckRelAndTag() }


{***************************************************************************
 * function CRT_CleanupRelAndTag()
 *  Cleanup (svn revert) in top level releases/ and tags/.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_CleanupRelAndTag(    scriptsPath    : TDynamicString;
                                  projectPath    : TDynamicString;
                              var projPathRelTop : TDynamicString;
                              var projPathTagsTop : TDynamicString;
                                  )              : Integer;
var
   rc             : Integer;

begin 

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*In CRT_CleanupRelAndTag().');
   
   { Construct top level paths in releases/ and tags/, based on the full project path. }
   projPathRelTop := StringReplace(projectPath, ('\' + constSvnDirTrunk + '\'), ('\' + constSvnDirReleases + '\'), '');
   projPathTagsTop := StringReplace(projectPath, ('\' + constSvnDirTrunk + '\'), ('\' + constSvnDirTags + '\'), '');

   {*** Attempt to revert any svn adds in releases/ or tags/ that may have been left behind by a crashed script run. ***}
   DoSvnRevert(scriptsPath,
               projectPath,
               projPathRelTop,
               projPathTagsTop);

end; { end CRT_CleanupRelAndTag() }
   

{***************************************************************************
 * function CRT_CreateSubDirs()
 *  Ensures that a given path exists.  If not, create subdirs comprising it.
 *  Return a list of directories created.
 *
 *  Note:  Assumes that string list newSubDirs has already been created.
 *
 *  If new subDir is created, it is added to var parm newSubDirs
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_CreateSubDirs(    scriptsPath : TDynamicString;
                               projectPath : TDynamicString;
                               filePath    : TString;
                           var newSubDirs  : TStringList;
                               )           : Integer;
var
   i             : Integer;
   rc            : Integer;
   existingPath  : TString;
   remainingPath : TString;
   leftStr       : TString;
   rightStr      : TString;
                  
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*Hello from CRT_CreateSubDirs()!');

   { Loop initialization. }
   existingPath  := '';
   remainingPath := filePath;
   
   { Loop until we run out of subdirs. }
   repeat
   begin

      { Extract the first (leftmost) subdir in the given path. }
      SplitStringIntoLeftAndRightWithAbort(remainingPath,
                                           '\',
                                           {var} leftStr,
                                           {var} rightStr);
      
      WriteToDebugFile('*  leftStr is "' + leftStr + '".');
      WriteToDebugFile('*  rightStr is "' + rightStr + '".');

      { Update existingPath and remainingPath. }
      existingPath := existingPath + leftStr + '\';
      remainingPath := rightStr;

      { Create this subdir if needed. }
      CreateSubDir(scriptsPath,
                   projectPath,
                   {subDir} existingPath,
                   {var} newSubDirs);
      
      
   end;
   until (rightStr = '');

end; { end CRT_CreateSubDirs() }


{***************************************************************************
 * function CRT_CopyGeneratedFilesToProjectOutputs()
 *  Copy all files that were generated by the PCB Release View applet to their
 *  normal homes in Project_Outputs/.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_CopyGeneratedFilesToProjectOutputs(    Project                   : IProject;
                                                    scriptsPath               : TDynamicString;
                                                    projectPath               : TString;
                                                    projOutPath               : TString;
                                                    releaseTempPathOutput     : TString;
                                                    releaseMode               : Boolean;
                                                    fullItemIdPrefix          : TString;
                                                var filePathsAndNamesToSvnAdd : TStringList;
                                                    )                         : Integer;
var
   i                          : Integer;
   findPath                   : TString;
   findParms                  : TString;
   rc                         : Integer;
   srcPathAndName             : TString;
   srcRelPathAndName          : TString;
   dstPathAndName             : TString;
   filePath                   : TString;
   fileName                   : TString;
   pos                        : Integer;
   leftStr                    : TString;
   rightStr                   : TString;
   newSubDirs                 : TStringList;
   generatedFilePathsAndNames : TStringList;
   generatedFilePathAndName   : TString;
   projectDoc                 : IServerDocument;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { TODO:  Currently we're doing a find command to find all release-generated files.
    This code dates from before I discovered all the info in ReleaseInfo/.  Shall we
    change over to use the list of generated files from ReleaseInfo/ (only works for
    release mode)?  Also, shall we reconcile said info vs. the results of a find? }
   
   { Create stringlist to hold list of all release-generated files (with paths). }
   filePathsAndNamesToSvnAdd := TStringList.Create;
   newSubDirs                := TStringList.Create;
   generatedFilePathsAndNames:= TStringList.Create;

   { Run external find unix command, so that we can get true casing for all results. }
   { FIXME:  Magic string to handle excluding temp file created by external bat script! }
   { FIXME:  Magic string to handle excluding odb/ subdir! }
   findPath     := releaseTempPathOutput;
   findParms    := '-not -regex "..generic_cmd_out.txt" -not -regex ".* odb\\.*" -type f';
   CRT_RunUnixFindCommand(scriptsPath,
                          findPath,
                          findParms,
                          {var} {findResults} generatedFilePathsAndNames);
   
   { Loop over all the PCB Release View applet-generated output files. }
   for i := 0 to (generatedFilePathsAndNames.Count - 1) do
   begin

      { The first 2 chars of the file will be a useless '.\'.  Strip these off. }
      generatedFilePathAndName   := Copy(generatedFilePathsAndNames.Strings(i), 3, MaxInt);
      WriteToDebugFile('* Found release-generated file "' + generatedFilePathAndName + '".');

      { Cache the source path/filename of this release-generated file. }
      srcPathAndName   := releaseTempPathOutput + '\' + generatedFilePathAndName;
      WriteToDebugFile('*  srcPathAndName is "' + srcPathAndName + '".');

      { Calculate the destination path/filename so that we may copy this to Project_Outputs/. }
      dstPathAndName   := StringReplace(srcPathAndName, releaseTempPathOutput, projOutPath, MkSet(rfReplaceAll));
      WriteToDebugFile('*  dstPathAndName is "' + dstPathAndName + '".');

      { In release mode, we need to strip off the PCB part number that is prefixed to all files. }
      { TODO:  Add variant suffix for any files affected by variance! }
      { TODO:  Handle design mode! }
      if (releaseMode) then
      begin

         { Strip off everything before the OutJob name from srcPathAndName. }
         srcRelPathAndName := StringReplace(srcPathAndName, (releaseTempPathOutput + '\'), '', MkSet(rfReplaceAll));
         WriteToDebugFile('*  srcRelPathAndName is "' + srcRelPathAndName + '".');

         { Replace the fullItemIdPrefix and separator char in srcRelPathAndName. }
         srcRelPathAndName := StringReplace(srcRelPathAndName, (fullItemIdPrefix + cCRT_ReleaseModePrefixSep), '', '');
         WriteToDebugFile('*  srcRelPathAndName is now "' + srcRelPathAndName + '".');

         { Construct dstPathAndName. }
         dstPathAndName := projOutPath + '\' + srcRelPathAndName;
                                                           
      end; { endif }

      { Extract path and filename for destination, so that we may verify that destination path actually exists. }
      filePath := ExtractFilePath(dstPathAndName);
      fileName := ExtractFileName(dstPathAndName);

      { If path doesn't exist, then proceed to create it. }
      if (not DirectoryExists(filePath)) then
      begin
//         MyAbort('Destination path "' + filePath + '" does not exist!');

         { Call CRT_CreateSubDirs() to create one or more components of this path. }
         CRT_CreateSubDirs(scriptsPath,
                           projectPath,
                           filePath,
                           {var} newSubDirs);
         
      end;

      { Proceed to do the file copy.  Preserve modification time across copy. }
      CRT_CopyFilePreserveModTime(srcPathAndName,
                                  dstPathAndName);

      { Record all files copied so that we may svn add and svn commit them later. }
      filePathsAndNamesToSvnAdd.Add(dstPathAndName);

      { Add this generated file to the project file (if needed). }
      CRT_ProjectAddGenerated(Project,
                              {addMePathAndName} dstPathAndName);

      
   end; { endfor }

   {* Save modifications to project file (due to adding generated files). *}
   { Note:  If/when the script crashes here, it means that Altium itself is unhappy and needs to be shut down and restarted! }
   projectDoc := Client.OpenDocument('PrjPcb', project.DM_ProjectFullPath);
   projectDoc.DoFileSave('PrjPcb');

   { Add project file itself to list of files to svn commit, since it may have just changed. }
   filePathsAndNamesToSvnAdd.Add(project.DM_ProjectFullPath);

   
   { Free stringlists that we no longer need. }
   generatedFilePathsAndNames.Free;
   newSubDirs.Free;

end; {end CRT_CopyGeneratedFilesToProjectOutputs() }


{***************************************************************************
 * function CRT_CreateRelAndTagSubDirs()
 *  Create project level subdirs in both releases/ and tags/.
 *
 *  Returns list of new subdirs created in var parm newSubDirs.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_CreateRelAndTagSubDirs(    scriptsPath               : TDynamicString;
                                        projectPath               : TDynamicString;
                                        fullItemIdPrefix          : TString;
                                        projPathRelTop            : TDynamicString;
                                        projPathTagsTop           : TDynamicString;
                                    var projPathTagsPref          : TString;
                                    var projPathTagsSnap          : TString;
                                    var projPathTagsDeps          : TString;
                                    var basePathRel               : TString;
                                    var basePathTags              : TString;
                                    var filePathsAndNamesToSvnAdd : TStringList;
                                        )                         : Integer;
var
   rc               : Integer;
   i                : Integer;
   pos              : Integer;
   subDirs          : TStringList;
   newSubDirs       : TStringList;
   
begin 

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*In CRT_CreateRelAndTagSubDirs().');

   { Construct the base path of the project releases/ dir (eg. h:/projects/MMVOA/releases/). }
   pos := AnsiPos('\' + constSvnDirTrunk + '\', projectPath);
   if (pos = 0) then
      MyAbort('Did not find "trunk" in projectPath!');

   basePathRel := Copy(projectPath, 1, pos) + constSvnDirReleases + '\';
   basePathTags := Copy(projectPath, 1, pos) + constSvnDirTags + '\';
   WriteToDebugFile('basePathRel is "' + basePathRel + '".');
   WriteToDebugFile('basePathTags is "' + basePathTags + '".');

   
   { Attempt to restore / update some or all of these subDirs from svn repo. }
   { Do an svn update --depth=empty to update empty directory structure for releases/ and tags/. }
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
   subDirs      := TStringList.Create;
   subDirs.Add(basePathRel);
   subDirs.Add(basePathTags);
   IssueSvnCommand(scriptsPath,
                   projectPath,
                   constSvnCmdUpdateDepthEmpty,
                   subDirs);
   subDirs.Free;

   { In tags/, we also need a subdir for our fullItemIdPrefix, since a given project will undergo several release-tag operations. }
   projPathTagsPref := projPathTagsTop + fullItemIdPrefix + '\';

   { In tags/, we will also add a "Design" subdir, to follow suit with what Altium PCB Release View applet does. }
   projPathTagsSnap := projPathTagsPref + cCRT_TagsDesignSubDir + '\';

   { In tags/, we further need a subdir to hold project dependencies (scripts, OutJobs, Excel templates, etc.). }
   projPathTagsDeps := projPathTagsSnap + cCRT_TagsDependenciesSubDir + '\';

   WriteToDebugFile('projPathRelTop is "' + projPathRelTop + '".');
   WriteToDebugFile('projPathTagsTop is "' + projPathTagsTop + '".');
   WriteToDebugFile('projPathTagsPref is "' + projPathTagsPref + '".');
   WriteToDebugFile('projPathTagsSnap is "' + projPathTagsSnap + '".');
   WriteToDebugFile('projPathTagsDeps is "' + projPathTagsDeps + '".');

   { Call CRT_CreateSubDirs() to create releases/ path. }
   newSubDirs      := TStringList.Create;
   CRT_CreateSubDirs(scriptsPath,
                     projectPath,
                     {filePath} projPathRelTop,
                     {var} newSubDirs);
   newSubDirs.Free;

   { Call CRT_CreateSubDirs() to create tags/ path. }
   { Note that we only create subdirs in our working copy up to projPathTagsPref.
    The subdir for projPathTagsSnap will be created by the server side svn copy of the project working directory.
    The subdir for projPathTagsDeps will have to be created subsequent to that, server side, since projPathTagsDeps
    now lives under projPathTagsSnap. }
   newSubDirs      := TStringList.Create;
   CRT_CreateSubDirs(scriptsPath,
                     projectPath,
                     {filePath} projPathTagsPref,
                     {var} newSubDirs);
   newSubDirs.Free;

   { Add projPathTagsPref to list of files/dirs to svn add. }
   filePathsAndNamesToSvnAdd.Add(projPathTagsPref);   

end; { end CRT_CreateRelAndTagSubDirs() }


{***************************************************************************
 * function CRT_CreateZipFile()
 *  Create a zipfile in a particular subdirectory of releaseTempPathOutputs.
 *
 *  Allow caller to specify zipfile name, a list of files/extensions to exclude
 *  from zipfile, and a list of additional files to add to the zipfile.
 *
 *  Returns path and name of new zipfile as var parm zipFilePathAndName.
 *  Returns:  0 on success, 1 if unable to delete one or more files
 ***************************************************************************}
function CRT_CreateZipFile(    releaseTempPathOutput : TString;
                               subDir                : TString;
                               zipFileName           : TString;
                               excludes              : TStringList;
                               addlIncludes          : TStringList;
                           var zipFilePathAndName    : TDynamicString;
                               )                     : Integer;
var
   Zip       : TXceedZip;
   fileList  : TStringList;
   i         : Integer;
   msg       : TDynamicString;
   recursive : Boolean;
   fileName  : TString;
   
begin
   
   { For now, assume/hope/pray that we will succeed. }
   result := 0;

//   ShowMessage('Hello World from CreateZipFile.');

   { Prepend the path to the zipfile name to yield a path + filename. }
   zipFilePathAndName := releaseTempPathOutput  + '\' + subDir + '\' + zipFileName;

   Try // begin { Emacs pascal mode doesn't understand Try construct.  Add fake "begin" keyword. }

      { Create zipfile }
      Zip := TXCeedZip.Create(zipFilePathAndName);

      { Setup Zipper so that we don't generate a temporary folder/file. }
      Zip.UseTempFile := False;

      { Flag to keep paths for files in subdirectories. }
      Zip.PreservePaths := True;

      { Initialize list of files. }
      fileList := TStringList.Create;

      { Fetch a list of all files in this subdirectory, subject to specified excludes. }
      recursive := True;
      FindFilesWithExcludesSpecifyRecursion(releaseTempPathOutput ,
                                            subDir,
                                            '*.*',
                                            excludes,
                                            recursive,
                                            {var} fileList);
                                            
      { Loop over the non-excluded files in this ProjectOutputs/ subdir. }
      for i := 0 to fileList.Count - 1 do
      begin

         { Strip off leading path info so that said path info doesn't get stored in zipfile. }
         fileName := StringReplace(fileList.Strings[i], AnsiUpperCase(releaseTempPathOutput  + '\' + subDir + '\'), '', '');
         
         WriteToDebugFile('*Attempting to add file ' + fileName + ' to zipfile....');
         
         { Add this file to the pending zip file that we will create. }
         Zip.AddFilesToProcess(fileName);
         
      end; { endfor i }
      
      { See if we have any additional files we're supposed to add to the zipfile. }
      for i := 0 to addlIncludes.Count - 1 do
      begin

         { FIXME:  Handle stripping path from this additional file! }
         
         WriteToDebugFile('*Attempting to add additional file ' + addlIncludes.Strings[i] + ' to zipfile....');
         
         { Add this file to the pending zip file that we will create. }
         Zip.AddFilesToProcess(addlIncludes.Strings[i]);
         
      end; { endfor i }
      
      { Create the zip file, now that we've identified all the files we want added. }
      WriteToDebugFile('*About to create zipfile ' + zipFilePathAndName);
      Zip.Zip;
      
      Finally
      Zip.Free;
      
   end; { endtry }

   { Free the file list. }
   fileList.Free;
      
end; { end CRT_CreateZipFile() }


{***************************************************************************
 * function CRT_CreateAllZipFiles()
 *  Create all zipfiles that have been configured and for which we have files.
 *
 *  Note:  Assumes that filePathsAndNamesToSvnAdd has already been Created.
 *
 *  Returns list of new zipfiles created as var parm filePathsAndNamesToSvnAdd.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_CreateAllZipFiles(    scriptsPath               : TDynamicString;
                                   projectName               : TString;
                                   releaseTempPathOutput     : TString;
                                   CRT_GalacticStringList    : TStringList;
                                   releaseMode               : Boolean;
                                   fullItemIdPrefix          : TString;
                                   projPathRelTop            : TString;
                               var filePathsAndNamesToSvnAdd : TStringList;
                                   )                         : Integer;
var
   rc                  : Integer;
   excludes            : TStringList;
   addlIncludes        : TStringList;
   findParms           : TStringList;
   pcbDocPath          : TDynamicString;
   bomPath             : TDynamicString;
   zipFilePathAndName  : TDynamicString;
   i                   : Integer;
   state               : Integer;
   zipFileExists       : Boolean;
   zipFileName         : TString;
   rcBool              : Boolean;
   idx                 : Integer;
   subDir              : TString;
   excludesStr         : TString;
   fileNewPathAndName  : TString;
   fileTempPathAndName : TString;
   
begin
   { Assume that we'll succeed in creating zipfiles. }
   rc := 0;

   {* Proceed to create all zip files. *}
   { Search galactic string list looking for zipfile definitions. }
   i := 1;
   repeat
   begin

      { See if the initial/next zipfile definition exists. }
      zipFileExists   := CLF_IsNameInStringList('Config_zipfile' + IntToStr(i), CRT_GalacticStringList);
      if (zipFileExists) then
      begin

         { Get name of zipfile. }
         idx         := CRT_GalacticStringList.IndexOfName('Config_zipfile' + IntToStr(i));

         { NOTE:  SPI-ism:  We prepend fullItemIdPrefix (eg. "[14100-001-00020-01]") and "_ProjectName_" to all zipfile names. }
         { NOTE:  We later use this scheme to recognize zipfiles created by this script! }
         zipFileName := fullItemIdPrefix + '_' + projectName + '_' + CRT_GalacticStringList.ValueFromIndex(idx);
         WriteToDebugFile('* zipFileName is "' + zipFileName + '".');
         
         { Initialize lists of excludes and additional includes. }
         excludes := TStringList.Create;
         addlIncludes := TStringList.Create;
         findParms := TStringList.Create;

         { We always want to exclude zipfiles that we create here in this script from being packaged in release zipfiles. }
         excludes.Add(fullItemIdPrefix + '_' + projectName + '_*zip');

         { Get name of SubDir that this zipfile is interested in packaging up. }
         if (CLF_IsNameInStringList('Config_zipfile' + IntToStr(i) + '_subdir', CRT_GalacticStringList)) then
         begin

            idx         := CRT_GalacticStringList.IndexOfName('Config_zipfile' + IntToStr(i) + '_subdir');
            subDir      := CRT_GalacticStringList.ValueFromIndex(idx);
            WriteToDebugFile('* subDir is "' + subDir + '".');

         end

         else
            MyAbort('Zipfile definition failed to include name of relevant output subdir!');

         { Get name of excludes that this zipfile specifies. }
         if (CLF_IsNameInStringList('Config_zipfile' + IntToStr(i) + '_excludes', CRT_GalacticStringList)) then
         begin

            idx         := CRT_GalacticStringList.IndexOfName('Config_zipfile' + IntToStr(i) + '_excludes');
            excludesStr := CRT_GalacticStringList.ValueFromIndex(idx);

            { Perform keyword substitition on excludesStr. }
            excludesStr := StringReplace(excludesStr, '$fullItemIdPrefix$', fullItemIdPrefix, MkSet(rfReplaceAll));
            
            WriteToDebugFile('* excludesStr is "' + excludesStr + '".');

            { Split this entry into a string list named "excludes". }
            CLF_SplitDelimitedUnquotedStringIntoStringList(excludesStr,
                                                           constStringDelimiter,
                                                           {var} excludes);
            
         end;


         { Attempt to zip up the files in this releaseTempPathOutput/ subdir. }
         { FIXME:  Verify that the specified subdir was created by this release process! }
         rc := rc | CRT_CreateZipFile(releaseTempPathOutput,
                                      subDir,
                                      zipFileName,
                                      excludes,
                                      addlIncludes,
                                      {var} zipFilePathAndName);

         { Move zipfile to its new home in releases/. }
         WriteToDebugFile('* zipFilePathAndName is "' + zipFilePathAndName + '".');

         fileTempPathAndName := (releaseTempPathOutput + '\' + subDir + '\');
         WriteToDebugFile('* fileTempPathAndName is "' + fileTempPathAndName + '".');

         fileNewPathAndName := StringReplace(zipFilePathAndName, fileTempPathAndName, projPathRelTop, MkSet(rfReplaceAll));
         WriteToDebugFile('* fileNewPathAndName is "' + fileNewPathAndName + '".');
         
         CLF_MoveFile(zipFilePathAndName,
                      fileNewPathAndName);

         
         { Add this zipfile to the list of files to svn add and svn commit. }
         filePathsAndNamesToSvnAdd.Add(fileNewPathAndName);

         { Free lists of excludes and additional includes. }
         excludes.Free;
         addlIncludes.Free;
         findParms.Free;

         
      end; { endif zipFileExists }

      { Increment loop counter. }
      i := i + 1;

   end; { end repeat }
   until (zipFileExists = False);
   
   { Give return code to caller. }
   result := rc;

end; { end CRT_CreateAllZipFiles() }


{***************************************************************************
 * function CRT_FindProjectLogFiles()
 *  Find all ECO log files in ProjectLogs/ directory.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_FindProjectLogFiles(    projLogPath               : TString;
                                 var filePathsAndNamesToSvnAdd : TStringList;
                                     )                         : Integer;
var
   rc       : Integer;
   i        : Integer;
   subDir   : TString;
   mask     : TString;

begin 

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*Hello World from CRT_FindProjectLogFiles().');

   { Find all files in the ProjectLogs/ subdir so that we may add them to svn. }
   subDir   := '';
   mask     := '*.*';
   MyFindFiles({projOutPath} projLogPath,
               subDir,
               mask,
               {var FilesOnly} filePathsAndNamesToSvnAdd);

end; { end CRT_FindProjectLogFiles() }


{***************************************************************************
 * function CRT_SvnAddAllFiles()
 *  Svn add all output files, zipfiles, logfiles, and tags/ subdir.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_SvnAddAllFiles(    scriptsPath               : TDynamicString;
                                projectPath               : TDynamicString;
                                projOutPath               : TDynamicString;
                                basePathRel               : TString;
                                basePathTags              : TString;
                                filePathsAndNamesToSvnAdd : TStringList;
                            var commitPathRel             : TString;
                            var commitPathTags            : TString;
                                )                         : Integer;
var
   i               : Integer;
   filePathAndName : TString;
   filePath        : TString;
   len             : Integer;
   svnOut          : TStringList;
                   
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*Hello from CRT_SvnAddAllFiles().');

   { Cache string length of projOutPath. }
   len := Length(projOutPath);
   
   { Loop over all such files. }
   for i := 0 to (filePathsAndNamesToSvnAdd.Count - 1) do
   begin

      { Extract file path and name. }
      filePathAndName := filePathsAndNamesToSvnAdd.Strings[i];

      { See if this file is in ProjectOutputs/. }
      if (CLF_DoesStringStartWith(AnsiUpperCase(filePathAndName), AnsiUpperCase(projOutPath))) then
      begin

         { Strip off leading ProjectOutputs/ path in order to shorten filenames as seen by external svn commands. }
         filePathAndName := Copy(filePathAndName, len+2, MaxInt);

         { Store back to string list. }
         filePathsAndNamesToSvnAdd.Strings[i] := filePathAndName;
         
      end;
      
      WriteToDebugFile('* Will attempt to svn add file "' + filePathAndName + '".');

   end; { endfor }

   { Initialize svnOut string list. }
   svnOut := TStringList.Create;

   { Call CLF_SvnAddFiles() to do all the real work. }
   CLF_SvnAddFilesGetOutput(scriptsPath,
                            projOutPath,
                            {allProjectDocs} filePathsAndNamesToSvnAdd,
                            {var} svnOut);

   
   {* Determine the leaf-most releases/ and tags/ subdir that we may commit. *}
   commitPathRel             := '';
   commitPathTags            := '';

   { Loop over all lines of svn output. }
   for i := 0 to (svnOut.Count - 1) do
   begin

//      WriteToDebugFile('* Examining svnOut line ' + IntToStr(i) + ': "' + svnOut.Strings[i] + '".');

      { Extract file path and name. }
      filePathAndName := Copy(svnOut.Strings[i], constSvnAddFileNameStartsAtCol, MaxInt);

      { If filePathAndName is a (release) zipfile, then take just the path part of it. }
      if (CLF_DoesStringEndWith(AnsiUpperCase(filePathAndName), '.ZIP')) then
      begin

         filePath := ExtractFilePath(filePathAndName);
         
      end { endif }

      { Else we assume that we have a subdir.  Treat it as such. }
      else
      begin
         filePath := filePathAndName;

      end; { endelse }

      
      { See if we have a note from svn that it added a releases/ subdir. }
      if (CLF_DoesStringStartWith(AnsiUpperCase(filePath), AnsiUpperCase(basePathRel))) then
      begin

         WriteToDebugFile('* Found subdir of releases/ that will be svn added: "' + filePath + '".');

         { See if we have an initial (leafmost) entry. }
         if (commitPathRel = '') then
         begin

            WriteToDebugFile('* Found leafmost subdir of releases/ that will be svn added: "' + filePath + '".');
            commitPathRel := filePath;

         end; { endif }
         
      end { endif found subdir of releases/ }

      { Else see if we have a note from svn that it added a tags/ subdir. }
      else if (CLF_DoesStringStartWith(AnsiUpperCase(filePath), AnsiUpperCase(basePathTags))) then
      begin

         WriteToDebugFile('* Found subdir of tags/ that will be svn added: "' + filePath + '".');

         { See if we have an initial (leafmost) entry. }
         if (commitPathTags = '') then
         begin

            WriteToDebugFile('* Found leafmost subdir of tags/ that will be svn added: "' + filePath + '".');
            commitPathTags := filePath;

         end; { endif }
         
      end { end elsif found subdir of tags/ }
      
   end; { endfor }

   { Free svnOut list. }
   svnOut.Free;
 
end; { end CRT_SvnAddAllFiles() }


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
      
      CRT_UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  ' + alterVerbProgressive + ' svn property svn:keywords on xls BOM file(s).');

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
      
      CRT_UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  Tweaking $' + 'Rev$ text in .xls BOM file(s).');

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
 * function CRT_SvnCommitAllFiles()
 *  Svn commit all output files, zipfiles, logfiles, and releases/ and tags/ subdirs.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_SvnCommitAllFiles(scriptsPath    : TDynamicString;
                               projectPath    : TDynamicString;
                               projOutPath    : TDynamicString;
                               projLogPath    : TDynamicString;
                               commitPathRel  : TString;
                               commitPathTags : TString;
                               )              : Integer;
var
   rc          : Integer;
   parms       : TStringList;
   i           : Integer;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Create list of parameters for svn command. }
   parms := TStringList.Create;

   { Make sure that we're not running in a debug mode where we've disabled commits! }
   if (enableSvnCommits = True) then
   begin
   
      { Set commit message. }
      parms.Add('-m');
      parms.Add('Automated commit of release-generated output files, zipfiles of such, logfiles, and releases/ and tags/ subdirs.  Performed by script ' + constThisScriptName + ', ' + CRT_GetScriptVersionAndRev(constScriptVersion) + '.');
      
      { Attempt to checkin all new / changed files in ProjectLogs/ directory. }
      parms.Add(projLogPath);
      
      { Attempt to checkin all new / changed files in ProjectOutputs/ directory. }
      parms.Add(projOutPath);

      { Attempt to checkin all new subdirs and zipfiles in releases/. }
      parms.Add(commitPathRel);

      { Attempt to checkin all new subdirs and zipfiles in tags/. }
      parms.Add(commitPathTags);
      
      { Issue command to commit above files. }
      IssueSvnCommand(scriptsPath,
                      projectPath,
                      constSvnCmdCommit,
                      parms);

      { Free list of parameters. }
      parms.Free;

   end; { endif }
      
end; { end CRT_SvnCommitAllFiles() }


{***************************************************************************
 * function CRT_CreateSnapshotInTags()
 *  Perform svn server side copy-with-commit to copy snapshot of project to
 *  new subdir in tags/.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CRT_CreateSnapshotInTags(scriptsPath             : TDynamicString;
                                  projectPath             : TDynamicString;
                                  projectPathUrl          : TString;
                                  sourceFilePathsAndNames : TStringList;
                                  releaseViewDeps         : TStringList;
                                  releaseInfoSources      : TStringList;
                                  releaseInfoDeps         : TStringList;
                                  projPathTagsSnap        : TString;
                                  projPathTagsDeps        : TString;
                                  basePathTags            : TString;
                                  )                       : Integer;
var
   rc                      : Integer;
   i                       : Integer;
   projTopTagPath          : TString;
   projTagPath             : TString;
   finalSubDir             : TString;
   parms                   : TStringList;
//   projectPathUrl          : TString;
   projTagPathUrl          : TString;
   basePathTagsUrl         : TString;
   basePath                : TString;
   basePathUrl             : TString;
   projectPathRel          : TString;
   projPathTagsSnapRel     : TString;
   projPathTagsDepsRel     : TString;
   projPathTagsSnapUrl     : TString;
   projPathTagsDepsUrl     : TString;
   sourceFilePathAndName   : TString;
   depsList                : TStringList;
   depPathsUrlCache        : TStringList;
   depPathsAndNamesUrl     : TStringList;
   depPathAndName          : TString;
   depPathAndNameUrl       : TString;
   depPathAndNameUrlAndRev : TString;
   depPath                 : TString;
   depName                 : TString;
   depPathUrl              : TString;
   addlDeps                : TStringList;
   leftStr                 : TString;
   rightStr                : TString;
   revision                : TString;

begin 

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*In CRT_CreateSnapshotInTags().');

   { Construct top level path in, based on the full project path. }
//   projTopTagPath := StringReplace(projectPath, ('\' + constSvnDirTrunk + '\'), ('\' + constSvnDirTags + '\'), '');

   { TODO:  Construct basePathUrl by cutting down given projectPathUrl, rather
    than doing the now-extraneous svn info command below. }
   
   { Get our lowest common denominator directory, the root of trunk/, tags/, and releases/. }
   { Strip off trailing '\tags\' to get basePath. }
   WriteToDebugFile('basePathTags is "' + basePathTags + '".');
   basePath := ExtractFilePath(StripTrailingBackslash(basePathTags));
   WriteToDebugFile('basePath is "' + basePath + '".');

   { Strip off that common denominator from projectPath, projPathTagsSnap, and projPathTagsDeps. }
   projectPathRel := StringReplace(projectPath, basePath, '', '');
   WriteToDebugFile('projectPathRel is "' + projectPathRel + '".');

   projPathTagsSnapRel := StringReplace(projPathTagsSnap, basePath, '', '');
   WriteToDebugFile('projPathTagsSnapRel is "' + projPathTagsSnapRel + '".');

   projPathTagsDepsRel := StringReplace(projPathTagsDeps, basePath, '', '');
   WriteToDebugFile('projPathTagsDepsRel is "' + projPathTagsDepsRel + '".');

   
   {** Determine svn server URL for base path in trunk/. **}
   { Issue svn info command to query for svn server URL for project directory. }
   GetFileOrDirSvnServerUrl(scriptsPath,
                            projectPath,
                            {fileOrDir} basePath,
                            {var} {fileOrDirUrl} basePathUrl);
   WriteToDebugFile('basePathUrl is "' + basePathUrl + '".');

   { Construct URLs for projectPathUrl, projPathTagsSnap, and projPathTagsDeps. }
   { Convert Windoze '\' path separators to Unix '/' path separators. }
//   projectPathUrl := basePathUrl + '/' + StringReplace(projectPathRel, '\', '/', MkSet(rfReplaceAll));
   WriteToDebugFile('projectPathUrl is "' + projectPathUrl + '".');

   projPathTagsSnapUrl := basePathUrl + '/' + StringReplace(projPathTagsSnapRel, '\', '/', MkSet(rfReplaceAll));
   WriteToDebugFile('projPathTagsSnapUrl is "' + projPathTagsSnapUrl + '".');

   projPathTagsDepsUrl := basePathUrl + '/' + StringReplace(projPathTagsDepsRel, '\', '/', MkSet(rfReplaceAll));
   WriteToDebugFile('projPathTagsDepsUrl is "' + projPathTagsDepsUrl + '".');
   
   { TODO:  Shall we reconcile source files (and their revision numbers) as declared
    in ReleaseInfo/ dir vs. source files currently in project trunk/ working copy?? }
   
   {** Prepare to do server side svn-copy-with-commit for primary project snapshot. **}
   { Make sure that we're not running in a debug mode where we've disabled commits! }
   if (enableSvnCommits = True) then
   begin
      
      { Initialize list of svn parms. }
      parms := TStringList.Create;

      { Specify URL of project trunk/ directory as the source for this svn copy. }
      parms.Add(projectPathUrl);

      { Specify new snapshot subdir in tags/ as the destination for this svn copy. }
      parms.Add(projPathTagsSnapUrl);
      
      { Set commit message. }
      parms.Add('-m');
      parms.Add('Automated commit to perform server side copy-with-commit to copy snapshot of ' + constSvnDirTrunk + '/ working directory to new subdir in ' + constSvnDirTags + '/.  Performed by script ' + constThisScriptName + ', ' + CRT_GetScriptVersionAndRev(constScriptVersion) + '.');
      
      {** Issue svn server side copy-with-commit command to copy project snapshot to new subdir in tags/. **}
      { Note that since this is a server side copy, it includes the commit operation as well. }
      IssueSvnCommand(scriptsPath,
                      projectPath,
                      constSvnCmdCopy,
                      parms);
      
      { Free list of parms to svn copy. }
      parms.Free;

   end; { endif enableSvnCommits }

   { TODO:  Should we reconcile project depenencies from these 3 sources??
    1.  Project file
    2.  Dependency files in releaseTempDir
    3.  Dependencies declared in ReleaseInfo/

    At the moment, have commented out old code that found superset of #'s 1 & 2 above,
    and then had to figure out URLs for each.
    Currently relying on #3 only.
    }
   
   { Init list of dependencies that we've handled via entries in project file. }
   depsList              := TStringList.Create;

   { Init list of dependency path URLs cache. }
   depPathsUrlCache          := TStringList.Create;

   { Init list of dependency URLs. }
   depPathsAndNamesUrl          := TStringList.Create;

{$IfDef foo27}   
   {** Gather list (and URLs) for all project dependencies (eg. OutJob and script files in r:/trunk/, etc.). **}
   { Loop over all the source files in the project (as declared in project file). }
   for i := 0 to (sourceFilePathsAndNames.Count - 1) do
   begin

      { Extract current source file. }
      sourceFilePathAndName := sourceFilePathsAndNames.Strings[i];

      { See if this source file lives under the normal project hierarchy. }
      if (CLF_DoesStringStartWith(AnsiUpperCase(sourceFilePathAndName), AnsiUpperCase(projectPath))) then
      begin

         WriteToDebugFile('* This source file lives under the normal project hierarchy: "' + sourceFilePathAndName + '".');
      end { endif }

      { Else this is an external file dependency. }
      else
      begin

         WriteToDebugFile('* This source file is an external dependency: "' + sourceFilePathAndName + '".');

         { Split it into path and name. }
         depPath := ExtractFilePath(sourceFilePathAndName);
         depName := ExtractFileName(sourceFilePathAndName);

         { Add this filename to the list of dependencies that we know about. }
         depsList.Add(depName + constStringEquals + depPath);         

      end; { endelse }
      
   end; { endfor }

   
   { Loop over all the dependencies that were identified by Altium PCB Release View applet. }
   { This will be a superset of those listed in project file, but will not include path info! }
   for i := 0 to (releaseViewDeps.Count - 1) do
   begin

      WriteToDebugFile('* PCB Release View applet reported this line: "' + releaseViewDeps.Strings[i] + '".');

      { Extract dependency file name. }
      depName := ExtractFileName(releaseViewDeps.Strings[i]);

//      WriteToDebugFile('* PCB Release View applet reported this file as an external dependency: "' + depName + '".');

      { See if we already know about this dependency. }
      if (CLF_IsNameInStringList(depName, depsList)) then
      begin

         WriteToDebugFile('*  We already know about this file as an external dependency: "' + depName + '".');
      end { endif }

      else
      begin

         WriteToDebugFile('*  We need to find the actual path for this external dependency file: "' + depName + '".');

         { Create stringlist to find this dependency file. }
         addlDeps := TStringList.Create;

         { Attempt to find such a file in our global scripts and libraries working copy. }
         MyFindFilesSpecifyRecursion(cCRT_GlobalScriptsAndLibsWc,
                                     {subDir} '',
                                     {mask} depName,
                                     {recursive} True,
                                     {var} addlDeps);

         { See if we get exactly one hit. }
         if (addlDeps.Count <> 1) then
            MyAbort('Did not get exactly 1 hit when looking for external dependency file "' + depName + '".  Got ' + IntToStr(addlDeps.Count) + ' hits instead.');

         { TODO:  Diff the hit file with the copy sitting in releaseTemp and make sure it's the same file! }

         { Extract file path and name. }
         depPathAndName := addlDeps.Strings[0];
         
         { Free string list. }
         addlDeps.Free;

         { Split it into path and name. }
         depPath := ExtractFilePath(depPathAndName);

         { Add this filename to the list of dependencies that we know about. }
         depsList.Add(depName + constStringEquals + depPath);         

      end; { endelse }
      
      
   end; { endfor }

   
   { Loop over all the dependencies that we found (from 2 sources). }
   for i := 0 to (depsList.Count - 1) do
   begin

      { Extract dependency file name. }
      SplitStringIntoLeftAndRightWithAbort(depsList.Strings[i],
                                           constStringEquals,
                                           {var leftStr} depName,
                                           {var rightStr} depPath);
      
      WriteToDebugFile('* Processing external dependency: "' + depName + '" at path "' + depPath + '".');

      { See if we have a cached URL for this depPath. }
      if (CLF_IsNameInStringList(AnsiUpperCase(depPath), depPathsUrlCache)) then
      begin

         WriteToDebugFile('*  We got a hit in our depPathsUrlCache for this depPath: "' + depPath + '".');

         { Construct full URL for this dependency file. }
         depPathAndNameUrl := depPathsUrlCache.ValueFromIndex(depPathsUrlCache.IndexOfName(AnsiUpperCase(depPath))) + '/' + depName;

      end { endif }

      { Else lookup URL for this depPath and then cache the result for future use. }
      else
      begin

         WriteToDebugFile('*  We missed in our depPathsUrlCache for this depPath: "' + depPath + '".');

         GetFileOrDirSvnServerUrl(scriptsPath,
                                  depPath,
                                  {fileOrDir} '.',
                                  {var} {fileOrDirUrl} depPathUrl);
         WriteToDebugFile('*   depPathUrl is "' + depPathUrl + '".');

         { Add this depPathUrl to our cache. }
         depPathsUrlCache.Add(AnsiUpperCase(depPath) + constStringEquals + depPathUrl);
         
         { Construct full URL for this dependency file. }
         depPathAndNameUrl := depPathUrl + '/' + depName;
         
      end; { endelse }
      
      WriteToDebugFile('*   depPathAndNameUrl is "' + depPathAndNameUrl + '".');

      { Store depPathAndNameUrl to stringlist. }
      depPathsAndNamesUrl.Add(depPathAndNameUrl);

   end; { endfor }
{$EndIf}

   { Loop over all the dependency files as declared in ReleaseInfo/ dir. }
   for i := 0 to (releaseInfoDeps.Count - 1) do
   begin

      { Extract current dependency file. }
      depPathAndNameUrlAndRev := releaseInfoDeps.Strings[i];

      { depPathAndNameUrlAndRev looks something like:
       "https://192.168.10.247:9880/spi/projects/common/altium_libraries/trunk/templates/OutJobs/SPI01-0_Schematic_Review_netlist_csv_bom_sch_pdf.OutJob|Revision=1863" }
         
      { Proceed to break the string down.  Split into URL value and Revision name=value. }
      SplitStringIntoLeftAndRightWithAbort(depPathAndNameUrlAndRev,
                                           constStringDelimiter,
                                           {var} leftStr,
                                           {var} rightStr);
      depPathAndNameUrl := leftStr;
      WriteToDebugFile('*   depPathAndNameUrl is "' + depPathAndNameUrl + '".');

      { From the right string (Revision name=value), extract the Revision value. }
      SplitStringIntoLeftAndRightWithAbort(rightStr,
                                           constStringEquals,
                                           {var} leftStr,
                                           {var} rightStr);
      revision := rightStr;
      WriteToDebugFile('*   revision is "' + revision + '".');

      { Append "@revision" to depPathAndNameUrl so that svn copy copies the specified rev. }
      { FIXME:  Magic string! }
      depPathAndNameUrl := depPathAndNameUrl + '@' + revision;
      WriteToDebugFile('*   depPathAndNameUrl is now "' + depPathAndNameUrl + '".');

      { Store depPathAndNameUrl to stringlist. }
      depPathsAndNamesUrl.Add(depPathAndNameUrl);

   end; { endfor }

   
   {** Prepare to do server side svn mkdir to create subdir for project dependencies. **}
   { This can't be done before the server side copy, since the Dependencies/ subdir is
    a subdir of the new server side Design/ subdir. }
   { Make sure that we're not running in a debug mode where we've disabled commits! }
   if (enableSvnCommits = True) then
   begin
   
      { Initialize list of svn parms. }
      parms := TStringList.Create;

      { Specify new Dependencies subdir in tags/ as the destination for this svn mkdir. }
      parms.Add(projPathTagsDepsUrl);
      
      { Set commit message. }
      parms.Add('-m');
      parms.Add('Automated commit to create ' + cCRT_TagsDependenciesSubDir + '/ subdir in ' + constSvnDirTags + '/.  Performed by script ' + constThisScriptName + ', ' + CRT_GetScriptVersionAndRev(constScriptVersion) + '.');
      
      {** Issue svn server side mkdir to create Dependencies/ as a new subdir in tags/. **}
      { Note that since this is a server side mkdir, it includes the commit operation as well. }
      IssueSvnCommand(scriptsPath,
                      projectPath,
                      constSvnCmdMkdir,
                      parms);
      
      { Free list of parms to svn copy. }
      parms.Free;

   end; { endif enableSvnCommits }

   
   {** Prepare to do server side svn-copy-with-commit for all project dependencies. **}
   { Make sure that we're not running in a debug mode where we've disabled commits! }
   if (enableSvnCommits = True) then
   begin
   
      { Initialize list of svn parms. }
      parms := TStringList.Create;

      { Loop over all the dependency file URLs as the source for this svn copy. }
      for i := 0 to (depPathsAndNamesUrl.Count - 1) do
      begin

         { Specify URL of a particular dependency file as source for this svn copy. }
         parms.Add(depPathsAndNamesUrl.Strings[i]);

      end; { endfor }
      
      { Specify new Dependencies subdir in tags/ as the destination for this svn copy. }
      parms.Add(projPathTagsDepsUrl);
      
      { Set commit message. }
      parms.Add('-m');
      parms.Add('Automated commit to perform server side copy-with-commit to copy project dependencies to new subdir in ' + constSvnDirTags + '/.  Performed by script ' + constThisScriptName + ', ' + CRT_GetScriptVersionAndRev(constScriptVersion) + '.');
      
      {** Issue svn server side copy-with-commit command to copy project snapshot to new subdir in tags/. **}
      { Note that since this is a server side copy, it includes the commit operation as well. }
      IssueSvnCommand(scriptsPath,
                      projectPath,
                      constSvnCmdCopy,
                      parms);
      
      { Free list of parms to svn copy. }
      parms.Free;

   end; { endif enableSvnCommits }

   { Free string lists. }
   depsList.Free;
   depPathsUrlCache.Free;
   depPathsAndNamesUrl.Free;

end; { end CRT_CreateSnapshotInTags() }


{***************************************************************************
 * procedure CNF_CommitReleaseTag()
 *  Perform all operations of this script.
 ***************************************************************************}
procedure CNF_CommitReleaseTag(foo2 : Integer;
                               );
                                      
var
   Workspace                 : IWorkspace;
   Project                   : IProject;
   Document                  : IDocument;
   projectName               : TDynamicString;
   projectPath               : TDynamicString;
   projOutPath               : TDynamicString;
   projLogPath               : TDynamicString;
   CRT_GalacticStringList    : TStringList;
   releaseMode               : Boolean;
   sourceFilePathsAndNames   : TStringList;
   projectPathUrl            : TString;
   i                         : Integer;
   scriptsPath               : TDynamicString;
   rc                        : Integer;
   releaseTempPath           : TString;
   releaseTempPathBom        : TString;
   releaseTempPathCheckout   : TString;
   releaseTempPathSnapshot   : TString;
   releaseTempPathDeps       : TString;
   releaseTempPathOutput     : TString;
   filePathsAndNamesToSvnAdd : TStringList;
   step                      : Integer;
   projPathRelTop            : TDynamicString;
   projPathTagsTop           : TDynamicString;
   projPathTagsPref          : TString;
   projPathTagsSnap          : TString;
   projPathTagsDeps          : TString;
   basePathRel               : TString;
   basePathTags              : TString;
   commitPathRel             : TString;
   commitPathTags            : TString;
   releaseViewDeps           : TStringList;
   configurationName         : TString;
   fullItemId                : TString;
   fullItemIdPrefix          : TString;
   releaseInfoSources        : TStringList;
   releaseInfoDeps           : TStringList;
   releaseInfoGenerated      : TStringList;
                             
begin

   {****** Initialize script. ******}
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

   { Declare that we are running the CRT script. }
   whichScriptIsThis := constWhichScriptCrt;

   { This flag is not actually used in this script, but set it to True to keep other code happy. }
   enableGenerateOutputs := True;

   { Set this flag to False to work in debugging mode with no svn commits. }
   enableSvnCommits      := True;

   { Open debug file. }
   OpenDebugFile((projectPath + constThisScriptNameNoExt + '_Debug.txt'));
   WriteToDebugFile('**Script ' + constThisScriptName + ' started at ' + DateTimeToStr(Date) + ' ' + TimeToStr(Now));

   { Open summary file. }
   OpenSummaryFile((projectPath + constThisScriptNameNoExt + '_Summary.txt'));
   WriteToSummaryFile('Actions performed by this script:');
   WriteToSummaryFile('');

   { Populate the strings lists that tell us what OutJob files to run, what subdirs to create, etc. }
   CRT_PopulateStringLists({var} CRT_GalacticStringList);

   {****** STEP 0.  Cleanup and prepare. ******}
   WriteToDebugFile('');
   WriteToDebugFile('**About to perform step 0 sanity checks...');

   { Set to start out at step 0-1. }
   step := 1;
   WriteToDebugFile('**About to start step 0:  Cleanup and prepare.');


//   {*** Verify that scripts and libraries global working copy is up-to-date. ***}
//   CRT_UpdateGuiStatusMessage('Status:  Starting step 0-' + IntToStr(step) + ':  Checking that scripts and libraries global working copy is up-to-date.');
//   CheckThatSvnScriptsWorkingCopyIsUpdated(scriptsPath,
//                                           constThisScriptName);
//   WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Checked that scripts and libraries global working copy is up-to-date.');
   
   
   {*** Analyze project hierarchy. ***}
   sourceFilePathsAndNames := TStringList.Create;
   CRT_UpdateGuiStatusMessage('Status:  Starting step 0-' + IntToStr(step) + ':  Analyzing project hierarchy.');
   CRT_GetSourceFiles(Project,
                      scriptsPath,
                      projectPath,
                      {var} sourceFilePathsAndNames,
                      {var} projectPathUrl);
   WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Analyzed project hierarchy.');

   
   {*** Ask user to show us the location of the temp directory where the PCB Release View applet generated our output files. ***}
   CRT_UpdateGuiStatusMessage('Status:  Starting step 0-' + IntToStr(step) + ':  Getting user to show us the location of the directory where PCB Release View applet generated output files.');
   CRT_FindReleaseTempDir(scriptsPath,
                          projectName,
                          projectPath,
                          projectPathUrl,
                          {var} releaseTempPath,
                          {var} releaseTempPathBom,
                          {var} releaseTempPathCheckout,
                          {var} releaseTempPathSnapshot,
                          {var} releaseTempPathDeps,
                          {var} releaseTempPathOutput,
                          {var} releaseMode,
                          {var} releaseViewDeps,
                          {var} configurationName,
                          {var} fullItemId,
                          {var} fullItemIdPrefix,
                          {var} releaseInfoSources,
                          {var} releaseInfoDeps,
                          {var} releaseInfoGenerated);
   WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Got user to show us the location of the directory where PCB Release View applet generated output files.');
   
   
   {*** Cleanup any mess left behind in releaseTempPathOutput. ***}
   CRT_UpdateGuiStatusMessage('0-' + IntToStr(step) + '.  Cleaning up any mess in releaseTempPathOutput.');
   CRT_CleanupReleaseTempPath(projectName,
                              releaseTempPathOutput);
   WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Cleaned up any mess in releaseTempPathOutput.');
   
                                            
   {*** Cleanup any modified, missing, etc. files in Project_Outputs/ prior to us doing anything. ***}
   CRT_UpdateGuiStatusMessage('0-' + IntToStr(step) + '.  Reverting any previous modifications to Project_Outputs/ subdir.');
   CRT_CleanupProjectOutputs(scriptsPath,
                             projectPath,
                             projOutPath);
   WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Reverted any previous modifications to Project_Outputs/ subdir.');

   
   { Only do these next steps if project outputs have been generated in Release Mode. }
   if (releaseMode = True) then
   begin
      
      {*** Perform any sanity checks for release and tag operations that we can in advance, so that we fail now rather than later. ***}
      CRT_UpdateGuiStatusMessage('Status:  Starting step 0-' + IntToStr(step) + ':  Performing sanity checks for release-and-tag.');
      CRT_SanityCheckRelAndTag(scriptsPath,
                               projectPath);
      WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Performed sanity checks for release-and-tag.');

      {*** Perform sanity checks and cleanup top level releases/ and tags/ directories. ***}
      CRT_UpdateGuiStatusMessage('Status:  Starting step 0-' + IntToStr(step) + ':  Cleaning up ' + constSvnDirReleases + '/ and ' + constSvnDirTags + '/ if needed.');
      CRT_CleanupRelAndTag(scriptsPath,
                           projectPath,
                           {var} projPathRelTop,
                           {var} projPathTagsTop);
      WriteToSummaryFile('0-' + IntToStr(StepPlusPlus(step)) + '.  Cleaned up ' + constSvnDirReleases + '/ and ' + constSvnDirTags + '/ if needed.');

   end; { endif }
   
   
   {****** STEP 1.  Copy release-generated outputs to Project_Outputs/.  Create rel-and-tag subdirs.  Create zipfiles.  Svn add everything. ******}
   { Set to start out at step 1-1. }
   step := 1;
   WriteToDebugFile('');
   WriteToDebugFile('**About to start step 1:  Copy release-generated outputs to Project_Outputs/.  Create rel-and-tag subdirs.  Create zipfiles.  Svn add everything.');
   WriteToSummaryFile('');

   {*** Copy all release-generated files back to their home location in Project_Outputs/. ***}
   CRT_UpdateGuiStatusMessage('1-' + IntToStr(step) + '.  Copying all release-generated files to home location in Project_Outputs/.');
   CRT_CopyGeneratedFilesToProjectOutputs(Project,
                                          scriptsPath,
                                          projectPath,
                                          projOutPath,
                                          releaseTempPathOutput,
                                          releaseMode,
                                          fullItemIdPrefix,
                                          {var} filePathsAndNamesToSvnAdd);
   WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  Copied all release-generated files to home location in Project_Outputs/.');

   
   { Only do these next steps if project outputs have been generated in Release Mode. }
   if (releaseMode = True) then
   begin
      
      {*** Attempt to create (new?) subdirs in releases/ and tags/. ***}
      CRT_UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  Creating new subdirs in ' + constSvnDirReleases + '/ and ' + constSvnDirTags + '/.');
      CRT_CreateRelAndTagSubDirs(scriptsPath,
                                 projectPath,
                                 fullItemIdPrefix,
                                 projPathRelTop,
                                 projPathTagsTop,
                                 {var} projPathTagsPref,
                                 {var} projPathTagsSnap,
                                 {var} projPathTagsDeps,
                                 {var} basePathRel,
                                 {var} basePathTags,
                                 {var} filePathsAndNamesToSvnAdd);
      WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  Created new subdirs in ' + constSvnDirReleases + '/ and ' + constSvnDirTags + '/.');


      {*** Attempt to create all zipfiles ***}
      CRT_UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  Creating appropriate zipfiles.');
      CRT_CreateAllZipFiles(scriptsPath,
                            projectName,
                            releaseTempPathOutput,
                            CRT_GalacticStringList,
                            releaseMode,
                            fullItemIdPrefix,
                            projPathRelTop,
                            {var} filePathsAndNamesToSvnAdd);
      WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  Created appropriate zipfiles.');

   end; { endif releaseMode }
   
      
   {*** Find all log files in ProjectLogs/ ***}
   CRT_UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  Finding all logs in ProjectLogs/.');
   CRT_FindProjectLogFiles(projLogPath,
                           {var} filePathsAndNamesToSvnAdd);
   WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  Found all logs in ProjectLogs/.');
   

   {*** Svn add all generated output files, release zipfiles, logfiles, and tags/ subdir. ***}
   CRT_UpdateGuiStatusMessage('Status:  Starting step 1-' + IntToStr(step) + ':  Svn adding all generated output files, release zipfiles, logfiles, and releases/ and tags/ subdirs.');
   CRT_SvnAddAllFiles(scriptsPath,
                      projectPath,
                      projOutPath,
                      basePathRel,
                      basePathTags,
                      filePathsAndNamesToSvnAdd,
                      {var} commitPathRel,
                      {var} commitPathTags);                      
   WriteToSummaryFile('1-' + IntToStr(StepPlusPlus(step)) + '.  Svn added all generated output files, release zipfiles, logfiles, and releases/ and tags/ subdirs.');
   

   { Only do these next steps if project outputs have been generated in Release Mode. }
   if (releaseMode = True) then
   begin
      
      {****** STEP 2.  Commit files to svn.  Perform svn tag operation. ******}
      { Set to start out at step 2-1. }
      step := 1;
      WriteToDebugFile('');
      WriteToDebugFile('**About to start step 2:  Commit files to svn.  Perform svn tag operation.');
      WriteToSummaryFile('');

      CRT_UpdateGuiStatusMessage('Status:  Starting step 2-' + IntToStr(step) + ':  Performing svn commit of copied generated output files, release zipfiles, logfiles, and releases/ and tags/ subdirs.');
      CRT_SvnCommitAllFiles(scriptsPath,
                            projectPath,
                            projOutPath,
                            projLogPath,
                            commitPathRel,
                            commitPathTags);
      WriteToSummaryFile('2-' + IntToStr(StepPlusPlus(step)) + '.  Performed svn commit of copied generated output files, release zipfiles, logfiles, and releases/ and tags/ subdirs.');

   
      {*** Create snapshot of project working directory in new subdir in tags/. ***}
      CRT_UpdateGuiStatusMessage('Status:  Starting step 2-' + IntToStr(step) + ':  Performing server side copy-with-commit to copy snapshot of project ' + constSvnDirTrunk + '/ working directory to new subdir in ' + constSvnDirTags + '/.');
      CRT_CreateSnapshotInTags(scriptsPath,
                               projectPath,
                               projectPathUrl,
                               sourceFilePathsAndNames,
                               releaseViewDeps,
                               releaseInfoSources,
                               releaseInfoDeps,
                               projPathTagsSnap,
                               projPathTagsDeps,
                               basePathTags);
      WriteToSummaryFile('2-' + IntToStr(StepPlusPlus(step)) + '.  Performed server side copy-with-commit to copy snapshot of project ' + constSvnDirTrunk + '/ working directory to new subdir in ' + constSvnDirTags + '/.');


      {*** Delete all contents of releaseTempPath/ as a final step. ***}
      CRT_UpdateGuiStatusMessage('Status:  Starting step 2-' + IntToStr(step) + ':  Deleting all files and subdirs in releaseTempPath.');
      SetCurrentDir('C:\');	{ Change directory out of releaseTempPath so that we may delete all files and subdirs there! }

      CLF_DeleteFilesAndDirs({filePath} releaseTempPathBom);
      CLF_DeleteFilesAndDirs({filePath} releaseTempPathSnapshot);
      CLF_DeleteFilesAndDirs({filePath} releaseTempPathOutput);

      { Checkout subdir only exists for real release temp dir, not for unzipped.
       If it doesn't exist, it will be a null string added to releaseTempPath. }
      if (releaseTempPathCheckout <> (releaseTempPath + '\')) then
         CLF_DeleteFilesAndDirs({filePath} releaseTempPathCheckout);
      
      WriteToSummaryFile('2-' + IntToStr(StepPlusPlus(step)) + '.  Deleted all files and subdirs in releaseTempPath.');
      
   end; { endif releaseMode }
   
      
   {****** Wrap things up ******}
   { Free the strings lists that were created by CRT_PopulateStringLists(). }
   CRT_GalacticStringList.Free;
   
   { Free remaining string lists that we created in this function. }

   { Free string lists created by other functions. }
   sourceFilePathsAndNames.Free;
   releaseViewDeps.Free;
   releaseInfoSources.Free;
   releaseInfoDeps.Free;
   releaseInfoGenerated.Free;
   
   { Call AtExit() procedure to write debug outputs to file. }
   WriteToDebugFile('**About to exit script.');
   AtExit(0);                   { Report success at exit }

end; { end CNF_CommitReleaseTag() }


{***************************************************************************
 * procedure TCommitReleaseTagForm.clickedOk()
 *  This is the handler for primary dialog box "OK" click.
 ***************************************************************************}
procedure TCommitReleaseTagForm.clickedOk(Sender : TPanel);

begin

//   ShowMessage('Hello world from TCommitReleaseTagForm.clickedOk()');

   { Figure out if we got here from the initial Ok click to start the script or
    the final Ok click to end the script. }
   
   { See if this is the initial Ok click. }
   if (CommitReleaseTagForm.formButtonOk.Left <> 450) then
   begin
   
      { Tell the user that we are proceeding to run script. }
      formButtonsLabel1.Caption := 'OK.  Proceeding to run script.';
      formButtonsLabel1.Update;

      { Disable (grey out) OK and Cancel buttons on primary form. }
      formButtonOk.Enabled := False;
      formButtonOk.Update;

      formButtonCancel.Enabled := False;
      formButtonCancel.Update;

      { Call CNF_CommitReleaseTag() to do all the actual work. }
      CNF_CommitReleaseTag(99);

   end

   { Else this is the final ok to end the script.
    Close the modal window. }
   else
   begin
      ModalResult := mrOK;
      CommitReleaseTagForm.Close;
   end;
   
   { Return to caller. }
   Exit;

end; { end TCommitReleaseTagForm.clickedOk() }


{***************************************************************************
 * procedure TCommitReleaseTagForm.clickedCancel()
 *  This is the handler for primary dialog box "Cancel" click.
 ***************************************************************************}
procedure TCommitReleaseTagForm.clickedCancel(Sender : TPanel);

var
   Action : TCloseAction;
        
begin

   { Close dialog box. }
   ModalResult := mrCancel;
   CommitReleaseTagForm.Close;

   ShowError('Script canceled at User request.');

   { Exit script now. }
   Exit;
   
end; { end TCommitReleaseTagForm.clickedCancel() }


{***************************************************************************
 * procedure SPI_Commit_Release_Tag()
 *  Script entry point.
 *
 *  Returns:  (nothing--procedure, not a function).
 ***************************************************************************}
procedure SPI_Commit_Release_Tag;

//var

begin

   { Override GUI text entries. }
   CommitReleaseTagForm.Caption := 'Welcome to ' + constThisScriptNameNoExt + ' ' + CRT_GetScriptVersionAndRev(constScriptVersion) + ' script main menu!';
 
   { Override GUI text entries. }
   CommitReleaseTagForm.formText01.Caption := 'You have launched script ' + constThisScriptName + '.';
   CommitReleaseTagForm.formText01.Font.Style := MkSet(fsBold);
   CommitReleaseTagForm.formText02.Caption := '';

   CommitReleaseTagForm.formText03.Caption := 'This script is intended to commit output files to svn, and perform release-tag operations in svn for fabrication or assembly outputs.';
   CommitReleaseTagForm.formText03.Font.Style := MkSet(fsBold);
   CommitReleaseTagForm.formText04.Caption := '';
   CommitReleaseTagForm.formText05.Caption := '';
   CommitReleaseTagForm.formText06.Caption := '';
   CommitReleaseTagForm.formText07.Caption := '';

   CommitReleaseTagForm.formText08.Caption := ''; {This script will also:';}
   CommitReleaseTagForm.formText08.Font.Style := MkSet(fsBold);
   CommitReleaseTagForm.formText09.Caption := '';
   CommitReleaseTagForm.formText10.Caption := '';
   CommitReleaseTagForm.formText11.Caption := '';
   CommitReleaseTagForm.formText12.Caption := '';
   CommitReleaseTagForm.formText13.Caption := '';

   CommitReleaseTagForm.formText14.Caption := 'Preconditions:';
   CommitReleaseTagForm.formText14.Font.Style := MkSet(fsBold);
   CommitReleaseTagForm.formText15.Caption := '1.  You must have already run a release mode fabrication or assembly configuration, which generated outputs in a temp directory.';
   CommitReleaseTagForm.formText16.Caption := '2.  Outputs of said release mode run must be available in one of two forms:';
   CommitReleaseTagForm.formText17.Caption := '2a.  The release mode temp dir (set in DXP->Preferences) that was configured to not delete files on completion of release.';
   CommitReleaseTagForm.formText18.Caption := '2b.  A temp directory in which you have unzipped the release zipfile generated by Altium PCB Release View applet and sent to Vault server.';
   CommitReleaseTagForm.formText19.Caption := '';
   CommitReleaseTagForm.formText20.Caption := '';

   CommitReleaseTagForm.formText21.Caption := 'Notes:';
   CommitReleaseTagForm.formText21.Font.Style := MkSet(fsBold);
   CommitReleaseTagForm.formText22.Caption := '';
   CommitReleaseTagForm.formText23.Caption := '';
   CommitReleaseTagForm.formText24.Caption := '';
   CommitReleaseTagForm.formText25.Caption := '';
   CommitReleaseTagForm.formText26.Caption := '';
   
   CommitReleaseTagForm.formText27.Caption := '';
   CommitReleaseTagForm.formText28.Caption := '';
   CommitReleaseTagForm.formText29.Caption := '';
   
   CommitReleaseTagForm.formText30.Caption := ''; //'Options:';
   CommitReleaseTagForm.formText30.Font.Style := MkSet(fsBold);
   
   CommitReleaseTagForm.formButtonsLabel1.Caption := 'Shall I run (OK), or shall I Cancel running this script?';
   CommitReleaseTagForm.formButtonsLabel1.Font.Style := MkSet(fsBold);

   { Set initial status message. }
   CommitReleaseTagForm.formStatusBar1.SimpleText := 'Status:  Awaiting user permission to run script.';

   
   { Run GUI dialog box asking user for permission to run. }
   CommitReleaseTagForm.ShowModal;

   { Note:  Control now passes to one of the handler functions above. }
   
end; { end SPI_Commit_Release_Tag() }


end.
