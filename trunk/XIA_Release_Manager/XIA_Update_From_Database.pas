{***************************************************************************
 XIA_Update_From_Database.pas
 Altium DelphiScript (basically Pascal) that will:
 1.  Update user parameters with respect to database (Update Parameters from Database operation).
 2.  Update system parameters (footprints) with respect to database (Update from Libraries operation).
 3.  Make sure that most components have a Comment parameter set to "=VALUE" (former UpdateComments script).
 4.  Syncronize Description fields between database and schematic components.
 5.  Check, and if necessary, fix path to DBLib file.
 6.  Check, and if necessary, fix path to DBLink file.
 7.  Update database link on any components still pointing to old DBLib files
     Previous1_database_library_file.DBLib or Previous2_database_library_file.DBLib.
 8.  Check that the schematic symbol in use on schematic pages matches the one specified
     by the database.  If not, don't fix anything, but report the error to user.
 9.  Synchronize varied database components with respect to database.
 10.  If enabled by the presence of a particular .xml filename being part of the design,
	 then restore/maintain "legacy/local" database parameters a function of (logical) refdes.
	 This key is one of a number of parameters that gets lost upon doing an initial DBLib link
	 of a component on a schematic page that was just imported from a legacy Orcad design.
 11.  If enabled by the presence of a particular .xml filename being part of the design,
	 then restore/maintain a set of "legacy/local" database properties as a function of
	 local database key.  These parameters must be "user" parameters.
 
 ***************************************************************************}

{***************************************************************************
 * Copyright (c) 2009-2012 XIA LLC.
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
 *  1.  This script requires functions and constants defined in XIA_Utils.pas.
 *  Both of these scripts must be in the same script project.
 *  
 * Notes:
 *  1.  Tested with Altium 10.  (May or may not work with Altium 9--not tested.)
 *  2.  Tested with Windows 7 x64.
 *  3.  This file should no longer have TAB characters in it.
 *
 * TODO:
 *  1.  Check to see if schematic symbols in design are out of date with respect
 *  to database.
 *  2.  Support pushing/sync'ing IBIS, SPICE, etc. models from database to schematic.
 *  3.  Figure out if we can ignore parameters attached to schematic symbol pins.
 *
 * LIMITATIONS:
 *  1.  This script does not currently handle the database wanting to push/sync any
 *  "Models" other than footprints.  So IBIS models, SPICE models, etc. are not supported.
 *  Theoretically, this script will not touch any non-footprint models currently
 *  attached to a schematic part, but this has not been extensively tested.
 *
 * WHAT THIS SCRIPT WILL DO:
 *  1.  Update user parameters with respect to database (Update Parameters from Database operation).
 *  2.  Update system parameters (footprints) with respect to database (Update from Libraries operation).
 *  3.  Make sure that most components have a Comment parameter set to "=VALUE" (former UpdateComments script).
 *  4.  Syncronize Description fields between database and schematic components.
 *  5.  Check, and if necessary, fix path to DBLib file.
 *  6.  Check, and if necessary, fix path to DBLink file.
 *  7.  Update database link on any components still pointing to old DBLib files
 *   Previous1_database_library_file.DBLib or Previous2_database_library_file.DBLib.
 *  8.  Check that the schematic symbol in use on schematic pages matches the one specified
 *   by the database.  If not, don't fix anything, but report the error to user.
 *  9.  Synchronize varied database components with respect to database.
 * 10.  If enabled by the presence of a particular .xml filename being part of the design,
	 then restore/maintain a "legacy/local" database parameters as a function of (logical) refdes.
	 This key is one of a number of parameters that gets lost upon doing an initial DBLib link
	 of a component on a schematic page that was just imported from a legacy Orcad design.
 * 11.  If enabled by the presence of a particular .xml filename being part of the design,
	 then restore/maintain a set of "legacy/local" database properties as a function of
	 local database key.  These parameters must be "user" parameters.
 
 * WHAT THIS SCRIPT WILL *NOT* DO:
 *  1.  Create an ECO screeen to allow user to approve database-to-schematic changes.  It is assumed
 *  that all database updates will be unconditionally pushed to schematic.
 *
 *  2.  Save and/or checkin changes.  This is intentional to allow a user to close one or more
 *  files without saving if there were unwanted changes or script problems.
 *  
 * CAD SETUP REQUIREMENTS:
 *  (Requirements for items 1,2,3,4 can be eliminated by commenting out call to CheckThatSvnScriptsWorkingCopyIsUpdated() below.)
 *  
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
 *  8.  A company Altium components database that is setup and accessible to all Altium
 *  users.  The exact form and setup of this is "left as an exercise to the reader".
 *
 * XIA-ism's: (Assumptions / constraints / weirdness / etc. that may be very specific to my company)
 *  1.  XIA is currently using a company component database to manage which parts
 *  are available to be used in company Altium designs.  This database currently specifies
 *  schematic symbols, user parameters and footprints, but not IBIS, SPICE, etc. models.
 *  1a.  In 99% of the cases, we wish the Altium Comment field to be set to "=VALUE".
 *  This means that the VALUE field (pushed from database to schematic) will end up being
 *  displayed in the Comment field.
 *  1b.  There are a few components in our database that allow "freeform" text to be inserted
 *  into the Comment field.  These are mostly for use in assembly instructions, etc.
 *  that are one-off, unique-to-a-given-design type of things.
 *  (eg. "Install standoff from bottom side of board at N5.  Install machine screw from top")
 *  1c.  These components that allow "freeform" text in the Comment field are identified
 *  by special text placed in the VALUE or CATEGORY fields.
 *  See CheckUserParametersForComp().
 *  
 *  2.  The standard method to link a schematic component to the database is to use our DBLib file
 *  'Customary_database_library_file.DBLib'.  This script will verify that the standard DBLib file
 *  is part of the project.  
 *  See InitDatabaseInfo().
 *
 *  3.  XIA's current DBLib file is 'Customary_database_library_file.DBLib'.
 *  3a.  When migrating older designs that were linked to the older 'Previous1_database_library_file.DBLib'
 *  file, it is necessary to append a "$" char to the database table name.
 *  See CheckAndCorrectDatabaseLibraryName().
 *  3b.  When migrating more recent designs that were linked to 'Previous2_database_library_file.DBLib',
 *  there are no changes to database table names that need to be made.
 *  See CheckAndCorrectDatabaseLibraryName().
 *
 *  4.  While the standard method for using database components at XIA is to link a component
 *  using the DBLib file, we have provision for using a DBLink file as well.  The reason
 *  to use our DBLink file is when migrating old designs from Orcad to Altium.  Use of the
 *  DBLink file will not change the schematic symbol to use the current/approved one listed
 *  in the database.  This allows the designer to avoid re-drawing the schematic page to
 *  use the current/approved schematic symbol, but at a cost of not fully completing the
 *  migration process at that time.
 *  4a.  Just as XIA enforces the use of a specific DBLib file, we enforce that the DBLink
 *  file (if used at all) must be one very specific one.
 *  See InitDatabaseInfo().
 *
 *  5.  The database key field that XIA uses is called "DB_KEY".
 *  6a.  In much earlier configurations of our component database, the DB key was "OLD_DB_KEY".
 *
 *  6.  XIA footprints distributed via database must end in a suffix that includes:
 *  (a) The slash "/" character (each footprint name must have only this one "/" char).
 *  (b) One or more capital letters that indicate what type of footprint this is (IPC compliant, experimental, etc.).
 *  (c) A versioning number.
 *  Example footprint name:  "CAP_SM_0402/I2".
 *  See StripVersionNumbersFromFootprintName().
 *
 *  7.  A database field made available to Altium that contains a duplicate copy of the
 *  desired schematic symbol name.  This field is currently called 'DBSCHSYMBOL'.
 *
 * NOTES RE/ SCRIPT PROBLEMS:
 *  1.  This script will always generate both _Debug.txt and _Report.txt files.
 *  The _Report.txt file contains only what's shown in the last dialog box on
 *  screen.  However, the _Debug.txt file contains lots of debugging information.
 *  If this script ever aborts due to some unexpected and/or unexplained-on-screen
 *  error, be sure to check the _Debug.txt file and try to figure out what
 *  happened.  If you had a previous version of the _Debug.txt file open, be
 *  sure to close the file and re-open it.  Some text editors will not detect
 *  that this file has changed underneath it.
 *
 *  2.  This script will make a copy of the _Report.txt file with date and time
 *  embedded in the file name and put it in the ProjectLogs/ directory.  The idea
 *  is that this is the equivalent of the Altium DatabaseMessages.txt or ECO log file.
 *  
 ***************************************************************************}


uses
SysUtils;

{***************************************************************************
 * Forward declarations for form objects.
 ***************************************************************************}
Interface

type
   TUpdateFromDatabaseForm = class(TForm)
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

   formCbFlagOp0_1 : TCheckBox;

   formButtonOk: TButton;                 
   formButtonCancel: TButton;             
   formButtonsLabel1 :TLabel;         
   formStatusBar1: TXStatusBar;   
   procedure TUpdateFromDatabaseForm.clickedOk(Sender : TPanel);
   procedure TUpdateFromDatabaseForm.clickedCancel(Sender : TPanel);
   procedure TUpdateFromDatabaseForm.bCheck0_1(Sender : TPanel);
end;


{***************************************************************************
 * Global constants.
 ***************************************************************************}
const
{* Declare the version and name of this script. *}
   constScriptVersion          = 'v1.14.0_gc $Revision$';
   constThisScriptNameNoExt    = 'XIA_Update_From_Database';
   constThisScriptName         = constThisScriptNameNoExt + '.pas';

{ Note:  We implicitly rely on a number of constants defined in XIA_Utils.pas.
 That script and this one must both be part of an Altium script project!
 That way, we can use constants and functions defined in the other script. }
   

{***************************************************************************
 * Global variables.  Highly evil.  Ick ick.
 ***************************************************************************}
var
   UpdateFromDatabaseForm : TUpdateFromDatabaseForm;
   enableFootprintUpdates : Boolean;	{ Are we allowed to update/modify/add/delete footprints? }


{***************************************************************************
 * procedure TUpdateFromDatabaseForm.clickedOk()
 *  This is the handler for primary dialog box "OK" click.
 ***************************************************************************}
procedure TUpdateFromDatabaseForm.clickedOk(Sender : TPanel);
begin

//   ShowMessage('Hello world from TUpdateFromDatabaseForm.clickedOk()');

   { Attempt to close UpdateFromDatabaseForm. }
//   UpdateFromDatabaseForm.Close;
   Close;

end; { end TUpdateFromDatabaseForm.clickedOk() }


{***************************************************************************
 * procedure UfdUpdateGuiStatusMessage()
 *  Update status message in dialog box and write said status to the debug file.
 ***************************************************************************}
procedure UfdUpdateGuiStatusMessage(msg :  TDynamicString);
begin

   { Change text in GUI status line. }
   formStatusBar1.SimpleText := msg;

   { Force a screen refresh of GUI status line. }
   formStatusBar1.Update;

   { Copy this message to debug file. }
   WriteToDebugFile('');
   WriteToDebugFile('* ' + msg);

end; { end UfdUpdateGuiStatusMessage() }


{***************************************************************************
 * procedure UfdAtExit()
 *  Put results in our dialog box list and return to AtExit() for the rest of the cleanup routines.
 ***************************************************************************}
procedure UfdAtExit(rc : Integer);
var
   i        : Integer;

begin 

   {* Transform existing GUI dialog box so that there is a big list box available. *}

   { Nuke most text fields to make room for the big list box. }
   UpdateFromDatabaseForm.formText03.Free;
   UpdateFromDatabaseForm.formText04.Free;
   UpdateFromDatabaseForm.formText05.Free;
   UpdateFromDatabaseForm.formText06.Free;
   UpdateFromDatabaseForm.formText07.Free;
   UpdateFromDatabaseForm.formText08.Free;
   UpdateFromDatabaseForm.formText09.Free;
   UpdateFromDatabaseForm.formText10.Free;
   UpdateFromDatabaseForm.formText11.Free;
   UpdateFromDatabaseForm.formText12.Free;
   UpdateFromDatabaseForm.formText13.Free;
   UpdateFromDatabaseForm.formText14.Free;
   UpdateFromDatabaseForm.formText15.Free;
   UpdateFromDatabaseForm.formText16.Free;
   UpdateFromDatabaseForm.formText17.Free;
   UpdateFromDatabaseForm.formText18.Free;
   UpdateFromDatabaseForm.formText19.Free;
   UpdateFromDatabaseForm.formText20.Free;
   UpdateFromDatabaseForm.formText21.Free;
   UpdateFromDatabaseForm.formText22.Free;
   UpdateFromDatabaseForm.formText23.Free;
   UpdateFromDatabaseForm.formText24.Free;
   UpdateFromDatabaseForm.formText25.Free;
   UpdateFromDatabaseForm.formText26.Free;
   UpdateFromDatabaseForm.formText27.Free;
   UpdateFromDatabaseForm.formText28.Free;
   UpdateFromDatabaseForm.formText29.Free;
   UpdateFromDatabaseForm.formText30.Free;

   { Transform existing GUI dialog box so that there is a big list box available. }
   UpdateFromDatabaseForm.listBox1.Left := 14;
   UpdateFromDatabaseForm.listBox1.Top := 40;
   UpdateFromDatabaseForm.listBox1.Width := 972;
   UpdateFromDatabaseForm.listBox1.Height := 640;

   { Nuke Sync footprints checkbox. }
   UpdateFromDatabaseForm.formCbFlagOp0_1.Free;

   { Move Ok button to center. }
   UpdateFromDatabaseForm.formButtonOk.Left := 450;
   UpdateFromDatabaseForm.formButtonOk.Enabled := True;
   UpdateFromDatabaseForm.formButtonOk.Update;

   { Nuke Cancel button. }
   UpdateFromDatabaseForm.formButtonCancel.Free;

   { Run GUI dialog box to display the summary messages. }
   //        ShowMessage('About to display modal dialog');
   UpdateFromDatabaseForm.formText01.Caption := SummaryMessages.Strings[0];

   UpdateFromDatabaseForm.listBox1.Clear;


   {* Proceed to output summary messages in the list box. *}
   
   { Loop over all the summary messages. }
   for i := 1 to SummaryMessages.Count - 1 do
   begin

      { Add this line of message to the list box on screen. }
      UpdateFromDatabaseForm.listBox1.Items.Insert((i-1), SummaryMessages.Strings[i]);

      { Update the size of the horizontal scroll bars if needed. }
      { Code stolen from http://www.delphipages.com/forum/showthread.php?t=203460 }
//    if (UpdateFromDatabaseForm.listBox1.Canvas.TextWidth(SummaryMessages.Strings[i] + 'W') > UpdateFromDatabaseForm.listBox1.ScrollWidth) then
//    begin
//       UpdateFromDatabaseForm.listBox1.ScrollWidth := UpdateFromDatabaseForm.listBox1.Canvas.TextWidth(SummaryMessages.Strings[i] + 'W');
//    end;

      { For some reason, that's not enough.  Double it instead of adding the width of a 'W' char. }
      if (UpdateFromDatabaseForm.listBox1.Canvas.TextWidth(SummaryMessages.Strings[i] + SummaryMessages.Strings[i]) > UpdateFromDatabaseForm.listBox1.ScrollWidth) then
      begin
         UpdateFromDatabaseForm.listBox1.ScrollWidth := UpdateFromDatabaseForm.listBox1.Canvas.TextWidth(SummaryMessages.Strings[i] + SummaryMessages.Strings[i]);
      end;    

   end;

   { If we were given a sucess error code, meaning we're exiting successfully, report that. }
   if ( (rc = 0) and (enableGenerateOutputs = True) and (enableSvnCommits = True) ) then
   begin
      UpdateFromDatabaseForm.formButtonsLabel1.Caption := 'Script is exiting successfully.  Click Ok to finish.';
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
            UpdateFromDatabaseForm.formButtonsLabel1.Caption := 'ERROR ERROR ERROR Script was running with enableGenerateOutputs set to False, meaning that I generated no outputs!!!';
         end;

         { Report if enableSvnCommits was disabled. }
         if (enableSvnCommits = False) then
         begin
            UpdateFromDatabaseForm.formButtonsLabel1.Caption := 'ERROR ERROR ERROR Script was running with enableSvnCommits set to False, meaning that I did no svn commits!!!';
         end;

         { Report if we have an error code from elsewhere in this script. }
         if (rc <> 0) then
         begin
            UpdateFromDatabaseForm.formButtonsLabel1.Caption := 'Failed while running this operation: ' +                 formStatusBar1.SimpleText + constLineBreak + constLineBreak +
            'ERROR:  Script is exiting prematurely due to error!';
         end;
         
      end;

   end;

   { Append a list of system fonts available. }
// UpdateFromDatabaseForm.listBox1.Items.AddStrings(Screen.Fonts);

   
   { Display UpdateFromDatabase dialog box that we just finished configuring. }
   //        UpdateFromDatabaseForm.ShowModal;
   
   { Since that form is already modal, we simply exit and rely on the click handler
    to do the remaining cleanup. }   


end; { end UfdAtExit() }
   

{***************************************************************************
 * function SplitDelimitedStringIntoList()
 *  Split apart a delimited string.  We can't use the builtin TStringList
 *  function for this because it will also split on spaces, which we don't want.
 *
 *  Handle some of the idiosyncracies of the .csv format as implemented by Excel.
 *  See http://creativyst.com/Doc/Articles/CSV/CSV01.htm .
 *
 *  Specifically, support:
 *  1.  Quoting a comma character within a field by surrounding the whole field
 *  with double quotes (").
 *  2.  Support an actual double-quote character (") being encoded as "" (two
 *  double-quote chars).
 *
 *  TODO:
 *  1.  Support quoted numerics of the form ="08075" that will survive import/export
 *  to/from Excel and not have leading 0's removed, number converted to scientific
 *  notation, etc.
 *
 *  LIMITATIONS:
 *  1.  This code does not do a character-by-character stateful inspection.  The
 *  current implementation of double-quoted field support WILL break if you attempt
 *  to combine a quoted field with an escaped double-quote followed by a comma.
 *  eg. "This is a quoted field with escaped quotes and embedded commas "", that will break the parser."
 *                                                                        ^ Parser will break this into 2 fields at this point.
 *
 *  Returns string list in var parm list.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function SplitDelimitedStringIntoList(    delimString : TDynamicString;
                                          delimiter   : String;
                                      var list        : TStringList;
                                          )           : Integer;

var
   position  : Integer;
   leftStr   : TDynamicString;
   rightStr  : TDynamicString;
   haveQuote : Boolean;
   
begin

   { Assume success. }
   result := 0;

   {* Initialize loop. *}
   { The initial right string is the entire delimString, but with escaped-quotes ("") replaced with '|' chars. }
   rightStr := StringReplace(delimString, (constStringQuote + constStringQuote), constStringDelimiter, MkSet(rfReplaceAll));

   {* Loop until we run out of delimiters. *}
   { Note:  Here we assume that there is not a delimiter at the end of the delimString! }
   while (AnsiPos(delimiter, rightStr) <> 0) do
   begin

      { Determine if this field starts with a quote char. }
      { Note:  Leftmost index in a string for purposes of Copy() is 1! }
      if (Copy(rightStr, 1, 1) = constStringQuote) then
      begin

         { Flag that this is a quoted field. }
         haveQuote := True;

         { Find the position of the next closequote-delimiter pair. }
         position := AnsiPos((constStringQuote + delimiter), rightStr);
//         WriteToDebugFile('Have quoted field.  Initial rightStr is "' + rightStr + '", position is ' + IntToStr(position));

         { The left string is everything up until the char before the delimiter, excluding the initial openquote. }
         { Note:  Leftmost index in a string for purposes of Copy() is 1! }
         leftStr := Copy(rightStr, 2, (position-1-1));
//         WriteToDebugFile('Have quoted field.  New leftStr is "' + leftStr + '".');

         { The new right string is everything after the closequote-delimiter pair. }
         rightStr := Copy(rightStr, (position+2), MaxInt);
//         WriteToDebugFile('Have quoted field.  New rightStr is "' + rightStr + '".');

      end { endif }

      { Else this is not a quoted field. }
      else
      begin
      
         { Flag that this is a non-quoted field. }
         haveQuote := False;

         { Find the position of the next delimiter character. }
         position := AnsiPos(delimiter, rightStr);
//         WriteToDebugFile('Have non-quoted field.  Initial rightStr is "' + rightStr + '", position is ' + IntToStr(position));

         { The left string is everything up until the char before the delimiter. }
         { Note:  Leftmost index in a string for purposes of Copy() is 1! }
         leftStr := Copy(rightStr, 1, (position-1));
//         WriteToDebugFile('Have non-quoted field.  New leftStr is "' + leftStr + '".');

         { The new right string is everything after the delimiter char. }
         rightStr := Copy(rightStr, (position+1), MaxInt);
//         WriteToDebugFile('Have non-quoted field.  New rightStr is "' + rightStr + '".');

      end; { endelse }

      { Convert any remapped escaped quote characters ("") but now (|) back into a single quote char ("). }
      leftStr := StringReplace(leftStr, constStringDelimiter, constStringQuote, MkSet(rfReplaceAll));

      { Add the left string to our running string list. }
      list.Add(leftStr);
//      WriteToDebugFile(' In SplitDelimitedStringIntoList(), adding substring "' + leftStr + '".');
      
   end; { endwhile }

   { If the last field started with an openquote, then it must end with a closequote.
    Exclude that last character. }
   if (haveQuote) then
   begin

      { Strip off final character. }
      SetLength(rightStr, (Length(rightStr)-1));

   end; { endif }

   { Convert any remapped escaped quote characters ("") but now (|) back into a single quote char ("). }
   rightStr := StringReplace(rightStr, constStringDelimiter, constStringQuote, MkSet(rfReplaceAll));

   { Add the last string to the string list. }
   list.Add(rightStr);
//   WriteToDebugFile(' In SplitDelimitedStringIntoList(), adding substring "' + rightStr + '".');
      
end; { end SplitDelimitedStringIntoList() }


{***************************************************************************
 * function EncodeLocalParameters()
 *  Encode all local parameters read from .xml files before storing in
 *  stringlists.
 *
 *  Returns encoded string.
 ***************************************************************************}
function EncodeLocalParameters(rawParmValue : TDynamicString;
                               )            : TDynamicString;

begin

   WriteToDebugFile('In EncodeLocalParameters(), rawParmValue starts as "' + rawParmValue + '".');

   { Handle the .xml encoding of a double quote char ("). }
   rawParmValue := StringReplace(rawParmValue, '&quot;', '"', MkSet(rfReplaceAll));

   { Handle various other .xml encodings. }
   rawParmValue := StringReplace(rawParmValue, '&amp;', '&', MkSet(rfReplaceAll));
   rawParmValue := StringReplace(rawParmValue, '&gt;', '>', MkSet(rfReplaceAll));
   rawParmValue := StringReplace(rawParmValue, '&lt;', '<', MkSet(rfReplaceAll));

   { Make sure there are no constStringLocDbParmsEqualsRemap chars in the raw parameter values, since we're trying
    to use this as the char to remap '=' chars to. }
   if (AnsiPos(constStringLocDbParmsEqualsRemap, rawParmValue) <> 0) then
      MyAbort('Sorry, but there is a "\" char in a local parameter value read from .xml file.  This is a reserved character.');
   
   
   { We will change all '=' chars into constStringLocDbParmsEqualsRemap chars since '=' is our Name/Value
    separator character and we have not had luck trying to assign a different
    Name/Value separator char and have name searches still work.
    Possibly this is an Altium scripting bug. }
   result := StringReplace(rawParmValue, '=', constStringLocDbParmsEqualsRemap, MkSet(rfReplaceAll));

   WriteToDebugFile('In EncodeLocalParameters(), result is "' + result + '".');

end; { end EncodeLocalParameters() }


{***************************************************************************
 * function DecodeLocalParameters()
 *  Decode all local parameters read from stringlists before use.
 *
 *  Returns decoded string.
 ***************************************************************************}
function DecodeLocalParameters(encodedParmValue : TDynamicString;
                               )                : TDynamicString;

begin
   WriteToDebugFile('In DecodeLocalParameters(), encodedParmValue starts as "' + encodedParmValue + '".');

   { Change all constStringLocDbParmsEqualsRemap chars back to '='. }
   result := StringReplace(encodedParmValue, constStringLocDbParmsEqualsRemap, '=', MkSet(rfReplaceAll));

   WriteToDebugFile('In DecodeLocalParameters(), result is "' + result + '".');

end; { end DecodeLocalParameters() }


{***************************************************************************
 * function ReadXmlDataIntoCache()
 *  Read simple data from an .xml file.
 *
 *  For our purposes here, we make the following simplifying assumptions:
 *  1.  We will not read/parse/validate the entire .xml file.
 *  2.  We only care about the cell data itself.
 *  3.  We look for <Row> and </Row> tags.
 *  4.  Within such, we look for <Cell> and </Cell> tags.
 *  5.  Within such, we look for <Data> and </Data> tags.
 *  6.  We will parse "Index="x"" within a <Cell> tag.
 *
 *  Directly store this information in our homebrewed database cache.
 *  This cache consists of two stringlists:  a cache list and an index list.
 *  
 *  In the cache list, we store NAME=VALUE pairs for parameter names and
 *  parameter values.
 *  
 *  Once we have stored all the NAME=VALUE pairs for a given component,
 *  we write an entry to the index list specifying the component KEY
 *  and the starting and ending indices of the NAME=VALUE pairs for that
 *  particular component.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function ReadXmlDataIntoCache(    xmlFilePath     : TDynamicString;
                              var locDbKeyName    : TDynamicString;
                              var locDbParmsCache : TStringList;
                              var locDbParmsIndex : TStringList;
                                  )               : Integer;

var
   i                : Integer;
   j                : Integer;
   colIndex         : Integer;
   indexVal         : Integer;
   position         : Integer;
   rawXmlFile       : TStringList;
   state            : Integer;
   line             : TDynamicString;
   cellField        : TDynamicString;
   indexField       : TDynamicString;
   dataField        : TDynamicString;
   justData         : TDynamicString;
   recordFieldNames : Boolean;
   fieldNames       : TStringList;
   nameValuePair    : TDynamicString;
   leftStr          : TDynamicString;
   rightStr         : TDynamicString;
   locDbParmsStart  : Integer;
   locDbParmsEnd    : Integer;
   locDbKey         : TDynamicString;
   
begin

   { Assume success. }
   result := 0;

   { Flag that we do not yet know the name of the local database key field. }
   locDbKeyName := '';
   
   { Since we have not yet populated this local database cache, init both of these to 0. }
   locDbParmsStart  := 0;
   locDbParmsEnd    := 0;
   
   { Create stringlist to read in raw .xml file. }
   rawXmlFile := TStringList.Create;
   fieldNames := TStringList.Create;

   { Verify that file is readable (eg. not flocked by Excel). }
   VerifyFileIsReadable(xmlFilePath);
         
   { Read raw xml file into stringlist. }
   rawXmlFile.LoadFromFile(xmlFilePath);

   { Init state to be 0. }
   state := 0;

   { Flag that we should be recording field names from the top line of the XML file. }
   recordFieldNames := True;

   
   { Loop over all the lines in the file. }
   for i := 0 to (rawXmlFile.Count-1) do
   begin

      { Cache current line of file. }
      line := rawXmlFile.Strings(i);
      WriteToDebugFile('Line is "' + line + '".');

      { Examine current state and act appropriately. }
      case state of

        { State 0:  Look for a <Row> tag. }
        0 :
           begin

              { If we find a '<Row' tag, then advance state. }
              if (AnsiPos('<Row', line) <> 0) then
              begin

                 { Record the starting index of the new database record that we're going to add to the cache. }
                 locDbParmsStart := locDbParmsCache.Count;

                 { Clear the key field for this line. }
                 locDbKey          := '';
                 
                 { Indicate that we will start getting data for column 0. }
                 colIndex := 0;
                 
                 { Advance state. }
                 state := 1;
                 WriteToDebugFile('Found <Row.  Advancing state!');
                 
              end; { endif }
                 
           end; { endcase 0 }

        { State 1:  Look for a <Cell> tag. }
        1 :
           begin

              { If we find a '</Row' tag, then advance state. }
              if (AnsiPos('</Row', line) <> 0) then
              begin

                 { Record the ending index of the new database record that just added to the cache. }
                 locDbParmsEnd   := (locDbParmsCache.Count - 1);

                 { Create a new record in the cache index, now that we've added this new entry. }
                 { If this were locDbKey C000010001 and it now occupied indices 33 to 75 in the cache, the
                  entry in index would be "C000010001=33|75". }
                 nameValuePair    := locDbKey + constStringEquals + IntToStr(locDbParmsStart) + constStringDelimiter + IntToStr(locDbParmsEnd);
                 
                 locDbParmsIndex.add(nameValuePair);
                 WriteToDebugFile('In ReadXmlDataIntoCache(), adding new entry to cache index : "' + nameValuePair + '".');

                 { Flag that we are done recording field names from the top line of the XML file. }
                 recordFieldNames := False;
                 
                 { Advance state. }
                 state := 0;
                 WriteToDebugFile('Found </Row.  Advancing state!');

              end { endif }
                 
              { Else if we find a '<Cell' tag, then proceed to look for <Data> tag. }
              else if (AnsiPos('<Cell', line) <> 0) then
              begin

//                 WriteToDebugFile('Found "<Cell".  Proceeding.');

                 { Copy everything starting with '<Cell> to cellField. }
                 cellField  := Copy(line, AnsiPos('<Cell', line), MaxInt);
//                 WriteToDebugFile('cellField is "' + cellField + '".');

                 { Look for '>' within cellField. }
                 position   := AnsiPos('>', cellField);
                 if (position = 0) then
                    MyAbort('In ReadXmlDataIntoCache(), did not find ">" to end <Cell> tag.');

                 { Copy everything after end of <Cell> tag as dataField. }
                 dataField  := Copy(cellField, (position+1), MaxInt);
//                 WriteToDebugFile('dataField is "' + dataField + '".');

                 { Chop off everything after end of <Cell> tag in cellField. }
                 SetLength(cellField, position);
//                 WriteToDebugFile('cellField is now "' + cellField + '".');

                 { Look for optional "Index=" within the <Cell> tag. }
                 position   := AnsiPos('Index=', cellField);
                 if (position <> 0) then
                 begin

                    { If we have an "Index=" constuct in lieu of column 0, which is
                     our key column, we have a serious problem. }
                    if (colIndex = 0) then
                       MyAbort('In ReadXmlDataIntoCache(), missing key field for local database record (row)!');
                    
                    { Extract just the "Index="x"" portion. }
                    indexField := Copy(cellField, position, MaxInt);
                    WriteToDebugFile('indexField is "' + indexField + '".');

                    { Look for '>' within indexField. }
                    position   := AnsiPos('>', indexField);
                    if (position = 0) then
                       MyAbort('In ReadXmlDataIntoCache(), did not find ">" to end "Index=".');

                    { Chop off indexField to end just before ">". }
                    SetLength(indexField, (position-1));
                    WriteToDebugFile('indexField is now "' + indexField + '".');

                    { Look for ' ' (space) within chopped indexField. }
                    position   := AnsiPos(' ', indexField);
                    if (position <> 0) then
                    begin
                       SetLength(indexField, (position-1));
                       WriteToDebugFile('indexField is now "' + indexField + '".');
                    end; { endif }                    

                    { Strip out all quote chars from indexField. }
                    indexField := StringReplace(indexField, constStringQuote, '', MkSet(rfReplaceAll));
//                    WriteToDebugFile('indexField is now "' + indexField + '".');

                    { Split the indexField into "Index=" and "x". }
                    SplitStringIntoLeftAndRight(indexField,
                                                constStringEquals,
                                                {var} leftStr,
                                                {var} rightStr);

                    { Convert rightStr to integer. }
                    indexVal := StrToInt(rightStr);
//                    WriteToDebugFile('indexVal is ' + IntToStr(indexVal) + '.');

                    { We have null data (null parameter values) to write. }
                    justData := '';
                    
                    { When we have this "Index=" construct at all, it's because Excel
                     is skipping over a number of blank fields.  So we need to loop
                     over all the blank fields between the last field and the given
                     index of this upcoming field and insert blanks for all such fields. }
                    for colIndex := colIndex to (indexVal-2) do
                    begin

                       { Create new NAME=VALUE pair to store parameter name and parameter value. }
                       nameValuePair    := fieldNames.Strings(colIndex) + constStringEquals + justData;

                       { Write this NAME=VALUE pair to cache list. }
                       locDbParmsCache.add(nameValuePair);
                       WriteToDebugFile('New nameValuePair being added to cache is "' + nameValuePair + '".');

                    end; { endfor }

                 end; { endif }

                 { Look for <Data> tag. }
                 position   := AnsiPos('<Data', dataField);
                 if (position = 0) then
                    MyAbort('In ReadXmlDataIntoCache(), did not find "<Data" tag.');

                 { Look for '>' within dataField. }
                 position   := AnsiPos('>', dataField);
                 if (position = 0) then
                    MyAbort('In ReadXmlDataIntoCache(), did not find ">" to end <Data> tag.');

                 { Copy off just the data part of the dataField. }
                 justData := Copy(dataField, (position+1), MaxInt);
//                 WriteToDebugFile('justData is "' + justData + '".');

                 { Look for '</Cell>' within justData. }
                 position   := AnsiPos('</Cell>', justData);
                 if (position = 0) then
                    MyAbort('In ReadXmlDataIntoCache(), did not find "</Cell>" to end <Cell> tag.');

                 { Look for '</Data>' within justData. }
                 position   := AnsiPos('</Data>', justData);
                 if (position = 0) then
                    MyAbort('In ReadXmlDataIntoCache(), did not find "</Data>" to end <Data> tag.');

                 { Chop off justData to end just before </Data>. }
                 SetLength(justData, (position-1));
//                 WriteToDebugFile('justData is now "' + justData + '".');

                 { See if we need to record this as a field name. }
                 if (recordFieldNames) then
                 begin

                    fieldNames.Add(justData);

                    { If needed, record the name of the local database key field. }
                    if (locDbKeyName = '') then
                    begin
                       
                       locDbKeyName := justData;
                       WriteToDebugFile('Found name of local database key field.  It is "' + locDbKeyName + '".');
                       
                    end; { endif }
                       
                 end { endif }

                 { Else we have a new property. }
                 else
                 begin

                    { Suppress any parameters that have value "*" to have null value. }
                    { This is seen due to copy-pasting from Altium Parameter Manager to Excel. }
                    if (justData = '*') then
                       justData := '';

                    { Escape any evil characters that may be in the raw string read from .xml file. }
                    justData := EncodeLocalParameters(justData);
                                        
                    { See if we need to record the key field for this line. }
                    if (colIndex = 0) then
                    begin

                       { Store the key field for this line. }
                       locDbKey          := justData;
                       
                    end; { endif }

                    { Create new NAME=VALUE pair to store parameter name and parameter value. }
                    nameValuePair    := fieldNames.Strings(colIndex) + constStringEquals + justData;

                    { Write this NAME=VALUE pair to cache list. }
                    locDbParmsCache.add(nameValuePair);
                    WriteToDebugFile('New nameValuePair being added to cache is "' + nameValuePair + '".');

                 end; { endelse }

                 { Increment column index. }
                 colIndex := colIndex + 1;
                 
              end; { endif }

           end; { endcase 1 }

      else MyAbort('In ReadXmlDataIntoCache(), in unknown state ' + IntToStr(state));
      end; { endcase }          
      
   end; { endfor }

   { Free local stringlists. }
   rawXmlFile.Free;
   fieldNames.Free;
   
end; { end ReadXmlDataIntoCache() }


{***************************************************************************
 * function ChangeFullLogRefDesesToLogRefDeses()
 *  The .xml files (due to being copy-pasted from Altium parameter manager)
 *  are likely to contain a line for each subpart of a given component
 *  (eg. separate lines for J2A, J2B, J2C, etc.).  However, when we parse
 *  the logical design later on, we will only see it as "J2", not "J2A".
 *  And it's not easy to get from one to the other, due to the differences
 *  between ISch_Component and IComponent datastructures.
 *
 *  So what we're going to do is to change all J2A's to J2's, etc. in the
 *  local database parameter caches that we already have in memory.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function ChangeFullLogRefDesesToLogRefDeses(    Project                 : IProject;
                                                refDesFieldName         : TDynamicString;
                                            var locDbParmsByRefDesCache : TStringList;
                                            var locDbParmsByRefDesIndex : TStringList;
                                                )                       : Integer;

var
   flatSchem     : IDocument;
   i             : Integer;
   component     : IComponent;
   part          : IPart;
   logRefDes     : TDynamicString;
   fullLogRefDes : TDynamicString;
   position      : Integer;
   indexValue    : TDynamicString;

begin
   
   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   {** Analyze flattened pseudo-schematic in order to map physical designators to logical designators, etc. **}
   { Note:  Code borrowed & adapted from AgileBOMV1.1.pas. }

   { Get a reference to the flattened schematic document. }
   flatSchem := Project.DM_DocumentFlattened;

   { Sanity check. }
   if (flatSchem = Nil) then
      MyAbort('Unable to get flatSchem.  Probably this means the project hasn''t been compiled, but it should have been so already.');

   { Output debug info. }
   WriteToDebugFile('*About to process flattened components....');

   { Loop over all physical components in flattened schematic document. }
   for i := 0 to (flatSchem.DM_ComponentCount - 1) do
   begin

      { Retrieve reference to next flattened schematic component. }
      component := flatSchem.DM_Components(i);

      { Retrieve reference to the 0th subpart of this component. }
      part         := component.DM_SubParts(0);

      { Retrieve logical refdes for this component and full logical refdes of the 0th subpart of this component. }
      logRefDes     := part.DM_LogicalDesignator;
      fullLogRefDes := part.DM_FullLogicalDesignator;

      
      { See if the logical refdes (eg. "J2") for this component differs from the 0th full logical refdes (eg. "J2A"). }
      if (logRefDes <> fullLogRefDes) then
      begin

         WriteToDebugFile('* In ChangeFullLogRefDesesToLogRefDeses(), about to change full logical refdes "' + fullLogRefDes + '" into logical refdes "' + logRefDes + '".');

         { Look for the entry for this component in the locDbParmsByRefDesCache stringlist. }
         position := locDbParmsByRefDesCache.IndexOf(refDesFieldName + '=' + fullLogRefDes);

         { See if we found it. }
         if (position >= 0) then
         begin

            WriteToDebugFile('* Found cache entry with full logical refdes at index ' + IntToStr(position) + '.');
            WriteToDebugFile('* Original cache entry is "' + locDbParmsByRefDesCache.Strings(position) + '".');

            { Overwrite this entry with new one using the logical refdes. }
            locDbParmsByRefDesCache.Strings(position) := (refDesFieldName + '=' + logRefDes);
            WriteToDebugFile('* New cache entry is      "' + locDbParmsByRefDesCache.Strings(position) + '".');

            { Now we must also change the entry in the cache index. }
            position := locDbParmsByRefDesIndex.IndexOfName(fullLogRefDes);

            { Sanity check }
            if (position < 0) then
               MyAbort('Found inconsistency in local database parameter cache!');

            { Retrieve the value of the cache index entry. }
            indexValue := locDbParmsByRefDesIndex.ValueFromIndex(position);
            WriteToDebugFile('* Original index entry is "' + locDbParmsByRefDesIndex.Strings(position) + '".');
            
            { Overwrite this entry with new one using the logical refdes. }
            locDbParmsByRefDesIndex.Strings(position) := (logRefDes + '=' + indexValue);
            WriteToDebugFile('* New index entry is      "' + locDbParmsByRefDesIndex.Strings(position) + '".');

         end; { endif found entry in local database cache }
         
      end; { endif logical refdes differs from full logical refdes }
   
   end; { endfor }

end; { end ChangeFullLogRefDesesToLogRefDeses() }


{***************************************************************************
 * function InitDatabaseInfo()
 *  Initialize database info at start of script.
 *  
 *  This function will attempt to change any DBLib file to the approved
 *  DBLib file, specified with an absolute path.
 *
 *  This function will add the required DBLib file if no DBLib file exists.
 *
 *  This function will also replace any DBLink file with the approved
 *  DBLink file, specified with an absolute path.
 *
 *  Returns reference to DBLib file as var parm dbLibDoc.
 *  Returns newly created database table list as var parm dbTableList.
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function InitDatabaseInfo(    Project                 : IProject;
                              projectName             : TDynamicString;
                              projectPath             : TDynamicString;
                          var locDbKeyName            : TDynamicString;
                          var locDbParmsByRefDesCache : TStringList;
                          var locDbParmsByRefDesIndex : TStringList;
                          var locDbParmsByKeyCache    : TStringList;
                          var locDbParmsByKeyIndex    : TStringList;
                          var dbLibDoc                : IDatabaseLibDocument;
                          var dbTableList             : TStringList;
                          var changedFiles            : TStringList;
                              )                       : Integer;

var
   i                : Integer;
   k                : Integer;
   document         : IDocument;
   fieldNames       : WideString;
   leftStr          : WideString;
   rightStr         : WideString;
   tableIndex       : Integer;
   dbTableName      : TDynamicString;
   hasCorrectDbLib  : Boolean;
   hasDbLink        : Boolean;
   hasCorrectDbLink : Boolean;
   refDesFieldName  : TDynamicString;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from InitDatabaseInfo()');

   { Until we find out otherwise, there is no legacy/local database or database key. }
   locDbKeyName := '';   
   
   {* Verify that the project the correct DBLib file attached. *}
   UfdUpdateGuiStatusMessage('Status:  Checking DBLib files.');
   hasCorrectDbLib := False;

   { Look over all project documents and see if we find a DBLib file. }
   {** Loop over all logical documents in the project (counting backwards). **}
   for k := (Project.DM_LogicalDocumentCount - 1) downto 0 do
   begin
      document := Project.DM_LogicalDocuments(k);
      WriteToDebugFile('Examining project document ' + document.DM_FullPath);
      
      { See if this document is a DBLib file. }
      if (document.DM_DocumentKind = constKindDbLib) then
      begin

         { Determine if the project has the required DBLib file. }
         if (document.DM_FullPath = constRequiredDbLibFilePath) then
         begin

            { Flag that the project has the required DBLib file. }
            hasCorrectDbLib    := True;
            
         end

         { Else this is some other DBLib file, or possibly the correct one but with a relative path. }
         else
         begin

            { Attempt to remove this DBLib file. }
            WriteToDebugFile('Removing DBLib file "' + document.DM_FullPath + '" from project.');
            WriteToSummaryFile('INFO:     ' + 'Removing DBLib file "' + document.DM_FullPath + '" from project.');
            Project.DM_RemoveSourceDocument(document.DM_FullPath);
            
            { Add project file to the list of changed files. }
            changedFiles.Add(Project.DM_ProjectFileName);
      
         end;
         
      end; { endif }

   end; { endfor }

   { If the project does not have the required DBLib file, then add it. }
   if (not hasCorrectDbLib) then
   begin

      { Attempt to add our XIA_Database_Kludged_no_sym_no_desc_no_footprints.DBLib file, with the proper path. }
      Project.DM_AddSourceDocument(constRequiredDbLibFilePath);
      WriteToDebugFile('Added required DBLib file "' + constRequiredDbLibFilePath + '" to project.');
      WriteToSummaryFile('INFO:     ' + 'Added required DBLib file "' + constRequiredDbLibFilePath + '" to project.');

      { Add project file to the list of changed files. }
      changedFiles.Add(Project.DM_ProjectFileName);
      
   end; { endif }


   { Attempt to open DBLib file. }
   { TODO:  What's the point of this operation again? }
   dbLibDoc := IntegratedLibraryManager.GetAvailableDBLibDocAtPath(constRequiredDbLibFilePath);
   
   { Sanity check. }
   if (dbLibDoc = Nil) then
      MyAbort('Unable to open required DBLib file "' + constRequiredDbLibFilePath + '"!');


   {* Verify that if the project has a DBLink file DBLib, that the correct one is attached. *}
   UfdUpdateGuiStatusMessage('Status:  Checking DBLink files.');
   hasDbLink        := False;
   hasCorrectDbLink := False;

   { Look over all project documents and see if we find a DBLink file. }
   {** Loop over all logical documents in the project (counting backwards). **}
   for k := (Project.DM_LogicalDocumentCount - 1) downto 0 do
   begin
      document := Project.DM_LogicalDocuments(k);
      WriteToDebugFile('Examining project document ' + document.DM_FullPath);
      
      { See if this document is a DBLink file. }
      if (document.DM_DocumentKind = constKindDbLink) then
      begin

         { Flag that the project has DBLink file(s). }
         hasDbLink      := True;

         { Determine if the project has the approved DBLink file. }
         if (document.DM_FullPath = constApprovedDblinkFilePath) then
         begin

            { Flag that the project has the approved DBLink file. }
            hasCorrectDbLink    := True;
            
         end

         { Else this is some other DBLink file, or possibly the correct one but with a relative path. }
         else
         begin

            { Attempt to remove this DBLink file. }
            WriteToDebugFile('Removing DBLink file "' + document.DM_FullPath + '" from project.');
            WriteToSummaryFile('INFO:     ' + 'Removing DBLink file "' + document.DM_FullPath + '" from project.');
            Project.DM_RemoveSourceDocument(document.DM_FullPath);
            
            { Add project file to the list of changed files. }
            changedFiles.Add(Project.DM_ProjectFileName);
      
         end;
         
      end; { endif }

   end; { endfor }

   { If the project has DBLink files, but not the correct DBLink file, then add correct DBLink file. }
   if ( hasDbLink and (not hasCorrectDbLink) ) then
   begin

      { Attempt to add our Database_link_file_no_symbol_sync.DBLink file, with the proper path. }
      Project.DM_AddSourceDocument(constApprovedDblinkFilePath);
      WriteToDebugFile('Added approved DBLink file "' + constApprovedDblinkFilePath + '" to project.');
      WriteToSummaryFile('INFO:     ' + 'Added approved DBLink file "' + constApprovedDblinkFilePath + '" to project.');

      { Add project file to the list of changed files. }
      changedFiles.Add(Project.DM_ProjectFileName);
      
   end;
   

   {* Get a list of database tables for later use. *}
   { Initialize list of database tables. }
   dbTableList := TStringList.Create;        

   { Loop over all tables in this database. }
   WriteToDebugFile('Getting list of database tables....');
   for tableIndex := 0 to (dbLibDoc.GetTableCount - 1) do
   begin

      { Retrieve name of this database table. }
      dbTableName := dbLibDoc.GetTableNameAt(tableIndex);

      WriteToDebugFile(' Found db table "' + dbTableName + '".');
      dbTableList.Add(dbTableName);

   end; { endfor }


   {* Look for a refdes-to-local-database-parms file as part of this project. }
   { Look over all project documents and see if we find an unknown file. }
   {** Loop over all logical documents in the project (counting backwards). **}
   for k := (Project.DM_LogicalDocumentCount - 1) downto 0 do
   begin
      document := Project.DM_LogicalDocuments(k);
      WriteToDebugFile('Examining project document ' + document.DM_FullPath);

      { See if this is the refdes-to-local-database-key file. }
      if (AnsiUpperCase(document.DM_FullPath) = AnsiUpperCase(projectPath + projectName + '_RefDes_to_parms' + constExtXml)) then
      begin

         WriteToDebugFile('Found RefDes_to_parms file!');

         { Update status message displayed to user. }
         UfdUpdateGuiStatusMessage('Status:  Reading local database (refdes-to-parms) information into in-memory caches.');

         { Attempt to read in XML file. }
         ReadXmlDataIntoCache(document.DM_FullPath,
                              {var} refDesFieldName {locDbKeyName},
                              {var} locDbParmsByRefDesCache {locDbParmsCache},
                              {var} locDbParmsByRefDesIndex {locDbParmsIndex} );


         { We need to strip off trailing "A", "B", etc. from all RefDes'es before
          we let them loose upon the world.  This inconsistency is due to the fact
          that the local .xml files will be created by copy-pasting from Altium
          Parameter Manager, which gives the full logical RefDes (eg. "J2A").
          But later on this script will operate in such a way that it only has
          access to the logical RefDes (eg. "J2").  So we're going to identify
          components where this is even an issue and then overwrite all "J2A"'s
          with "J2"'s, etc. }
         ChangeFullLogRefDesesToLogRefDeses(Project,
                                            refDesFieldName,
                                            {var} locDbParmsByRefDesCache,
                                            {var} locDbParmsByRefDesIndex);
         
      end; { endif }

 
      { See if this is the local-database-key-to-local-parms file. }
      if (AnsiUpperCase(document.DM_FullPath) = AnsiUpperCase(projectPath + projectName + '_local_db_key_to_parms' + constExtXml)) then
      begin

         WriteToDebugFile('Found local_db_key_to_parms file!');

         { Update status message displayed to user. }
         UfdUpdateGuiStatusMessage('Status:  Reading local database (local-db-key-to-parms) information into in-memory caches.');

         { Attempt to read in XML file. }
         ReadXmlDataIntoCache(document.DM_FullPath,
                              {var} locDbKeyName,
                              {var} locDbParmsByKeyCache {locDbParmsCache},
                              {var} locDbParmsByKeyIndex {locDbParmsIndex} );

      end; { endif }
      
   end; { endfor }
   
end; { end InitDatabaseInfo() }


{***************************************************************************
 * function AddSchParm()
 *  Add a new schematic component parameter, given its name and new value.
 *
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function AddSchParm(    document          : IDocument,
                        component         : ISch_Component;
                        suppressFpUpdates : Boolean;
                        dbParmName        : TDynamicString;
                        dbParmValue       : TDynamicString;
                        dbParmVisible     : TDynamicString;
                    var changedFiles      : TStringList;
                        )                 : Integer;

var
   Parameter : ISch_Parameter;
   Model     : ISch_Implementation;

begin

   { Assume success. }
   result := 0;

   { See if it's a footprint (aka model) that we need to add.  These are handled differently. }
   if ( (AnsiPos(constDbParmModelType, dbParmName) <> 0) or (AnsiPos(constDbParmModelName, dbParmName) <> 0) ) then
   begin

      { FIXME:  Currently we assume that all models are of type PCBLIB.
       Thus, we ignore CurrentModelType parms completely. }

      if (AnsiPos(constDbParmModelName, dbParmName) <> 0) then
      begin
         
         { See if we are allowed to update footprints. }
         if ( (enableFootprintUpdates) and (not suppressFpUpdates) ) then
         begin

            { Create a new model for the given schematic component. }
            Model := component.AddSchImplementation;

            { Clear all datafile links. }
            Model.ClearAllDatafileLinks;

            Model.ModelName   := dbParmValue;
            Model.ModelType   := constKindPcbLib;
            Model.Description := '';
            Model.MapAsString := '';

            { Set this model to be not the current one.  We'll double check this later. }
            Model.IsCurrent   := False;

            { Add this file to the list of changed files. }
            changedFiles.Add(document.DM_FileName);
            
            { Tell summary file what we did. }
            WriteToSummaryFile('INFO:     ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Added missing footprint "' + dbParmName + '"="' + dbParmValue + '".');

         end { endif enableFootprintUpdates }

         { Else we are not allowed to update footprints. }
         else
         begin

            { Tell summary file what we were not allowed to fix. }
            WriteToSummaryFile('ERROR:    ' + component.Designator.Name + ' ' + component.Designator.Text + ':  I was not allowed to fix missing footprint "' + dbParmName + '"="' + dbParmValue + '".');
            
         end; { endelse enableFootprintUpdates }

      end; { endif }

   end { endif }

   { Else it's a user parameter. }
   else
   begin
   
      { Add the parameter to the component. }
      Parameter             := SchServer.SchObjectFactory(eParameter, eCreate_Default);
      Parameter.Name        := dbParmName;
      Parameter.ShowName    := False;
      Parameter.Text        := dbParmValue;

      { Make this new parameter visible or hidden, as we've been instructed. }
      if (dbParmVisible = constDbParmParmVisibleVisible) then
         Parameter.IsHidden := False
      else
         Parameter.IsHidden := True;

      { Place parm object 0.2 Dxp Units to the right of the component. }
      { This, of course, is (a) a blind guess as to what would look good, and (b) completely arbitrary. }
      Parameter.Location := Point((component.Location.X + DxpsToCoord(0.2)), component.Location.Y);

      { Place the new parm. }
      component.AddSchObject(Parameter);
      SchServer.RobotManager.SendMessage(component.I_ObjectAddress, c_BroadCast, SCHM_PrimitiveRegistration, Parameter.I_ObjectAddress);

      { Add this file to the list of changed files. }
      changedFiles.Add(document.DM_FileName);
      
      { Tell summary file what we did. }
      WriteToSummaryFile('INFO:     ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Added missing parameter "' + dbParmName + '"="' + dbParmValue + '".  Visibility was set to "' + dbParmVisible +'".');

   end;

end; { end AddSchParm() }


{***************************************************************************
 * function SetModelToBeCurrentNotCurrent()
 *  Sets the "current" flag in the specified footprint model to the specified value.
 *
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function SetModelToBeCurrentNotCurrent(    document     : IDocument,
                                           SchModel     : ISch_Implementation;
                                           current      : Boolean;
                                       var changedFiles : TStringList;
                                           )            : Integer;

begin

   { Assume success. }
   result := 0;

   {** Make the change to the sch page! **}
   { Notify the server process that we're about to make a change. }
   SchServer.RobotManager.SendMessage(SchModel.I_ObjectAddress, c_BroadCast,
                                      SCHM_beginModify, c_NoEventData);

   { Make the change. }
   SchModel.IsCurrent := current;

   { Notify the server process that we're done making changes. }
   SchServer.RobotManager.SendMessage(SchModel.I_ObjectAddress, c_BroadCast,
                                      SCHM_endModify, c_NoEventData);

   { Add this file to the list of changed files. }
   changedFiles.Add(document.DM_FileName);

   { Clean up the robots in Schematic editor }
   SchServer.ProcessControl.PostProcess(document, '');
   
   { Refresh schematic sheet }
   //                 document.GraphicallyInvalidate;
   
end; { end SetModelToBeCurrentNotCurrent() }


{***************************************************************************
 * function SetModelToBeCurrent()
 *  Sets a specified footprint model to be the "current" one.
 *
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function SetModelToBeCurrent(    document     : IDocument,
                                 component    : ISch_Component;
                                 SchModel     : ISch_Implementation;
                                 msg          : TDynamicString;
                             var changedFiles : TStringList;
                                 )            : Integer;

begin

   { If this footprint is already current, then there's nothing we need to do. }
   if (SchModel.IsCurrent) then
   begin
      WriteToDebugFile('This footprint was already current.  Nothing to do.');
   end

   { Else we need to make this model the current one. }
   else
   begin

      { Call SetModelToBeCurrentNotCurrent() to do the real work. }
      result := SetModelToBeCurrentNotCurrent(document,
                                              SchModel,
                                              True, {current,}
                                              {var} changedFiles);


      { Tell summary file what we did. }
      WriteToDebugFile(msg);
      WriteToSummaryFile(msg);

   end { endelse }
   
end; { end SetModelToBeCurrent() }


{***************************************************************************
 * function SetModelToBeNotCurrent()
 *  Sets a specified footprint model to be NOT the "current" one.
 *
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function SetModelToBeNotCurrent(    document     : IDocument,
                                    component    : ISch_Component;
                                    SchModel     : ISch_Implementation;
                                var changedFiles : TStringList;
                                    )            : Integer;

begin

   { If this footprint is already not current, then there's nothing we need to do. }
   if (not SchModel.IsCurrent) then
   begin
      WriteToDebugFile('This footprint was already not current.  Nothing to do.');
   end

   { Else we need to make this model not the current one. }
   else
   begin

      { Call SetModelToBeCurrentNotCurrent() to do the real work. }
      result := SetModelToBeCurrentNotCurrent(document,
                                              SchModel,
                                              False, {current,}
                                              {var} changedFiles);
      
      { Tell summary file what we did. }
      WriteToDebugFile('INFO:     ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Set footprint "' + SchModel.ModelName + '" to be deselected.');
      WriteToSummaryFile('INFO:     ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Set footprint "' + SchModel.ModelName + '" to be deselected.');
      
   end; { endelse }
 
end; { end SetModelToBeNotCurrent() }


{***************************************************************************
 * function UpdateSchParmValueByRef()
 *  Perform a schematic component parameter update given a reference to the parameter,
 *  plus it's new value.
 *
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function UpdateSchParmValueByRef(    document     : IDocument,
                                     component    : ISch_Component;
                                     Parameter    : ISch_Parameter;
                                     dbParmName   : TDynamicString;
                                     dbParmValue  : TDynamicString;
                                 var changedFiles : TStringList;
                                     )            : Integer;

begin

   { Assume success. }
   result := 0;

   { Notify the server process that we're about to make a change. }
   SchServer.RobotManager.SendMessage(Parameter.I_ObjectAddress, c_BroadCast,
                                      SCHM_beginModify, c_NoEventData);

   { Make the change. }
   Parameter.Text := dbParmValue;

   { Notify the server process that we're done making changes. }
   SchServer.RobotManager.SendMessage(Parameter.I_ObjectAddress, c_BroadCast,
                                      SCHM_endModify, c_NoEventData);

   { Clean up the robots in Schematic editor }
   SchServer.ProcessControl.PostProcess(document, '');
   
//   { Refresh schematic sheet }
//   document.GraphicallyInvalidate;
                  
   { Add this file to the list of changed files. }
   changedFiles.Add(document.DM_FileName);
   
   { Tell summary file what we did. }
   WriteToSummaryFile('INFO:     ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Set parameter "' + dbParmName + '"="' + dbParmValue + '".');

end; { end UpdateSchParmValueByRef() }


{***************************************************************************
 * function StripOutNumbers()
 *  Strip all numeric characters (0 to 9) from a string.
 *  
 *  Returns stripped string in var parm stripped.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function StripOutNumbers(    stripMe  : TDynamicString;
                         var stripped : TDynamicString;
                             )        : Integer;
begin

   { Assume success. }
   result := 0;

   { Strip off all numbers from the given string. }
   stripped := StringReplace(stripMe, '0', '', MkSet(rfReplaceAll));
   stripped := StringReplace(stripped, '1', '', MkSet(rfReplaceAll));
   stripped := StringReplace(stripped, '2', '', MkSet(rfReplaceAll));
   stripped := StringReplace(stripped, '3', '', MkSet(rfReplaceAll));
   stripped := StringReplace(stripped, '4', '', MkSet(rfReplaceAll));
   stripped := StringReplace(stripped, '5', '', MkSet(rfReplaceAll));
   stripped := StringReplace(stripped, '6', '', MkSet(rfReplaceAll));
   stripped := StringReplace(stripped, '7', '', MkSet(rfReplaceAll));
   stripped := StringReplace(stripped, '8', '', MkSet(rfReplaceAll));
   stripped := StringReplace(stripped, '9', '', MkSet(rfReplaceAll));

end; { end StripOutNumbers() }


{***************************************************************************
 * function UpdateSchDatabaseLink()
 *  Update the database library name and database table name for a component.
 *
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function UpdateSchDatabaseLink(    document     : IDocument,
                                   component    : ISch_Component;
                                   dbLibName    : TDynamicString;
                                   dbTableName  : TDynamicString;
                               var changedFiles : TStringList;
                                   )            : Integer;

var
   rc : Integer;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Attempting to update component database link fields.');
   //     ShowMessage('Attempting to update component database link fields.');

   {** Make the change to the sch page! **}
   { Notify the server process that we're about to make a change. }
   SchServer.RobotManager.SendMessage(component.I_ObjectAddress, c_BroadCast,
                                      SCHM_beginModify, c_NoEventData);

   { Make the change. }
   component.SourceLibraryName   := dbLibName;
   component.DatabaseTableName   := dbTableName;

   { Notify the server process that we're done making changes. }
   SchServer.RobotManager.SendMessage(component.I_ObjectAddress, c_BroadCast,
                                      SCHM_endModify, c_NoEventData);

   { Clean up the robots in Schematic editor }
   SchServer.ProcessControl.PostProcess(document, '');
   
   { Refresh schematic sheet }
   //     document.GraphicallyInvalidate;
   
   { Add this file to the list of changed files. }
   changedFiles.Add(document.DM_FileName);
   
   { Tell summary file what we did. }
   WriteToSummaryFile('INFO:     ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Set SourceLibraryName   ="' + dbLibName + '".');
   WriteToSummaryFile('INFO:     ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Set DatabaseTableName   ="' + dbTableName + '".');
   
end; { end UpdateSchDatabaseLink() }


{***************************************************************************
 * function UpdateSchDescription()
 *  Update the Description field for this schematic component.
 *
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function UpdateSchDescription(    document     : IDocument,
                                  component    : ISch_Component;
                                  dbParmValue  : TDynamicString;
                              var changedFiles : TStringList;
                                  )            : Integer;
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Attempting to update component Description field.');
   //    ShowMessage('Attempting to update component Description field.');

   {** Make the change to the sch page! **}
   { Notify the server process that we're about to make a change. }
   SchServer.RobotManager.SendMessage(component.I_ObjectAddress, c_BroadCast,
                                      SCHM_beginModify, c_NoEventData);

   { Make the change. }
   component.ComponentDescription := dbParmValue;

   { Notify the server process that we're done making changes. }
   SchServer.RobotManager.SendMessage(component.I_ObjectAddress, c_BroadCast,
                                      SCHM_endModify, c_NoEventData);

   { Clean up the robots in Schematic editor }
   SchServer.ProcessControl.PostProcess(document, '');
   
   { Refresh schematic sheet }
//    document.GraphicallyInvalidate;
   
   { Add this file to the list of changed files. }
   changedFiles.Add(document.DM_FileName);
   
   { Tell summary file what we did. }
   WriteToSummaryFile('INFO:     ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Set parameter "Description="' + dbParmValue + '".');
      
end; { end UpdateSchDescription() }


{***************************************************************************
 * function StripVersionNumbersFromFootprintName()
 *  Strip off versioning numbers from a given footprint name.
 *
 *  NOTE:  This whole function is highly XIA-specific!
 *  
 *  Returns fpNameStripped as var parm.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function StripVersionNumbersFromFootprintName(    fpName         : TDynamicString;
                                              var fpNameStripped : TDynamicString;
                                                  )              : Integer;

var
   rc               : Integer;
   fpPrefix         : TDynamicString;
   fpSuffix         : TDynamicString;
   fpSuffixStripped : TDynamicString;
                    
begin

   { Assume success. }
   result := 0;

   { Init all return values to null strings. }
   fpPrefix         := '';
   fpSuffix         := '';
   fpSuffixStripped := '';
   
   { Split apart the model (footprint) name into a name and a suffix.
    eg.  'CAP_SM_0402/I2' becomes 'CAP_SM_0402' and 'I2'.
    These will be used for "fuzzy" matches, where the first part of the
    footprint name matches, but the suffix differs. }
   { Note:  Handle the case where the degenerate former footprint name does not contain '/' char. }
   rc := SplitStringIntoLeftAndRight(fpName,
                                     constStringFpNameVersionDelim,
                                     fpPrefix,
                                     fpSuffix);

   { Strip off all numbers from the suffix string. }
   if (rc = 0) then
      StripOutNumbers(fpSuffix,
                      fpSuffixStripped);

   { Construct stripped name and return it to caller. }
   fpNameStripped := fpPrefix + constStringFpNameVersionDelim + fpSuffixStripped;
//   WriteToDebugFile('*In StripVersionNumbersFromFootprintName(), fpName is "' + fpName + '", fpNameStripped is "' + fpNameStripped + '".');

   { Return rc as this function's return code. }
   result := rc;
   
end; { end StripVersionNumbersFromFootprintName() }


{***************************************************************************
 * function UpdateSchCurrentModel()
 *  Update the which model is the "current" one for this schematic component.
 *
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function UpdateSchCurrentModel(    document     : IDocument,
                                   component    : ISch_Component;
                                   dbParmValue  : TDynamicString;
                               var changedFiles : TStringList;
                                   )            : Integer;

var
   ImplIterator   : ISch_Iterator;
   found          : Boolean;
   compFpStripped : TDynamicString;
   SchModel       : ISch_Implementation;
   footprintIndex : Integer;
   dbFpStripped   : TDynamicString;
   rc             : Integer;
   msg            : TDynamicString;
   
begin

   { Assume success. }
   result := 0;

   { Create iterator to iterate over sch implementations (models). }
   ImplIterator := component.SchIterator_Create;
   ImplIterator.AddFilter_ObjectSet(MkSet(eImplementation));

   try // begin { Emacs pascal mode doesn't understand Try construct.  Add fake "begin" keyword. }

      { Flag that we have not yet found what we're looking for. }
      WriteToDebugFile('Looking for a footprint named "' + dbParmValue + '", which we will make selected.');
      found := False;
      
      { Strip all versioning numbers out of the component model (footprint) name. }
      { This is a bit confusing.  The footprint name passed in as parm dbParmValue is
       actually the name of the previously current footprint attached to this component,
       from before we updated footprints with respect to database. }
      rc := StripVersionNumbersFromFootprintName(dbParmValue,
                                                 {var} compFpStripped);
      
      { Get a reference to the first model. }
      SchModel := ImplIterator.FirstSchObject;

      { Loop over all models. }
      footprintIndex := 0;
      while ( (SchModel <> Nil) and (rc = 0) ) do
      begin

         { FIXME:  Currently we ignore all models that are of type other than PCBLIB. }

         { Make sure this model is a footprint. }
         if (AnsiUpperCase(SchModel.ModelType) = constKindPcbLib) then
         begin
            
            { See if we have not yet found the model we are supposed to make "current". }
            if (not found) then
            begin

               { Strip all versioning numbers out of the database model (footprint) name. }
               StripVersionNumbersFromFootprintName(SchModel.ModelName,
                                                    {var} dbFpStripped);
      
               { See if we need to make this model the "current" one, in order to maintain the previous state. }
               { This can either be an exact match or a "fuzzy" match. }
               if ( (SchModel.ModelName = dbParmValue) or 
                   (compFpStripped = dbFpStripped) ) then
               begin

                  { Report whether we had an exact match or a "fuzzy" match. }
                  if (SchModel.ModelName = dbParmValue) then
                  begin
                     WriteToDebugFile('Found footprint "' + SchModel.ModelName + '" that we were ordered to make selected!');
                     msg := ('INFO:     ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Set footprint "' + SchModel.ModelName + '" to be the selected one, via exact match for previous selected footprint.');
                  end

                  else
                  begin
                     WriteToDebugFile('Found footprint "' + SchModel.ModelName + '" that was a fuzzy match for previous selected footprint "' + dbParmValue + '".');
                     msg := ('INFO:     ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Set footprint "' + SchModel.ModelName + '" to be the selected one, via fuzzy match for previous selected footprint "' + dbParmValue + '".');
                  end;
                  
                  { Flag that we found a match. }
                  found := True;

                  { Call SetModelToBeCurrent() to do all the real work. }
                  SetModelToBeCurrent(document,
                                      component,
                                      SchModel,
                                      msg,
                                      {var} changedFiles);

               end { endif need to make this model the current one }

               { Else this model should NOT be the current one.  Make sure that this one is NOT current. }
               else
               begin

                  { Call SetModelToBeNotCurrent() to do all the real work. }
                  SetModelToBeNotCurrent(document,
                                         component,
                                         SchModel,
                                         {var} changedFiles);

               end; { endelse }
               
            end { endif not found }

            { Else we've already found the model that we made current.  Make sure that this one is NOT current. }
            else
            begin

               { Call SetModelToBeNotCurrent() to do all the real work. }
               SetModelToBeNotCurrent(document,
                                      component,
                                      SchModel,
                                      {var} changedFiles);

            end; { endelse }

            { Increment footprint index. }
            footprintIndex := footprintIndex + 1;
            
         end { endif is footprint }

         { Else note that we will ignore this model completely. }
         else
         begin
            
            WriteToDebugFile('Ignoring model named "' + SchModel.ModelName + '" of type "' + SchModel.ModelType + '".');
            
         end; { endelse }
         
         { Advance to next model within this schematic component. }
         SchModel := ImplIterator.NextSchObject;
         
      end; { endwhile SchModel <> Nil }

      { See if we failed to find a footprint to mark as the current one. }
      if (not found) then
      begin

         { Get a reference to the first model. }
         SchModel := ImplIterator.FirstSchObject;

         { Loop over all models. }
         footprintIndex := 0;
         while (SchModel <> Nil) do
         begin
            
            { Make sure this model is a footprint. }
            if (AnsiUpperCase(SchModel.ModelType) = constKindPcbLib) then
            begin

               { Set the 0th footprint to be the current one. }
               if (footprintIndex = 0) then
               begin
                  
                  WriteToDebugFile('Could not find a footprint named "' + dbParmValue + '"!  Setting the (new?) primary footprint to be selected.');

                  msg := ('WARNING:  ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Set primary footprint "' + SchModel.ModelName + '" to be the selected one, since I couldn''t find a match for previous selected footprint "' + dbParmValue + '".');

                  { Call SetModelToBeCurrent() to do all the real work. }
                  SetModelToBeCurrent(document,
                                      component,
                                      SchModel,
                                      msg,
                                      {var} changedFiles);

               end { endif footprintIndex = 0 }

               { Else set this footprint to be not current. }
               else
               begin

                  { We've already found the model to be made current.  Make sure that this one is NOT current. }
                  SetModelToBeNotCurrent(document,
                                         component,
                                         SchModel,
                                         {var} changedFiles);

               end; { endelse }

               { Increment footprint index. }
               footprintIndex := footprintIndex + 1;
               
            end { endif is model a footprint }

            { Else note that we will ignore this model completely. }
            else
            begin
               
               WriteToDebugFile('Ignoring model named "' + SchModel.ModelName + '" of type "' + SchModel.ModelType + '".');
               
            end; { endelse }
            
            { Advance to next model within this schematic component. }
            SchModel := ImplIterator.NextSchObject;

         end; { endwhile }

      end; { endif not found }

      { Finally clause... }
      finally

      { Free iterator. }
      component.SchIterator_Destroy(ImplIterator);
   end; { endtry }

end; { end UpdateSchCurrentModel() }


{***************************************************************************
 * function UpdateSchModel()
 *  Update a model for this schematic component.
 *
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function UpdateSchModel(    document          : IDocument,
                            component         : ISch_Component;
                            suppressFpUpdates : Boolean;
                            dbParmName        : TDynamicString;
                            dbParmValue       : TDynamicString;
                        var changedFiles      : TStringList;
                            )                 : Integer;

var
   ImplIterator         : ISch_Iterator;
   SchModel             : ISch_Implementation;
   desiredIndex         : Integer;
   footprintIndex       : Integer;
   rc                   : Integer;
   
begin

   { Assume success. }
   result := 0;

   { Create iterator to iterate over sch implementations (models). }
   ImplIterator := component.SchIterator_Create;
   ImplIterator.AddFilter_ObjectSet(MkSet(eImplementation));

   try // begin { Emacs pascal mode doesn't understand Try construct.  Add fake "begin" keyword. }

      { These system parameter are handled differently.  We are told to look for a parameter named
       "ModelName0".  But that "0" part is a fiction that we invented earlier.  So what we really
       want to do is strip off everything but that index and use that directly to index
       into the list of system parameters. }
      desiredIndex := StrToInt(StringReplace(dbParmName, constDbParmModelName, '', ''));
      WriteToDebugFile('Looking for system parm "' + dbParmName + '", which is model index ' + IntToStr(desiredIndex) + '.');

      { Get a reference to the first model. }
      SchModel := ImplIterator.FirstSchObject;

      { Loop over all models. }
      footprintIndex := 0;
      while (SchModel <> Nil) do
      begin

         { Make sure this model is a footprint. }
         if (AnsiUpperCase(SchModel.ModelType) = constKindPcbLib) then
         begin

            { See if this is the parameter that we were ordered to modify. }
            if (footprintIndex = desiredIndex) then
            begin

               WriteToDebugFile('Found system parm that we were ordered to update!');
               WriteToDebugFile(' Change from "' + SchModel.ModelName + '" to "' + dbParmValue + '".');

               { See if we are allowed to update footprints. }
               if ( (enableFootprintUpdates) and (not suppressFpUpdates) ) then
               begin
                  
                  {** Make the change to the sch page! **}
                  { Notify the server process that we're about to make a change. }
                  SchServer.RobotManager.SendMessage(SchModel.I_ObjectAddress, c_BroadCast,
                                                     SCHM_beginModify, c_NoEventData);

                  { Make the change. }
                  SchModel.ModelName := dbParmValue;

                  { Set this model to be not the current one.  We'll double check this later. }
                  SchModel.IsCurrent := False;

                  { Notify the server process that we're done making changes. }
                  SchServer.RobotManager.SendMessage(SchModel.I_ObjectAddress, c_BroadCast,
                                                     SCHM_endModify, c_NoEventData);

                  { Clean up the robots in Schematic editor }
                  SchServer.ProcessControl.PostProcess(document, '');
                  
                  { Refresh schematic sheet }
                  //               document.GraphicallyInvalidate;
                  
                  { Add this file to the list of changed files. }
                  changedFiles.Add(document.DM_FileName);
                  
                  { Tell summary file what we did. }
                  WriteToSummaryFile('INFO:     ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Set footprint "' + dbParmName + '"="' + dbParmValue + '".');

               end { endif enableFootprintUpdates }

               { Else we are not allowed to update footprints. }
               else
               begin

                  { Tell summary file what we were not allowed to fix. }
                  WriteToSummaryFile('ERROR:    ' + component.Designator.Name + ' ' + component.Designator.Text + ':  I was not allowed to set footprint "' + dbParmName + '"="' + dbParmValue + '".');
                  
               end; { endelse enableFootprintUpdates }

            end; { endif }
            
            { Increment footprint index. }
            footprintIndex := footprintIndex + 1;
            
         end { endif is model a footprint }

         { Else note that we will ignore this model completely. }
         else
         begin
            
            WriteToDebugFile('Ignoring model named "' + SchModel.ModelName + '" of type "' + SchModel.ModelType + '".');
            
         end; { endelse }
         
         { Advance to next model within this schematic component. }
         SchModel := ImplIterator.NextSchObject;
         
      end; { endwhile SchModel <> Nil }
      
      finally

      { Free iterator. }
      component.SchIterator_Destroy(ImplIterator);
   end; { endtry }


end; { end UpdateSchModel() }


{***************************************************************************
 * function UpdateSchUserParm()
 *  Update a specific "user parameter" one for this schematic component.
 *
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function UpdateSchUserParm(    document     : IDocument,
                               component    : ISch_Component;
                               dbParmName   : TDynamicString;
                               dbParmValue  : TDynamicString;
                           var changedFiles : TStringList;
                               )            : Integer;

var
   PIterator            : ISch_Iterator;
   Parameter            : ISch_Parameter;
   rc                   : Integer;
   
begin

   { Assume success. }
   result := 0;

   { Setup to examine user-defined schematic parameters. }
   try // begin { Emacs pascal mode doesn't understand Try construct.  Add fake "begin" keyword. }
      PIterator := component.SchIterator_Create;
      PIterator.AddFilter_ObjectSet(MkSet(eParameter));

      { Get a reference to the first parameter within this schematic component. }
      Parameter := PIterator.FirstSchObject;

      { Loop over all parameters currently attached to this component. }
      while (Parameter <> Nil) do
      begin

         { See if this is the parameter that we were ordered to modify. }
         if (Parameter.Name = dbParmName) then
         begin

            WriteToDebugFile('Found user-defined parm that we were ordered to update!');

            { Call UpdateSchParmValueByRef() to do all the real work. }
            UpdateSchParmValueByRef(document,
                                    component,
                                    Parameter,
                                    dbParmName,
                                    dbParmValue,
                                    {var} changedFiles);
            
         end; { endif }
         
         { Advance to next parameter in this schematic component. }
         Parameter := PIterator.NextSchObject;

      end;

      { We're now done iterating through user-defined schematic parameters. }
      finally
      component.SchIterator_Destroy(PIterator);
   end;

end; { end UpdateSchUserParm() }


{***************************************************************************
 * function UpdateSchParmValueByName()
 *  Perform a schematic component parameter update given the name of the parameter,
 *  plus it's new value.
 *
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function UpdateSchParmValueByName(    document          : IDocument,
                                      component         : ISch_Component;
                                      suppressFpUpdates : Boolean;
                                      dbParmName        : TDynamicString;
                                      dbParmValue       : TDynamicString;
                                  var changedFiles      : TStringList;
                                      )                 : Integer;
begin

   { Assume success. }
   result := 0;

   { We must handle updating the Description field differently. }
   if (dbParmName = constDbParmDescription) then
   begin

      { Call UpdateSchDescription() to do all the real work. }
      UpdateSchDescription(document,
                           component,
                           dbParmValue,
                           {var} changedFiles);

   end { endif }

   { See if it's the fake parameter to mark "current" footprint.  Handle this as a special case. }
   else if (AnsiPos(constDbParmCurrentModelType, dbParmName) <> 0) then
   begin

      { FIXME:  Currently we assume that all models are of type PCBLIB.
       Thus, we ignore CurrentModelType parms completely. }

      { Do nothing }
   end
   
   { See if it's the fake parameter to mark "current" footprint.  Handle this as a special case. }
   else if (AnsiPos(constDbParmCurrentModelName, dbParmName) <> 0) then
   begin

      { Call UpdateSchCurrentModel() to do all the real work. }
      UpdateSchCurrentModel(document,
                            component,
                            dbParmValue,
                            {var} changedFiles);

   end { end elsif }

   { See if it's a system parameter (aka model) that needs to be updated.  Handle these differently. }
   else if (AnsiPos(constDbParmModelType, dbParmName) <> 0) then
   begin
      
      { FIXME:  Currently we assume that all models are of type PCBLIB.
       Thus, we ignore ModelType parms completely. }

   end { end elsif }   

   { See if it's a system parameter (aka model) that needs to be updated.  Handle these differently. }
   else if (AnsiPos(constDbParmModelName, dbParmName) <> 0) then
   begin
      
      { Call UpdateSchModel() to do all the real work. }
      UpdateSchModel(document,
                     component,
                     suppressFpUpdates,
                     dbParmName,
                     dbParmValue,
                     {var} changedFiles);
      
   end { end elsif }

   { Else it's a user-defined schematic parameter. }
   else
   begin

      { Call UpdateSchUserParm() to do all the real work. }
      UpdateSchUserParm(document,
                        component,
                        dbParmName,
                        dbParmValue,
                        {var} changedFiles);
      
   end; { endelse }

end; { end UpdateSchParmValueByName() }


{***************************************************************************
 * function GetSysParametersFromComp()
 *  Get all system parameters (aka models aka footprints) for a given schematic component.
 *
 *  Returns component's user parameters as var parm compSysParmsList.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function GetSysParametersFromComp(    document         : IDocument,
                                      component        : ISch_Component;
                                  var compSysParmsList : TStringList;
                                      )                : Integer;

var
   footprintIndex : Integer;
   currentIndex   : Integer;
   j              : Integer;
   ImplIterator   : ISch_Iterator;
   SchModel       : ISch_Implementation;
   ModelDataFile  : ISch_ModelDatafileLink;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Designator:             ' + component.Designator.Text);
   WriteToDebugFile(' DesignItemID:          ' + component.DesignItemID);
   WriteToDebugFile(' Library Reference:     ' + component.LibReference);
   WriteToDebugFile(' Library Path:          ' + component.LibraryPath);
   WriteToDebugFile(' Source Library Name:   ' + component.SourceLibraryName);
   WriteToDebugFile(' Database Library Name: ' + component.DatabaseLibraryName);
   WriteToDebugFile(' Database Table Name:   ' + component.DatabaseTableName);

   { Create iterator to iterate over sch implementations (models). }
   ImplIterator := component.SchIterator_Create;
   ImplIterator.AddFilter_ObjectSet(MkSet(eImplementation));

   try // begin { Emacs pascal mode doesn't understand Try construct.  Add fake "begin" keyword. }

      { Get a reference to the first model. }
      SchModel := ImplIterator.FirstSchObject;

      { Loop over all models. }
      footprintIndex := 0;
      currentIndex := 0;
      while (SchModel <> Nil) do
      begin
         WriteToDebugFile(' Implementation Model details:');
         WriteToDebugFile('   ModelName:   ' + SchModel.ModelName + constLineBreak + 
                          '   ModelType:   ' + SchModel.ModelType + constLineBreak + 
                          '   Description: ' + SchModel.Description + constLineBreak + 
                          '   IsCurrent:   ' + BoolToStr(SchModel.IsCurrent));
         WriteToDebugFile('   Map:         ' + SchModel.MapAsString);

         { Loop over all data file links. }
         for j := 0 to SchModel.DatafileLinkCount - 1 do
         begin
            ModelDataFile := SchModel.DatafileLink[j];
            if ModelDataFile <> Nil then
            begin
               WriteToDebugFile(' Implemenation Data File Link Details:');
               WriteToDebugFile('   Data File Location: ' + ModelDataFile.Location +
                                ', Entity Name: '         + ModelDataFile.EntityName +
                                ', FileKind: '            + ModelDataFile.FileKind);
            end; { endif ModelDataFile <> Nil }
         end; { endfor loop over DatafileLinkCount }

         { Make sure this model is a footprint. }
         if (AnsiUpperCase(SchModel.ModelType) = constKindPcbLib) then
         begin
            
            { Store this model in list, for later use in checking against database parameters. }
            compSysParmsList.Add(constDbParmModelType + IntToStr(footprintIndex) + constStringEquals + SchModel.ModelType);
            WriteToDebugFile    (constDbParmModelType + IntToStr(footprintIndex) + constStringEquals + SchModel.ModelType);
            
            compSysParmsList.Add(constDbParmModelName + IntToStr(footprintIndex) + constStringEquals + SchModel.ModelName);
            WriteToDebugFile    (constDbParmModelName + IntToStr(footprintIndex) + constStringEquals + SchModel.ModelName);

            { If this model is the "current" one, then store it again. }
            if (SchModel.IsCurrent) then
            begin
               compSysParmsList.Add(constDbParmCurrentModelType + IntToStr(currentIndex) + constStringEquals + SchModel.ModelType);
               WriteToDebugFile    (constDbParmCurrentModelType + IntToStr(currentIndex) + constStringEquals + SchModel.ModelType);
               
               compSysParmsList.Add(constDbParmCurrentModelName + IntToStr(currentIndex) + constStringEquals + SchModel.ModelName);
               WriteToDebugFile    (constDbParmCurrentModelName + IntToStr(currentIndex) + constStringEquals + SchModel.ModelName);

               { Increment index into "current" models. }
               currentIndex := currentIndex + 1;
               
            end; { endif }

            { Increment footprint index. }
            footprintIndex := footprintIndex + 1;
               
         end; { endif is model a footprint }
                 
         { Advance to next model within this schematic component. }
         WriteToDebugFile('');
         SchModel := ImplIterator.NextSchObject;
         
      end; { endwhile SchModel <> Nil }
      
      finally

      { Free iterator. }
      component.SchIterator_Destroy(ImplIterator);
   end; { endtry }

end; { end GetSysParametersFromComp() }


{***************************************************************************
 * function CheckAndCorrectDatabaseLibraryName()
 *  Verify that the DatabaseLibraryName for a given component is correct.
 *  If it's outdated, then correct it, along with the dbTableName.
 *
 *  NOTE:  The adding '$' char to database table name is highly XIA-specific!
 *  
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CheckAndCorrectDatabaseLibraryName(    document     : IDocument,
                                                component    : ISch_Component;
                                            var changedFiles : TStringList;
                                                )            : Integer;
var
   dbLibName   : TDynamicString;
   dbTableName : TDynamicString;
   
begin

   { Assume success. }
   result := 0;

   { See if we need to update from Previous1_database_library_file.DBLib to Customary_database_library_file.DBLib. }
   if ( (constOldDbLib1FileName <> '') and (component.DatabaseLibraryName = constOldDbLib1FileName) ) then
   begin

      { Set new database library name to be 'Customary_database_library_file.DBLib'. }
      dbLibName := constRequiredDbLibFileName;

      { Append a '$' char to the end of the existing table name. }
      { NOTE:  This operation is highly XIA-specific! }
      dbTableName := component.DatabaseTableName + '$';
      
      { Call UpdateSchDatabaseLink() to update the database library and database table fields. }
      UpdateSchDatabaseLink(document,
                            component,
                            dbLibName,
                            dbTableName,
                            {var} changedFiles);

   end; { endif }
   
   { See if we need to update from Previous2_database_library_file.DBLib to Customary_database_library_file.DBLib. }
   if ( (constOldDbLib2FileName <> '') and (component.DatabaseLibraryName = constOldDbLib2FileName) ) then
   begin

      { Set new database library name to be 'Customary_database_library_file.DBLib'. }
      dbLibName := constRequiredDbLibFileName;

      { Retain the existing table name. }
      dbTableName := component.DatabaseTableName;

      { Call UpdateSchDatabaseLink() to update the database library and database table fields. }
      UpdateSchDatabaseLink(document,
                            component,
                            dbLibName,
                            dbTableName,
                            {var} changedFiles);

   end; { endif }
   
end; { end CheckAndCorrectDatabaseLibraryName() }
   

{***************************************************************************
 * function GetUserParametersFromComp()
 *  Get all user parameters for a given schematic component.
 *  In the process, attempt to identify the database key.
 *
 *  In addition, store certain information about logical components for later
 *  use when dealing with component variants.
 *
 *  NOTE:  Assumes that compUserParmsList string list has already been created.
 *  NOTE:  Assumes that logCompCacheByRefDes and logCompCacheByUniqueId string lists have already been created.
 *  
 *  Returns flag whether comp is only linked via DBLink file as var parm linkedOnlyViaDbLink.
 *  Returns database key for this comp as var parm compDbKey.
 *  Returns database table for this comp as var parm compDbTable.
 *  Returns component's user parameters as var parm compUserParmsList.
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function GetUserParametersFromComp(    document               : IDocument,
                                       component              : ISch_Component;
                                       locDbKeyName           : WideString;
                                   var locDbKey               : WideString;
                                   var suppressFpUpdates      : Boolean;
                                   var linkedOnlyViaDbLink    : Boolean;
                                   var compDbKey              : WideString;
                                   var compDbTable            : WideString;
                                   var compUserParmsList      : TStringList;
                                   var logCompCacheByRefDes   : TStringList;
                                   var logCompCacheByUniqueId : TStringList;
                                   var changedFiles           : TStringList;
                                       )                      : Integer;

var
   PIterator    : ISch_Iterator;
   i            : Integer;
   Parameter    : ISch_Parameter;
   TableIndex   : Integer;
   position     : Integer;
   dbLibName    : TDynamicString;
   dbTableName  : TDynamicString;
   foundDbKey   : Boolean;
   
begin

   { Assume success. }
   result := 0;

   { Unless we find out otherwise, we have no reason to suppress footprint updates. }
   suppressFpUpdates := False;   

   { Attempt to extract database related info. }
   WriteToDebugFile('DesignItemID       ="' + component.DesignItemID + '"');
   WriteToDebugFile('LibReference       ="' + component.LibReference + '"');
   WriteToDebugFile('DatabaseLibraryName="' + component.DatabaseLibraryName + '"');
   WriteToDebugFile('DatabaseTableName  ="' + component.DatabaseTableName + '"');
   WriteToDebugFile('UniqueId           ="' + component.UniqueId + '"');
   WriteToDebugFile('locDbKeyName       ="' + locDbKeyName + '"');

   { Return database table name to caller. }
   compDbTable := component.DatabaseTableName;

   { Clear the legacy/local database key until we find it for this component. }
   locDbKey := '';

   { Clear the database key until we find it for this component. }
   compDbKey := '';
   foundDbKey := False;

   { Report parameters currently attached to this component. }
   WriteToDebugFile(component.Designator.Name + ' ' + component.Designator.Text);
   WriteToDebugFile(' Parameters');
   
   { Extract the component description, which is handled differently than the rest of the parameters. }
   WriteToDebugFile('  "Description=' + component.ComponentDescription + '"');
   compUserParmsList.Add('Description=' + component.ComponentDescription);

   { Setup to examine user-defined schematic parameters. }
   try // begin { Emacs pascal mode doesn't understand Try construct.  Add fake "begin" keyword. }

      { Create iterator and setup filter. }
      PIterator := component.SchIterator_Create;
      PIterator.AddFilter_ObjectSet(MkSet(eParameter));

      { Get a reference to the first parameter. }
      Parameter := PIterator.FirstSchObject;

      { Loop over all parameters currently attached to this component. }
      while (Parameter <> Nil) do
      begin
         WriteToDebugFile('  "' + Parameter.Name + constStringEquals + Parameter.Text + '"');

         { See if we have a user parameter named "Description". }
         if (AnsiUpperCase(Parameter.Name) = AnsiUpperCase(constDbParmDescription)) then
         begin
            WriteToDebugFile('ERROR:    ' + 'Designator ' + component.Designator.Text + ':  Found user parameter named "' + Parameter.Name + '", which will get confused with system parameter "' + constDbParmDescription + '".  Please delete or rename this user parameter!');
            WriteToSummaryFile('ERROR:    ' + 'Designator ' + component.Designator.Text + ':  Found user parameter named "' + Parameter.Name + '", which will get confused with system parameter "' + constDbParmDescription + '".  Please delete or rename this user parameter!');
         end

         { Else we have a user parameter that we actually want to store... }
         else
         begin

            { See if we found the legacy/local database key. }
            { NOTE:  This may be the same parameter as the one below, so this MUST be an independent "if" statement! }
            if ( (locDbKeyName <> '') and (Parameter.Name = locDbKeyName) ) then
            begin

               { Store the legacy/local database key for this component. }
               locDbKey := Parameter.Text;
               WriteToDebugFile('Found locDbKey parameter.  Using: "' + locDbKey + '" as local database key.');

            end; { endif }
            
            { See if we found the parameter called "OLD_DB_KEY".  This is used as our interim database key. }
            if ( (Parameter.Name = constDbParmDbKeyInterim) and
                (not foundDbKey) and
                (constDbParmDbKeyInterim <> '') ) then
            begin

               { We found the "OLD_DB_KEY" parameter.  Save this value for use as our interim database key, until we find "DB_KEY". }
               compDbKey := Parameter.Text;
               WriteToDebugFile('Found "' + constDbParmDbKeyInterim + '" parameter.  Using: "' + compDbKey + '" as interim database key.');
            end

            { See if we found the parameter called "DB_KEY".  This is used as our real database key. }
            else if (Parameter.Name = constDbParmDbKey) then
            begin

               { We found the "DB_KEY" parameter.  Save this value for use as our database key. }
               compDbKey := Parameter.Text;
               foundDbKey := True;
               WriteToDebugFile('Found "' + constDbParmDbKey + '" parameter.  Using: "' + compDbKey + '" as database key.');
            end

            { Else see if we found the parameter called 'SUPPRESS_FP_UPDATES'.  This being set to 1 will cause us to suppress footprint updates for this component only. }
            else if (Parameter.Name = constDbParmNameSuppressFpUpdates) then
            begin

               { See if parameter is set to appropriate value. }
               suppressFpUpdates := (Parameter.Text = constDbParmValueSuppressFpUpdates);
               WriteToDebugFile('Found "' + constDbParmNameSuppressFpUpdates + '" parameter, value is "' + Parameter.Text + '".  Set suppressFpUpdates to ' + BoolToStr(suppressFpUpdates) + '.');
            end;

            { Make sure we're not seeing this parameter twice, as can happen in degenerate cases with bad sch symbols. }
            position := compUserParmsList.IndexOfName(Parameter.Name);
            if (position >= 0) then
            begin

               { Delete the previous instance of this parameter. }
               WriteToDebugFile('Deleting previous instance of parameter name "' + Parameter.Name + '".');
               compUserParmsList.Delete(position);
               
               //                MyAbort('Found problem with sch symbol for ' + component.Designator.Text + '.  There are multiple parameters named "' + Parameter.Name + '"!' + constLineBreak +
                                         //                          'Please try to update this sch symbol and/or speak to Jeff to address this problem.');
            end;
            
            { Store this parameter in list, for later use in checking against database parameters. }
            compUserParmsList.Add(Parameter.Name + constStringEquals + Parameter.Text);

         end; { endelse }
         
         { Advance to next parameter in this schematic component. }
         Parameter := PIterator.NextSchObject;

      end; { endwhile }

      { We're now done iterating through user-defined schematic parameters. }
      finally
      component.SchIterator_Destroy(PIterator);
   end;

   { Handle a special case where there are extant designs which use a secondary sch symbol
    (eg. design item ID = "R000032009A.2", but no "DB_KEY" parameter attached, but an
    "OLD_DB_KEY" property = "R000032009A".  So here we want to detect that the "OLD_DB_KEY"
    value is a subset of the design item ID, and then take the design item ID, not
    the value of "OLD_DB_KEY" as the new value for "DB_KEY". }
   { NOTE:  This is highly XIA-specific! }
   if ( (component.DesignItemID <> compDbKey) and (AnsiPos(compDbKey, component.DesignItemID) <> 0) and (not foundDbKey) ) then
   begin
      WriteToDebugFile('Detected subset.  Will use DesignItemID as compDbKey.  DesignItemID is "' + component.DesignItemID + '", compDbKey is "' + compDbKey + '".');

      { In this special case, use the DesignItemID as the database key. }
      compDbKey := component.DesignItemID;
      
   end;
   
   { Determine if this component is "really" linked to the database using the DBLib file, or
    if it has only the tenuous connection via the approved DBLink file.
    If a real link using DBLib, then the DesignItemID will equal the "DB_KEY" database key.
    On the other hand, with DBLink, the DesignItemID will be the name of the schematic symbol in use.
    Flag this in case we have to handle symbol and/or footprint checking differently. }
   if (component.DesignItemID = compDbKey) then
   begin

      { Flag that we're really linked using DBLib file, the normal case. }
      linkedOnlyViaDbLink := False;
   end

   { Else we have only a DBLink connection.  We may have to handle checking symbols differently. }
   else
   begin
      linkedOnlyViaDbLink := True;
      WriteToDebugFile('DesignItemID is "' + component.DesignItemID + '", compDbKey is "' + compDbKey + '", looks like only a DBLink connection.');

      { Postpone warning messages about DBLink status until we know for sure
       if this part is linked to the database at all. }

   end; { endif }

   {* Cache information about this logical component for later use in dealing with varied components. *}
   { Format:  LogicalDesignator=ComponentDatabaseKey|ComponentDatabaseTable|UniqueId }
   logCompCacheByRefDes.Add(component.Designator.Text + constStringEquals + compDbKey + constStringDelimiter + component.DatabaseTableName + constStringDelimiter + component.UniqueId);
   
   { Format:  UniqueId=ComponentDatabaseKey|ComponentDatabaseTable|LogicalDesignator }
   logCompCacheByUniqueId.Add(component.UniqueId + constStringEquals + compDbKey + constStringDelimiter + component.DatabaseTableName + constStringDelimiter + component.Designator.Text);

end; { end GetUserParametersFromComp() }


{***************************************************************************
 * function CheckUserParametersForComp()
 *  Check all user parameters for a given schematic component.
 *  Currently this involves making sure that (with a few specific exceptions),
 *  that the "Comment" parameter is set to "=VALUE".
 *
 *  NOTE:  Assumes that compUserParmsList string list has already been created.
 *
 *  NOTE:  This whole function is highly XIA-specific!
 *  
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CheckUserParametersForComp(    document          : IDocument,
                                        component         : ISch_Component;
                                        compUserParmsList : TStringList;
                                    var changedFiles      : TStringList;
                                       )                  : Integer;
var
   position     : Integer;
   valueParm    : TDynamicString;
   categoryParm : TDynamicString;
   commentParm  : TDynamicString;

begin

   { Store value of "Comment" parameter. }
   { Here we're going to cheat and use a faster way to get the Comment parameter value. }
   commentParm := component.Comment.Text;
   
   { See if the Comment parameter is OK by virtue of already being set to "=VALUE". }
   if (commentParm = constDbValCommentStd) then
      WriteToDebugFile('INFO:     ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Comment="' + commentParm + '".  Ok.') { no semi }

   { Else we need to retrieve more information and examine things further... }
   else
   begin

      { Clear the VALUE and CATEGORY parameter values before getting started. }
      valueParm := '';
      categoryParm := '';
      
      {* Try to find "VALUE" parameter. *}
      position := compUserParmsList.IndexOfName(constDbParmValue);

      { If we found it, then store its value. }
      if (position >= 0) then
         valueParm := compUserParmsList.ValueFromIndex(position);


      {* Try to find "CATEGORY" parameter. *}
      position := compUserParmsList.IndexOfName(constDbParmCategory);

      { If we found it, then store its value. }
      if (position >= 0) then
         categoryParm := compUserParmsList.ValueFromIndex(position);


      {* Proceed to check the value of the "Comment" parameter. *}
      { In some degenerate cases (ie. title blocks) there won't be a VALUE parameter at all. }
      if (valueParm = '') then
         WriteToDebugFile('INFO:     ' + component.Designator.Name + ' ' + component.Designator.Text + ':  No VALUE parameter set.  Ignoring Comment parameter.') { no semi }

      { Else see if the Comment parameter is OK by virtue of "VALUE" field being set to 'ALLOW_COMMENT_TO_DIFFER_FROM_THIS_VALUE'. }
      else if (valueParm = constDbValValueFreeform) then
         WriteToDebugFile('INFO:     ' + component.Designator.Name + ' ' + component.Designator.Text + ':  VALUE="' + valueParm + '", Comment="' + commentParm + '".  Ok.') { no semi }
         
      { Else see if the Comment parameter is OK by virtue of "CATEGORY" field being set to 'ALLOW_COMMENT_TO_DIFFER_FROM_VALUE_FIELD'. }
      else if (categoryParm = constDbValCategoryFreeform) then
         WriteToDebugFile('INFO:     ' + component.Designator.Name + ' ' + component.Designator.Text + ':  CATEGORY="' + categoryParm + '", Comment="' + commentParm + '".  Ok.') { no semi }

      { Else we need to update the Comment parameter to be "=VALUE". }
      else
      begin      
         WriteToDebugFile('Found "' + constDbParmComment + '" that needs to be set to "' + constDbValCommentStd + '"!');
         
         { Call UpdateSchParmValueByRef() to do all the real work of updating the Comment parameter. }
         UpdateSchParmValueByName(document,
                                  component,
                                  False {suppressFpUpdates},
                                  constDbParmComment,
                                  constDbValCommentStd,
                                  {var} changedFiles);

      end { endelse }

   end; { endelse }

end; { end CheckUserParametersForComp() }


{***************************************************************************
 * function CheckDbVsCompParmMatch()
 *  Check that a given database parameter matches the corresponding component parameter.
 *
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CheckDbVsCompParmMatch(    document          : IDocument,
                                    component         : ISch_Component;
                                    suppressFpUpdates : Boolean;
                                    compSysParmsList  : TStringList;
                                    compUserParmsList : TStringList;
                                    dbParmName        : TDynamicString;
                                    dbParmValue       : TDynamicString;
                                    dbParmVisible     : TDynamicString;
                                var changedFiles      : TStringList;
                                    )                 : Integer;

var
   position      : Integer;
   compParmValue : TDynamicString;
   
begin

   { Assume success. }
   result := 0;

   { See if this is our placeholder fake parameter to mark current footprint. }
   if ( (AnsiPos(constDbParmCurrentModelType, dbParmName) <> 0) or (AnsiPos(constDbParmCurrentModelName, dbParmName) <> 0) ) then
   begin

      WriteToDebugFile('Checking current model.  dbParmName is "' + dbParmName + '", dbParmValue is "' + dbParmValue + '".');

      { Attempt to update the component level parameter to match the database parameter. }
      UpdateSchParmValueByName(document,
                               component,
                               suppressFpUpdates,
                               dbParmName,
                               dbParmValue,
                               {var} changedFiles);
      
   end

   { Else, see if this is a system parameter that we need to check. }
   else if ( (AnsiPos(constDbParmModelType, dbParmName) <> 0) or (AnsiPos(constDbParmModelName, dbParmName) <> 0) ) then
   begin

      { Look for this parameter in the component parameter list. }
      { Note:  Find() method assumes assumes that compSysParmsList is sorted from the get-go. }
      { FIXME:  Find method causes a script crash.  Dunno why. }
      position := compSysParmsList.IndexOfName(dbParmName); { Find(dbParmName); }
//    WriteToDebugFile('Looking for this in component system parameter list.  position is ' + IntToStr(position));

      { See if this DB parameter exists in component parameter list. }
      if (position >= 0) then
      begin

         { Extract component level parameter value. }
         compParmValue := compSysParmsList.ValueFromIndex(position);
         
         { See if the value of the DB parameter matches the value of the component parameter. }
         if (compParmValue = dbParmValue) then
         begin
            WriteToDebugFile(' DB parameter "' + dbParmName + '=' + dbParmValue + '" matches component parameter.  Ok.');
         end
         
         { Else we have a mismatch! }
         else
         begin
            WriteToDebugFile(' DB parameter "' + dbParmName + '=' + dbParmValue + '" != component parameter "' + dbParmName + '=' + compParmValue + '"!!.');
            
            { Attempt to update the component level parameter to match the database parameter. }
            UpdateSchParmValueByName(document,
                                     component,
                                     suppressFpUpdates,
                                     dbParmName,
                                     dbParmValue,
                                     {var} changedFiles);
            
         end;
         
      end { endif }

      { Else we have a DB parameter that doesn't yet exist as a component-level parameter. }
      else
      begin
         
         WriteToDebugFile(' DB parameter "' + dbParmName + '=' + dbParmValue + '" != component parameter that doesn''t exist!');

         { Attempt to add the missing component level parameter to match the database parameter. }
         AddSchParm(document,
                    component,
                    suppressFpUpdates,
                    dbParmName,
                    dbParmValue,
                    dbParmVisible,
                    {var} changedFiles);
         
      end; { endelse }

   end

   { Else it's a user level parameter. }
   else
   begin
      
      { Look for this parameter in the component parameter list. }
      { Note:  Find() method assumes assumes that compUserParmsList is sorted from the get-go. }
      { FIXME:  Find method causes a script crash.  Dunno why. }
      position := compUserParmsList.IndexOfName(dbParmName); { Find(dbParmName); }
//    WriteToDebugFile('Looking for this in component user parameter list.  position is ' + IntToStr(position));

      { See if this DB parameter exists in component parameter list. }
      if (position >= 0) then
      begin

         { Extract component level parameter value. }
         compParmValue := compUserParmsList.ValueFromIndex(position);
         
         { See if the value of the DB parameter matches the value of the component parameter. }
         if (compParmValue = dbParmValue) then
         begin
            WriteToDebugFile(' DB parameter "' + dbParmName + '=' + dbParmValue + '" matches component parameter.  Ok.');
         end
         
         { Else we have a mismatch! }
         else
         begin
            WriteToDebugFile(' DB parameter "' + dbParmName + '=' + dbParmValue + '" != component parameter "' + dbParmName + '=' + compParmValue + '"!!.');
            
            { Attempt to update the component level parameter to match the database parameter. }
            { NOTE:  We make no attempt to enforce the visibility of the parameter in question.
             We only care about the parameter value! }
            UpdateSchParmValueByName(document,
                                     component,
                                     suppressFpUpdates,
                                     dbParmName,
                                     dbParmValue,
                                     {var} changedFiles);
            
         end;
         
      end { endif }

      { Else we have a DB parameter that doesn't yet exist as a component-level parameter. }
      else
      begin
         
         WriteToDebugFile(' DB parameter "' + dbParmName + '=' + dbParmValue + '" != component parameter that doesn''t exist!');

         { Attempt to add the missing component level parameter to match the database parameter. }
         { Specify whether the parameter to be added should be visible or not. }
         AddSchParm(document,
                    component,
                    suppressFpUpdates,
                    dbParmName,
                    dbParmValue,
                    dbParmVisible,
                    {var} changedFiles);
         
      end; { endelse }

   end; { endelse }
   
end; { end CheckDbVsCompParmMatch() }


{***************************************************************************
 * function SearchForCompInDatabaseCache()
 *  Look for a component in a database cache.
 *  If found, return starting and ending indices for this component in cache list.
 *
 *  NOTE:  Assumes that dbParmsIndex string list has already been created.
 *  
 *  Returns whether this component was found in the specified db cache as var parm foundInDbCache.
 *  Returns limits of dbParmsCache as var parms dbParmsStart and dbParmsEnd.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function SearchForCompInDatabaseCache(    dbKey          : WideString;
                                          dbParmsIndex   : TStringList;
                                      var foundInDbCache : Boolean;
                                      var dbParmsStart   : Integer;
                                      var dbParmsEnd     : Integer;
                                          )              : Integer;

var
   i                : Integer;
   cacheIndex       : Integer;
   leftStr          : TDynamicString;
   rightStr         : TDynamicString;
   dbParmsStartStr  : TDynamicString;
   dbParmsEndStr    : TDynamicString;
   dbParmValuesStr  : TDynamicString;
   dbParmValuesList : TStringList;
   dbParmValue      : TDynamicString;

begin

   { Assume success. }
   result := 0;

   { Until we find out otherwise, we have not found this component in database cache. }
   foundInDbCache := False;         


   {* Attempt to retrieve local database parameters for this component. *}
   { See if we have a valid database key. }
   if (dbKey <> '') then
   begin

      { Search database cache string list. }
      WriteToDebugFile('Searching for dbKey "' + dbKey + '" in dbParmsIndex.');

      { Search for dbKey in string list. }
      cacheIndex := dbParmsIndex.IndexOfName(dbKey);

      { See if we got a hit. }
      if (cacheIndex >= 0) then
      begin

         { Extract the right side of this entry in the cache index.
          This contains the start and end indices into the cache itself. }
         { Note:  Entry in cache index will look something like "C000010001=33|75". }
         SplitStringIntoLeftAndRightWithAbort(dbParmsIndex.Strings[cacheIndex],
                                              constStringEquals,
                                              leftStr,
                                              rightStr);

         { Split that right string again into strings that represent the start and
           end indices into the cache itself. }
         SplitStringIntoLeftAndRightWithAbort(rightStr,
                                              constStringDelimiter,
                                              dbParmsStartStr,
                                              dbParmsEndStr);

         { Convert the start and end indices into the cache into integers. }
         dbParmsStart := StrToInt(dbParmsStartStr);
         dbParmsEnd   := StrToInt(dbParmsEndStr);
         
         { Flag that we found this component in the database cache. }
         foundInDbCache := True;
         WriteToDebugFile('Found dbKey "' + dbKey + '" in dbParmsIndex.  dbParmsStart is ' + IntToStr(dbParmsStart) + ', dbParmsEnd is ' + IntToStr(dbParmsEnd) + '.');

      end { endif got cache hit }

   end; { endif valid database key }

end; { end SearchForCompInDatabaseCache() }


{***************************************************************************
 * function CheckAllParametersVsLocalDatabase()
 *  Check all local database parameters versus the current user and system paramters
 *  for a given component.
 *
 *  NOTE:  Assumes that all string lists have already been created.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CheckAllParametersVsLocalDatabase(    document                : IDocument;
                                               component               : ISch_Component;
                                               locDbKeyName            : TDynamicString;
                                               locDbKey                : WideString;
                                               locDbParmsByRefDesCache : TStringList;
                                               locDbParmsByRefDesIndex : TStringList;
                                               locDbParmsByKeyCache    : TStringList;
                                               locDbParmsByKeyIndex    : TStringList;
                                               compSysParmsList        : TStringList;
                                               compUserParmsList       : TStringList;
                                           var suppressFpUpdates       : Boolean;
                                           var changedFiles            : TStringList;
                                               )                       : Integer;

var
   i                    : Integer;
   compRefDes           : WideString;
   encRefDes            : WideString;
   foundInLocDbByRefDes : Boolean;
   foundInLocDbByKey    : Boolean;
   leftStr              : TDynamicString;
   rightStr             : TDynamicString;
   locDbParmsStartStr   : TDynamicString;
   locDbParmsEndStr     : TDynamicString;
   locDbParmValuesStr   : TDynamicString;
   locDbParmValuesList  : TStringList;
   locDbParmValue       : TDynamicString;
   dbParmName           : TDynamicString;
   dbParmValue          : TDynamicString;
   dbParmVisible        : TDynamicString;
   position             : Integer;
   locDbKeyNew          : WideString;
   dbParmsStart         : Integer;
   dbParmsEnd           : Integer;

begin

   { Assume success. }
   result := 0;
   
   { Retrieve this component's logical refdes. }
   compRefDes          := component.Designator.Text;

   { Init local variable to null. }
   locDbKeyNew := '';

   WriteToDebugFile('In CheckAllParametersVsLocalDatabase(), locDbKey is "' + locDbKey + '".');
   
   {* See if we should restore/maintain legacy/local database parameters as a function of RefDes. *}
   if (locDbParmsByRefDesCache.Count > 0) then
   begin

      WriteToDebugFile('About to check local database as a function of RefDes for "' + compRefDes + '"...');

      { Encode the RefDes so that we may compare it to strings that were already similarly encoded. }
      encRefDes := EncodeLocalParameters(compRefDes);
      
      { Look for this RefDes in the local-database-cache-by-RefDes. }
      SearchForCompInDatabaseCache(encRefDes {dbKey},
                                   locDbParmsByRefDesIndex {dbParmsIndex},
                                   {var} foundInLocDbByRefDes {foundInDbCache},
                                   {var} dbParmsStart,
                                   {var} dbParmsEnd);

      { See if this RefDes was found in the local-database-cache-by-RefDes. }
      if (foundInLocDbByRefDes) then
      begin

         { Loop over all parameters cached for this RefDes. }
         { The initial parameter here will be RefDes, which we will skip. }
         for i := (dbParmsStart+1) to dbParmsEnd do
         begin

            { Split the cache entry (NAME=VALUE) into name and value pieces. }
            SplitStringIntoLeftAndRightWithAbort(locDbParmsByRefDesCache.Strings[i],
                                                 constStringEquals,
                                                 leftStr,
                                                 rightStr);
            
            { Assign database parameter name, value, and visibility. }
            { Decode all local parameters before use in schematic land. }
            dbParmName    := DecodeLocalParameters(leftStr);
            dbParmValue   := DecodeLocalParameters(rightStr);
            dbParmVisible := 'False';

            { Check that the local database parameter matches the component level parameter. }
            CheckDbVsCompParmMatch(document,
                                   component,
                                   suppressFpUpdates,
                                   compSysParmsList,
                                   compUserParmsList,
                                   dbParmName,
                                   dbParmValue,
                                   dbParmVisible,
                                   {var} changedFiles);

            { See if this local database parameter is in fact the local database key. }
            if (dbParmName = locDbKeyName) then
            begin
               locDbKeyNew := dbParmValue;
               WriteToDebugFile('For "' + compRefDes + '", found locDbKeyNew of "' + locDbKeyNew + '".');
            end

            { Else see if we found the parameter called 'SUPPRESS_FP_UPDATES'.  This being set to 1 will cause us to suppress footprint updates for this component only. }
            else if (dbParmName = constDbParmNameSuppressFpUpdates) then
            begin

               { See if parameter is set to appropriate value. }
               suppressFpUpdates := (dbParmValue = constDbParmValueSuppressFpUpdates);
               WriteToDebugFile('Found "' + constDbParmNameSuppressFpUpdates + '" parameter, value is "' + dbParmValue + '".  Set suppressFpUpdates to ' + BoolToStr(suppressFpUpdates) + '.');
            end;
         end; { endfor }

      end { endif found in the local-database-cache-by-RefDes. }

      { Else this part was not found in the local-database-cache-by-RefDes. }
      else
      begin
         
         WriteToDebugFile('NOTE:   Component "' + compRefDes + '" was not found in local-database-cache-by-RefDes.');
         WriteToSummaryFile('NOTE:   Component "' + compRefDes + '" was not found in local-database-cache-by-RefDes.');

      end;

   end; { endif have local database parameters }
      
   {* See if we should restore/maintain legacy/local database parameters as a function of Key. *}
   if (locDbParmsByKeyCache.Count > 0) then
   begin

      { Use the new version of locDbKey if it exists. }
      if (locDbKeyNew <> '') then
         locDbKey := locDbKeyNew;

      { Make sure locDbKey is non-null. }
      if (locDbKey <> '') then
      begin

         WriteToDebugFile('About to check local database as a function of Key for "' + compRefDes + '", locDbKey "' + locDbKey + '"...');

         { Look for this Key in the local-database-cache-by-Key. }
         SearchForCompInDatabaseCache(locDbKey {dbKey},
                                      locDbParmsByKeyIndex {dbParmsIndex},
                                      {var} foundInLocDbByKey {foundInDbCache},
                                      {var} dbParmsStart,
                                      {var} dbParmsEnd);

         { See if this Key was found in the local-database-cache-by-Key. }
         if (foundInLocDbByKey) then
         begin

            { Loop over all parameters cached for this Key. }
            for i := dbParmsStart to dbParmsEnd do
            begin

               { Split the cache entry (NAME=VALUE) into name and value pieces. }
               SplitStringIntoLeftAndRightWithAbort(locDbParmsByKeyCache.Strings[i],
                                                    constStringEquals,
                                                    leftStr,
                                                    rightStr);
               
               { Assign database parameter name, value, and visibility. }
               { Decode all local parameters before use in schematic land. }
               dbParmName    := DecodeLocalParameters(leftStr);
               dbParmValue   := DecodeLocalParameters(rightStr);
               dbParmVisible := 'False';

               { Check that the local database parameter matches the component level parameter. }
               CheckDbVsCompParmMatch(document,
                                      component,
                                      suppressFpUpdates,
                                      compSysParmsList,
                                      compUserParmsList,
                                      dbParmName,
                                      dbParmValue,
                                      dbParmVisible,
                                      {var} changedFiles);

               { Else see if we found the parameter called 'SUPPRESS_FP_UPDATES'.  This being set to 1 will cause us to suppress footprint updates for this component only. }
               if (dbParmName = constDbParmNameSuppressFpUpdates) then
               begin
                  
                  { See if parameter is set to appropriate value. }
                  suppressFpUpdates := (dbParmValue = constDbParmValueSuppressFpUpdates);
                  WriteToDebugFile('Found "' + constDbParmNameSuppressFpUpdates + '" parameter, value is "' + dbParmValue + '".  Set suppressFpUpdates to ' + BoolToStr(suppressFpUpdates) + '.');
               end;

            end; { endfor }

         end { endif found in the local-database-cache-by-Key. }

         { Else this part was not found in the local-database-cache-by-Key. }
         else
         begin
            
            WriteToDebugFile('NOTE:   Component "' + compRefDes + '", key "' + locDbKey + '" was not found in local-database-cache-by-Key.');
            WriteToSummaryFile('NOTE:   Component "' + compRefDes + '", key "' + locDbKey + '" was not found in local-database-cache-by-Key.');

         end;

      end; { endif locDbKey non-null }

   end; { endif have local database parameters }
   
end; { end CheckAllParametersVsLocalDatabase() }


{***************************************************************************
 * function GetAllParametersFromDatabaseTable()
 *  Look for a component in a particular database table.
 *  If found, get all system and user parameters for a given database entry.
 *
 *  NOTE:  Assumes that dbParmsCache and dbParmsCacheIndex string lists have already been created.
 *  
 *  Returns whether this component was found in the specified db dable as var parm foundInThisDbTable.
 *  Returns cache of database parameters as var parm dbParmsCache.
 *  Returns index into database cache as var parm dbParmsCacheIndex.
 *  Returns limits of dbParmsCache as var parms dbParmsStart and dbParmsEnd.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function GetAllParametersFromDatabaseTable(    dbLibDoc           : IDatabaseLibDocument;
                                               compDbKey          : WideString;
                                               tableIndex         : Integer;
                                           var foundInThisDbTable : Boolean;
                                           var dbParmsCache       : TStringList;
                                           var dbParmsCacheIndex  : TStringList;
                                           var dbParmsStart       : Integer;
                                           var dbParmsEnd         : Integer;
                                               )                  : Integer;

var
   i             : Integer;
   dbParmsStr    : WideString;

begin
   
   { Assume success. }
   result := 0;

   { Flag that we have not yet found what we're looking for. }
   foundInThisDbTable := False;

   { Clear the start and end limits of what's populated in dbParmsCache. }
   dbParmsStart := -1;
   dbParmsEnd   := -1;
   
   { Look for this key in the current table in the database. }
   dbParmsStr := dbLibDoc.GetParametersForComponent(tableIndex, compDbKey);
//   WriteToDebugFile('In GetAllParametersFromDatabaseTable(), tableIndex is ' + IntToStr(tableIndex) + ', dbParmsStr is "' + dbParmsStr + '"');

   { See if we found it.  If so, process it. }
   if (dbParmsStr <> '') then
   begin

      { Flag that we found the database entry. }
      foundInThisDbTable := True;

      { Record the starting index of the new database record that we're going to add to the cache. }
      dbParmsStart := dbParmsCache.Count;

      { Split apart the gigantic string returned by the database table query and add to cache.}
      SplitDelimitedStringIntoList(dbParmsStr,
                                   constStringDelimiter,
                                   {var} dbParmsCache);

      { Record the ending index of the new database record that just added to the cache. }
      dbParmsEnd   := (dbParmsCache.Count - 1);

      { Create a new record in the cache index, now that we've added this new entry. }
      { If this were compDbKey C000010001 and it now occupied indices 33 to 75 in the cache, the
       entry in index would be "C000010001=33|75". }
      dbParmsCacheIndex.add(compDbKey + constStringEquals + IntToStr(dbParmsStart) + constStringDelimiter + IntToStr(dbParmsEnd));
      WriteToDebugFile('In GetAllParametersFromDatabaseTable(), adding new entry to cache index : "' + compDbKey + constStringEquals + IntToStr(dbParmsStart) + constStringDelimiter + IntToStr(dbParmsEnd) + '".');

   end; { endif }

end; { end GetAllParametersFromDatabaseTable() }


{***************************************************************************
 * function GetAllParametersFromDatabase()
 *  Look for a component in the database.
 *  If found, get all system and user parameters for a given database entry.
 *
 *  NOTE:  Assumes that dbParmsCache and dbParmsCacheIndex string lists have already been created.
 *  
 *  Returns whether this component was found in the specified db dable as var parm foundInThisDbTable.
 *  Returns cache of database parameters as var parm dbParmsCache.
 *  Returns index into database cache as var parm dbParmsCacheIndex.
 *  Returns limits of dbParmsCache as var parms dbParmsStart and dbParmsEnd.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function GetAllParametersFromDatabase(    compRefDes          : WideString;
                                          linkedOnlyViaDbLink : Boolean;
                                          dbLibDoc            : IDatabaseLibDocument;
                                          dbTableList         : TStringList;
                                          compDbKey           : WideString;
                                          compDbTable         : WideString;
                                      var linkedToDb          : Boolean;
                                      var dbParmsCache        : TStringList;
                                      var dbParmsCacheIndex   : TStringList;
                                      var dbParmsStart        : Integer;
                                      var dbParmsEnd          : Integer;
                                          )                   : Integer;

var
   dbTableName        : TDynamicString;
   tableIndex         : Integer;
   foundInDbCache     : Boolean;
   foundInThisDbTable : Boolean;
   cacheIndex         : Integer;
   leftStr            : TDynamicString;
   rightStr           : TDynamicString;
   dbParmsStartStr    : TDynamicString;
   dbParmsEndStr      : TDynamicString;

begin

   { Assume success. }
   result := 0;

   { Flag that we haven't yet found this component in the database cache. }
   foundInDbCache := False;
   
   { Flag that we haven't found the appropriate database table. }
   tableIndex := -1;

   { Look for this component in database cache. }
   SearchForCompInDatabaseCache(compDbKey {dbKey},
                                dbParmsCacheIndex {dbParmsIndex},
                                {var} foundInDbCache,
                                {var} dbParmsStart,
                                {var} dbParmsEnd);


   { If we were given the name of a database table, then look that name up in the list
    of database table names. }
   { We will unconditionally do this step, even if the part is already in the database cache,
    as a sanity check to be sure that the database table name is properly set. }
   if (compDbTable <> '') then
   begin

      { Look up table name from component in list of database tables. }
      tableIndex := dbTableList.IndexOf(compDbTable);
      WriteToDebugFile(' compDbTable "' + compDbTable + '" is index ' + IntToStr(tableIndex) + ' in list of db tables.');

      { Throw an error if we failed here. }
      if (tableIndex < 0) then
      begin
         
         WriteToDebugFile('ERROR:    ' + 'Designator ' + compRefDes + ':  compDbTable "' + compDbTable + '" not found in list of db tables.  Please fix db table name!!');
         WriteToSummaryFile('ERROR:    ' + 'Designator ' + compRefDes + ':  compDbTable "' + compDbTable + '" not found in list of db tables.  Please fix db table name!!');

      end;   
      
   end; { endif }

   { If this component was not found in the database cache, then proceed to look for it in the database itself. }
   if (not foundInDbCache) then
   begin
   
      { If we have a valid index into the list of database tables, then look for this component in that table. }
      if (tableIndex >= 0) then
      begin

         { Look for this component in this database table.
          If found, get all parameters specified in database. }
         GetAllParametersFromDatabaseTable(dbLibDoc,
                                           compDbKey,
                                           tableIndex,
                                           {var} foundInThisDbTable,
                                           {var} dbParmsCache,
                                           {var} dbParmsCacheIndex,
                                           {var} dbParmsStart,
                                           {var} dbParmsEnd);
         
      end; { endif }

      { If the database entry for this component has not yet been found, then loop over all db tables and look in each one. }
      if (not foundInThisDbTable) then
      begin

         WriteToDebugFile(' Proceeding to query all tables for this component.');

         { Loop over all tables in this database. }
         for tableIndex := 0 to (dbLibDoc.GetTableCount - 1) do
         begin

            { If the database entry for this component has not yet been found, then keep looking for it. }
            if (not foundInThisDbTable) then
            begin

               { Look for this component in this database table.
                If found, get all parameters specified in database. }
               GetAllParametersFromDatabaseTable(dbLibDoc,
                                                 compDbKey,
                                                 tableIndex,
                                                 {var} foundInThisDbTable,
                                                 {var} dbParmsCache,
                                                 {var} dbParmsCacheIndex,
                                                 {var} dbParmsStart,
                                                 {var} dbParmsEnd);
               
            end; { endif not foundInThisDbTable }
            
         end; { endfor loop over db tables }

      end; { endif not foundInThisDbTable }

   end; { endif not foundInDbCache }

   
   { Tell the caller whether this component is successfully linked to database. }
   linkedToDb := (foundInDbCache or foundInThisDbTable);   
         
   { If we didn't find this component in the database, then flag an error for the user. }
   if (not linkedToDb) then
   begin

      { Tell summary file about this problem. }
      WriteToDebugFile('ERROR:    ' + 'Designator ' + compRefDes + ':  Could not find database link for this component!  Please fix!!!');
      WriteToSummaryFile('ERROR:    ' + 'Designator ' + compRefDes + ':  Could not find database link for this component!  Please fix!!!');

   end { endif }

   { Else we're linked to database.  But report if we have only a DBLink connection. }
   else if (linkedOnlyViaDbLink) then
   begin
      WriteToDebugFile('WARNING:  ' + 'Designator ' + compRefDes + ':  This component is linked using only DBLink file!!  Sch symbol may differ and DB lookups are slower!');
      WriteToSummaryFile('WARNING:  ' + 'Designator ' + compRefDes + ':  This component is linked using only DBLink file!!  Sch symbol may differ and DB lookups are slower!');
   end; { end elsif}
      
end; { end GetAllParametersFromDatabase() }


{***************************************************************************
 * function CheckAllParametersVsDatabase()
 *  Check all database parameters versus the current user and system paramters
 *  for a given component.
 *
 *  Returns number of system parameters (footprints) specified in db record in var parm dbNumFootprints.
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CheckAllParametersVsDatabase(    document          : IDocument;
                                          component         : ISch_Component;
                                          suppressFpUpdates : Boolean;
                                          linkedToDb        : Boolean;
                                          dbParmsCache      : TStringList;
                                          dbParmsStart      : Integer;
                                          dbParmsEnd        : Integer;
                                          compSysParmsList  : TStringList;
                                          compUserParmsList : TStringList;
                                      var dbNumFootprints   : Integer;
                                      var changedFiles      : TStringList;
                                          )                 : Integer;

var
   i             : Integer;
   compRefDes    : TDynamicString;
   locDbKey      : TDynamicString;
   leftStr       : TDynamicString;
   rightStr      : TDynamicString;
   dbParmName    : TDynamicString;
   dbParmValue   : TDynamicString;
   dbParmVisible : TDynamicString;
   dbDescription : TDynamicString;
   dbSymName     : TDynamicString;
   position      : Integer;

begin

   { Assume success. }
   result := 0;

   { Retrieve this component's logical refdes. }
   compRefDes          := component.Designator.Text;

   { Make sure component is actually linked to database. }
   if (linkedToDb) then
   begin
   
      WriteToDebugFile('About to check all database parameters for component "' + compRefDes + '"...');

      { Init number of footprints allowed by database record to be 0. }
      dbNumFootprints := 0;

      { Loop over all database parameters. }
      for i := dbParmsStart to dbParmsEnd do
      begin

         //    WriteToDebugFile('db entry is "' + dbParmsCache.Strings[i] + '".');

         { Clear this variable, since it will not always be set in the following code. }
         dbParmVisible := '';

         { This entry will look something like "foo=bar".
          So split it into a left string (before '=' char) and a right string (after '=' char). }
         SplitStringIntoLeftAndRightWithAbort(dbParmsCache.Strings[i],
                                              constStringEquals,
                                              leftStr,
                                              rightStr);

         {* Look for a few keywords in the left string. *}

         { Note:  The order that user parameters are declared in the db entry seems to be Name, Value, Visible:
          "ParameterName0=Rating".
          "ParameterValue0=50V".
          "ParameterVisible0=True". }        
         
         { Look for left string to start with 'ParameterName' and if so, cache the right string. }
         if (AnsiPos(constDbParmParameterName, leftStr) <> 0) then
         begin
            dbParmName := rightStr;
            //                  WriteToDebugFile('dbParmName is "' + dbParmName + '".');
         end

         { Look for left string to start with 'ParameterValue' and if so, cache the right string. }
         else if (AnsiPos(constDbParmParameterValue, leftStr) <> 0) then
         begin
            dbParmValue := rightStr;
            //                  WriteToDebugFile('dbParmValue is "' + dbParmValue + '".');

            {* Check the schematic symbol name vs. the one called out by the database. *}
            { See if this user parameter is equal to the magical one that we use to check whether the
             schematic symbol in use in the SchDoc file is the one called out by the database. }
            if (dbParmName = constDbParmShadowSymName) then
            begin

               { See if this is a placeholder schematic symbol. }
               { Note:  Leftmost index in a string for purposes of Copy() is 1! }
               if (Copy(dbParmValue, 1, Length(constDbSchPlaceholderPrefix)) = constDbSchPlaceholderPrefix) then
               begin

                  { In this case we have no need to separate symbol library name from symbol name. }
                  dbSymName := dbParmValue;

               end { endif }

               { Else this is a real schematic symbol with a library name and a symbol name. }
               else
               begin
                  
                  { First we need to strip off just the symbol name from the string provided by the database, which
                   also includes the name of the schematic library file. }
                  { For example, database will specify "FOO_CAPS\CAP_NP", but we want to extract just the "CAP_NP" part. }
                  position := LastDelimiter(constStringSymLibNameDelim, dbParmValue);

                  { Sanity check. }
                  if (position = 0) then
                     MyAbort('Unable to find delimiter "' + constStringSymLibNameDelim + '" in schematic symbol name "' + dbParmValue + '" given by database.');

                  { Extract just the symbol name. }
                  dbSymName := Copy(dbParmValue, (position+1), MaxInt);

               end; { endelse }
               
               { Now check that the sch symbol desired by the database is the one actually in use for this comp. }
               if (dbSymName = component.LibReference) then
               begin
                  WriteToDebugFile('Schematic symbol matches the symbol that database wants, specifically "' + dbParmValue + '".');
               end

               { Else there is a mismatch.  We dare not actually swap out the component in question, since
                this will generally result in a new symbol with different shape, different connection points, etc.
                So we're just going to flag this for the user and he/she needs to fix it manually. }
            else
            begin
               WriteToDebugFile('ERROR:    ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Schematic symbol mismatch.  Database wants symbol "' + dbParmValue + '", but SchDoc is currently using symbol "' + component.LibReference + '".  You MUST fix this issue manually, since new symbol may be larger, smaller, have different connection points, etc.');
               WriteToSummaryFile('ERROR:    ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Schematic symbol mismatch.  Database wants symbol "' + dbParmValue + '", but SchDoc is currently using symbol "' + component.LibReference + '".  You MUST fix this issue manually, since new symbol may be larger, smaller, have different connection points, etc.');
               
            end; { endelse }

            end; { endif }
            
         end { elsif }
         
         { Look for left string to start with 'ParameterVisible' and if so, check component vs. database entry. }
         else if (AnsiPos(constDbParmParameterVisible, leftStr) <> 0) then
         begin
            dbParmVisible := rightStr;
            //                  WriteToDebugFile('dbParmVisible is "' + dbParmVisible + '".');

            { Check that the database parameter matches the component level parameter. }
            CheckDbVsCompParmMatch(document,
                                   component,
                                   suppressFpUpdates,
                                   compSysParmsList,
                                   compUserParmsList,
                                   dbParmName,
                                   dbParmValue,
                                   dbParmVisible,
                                   {var} changedFiles);
         end

         { Look for left string to start with 'Description' and if so, act on it. }
         else if (AnsiPos(constDbParmDescription, leftStr) <> 0) then
         begin
            dbDescription := rightStr;
            WriteToDebugFile('Description is "' + dbDescription + '".');
            
            { Check that the database parameter matches the component level parameter. }
            dbParmName := constDbParmDescription;
            dbParmValue := rightStr;
            CheckDbVsCompParmMatch(document,
                                   component,
                                   suppressFpUpdates,
                                   compSysParmsList,
                                   compUserParmsList,
                                   dbParmName,
                                   dbParmValue,
                                   dbParmVisible,
                                   {var} changedFiles);
         end { elsif }

         { Look for left string to start with 'ModelType' or 'ModelName'. }
         else if ( (AnsiPos(constDbParmModelType, leftStr) <> 0) or (AnsiPos(constDbParmModelName, leftStr) <> 0) ) then
         begin

            dbParmName := leftStr;
            dbParmValue := rightStr;

            { Check that the database parameter matches the component level parameter. }
            CheckDbVsCompParmMatch(document,
                                   component,
                                   suppressFpUpdates,
                                   compSysParmsList,
                                   compUserParmsList,
                                   dbParmName,
                                   dbParmValue,
                                   dbParmVisible,
                                   {var} changedFiles);

            { If needed, increment the number of footprints specified in the database record for this component. }
            if (AnsiPos(constDbParmModelType, leftStr) <> 0) then
            begin
               dbNumFootprints := dbNumFootprints + 1;
            end;

         end { elsif }

      end; { endfor loop over database parameters }

   end; { endif linkedToDb }
   
end; { end CheckAllParametersVsDatabase() }


{***************************************************************************
 * function MaintainCurrentFootprint()
 *  Maintain that the footprint that was previously marked "current" will
 *  remain so, in the face of changing around the order of footprints and
 *  possibly updating them as well.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function MaintainCurrentFootprint(    document          : IDocument,
                                      component         : ISch_Component;
                                      compSysParmsList  : TStringList;
                                      compUserParmsList : TStringList;
                                  var changedFiles      : TStringList;
                                      )                 : Integer;

var
   i                 : Integer;
   leftStr           : TDynamicString;
   rightStr          : TDynamicString;
   dbParmName        : TDynamicString;
   dbParmValue       : TDynamicString;
   dbParmVisible     : TDynamicString;
   suppressFpUpdates : Boolean;
   
begin

   { Assume success. }
   result := 0;

   { Set this local var to safe value for our purposes here. }
   suppressFpUpdates := False;

   { Init this to a defined, yet bogus value. }
   dbParmVisible := '';
   
   {** Look through the system parameters attached to the component. **}
   WriteToDebugFile('About to loop over all system parameters...');
   for i := 0 to (compSysParmsList.Count - 1) do
   begin

      { This entry will look something like "foo=bar".
       So split it into a left string (before '=' char) and a right string (after '=' char). }
      SplitStringIntoLeftAndRightWithAbort(compSysParmsList.Strings[i],
                                           constStringEquals,
                                           leftStr,
                                           rightStr);

      {* Look for a few keywords in the left string. *}

      { Look for left string to start with 'CurrentModelType' or 'CurrentModelName'. }
      if ( (AnsiPos(constDbParmCurrentModelType, leftStr) <> 0) or (AnsiPos(constDbParmCurrentModelName, leftStr) <> 0) ) then
      begin

         dbParmName := leftStr;
         dbParmValue := rightStr;

         WriteToDebugFile('In MaintainCurrentFootprint(), dbParmName is "' + dbParmName + '", dbParmValue is "' + dbParmValue + '".');
         
         { Check that the database parameter matches the component level parameter. }
         CheckDbVsCompParmMatch(document,
                                component,
                                suppressFpUpdates,
                                compSysParmsList,
                                compUserParmsList,
                                dbParmName,
                                dbParmValue,
                                dbParmVisible,
                                {var} changedFiles);

      end; { endif }

   end; { endfor }

end; { end MaintainCurrentFootprint() }


{***************************************************************************
 * function DeleteExcessFootprints()
 *  Delete any footprints that exceed the number of footprints specified
 *  by database record.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function DeleteExcessFootprints(    document        : IDocument,
                                    component       : ISch_Component;
                                    dbNumFootprints : Integer;
                                var changedFiles    : TStringList;
                                    )               : Integer;

var
   ImplIterator   : ISch_Iterator;
   SchModel       : ISch_Implementation;
   SchModelNext   : ISch_Implementation;
   modelName      : TDynamicString;
   modelIndex     : Integer;
   footprintIndex : Integer;
   
begin

   { Assume success. }
   result := 0;

   { Create iterator to iterate over sch implementations (models). }
   ImplIterator := component.SchIterator_Create;
   ImplIterator.AddFilter_ObjectSet(MkSet(eImplementation));

   try // begin { Emacs pascal mode doesn't understand Try construct.  Add fake "begin" keyword. }

      WriteToDebugFile('Checking for excess footprints.  This comp should have ' + IntToStr(dbNumFootprints) + ' footprints.');

      { Get a reference to the first model. }
      SchModel := ImplIterator.FirstSchObject;

      { Loop over all models. }
      modelIndex := 0;
      footprintIndex := 0;
      while (SchModel <> Nil) do
      begin

         { Look ahead to get the reference to the next sch model after this one. }
         SchModelNext := ImplIterator.NextSchObject;
            
         { Make sure this model is a footprint. }
         if (AnsiUpperCase(SchModel.ModelType) = constKindPcbLib) then
         begin
            
            { Cache the name of this footprint. }
            modelName := SchModel.ModelName;
            
            { See if this model (footprint) exceeds the number allowed by the database entry for this component. }
            if ( footprintIndex > (dbNumFootprints-1) ) then
            begin

               { Delete the current sch model. }
               component.RemoveSchImplementation(SchModel);

               { Tell user what we just did. }
               WriteToDebugFile('INFO:     ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Removed excess footprint "ModelName' + IntToStr(footprintIndex) + '"="' + modelName + '".');
               WriteToSummaryFile('INFO:     ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Removed excess footprint "ModelName' + IntToStr(footprintIndex) + '"="' + modelName + '".');

            end; { endif }

            { Increment footprint index. }
            footprintIndex := footprintIndex + 1;
               
         end; { endif is model a footprint }
         
         { Advance to next model within this schematic component. }
         modelIndex := modelIndex + 1;
         SchModel := SchModelNext;
         
      end; { endwhile SchModel <> Nil }

      { Finally clause... }
      finally

      { Free iterator. }
      component.SchIterator_Destroy(ImplIterator);
   end; { endtry }

end; { end DeleteExcessFootprints() }


{***************************************************************************
 * function VerifyOneFootprintIsCurrent()
 *  Verify that one footprint in the current component is marked "current".
 *  If not, set the primary footprint to be "current".
 *
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function VerifyOneFootprintIsCurrent(    document      : IDocument,
                                         component     : ISch_Component;
                                      var changedFiles : TStringList;
                                          )            : Integer;

var
   ImplIterator   : ISch_Iterator;
   SchModel       : ISch_Implementation;
   numCurrent     : Integer;
   msg            : TDynamicString;
   footprintIndex : Integer;
   
begin

   { Assume success. }
   result := 0;

   { Create iterator to iterate over sch implementations (models). }
   ImplIterator := component.SchIterator_Create;
   ImplIterator.AddFilter_ObjectSet(MkSet(eImplementation));

   try // begin { Emacs pascal mode doesn't understand Try construct.  Add fake "begin" keyword. }

      { Flag that we have not yet found what we're looking for. }
      WriteToDebugFile('Double checking that one footprint for this component is marked selected.');
      numCurrent := 0;

      { Get a reference to the first model. }
      SchModel := ImplIterator.FirstSchObject;

      { Loop over all models. }
      while (SchModel <> Nil) do
      begin

         { Make sure this model is a footprint. }
         if (AnsiUpperCase(SchModel.ModelType) = constKindPcbLib) then
         begin
            
            { If this footprint is already current, then increment our count. }
            if (SchModel.IsCurrent) then
            begin
               numCurrent := numCurrent + 1;            
            end;

         end; { endif is model a footprint }
         
         { Advance to next model within this schematic component. }
         SchModel := ImplIterator.NextSchObject;
         
      end; { endwhile SchModel <> Nil }

      { Sanity check. }
      if (numCurrent > 1) then
         MyAbort('Found multiple footprints that were marked as selected!!');

      { If 0 footprints were marked current, then mark the primary one as current. }
      if (numCurrent = 0) then
      begin

         { Get a reference to the first model. }
         SchModel := ImplIterator.FirstSchObject;

         { Loop over all models. }
         footprintIndex := 0;
         while (SchModel <> Nil) do
         begin
            
            { Make sure this model is a footprint. }
            if (AnsiUpperCase(SchModel.ModelType) = constKindPcbLib) then
            begin

               { Set the 0th footprint to be the current one. }
               if (footprintIndex = 0) then
               begin
                  
                  WriteToDebugFile('Could not find a footprint marked as selected!  Setting the new primary footprint to be selected.');
                  
                  msg := ('WARNING:  ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Set primary footprint "' + SchModel.ModelName + '" to be the selected one, since no footprint was already marked as selected!');

                  { Call SetModelToBeCurrent() to do all the real work. }
                  SetModelToBeCurrent(document,
                                      component,
                                      SchModel,
                                      msg,
                                      {var} changedFiles);

               end; { endif footprintIndex = 0 }

               { Increment footprint index. }
               footprintIndex := footprintIndex + 1;
               
            end; { endif is model a footprint }

            { Advance to next model within this schematic component. }
            SchModel := ImplIterator.NextSchObject;

         end; { endwhile }

      end; { endif }

      { Finally clause... }
      finally

      { Free iterator. }
      component.SchIterator_Destroy(ImplIterator);
   end; { endtry }

end; { end VerifyOneFootprintIsCurrent() }


{***************************************************************************
 * function CheckSchComponentsVsDatabase()
 *  Attempt to retrieve user and system parameters for all components in a sch page.
 *  Then proceed to check these parameters against the database entry for each comp.
 *
 *  In addition, store certain information about logical components for later
 *  use when dealing with component variants.
 *
 *  NOTE:  Assumes that dbParmsCache and dbParmsCacheIndex string lists have already been created.
 *  NOTE:  Assumes that logCompCacheByRefDes and logCompCacheByUniqueId string lists have already been created.
 *  
 *  Returns cache of database parameters as var parm dbParmsCache.
 *  Returns index into database cache as var parm dbParmsCacheIndex.
 *  Returns logical component information indexed by logRefDes as var parm phyCompCacheByRefDes.
 *  Returns logical component information indexed by logUniqueId as var parm phyCompCacheByUniqueId.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CheckSchComponentsVsDatabase(    document                : IDocument,
                                          locDbKeyName            : WideString;
                                          locDbParmsByRefDesCache : TStringList;
                                          locDbParmsByRefDesIndex : TStringList;
                                          locDbParmsByKeyCache    : TStringList;
                                          locDbParmsByKeyIndex    : TStringList;
                                          dbLibDoc                : IDatabaseLibDocument;
                                          dbTableList             : TStringList;
                                      var dbParmsCache            : TStringList;
                                      var dbParmsCacheIndex       : TStringList;
                                      var logCompCacheByRefDes    : TStringList;
                                      var logCompCacheByUniqueId  : TStringList;
                                      var changedFiles            : TStringList;
                                          )                       : Integer;
var                                                           
   CurrentSch          : ISch_Sheet;
   Iterator            : ISch_Iterator;
   component           : ISch_Component;
   i                   : Integer;
   compDbKey           : WideString;
   compDbTable         : WideString;
   compSysParmsList    : TStringList;
   compUserParmsList   : TStringList;
   linkedToDb          : Boolean;
   dbNumFootprints     : Integer;
   dbParmsStart        : Integer;
   dbParmsEnd          : Integer;
   linkedOnlyViaDbLink : Boolean;
   suppressFpUpdates   : Boolean;
   compRefDes          : WideString;
   locDbKey            : WideString;
   foundInLocDb        : Boolean;
   locDbParmsStart     : Integer;
   locDbParmsEnd       : Integer;
                                      
begin

   { Assume success. }
   result := 0;

   { Sanity check. }
   if SchServer = Nil then
      MyAbort('SchServer was Nil!!');

   { Initialize the robots in Schematic editor. }
   SchServer.ProcessControl.PreProcess(document, '');

   CurrentSch := SchServer.GetSchDocumentByPath(document.DM_FullPath);

   { Sanity check. }
   if CurrentSch = Nil then
      MyAbort('CurrentSch was Nil!!');

   { Look for components only }
   Iterator := CurrentSch.SchIterator_Create;
   Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

   WriteToDebugFile('Component parameters report:');

   { Initialize list of system parameters currently attached to a component. }
   compSysParmsList := TStringList.Create;       
   compSysParmsList.Duplicates := dupIgnore;
   compSysParmsList.Sorted := True;

   { Initialize list of user parameters currently attached to a component. }
   compUserParmsList := TStringList.Create;      
   compUserParmsList.Duplicates := dupIgnore;
   compUserParmsList.Sorted := True;

   try // begin { Emacs pascal mode doesn't understand Try construct.  Add fake "begin" keyword. }

      { Loop over all components in this schematic page. }
      component := Iterator.FirstSchObject;
      while (component <> Nil) do
      begin

         { Exclude bogus components, such as title blocks. }
         if (component.Designator.Text <> '*') then 
         begin           

            {** Get system parameters (aka models aka footprints) currently attached to this component. **}
            GetSysParametersFromComp(document,
                                     component,
                                     {var} compSysParmsList);

            
            {** Check and, if needed, correct DatabaseLibraryName and dbTableName. **}
            CheckAndCorrectDatabaseLibraryName(document,
                                               component,
                                               {var} changedFiles);

            
            {** Get user parameters currently attached to this component. **}
            GetUserParametersFromComp(document,
                                      component,
                                      locDbKeyName,
                                      {var} locDbKey,
                                      {var} suppressFpUpdates,
                                      {var} linkedOnlyViaDbLink,
                                      {var} compDbKey,
                                      {var} compDbTable,
                                      {var} compUserParmsList,
                                      {var} logCompCacheByRefDes,
                                      {var} logCompCacheByUniqueId,
                                      {var} changedFiles);
            
            
            {** Check all parameters for this component compared to what the local database (if any) says they should be. **}
            CheckAllParametersVsLocalDatabase(document,
                                              component,
                                              locDbKeyName,
                                              locDbKey,
                                              locDbParmsByRefDesCache,
                                              locDbParmsByRefDesIndex,
                                              locDbParmsByKeyCache,
                                              locDbParmsByKeyIndex,
                                              compSysParmsList,
                                              compUserParmsList,
                                              {var} suppressFpUpdates,
                                              {var} changedFiles);

            
            {** Get all database parameters for this component. **}
            compRefDes          := component.Designator.Text;
            GetAllParametersFromDatabase(compRefDes,
                                         linkedOnlyViaDbLink,
                                         dbLibDoc,
                                         dbTableList,
                                         compDbKey,
                                         compDbTable,
                                         {var} linkedToDb,
                                         {var} dbParmsCache,
                                         {var} dbParmsCacheIndex,
                                         {var} dbParmsStart,
                                         {var} dbParmsEnd);

            
            {** Check all parameters for this component compared to what the database says they should be. **}
            CheckAllParametersVsDatabase(document,
                                         component,
                                         suppressFpUpdates,
                                         linkedToDb,
                                         dbParmsCache,
                                         dbParmsStart,
                                         dbParmsEnd,
                                         compSysParmsList,
                                         compUserParmsList,
                                         {var} dbNumFootprints,
                                         {var} changedFiles);

            
            {** Check user parameters for this component. **}
            CheckUserParametersForComp(document,
                                       component,
                                       compUserParmsList,
                                       {var} changedFiles);

            
            { Make sure component is successfully linked to database. }
            if (linkedToDb) then
            begin
            
               { See if we are allowed to update footprints. }
               if ( (enableFootprintUpdates) and (not suppressFpUpdates) ) then
               begin
                  
                  {** Maintain which footprint is marked "current", across changes to ordering of footprints and updating of same. **}
                  MaintainCurrentFootprint(document,
                                           component,
                                           compSysParmsList,
                                           compUserParmsList,
                                           {var} changedFiles);

                  
                  {** Delete any excess (beyond what is specified in database record) footprints from this component. **}
                  DeleteExcessFootprints(document,
                                         component,
                                         dbNumFootprints,
                                         {var} changedFiles);

                  
                  {** Verify that after whatever changes were made, that exactly one footprint is marked "current". **}
                  VerifyOneFootprintIsCurrent(document,
                                              component,
                                              {var} changedFiles);

               end; { endif enableFootprintUpdates }

            end; { endif linkedToDb }
            
            { Clear list of parameters currently attached to this component. }
            compSysParmsList.Clear;
            compUserParmsList.Clear;
            
         end; { endif component.Designator.Text <> '*' }
            
         { Move on to next schematic component. }
         WriteToDebugFile('');
         component := Iterator.NextSchObject;

      end; { endwhile }

      { When we're all done, free the sch iterator. }
      finally
      CurrentSch.SchIterator_Destroy(Iterator);
   end;

   { Free list of parameters currently attached to a component. }
   compSysParmsList.Free;
   compUserParmsList.Free;
         
   { Clean up the robots in Schematic editor. }
   SchServer.ProcessControl.PostProcess(document, '');

   { Refresh the screen. }
   CurrentSch.GraphicallyInvalidate;
   
end; { end CheckSchComponentsVsDatabase() }


{***************************************************************************
 * function CachePhysicalComponentInfo()
 *  Analyze the "flattened" pseudo-schematic page and retrieve and store
 *  information for mapping physical components back to schematic level
 *  logical components.  This information will be needed later when handling
 *  component variants, since these are stored with respect to physical
 *  components.
 *
 *  NOTE:  Assumes that design has already been compiled!
 *  NOTE:  Assumes that phyCompCacheByRefDes and phyCompCacheByUniqueId string lists have already been created.
 *  
 *  Returns physical component information indexed by physRefDes as var parm phyCompCacheByRefDes.
 *  Returns physical component information indexed by physUniqueId as var parm phyCompCacheByUniqueId.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CachePhysicalComponentInfo(    Project                : IProject;
                                    var phyCompCacheByRefDes   : TStringList;
                                    var phyCompCacheByUniqueId : TStringList;
                                        )                      : Integer;

var
   flatSchem : IDocument;
   i         : Integer;
   component : IComponent;
   part      : IPart;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   {** Analyze flattened pseudo-schematic in order to map physical designators to logical designators, etc. **}
   { Note:  Code borrowed & adapted from AgileBOMV1.1.pas. }

   { Get a reference to the flattened schematic document. }
   flatSchem := Project.DM_DocumentFlattened;

   { Sanity check. }
   if (flatSchem = Nil) then
      MyAbort('Unable to get flatSchem.  Probably this means the project hasn''t been compiled, but it should have been so already.');

   { Output debug info. }
   WriteToDebugFile('*About to process flattened components....');

   { Loop over all physical components in flattened schematic document. }
   for i := 0 to (flatSchem.DM_ComponentCount - 1) do
   begin

      { Retrieve reference to next flattened schematic component. }
      component := flatSchem.DM_Components(i);

      { Retrieve reference to the 0th subpart of this component. }
      part         := component.DM_SubParts(0);
   
      { Output debug info. }
      WriteToDebugFile('* Processing flattened component logical refdes "' + part.DM_LogicalDesignator + '", physical refdes "' + part.DM_PhysicalDesignator + '", UniqueId "' + part.DM_UniqueId + '", UniqueIdName "' + part.DM_UniqueIdName + '", UniqueIdPath "' + part.DM_UniqueIdPath + '".');

      {* Cache information about this physical component for later use in dealing with varied components. *}
      { Format:  PhysicalDesignator=LogicalDesignator|UniqueIdPath\UniqueId }
      phyCompCacheByRefDes.Add(part.DM_PhysicalDesignator + constStringEquals + part.DM_LogicalDesignator + constStringDelimiter + part.DM_UniqueIdPath + constStringUniqueIdPathSep + part.DM_UniqueId);

      { Format:  UniqueIdPath\UniqueId=LogicalDesignator|PhysicalDesignator }
      phyCompCacheByUniqueId.Add(part.DM_UniqueIdPath + constStringUniqueIdPathSep + part.DM_UniqueId + constStringEquals + part.DM_LogicalDesignator + constStringDelimiter + part.DM_PhysicalDesignator);

   end; { endfor }
   
end; { end CachePhysicalComponentInfo() }


{***************************************************************************
 * function ExtractDbUserParmsForComp()
 *  Extract just the database user parameters for a given database componennt.
 *  
 *  Note:  Assumes that dbCompUserParms string list has already been created.
 *
 *  Returns database user parameters for specified component as var parm dbCompUserParms.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function ExtractDbUserParmsForComp(    dbParmsCache      : TStringList;
                                       dbParmsStart      : Integer;
                                       dbParmsEnd        : Integer;
                                   var dbCompUserParms   : TStringList;
                                       )                 : Integer;

var
   i           : Integer;
   leftStr     : TDynamicString;
   rightStr    : TDynamicString;
   dbParmName  : TDynamicString;
   dbParmValue : TDynamicString;

begin

   { Assume success. }
   result := 0;

   { Loop over all database parameters. }
   WriteToDebugFile('*In ExtractDbUserParmsForComp()...');
   for i := dbParmsStart to dbParmsEnd do
   begin

//    WriteToDebugFile('db entry is "' + dbParmsCache.Strings[i] + '".');

      { This entry will look something like "foo=bar".
       So split it into a left string (before '=' char) and a right string (after '=' char). }
      SplitStringIntoLeftAndRightWithAbort(dbParmsCache.Strings[i],
                                           constStringEquals,
                                           leftStr,
                                           rightStr);

      {* Look for a few keywords in the left string. *}

      { Note:  The order that user parameters are declared in the db entry seems to be Name, Value, Visible:
       "ParameterName0=Rating".
       "ParameterValue0=50V".
       "ParameterVisible0=True". }        
      
      { Look for left string to start with 'ParameterName' and if so, cache the right string. }
      if (AnsiPos(constDbParmParameterName, leftStr) <> 0) then
      begin
         dbParmName := rightStr;
      end

      { Look for left string to start with 'ParameterValue' and if so, store the previously stored name along with this value. }
      else if (AnsiPos(constDbParmParameterValue, leftStr) <> 0) then
      begin
         dbParmValue := rightStr;

         { Add this user parameter as a NAME=VALUE pair to the dbCompUserParms string list. }
         dbCompUserParms.Add(dbParmName + constStringEquals + dbParmValue);
         WriteToDebugFile('* Adding this entry to dbCompUserParms: "' + dbParmName + constStringEquals + dbParmValue + '".');
         
      end { elsif }
      
      { Look for left string to start with 'Description' and if so, store it. }
      else if (AnsiPos(constDbParmDescription, leftStr) <> 0) then
      begin
         dbParmValue := rightStr;
         
         { Check that the database parameter matches the component level parameter. }
         dbParmName := constDbParmDescription;
         dbParmValue := rightStr;

         { Add this user parameter as a NAME=VALUE pair to the dbCompUserParms string list. }
         dbCompUserParms.Add(dbParmName + constStringEquals + dbParmValue);
         WriteToDebugFile('* Adding this entry to dbCompUserParms: "' + dbParmName + constStringEquals + dbParmValue + '".');
         
      end { elsif }

   end; { endfor loop over database parameters }
   
end; { end ExtractDbUserParmsForComp() }


{***************************************************************************
 * function WriteDbCompDiffsForVariedCompToProjectFile()
 *  Analyze the differences in user parameters between the database entries
 *  for the specified varied component vs. the database entries for the
 *  original component.  For all fields that differ, create a text entry
 *  for the project file describing the varied parameter.
 *
 *  Note that we are NOT parsing and analyzing the previously existing entry
 *  in the project file describing this varied component.  Instead, we
 *  are examining the database entries for both the original and varied
 *  components and computing a list of differing parameters from scratch.
 *
 *  The physical component to be varied is described by physical refdes phyDesignator
 *  and physical unique ID phyUniqueId.  The database key for the variant
 *  component to be loaded in place of this physical part is described by varCompDbKey.
 *
 *  We rely on already-populated string lists logCompCacheByRefDes and logCompCacheByUniqueId
 *  to have stored information about all logical components that have been previously
 *  processed by this script.  Information stored includes the database key for
 *  all such components.
 *
 *  We rely on already-populated string lists phyCompCacheByRefDes and phyCompCacheByUniqueId
 *  to store mapping info that allows us to map from a physical component back
 *  to its logical schematic component.
 *
 *  NOTE:  Assumes that dbParmsCache and dbParmsCacheIndex string lists have already been created.
 *  
 *  Returns possibly incremented index of parameter variants as var parm parmVarIdx.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function WriteDbCompDiffsForVariedCompToProjectFile(    Project                : IProject;
                                                        newProjFileStrs        : TStringList;
                                                        dbLibDoc               : IDatabaseLibDocument;
                                                        dbTableList            : TStringList;
                                                    var dbParmsCache           : TStringList;
                                                    var dbParmsCacheIndex      : TStringList;
                                                        logCompCacheByRefDes   : TStringList;
                                                        logCompCacheByUniqueId : TStringList;
                                                        phyCompCacheByRefDes   : TStringList;
                                                        phyCompCacheByUniqueId : TStringList;
                                                        phyDesignator          : TDynamicString;
                                                        phyUniqueId            : TDynamicString;
                                                        varCompDbKey           : TDynamicString;
                                                    var parmVarIdx             : Integer;
                                                        )                      : Integer;
var
   i                  : Integer;
   j                  : Integer;
   leftStr            : TDynamicString;
   rightStr           : TDynamicString;
   logDesignator      : TDynamicString;
   logUniqueId        : TDynamicString;
   position           : Integer;
   logCompDbKey       : TDynamicString;
   logCompDbTable     : TDynamicString;
   logUniqueId2       : TDynamicString;
   linkedToDb         : Boolean;
   dbParmsStart       : Integer;
   dbParmsEnd         : Integer;
   origCompUserParms  : TStringList;
   varCompUserParms   : TStringList;
   varIdx             : Integer;
   origParmName       : TDynamicString;
   origParmValue      : TDynamicString;
   varParmValue       : TDynamicString;
   writeParmVariation : Boolean;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Output debug info. }
   WriteToDebugFile('*In WriteDbCompDiffsForVariedCompToProjectFile(), phyDesignator is "' + phyDesignator + '", phyUniqueId is "' + phyUniqueId + '", varCompDbKey is "' + varCompDbKey + '".');

   {* Try to find this physical component in the phyCompCache* lists. *}
   { Look for this physical designator in the phyCompCacheByRefDes list. }
   i := phyCompCacheByRefDes.IndexOfName(phyDesignator);

   if (i = -1) then
      MyAbort('Failed to find physical designator ' + phyDesignator + ' in phyCompCacheByRefDes!  Perhaps refdes''es have changed since variant information was created?');

   WriteToDebugFile('* Found matching entry in phyCompCacheByRefDes, entry is "' + phyCompCacheByRefDes.Strings(i) + '".');

   
   { Look for this physical unique ID in the phyCompCacheByUniqueId list. }
   j := phyCompCacheByUniqueId.IndexOfName(phyUniqueId);

   if (j = -1) then
      MyAbort('Failed to find physical unique ID ' + phyUniqueId + ' in phyCompCacheByUniqueId!');

   WriteToDebugFile('* Found matching entry in phyCompCacheByUniqueId, entry is "' + phyCompCacheByUniqueId.Strings(j) + '".');

   
   {* Extract the logical refdes associated with this part. *}
   SplitStringIntoLeftAndRight(phyCompCacheByRefDes.ValueFromIndex(i),
                               constStringDelimiter,
                               {var} leftStr,
                               {var} rightStr);

   logDesignator := leftStr;
   WriteToDebugFile('* Logical refdes is "' + logDesignator + '".');

   {* TODO:  Make sure both matches are describing the same component! *}
   

   {* Extract the logical unique ID associated with this part (the final entry after the final path separator). *}
   { Find the index of the last path separator. }
   position := LastDelimiter(constStringUniqueIdPathSep, rightStr);
   if (position = 0) then
      MyAbort('Failed to find path separator in unique ID path "' + rightStr + '"!');

   { Now extract just the logical unique ID. }
   logUniqueId := Copy(rightStr, (position+1), MaxInt);
   WriteToDebugFile('* Logical UniqueId is "' + logUniqueId + '".');

   
   {* Try to find this logical component in the logCompCache* lists. *}
   { Look for this logical designator in the logCompCacheByRefDes list. }
   i := logCompCacheByRefDes.IndexOfName(logDesignator);

   if (i = -1) then
      MyAbort('Failed to find logical designator ' + logDesignator + ' in logCompCacheByRefDes!');

   WriteToDebugFile('* Found matching entry in logCompCacheByRefDes, entry is "' + logCompCacheByRefDes.Strings(i) + '".');


   {* Split this entry into database key, database table, and unique ID fields. *}
   SplitStringIntoLeftAndRight(logCompCacheByRefDes.ValueFromIndex(i),
                               constStringDelimiter,
                               {var} leftStr,
                               {var} rightStr);

   logCompDbKey   := leftStr;

   SplitStringIntoLeftAndRight(rightStr,
                               constStringDelimiter,
                               {var} logCompDbTable,
                               {var} logUniqueId2);

   WriteToDebugFile('* logCompDbKey is "' + logCompDbKey + '", logCompDbTable is "' + logCompDbTable + '", logUniqueId2 is "' + logUniqueId2 + '".');

   
   { Make sure the logical unique ID just extracted matches what we were already expecting. }
   if (logUniqueId <> logUniqueId2) then
      MyAbort('WriteDbCompDiffsForVariedCompToProjectFile(), logUniqueId does not match logUniqueId2!');


   { Init string lists to hold database user parameters for both original and varied components. }
   origCompUserParms := TStringList.Create;
   varCompUserParms  := TStringList.Create;

   origCompUserParms.NameValueSeparator := constStringEquals;
   varCompUserParms.NameValueSeparator := constStringEquals;
   

   {* Retrieve database parameters for the varied component. *}
   GetAllParametersFromDatabase((logDesignator+'_varied'), {compRefDes,}
                                False {linkedOnlyViaDbLink},
                                dbLibDoc,
                                dbTableList,
                                varCompDbKey {compDbKey},
                                logCompDbTable {compDbTable},
                                {var} linkedToDb,
                                {var} dbParmsCache,
                                {var} dbParmsCacheIndex,
                                {var} dbParmsStart,
                                {var} dbParmsEnd);

   WriteToDebugFile('* Found varied component in database.  dbParmsStart is ' + IntToStr(dbParmsStart) + ', dbParmsEnd is ' + IntToStr(dbParmsEnd) + '.');
   
   { It only makes sense to proceed if the varied component is actually linked to a valid database component. }
   if (linkedToDb) then
   begin
      
      {* Extract just the database user parameters for the varied component. *}
      ExtractDbUserParmsForComp(dbParmsCache,
                                dbParmsStart,
                                dbParmsEnd,
                                {var} varCompUserParms {dbCompUserParms}
                                );

      {* Retrieve database parameters for the original component. *}
      GetAllParametersFromDatabase(logDesignator, {compRefDes,}
                                   False {linkedOnlyViaDbLink},
                                   dbLibDoc,
                                   dbTableList,
                                   logCompDbKey {compDbKey},
                                   logCompDbTable {compDbTable},
                                   {var} linkedToDb,
                                   {var} dbParmsCache,
                                   {var} dbParmsCacheIndex,
                                   {var} dbParmsStart,
                                   {var} dbParmsEnd);

      WriteToDebugFile('* Found original component in database.  dbParmsStart is ' + IntToStr(dbParmsStart) + ', dbParmsEnd is ' + IntToStr(dbParmsEnd) + '.');

      {* Extract just the database user parameters for the original component. *}
      ExtractDbUserParmsForComp(dbParmsCache,
                                dbParmsStart,
                                dbParmsEnd,
                                {var} origCompUserParms {dbCompUserParms}
                                );

      { Explicitly sort both sets of user parameters. }
      origCompUserParms.Sort;
      varCompUserParms.Sort;

      { Loop over all the database user parameters in the original component. }
      for i := 0 to (origCompUserParms.Count - 1) do
      begin

         { Extract the name and value of the current parameter in the original component. }
         SplitStringIntoLeftAndRight(origCompUserParms.Strings(i),
                                     constStringEquals,
                                     {var} origParmName,
                                     {var} origParmValue);

         { Try to find this user parameter name in the list of varied component parameters. }
         varIdx := varCompUserParms.IndexOfName(origParmName);

         { Flag that we don't yet have a reason that we need to create a parameter variation for this one. }
         writeParmVariation := False;
         
         { See if that user parameter name was found among the parameters of the varied component. }
         if (varIdx >= 0) then
         begin
         
            { Extract the value of the corrsponding parameter in the varied component. }
            varParmValue := varCompUserParms.ValueFromIndex(varIdx);

            { Now see if the parameter value of the original component differs from the parameter
             value of the varied component.  In general, many parameters (eg. Rating, Tolerance, etc.)
             may be the same between the original and varied components.  Others (eg. MFGNUMBER, VALUE, etc.)
             are expected to be different. }
            if (varParmValue = origParmValue) then
            begin
               WriteToDebugFile('* User parameter named "' + origParmName + '", with original value "' + origParmValue + '" is THE SAME between original and varied components.');
            end { endif }

            else
            begin
               WriteToDebugFile('* User parameter named "' + origParmName + '", with original value "' + origParmValue + '" DIFFERS compared to varied value "' + varParmValue + '".');

               { Flag that we must create a parameter variation for this one. }
               writeParmVariation := True;
               
            end; { endelse }

         end { endif }

         { Else that parameter did not exist in the varied component.
          Hopefully this is because the user wanted a resistor loaded instead of a capacitor, etc.
          Different types of components have different sets of user parameters. }
         else
         begin
            WriteToDebugFile('* Warning:  User parameter named "' + origParmName + '" was not found in varied database component!');

            { Setup for the varied component to have a null string for this parameter. }
            varParmValue := '';
            
            { Flag that we must create a parameter variation for this one. }
            writeParmVariation := True;
            
         end; { endelse }

         { See if we need to create a parameter variation for this parameter. }
         if (writeParmVariation) then
         begin

            { The Altium Variant manager GUI does an odd thing where it converts all null ('') strings to a single space (' ').
             We're going to replicate this "feature" so that said GUI doesn't feel it necessary to "fix" our project file. }
            if (varParmValue = '') then
               varParmValue := ' ';

            { If this parameter is the VALUE parameter, then we must also write a parameter variant
             to describe the Comment field.  This is because most XIA components are expected to be
             set with Comment set to "=VALUE".  But for a varied component, the "=VALUE" evaluates
             to the unvaried VALUE parameter, not the varied one.  So we must create another
             parameter variant to explicitly set Comment to VariedVALUE. }
            { Note:  For now we are assuming that we're not dealing with any of the types of
             components whose Comment field is not set to "=VALUE".  This is because I can see no
             need for any of those to be a varied database component.  JWC 2011-12-23. }
            { NOTE:  This operation is likely XIA specific! }
            if (origParmName = constDbParmValue) then
            begin

               { Write out info lines for this parmVariant. }
               { FIXME:  Currently we are adding a suffix string "(varied)" to the Comment field
                in order to get this component to appear on its own line in the XLS BOM.
                This is because the distributor stock check insists on using the non-varied
                supplier part numbers, instead of the varied ones.  This leads to 2 problems:
                1.  If this component is grouped with other components (which aren't varied), then
                all the stock check info will fail for that entire BOM line.
                2.  If this varied component is on its own BOM line, the stock check info will
                appear correct (supplier and supplier part numbers are what we want them to be),
                but the stock data returned from Digikey, Mouser, etc. will actually be the
                data from the non-varied component.

                So as a workaround we're choosing to force #2 above so that at least we don't
                screw up the BOM lookup for other components.

                This issue was reported to Altium as SupportCenter Case 00183687 and has been
                reportedly forwarded to development for fix as Ticket #5842.  JWC 2011/12/29. }
               newProjFileStrs.Add('ParamVariation' + IntToStr(parmVarIdx) + '=ParameterName=' + constDbParmComment + '|VariantValue=' + varParmValue + '(varied)');
               newProjFileStrs.Add('ParamDesignator' + IntToStr(parmVarIdx) + constStringEquals + phyDesignator);

               { Increment parameter variant index. }
               parmVarIdx  := parmVarIdx + 1;
            
            end; { endif }
            
            { Write out info lines for this parmVariant. }
            newProjFileStrs.Add('ParamVariation' + IntToStr(parmVarIdx) + '=ParameterName=' + origParmName + '|VariantValue=' + varParmValue);
            newProjFileStrs.Add('ParamDesignator' + IntToStr(parmVarIdx) + constStringEquals + phyDesignator);

            { Increment parameter variant index. }
            parmVarIdx  := parmVarIdx + 1;
            
         end; { endif }

      end; { endfor }
      
   end { endif }

   { Else the varied component does not have a valid database link.  Flag this as an error. }
   else
   begin
      WriteToDebugFile('ERROR:  There is an attempt to make a component variation for physical refdes ' + phyDesignator + ', calling out varied database component ' + varCompDbKey + '.  But this is not a valid database component!  Please fix!');
      WriteToSummaryFile('ERROR:  There is an attempt to make a component variation for physical refdes ' + phyDesignator + ', calling out varied database component ' + varCompDbKey + '.  But this is not a valid database component!  Please fix!');

   end; { endelse }
   
   { Free string lists. }
   origCompUserParms.Free;
   varCompUserParms.Free;
   
end; { end WriteDbCompDiffsForVariedCompToProjectFile() }


{***************************************************************************
 * function WriteAllVariedCompsToProjectFile()
 *  Write text lines describing all varied components to the project file.
 *
 *  Varied components will fall into one of two categories:
 *  1.  varied components that do not override the database key.  These are
 *  hopefully things like a Comment override saying "Place sticker with VarA
 *  here", etc.  For these, the script will simply write out all varied
 *  parameters that currently exist and make no changes.
 *  
 *  2.  Varied components that override the database key.  For these, we will
 *  compute from scratch the parameter differences between the original
 *  database component and the varied database component.  We will write
 *  all such parameter variations to the project file. *
 *  
 *  NOTE:  Assumes that dbParmsCache and dbParmsCacheIndex string lists have already been created.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function WriteAllVariedCompsToProjectFile(    Project                : IProject;
                                              newProjFileStrs               : TStringList;
                                              dbLibDoc               : IDatabaseLibDocument;
                                              dbTableList            : TStringList;
                                          var dbParmsCache           : TStringList;
                                          var dbParmsCacheIndex      : TStringList;
                                              logCompCacheByRefDes   : TStringList;
                                              logCompCacheByUniqueId : TStringList;
                                              phyCompCacheByRefDes   : TStringList;
                                              phyCompCacheByUniqueId : TStringList;
                                              )                      : Integer;
var
   i             : Integer;
   j             : Integer;
   k             : Integer;
   projVariant   : IProjectVariant;
   compVariant   : IComponentVariant;
   parmVariant   : IParameterVariation;
   altPart       : TDynamicString;
   parmVarIdx    : Integer;
   foundDbKey    : Boolean;
   phyDesignator : TDynamicString;
   phyUniqueId   : TDynamicString;
   varCompDbKey     : TDynamicString;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Loop over all project variations }
   for i := 0 to (Project.DM_ProjectVariantCount-1) do
   begin

      { (Re-)Initialize parameter variant index to 1 as we start with a new project variation. }
      parmVarIdx  := 1;
      
      { Retrieve a reference to this projVariant. }
      projVariant := Project.DM_ProjectVariants(i);

      { Output debug info. }
      WriteToDebugFile('* Processing project variant "' + projVariant.DM_Description + '".');
      
      { Write out header for this projVariant. }
      newProjFileStrs.Add('[ProjectVariant' + IntToStr(i+1) + ']');
      newProjFileStrs.Add('Description=' + projVariant.DM_Description);
      newProjFileStrs.Add('AllowFabrication=0');		{ Write out this hardcoded string, per example project files that I have laying aroud.  TODO:  What does this do??? }
      newProjFileStrs.Add('ParameterCount=0');		{ Write out this hardcoded string, per example project files that I have laying aroud.  TODO:  What does this do??? }
      newProjFileStrs.Add('VariationCount=' + IntToStr(projVariant.DM_VariationCount));

      { Loop over all component variations. }
      for j := 0 to (projVariant.DM_VariationCount-1) do
      begin
         
         { Retrieve a reference to this compVariant. }
         compVariant := projVariant.DM_Variations(j);

         { Output debug info. }
         WriteToDebugFile('* Processing component variant "' + compVariant.DM_PhysicalDesignator + '", UniqueId "' + compVariant.DM_UniqueId + '".');
      
         { The example project files that I have show a " " (space char) on lines where AlternatePart is null.
          I have no idea if this really matters, but I will attempt to recreate this "feature". }
         if (compVariant.DM_AlternatePart = '') then
            altPart := ' ' { nosemi }
         else
            altPart := compVariant.DM_AlternatePart;
            
         { Write out info line for this compVariant. }
         newProjFileStrs.Add('Variation' + IntToStr(j+1) + '=Designator=' + compVariant.DM_PhysicalDesignator + '|UniqueId=' + compVariant.DM_UniqueId + '|Kind=' + IntToStr(compVariant.DM_VariationKind) + '|AlternatePart=' + altPart);

         { Flag that we have not yet found an override to the database key parameter. }
         varCompDbKey := '';
         foundDbKey := False;
         
         { Loop over all parameter variations.  Look for overrides to database key field. }
         for k := 0 to (compVariant.DM_VariationCount-1) do
         begin

            { Retrieve a reference to this parmVariant. }
            parmVariant := compVariant.DM_Variations(k);

            { Output debug info. }
            WriteToDebugFile('*  k=' + IntToStr(k));
      
            { See if we found the parameter called "OLD_DB_KEY".  This is used as our interim database key. }
            if ( (parmVariant.DM_ParameterName = constDbParmDbKeyInterim) and
                (not foundDbKey) and
                (constDbParmDbKeyInterim <> '') ) then
            begin

               { We found the "OLD_DB_KEY" parameter.  Save this value for use as our interim database key, until we find "DB_KEY". }
               varCompDbKey := parmVariant.DM_VariedValue;
               WriteToDebugFile('Found "' + constDbParmDbKeyInterim + '" parameter.  Using: "' + varCompDbKey + '" as interim database key.');
            end

            { See if we found the parameter called "DB_KEY".  This is used as our real database key. }
            else if (parmVariant.DM_ParameterName = constDbParmDbKey) then
            begin

               { We found the "DB_KEY" parameter.  Save this value for use as our database key. }
               varCompDbKey := parmVariant.DM_VariedValue;
               foundDbKey := True;
               WriteToDebugFile('Found "' + constDbParmDbKey + '" parameter.  Using: "' + varCompDbKey + '" as database key.');
            end;
            
         end; { endfor k }

         { See if we found the database key being varied.
          If so, we want to (re-)compute the differences between all the database components for the unvaried part
          vs. the varied part.  (Re-)Write all these out to the project file. }
         if (varCompDbKey <> '') then
         begin

            { Record the refdes and unique ID of this varied physical component. }
            phyDesignator := compVariant.DM_PhysicalDesignator;
            phyUniqueId   := compVariant.DM_UniqueId;

            { Call WriteDbCompDiffsForVariedCompToProjectFile() to compare the unvaried vs. varied component
             and write all differences between them out to project file. }
            { Note:  We will not be attempting to compare the current set of differences to the existing
             set of differences already recorded in the existing project file and the internal data structures.
             Rather, we will re-create this set of differences from scratch. }
            WriteDbCompDiffsForVariedCompToProjectFile(Project,
                                                       newProjFileStrs,
                                                       dbLibDoc,
                                                       dbTableList,
                                                       {var} dbParmsCache,
                                                       {var} dbParmsCacheIndex,
                                                       logCompCacheByRefDes,
                                                       logCompCacheByUniqueId,
                                                       phyCompCacheByRefDes,
                                                       phyCompCacheByUniqueId,
                                                       phyDesignator,
                                                       phyUniqueId,
                                                       varCompDbKey,
                                                       {var} parmVarIdx);


         end { endif }

         { Else we did not find the database key to be varied.
          So just write out existing (presumably only "Comment" parameter) parameter variations to project file. }
         else
         begin

            { Loop over all parameter variations.  Write to project file. }
            for k := 0 to (compVariant.DM_VariationCount-1) do
            begin

               { Retrieve a reference to this parmVariant. }
               parmVariant := compVariant.DM_Variations(k);

               { Write out info lines for this parmVariant. }
               newProjFileStrs.Add('ParamVariation' + IntToStr(parmVarIdx) + '=ParameterName=' + parmVariant.DM_ParameterName + '|VariantValue=' + parmVariant.DM_VariedValue);
               newProjFileStrs.Add('ParamDesignator' + IntToStr(parmVarIdx) + constStringEquals + compVariant.DM_PhysicalDesignator);

               { Increment parameter variant index. }
               parmVarIdx  := parmVarIdx + 1;
               
            end; { endfor k }

         end; { endelse }

      end; { endfor j }      
      
      { Write out footer for this projVariant. }
      newProjFileStrs.Add('ParamVariationCount=' + IntToStr(parmVarIdx-1));
      newProjFileStrs.Add('');

   end; { endfor i }

end; { end WriteAllVariedCompsToProjectFile() }


{***************************************************************************
 * function CheckAllVariedCompsVsDatabase()
 *
 *  Here's the plan:
 *
 *  1.  Expect the designer to fire up the Project->Variants GUI.
 *  Find the component that should be loaded with another database comp.
 *  Set checkbox by "OLD_DB_KEY" or "DB_KEY" and type in OLD_DB_KEY for varied
 *  component.  For example, if one wants C1 to be C000001112 instead of
 *  C000001111, then put C000001112 as the varied value for "DB_KEY".
 *
 *  2.  Now, if that's all that's done, the BOM will still show the non-varied
 *  values for all other fields in C1 (eg. Comment, Description, MFGNUMBER, etc.).
 *  This will result in purchasing and stuffing the original, non-varied part.
 *
 *  3.  What we've been doing so far is expecting the designer to manually
 *  take a snapshot of all the component differences between C000001111 and
 *  C000001112, and creating varied parameters for all differences
 *  (eg. Comment, Description, MFGNUMBER, etc.).  This is bad for a number of reasons:
 *  (a) It's a complete pain in the a**, (b) It requires a lot of manual, error
 *  prone work, and (c) Database changes/improvements (eg. adding additional Supplier
 *  info) for C000001112 will never get propagated to schematic.
 *
 *  4.  So what we're going to do is to examine all varied components.  For those
 *  that have an override of "OLD_DB_KEY" or "DB_KEY", we're going to compare
 *  all database parameters for the original component vs. the varied component.
 *  For all such db parameters that differ, we will (re-)create varied parameters
 *  to describe all such differences.
 *
 *  5.  Now, we have the same problem we had for modifying project-level parameters,
 *  namely that Altium provides no (documented) interface for modifying or creating
 *  varied parameters.  So we will take the same approach that we took for project-
 *  level parameters:  We will re-write the project file.

 *  FIXME:  We're currently doing this in a very kludgey way, by brutally
 *  re-writing the .PrjPcb project file itself.  This is because I've been
 *  unable to figure out how to modify and/or add varied component parameters
 *  within Altium DelphiScript.
 *  This missing feature has apparently been accepted to Altium's TODO list for a
 *  future release of the tool.
 *  See http://bugcrunch.live.altium.com/#Bug/1085 and http://forum.live.altium.com/#posts/189325
 *  
 *  NOTE:  Assumes that dbParmsCache and dbParmsCacheIndex string lists have already been created.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CheckAllVariedCompsVsDatabase(    Project                : IProject;
                                           dbLibDoc               : IDatabaseLibDocument;
                                           dbTableList            : TStringList;
                                       var dbParmsCache           : TStringList;
                                       var dbParmsCacheIndex      : TStringList;
                                           logCompCacheByRefDes   : TStringList;
                                           logCompCacheByUniqueId : TStringList;
                                           phyCompCacheByRefDes   : TStringList;
                                           phyCompCacheByUniqueId : TStringList;
                                       var changedFiles           : TStringList;
                                       )                          : Integer;


var
   i               : Integer;
   k               : Integer;
   projFilePath    : TDynamicString;
   projFileStrs    : TStringList;
   newProjFileStrs : TStringList;
   state           : Integer;
   projFile        : TextFile;
   rc              : Integer;
   AServerDocument : IServerDocument;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('*In CheckAllVariedCompsVsDatabase()...');

   { Retrieve the name of the project file. }
   projFilePath := Project.DM_ProjectFullPath;
   WriteToDebugFile('*Found project file at path ' + projFilePath);
   
   {** Proceed with the kludge to read in, modify, and re-write the project file itself. **}
   { Init string list to read in project file. }
   projFileStrs := TStringList.Create;
   newProjFileStrs := TStringList.Create;

   { Read existing project file into projFileStrs. }
   projFileStrs.LoadFromFile(projFilePath);

   {* Copy existing project file to new project file, except completely rewrite the project parameters section. *}
   { Loop over all existing lines in the project file. }
   state := 0;
   for i := 0 to (projFileStrs.Count - 1) do
   begin

      { Examine current state and act appropriately. }
      case state of

        { State 0:  Project file before the project variants section. }
        { In state 0, unconditionally copy line from input to output.
         Look for a line that looks like "[ProjectVariant". When we find it, advance state. }
        0 :
           begin

              { Look for the line that signifies the start of the project variants section. }
              if (AnsiPos('[ProjectVariant', projFileStrs.Strings[i]) <> 0) then
              begin

                 { Advance state. }
                 state := 1;
                 WriteToDebugFile('*Found "[ProjectVariant" marker.  Advancing state.');

                 {* Output all project variant info to output file now. *}
                 WriteAllVariedCompsToProjectFile(Project,
                                                  newProjFileStrs,
                                                  dbLibDoc,
                                                  dbTableList,
                                                  {var} dbParmsCache,
                                                  {var} dbParmsCacheIndex,
                                                  logCompCacheByRefDes,
                                                  logCompCacheByUniqueId,
                                                  phyCompCacheByRefDes,
                                                  phyCompCacheByUniqueId);
                 
              end { endif }

              { Else we didn't find "[ProjectVariant".  Just copy this line from input to output. }
              else
              begin

                 { Unconditionally copy this line of the original project file back out to the new project file. }
                 newProjFileStrs.Add(projFileStrs.Strings[i]);

              end; { endelse }

           end; { endcase 0 }

        { State 1:  Project file during the project variants section. }
        { In state 1, ignore all lines.  Look for a line that starts with "[" and does not
         contain "[ProjectVariant".  When we find it, output current line and advance state. }
        1 :
           begin

              { Look for a line that begins with "[" and does not contain "[ProjectVariant" }
              { Note:  Leftmost index in a string for purposes of Copy() is 1! }
              if ( (Copy(projFileStrs.Strings[i], 1, 1) = '[') and (AnsiPos('[ProjectVariant', projFileStrs.Strings[i]) = 0) ) then
              begin

                 { Copy this line of the original project file back out to the new project file. }
                 newProjFileStrs.Add(projFileStrs.Strings[i]);

                 { Advance state. }
                 state := 2;
                 WriteToDebugFile('*Found "[" with no "[ProjectVariant".  Advancing state.');

              end; { endif }

           end; { endcase 1 }

        { State 2:  Project file after the project variants section. }
        { In state 2, copy all lines from input to output and remain in this state forever. }
        2 :
           begin

              { Unconditionally copy this line of the original project file back out to the new project file. }
              newProjFileStrs.Add(projFileStrs.Strings[i]);

           end; { endcase 2 }

      else MyAbort('Unknown state ' + IntToStr(state));
      end; { endcase }          
      
   end; { endfor }

   { Sanity check. }
   if (state <> 2) then
      MyAbort('Encountered problem modifying project file.  Project file may be corrupted!');

   
   { See if re-generating the component variants resulted in a project file that differs from
    the existing project file. }
   rc := DiffStringLists(projFileStrs,
                         newProjFileStrs);

   { If so, rewrite project file. }
   if (rc = 1) then
   begin

      WriteToDebugFile('*Need to re-write project file!');

      WriteToSummaryFile('INFO:     ' + 'Updated one or more varied components with respect to database.  This results in changes to project file.');

      { Add project file to the list of changed files. }
      changedFiles.Add(Project.DM_ProjectFileName);

      { Try to re-open project file for writing. }
      AssignFile(projFile, projFilePath);
      ReWrite(projFile);
      
      { Loop over all lines in newProjFileStrs. }
      for i := 0 to (newProjFileStrs.Count - 1) do
      begin

         { Write out the next line of the new project file. }
         WriteLn(projFile, newProjFileStrs.Strings(i));
         
      end; { endfor }

   end { endif }

   { Else we have no changes to make to the project file.  Don't re-write it. }
   else
   begin
      WriteToDebugFile('*No need to re-write project file.  It is unchanged.');
   end; { endelse }

   { Close project file. }
   CloseFile(projFile);
   
   { Free projFileStrs. }
   projFileStrs.Free;
   newProjFileStrs.Free;

   { We have determined experimentally that Altium will now notice when the project file
    changes.  It will automatically re-load the project file.  And it appears to be
    unhelpful to actually notify it that the project file has changed, as this causes
    it to duplicate component variants. }
   { Notify Altium that we've changed the project file contents out from under it. }
   { TODO:  Is this really doing anything?  Is there a better way to do this? }
//   AServerDocument := Client.GetDocumentByPath(projFilePath);
//   AServerDocument.DoFileLoad;
   
end; { end CheckAllVariedCompsVsDatabase() }


{***************************************************************************
 * procedure UpdateFromDatabase()
 *  Do all the actual work of the script.
 *
 *  Returns:  (nothing--procedure, not a function).
 ***************************************************************************}
procedure UpdateFromDatabase(foo : Integer);
var
   WorkSpace               : IWorkSpace;
   Project                 : IProject;
   projectPath             : TDynamicString;
   projOutPath             : TDynamicString;
   projectName             : TDynamicString;
   projLogPath             : TDynamicString;
   scriptsPath             : TDynamicString;
   document                : IDocument;
   Component               : IComponent;
   Net                     : INet;
   i                       : Integer;
   k                       : Integer;
   l                       : Integer;
   rc                      : Integer;
   changedFiles            : TStringList;
   locDbKeyName            : WideString;
   locDbParmsByRefDesCache : TStringList;
   locDbParmsByRefDesIndex : TStringList;
   locDbParmsByKeyCache    : TStringList;
   locDbParmsByKeyIndex    : TStringList;
   dbLibDoc                : IDatabaseLibDocument;
   dbTableList             : TStringList;
   dbParmsCache            : TStringList;
   dbParmsCacheIndex       : TStringList;
   logCompCacheByRefDes    : TStringList;
   logCompCacheByUniqueId  : TStringList;
   phyCompCacheByRefDes    : TStringList;
   phyCompCacheByUniqueId  : TStringList;
   timestamp               : TDynamicString;
   startTime               : TDateTime;
   endTime                 : TDateTime;

begin

   { Initialize the list of changed files. }
   changedFiles := TStringList.Create;
   changedFiles.Duplicates := dupIgnore;
   changedFiles.Sorted := True;

   { Specify that we are running the XIA_Update_From_Database script. }
   whichScriptIsThis     := constWhichScriptUfd;

   { Initialize local database related string lists. }
   locDbParmsByRefDesCache := TStringList.Create;
   locDbParmsByRefDesIndex := TStringList.Create;
   locDbParmsByKeyCache    := TStringList.Create;
   locDbParmsByKeyIndex    := TStringList.Create;

   { I have not been able to assign a different NameValueSeparator and have it actually work.  JWC 2012/01/20. }
//   locDbParmsByRefDesIndex.NameValueSeparator := '/';
//   locDbParmsByKeyIndex.NameValueSeparator := '/';

   
   {*** Run standard script initialization routine. ***}
   { Note:  This code is located in XIA_Utils.pas. }
   rc := InitScript(Workspace,
                    Project,
                    scriptsPath,
                    projectName,
                    projectPath,
                    projOutPath,
                    projLogPath);

   { Make sure init function succeeded.  If not, we have a _serious_ problem and we need to Exit; now. }
   if (rc <> 0) then
      Exit;

   {****** Initialize script. ******}
   { These flags are not actually used in this script, but set them to True to keep other code happy. }
   enableGenerateOutputs := True;
   enableSvnCommits      := True;


   { Open debug file. }
   OpenDebugFile((projectPath + constThisScriptNameNoExt + '_Debug.txt'));

   { Record the wall clock time when we ended this script. }
   startTime := Now();
   WriteToDebugFile('**Script ' + constThisScriptName + ' started at ' + DateTimeToStr(Date) + ' ' + TimeToStr(startTime));
   WriteToDebugFile('Project : ' +  Project.DM_ProjectFileName);

   { Open summary file. }
   OpenSummaryFile((projectPath + constThisScriptNameNoExt + '_Report.txt'));
   WriteToSummaryFile('Actions performed by this script:');
   WriteToSummaryFile('');

   { Report if script is being run with "sync footprints" unchecked. }
   if (not enableFootprintUpdates) then
      WriteToSummaryFile('ERROR:    This script is being run with database-to-schematic footprint sync disabled!!  Hopefully you have a good reason for doing this.');

   
   { Compile project before proceeding. }
   UfdUpdateGuiStatusMessage('Status:  Compiling project.');
   Project.DM_Compile;


   {** Verify that scripts and libraries global working copy is up-to-date. **}
   { Note:  Comment out this call if you don't want any dependence on external svn commands. }
   CheckThatSvnScriptsWorkingCopyIsUpdated(scriptsPath,
                                           constThisScriptName);
   

   { Record the wall clock time when we started the real work of this script. }
   startTime := Now();
   
   {** Initialize local database caches. **}
   locDbParmsByRefDesCache   := TStringList.Create;
   locDbParmsByRefDesIndex   := TStringList.Create;
   locDbParmsByKeyCache      := TStringList.Create;
   locDbParmsByKeyIndex      := TStringList.Create;

   
   {** Initialize database info. **}
   InitDatabaseInfo(Project,
                    projectName,
                    projectPath,
                    {var} locDbKeyName,
                    {var} locDbParmsByRefDesCache,
                    {var} locDbParmsByRefDesIndex,
                    {var} locDbParmsByKeyCache,
                    {var} locDbParmsByKeyIndex,
                    {var} dbLibDoc,
                    {var} dbTableList,
                    {var} changedFiles);
   

   {** Initialize database cache. **}
   dbParmsCache      := TStringList.Create;
   dbParmsCacheIndex := TStringList.Create;

   {** Initialize caches of logical components in the design. **}
   logCompCacheByRefDes   := TStringList.Create;
   logCompCacheByUniqueId := TStringList.Create;

   logCompCacheByRefDes.Sorted := True;
   logCompCacheByRefDes.Duplicates := dupError;
   logCompCacheByRefDes.NameValueSeparator := constStringEquals;

   logCompCacheByUniqueId.Sorted := True;
   logCompCacheByUniqueId.Duplicates := dupError;
   logCompCacheByUniqueId.NameValueSeparator := constStringEquals;

   
   {** Initialize caches of physical components in the design. **}
   phyCompCacheByRefDes   := TStringList.Create;
   phyCompCacheByUniqueId := TStringList.Create;
   
   phyCompCacheByRefDes.Sorted := True;
   phyCompCacheByRefDes.Duplicates := dupError;
   phyCompCacheByRefDes.NameValueSeparator := constStringEquals;

   phyCompCacheByUniqueId.Sorted := True;
   phyCompCacheByUniqueId.Duplicates := dupError;
   phyCompCacheByUniqueId.NameValueSeparator := constStringEquals;
   
   {** Loop over all logical documents in the project.... **}
   for k := 0 to Project.DM_LogicalDocumentCount - 1 do
   begin

      { Retrieve a reference to next logical document. }
      document := Project.DM_LogicalDocuments(k);
      WriteToDebugFile('Examining project document ' + document.DM_FullPath);
      
      { See if this document is a schematic page. }
      if (document.DM_DocumentKind = constKindSch) then
      begin
         
         UfdUpdateGuiStatusMessage('Status:  Checking components in schematic page ' + document.DM_FullPath);

         {* Check all components in this schematic page with respect to database. *}
         { Extract component parameter information from all components in this schematic page.
          Then proceed to check against the database entry for each component. }
         CheckSchComponentsVsDatabase(document,
                                      locDbKeyName,
                                      locDbParmsByRefDesCache,
                                      locDbParmsByRefDesIndex,
                                      locDbParmsByKeyCache,
                                      locDbParmsByKeyIndex,
                                      dbLibDoc,
                                      dbTableList,
                                      {var} dbParmsCache,
                                      {var} dbParmsCacheIndex,
                                      {var} logCompCacheByRefDes,
                                      {var} logCompCacheByUniqueId,
                                      {var} changedFiles);

      end; { endif }

   end; { endfor loop over all logical documents in project }


   {** Analyze and cache physical component to logical component mappings. *}
   CachePhysicalComponentInfo(Project,
                              {var} phyCompCacheByRefDes,
                              {var} phyCompCacheByUniqueId);
   
   {** Check all varied components with respect to database. **}
   CheckAllVariedCompsVsDatabase(Project,
                                 dbLibDoc,
                                 dbTableList,
                                 {var} dbParmsCache,
                                 {var} dbParmsCacheIndex,
                                 logCompCacheByRefDes,
                                 logCompCacheByUniqueId,
                                 phyCompCacheByRefDes,
                                 phyCompCacheByUniqueId,
                                 {var} changedFiles);

   
   { See if we have any changed files. }
   if (changedFiles.Count > 0) then
      begin

         WriteToSummaryFile('');
         WriteToSummaryFile('These files were changed.  Please save them and check them in:  ');
         
         { Loop over all the files and directories returned to us. }
         for i := 0 to changedFiles.Count - 1 do
         begin
            WriteToSummaryFile(' ' + changedFiles.Strings[i]);
            
         end; { endfor }

         WriteToSummaryFile('');
         WriteToSummaryFile('You still need to:');
         WriteToSummaryFile('1.  Save and checkin saved files.');
         WriteToSummaryFile('2.  Push changes from schematic to layout.');
         
      end { endif }

   { Else we have no changed files. }
      else
      begin

         WriteToSummaryFile('');
         WriteToSummaryFile('No files were changed.  All schematic components were already synchronized to database (or else the mismatches need to be fixed manually).');

      end;


   { Free local database caches. }
   locDbParmsByRefDesCache.Free;
   locDbParmsByRefDesIndex.Free;
   locDbParmsByKeyCache.Free;
   locDbParmsByKeyIndex.Free;

   { Free list of database tables. }
   dbTableList.Free;

   { Free database cache. }
   dbParmsCache.Free;
   dbParmsCacheIndex.Free;

   { Free caches of logical components. }
   logCompCacheByRefDes.Free;
   logCompCacheByUniqueId.Free;
   
   { Free caches of physical components. }
   phyCompCacheByRefDes.Free;
   phyCompCacheByUniqueId.Free;
   
   
   { Record the wall clock time when we ended this script. }
   endTime := Now();
   
   { Timestamp the end of our actions, before we present the last dialog box to the user. }
   WriteToDebugFile('');
   WriteToDebugFile('**Script ' + constThisScriptName + ' ending at ' + DateTimeToStr(Date) + ' ' + TimeToStr(endTime));
   WriteToDebugFile('**Script took ' + FormatDateTime('h:n:s', (endTime-startTime)) + ' (hrs:mins:secs) to run on this project on this PC.');

   
   { Write a copy of our summary file to the ProjectLogs directory, using a filename with timestamp embedded in it. }
   { Attempt to use international-ish time/date format year-month-date_hour_minute_seconds. }
   timestamp := FormatDateTime('yyyy-mm-dd_hh_nn_ss', Now());
   SummaryMessages.SaveToFile(projLogPath + '\' + constThisScriptNameNoExt + '_' + timestamp + '.LOG');
   
   
   {****** Wrap things up ******}

   UfdUpdateGuiStatusMessage('Status:  Done checking all components.');

   { If there are <= 55 lines in the summary messages list, then AtExit() will just create
    a ShowMessage() dialog box and we no longer need the custom dialog box we already have up. }
   if (SummaryMessages.Count <= constMaxLinesInShowMessage) then
   begin
      ModalResult := mrOK;
      UpdateFromDatabaseForm.Close;
   end;
      
      
   { Call AtExit() procedure to write debug outputs to file. }
   WriteToDebugFile('**About to exit script.');
//   ShowMessage('About to call AtExit()');
   AtExit(0);                   { Report success at exit }
//   ShowMessage('Back from AtExit()');
   
end;


{***************************************************************************
 * procedure TUpdateFromDatabaseForm.clickedOk()
 *  This is the handler for primary dialog box "OK" click.
 ***************************************************************************}
procedure TUpdateFromDatabaseForm.clickedOk(Sender : TPanel);

begin

//   ShowMessage('Hello world from TUpdateFromDatabaseForm.clickedOk()');

   { Figure out if we got here from the initial Ok click to start the script or
    the final Ok click to end the script. }
   
   { See if this is the initial Ok click. }
   if (UpdateFromDatabaseForm.formButtonOk.Left <> 450) then
   begin
   
      { Tell the user that we are proceeding to run script. }
      formButtonsLabel1.Caption := 'OK.  Proceeding to run script.';
      formButtonsLabel1.Update;

      { Disable (grey out) OK and Cancel buttons on primary form. }
      formButtonOk.Enabled := False;
      formButtonOk.Update;

      formButtonCancel.Enabled := False;
      formButtonCancel.Update;

      { Call UpdateFromDatabase() to do all the actual work. }
      UpdateFromDatabase(99);

   end

   { Else this is the final ok to end the script.
    Close the modal window. }
   else
   begin
      ModalResult := mrOK;
      UpdateFromDatabaseForm.Close;
   end;
   
   { Return to caller. }
   Exit;

end; { end TUpdateFromDatabaseForm.clickedOk() }


{***************************************************************************
 * procedure TUpdateFromDatabaseForm.clickedCancel()
 *  This is the handler for primary dialog box "Cancel" click.
 ***************************************************************************}
procedure TUpdateFromDatabaseForm.clickedCancel(Sender : TPanel);

var
   Action : TCloseAction;
        
begin

   { Close dialog box. }
   ModalResult := mrCancel;
   UpdateFromDatabaseForm.Close;

   ShowError('Script canceled at User request.');

   { Exit script now. }
   Exit;
   
end; { end TUpdateFromDatabaseForm.clickedCancel() }


{***************************************************************************
 * procedure TUpdateFromDatabaseForm.bCheck0_1()
 *  This is the handler for checking/unchecking "Sync footprints" checkbox.
 ***************************************************************************}
procedure TUpdateFromDatabaseForm.bCheck0_1(Sender : TPanel);
begin

   { See if the user just unchecked this box. }
   if (formCbFlagOp0_1.Checked = False) then
   begin

      { Disable footprint sync. }
      enableFootprintUpdates := False;

   end { endif }

   { Else the user re-checked this box. }
   else
   begin

      { Enable footprint sync. }
      enableFootprintUpdates := True;

   end; { endelse }
      
end; { end TUpdateFromDatabaseForm.bCheck0_1() }


{***************************************************************************
 * procedure XIA_Update_From_Database()
 *  Script entry point.
 *
 *  Returns:  (nothing--procedure, not a function).
 ***************************************************************************}
procedure XIA_Update_From_Database;

//var

begin

   { Override GUI text entries. }
   { Note:  Don't condense this onto fewer lines or you will have svn replacing $Revision.*$
    with svn rev number on checkin! }
   UpdateFromDatabaseForm.Caption := 'Welcome to ' + constThisScriptNameNoExt + ' ' +
   StringReplace(StringReplace(constScriptVersion, '$Revision:', ', svn rev', ''),
                 ' $', '', '') + ' script main menu!';
 
   { Override GUI text entries. }
   UpdateFromDatabaseForm.formText01.Caption := 'You have launched script ' + constThisScriptName + '.';
   UpdateFromDatabaseForm.formText01.Font.Style := MkSet(fsBold);
   UpdateFromDatabaseForm.formText02.Caption := '';

   UpdateFromDatabaseForm.formText03.Caption := 'This script is intended as a replacement for the 3 update-against-database steps that we have previously done:';
   UpdateFromDatabaseForm.formText03.Font.Style := MkSet(fsBold);
   UpdateFromDatabaseForm.formText04.Caption := '1.  Update user parameters with respect to database (Update Parameters from Database operation).';
   UpdateFromDatabaseForm.formText05.Caption := '2.  Update system parameters (footprints) with respect to database (Update from Libraries operation).';
   UpdateFromDatabaseForm.formText06.Caption := '3.  Make sure that most components have a Comment parameter set to "' + constDbValCommentStd + '" (former UpdateComments script).';
   UpdateFromDatabaseForm.formText07.Caption := '';

   UpdateFromDatabaseForm.formText08.Caption := 'This script will also:';
   UpdateFromDatabaseForm.formText08.Font.Style := MkSet(fsBold);
   UpdateFromDatabaseForm.formText09.Caption := '4.  Synchronize Description fields between database and schematic components.';
   UpdateFromDatabaseForm.formText10.Caption := '5.  Check and if necessary, fix path to DBLib file.';
   UpdateFromDatabaseForm.formText11.Caption := '6.  Check and if necessary, fix path to DBLink file.';
   UpdateFromDatabaseForm.formText12.Caption := '7.  Update database link on any components still pointing to old DBLib files "' + constOldDbLib1FileName + '" or "' +  '"' + constOldDbLib2FileName + '".';
   UpdateFromDatabaseForm.formText13.Caption := '8.  Check that the schematic symbol in use on schematic pages matches the one specified by the database.  If not, don''t fix anything, but report the error to user.';
   UpdateFromDatabaseForm.formText14.Caption := '9.  Synchronize varied database components with respect to database.';
   UpdateFromDatabaseForm.formText15.Caption := '10.  If enabled, restore/maintain "legacy/local" database parameters as function of logical refdes.';
   UpdateFromDatabaseForm.formText16.Caption := '11.  If enabled, restore/maintain a set of "legacy/local" database properties as a function of local database key.';
   UpdateFromDatabaseForm.formText17.Caption := '';

   UpdateFromDatabaseForm.formText18.Caption := 'Preconditions:';
   UpdateFromDatabaseForm.formText18.Font.Style := MkSet(fsBold);
   UpdateFromDatabaseForm.formText19.Caption := '1.  All project files should have been saved prior to running this script.';
   UpdateFromDatabaseForm.formText20.Caption := '';
   
   UpdateFromDatabaseForm.formText21.Caption := 'Notes:';
   UpdateFromDatabaseForm.formText21.Font.Style := MkSet(fsBold);
   UpdateFromDatabaseForm.formText22.Caption := '1.  This will take a minute or two to run.';
   UpdateFromDatabaseForm.formText23.Caption := '2.  You''ll need to save modified files after I''m done.';
   UpdateFromDatabaseForm.formText24.Caption := '3.  This script only synchronizes the schematic components to the database.';
   UpdateFromDatabaseForm.formText25.Caption := '4.  Thus, you will still need to push changes from schematic to layout!';
   UpdateFromDatabaseForm.formText26.Caption := '';
   
   UpdateFromDatabaseForm.formText27.Caption := '';
   UpdateFromDatabaseForm.formText28.Caption := '';
   UpdateFromDatabaseForm.formText29.Caption := '';
   
   UpdateFromDatabaseForm.formText30.Caption := 'Options:';
   UpdateFromDatabaseForm.formText30.Font.Style := MkSet(fsBold);
   
   UpdateFromDatabaseForm.formButtonsLabel1.Caption := 'Shall I run (OK), or shall I Cancel running this script?';
   UpdateFromDatabaseForm.formButtonsLabel1.Font.Style := MkSet(fsBold);

   { Override GUI check box labels. }
   formCbFlagOp0_1.Caption := 'Sync footprints from database to schematic.  (Only uncheck for legacy designs using old Orcad footprints!)';

   { Choose which checkboxes are checked on startup. }
   formCbFlagOp0_1.Checked := True; { 'Enable database-to-schematic footprint sync' }

   { Set initial status message. }
   UpdateFromDatabaseForm.formStatusBar1.SimpleText := 'Status:  Awaiting user permission to run script.';

   { Flag that we are allowed to update footprints. }
   enableFootprintUpdates := True;

   
   { Run GUI dialog box asking user for permission to run. }
   UpdateFromDatabaseForm.ShowModal;

   { Note:  Control now passes to one of the handler functions above. }
   
end; { end XIA_Update_From_Database() }


end.
