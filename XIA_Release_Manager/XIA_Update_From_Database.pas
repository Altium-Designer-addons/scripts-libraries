{***************************************************************************
 XIA_Update_From_Database.pas
 Altium DelphiScript (basically Pascal) that will:
 1.  Update user parameters with respect to database (Update Parameters from Database operation).
 2.  Update system parameters (footprints) with respect to database (Update from Libraries operation).
 3.  Make sure that most components have a Comment parameter set to "=VALUE" (former UpdateComments script).
 4.  Check, and if necessary, fix path to DBLib file.
 5.  Check, and if necessary, fix path to DBLink file.
 6.  Update database link on any components still pointing to old DBLib file Old_database_library_file.DBLib.
 
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
 *  1.  This script requires functions and constants defined in XIA_Release_Manager.pas.
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
 *  4.  Check, and if necessary, fix path to DBLib file.
 *  5.  Check, and if necessary, fix path to DBLink file.
 *  6.  Update database link on any components still pointing to old DBLib file Old_database_library_file.DBLib.
 
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
 *  'Current_database_library_file.DBLib'.  This script will verify that the standard DBLib file
 *  is part of the project.  
 *  See InitDatabaseInfo().
 *
 *  3.  XIA's current DBLib file is 'Current_database_library_file.DBLib'.
 *  3a.  When migrating older designs that were linked to the older 'Old_database_library_file.DBLib'
 *  file, it is necessary to append a "$" char to the database table name.
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

   formButtonOk: TButton;                 
   formButtonCancel: TButton;             
   formButtonsLabel1 :TLabel;         
   formStatusBar1: TXStatusBar;   
   procedure TUpdateFromDatabaseForm.clickedOk(Sender : TPanel);
   procedure TUpdateFromDatabaseForm.clickedCancel(Sender : TPanel);
end;


{***************************************************************************
 * Global constants.
 ***************************************************************************}
const
{* Declare the version and name of this script. *}
   constScriptVersion          = 'v1.9.8_gc $Revision$';
   constThisScriptNameNoExt    = 'XIA_Update_From_Database';
   constThisScriptName         = constThisScriptNameNoExt + '.pas';

{ Note:  We implicitly rely on a number of constants defined in XIA_Release_Manager.pas.
 That script and this one must both be part of an Altium script project!
 That way, we can use constants and functions defined in the other script. }
   

{***************************************************************************
 * Global variables.  Highly evil.  Ick ick.
 ***************************************************************************}
var
   UpdateFromDatabaseForm : TUpdateFromDatabaseForm;


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

   { Transform existing GUI dialog box so that there is a big list box available. }
   UpdateFromDatabaseForm.listBox1.Left := 14;
   UpdateFromDatabaseForm.listBox1.Top := 40;
   UpdateFromDatabaseForm.listBox1.Width := 972;
   UpdateFromDatabaseForm.listBox1.Height := 740;

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
 * function SplitDelimitedString()
 *  Split apart a delimited string.  We can't use the builtin TStringList
 *  function for this because it will also split on spaces, which we don't want.
 *
 *  Returns string list in var parm list.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function SplitDelimitedString(    delimString : TDynamicString;
                                  delimiter   : String;
                              var list        : TStringList;
                                  )           : Integer;

var
   position  : Integer;
   leftStr   : TDynamicString;
   rightStr  : TDynamicString;
   
begin

   { Assume success. }
   result := 0;

   { Initialize loop. }
   rightStr := delimString;

   { Loop until we run out of delimiters. }
   { Here we assume that there is not a delimiter at the end of the delimString. }
   while (AnsiPos(delimiter, rightStr) <> 0) do
   begin

      { Find the position of the next delimiter character. }
      position := AnsiPos(delimiter, rightStr);
//    WriteToDebugFile('rightStr is "' + rightStr + '", position is ' + IntToStr(position));

      { The left string is everything up until the char before the delimiter. }
      leftStr := Copy(rightStr, 0, (position-1));

      { Add the left string to our running string list. }
      list.Add(leftStr);
      WriteToDebugFile(' Adding substring "' + leftStr + '".');

      { The new right string is everything after the delimiter char. }
      rightStr := Copy(rightStr, (position+1), MaxInt);
//    WriteToDebugFile('rightStr is "' + rightStr + '".');

   end; { endwhile }

   { Add the last string to the string list. }
   list.Add(rightStr);
   WriteToDebugFile(' Adding substring "' + rightStr + '".');

end; { end SplitDelimitedString() }


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
function InitDatabaseInfo(    Project      : IProject;
                          var dbLibDoc     : IDatabaseLibDocument;
                          var dbTableList  : TStringList;
                          var changedFiles : TStringList;
                              )            : Integer;

var
   k                  : Integer;
   document           : IDocument;
   tableIndex         : Integer;
   dbTableName        : TDynamicString;
   hasDbLink          : Boolean;
   hasCorrectDbLink   : Boolean;
   
begin

   { Assume success. }
   result := 0;

   {* Verify that correct DBLib file is attached to project. *}
   { Attempt to open DBLib file, with specifying full path. }
   dbLibDoc := IntegratedLibraryManager.GetAvailableDBLibDocAtPath(constRequiredDbLibFilePath);

   { Sanity check. }
   if (dbLibDoc = Nil) then
   begin

      { Update status message. }
      UfdUpdateGuiStatusMessage('Status:  Attempting to fix problem with DBLib file attached to project.');
      
      {** Loop over all logical documents in the project (counting backwards). **}
      for k := (Project.DM_LogicalDocumentCount - 1) downto 0 do
      begin
         document := Project.DM_LogicalDocuments(k);
         WriteToDebugFile('Examining project document ' + document.DM_FullPath);
         
         { See if this document is a DBLib file. }
         if (document.DM_DocumentKind = constKindDbLib) then
         begin

//          ShowMessage('Found DBLib file at ' + document.DM_FullPath);

            { Attempt to remove this DBLib file (presumably our
             Current_database_library_file.DBLib file, but with a funky or relative path). }
            WriteToDebugFile('Removing DBLib file "' + document.DM_FullPath + '" from project.');
            WriteToSummaryFile('INFO:     ' + 'Removing DBLib file "' + document.DM_FullPath + '" from project.');
            Project.DM_RemoveSourceDocument(document.DM_FullPath);

         end; { endif }

      end; { endfor }

      { Attempt to add our Current_database_library_file.DBLib file, with the proper path. }
      Project.DM_AddSourceDocument(constRequiredDbLibFilePath);
      WriteToDebugFile('Added required DBLib file "' + constRequiredDbLibFilePath + '" to project.');
      WriteToSummaryFile('INFO:     ' + 'Added required DBLib file "' + constRequiredDbLibFilePath + '" to project.');

      { Attempt one more time to open DBLib file. }
      dbLibDoc := IntegratedLibraryManager.GetAvailableDBLibDocAtPath(constRequiredDbLibFilePath);
      
      { Sanity check. }
      if (dbLibDoc = Nil) then
         MyAbort('Still unable to open required DBLib file "' + constRequiredDbLibFilePath + '"!');

      { Add project file to the list of changed files. }
      changedFiles.Add(Project.DM_ProjectFileName);
      
   end; { endif }


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

end; { end InitDatabaseInfo() }


{***************************************************************************
 * function AddSchParm()
 *  Add a new schematic component parameter, given its name and new value.
 *
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function AddSchParm(    document      : IDocument,
                        component     : ISch_Component;
                        dbParmName    : TDynamicString;
                        dbParmValue   : TDynamicString;
                        dbParmVisible : TDynamicString;
                    var changedFiles  : TStringList;
                        )             : Integer;

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
function UpdateSchModel(    document     : IDocument,
                            component    : ISch_Component;
                            dbParmName   : TDynamicString;
                            dbParmValue  : TDynamicString;
                        var changedFiles : TStringList;
                            )            : Integer;

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
function UpdateSchParmValueByName(    document     : IDocument,
                                      component    : ISch_Component;
                                      dbParmName   : TDynamicString;
                                      dbParmValue  : TDynamicString;
                                  var changedFiles : TStringList;
                                      )            : Integer;
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

   { See if we need to update from Old_database_library_file.DBLib to Current_database_library_file.DBLib. }
   if ( (constOldDbLibFileName <> '') and (component.DatabaseLibraryName = constOldDbLibFileName) ) then
   begin

      { Set new database library name to be 'Current_database_library_file.DBLib'. }
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
   
end; { end CheckAndCorrectDatabaseLibraryName() }
   

{***************************************************************************
 * function GetUserParametersFromComp()
 *  Get all user parameters for a given schematic component.
 *  In the process, attempt to identify the database key.
 *
 *  Returns flag whether comp is only linked via DBLink file as var parm linkedOnlyViaDbLink.
 *  Returns database key for this comp as var parm compDbKey.
 *  Returns database table for this comp as var parm compDbTable.
 *  Returns component's user parameters as var parm compUserParmsList.
 *  Returns possibly modified list of changed files as var parm changedFiles.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function GetUserParametersFromComp(    document            : IDocument,
                                       component           : ISch_Component;
                                   var linkedOnlyViaDbLink : Boolean;
                                   var compDbKey           : WideString;
                                   var compDbTable         : WideString;
                                   var compUserParmsList   : TStringList;
                                   var changedFiles        : TStringList;
                                       )                   : Integer;

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

   { Attempt to extract database related info. }
   WriteToDebugFile('DesignItemID       ="' + component.DesignItemID + '"');
   WriteToDebugFile('LibReference       ="' + component.LibReference + '"');
   WriteToDebugFile('DatabaseLibraryName="' + component.DatabaseLibraryName + '"');
   WriteToDebugFile('DatabaseTableName  ="' + component.DatabaseTableName + '"');

   { Return database table name to caller. }
   compDbTable := component.DatabaseTableName;

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
         
         { Advance to next parameter in this schematic component. }
         Parameter := PIterator.NextSchObject;

      end;

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
      WriteToDebugFile('DesignItemID is "' + component.DesignItemID + '", compDbKey is "' + compDbKey + '".');

      WriteToDebugFile('WARNING:  ' + component.Designator.Name + ' ' + component.Designator.Text + ':  This component is linked using only DBLink file!!  Sch symbol may differ and DB lookups are slower!');
      WriteToSummaryFile('WARNING:  ' + component.Designator.Name + ' ' + component.Designator.Text + ':  This component is linked using only DBLink file!!  Sch symbol may differ and DB lookups are slower!');
      
   end; { endif }

end; { end GetUserParametersFromComp() }


{***************************************************************************
 * function CheckUserParametersForComp()
 *  Check all user parameters for a given schematic component.
 *  Currently this involves making sure that (with a few specific exceptions),
 *  that the "Comment" parameter is set to "=VALUE".
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
                    dbParmName,
                    dbParmValue,
                    dbParmVisible,
                    {var} changedFiles);
         
      end; { endelse }

   end; { endelse }
   
end; { end CheckDbVsCompParmMatch() }


{***************************************************************************
 * function GetAllParametersFromDatabaseTable()
 *  Look for a component in a particular database table.
 *  If found, get all system and user parameters for a given database entry.
 *
 *  Returns whether this component was found in the specified db dable as var parm foundInThisDbTable.
 *  Returns cache of database parameters as var parm dbParmsCache.
 *  Returns index into database cache as var parm dbParmsCacheIndex.
 *  Returns limits of dbParmsCache as var parms dbParmsStart and dbParmsEnd.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function GetAllParametersFromDatabaseTable(    document           : IDocument;
                                               component          : ISch_Component;
                                               dbLibDoc           : IDatabaseLibDocument;
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
      SplitDelimitedString(dbParmsStr,
                           constStringDelimiter,
                           dbParmsCache);

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
 *  Returns whether this component was found in the specified db dable as var parm foundInThisDbTable.
 *  Returns cache of database parameters as var parm dbParmsCache.
 *  Returns index into database cache as var parm dbParmsCacheIndex.
 *  Returns limits of dbParmsCache as var parms dbParmsStart and dbParmsEnd.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function GetAllParametersFromDatabase(    document          : IDocument;
                                          component         : ISch_Component;
                                          dbLibDoc          : IDatabaseLibDocument;
                                          dbTableList       : TStringList;
                                          compDbKey         : WideString;
                                          compDbTable       : WideString;
                                      var linkedToDb        : Boolean;
                                      var dbParmsCache      : TStringList;
                                      var dbParmsCacheIndex : TStringList;
                                      var dbParmsStart      : Integer;
                                      var dbParmsEnd        : Integer;
                                          )                 : Integer;

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
   if (compDbKey <> '') then
   begin

      { Search database cache string list. }
      WriteToDebugFile('Searching for compDbKey "' + compDbKey + '" in dbParmsCacheIndex.');
      cacheIndex := dbParmsCacheIndex.IndexOfName(compDbKey);

      { See if we got a hit. }
      if (cacheIndex >= 0) then
      begin

         { Extract the right side of this entry in the cache index.
          This contains the start and end indices into the cache itself. }
         { Note:  Entry in cache index will look something like "C000010001=33|75". }
         SplitStringIntoLeftAndRightWithAbort(dbParmsCacheIndex.Strings[cacheIndex],
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
         WriteToDebugFile('Found compDbKey "' + compDbKey + '" in dbParmsCache.  dbParmsStart is ' + IntToStr(dbParmsStart) + ', dbParmsEnd is ' + IntToStr(dbParmsEnd) + '.');

      end; { endif got cache hit }

   end; { endif compDbKey <> '' }
   

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
         
         WriteToDebugFile('ERROR:    ' + 'Designator ' + component.Designator.Text + ':  compDbTable "' + compDbTable + '" not found in list of db tables.  Please fix db table name!!');
         WriteToSummaryFile('ERROR:    ' + 'Designator ' + component.Designator.Text + ':  compDbTable "' + compDbTable + '" not found in list of db tables.  Please fix db table name!!');

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
         GetAllParametersFromDatabaseTable(document,
                                           component,
                                           dbLibDoc,
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
               GetAllParametersFromDatabaseTable(document,
                                                 component,
                                                 dbLibDoc,
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
      WriteToDebugFile('ERROR:    ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Could not find database link for this component!  Please fix!!!');
      WriteToSummaryFile('ERROR:    ' + component.Designator.Name + ' ' + component.Designator.Text + ':  Could not find database link for this component!  Please fix!!!');

   end; { endif }
      
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
                                          dbLibDoc          : IDatabaseLibDocument;
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
   leftStr       : TDynamicString;
   rightStr      : TDynamicString;
   dbParmName    : TDynamicString;
   dbParmValue   : TDynamicString;
   dbParmVisible : TDynamicString;
   dbDescription : TDynamicString;

begin

   { Assume success. }
   result := 0;

   { Init number of footprints allowed by database record to be 0. }
   dbNumFootprints := 0;

   { Loop over all database parameters. }
   WriteToDebugFile('About to loop over all database parameters...');
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
         
      end { elsif }
      
      { Look for left string to start with 'ParameterVisible' and if so, check component vs. database entry. }
      else if (AnsiPos(constDbParmParameterVisible, leftStr) <> 0) then
      begin
         dbParmVisible := rightStr;
         //                  WriteToDebugFile('dbParmVisible is "' + dbParmVisible + '".');

         { Check that the database parameter matches the component level parameter. }
         CheckDbVsCompParmMatch(document,
                                component,
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
   i             : Integer;
   leftStr       : TDynamicString;
   rightStr      : TDynamicString;
   dbParmName    : TDynamicString;
   dbParmValue   : TDynamicString;
   dbParmVisible : TDynamicString;
   
begin

   { Assume success. }
   result := 0;

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
 * function CheckComponentsVsDatabase()
 *  Attempt to retrieve user and system parameters for all components in a sch page.
 *  Then proceed to check these parameters against the database entry for each comp.
 *
 *  Returns cache of database parameters as var parm dbParmsCache.
 *  Returns index into database cache as var parm dbParmsCacheIndex.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CheckComponentsVsDatabase(    dbLibDoc          : IDatabaseLibDocument;
                                       dbTableList       : TStringList;
                                       document          : IDocument,
                                   var dbParmsCache      : TStringList;
                                   var dbParmsCacheIndex : TStringList;
                                   var changedFiles      : TStringList;
                                       )                 : Integer;
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
                                      {var} linkedOnlyViaDbLink,
                                      {var} compDbKey,
                                      {var} compDbTable,
                                      {var} compUserParmsList,
                                      {var} changedFiles);

            
            {** Check user parameters for this component. **}
            CheckUserParametersForComp(document,
                                       component,
                                       compUserParmsList,
                                       {var} changedFiles);
            

            {** Get all database parameters for this component. **}
            GetAllParametersFromDatabase(document,
                                         component,
                                         dbLibDoc,
                                         dbTableList,
                                         compDbKey,
                                         compDbTable,
                                         {var} linkedToDb,
                                         {var} dbParmsCache,
                                         {var} dbParmsCacheIndex,
                                         {var} dbParmsStart,
                                         {var} dbParmsEnd);

            
            { Make sure component is successfully linked to database. }
            if (linkedToDb) then
            begin
            
               {** Check all parameters for this component compared to what the database says they should be. **}
               CheckAllParametersVsDatabase(document,
                                            component,
                                            dbLibDoc,
                                            dbParmsCache,
                                            dbParmsStart,
                                            dbParmsEnd,
                                            compSysParmsList,
                                            compUserParmsList,
                                            {var} dbNumFootprints,
                                            {var} changedFiles);

               
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

            end; { endif }
            
            { Clear list of parameters currently attached to this component. }
            compSysParmsList.Clear;
            compUserParmsList.Clear;
            
         end; { endif }
            
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
   
end; { end CheckComponentsVsDatabase() }


{***************************************************************************
 * procedure UpdateFromDatabase()
 *  Do all the actual work of the script.
 *
 *  Returns:  (nothing--procedure, not a function).
 ***************************************************************************}
procedure UpdateFromDatabase(foo : Integer);
var
   WorkSpace         : IWorkSpace;
   Project           : IProject;
   projectPath       : TDynamicString;
   projOutPath       : TDynamicString;
   projectName       : TDynamicString;
   projLogPath       : TDynamicString;
   scriptsPath       : TDynamicString;
   document          : IDocument;
   Component         : IComponent;
   Net               : INet;
   i                 : Integer;
   k                 : Integer;
   l                 : Integer;
   rc                : Integer;
   changedFiles      : TStringList;
   dbLibDoc          : IDatabaseLibDocument;
   dbTableList       : TStringList;
   dbParmsCache      : TStringList;
   dbParmsCacheIndex : TStringList;
   timestamp         : TDynamicString;
   startTime         : TDateTime;
   endTime           : TDateTime;

begin

   { Initialize the list of changed files. }
   changedFiles := TStringList.Create;
   changedFiles.Duplicates := dupIgnore;
   changedFiles.Sorted := True;

   { Specify that we are running the XIA_Update_From_Database script. }
   whichScriptIsThis     := constWhichScriptUfd;


   {*** Run standard script initialization routine. ***}
   { Note:  This code is located in XIA_Release_Manager.pas. }
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
   WriteToDebugFile('**Script ' + constThisScriptName + ' started at ' + DateTimeToStr(Date) + ' ' + TimeToStr(startTime));
   WriteToDebugFile('Project : ' +  Project.DM_ProjectFileName);

   { Open summary file. }
   OpenSummaryFile((projectPath + constThisScriptNameNoExt + '_Report.txt'));
   WriteToSummaryFile('Actions performed by this script:');
   WriteToSummaryFile('');

   { Compile project before proceeding. }
   UfdUpdateGuiStatusMessage('Status:  Compiling project.');
   Project.DM_Compile;


   {** Verify that scripts and libraries global working copy is up-to-date. **}
   { Note:  Comment out this call if you don't want any dependence on external svn commands. }
   CheckThatSvnScriptsWorkingCopyIsUpdated(scriptsPath,
                                           constThisScriptName);
   

   { Record the wall clock time when we started the real work of this script. }
   startTime := Now();
   
   {** Initialize database info. **}
   InitDatabaseInfo(Project,
                    {var} dbLibDoc,
                    {var} dbTableList,
                    {var} changedFiles);


   {** Initialize database cache. **}
   dbParmsCache      := TStringList.Create;
   dbParmsCacheIndex := TStringList.Create;

   
   {** Loop over all logical documents in the project.... **}
   for k := 0 to Project.DM_LogicalDocumentCount - 1 do
   begin
      document := Project.DM_LogicalDocuments(k);
      WriteToDebugFile('Examining project document ' + document.DM_FullPath);
      
      { See if this document is a schematic page. }
      if (document.DM_DocumentKind = constKindSch) then
      begin
         
         UfdUpdateGuiStatusMessage('Status:  Checking components in schematic page ' + document.DM_FullPath);

         { Extract component parameter information from all components in this schematic page.
          Then proceed to check against the database entry for each component. }
         CheckComponentsVsDatabase(dbLibDoc,
                                   dbTableList,
                                   document,
                                   {var} dbParmsCache,
                                   {var} dbParmsCacheIndex,
                                   {var} changedFiles);
      end; { endif }

   end; { endfor loop over all logical documents in project }


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

         WriteToSummaryFile('No files were changed.  All schematic components were already synchronized to database.');

      end;

   { Free list of database tables. }
   dbTableList.Free;

   { Free database cache. }
   dbParmsCache.Free;
   dbParmsCacheIndex.Free;

   
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

      { Clear text asking the user for permission to run script. }
      UpdateFromDatabaseForm.formText22.Caption := '';
      UpdateFromDatabaseForm.formText22.Update;
      
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
   UpdateFromDatabaseForm.formText09.Caption := '4.  Check and if necessary, fix path to DBLib file.';
   UpdateFromDatabaseForm.formText10.Caption := '5.  Check and if necessary, fix path to DBLink file.';
   UpdateFromDatabaseForm.formText11.Caption := '6.  Update database link on any components still pointing to old DBLib file "' + constOldDbLibFileName + '".';
   UpdateFromDatabaseForm.formText12.Caption := '';
   
   UpdateFromDatabaseForm.formText13.Caption := 'Preconditions:';
   UpdateFromDatabaseForm.formText13.Font.Style := MkSet(fsBold);
   UpdateFromDatabaseForm.formText14.Caption := '1.  All project files should have been saved prior to running this script.';
   UpdateFromDatabaseForm.formText15.Caption := '';
   
   UpdateFromDatabaseForm.formText16.Caption := 'Notes:';
   UpdateFromDatabaseForm.formText16.Font.Style := MkSet(fsBold);
   UpdateFromDatabaseForm.formText17.Caption := '1.  This will take a minute or two to run.';
   UpdateFromDatabaseForm.formText18.Caption := '2.  You''ll need to save modified files after I''m done.';
   UpdateFromDatabaseForm.formText19.Caption := '3.  This script only synchronizes the schematic components to the database.';
   UpdateFromDatabaseForm.formText20.Caption := '4.  Thus, you will still need to push changes from schematic to layout!';
   UpdateFromDatabaseForm.formText21.Caption := '';
   
   UpdateFromDatabaseForm.formText22.Caption := 'Shall I run (OK), or shall I Cancel running this script?';
   UpdateFromDatabaseForm.formText22.Font.Style := MkSet(fsBold);
   UpdateFromDatabaseForm.formText23.Caption := '';
   UpdateFromDatabaseForm.formText24.Caption := '';
   UpdateFromDatabaseForm.formText25.Caption := '';

   UpdateFromDatabaseForm.formButtonsLabel1.Caption := '';

   UpdateFromDatabaseForm.formStatusBar1.SimpleText := 'Status:  Awaiting user permission to run script.';

   
   { Run GUI dialog box asking user for permission to run. }
   UpdateFromDatabaseForm.ShowModal;

   { Note:  Control now passes to one of the handler functions above. }
   
end; { end XIA_Update_From_Database() }


end.
