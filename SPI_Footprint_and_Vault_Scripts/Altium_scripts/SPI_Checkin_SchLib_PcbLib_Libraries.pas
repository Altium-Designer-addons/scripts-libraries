{***************************************************************************
 SPI_Checkin_SchLib_PcbLib_Libraries.pas
 Altium DelphiScript (basically Pascal) that will attempt to perform voodoo
 magic on SchLib and PcbLib libraries when checking them into subversion.
 ***************************************************************************}

{***************************************************************************
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
 * Some code borrowed from various XIA code files downloaded from:
 *  http://code.google.com/p/altium-designer-addons/wiki/Release_Manager
 * Copyright (c) 2009-2011 XIA LLC.
 *  (Sorting code stolen from Netlister.pas script from Altium 9 installation.)
 *  (Some code stolen from Altium examples and forum posts)
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
 *  Both of these scripts must be in the same script project.
 *  
 * Notes:
 *  1.  Tested with Altium 10.  (May or may not work with Altium 9--not tested.)
 *  2.  Tested with Windows 7 x64.
 *  3.  This file should no longer have TAB characters in it.
 *
 * WHAT THIS SCRIPT WILL DO:
 *  1.  Get list of all new / modified SchLib / PcbLib files in this project.
 *  2.  Ask user if it's ok to check in all new / modified SchLib / PcbLib files in this project.
 *  3.  Iterate through all new/modified SchLib/PcbLib files and set the symbol/footprint name as per our rules.
 *  4.  Perform svn add of all new/modified libs not yet in subversion.
 *  5.  Perform svn propset for all new/modified libs.
 *  6.  Perform svn checkin, asking user for commit message.
 *  7.  Change the symbol/footprint name for all libs just checked in to remove ':' chars.
 *  8.  Perform auto svn checkin.
 
 * WHAT THIS SCRIPT WILL *NOT* DO:
 *  
 * CAD SETUP REQUIREMENTS:
 *
 * SPI-ism's: (Assumptions / constraints / weirdness / etc. that may be very specific to my company)
 *
 * NOTES RE/ SCRIPT PROBLEMS:
 *  1.  This script will always generate a _Debug.txt file.
 *  The _Debug.txt file contains lots of debugging information.
 *  If this script ever aborts due to some unexpected and/or unexplained-on-screen
 *  error, be sure to check the _Debug.txt file and try to figure out what
 *  happened.  If you had a previous version of the _Debug.txt file open, be
 *  sure to close the file and re-open it.  Some text editors will not detect
 *  that this file has changed underneath it.
 *
 ***************************************************************************}


uses
SysUtils;

   
{***************************************************************************
 * Forward declarations for form objects.
 ***************************************************************************}
Interface

type
   TCheckinSchPcbLibsForm = class(TForm)
   formText01 : TLabel;    
   formEditBox1 : TMemo;
   formButtonOk: TButton;                 
   formButtonCancel: TButton;             
   formButtonsLabel1 :TLabel;         
   formStatusBar1: TXStatusBar;   
   procedure TCheckinSchPcbLibsForm.clickedOk(Sender : TPanel);
   procedure TCheckinSchPcbLibsForm.clickedCancel(Sender : TPanel);
end;


{***************************************************************************
 * Function forward declarations.
 ***************************************************************************}


{***************************************************************************
 * Global constants.
 ***************************************************************************}
const
{* Declare the version and name of this script. *}
   constScriptVersion          = 'v0.3.0 $Revision$';
   constThisScriptNameNoExt    = 'SPI_Checkin_SchLib_PcbLib_Libraries';
   constThisScriptName         = constThisScriptNameNoExt + '.pas';
{}
   { Constants used by this script. }
   constSvnRevString           = '_Svn_$' + 'Rev:: 123456 $:' { Note:  Intentionally split apart! }


{ Note:  We implicitly rely on a number of constants defined in XIA_Utils.pas. 
 That script and this one must both be part of the Pcb project!
 That way, we can use constants and functions defined in the other script. }
   

{***************************************************************************
 * Global variables.  Highly evil.  Ick ick.
 ***************************************************************************}
var
   CheckinSchPcbLibsForm : TCheckinSchPcbLibsForm;
   step                  : Integer;
   formResult            : TString;

{***************************************************************************
 * BEGIN Form related functions.
 ***************************************************************************}

{***************************************************************************
 * procedure TCheckinSchPcbLibsForm.clickedOk()
 *  This is the handler for primary dialog box "OK" click.
 ***************************************************************************}
procedure TCheckinSchPcbLibsForm.formButtonOkClick(Sender: TObject);
begin

//   ShowMessage('Hello world from TCheckinSchPcbLibsForm.formButtonOkClick');

   { Store the result from the form. }
   formResult            := 'Ok';

   { Close the form. }
   Close;
   
end;

{***************************************************************************
 * procedure TCheckinSchPcbLibsForm.clickedCancel()
 *  This is the handler for primary dialog box "CANCEL" click.
 ***************************************************************************}
procedure TCheckinSchPcbLibsForm.formButtonCancelClick(Sender: TObject);
begin

//   ShowMessage('Hello world from TCheckinSchPcbLibsForm.formButtonCancelClick');

   { Store the result from the form. }
   formResult            := 'Cancel';

   { Close the form. }
   Close;
   
end;


{***************************************************************************
 * BEGIN Support functions.
 ***************************************************************************}


{***************************************************************************
 * function CSPL_GetNewAndModifiedDocs()
 *  Extract lists of new (not yet in svn) and modified project documents.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CSPL_GetNewAndModifiedDocs(    allProjectDocs : TStringList;
                                        svnExtStatus   : TStringList;
                                    var newProjectDocs : TStringList;
                                    var modProjectDocs : TStringList;
                                    var newSchLibs     : TStringList;
                                    var modSchLibs     : TStringList;
                                    var newPcbLibs     : TStringList;
                                    var modPcbLibs     : TStringList;
                                        )              : Integer;

var
   rc              : Integer;
   i               : Integer;
   firstCharStatus : TString;
   currentDoc      : TString;
   currentDocExt   : TString;
   
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Create string lists to hold lists of project files that have been added/modified. }
   newProjectDocs := TStringList.Create;
   modProjectDocs := TStringList.Create;
   newSchLibs         := TStringList.Create;
   modSchLibs         := TStringList.Create;
   newPcbLibs         := TStringList.Create;
   modPcbLibs         := TStringList.Create;

   { Loop over all project documents. }
   { Note:  Here we assume that another function has made sure that there are the
    same number of lines in allProjectDocs as in svnExtStatus string lists! }
   for i := 0 to (allProjectDocs.Count - 1) do
   begin

      { Retrieve the name of the current project document. }
      currentDoc := allProjectDocs.Strings[i];
      
      { Extract the file extension for this document and convert to upper case. }
      currentDocExt := AnsiUpperCase(ExtractFileExt(currentDoc));

      { Extract just the 1st char, which indicates local status. }
      firstCharStatus := Copy(svnExtStatus.Strings[i], 1, 1);

      { See if we have a '?', indicating a file not yet in svn. }
      if (firstCharStatus = constSvnRepStatusNotInSvn) then
      begin
         
         { Flag that this document needs to be svn added. }
         newProjectDocs.Add(currentDoc);
         WriteToDebugFile('Identified new project document "' + currentDoc + '".');
         
         { Flag that this document will need to be checked in. }
         modProjectDocs.Add(currentDoc);

         {* Add it to the appropriate list of new-to-svn files to be shown to user. *}
         { See if it's an SchLib file. }
         if (AnsiUpperCase(currentDocExt) = ('.' + constKindSchLib)) then
         begin
            newSchLibs.Add(ExtractFileName(currentDoc));            
         end { endif }
         
         { Else see if it's an Pcblib file. }
         else if (AnsiUpperCase(currentDocExt) = ('.' + constKindPcblib)) then
         begin
            newPcbLibs.Add(ExtractFileName(currentDoc));            
         end; { end elsif }

      end { endif '?' }

      { Else see if we have 'A' or 'M', indicating file is already added to svn,
       or that it's been modified.  For our purposes here, the 'A' indicates
       that the file has already been scheduled for addition, and thus we will
       not consider it "new" and will not do an svn add operation on it. }
      else if ( (firstCharStatus = constSvnRepStatusAdded) or (firstCharStatus = constSvnRepStatusModified) {or (firstCharStatus = constSvnRepStatusHappy)} ) then
      begin
         
         { Flag that this document is modified and needs to be checked in. }
         modProjectDocs.Add(currentDoc);
         WriteToDebugFile('Identified modified project document "' + currentDoc + '".');

         {* Add it to the appropriate list of new-to-svn and modified-wrt-svn files to be shown to user. *}
         { Handle files with an 'A' status. }
         if (firstCharStatus = constSvnRepStatusAdded) then
         begin
            
            { See if it's an SchLib file. }
            if (AnsiUpperCase(currentDocExt) = ('.' + constKindSchLib)) then
            begin
               newSchLibs.Add(ExtractFileName(currentDoc));            
            end { endif }
            
            { Else see if it's an Pcblib file. }
            else if (AnsiUpperCase(currentDocExt) = ('.' + constKindPcblib)) then
            begin
               newPcbLibs.Add(ExtractFileName(currentDoc));            
            end; { end elsif }

         end { endif 'A' status }

         { Handle files with an 'M' status. }
         else
         begin
            
            { See if it's an SchLib file. }
            if (AnsiUpperCase(currentDocExt) = ('.' + constKindSchLib)) then
            begin
               modSchLibs.Add(ExtractFileName(currentDoc));            
            end { endif }
            
            { Else see if it's an Pcblib file. }
            else if (AnsiUpperCase(currentDocExt) = ('.' + constKindPcblib)) then
            begin
               modPcbLibs.Add(ExtractFileName(currentDoc));            
            end; { end elsif }

         end; { endelse }

      end { endif 'A' or 'M' }

      { Else check that the status reported as ' ' (space) aka "happy". }
      else if (firstCharStatus <> constSvnRepStatusHappy) then
      begin

         MyAbort('In CSPL_GetNewAndModifiedDocs(), got unsupported first status char "' + firstCharStatus + '" for project file "' + currentDoc + '"!');
         
      end; { endelse }
        
   end; { endfor }
   
   
end; { end CSPL_GetNewAndModifiedDocs() }


{***************************************************************************
 * function CSPL_AddNewDocsToSvn()
 *  Add new source documents to subversion.
 *  
 *  Just do svn add, no commits.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CSPL_AddNewDocsToSvn(project        : IProject;
                              projectPath    : TDynamicString;
                              scriptsPath    : TDynamicString;
                              newProjectDocs : TStringList;
                              )              : Integer;
                                                                 
var                                                              
   i                   : Integer;
   rc                  : Integer;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CSPL_AddNewDocsToSvn().');

   { See if we have any files to add to svn. }
   if (newProjectDocs.Count > 0) then
   begin
      
      { Try to add all these files to svn.
       Note:  It may report lines like "xyz is already under version control". }
      rc := IssueSvnCommand(scriptsPath,
                            projectPath,
                            constSvnCmdAdd,
                            {parms} newProjectDocs);

      {* Order Altium to refresh the project's svn status for display purposes. *}
//      CLF_RefreshProjectSvnStatus(99);

   end; { endif }

end; { end CSPL_AddNewDocsToSvn() }


{***************************************************************************
 * function CSPL_AddSvnKeywordsToModDocs()
 *  Add svn keywords properties to all modified documents.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CSPL_AddSvnKeywordsToModDocs(project        : IProject;
                                      projectPath    : TDynamicString;
                                      scriptsPath    : TDynamicString;
                                      modProjectDocs : TStringList;
                                      )              : Integer;
                                                                 
var                                                              
   i                   : Integer;
   rc                  : Integer;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CSPL_AddSvnKeywordsToModDocs().');

   { See if we have any modified files. }
   if (modProjectDocs.Count > 0) then
   begin
      
      { Try to set "svn:keywords Rev" property for all these files. }
      rc := IssueSvnCommand(scriptsPath,
                            projectPath,
                            constSvnCmdPropSetKeywordsRevOnly,
                            {parms} modProjectDocs);

      {* Order Altium to refresh the project's svn status for display purposes. *}
//      CLF_RefreshProjectSvnStatus(99);

   end; { endif }

end; { end CSPL_AddSvnKeywordsToModDocs() }


{***************************************************************************
 * function CSPL_GetCommitMessage()
 *  Ask user for an svn commit message.  Provide a template one for him/her to use.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CSPL_GetCommitMessage(    modProjectDocs : TStringList;
                                   newSchLibs     : TStringList;
                                   modSchLibs     : TStringList;
                                   newPcbLibs     : TStringList;
                                   modPcbLibs     : TStringList;
                                   pass           : Integer;
                               var commitMsg      : TStringList;
                                   )              : Integer;

var                                                              
   i                  : Integer;
   rc                 : Integer;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CSPL_GetCommitMessage().');

   { Sort all the human readable string lists. }
   newSchLibs.Sort;
   modSchLibs.Sort;
   newPcbLibs.Sort;
   modPcbLibs.Sort;

   { Construct global commit message header. }
   CheckinSchPcbLibsForm.formEditBox1.Lines.Clear;
   
   { In pass 1, prompt the user to enter the first line of the commit message. }
   if (pass = 1) then
   begin
      CheckinSchPcbLibsForm.formEditBox1.Lines.Add('(Edit this line to summarize what you''ve done (eg. "Created 0805 cap footprints with STEP models.")!)');
   end
   
   { In pass 2, just use a canned one. }
   else
   begin
      CheckinSchPcbLibsForm.formEditBox1.Lines.Add('Automated checkin by script ' + constThisScriptNameNoExt + ', ' + constScriptVersion + ', to remove ":" chars from symbol/footprint names for compatibility with Vault.');

   end;
   
   CheckinSchPcbLibsForm.formEditBox1.Lines.Add('');
   CheckinSchPcbLibsForm.formEditBox1.Lines.Add('Preparing to release to Vault....');

   { Construct heading for adding new symbols. }
   if (newSchLibs.Count > 0) then
   begin
      CheckinSchPcbLibsForm.formEditBox1.Lines.Add('');
      CheckinSchPcbLibsForm.formEditBox1.Lines.Add('Adding new symbols:');
      
      { Loop over all the SchLib files that are getting added. }
      for i := 0 to (newSchLibs.Count - 1) do
      begin
         CheckinSchPcbLibsForm.formEditBox1.Lines.Add(newSchLibs.Strings[i]);

      end;
   end; { endif }

   { Construct heading for adding new footprints. }
   if (newPcbLibs.Count > 0) then
   begin
      CheckinSchPcbLibsForm.formEditBox1.Lines.Add('');
      CheckinSchPcbLibsForm.formEditBox1.Lines.Add('Adding new footprints:');
      
      { Loop over all the PcbLib files that are getting added. }
      for i := 0 to (newPcbLibs.Count - 1) do
      begin
         CheckinSchPcbLibsForm.formEditBox1.Lines.Add(newPcbLibs.Strings[i]);

      end;
   end; { endif }

   { Construct heading for modified symbols. }
   if (modSchLibs.Count > 0) then
   begin
      CheckinSchPcbLibsForm.formEditBox1.Lines.Add('');
      CheckinSchPcbLibsForm.formEditBox1.Lines.Add('Modified symbols:');
      
      { Loop over all the SchLib files that are modified. }
      for i := 0 to (modSchLibs.Count - 1) do
      begin
         CheckinSchPcbLibsForm.formEditBox1.Lines.Add(modSchLibs.Strings[i]);

      end;
   end; { endif }

   { Construct heading for modified footprints. }
   if (modPcbLibs.Count > 0) then
   begin
      CheckinSchPcbLibsForm.formEditBox1.Lines.Add('');
      CheckinSchPcbLibsForm.formEditBox1.Lines.Add('Modified footprints:');
      
      { Loop over all the PcbLib files that are modified. }
      for i := 0 to (modPcbLibs.Count - 1) do
      begin
         CheckinSchPcbLibsForm.formEditBox1.Lines.Add(modPcbLibs.Strings[i]);

      end;
   end; { endif }

   { Setup various captions, etc. in the form. }
   CheckinSchPcbLibsForm.Caption := 'Please enter subversion commit message.';
   CheckinSchPcbLibsForm.formText01.Caption := 'Please enter 1st line of log message.  I have summarized which files are new or modified for you.  Click Ok to proceed, Cancel to cancel.';
   CheckinSchPcbLibsForm.formStatusBar1.SimpleText := 'Awaiting user input....';

   { In pass 1, actually get input from user. }
   if (pass = 1) then
   begin

      { Popup GUI list box to get checkin message. }
      CheckinSchPcbLibsForm.ShowModal;

      { Handle if the user clicked Cancel. }
//      ShowMessage('Back from modal dialog!');
      if (formResult <> 'Ok') then
         MyAbort('User requested cancel rather than proceed with svn commit.');

   end; { endif pass 1 }

   { Copy user-modified commit message to stringlist. }
   commitMsg := TStringList.Create;
   commitMsg.AddStrings(CheckinSchPcbLibsForm.formEditBox1.Lines);

end; { end CSPL_GetCommitMessage() }


{***************************************************************************
 * function CSPL_SvnCheckinProject()
 *  Commit all changed project files to svn.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CSPL_SvnCheckinProject(    scriptsPath    : TDynamicString;
                                    projectPath    : TDynamicString;
                                    modProjectDocs : TStringList;
                                    newSchLibs     : TStringList;
                                    modSchLibs     : TStringList;
                                    newPcbLibs     : TStringList;
                                    modPcbLibs     : TStringList;
                                var commitMsg      : TStringList;
                                    )              : Integer;
                                                                 
var                                                              
   i                  : Integer;
   rc                 : Integer;
   parms              : TStringList;
   commitMsgFile      : TString;
   newRevNum          : TString;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CSPL_SvnCheckinProject().');


   { Construct name of commit message file. }
   commitMsgFile      := projectPath + '__commit_message.txt';

   { Make sure we delete any existing instance of this file. }
   DeleteFileWithVerify(commitMsgFile);

   { Write user-modified commit message to file. }
   commitMsg.SaveToFile(commitMsgFile);

   { Create list of parameters for svn command. }
   parms := TStringList.Create;

   { Specifiy path to file containing commit message. }
   parms.Add('-F');
   parms.Add(commitMsgFile);

   { Add current directory "." to list of things to checkin. }
   parms.Add('.');

   {* Attempt to checkin all added/modified project files, including the project file. *}
   for i := 0 to (modProjectDocs.Count - 1) do
   begin

      { Add this project source file (which includes full path) to list of files to checkin. }
      parms.Add(modProjectDocs.Strings[i]);

   end; { endfor }
   
   {* Issue command to checkin generated files, and ask to look for "Committed revision" in svn output. *}
   IssueSvnCommandLookForOutputLine(scriptsPath,
                                    projectPath,
                                    constSvnCmdCommit,
                                    parms,
                                    constSvnRepCommitCommittedRev,
                                    {var} newRevNum);

   { Delete svn commit message file to clean up after ourselves. }
   DeleteFileWithVerify(commitMsgFile);

   { Free list of parameters. }
   parms.Free;
      
   { Make sure we found the desired line of output from svn.exe. }
   if (newRevNum = '') then
   begin
      MyAbort('Did not find file containing "' + constSvnRepCommitCommittedRev + '" in output from svn.exe!!');
   end;
   
   { Strip off "Committed revision " from start of newRevNum. }
   newRevNum := StringReplace(newRevNum, constSvnRepCommitCommittedRev, '', '');
   
   { Strip off "." from end of newRevNum and report new rev number. }
   newRevNum := StringReplace(newRevNum, '.', '', '');
   WriteToDebugFile('Svn commit succeeded with rev ' + newRevNum + '".');

   { Free stringlist holding commit message. }
   commitMsg.Free;                                              
   
end; { end CSPL_SvnCheckinProject() }


{***************************************************************************
 * function CSPL_ReOpenProjectAndRefreshSvnStatus()
 *  Re-open our original project and force Altium to refresh its svn status in Altium GUI.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CSPL_ReOpenProjectAndRefreshSvnStatus(WorkSpace      : IWorkSpace;
                                               project        : IProject;
                                               projectPath    : TString;
                                               modProjectDocs : TStringList;
                                               )              : Integer;

var                                                              
   i                  : Integer;
   rc                 : Integer;
   origProjPath       : TString;
   newProjPath        : TString;
   currentDoc         : TString;
   currentDocExt      : TString;
   found              : Boolean;
   projectPathAndName : TString;

begin

   { Assume success. }
   result := 0;

   { Calculate our original project path and name. }
   projectPathAndName := Project.DM_ProjectFullPath;

   WriteToDebugFile('Hello from CSPL_ReOpenProjectAndRefreshSvnStatus().');

   { Record our original focused project full path. }
   origProjPath        := Workspace.DM_FocusedProject.DM_ProjectFullPath;
   WriteToDebugFile(' Current project starts out as "' + origProjPath + '".');

   { Attempt to re-open our project. }
   { Unfortunately, this doesn't work. }
//   ResetParameters;
//   AddStringParameter('ObjectKind','Project');
//   AddStringParameter('FileName', projectPathAndName);
//   RunProcess('WorkspaceManager:OpenObject');   
   
   { Re-open a modified SchLib or PcbLib document to get our project back in focus. }
   i := 0;
   found := False;
   while ( (found = False) and (i < modProjectDocs.Count) ) do
   begin

      WriteToDebugFile(' i is ' + IntToStr(i));

      { Get the name of the current document. }
      currentDoc := modProjectDocs.Strings[i];
      
      { If currentDoc does not contain a drive letter:, then it means we have a project-relative
       file.  This is good for doing svn commands, but bad for what we need to do here. }
      if (ExtractFileDrive(currentDoc) = '') then
      begin
         WriteToDebugFile('Pre-pending project path to document "' + currentDoc + '".');
         currentDoc := projectPath + currentDoc;
         
      end; { endif }

      { Extract the file extension for this document and convert to upper case. }
      currentDocExt := AnsiUpperCase(ExtractFileExt(currentDoc));

      { See if it's an SchLib or PcbLib file. }
      if ( (currentDocExt = ('.' + constKindSchLib)) or 
          (currentDocExt = ('.' + constKindPcbLib)) ) then
      begin

         { Attempt to open this file. }
         ResetParameters;
         AddStringParameter('ObjectKind', 'Document');
         AddStringParameter('FileName', currentDoc);
         RunProcess('WorkspaceManager:OpenObject');

         { Flag that we found a file to open. }
         found := True;

      end; { endif }
      
      { Increment loop counter. }
      i:=i+1;

   end; { endwhile }
   
   { Record our new focused project full path. }
   newProjPath        := Workspace.DM_FocusedProject.DM_ProjectFullPath;
   WriteToDebugFile(' After re-opening project, current project is now "' + newProjPath + '".');
   WriteToDebugFile(' Original project was "' + projectPathAndName + '".');

   { Sanity check. }
   if (newProjPath <> projectPathAndName) then
      MyAbort('Unable to re-open original project!');
   
   { Order Altium to refresh the project's svn status for display purposes. }
   CLF_RefreshProjectSvnStatus(99);

   { Close all project files. }
   ResetParameters;
   AddStringParameter('ObjectKind', 'FocusedProjectDocuments');
   RunProcess('WorkspaceManager:CloseObject');
         
end; { end CSPL_ReOpenProjectAndRefreshSvnStatus() }


{***************************************************************************
 * function CSPL_RemoveIllegalVaultCharsFromSymbolOrFootprintName()
 *  Calculate a new symbol or footprint name that is compatible with the Vault.
 *  To the best of my knowledge, we need only remove the ":" chars.
 *  But what the hell, let's remove the "$" and " " chars while we're at it.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CSPL_RemoveIllegalVaultCharsFromSymbolOrFootprintName(symbolFootprintName : TString;
                           )                                                       : TString;

var                                                              
   i                      : Integer;
   newSymbolFootprintName : TString;

begin

   { Warning:  Assumes/requires that there are no " " (space) chars in the name that we want to keep! }
   
   { Calculate new symbol name, replacing all ":" with "_" chars. }
   newSymbolFootprintName := StringReplace(symbolFootprintName, ':', '', MkSet(rfReplaceAll));
   newSymbolFootprintName := StringReplace(newSymbolFootprintName, '$', '', MkSet(rfReplaceAll));
   newSymbolFootprintName := StringReplace(newSymbolFootprintName, ' ', '', MkSet(rfReplaceAll));
         
   { Call StripTrailingChar to remove all trailing spaces. }
//   result := StripTrailingChar({str} newSymbolFootprintName,
//                               {char} ' ');

   { Return new symbol/footprint name to caller. }
   result := newSymbolFootprintName;
   
end; { end CSPL_RemoveIllegalVaultCharsFromSymbolOrFootprintName() }


{***************************************************************************
 * function CSPL_ModifySchLib()
 *  Modify SchLib and PcbLib documents to correct the symbol/footprint name.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CSPL_ModifySchLib(project                : IProject;
                           currentDoc             : TString;
                           newSymbolFootprintName : TString;
                           pass                   : Integer;
                           )                      : Integer;

var                                                              
   i                      : Integer;
   rc                     : Integer;
   schLib                 : ISCH_Lib;
   symbolIterator         : ISCH_Iterator;
   symbol                 : ISCH_Component;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CSPL_ModifySchLib().');

   { Attempt to open this file. }
   ResetParameters;
   AddStringParameter('ObjectKind', 'Document');
   AddStringParameter('FileName', currentDoc);
   RunProcess('WorkspaceManager:OpenObject');
   
   If SchServer = Nil Then Exit;
   schLib := SchServer.GetCurrentSchDocument;
   If schLib = Nil Then Exit;
   
   { Create iterator to loop over all symbols in this library. }
   symbolIterator := schLib.SchLibIterator_Create;
   symbolIterator.AddFilter_ObjectSet(MkSet(eSchComponent));
   i:=0;
   
   try // begin
      
      { Get the first symbol in the library }
      symbol := symbolIterator.FirstSchObject;
      while (symbol <> Nil) do
      begin
         
         { Increment number of symbols that we've found. }
         i:=i+1;
               
         { Sanity check. }
         if (i > 1) then
            MyAbort('Expected to find 1 symbol within library "' + currentDoc + '".  Instead, found ' + IntToStr(i) + '!');
         
         { Report existing symbol name. }
         WriteToDebugFile(' Found symbol "' + symbol.LibReference + '" within this SchLib file.');

         { Report existing Vault linkage info. }
         WriteToDebugFile(' SymbolItemGUID is "' + symbol.SymbolItemGUID + '".');
         WriteToDebugFile(' SymbolReference is "' + symbol.SymbolReference + '".');
         WriteToDebugFile(' SymbolRevisionGUID is "' + symbol.SymbolRevisionGUID + '".');
         WriteToDebugFile(' SymbolVaultGUID is "' + symbol.SymbolVaultGUID + '".');

         { In pass 1, use the supplied new symbol name. }
         if (pass = 1) then
         begin
            
            { Change/correct symbol name to add $Rev$ magical subversion string. }
            symbol.LibReference := newSymbolFootprintName;

         end { endif }

         { In pass 2, remove the ":" chars from symbol name, but otherwise leave it alone. }
         else
         begin

            { Calculate and assign new symbol name. }
            symbol.LibReference := CSPL_RemoveIllegalVaultCharsFromSymbolOrFootprintName(symbol.LibReference);

         end; { endelse }
         
         { Report new symbol name. }
         WriteToDebugFile(' New symbol name is "' + symbol.LibReference + '" within this SchLib file.');

         { Report new Vault linkage info. }
         WriteToDebugFile(' SymbolItemGUID is "' + symbol.SymbolItemGUID + '".');
         WriteToDebugFile(' SymbolReference is "' + symbol.SymbolReference + '".');
         WriteToDebugFile(' SymbolRevisionGUID is "' + symbol.SymbolRevisionGUID + '".');
         WriteToDebugFile(' SymbolVaultGUID is "' + symbol.SymbolVaultGUID + '".');

         { Get the next symbol in the library. }
         symbol := symbolIterator.NextSchObject;
         
      end;
      
      finally  // begin

         { Destroy the library iterator. }
         schLib.SchIterator_Destroy(symbolIterator);

         //      end;

   end; { end try }

   { Save SchLib file. }
   ResetParameters;
   AddStringParameter('ObjectKind', 'FocusedDocument');
   AddStringParameter('SaveMode', 'Standard');
   RunProcess('WorkspaceManager:SaveObject');

   {* Order Altium to refresh the project's svn status for display purposes. *}
   CLF_RefreshProjectSvnStatus(99);

   { Close SchLib file. }
   ResetParameters;
   AddStringParameter('ObjectKind', 'FocusedProjectDocuments');
   RunProcess('WorkspaceManager:CloseObject');
         
   { Sanity check. }
   if (i <> 1) then
      MyAbort('Expected to find 1 symbol within library "' + currentDoc + '".  Instead, found ' + IntToStr(i) + '!');
         
//   { In pass 2, abort. }
//   if (pass = 2) then
//   begin
//      
//      MyAbort('foo');
//      
//   end { endif }

         
end; { end CSPL_ModifySchLib() }


{***************************************************************************
 * function CSPL_ModifyPcbLib()
 *  Modify SchLib and PcbLib documents to correct the symbol/footprint name.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CSPL_ModifyPcbLib(project                : IProject;
                           currentDoc             : TString;
                           newSymbolFootprintName : TString;
                           pass                   : Integer;
                           )                      : Integer;

var                                                              
   i                      : Integer;
   rc                     : Integer;
   pcbLib                 : IPCB_Library;
   footprintIterator      : IPCB_LibraryIterator;
   footprint              : IPCB_LibComponent;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CSPL_ModifyPcbLib().');


   { Attempt to open this file. }
   ResetParameters;
   AddStringParameter('ObjectKind', 'Document');
   AddStringParameter('FileName', currentDoc);
   RunProcess('WorkspaceManager:OpenObject');

   { Get a reference to the current PcbLib file. }
   pcbLib := PCBServer.GetCurrentPCBLibrary;
   if (pcbLib) = Nil then
      MyAbort('Unable to get reference to current PcbLib file!');

   
   { Prepare to iterate through all the footprints in this PcbLib. }
   footprintIterator := pcbLib.LibraryIterator_Create;
   footprintIterator.SetState_FilterAll;
   i:=0;

   { Iterate through all the footprints in this PcbLib. }
   try // begin { Fake keyword to help xemacs formatting. }
      footprint := footprintIterator.FirstPCBObject;
      while (footprint <> Nil) do
      begin

         { Increment number of footprints that we've found. }
         i:=i+1;
         
         { Sanity check. }
         if (i > 1) then
            MyAbort('Expected to find 1 footprint within library "' + currentDoc + '".  Instead, found ' + IntToStr(i) + '!');
   
         { Report existing footprint name. }
         WriteToDebugFile(' Found footprint "' + footprint.name + '" within this PcbLib file.');

         { In pass 1, use the supplied new footprint name. }
         if (pass = 1) then
         begin
            
            { Change/correct footprint name to add $Rev$ magical subversion string. }
            footprint.name := newSymbolFootprintName;

         end { endif }

         { In pass 2, remove the ":" chars from footprint name, but otherwise leave it alone. }
         else
         begin

            { Calculate and assign new footprint name. }
            footprint.name := CSPL_RemoveIllegalVaultCharsFromSymbolOrFootprintName(footprint.name);

         end; { endelse }

         { Report new footprint name. }
         WriteToDebugFile(' New footprint name is "' + footprint.name + '" within this PcbLib file.');

         { Iterate to next footprint in library. }
         footprint := footprintIterator.NextPCBObject;
      end; // end  { Fake keyword to help xemacs formatting. }

   { Destroy footprint iterator. }
   finally // begin  { Fake keyword to help xemacs formatting. }
      pcbLib.LibraryIterator_Destroy(footprintIterator);
      //end  { Fake keyword to help xemacs formatting. }
   end;
   
   { Save PcbLib file. }
   ResetParameters;
   AddStringParameter('ObjectKind', 'FocusedDocument');
   AddStringParameter('SaveMode', 'Standard');
   RunProcess('WorkspaceManager:SaveObject');

   {* Order Altium to refresh the project's svn status for display purposes. *}
   CLF_RefreshProjectSvnStatus(99);

   { Close PcbLib file. }
   ResetParameters;
   AddStringParameter('ObjectKind', 'FocusedProjectDocuments');
   RunProcess('WorkspaceManager:CloseObject');
   
   { Sanity check. }
   if (i <> 1) then
      MyAbort('Expected to find 1 footprint within library "' + currentDoc + '".  Instead, found ' + IntToStr(i) + '!');
   
end; { end CSPL_ModifyPcbLib() }


{***************************************************************************
 * function CSPL_ModifySchLibsAndPcbLibs()
 *  Modify SchLib and PcbLib documents to correct the symbol/footprint name.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CSPL_ModifySchLibsAndPcbLibs(project        : IProject;
                                      projectPath    : TString;
                                      modProjectDocs : TStringList;
                                      pass           : Integer;
                                      )              : Integer;

var                                                              
   i                      : Integer;
   rc                     : Integer;
   currentDoc             : TString;
   currentDocName         : TString;
   currentDocExt          : TString;
   newSymbolFootprintName : TString;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CSPL_ModifySchLibsAndPcbLibs().');

   { Loop over list of modified project documents. }
   for i := 0 to (modProjectDocs.Count - 1) do
   begin

      { Retrieve the path+name of the current project document. }
      currentDoc := modProjectDocs.Strings[i];

      { If currentDoc does not contain a drive letter:, then it means we have a project-relative
       file.  This is good for doing svn commands, but bad for what we need to do here,
       since it will cause the Altium GUI to popup dialog boxes asking the user where
       to save each SchLib/PcbLib file that we modify.  So pre-pend the project path
       if needed. }
      if (ExtractFileDrive(currentDoc) = '') then
      begin
         WriteToDebugFile('Pre-pending project path to document "' + currentDoc + '".');
         currentDoc := projectPath + currentDoc;
         
      end; { endif }

      { Extract just the file name of current project document. }
      currentDocName := ExtractFileName(currentDoc);
      
      { Extract the file extension for this document. }
      currentDocExt := ExtractFileExt(currentDoc);

      { Construct the new name of this symbol/footprint. }
      { Strip off the .SchLib or .PcbLib extension and add svn revision number string. }
      newSymbolFootprintName := ChangeFileExt(currentDocName, '') + constSvnRevString;

      WriteToDebugFile(' Examining project document "' + currentDoc + '".');

      { See if it's an SchLib file. }
      if (AnsiUpperCase(currentDocExt) = ('.' + constKindSchLib)) then
      begin
         
         WriteToDebugFile(' Identified "' + currentDoc + '" as an SchLib file.');

         { Call CSPL_ModifySchLib() to do all the real work. }
         CSPL_ModifySchLib(project,
                           currentDoc,
                           newSymbolFootprintName,
                           pass);
         
      end { endif }
      
      { Else see if it's an Pcblib file. }
      else if (AnsiUpperCase(currentDocExt) = ('.' + constKindPcblib)) then
      begin
         
         WriteToDebugFile(' Identified "' + currentDoc + '" as a Pcblib file.');

         { Call CSPL_ModifyPcbLib() to do all the real work. }
         CSPL_ModifyPcbLib(project,
                           currentDoc,
                           newSymbolFootprintName,
                           pass);
         
      end; { endif }
      
   end; { endfor }      

end; { end CSPL_ModifySchLibsAndPcbLibs() }


{***************************************************************************
 * END Support functions.
 ***************************************************************************}

{***************************************************************************
 * BEGIN Import related functions.
 ***************************************************************************}



{***************************************************************************
 * function CSPL_GetSchLibsAndPcbLibsInProject()
 *  Get lists of
 *  1.  All SchLib docs in the current project.
 *  2.  All PcbLib docs in the current project.
 *  3.  All source files in the current project (including the above ones).
 *
 *  Returns references to new string lists schLibDocs, pcbLibDocs, and allProjectDocs.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CSPL_GetSchLibsAndPcbLibsInProject(    Project        : IProject;
                                                projectPath    : TString;
                                            var schLibDocs     : TStringList;
                                            var pcbLibDocs     : TStringList;
                                            var allProjectDocs : TStringList;
                                                )              : Integer;

var
   i                  : Integer;
   k                  : Integer;
   document           : IDocument;
   docFullPathAndName : TString;
   docFullPath        : TString;
   docName            : TString;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CSPL_GetSchLibsAndPcbLibsInProject()');

   { Create string lists. }
   schLibDocs       := TStringList.Create;
   pcbLibDocs       := TStringList.Create;
   allProjectDocs   := TStringList.Create;

   { Add the project file itself to list of allProjectDocs. }
   allProjectDocs.Add(ExtractFileName(Project.DM_ProjectFullPath));
   WriteToDebugFile('Added project file ' + Project.DM_ProjectFullPath);
   
   { Look over all project documents looking for SchLib and PcbLib files. }
   {** Loop over all logical documents in the project. **}
   for k := 0 to (Project.DM_LogicalDocumentCount - 1) do
   begin

      { Get next document in project. }
      document := Project.DM_LogicalDocuments(k);

      { Extract full path and filename of this document. }
      docFullPathAndName := document.DM_FullPath;

      { Extract full path of this document. }
      docFullPath        := ExtractFilePath(docFullPathAndName);
            
      { See if the path part matches the project path. }
      if (AnsiUpperCase(docFullPath) = AnsiUpperCase(projectPath)) then
      begin

         WriteToDebugFile('Can use project-relative name for file "' + docFullPathAndName + '".');         
         docName            := ExtractFileName(docFullPathAndName);
         
      end

      { Else see if it's in a subdirectory of the project dir. }
      else if (CLF_DoesStringStartWith(docFullPathAndName, projectPath)) then
      begin
         WriteToDebugFile('Can use project-relative name for file in subdir "' + docFullPathAndName + '".');         
         docName            := StringReplace(docFullPathAndName, projectPath, '', '');

      end
      
      { Else see if it's a file in the footprints\Mentor_LP-Wizard directory and handle this specially. }
      else if (CLF_DoesStringStartWith(docFullPathAndName, 'R:\trunk\footprints\Mentor_LP-Wizard\')) then
      begin
         WriteToDebugFile('Found footprints\Mentor_LP-Wizard file.');
         docName            := StringReplace(docFullPathAndName, 'R:\trunk\footprints\Mentor_LP-Wizard\', '..\..\Mentor_LP-Wizard\', '');

      end
      
      { Else it lives somewhere else.  Use the full absolute path name. }
      else
      begin

         WriteToDebugFile('Must use full absolute path for file "' + docFullPathAndName + '".');         
         docName            := docFullPathAndName;

      end; { endelse }
      
      WriteToDebugFile('Examining project document ' + docName);
      WriteToDebugFile('Document kind is ' + document.DM_DocumentKind);
      
      { See if this document is an SchLib file. }
      if (AnsiUpperCase(document.DM_DocumentKind) = constKindSchLib) then
      begin

         { Add this to the list of SchLib documents in project. }
         schLibDocs.Add(docName);
         WriteToDebugFile('Identified it as an SchLib.');

      end { endif}

      { Else see if this document is a Pcblib file. }
      else if (AnsiUpperCase(document.DM_DocumentKind) = constKindPcblib) then
      begin

         { Add this to the list of Pcblib documents in project. }
         pcblibDocs.Add(docName);
         WriteToDebugFile('Identified it as an Pcblib.');

      end; { end elsif}

      { Add this to the list of all documents in project. }
      allProjectDocs.Add(docName);

   end; { endfor }

end; { end CSPL_GetSchLibsAndPcbLibsInProject() }



{***************************************************************************
 * END Import related functions.
 ***************************************************************************}



{***************************************************************************
 * procedure SPI_Checkin_SchLib_PcbLib_Libraries()
 *  Script entry point.
 *
 *  Returns:  (nothing--procedure, not a function).
 ***************************************************************************}
procedure SPI_Checkin_SchLib_PcbLib_Libraries;
var
   WorkSpace      : IWorkSpace;
   project        : IProject;
   projectPath    : TDynamicString;
   projOutPath    : TDynamicString;
   projectName    : TDynamicString;
   projLogPath    : TDynamicString;
   scriptsPath    : TDynamicString;
   document       : IDocument;
   timestamp      : TDynamicString;
   startTime      : TDateTime;
   endTime        : TDateTime;
   rc             : Integer;
   i              : Integer;
   schLibDocs     : TStringList;
   pcbLibDocs     : TStringList;
   allProjectDocs : TStringList;
   svnExtStatus   : TStringList;
   newProjectDocs : TStringList;
   modProjectDocs : TStringList;
   newSchLibs     : TStringList;
   modSchLibs     : TStringList;
   newPcbLibs     : TStringList;
   modPcbLibs     : TStringList;
   pass           : Integer;
   commitMsg      : TStringList;

begin

   {*** Run standard script initialization routine. ***}
   { Note:  This code is located in XIA_Utils.pas. }
   rc := InitScript(Workspace,
                    project,
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

   { Declare that we are running the CLF script. }
   whichScriptIsThis := constWhichScriptClf;

   { Delete useless ProjectLogs/ directory that InitScript() insists on creating for us. }
   { Note:  We're not bothering to test for success.  If there are files in there, it will fail, and that's ok. }
   RemoveDir(projLogPath);

   { Record the wall clock time when we started the real work of this script. }
   startTime := Now();

   { Open debug file. }
   OpenDebugFile((projectPath + constThisScriptNameNoExt + '_Debug.txt'));
   WriteToDebugFile('**Script ' + constThisScriptName + ' started at ' + DateTimeToStr(Date) + ' ' + TimeToStr(startTime));
   WriteToDebugFile('Project : ' +  project.DM_ProjectFileName);

   { Open summary file. }
   OpenSummaryFile((projectPath + constThisScriptNameNoExt + '_Report.txt'));
   WriteToSummaryFile('Actions performed by this script:');
   WriteToSummaryFile('');

   { Set initial "step" to 1. }
   step := 1;

   { Issue confirmation modal dialog box with specified confirmation message,
    specified reply after clicking Ok, and specified reply after clicking Cancel. }
   IssueConfirmationWithOkOrCancel('Welcome to script ' + constThisScriptNameNoExt + ', ' + constScriptVersion + '.' + constLineBreak + constLineBreak + 
                                   'This script will cleanup and svn commit all new and changed SchLib and PcbLib files in project "' + projectName + '".' + constLineBreak + constLineBreak + 
                                   'Shall I run on this project (OK) or shall I Cancel running this script?',
                                   '',
                                   'Canceled at user request.');


   { Try to close all project documents for the actual project. }
   CLF_UpdateGuiStatusMessage('Closing all documents in project before starting script.');
   ResetParameters;
   AddStringParameter('ObjectKind', 'FocusedProjectDocuments');
   RunProcess('WorkspaceManager:CloseObject');

   { Get lists of all SchLib docs, PcbLib docs, and all project docs in this project. }
   CSPL_GetSchLibsAndPcbLibsInProject(Project,
                                      projectPath,
                                      {var} schLibDocs,
                                      {var} pcbLibDocs,
                                      {var} allProjectDocs);

   { Get extended svn status for all files in project. }
   CLF_SvnGetExtendedStatus(scriptsPath,
                            projectPath,
                            allProjectDocs,
                            {var} svnExtStatus);

   { Get a list of all new and modified project documents. }
   CSPL_GetNewAndModifiedDocs(allProjectDocs,
                              svnExtStatus,
                              {var} newProjectDocs,
                              {var} modProjectDocs,
                              {var} newSchLibs,
                              {var} modSchLibs,
                              {var} newPcbLibs,
                              {var} modPcbLibs);

   { Setup for pass 1:  Turn on real $Rev$ keyword and checkin all new/modified files,
    with user-specified commit message. }
   pass := 1;

   { Ask user for commit message (in advance). }
   CSPL_GetCommitMessage(modProjectDocs,
                         newSchLibs,
                         modSchLibs,
                         newPcbLibs,
                         modPcbLibs,
                         pass,
                         {var} commitMsg);
   
   { Add all new source documents to subversion. }
   CSPL_AddNewDocsToSvn(project,
                        projectPath,
                        scriptsPath,
                        newProjectDocs);

   { Set svn:keywords properties for all modified source documents. }
   CSPL_AddSvnKeywordsToModDocs(project,
                                projectPath,
                                scriptsPath,
                                modProjectDocs);

   
   { Modify the symbol/footprint name in all modified SchLib/PcbLib files. }
   CSPL_ModifySchLibsAndPcbLibs(project,
                                projectPath,
                                modProjectDocs,
                                pass);
                                
   { Checkin all modified project files to svn. }
   CSPL_SvnCheckinProject(scriptsPath,
                          projectPath,
                          modProjectDocs,
                          newSchLibs,
                          modSchLibs,
                          newPcbLibs,
                          modPcbLibs,
                          {var} commitMsg);

   
   { Setup for pass 2:  Remove all ":" chars from symbol/footprint names for compatibility
    with Vault.  Re-commit all files. }
   pass := 2;

   { Provide automated commit message. }
   CSPL_GetCommitMessage(modProjectDocs,
                         newSchLibs,
                         modSchLibs,
                         newPcbLibs,
                         modPcbLibs,
                         pass,
                         {var} commitMsg);

   { Modify the symbol/footprint name in all modified SchLib/PcbLib files. }
   CSPL_ModifySchLibsAndPcbLibs(project,
                                projectPath,
                                modProjectDocs,
                                pass);
                                
   { Checkin all modified project files to svn. }
   CSPL_SvnCheckinProject(scriptsPath,
                          projectPath,
                          modProjectDocs,
                          newSchLibs,
                          modSchLibs,
                          newPcbLibs,
                          modPcbLibs,
                          {var} commitMsg);


   {* Wrap up. *}
   CSPL_ReOpenProjectAndRefreshSvnStatus(WorkSpace,
                                         project,
                                         projectPath,
                                         modProjectDocs);
   
   
   { Free all string lists. }
   schLibDocs.Free;
   pcbLibDocs.Free;
   allProjectDocs.Free;
   svnExtStatus.Free;
   newProjectDocs.Free;
   modProjectDocs.Free;
   newSchLibs.Free;
   modSchLibs.Free;
   newPcbLibs.Free;
   modPcbLibs.Free;

   CLF_UpdateGuiStatusMessage('Script is finishing....');

   { Record the wall clock time when we ended this script. }
   endTime := Now();
   
   { Timestamp the end of our actions, before we present the last dialog box to the user. }
   WriteToDebugFile('');
   WriteToDebugFile('**Script ' + constThisScriptName + ' ending at ' + DateTimeToStr(Date) + ' ' + TimeToStr(endTime));
   WriteToDebugFile('**Script took ' + FormatDateTime('h:n:s', (endTime-startTime)) + ' (hrs:mins:secs) to run on this project on this PC.');


   { Tell user what he/she still needs to do. }
   CLF_WriteToSummaryAndDebugFiles('');
   CLF_WriteToSummaryAndDebugFiles('You still need to:');
   CLF_WriteToSummaryAndDebugFiles('1.  Release libraries to Vault.');

   ShowMessage('Script has completed successfully.' + constLineBreak + constLineBreak +
               'You still need to:' + constLineBreak +
               '1.  Release libraries to Vault.');


   {****** Wrap things up ******}
   { Call AtExit() procedure to write debug outputs to file. }
   
   WriteToDebugFile('**About to exit script.');
//   ShowMessage('About to call AtExit()');
   AtExit(0);                   { Report success at exit }
//   ShowMessage('Back from AtExit()');

end; { end SPI_Checkin_SchLib_PcbLib_Libraries() }

end.
