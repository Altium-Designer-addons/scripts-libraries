{***************************************************************************
 SPI_Cleanup_AltLib_Footprint.pas
 Altium DelphiScript (basically Pascal) that will attempt to cleanup a new
 footprint imported from pre-supplied Altium libraries (eg. passives).
 ***************************************************************************}

{***************************************************************************
 * Sierra Photonics Inc. has derived this file from XIA_Update_From_Database.pas.
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
 *  1.  This script requires functions and constants defined in SPI_Cleanup_LPW_Footprint.pas.
 *  Both of these scripts must be in the same script project.
 *  2.  This script requires functions and constants defined in XIA_Utils.pas.
 *  Both of these scripts must be in the same script project.
 *  
 * Notes:
 *  1.  Tested with Altium 10.  (May or may not work with Altium 9--not tested.)
 *  2.  Tested with Windows 7 x64.
 *  3.  This file should no longer have TAB characters in it.
 *
 * WHAT THIS SCRIPT WILL DO:
 
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
   TCleanupAltLibFootprintForm = class(TForm)
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
   procedure TCleanupAltLibFootprintForm.clickedOk(Sender : TPanel);
   procedure TCleanupAltLibFootprintForm.clickedCancel(Sender : TPanel);
   procedure TCleanupAltLibFootprintForm.bCheck0_1(Sender : TPanel);
end;

   
{***************************************************************************
 * Function forward declarations.
 ***************************************************************************}


{***************************************************************************
 * Global constants.
 ***************************************************************************}
const
{* Declare the version and name of this script. *}
   constScriptVersion          = 'v0.10.3 $Revision$';
   constThisScriptNameNoExt    = 'SPI_Cleanup_AltLib_Footprint';
   constThisScriptName         = constThisScriptNameNoExt + '.pas';
{}
{}
   { Constants related to courtyard. }
   constOldWidthCourtyardMm    = 0.1;       { Width of courtyard lines in mm in source libraries. }
   constOldLayerCourtyard      = eMechanical15; { Mech layer for courtyard lines in source libraries. }
{}
   { Constants related to assembly drawing. }
   constOldWidthAssyDrawingMm  = 0.1;       { Width of assembly drawing lines in mm in LP Wizard source. }
   constOldLayerAssyDrawing    = eMechanical13; { Mech layer for assembly drawing lines in source libraries. }
   constNewWidthAssyDrawingMm  = 0.1;       { Standard width of assembly drawing lines in our destination library. }
   constNewMinWidthAssyDrawingMm= 0.08;     { Minimal width of assembly drawing lines in our destination library. }
   constRotationAssyText       = 0;         { Rotation of text for chip caps & chip resistors. }
   constThreshSmallAssyOutlineMm=1.0;       { If the assembly outline is less than this, use smaller line widths! }
   constAllowNumCommentChars   = 4;         { Check that .Comment can grow to this many chars.  If not, break assembly outline to accommodate it. }
{}
   { Constants related to silkscreen. }
   constOldWidthSilkscreenMm   = 0.2;       { Width of silkscreen lines in mm in source libraries. }
   constOldLayerSilkscreen     = eTopOverlay; { Layer for silkscreen lines in source libraries. }
   constNewLayerSilkscreen     = 0;         { Flag that we wish to kill off silkscreen lines in source libraries. }
{}
{}

{ Note:  We implicitly rely on a number of constants defined in SPI_Cleanup_LPW_Footprint.pas.
 That script and this one must both be part of the Pcb project!
 That way, we can use constants and functions defined in the other script. }
   
{ Note:  We implicitly rely on a number of constants defined in XIA_Utils.pas. 
 That script and this one must both be part of the Pcb project!
 That way, we can use constants and functions defined in the other script. }
   

{***************************************************************************
 * Global variables.  Highly evil.  Ick ick.
 ***************************************************************************}
var
   CleanupAltLibFootprintForm : TCleanupAltLibFootprintForm;
//   step                    : Integer;

{***************************************************************************
 * BEGIN Form related functions.
 ***************************************************************************}

{***************************************************************************
 * procedure CALF_UpdateGuiStatusMessage()
 *  Update status message in dialog box and write said status to the debug file.
 ***************************************************************************}
procedure CALF_UpdateGuiStatusMessage(msg : TString);
begin

   { Change text in GUI status line. }
   formStatusBar1.SimpleText := msg;

   { Force a screen refresh of GUI status line. }
   formStatusBar1.Update;
   CleanupAltLibFootprintForm.Update;

   { Copy this message to debug file. }
   WriteToDebugFile('');
   WriteToDebugFile('* ' + msg);

end; { end CALF_UpdateGuiStatusMessage() }


{***************************************************************************
 * procedure CALF_WriteToSummaryAndDebugFiles()
 *  Write a new line of text to the summary file.
 *  Also write said line of text to debug file with '**' pre-pended.
 ***************************************************************************}
procedure CALF_WriteToSummaryAndDebugFiles(msg : TString);
begin

   { Call CLF_WriteToSummaryAndDebugFiles() to do all the real work. }
   CLF_WriteToSummaryAndDebugFiles(msg);
   
end; { end CALF_WriteToSummaryAndDebugFiles() }


{***************************************************************************
 * procedure CALF_WriteToSummaryAndDebugFilesWithStepNum()
 *  Write a new line of text to the summary file and debug file with
 *  a running step number pre-pended.
 ***************************************************************************}
procedure CALF_WriteToSummaryAndDebugFilesWithStepNum(msg : TString);
begin

   { Call CLF_WriteToSummaryAndDebugFilesWithStepNum() to do all the real work. }
   CLF_WriteToSummaryAndDebugFilesWithStepNum(msg);

end; { end CALF_WriteToSummaryAndDebugFilesWithStepNum() }


{***************************************************************************
 * procedure CALF_AtExit()
 *  Put results in our dialog box list and return to AtExit() for the rest of the cleanup routines.
 ***************************************************************************}
procedure CALF_AtExit(rc : Integer);
var
   i        : Integer;

begin 

   {* Transform existing GUI dialog box so that there is a big list box available. *}

   { Nuke most text fields to make room for the big list box. }
   CleanupAltLibFootprintForm.formText03.Free;
   CleanupAltLibFootprintForm.formText04.Free;
   CleanupAltLibFootprintForm.formText05.Free;
   CleanupAltLibFootprintForm.formText06.Free;
   CleanupAltLibFootprintForm.formText07.Free;
   CleanupAltLibFootprintForm.formText08.Free;
   CleanupAltLibFootprintForm.formText09.Free;
   CleanupAltLibFootprintForm.formText10.Free;
   CleanupAltLibFootprintForm.formText11.Free;
   CleanupAltLibFootprintForm.formText12.Free;
   CleanupAltLibFootprintForm.formText13.Free;
   CleanupAltLibFootprintForm.formText14.Free;
   CleanupAltLibFootprintForm.formText15.Free;
   CleanupAltLibFootprintForm.formText16.Free;
   CleanupAltLibFootprintForm.formText17.Free;
   CleanupAltLibFootprintForm.formText18.Free;
   CleanupAltLibFootprintForm.formText19.Free;
   CleanupAltLibFootprintForm.formText20.Free;
   CleanupAltLibFootprintForm.formText21.Free;
   CleanupAltLibFootprintForm.formText22.Free;
   CleanupAltLibFootprintForm.formText23.Free;
   CleanupAltLibFootprintForm.formText24.Free;
   CleanupAltLibFootprintForm.formText25.Free;
   CleanupAltLibFootprintForm.formText26.Free;
   CleanupAltLibFootprintForm.formText27.Free;
   CleanupAltLibFootprintForm.formText28.Free;
   CleanupAltLibFootprintForm.formText29.Free;
   CleanupAltLibFootprintForm.formText30.Free;

   { Transform existing GUI dialog box so that there is a big list box available. }
   CleanupAltLibFootprintForm.listBox1.Left := 14;
   CleanupAltLibFootprintForm.listBox1.Top := 40;
   CleanupAltLibFootprintForm.listBox1.Width := 972;
   CleanupAltLibFootprintForm.listBox1.Height := 640;

   { Move Ok button to center. }
   CleanupAltLibFootprintForm.formButtonOk.Left := 450;
   CleanupAltLibFootprintForm.formButtonOk.Enabled := True;
   CleanupAltLibFootprintForm.formButtonOk.Update;

   { Nuke Cancel button. }
   CleanupAltLibFootprintForm.formButtonCancel.Free;

   { Run GUI dialog box to display the summary messages. }
   //        ShowMessage('About to display modal dialog');
   CleanupAltLibFootprintForm.formText01.Caption := SummaryMessages.Strings[0];

   CleanupAltLibFootprintForm.listBox1.Clear;


   {* Proceed to output summary messages in the list box. *}
   
   { Loop over all the summary messages. }
   for i := 1 to SummaryMessages.Count - 1 do
   begin

      { Add this line of message to the list box on screen. }
      CleanupAltLibFootprintForm.listBox1.Items.Insert((i-1), SummaryMessages.Strings[i]);

      { Update the size of the horizontal scroll bars if needed. }
      { Code stolen from http://www.delphipages.com/forum/showthread.php?t=203460 }
//    if (CleanupAltLibFootprintForm.listBox1.Canvas.TextWidth(SummaryMessages.Strings[i] + 'W') > CleanupAltLibFootprintForm.listBox1.ScrollWidth) then
//    begin
//       CleanupAltLibFootprintForm.listBox1.ScrollWidth := CleanupAltLibFootprintForm.listBox1.Canvas.TextWidth(SummaryMessages.Strings[i] + 'W');
//    end;

      { For some reason, that's not enough.  Double it instead of adding the width of a 'W' char. }
      if (CleanupAltLibFootprintForm.listBox1.Canvas.TextWidth(SummaryMessages.Strings[i] + SummaryMessages.Strings[i]) > CleanupAltLibFootprintForm.listBox1.ScrollWidth) then
      begin
         CleanupAltLibFootprintForm.listBox1.ScrollWidth := CleanupAltLibFootprintForm.listBox1.Canvas.TextWidth(SummaryMessages.Strings[i] + SummaryMessages.Strings[i]);
      end;    

   end;

   { If we were given a sucess error code, meaning we're exiting successfully, report that. }
   if (rc = 0) then
   begin
      CleanupAltLibFootprintForm.formButtonsLabel1.Caption := 'Script is exiting successfully.  Click Ok to finish.';
   end

   { Else report error exit condition. }
   else
   begin

      CleanupAltLibFootprintForm.formButtonsLabel1.Caption := 'Failed while running this operation: ' + CleanupAltLibFootprintForm.formStatusBar1.SimpleText + constLineBreak + constLineBreak +
      'ERROR:  Script is exiting prematurely due to error!';

   end;

   { Since that form is already modal, we simply exit and rely on the click handler
    to do the remaining cleanup. }   


end; { end CALF_AtExit() }
   
{***************************************************************************
 * END Form related functions.
 ***************************************************************************}

{***************************************************************************
 * BEGIN Support functions.
 ***************************************************************************}
{***************************************************************************
 * procedure CALF_Abort()
 *  Call cleanup routines and then abort script.
 ***************************************************************************}
procedure CALF_Abort(msg : TDynamicString);
begin

   { Save abort message to debug file. }
   WriteToDebugFile('');
   WriteToDebugFile('**In CALF_Abort()!!!');
   WriteToDebugFile(msg);

   { Attempt to close modal dialog box. }
   CleanupAltLibFootprintForm.Close;

   { Give error message to user. }
   ShowError(msg + constLineBreak + constLineBreak +
               'Aborting script!!!' + constLineBreak + constLineBreak +
               'Afterwards, hit Control-F3 (or go to Run->Stop) to shut down script execution.' + constLineBreak +
               'Then, click on a file in your PCB project to reset focus.');
   
   { Call AtExit() procedure to write debug outputs to file. }
   AtExit(1);                   { Report error at exit }

   { Now do the real abort. }
   Abort;
end; { end CALF_Abort() }


{***************************************************************************
 * function CALF_CalculateLibFileNameSuffixAndDescription()
 *  Create the SPI-specific suffix that we will append to the IPC package code.
 *  This includes a field indicating package color (eg. "Blk" for black).
 *  This includes a field indicating package marking (eg. "BLNK" for blank).
 *  This includes a field indicating IPC footprint compliance ("IPC").
 *  This includes a field indicating source of footprint (eg. "AltLib" for derived from pre-supplied Altium library file).
 *  
 *  Create the description string while we're at it.
 *
 *  NOTE:   This entire function is very SPI-specific!
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CALF_CalculateLibFileNameSuffixAndDescription(    packageHeightMm      : Real;
                                                           packageColorCode     : TString;
                                                           packageMarkingText   : TString;
                                                           cnfGalacticInfo      : TStringList;
                                                       var libFileName          : TString;
                                                       var libFileNameSuffix    : TString;
                                                       var libDescription       : TString;
                                                       var allowNumCommentChars : Integer;
                                                           )                    : Integer;

var
   mfgName                 : TString;
   mfgPkgCode              : TString;
   hasEp                   : Boolean;
   thViasCode              : TString;
   thViasDesc              : TString;
   packageColorDesc        : TString;
   packageMarkingCode      : TString;
   packageMarkingDesc      : TString;
   footprintStandardCode   : TString;
   footprintStandardDesc   : TString;
   footprintSourceCode     : TString;
   footprintSourceDesc     : TString;
   altLibFpName            : TString;
   altLibDescription       : TString;
   altLibDescWithHeight    : TString;
   footprintType           : TString;
   footprintSizeCodeMetric : TString;
   footprintSizeCodeEIA    : TString;
   footprintIpcStandard    : TString;
   
begin                                                               

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CALF_CalculateLibFileNameSuffixAndDescription().');

   { Retrieve information from the source Altium PcbLib. }
   altLibFpName            := cnfGalacticInfo.Values(constGilLibraryFileName);
   altLibDescription       := cnfGalacticInfo.Values(constGilLibraryFileDescription);

   { Insert package height information into the description string. }
   altLibDescWithHeight    := StringReplace(altLibDescription, ', IPC', ('x' + FloatToStr(packageHeightMm) + 'mm, IPC'), 0);


   { Parse the footprint name from source Altium PcbLib. }
   footprintType           := Copy(altLibFpName, 1, 4);
   footprintSizeCodeMetric := Copy(altLibFpName, 5, 4);
   footprintIpcStandard    := Copy(altLibFpName, 9, 1);

   { Parse footprint type and decide how many characters are needed for the .Comment string. }
   if (footprintType = 'RESC') then
      allowNumCommentChars := 4 {nosemi}   { eg. "100K" }
   else if (footprintType = 'CAPC') then
      allowNumCommentChars := 5 {nosemi}   { eg. "100nF" }
   else
      CALF_Abort('Unsupported footprint type "' + footprintType + '"!');

   { Convert metric size code to EIA size code. }
   if (footprintSizeCodeMetric = '1005') then
      footprintSizeCodeEIA  := '0402' {nosemi}
   else if (footprintSizeCodeMetric = '1608') then
      footprintSizeCodeEIA  := '0603' {nosemi}
   else if (footprintSizeCodeMetric = '2012') then
      footprintSizeCodeEIA  := '0805' {nosemi}
   else if (footprintSizeCodeMetric = '3216') then
      footprintSizeCodeEIA  := '1206' {nosemi}
   else
      CALF_Abort('Unsupported footprint size code (metric) "' + footprintSizeCodeMetric + '"!');

   { Sanity check. }
   if (footprintIpcStandard <> 'N') then
      CALF_Abort('Unsupported footprint IPC standard "' + footprintIpcStandard + '"!');


   { Store info to galactic string list. }
   cnfGalacticInfo.add(constGilFootprintType + constStringEquals + footprintType);

   

   
   { Here we assume that since we're only dealing with chip passives, that there will never be EP pads,
    and thus never the possibility of thermal vias. }
   thViasCode := '';
   thViasDesc := '';

   
   {* Expand the color code into the English word for that color. *}
   CLF_TranslateColorCode(packageColorCode,
                          {var} packageColorDesc);
   

   {* Construct the package marking code and text. *}
   if (packageMarkingText = '') then
   begin
      packageMarkingCode := 'BLNK';
      packageMarkingDesc := 'No marking';
   end

   else
   begin
      packageMarkingCode := 'Marked_'    + packageMarkingText;
      packageMarkingDesc := 'Marked as ' + packageMarkingText;
   end; { endelse }


   {* Construct the footprint standard code and text. *}
   footprintStandardCode := 'IPC';
   footprintStandardDesc := 'IPC Compliant';

   {* Construct the footprint footprint source code and text. *}
   footprintSourceCode   := 'AltLib';
   footprintSourceDesc   := 'Footprint adapted from Altium supplied libraries';

   
   {** Construct the library file name. **}
   libFileName           := footprintType + footprintSizeCodeMetric + 'X' + FloatToStr(packageHeightMm * 100) + footprintIpcStandard + '_EIA' + footprintSizeCodeEIA;

   
   {** Construct the library file name suffix. **}

   { Suffix must be "mfgName_mfgPkgCode[_NoThVias]_Blk_BLNK_IPC_LPW". }
   libFileNameSuffix := '_' +                     packageColorCode + '_' + packageMarkingCode + '_' + footprintStandardCode + '_' + footprintSourceCode;
   WriteToDebugFile(' libFileNameSuffix is "' + libFileNameSuffix + '".');
   libFileName           := libFileName + libFileNameSuffix;
   

   {** Create library part description field. **}
   libDescription := altLibDescWithHeight + '; ' + packageColorDesc + '; ' + packageMarkingDesc + '; ' + footprintStandardDesc + '; ' + footprintSourceDesc;
   WriteToDebugFile(' libDescription is "' + libDescription + '".');
   
end; { end CALF_CalculateLibFileNameSuffixAndDescription() }

{***************************************************************************
 * END Support functions.
 ***************************************************************************}

{***************************************************************************
 * BEGIN Import related functions.
 ***************************************************************************}
{***************************************************************************
 * function CALF_ChooseXmlCmdFile()
 *  Find the .xml command file that describes which footprints should be
 *  cleaned up and readied for use at SPI.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CALF_ChooseXmlCmdFile(    projectPath     : TDynamicString;
                               var cnfGalacticInfo : TStringList;
                               var commandFilePath : TString;
                                   )               : Integer;

var
   i                : Integer;
   resultFromDialog : Boolean;
   pathFromDialog   : TString;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CALF_ChooseXmlCmdFile()');
   CALF_UpdateGuiStatusMessage('Please choose .xml command file which describes Altium library footprints that we wish to cleanup, and add SPI-specific features to.');

   { Ask the user to identify the .xml command file that describes desired operations. }
   ResetParameters;
   AddStringParameter('Dialog','FileOpenSave');
   AddStringParameter('Mode', '0'); { OpenFile dialog }
   AddStringParameter('Path', (projectPath + '\*.xml'));
   AddStringParameter('Prompt', 'Choose .xml command file which describes Altium library footprints that we wish to cleanup, and add SPI-specific features to.  Click "Open".');
   AddStringParameter('FileType1', 'Excel xml file (*.xml)|*.xml');
   
   RunProcess('Client:RunCommonDialog');

   { Get parameters back from call to RunCommonDialog. }
   pathFromDialog   := '';
   GetStringParameter('Result', resultFromDialog);
   GetStringParameter('Path', pathFromDialog);
   
   WriteToDebugFile(' Result is ' + BoolToStr(resultFromDialog) + '.');
   WriteToDebugFile(' Path is ' + pathFromDialog + '.');

   { Sanity check }
   if (resultFromDialog = False) then
      CLF_Abort('Did not get name of .xml command file from user dialog!');

   { Return file name with full path to caller. }
   commandFilePath := pathFromDialog;
   CLF_WriteToSummaryAndDebugFilesWithStepNum('User chose .xml command file "' + commandFilePath + '".');
   
end; { end CALF_ChooseXmlCmdFile() }


{***************************************************************************
 * function CALF_AddSourceDocumentsToProjectAndSvn()
 *  Add our source documents to the project and to subversion.  This includes
 *  the .xml command file and the Altium supplied .PcbLib source file.
 *  
 *  Just do svn add, no commits.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CALF_AddSourceDocumentsToProjectAndSvn(    project         : IProject;
                                                    projectPath     : TDynamicString;
                                                    scriptsPath     : TDynamicString;
                                                    commandFilePath : TString;
                                                    sourcePcbLib    : IPCB_Library;
                                                var cnfGalacticInfo : TStringList;
                                                    )               : Integer;
                                                                 
var                                                              
   i                 : Integer;
   rc                : Integer;
   parms             : TStringList;
   dummy             : TStringList;
   srcPcbLibFilePath : TString;
   projectDoc        : IServerDocument;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CALF_AddSourceDocumentsToProjectAndSvn().');
   CLF_UpdateGuiStatusMessage('Proceeding to add source documents to project and to svn.');

   { Add projectPath to galactic string list, for later use. }
   cnfGalacticInfo.add(constGilProjectFileName + constStringEquals + projectPath);
   

   {* Add command file to project. *}
   CLF_ProjectAddFile(Project,
                      {addMePath} commandFilePath);
   CLF_WriteToSummaryAndDebugFilesWithStepNum('Added footprint command file "' + commandFilePath + '" to project.');
   
   
   {* Add source PcbLib file to project. *}
   { Retrieve info from galactic string list. }
   { Note:  We should be able to get this via sourcePcbLib.DM_FullPath or some such, but it doesn't seem to work.
    Not to worry, though, since we had this information previously. }
   srcPcbLibFilePath := cnfGalacticInfo.Values(constGilSpecAltPcbLibFilePath);
   
   CLF_ProjectAddFile(Project,
                      {addMePath} srcPcbLibFilePath);
   CLF_WriteToSummaryAndDebugFilesWithStepNum('Added source PcbLib file "' + srcPcbLibFilePath + '" to project.');
   
   
   {* Save modifications to project file. *}
   { Note:  If/when the script crashes here, it means that Altium itself is unhappy and needs to be shut down and restarted! }
   projectDoc := Client.OpenDocument('PrjPcb', project.DM_ProjectFullPath);
   projectDoc.DoFileSave('PrjPcb');
   
   
   {* Svn add the source documents. *}

   { Create string list. }
   parms := TStringList.Create();

   { Setup which files to add to svn. }
   { TODO:  If these files are now part of our project, can we use the builtin version control to do the svn add?? }
   parms.Add(commandFilePath);
   parms.Add(srcPcbLibFilePath);

   { Call CLF_SvnAddFiles() to do a safe svn add supporting svn rev 1.7.x. }
   CLF_SvnAddFiles(scriptsPath,
                   projectPath,
                   parms);
   
   { Free string list. }
   parms.Free();

   CLF_WriteToSummaryAndDebugFilesWithStepNum('Added above files to svn.');
   
   {* Order Altium to refresh the project's svn status for display purposes. *}
   CLF_RefreshProjectSvnStatus(99);

end; { end CALF_AddSourceDocumentsToProjectAndSvn() }

{***************************************************************************
 * END Import related functions.
 ***************************************************************************}

{***************************************************************************
 * BEGIN Footprint-related functions.
 ***************************************************************************}
{***************************************************************************
 * function CALF_OpenPcbLibFileAndSelectedFootprint()
 *  Find the one component in the source PcbDoc file and explode it.
 *  That is to say, unlock its primitives.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CALF_OpenPcbLibFileAndSelectedFootprint(var cnfGalacticInfo : TStringList;
                                                 var sourcePcbLib    : IPCB_Library;
                                                     )               : Integer;

var                                         
   iterator          : IPCB_BoardIterator;
   srcPcbLibFilePath : TString;
   srcFootprintName  : TString;
   footprint         : IPCB_LibComponent;
   found             : Boolean;
   
begin

   { Assume success. }
   result := 0;
   
   CALF_UpdateGuiStatusMessage('Attempting to open selected PcbLib library file and selected footprint therein.');

   { Attempt to start PCB server. }
   Client.StartServer(constKindPcb);

   { Tell the PCB Server to get ready for us. }
   PCBServer.PreProcess;

   { Check if PCB server is alive. }
   if (PCBServer = Nil) then
      CALF_Abort('PCBServer is Nil.  D''oh.');
   
   { Initialize the PCB editor. }
   PCBServer.PreProcess;

   
   { Retrieve info from galactic string list. }
   srcPcbLibFilePath := cnfGalacticInfo.Values(constGilSpecAltPcbLibFilePath);
   srcFootprintName  := cnfGalacticInfo.Values(constGilSpecAltFootprintName);

   
   { Manually open PcbLib document and then use GetCurrentPCBLibrary. } 
   ResetParameters;
   AddStringParameter('ObjectKind', 'Document');
   AddStringParameter('FileName', srcPcbLibFilePath);
   RunProcess('WorkspaceManager:OpenObject');
   
   sourcePcbLib := PCBServer.GetCurrentPCBLibrary;
   If sourcePcbLib = Nil Then
      CALF_Abort('Unable to open source PcbLib file!');

   { Look for the desired footprint by name within this PcbLib library. }
   footprint := sourcePcbLib.GetComponentByName(srcFootprintName);
   
   { Sanity check. }
   if (footprint = Nil) then
      CALF_Abort('Did not find specified footprint "' + srcFootprintName + '" within specified PcbLib' + constLineBreak + '"' + srcPcbLibFilePath + '"!');

   { Select desired footprint. }
   sourcePcbLib.CurrentComponent := footprint;
   sourcePcbLib.RefreshView;

   { Refresh the PcbLib document. }
   ResetParameters;
   AddStringParameter('Action', 'All');
   RunProcess('PCB:Zoom');


   { Retrieve the footprint name and description fields and store to galactic string list. }
   cnfGalacticInfo.add(constGilLibraryFileName        + constStringEquals + footprint.Name);
   cnfGalacticInfo.add(constGilLibraryFileDescription + constStringEquals + footprint.Description);

end; { end CALF_OpenPcbLibFileAndSelectedFootprint() }


{***************************************************************************
 * function CALF_CreateNewFeaturesAssembly()
 *  Create all new 2D features on assembly drawing layer.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CALF_CreateNewFeaturesAssembly(    allowNumCommentChars : Integer;
                                        var cnfGalacticInfo      : TStringList;
                                        var trackQueue           : TInterfaceList;
                                        var arcQueue             : TInterfaceList;
                                        var textQueue            : TInterfaceList;
                                        var padQueue             : TInterfaceList;
                                        var regionQueue          : TInterfaceList;
                                        var fillQueue            : TInterfaceList;
                                        var bodyQueue            : TInterfaceList;
                                        var primNames            : TStringList;
                                            )                    : Integer;
var
   courtyardWestMm            : Real;
   courtyardEastMm            : Real;
   courtyardNorthMm           : Real;
   courtyardSouthMm           : Real;
   assemblyWestMm             : Real;
   assemblyEastMm             : Real;
   assemblyNorthMm            : Real;
   assemblySouthMm            : Real;
   footprintType              : TString;
   commentXcoord              : Real;
   refdesXcoord               : Real;
   commentRefDesSouthmostYmm  : Real;
   commentRefDesNorthmostYmm  : Real;
   commentXmm                 : Real;
   commentYmm                 : Real;
   refdesXmm                  : Real;
   refdesYmm                  : Real;
   commentRotXmm              : Real;
   commentRotYmm              : Real;
   refdesRotXmm               : Real;
   refdesRotYmm               : Real;
   spacing                    : Real;
   assyWestBreakAtMm          : Real;
   assyWestResumeAtMm         : Real;
   assyEastBreakAtMm          : Real;
   assyEastResumeAtMm         : Real;
   trackDst                   : IPCB_Track;
   constWidthAssyTextMm       : Real;
   assyTextRotation           : Integer;
   assyTextRotationRot        : Integer;
   commentRotation            : Integer;
   commentRotationRot         : Integer;
   refdesRotation             : Integer;
   refdesRotationRot          : Integer;
   justOneStringInAssyOutline : Boolean;

begin

   { Assume success. }
   result := 0;

   { Compute something that we think is a constant but Altium delphiscript won't let us put under const. }
   constWidthAssyTextMm   := ((constHeightAssyTextMm + constLineWidthAssyTextMm) * constStrokeFontAspectRatio);
   
   {* Retrieve coordinate info from galactic string list. *}
   { Retrieve the bounds for the courtyard outline. }
   CLF_RetrieveBoundingRectangleByNamePrefix({namePrefix} 'Courtyard',
                                             {var} cnfGalacticInfo,
                                             {var boundaryWestMm} courtyardWestMm,
                                             {var boundaryEastMm} courtyardEastMm,
                                             {var boundaryNorthMm} courtyardNorthMm,
                                             {var boundarySouthMm} courtyardSouthMm);

   { Retrieve the bounds for the assembly outline. }
   CLF_RetrieveBoundingRectangleByNamePrefix({namePrefix} 'Assembly',
                                             {var} cnfGalacticInfo,
                                             {var boundaryWestMm} assemblyWestMm,
                                             {var boundaryEastMm} assemblyEastMm,
                                             {var boundaryNorthMm} assemblyNorthMm,
                                             {var boundarySouthMm} assemblySouthMm);

   { Secretly store this information in the footprint for later use by PCB helper scripts. }
   CLF_CreateNewTextFpInfo({name} constClfSecretAssyOutline,
                           {value} FloatToStr(assemblyWestMm) + '_' + FloatToStr(assemblyEastMm) + '_' + FloatToStr(assemblyNorthMm) + '_' + FloatToStr(assemblySouthMm),
                           {var} textQueue,
                           {var} primNames);

   { Update names of tracks that form existing assembly outline. }
   CLF_UpdateTrackNamesFromBoundingRectangle({layer} constNewLayerAssyDrawing,
                                             {namePrefix} 'Assembly',
                                             {var} cnfGalacticInfo,
                                             {var} trackQueue,
                                             {var} primNames);

   { Retrieve other info from galactic string list. }
   footprintType      := cnfGalacticInfo.Values(constGilFootprintType);

   { Retrieve standard text rotation for assembly text. }
   assyTextRotation   := constRotationAssyText;
   assyTextRotationRot:= constRotationAssyText + 180; { TODO:  This is hardcoded assuming standard rotation is 0! }

   { Assign standard rotations for .Comment and .Designator texts. }
   commentRotation           := assyTextRotation;
   commentRotationRot        := assyTextRotationRot;
   refdesRotation            := assyTextRotation;
   refdesRotationRot         := assyTextRotationRot;

   { By default, we assume that both .Designator and .Comment strings will be inside the assembly outline. }
   justOneStringInAssyOutline := False;
   
   
   {* Decide if we have a "small" component that needs to use minimal feature sizes for assembly. *}
   if (  ( (assemblyEastMm - assemblyWestMm) < constThreshSmallAssyOutlineMm ) or
       ( (assemblyNorthMm - assemblySouthMm) < constThreshSmallAssyOutlineMm )  ) then
   begin

      WriteToDebugFile('Hello from CALF_CreateNewFeaturesAssembly().  Using minimal assembly line widths!');

      { Update the width of the assembly lines to use thinner lines for this small footprint. }
      CLF_UpdateTrackWidthFromBoundingRectangle({layer} constNewLayerAssyDrawing,
                                                {namePrefix} 'Assembly',
                                                {newWidthMm} constNewMinWidthAssyDrawingMm,
                                                {var} cnfGalacticInfo,
                                                trackQueue);

   end; { endif }


   {* Compute locations for .Comment and .Designator text strings. *}
   { Ideally we want enough room for the assembly pin 1 marker, a rotated .Designator string,
    the normal .Comment string, and the normal .Designator string. }
   { See if there's enough room for absolutely everything. }
   commentRefDesSouthmostYmm      := assemblySouthMm;
   commentRefDesNorthmostYmm      := assemblyNorthMm;
   WriteToDebugFile('commentRefDesSouthmostYmm is ' + FloatToStr(commentRefDesSouthmostYmm) + '.');
   WriteToDebugFile('commentRefDesNorthmostYmm is ' + FloatToStr(commentRefDesNorthmostYmm) + '.');
   WriteToDebugFile('( commentRefDesSouthmostYmm + (2.0*(constHeightAssyTextMm)) ) is ' + FloatToStr(( commentRefDesSouthmostYmm + (2.0*(constHeightAssyTextMm)) )) + '.');

   { Setup default X coordinates for both.  We'll override later if absolutely needed. }
   commentXmm                    := (assemblyWestMm + constDeltaYInnerAssyToTextMm);
   refdesXmm                     := commentXmm;

   { Setup default X coordinates for rotated .Comment and rotated .Designator texts. }
   commentRotXmm                 := (-1.0 * commentXmm);
   refdesRotXmm                  := (-1.0 * refdesXmm);

   { See if there's enough room for the basic 2 strings. }
   if ( commentRefDesSouthmostYmm + constDeltaYInnerAssyToTextMm + (2.0*(constHeightAssyTextMm + constDeltaYInnerAssyToTextMm)) < commentRefDesNorthmostYmm ) then
   begin
      WriteToDebugFile('Enough room for 2 lines of text with proper spacing!');

      commentYmm                  := 0 - constHeightAssyTextMm - (0.5 * constDeltaYInnerAssyToTextMm);
      refdesYmm                   := 0 + (0.5 * constDeltaYInnerAssyToTextMm);

      { Compute the locations of rotated .Comment and rotated .Designator texts. }
      refdesRotYmm                := commentYmm;
      commentRotYmm               := refdesYmm;

      { Account for offsets needed due to text rotation. }
      { TODO:  This assumes standard rotation is 0! }
      commentRotYmm                 := (commentRotYmm + constHeightAssyTextMm);
      refdesRotYmm                  := (refdesRotYmm + constHeightAssyTextMm);   

   end {nosemi}

   { Else see if we can squeeze in the basic 2 strings. }
   else if ( commentRefDesSouthmostYmm + (2.0*(constHeightAssyTextMm)) < commentRefDesNorthmostYmm ) then
   begin
      WriteToDebugFile('Enough room for 2 lines of text but without proper spacing!');

      { Compute the emergency spacing. }
      spacing                     := ((commentRefDesNorthmostYmm - ( commentRefDesSouthmostYmm + (2.0*(constHeightAssyTextMm)))) * 0.33);
      
      commentYmm                  := commentRefDesSouthmostYmm + spacing;
      refdesYmm                   := commentYmm + (constHeightAssyTextMm + spacing);

      { Compute the locations of rotated .Comment and rotated .Designator texts. }
      refdesRotYmm                := commentYmm;
      commentRotYmm               := refdesYmm;
      
      { Account for offsets needed due to text rotation. }
      { TODO:  This assumes standard rotation is 0! }
      commentRotYmm                 := (commentRotYmm + constHeightAssyTextMm);
      refdesRotYmm                  := (refdesRotYmm + constHeightAssyTextMm);   

   end {nosemi}

   { Else see if we can squeeze in just 1 string. }
   else if ( commentRefDesSouthmostYmm + (1.0*(constHeightAssyTextMm)) < commentRefDesNorthmostYmm ) then
   begin
      WriteToDebugFile('Enough room for just 1 lines of text!');

      { Compute the emergency spacing. }
      spacing                     := ((commentRefDesNorthmostYmm - ( commentRefDesSouthmostYmm + (1.0*(constHeightAssyTextMm)))) * 0.5);

      { Flag that just the .Comment string will be inside the assembly outline. }
      justOneStringInAssyOutline := True;
   
      { We will have .Designator rotated 270 degrees. }
      refdesXmm                   := (courtyardWestMm + (0.5*constDeltaYInnerAssyToTextMm));
      refdesYmm                   := (courtyardNorthMm - (0.5*constDeltaYInnerAssyToTextMm));
      refdesRotation              := 270;
      refdesRotationRot           := 90;

      { Only .Comment will be in the usual place. }
      commentYmm                  := commentRefDesSouthmostYmm + spacing;
      commentXmm                  := (refdesXmm + (0.5*constDeltaYInnerAssyToTextMm) + constHeightAssyTextMm);

      { Compute the locations of rotated .Comment and rotated .Designator texts. }
      { Note:  This is hardcoded assuming a fixed rotation! }
      commentRotYmm               := commentYmm;
      commentRotYmm               := (commentRotYmm + constHeightAssyTextMm);
      commentRotXmm               := (-1.0 * commentXmm);

      { Compute the locations of rotated .Comment and rotated .Designator texts. }
      { Note:  This is hardcoded assuming a fixed rotation! }
      refdesRotXmm                := (courtyardEastMm - (0.5*constDeltaYInnerAssyToTextMm));
      refdesRotYmm                := (courtyardSouthMm + (0.5*constDeltaYInnerAssyToTextMm));

   end {nosemi}

   { Else panic and assume that the user will have to clean up after us. }
   else
   begin
      CLF_WriteToSummaryAndDebugFilesWithStepNum('**Not enough room for .Comment and .Designator assembly text!  User will have to clean up after me!**');

      commentXmm                  := 0;
      commentYmm                  := 0;
      refdesXmm                   := 0;
      refdesYmm                   := 0;

      commentRotXmm               := 0;
      commentRotYmm               := 0;
      refdesRotXmm                := 0;
      refdesRotYmm                := 0;
   end;

   
   {* See if we will have enough room for .Comment string within assembly outlines. *}
   WriteToDebugFile('commentXmm is ' + FloatToStr(commentXmm) + '.');
   WriteToDebugFile('allowNumCommentChars is ' + IntToStr(allowNumCommentChars) + '.');
   WriteToDebugFile('constWidthAssyTextMm is ' + FloatToStr(constWidthAssyTextMm) + '.');
   WriteToDebugFile('assemblyEastMm is ' + FloatToStr(assemblyEastMm) + '.');
   WriteToDebugFile('( (commentXmm + (allowNumCommentChars*constWidthAssyTextMm)) is ' + FloatToStr(( (commentXmm + (allowNumCommentChars*constWidthAssyTextMm)))) + '.');
   if ( (commentXmm + (allowNumCommentChars*constWidthAssyTextMm)) > assemblyEastMm ) then
   begin

      WriteToDebugFile('Need to break assembly outlines to accommodate expansion of .Comment!');

      { Flag that assembly outline east should be broken to accommodate .Comment string expansion. }
      assyEastBreakAtMm             := CLF_MaxReal((commentYmm - constDeltaYInnerAssyToTextMm), assemblySouthMm);

      if (justOneStringInAssyOutline) then
         assyEastResumeAtMm         := CLF_MinReal((commentYmm + constHeightAssyTextMm + constDeltaYInnerAssyToTextMm), assemblyNorthMm) { nosemi }
      else
         assyEastResumeAtMm         := CLF_MinReal((refdesYmm  + constHeightAssyTextMm + constDeltaYInnerAssyToTextMm), assemblyNorthMm);

      { Break the west outline in the same way. }
      assyWestBreakAtMm             := assyEastBreakAtMm;
      assyWestResumeAtMm            := assyEastResumeAtMm;

      {* Break the outer assembly outline, west line, to accommodate .Comment string expansion. *}
      CLF_BreakVerticalTrack({layer} constNewLayerAssyDrawing,
                             {widthMm} constNewWidthAssyDrawingMm,
                             {X1mm} assemblyWestMm,
                             {Y1mm} assemblySouthMm,
                             {X2mm} assemblyWestMm,
                             {Y2mm} assemblyNorthMm,
                             {YbreakMm} assyWestBreakAtMm,
                             {YresumeMm} assyWestResumeAtMm,
                             {var} trackQueue,
                             {var} primNames
                             );

      {* Break the outer assembly outline, east line, to accommodate .Comment string expansion. *}
      CLF_BreakVerticalTrack({layer} constNewLayerAssyDrawing,
                             {widthMm} constNewWidthAssyDrawingMm,
                             {X1mm} assemblyEastMm,
                             {Y1mm} assemblySouthMm,
                             {X2mm} assemblyEastMm,
                             {Y2mm} assemblyNorthMm,
                             {YbreakMm} assyEastBreakAtMm,
                             {YresumeMm} assyEastResumeAtMm,
                             {var} trackQueue,
                             {var} primNames
                             );

      {* Move the .Comment text in X outside the assembly outline to give it as much room as possible to grow. *}
      if (not justOneStringInAssyOutline) then
      begin
         commentXmm                  := (courtyardWestMm + (0.5*constDeltaYInnerAssyToTextMm));
         refdesXmm                   := commentXmm;

         commentRotXmm               := (-1.0 * commentXmm);
         refdesRotXmm                := (-1.0 * refdesXmm);
      end;
      
   end

   { Else flag that we don't need to break assembly outlines. }
   else
   begin
      assyEastBreakAtMm             := 0.0;
      assyEastResumeAtMm            := 0.0;

      assyWestBreakAtMm             := 0.0;
      assyWestResumeAtMm            := 0.0;

   end;

   
   {** Create any new tracks that we wish to create. **}
   

   {** Create any new arcs that we wish to create. **}
   
   {** Create any new texts that we wish to create. **}

   { .Comment string on assembly drawing. }
   CLF_CreateNewText(constNewLayerAssyDrawing,
                     constLineWidthAssyTextMm,
                     constHeightAssyTextMm,
                     commentXmm,
                     commentYmm,
                     commentRotation,                     
                     '.Comment',
                     {var} textQueue,
                     {name} 'Assembly_.Comment',
                     {var} primNames
                     );

   { Secretly store this information in the footprint for later use by PCB helper scripts. }
   CLF_CreateNewTextFpInfo({name} constClfSecretAssyComment,
                           {value} FloatToStr(commentXmm) + '_' + FloatToStr(commentYmm) + '_' + IntToStr(commentRotation) + '_' + FloatToStr(constLineWidthAssyTextMm) + '_' + FloatToStr(constHeightAssyTextMm),
                           {var} textQueue,
                           {var} primNames);

   { .Designator string on assembly drawing. }
   CLF_CreateNewText(constNewLayerAssyDrawing,
                     constLineWidthAssyTextMm,
                     constHeightAssyTextMm,
                     refdesXmm,
                     refdesYmm,
                     refdesRotation,                     
                     '.Designator',
                     {var} textQueue,
                     {name} 'Assembly_.Designator',
                     {var} primNames
                     );

   { Secretly store this information in the footprint for later use by PCB helper scripts. }
   CLF_CreateNewTextFpInfo({name} constClfSecretAssyDesignator,
                           {value} FloatToStr(refdesXmm) + '_' + FloatToStr(refdesYmm) + '_' + IntToStr(refdesRotation) + '_' + FloatToStr(constLineWidthAssyTextMm) + '_' + FloatToStr(constHeightAssyTextMm),
                           {var} textQueue,
                           {var} primNames);


//   {* Store information for rotated .Comment and .Designator strings. }
//   { .Comment string on assembly drawing. }
//   CLF_CreateNewText(constNewLayerAssyDrawing,
//                     constLineWidthAssyTextMm,
//                     constHeightAssyTextMm,
//                     commentRotXmm,
//                     commentRotYmm,
//                     commentRotationRot,                     
//                     'CommentRot',
//                     {var} textQueue,
//                     {name} 'Assembly_.CommentRot',
//                     {var} primNames
//                     );

   { Secretly store this information in the footprint for later use by PCB helper scripts. }
   CLF_CreateNewTextFpInfo({name} constClfSecretAssyCommentRot,
                           {value} FloatToStr(commentRotXmm) + '_' + FloatToStr(commentRotYmm) + '_' + IntToStr(commentRotationRot) + '_' + FloatToStr(constLineWidthAssyTextMm) + '_' + FloatToStr(constHeightAssyTextMm),
                           {var} textQueue,
                           {var} primNames);

//   { .Designator string on assembly drawing. }
//   CLF_CreateNewText(constNewLayerAssyDrawing,
//                     constLineWidthAssyTextMm,
//                     constHeightAssyTextMm,
//                     refdesRotXmm,
//                     refdesRotYmm,
//                     refdesRotationRot,                     
//                     'RefDesRot',
//                     {var} textQueue,
//                     {name} 'Assembly_.DesignatorRot',
//                     {var} primNames
//                     );

   { Secretly store this information in the footprint for later use by PCB helper scripts. }
   CLF_CreateNewTextFpInfo({name} constClfSecretAssyDesignatorRot,
                           {value} FloatToStr(refdesRotXmm) + '_' + FloatToStr(refdesRotYmm) + '_' + IntToStr(refdesRotationRot) + '_' + FloatToStr(constLineWidthAssyTextMm) + '_' + FloatToStr(constHeightAssyTextMm),
                           {var} textQueue,
                           {var} primNames);


   CLF_WriteToSummaryAndDebugFilesWithStepNum('Created .Comment and .Designator text strings on assembly drawing layer for new .PcbLib file.');
   
end; { end CALF_CreateNewFeaturesAssembly() }


{***************************************************************************
 * function CALF_CreateNewFeaturesSilkscreen()
 *  Create all new 2D features on the Silkscreen layer.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CALF_CreateNewFeaturesSilkscreen(var cnfGalacticInfo : TStringList;
                                         var trackQueue      : TInterfaceList;
                                         var arcQueue        : TInterfaceList;
                                         var textQueue       : TInterfaceList;
                                         var padQueue        : TInterfaceList;
                                         var regionQueue     : TInterfaceList;
                                         var fillQueue       : TInterfaceList;
                                         var bodyQueue       : TInterfaceList;
                                         var primNames       : TStringList;
                                             )               : Integer;
var
   rc                    : Integer;
   courtyardWestMm       : Real;
   courtyardEastMm       : Real;
   courtyardNorthMm      : Real;
   courtyardSouthMm      : Real;
   footprintType         : TString;
   pkgDimsTotalWidthMax  : Real;
   pkgDimsTotalLengthMax : Real;

begin

   { Retrieve footprint type. }
   footprintType         := cnfGalacticInfo.Values(constGilFootprintType);
   
   {* Retrieve coordinate info from galactic string list. *}
   { Retrieve the bounds for the courtyard outline. }
   CLF_RetrieveBoundingRectangleByNamePrefix({namePrefix} 'Courtyard',
                                             {var} cnfGalacticInfo,
                                             {var boundaryWestMm} courtyardWestMm,
                                             {var boundaryEastMm} courtyardEastMm,
                                             {var boundaryNorthMm} courtyardNorthMm,
                                             {var boundarySouthMm} courtyardSouthMm);

   { For molded capacitors, break the west silkscreen line to make a triangle to indicate anode (pin 1). }
   if (footprintType = 'Capacitor') then
   begin

      { Retrieve package total width and length parameters. }
      pkgDimsTotalWidthMax  := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsTotalWidthMax));
      pkgDimsTotalLengthMax := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsTotalLengthMax));

      { Create 3 sides of standard bounding rectangle, on north, east, and south sides. }
      CLF_CreateNewTrackPeninsula2({layer} eTopOverlay,
                                   {widthMm} constWidthSilkMm,
                                   {boundaryWestMm} courtyardWestMm,
                                   {boundaryEastMm} courtyardEastMm,
                                   {boundaryNorthMm} courtyardNorthMm,
                                   {boundarySouthMm} courtyardSouthMm,
                                   {omitSide} 'West',
                                   {var} trackQueue,
                                   {namePrefix} 'Silkscreen',
                                   {var} primNames);

      { Create a triangle extending to the west of the courtyard line indicating anode (pin 1). }
      CLF_CreateNewTrack({layer} eTopOverlay,
                         {widthMm} constWidthSilkMm,
                         {X1mm} courtyardWestMm,
                         {X2mm} courtyardWestMm,
                         {Y1mm} courtyardNorthMm,
                         {Y2mm} constYlenMoldCapTriangle,
                         {var} trackQueue,
                         {name} ('Silkscreen-west-1'),
                         {var} primNames);
      
      CLF_CreateNewTrack({layer} eTopOverlay,
                         {widthMm} constWidthSilkMm,
                         {X1mm} courtyardWestMm,
                         {X2mm} (courtyardWestMm-constYlenMoldCapTriangle),
                         {Y1mm} constYlenMoldCapTriangle,
                         {Y2mm} 0,
                         {var} trackQueue,
                         {name} ('Silkscreen-west-2'),
                         {var} primNames);
      
      CLF_CreateNewTrack({layer} eTopOverlay,
                         {widthMm} constWidthSilkMm,
                         {X1mm} (courtyardWestMm-constYlenMoldCapTriangle),
                         {X2mm} courtyardWestMm,
                         {Y1mm} 0,
                         {Y2mm} -1*constYlenMoldCapTriangle,
                         {var} trackQueue,
                         {name} ('Silkscreen-west-3'),
                         {var} primNames);
      
      CLF_CreateNewTrack({layer} eTopOverlay,
                         {widthMm} constWidthSilkMm,
                         {X1mm} courtyardWestMm,
                         {X2mm} courtyardWestMm,
                         {Y1mm} -1*constYlenMoldCapTriangle,
                         {Y2mm} courtyardSouthMm,
                         {var} trackQueue,
                         {name} ('Silkscreen-west-4'),
                         {var} primNames);

      { Create a cross "+" just north of the triangle. }
      CLF_CreateNewTrack({layer} eTopOverlay,
                         {widthMm} constWidthMoldCapCross,
                         {X1mm} (courtyardWestMm-constLenMoldCapCross-constWidthSilkMm),
                         {X2mm} (courtyardWestMm-constWidthSilkMm),
                         {Y1mm} (-1.5*constLenMoldCapCross),
                         {Y2mm} (-1.5*constLenMoldCapCross),
                         {var} trackQueue,
                         {name} ('Silkscreen-cross-1'),
                         {var} primNames);

      CLF_CreateNewTrack({layer} eTopOverlay,
                         {widthMm} constWidthMoldCapCross,
                         {X1mm} (courtyardWestMm-(0.5*constLenMoldCapCross)-constWidthSilkMm),
                         {X2mm} (courtyardWestMm-(0.5*constLenMoldCapCross)-constWidthSilkMm),
                         {Y1mm} (-2*constLenMoldCapCross),
                         {Y2mm} (-1*constLenMoldCapCross),
                         {var} trackQueue,
                         {name} ('Silkscreen-cross-2'),
                         {var} primNames);

      
      { Create a cross "+" just south of the triangle. }
      CLF_CreateNewTrack({layer} eTopOverlay,
                         {widthMm} constWidthMoldCapCross,
                         {X1mm} (courtyardWestMm-constLenMoldCapCross-constWidthSilkMm),
                         {X2mm} (courtyardWestMm-constWidthSilkMm),
                         {Y1mm} (1.5*constLenMoldCapCross),
                         {Y2mm} (1.5*constLenMoldCapCross),
                         {var} trackQueue,
                         {name} ('Silkscreen-cross-3'),
                         {var} primNames);

      CLF_CreateNewTrack({layer} eTopOverlay,
                         {widthMm} constWidthMoldCapCross,
                         {X1mm} (courtyardWestMm-(0.5*constLenMoldCapCross)-constWidthSilkMm),
                         {X2mm} (courtyardWestMm-(0.5*constLenMoldCapCross)-constWidthSilkMm),
                         {Y1mm} (2*constLenMoldCapCross),
                         {Y2mm} (1*constLenMoldCapCross),
                         {var} trackQueue,
                         {name} ('Silkscreen-cross-4'),
                         {var} primNames);

      
      { Create rectangular fill on silkscreen north of the component (as an extension of the anode bar on component itself). }
      CLF_CreateNewFill({layer} eTopOverlay,
                        {boundaryWestMm} (-0.5 * pkgDimsTotalLengthMax) + constCompBodyMoldCapAnodeMarkLen + constCompBodyPinThicknessMoldMm,
                        {boundaryEastMm} (-0.5 * pkgDimsTotalLengthMax) + (2*constCompBodyMoldCapAnodeMarkLen) + constCompBodyPinThicknessMoldMm,
                        {boundaryNorthMm} (0.5 * pkgDimsTotalWidthMax) + constLenAnodeExtMoldCap,
                        {boundarySouthMm} (0.5 * pkgDimsTotalWidthMax),
                        {var} fillQueue,
                        {name} ('Silkscreen-anode-ext-1'),
                        {var} primNames);
      
      CLF_CreateNewFill({layer} eTopOverlay,
                        {boundaryWestMm} (-0.5 * pkgDimsTotalLengthMax) + constCompBodyMoldCapAnodeMarkLen + constCompBodyPinThicknessMoldMm,
                        {boundaryEastMm} (-0.5 * pkgDimsTotalLengthMax) + (2*constCompBodyMoldCapAnodeMarkLen) + constCompBodyPinThicknessMoldMm,
                        {boundaryNorthMm} (-0.5 * pkgDimsTotalWidthMax),
                        {boundarySouthMm} (-0.5 * pkgDimsTotalWidthMax) - constLenAnodeExtMoldCap,
                        {var} fillQueue,
                        {name} ('Silkscreen-anode-ext-2'),
                        {var} primNames);
      
   end

   { Else create the full rectangle. }
   else
   begin

      { Create a set of tracks on the silkscreen layer to sketch the extent of the component. }
      CLF_CreateNewTrackRectangle2({layer} eTopOverlay,
                                   {widthMm} constWidthSilkMm,
                                   {boundaryWestMm} courtyardWestMm,
                                   {boundaryEastMm} courtyardEastMm,
                                   {boundaryNorthMm} courtyardNorthMm,
                                   {boundarySouthMm} courtyardSouthMm,
                                   {var} trackQueue,
                                   {namePrefix} 'Silkscreen',
                                   {var} primNames);

   end; { endelse }

end; { end CALF_CreateNewFeaturesSilkscreen() }


{***************************************************************************
 * function CALF_CreateNewFeatures()
 *  Create all new 2D features that we want to have in the destination footprint.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CALF_CreateNewFeatures(    scriptsPath          : TDynamicString;
                                    projectPath          : TDynamicString;
                                    allowNumCommentChars : Integer;
                                var cnfGalacticInfo      : TStringList;
                                var trackQueue           : TInterfaceList;
                                var arcQueue             : TInterfaceList;
                                var textQueue            : TInterfaceList;
                                var padQueue             : TInterfaceList;
                                var regionQueue          : TInterfaceList;
                                var fillQueue            : TInterfaceList;
                                var bodyQueue            : TInterfaceList;
                                var primNames            : TStringList;
                                    )                    : Integer;
var
   i        : Integer;

begin

   { Assume success. }
   result := 0;

   {* Create all new features on silkscreen layer. *}
   CALF_CreateNewFeaturesSilkscreen({var} cnfGalacticInfo,
                                   {var} trackQueue,
                                   {var} arcQueue,
                                   {var} textQueue,
                                   {var} padQueue,
                                   {var} regionQueue,
                                   {var} fillQueue,
                                   {var} bodyQueue,
                                   {var} primNames
                                   );

   {* Create all new features on assembly layer. *}
   CALF_CreateNewFeaturesAssembly(allowNumCommentChars,
                                  {var} cnfGalacticInfo,
                                  {var} trackQueue,
                                  {var} arcQueue,
                                  {var} textQueue,
                                  {var} padQueue,
                                  {var} regionQueue,
                                  {var} fillQueue,
                                  {var} bodyQueue,
                                  {var} primNames
                                  );

{$IfDef foo29}
   { We don't try to create pin landing drawings for Altium library-derived passive footprints. }
   {* Create all new features on pin landing layer. *}
   CLF_CreateNewFeaturesPinLand({var} cnfGalacticInfo,
                                {var} trackQueue,
                                {var} arcQueue,
                                {var} textQueue,
                                {var} padQueue,
                                {var} regionQueue,
                                {var} fillQueue,
                                {var} bodyQueue,
                                {var} primNames
                                );
{$EndIf foo29}

   {* Create all new features on courtyard layer. *}
   CLF_CreateNewFeaturesCourtyard({var} cnfGalacticInfo,
                                  {var} trackQueue,
                                  {var} arcQueue,
                                  {var} textQueue,
                                  {var} padQueue,
                                  {var} regionQueue,
                                  {var} fillQueue,
                                  {var} bodyQueue,
                                  {var} primNames
                                  );

   {* Create all new features on courtbody layer. *}
   CLF_CreateNewFeaturesCourtbody({var} cnfGalacticInfo,
                                  {var} trackQueue,
                                  {var} arcQueue,
                                  {var} textQueue,
                                  {var} padQueue,
                                  {var} regionQueue,
                                  {var} fillQueue,
                                  {var} bodyQueue,
                                  {var} primNames
                                  );

end; { end CALF_CreateNewFeatures() }


{***************************************************************************
 * function CALF_CopyFootprintFromSourcePcbLib()
 *  Copy, modify, and customize the footprint from supplied Altium library.
 *  Queue all these features up for later use (eg. writing to one or more new .PcbLib files).
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CALF_CopyFootprintFromSourcePcbLib(    sourcePcbLib    : IPCB_Library;
                                            var trackQueue      : TInterfaceList;
                                            var arcQueue        : TInterfaceList;
                                            var textQueue       : TInterfaceList;
                                            var padQueue        : TInterfaceList;
                                            var regionQueue     : TInterfaceList;
                                            var fillQueue       : TInterfaceList;
                                            var bodyQueue       : TInterfaceList;
                                            var primNames       : TStringList;
                                            var cnfGalacticInfo : TStringList;
                                                )               : Integer;
                                                                
var                                                             
   i                                                            : Integer;
   k              : Integer;
   rc             : Integer;
   boardSrc       : IPCB_Board;
                                                     
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CALF_CopyFootprintFromSourcePcbLib()');
   CALF_UpdateGuiStatusMessage('Proceeding to copy and copy-modify features from imported .PcbDoc file.');
   
   {* Configure old layer / old width and new layer / new width for all Altium source library tracks. *}

   { Destination is courtyard. }
   cnfGalacticInfo.add(constGilModTracksOldLayer1   + constStringEquals + IntToStr(constOldLayerCourtyard));
   cnfGalacticInfo.add(constGilModTracksOldWidthMm1 + constStringEquals + FloatToStr(constOldWidthCourtyardMm));
   cnfGalacticInfo.add(constGilModTracksNewLayer1   + constStringEquals + IntToStr(constNewLayerCourtyard));
   cnfGalacticInfo.add(constGilModTracksNewWidthMm1 + constStringEquals + FloatToStr(constNewWidthCourtyardMm));
   
   { Destination is assembly drawing. }
   cnfGalacticInfo.add(constGilModTracksOldLayer2   + constStringEquals + IntToStr(constOldLayerAssyDrawing));
   cnfGalacticInfo.add(constGilModTracksOldWidthMm2 + constStringEquals + FloatToStr(constOldWidthAssyDrawingMm));
   cnfGalacticInfo.add(constGilModTracksNewLayer2   + constStringEquals + IntToStr(constNewLayerAssyDrawing));
   cnfGalacticInfo.add(constGilModTracksNewWidthMm2 + constStringEquals + FloatToStr(constNewWidthAssyDrawingMm));
   
   { Destination is none (being deleted). }
   cnfGalacticInfo.add(constGilModTracksOldLayer3   + constStringEquals + IntToStr(constOldLayerSilkscreen));
   cnfGalacticInfo.add(constGilModTracksOldWidthMm3 + constStringEquals + FloatToStr(constOldWidthSilkscreenMm));
   cnfGalacticInfo.add(constGilModTracksNewLayer3   + constStringEquals + IntToStr(0));
   cnfGalacticInfo.add(constGilModTracksNewWidthMm3 + constStringEquals + FloatToStr(0));

   { Destination is none (not applicable). }
   cnfGalacticInfo.add(constGilModTracksOldLayer4   + constStringEquals + IntToStr(0));
   cnfGalacticInfo.add(constGilModTracksOldWidthMm4 + constStringEquals + FloatToStr(0));
   cnfGalacticInfo.add(constGilModTracksNewLayer4   + constStringEquals + IntToStr(0));
   cnfGalacticInfo.add(constGilModTracksNewWidthMm4 + constStringEquals + FloatToStr(0));

  
   { Retrieve a reference to current PcbLib "board". }
   boardSrc        := sourcePcbLib.Board;

   {** Copy all pads from source PcbDoc file to destination library component. **}
   rc := CLF_CopyAllPads(boardSrc,
                         {var} cnfGalacticInfo,
                         {var} padQueue,
                         {var} primNames
                         );                        


{$IfDef foo29}
   { At the moment, we don't need to do this for Altium library-derived passive footprints. }
   { Create bounds for all groups of pads.  These will be used later for finishing silkscreen touches, etc. }
   CLF_CreatePadGroupBounds({var} cnfGalacticInfo, 
                            padQueue,
                            {var} trackQueue,
                            {var} primNames
                            );
{$EndIf foo29}

   {** Copy all regions from source PcbDoc file to destination library component. **}
   rc := CLF_CopyAllRegions(boardSrc,
                            {var} cnfGalacticInfo,
                            {var} regionQueue,
                            {var} fillQueue,
                            {var} primNames,
                            {var} padQueue
                            );                            
   
   {** Copy all tracks from source PcbDoc file to destination library component. **}
   rc := CLF_CopyAllTracks(boardSrc,
                           {var} cnfGalacticInfo,
                           {var} trackQueue,
                           {var} primNames
                           );

{$IfDef foo29}
   { At the moment, we don't need to do this for Altium library-derived passive footprints. }
   {** Copy all arcs from source PcbDoc file to destination library component. **}
   rc := CLF_CopyAllArcs(boardSrc,
                         {var} cnfGalacticInfo,
                         {var} arcQueue,
                         {var} primNames
                         );
{$EndIf foo29}

   CLF_WriteToSummaryAndDebugFilesWithStepNum('Copied and copy-modified all regions, pads, tracks, and arcs from .PcbDoc to queue for new .PcbLib file.');

   { Note:  We're intentionally NOT copying any text, since the only text that
    Mentor LP Wizard generates is one that we want to nuke anyway! }

end; { end CALF_CopyFootprintFromSourcePcbLib() }


{***************************************************************************
 * function CALF_CreateNewFeatures2dAndModify()
 *  Add all new 2D features and modify them as needed.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CALF_CreateNewFeatures2dAndModify(    scriptsPath          : TDynamicString;
                                               projectPath          : TDynamicString;
                                               sourcePcbLib         : IPCB_Library;
                                               allowNumCommentChars : Integer;
                                           var trackQueue           : TInterfaceList;
                                           var arcQueue             : TInterfaceList;
                                           var textQueue            : TInterfaceList;
                                           var padQueue             : TInterfaceList;
                                           var regionQueue          : TInterfaceList;
                                           var fillQueue            : TInterfaceList;
                                           var bodyQueue            : TInterfaceList;
                                           var primNames            : TStringList;
                                           var cnfGalacticInfo      : TStringList;
                                               )                    : Integer;

var
   i                 : Integer;
   rc                : Integer;
   srcPcbLibFilePath : TString;
                                                     
begin

   { Assume success. }
   result := 0;

   {* Close the source PcbLib document. *}
   { Retrieve info from galactic string list. }
   { Note:  We should be able to get this via sourcePcbLib.DM_FullPath or some such, but it doesn't seem to work.
    Not to worry, though, since we had this information previously. }
   srcPcbLibFilePath := cnfGalacticInfo.Values(constGilSpecAltPcbLibFilePath);

   { Close the source PcbLib document. }
   ResetParameters;
   AddStringParameter('ObjectKind', 'Document');
   AddStringParameter('FileName', srcPcbLibFilePath);
   RunProcess('WorkspaceManager:CloseObject');

   { Try to close all project documents. }
   ResetParameters;
   AddStringParameter('ObjectKind', 'FocusedProjectDocuments');
   RunProcess('WorkspaceManager:CloseObject');

   
   {** Create all new 2D features that we want in destination library component. **}
   CALF_UpdateGuiStatusMessage('Proceeding to add new 2D SPI-specific features to new footprint.');
   rc := CALF_CreateNewFeatures(scriptsPath,
                                projectPath,
                                allowNumCommentChars,
                                {var} cnfGalacticInfo,
                                {var} trackQueue,
                                {var} arcQueue,
                                {var} textQueue,
                                {var} padQueue,
                                {var} regionQueue,
                                {var} fillQueue,
                                {var} bodyQueue,
                                {var} primNames
                                );

{$IfDef foo29}
   { At the moment, we don't need to do this for Altium library-derived passive footprints. }
   {** Do one last pass through and modify any features we wish to modify. **}
   rc := CLF_FinalModifyOfFeatures({var} cnfGalacticInfo,
                                   {var} trackQueue,
                                   {var} arcQueue,
                                   {var} textQueue,
                                   {var} padQueue,
                                   {var} regionQueue,
                                   {var} fillQueue,
                                   {var} bodyQueue,
                                   {var} primNames
                                   );
{$EndIf foo29}

end; { end CALF_CreateNewFeatures2dAndModify() }


{***************************************************************************
 * function CALF_CreateAllNewFootprints()
 *  Create all new footprints.  This is typically just a single, unmarked
 *  footprint.  But it may also be the unmarked footprint plus one or more
 *  derived, marked footprints.
 *
 *  Call CALF_CopyFootprintFromSourcePcbLib() to copy and copy-modify all features
 *  (tracks, pads, arcs, etc.) from footprint (PcbDoc) that gets imported from
 *  LP Wizard.  Also add various features of our own choosing.
 *
 *  Call CALF_CreateNewFootprintFromQueuedFeatures() to create a new footprint based
 *  on all the features (tracks, pads, arcs, etc.) that we already have queued, plus
 *  the specified libFileName and pcbLibFileName. 
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CALF_CreateAllNewFootprints(    scriptsPath     : TDynamicString;
                                         projectPath     : TDynamicString;
                                         sourcePcbLib    : IPCB_Library;
                                         project         : IProject;
                                     var cnfGalacticInfo : TStringList;
                                         )               : Integer;

var
   i                    : Integer;
   k                    : Integer;
   rc                   : Integer;
   trackQueue           : TInterfaceList;
   arcQueue             : TInterfaceList;
   textQueue            : TInterfaceList;
   padQueue             : TInterfaceList;
   regionQueue          : TInterfaceList;
   fillQueue            : TInterfaceList;
   bodyQueue            : TInterfaceList;
   primNames            : TStringList;
   libSubDir            : TString;
   libFileName          : TString;
   libFileNameSuffix    : TString;
   libFileNameSuffixOld : TString;
   libDescription       : TString;
//   libHeightMm        : Real;
   pcbLibFileName       : TString;
   csvReportStrs        : TStringList;
   csvReportOld         : TStringList;
   csvReportOut         : TStringList;
   reportFilePath       : TString;
   csvReportFilePath    : TString;
   derivedFpNum         : Integer;
   currDerivedFpNum     : Integer;
   doBaseFp             : Boolean;
   haveDerivedFp        : Boolean;
   hasThVias            : Boolean;
   packageColorCode     : TString;
   packageMarkingText   : TString;
   trackQueue2dCount    : Integer;
   textQueue2dCount     : Integer;
   hasEp                : Boolean;
   stepFilePath         : TString;
   allowNumCommentChars : Integer;
   packageHeightMm      : Real;
   derivedByMethod      : TString;
   baseHeightMm         : Real;
   baseColorCode        : TString;
   baseMarkingText      : TString;
   baseStepFilePath     : TString;
   fcstdFilePath        : TString;
   iniFilePath          : TString;
   logFilePath          : TString;
   pcbDocOrStepFilePath : TString;
   deletedStepFile      : Boolean;
   mode                 : Boolean;
   filesAreReverted     : Boolean;
                                                     
begin

   { Assume success. }
   result := 0;

   { Until we find out otherwise, we know about 0 derived footprints. }
   derivedFpNum    := 0;
   currDerivedFpNum:= 0;

   { Flag that we are working on the base footprint. }
   doBaseFp        := True;
   
   WriteToDebugFile('Hello from CALF_CreateAllNewFootprints()');
   
   { Create various lists to queue all the features we wish to copy to or create in
    the destination library component. }
   trackQueue     := TInterfaceList.Create();
   arcQueue       := TInterfaceList.Create();
   textQueue      := TInterfaceList.Create();
   padQueue       := TInterfaceList.Create();
   regionQueue    := TInterfaceList.Create();
   fillQueue      := TInterfaceList.Create();
   bodyQueue      := TInterfaceList.Create();

   { Create a stringlist to hold names we assign to various primitives (tracks, arcs, etc.). }
   primNames      := TStringList.Create();
   primNames.Add('**Unknown!**');
   
   { Store captions for all pad groups. }
   { Note:  Must have 'pads' prefix as below! }
   { Warning:  Be sure the ordering here matches that of the defined constants at the top of this code file! }
   primNames.Add('padsWest');
   primNames.Add('padsEast');
   primNames.Add('padsNorth');
   primNames.Add('padsSouth');
   primNames.Add('padsCenter');
   primNames.Add('padsEp');
   primNames.Add('padsUnknown');

   { Create a stringlist to hold our .csv report file. }
   csvReportStrs     := TStringList.Create();

   { Record that this footprint has no EP pin, since we're only dealing with passives in this script. }
   hasEp                 := False;
   cnfGalacticInfo.add(constGilHasEp + constStringEquals + BoolToStr(hasEp));

   { Call CALF_CopyFootprintFromSourcePcbLib() to copy and copy-modify all features
    (tracks, pads, arcs, etc.) from source PcbLib file. }
   CALF_CopyFootprintFromSourcePcbLib(sourcePcbLib,
                                      {var} trackQueue,
                                      {var} arcQueue,
                                      {var} textQueue,
                                      {var} padQueue,
                                      {var} regionQueue,
                                      {var} fillQueue,
                                      {var} bodyQueue,
                                      {var} primNames,
                                      {var} cnfGalacticInfo);

   
   {** Get all parameters ready to go for creating new footprint. **}
   { User should have specified component height in .xml file. }
   if (cnfGalacticInfo.Values(constGilSpecCompHeight) = '') then
      CALF_Abort('Component height must be specified in .xml file!');
      
   { User should have specified STEP model in .xml file. }
   if (cnfGalacticInfo.Values(constGilStepFilePath) = '') then
      CALF_Abort('STEP model must be specified in .xml file!');

   { Retrieve baseline component height. }
   packageHeightMm       := StrToFloat(cnfGalacticInfo.Values(constGilSpecCompHeight));
   baseHeightMm          := packageHeightMm;

   { Configure default package color code and package marking. }
   packageColorCode      := 'Blk';   { Configure for black component body. }
   packageMarkingText    := '';

   { See if the user specified a color code (eg. "Rst" for rust orange for chip caps). }
   if (CLF_IsNameInStringList({name} constGilSpecCompColorCode,
       {stringlist} cnfGalacticInfo)) then
      packageColorCode   := cnfGalacticInfo.Values(constGilSpecCompColorCode);
      
   baseColorCode         := packageColorCode;
   baseMarkingText       := packageMarkingText;

   { Retrieve baseline user-specified STEP file. }
   stepFilePath          := cnfGalacticInfo.Values(constGilStepFilePath);
   baseStepFilePath      := stepFilePath;
   
   
   { Call CALF_CalculateLibFileNameSuffixAndDescription() to get the name, desciption, and
    various useful parameters for the footprint we're about to create. }
   CALF_CalculateLibFileNameSuffixAndDescription(packageHeightMm,
                                                 packageColorCode,
                                                 packageMarkingText,
                                                 cnfGalacticInfo,
                                                 {var} libFileName,
                                                 {var} libFileNameSuffix,
                                                 {var} libDescription,
                                                 {var} allowNumCommentChars);
   
   { Add and modify various 2D features of our own choosing. }
   CALF_CreateNewFeatures2dAndModify(scriptsPath,
                                     projectPath,
                                     sourcePcbLib,
                                     allowNumCommentChars,
                                     {var} trackQueue,
                                     {var} arcQueue,
                                     {var} textQueue,
                                     {var} padQueue,
                                     {var} regionQueue,
                                     {var} fillQueue,
                                     {var} bodyQueue,
                                     {var} primNames,
                                     {var} cnfGalacticInfo);
                                      
   
   { Record the state of the track queue, now that we're done with copying, modifying,
    and creating all 2D features.  Currently the 3D operations will create additional
    tracks and texts.  3D operations will not create arcs, pads, regions, etc. }
   trackQueue2dCount := trackQueue.Count;
   textQueue2dCount  := textQueue.Count;
   
   
   {** Loop over all the footprints we wish to create, including the base footprint. **}
   repeat
   begin
   
      { TODO:  If we want to invent some more time-efficient way of recording the results
       of base footprint csvReportStrs, then we should take a snapshot of it here. }

      { TODO:  Figure out how to take snapshot of summary file before working on any derived footprints. }
      

      { See if this is the base footprint that we're working on. }
      if (doBaseFp) then
      begin

         { We have already retrieved the results of the previous call to CALF_CalculateLibFileNameSuffixAndDescription(). }

         { libFileName and libDescription are already valid for the base footprint, so nothing to do for them. }

         { The base footprint shall live in the project directory, not any subdir thereof. }
         libSubDir      := '';
      
      end { endif }

      { Else create values appropriate to derived footprint. }
      else
      begin

         { Call CALF_CalculateLibFileNameSuffixAndDescription() to get the name, desciption, and
          various useful parameters for the footprint we're about to create. }
         CALF_CalculateLibFileNameSuffixAndDescription(packageHeightMm,
                                                       packageColorCode,
                                                       packageMarkingText,
                                                       cnfGalacticInfo,
                                                       {var} libFileName,
                                                       {var} libFileNameSuffix,
                                                       {var} libDescription,
                                                       {var} allowNumCommentChars);
         
         { TODO:  Configure any subdirectory (eg. "Derived/") that we want to use for this footprint. }
         libSubDir      := 'Derived\';
      
      end; { endelse }

      { Construct the full path and name and extension for new PcbLib file that we are about to create. }
      pcbLibFileName := (projectPath + libSubDir + libFileName + '.PcbLib');

      { Write name of upcoming footprint to summary file. }
      CLF_WriteToSummaryAndDebugFilesWithStepNum('About to start work on footprint "' + libFileName + '".');
      
      { Construct the full path and name and extension for new csvReport file that we are about to create. }
      csvReportFilePath       := (projectPath + libSubDir + libFileName + '_Features_Report.csv');
     
      { Construct the full path and name and extension for new script report file that we are about to create. }
      reportFilePath       := (projectPath + libSubDir + libFileName + '_Script_Report.txt');

      { Setup some constants that are needed to create thermal body later on. }
      cnfGalacticInfo.add(constGilPkgDimsStandoffMin + constStringEquals + '0.0');
      cnfGalacticInfo.add(constGilPkgDimsHeightMax   + constStringEquals + FloatToStr(packageHeightMm));         

      
      {** Read in old CSV report file for later comparison with new CSV report file. **}
      CLF_RevertOldCsvFileAndReadIn(projectPath,
                                    scriptsPath,
                                    {pcbLibOrFcstdFilePath} pcbLibFileName,
                                    {reportOrIniFilePath} reportFilePath,
                                    stepFilePath,
                                    {csvOrLogFilePath} csvReportFilePath,
                                    {var} csvReportOld,
                                    {var} deletedStepFile);
      
      {** Create all new 3D features that we want in destination library component. **}
      CALF_UpdateGuiStatusMessage('Proceeding to add new 3D SPI-specific features to new footprint.');
      rc := CLF_CreateNewFeatures3d(scriptsPath,
                                    projectPath,
                                    {libHeightMm} packageHeightMm,
                                    stepFilePath,                                         
                                    {allow3dExtrusion} False,
                                    doBaseFp,
                                    currDerivedFpNum,
                                    trackQueue2dCount,
                                    textQueue2dCount,
                                    {var} cnfGalacticInfo,
                                    {var} trackQueue,
                                    {var} arcQueue,
                                    {var} textQueue,
                                    {var} padQueue,
                                    {var} regionQueue,
                                    {var} fillQueue,
                                    {var} bodyQueue,
                                    {var} primNames,
                                    {var} csvReportStrs                               
                                    );
      
      { Secretly store library name in the footprint for later use by PCB helper scripts. }
      CLF_CreateNewTextFpInfo({name} constClfSecretInfoLibName,
                              {value} libFileName,
                              {var} textQueue,
                              {var} primNames);

      { Call CLF_CreateNewFootprintFromQueuedFeatures() to create a new footprint based
       on all the features (tracks, pads, arcs, etc.) that we already have queued, plus
       the specified libFileName and pcbLibFileName. }
      CLF_CreateNewFootprintFromQueuedFeatures(project,
                                               trackQueue,
                                               arcQueue,
                                               textQueue,
                                               padQueue,
                                               regionQueue,
                                               fillQueue,
                                               bodyQueue,
                                               libFileName,
                                               libDescription,
                                               {libHeightMm} packageHeightMm,
                                               pcbLibFileName,
                                               {var} primNames,
                                               {var} csvReportStrs,
                                               {var} cnfGalacticInfo);

      { Add all generated files (PcbLib + report file + csv file) to project and to svn. }
      fcstdFilePath := ''; 		{ We don't have this file.  Set to null string. }
      iniFilePath := ''; 		{ We don't have this file.  Set to null string. }
      logFilePath := ''; 		{ We don't have this file.  Set to null string. }
      CLF_AddGeneratedDocumentsToProjectAndSvn(project,
                                               projectPath,
                                               scriptsPath,
                                               libSubDir,
                                               libFileName,
                                               pcbLibFileName,
                                               csvReportFilePath,
                                               fcstdFilePath,
                                               iniFilePath,
                                               stepFilePath,
                                               logFilePath,
                                               reportFilePath,
                                               cnfGalacticInfo);

      
      { Generate CSV report file to detail all the features (tracks, arcs, texts, etc.) present in this
       footprint, along with all their associated parameters. }
      CALF_UpdateGuiStatusMessage('Proceeding to write CSV report file to disk....');
      CLF_CsvReportFileWrite(csvReportStrs,
                             {var} csvReportOut,
                             csvReportFilePath);


      {** Save a copy of summary file to a per-PcbLib file name. **}
      { Note that this file has already been touched and added to project and svn, so we only need to write it now. }

      { (Re-)Write a copy of our summary file to the reportFileName, which is named similarly to the .PcbLib file. }
      SummaryMessages.SaveToFile(reportFilePath);


      { See if the CSV report file has changed compared to before this script run.  If it has not,
       then proceed to revert all generated files, so that user is not tempted to checkin effectively
       unchanged binary file .PcbLib. }
      mode                 := False; 	{ "Footprint" mode }
      pcbDocOrStepFilePath := ''; 		{ We don't have this file.  Set to null string. }
      CLF_RevertGeneratedFilesIfNeeded(projectPath,
                                       scriptsPath,
                                       {pcbLibOrFcstdFilePath} pcbLibFileName,
                                       {reportOrIniFilePath} reportFilePath,
                                       pcbDocOrStepFilePath,
                                       {csvOrLogFilePath} csvReportFilePath,
                                       mode,
                                       {var} {csvOrLogFileOld} csvReportOld,
                                       {var} {csvOrLogFileOut} csvReportOut,
                                       {var} filesAreReverted);

      for i := 0 to (cnfGalacticInfo.Count - 1) do
      begin
         
//         WriteToDebugFile(' cnfGalacticInfo[' + IntToStr(i) + '] is "' + cnfGalacticInfo[i] + '".');
         
      end; { endfor i }

         
      {** See if we have a(nother) derived footprint that we have been ordered to create. **}
      WriteToDebugFile('Checking for order to derive new footprint # ' + IntToStr(derivedFpNum) + '.');
      haveDerivedFp       :=  CLF_IsNameInStringList({name} (constGilDeriveByMethod + IntToStr(derivedFpNum)),
                                                     {stringlist} cnfGalacticInfo);

      { Configure color, marking, and height to use the baseline values until & unless we find out otherwise. }
      packageColorCode      := baseColorCode;
      packageMarkingText    := baseMarkingText;
      packageHeightMm       := baseHeightMm;    { Configure for base component height. }
      stepFilePath          := baseStepFilePath;
      
      { If needed, increment the number of derived footprints we know about. }
      if (haveDerivedFp) then
      begin

         { Retrieve the type of derived footprint that we need to deal with. }
         derivedByMethod    := cnfGalacticInfo.Values(constGilDeriveByMethod + IntToStr(derivedFpNum));

         { Handle a derived by height footprint. }
         if (derivedByMethod = constGilDeriveByHeight) then
         begin

            { Retrieve the height of this new derived footprint. }
            packageHeightMm     := cnfGalacticInfo.Values(constGilDeriveHeight + IntToStr(derivedFpNum));

            { Make sure that the .xml file specified a new STEP file to go along with the new height. }
            if (not CLF_IsNameInStringList({name} (constGilDerivedStepFilePath + IntToStr(derivedFpNum)),
                {stringlist} cnfGalacticInfo)) then
               CALF_Abort('Sorry, but .xml file MUST specify a new STEP file to go along with a derived footprint with different height!');

            { Retrieve the STEP model for this new derived footprint. }
            stepFilePath     := cnfGalacticInfo.Values(constGilDerivedStepFilePath + IntToStr(derivedFpNum));

            WriteToDebugFile(' stepFilePath is "' + stepFilePath + '".');
            WriteToDebugFile(' baseStepFilePath is "' + baseStepFilePath + '".');

            { Make sure this is a different STEP model than the base one! }
            if (stepFilePath = baseStepFilePath) then
               CALF_Abort('Sorry, but .xml file MUST specify a different STEP file (than the base STEP file) to go along with a derived footprint with different height!');
               
         end { endif }

         { Handle a derived by marking text footprint. }
         else if (derivedByMethod = constGilDeriveByMarkingText) then
         begin

            { Retrieve the desired marking text of this new derived footprint. }
            packageMarkingText    := cnfGalacticInfo.Values(constGilDeriveMarkingText + IntToStr(derivedFpNum));
            
         end { end elsif }

         else
            CALF_Abort('Unsupported method to derive new footprint: "' + derivedByMethod + '".');

         
         { Increment derived footprint number. }
         currDerivedFpNum:= derivedFpNum;
         derivedFpNum    := derivedFpNum + 1;
         
         { Flag that we are no longer working on the base footprint. }
         doBaseFp        := False;

         { TODO:  Record count of csvReportStrs here, before working on any derived footprints. }
         { For now, just clear it and force/assume that it will be completely regenerated for each derived footprint. }
         csvReportStrs.Clear();

      end; { endif haveDerivedFp }

      { If we have a whole lot of derived footprints, we need to start closing them as we go along. }
      if (derivedFpNum > 1) then
      begin

//         ShowMessage('Attempting to close project documents.');
         
         ResetParameters;
         AddStringParameter('ObjectKind', 'Document');
         AddStringParameter('FileName', pcbLibFileName);
         RunProcess('WorkspaceManager:CloseObject');

         { Try to close all project documents. }
         ResetParameters;
         AddStringParameter('ObjectKind', 'FocusedProjectDocuments');
         RunProcess('WorkspaceManager:CloseObject');
         
      end; { endif }
      
   end { end repeat loop over all footprints we wish to create. }
   until (not haveDerivedFp);   { Loop until we have no more derived footprints to create. }
   
   WriteToDebugFile('Have no more footprints to derive.');

   { Free the various lists. }
   trackQueue.Free();
   arcQueue.Free();
   textQueue.Free();
   padQueue.Free();
   regionQueue.Free();
   fillQueue.Free();
   bodyQueue.Free();

   primNames.Free();

   csvReportStrs.Free();

end; { end CALF_CreateAllNewFootprints() }

{***************************************************************************
 * END Footprint-related functions.
 ***************************************************************************}


{***************************************************************************
 * procedure CNF_CleanupAltLibFootprint()
 *  This is the effective main program for the script.
 *
 *  Returns:  (nothing--procedure, not a function).
 ***************************************************************************}
procedure CNF_CleanupAltLibFootprint(foo : Integer);
var
   WorkSpace         : IWorkSpace;
   project           : IProject;
   projectPath       : TDynamicString;
   projOutPath       : TDynamicString;
   projectName       : TDynamicString;
   projLogPath       : TDynamicString;
   scriptsPath       : TDynamicString;
   document          : IDocument;
   lpWizardFilesPath : TString;
   sourcePcbLib      : IDocument;
   importedProject   : IProject;
   timestamp         : TDynamicString;
   startTime         : TDateTime;
   endTime           : TDateTime;
   rc                : Integer;
   cnfGalacticInfo   : TStringList;
   i                 : Integer;
   padsFilePath      : TString;
   commandFilePath   : TString;

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
   whichScriptIsThis := constWhichScriptCalf;

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

   { Record the wall clock time when we started the real work of this script. }
   startTime := Now();

   { Delete useless ProjectLogs/ directory that InitScript() insists on creating for us. }
   { Note:  We're not bothering to test for success.  If there are files in there, it will fail, and that's ok. }
   RemoveDir(projLogPath);

   { Create stringlist to hold various coordinates and flags that will come up along the way. }
   cnfGalacticInfo := TStringList.Create;

   { Try to close all project documents for the actual project. }
   CALF_UpdateGuiStatusMessage('Closing all documents in project before starting script.');
   ResetParameters;
   AddStringParameter('ObjectKind', 'FocusedProjectDocuments');
   RunProcess('WorkspaceManager:CloseObject');

   { Ask the user to choose the .xml command file that describes which footprint we want to ready for use at SPI. }
   CALF_ChooseXmlCmdFile(projectPath,
                         {var} cnfGalacticInfo,
                         {var} commandFilePath);
   
   {** Parse the footprint-specific .xml command file (if one exists) **}
   CLF_ParseCommandFile(projectPath,
                        commandFilePath,
                        {var} cnfGalacticInfo);
   
   {** Open source PcbLib file and selected footprint therein. **}
   CALF_OpenPcbLibFileAndSelectedFootprint({var} cnfGalacticInfo,
                                           {var} sourcePcbLib);

   {** Add source documents (.xml file, source PcbLib file) to project and to svn. **}
   CALF_AddSourceDocumentsToProjectAndSvn(project,
                                          projectPath,
                                          scriptsPath,
                                          commandFilePath,
                                          sourcePcbLib,
                                          {var} cnfGalacticInfo);

   {** Copy the footprint from pre-supplied Altium PcbLib library. **}
   { We also do a number of modifications and additions to the footprint(s). }
   { Call CALF_CreateAllNewFootprints() to do all the work. }
   rc := CALF_CreateAllNewFootprints(scriptsPath, 
                                    projectPath,
                                    sourcePcbLib,
                                    project,
                                    {var} cnfGalacticInfo);

   
   CALF_UpdateGuiStatusMessage('Script is finishing....');

   { Record the wall clock time when we ended this script. }
   endTime := Now();
   
   { Timestamp the end of our actions, before we present the last dialog box to the user. }
   WriteToDebugFile('');
   WriteToDebugFile('**Script ' + constThisScriptName + ' ending at ' + DateTimeToStr(Date) + ' ' + TimeToStr(endTime));
   WriteToDebugFile('**Script took ' + FormatDateTime('h:n:s', (endTime-startTime)) + ' (hrs:mins:secs) to run on this project on this PC.');


   { Tell user what he/she still needs to do. }
   CLF_WriteToSummaryAndDebugFiles('');
   CLF_WriteToSummaryAndDebugFiles('You still need to:');
   CLF_WriteToSummaryAndDebugFiles('1.  Review new footprint(s).');
   CLF_WriteToSummaryAndDebugFiles('2.  Perform svn checkin.');


   {****** Wrap things up ******}
   { Call AtExit() procedure to write debug outputs to file. }
   
   WriteToDebugFile('**About to exit script.');
//   ShowMessage('About to call AtExit()');
   AtExit(0);                   { Report success at exit }
//   ShowMessage('Back from AtExit()');

end; { end CNF_CleanupAltLibFootprint() }


{***************************************************************************
 * procedure TCleanupAltLibFootprintForm.clickedOk()
 *  This is the handler for primary dialog box "OK" click.
 ***************************************************************************}
procedure TCleanupAltLibFootprintForm.clickedOk(Sender : TPanel);

begin

//   ShowMessage('Hello world from TCleanupAltLibFootprintForm.clickedOk()');

   { Figure out if we got here from the initial Ok click to start the script or
    the final Ok click to end the script. }
   
   { See if this is the initial Ok click. }
   if (CleanupAltLibFootprintForm.formButtonOk.Left <> 450) then
   begin
   
      { Tell the user that we are proceeding to run script. }
      formButtonsLabel1.Caption := 'OK.  Proceeding to run script.';
      formButtonsLabel1.Update;

      { Disable (grey out) OK and Cancel buttons on primary form. }
      formButtonOk.Enabled := False;
      formButtonOk.Update;

      formButtonCancel.Enabled := False;
      formButtonCancel.Update;

      { Call CNF_CleanupAltLibFootprint() to do all the actual work. }
      CNF_CleanupAltLibFootprint(99);

   end

   { Else this is the final ok to end the script.
    Close the modal window. }
   else
   begin
      ModalResult := mrOK;
      CleanupAltLibFootprintForm.Close;
   end;
   
   { Return to caller. }
   Exit;

end; { end TCleanupAltLibFootprintForm.clickedOk() }


{***************************************************************************
 * procedure TCleanupAltLibFootprintForm.clickedCancel()
 *  This is the handler for primary dialog box "Cancel" click.
 ***************************************************************************}
procedure TCleanupAltLibFootprintForm.clickedCancel(Sender : TPanel);

var
   Action : TCloseAction;
        
begin

   { Close dialog box. }
   ModalResult := mrCancel;
   CleanupAltLibFootprintForm.Close;

   ShowError('Script canceled at User request.');

   { Exit script now. }
   Exit;
   
end; { end TCleanupAltLibFootprintForm.clickedCancel() }


{***************************************************************************
 * procedure SPI_Cleanup_AltLib_Footprint()
 *  Script entry point.
 *
 *  Returns:  (nothing--procedure, not a function).
 ***************************************************************************}
procedure SPI_Cleanup_AltLib_Footprint;

//var

begin

   { Override GUI text entries. }
   { Note:  Don't condense this onto fewer lines or you will have svn replacing $Revision.*$
    with svn rev number on checkin! }
   CleanupAltLibFootprintForm.Caption := 'Welcome to ' + constThisScriptNameNoExt + ' ' +
   StringReplace(StringReplace(constScriptVersion, '$Revision:', ', svn rev', ''),
                 ' $', '', '') + ' script main menu!';
 
   { Override GUI text entries. }
   CleanupAltLibFootprintForm.formText01.Caption := 'You have launched script ' + constThisScriptName + '.';
   CleanupAltLibFootprintForm.formText01.Font.Style := MkSet(fsBold);
   CleanupAltLibFootprintForm.formText02.Caption := '';

   CleanupAltLibFootprintForm.formText03.Caption := 'This script is intended to import a footprint from Mentor LP Wizard, clean it up, and add SPI-specific features.';
   CleanupAltLibFootprintForm.formText03.Font.Style := MkSet(fsBold);
   CleanupAltLibFootprintForm.formText04.Caption := '';
   CleanupAltLibFootprintForm.formText05.Caption := '';
   CleanupAltLibFootprintForm.formText06.Caption := '';
   CleanupAltLibFootprintForm.formText07.Caption := '';

   CleanupAltLibFootprintForm.formText08.Caption := ''; {This script will also:';}
   CleanupAltLibFootprintForm.formText08.Font.Style := MkSet(fsBold);
   CleanupAltLibFootprintForm.formText09.Caption := '';
   CleanupAltLibFootprintForm.formText10.Caption := '';
   CleanupAltLibFootprintForm.formText11.Caption := '';
   CleanupAltLibFootprintForm.formText12.Caption := '';
   CleanupAltLibFootprintForm.formText13.Caption := '';

   CleanupAltLibFootprintForm.formText14.Caption := 'Preconditions:';
   CleanupAltLibFootprintForm.formText14.Font.Style := MkSet(fsBold);
   CleanupAltLibFootprintForm.formText15.Caption := '1.  There must be an .xml command file setup to specify which Altium library, and which footprint within it, to cleanup and adapt for use at SPI.';
   CleanupAltLibFootprintForm.formText16.Caption := '';
   CleanupAltLibFootprintForm.formText17.Caption := '';
   CleanupAltLibFootprintForm.formText18.Caption := '';
   CleanupAltLibFootprintForm.formText19.Caption := '';
   CleanupAltLibFootprintForm.formText20.Caption := '';

   CleanupAltLibFootprintForm.formText21.Caption := 'Notes:';
   CleanupAltLibFootprintForm.formText21.Font.Style := MkSet(fsBold);
   CleanupAltLibFootprintForm.formText22.Caption := '';
   CleanupAltLibFootprintForm.formText23.Caption := '';
   CleanupAltLibFootprintForm.formText24.Caption := '';
   CleanupAltLibFootprintForm.formText25.Caption := '';
   CleanupAltLibFootprintForm.formText26.Caption := '';
   
   CleanupAltLibFootprintForm.formText27.Caption := '';
   CleanupAltLibFootprintForm.formText28.Caption := '';
   CleanupAltLibFootprintForm.formText29.Caption := '';
   
   CleanupAltLibFootprintForm.formText30.Caption := ''; //'Options:';
   CleanupAltLibFootprintForm.formText30.Font.Style := MkSet(fsBold);
   
   CleanupAltLibFootprintForm.formButtonsLabel1.Caption := 'Shall I run (OK), or shall I Cancel running this script?';
   CleanupAltLibFootprintForm.formButtonsLabel1.Font.Style := MkSet(fsBold);

   { Set initial status message. }
   CleanupAltLibFootprintForm.formStatusBar1.SimpleText := 'Status:  Awaiting user permission to run script.';

   
   { Run GUI dialog box asking user for permission to run. }
   CleanupAltLibFootprintForm.ShowModal;

   { Note:  Control now passes to one of the handler functions above. }
   
end; { end SPI_Cleanup_AltLib_Footprint() }




end.
