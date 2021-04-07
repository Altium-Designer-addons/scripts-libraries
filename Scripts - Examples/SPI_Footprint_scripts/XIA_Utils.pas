{***************************************************************************
 XIA_Utils.pas
    Altium DelphiScript (basically Pascal) to implement various helper functions
 needed by XIA_Release_Manager, XIA_Update_From_Database, etc.
 ***************************************************************************}

{***************************************************************************
 * Sierra Photonics Inc. has made updates to this file.  
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
 * Most of this code is borrowed from XIA_Utils.pas downloaded from:
 *  http://code.google.com/p/altium-designer-addons/wiki/Release_Manager
 * Copyright (c) 2009-2012 XIA LLC.
 *  (Some code stolen from Altium examples and forum posts)
 *  Author:        Jeff Collins, jcollins@xia.com
 *  Author:        Author: jeffrey.william.collins@gmail.com 
 *  Check-in Date: Date: 2012-02-03 19:22:00 -0800 (Fri, 03 Feb 2012) 
 *  Version #:     Revision: 176 
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
 *  
 ***************************************************************************}


uses
SysUtils;


   
{***************************************************************************
 * Function forward declarations.
 ***************************************************************************}
procedure WriteToDebugFile(msg : TDynamicString); forward;
procedure MyAbort(msg : TDynamicString); forward;
function StripTrailingBackslash(filePath : TDynamicString;
                                )        : TDynamicString; forward;


{***************************************************************************
 * Global constants.
 ***************************************************************************}
const
{* Declare the version and name of this script. *}
   constScriptVersion            = 'v1.1.31 $Revision$';
   constThisScriptNameNoExt      = 'XIA_Utils';
   constThisScriptName           = constThisScriptNameNoExt + '.pas';

{* Declare acronyms for the various scripts that could be running and using some of our code. *}
   constWhichScriptXrm           = 'XRM';     { XIA_Release_Manager.pas }
   constWhichScriptUfd           = 'UFD';     { XIA_Update_From_Database.pas }
   constWhichScriptClf           = 'CLF';     { SPI_Cleanup_LPW_Footprint.pas }
   constWhichScriptCalf          = 'CALF';    { SPI_Cleanup_AltLib_Footprint.pas }
   constWhichScriptCspl          = 'CSPL';    { SPI_Checkin_SchLib_PcbLib_Libraries.pas }
   constWhichScriptCrt           = 'CRT';     { SPI_Commit_Release_Tag.pas }

{* Declare the name of the script project of which this script must be part. }
   constScriptProjectName        = 'TRT_Std_Scripts';

{* Declare the global working copy (with scripts and libraries) that must be up-to-date before we allow this script to actually run. *}
   constGlobalScriptsAndLibsWc   = 'R:\ALTIUM_LIBRARIES\'; { Leave the trailing "\" char there! }
   
{* Declare file and path names to the required DBLib file and the approved DBLink file. *}
   { Note:  These are only needed by the XIA_Update_From_Database script. }
   { Note:  If there is no old DBLib file to upgrade, set constOldDbLibFileName to ''. }
   constOldDbLib1FileName        = 'Previous1_database_library_file.DBLib';
   constOldDbLib2FileName        = 'Previous2_database_library_file.DBLib';
   constRequiredDbLibFileName    = 'Customary_database_library_file.DBLib';
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
   constCommitTimeoutLimit       = 100000000;           { Each unit here means one Sleep(100) call. }

{* Various boring constants. *}
   constLineBreak                = #13 + #10;           { Windows CR/LF pair }
   MaxInt                        = 1000000;             { Large positive integer. }
   constStringDelimiter          = '|';                 { Standard delimiter used to split delimited string into a stringList. }
   constStringQuote              = '"';                 { Standard quote char used to protect strings containing spaces when splitting delimited string into a stringList. }
   constStringEquals             = '=';                 { Standard char to use for NAME=VALUE pairs. }
   constStringFpNameVersionDelim = '/';                 { The character that appears exactly once in footprint names to separate FP name on left from version number on right. }
   constStringSymLibNameDelim    = '\';                 { The character to delimit the schematic library name from the schematic symbol itself. }
   constStringUniqueIdPathSep    = '\';                 { The character to delimit UniqueId paths / names. }
   constStringCsvDelim           = ',';                 { The character to delimit .csv files. }
   constStringLocDbParmsEqualsRemap = '\';              { The character to which to remap any "=" chars that appear in local database parameter xml files. }
   constMaxWindowsCmdLine        = 8191;				{ Max length of cmd line in Windows XP or newer.  See http://support.microsoft.com/default.aspx?scid=kb;en-us;830473 . }
   
   
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
   constDbParmShadowSymName      = 'DBSCHSYMBOL';       { Name of parameter used as a shadow copy of the desired schematic symbol name. }
   constDbParmDbKeyInterim       = 'OLD_DB_KEY';        { Name of former database key, that we will use on an interim basis until we find the real db key. }
   constDbParmDbKey              = 'DB_KEY';            { Name of real database key. }
   constDbSchPlaceholderPrefix   = 'PLACEHOLDER';       { Prefix to be prepended to any schematic symbol names that have not yet been created. }
   constDbValCommentStd          = '=VALUE';            { "Standard" setting for Comment value. }
   constDbValValueFreeform       = 'ALLOW_COMMENT_TO_DIFFER_FROM_THIS_VALUE';  { Value of the VALUE field used to flag that Comment field for this component is freeform. }
   constDbValCategoryFreeform    = 'ALLOW_COMMENT_TO_DIFFER_FROM_VALUE_FIELD'; { Value of the CATEGORY field used to flag that Comment field for this component is freeform. }
   constDbParmNameSuppressFpUpdates  = 'SUPPRESS_FP_UPDATES';{ Name of parameter that will cause us to suppress footprint updates for this component only. }
   constDbParmValueSuppressFpUpdates = '1';             { Value of above parameter that will cause us to suppress footprint updates for this component only. }

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
   constExtXml                   = '.xml';        { Extensible markup language local database files. }
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
   constKindPcb                  = 'PCB';         { Altium PcbDoc document.  Must be all UPPER case. }
   constKindOutJob               = 'OUTPUTJOB';
   constKindHarness              = 'Harness';
   constKindSchLib               = 'SCHLIB';
   constKindUnknown              = 'UNKNOWN';
   constKindPcbProj              = 'PcbProject';

{* Setup constants for subdirs that Altium will decide to create within our ProjectOutputs/ subdirs. *}
   constSubdirOdb                = 'odb';         { "Working" odb/ subdir within XRM5_Fabrication/. }
   constSubdirPreviews           = '__Previews/'; { Annoying new subdir being created as of AD10.818.23272. }
   
{* Setup constants for some of the svn commands that we will issue, to avoid having the code full of magic numbers (er, magic strings). *}
   constSvnCmdPropSetKeywords    = 'propset svn:keywords "Date Rev Author Id"';
   constSvnCmdPropSetKeywordsRevOnly = 'propset svn:keywords "Rev"';
   constSvnCmdPropDelKeywords    = 'propdel svn:keywords';
   constSvnCmdUpdateDepthEmpty   = 'update --depth=empty';
   constSvnCmdCommit             = 'commit';
   constSvnCmdAdd                = 'add --parents';
   constSvnCmdRevert             = 'revert';
   constSvnCmdMove               = 'move';
   constSvnCmdCopy               = 'copy';
   constSvnCmdMkdir              = 'mkdir';
   constSvnCmdInfo               = 'info';
   constSvnCmdStatus             = 'status';
   constSvnCmdStatusWrtServer    = 'status -u';

{* Setup constants for parsing some results from svn commands. *}
   constSvnRepStatusHappy        = ' ';
   constSvnRepStatusMissing      = '!';
   constSvnRepStatusNotInSvn     = '?';
   constSvnRepStatusAdded        = 'A';
   constSvnRepStatusModified     = 'M';
   constSvnRepInfoUrl            = 'URL: ';
   constSvnRepInfoLastChangedRev = 'Last Changed Rev: ';
   constSvnRepCommitCommittedRev = 'Committed revision ';
   constSvnRepStatusStatAgainstRev='Status against revision:';
   constSvnStatusUvRemFieldsStartAtCol = 10;
   constSvnAddFileNameStartsAtCol= 11;
   constSvnWarnNodeNotInSvn      = 'svn: warning: W155007:';
   
{* Setup some standard directory names we'll be expecting to use with svn. *}
   constSvnDirTrunk              = 'trunk';
   constSvnDirReleases           = 'releases';
   constSvnDirTags               = 'tags';

{* Names of our external batch files (without .bat extension). *}
   constBatFileSvn               = 'svn_cmd';
   constBatFileStandardSed       = 'sed_cmd';
   constBatFilePatchingSed       = 'patch_with_sed';

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
   whichScriptIsThis	 : TString;
   DebugFile		 : TextFile;
   isDebugFileOpen	 : Boolean;
   SummaryFile		 : TextFile;
   SummaryMessages	 : TStringList;
   enableGenerateOutputs : Boolean;
   enableSvnCommits	 : Boolean;


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
                             var svnAddExcludes             : TStringList;
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
   svnAddExcludes := TStringList.Create;
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
   svnAddExcludes.Add(constSubdirPreviews);                     { Exclude new __Previews/ subdir from being added to svn. }
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
   svnAddExcludes.Add(constSubdirPreviews);                     { Exclude new __Previews/ subdir from being added to svn. }
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
   svnAddExcludes.Add(constSubdirPreviews);                     { Exclude new __Previews/ subdir from being added to svn. }
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
   svnAddExcludes.Add(constSubdirPreviews);                     { Exclude new __Previews/ subdir from being added to svn. }
   zipDoCheckForExisting.Add(BoolToStr(False));
   zipExcludes.Add('"Status Report.Txt"|*.ZIP');                 { Specific filenames and/or specific file extensions to exclude from zipfile.  Case insensitive.  Must quote protect anything containing a space char!}
   zipFindAddlFiles.Add('');                                    { No additional outputs to find to include in this zipfile. }
   zipFileNames.Add   ('$projectName$_purch_rev_$bomRevNum$.zip'); { Variables in $dollarsigns$ will be substituted in later.  Don't attempt to use "$" char otherwise. }
   relAndTagSubDir.Add('$projectName$_purch_rev_$bomRevNum$');  { Variables in $dollarsigns$ will be substituted in later.  Don't attempt to use "$" char otherwise. }

   { Populate everything associated with OutJob #5:  fabrication outputs. }
   { NOTE:  For zipFileName, it MUST end with "_$pcbDocRevNum$.zip" unless you rewrite CheckForExistingZipFile()! }
   { Note:  We must exclude Altium-created odb/ subdir from being added to svn because Altium will nuke this subdir and re-create it every time.
    If we do nothing, svn will be unhappy because the .svn subdir that stores working copy info will get nuked along with everything else, resulting
    in an svn error on commit.  It's ok to exclude this because the same odb files are also zipped up by Altium into XRM5/ProjectName.zip. }
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
   deleteExcludes.Add('*_*.zip');                               { Exclude previously created fabrication release zipfiles (that include "_" char in filename). }
   svnAddExcludes.Add(constSubdirOdb+'|'+constSubdirPreviews);  { Exclude Altium-created odb/ subdir and new __Previews/ subdir from being added to svn. }
   zipDoCheckForExisting.Add(BoolToStr(True));                  { Check for existing zipfiles with same version number (though possibly different svn rev #). }
   zipExcludes.Add('"Status Report.Txt"|*.REP|*.APR_LIB|*.RUL|*_*.ZIP');{ Specific filenames and/or specific file extensions to exclude from zipfile.  Case insensitive.  Must quote protect anything containing a space char!}
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
   deleteExcludes.Add('*_*.zip');                               { Exclude previously created assembly release zipfiles (that include "_" char in filename). }
   svnAddExcludes.Add(constSubdirPreviews);                     { Exclude new __Previews/ subdir from being added to svn. }
   zipDoCheckForExisting.Add(BoolToStr(True));                  { Check for existing zipfiles with same version number (though possibly different svn rev #). }
   zipExcludes.Add('"Status Report.Txt"|*.REP|*.APR_LIB|*.RUL|*_*.ZIP');{ Specific filenames and/or specific file extensions to exclude from zipfile.  Case insensitive.  Must quote protect anything containing a space char!}
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
                         var svnAddExcludes             : TStringList;
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
   svnAddExcludes.Free;
   zipDoCheckForExisting.Free;
   zipExcludes.Free;
   zipFindAddlFiles.Free;
   zipFileNames.Free;
   relAndTagSubDir.Free;
   
end; { end FreeStringLists() }


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

      { Return the original string as the left string. }
      leftStr := splitMe;
      
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
   path         : TString;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('In MyFindFilesSpecifyRecursion(), projOutPath is "' + projOutPath + '".');
   WriteToDebugFile('In MyFindFilesSpecifyRecursion(), subDir is "' + subDir + '".');
   WriteToDebugFile('In MyFindFilesSpecifyRecursion(), mask is "' + mask + '".');

   { Initialize lists of found files. }
   FilesAndDirs := TStringList.Create;

   { Construct path in a safe fashion. }
   path := StripTrailingBackslash(projOutPath) + '\';

   { Add in subdir if warranted. }
   if (subDir <> '') then
      path := path + subDir + '\'; 
   
   { Fetch a list of all files in this subdirectory.  Doesn't seem to work with the ( - faDirectory ) added in, so
    we'll have to weed out directory results later. }
   { This will be either a recursive or non-recursive search, depending on value of parm "recursive". }
   FindFiles(path, mask, (faAnyFile) { - faDirectory)}, recursive, FilesAndDirs);

   { Loop over all the files and directories returned to us. }
   for i := 0 to (FilesAndDirs.Count - 1) do
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
 * function FindFilesWithExcludesSpecifyRecursion()
 *  Search a given directory for files matching specified mask.
 *  Exclude all files matching an exclude string.
 *  
 *  Note:  Assumes that list FilesOnly has already been created!
 *
 *  Returns:  String list of generated files in var parameter FilesOnly.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function FindFilesWithExcludesSpecifyRecursion(    projOutPath : TString;
                                                   subDir      : TString;
                                                   mask        : TString;
                                                   excludes    : TStringList;
                                                   recursive   : Boolean;
                                               var FilesOnly   : TStringList);
var
   excludedFiles : TStringList;
   i             : Integer;

begin

   {** Find files in this subdir matching mask "mask" **}
   { Call MyFindFilesSpecifyRecursion() to find all the files matching specified mask in specified subDir. }
   result := MyFindFilesSpecifyRecursion(projOutPath,
                                         subDir,
                                         mask,
                                         recursive,
                                         {var} FilesOnly);

   
   {** Find files in this subdir matching one of the excludes masks. **}
   { Initialize lists of excluded files. }
   excludedFiles := TStringList.Create;

   { Loop over all the excludes entries. }
   for i := 0 to excludes.Count - 1 do
   begin

      WriteToDebugFile('In FindFilesWithExcludesSpecifyRecursion(), looking for excludes entry "' + excludes.Strings[i] + '".');
      
      { Call MyFindFilesSpecifyRecursion() to find files matching our excludes mask. }
      result := MyFindFilesSpecifyRecursion(projOutPath,
                                            subDir,
                                            excludes.Strings[i],
                                            recursive,
                                            {var} excludedFiles);

   end; { endfor }


   {** Modify the original list of files found, by deleting files that should be excluded. **}
   { Loop over the original list of files.
    Note that we must loop backwards so that we delete the later files first.
    This way, we don't change the indices of things that we have yet to delete. }
   for i := FilesOnly.Count - 1 downto 0 do
   begin

      { If this file also exists in the excludedFiles list, then delete it from this list. }
      if (excludedFiles.IndexOf(FilesOnly.Strings[i]) >= 0) then
      begin

         WriteToDebugFile('*Found file ' + FilesOnly.Strings[i] + ' that I am removing from the list.');

         { Do the deletion. }
         FilesOnly.Delete(i);

      end; { endif }

   end; {endfor i}


   {** Clean up **}
   { Free up excludedFiles list. }
   excludedFiles.Free;
   
end; { end FindFilesWithExcludesSpecifyRecursion() }


{***************************************************************************
 * function FindFilesWithExcludes()
 *  Search a given directory for files matching specified mask.
 *  Exclude all files matching an exclude string.
 *  
 *  Note:  Assumes that list FilesOnly has already been created!
 *
 *  Returns:  String list of generated files in var parameter FilesOnly.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function FindFilesWithExcludes(    projOutPath : TString;
                                   subDir      : TString;
                                   mask        : TString;
                                   excludes    : TStringList;
                               var FilesOnly   : TStringList);
var
   i         : Integer;
   recursive : Boolean;
                                                             
begin

   { Call FindFilesWithExcludesSpecifyRecursion() to do all the real work. }
   recursive := False;
   FindFilesWithExcludesSpecifyRecursion(projOutPath,
                                         subDir,
                                         mask,
                                         excludes,
                                         recursive,
                                         {var} FilesOnly);
   
end; { end FindFilesWithExcludes() }


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
 * function VerifyFileIsAccessible()
 *  Verify that a given file is accessible (eg. not flocked by Excel or Acroread).
 *  If it is flocked, warn the user to close it, then check one more time.
 *  If on the 2nd check the file is still flocked, then abort script with error.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function VerifyFileIsAccessible(filePath : TDynamicString;
                                verb     : TDynamicString;
                                )         : Boolean;
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Make sure the original xls BOM file is writeable (eg. has been closed by Excel). }
   if (not IsFileWriteable(filePath)) then
   begin

      { Display warning dialog box to user. }
      ShowWarning('File "' + filePath + '" is not ' + verb + ' (probably due to being flocked).' + constLineBreak + constLineBreak +
                  'Probably is it still open in Excel or Acroread.' + constLineBreak + constLineBreak +
                  'Please close this file and then click Ok.' + constLineBreak + constLineBreak +
                  'I will try one more time.');

      { Check one more time. }
      if (not IsFileWriteable(filePath)) then
         MyAbort('File "' + filePath + '" is still not ' + verb + '.');
      

   end; { endif }

end; { end VerifyFileIsAccessible() }


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

   { Call VerifyFileIsAccessible() to do all the real work. }
   result := VerifyFileIsAccessible(filePath,
                                    'writeable');
   
end; { end VerifyFileIsWriteable() }


{***************************************************************************
 * function VerifyFileIsReadable()
 *  Verify that a given file is readable (eg. not flocked by Excel or Acroread).
 *  If it is flocked, warn the user to close it, then check one more time.
 *  If on the 2nd check the file is still flocked, then abort script with error.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function VerifyFileIsReadable(filePath : TDynamicString;
                              )        : Boolean;
begin

   { Call VerifyFileIsAccessible() to do all the real work. }
   result := VerifyFileIsAccessible(filePath,
                                    'readable');
   
end; { end VerifyFileIsReadable() }


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

// ShowMessage('In OpenDebugFile(), fileName is ' + fileName);

   { See if the file already exists. }
   if (FileExists(fileName)) then
   begin
      
      { Truncate old version of debug file and verify that we succeeded. }
      TruncateFileWithVerify(fileName);
      
      { Try to open debug file for writing. }
      AssignFile(DebugFile, fileName);
      
   end { endif }

   { Else it doesn't yet exist.  Create it. }
   else
   begin

      { Try to open debug file for writing. }
      AssignFile(DebugFile, fileName);
      ReWrite(DebugFile);

      { Close debug file. }
      CloseFile(DebugFile);
      
   end; { endelse }

   { Flag that debug file is now open. }
   isDebugFileOpen := True;
  
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

   { Sanity check to see if DebugFile is actually open. }
   if (isDebugFileOpen) then
   begin
   
      { Reopen file in append mode. }
      Append(DebugFile);
      
      { Write new line of text to debug file. }
      WriteLn(DebugFile, msg);
      
      { Close debug file. }
      CloseFile(DebugFile);

   end;

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

   { See if the file already exists. }
   if (FileExists(fileName)) then
   begin
      
      { Truncate old version of summary file and verify that we succeeded. }
      TruncateFileWithVerify(fileName);
      
      { Try to open summary file for writing. }
      AssignFile(SummaryFile, fileName);

   end { endif }

   { Else it doesn't yet exist.  Create it. }
   else
   begin

      { Try to open summary file for writing. }
      AssignFile(SummaryFile, fileName);
      ReWrite(SummaryFile);

      { Close summary file. }
      CloseFile(SummaryFile);
      
   end; { endelse }
   
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
 *  Note:  foo is a dummy parameter that exists only to keep CloseSummaryFile()
 *  from being offered as the script entry point in Altium.
 ***************************************************************************}
procedure CloseSummaryFile(foo : Integer);
begin

   { Free string list. }
   SummaryMessages.Free;
   
end; { end CloseSummaryFile() }


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

   if (whichScriptIsThis = constWhichScriptClf) then
   begin
      
      { Call CLF_AtExit() to do all the real work. }
      CLF_AtExit(rc);
      
   end      
   
   else if (whichScriptIsThis = constWhichScriptCalf) then
   begin
      
      { Call CALF_AtExit() to do all the real work. }
      CALF_AtExit(rc);
      
   end      

   else if (whichScriptIsThis = constWhichScriptCrt) then
   begin
      
      { Call CRT_AtExit() to do all the real work. }
      CRT_AtExit(rc);
      
   end      

   { Else if we have less than or equal to 55 lines in the summary messages list,
    or we're not being called from the UFD script, then just use ShowMessage().}
   else if (  (SummaryMessages.Count <= constMaxLinesInShowMessage) or
       ( (whichScriptIsThis <> constWhichScriptUfd) and
       (whichScriptIsThis <> constWhichScriptClf) )  ) then
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

         { Handle script-specific functions. }
         if (whichScriptIsThis = constWhichScriptXrm) then
         begin
         
            { Call script-specific function to add to our running error message. }
            XrmAtExitMsg(rc,
                         {var} msg);

         end; { endif }         
         
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
 * function IssueSvnCommandGetOutputAllowErrors()
 *  Shell out and call bat file to issue a generic svn command.
 *  Report its return code, and return all output generated by svn.exe.
 *
 *  Returns all svn output in var parm svnOut.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function IssueSvnCommandGetOutputAllowErrors(    scriptsPath : TDynamicString;
                                                 projectPath : TDynamicString;
                                                 command     : TDynamicString;
                                                 parms       : TStringList;
                                             var svnOut      : TStringList;
                                             var cmdLine     : TString;
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

   { Sanity check on max size of cmdLine, per windoze limitations. }
   if (Length(cmdLine) > constMaxWindowsCmdLine) then
   begin
      WriteToDebugFile('cmdLine is "' + cmdLine + '".');
      MyAbort('Length of cmdLine (' + IntToStr(Length(cmdLine)) + ') exceeds max for Windows XP or newer of ' + IntToStr(constMaxWindowsCmdLine) + '!');

   end; { endif }
   
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

   { Cleanup (aka delete) out file from svn command. }
   CleanupSvnRcFile(svnOutPath);

   { Check return code from external svn command. }
   result := StrToInt(svnRc);
   if (result <> 0) then
      WriteToDebugFile('*ERROR/WARNING:  Return code from svn.exe was ' + svnRc + '!!') {nosemi}

   else
      WriteToDebugFile('*Return code from svn.exe was ' + svnRc + '.');

   { Copy all lines from svnOut to DebugFile. }
   for i := 0 to (svnOut.Count - 1) do
   begin

      { Copy this line to debug file. }
      WriteToDebugFile(svnOut.Strings[i]);
   end; { endfor }

end; { end IssueSvnCommandGetOutputAllowErrors() }


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
   rc      : Integer;
   i       : Integer;
   msg     : TDynamicString;
   cmdLine : TString;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Call IssueSvnCommandGetOutputAllowErrors() do to most of the work. }
   rc := IssueSvnCommandGetOutputAllowErrors(scriptsPath,
                                             projectPath,
                                             command,
                                             parms,
                                             {var} svnOut,
                                             {var} cmdLine);
   
   { Check return code from external svn command. }
   if (rc <> 0) then
   begin

      { Abort on error for all svn commands except 'move' and 'revert'. }
      { TODO:  This code is for legacy compatibility.  Any function that wishes
       to allow errors when calling IssueSvnCommand() should use the new IssueSvnCommandGetOutputAllowErrors()
       function instead! }
      if ( (command <> constSvnCmdMove) and (command <> constSvnCmdRevert) ) then
      begin

         { Abort script with error message. }
         MyAbort('External svn command reported error.  cmdLine was "' + cmdLine + '".  Return code was ' + IntToStr(rc) + '!');
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
 * function IssueSvnCommandAllowErrors()
 *  Shell out and call bat file to issue a generic svn command.
 *  Ignore any error return code.  Ignore its output.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function IssueSvnCommandAllowErrors(    scriptsPath : TDynamicString;
                                        projectPath : TDynamicString;
                                        command     : TDynamicString;
                                        parms       : TStringList;
                                    var cmdLine     : TString;
                                        )           : Integer;
var
   rc      : Integer;
   svnOut  : TStringList;
   i       : Integer;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Initialize svnOut string list. }
   svnOut := TStringList.Create;

   { Call IssueSvnCommandGetOutputAllowErrors() to do all the real work. }
   result := IssueSvnCommandGetOutputAllowErrors(scriptsPath,
                                                 projectPath,
                                                 command,
                                                 parms,
                                                 {var} svnOut,
                                                 {var} cmdLine);

   { Free svnOut list. }
   svnOut.Free;
      
end; { end IssueSvnCommandAllowErrors() }


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
   rc      : Integer;
   parms   : TStringList;
   cmdLine : TString;

begin

   { Create list of parameters for svn command. }
   parms := TStringList.Create;
   parms.Add('--depth=infinity');
   parms.Add(dir1);
   parms.Add(dir2);
   
   { Issue command to svn revert. }
   result := IssueSvnCommandAllowErrors(scriptsPath,
                                        projectPath,
                                        constSvnCmdRevert,
                                        parms,
                                        {var} cmdLine);
   
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
   
   { Sanity check on max size of cmdLine, per windoze limitations. }
   if (Length(cmdLine) > constMaxWindowsCmdLine) then
   begin
      WriteToDebugFile('cmdLine is "' + cmdLine + '".');
      MyAbort('Length of cmdLine (' + IntToStr(Length(cmdLine)) + ') exceeds max for Windows XP or newer of ' + IntToStr(constMaxWindowsCmdLine) + '!');

   end; { endif }

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

   { Fetch a list of all files in this subdirectory, subject to the specified excludes. }
   FindFilesWithExcludes(projOutPath,
                         subDir,
                         '*.*',
                         excludes,
                         {var} generatedFiles);
   
   { Loop over all the files. }
   for i := 0 to generatedFiles.Count - 1 do
   begin

      WriteToDebugFile('*About to delete old output file "' + generatedFiles.Strings[i] + '".');

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
         rc := rc | DeleteOutputFiles(projOutPath,
                                      subDir,
                                      excludes);

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
 * function DiffStringLists()
 *  Diff two string lists and say whether or not they are exactly the same.
 *
 *  Returns:  0 when the lists are the same, 1 if they are different.
 ***************************************************************************}
function DiffStringLists(listA : TStringList;
                         listB : TStringList;
                         )     : Integer;
var                                                        
   i     : Integer;
   rc    : Integer;
                  
begin

   { Assume the lists are the same until we find out otherwise. }
   rc := 0;

   { If they have different numbers of lines, then the lists differ. }
   if (listA.Count <> listB.Count) then
   begin
      rc := 1;
   end

   { Else they have the same number of lines.  Proceed to check them line-by-line. }
   else
   begin

      { Loop over all lines in listA. }
      for i := 0 to listA.Count - 1 do
      begin

         { Compare the contents of this line from each list. }
         if (listA.Strings[i] <> listB.Strings[i]) then
         begin

            { Now we know that the lists differ. }
            { TODO:  Abort out of loop to speed things up. }
            rc := 1;
            
         end; { endif }
         
      end; { endfor }

   end; { endelse }
      
   { Return result to caller. }
   result := rc;

end; { end DiffStringLists() }


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
   listA : TStringList;
   listB : TStringList;
                  
begin

   { Create string lists to hold contents of both files. }
   listA := TStringList.Create;
   listB := TStringList.Create;

   { Load both files into string lists. }
   listA.LoadFromFile(filePathA);
   listB.LoadFromFile(filePathB);

   { Call DiffStringLists() to compare the two string lists. }
   rc := DiffStringLists(listA,
                         listB);

   { Free listA & listB lists.}
   listA.Free;
   listB.Free;
   
   { Return result to caller. }
   result := rc;

end; { end DiffFiles() }


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
   position     : Integer;
   rc           : Integer;
   newSubDirs   : TStringList;

begin 

   { For now, assume/hope/pray that we will succeed. }
   rc := 0;

   { Flag that debug file is not yet open. }
   isDebugFileOpen	 := False;

   { Flag that scripts path is not yet known. }
   scriptsPath := '';   
   
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
      { TODO:  This should be a better string comparison than just a subset test. }
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

         { Retrieve the script project's directory. }
         scriptsPath := ExtractFilePath(Project.DM_ProjectFullPath);

//         ShowMessage('Final scriptsPath is "' + scriptsPath + '".');
//         Exit;

      end;
      
   end;

   { Make sure we found our scripts project. }
   if (scriptsPath = '') then
   begin
      ShowError('Could not find script project "' + constScriptProjectName + '"open in Altium!  Exiting now!!!');
      
      { Set error return code. }
      rc := 1;
      result := rc;
      Exit;
   end;

   { Dirty kludge to support having some SPI/XIA era files in a separate directory using svn externals. }
   { FIXME:  Kludge!! }
   if (constScriptProjectName = 'TRT_Std_Scripts') then
   begin

      scriptsPath := scriptsPath + '..\GoogleCode_SPI\Altium_scripts\';

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
//      ShowError('ProjectLogs directory "' + projLogPath + '" does not exist!  Will attempt to create it.');
      { Just silently create it and be done with it. }

      {** Attempt to create this directory. **}
      { Create useless list. }
      newSubDirs := TStringList.Create;
      
      { Call CreateSubDir() to do the actual work. }
      { Note that we won't use the var parm newSubDirs returned by this function. }
      CreateSubDir(scriptsPath,
                   projectPath,
                   projLogPath,
                   {var} newSubDirs);
      
      { Free useless list. }
      newSubDirs.Free;
   
   end;

   { Give return code to caller. }
   result := rc;
   
end; { end InitScript() }


end. { end XIA_Utils.pas }
