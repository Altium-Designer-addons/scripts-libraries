## Function list: ##

### Manage string lists to configure this script for your company's needs ###
```Delphi

```
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
```
```

### Low-level misc string manipulation functions ###
```Delphi

```
{***************************************************************************
 * function StripTrailingBackslash()
 *  Strip any trailing backslashes from a file path.
 *
 *  Returns stripped string as function return value.
 ***************************************************************************}
function StripTrailingBackslash(filePath : TDynamicString;
                                )        : TDynamicString;

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



```
```

### Low-level misc file handling functions ###
```Delphi

```
{***************************************************************************
 * function DeleteFileWithVerify()
 *  Try to delete a file and make sure we succeeded.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function DeleteFileWithVerify(filePath : TDynamicString;
                              )        : Integer;

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

{***************************************************************************
 * function IsFileZeroLength()
 *  Determine if a given file is 0 length.
 *
 *  Returns:  True if file is 0 length, False if non-existent or non-zero length.
 ***************************************************************************}
function IsFileZeroLength(filePath : TDynamicString;
                          )        : Boolean;

{***************************************************************************
 * function IsFileWriteable()
 *  Determine if a given file is writeable (eg. not flocked by Excel or Acroread)..
 *
 *  Returns:  True if file is writeable, False if non-existent or non-writeable.
 ***************************************************************************}
function IsFileWriteable(filePath : TDynamicString;
                         )        : Boolean;

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

{***************************************************************************
 * function TruncateFile()
 *  Try to truncate a file to zero length.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function TruncateFile(filePath : TDynamicString;
                      )      : Integer;

{***************************************************************************
 * function TruncateFileWithVerify()
 *  Try to truncate a file to zero length and make sure we succeeded.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function TruncateFileWithVerify(filePath : TDynamicString;
                                )        : Integer;


{***************************************************************************
 * procedure OpenDebugFile()
 *  Open debug file for the first time.
 ***************************************************************************}
procedure OpenDebugFile(fileName :  TDynamicString);

{***************************************************************************
 * procedure WriteToDebugFile()
 *  Write a new line of text to the debug file.
 *  
 *  Note that we will be operating in append-write-close mode in order to
 *  prevent Altium from flocking this file in the event of an unhandled
 *  Altium script crash.
 ***************************************************************************}
procedure WriteToDebugFile(msg : TDynamicString);

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

{***************************************************************************
 * procedure OpenSummaryFile()
 *  Open summary file for the first time.
 ***************************************************************************}
procedure OpenSummaryFile(fileName :  TDynamicString);

{***************************************************************************
 * procedure WriteToSummaryFile()
 *  Write a new line of text to the summary file.
 *  
 *  Note that we will be operating in append-write-close mode in order to
 *  prevent Altium from flocking this file in the event of an unhandled
 *  Altium script crash.
 ***************************************************************************}
procedure WriteToSummaryFile(msg : TDynamicString);

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

```
```

### Low-level misc user interface functions ###
```Delphi

```
{***************************************************************************
 * procedure UpdateGuiStatusMessage()
 *  Update status message in dialog box and write said status to the debug file.
 ***************************************************************************}
procedure UpdateGuiStatusMessage(msg :  TDynamicString);

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

{***************************************************************************
 * procedure IssueConfirmationWithOkOrCancel()
 *  Present the user with a confirmation dialog box with a specified message in it.
 *  If user clicks OK, optionally display "ok" message and return to caller.
 *  If user clicks Cancel, then abort script with "cancel" message.
 ***************************************************************************}
procedure IssueConfirmationWithOkOrCancel(msg       : TDynamicString;
                                          okMsg     : TDynamicString;
                                          cancelMsg : TDynamicString);

{***************************************************************************
 * procedure IssueWarningWithOkOrCancel()
 *  Present the user with a warning dialog box with a specified message in it.
 *  If user clicks OK, optionally display "ok" message and return to caller.
 *  If user clicks Cancel, then abort script with "cancel" message.
 ***************************************************************************}
procedure IssueWarningWithOkOrCancel(msg       : TDynamicString;
                                     okMsg     : TDynamicString;
                                     cancelMsg : TDynamicString);

```
```

### Low-level functions to handle calling external scripts ###
```Delphi

```
{***************************************************************************
 * function CleanupSvnRcFile()
 *  Try to delete rc file from external svn command.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CleanupSvnRcFile(svnRcPath : TDynamicString;
                          )         : Integer;


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


```
```

### Mid-level script functions (mostly file related) ###
```Delphi

```
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

{***************************************************************************
 * function CheckThatSvnScriptsWorkingCopyIsUpdated()
 *  Make sure that the working copy containing our scripts and libraries
 *  is up-to-date with respect to the svn server.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CheckThatSvnScriptsWorkingCopyIsUpdated(scriptsPath : TDynamicString;
                                                 )           : Integer;

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

{***************************************************************************
 * function CheckForUnsavedSource()
 *  Iterate over all the source documents in the project.
 *  Make sure that they've all been saved to disk.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CheckForUnsavedSource(sourceFilePaths : TStringList;
                               )               : Integer;

```
```

### Mid-level functions to analyze project ###
```Delphi

```
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

{***************************************************************************
 * function SanityCheckRelAndTag()
 *  Perform sanity checks before starting release & tag operation.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function SanityCheckRelAndTag(scriptsPath : TDynamicString;
                              projectPath : TDynamicString;
                              )           : Integer;

```
```

### Mid-level functions to make changes to project ###
```Delphi

```
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


```
```

### Mid-level functions to generate and fix Altium output files ###
```Delphi

```
{***************************************************************************
 * function DiffFiles()
 *  Diff 2 text files on the disk and say whether or not they are exactly the same.
 *
 *  Returns:  0 when the files are the same, 1 if they are different.
 ***************************************************************************}
function DiffFiles(filePathA : TString;
                   filePathB : TString;
                   )         : Integer;


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

{***************************************************************************
 * function VerifyBomFilteringIsHappy()
 *  Ask the user if the filtering in the BOM file(s) turned out ok.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function VerifyBomFilteringIsHappy(var step : Integer;
                                       )    : Integer;

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

{***************************************************************************
 * function DelSvnPropsForXlsBomFiles()
 *  Del svn:keywords properties on all Excel (.xls) BOM files that were
 *  generated in all ProjectOutputs/ output subdirs.
 *
 *  We will also use sed to replace "$Rev:: 21074         $:" with "Rev 12345              ".
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


```
```

### Mid-level functions to checkin generated files ###
```Delphi

```
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


```
```

### Mid-level functions to package (zip) output files ###
```Delphi

```
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


```
```

### Mid-level functions related to release-and-tag ###
```Delphi

```
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

{***************************************************************************
 * function CleanupRelAndTag()
 *  Cleanup (svn revert) in top level releases/ and tags/.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CleanupRelAndTag(scriptsPath : TDynamicString;
                          projectPath  : TDynamicString;
                          )            : Integer;

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


```
```

### High level code ###
```Delphi

```
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

```
```

<a href='Hidden comment: 
===foo===
<code language="Delphi">
```

```


Unknown end tag for &lt;/code&gt;


'></a>
