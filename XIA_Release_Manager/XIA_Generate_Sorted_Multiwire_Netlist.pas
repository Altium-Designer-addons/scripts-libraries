{***************************************************************************
 XIA_Generate_Sorted_Multiwire_Netlist.pas
 Altium DelphiScript (basically Pascal) that will generate a sorted multiwire
 netlist, free of limitations on the number of characters in netnames.
 
 ***************************************************************************}

{***************************************************************************
 * Copyright (c) 2009-2011 XIA LLC.
 *  (Sorting code stolen from Netlister.pas script from Altium 9 installation.)
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
 * WHAT THIS SCRIPT WILL DO:
 
 * WHAT THIS SCRIPT WILL *NOT* DO:
 *  
 * CAD SETUP REQUIREMENTS:
 *
 * XIA-ism's: (Assumptions / constraints / weirdness / etc. that may be very specific to my company)
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


{***************************************************************************
 * Global constants.
 ***************************************************************************}
const
{* Declare the version and name of this script. *}
   constScriptVersion          = 'v1.2.1_gc $Revision$';
   constThisScriptNameNoExt    = 'XIA_Generate_Sorted_Multiwire_Netlist';
   constThisScriptName         = constThisScriptNameNoExt + '.pas';
{}
   { BEGIN code borrowed from Altium script Netlister.pas, (c) 2003 Altium Limited. }
   //Typed Constants not supported in DelphiScript.
   //Numbers : Set Of Char = ['0'..'9'];
   Numbers = '0123456789';
{}
   Great_Equal = 0;
   Less_Equal  = 1;
   Less_Than   = 2;
   Great_Than  = 3;
   { END code borrowed from Altium script Netlister.pas, (c) 2003 Altium Limited. }

{ Note:  We implicitly rely on a number of constants defined in XIA_Utils.pas.
 That script and this one must both be part of the Pcb project!
 That way, we can use constants and functions defined in the other script. }
   

{***************************************************************************
 * Global variables.  Highly evil.  Ick ick.
 ***************************************************************************}
//var


{***************************************************************************
 * procedure GSMN_Abort()
 *  Call cleanup routines and then abort script.
 ***************************************************************************}
procedure GSMN_Abort(msg : TDynamicString);
begin

   { Save abort message to debug file. }
   WriteToDebugFile('');
   WriteToDebugFile('**In GSMN_Abort()!!!');
   WriteToDebugFile(msg);
   
   { Give error message to user. }
   ShowError(msg + constLineBreak + constLineBreak +
               'Aborting script!!!' + constLineBreak + constLineBreak +
               'Afterwards, hit Control-F3 (or go to Run->Stop) to shut down script execution.' + constLineBreak +
               'Then, click on a file in your PCB project to reset focus.');
   
   { Call AtExit() procedure to write debug outputs to file. }
//   AtExit(1);                   { Report error at exit }

   { We don't want to call AtExit() since we don't really have a summary file
    and we don't want to pop up any dialog boxes.  So just do this and call it good. }
   { Close debug file. }
   CloseDebugFile(0);

   { Now do the real abort. }
   Abort;
end; { end GSMN_Abort() }




{***************************************************************************
 * function ExtractNumberFromAlphaNumString()
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function ExtractNumberFromAlphaNumString(    str : TDynamicString;
                                         var idx : Integer;
                                         var num : Integer;
                                             )   : Integer;

var
   currChar      : TDynamicString;
   numStr        : TDynamicString;
   strLen        : Integer;
   currCharIsNum : Boolean;
   resultKnown : Boolean;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Clear result num }
   num := -1;
   
   { Cache the length of the string. }
   strLen := Length(str);

   { Loop over all the chars in the strings....} 
   resultKnown := False;
   
   { Extract the initial character. }
   currChar    := Copy(str, idx, 1);

   { Initialize the string version of the number. }
   numStr      := '';

   { Loop until we run out of numeric chars. }
   repeat

      { See if this char is numeric. }
      currCharIsNum := ( (currChar >= '0') and (currChar <= '9') );

      { If it is numeric, then add it to the running string version of the number. }
      if (currCharIsNum) then
      begin

         { Add to the running string version of the number. }
         numStr      := numStr + currChar;

         { Increment the index into the string. }
         idx := idx + 1;
         
         { Extract the next character. }
         if (idx <= strLen) then
            currChar    := Copy(str, idx, 1);

      end { endif }

      { Else it's not numeric.  So convert what we already have to integer and call it good. }
      else
      begin

         { Convert already-accumulated number string to integer. }
         num := StrToInt(numStr);
         
         { Flag that we now have a result. }
         resultKnown := True;

         { Don't increment index.  we wish to leave this char to be found by parent function. }
      
      end; { endelse }

      { Loop until we have a known result or we find the ":" delimiter or we run out of string characters. }
   until ( (resultKnown) or (currChar = ':') or (idx > strLen) );

   { If we didn't record a result before we aborted out of the loop (eg. by hitting ":" delimiter),
    then convert accumulated num string to Integer. }
   if (not resultKnown) then
   begin

      { Convert already-accumulated number string to integer. }
      num := StrToInt(numStr);
         
   end; { endif }
      
end; { end ExtractNumberFromAlphaNumString() }


{***************************************************************************
 * function CompareStringAlphaNum()
 *  Compare two alpha-numeric strings.  For our purposes here, this means
 *  strings matching the regular expression [a-zA-Z]+[0-9]+.  In other words,
 *  on or more alphabetic (either case) characters followed by one or more
 *  numeric characters.  When we have two alpha-numeric strings, first compare
 *  the alphabetic parts.  If these are the same, then convert the numeric
 *  characters to integer and then compare those integers.
 *
 *  The idea here is that we want RefDes'es and pin numbers to be ordered like:
 *  C1
 *  C2
 *  ..
 *  C9
 *  C10
 *  C11
 *  ..
 *  C99
 *  C100
 *
 *  And not like:
 *  C10
 *  C11
 *  ..
 *  C19
 *  C1
 *  C20
 *  C21
 *  ..
 *  C29
 *  C2
 *  etc.
 *
 *  If either of these strings is not "alpha-numeric" by the above definition,
 *  then do a simple string comparison and call it good.
 *  
 *  Returns modified string as var parm padMe.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CompareStringAlphaNum(a          : AnsiString; 
                               b          : AnsiString;
                               Comparison : Integer) : Boolean;
var
   aIsAlphaNum  : Boolean;
   bIsAlphaNum  : Boolean;
   aIsNumeric   : Boolean;
   bIsNumeric   : Boolean;
   aFirstNum    : Integer;
   bFirstNum    : Integer;
   aAlpha       : TDynamicString;
   bAlpha       : TDynamicString;
   aNum         : Integer;
   bNum         : Integer;
   aIdx         : Integer;
   bIdx         : Integer;
   aLen         : Integer;
   bLen         : Integer;
   aChar        : TDynamicString;
   bChar        : TDynamicString;
   resultKnown  : Boolean;
   aCharIsNum   : Boolean;
   bCharIsNum   : Boolean;
   aCharIsUpper : Boolean;
   bCharIsUpper : Boolean;
   aCharIsLower : Boolean;
   bCharIsLower : Boolean;

begin

   { Set the result of the function to be false, just in case. }
   Result := False;

//   WriteToDebugFile('*In CompareStringAlphaNum(), a is "' + a + '", b is "' + b + '".');

   { Cache the length of both strings. }
   aLen := Length(a);
   bLen := Length(b);
   
   { Loop over all the chars in the strings....} 
   aIdx        := 1;
   bIdx        := 1;
   resultKnown := False;
   
   { Extract the initial characters from both strings. }
   aChar       := Copy(a, aIdx, 1);
   bChar       := Copy(b, bIdx, 1);
   repeat

      { Determine if current a and b chars are numeric (eg. [0-9]). }
      aCharIsNum := ( (aChar >= '0') and (aChar <= '9') );
      bCharIsNum := ( (bChar >= '0') and (bChar <= '9') );

      { See if both current chars are numeric. }
      if ( (aCharIsNum) and (bCharIsNum) ) then
      begin

         { Extract the number from the A string. }
         ExtractNumberFromAlphaNumString(a {str},
                                         aIdx {var idx},
                                         aNum {var num} );
         
         { Extract the number from the B string. }
         ExtractNumberFromAlphaNumString(b {str},
                                         bIdx {var idx},
                                         bNum {var num} );

         { Compare the extracted numbers from both strings. }
         if (aNum <> bNum) then
         begin

            { Flag that we now have a result. }
            resultKnown := True;
            
            { Do a comparison based on extracted numbers from both strings. }
            case Comparison Of
              Great_Equal : Result := aNum >= bNum;
              Less_Equal  : Result := aNum <= bNum;
              Less_Than   : Result := aNum < bNum;
              Great_Than  : Result := aNum > bNum;
            end { endcase }

         end; { endif }
         
      end { endif both chars numeric }

      { Else see if only A char is numeric. }
      { In this case, we define A is "less" than B. }
      else if ( (aCharIsNum) and (not bCharIsNum) ) then
      begin

         { Flag that we now have a result. }
         resultKnown := True;
      
         { Provide the result based on what we already know. }
         case Comparison Of
           Great_Equal : Result := False;
           Less_Equal  : Result := True;
           Less_Than   : Result := True;
           Great_Than  : Result := False;
         end { endcase }

      end { endelsif}
      
      { Else see if only B char is numeric. }
      { In this case, we define B is "less" than A. }
      else if ( (not aCharIsNum) and (bCharIsNum) ) then
      begin

         { Flag that we now have a result. }
         resultKnown := True;
      
         { Provide the result based on what we already know. }
         case Comparison Of
           Great_Equal : Result := True;
           Less_Equal  : Result := False;
           Less_Than   : Result := False;
           Great_Than  : Result := True;
         end { endcase }

      end { endelsif}

      { Else neither current char is numeric.... }
      else
      begin
      
         { Compare the current characters in both strings. }
         if (aChar <> bChar) then
         begin

            { Flag that we now have a result. }
            resultKnown := True;
               
            { Do a comparison based on current characters from both strings. }
            case Comparison Of
              Great_Equal : Result := aChar >= bChar;
              Less_Equal  : Result := aChar <= bChar;
              Less_Than   : Result := aChar < bChar;
              Great_Than  : Result := aChar > bChar;
            end { endcase }

         end; { endif (aChar <> bChar) }

      end; { endelse }

      { Increment indices }
      aIdx        := aIdx + 1;
      bIdx        := bIdx + 1;

      { Extract the next characters from both strings. }
      if (aIdx <= aLen) then
         aChar       := Copy(a, aIdx, 1);
      if (bIdx <= bLen) then
         bChar       := Copy(b, bIdx, 1);
      
      { Loop until we have a known result or we find the ":" delimiter or we run out of string characters. }
   until ( (resultKnown) or (aChar = ':') or (bChar = ':') or (aIdx > aLen) or (bIdx > bLen) );

   { If we didn't get a known result (eg. character differences) before we hit the ":" delimiter,
    then we break the tie by declaring the string with remaining characters to be less than
    the other one.  This is arbitrary for compatiblity with unix sort. }
   if (not resultKnown) then
   begin

      { Do a comparison based on current characters from both strings. }
      case Comparison Of
        Great_Equal : Result := not ( (bChar = ':') or (bIdx > bLen) ); { a >= b }
        Less_Equal  : Result := not ( (aChar = ':') or (aIdx > aLen) ); { a <= b }
        Less_Than   : Result := not ( (aChar = ':') or (aIdx > aLen) ); { a < b }
        Great_Than  : Result := not ( (bChar = ':') or (bIdx > bLen) ); { a > b }
      end { endcase }

   end; { endif }

end; { end CompareStringAlphaNum() }


{***************************************************************************
 * function CompareStringMultiwire(()
 *  Compare two Multiwire netlist entries.
 *  Each will look like "PhyRefDes.PinNumber:NetName".
 *  Sort by PhyRefDes.  If both PhyRefDes'es are the same, then sort by PinNumber.
 *  When sorting either of these, convert numeric part of them to integer
 *  and do integer sort.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CompareStringMultiwire(a          : AnsiString; 
                                b          : AnsiString;
                                Comparison : Integer) : Boolean;
var
   a1, b1              : AnsiString;
   aPhyRefDesDotPinNum : TDynamicString;
   aPhyRefDes          : TDynamicString;
   aPinNum             : TDynamicString;
   aNetName            : TDynamicString;
   bPhyRefDesDotPinNum : TDynamicString;
   bPhyRefDes          : TDynamicString;
   bPinNum             : TDynamicString;
   bNetName            : TDynamicString;

begin

//   WriteToDebugFile('*In CompareStringMultiwire(), a is "' + a + '", b is "' + b + '".');


   { The code in CompareStringAlphaNum() is now smart enough to handle our delimiters. }
   Result := CompareStringAlphaNum(a, 
                                   b,
                                   Comparison);
   

end; { end CompareStringMultiwire() }


{***************************************************************************
 * BEGIN code borrowed from Altium script Netlister.pas, (c) 2003 Altium Limited.
 ***************************************************************************}
{..............................................................................}
Function LessThan(Const a, b : AnsiString) : Boolean;
Begin
    Result := CompareStringMultiwire(a, b, Less_Than);
End;
{..............................................................................}

{..............................................................................}
Function GreatThan(Const a, b : AnsiString) : Boolean;
Begin
    Result := CompareStringMultiwire(a, b, Great_Than);
End;
{..............................................................................}

{..............................................................................}
Function SortedListCompare(Const S1, S2 : AnsiString) : Integer;
Begin
    If S1 = S2 Then
        Result := 0
    Else If LessThan(S1, S2) Then
        Result := -1
    Else If GreatThan(S1, S2) Then
        Result := +1
    Else
    Begin
        {Handle the special case N01 and N001 - suffix is numerically same}
        {but alphanumerically different}
        {So resort to using straight string comparison}
        If S1 < S2 Then
            Result := -1
        Else If S1 > S2 Then
            Result := +1
        Else
            Result := 0;
    End;
End;
{..............................................................................}

{..............................................................................}
Function ListSort(List : TStringList;Index1,Index2 : Integer) : integer;
Begin
    Result := SortedListCompare(List[Index1],List[Index2]);
End;
{..............................................................................}

{..............................................................................}
procedure QuickSort(StringList : TStringList; L, R: Integer);
Var
  I, J, P : Integer;
Begin
    P := StringList.Count;
    If (L >= P) or (R >= P) Then Exit;

    Repeat
        I := L;
        J := R;
        P := (L + R) div 2;
        Repeat
            While ListSort(StringList, I, P) < 0 do Inc(I);
            While ListSort(StringList, J, P) > 0 do Dec(J);

            If I <= J then
            Begin
                StringList.Exchange(I,J);
                If P = I Then
                    P := J
                Else If P = J Then
                    P := I;
                Inc(I);
                Dec(J);
            End;
        Until I > J;

        If L < J Then QuickSort(StringList, L, J);
        L := I;
    Until I >= R;
End;
{..............................................................................}

{..............................................................................}
Procedure SortList(StringList : TStringList);
Begin
    QuickSort(StringList, 0, StringList.Count - 1)
End;
{..............................................................................}
{***************************************************************************
 * END code borrowed from Altium script Netlister.pas, (c) 2003 Altium Limited.
 ***************************************************************************}


{***************************************************************************
 * function GenerateUnsortedMultiwireNetlist()
 *  Analyze the "flattened" pseudo-schematic page and retrieve and store
 *  netlist in "multiwire" format.
 *  
 *  NOTE:  Assumes that multiwireNetlist string list has already been created.
 *  
 *  Returns unsorted multiwire netlist as var parm multiwireNetlist.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function GenerateUnsortedMultiwireNetlist(    Project          : IProject;
                                          var multiwireNetlist : TStringList;
                                              )                : Integer;

var
   flatSchem : IDocument;
   i         : Integer;
   j         : Integer;
   k         : Integer;
   component : IComponent;
   phyRefDes : TDynamicString;
   pinNum    : TDynamicString;
   netName   : TDynamicString;
   part      : IPart;
//   pin     : IPin;
   pin       : INetItem;
   net       : INet;
   pinCount  : Integer;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   {** Analyze flattened pseudo-schematic in order to extract netlist information, etc. **}
   { Note:  Code borrowed & adapted from AgileBOMV1.1.pas. }

   { Get a reference to the flattened schematic document. }
   flatSchem := Project.DM_DocumentFlattened;

   { If we couldn't get the flattened sheet, then most likely the project has not been compiled recently. }
   if (flatSchem = Nil) then
   begin

      { Compile project before proceeding. }
      WriteToDebugFile('Status:  Compiling project.');
      Project.DM_Compile;

      { Get a reference to the flattened schematic document. }
      flatSchem := Project.DM_DocumentFlattened;

      { Sanity check.  If it's still Nil, then we're in serious trouble. }
      if (flatSchem = Nil) then
         GSMN_Abort('Unable to get flatSchem even after compiling project.  Maybe there was some error in the compile?');

   end; { endif }
   

   { Output debug info. }
   WriteToDebugFile('*About to process flattened components....');

   { Loop over all physical nets in flattened schematic document. }
   for i := 0 to (flatSchem.DM_NetCount - 1) do
   begin

      { Retrieve reference to the ith net. }
      net := flatSchem.DM_Nets(i);

      { Cache the number of pins connected to this net. }
      pinCount  := net.DM_PinCount;

      { Cache physical net name. }
      netName   := net.DM_NetName;
         
      { Loop over all the pins connected to this net. }
      for j := 0 to (pinCount - 1) do
      begin
         
         { Retrieve reference to the jth pin connection for this net. }
         pin       := net.DM_Pins(j);
         
         { Cache physical refdes. }
         phyRefDes := pin.DM_PhysicalPartDesignator;
         
         { Cache pin number. }
         pinNum    := pin.DM_PinNumber;
         
         { If this net has more than 1 connection or it has a non-default name, then we care about it. }
         if ( (pinCount > 1) or (netName <> ('Net' + phyRefDes + '_' + pinNum)) ) then
         begin

            { Output debug information. }
            WriteToDebugFile('*    ' + phyRefDes + '.' + pinNum + ':' + netName);

            { Write entry to unsorted multiwire netlist. }
            multiwireNetlist.Add(phyRefDes + '.' + pinNum + ':' + netName);

         end; { endfor j }
            
      end; { endif }

   end; { endfor i }
      
end; { end GenerateUnsortedMultiwireNetlist() }


{***************************************************************************
 * function GenerateAndSortMultiwireNetlist()
 *  Generate the unsorted multiwire netlist.  Sort it.  Write it to disk.
 *  
 *  This is the entry point for the script when called from XIA_Release_Manager.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function GenerateAndSortMultiwireNetlist(Project      : IProject;
                                         TargetFolder : TDynamicString;
                                         )            : Integer;

var
   multiwireNetlist : TStringList;
   TargetFileName   : TDynamicString;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   {** Initialize unsorted multiwire netlist. **}
   multiwireNetlist   := TStringList.Create;
   multiwireNetlist.CaseSensitive := False;


   {** Parse the flattened schematic design and extract unsorted multiwire netlist. **}
   GenerateUnsortedMultiwireNetlist(Project,
                                    {var} multiwireNetlist);
   

   { Sort the multiwire netlist. }
   SortList(multiwireNetlist);

   
   { Set output target filename. }
   { Keep "MULTIWIRE_1.NET" name for compatibility with existing projects. }
   TargetFileName := TargetFolder + '\' + 'MULTIWIRE_1.NET';
   
   
   { Write sorted multiwire netlist to file. }
   multiwireNetlist.SaveToFile(TargetFileName);

   { Set the Editor Type and put in the Generated folder. }
   { What is this first step actually supposed to do??? }
   VFS_SetFileEditorName(TargetFileName, 'XIA_SortedMultiwireNetlist');
   Project.DM_AddGeneratedDocument(TargetFileName);
   
   
   { Free multiwire netlist. }
   multiwireNetlist.Free;
         
end; { end GenerateAndSortMultiwireNetlist() }


{***************************************************************************
 * procedure Generate()
 *  This is the entry point for the script when called from an OutJob.
 *
 *  Returns:  (nothing--procedure, not a function).
 ***************************************************************************}
procedure Generate(Parameters : String);
var
   WorkSpace        : IWorkSpace;
   Project          : IProject;
   projectPath      : TDynamicString;
   projOutPath      : TDynamicString;
   projectName      : TDynamicString;
   projLogPath      : TDynamicString;
   scriptsPath      : TDynamicString;
   document         : IDocument;
   timestamp        : TDynamicString;
   startTime        : TDateTime;
   endTime          : TDateTime;
   rc               : Integer;
   TargetFolder     : TDynamicString;
   parmsStringList  : TStringList;
   i                : Integer;

begin

   { Specify that we are running the XIA_Generate_Sorted_Multiwire_Netlist script. }
//   whichScriptIsThis     := constWhichScriptUfd;


   {*** Run standard script initialization routine. ***}
   { Note:  This code is located in XIA_Release_Manager.pas. }
//   rc := InitScript(Workspace,
//                    Project,
//                    scriptsPath,
//                    projectName,
//                    projectPath,
//                    projOutPath,
//                    projLogPath);

//   { Make sure init function succeeded.  If not, we have a _serious_ problem and we need to Exit; now. }
//   if (rc <> 0) then
//      Exit;

//   ShowMessage('Hello world!  Parameters is "' + Parameters + '".');

   { Attempt to get reference to current workspace. }
   Workspace  := GetWorkspace;
   if (Workspace = nil) then
   begin
      ShowError('Unable to find current workspace.');
      Exit;
   end;
      
   { Attempt to determine which is the currently focused project. }
   Project := Workspace.DM_FocusedProject;
   if (Project = nil) then
   begin
      ShowError('Unable to find current project.');
      Exit;
   end;

   { Retrieve the project working directory. }
   projectPath := ExtractFilePath(Project.DM_ProjectFullPath);



   {****** Initialize script. ******}
   { These flags are not actually used in this script, but set them to True to keep other code happy. }
   enableGenerateOutputs := True;
   enableSvnCommits      := True;


   { Open debug file. }
   OpenDebugFile((projectPath + constThisScriptNameNoExt + '_Debug.txt'));
   WriteToDebugFile('**Script ' + constThisScriptName + ' started at ' + DateTimeToStr(Date) + ' ' + TimeToStr(startTime));
   WriteToDebugFile('Project : ' +  Project.DM_ProjectFileName);

   { Write debug info. }
   WriteToDebugFile('*Parameters is "' + Parameters + '".');

   {*** Read parameters given to us by Altium as it invokes us via OutJob. ***}
   {** Initialize parameters string list. **}
   parmsStringList  := TStringList.Create;

   { Add quote chars to beginning and end of string to match up with those that get
    added around delimiters. }
   Parameters := '"' + Parameters + '"';

   { Replace all '|' chars with '"|"'. }
   Parameters := StringReplace(Parameters, '|', '"|"', MkSet(rfReplaceAll));
   
   { Write debug info. }
   WriteToDebugFile('*Parameters is now "' + Parameters + '".');
   
   { Read '|' separated Parameters into parmsStringList. }
   parmsStringList.Delimiter := '|';
   parmsStringList.QuoteChar := constStringQuote;
   parmsStringList.DelimitedText := Parameters;

   
   { Write out all the parameters from the parmsStringList. }
   for i := 0 to (parmsStringList.Count - 1) do
   begin
      WriteToDebugFile('* parmsStringList[' + IntToStr(i) + '] is ' + parmsStringList.Strings[i]);
   end;

   {** Extract useful parameters from Altium command line (as it were) **}
   { Extract 'TargetFolder' parameter. }
   i := parmsStringList.IndexOfName('TargetFolder');

   if (i < 0) then
      GSMN_Abort('Unable to find "TargetFolder" parameter passed in from Altium!');
   
   TargetFolder := parmsStringList.ValueFromIndex(i);
   WriteToDebugFile('* TargetFolder is "' + TargetFolder + '".');
   
//   GSMN_Abort('foo');

   
   { Record the wall clock time when we started the real work of this script. }
   startTime := Now();

   { Call GenerateAndSortMultiwireNetlist() to do all the real work. }
   GenerateAndSortMultiwireNetlist(Project,
                                   TargetFolder);

   
   { Record the wall clock time when we ended this script. }
   endTime := Now();
   
   { Timestamp the end of our actions, before we present the last dialog box to the user. }
   WriteToDebugFile('');
   WriteToDebugFile('**Script ' + constThisScriptName + ' ending at ' + DateTimeToStr(Date) + ' ' + TimeToStr(endTime));
   WriteToDebugFile('**Script took ' + FormatDateTime('h:n:s', (endTime-startTime)) + ' (hrs:mins:secs) to run on this project on this PC.');

   
   { Write a copy of our summary file to the ProjectLogs directory, using a filename with timestamp embedded in it. }
   { Attempt to use international-ish time/date format year-month-date_hour_minute_seconds. }
   timestamp := FormatDateTime('yyyy-mm-dd_hh_nn_ss', Now());
//   SummaryMessages.SaveToFile(projLogPath + '\' + constThisScriptNameNoExt + '_' + timestamp + '.LOG');
   
   
   {****** Wrap things up ******}

   { Call AtExit() procedure to write debug outputs to file. }
   WriteToDebugFile('**About to exit script.');
//   ShowMessage('About to call AtExit()');
//   AtExit(0);                   { Report success at exit }
//   ShowMessage('Back from AtExit()');

   { We don't want to call AtExit() since we don't really have a summary file
    and we don't want to pop up any dialog boxes.  So just do this and call it good. }
   { Close debug file. }
   CloseDebugFile(0);

end;



end.
