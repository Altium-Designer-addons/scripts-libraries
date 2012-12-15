{***************************************************************************
 SPI_Cleanup_PcbDoc_Layout.pas
 Altium DelphiScript (basically Pascal) that will attempt to cleanup SPI-specific
 features in a PcbDoc file.  Currently this only includes:
 1.  Split .Comment text into lines for multi-line instruction footprints.
 2.  Cleanup Drill Chart footprint so that gerber generation actually works.
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
 *  1.  This script requires functions and constants defined in XIA_Utils.pas.
 *  It also uses certain functions from SPI_Cleanup_LPW_Footprint.pas.
 *  All of these scripts must be in the same script project.
 *  
 * Notes:
 *  1.  Tested with Altium 10.  (May or may not work with Altium 9--not tested.)
 *  2.  Tested with Windows 7 x64.
 *  3.  This file should no longer have TAB characters in it.
 *
 * WHAT THIS SCRIPT WILL DO:
 *  1.  Examine all Pcb components in the selected & open PcbDoc file.
 *  2.  For any that match a pre-defined list of Vault components, proceed to split a
 *      long Comment string into multiple lines (.Comment1, .Comment2, etc.)
 *  3.  On finding the Drill Chart component, move the ".Legend" string out of the
 *      footprint so that gerber generation will actually work.
 
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
 *  TODO:
 *  1.  Support footprints with more than 2 lines.
 *  2.  Store info about original locations of .Comment1, .Comment2, etc.
 *  3.  Update split text when Comment changes.
 *  4.  Open PcbDoc file in the selected project, rather than requiring that it
 *      already be open.
 *  5.  Check that freestanding .Legend text is present and in the right place!
 *
 *  TODO for schematic side script:
 *  1.  Audit length of Comment and choose footprint with appropriate number
 *  of lines in it.
 *
 ***************************************************************************}

{***************************************************************************
 * Global constants.
 ***************************************************************************}
const
{* Declare the version and name of this script. *}
   constScriptVersion          = 'v0.2.0 $Revision$';
   constThisScriptNameNoExt    = 'SPI_Cleanup_PcbDoc_Layout';
   constThisScriptName         = constThisScriptNameNoExt + '.pas';
{}
   cCPL_lenItem                = 15;	{ Number of characters comprising the Item part of the ItemRev per SPI standards. }
   cCPL_lenMaxInsnComment      = 84;	{ Max num characters that can be present on each line of a split Comment string for INSN: type. }
   cCPL_lenMaxNoteComment      = 89;	{ Max num characters that can be present on each line of a split Comment string for NOTE: type. }


{***************************************************************************
 * function CPL_Init()
 *  Initialize any string lists we need for this script.
 ***************************************************************************}
function CPL_Init(var multiLineFootprintLibRefs  : TStringList;
                  var drillChartFootprintLibRefs : TStringList;
                      )                          : Integer;

var
   i         : Integer;
   
begin

   { Assume success. }
   result := 0;

   { Initialize stringlists. }
   multiLineFootprintLibRefs  := TStringList.Create();
   drillChartFootprintLibRefs := TStringList.Create();

   { Create a list of all the Vault components which have multi-line instruction footprints attached to them. }
   { TODO:  Currently we're only handling 2-line footprints! }
   { TODO:  Currently we don't attempt to handle freeform instructions which may
    or may not be using multi-line footprints.  Such part #'s are not currently in this list! }
   { TODO:  We should be keying off the names of multiline footprints, rather
    than defining components that use them a priori! }
   { NOTE:  The "=foo" is so that we can treat these as name-value pairs, and re-use related code. }
   multiLineFootprintLibRefs.Add('00512-001-10300' + '=foo');
   multiLineFootprintLibRefs.Add('00512-001-10400' + '=foo');
   multiLineFootprintLibRefs.Add('00512-001-10600' + '=foo');
   multiLineFootprintLibRefs.Add('00512-001-10900' + '=foo');
   multiLineFootprintLibRefs.Add('00512-001-11400' + '=foo');
   multiLineFootprintLibRefs.Add('00512-001-11900' + '=foo');
   multiLineFootprintLibRefs.Add('00512-001-12200' + '=foo');
   multiLineFootprintLibRefs.Add('00512-001-15000' + '=foo');
   multiLineFootprintLibRefs.Add('00512-001-15001' + '=foo');
   multiLineFootprintLibRefs.Add('00512-001-15002' + '=foo');
   multiLineFootprintLibRefs.Add('00512-001-15003' + '=foo');
   multiLineFootprintLibRefs.Add('00512-001-16100' + '=foo');
   multiLineFootprintLibRefs.Add('00512-001-16101' + '=foo');
   multiLineFootprintLibRefs.Add('00512-001-19000' + '=foo');
   multiLineFootprintLibRefs.Add('00512-001-19001' + '=foo');
   multiLineFootprintLibRefs.Add('00513-001-10200' + '=foo');
   multiLineFootprintLibRefs.Add('00518-001-10300' + '=foo');
   multiLineFootprintLibRefs.Add('00518-001-15000' + '=foo');
   multiLineFootprintLibRefs.Add('00518-001-15100' + '=foo');
   multiLineFootprintLibRefs.Add('00518-001-20100' + '=foo');

   { Create a list of all the Vault components which are the drill chart component. }
   drillChartFootprintLibRefs.Add('00513-001-30100' + '=foo');
   
end; { end CPL_Init() }


{***************************************************************************
 * function CPL_ConvertItemAndRevToItem()
 *  Convert a PCB component's Item-and-Rev code to just the Item code.
 ***************************************************************************}
function CPL_ConvertItemAndRevToItem(    itemAndRev : TString;
                                     var item       : TString;
                                         )          : Integer;

var
   i         : Integer;
   
begin

   { Assume success. }
   result := 0;

   { Copy the initial cCPL_lenItem characters. }
   item := Copy(itemAndRev, 1, cCPL_lenItem);
   
end; { end CPL_ConvertItemAndRevToItem() }


{***************************************************************************
 * function CPL_SplitStringAtSpace()
 *  Split a string into left and right at the right-most space character
 *  at or before the specified mark.
 ***************************************************************************}
function CPL_SplitStringAtSpace(    splitMe  : TString;
                                    maxChars : Integer;
                                var leftStr  : TString;
                                var rightStr : TString;
                                    )        : Integer;

var                                                                                        
   i       : Integer;
   pos     : Integer;
   errFlag : Boolean;
   found   : Boolean;
   
begin

   { Assume success. }
   result := 0;

   { Loop initialization. }
   pos := maxChars + 1;		{ Having a space one to the right is actually the ideal case. }
   found   := False;
   
   { Repeat until we find a space character. }
   repeat
   begin

      { See if we have a space char. }
      if (Copy(splitMe, pos, 1) = ' ') then
      begin

         { Set leftStr to everything to the left of our current position. }
         leftStr := Copy(splitMe, 1, pos-1);
         
         { Set rightStr to everything to the right of our current position. }
         rightStr := Copy(splitMe, pos+1, MaxInt);
         
         WriteToDebugFile('* Successfully split string!  leftStr is "' + leftStr + '", rightStr is "' + rightStr + '".');

         { Flag that we found our space char. }
         found   := True;

      end; { endif }

      { Decrement string position. }
      pos := pos - 1;

   end;
   until ( (Found) or (pos <= 0) );

   { Sanity check. }
   if (not Found) then
      MyAbort('Unable to split string "' + splitMe + '"!');
   
end; { end CPL_SplitStringAtSpace() }


{***************************************************************************
 * function CPL_SplitCommentIntoMultipleLines()
 *  Split an overly-long Comment string into multiple lines of no more than
 *  a constant number of characters.
 ***************************************************************************}
function CPL_SplitCommentIntoMultipleLines(    component    : IPCB_Component;
                                           var commentLines : TStringList;
                                               )            : Integer;

var                                                                                        
   i           : Integer;
   commentText : TString;
   leftStr     : TString;
   rightStr    : TString;
   splitMe     : TString;
   maxChars    : Integer;
   
begin

   { Assume success. }
   result := 0;

   { Initialize stringlist. }
   commentLines         := TStringList.Create();

   { Extract comment text. }
   commentText := component.Comment.Text;

   { Setup for NOTE: type Comment, where the Comment can use all the available
    space. }
   maxChars    := cCPL_lenMaxNoteComment;
   
   
   { See if we have INSN: type Comment.  If so, we must account for the fact that
    these footprints will have a ~5 char refdes in front of the Comment text. }
   if ( (CLF_DoesStringStartWith(commentText, 'FAB INSN')) or
       (CLF_DoesStringStartWith(commentText, 'ASSY INSN')) or
       (CLF_DoesStringStartWith(commentText, 'DRILL INSN')) or
       (CLF_DoesStringStartWith(commentText, 'REWORK INSN')) ) then
   begin

      maxChars    := cCPL_lenMaxInsnComment;

   end; 
       
   { Initialize loop. }
   splitMe  := commentText;

   { FIXME:  Implement loop to handle more than 2 lines! }

   { Split the current string into left and right pieces. }
   CPL_SplitStringAtSpace(splitMe,
                          maxChars,
                          {var} leftStr,
                          {var} rightStr);

   { Add splits to list. }
   commentLines.Add(leftStr);
   commentLines.Add(rightStr);
   
end; { end CPL_SplitCommentIntoMultipleLines() }


{***************************************************************************
 * function CPL_IsThisFootprintOfInterest()
 *  Determine if a given component is a member of the interstingFootprints list.
 ***************************************************************************}
function CPL_IsThisFootprintOfInterest(interestingFootprints : TStringList;
                                       component             : IPCB_Component;
                                       debugNote             : TString;
                                       )                     : Boolean;

var
   i          : Integer;
   itemAndRev : TString;
   item       : TString;
   
begin

   { Assume that this is not a multi-line instruction footprint. }
   result := False;
  
   { Extract the item-and-rev number (aka. libRef) for this component. }
   itemAndRev := component.SourceLibReference;

   { Extract just the item number for this component. }
   CPL_ConvertItemAndRevToItem(itemAndRev,
                               {var} item);

   WriteToDebugFile('* itemAndRev is "' + itemAndRev + '".');
   WriteToDebugFile('* item is       "' + item + '".');

   { See if the item code is in our stringlist. }
   if (CLF_IsNameInStringList(item, interestingFootprints)) then
   begin

      WriteToDebugFile('*  ' + debugNote);

      { Flag that we found such a thing. }
      result := True;

   end;
   
end; { end CPL_IsThisFootprintOfInterest() }


{***************************************************************************
 * function CPL_IsThisMultiLineInsnFootprint()
 *  Determine if a given component is a multi-line instruction footprint that
 *  we need to worry about. 
 ***************************************************************************}
function CPL_IsThisMultiLineInsnFootprint(multiLineFootprintLibRefs : TStringList;
                                          component                 : IPCB_Component;
                                          )                         : Boolean;

var
   i         : Integer;
   debugNote : TString;
   
begin

   { Call CPL_IsThisFootprintOfInterest() to do all the real work. }
   debugNote := 'This is a multi-line instruction footprint!!';
   result := CPL_IsThisFootprintOfInterest({interestingFootprints} multiLineFootprintLibRefs,
                                           component,
                                           debugNote);
   
end; { end CPL_IsThisMultiLineInsnFootprint() }


{***************************************************************************
 * function CPL_IsThisDrillChart()
 *  Determine if a given component is the drill chart component/footprint.
 ***************************************************************************}
function CPL_IsThisDrillChart(drillChartFootprintLibRefs : TStringList;
                              component                  : IPCB_Component;
                              )                          : Boolean;

var
   i         : Integer;
   debugNote : TString;
   
begin

   { Call CPL_IsThisFootprintOfInterest() to do all the real work. }
   debugNote := 'This is the drill chart footprint!!';
   result := CPL_IsThisFootprintOfInterest({interestingFootprints} drillChartFootprintLibRefs,
                                           component,
                                           debugNote);
   
end; { end CPL_IsThisDrillChart() }


{***************************************************************************
 * function CPL_AlterTextInMultilineFootprint()
 *  Now that we have identified a multi-line instruction footprint, proceed
 *  to alter its .Comment1, .Comment2, etc. text to contain the .Comment text.
 ***************************************************************************}
function CPL_AlterTextInMultilineFootprint(    board     : IPCB_Board;
                                           var component : IPCB_Component;
                                               )         : Integer;

var
   i            : Integer;
   iterator     : IPCB_GroupIterator;
   textObj      : IPCB_Text;
   commentLines : TStringList;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('*Hello from CPL_AlterTextInMultilineFootprint()');

   { Split the .Comment string into multiple lines. }
   CPL_SplitCommentIntoMultipleLines(component,
                                     {var} commentLines);

   { Setup an iterator so that we can iterate over all texts in this PCB component. }
   iterator        := component.GroupIterator_Create;
   iterator.AddFilter_ObjectSet(MkSet(eTextObject));
   iterator.AddFilter_LayerSet(AllLayers);

   { Get a reference to the first PCB object. }
   textObj := iterator.FirstPCBObject;

   { Loop over all objects in this PcbDoc file. }
   while (textObj <> nil) do
   begin
      
      WriteToDebugFile('*Found text ' + textObj.Text);

      { Loop over all lines of Comment text. }
      for i := 0 to (commentLines.Count - 1) do
      begin
      
         { See if we have a line of Comment text to assign to this text string. }
         if (textObj.Text = '.Comment' + IntToStr(i+1)) then
         begin

            WriteToDebugFile('*About to replace this with Comment text "' + commentLines.Strings(i) + '".');

            { Notify Altium that the text object is going to be modified. }
            textObj.BeginModify;

            { Attempt to alter the text object. }
            textObj.Text := commentLines.Strings(i);

            { Notify Altium that the text object has been modified. }
            textObj.EndModify;
            board.DispatchMessage(board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, textObj.I_ObjectAddress);
            PcbServer.PostProcess;

         end; { endif }
         
      end; { endfor }

      { Advance to next textObj in this Pcb component. }
      textObj := iterator.NextPCBObject;

   end;  { endwhile loop over all PCB texts in this Pcb component. }

   { Free group iterator. }
   component.GroupIterator_Destroy(iterator);

   { Free string lists. }
   commentLines.Free();

end; { end CPL_AlterTextInMultilineFootprint() }


{***************************************************************************
 * function CPL_AlterDrillChartFootprint()
 *  Now that we have identified the Drill Chart footprint, proceed to move
 *  its .Legend text out of the footprint, so that it works in gerber generation.
 ***************************************************************************}
function CPL_AlterDrillChartFootprint(    board     : IPCB_Board;
                                      var component : IPCB_Component;
                                          )         : Integer;

var
   i          : Integer;
   iterator   : IPCB_GroupIterator;
   textObj    : IPCB_Text;
   textObjNew : IPCB_Text;
   found      : Boolean;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('*Hello from CPL_AlterDrillChartFootprint()');

   { Setup an iterator so that we can iterate over all texts in this PCB component. }
   iterator        := component.GroupIterator_Create;
   iterator.AddFilter_ObjectSet(MkSet(eTextObject));
   iterator.AddFilter_LayerSet(AllLayers);

   { Get a reference to the first PCB object. }
   textObj := iterator.FirstPCBObject;

   { Loop over all objects in this PcbDoc file. }
   found      := False;
   while ( (textObj <> nil) and (not found) ) do
   begin
      
      WriteToDebugFile('*Found text ' + textObj.Text);

      { See if we have a line of Comment text to assign to this text string. }
      if ( (textObj.Text = '.Legend') or (textObj.Text = 'Legend is not interpreted until output') ) then
      begin
         
         WriteToDebugFile('*About to move Legend text to standalone string!');

         { Flag that we have found the string in question. }
         found      := True;

         { Create new string.  Replicating results in the new string also being part of the footprint, which we don't want! }
         textObjNew := PCBServer.PCBObjectFactory(eTextObject, eNoDimension, eCreate_Default);
         
         { Notify Altium that PCB object will be modified. }
         PCBServer.SendMessageToRobots(textObjNew.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_NoEventData);

         { Copy properties from old text object to new one. }
         { NOTE:  SPI-ism:  We are moving the .Legend string by magic numbers.
          It seemed pointless to update the footprint, since the .Legend string needs to be moved
          out of the footprint anyway! }
         textObjNew.Text			:= '.Legend';
         textObjNew.Xlocation		:= textObj.Xlocation + MilsToCoord(70);
         textObjNew.Ylocation 		:= textObj.Ylocation + MilsToCoord(10);
         textObjNew.Layer			:= textObj.Layer;
         textObjNew.Size			:= textObj.Size;
         textObjNew.Rotation		:= textObj.Rotation;
         textObjNew.Width			:= textObj.Width;

         { Add new string to board. }
         board.AddPCBObject(textObjNew);

         { Notify Altium that board has been modified. }
         PCBServer.SendMessageToRobots(textObjNew.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_NoEventData);
         PCBServer.SendMessageToRobots(board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, textObjNew.I_ObjectAddress);

         { Delete the .Legend string from the footprint. }
         board.RemovePCBObject(textObj);
         
         { Notify Altium that board has been modified. }
         PcbServer.PostProcess;

      end; { endif }
      
      { Advance to next textObj in this Pcb component. }
      textObj := iterator.NextPCBObject;

   end;  { endwhile loop over all PCB texts in this Pcb component. }

   { Free group iterator. }
   component.GroupIterator_Destroy(iterator);

end; { end CPL_AlterDrillChartFootprint() }


{***************************************************************************
 * function CPL_FindInstructionPcbComponents()
 *  Iterate through all footprints in PcbDoc file and try to find instruction-type
 *  footprints.
 ***************************************************************************}
function CPL_FindInstructionPcbComponents(multiLineFootprintLibRefs  : TStringList;
                                          drillChartFootprintLibRefs : TStringList;
                                          )                          : Integer;

var
   board           : IPCB_Board;
   i               : Integer;
   iterator        : IPCB_BoardIterator;
   component       : IPCB_Component;
   numComps        : Integer;
   isMultiLineInsn : Boolean;
   isDrillChart    : Boolean;
   
begin

   { Assume success. }
   result := 0;

   { Flag that we have not yet found any components. }
   numComps := 0;

   { Attempt to start PCB server. }
   Client.StartServer(constKindPcb);

   { Tell the PCB Server to get ready for us. }
   PCBServer.PreProcess;

   { Check if PCB server is alive. }
   if (PCBServer = Nil) then
      MyAbort('PCBServer is Nil.  D''oh.');
   
   { Initialize the PCB editor. }
   PCBServer.PreProcess;

//   { I cannot get PCBServer.GetPCBBoardByPath(pcbDocPath) to work, so I'm going
//    to manually open the PCB document and then use PCBServer.GetCurrentPCBBoard. }
//   ResetParameters;
//   AddStringParameter('ObjectKind', 'Document');
//   AddStringParameter('FileName', importedPcbDoc.DM_FullPath);
//   RunProcess('WorkspaceManager:OpenObject');
   
   { Attempt to open the project's PcbDoc file. }
   { FIXME:  Why does this not work?? GetPCBBoardByPath(pcbDocPath); }
   board := PCBServer.GetCurrentPCBBoard;

   { Sanity check }
   if (board = Nil) then
      CLF_Abort('Unable to open PcbDoc file ' + pcbDocPath);

   { Setup an iterator so that we can iterate over all PCB components. }
   iterator        := board.BoardIterator_Create;
   iterator.AddFilter_ObjectSet(MkSet(eComponentObject));
   iterator.AddFilter_LayerSet(AllLayers);
   iterator.AddFilter_Method(eProcessAll);

   { Get a reference to the first PCB object. }
   component := iterator.FirstPCBObject;

   { Loop over all objects in this PcbDoc file. }
   while (component <> nil) do
   begin
      
      WriteToDebugFile('*Found component ' + component.SourceDesignator );
      WriteToDebugFile('* Comment.Text is "' + component.Comment.Text + '".');
      WriteToDebugFile('* Name.Text is "' + component.Name.Text + '".');
      WriteToDebugFile('* SourceLibReference is "' + component.SourceLibReference + '".');
      
      { Increment the number of components that we've found. }
      numComps := numComps + 1;

      { See if this is a multi-line instruction footprint. }
      isMultiLineInsn := CPL_IsThisMultiLineInsnFootprint(multiLineFootprintLibRefs,
                                                          component);

      { If so, then proceed to alter certain text strings within this Pcb component. }
      if (isMultiLineInsn) then
         begin
            CPL_AlterTextInMultilineFootprint(board,
                                              {var} component);

         end; { endif }

      
      { See if this is the drill-chart footprint. }
      isDrillChart := CPL_IsThisDrillChart(drillChartFootprintLibRefs,
                                           component);
      
      { If so, then proceed to move ".Legend" text outside the footprint. }
      if (isDrillChart) then
         begin
            CPL_AlterDrillChartFootprint(board,
                                         {var} component);

         end; { endif }


      { Advance to next component in this PcbDoc file. }
      component := iterator.NextPCBObject;

   end;  { endwhile loop over all PCB components in this PcbDoc file. }

   { Free PCB component iterator. }
   board.BoardIterator_Destroy(iterator);

   { Attempt to unlock primitives for our one component. }
//   component.SetState_PrimitiveLock(False);

   


   {* Save modified PcbDoc file. *}
   ResetParameters;
   AddStringParameter('ObjectKind','FocusedDocument');
//   RunProcess('WorkspaceManager:SaveObject');   

   
   {* Ask the GUI to redraw the screen for us. *}
   ResetParameters;
   AddStringParameter('Action','Redraw');
   RunProcess('PCB:Zoom');
   //   PCBBoard.GraphicallyInvalidate;

   

end; { end CPL_FindInstructionPcbComponents() }


{***************************************************************************
 * procedure SPI_Cleanup_PcbDoc_Layout()
 *  Script entry point.
 *
 *  Returns:  (nothing--procedure, not a function).
 ***************************************************************************}
procedure SPI_Cleanup_PcbDoc_Layout;
var
   WorkSpace                  : IWorkSpace;
   project                    : IProject;
   projectPath                : TDynamicString;
   projOutPath                : TDynamicString;
   projectName                : TDynamicString;
   projLogPath                : TDynamicString;
   scriptsPath                : TDynamicString;
   document                   : IDocument;
   timestamp                  : TDynamicString;
   startTime                  : TDateTime;
   endTime                    : TDateTime;
   rc                         : Integer;
   i                          : Integer;
   step                       : Integer;
   multiLineFootprintLibRefs  : TStringList;
   drillChartFootprintLibRefs : TStringList;
   
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

   { Initialize string lists used by this script. }
   CPL_Init({var} multiLineFootprintLibRefs,
            {var} drillChartFootprintLibRefs);

   { Issue confirmation modal dialog box with specified confirmation message,
    specified reply after clicking Ok, and specified reply after clicking Cancel. }
   IssueConfirmationWithOkOrCancel('Welcome to script ' + constThisScriptNameNoExt + ', ' + constScriptVersion + '.' + constLineBreak + constLineBreak + 
                                   'This script will attempt to cleanup all SPI-specific instruction footprints in the PcbDoc layout file in project "' + projectName + '".' + constLineBreak +
                                   'It will also do a cleanup operation on the Drill Chart component.' + constLineBreak +
                                   constLineBreak + 
                                   'Shall I run on this project (OK) or shall I Cancel running this script?',
                                   '',
                                   'Canceled at user request.');


   { Try to find all instruction-type PCB components and modify them as needed. }
   CPL_FindInstructionPcbComponents(multiLineFootprintLibRefs,
                                    drillChartFootprintLibRefs);
   
   { Try to close all project documents for the actual project. }
//   CLF_UpdateGuiStatusMessage('Closing all documents in project before starting script.');
//   ResetParameters;
//   AddStringParameter('ObjectKind', 'FocusedProjectDocuments');
//   RunProcess('WorkspaceManager:CloseObject');



   
   { Record the wall clock time when we ended this script. }
   endTime := Now();
   
   { Timestamp the end of our actions, before we present the last dialog box to the user. }
   WriteToDebugFile('');
   WriteToDebugFile('**Script ' + constThisScriptName + ' ending at ' + DateTimeToStr(Date) + ' ' + TimeToStr(endTime));
   WriteToDebugFile('**Script took ' + FormatDateTime('h:n:s', (endTime-startTime)) + ' (hrs:mins:secs) to run on this project on this PC.');


   ShowMessage('Script has completed successfully.' + constLineBreak + constLineBreak +
               'You still need to:' + constLineBreak +
               '1.  Review changes.' + constLineBreak +
               '2.  Save file if everything looks good.');


   {****** Wrap things up ******}
   { Call AtExit() procedure to write debug outputs to file. }

   { Free string lists. }
   multiLineFootprintLibRefs.Free;
   drillChartFootprintLibRefs.Free;
   
   WriteToDebugFile('**About to exit script.');
//   ShowMessage('About to call AtExit()');
   AtExit(0);                   { Report success at exit }
//   ShowMessage('Back from AtExit()');

end; { end SPI_Cleanup_PcbDoc_Layout() }

end.

