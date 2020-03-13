{***************************************************************************
 SPI_Setup_PCB_Layers.pas
 Altium DelphiScript (basically Pascal) that will attempt to setup SPI-specific
 layer names (copper, silkscreen, soldermask, solderpaste, mechanical) and
 configure needed mechanical layer pairs.
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
 * Global constants.
 ***************************************************************************}
const
{* Declare the version and name of this script. *}
   constScriptVersion          = 'v0.0.2 $Revision$';
   constThisScriptNameNoExt    = 'SPI_Setup_PCB_Layers';
   constThisScriptName         = constThisScriptNameNoExt + '.pas';


{***************************************************************************
 * function SPL_SetLayerName()
 *  Setup a more-human-readable layer name for the specified layer
 *  (eg. "Silkscreen Top" instead of "Top Overlay".
 ***************************************************************************}
function SPL_SetLayerName(layerStack : IPCB_LayerStack;
                          layer      : Integer;
                          layerName  : TString;
                          )          : Integer;

var
   layerObj  :  IPCB_LayerObject;
   
begin

   { Assume success. }
   result := 0;
   
   { Configure layer name. }
   layerObj := layerStack.LayerObject(layer);
   layerObj.Name := layerName;

end; { end SPL_SetLayerName() }


{***************************************************************************
 * function SPL_SetupMechPair()
 *  Iterate through all layers (not just those in the layer stack) and
 *  setup SPI-specific layer names.
 ***************************************************************************}
function SPL_SetupMechPair(layerStack : IPCB_LayerStack;
                           mechPairs  : IPCB_MechanicalLayerPairs;
                           layer1     : Integer;
                           layer2     : Integer;
                           )          : Integer;

var
   layerObj  :  IPCB_MechanicalLayer;
   PCBBoard   : IPCB_Board;
   
begin

   { Assume success. }
   result := 0;

   { This should succeed, since we've done it before anyway. }
   PCBBoard  :=  PCBServer.GetCurrentPCBBoard;
   
   { Enable mech layer 1. }
   layerObj := layerStack.LayerObject(layer1);
   layerObj.SetState_MechLayerEnabled(True);

   { Enable mech layer 2. }
   layerObj := layerStack.LayerObject(layer2);
   layerObj.SetState_MechLayerEnabled(True);

   { Display mech layer 1. }
   layerObj := layerStack.LayerObject(layer1);
   layerObj.IsDisplayed[PCBBoard] := True;
//   PCBBoard.SetState_LayerIsDisplayed(layer1) := True;

   { Display mech layer 2. }
   layerObj := layerStack.LayerObject(layer2);
   layerObj.IsDisplayed[PCBBoard] := True;
//   PCBBoard.SetState_LayerIsDisplayed(layer2) := True;


   { Setup layer pair for specified mech layers. }
   mechPairs.AddPair(layer1,
                     layer2);

end; { end SPL_SetupMechPair() }


{***************************************************************************
 * procedure SPI_SetupPcbLayers()
 *  Iterate through all layers (not just those in the layer stack) and
 *  setup SPI-specific layer names.
 ***************************************************************************}
procedure SPI_SetupPcbLayers;
var
   PCBBoard   : IPCB_Board;
   layerStack : IPCB_LayerStack;
   i          : Integer;
   layer      : Integer;
   layerName  : TString;
   layerNum   : Integer;
   mechPairs  : IPCB_MechanicalLayerPairs;
   
begin

   { Attempt to get a reference to current PCB board. }
   PCBBoard  :=  PCBServer.GetCurrentPCBBoard;
   If  PCBBoard  =  Nil  Then  Exit;

   { Attempt to get a reference to the PCB layer stack. }
   layerStack  :=  PCBBoard.LayerStack;
   If  layerStack  =  Nil  Then  Exit;

   { Loop over all internal routing layers. }
   layerNum := 1;
   for layer := eMidLayer1 to eMidLayer30 do
   begin
      
      { Configure internal layer name. }
      layerName := 'Routing ' + IntToStr(layerNum);
      SPL_SetLayerName(layerStack,
                       layer,
                       layerName);

      { Increment human readable layer name. }
      layerNum := layerNum + 1;
   end;

   { Loop over all internal plane layers. }
   layerNum := 1;
   for layer := eInternalPlane1 to eInternalPlane16 do
   begin
      
      { Configure internal layer name. }
      layerName := 'Plane ' + IntToStr(layerNum);
      SPL_SetLayerName(layerStack,
                       layer,
                       layerName);

      { Increment human readable layer name. }
      layerNum := layerNum + 1;
   end;

   { Configure copper layer names. }
   SPL_SetLayerName(layerStack,
                    {layer} eTopLayer,
                    {layerName} 'Top Copper');
   
   SPL_SetLayerName(layerStack,
                    {layer} eBottomLayer,
                    {layerName} 'Bot Copper');

   { Configure silkscreen layer names. }
   SPL_SetLayerName(layerStack,
                    {layer} eTopOverlay,
                    {layerName} 'Silkscreen Top');
   
   SPL_SetLayerName(layerStack,
                    {layer} eBottomOverlay,
                    {layerName} 'Silkscreen Bot');

   { Configure soldermask layer names. }
   SPL_SetLayerName(layerStack,
                    {layer} eTopSolder,
                    {layerName} 'Soldermask Top');
   
   SPL_SetLayerName(layerStack,
                    {layer} eBottomSolder,
                    {layerName} 'Soldermask Bot');

   { Configure solderpaste layer names. }
   SPL_SetLayerName(layerStack,
                    {layer} eTopPaste,
                    {layerName} 'Solderpaste Top');
   
   SPL_SetLayerName(layerStack,
                    {layer} eBottomPaste,
                    {layerName} 'Solderpaste Bot');

   {* Configure mechanical layer names. *}

   { Configure assembly drawing layer names. }
   SPL_SetLayerName(layerStack,
                    {layer} eMechanical1,
                    {layerName} 'Assy Top');
   
   SPL_SetLayerName(layerStack,
                    {layer} eMechanical2,
                    {layerName} 'Assy Bot');
   
   { Configure pin landing drawing layer names. }
   SPL_SetLayerName(layerStack,
                    {layer} eMechanical3,
                    {layerName} 'Pinland Top');
   
   SPL_SetLayerName(layerStack,
                    {layer} eMechanical4,
                    {layerName} 'Pinland Bot');
   
   { Configure component body layer names. }
   SPL_SetLayerName(layerStack,
                    {layer} eMechanical5,
                    {layerName} 'Compbody Top');
   
   SPL_SetLayerName(layerStack,
                    {layer} eMechanical6,
                    {layerName} 'Compbody Bot');
   
   { Configure courtyard layer names. }
   SPL_SetLayerName(layerStack,
                    {layer} eMechanical7,
                    {layerName} 'Courtyard Top');
   
   SPL_SetLayerName(layerStack,
                    {layer} eMechanical8,
                    {layerName} 'Courtyard Bot');
   
   { Configure courtyard body layer names. }
   SPL_SetLayerName(layerStack,
                    {layer} eMechanical9,
                    {layerName} 'Courtbody Top');
   
   SPL_SetLayerName(layerStack,
                    {layer} eMechanical10,
                    {layerName} 'Courtbody Bot');
   
   { Configure courtyard body layer names. }
   SPL_SetLayerName(layerStack,
                    {layer} eMechanical11,
                    {layerName} 'EngNotes Top');
   
   SPL_SetLayerName(layerStack,
                    {layer} eMechanical12,
                    {layerName} 'EngNotes Bot');
   
   { Configure fab drawing. }
   SPL_SetLayerName(layerStack,
                    {layer} eMechanical13,
                    {layerName} 'Fab Drawing');
   
   { Configure global layer names. }
   SPL_SetLayerName(layerStack,
                    {layer} eMechanical14,
                    {layerName} 'Global');

   SPL_SetLayerName(layerStack,
                    {layer} eMechanical15,
                    {layerName} 'Global_Mir');

   
   {* Setup all required mechanical layer pairs. *}
   mechPairs  := PCBBoard.MechanicalPairs;

   { Clear all existing mech layer pairs. }
   mechPairs.Clear;
//   ShowMessage('Cleared mech layer pairs!');

   { Setup mech layer pair for assembly drawings. }
   SPL_SetupMechPair(layerStack,
                     mechPairs,
                     {layer1} eMechanical1,
                     {layer2} eMechanical2);

   { Setup mech layer pair for pin landing drawings. }
   SPL_SetupMechPair(layerStack,
                     mechPairs,
                     {layer1} eMechanical3,
                     {layer2} eMechanical4);
   
   { Setup mech layer pair for component bodies. }
   SPL_SetupMechPair(layerStack,
                     mechPairs,
                     {layer1} eMechanical5,
                     {layer2} eMechanical6);

   { Setup mech layer pair for courtyards. }
   SPL_SetupMechPair(layerStack,
                     mechPairs,
                     {layer1} eMechanical7,
                     {layer2} eMechanical8);

   { Setup mech layer pair for courtyard bodies. }
   SPL_SetupMechPair(layerStack,
                     mechPairs,
                     {layer1} eMechanical9,
                     {layer2} eMechanical10);

   { Setup mech layer pair for engineering notes. }
   SPL_SetupMechPair(layerStack,
                     mechPairs,
                     {layer1} eMechanical11,
                     {layer2} eMechanical12);

   {* Save modified PcbDoc file. *}
   ResetParameters;
   AddStringParameter('ObjectKind','FocusedDocument');
   RunProcess('WorkspaceManager:SaveObject');   

   
   {* Ask the GUI to redraw the screen for us. *}
   ResetParameters;
   AddStringParameter('Action','Redraw');
   RunProcess('PCB:Zoom');
   //   PCBBoard.GraphicallyInvalidate;

   { Even this doesn't seem to redraw the layer tabs, since we've enabled additional mechanical layers. ;-( }
   //Full PCB system update
   PCBBoard.ViewManager_FullUpdate;
   // Refresh PCB screen
   Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
   
   ShowInfo('All done!  Configured layer names and mechanical layer pairs.  PcbDoc file saved.');

end; { end SPI_SetupPcbLayers() }
