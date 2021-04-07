{***************************************************************************
 SPI_Cleanup_LPW_Footprint.pas
 Altium DelphiScript (basically Pascal) that will attempt to cleanup a new
 footprint imported from Mentor LP Wizard tool. 
 ***************************************************************************}

{***************************************************************************
 * Sierra Photonics Inc. original / modified / updated code is subject to:
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
 * Some code additons/changes authored by Kevin Yee kyee4@jhu.edu
 ***************************************************************************}


{***************************************************************************
 * Some code borrowed from FormatPaintBrush.pas downloaded from:
 * https://altium-designer-addons.googlecode.com/svn/trunk/FormatPaintBrush/FormatPaintBrush.pas
 * Created by:    Petar Perisin                                               
 ***************************************************************************}

{***************************************************************************
 * Some code borrowed from AutoSTEPplacer.pas downloaded from:
 * https://altium-designer-addons.googlecode.com/svn/trunk/AutoSTEPplacer/AutoSTEPplacer.pas
 * Created by:    Petar Perisin                                               
 ***************************************************************************}

{***************************************************************************
 * Some code borrowed from CreateComponentBody.pas downloaded from:
 * http://forum.live.altium.com/#posts/64592 CreateComponentBody.zip
 * Created by:    Darren Moore
 ***************************************************************************}

{***************************************************************************
 * Some code borrowed from ComponentBodyExample.pas downloaded from:
 * http://forum.live.altium.com/#posts/64592?os=20 ComponentBodyExample.zip
 * Created by:    Paul Nicholls 
 ***************************************************************************}

{***************************************************************************
 * Some code borrowed from ConstructRegionsFromContourSet.pas.
 * This is an example script file supplied by Altium as
 *  "Scripts/Delphiscript Scripts/PCB/CreateRegionsFromBitmap/ConstructRegionsFromContourSet.pas"
 ***************************************************************************}

{***************************************************************************
 * Some code borrowed from ConstructContourSetFromPicture.pas.
 * This is an example script file supplied by Altium as
 *  "Scripts/Delphiscript Scripts/PCB/CreateRegionsFromBitmap/ConstructContourSetFromPicture.pas."
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
 *  1.  Tested with Altium 10 & 13.  (May or may not work with Altium 9--not tested.)
 *  2.  Tested with Windows 7 x64.
 *  3.  This file should no longer have TAB characters in it.
 *
 * THEORY OF OPERATIONS
 * This script is part of a multiscript process that will create a footprint and 3D
 * model and attach them all together in an Altium component library file (PcbLib).
 * This script should be run from Altium itself. The script will prompt a user to
 * choose a file generated by Mentor Graphics' LP Wizard and will proceed to import
 * that file into Altium while adding and removing features based on the desire of
 * the user. It will also call FC3DM_IC.py to generate a 3D model of the component
 * using the scripting interface of FreeCAD.
 *  
 * WHAT THIS SCRIPT WILL DO:
 * This script will do the following:
 *    Import footprint generated from Mentor Graphics' LP Wizard
 *    Convert the PcbDoc into a PcbLib
 *    Cleanup the things that get misplaced during import (i.e. regions on the keepout layer)
 *    Incorporate a 3D model (either pre-made or automatically generated) into the footprint
 *    Keep a text file that describes all footprint features (so that subsequent runs are
 *       treated as an identical footprint rather than a new footprint)
 *  
 * The overarching design goal is for the script to cleanup after itself when/if it crashes. 
 * 
 * WHAT THIS SCRIPT WILL *NOT* DO:
 * This script can only import footprints from LP Wizard. Any other footprints will cause the
 * script to abort. Throughout the script some assumptions (that are documented within the code) and
 * accompanying these assumptions are sanity checks that will abort the script if one of those
 * assumptions is violated.
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
   TCleanupLpwFootprintForm = class(TForm)
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
   procedure TCleanupLpwFootprintForm.clickedOk(Sender : TPanel);
   procedure TCleanupLpwFootprintForm.clickedCancel(Sender : TPanel);
   procedure TCleanupLpwFootprintForm.bCheck0_1(Sender : TPanel);
end;

   
{***************************************************************************
 * Function forward declarations.
 ***************************************************************************}
function CLF_CreateNewTrackRectangle2(    layer           : Integer;
                                          widthMm         : Real;
                                          boundaryWestMm  : Real;
                                          boundaryEastMm  : Real;
                                          boundaryNorthMm : Real;
                                          boundarySouthMm : Real;
                                      var trackQueue      : TInterfaceList;
                                          namePrefix      : TString;
                                      var primNames       : TStringList;
                                          )               : Integer; forward;

function CLF_ExtrudeGeometricPolygonInto3d(    boardSide         : Integer;
                                               layer             : Integer;
                                               colorText         : Integer;
                                               colorBackground   : Integer;
                                               opacity           : Real;
                                               overallHeightMm   : Real;
                                               standoffHeightMm  : Real;
                                               AGeometricPolygon : IPCB_GeometricPolygon;
                                               identifier        : TString;
                                               doQueueRegions    : Boolean;
                                               doQueueBodies     : Boolean;
                                           var bodyDst           : IPCB_ComponentBody;
                                           var regionQueue       : TInterfaceList;
                                           var bodyQueue         : TInterfaceList;
                                           var primNames         : TStringList;
                                           var boundsList        : TStringList;
                                               )                 : Integer; forward;

{***************************************************************************
 * Global constants.
 ***************************************************************************}
const
{* Declare the version and name of this script. *}
   constScriptVersion          = 'v0.16.32 $Revision$';
   constThisScriptNameNoExt    = 'SPI_Cleanup_LPW_Footprint';
   constThisScriptName         = constThisScriptNameNoExt + '.pas';
{}
   { Suffix that will be appended to all 3d models searched for or generated. }
   const3dModelCompanySuffix   = 'TRT';
{}
   { Constants related to courtyard. }
   constOldWidthCourtyardMm    = 0.05;      { Width of courtyard lines in mm in LP Wizard source. }
   constNewWidthCourtyardMm    = 0.0254;    { Width of courtyard lines in mm in our destination library. }
   constNewLayerCourtyard      = eMechanical7; { Mech layer for courtyard lines in our destination libary. }
{}
   { Constants related to courtbody. }
   constWidthCourtBodyBorderMm = 0.025;     { Width of lines we will add diagonally between courtyard outline and courtbody 3D region. }
   constDeltaXyCourtBodyBorderMm = 0.15;    { Offset in x and y from courtyard outline to courtbody 3D region. }
   constBoardSideCourtBody     = eBoardSide_Top; { Board side for courtbody extrusion. }
   constLayerCourtBody         = eMechanical9; { Mech layer for courtbody in our destination libary. }
   constCourtBodyColor         = 127485;    { 3D color for courtbody extrusion.  This is a bright yellow color. }
   constCourtBodyOpacity       = 0.0;       { 3D opacity for courtbody extrusion.  (0 = completely hidden) }
   constOverallHeightCourtBodyMm = 0.0;     { Overall height of courtbody extrusion. }
   constStandoffHeightCourtBodyMm= -0.001;  { Standoff height of courtbody extrusion. }
   constNameCourtBody          = 'Courtyard_body';
{}
   { Constants related to extruded 3D component body.  Also used for real STEP model when available. }
   constBoardSideCompBody      = eBoardSide_Top; { Board side for compbody extrusion. }
   constLayerCompBody          = eMechanical5; { Mech layer for compbody in our destination libary. }
   constCompBodyColor          = 4404530;   { 3D color for compbody extrusion.  This is a mostly black color. }
   constCompBodyOpacity        = 1.0;       { 3D opacity for compbody extrusion.  (1 = completely opaque) }
   constCompBodyOpacityThermal = 0.0;       { 3D opacity for compbody extrusion for thermal modeling.  (0 = completely hidden) }
   constCompBodyBallColor      = 10921648;  { 3D color for a compbody BGA sphere.  This is a medium gray color. }
   constCompBodyChipInd        = 8287349;   { 3D color for chip inductor body.  This is a darker gray color. }
   constCompBodyMoldCap        = 61183;     { 3D color for molded capacitor body.  This is a bright yellow color. }
   constCompBodyPinHeightMoldRatio = 0.45;  { Multiply package height by this ratio to get the height of molded pin. }
   constCompBodyPinThicknessMoldMm = 0.1;   { Thickness (z height) of a molded capacitor pin. }
   constCompBodyPinThicknessQfnMm = 0.2;    { Thickness (z height) of a DFN/QFN pin. }
   constCompBodyPinThicknessSoicMm = 0.2;   { Thickness (z height) of an SOIC/SOP/SOT pin. }
   constCompBodyPinEscapeHeightSoicMult = 0.5; { Specify that for SOIC/SOP/QFP/etc., pins disappear into plastic body 50% of the way up the plastic body height. }
   constCompBodyPinEscapeHeightSotMult = 0.66; { Specify that for SOT, pins disappear into plastic body 66% of the way up the plastic body height. }
   constNameCompBody           = 'Component_body';
   constNameCompBodyThermal    = 'Component_body_thermal';
   constP1soicChamferOffset    = 0.25;       { Default pin 1 chamfer offset for SOICs if not found in cnfGalacticInfo }
{}
   { Constants related to extruded 3D component body pin 1 marker. }
   constNomCompBodyPin1MarkArcRadMm   = 0.5;    { Nominal sized compbody pin 1 mark arc radius. }
   constMinCompBodyPin1MarkArcRadMm   = 0.3;    { Minimal sized compbody pin 1 mark arc radius. }
   constCompBodyPin1MarkColor  = 16777215;  { 3D color for compbody pin 1 marker.  This is a bright white. }
   constCompBodyMarkHeight     = 0.001;     { Height of the pin 1 marker cylinder over and above component height. }
   constCompBodyMoldCapAnodeMarkLen = 0.4;  { For molded capacitor, length of anode marker bar. }
{}
   { Constants related to pin landing drawing. }
   constBoardSidePinLand       = eBoardSide_Top; { Board side for pin landing drawing. }
   constLayerPinLand           = eMechanical3; { Mech layer for compbody in our destination libary. }
   constWidthPinLand           = 0.025;     { Width of pin landing drawing lines in our destination library. }
{}
   { Constants related to assembly drawing. }
   constOldWidthAssyDrawingMm  = 0.001;     { Width of assembly drawing lines in mm in LP Wizard source. }
   constNewWidthAssyDrawingMm  = 0.15;      { Width of assembly drawing lines in our destination library. }
   constNewWidthAssyDrawingChipMm= 0.1;     { For chip components, width of assembly drawing lines in our destination library. }
   constNewLayerAssyDrawing    = eMechanical1;  { Mech layer for assembly drawing lines in our destination libary. }
{}
   { Constants related to inner assembly outline. }
   constWidthThinAssyDrawingMm = 0.08;      { Width of thin assembly drawing lines in our destination library. }
   constNomDeltaXyAssyToInnerAssyMm = 0.25; { Nominal offset in x and y from assembly outline to inner assembly outline. }
   constMinDeltaXyAssyToInnerAssyMm = 0.15; { Minimal offset in x and y from assembly outline to inner assembly outline. }
   constSoicDeltaXWestAssyToInnerAssyMm = 0.55; { For SOIC footprints, Offset in x on west side of part from from assembly outline to inner assembly outline. }
   constThreshSmallAssyOutlineMm  = 2.11;     { Threshhold in x or y of assembly outline to declare a component "small". }
   constDeltaYInnerAssyToTextMm = 0.15;     { Offset in y from inner assembly outline to assembly text. }
   constDeltaXAssyTextToTextMm = 0.75;      { Offset in x from .Comment to .Designator. }
{}
   { Constants related to assembly text. }
   constLineWidthAssyTextMm    = 0.0762;    { Width of text in mm (.Comment and .Designator) that we will add to assembly drawing. }
   constHeightAssyTextMm       = 0.381;     { Height of text in mm (.Comment and .Designator) that we will add to assembly drawing. }
   constStrokeFontAspectRatio  = 0.826;     { Mutiply constHeightAssyTextMm by this to get the approximate character width, including char-to-char spacing.  Empirically determined. }
   constRotationAssyText       = 270;       { Rotation of assembly text that we will add to assembly drawing. }
   constAllowNumCommentChars   = 8;         { Check that .Comment can grow to this many chars.  If not, break assembly outline to accommodate it. }
{}
   { Constants related to assembly pin 1 marker. }
   constNomAssyPin1MarkArcRadMm   = 0.5;    { Nominal sized assembly pin 1 mark arc radius. }
   constNomAssyPin1MarkArcWidthMm = 0.1;    { Nominal sized assembly pin 1 mark arc line width. }
   constMinAssyPin1MarkArcRadMm   = 0.1;    { Minimal sized assembly pin 1 mark arc radius. }
   constMinAssyPin1MarkArcWidthMm = 0.25;   { Minimal sized assembly pin 1 mark arc line width. }
{}
   { Constants related to silkscreen interior pin 1 marker. }
   constIntSilkPin1MarkArcRadMm   = 0.125;  { Interior silkscreen pin 1 mark arc radius. }
   constIntSilkPin1MarkArcWidthMm = 0.25;   { Interior silkscreen pin 1 mark arc line width. }
   constIntSilkPin1MarkSpaceMm    = 0.05;   { Spacing between silkscreen body and nearest part of arc. }
   constSilkPin1MarkXThreshCenterMm = 1.00; { Threshold where if the silkscreen body is less than this, then center the pin 1 marking in x. }
   constSilkLayer                 = eTopOverlay;
   constWidthSilkMm               = 0.2;    { Width of silkscreen lines before and after. }
   constYlenMoldCapTriangle       = 0.5;    { Y length of leg of triangle anode marker for molded cap package. }
   constLenMoldCapCross           = 0.5;    { X & Y length of cross marker for anode of molded cap package. }
   constWidthMoldCapCross         = 0.15;   { Line width of cross marker for molded cap package. }
   constLenAnodeExtMoldCap        = 0.5;    { Length of "extension" of anode bar that exists on silkscreen. }
{}
   { Constants related to 3D text. }
   const3dTextInitialHeightMm     = 0.1;    { Initial text height in mm. }
   const3dTextIncrementHeightMm   = 0.025;  { Incremental change to text height in mm. }
{}
   { Constants related to unwanted features of LP Wizard generated footprint. }
   constOldWidthLinesToKill       = 0.10;   { Width of LP Wizard assembly lines that we just want to kill off. }
{}
   { Constants related to solderpaste tiling regions. }
   constOldLayerSolderPasteRegion = eKeepOutLayer;  { Coming from LP Wizard, solderpaste regions appear on KeepOut layer. }
   constNewLayerSolderPasteRegion = eTopPaste;      { Move these to solderpaste top layer. }
{}
   { Constants related to EP pin. }
   constNewNameOfEp               = 'EP';      { New name of center exposed pad (mostly DFN/QFN packages). }
   constPrefixEpPins              = constNewNameOfEp; { Name prefix of EP pins. }
{}
   { Constants related to fiducial pins. }
   constPrefixFiducialPins         = 'FID'; { Name prefix of fiducial pins. }
   constPasteExpansionSuppressMm   = -10.0; { What to set the solderpaste expansion to in order to suppress all solderpaste. }
   constFiducialKeepoutWidthMm     = 0.001; { Width in mm of keepout arc to create around fiducial pins. }
   constFiducialLayer              = eTopLayer; { Standard layer for fiducial pins. }
{}
   { Named parameters that we will pass around from function to function. }
   constParmHaveSolderPasteTiles = 'HaveSolderPasteTiles';
{}
   { Other things to define so they don't end up as magic names / magic numbers. }
   constMagicalExclude3dTextName = 'MagicalExclude3dTextName';  { Name 3D text this when adding to textQueue so that we suppress its (double) reporting later on. }
   constMagicalExclude3dRegionName = 'MagicalExclude3dRegionName';  { Name 3D region this when adding to regionQueue so that we suppress its (double) reporting later on. }
{}
   { Names of secret text strings that we will create for later use by PCB helper scripts. }
   constClfSecretInfoLibName       = 'Fp_info_baseline_Info_lib_name';
   constClfSecretAssyOutline       = 'Fp_info_baseline_Assembly_outline';
   constClfSecretAssyOutlineInner  = 'Fp_info_baseline_Assembly_outline_inner';
   constClfSecretAssyArcPin1       = 'Fp_info_baseline_Assembly_arc_marker_pin_1';
   constClfSecretAssyComment       = 'Fp_info_baseline_Assembly_.Comment';
   constClfSecretAssyDesignator    = 'Fp_info_baseline_Assembly_.Designator';
   constClfSecretAssyCommentRot    = 'Fp_info_baseline_Assembly_.CommentRot';
   constClfSecretAssyDesignatorRot = 'Fp_info_baseline_Assembly_.DesignatorRot';
   constClfSecretCourtyardOutline  = 'Fp_info_baseline_Courtyard_outline';
   constClfSecretCourtbodyOutline  = 'Fp_info_baseline_Courtbody_outline';
   constClfSecretFillSoldermaskSliver= 'Fp_info_baseline_Fill_for_soldermask_slivers_';
   constClfSecretPinFiducial       = 'Fp_info_baseline_Pin_fiducial_';
   constClfSecretVaultGUID         = 'Fp_info_Vault_GUID';
   constClfSecretInfoFootprintType = 'Fp_info_footprint_type';
{}
   { Keys to dealing with padGroup list. }
   { Warning:  Don't reorder these! }
   constPadGroupReservedDontUse    = 0;
   constPadGroupWest               = 1;
   constPadGroupEast               = 2;
   constPadGroupNorth              = 3;
   constPadGroupSouth              = 4;
   constPadGroupCenter             = 5;
   constPadGroupEp                 = 6;
   constPadGroupUnknown            = 7;
{}
   { Names of keys in the galactic info list. }
   constGilFootprintType           = 'FootprintType';
   constGilOldPlb09FileName        = 'plb09OldFileName';
   constGilOldPadsFileName         = 'padsOldFileName';
   constGilPlb09FileName           = 'plb09FileName';
   constGilPadsFileName            = 'padsFileName';
   constGilLogFileName             = 'logFileName';
   constGilProjectFileName         = 'ProjectFileName';
   constGilLibraryFileName         = 'LibFileName';
   constGilLibraryFileNameSuffix   = 'LibFileNameSuffix';
   constGilLibraryFileDescription  = 'LibFileDescription';
   constGilHasEp                   = 'HasEp';
   constGilFixExposedPad           = 'fixExposedPad';
   constGilEpWidthRounded            = 'epWidthRounded';
   constGilEpLengthRounded           = 'epLengthRounded';
{}
   { Keys related to modifying tracks. }
   constGilModTracksOldLayer1      = 'ModTracksOldLayer1';
   constGilModTracksOldWidthMm1    = 'ModTracksOldWidthMm1';
   constGilModTracksNewLayer1      = 'ModTracksNewLayer1';
   constGilModTracksNewWidthMm1    = 'ModTracksNewWidthMm1';
   
   constGilModTracksOldLayer2      = 'ModTracksOldLayer2';
   constGilModTracksOldWidthMm2    = 'ModTracksOldWidthMm2';
   constGilModTracksNewLayer2      = 'ModTracksNewLayer2';
   constGilModTracksNewWidthMm2    = 'ModTracksNewWidthMm2';
   
   constGilModTracksOldLayer3      = 'ModTracksOldLayer3';
   constGilModTracksOldWidthMm3    = 'ModTracksOldWidthMm3';
   constGilModTracksNewLayer3      = 'ModTracksNewLayer3';
   constGilModTracksNewWidthMm3    = 'ModTracksNewWidthMm3';

   constGilModTracksOldLayer4      = 'ModTracksOldLayer4';
   constGilModTracksOldWidthMm4    = 'ModTracksOldWidthMm4';
   constGilModTracksNewLayer4      = 'ModTracksNewLayer4';
   constGilModTracksNewWidthMm4    = 'ModTracksNewWidthMm4';
{}
   { Keys related to the specify STEP file command file command. }
   constGilStepFileRelPath         = 'stepFileRelPath';
   constGilStepFilePath            = 'stepFilePath';
   constGilStepXRot                = 'stepXRot';
   constGilStepYRot                = 'stepYRot';
   constGilStepZRot                = 'stepZRot';
   constGilStepZOff                = 'stepXOff';
{}
   { Name of FreeCad python script. }
   constFreeCadPythonScriptName    = 'FC3DM_IC.py';
{}
   { Keys related to FreeCAD FC3DM autogenerated models. }
   constFC3DM_pivotPointHeightRatio = 'FC3DM_pivotPointHeightRatio';
   constFC3DM_P1chamferOffset      = 'FC3DM_P1chamferOffset';
   constFC3DM_highestRevNumber     = 'FC3DM_highestRevNumber';
   constFC3DM_fcstdFilePath        = 'FC3DM_fcstdFilePath';
   constFC3DM_iniFilePath          = 'FC3DM_iniFilePath';
   constFC3DM_stepFilePath         = 'FC3DM_stepFilePath';
   constFC3DM_logFilePath          = 'FC3DM_logFilePath';
   constFC3DM_epPin1ChamferRadius  = 'FC3DM_epPin1ChamferRadius';
   constFC3DM_gullwingMoldAngle    = 12;
   constFC3DM_qfnMoldAngle         = 0;   
{}
   { Keys related to the specify component height command file command. }
   constGilSpecCompHeight          = 'specCompHeight';
{}
   { Keys related to the specify component color code command file command. }
   constGilSpecCompColorCode       = 'specCompColorCode';
{}
   { Keys related to the split pin command file command. }
   constGilSplitPinPinName         = 'splitPinPinName';
   constGilSplitPinPinName1        = 'splitPinPinName1';
   constGilSplitPinX1center        = 'splitPinX1center';
   constGilSplitPinY1center        = 'splitPinY1center';
   constGilSplitPinX1size          = 'splitPinX1size';
   constGilSplitPinY1size          = 'splitPinY1size';
   constGilSplitPinPinName2        = 'splitPinPinName2';
   constGilSplitPinX2center        = 'splitPinX2center';
   constGilSplitPinY2center        = 'splitPinY2center';
   constGilSplitPinX2size          = 'splitPinX2size';
   constGilSplitPinY2size          = 'splitPinY2size';
{}
   { Keys related to the derive footprint by marking command file command. }
   constGilDeriveByMethod          = 'deriveByMethod';
   constGilDeriveByMarking         = 'deriveByMarking';
   constGilDeriveMfgName           = 'deriveMfgName';
   constGilDeriveMfgPn             = 'deriveMfgPn';
{}
   { Keys related to the derive footprint by marking command file command. }
   constGilDeriveByMarkingText     = 'deriveByMarkingText';
   constGilDeriveMarkingText       = 'deriveMarkingText';
{}
   { Keys related to the derive footprint by height command file command. }
   constGilDeriveByHeight          = 'deriveByHeight';
   constGilDeriveHeight            = 'deriveHeight';
{}
   { Keys related to the specify STEP file for derived footprint command. }
   constGilDerivedStepFileRelPath  = 'derivedStepFileRelPath';
   constGilDerivedStepFilePath     = 'derivedStepFilePath';
{}
   { Keys related to the specify 3D text command file command. }
   constGil3dTextText              = '3dTextText';
   constGil3dTextRotation          = '3dTextRotation';
   constGil3dTextX1                = '3dTextX1';
   constGil3dTextY1                = '3dTextY1';
   constGil3dTextX2                = '3dTextX2';
   constGil3dTextY2                = '3dTextY2';
   constGil3dTextFontName          = '3dTextFontName';
   constGil3dTextBold              = '3dTextBold';
   constGil3dTextItalic            = '3dTextItalic';
   constGil3dTextCenterInX         = '3dTextCenterInX';
   constGil3dTextCenterInY         = '3dTextCenterInY';
   constGil3dTextColorText         = '3dTextColorText';
   constGil3dTextColorBackground   = '3dTextColorBackground';
{}
   { Keys related to the specify 3D image command file command. }
   constGil3dImageImageFileRelPath = '3dImageImageFileRelPath';
   constGil3dImageImageFilePath    = '3dImageImageFilePath';
   constGil3dImageRotation         = '3dImageRotation';
   constGil3dImageX1               = '3dImageX1';
   constGil3dImageY1               = '3dImageY1';
   constGil3dImageX2               = '3dImageX2';
   constGil3dImageY2               = '3dImageY2';
   constGil3dImageNegative         = '3dImageNegative';
   constGil3dImageCenterInX        = '3dImageCenterInX';
   constGil3dImageCenterInY        = '3dImageCenterInY';
   constGil3dImageColorImage       = '3dImageColorImage';
   constGil3dImageColorBackground  = '3dImageColorBackground';
{}
   { Keys related to the specify Altium PcbLib library source command. }
   constGilSpecAltPcbLibFileRelPath= 'SpecAltiumPcbLibFileRelPath';
   constGilSpecAltPcbLibFilePath   = 'SpecAltiumPcbLibFilePath';
{}
   { Keys related to the specify Altium footprint source command. }
   constGilSpecAltFootprintName    = 'SpecAltiumFootprintName';
{}
   { Keys related to package dimensions read from .plb09 file. }
   constGilPrefixLpWizardInfo      = 'LPW_';    { Prefix to pre-pend to all data extracted from LP Wizard .plb09 file. }
   constGilPrefixLpWizardPkgDims   = 'LPWPD_';  { Prefix to pre-pend to package dimensions extracted from LP Wizard .plb09 file. }
   constGilPrefixLpWizardBuildInfo = 'LPWBI_';  { Prefix to pre-pend to build info extracted from LP Wizard .plb09 file. }
   constGilPkgDimsPins             = constGilPrefixLpWizardPkgDims + 'Pins';
   constGilPkgDimsPitch            = constGilPrefixLpWizardPkgDims + 'Pitch';
   constGilPkgDimsStandoffMin      = constGilPrefixLpWizardPkgDims + 'StandoffMin';
   constGilPkgDimsHeightMax        = constGilPrefixLpWizardPkgDims + 'HeightMax';
   constGilPkgDimsEpWidthMax       = constGilPrefixLpWizardPkgDims + 'EpWidthMax';
   constGilPkgDimsEpLengthMax      = constGilPrefixLpWizardPkgDims + 'EpLengthMax';
   constGilPkgDimsEpChamfer        = constGilPrefixLpWizardPkgDims + 'EpChamfer';
   constGilPkgDimsEpCornerRad      = constGilPrefixLpWizardPkgDims + 'EpCornerRad';
   constGilPkgDimsTotalWidthMin    = constGilPrefixLpWizardPkgDims + 'TotalWidthMin';
   constGilPkgDimsTotalWidthMax    = constGilPrefixLpWizardPkgDims + 'TotalWidthMax';
   constGilPkgDimsTotalLengthMin   = constGilPrefixLpWizardPkgDims + 'TotalLengthMin';
   constGilPkgDimsTotalLengthMax   = constGilPrefixLpWizardPkgDims + 'TotalLengthMax';
   constGilPkgDimsBodyWidthMin     = constGilPrefixLpWizardPkgDims + 'BodyWidthMin';
   constGilPkgDimsBodyWidthMax     = constGilPrefixLpWizardPkgDims + 'BodyWidthMax';
   constGilPkgDimsBodyLengthMin    = constGilPrefixLpWizardPkgDims + 'BodyLengthMin';
   constGilPkgDimsBodyLengthMax    = constGilPrefixLpWizardPkgDims + 'BodyLengthMax';
   constGilPkgDimsPinWidthMin      = constGilPrefixLpWizardPkgDims + 'PinWidthMin';
   constGilPkgDimsPinWidthMax      = constGilPrefixLpWizardPkgDims + 'PinWidthMax';
   constGilPkgDimsPinLandMin       = constGilPrefixLpWizardPkgDims + 'PinLandMin';
   constGilPkgDimsPinLandMax       = constGilPrefixLpWizardPkgDims + 'PinLandMax';
   constGilPkgDimsBallDiamNom      = constGilPrefixLpWizardPkgDims + 'BallDiamNom';
   constGilPkgDimsHasDshapePads    = constGilPrefixLpWizardPkgDims + 'HasDshapedPads';

{} 
   { Constants related to settings we should setup in the new PcbLib file. }
   constGridSpacingMm              = 0.05;  { Grid spacing to setup for new PcbLib file. }
{}
   { Constants related to LP Wizard library file. }
   constLpWizardMaxLinesInFile     = 3;             { The maximum number of lines there should be in the file if there is only 1 footprint contained therein. }
   constLpWizardFieldSepChar       = #9;            { Tab character. }
   constLpWizardFieldNamesLine     = 1;             { Line in the .plb09 file holding field names (counts starting from 0). }
   constLpWizardFieldValuesLine    = 2;             { Line in the .plb09 file holding field values (counts starting from 0). }
   constLpWizardDimensionsField    = 'Component';   { Title of the LP Wizard package dimensions field. }
   constLpWizardBuildInfoField     = 'Build Info';  { Title of the LP Wizard build info field. }
   constLpWizardDescriptionField   = 'Description'; { Title of the LP Wizard description field. }
   constLpWizardManufacturerField  = 'Manufacturer';{ Title of the LP Wizard manufacturer field. }
   constLpWizardMfgPkgCodeField    = 'Mfr Package Code';    { Title of the LP Wizard manufacturer package code field. }
   constLpWizardDimsFieldSepChar   = ';';           { How to split apart the package dimensions field. }
   constLpWizardDescFieldSepChar   = ',';           { How to split apart the description field. }
{}
   { Constants related to the LP Wizard library file, Build Info field . }
   constLpWizardBuildInfoSectionStdPrefs   = 'Stg_Pf';  { Section name for standard preferences. }
   constLpWizardBuildInfoSectionGlobPrefs  = 'Glbl_Pf'; { Section name for global preferences. }
   constLpWizardBuildInfoSectionRulePrefs  = 'Rl_Pf';   { Section name for rule preferences. }
   constLpWizardBuildInfoSectionDraftPrefs = 'Drft_Pf'; { Section name for drafting preferences. }
   constLpWizardBuildInfoSectionPadShape   = 'dflt';    { Section name for determining pad shape (rectangular or D-shaped). }
   constLpWizardBuildInfoSectionGenInfo    = 'Land';    { Section name for general info. }
   constLpWizardBuildInfoSilkToPadField    = constGilPrefixLpWizardBuildInfo + constLpWizardBuildInfoSectionRulePrefs  + '_' + 'silk_mn';   { Silkscreen to pad parameter. }
   constLpWizardBuildInfoSilkOutlineField  = constGilPrefixLpWizardBuildInfo + constLpWizardBuildInfoSectionDraftPrefs + '_' + 'silk_map';  { Silkscreen outline parameter. }
   constLpWizardBuildInfoAssyOutlineField  = constGilPrefixLpWizardBuildInfo + constLpWizardBuildInfoSectionDraftPrefs + '_' + 'asy_map';   { Assembly outline parameter. }
   constLpWizardBuildInfoBodyLineWidthField  = constGilPrefixLpWizardBuildInfo + constLpWizardBuildInfoSectionDraftPrefs + '_' + 'bdy_ln_w';    { 3D body line width. }
   constLpWizardBuildInfoAssyLineWidthField  = constGilPrefixLpWizardBuildInfo + constLpWizardBuildInfoSectionDraftPrefs + '_' + 'asy_ln_w';    { Assembly line width. }
   constLpWizardBuildInfoSilkLineWidthField  = constGilPrefixLpWizardBuildInfo + constLpWizardBuildInfoSectionDraftPrefs + '_' + 'silk_ln_w';   { Silkscreen line width. }
   constLpWizardBuildInfoEpSolderpastePct    = constGilPrefixLpWizardBuildInfo + constLpWizardBuildInfoSectionDraftPrefs + '_' + 'pm_tab_os_pct';{ EP solderpaste percentage. }
   constLpWizardBuildInfoXYRounding          = constGilPrefixLpWizardBuildInfo + constLpWizardBuildInfoSectionStdPrefs + '_' + 'xy_rnd';    { EP rounding factor. }
   constLpWizardBuildInfoPadShape          = constGilPrefixLpWizardBuildInfo + constLpWizardBuildInfoSectionPadShape + '_' + 'shp'; { Pad Shape. 1 = rectangular, 2 = D-shaped }
   constLpWizardBuildInfoPadToPadField     = constGilPrefixLpWizardBuildInfo + constLpWizardBuildInfoSectionRulePrefs  + '_' + 'g_mn';   { Pad to pad parameter. }
{}
   { Requirements for various LP Wizard library file, Build Info values. }
   constLpWizardBuildInfoSilkToPadValue    = '0.1';     { Required silkscreen-to-pad parameter, in mm, expressed as a unitless string. }
   constLpWizardBuildInfoSilkOutlineValue  = '0';       { Required silkscreen outline mode, as a string.  0 = max body outline, 1 = nom body outline. }
   constLpWizardBuildInfoAssyOutlineValue  = '0';       { Required assembly outline mode, as a string.  0 = max body outline, 1 = nom body outline. }
{}
   {* Setup constants for some common file extensions. *}
   constExtPadsAscii               = '.asc';       { PADS ASCII layout file. }
   constExtLpWizardLibrary         = '.plb09';     { Mentor LP Wizard library file. }
   constExtStepModel1              = '.stp';       { STEP model file. }
   constExtStepModel2              = '.step';      { STEP model file. }
{}
   { Constants for some directory structures. }
   constLpWizardFilesDir           = '..\..\Mentor_LP-Wizard\';
   constSpi3dModelsMeDir           = '..\..\..\Mechanical\3D-models\TRT_Created\SolidWorks\';
   constSpi3dModelsFcDir           = '..\..\..\Mechanical\3D-models\TRT_Created\';
   constSpi3dModelsScriptSubDir    = 'FreeCAD_macros\';
   constSpi3dModelsGlobalIniFile   = 'FC3DM_global.ini';
{}
   { Constants describing some Altium behaviors. }
   constAltiumHistoryDir           = 'History';     { Name of annoying History/ dir that is part of all Altium projects. }
{}
   { Constants related to .csv report file that we will generate describing all footprint features. }
   constCsvRptPrefixLayerUsed      = '00_LayerUsed_';
   constCsvRptPrefixFootprintProps = '01_FootprintProps_';
   constCsvRptPrefixArc            = '02_Arc_';
   constCsvRptPrefixFill           = '03_Fill_';
   constCsvRptPrefixPad            = '04_Pad_';
   constCsvRptPrefixRegion         = '05_Region_';
   constCsvRptPrefixText           = '06_Text_';
   constCsvRptPrefixTrack          = '07_Track_';
   constCsvRptPrefixBody           = '08_Body_';
   constCsvRptPrefix3dText         = '09_3dText_';
   constCsvRptPrefix3dRegions      = '10_3dRegions_';
{}
   { Constants used in constructing and manipulating csv string list. }
   constCsvRptFieldSepChar         = '/';
{}
   { General properties }
   constCsvRptFieldObjectKind                         = '000_ObjectKind';
   constCsvRptFieldIdentifier                         = '001_Identifier';
   constCsvRptFieldLayer                              = '002_Layer';
   constCsvRptFieldWidth                              = '003_Width';
   constCsvRptFieldRotation                           = '004_Rotation';
   constCsvRptFieldX1                                 = '005_X1';
   constCsvRptFieldY1                                 = '006_Y1';
   constCsvRptFieldX2                                 = '007_X2';
   constCsvRptFieldY2                                 = '008_Y2';
   constCsvRptFieldKeepout                            = '009_Keepout';
   constCsvRptFieldLocked                             = '010_Locked';
   constCsvRptFieldHide                               = '011_Hide';
   constCsvRptFieldNet                                = '012_Net';
   constCsvRptFieldComponent                          = '013_Component';
{}
   { Arc related properties }
   constCsvRptFieldRadius                             = '020_Radius';
   constCsvRptFieldStartAngle                         = '021_StartAngle';
   constCsvRptFieldEndAngle                           = '022_StopAngle';
{}
   { 3D body-specific parameters }
   constCsvRptFieldModelType                          = '030_ModelType';
   constCsvRptFieldBodyProjection                     = '031_BodyProjection';
   constCsvRptFieldBodyColor                          = '032_BodyColor';
   constCsvRptFieldBodyOpacity                        = '033_BodyOpacity';
   constCsvRptFieldModelRotationX                     = '034_ModelRotationX';
   constCsvRptFieldModelRotationY                     = '035_ModelRotationY';
   constCsvRptFieldModelRotationZ                     = '036_ModelRotationZ';
   constCsvRptFieldModelStandoffHeight                = '037_ModelStandoffHeight';
   constCsvRptFieldModelOverallHeight                 = '038_ModelOverallHeight';
   constCsvRptFieldModelRadius                        = '039_ModelRadius';
{}
   { Text related properties }
   constCsvRptFieldString                             = '050_String';
   constCsvRptFieldTextHeight                         = '051_TextHeight';
   constCsvRptFieldTextWidth                          = '052_TextWidth';
   constCsvRptFieldStrokeFont                         = '053_StrokeFont';
   constCsvRptFieldAutoposition                       = '054_Autoposition';
   constCsvRptFieldMirror                             = '055_Mirror';
{}
   { Text related properties -- True Type fonts }
   constCsvRptFieldTextKind                           = '060_TextKind';
   constCsvRptFieldStringType                         = '061_StringType';
   constCsvRptFieldTrueTypeFontName                   = '062_TrueTypeFontName';
   constCsvRptFieldBold                               = '063_Bold';
   constCsvRptFieldItalic                             = '064_Italic';
   constCsvRptFieldInverted                           = '065_Inverted';
   constCsvRptFieldInvertedBorderWidth                = '066_InvertedBorderWidth';
   constCsvRptFieldInvertedRectangleWidth             = '067_InvertedRectangleWidth';
   constCsvRptFieldInvertedRectangleHeight            = '068_InvertedRectangleHeight';
   constCsvRptFieldUseInvertedRectangle               = '069_UseInvertedRectangle';
   constCsvRptFieldInvertedTextJustification          = '070_InvertedTextJustification';
   constCsvRptFieldInvertedTextOffsetFromInvertedRect = '071_InvertedTextOffsetFromInvertedRect';
{}
   { Text related properties -- Barcodes }
   constCsvRptFieldBarCodeFullWidth                   = '080_BarCodeFullWidth';
   constCsvRptFieldBarCodeFullHeight                  = '081_BarCodeFullHeight';
   constCsvRptFieldBarCodeFullXMargin                 = '082_BarCodeFullXMargin';
   constCsvRptFieldBarCodeFullYMargin                 = '083_BarCodeFullYMargin';
   constCsvRptFieldBarCodeMinWidth                    = '084_BarCodeMinWidth';
   constCsvRptFieldBarCodeType                        = '085_BarCodeType';
   constCsvRptFieldBarCodeRenderMode                  = '086_BarCodeRenderMode';
   constCsvRptFieldBarCodeInverted                    = '087_BarCodeInverted';
   constCsvRptFieldBarCodeTextFontName                = '088_BarCodeTextFontName';
   constCsvRptFieldBarCodeShowText                    = '089_BarCodeShowText';
{}
{}
   { Pad related properties }
   constCsvRptFieldDrillType                          = '090_DrillType';
   constCsvRptFieldHoleType                           = '090_HoleType';
   constCsvRptFieldHoleSize                           = '090_HoleSize';
   constCsvRptFieldHoleWidth                          = '090_HoleWidth';
   constCsvRptFieldHoleRotation                       = '090_HoleRotation';
{}
   constCsvRptFieldSolderpasteOverride                = '100_SolderpasteOverride';
   constCsvRptFieldSolderpasteExpansion               = '101_SolderpasteExpansion';
   constCsvRptFieldSolderpasteExpansionMode           = '102_SolderpasteExpansionMode';
   constCsvRptFieldSoldermaskOverride                 = '103_SoldermaskOverride';
   constCsvRptFieldSoldermaskExpansion                = '104_SoldermaskExpansion';
   constCsvRptFieldSoldermaskExpansionMode            = '105_SoldermaskExpansionMode';
   constCsvRptFieldSoldermaskTentingTop               = '106_SoldermaskTentingTop';
   constCsvRptFieldSoldermaskTentingBot               = '107_SoldermaskTentingBot';
{}
   constCsvRptFieldFabTestPointTop                    = '110_FabTestPointTop';
   constCsvRptFieldFabTestPointBot                    = '111_FabTestPointBot';
   constCsvRptFieldAssyTestPointTop                   = '112_AssyTestPointTop';
   constCsvRptFieldAssyTestPointBot                   = '113_AssyTestPointBot';
{}
   constCsvRptFieldElectricalType                     = '114_ElectricalType';
   constCsvRptFieldPlated                             = '115_Plated';
   constCsvRptFieldPadJumperID                        = '116_PadJumperID';
{}
   constCsvRptFieldStackMode                          = '119_StackMode';
{}
   constCsvRptFieldPadShapeAll                        = '130_PadShapeAll';
   constCsvRptFieldPadXSizeAll                        = '131_PadXSizeAll';
   constCsvRptFieldPadYSizeAll                        = '132_PadYSizeAll';
{}
   constCsvRptFieldPadShapeTop                        = '140_PadShapeTop';
   constCsvRptFieldPadXSizeTop                        = '141_PadXSizeTop';
   constCsvRptFieldPadYSizeTop                        = '142_PadYSizeTop';
   constCsvRptFieldPadXOffsetTop                      = '143_PadXOffsetTop';
   constCsvRptFieldPadYOffsetTop                      = '144_PadYOffsetTop';
   constCsvRptFieldPadCornerRadTop                    = '145_PadCornerRadTop';
{}
   constCsvRptFieldPadShapeMid                        = '145_PadShapeMid';
   constCsvRptFieldPadXSizeMid                        = '146_PadXSizeMid';
   constCsvRptFieldPadYSizeMid                        = '147_PadYSizeMid';
{}
   constCsvRptFieldPadShapeMid1                       = '150_PadShapeMid1';
   constCsvRptFieldPadXSizeMid1                       = '151_PadXSizeMid1';
   constCsvRptFieldPadYSizeMid1                       = '152_PadYSizeMid1';
   constCsvRptFieldPadXOffsetMid1                     = '153_PadXOffsetMid1';
   constCsvRptFieldPadYOffsetMid1                     = '154_PadYOffsetMid1';
   constCsvRptFieldPadCornerRadMid1                   = '155_PadCornerRadMid1';
{}
   constCsvRptFieldPadShapeMid2                       = '160_PadShapeMid2';
   constCsvRptFieldPadXSizeMid2                       = '161_PadXSizeMid2';
   constCsvRptFieldPadYSizeMid2                       = '162_PadYSizeMid2';
   constCsvRptFieldPadXOffsetMid2                     = '163_PadXOffsetMid2';
   constCsvRptFieldPadYOffsetMid2                     = '164_PadYOffsetMid2';
   constCsvRptFieldPadCornerRadMid2                   = '165_PadCornerRadMid2';
{}
   constCsvRptFieldPadShapeMid3                       = '170_PadShapeMid3';
   constCsvRptFieldPadXSizeMid3                       = '171_PadXSizeMid3';
   constCsvRptFieldPadYSizeMid3                       = '172_PadYSizeMid3';
   constCsvRptFieldPadXOffsetMid3                     = '173_PadXOffsetMid3';
   constCsvRptFieldPadYOffsetMid3                     = '174_PadYOffsetMid3';
   constCsvRptFieldPadCornerRadMid3                   = '175_PadCornerRadMid3';
{}
   constCsvRptFieldPadShapeMid4                       = '180_PadShapeMid4';
   constCsvRptFieldPadXSizeMid4                       = '181_PadXSizeMid4';
   constCsvRptFieldPadYSizeMid4                       = '182_PadYSizeMid4';
   constCsvRptFieldPadXOffsetMid4                     = '183_PadXOffsetMid4';
   constCsvRptFieldPadYOffsetMid4                     = '184_PadYOffsetMid4';
   constCsvRptFieldPadCornerRadMid4                   = '185_PadCornerRadMid4';
{}
   constCsvRptFieldPadShapeMid5                       = '190_PadShapeMid5';
   constCsvRptFieldPadXSizeMid5                       = '191_PadXSizeMid5';
   constCsvRptFieldPadYSizeMid5                       = '192_PadYSizeMid5';
   constCsvRptFieldPadXOffsetMid5                     = '193_PadXOffsetMid5';
   constCsvRptFieldPadYOffsetMid5                     = '194_PadYOffsetMid5';
   constCsvRptFieldPadCornerRadMid5                   = '195_PadCornerRadMid5';
{}
   constCsvRptFieldPadShapeMid6                       = '200_PadShapeMid6';
   constCsvRptFieldPadXSizeMid6                       = '201_PadXSizeMid6';
   constCsvRptFieldPadYSizeMid6                       = '202_PadYSizeMid6';
   constCsvRptFieldPadXOffsetMid6                     = '203_PadXOffsetMid6';
   constCsvRptFieldPadYOffsetMid6                     = '204_PadYOffsetMid6';
   constCsvRptFieldPadCornerRadMid6                   = '205_PadCornerRadMid6';
{}
   constCsvRptFieldPadShapeMid7                       = '210_PadShapeMid7';
   constCsvRptFieldPadXSizeMid7                       = '211_PadXSizeMid7';
   constCsvRptFieldPadYSizeMid7                       = '212_PadYSizeMid7';
   constCsvRptFieldPadXOffsetMid7                     = '213_PadXOffsetMid7';
   constCsvRptFieldPadYOffsetMid7                     = '214_PadYOffsetMid7';
   constCsvRptFieldPadCornerRadMid7                   = '215_PadCornerRadMid7';
{}
   constCsvRptFieldPadShapeMid8                       = '220_PadShapeMid8';
   constCsvRptFieldPadXSizeMid8                       = '221_PadXSizeMid8';
   constCsvRptFieldPadYSizeMid8                       = '222_PadYSizeMid8';
   constCsvRptFieldPadXOffsetMid8                     = '223_PadXOffsetMid8';
   constCsvRptFieldPadYOffsetMid8                     = '224_PadYOffsetMid8';
   constCsvRptFieldPadCornerRadMid8                   = '225_PadCornerRadMid8';
{}
   constCsvRptFieldPadShapeMid9                       = '230_PadShapeMid9';
   constCsvRptFieldPadXSizeMid9                       = '231_PadXSizeMid9';
   constCsvRptFieldPadYSizeMid9                       = '232_PadYSizeMid9';
   constCsvRptFieldPadXOffsetMid9                     = '233_PadXOffsetMid9';
   constCsvRptFieldPadYOffsetMid9                     = '234_PadYOffsetMid9';
   constCsvRptFieldPadCornerRadMid9                   = '235_PadCornerRadMid9';
{}
   constCsvRptFieldPadShapeMid10                      = '240_PadShapeMid10';
   constCsvRptFieldPadXSizeMid10                      = '241_PadXSizeMid10';
   constCsvRptFieldPadYSizeMid10                      = '242_PadYSizeMid10';
   constCsvRptFieldPadXOffsetMid10                    = '243_PadXOffsetMid10';
   constCsvRptFieldPadYOffsetMid10                    = '244_PadYOffsetMid10';
   constCsvRptFieldPadCornerRadMid10                  = '245_PadCornerRadMid10';
{}
   constCsvRptFieldPadShapeMid11                      = '250_PadShapeMid11';
   constCsvRptFieldPadXSizeMid11                      = '251_PadXSizeMid11';
   constCsvRptFieldPadYSizeMid11                      = '252_PadYSizeMid11';
   constCsvRptFieldPadXOffsetMid11                    = '253_PadXOffsetMid11';
   constCsvRptFieldPadYOffsetMid11                    = '254_PadYOffsetMid11';
   constCsvRptFieldPadCornerRadMid11                  = '255_PadCornerRadMid11';
{}
   constCsvRptFieldPadShapeMid12                      = '260_PadShapeMid12';
   constCsvRptFieldPadXSizeMid12                      = '261_PadXSizeMid12';
   constCsvRptFieldPadYSizeMid12                      = '262_PadYSizeMid12';
   constCsvRptFieldPadXOffsetMid12                    = '263_PadXOffsetMid12';
   constCsvRptFieldPadYOffsetMid12                    = '264_PadYOffsetMid12';
   constCsvRptFieldPadCornerRadMid12                  = '265_PadCornerRadMid12';
{}
   constCsvRptFieldPadShapeMid13                      = '270_PadShapeMid13';
   constCsvRptFieldPadXSizeMid13                      = '271_PadXSizeMid13';
   constCsvRptFieldPadYSizeMid13                      = '272_PadYSizeMid13';
   constCsvRptFieldPadXOffsetMid13                    = '273_PadXOffsetMid13';
   constCsvRptFieldPadYOffsetMid13                    = '274_PadYOffsetMid13';
   constCsvRptFieldPadCornerRadMid13                  = '275_PadCornerRadMid13';
{}
   constCsvRptFieldPadShapeMid14                      = '280_PadShapeMid14';
   constCsvRptFieldPadXSizeMid14                      = '281_PadXSizeMid14';
   constCsvRptFieldPadYSizeMid14                      = '282_PadYSizeMid14';
   constCsvRptFieldPadXOffsetMid14                    = '283_PadXOffsetMid14';
   constCsvRptFieldPadYOffsetMid14                    = '284_PadYOffsetMid14';
   constCsvRptFieldPadCornerRadMid14                  = '285_PadCornerRadMid14';
{}
   constCsvRptFieldPadShapeMid15                      = '290_PadShapeMid15';
   constCsvRptFieldPadXSizeMid15                      = '291_PadXSizeMid15';
   constCsvRptFieldPadYSizeMid15                      = '292_PadYSizeMid15';
   constCsvRptFieldPadXOffsetMid15                    = '293_PadXOffsetMid15';
   constCsvRptFieldPadYOffsetMid15                    = '294_PadYOffsetMid15';
   constCsvRptFieldPadCornerRadMid15                  = '295_PadCornerRadMid15';
{}
   constCsvRptFieldPadShapeMid16                      = '300_PadShapeMid16';
   constCsvRptFieldPadXSizeMid16                      = '301_PadXSizeMid16';
   constCsvRptFieldPadYSizeMid16                      = '302_PadYSizeMid16';
   constCsvRptFieldPadXOffsetMid16                    = '303_PadXOffsetMid16';
   constCsvRptFieldPadYOffsetMid16                    = '304_PadYOffsetMid16';
   constCsvRptFieldPadCornerRadMid16                  = '305_PadCornerRadMid16';
{}
   constCsvRptFieldPadShapeMid17                      = '310_PadShapeMid17';
   constCsvRptFieldPadXSizeMid17                      = '311_PadXSizeMid17';
   constCsvRptFieldPadYSizeMid17                      = '312_PadYSizeMid17';
   constCsvRptFieldPadXOffsetMid17                    = '313_PadXOffsetMid17';
   constCsvRptFieldPadYOffsetMid17                    = '314_PadYOffsetMid17';
   constCsvRptFieldPadCornerRadMid17                  = '315_PadCornerRadMid17';
{}
   constCsvRptFieldPadShapeMid18                      = '320_PadShapeMid18';
   constCsvRptFieldPadXSizeMid18                      = '321_PadXSizeMid18';
   constCsvRptFieldPadYSizeMid18                      = '322_PadYSizeMid18';
   constCsvRptFieldPadXOffsetMid18                    = '323_PadXOffsetMid18';
   constCsvRptFieldPadYOffsetMid18                    = '324_PadYOffsetMid18';
   constCsvRptFieldPadCornerRadMid18                  = '325_PadCornerRadMid18';
{}
   constCsvRptFieldPadShapeMid19                      = '330_PadShapeMid19';
   constCsvRptFieldPadXSizeMid19                      = '331_PadXSizeMid19';
   constCsvRptFieldPadYSizeMid19                      = '332_PadYSizeMid19';
   constCsvRptFieldPadXOffsetMid19                    = '333_PadXOffsetMid19';
   constCsvRptFieldPadYOffsetMid19                    = '334_PadYOffsetMid19';
   constCsvRptFieldPadCornerRadMid19                  = '335_PadCornerRadMid19';
{}
   constCsvRptFieldPadShapeMid20                      = '340_PadShapeMid20';
   constCsvRptFieldPadXSizeMid20                      = '341_PadXSizeMid20';
   constCsvRptFieldPadYSizeMid20                      = '342_PadYSizeMid20';
   constCsvRptFieldPadXOffsetMid20                    = '343_PadXOffsetMid20';
   constCsvRptFieldPadYOffsetMid20                    = '344_PadYOffsetMid20';
   constCsvRptFieldPadCornerRadMid20                  = '345_PadCornerRadMid20';
{}
   constCsvRptFieldPadShapeMid21                      = '350_PadShapeMid21';
   constCsvRptFieldPadXSizeMid21                      = '351_PadXSizeMid21';
   constCsvRptFieldPadYSizeMid21                      = '352_PadYSizeMid21';
   constCsvRptFieldPadXOffsetMid21                    = '353_PadXOffsetMid21';
   constCsvRptFieldPadYOffsetMid21                    = '354_PadYOffsetMid21';
   constCsvRptFieldPadCornerRadMid21                  = '355_PadCornerRadMid21';
{}
   constCsvRptFieldPadShapeMid22                      = '360_PadShapeMid22';
   constCsvRptFieldPadXSizeMid22                      = '361_PadXSizeMid22';
   constCsvRptFieldPadYSizeMid22                      = '362_PadYSizeMid22';
   constCsvRptFieldPadXOffsetMid22                    = '363_PadXOffsetMid22';
   constCsvRptFieldPadYOffsetMid22                    = '364_PadYOffsetMid22';
   constCsvRptFieldPadCornerRadMid22                  = '365_PadCornerRadMid22';
{}
   constCsvRptFieldPadShapeMid23                      = '370_PadShapeMid23';
   constCsvRptFieldPadXSizeMid23                      = '371_PadXSizeMid23';
   constCsvRptFieldPadYSizeMid23                      = '372_PadYSizeMid23';
   constCsvRptFieldPadXOffsetMid23                    = '373_PadXOffsetMid23';
   constCsvRptFieldPadYOffsetMid23                    = '374_PadYOffsetMid23';
   constCsvRptFieldPadCornerRadMid23                  = '375_PadCornerRadMid23';
{}
   constCsvRptFieldPadShapeMid24                      = '380_PadShapeMid24';
   constCsvRptFieldPadXSizeMid24                      = '381_PadXSizeMid24';
   constCsvRptFieldPadYSizeMid24                      = '382_PadYSizeMid24';
   constCsvRptFieldPadXOffsetMid24                    = '383_PadXOffsetMid24';
   constCsvRptFieldPadYOffsetMid24                    = '384_PadYOffsetMid24';
   constCsvRptFieldPadCornerRadMid24                  = '385_PadCornerRadMid24';
{}
   constCsvRptFieldPadShapeMid25                      = '390_PadShapeMid25';
   constCsvRptFieldPadXSizeMid25                      = '391_PadXSizeMid25';
   constCsvRptFieldPadYSizeMid25                      = '392_PadYSizeMid25';
   constCsvRptFieldPadXOffsetMid25                    = '393_PadXOffsetMid25';
   constCsvRptFieldPadYOffsetMid25                    = '394_PadYOffsetMid25';
   constCsvRptFieldPadCornerRadMid25                  = '395_PadCornerRadMid25';
{}
   constCsvRptFieldPadShapeMid26                      = '400_PadShapeMid26';
   constCsvRptFieldPadXSizeMid26                      = '401_PadXSizeMid26';
   constCsvRptFieldPadYSizeMid26                      = '402_PadYSizeMid26';
   constCsvRptFieldPadXOffsetMid26                    = '403_PadXOffsetMid26';
   constCsvRptFieldPadYOffsetMid26                    = '404_PadYOffsetMid26';
   constCsvRptFieldPadCornerRadMid26                  = '405_PadCornerRadMid26';
{}
   constCsvRptFieldPadShapeMid27                      = '410_PadShapeMid27';
   constCsvRptFieldPadXSizeMid27                      = '411_PadXSizeMid27';
   constCsvRptFieldPadYSizeMid27                      = '412_PadYSizeMid27';
   constCsvRptFieldPadXOffsetMid27                    = '413_PadXOffsetMid27';
   constCsvRptFieldPadYOffsetMid27                    = '414_PadYOffsetMid27';
   constCsvRptFieldPadCornerRadMid27                  = '415_PadCornerRadMid27';
{}
   constCsvRptFieldPadShapeMid28                      = '420_PadShapeMid28';
   constCsvRptFieldPadXSizeMid28                      = '421_PadXSizeMid28';
   constCsvRptFieldPadYSizeMid28                      = '422_PadYSizeMid28';
   constCsvRptFieldPadXOffsetMid28                    = '423_PadXOffsetMid28';
   constCsvRptFieldPadYOffsetMid28                    = '424_PadYOffsetMid28';
   constCsvRptFieldPadCornerRadMid28                  = '425_PadCornerRadMid28';
{}
   constCsvRptFieldPadShapeMid29                      = '430_PadShapeMid29';
   constCsvRptFieldPadXSizeMid29                      = '431_PadXSizeMid29';
   constCsvRptFieldPadYSizeMid29                      = '432_PadYSizeMid29';
   constCsvRptFieldPadXOffsetMid29                    = '433_PadXOffsetMid29';
   constCsvRptFieldPadYOffsetMid29                    = '434_PadYOffsetMid29';
   constCsvRptFieldPadCornerRadMid29                  = '435_PadCornerRadMid29';
{}
   constCsvRptFieldPadShapeMid30                      = '440_PadShapeMid30';
   constCsvRptFieldPadXSizeMid30                      = '441_PadXSizeMid30';
   constCsvRptFieldPadYSizeMid30                      = '442_PadYSizeMid30';
   constCsvRptFieldPadXOffsetMid30                    = '443_PadXOffsetMid30';
   constCsvRptFieldPadYOffsetMid30                    = '444_PadYOffsetMid30';
   constCsvRptFieldPadCornerRadMid30                  = '445_PadCornerRadMid30';
{}
   constCsvRptFieldPadShapeBot                        = '450_PadShapeBot';
   constCsvRptFieldPadXSizeBot                        = '451_PadXSizeBot';
   constCsvRptFieldPadYSizeBot                        = '452_PadYSizeBot';
   constCsvRptFieldPadXOffsetBot                      = '453_PadXOffsetBot';
   constCsvRptFieldPadYOffsetBot                      = '454_PadYOffsetBot';
   constCsvRptFieldPadCornerRadBot                    = '455_PadCornerRadBot';
{}
{}

{ Note:  We implicitly rely on a number of constants defined in XIA_Utils.pas. 
 That script and this one must both be part of the Pcb project!
 That way, we can use constants and functions defined in the other script. }
   

{***************************************************************************
 * Global variables.  Highly evil.  Ick ick.
 ***************************************************************************}
var
   CleanupLpwFootprintForm : TCleanupLpwFootprintForm;
   step                    : Integer;

{***************************************************************************
 * BEGIN Form related functions.
 ***************************************************************************}

{***************************************************************************
 * procedure CLF_UpdateGuiStatusMessage()
 *  Update status message in dialog box and write said status to the debug file.
 ***************************************************************************}
procedure CLF_UpdateGuiStatusMessage(msg : TString);
begin

   { Change text in GUI status line. }
   formStatusBar1.SimpleText := msg;

   { Force a screen refresh of GUI status line. }
   formStatusBar1.Update;
   CleanupLpwFootprintForm.Update;

   { Copy this message to debug file. }
   WriteToDebugFile('');
   WriteToDebugFile('* ' + msg);

end; { end CLF_UpdateGuiStatusMessage() }


{***************************************************************************
 * procedure CLF_WriteToSummaryAndDebugFiles()
 *  Write a new line of text to the summary file.
 *  Also write said line of text to debug file with '**' pre-pended.
 ***************************************************************************}
procedure CLF_WriteToSummaryAndDebugFiles(msg : TString);
begin

   { Write the message to both the summary file and the debug file. }
   WriteToSummaryFile(msg);
   WriteToDebugFile('**' + msg);
   
end; { end CLF_WriteToSummaryAndDebugFiles() }


{***************************************************************************
 * procedure CLF_WriteToSummaryAndDebugFilesWithStepNum()
 *  Write a new line of text to the summary file and debug file with
 *  a running step number pre-pended.
 ***************************************************************************}
procedure CLF_WriteToSummaryAndDebugFilesWithStepNum(msg : TString);
begin

   { Call CLF_WriteToSummaryAndDebugFiles() to do all the real work. }
   CLF_WriteToSummaryAndDebugFiles(IntToStr(step) + '. ' + msg);

   { Increment global step number. }
   step := step + 1;
end; { end CLF_WriteToSummaryAndDebugFilesWithStepNum() }


{***************************************************************************
 * procedure CLF_AtExit()
 *  Put results in our dialog box list and return to AtExit() for the rest of the cleanup routines.
 ***************************************************************************}
procedure CLF_AtExit(rc : Integer);
var
   i        : Integer;

begin 

   {* Transform existing GUI dialog box so that there is a big list box available. *}

   { Nuke most text fields to make room for the big list box. }
   CleanupLpwFootprintForm.formText03.Free;
   CleanupLpwFootprintForm.formText04.Free;
   CleanupLpwFootprintForm.formText05.Free;
   CleanupLpwFootprintForm.formText06.Free;
   CleanupLpwFootprintForm.formText07.Free;
   CleanupLpwFootprintForm.formText08.Free;
   CleanupLpwFootprintForm.formText09.Free;
   CleanupLpwFootprintForm.formText10.Free;
   CleanupLpwFootprintForm.formText11.Free;
   CleanupLpwFootprintForm.formText12.Free;
   CleanupLpwFootprintForm.formText13.Free;
   CleanupLpwFootprintForm.formText14.Free;
   CleanupLpwFootprintForm.formText15.Free;
   CleanupLpwFootprintForm.formText16.Free;
   CleanupLpwFootprintForm.formText17.Free;
   CleanupLpwFootprintForm.formText18.Free;
   CleanupLpwFootprintForm.formText19.Free;
   CleanupLpwFootprintForm.formText20.Free;
   CleanupLpwFootprintForm.formText21.Free;
   CleanupLpwFootprintForm.formText22.Free;
   CleanupLpwFootprintForm.formText23.Free;
   CleanupLpwFootprintForm.formText24.Free;
   CleanupLpwFootprintForm.formText25.Free;
   CleanupLpwFootprintForm.formText26.Free;
   CleanupLpwFootprintForm.formText27.Free;
   CleanupLpwFootprintForm.formText28.Free;
   CleanupLpwFootprintForm.formText29.Free;
   CleanupLpwFootprintForm.formText30.Free;

   { Transform existing GUI dialog box so that there is a big list box available. }
   CleanupLpwFootprintForm.listBox1.Left := 14;
   CleanupLpwFootprintForm.listBox1.Top := 40;
   CleanupLpwFootprintForm.listBox1.Width := 972;
   CleanupLpwFootprintForm.listBox1.Height := 640;

   { Move Ok button to center. }
   CleanupLpwFootprintForm.formButtonOk.Left := 450;
   CleanupLpwFootprintForm.formButtonOk.Enabled := True;
   CleanupLpwFootprintForm.formButtonOk.Update;

   { Nuke Cancel button. }
   CleanupLpwFootprintForm.formButtonCancel.Free;

   { Run GUI dialog box to display the summary messages. }
   //        ShowMessage('About to display modal dialog');
   CleanupLpwFootprintForm.formText01.Caption := SummaryMessages.Strings[0];

   CleanupLpwFootprintForm.listBox1.Clear;


   {* Proceed to output summary messages in the list box. *}
   
   { Loop over all the summary messages. }
   for i := 1 to SummaryMessages.Count - 1 do
   begin

      { Add this line of message to the list box on screen. }
      CleanupLpwFootprintForm.listBox1.Items.Insert((i-1), SummaryMessages.Strings[i]);

      { Update the size of the horizontal scroll bars if needed. }
      { Code stolen from http://www.delphipages.com/forum/showthread.php?t=203460 }
//    if (CleanupLpwFootprintForm.listBox1.Canvas.TextWidth(SummaryMessages.Strings[i] + 'W') > CleanupLpwFootprintForm.listBox1.ScrollWidth) then
//    begin
//       CleanupLpwFootprintForm.listBox1.ScrollWidth := CleanupLpwFootprintForm.listBox1.Canvas.TextWidth(SummaryMessages.Strings[i] + 'W');
//    end;

      { For some reason, that's not enough.  Double it instead of adding the width of a 'W' char. }
      if (CleanupLpwFootprintForm.listBox1.Canvas.TextWidth(SummaryMessages.Strings[i] + SummaryMessages.Strings[i]) > CleanupLpwFootprintForm.listBox1.ScrollWidth) then
      begin
         CleanupLpwFootprintForm.listBox1.ScrollWidth := CleanupLpwFootprintForm.listBox1.Canvas.TextWidth(SummaryMessages.Strings[i] + SummaryMessages.Strings[i]);
      end;    

   end;

   { If we were given a sucess error code, meaning we're exiting successfully, report that. }
   if ( (rc = 0) and (enableGenerateOutputs = True) and (enableSvnCommits = True) ) then
   begin
      CleanupLpwFootprintForm.formButtonsLabel1.Caption := 'Script is exiting successfully.  Click Ok to finish.';
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
            CleanupLpwFootprintForm.formButtonsLabel1.Caption := 'ERROR ERROR ERROR Script was running with enableGenerateOutputs set to False, meaning that I generated no outputs!!!';
         end;

         { Report if enableSvnCommits was disabled. }
         if (enableSvnCommits = False) then
         begin
            CleanupLpwFootprintForm.formButtonsLabel1.Caption := 'ERROR ERROR ERROR Script was running with enableSvnCommits set to False, meaning that I did no svn commits!!!';
         end;

         { Report if we have an error code from elsewhere in this script. }
         if (rc <> 0) then
         begin
            CleanupLpwFootprintForm.formButtonsLabel1.Caption := 'Failed while running this operation: ' +                 formStatusBar1.SimpleText + constLineBreak + constLineBreak +
            'ERROR:  Script is exiting prematurely due to error!';
         end;
         
      end;

   end;

   { Append a list of system fonts available. }
// CleanupLpwFootprintForm.listBox1.Items.AddStrings(Screen.Fonts);

   
   { Display CleanupLpwFootprint dialog box that we just finished configuring. }
   //        CleanupLpwFootprintForm.ShowModal;
   
   { Since that form is already modal, we simply exit and rely on the click handler
    to do the remaining cleanup. }   


end; { end CLF_AtExit() }
   
{***************************************************************************
 * END Form related functions.
 ***************************************************************************}

{***************************************************************************
 * BEGIN Support functions.
 ***************************************************************************}

{***************************************************************************
 * procedure CLF_Abort()
 *  Call cleanup routines and then abort script.
 ***************************************************************************}
procedure CLF_Abort(msg : TDynamicString);
begin

   { Save abort message to debug file. }
   WriteToDebugFile('');
   WriteToDebugFile('**In CLF_Abort()!!!');
   WriteToDebugFile(msg);

   { Attempt to close modal dialog box. }
   CleanupLpwFootprintForm.Close;

   { Give error message to user. }
   ShowError(msg + constLineBreak + constLineBreak +
               'Aborting script!!!' + constLineBreak + constLineBreak +
               'Afterwards, hit Control-F3 (or go to Run->Stop) to shut down script execution.' + constLineBreak +
               'Then, click on a file in your PCB project to reset focus.');
   
   { Call AtExit() procedure to write debug outputs to file. }
   AtExit(1);                   { Report error at exit }

   { Now do the real abort. }
   Abort;
end; { end CLF_Abort() }


{***************************************************************************
 * function CLF_VarPlusPlus()
 *  Post increment given integer parameter incMe.
 *
 *  Returns incremented value of incMe as var parm.
 *  Returns original value of incMe (before increment) as function return value.
 ***************************************************************************}
function CLF_VarPlusPlus(var incMe : Integer;
                             )    : Integer;
   
begin

   { Return original value of incMe as function return value. }
   result := incMe;
   
   { Increment incMe }
   incMe := incMe + 1;

end; { end CLF_VarPlusPlus() }


{***************************************************************************
 * function CLF_DoesStringStartWith()
 *  See if the haystack larger string starts with the needle smaller string.
 *  
 *  I'm only implementing this because AnsiStartsStr() is not available!
 *  
 *  Returns:  True if haystack starts with needle, False otherwise.
 ***************************************************************************}
function CLF_DoesStringStartWith(haystack : TString;
                                 needle   : TString;
                                 )        : Boolean;
begin

   { See if the haystack string starts with needle. }
   result := (Copy(haystack, 1, Length(needle)) = needle);
   
end; { end CLF_DoesStringStartWith() }


{***************************************************************************
 * function CLF_DoesStringEndWith()
 *  See if the haystack larger string ends with the needle smaller string.
 *
 *  I'm only implementing this because AnsiEndsStr() is not available!
 *  
 *  Returns:  True if haystack ends with needle, False otherwise.
 ***************************************************************************}
function CLF_DoesStringEndWith(haystack : TString;
                               needle   : TString; 
                               )      : Boolean;
var
   lenNeedle   : Integer;
   lenHaystack : Integer;
   endHaystack : TString;
   
begin

   { Get lengths of needle and haystack. }
   lenNeedle := Length(needle);
   lenHaystack := Length(haystack);

   { Get the end part of haystack. }
   endHaystack := Copy(haystack, (lenHaystack - lenNeedle + 1), Length(needle));
//   WriteToDebugFile('In CLF_DoesStringEndWith(), endHaystack is "' + endHaystack + '".');
   
   { See if the haystack string ends with needle. }
   result := (endHaystack = needle);
   
end; { end CLF_DoesStringEndWith() }


{***************************************************************************
 * function CLF_StripTrailingChar()
 *  Strip any trailing specified char from a specified string.
 *
 *  Returns stripped string as function return value.
 ***************************************************************************}
function CLF_StripTrailingChar(stripMe   : TString;
                               delimiter : TString;                               
                               )         : TString;
var
   len      : Integer;
   position : Integer;
   temp     : TString;
   
begin

   { Copy input to temp. }
   temp := stripMe;

   //   ShowMessage('String is "' + temp + '".');

   { Repeat until we no longer have a delimiter char as the last char in this string. }
   repeat
   begin
      
      { Determine string length and position of last delimiter char in string. }
      len := Length(temp);
      position := LastDelimiter(delimiter, temp);
      
      { If the position of the last delimiter char is at the end of the string, then strip it off. }
      if ( (position = len) and (len > 0) ) then
      begin
         SetLength(temp, (position-1));
      end;
      
   end;
   until ( (Length(temp) = 0) or (LastDelimiter(delimiter, temp) <> (Length(temp))) );
   
   //   ShowMessage('String is now "' + temp + '".');

   //   ShowMessage('String is "' + temp + '".');
   //   ShowMessage('Strlen is ' + IntToStr(Length(temp)));
   //   ShowMessage('LastDelimiter returns ' + IntToStr(LastDelimiter(delimiter, temp)));

   { Return temp as function return value. }
   result := temp;

end; { end CLF_StripTrailingChar() }


{***************************************************************************
 * function CLF_StripLeadingChar()
 *  Strip any leading specified char from a specified string.
 *
 *  Returns stripped string as function return value.
 ***************************************************************************}
function CLF_StripLeadingChar(stripMe   : TString;
                              delimiter : TString;                               
                              )         : TString;
var
   i                    : Integer;
   len                  : Integer;
   position             : Integer;
   stillHaveLeadingChar : Boolean;
   startRealString      : Integer;
   currChar             : TString;
   
begin

//   WriteToDebugFile('Hello from CLF_StripLeadingChar(), initial string is "' + stripMe + '".');

   { Flag that we're still seeing the leading character. }
   stillHaveLeadingChar := True;
   startRealString      := 0;
   
   { Determine string length and position of last delimiter char in string. }
   len := Length(stripMe);

   { Loop over all the characters in the string. }
   for i := 1 to (len) do
   begin

      { See if we're still in the state of having the leading character. }
      if (stillHaveLeadingChar) then
      begin

         { Extract current character. }
         currChar := Copy(stripMe, i, 1);
         
         { See if we now have a character other than the specified leading character. }
         if (currChar <> delimiter) then
         begin

            { Flag that we have no more of the leading character. }
            stillHaveLeadingChar := False;
            startRealString      := i;
         end;

      end { endif }

   end; { endfor }

   { Return the string stripped of leading characters. }
   result := Copy(stripMe, startRealString, MaxInt);

//   WriteToDebugFile(' Final string is "' + result + '".');
end; { end CLF_StripLeadingChar() }


{***************************************************************************
 * function CLF_SplitDelimitedUnquotedStringIntoStringList()
 *  Split a string that does not have quote protection around whitespace
 *  into fields based solely on a delimiter, and ignoring said whitespace.
 *  
 *  Note:  Assumes that stringList has already been created.
 *
 *  Returns populated string list in var parameter stringList.
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_SplitDelimitedUnquotedStringIntoStringList(    delimitedString : TDynamicString;
                                                            delimiter       : TDynamicString;
                                                        var stringList      : TStringList;
                                                            )               : Integer;
var
   position : Integer;
   leftStr  : TString;
   rightStr : TString;
   tempStr : TString;
                                         
begin

   { Assume success. }
   result := 0;
   leftStr := '';
   rightStr := '';
   tempStr := delimitedString;

//   WriteToDebugFile('Hello from CLF_SplitDelimitedUnquotedStringIntoStringList()');

   { Find the position of the first delimiter character. }
   position := AnsiPos(delimiter, tempStr);

   { Loop until we find no more delimiters in the remaining string. }
   while (position > 0) do
   begin

      { The left string is everything up until the char before the delimiter. }
      leftStr := Copy(tempStr, 0, (position-1));
      //            WriteToDebugFile('db leftStr is "' + leftStr + '".');

      { The right string is everything after the delimiter char. }
      rightStr := Copy(tempStr, (position+1), MaxInt);
      //            WriteToDebugFile('db rightStr is "' + rightStr + '".');

      { Add left string to the stringList. }
      stringList.Add(leftStr);
//      WriteToDebugFile(' Found field "' + leftStr + '".');

      { Use the rightStr as our new tempStr going forward. }
      tempStr := rightStr;

      { Find the position of the next delimiter character. }
      position := AnsiPos(delimiter, tempStr);
      
   end; { endwhile }

   { Add temp string as the final entry to the stringList. }
   stringList.Add(tempStr);
   
end; { end CLF_SplitDelimitedUnquotedStringIntoStringList() }


{***************************************************************************
 * function CLF_ExtractTrailingNumberFromStringReturnLeadingAlpha()
 *  Examine a string that has a trailing number (eg. "FOO_SPI17").
 *  Extract both that trailing number and the (presumably) alphabetic leading chars.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ExtractTrailingNumberFromStringReturnLeadingAlpha(    getMyTrailingNumber : TDynamicString;
                                                               var leadingAlpha        : TString;
                                                               var trailingNumber      : Integer
                                                                   )                   : Integer;
var
   position  : Integer;
   currChar  : TString;
   rightStr  : TString;
   revNumStr : TString;
   len       : Integer;
   i         : Integer;
   done      : Boolean;
                                         
begin

   { Assume success. }
   result := 0;

   { Initialize rev number to -1. }
   trailingNumber := -1;
   leadingAlpha   := '';
   revNumStr      := '';

   { Get the length of the string. }
   len := Length(getMyTrailingNumber);

   { Flag that we are not yet done. }
   done := False;
   
   WriteToDebugFile('Hello from CLF_ExtractTrailingNumberFromStringReturnLeadingAlpha()');
   WriteToDebugFile(' getMyTrailingNumber is "' + getMyTrailingNumber + '".');

   { Initialize loop to begin at last character. }
   i := len;

   { Loop until we've exhausted all characters or exhausted all trailing numbers. }
   while ( (i >= 1) and (not done) ) do
   begin

      { Extract the current character. }
      currChar := Copy(getMyTrailingNumber, i, 1);
//      WriteToDebugFile(' currChar is "' + currChar + '".');

      { See if the current char is a number (0-9). }
      if ( (currChar >= '0') and (currChar <= '9') ) then
      begin

         { Prepend this number char onto the running revNumStr. }
         revNumStr := currChar + revNumStr;
//         WriteToDebugFile(' revNumStr is "' + revNumStr + '".');

         { Decrement loop counter. }
         i := i - 1;

      end { endif }

      { Else we've found our first non-number.  Flag that we're done. }
      else
      begin

         { Set done flag. }
         done := True;

         { Record the leading alpha part of the string. }
         leadingAlpha := Copy(getMyTrailingNumber, 1, i);
         WriteToDebugFile(' leadingAlpha is "' + leadingAlpha + '".');
         
      end; { endelse }
         
   end; { endwhile }

   { See if we succeeded in finding a trailing number. }
   if (revNumStr <> '') then
   begin

      { Convert string to integer and return this to caller. }
      trailingNumber := StrToInt(revNumStr);
      WriteToDebugFile(' trailingNumber is ' + IntToStr(trailingNumber) + '.');
   end

   { Else return error code meaning that we found no trailing number. }
   else
   begin
      result := 1;

   end; { endelse }
   
end; { end CLF_ExtractTrailingNumberFromStringReturnLeadingAlpha() }


{***************************************************************************
 * function CLF_ExtractTrailingNumberFromString()
 *  Examine a string that has a trailing number (eg. "FOO_SPI17").
 *  Extract just that trailing number.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ExtractTrailingNumberFromString(    getMyTrailingNumber : TDynamicString;
                                             var trailingNumber      : Integer
                                                 )                   : Integer;
var
   leadingAlpha : TString;
                                         
begin

   { Call CLF_ExtractTrailingNumberFromStringReturnLeadingAlpha() to do all the real work. }
   CLF_ExtractTrailingNumberFromStringReturnLeadingAlpha(getMyTrailingNumber,
                                                         {var} leadingAlpha,
                                                         {var} trailingNumber);

end; { end CLF_ExtractTrailingNumberFromString() }


{***************************************************************************
 * function CLF_GetSortablePadName()
 *  Convert a pin name (eg. "1", "10", "A17") into a version with enough leading
 *  zeroes that a set of such can be correctly sorted.
 *
 *  Returns:  Modified, sortable version of pad name.
 ***************************************************************************}
function CLF_GetSortablePadName(padName : TString;
                                )       : TString;
var
   rc             : Integer;
   trailingNumber : Integer;
   leadingAlpha   : TString;
                                         
begin

   { Attempt to get the trailing number from the pad name. }
   rc := CLF_ExtractTrailingNumberFromStringReturnLeadingAlpha({getMyTrailingNumber} padName,
                                                               {var} leadingAlpha,
                                                               {var} trailingNumber);
   
   { See if this succeeded (eg. there was a trailing number at all). }
   if (rc = 0) then
   begin

      { Assemble a sortable pad name from the alpha prefix of the original pad name and a number with leading 0's. }
      result := leadingAlpha + Format('%.4d', [trailingNumber]);
      
   end

   { Else return the original pad name, since there was no trailing number. }
   else
   begin

      result := padName;

   end;
   
end; { end CLF_GetSortablePadName() }


{***************************************************************************
 * function CLF_MinReal()
 *  Real (float) version of a Min() function.  Builtin one doesn't work with Reals.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_MinReal(a : Real;
                     b : Real;
                     ) : Real;
begin

   if (a < b) then
      result := a {nosemi}
   else
      result := b;
   
end; { end CLF_MinReal() }


{***************************************************************************
 * function CLF_MaxReal()
 *  Real (float) version of a Max() function.  Builtin one doesn't work with Reals.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_MaxReal(a : Real;
                     b : Real;
                     ) : Real;
begin

   if (a > b) then
      result := a {nosemi}
   else
      result := b;
   
end; { end CLF_MaxReal() }


{***************************************************************************
 * function CLF_RunGenericExternalCommand()
 *  Shell out and call specified batFile to run some command.
 *  Check its return code.  Return all its output lines.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_RunGenericExternalCommand(    scriptsPath : TDynamicString;
                                           projectPath : TDynamicString;
                                           batFile     : TDynamicString;
                                           command     : TDynamicString;
                                           parms       : TStringList;
                                       var genOut      : TStringList;
                                           )           : Integer;
var
   genRcPath    : TDynamicString;
   genOutPath   : TDynamicString;
   rc           : Integer;
   genRc        : TDynamicString;
   i            : Integer;
   cmdLine      : TDynamicString;
   projectDrive : TDynamicString;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Use (projectPath + "gen_rc.txt") as the file with which the external script will
    communicate back to us the return code from gen.exe. }
   genRcPath := (projectPath + '\' + batFile + constExtBatScriptRcFile);

   { Attempt to delete any old version of this file from disk. }
   DeleteFileWithVerify(genRcPath);
   
   { Use (projectPath + "svn_out.txt") as the file with which the external script will
    communicate back to us the output from svn.exe. }
   genOutPath := (projectPath + '\' + batFile + constExtBatScriptOutFile);

   { Attempt to delete any old version of output file from disk. }
   DeleteFileWithVerify(genOutPath);
   
   { Extract the DOS drive (ie. C:) on which the project working directory exists. }
   projectDrive := ExtractFileDrive(projectPath);
   
   { Shell out to run generic_cmd.bat. }
   { When calling generic_cmd.bat, it needs the following command line parameters:
    %1 : Path to Altium_scripts directory.
    %2 : Drive on which Altium working directory is located.
    %3 : Path to Altium working directory.
    %4 : The file to which generic_cmd.bat should write its return code.
    %5 : The file to which generic_cmd.bat should write its output.
    %6 : The command we wish to run (eg. find).
    %7 .. : Parameters to supply to said command.
    }
   { Use /k to keep window open after running.  Use /c to close after running. }
   { Add one layer of double quotes around everything after /c. }
   { Add double quote protection to pathSubDir since this is assumed to contain spaces in it. }
   cmdLine := 'cmd.exe /c "' + scriptsPath + '\' + batFile + constExtBatScript + ' ' + scriptsPath + ' ' + projectDrive + ' "' + projectPath + '" "' + genRcPath + '" "' + genOutPath + '" ' + command + ' ';

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
   
   { Now that the cmdLine is ready, actually shell out and run it. }
   WriteToDebugFile('');
   WriteToDebugFile('*About to call ' + batFile + ' as: ' + cmdLine);
   rc := RunApplication(cmdLine);

   { Wait for the external gen command to complete and get its return code. }
   AwaitSvnCompletion(genRcPath,
                      constStandardTimeoutLimit,
                      {var} genRc);

   { Check return code from external command. }
   WriteToDebugFile('*Return code from ' + command + '.exe was ' + genRc);
   if (StrToInt(genRc) <> 0) then
   begin
      MyAbort('External gen command reported error.  cmdLine was "' + cmdLine + '".  Return code was ' + genRc + '!');
   end;


   { Read output from svn command into genOut. }
   genOut.LoadFromFile(genOutPath);

   { Copy all lines from genOut to DebugFile. }
   for i := 0 to genOut.Count - 1 do
   begin

      { Copy this line to debug file. }
      WriteToDebugFile(genOut.Strings[i]);
   end; { endfor }

   { Cleanup (aka delete) out file from svn command. }
   CleanupSvnRcFile(genRcPath);
   CleanupSvnRcFile(genOutPath);

end; { end CLF_RunGenericExternalCommand() }


{***************************************************************************
 * function CLF_SvnGetExtendedStatus()
 *  Get extended svn status information for a set of files.
 *  Create a string list to hold the 9 character status information for each.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_SvnGetExtendedStatus(    scriptsPath    : TDynamicString;
                                      projectPath    : TDynamicString;
                                      allProjectDocs : TStringList;
                                  var svnExtStatus   : TStringList;
                                      )              : Integer;

const
   numDocumentsPerBatch = 60;
   
var
   rc              : Integer;
   svnOut          : TStringList;
   i               : Integer;
   ninthCharStatus : TString;
   statusStr       : TString;
   j               : Integer;
   someProjectDocs : TStringList;
   
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('Hello from CLF_SvnGetExtendedStatus()');
   WriteToDebugFile(' scriptsPath is "' + scriptsPath + '".');
   WriteToDebugFile(' projectPath is "' + projectPath + '".');

   { Create string list to hold status info. }
   svnExtStatus      := TStringList.Create;

   { Create string list to hold subset of project document names. }
   someProjectDocs   := TStringList.Create;
   
   
   { We can't actually list more than about 100 file names on a valid windoze command line,
    before hitting limitations on command line length (8191 chars).  So we may have to split
    this up into batches. }
   j := 0;
   repeat

      { Copy the first numDocumentsPerBatch max document names into small string list. }
      for i := j to Min((j+numDocumentsPerBatch-1), (allProjectDocs.Count - 1)) do
      begin

         WriteToDebugFile(' Copying document number ' + IntToStr(i) + ' to someProjectDocs.');
         someProjectDocs.Add(allProjectDocs.Strings[i]);
         
      end; { endfor }
      
      
      {* Issue "svn status -vu" command to determine status of all project documents with respect to svn repo. *}
      { Initialize svnOut string list. }
      svnOut := TStringList.Create;

      { Call IssueSvnCommandGetOutput() to do all the real work. }
      IssueSvnCommandGetOutput(scriptsPath,
                               projectPath,
                               {command} constSvnCmdStatusWrtServer + ' -v',
                               {parms} someProjectDocs,
                               {var} svnOut);


      //   { Dump svn output to debug file. }
      //   for i := 0 to (svnOut.Count - 1) do
         //      WriteToDebugFile(' svnOut[' + IntToStr(i) + '] is "' + svnOut[i] + '".');

      { Remove junk lines from svn output. }
      for i := (svnOut.Count - 1) downto 0 do
      begin

         { Remove all lines that start with "Status against revision:", which are useless to us. }
         if (CLF_DoesStringStartWith(svnOut[i], constSvnRepStatusStatAgainstRev)) then
            svnOut.Delete(i);

      end; { endfor }

      { Sanity check. }
      if (svnOut.Count <> someProjectDocs.Count) then
         MyAbort('Did not get expected number of lines of output from svn status -uv command!  Got ' +
                 IntToStr(svnOut.Count) + ', expected ' + IntToStr(someProjectDocs.Count) + '.');

      { Loop over all remaining lines of svn output. }
      for i := 0 to (svnOut.Count - 1) do
      begin

         { See if we have a warning that the node (subdir) is not yet in svn.
          These will be fixed later by calling "svn add --parents". }
         if (CLF_DoesStringStartWith(svnOut.Strings[i], constSvnWarnNodeNotInSvn)) then
         begin
            
            { Flag that this document needs to be svn added. }
            { Create fake string starting with '?' so that the caller will get the message to "svn add --parents" this file. }
            statusStr := constSvnRepStatusNotInSvn + '        ';
            WriteToDebugFile(' statusStr is "' + statusStr + '".  This file needs "svn add --parents" :"' + someProjectDocs.Strings[i] + '".');
         end

         { Else process the status output as usual. }
         else
         begin
         
            { Extract all 9 chars of svn status info for this file. }
            statusStr := Copy(svnOut.Strings[i],1,(constSvnStatusUvRemFieldsStartAtCol-1));
            WriteToDebugFile(' statusStr is "' + statusStr + '".');

            { Extract just the 9th char, which indicates status with respect to repo. }
            ninthCharStatus := Copy(svnOut.Strings[i],(constSvnStatusUvRemFieldsStartAtCol-1),1);

            //      ninthCharStatus := '*';

            { Sanity check }
            if (ninthCharStatus = '*') then
               MyAbort('Svn working copy is out of date with respect to server!  You need to do an svn update of the project directory!');
            
         end; { endelse }

         { Add these 9 chars as a string to stringlist, at a position corresponding to this file number. }
         svnExtStatus.Add(statusStr);

      end; { endfor }

      { Free svnOut list. }
      svnOut.Free;

      { Increment outer loop counter. }
      j := j+numDocumentsPerBatch;

      { Clear string list holding subset of project docs. }
      someProjectDocs.Clear;
      
      { Repeat until we've queried all the files in this (possibly huge) project. }
   until (j > allProjectDocs.Count);

   { Free string list of some project docs. }
   someProjectDocs.Free;
   
end; { end CLF_SvnGetExtendedStatus() }


{***************************************************************************
 * function CLF_SvnAddFilesGetOutput()
 *  Safely (supporting svn client v1.7.x) add files to svn.  This means
 *  first check to see that they need to be added.  If so, then do the svn add.
 *  Return all text output by svn, for future use.
 *
 *  Note that in svn client 1.6.x, it was merely a warning, not an error,
 *  when attempting to svn add files that were already in svn.
 *  With svn client v1.7.x, it is now an error. *sigh*
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_SvnAddFilesGetOutput(    scriptsPath    : TDynamicString;
                                      projectPath    : TDynamicString;
                                      allProjectDocs : TStringList;
                                  var svnOut         : TStringList;
                                      )              : Integer;

var
   rc              : Integer;
   i               : Integer;
   svnExtStatus    : TStringList;
   newProjectDocs  : TStringList;
   firstCharStatus : TString;
   currentDoc      : TString;
   
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('Hello from CLF_SvnAddFilesGetOutput()');
   WriteToDebugFile(' scriptsPath is "' + scriptsPath + '".');
   WriteToDebugFile(' projectPath is "' + projectPath + '".');

   { Call CLF_SvnGetExtendedStatus() to get status on all files in question. }
   CLF_SvnGetExtendedStatus(scriptsPath,
                            projectPath,
                            allProjectDocs,
                            {var} svnExtStatus);
   
   { Create string lists to hold lists of project files that have been added/modified. }
   newProjectDocs := TStringList.Create;

   { Loop over all project documents. }
   { Note:  Here we assume that another function has made sure that there are the
    same number of lines in allProjectDocs as in svnExtStatus string lists! }
   for i := 0 to (allProjectDocs.Count - 1) do
   begin

      { Retrieve the name of the current project document. }
      currentDoc := allProjectDocs.Strings[i];

      { Extract just the 1st char, which indicates local status. }
      firstCharStatus := Copy(svnExtStatus.Strings[i], 1, 1);

      { See if we have a '?', indicating a file not yet in svn. }
      if (firstCharStatus = constSvnRepStatusNotInSvn) then
      begin
         
         { Flag that this document needs to be svn added. }
         newProjectDocs.Add(currentDoc);
         WriteToDebugFile('Identified new project document "' + currentDoc + '".');
         
      end { endif '?' }

   end; { endfor }

   { See if we have any files that actually need to be svn added. }
   if (newProjectDocs.Count > 0) then
   begin
      
      { Try to add all these files to svn.  Get output from svn. }
      rc := IssueSvnCommandGetOutput(scriptsPath,
                                     projectPath,
                                     {command} constSvnCmdAdd,
                                     {parms} newProjectDocs,
                                     {var} svnOut);

   end { endif }

   { Else we have nothing to do.  Note this to debug file and call it good. }
   else
   begin

      WriteToDebugFile('No files actually need to be svn added.  Skipping this step.');

   end; { endelse }
   
   { Free string lists. }
   svnExtStatus.Free;
   newProjectDocs.Free;
      
end; { end CLF_SvnAddFilesGetOutput() }


{***************************************************************************
 * function CLF_SvnAddFiles()
 *  Safely (supporting svn client v1.7.x) add files to svn.  This means
 *  first check to see that they need to be added.  If so, then do the svn add.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_SvnAddFiles(scriptsPath    : TDynamicString;
                         projectPath    : TDynamicString;
                         allProjectDocs : TStringList;
                         )              : Integer;

var
   svnOut : TStringList;
   i      : Integer;

begin

   { Initialize svnOut string list. }
   svnOut := TStringList.Create;

   { Call CLF_SvnAddFilesGetOutput() to do all the real work. }
   result := CLF_SvnAddFilesGetOutput(scriptsPath,
                                      projectPath,
                                      allProjectDocs,
                                      {var} svnOut);

   { Free svnOut list. }
   svnOut.Free;

end; { end CLF_SvnAddFiles() }


{***************************************************************************
 * function CLF_GetActualCasingOfFileName()
 *  Shell out and run find.exe in order to obtain the actual casing of a file
 *  name that we got from Delphi FindFiles().  These are returned as all upper
 *  case filenames.  This offends the unix hacker in me.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_GetActualCasingOfFileName(    scriptsPath       : TDynamicString;
                                           fileNameUpperCase : TString;
                                       var fileNameTrueCase  : TString;
                                           )                 : Integer;
var
   rc           : Integer;
   parms        : TStringList;
   genOut       : TStringList;
   fileNameOnly : TString;
   filePathOnly : Tstring;
   
begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('Hello from CLF_GetActualCasingOfFileName()');
   WriteToDebugFile(' fileNameUpperCase is "' + fileNameUpperCase + '"');

   { Split the target file into path and name components. }
   fileNameOnly := ExtractFileName(fileNameUpperCase);
   filePathOnly := ExtractFilePath(fileNameUpperCase);
   
   { Create necessary string lists. }
   parms             := TStringList.Create();
   genOut            := TStringList.Create();

   parms.Add('-maxdepth');
   parms.Add('1');
   parms.Add('-iname');
   parms.Add(fileNameOnly);

   { Run generic external command, only in this case it will be "find.exe". }
   CLF_RunGenericExternalCommand(scriptsPath,
                                 filePathOnly,
                                 {batFile} 'generic_cmd',
                                 {command} 'find',
                                 parms,
                                 {var} genOut);

   { We expect exactly 1 line of output. }
   if (genOut.Count <> 1) then
      CLF_Abort('Did not get exactly 1 line of output from external find command.');

   { Return result to caller. }
   { Strip off the leading ".\". }
   fileNameTrueCase := filePathOnly + StringReplace(genOut[0], '.\', '', 0);
   WriteToDebugFile(' fileNameTrueCase is "' + fileNameTrueCase + '"');
   
   { Free string lists. }
   parms.Free();
   genOut.Free();

end; { end CLF_GetActualCasingOfFileName() }


{***************************************************************************
 * function CLF_RefreshProjectSvnStatus()
 *  Refresh a project's svn status to update the display in Workspace Manager.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_RefreshProjectSvnStatus(foo : Integer;
                                     )   : Integer;

var
   i        : Integer;

begin

   { Assume success. }
   result := 0;
   
   { Refresh svn status of target project. }
   ResetParameters;
   AddStringParameter('Action', 'RefreshProject');
   AddStringParameter('ObjectKind', 'FocusedProject');
   RunProcess('VersionControl:VersionControl');
   
end; { end CLF_RefreshProjectSvnStatus() }


{***************************************************************************
 * function CLF_DeleteDir()
 *  Perform an rmdir operation.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_DeleteDir(dirPath : TString;
                       )        : Integer;
var
   rc      : Integer;
   i       : Integer;
   dirName : TString;
           
begin
   
   { For now, assume/hope/pray that we will succeed. }
   result := 0;
   
   WriteToDebugFile('Hello from CLF_DeleteDir()');
   
   { Extract just the last entry from this directory name. }
   dirName := ExtractFileName(dirPath);
   
   { Don't attempt to delete "." or "..". }
   if ( (dirName <> '.') and (dirName <> '..') ) then
   begin
      
      { Delete the directory. }
      rc := RemoveDir(dirPath);
      
      { Sanity check. }
      if (not rc) then
      begin

     { Delay for 500 us before we make the second attempt to delete dir. }
     Sleep(500);

     { Try again to delete the directory. }
     rc := RemoveDir(dirPath);
     
     { Sanity check. }
     if (not rc) then
        CLF_Abort('Unable to delete directory "' + dirPath + '"!');

      end;
      
   end; { endif }
   
end; { end CLF_DeleteDir() }


{***************************************************************************
 * function CLF_DeleteFilesAndDirs()
 *  Perform a rm -rf (eg. delete everything below a given starting point).
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_DeleteFilesAndDirs(filePath : TString;
                                )        : Integer;
var
   rc            : Integer;
   i             : Integer;
   searchSubDirs : Boolean;
   fileList      : TStringList;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('Hello from CLF_DeleteFilesAndDirs()');

   { Create string lists to hold list files. }
   fileList := TStringList.Create();
   
   { Find all files and directories starting at the given filePath. }
   searchSubDirs := True;
   FindFiles(filePath, '*.*', faAnyFile, searchSubDirs, fileList);

   { Delete all files. }
   for i := 0 to (fileList.Count - 1) do
   begin
      WriteToDebugFile(' Attempting to delete file/dir "' + fileList[i] + '".');

      { Delete the file. }
      { Note:  This function will filter out any directories that we give it. }
      DeleteFileWithVerify(fileList[i]);
      
   end; { endfor }

   { Free string lists. }
   fileList.Free();


   { Create string lists to hold list files. }
   fileList := TStringList.Create();
   
   { Find all directories starting at the given filePath. }
   searchSubDirs := True;
   FindFiles(filePath, '*.*', faAnyFile, searchSubDirs, fileList);

   { Delete all directories. }
   { Note:  Reverse the order of traversal! }
   for i := (fileList.Count - 1) downto 0 do
   begin
      WriteToDebugFile(' Attempting to delete file/dir "' + fileList[i] + '".');

      { Delete the directory. }
      CLF_DeleteDir(fileList[i]);

   end; { endfor }

   { Delete the final directory. }
   CLF_DeleteDir(filePath);

   { Free string lists. }
   fileList.Free();

end; { end CLF_DeleteFilesAndDirs() }


{***************************************************************************
 * function CLF_MoveFile()
 *  Perform a filesystem move file.  Do so forcibly (eg "mv -f").  Abort on error.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_MoveFile(fileOldPath : TString;
                      fileNewPath : TString;
                      )           : Integer;
var
   rc        : Integer;
   i         : Integer;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('Hello from CLF_MoveFile()');

   { Delete the destintion file if it already exists. }
   DeleteFileWithVerify(fileNewPath);

   { Do a filesystem move for this file. }
   if (not RenameFile(fileOldPath, fileNewPath)) then
      CLF_Abort('Unable to move file "' + fileOldPath + '" to "' + fileNewPath + '"!');

end; { end CLF_MoveFile() }


{***************************************************************************
 * function CLF_MoveFileSvnAware()
 *  Move a file, but with awareness of svn.  In other words, if the file
 *  is already in svn, do an svn move.  If not, do a filesystem move.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_MoveFileSvnAware(scriptsPath : TDynamicString;
                              projectPath : TDynamicString;
                              fileOldPath : TString;
                              fileNewPath : TString;
                              )           : Integer;
var
   rc        : Integer;
   svnOut    : TStringList;
   i         : Integer;
   parms     : TStringList;
   doSvnMove : Boolean;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   WriteToDebugFile('Hello from CLF_MoveFileSvnAware()');

   { Initialize svnOut string list. }
   svnOut := TStringList.Create;

   { Initialize parms string list. }
   parms := TStringList.Create;

   { Tell svn which file to check status of. }
   parms.Add(fileOldPath);
   
   { Call IssueSvnCommandGetOutput() to do all the real work. }
   IssueSvnCommandGetOutput(scriptsPath,
                            projectPath,
                            constSvnCmdStatus,
                            parms,
                            {var} svnOut);

   { Assume that we need to do an svn move until we find out otherwise. }
   doSvnMove := True;

   { Loop over all the output produced by svn.exe (files and directories that differ from svn server). }
   for i := 0 to (svnOut.Count - 1) do
   begin

      { Look for a line that has a '?' char as the first char in a line of svn output.
       This indicates a file that exists locally, but not in the svn repo. }
      if (Copy(svnOut.Strings[i],1,1) = constSvnRepStatusNotInSvn) then
      begin

         WriteToDebugFile('File is not in svn.  Proceeding to do filesystem move.');

         { Do a filesystem move for this file. }
         if (not RenameFile(fileOldPath, fileNewPath)) then
            CLF_Abort('Unable to move file "' + fileOldPath + '" to "' + fileNewPath + '"!');

         { Flag that we do not need to do a filesystem move for this file. }
         doSvnMove := False;
                  
      end; { endif }
         
   end; { endfor }

   { If we need to do an svn move, then proceed to do so. }
   if (doSvnMove) then
   begin

      WriteToDebugFile('File is in svn.  Proceeding to do svn move.');

      { Delete the destintion file if it already exists, to avoid problems with svn move. }
      DeleteFileWithVerify(fileNewPath);
      
      { We already have the old file on the parms list.  Add the new file path. }
      parms.Add(fileNewPath);
   
      { Do an svn move operation. }
      IssueSvnCommand(scriptsPath,
                      projectPath,
                      constSvnCmdMove,
                      parms);
      
   end; { endif }

   { Free parms list. }
   parms.Free;

   { Free svnOut list. }
   svnOut.Free;

end; { end CLF_MoveFileSvnAware() }


{***************************************************************************
 * function CLF_RevertOldCsvFileAndReadIn()
 *  See if the csv report file already exists in svn.  If so, see if we need
 *  to revert it so that we can read in its contents as of its last checkin.
 *
 *  When acting in footprint mode, stepFilePath is a null string.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_RevertOldCsvFileAndReadIn(    projectPath           : TDynamicString;
                                           scriptsPath           : TDynamicString;
                                           pcbLibOrFcstdFilePath : TString;
                                           reportOrIniFilePath   : TString;
                                           stepFilePath          : TString;
                                           csvOrLogFilePath      : TString;
                                       var csvOrLogFileOld       : TStringList;
                                       var deletedStepFile       : Boolean;
                                           )                     : Integer;

var
   rc                 : Integer;
   svnOut             : TStringList;
   i                  : Integer;
   doSvnRevert        : Boolean;
   doReadIn           : Boolean;
   parms              : TStringList;
   firstCharStatus    : TString;
   ninthCharStatus    : TString;
   statusFieldsCsv    : TStringList;
   statusFieldsPcbLib : TStringList;
   svnRepoRevCsv      : TString;
   svnRepoRevPcbLib   : TString;
   dummy              : TStringList;
   filePathList       : TStringList;
   filePath           : TString;
   filesAdded         : Integer;
   stepFileAdded      : Boolean;

begin

   { For now, assume/hope/pray that we will succeed. }
   result := 0;

   { Assume that we will not find an un-added STEP file and delete it. }
   deletedStepFile := False;
   
   { Assign safe values for these variables. }
   svnRepoRevCsv         := '-1';
   svnRepoRevPcbLib      := '-1';
   
   WriteToDebugFile('Hello from CLF_RevertOldCsvFileAndReadIn()');

   WriteToDebugFile('pcbLibOrFcstdFilePath: ' + pcbLibOrFcstdFilePath);
   WriteToDebugFile('reportOrIniFilePath: ' + reportOrIniFilePath);
   WriteToDebugFile('stepFilePath: ' + stepFilePath);
   WriteToDebugFile('csvOrLogFilePath: ' + csvOrLogFilePath);

   { Store the  file name in a stringlist, so that we can loop over it.
    If stepFilePath is null (as in footprint mode or when old STEP file does not exist)
    do not include in string list. }
   filePathList := TStringList.Create;
   filePathList.Add(pcbLibOrFcstdFilePath);
   filePathList.Add(reportOrIniFilePath);
   if ( stepFilePath <> '' ) then
      filePathList.Add(stepFilePath);
   filePathList.Add(csvOrLogFilePath);

   {* Issue "svn status -vu" command to determine status of files with respect to svn repo. *}
   { Initialize svnOut string list. }
   svnOut := TStringList.Create;
   
   { Initialize parms string list. }
   parms := TStringList.Create;
   
   { Tell svn which files to check status of. }
   parms.Add('-v');

   { Loop over all files. }
   for i := 0 to filePathList.Count - 1 do
   begin

      filePath := filePathList[i];
      { If the file does not yet exist, then write a 0 length file to it so that we may check svn status. }
      if (not FileExists(filePath) ) then
      begin
          
      WriteToDebugFile('File does not exist: ' + filePath);
      { Create string list. }
      dummy := TStringList.Create();
      
      { Write null string list to file. }
      dummy.SaveToFile(filePath);
          
      { Free string list. }
      dummy.Free();
       
      end; { end if }

    parms.Add(filePath);
          
    end; { end loop }

   { Call IssueSvnCommandGetOutput() to do all the real work. }
   IssueSvnCommandGetOutput(scriptsPath,
                            projectPath,
                            {command} constSvnCmdStatusWrtServer,
                            parms,
                            {var} svnOut);

   { Free parms list. }
   parms.Free;

   { Dump svn output to debug file. }
   for i := 0 to (svnOut.Count - 1) do
      WriteToDebugFile(' svnOut[' + IntToStr(i) + '] is "' + svnOut[i] + '".');

   { Remove junk lines from svn output. }
   for i := (svnOut.Count - 1) downto 0 do

      { Remove all lines that start with "Status against revision:", which are useless to us. }
      if (CLF_DoesStringStartWith(svnOut[i], constSvnRepStatusStatAgainstRev)) then
         svnOut.Delete(i);

      { Sanity check. }
   if ( svnOut.Count <> filePathList.Count ) then
         CLF_Abort('Did not get expected number of lines of output from svn status -uv command!');
   
   { After removing junk lines (if needed), we should have 4 lines of output if acting in 3d model mode
    and 3 lines if acting in footprint mode:
    Line 0 will be the svn status of the PcbLib or FCStd file.
    Line 1 will be the svn status of the report or ini file.
    Line 2 will be the svn status of the csv or STEP file.
    Line 3 will be the svn status of the log file. }

   {* Decide if we should read in old csv file and/or do svn revert operation. *}
   { Flag that by default, we will not do svn revert operation. }
   doSvnRevert           := False;

   { Initialize parms string list. }
   parms := TStringList.Create;

   { Initialize filesAdded to zero. }
   filesAdded := 0;

   { Assume that a step file does not have "Added" status. }
   stepFileAdded := False;
   
   { Loop over all 4 files. }
   for i := 0 to filePathList.Count -1 do
   begin

      filePath := filePathList[i];
      
      { Extract first char of this line of output. }
      firstCharStatus :=    Copy(svnOut.Strings[i],1,1);
      WriteToDebugFile(' firstCharStatus is "' + firstCharStatus + '".');

      { Extract ninth char of this line of output. }
      ninthCharStatus :=    Copy(svnOut.Strings[i],9,1);
      WriteToDebugFile(' ninthCharStatus is "' + ninthCharStatus + '".');

      { Sanity check. }
      if ( ninthCharStatus = '*' ) then
         CLF_Abort('Svn working copy is out of date with respect to server!  You need to do an svn update of the project directory!');
      
      { Look for an 'A' char (file has been added but not ever checked into svn). There's no reason for us to revert this file. }
      if ( firstCharStatus = constSvnRepStatusAdded ) then
      begin
         
         Inc(filesAdded);
         
         WriteToDebugFile(' File was previously added to check-in queue: ' + filePath);
         
         if ( filePath = stepFilePath ) then
            stepFileAdded := True;
         
      end { end if }

      { Else look for a '?' char (file is not in svn). If file is a step file, this may have been caused by a previous run that crashed.
        Inform user that file will be deleted. }
      else if ( firstCharStatus = constSvnRepStatusNotInSvn ) then
      begin

         WriteToDebugFile(' File not in svn: ' + filePath);

         if ( filePath = stepFilePath ) then 
         begin
            ShowMessage('This STEP file is not in subversion and is probably a product of a failed script run. Will proceed to delete ' + filePath);
            WriteToDebugFile('STEP file not in svn. Will delete ' + filePath);
            DeleteFileWithVerify(filePath);
            deletedStepFile := True;
         end;
         
      end { end else if }

      { Else look for a ' ' char (file is up-to-date with respect to server.  In this case,
       we don't need to revert. }
      else if ( firstCharStatus = constSvnRepStatusHappy ) then
      begin

         WriteToDebugFile(' File unchanged since last check-in: ' + filePath);
         
      end { end elsif }

      { In all other cases ('M', etc.), flag that we want to do an svn revert operation. }
      else
      begin
         
         WriteToDebugFile(' Got to else case, so file is presumably "M" status.  Flagging to do both svn revert: ' + filePath);

         { Flag that we want to do svn revert. }
         doSvnRevert := True;

         { Add this file to list of files to revert. }
         parms.Add(filePath);
         
      end; { endelse }

   end; { end loop }

   { Free svnOut list. }
   svnOut.Free;

   { If the STEP file was the only file with "Added" status, abort the script, citing that the script has not been checked in since the last time it ran to completion. }
   if ( stepFileAdded and (filesAdded = 1) ) then
      CLF_Abort('STEP file was added to check-in queue while all other were not. Please check-in previously generated footprint files before running the script.');

   {* Do an svn revert of the files if needed. *}
   if (doSvnRevert) then
   begin
      
      WriteToDebugFile(' About to revert following files:');
      for i := 0 to parms.Count - 1 do
         WriteToDebugFile('  '  + parms[i]);
      
      { Call IssueSvnCommand() to do all the real work. }
      IssueSvnCommand(scriptsPath,
                      projectPath,
                      {command} constSvnCmdRevert,
                      parms);

   end; { endif }
   
   { Free parms list. }
   parms.Free;
   
   {* Read in the contents of the existing (reverted?) csv file. *}
   { Initialize stringlist. }
   csvOrLogFileOld  := TStringList.Create();
   
   WriteToDebugFile(' About to read in old csv/log file.');
   
   { Make sure it's readable (eg. not still open and flocked by Excel). }
   VerifyFileIsReadable(csvOrLogFilePath);
   
   { Read old csv/log file into stringlist. }
   csvOrLogFileOld.LoadFromFile(csvOrLogFilePath);
   
end; { end CLF_RevertOldCsvFileAndReadIn() }


{***************************************************************************
 * function CLF_ProjectAddRemoveFile()
 *  Add or remove a specified source document to/from the project.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ProjectAddRemoveFile(Project         : IProject;
                                  addRemoveMePath : IDocument;
                                  action          : TString;
                                  )               : Integer;

var
   i        : Integer;
   k        : Integer;
   document : IDocument;
   found    : Boolean;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_ProjectAddRemoveFile()');
   WriteToDebugFile('action is: ' + action);
   { Flag that we have not yet found the document we're looking for. }
   found := False;

   { Look over all project documents looking for PcbDoc files. }
   {** Loop over all logical documents in the project (counting backwards). **}
   for k := (Project.DM_LogicalDocumentCount - 1) downto 0 do
   begin

      { Retrieve reference to kth document in project. }
      document := Project.DM_LogicalDocuments(k);
      WriteToDebugFile(' Examining project document ' + document.DM_FullPath);

      { See if this is a match for the one we're looking for. }
      if (addRemoveMePath = document.DM_FullPath) then
      begin

         { Flag that we've found the document we're looking for. }
         WriteToDebugFile(' Found a match!');
         found := True;

         { If we're supposed to be removing the document from the project,
          then proceed to do so. }
         if (action = 'remove') then
         begin
            project.DM_RemoveSourceDocument(addRemoveMePath);
            WriteToDebugFile(' Removing document "' + addRemoveMePath + '" from project.');
         end;

      end;

   end; { endfor }

   { If we're supposed to be adding a document to the project and said
    document isn't already part of the project, then proceed to add it. }
   if ( (action = 'add') and (not found) ) then
   begin
      project.DM_AddSourceDocument(addRemoveMePath);
      WriteToDebugFile(' Adding document "' + addRemoveMePath + '" to project.');
   end;

end; { end CLF_ProjectAddRemoveFile() }


{***************************************************************************
 * function CLF_ProjectAddFile()
 *  Add a specified source document to the project, but only if it's not
 *  already part of the project.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ProjectAddFile(Project   : IProject;
                            addMePath : IDocument;
                            )         : Integer;

begin

   { Call CLF_ProjectAddRemoveFile() to do all the real work. }
   result := CLF_ProjectAddRemoveFile(Project,
                                      addMePath,
                                      'add');

end; { end CLF_ProjectAddFile() }


{***************************************************************************
 * function CLF_ProjectRemoveFile()
 *  Remove a specified source document from the project.  Remove multiple
 *  instances of it, if needed.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ProjectRemoveFile(Project      : IProject;
                               removeMePath : IDocument;
                               )               : Integer;

begin

   { Call CLF_ProjectAddRemoveFile() to do all the real work. }
   result := CLF_ProjectAddRemoveFile(Project,
                                      removeMePath,
                                      'remove');

end; { end CLF_ProjectRemoveFile() }


{***************************************************************************
 * function CLF_ReadXmlHandleSsIndex()
 *  Handle an "ssIndex="[0-9]+">" field within an XML file.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReadXmlHandleSsIndex(    field  : TString;
                                  var index : Integer;
                                      )     : Integer;
var
   j            : Integer;
   leftStr      : TString;
   rightStr     : TString;
   xmlSubfields : TStringList;
   
begin

   { Assume success. }
   result := 0;
   xmlSubfields := TStringList.Create();

   WriteToDebugFile('Hello from CLF_ReadXmlHandleSsIndex(), field is "' + field + '".');

   { See if this field contains "ss:Index=". }
   if (AnsiPos('ss:Index=', field) <> 0) then
   begin

      {* Split into subfields using " " as separator. *}

      { Split the string apart using " " as a separator. }
      CLF_SplitDelimitedUnquotedStringIntoStringList({delimitedString} field,
                                                     {delimiter} ' ',
                                                     {var stringList} xmlSubfields);

      { Loop over all the subfields. }
      for j:=0 to (xmlSubfields.Count - 1) do
      begin

         WriteToDebugFile(' subfield is "' + xmlSubfields[j] + '".');

         { See if this subfield starts with "ss:Index". }
         if (CLF_DoesStringStartWith(xmlSubfields[j], 'ss:Index')) then
         begin
            
            { Split the subfield into "ss:Index=" and "x". }
            SplitStringIntoLeftAndRight(xmlSubfields[j],
                                        constStringEquals,
                                        {var} leftStr,
                                        {var} rightStr);
            
            { Convert the rightStr into an integer and return that as var parm index. }
            index := StrToInt(StringReplace(rightStr, '"', '', MkSet(rfReplaceAll)));
            WriteToDebugFile('   index is now ' + IntToStr(index) + '.');

         end; { endif }
            
      end; { endfor }
      
   end; { endif }

   { Free string list. }
   xmlSubfields.Free();

end; { end CLF_ReadXmlHandleSsIndex() }


{***************************************************************************
 * function CLF_ReadXmlFileIntoStringList()
 *  Read simple data from an .xml file.
 *
 *  For our purposes here, we make the following simplifying assumptions:
 *  1.  We will not read/parse/validate the entire .xml file.
 *  2.  There should only be 1 worksheet.
 *  3.  We only care about the cell data itself.
 *  4.  We look for <Row> and </Row> tags.
 *  5.  Within such, we look for <Cell> and </Cell> tags.
 *  6.  Within such, we look for <Data> and </Data> tags.
 *  7.  We will parse "Index="x"" within a <Cell> tag.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReadXmlFileIntoStringList(    xmlFilePath     : TString;
                                       var xmlCellValues   : TStringList;
                                           )               : Integer;

var
   i             : Integer;
   j             : Integer;
   rc            : Integer;
   rawXmlFile    : TStringList;
   state         : Integer;
   line          : TDynamicString;
   currRow       : Integer;
   currCol       : Integer;
   xmlFields     : TStringList;
   leftStr       : TString;
   rightStr      : TString;
   nameValuePair : TString;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_ReadXmlFileIntoStringList()');

   { Start out at row 1, col 1. }
   currRow          := 1;
   currCol          := 1;
   
   { Create stringlist to read in raw .xml file. }
   rawXmlFile       := TStringList.Create;
   xmlFields        := TStringList.Create;

   { Verify that file is readable (eg. not flocked by Excel). }
   VerifyFileIsReadable(xmlFilePath);
         
   { Read raw xml file into stringlist. }
   rawXmlFile.LoadFromFile(xmlFilePath);

   { Init state to be 0. }
   state := 0;

   
   { Loop over all the lines in the file. }
   for i := 0 to (rawXmlFile.Count-1) do
   begin

      { Read current line of file, stripped of leading ' ' (space) chars. }
      line := CLF_StripLeadingChar(rawXmlFile.Strings(i), ' ');
      WriteToDebugFile('Line is "' + line + '".');

      { Examine current state and act appropriately. }
      case state of

        { State 0:  Look for a <Row> tag. }
        0 :
           begin

              { If we find a '<Row' tag, then advance state. }
              if (CLF_DoesStringStartWith(line, '<Row')) then
              begin

                 {* Split into fields using ">" as separator. *}
                 { Clear existing data from xmlFields stringlist. }
                 xmlFields.Clear;

                 { Split the string apart using ">" as a separator. }
                 CLF_SplitDelimitedUnquotedStringIntoStringList({delimitedString} line,
                                                                {delimiter} '>',
                                                                {var stringList} xmlFields);

                 { Loop over all the fields. }
                 for j:=0 to (xmlFields.Count - 1) do
                 begin

                    { Handle any "ss:Index="x"" fields. }
                    CLF_ReadXmlHandleSsIndex({field} xmlFields[j],
                                             {var index} currRow);

                    
                 end; { endfor }
                 
                 { Indicate that we will start getting data for column 1. }
                 currCol := 1;
                 
                 { Advance state. }
                 state := 1;
                 WriteToDebugFile('Found <Row.  Advancing state!');
                 
              end; { endif }
                 
           end; { endcase 0 }

        { State 1:  Look for a <Cell> tag. }
        1 :
           begin

              { If we find a '</Row' tag, then advance state. }
              if (CLF_DoesStringStartWith(line, '</Row')) then
              begin

                 { Increment row index. }
                 currRow          := currRow + 1;

                 { Advance state. }
                 state := 0;
                 WriteToDebugFile('Found </Row.  Advancing state!');

              end { endif }
                 
              { Else if we find a '<Cell' tag, then proceed to look for <Data> tag. }
              else if (CLF_DoesStringStartWith(line, '<Cell')) then
              begin

                 WriteToDebugFile('Found <Cell!');

                 {* Split into fields using ">" as separator. *}
                 { Clear existing data from xmlFields stringlist. }
                 xmlFields.Clear;

                 { Split the string apart using ">" as a separator. }
                 CLF_SplitDelimitedUnquotedStringIntoStringList({delimitedString} line,
                                                                {delimiter} '>',
                                                                {var stringList} xmlFields);

                 { Loop over all the fields. }
                 for j:=0 to (xmlFields.Count - 1) do
                 begin

                    { Handle any "ss:Index="x"" fields. }
                    CLF_ReadXmlHandleSsIndex({field} xmlFields[j],
                                             {var index} currCol);

                    { Look for a field that does not begin with "<".  That is our actual data. }
                    if (not CLF_DoesStringStartWith(xmlFields[j], '<')) then
                    begin

                       { The data field will look like "foo</Data". }
                       rc := SplitStringIntoLeftAndRight(xmlFields[j],
                                                         '<',
                                                         {var} leftStr,
                                                         {var} rightStr);

                       { Make sure that we found the '<' delimiter. }
                       if (rc = 0) then
                       begin

                          WriteToDebugFile('Found actual data.  It is "' + leftStr + '".');
                          
                          { Construct a name=value pair. }
                          nameValuePair := 'Row_' + IntToStr(currRow) + '_Col_' + IntToStr(currCol) + constStringEquals + leftStr;
                          xmlCellValues.Add(nameValuePair);
                          
                          WriteToDebugFile('Adding this to xmlCellValues: "' + nameValuePair + '".');

                       end;

                    end; {endif }
                    
                 end; { endfor }
                 
                 { Increment column index. }
                 currCol := currCol + 1;
                 
              end; { end elsif }

           end; { endcase 1 }

      else MyAbort('In CLF_ReadXmlFileIntoStringList(), in unknown state ' + IntToStr(state));
      end; { endcase }          
      
   end; { endfor }

   { Free local stringlists. }
   rawXmlFile.Free;
   xmlFields.Free;
   
end; { end CLF_ReadXmlFileIntoStringList() }


{***************************************************************************
 * function CLF_SetSolderPasteExpansion()
 *  Set the desired solderpaste expansion for a specified pin.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_SetSolderPasteExpansion(var pad                    : IPCB_Pad;
                                         solderpasteExpansionMm : Real;
                                         )                      : Integer;

var
   padCache : TPadCache;
   
begin                                                               

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_SetSolderPasteExpansion().');

   { Configure this pad to use the newly calculated solderpaste expansion. }
   padCache                           := pad.GetState_Cache;
   padCache.PasteMaskExpansion        := (MMsToCoord(solderpasteExpansionMm));
   padCache.PasteMaskExpansionValid   := eCacheManual;
   pad.SetState_Cache                 := padCache;
      
end; { end CLF_SetSolderPasteExpansion() }


{***************************************************************************
 * function CLF_CalculateAndSetSolderPasteExpansionForEp()
 *  Calculate the appropriate solderpaste expansion for an EP pad.
 *  Typically we want solderpaste on 40% of the area of the EP pad.
 *  But in Altium we must specify this as an expansion with respect to
 *  the boundaries of the pad.
 *
 *  So:
 *  A = area
 *  fA = fractional area = 0.4*A
 *  w = solderpaste expansion per side of pad (which will be a negative number)
 *  x = x size of pad
 *  y = y size of pad
 *
 *  Then:
 *  (x + 2*w) * (y + 2*w) = fA
 *  xy + 2xw + 2yw + 4w^2 = fA
 *
 *  Rearrange to fit quadratic equation, solving for w:
 *  4w^2 + (2x+2y)w + (xy - fA) = 0
 *  a = 4
 *  b = (2x + 2y)
 *  c = (xy - fA)
 *
 *  Solve quadratic equation for w:
 *  roots = (  (-b) +- sqrt( b^2 - (4*a*c) )  ) / (2*a)
 *
 *  The useful root turns out to be (  (-b) + sqrt( b^2 - (4*a*c) )  ) / (2*a)
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CalculateAndSetSolderPasteExpansionForEp(var cnfGalacticInfo : TStringList;
                                                      var pad             : IPCB_Pad;
                                                          )               : Integer;

var
   solderpastePct         : Real;
   area                   : Real;
   fractionalArea         : Real;
   XSizeMm                : Real;
   YSizeMm                : Real;
   a                      : Real;
   b                      : Real;
   c                      : Real;
   solderpasteExpansionMm : Real;
   
begin                                                               

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_CalculateAndSetSolderPasteExpansionForEp().');

   { Retrieve footprint type. }
   solderpastePct    := StrToFloat(cnfGalacticInfo.Values(constLpWizardBuildInfoEpSolderpastePct));
   WriteToDebugFile(' solderpastePct is ' + FloatToStr(solderpastePct) + '.');

   { Retrieve relevant parameters from pad itself. }
   XSizeMm           := CoordToMMs(pad.TopXSize);
   YSizeMm           := CoordToMMs(pad.TopYSize);

   { Compute the pad area and the desired fractional solderpaste area. }
   { Note:  It doesn't matter if pad is rotated, since we only care about area. }
   area              := XSizeMm * YSizeMm;
   fractionalArea    := (solderpastePct * area) * 0.01;

   { Compute quadratic coefficients. }
   a                 := 4;
   b                 := ( (2.0*XSizeMm) + (2.0*YSizeMm) );
   c                 := ( (XSizeMm*YSizeMm) - fractionalArea );
   
   { Solve quadratic. }
   solderpasteExpansionMm := (  (-1.0*b) + sqrt( sqr(b) - (4.0*a*c) )  ) / (2.0*a);
   WriteToDebugFile(' solderpasteExpansionMm is ' + FloatToStr(solderpasteExpansionMm) + '.');

   { Configure this pad to use the newly calculated solderpaste expansion. }
   CLF_SetSolderPasteExpansion({var} pad,
                               solderpasteExpansionMm);
      
end; { end CLF_CalculateAndSetSolderPasteExpansionForEp() }


{***************************************************************************
 * function CLF_IsFootprintDualRowGullwingType()
 *  Answers the query as to whether this footprint type is dual row gullwing
 *  (eg. SOIC, SOT, SOP).
 *  
 *  Returns:  True if dual row gullwing type, False otherwise.
 ***************************************************************************}
function CLF_IsFootprintDualRowGullwingType(footprintType : TString;
                                            )             : Boolean;

begin

   { Enumerate all known dual row gullwing type footprints. }
   result := ( (footprintType = 'SOIC') or (footprintType = 'SOT') or (footprintType = 'SOP') );

end; { end CLF_IsFootprintDualRowGullwingType() }


{***************************************************************************
 * function CLF_IsPadEp()
 *  Answers the query as to whether this pad is an exposed pad (EP).
 *  
 *  Returns:  True if pad is a EP, False otherwise.
 ***************************************************************************}
function CLF_IsPadEp(pad : IPCB_Pad;
                     )   : Boolean;
                                                     
begin

   { Currently we can do this check by looking for the pad name to start
    with a predefined prefix (eg. "EP"). }
   result := CLF_DoesStringStartWith(pad.Name, constPrefixEpPins);
      
end; { end CLF_IsPadEp() }


{***************************************************************************
 * function CLF_IsPadFiducial()
 *  Answers the query as to whether this pad is a fiducial.
 *  
 *  Returns:  True if pad is a fiducial, False otherwise.
 ***************************************************************************}
function CLF_IsPadFiducial(pad : IPCB_Pad;
                           )   : Boolean;
                                                     
begin

   { Currently we can do this check by looking for the pad name to start
    with a predefined prefix (eg. "FID"). }
   result := CLF_DoesStringStartWith(pad.Name, constPrefixFiducialPins);
      
end; { end CLF_IsPadFiducial() }


{***************************************************************************
 * function CLF_AddPrimNameToList()
 *  Adds a new name for a primitive to the primNames list.
 *  Sets the Index field within said primitive to link it to this name entry.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_AddPrimNameToList(var prim      : IPCB_Primitive;
                                   name      : TString;
                               var primNames : TStringList;
                                   )         : Integer;
                                                     
begin

   { Assume success. }
   result := 0;

   { Add index to this primitive to link it to the name we're about to create for it. }
   prim.Index               := primNames.Count;
   
   { Add primitive name to primNames list. }
   primNames.Add(name);
      
end; { end CLF_AddPrimNameToList() }


{***************************************************************************
 * function CLF_ChangePrimName()
 *  Changes an existing primitive name in the primNames list.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ChangePrimName(var prim      : IPCB_Primitive;
                                name      : TString;
                            var primNames : TStringList;
                                )         : Integer;
                                                     
begin

   { Assume success. }
   result := 0;

   { Add change primitive name in primNames list. }
   primNames.Strings(prim.Index)    := name;
      
end; { end CLF_ChangePrimName() }


{***************************************************************************
 * function CLF_GetPrimName()
 *  Get the current name of a primitive.
 *  
 *  Returns:  current name of primitive.
 ***************************************************************************}
function CLF_GetPrimName(prim      : IPCB_Primitive;
                         primNames : TStringList;
                         )         : TString;
                                                     
begin

   { Lookup current name in primNames stringlist. }
   result := primNames.Strings(prim.Index);
      
end; { end CLF_GetPrimName() }


{***************************************************************************
 * function CLF_IsNameInStringList()
 *  Answers the query as to whether a given name is in a given stringlist.
 *  
 *  Returns:  True if name is in stringlist, False otherwise.
 ***************************************************************************}
function CLF_IsNameInStringList(name       : TString;
                                stringlist : TStringList;
                                )          : Boolean;
                                                     
begin

   { If this name is actually in the stringlist, it will have an index >= 0. }
   result := (stringlist.IndexOfName(name) >= 0);
      
end; { end CLF_IsNameInStringList() }


{***************************************************************************
 * function CLF_GetPadGroupNum()
 *  Get the current group number for a pad.
 *  
 *  Returns:  Current group number for given pad.
 ***************************************************************************}
function CLF_GetPadGroupNum(padDst : IPCB_Pad;
                            )      : Integer;
                                                     
begin

   { We are currently storing pad group number in the pad's Index member. }
   result := padDst.Index;
      
end; { end CLF_GetPadGroupNum() }


{***************************************************************************
 * function CLF_AssignPadToGroupNum()
 *  Assign a given pad to the specified pad group number.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_AssignPadToGroupNum(padDst      : IPCB_Pad;
                                 padGroupNum : Integer;
                                 )           : Integer;

begin

   { Assume success. }
   result := 0;

   { We are currently storing pad group number in the pad's Index member. }
   padDst.Index := padGroupNum;
      
end; { end CLF_AssignPadToGroupNum() }


{***************************************************************************
 * function CLF_GetPadGroupNamePrefix()
 *  Get the name prefix for a pad group, by pad group number.
 *  
 *  Returns:  Current name prefix for specified pad group number.
 ***************************************************************************}
function CLF_GetPadGroupNamePrefix(padGroupNum : Integer;
                                   primNames   : TStringList;
                                   )           : TString;
                                                     
begin

   { Look this name up in the primNames stringlist. }
   result := primNames[padGroupNum];
      
end; { end CLF_GetPadGroupNamePrefix() }


{***************************************************************************
 * function CLF_ReportCsvPropertyStr()
 *  Report a given string property to the CSV file.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportCsvPropertyStr(    name     : TString;
                                      valueStr : TString;
                                  var line     : TString;
                                      )        : Integer;

begin

   { Assume success. }
   result := 0;

   { Add a field to this line of CSV output. }
   line := line + constCsvRptFieldSepChar + name         + '=' + valueStr;

end; { end CLF_ReportCsvPropertyStr() }


{***************************************************************************
 * function CLF_ReportCsvPropertyInt()
 *  Report a given integer property to the CSV file.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportCsvPropertyInt(    name     : TString;
                                      valueInt : Integer;
                                  var line     : TString;
                                      )        : Integer;

begin

   { Call CLF_ReportCsvPropertyStr() to do all the real work. }
   result := CLF_ReportCsvPropertyStr(name,
                                      {valueStr} IntToStr(valueInt),
                                      {var} line);

end; { end CLF_ReportCsvPropertyInt() }


{***************************************************************************
 * function CLF_ReportCsvPropertyFloat()
 *  Report a given real property to the CSV file.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportCsvPropertyFloat(    name       : TString;
                                        valueFloat : Real;
                                    var line       : TString;
                                        )          : Integer;

begin

   { Call CLF_ReportCsvPropertyStr() to do all the real work. }
   result := CLF_ReportCsvPropertyStr(name,
                                      {valueStr} FormatFloat('0.000', valueFloat),
                                      {var} line);

end; { end CLF_ReportCsvPropertyFloat() }


{***************************************************************************
 * function CLF_ReportCsvPropertyMm()
 *  Report a given coordinate property to the CSV file.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportCsvPropertyMm(    name    : TString;
                                     valueMm : Real;
                                 var line    : TString;
                                     )       : Integer;
var
   valueStr : TString;
   
begin

   { If the coordinate is 0.0, then report it as 0mm. }
   if (valueMm = 0.0) then
      valueStr := '0mm' {nosemi}

   { Else report it with 3 decimal places of precision. }
   { Note:  We don't use CoordUnitToString(valueCoord, eMetric) because that gives
    us only 2 digits to the right of the decimal point precision! }
   else
      valueStr := (FormatFloat('0.000', valueMm) + 'mm');
   
   { Call CLF_ReportCsvPropertyStr() to do all the real work. }
   result := CLF_ReportCsvPropertyStr(name,
                                      valueStr,
                                      {var} line);

end; { end CLF_ReportCsvPropertyMm() }


{***************************************************************************
 * function CLF_ReportCsvPropertyCoord()
 *  Report a given coordinate property to the CSV file.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportCsvPropertyCoord(    name       : TString;
                                        valueCoord : TCoord;
                                    var line       : TString;
                                     )             : Integer;
var
   valueMm  : Real;
   valueStr : TString;
   
begin

   { Convert coordinate to mm. }
   valueMm := CoordToMMs(valueCoord);

   { Call CLF_ReportCsvPropertyMm() to do all the real work. }
   result := CLF_ReportCsvPropertyMm(name,
                                     valueMm,
                                     {var} line);
   
end; { end CLF_ReportCsvPropertyCoord() }


{***************************************************************************
 * function CLF_ReportCsvPropertyTPadMode()
 *  Report a given enumerated property to the CSV file.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportCsvPropertyTPadMode(    name      : TString;
                                           valueEnum : TPadMode;
                                       var line      : TString;
                                           )         : Integer;
var
   modeStr : TString;
   
begin

   { Support valid and supported modes only! }
   case valueEnum of

     ePadMode_Simple        : modeStr := 'Simple';
//     ePadMode_LocalStack    : modeStr := 'LocalStack(??)';
//     ePadMode_ExternalStack : modeStr := 'ExternalStack(??)';
     
   else CLF_Abort('Unknown/unsupported TPadMode value ' + IntToStr(valueEnum));
   end; { endcase }          
   
   { Call CLF_ReportCsvPropertyStr() to do all the real work. }
   result := CLF_ReportCsvPropertyStr(name,
                                      {valueStr} modeStr,
                                      {var} line);

end; { end CLF_ReportCsvPropertyTPadMode() }


{***************************************************************************
 * function CLF_ReportCsvPropertyTExtendedDrillType()
 *  Report a given enumerated property to the CSV file.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportCsvPropertyTExtendedDrillType(    name      : TString;
                                                     valueEnum : TExtendedDrillType;
                                                 var line      : TString;
                                                     )         : Integer;
var
   drillStr : TString;
   
begin

   { Support valid and supported drills only! }
   case valueEnum of

     eDrilledHole : drillStr := 'Drilled';
//ePunchedHole
//eLaserDrilledHole
//ePlasmaDrilledHole 
     
   else CLF_Abort('Unknown/unsupported TExtendedDrillType value ' + IntToStr(valueEnum));
   end; { endcase }          
   
   { Call CLF_ReportCsvPropertyStr() to do all the real work. }
   result := CLF_ReportCsvPropertyStr(name,
                                      {valueStr} drillStr,
                                      {var} line);

end; { end CLF_ReportCsvPropertyTExtendedDrillType() }


{***************************************************************************
 * function CLF_ReportCsvPropertyTExtendedHoleType()
 *  Report a given enumerated property to the CSV file.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportCsvPropertyTExtendedHoleType(    name      : TString;
                                                    valueEnum : TExtendedHoleType;
                                                var line      : TString;
                                                    )         : Integer;
var
   holeStr : TString;
   
begin

   { Support valid and supported holes only! }
   case valueEnum of

     eRoundHole : holeStr := 'Round';
//eSquareHole,
//eSlotHole
     
   else CLF_Abort('Unknown/unsupported TExtendedHoleType value ' + IntToStr(valueEnum));
   end; { endcase }          
   
   { Call CLF_ReportCsvPropertyStr() to do all the real work. }
   result := CLF_ReportCsvPropertyStr(name,
                                      {valueStr} holeStr,
                                      {var} line);

end; { end CLF_ReportCsvPropertyTExtendedHoleType() }


{***************************************************************************
 * function CLF_ReportCsvPropertyTShape()
 *  Report a given enumerated property to the CSV file.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportCsvPropertyTShape(    name      : TString;
                                         valueEnum : TShape;
                                     var line      : TString;
                                         )         : Integer;
var
   shapeStr : TString;
   
begin

   { Support valid and supported shapes only! }
   case valueEnum of

     eRounded     : shapeStr := 'Rounded';
     eRectangular : shapeStr := 'Rectangular';
     eOctagonal   : shapeStr := 'Octagonal';
     eRoundedRectangular: shapeStr := 'RoundedRectangular';

   else CLF_Abort('Unknown/unsupported TShape value ' + IntToStr(valueEnum));
   end; { endcase }          
   
   { Call CLF_ReportCsvPropertyStr() to do all the real work. }
   result := CLF_ReportCsvPropertyStr(name,
                                      {valueStr} shapeStr,
                                      {var} line);

end; { end CLF_ReportCsvPropertyTShape() }


{***************************************************************************
 * function CLF_ReportCsvPropertyTBoardSide()
 *  Report a given enumerated property to the CSV file.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportCsvPropertyTBoardSide(    name      : TString;
                                             valueEnum : TBoardSide;
                                         var line      : TString;
                                             )         : Integer;
var
   sideStr : TString;
   
begin

   { Support valid and supported sides only! }
   case valueEnum of

     eBoardSide_Top    : sideStr := 'Top';
     eBoardSide_Bottom : sideStr := 'Bottom';
     
   else CLF_Abort('Unknown/unsupported TBoardSide value ' + IntToStr(valueEnum));
   end; { endcase }          
   
   { Call CLF_ReportCsvPropertyStr() to do all the real work. }
   result := CLF_ReportCsvPropertyStr(name,
                                      {valueStr} sideStr,
                                      {var} line);

end; { end CLF_ReportCsvPropertyTBoardSide() }


{***************************************************************************
 * function CLF_ReportCsvPropertyTCacheState()
 *  Report a given enumerated property to the CSV file.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportCsvPropertyTCacheState(    name      : TString;
                                              valueEnum : TCacheState;
                                          var line      : TString;
                                              )         : Integer;
var
   shapeStr : TString;
   
begin

   { Support valid and supported shapes only! }
   case valueEnum of

     eCacheInvalid : shapeStr := 'From Rule';
     eCacheValid   : shapeStr := 'From Rule';
     eCacheManual  : shapeStr := 'Manual';
     
   else CLF_Abort('Unknown/unsupported TCacheState value ' + IntToStr(valueEnum));
   end; { endcase }          
   
   { Call CLF_ReportCsvPropertyStr() to do all the real work. }
   result := CLF_ReportCsvPropertyStr(name,
                                      {valueStr} shapeStr,
                                      {var} line);

end; { end CLF_ReportCsvPropertyTCacheState() }


{***************************************************************************
 * function CLF_ReportCsvPropertyBool()
 *  Report a given boolean property to the CSV file.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportCsvPropertyBool(    name      : TString;
                                       valueBool : Boolean;
                                   var line      : TString;
                                       )         : Integer;
var
   valueStr : TString;
begin

   { Report the boolean value as either 'True' or 'False'. }
   if (valueBool) then
      valueStr := 'True' {nosemi}
   else
      valueStr := 'False';

   { Call CLF_ReportCsvPropertyStr() to do all the real work. }
   result := CLF_ReportCsvPropertyStr(name,
                                      valueStr,
                                      {var} line);

end; { end CLF_ReportCsvPropertyBool() }


{***************************************************************************
 * function CLF_CsvReportFieldName()
 *  Report a given field's name to a new line of the CSV file.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CsvReportFieldName(    fieldName : TString;
                                var line      : TString;
                                    )         : Integer;
var
   leftStr   : TString;
   rightStr  : TString;

begin

   { Prepend a comma to the new line. }
   if (line <> '') then
      line := line + ',';

   { Split this fieldName into a left half and a right half, with the left being
    just the numerical prefix we might some day use for sorting purposes. }
   SplitStringIntoLeftAndRight(fieldName, 
                               '_',
                               {var} leftStr,
                               {var} rightStr);

   { Add this field name to line. }
   line := line + rightStr;

end; { end CLF_CsvReportFieldValue() }


{***************************************************************************
 * function CLF_CsvReportFieldValue()
 *  Report a given field's value to a new line of the CSV file.
 *  If the given field was not defined, output a '*' char.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CsvReportFieldValue(    fieldName : TString;
                                     fieldList : TStringList;
                                 var line      : TString;
                                     )         : Integer;
begin

   { Prepend a comma to the new line. }
   if (line <> '') then
      line := line + ',';
   
   { See if the given field name was defined. }
   if (CLF_IsNameInStringList({name} fieldName,
       {stringlist} fieldList)) then

      { It was defined, so output its actual value. }
      line := line + fieldList.Values(fieldName) {nosemi}

   else

      { Else it was not defined, so output placeholder '*' char. }
      line := line + '*';


end; { end CLF_CsvReportFieldValue() }


{***************************************************************************
 * function CLF_CsvReportFileWrite()
 *  Report a given boolean property to the CSV file.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CsvReportFileWrite(    csvReportStrs     : TStringList;                               
                                var csvReportOut      : TStringList;
                                    csvReportFilePath : TString;
                                    )                 : Integer;
var
   i            : Integer;
   line         : TString;
   leftStr      : TString;
   rightStr     : TString;
   fieldList    : TStringList;
   
begin

   { Next, go through each line of the CSV report. }
   for i := 0 to (csvReportStrs.Count - 1) do
   begin

      { Retrieve this line from the stringlist. }
      WriteToDebugFile('csvReportStrs[' + IntToStr(i) + '] is "' + csvReportStrs.Strings(i) + '".')

   end; { endfor }

   { First, explicitly sort the stringlist. }
   csvReportStrs.Sort();

   { Create local output stringlist. }
   csvReportOut := TStringList.Create();
   
   { Next, go through each line of the CSV report. }
   for i := 0 to (csvReportStrs.Count - 1) do
   begin

//      WriteToDebugFile('csvReportStrs[' + IntToStr(i) + '] is "' + csvReportStrs.Strings(i) + '".');

      { Retrieve this line from the stringlist. }
      line := csvReportStrs[i];

      { Split this line into a left half and a right half, with the left being
       just the prefixed string we use for sorting purposes. }
      SplitStringIntoLeftAndRight(line, 
                                  constCsvRptFieldSepChar,
                                  {var} leftStr,
                                  {var} rightStr);

      { Create a stringlist to hold all the fields for a given line. }
      fieldList := TStringList.Create();

      { Now, split the right half into a stringlist of its own, called fieldList. }
      CLF_SplitDelimitedUnquotedStringIntoStringList({delimitedString} rightStr,
                                                     {delimiter} constCsvRptFieldSepChar,
                                                     {var stringList} fieldList);

      {* Output field values. *}
      
      { Switch over and now use "line" as our output line. }
      line := '';
      
      { Next, look for the presence or absence of all predefined field names in the fieldList.
       When this line does not have such a field defined, output '*'.  When it does have
       a field defined, output the actual field value. }

      { General properties }
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldObjectKind, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldIdentifier, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldLayer, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldWidth, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldRotation, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldX1, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldY1, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldX2, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldY2, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldKeepout, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldLocked, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldHide, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldNet, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldComponent, fieldList, {var} line);
      {}
      { Arc related properties }
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldRadius, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldStartAngle, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldEndAngle, fieldList, {var} line);
      {}
      { 3D body-specific parameters }
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldModelType, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldBodyProjection, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldBodyColor, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldBodyOpacity, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldModelRotationX, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldModelRotationY, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldModelRotationZ, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldModelStandoffHeight, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldModelOverallHeight, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldModelRadius, fieldList, {var} line);
      {}
      { Text related properties }
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldString, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldTextHeight, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldTextWidth, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldStrokeFont, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldAutoposition, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldMirror, fieldList, {var} line);
      {}
      { Text related properties -- True Type fonts }
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldTextKind, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldStringType, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldTrueTypeFontName, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldBold, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldItalic, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldInverted, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldInvertedBorderWidth, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldInvertedRectangleWidth, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldInvertedRectangleHeight, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldUseInvertedRectangle, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldInvertedTextJustification, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldInvertedTextOffsetFromInvertedRect, fieldList, {var} line);
      {}
      { Text related properties -- Barcodes }
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldBarCodeFullWidth, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldBarCodeFullHeight, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldBarCodeFullXMargin, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldBarCodeFullYMargin, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldBarCodeMinWidth, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldBarCodeType, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldBarCodeRenderMode, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldBarCodeInverted, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldBarCodeTextFontName, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldBarCodeShowText, fieldList, {var} line);
      {}
      {}
      { Pad related properties }
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldDrillType, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldHoleType, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldHoleSize, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldHoleWidth, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldHoleRotation, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldSolderpasteOverride, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldSolderpasteExpansion, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldSolderpasteExpansionMode, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldSoldermaskOverride, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldSoldermaskExpansion, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldSoldermaskExpansionMode, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldSoldermaskTentingTop, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldSoldermaskTentingBot, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldFabTestPointTop, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldFabTestPointBot, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldAssyTestPointTop, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldAssyTestPointBot, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldElectricalType, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPlated, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadJumperID, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldStackMode, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeAll, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeAll, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeAll, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeTop, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeTop, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeTop, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetTop, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetTop, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadTop, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid1, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid1, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid1, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid1, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid1, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid1, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid2, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid2, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid2, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid2, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid2, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid2, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid3, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid3, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid3, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid3, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid3, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid3, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid4, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid4, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid4, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid4, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid4, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid4, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid5, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid5, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid5, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid5, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid5, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid5, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid6, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid6, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid6, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid6, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid6, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid6, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid7, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid7, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid7, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid7, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid7, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid7, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid8, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid8, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid8, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid8, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid8, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid8, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid9, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid9, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid9, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid9, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid9, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid9, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid10, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid10, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid10, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid10, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid10, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid10, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid11, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid11, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid11, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid11, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid11, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid11, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid12, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid12, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid12, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid12, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid12, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid12, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid13, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid13, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid13, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid13, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid13, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid13, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid14, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid14, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid14, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid14, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid14, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid14, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid15, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid15, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid15, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid15, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid15, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid15, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid16, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid16, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid16, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid16, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid16, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid16, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid17, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid17, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid17, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid17, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid17, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid17, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid18, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid18, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid18, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid18, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid18, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid18, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid19, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid19, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid19, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid19, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid19, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid19, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid20, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid20, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid20, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid20, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid20, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid20, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid21, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid21, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid21, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid21, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid21, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid21, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid22, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid22, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid22, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid22, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid22, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid22, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid23, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid23, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid23, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid23, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid23, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid23, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid24, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid24, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid24, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid24, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid24, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid24, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid25, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid25, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid25, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid25, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid25, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid25, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid26, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid26, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid26, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid26, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid26, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid26, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid27, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid27, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid27, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid27, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid27, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid27, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid28, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid28, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid28, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid28, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid28, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid28, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid29, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid29, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid29, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid29, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid29, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid29, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeMid30, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeMid30, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeMid30, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetMid30, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetMid30, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadMid30, fieldList, {var} line);
      {}
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadShapeBot, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXSizeBot, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYSizeBot, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadXOffsetBot, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadYOffsetBot, fieldList, {var} line);
      CLF_CsvReportFieldValue({fieldName} constCsvRptFieldPadCornerRadBot, fieldList, {var} line);
      
      { Copy output line to output stringlist. }
      csvReportOut.Add(line);

      { Free fieldList stringlist. }
      fieldList.Free();

   end;

   {* Output field names. *}
      
   { Switch over and now use "line" as our output line. }
   line := '';

   { General properties }
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldObjectKind, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldIdentifier, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldLayer, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldWidth, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldRotation, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldX1, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldY1, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldX2, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldY2, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldKeepout, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldLocked, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldHide, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldNet, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldComponent, {var} line);
   {}
   { Arc related properties }
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldRadius, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldStartAngle, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldEndAngle, {var} line);
   {}
   { 3D body-specific parameters }
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldModelType, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldBodyProjection, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldBodyColor, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldBodyOpacity, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldModelRotationX, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldModelRotationY, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldModelRotationZ, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldModelStandoffHeight, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldModelOverallHeight, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldModelRadius, {var} line);
   {}
   { Text related properties }
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldString, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldTextHeight, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldTextWidth, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldStrokeFont, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldAutoposition, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldMirror, {var} line);
   {}
   { Text related properties -- True Type fonts }
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldTextKind, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldStringType, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldTrueTypeFontName, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldBold, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldItalic, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldInverted, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldInvertedBorderWidth, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldInvertedRectangleWidth, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldInvertedRectangleHeight, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldUseInvertedRectangle, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldInvertedTextJustification, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldInvertedTextOffsetFromInvertedRect, {var} line);
   {}
   { Text related properties -- Barcodes }
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldBarCodeFullWidth, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldBarCodeFullHeight, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldBarCodeFullXMargin, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldBarCodeFullYMargin, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldBarCodeMinWidth, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldBarCodeType, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldBarCodeRenderMode, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldBarCodeInverted, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldBarCodeTextFontName, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldBarCodeShowText, {var} line);
   {}
   {}
   { Pad related properties }
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldDrillType, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldHoleType, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldHoleSize, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldHoleWidth, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldHoleRotation, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldSolderpasteOverride, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldSolderpasteExpansion, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldSolderpasteExpansionMode, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldSoldermaskOverride, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldSoldermaskExpansion, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldSoldermaskExpansionMode, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldSoldermaskTentingTop, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldSoldermaskTentingBot, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldFabTestPointTop, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldFabTestPointBot, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldAssyTestPointTop, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldAssyTestPointBot, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldElectricalType, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPlated, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadJumperID, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldStackMode, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeAll, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeAll, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeAll, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeTop, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeTop, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeTop, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetTop, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetTop, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadTop, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid1, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid1, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid1, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid1, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid1, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid1, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid2, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid2, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid2, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid2, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid2, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid2, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid3, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid3, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid3, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid3, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid3, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid3, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid4, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid4, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid4, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid4, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid4, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid4, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid5, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid5, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid5, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid5, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid5, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid5, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid6, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid6, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid6, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid6, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid6, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid6, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid7, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid7, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid7, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid7, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid7, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid7, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid8, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid8, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid8, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid8, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid8, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid8, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid9, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid9, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid9, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid9, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid9, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid9, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid10, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid10, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid10, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid10, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid10, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid10, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid11, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid11, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid11, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid11, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid11, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid11, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid12, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid12, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid12, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid12, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid12, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid12, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid13, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid13, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid13, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid13, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid13, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid13, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid14, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid14, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid14, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid14, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid14, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid14, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid15, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid15, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid15, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid15, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid15, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid15, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid16, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid16, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid16, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid16, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid16, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid16, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid17, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid17, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid17, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid17, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid17, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid17, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid18, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid18, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid18, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid18, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid18, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid18, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid19, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid19, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid19, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid19, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid19, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid19, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid20, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid20, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid20, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid20, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid20, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid20, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid21, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid21, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid21, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid21, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid21, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid21, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid22, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid22, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid22, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid22, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid22, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid22, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid23, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid23, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid23, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid23, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid23, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid23, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid24, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid24, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid24, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid24, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid24, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid24, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid25, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid25, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid25, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid25, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid25, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid25, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid26, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid26, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid26, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid26, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid26, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid26, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid27, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid27, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid27, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid27, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid27, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid27, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid28, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid28, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid28, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid28, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid28, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid28, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid29, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid29, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid29, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid29, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid29, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid29, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeMid30, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeMid30, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeMid30, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetMid30, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetMid30, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadMid30, {var} line);
   {}
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadShapeBot, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXSizeBot, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYSizeBot, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadXOffsetBot, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadYOffsetBot, {var} line);
   CLF_CsvReportFieldName({fieldName} constCsvRptFieldPadCornerRadBot, {var} line);

   { Insert this line with all the field names declared at the start of the file. }
   csvReportOut.Insert(0, line);
   
   { Finally, write the actual csv report to disk. }
   csvReportOut.SaveToFile(csvReportFilePath);

end; { end CLF_CsvReportFileWrite() }


{***************************************************************************
 * function CLF_TranslateColorCode()
 *  Translate 3-letter color code into expanded color word.
 *
 *  *  NOTE:   This entire function is very SPI-specific!
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_TranslateColorCode(    packageColorCode : TString;
                                var packageColorText : TString;
                                    )                : Integer;
begin

   { Assume success. }
   result := 0;

   {* Expand the color code into the English word for that color. *}
   if (packageColorCode = 'Blk') then
      packageColorText := 'Black' {nosemi}

   else if (packageColorCode = 'Gry') then
      packageColorText := 'Gray' {nosemi}

   else if (packageColorCode = 'Org') then
      packageColorText := 'Orange' {nosemi}

   else if (packageColorCode = 'Rst') then
      packageColorText := 'Rust' {nosemi}

   else if (packageColorCode = 'Yel') then
      packageColorText := 'Yellow' {nosemi}

   { Sanity check. }
   else
      CLF_Abort('Unsupported package color code "' + packageColorCode + '"!');
   
end; { end CLF_TranslateColorCode() }


{***************************************************************************
 * function CLF_CalculateLibFileNameSuffixAndDescription()
 *  Create the SPI-specific suffix that we will append to the IPC package code.
 *  This includes a field indicating package color (eg. "Blk" for black).
 *  This includes a field indicating package marking (eg. "BLNK" for blank).
 *  This includes a field indicating IPC footprint compliance ("IPC").
 *  This includes a field indicating source of footprint (eg. "LPW" for Mentor LP Wizard tool).
 *  
 *  Create the description string while we're at it.
 *
 *  NOTE:   This entire function is very SPI-specific!
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CalculateLibFileNameSuffixAndDescription(    hasThVias             : Boolean;
                                                          packageColorCode      : TString;
                                                          packageMarkingMfgName : TString;
                                                          packageMarkingMfgPn   : TString;
                                                          packageMarkingText    : TString;
                                                          cnfGalacticInfo       : TStringList;
                                                      var libFileNameSuffix     : TString;
                                                      var libDescription        : TString;
                                                          )                     : Integer;

var
   mfgName               : TString;
   mfgPkgCode            : TString;
   hasEp                 : Boolean;
   thViasCode            : TString;
   thViasText            : TString;
   packageColorText      : TString;
   packageMarkingCode    : TString;
   footprintStandardCode : TString;
   footprintStandardText : TString;
   footprintSourceCode   : TString;
   footprintSourceText   : TString;
   lpWizardDescription   : TString;
   
begin                                                               

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_CalculateLibFileNameSuffixAndDescription().');

   { Retrieve mfg name and mfg pkg code. }
   mfgName               := cnfGalacticInfo.Values(constGilPrefixLpWizardInfo + constLpWizardManufacturerField);
   mfgPkgCode            := cnfGalacticInfo.Values(constGilPrefixLpWizardInfo + constLpWizardMfgPkgCodeField);
   
   { Sanity checks }
   if (mfgName = '') then
      CLF_Abort('Null manufacturer name in LP Wizard .plb09 file!  This is required.  Please re-run LP Wizard and fill in this info before export.');
   
   if (mfgPkgCode = '') then
      CLF_Abort('Null manufacturer package code in LP Wizard .plb09 file!  This is required.  Please re-run LP Wizard and fill in this info before export.');

   
   { Retrieve whether this package is known to have an exposed pad (EP). }
   hasEp            := StrToBool(cnfGalacticInfo.Values(constGilHasEp));

   { Retrieve LP wizard information from galactic string list. }
   lpWizardDescription  := cnfGalacticInfo.Values(constGilPrefixLpWizardInfo + constLpWizardDescriptionField);

   
   {* Costruct the thermal via code and text. *}
   { If this component has an EP pin, then we must specify that this base model footprint wiil not have
    thermal vias baked into the EP landing pad. }
   { TODO:  Support thermal vias! }
   if (hasEp) then
   begin
      thViasCode := '_NoThVias';
      thViasText := '; No Thermal Vias';
   end

   { Else for a component with no EP pin, we omit these fields completely. }
   else
   begin
      thViasCode := '';
      thViasText := '';
   end; { endelse }

   
   {* Expand the color code into the English word for that color. *}
   CLF_TranslateColorCode(packageColorCode,
                          {var} packageColorText);
   

   {* Construct the package marking code and text. *}
   if ( (packageMarkingMfgName = '') or (packageMarkingMfgPn = '') ) then
   begin

      {* Construct the package marking code and text. *}
      if (packageMarkingText = '') then
      begin
         packageMarkingCode := 'BLNK';
         packageMarkingText := 'No marking';
      end
      
      else
      begin
         packageMarkingCode := 'Marked_'    + packageMarkingText;
         packageMarkingText := 'Marked as ' + packageMarkingText;
      end; { endelse }

   end

   else
   begin
      packageMarkingCode := 'Marked_'     + packageMarkingMfgName + '_' + packageMarkingMfgPn;
      packageMarkingText := 'Marked for ' + packageMarkingMfgName + ' ' + packageMarkingMfgPn;
   end; { endelse }


   {* Construct the footprint standard code and text. *}
   footprintStandardCode := 'IPC';
   footprintStandardText := 'IPC Compliant';

   {* Construct the footprint footprint source code and text. *}
   footprintSourceCode   := 'LPW';
   footprintSourceText   := 'Footprint adapted from Mentor LP Wizard tool';

   
   {** Construct the library file name suffix. **}

   { Suffix must be "mfgName_mfgPkgCode[_NoThVias]_Blk_BLNK_IPC_LPW". }
   libFileNameSuffix := '_' +                     mfgName + '_' + mfgPkgCode + thViasCode + '_' + packageColorCode + '_' + packageMarkingCode + '_' + footprintStandardCode + '_' + footprintSourceCode;
   WriteToDebugFile(' libFileNameSuffix is "' + libFileNameSuffix + '".');
   

   {** Create library part description field. **}
   libDescription := lpWizardDescription + '; ' + mfgName + '_' + mfgPkgCode + thViasText + '; ' + packageColorText + '; ' + packageMarkingText + '; ' + footprintStandardText + '; ' + footprintSourceText;
   WriteToDebugFile(' libDescription is "' + libDescription + '".');
   
end; { end CLF_CalculateLibFileNameSuffixAndDescription() }

{***************************************************************************
 * END Support functions.
 ***************************************************************************}

{***************************************************************************
 * BEGIN Import related functions.
 ***************************************************************************}

{***************************************************************************
 * function CLF_ChoosePadsFile()
 *  Find the PADS .asc file that has just been exported from Mentor LP Wizard.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ChoosePadsFile(    lpWizardFilesPath : TDynamicString;
                            var cnfGalacticInfo   : TStringList;
                            var padsFilePath      : TString;
                                )                 : Integer;

var
   i                : Integer;
   resultFromDialog : Boolean;
   pathFromDialog   : TString;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_ChoosePadsFile()');
   CLF_UpdateGuiStatusMessage('Please choose PADS .asc file to import into Altium, cleanup, and add SPI-specific features to.');

   { Ask the user to identify the PADS .asc file that was exported from Mentor LP Wizard. }
   ResetParameters;
   AddStringParameter('Dialog','FileOpenSave');
   AddStringParameter('Mode', '0'); { OpenFile dialog }
   AddStringParameter('Path', (lpWizardFilesPath + '\*.asc'));
   AddStringParameter('Prompt', 'Choose the PADS .asc file that you just exported from Mentor LP Wizard.  Click "Open".  Then click "Next" 8 times.  Finally click "Finish".');
   AddStringParameter('FileType1', 'PADS PCB file (*.asc)|*.asc');
   
   RunProcess('Client:RunCommonDialog');

   { Get parameters back from call to RunCommonDialog. }
   pathFromDialog   := '';
   GetStringParameter('Result', resultFromDialog);
   GetStringParameter('Path', pathFromDialog);
   
   WriteToDebugFile(' Result is ' + BoolToStr(resultFromDialog) + '.');
   WriteToDebugFile(' Path is ' + pathFromDialog + '.');

   { Sanity check }
   if (resultFromDialog = False) then
      CLF_Abort('Did not get name of PADS .asc file from user dialog!');

   { Return file name with full path to caller. }
   padsFilePath := pathFromDialog;
   CLF_WriteToSummaryAndDebugFilesWithStepNum('User chose PADS .asc file "' + padsFilePath + '".');
   
end; { end CLF_ChoosePadsFile() }


{***************************************************************************
 * function CLF_ExtractLpWizardPackageDimensions()
 *  Extract package dimensions from information we have already gotten from
 *  Mentor LP Wizard .plb09 file.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ExtractLpWizardPackageDimensions(var cnfGalacticInfo : TStringList;
                                              var hasEp           : Boolean;
                                                  )               : Integer;

var
   i                      : Integer;
   k                      : Integer;
   rc                     : Integer;
   lpWizardPackageDimsStr : TString;
   lpWizardPackageDims    : TStringList;
   footprintType          : TString;
   pkgDimsPins            : Integer;
   pkgDimsPitch           : Real;
   pkgDimsStandoffMin     : Real;
   pkgDimsHeightMax       : Real;
   pkgDimsEpWidthMax      : Real;
   pkgDimsEpLengthMax     : Real;
   pkgDimsEpChamfer       : Real;
   pkgDimsEpCornerRad     : Real;
   pkgDimsTotalWidthMin   : Real;
   pkgDimsTotalWidthMax   : Real;
   pkgDimsTotalLengthMin  : Real;
   pkgDimsTotalLengthMax  : Real;
   pkgDimsBodyWidthMin    : Real;
   pkgDimsBodyWidthMax    : Real;
   pkgDimsBodyLengthMin   : Real;
   pkgDimsBodyLengthMax   : Real;
   pkgDimsPinWidthMin     : Real;
   pkgDimsPinWidthMax     : Real;
   pkgDimsPinLandMin      : Real;
   pkgDimsPinLandMax      : Real;
   pkgDimsBallDiamNom     : Real;
   pkgDimsPinsWestEast    : Integer;
   pkgDimsPinsNorthSouth  : Integer;
   junkInt                : Real;
   junkStr                : TString;

begin

   { Assume success. }
   result := 0;

   { Initialize all variables to 0 in case we don't find them all later on. }
   pkgDimsPins            := 0;
   pkgDimsPitch           := 0;
   pkgDimsStandoffMin     := 0;
   pkgDimsHeightMax       := 0;
   pkgDimsEpWidthMax      := 0;
   pkgDimsEpLengthMax     := 0;
   pkgDimsEpChamfer       := 0;
   pkgDimsEpCornerRad     := 0;
   pkgDimsTotalWidthMin   := 0;
   pkgDimsTotalWidthMax   := 0;
   pkgDimsTotalLengthMin  := 0;
   pkgDimsTotalLengthMax  := 0;
   pkgDimsBodyWidthMin    := 0;
   pkgDimsBodyWidthMax    := 0;
   pkgDimsBodyLengthMin   := 0;
   pkgDimsBodyLengthMax   := 0;
   pkgDimsPinWidthMin     := 0;
   pkgDimsPinWidthMax     := 0;
   pkgDimsPinLandMin      := 0;
   pkgDimsPinLandMax      := 0;
   pkgDimsBallDiamNom     := 0;
   pkgDimsPinsWestEast    := 0;
   pkgDimsPinsNorthSouth  := 0;
   hasEp                  := False;

   { Retrieve footprint type. }
   footprintType         := cnfGalacticInfo.Values(constGilFootprintType);
   WriteToDebugFile(' Footprint type is "' + footprintType + '".');

   { Tell user what we're doing. }
   WriteToDebugFile('Hello from CLF_ExtractLpWizardPackageDimensions()');
   CLF_UpdateGuiStatusMessage('Proceeding to import package dimensions from Mentor LP Wizard .plb09 file.');

   { Create string list to hold package dimensions. }
   lpWizardPackageDims := TStringList.Create();

   { Retrieve package dimensions field. }
   lpWizardPackageDimsStr := cnfGalacticInfo.Values(constGilPrefixLpWizardInfo + constLpWizardDimensionsField);
      
   { Split package dimensions field into sub-fields for our use here. }
   CLF_SplitDelimitedUnquotedStringIntoStringList({delimitedString} lpWizardPackageDimsStr, 
                                                  {delimiter} constLpWizardDimsFieldSepChar,
                                                  {var stringList} lpWizardPackageDims);
   
   { Loop over all the sub-fields. }
   for i := 0 to (lpWizardPackageDims.Count - 1) do
   begin

      WriteToDebugFile(' lpWizardPackageDims[' + IntToStr(i) + '] is "' + lpWizardPackageDims[i] + '".');

   end; { endfor }

   { Extract package dimensions for SOIC and SOT and SOP type packages. }
   if ( (footprintType = 'SOIC') or (footprintType = 'SOT') or (footprintType = 'SOP') ) then
   begin

      { Ignore first few fields. }
      i := 0;
      junkStr                := ((lpWizardPackageDims[CLF_VarPlusPlus(i)]));                      { 0 }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 1 }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 2 }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 3 }
      
      junkStr                := ((lpWizardPackageDims[CLF_VarPlusPlus(i)]));                      { 4 }
      pkgDimsPitch           := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 5 }

      { This field appears to be the base number of pins in the footprint, before any were removed. }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 6 }

      { Note:  For a footprint with a pin removed, field 7 is the only valid one telling us the actual # of pins! }
      pkgDimsPins            := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 7 }

      pkgDimsTotalWidthMin   := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 8 }
      pkgDimsTotalWidthMax   := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 9 }
      pkgDimsPinLandMin      := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 10 }
      pkgDimsPinLandMax      := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 11 }
      pkgDimsPinWidthMin     := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 12 }
      pkgDimsPinWidthMax     := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 13 }

      { SOIC & SOP have 2 fields for EP size. }
      if ( (footprintType = 'SOIC') or (footprintType = 'SOP') ) then
      begin
         
         { TODO:  These may be backwards!  Verify with non-square EP! }
         pkgDimsEpLengthMax  := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { SOIC/SOP=14 }
         pkgDimsEpWidthMax   := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { SOIC/SOP=15 }

      end;

      pkgDimsBodyWidthMin    := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { SOIC/SOP=16, SOT=14 }
      pkgDimsBodyWidthMax    := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { SOIC/SOP=17, SOT=15 }
      pkgDimsBodyLengthMin   := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { SOIC/SOP=18, SOT=16 }
      pkgDimsBodyLengthMax   := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { SOIC/SOP=19, SOT=17 }
      pkgDimsHeightMax       := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { SOIC/SOP=20, SOT=18 }
      pkgDimsStandoffMin     := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { SOIC/SOP=21, SOT=19 }
         
   end { endif }

   { Extract package dimensions for QFN type packages. }
   else if ( (footprintType = 'QFN') or (footprintType = 'DFN') ) then
   begin

      { Ignore first few fields. }
      i := 0;
      junkStr                := ((lpWizardPackageDims[CLF_VarPlusPlus(i)]));                      { 0 }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 1 }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 2 }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 3 }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 4 }
      
      pkgDimsPins            := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 5 }
      pkgDimsPitch           := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 6 }

      { TODO:  These may be backwards!  Verify with non-square part! }
      pkgDimsPinsWestEast    := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 7 }
      pkgDimsPinsNorthSouth  := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 8 }

      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 9 }
      
      if (footprintType = 'QFN') then
      begin
         junkInt             := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { QFN=10 }
      end;
      
      pkgDimsPinLandMin      := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { QFN=11, DFN=10 }
      pkgDimsPinLandMax      := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { QFN=12, DFN=11 }

      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { QFN=13, DFN=12 }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { QFN=14, DFN=13 }

      pkgDimsPinWidthMin     := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { QFN=15, DFN=14 }
      pkgDimsPinWidthMax     := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { QFN=16, DFN=15 }

      pkgDimsEpLengthMax     := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { QFN=17, DFN=16 }
      pkgDimsEpWidthMax      := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { QFN=18, DFN=17 }
      pkgDimsEpChamfer       := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { QFN=19, DFN=18 }
      pkgDimsEpCornerRad     := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { QFN=20, DFN=19 }

      pkgDimsTotalWidthMin   := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { QFN=21, DFN=20 }
      pkgDimsTotalWidthMax   := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { QFN=22, DFN=21 }
      pkgDimsTotalLengthMin  := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { QFN=23, DFN=22 }
      pkgDimsTotalLengthMax  := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { QFN=24, DFN=23 }
      
      pkgDimsHeightMax       := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { QFN=25, DFN=24 }

      { Set standoff to 0.001mm for purposes of creating simple extruded 3D drawings. }
      pkgDimsStandoffMin     := constCompBodyMarkHeight;

      { For QFN/DFN packages, body width and body length will be 0.
       For our purposes here, set them to something sane. }
      if (pkgDimsBodyWidthMin = 0.0) then
         pkgDimsBodyWidthMin := pkgDimsTotalWidthMin;

      if (pkgDimsBodyWidthMax = 0.0) then
         pkgDimsBodyWidthMax := pkgDimsTotalWidthMax;

      if (pkgDimsBodyLengthMin = 0.0) then
         pkgDimsBodyLengthMin := pkgDimsTotalLengthMin;

      if (pkgDimsBodyLengthMax = 0.0) then
         pkgDimsBodyLengthMax := pkgDimsTotalLengthMax;

      if ( pkgDimsEpChamfer <> 0.0 ) then
         WriteToDebugFile('EP chamfer parameter for QFN/DFN is not zero.');

   end { end elsif }

   { Extract package dimensions for BGA type packages. }
   else if (footprintType = 'BGA') then
   begin

      { Ignore first few fields. }
      i := 0;
      junkStr                := ((lpWizardPackageDims[CLF_VarPlusPlus(i)]));                      { 0 }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 1 }
      junkStr                := ((lpWizardPackageDims[CLF_VarPlusPlus(i)]));                      { 2 }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 3 }

      { This is actually row pitch.  For now, we don't support differing row vs. column pitches. }
      pkgDimsPitch           := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 4 }

      { Number of rows.  We don't currently do anything with this, so ignore it. }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 5 }

      { This is actually col pitch.  For now, we don't support differing row vs. column pitches. }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 6 }

      { Number of cols.  We don't currently do anything with this, so ignore it. }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 7 }

      pkgDimsPins            := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 8 }
      pkgDimsBallDiamNom     := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 9 }

      { FIXME:  2 of these are probably EP dimensions! }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 10 }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 11 }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 12 }

      pkgDimsTotalWidthMin   := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 13 }
      pkgDimsTotalWidthMax   := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 14 }
      pkgDimsTotalLengthMin  := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 15 }
      pkgDimsTotalLengthMax  := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 16 }
      
      pkgDimsHeightMax       := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 17 }


      { For purposes of this script (eg. extruding simple 3D body), we will consider the standoff height
       to be the ball diameter. }
      pkgDimsStandoffMin     := pkgDimsBallDiamNom;
      
   end { end elsif }

   { Extract package dimensions for QFP type packages. }
   else if (footprintType = 'QFP') then
   begin

      { Ignore first few fields. }
      i := 0;
      junkStr                := ((lpWizardPackageDims[CLF_VarPlusPlus(i)]));                      { 0 }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 1 }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 2 }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 3 }

      pkgDimsPins            := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 4 }

      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 5 }

      pkgDimsPitch           := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 6 }

      { TODO:  These may be backwards!  Verify with non-square part! }
      pkgDimsPinsWestEast    := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 7 }
      pkgDimsPinsNorthSouth  := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 8 }

      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 9 }

      pkgDimsTotalWidthMin   := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 10 }
      pkgDimsTotalWidthMax   := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 11 }
      pkgDimsTotalLengthMin  := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 12 }
      pkgDimsTotalLengthMax  := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 13 }
      
      pkgDimsPinLandMin      := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 14 }
      pkgDimsPinLandMax      := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 15 }
      pkgDimsPinWidthMin     := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 16 }
      pkgDimsPinWidthMax     := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 17 }

      { TODO:  These may be backwards!  Verify with non-square EP! }
      pkgDimsEpLengthMax     := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 18 }
      pkgDimsEpWidthMax      := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 19 }
      
      pkgDimsBodyWidthMin    := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 20 }
      pkgDimsBodyWidthMax    := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 21 }
      pkgDimsBodyLengthMin   := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 22 }
      pkgDimsBodyLengthMax   := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 23 }
      
      pkgDimsHeightMax       := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 24 }
      pkgDimsStandoffMin     := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 25 }
      
   end { end elsif }

   { Extract package dimensions for CHP type packages. }
   { TODO:  We should probably also check chip vs. molded package types. }
   { Currently we only really support chip inductors and molded capacitors. }
   else if (footprintType = 'Inductor') then
   begin

      { Setup that we have 2 pins. }
      pkgDimsPins            := 2;
      
      { Ignore first few fields. }
      i := 0;
      junkStr                := ((lpWizardPackageDims[CLF_VarPlusPlus(i)]));                      { 0 } { "CHP" }
      junkStr                := ((lpWizardPackageDims[CLF_VarPlusPlus(i)]));                      { 1 } { "INDC" }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 2 }

      pkgDimsTotalLengthMin  := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 3 }
      pkgDimsTotalLengthMax  := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 4 }

      { Termination length }
      pkgDimsPinLandMin      := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 5 }
      pkgDimsPinLandMax      := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 6 }

      { TODO:  Check that the termination length is the same for both sides of chip! }
      pkgDimsPinLandMin      := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 7 }
      pkgDimsPinLandMax      := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 8 }

      pkgDimsTotalWidthMin   := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 9 }
      pkgDimsTotalWidthMax   := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 10 }

      pkgDimsHeightMax       := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 11 }

      { Set the pin width to be the same as the total width. }
      pkgDimsPinWidthMin     := pkgDimsTotalWidthMin;
      pkgDimsPinWidthMax     := pkgDimsTotalWidthMax;
      pkgDimsBodyWidthMin    := pkgDimsTotalWidthMin;
      pkgDimsBodyWidthMax    := pkgDimsTotalWidthMax;
      pkgDimsBodyLengthMin   := pkgDimsTotalLengthMin - (2 * pkgDimsPinLandMin);
      pkgDimsBodyLengthMax   := pkgDimsTotalLengthMax - (2 * pkgDimsPinLandMax);
      
   end { end elsif }

   { Extract package dimensions for MLD type packages. }
   { TODO:  We should probably also check chip vs. molded package types. }
   { Currently we only really support chip inductors and molded capacitors. }
   else if (footprintType = 'Capacitor') then
   begin

      { Setup that we have 2 pins. }
      pkgDimsPins            := 2;
      
      { Ignore first few fields. }
      i := 0;
      junkStr                := ((lpWizardPackageDims[CLF_VarPlusPlus(i)]));                      { 0 } { "MLD" }
      junkStr                := ((lpWizardPackageDims[CLF_VarPlusPlus(i)]));                      { 1 } { "CAPMP" }
      junkInt                := (StrToInt(lpWizardPackageDims[CLF_VarPlusPlus(i)]));              { 2 }

      pkgDimsTotalLengthMin  := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 3 }
      pkgDimsTotalLengthMax  := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 4 }

      { Termination length }
      pkgDimsPinLandMin      := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 5 }
      pkgDimsPinLandMax      := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 6 }

      { Termination width }
      pkgDimsPinWidthMin     := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 7 }
      pkgDimsPinWidthMax     := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 8 }

      { TODO:  Check that the termination length is the same for both sides of chip! }
      pkgDimsPinLandMin      := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 9 }
      pkgDimsPinLandMax      := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 10 }

      { TODO:  Check that the termination width is the same for both sides of chip! }
      pkgDimsPinWidthMin     := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 11 }
      pkgDimsPinWidthMax     := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 12 }

      { Body (total) width }
      pkgDimsTotalWidthMin   := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 9 }
      pkgDimsTotalWidthMax   := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 10 }

      pkgDimsHeightMax       := (StrToFloat(lpWizardPackageDims[CLF_VarPlusPlus(i)]) / 100000.0); { 11 }

      { Set the body width to be the same as the total width. }
      pkgDimsBodyWidthMin    := pkgDimsTotalWidthMin;
      pkgDimsBodyWidthMax    := pkgDimsTotalWidthMax;
      pkgDimsBodyLengthMin   := pkgDimsTotalLengthMin - (2 * pkgDimsPinLandMin);
      pkgDimsBodyLengthMax   := pkgDimsTotalLengthMax - (2 * pkgDimsPinLandMax);

      { Set the "standoff height" to be the thickness of a pin. }
      pkgDimsStandoffMin     := constCompBodyPinThicknessMoldMm;

      
   end; { end elsif }

   { Determine if this part has an exposed pad (EP). }
   if ( (pkgDimsEpWidthMax > 0.0) and (pkgDimsEpLengthMax > 0.0) ) then
      hasEp := True;

   { Add this to galactic string list. }
   cnfGalacticInfo.add(constGilHasEp + constStringEquals + BoolToStr(hasEp));


   WriteToDebugFile(' pkgDimsPins is "' + IntToStr(pkgDimsPins) + '".');
   WriteToDebugFile(' pkgDimsPitch is "' + FloatToStr(pkgDimsPitch) + '".');
   WriteToDebugFile(' pkgDimsStandoffMin is "' + FloatToStr(pkgDimsStandoffMin) + '".');
   WriteToDebugFile(' pkgDimsHeightMax is "' + FloatToStr(pkgDimsHeightMax) + '".');
   WriteToDebugFile(' pkgDimsEpWidthMax is "' + FloatToStr(pkgDimsEpWidthMax) + '".');
   WriteToDebugFile(' pkgDimsEpLengthMax is "' + FloatToStr(pkgDimsEpLengthMax) + '".');
   WriteToDebugFile(' pkgDimsEpChamfer is "' + FloatToStr(pkgDimsEpChamfer) + '".');
   WriteToDebugFile(' pkgDimsEpCornerRad is "' + FloatToStr(pkgDimsEpCornerRad) + '".');
   WriteToDebugFile(' pkgDimsTotalWidthMin is "' + FloatToStr(pkgDimsTotalWidthMin) + '".');
   WriteToDebugFile(' pkgDimsTotalWidthMax is "' + FloatToStr(pkgDimsTotalWidthMax) + '".');
   WriteToDebugFile(' pkgDimsTotalLengthMin is "' + FloatToStr(pkgDimsTotalLengthMin) + '".');
   WriteToDebugFile(' pkgDimsTotalLengthMax is "' + FloatToStr(pkgDimsTotalLengthMax) + '".');
   WriteToDebugFile(' pkgDimsBodyWidthMin is "' + FloatToStr(pkgDimsBodyWidthMin) + '".');
   WriteToDebugFile(' pkgDimsBodyWidthMax is "' + FloatToStr(pkgDimsBodyWidthMax) + '".');
   WriteToDebugFile(' pkgDimsBodyLengthMin is "' + FloatToStr(pkgDimsBodyLengthMin) + '".');
   WriteToDebugFile(' pkgDimsBodyLengthMax is "' + FloatToStr(pkgDimsBodyLengthMax) + '".');
   WriteToDebugFile(' pkgDimsPinWidthMin is "' + FloatToStr(pkgDimsPinWidthMin) + '".');
   WriteToDebugFile(' pkgDimsPinWidthMax is "' + FloatToStr(pkgDimsPinWidthMax) + '".');
   WriteToDebugFile(' pkgDimsPinLandMin is "' + FloatToStr(pkgDimsPinLandMin) + '".');
   WriteToDebugFile(' pkgDimsPinLandMax is "' + FloatToStr(pkgDimsPinLandMax) + '".');
   WriteToDebugFile(' pkgDimsBallDiamNom is "' + FloatToStr(pkgDimsBallDiamNom) + '".');

   { Add these fields to galactic string list. }
   cnfGalacticInfo.add(constGilPkgDimsPins + constStringEquals + IntToStr(pkgDimsPins));
   cnfGalacticInfo.add(constGilPkgDimsPitch + constStringEquals + FloatToStr(pkgDimsPitch));
   cnfGalacticInfo.add(constGilPkgDimsStandoffMin + constStringEquals + FloatToStr(pkgDimsStandoffMin));
   cnfGalacticInfo.add(constGilPkgDimsHeightMax + constStringEquals + FloatToStr(pkgDimsHeightMax));
   cnfGalacticInfo.add(constGilPkgDimsEpWidthMax + constStringEquals + FloatToStr(pkgDimsEpWidthMax));
   cnfGalacticInfo.add(constGilPkgDimsEpLengthMax + constStringEquals + FloatToStr(pkgDimsEpLengthMax));
   cnfGalacticInfo.add(constGilPkgDimsEpChamfer + constStringEquals + FloatToStr(pkgDimsEpChamfer));
   cnfGalacticInfo.add(constGilPkgDimsEpCornerRad + constStringEquals + FloatToStr(pkgDimsEpCornerRad));
   cnfGalacticInfo.add(constGilPkgDimsTotalWidthMin + constStringEquals + FloatToStr(pkgDimsTotalWidthMin));
   cnfGalacticInfo.add(constGilPkgDimsTotalWidthMax + constStringEquals + FloatToStr(pkgDimsTotalWidthMax));
   cnfGalacticInfo.add(constGilPkgDimsTotalLengthMin + constStringEquals + FloatToStr(pkgDimsTotalLengthMin));
   cnfGalacticInfo.add(constGilPkgDimsTotalLengthMax + constStringEquals + FloatToStr(pkgDimsTotalLengthMax));
   cnfGalacticInfo.add(constGilPkgDimsBodyWidthMin + constStringEquals + FloatToStr(pkgDimsBodyWidthMin));
   cnfGalacticInfo.add(constGilPkgDimsBodyWidthMax + constStringEquals + FloatToStr(pkgDimsBodyWidthMax));
   cnfGalacticInfo.add(constGilPkgDimsBodyLengthMin + constStringEquals + FloatToStr(pkgDimsBodyLengthMin));
   cnfGalacticInfo.add(constGilPkgDimsBodyLengthMax + constStringEquals + FloatToStr(pkgDimsBodyLengthMax));
   cnfGalacticInfo.add(constGilPkgDimsPinWidthMin + constStringEquals + FloatToStr(pkgDimsPinWidthMin));
   cnfGalacticInfo.add(constGilPkgDimsPinWidthMax + constStringEquals + FloatToStr(pkgDimsPinWidthMax));
   cnfGalacticInfo.add(constGilPkgDimsPinLandMin + constStringEquals + FloatToStr(pkgDimsPinLandMin));
   cnfGalacticInfo.add(constGilPkgDimsPinLandMax + constStringEquals + FloatToStr(pkgDimsPinLandMax));
   cnfGalacticInfo.add(constGilPkgDimsBallDiamNom + constStringEquals + FloatToStr(pkgDimsBallDiamNom));

   { Free string lists. }
   lpWizardPackageDims.Free;
   
end; { end CLF_ExtractLpWizardPackageDimensions() }


{***************************************************************************
 * function CLF_ExtractLpWizardBuildInfo()
 *  Extract build information (program settings, etc.) from the Mentor LP Wizard .plb09 file.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ExtractLpWizardBuildInfo(var cnfGalacticInfo : TStringList;
                                          )               : Integer;

var
   i                       : Integer;
   k                       : Integer;
   rc                      : Integer;
   lpWizardBuildInfoStr    : TString;
   lpwBuildInfoSections    : TStringList;
   lpwBuildInfoSectionName : Tstring;
   lpwBuildInfoSectionStr  : Tstring;
   lpwBuildInfoSectionInfo : TStringList;
   leftStr                 : TString;
   rightStr                : TString;
   hasDshapePads           : Boolean;
                           
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_ExtractLpWizardBuildInfo()');
   CLF_UpdateGuiStatusMessage('Proceeding to import build info from Mentor LP Wizard .plb09 file.');

   { Create string lists. }
   lpwBuildInfoSections := TStringList.Create;

   { Retrieve build info field. }
   lpWizardBuildInfoStr := cnfGalacticInfo.Values(constGilPrefixLpWizardInfo + constLpWizardBuildInfoField);

   { Split the string apart using ">" as a separator. }
   CLF_SplitDelimitedUnquotedStringIntoStringList({delimitedString} lpWizardBuildInfoStr,
                                                  {delimiter} '>',
                                                  {var stringList} lpwBuildInfoSections);
      
   
   { Loop over all the sections. }
   for i := 0 to (lpwBuildInfoSections.Count - 1) do
   begin

      WriteToDebugFile(' lpwBuildInfoSections[' + IntToStr(i) + '] is "' + lpwBuildInfoSections[i] + '".');

      { Extract the section name by taking everything to the left of the first space (' ') char. }
      rc := SplitStringIntoLeftAndRight({splitMe} lpwBuildInfoSections[i],
                                        {delimiter} ' ',
                                        {var} leftStr,
                                        {var} rightStr);

      { Remove the leading '<' char from the section name. }
      lpwBuildInfoSectionName := StringReplace(leftStr, '<', '', 0);
      WriteToDebugFile(' lpwBuildInfoSectionName is "' + lpwBuildInfoSectionName + '".');

      { Remove all double quote ('"') chars and trailing slash ('/') chars from section string. }
      lpwBuildInfoSectionStr := CLF_StripTrailingChar(StringReplace(rightStr, '"', '', MkSet(rfReplaceAll)),'/');
      WriteToDebugFile(' lpwBuildInfoSectionStr is "' + lpwBuildInfoSectionStr + '".');
      
      { See if this is a section that we care to store.
       Currently we're only interested in the preferences sections ("*_Pf") and the "Land" section.
       The padstacks "p_stks" section is of no use to us.  Plus, it's nested within the "Land" section,
       so we'd have to improve this code in order to read it anyway. }
      if ( (lpwBuildInfoSectionName = constLpWizardBuildInfoSectionStdPrefs) or
          (lpwBuildInfoSectionName = constLpWizardBuildInfoSectionGlobPrefs) or
          (lpwBuildInfoSectionName = constLpWizardBuildInfoSectionRulePrefs) or
          (lpwBuildInfoSectionName = constLpWizardBuildInfoSectionDraftPrefs) or
          (lpwBuildInfoSectionName = constLpWizardBuildInfoSectionGenInfo) or
          (lpwBuildInfoSectionName = constLpWizardBuildInfoSectionPadShape)) then
      begin

         { Create section info string list. }
         lpwBuildInfoSectionInfo := TStringList.Create;

         { Extract all subfields within this section. }
         CLF_SplitDelimitedUnquotedStringIntoStringList({delimitedString} lpwBuildInfoSectionStr,
                                                        {delimiter} ' ',
                                                        {var stringList} lpwBuildInfoSectionInfo);
         
         { Loop over all the subfields within this section. }
         for k := 0 to (lpwBuildInfoSectionInfo.Count - 1) do
         begin

//            WriteToDebugFile(' lpwBuildInfoSectionInfo[' + IntToStr(k) + '] is "' + lpwBuildInfoSectionInfo[k] + '".');
            
            { Add this name/value pair to the galactic info list, with a prefix on the name. }
            { Note:  Here we rely on the fact that the LP Wizard Build Info already has a '=' between name and value.
             So all we have to do is prepend LpWizardBuildInfo prefix and section name and we're good to go. }
            cnfGalacticInfo.add(constGilPrefixLpWizardBuildInfo + lpwBuildInfoSectionName + '_' + lpwBuildInfoSectionInfo[k]);
            WriteToDebugFile(' Added string "' + cnfGalacticInfo[cnfGalacticInfo.Count-1] + '" to galactic string list.');
                      
         end; { endfor k }
            
         { Free section info string list. }
         lpwBuildInfoSectionInfo.Free;

      end; { endif }

   end; { endfor i }
   
   { If constLpWizardBuildInfoPadShape has a value of 'd', then the footprint has D-shaped pads. Add to cnfGalacticInfo. }
   hasDshapePads :=  (cnfGalacticInfo.Values(constLpWizardBuildInfoPadShape) = 'd');
   cnfGalacticInfo.add(constGilPkgDimsHasDshapePads + constStringEquals + BoolToStr(hasDshapePads));

   { Free string lists. }
   lpwBuildInfoSections.Free;
   
end; { end CLF_ExtractLpWizardBuildInfo() }


{***************************************************************************
 * function CLF_CheckLpWizardParameter()
 *  Check the value of a particular field from the Mentor LP Wizard .plb09 file.
 *  If it doesn't match the given required value, abort script!
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CheckLpWizardParameter(fieldName       : TString;
                                    requiredValue   : TString;
                                    fieldDesc       : TString;
                                    cnfGalacticInfo : TStringList;
                                    )               : Integer;

var
   i           : Integer;
   actualValue : TString;
                           
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('In CLF_CheckLpWizardParameter(), fieldName is "' + fieldName + '".');

   { Sanity check. }
   if (cnfGalacticInfo.IndexOfName(fieldName) < 0) then
      CLF_Abort('Unable to find required parameter "' + fieldName + '", "' + fieldDesc + '" in galactic string list!');
   
   { Retrieve value from LP Wizard build info. }
   actualValue := cnfGalacticInfo.Values(fieldName);
   WriteToDebugFile(' requiredValue is "' + requiredValue + '", actualValue is "' + actualValue + '".');

   { Make sure it matches the required value. }
   if (actualValue <> requiredValue) then
      CLF_Abort('This script requires LP Wizard ' + fieldDesc + ' parameter to have been set to ' + requiredValue + ', not ' + actualValue + '!' + constLineBreak +
                'Please fix this in LP Wizard and re-export the .asc and .plb09 files.  Then re-run this script.');
   
end; { end CLF_CheckLpWizardParameter() }


{***************************************************************************
 * function CLF_ExtractInfoFromLpWizardFile()
 *  Extract information from the Mentor LP Wizard .plb09 file, which was the
 *  original source of information for the export to pads .asc file and the
 *  imported .PcbDoc file that we will process.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ExtractInfoFromLpWizardFile(    padsFilePath    : TString;
                                         var cnfGalacticInfo : TStringList;
                                             )               : Integer;

var
   i                   : Integer;
   k                   : Integer;
   rc                  : Integer;
   lpwPlb09FilePath    : Tstring;
   lpwPlb09FileStrs    : TStringList;
   lpwPlb09FileLines   : Integer;
   lpwPlb09FieldNames  : TStringList;
   lpwPlb09FieldValues : TStringList;
   lpwPlb09Description : TString;
   leftStr             : TString;
   rightStr            : TString;
   footprintType       : TString;
   libFileNameSuffix   : TString;
   libFileName         : TString;
   libDescription      : TString;
   padsFileBaseName    : TString;
   padsFilePathOnly    : TString;
   hasEp               : Boolean;
   packageColorCode    : TString;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_ExtractInfoFromLpWizardFile()');
   CLF_UpdateGuiStatusMessage('Proceeding to extract all information from Mentor LP Wizard .plb09 file.');

   { Extract just the base name of the pads file, with no path and no extension. }
   padsFileBaseName := ChangeFileExt(ExtractFileName(padsFilePath), '');
   padsFilePathOnly := ExtractFilePath(padsFilePath);
   WriteToDebugFile(' padsFilePath is "' + padsFilePath + '".');
   WriteToDebugFile(' padsFileBaseName is "' + padsFileBaseName + '".');
   
   { Construct name of LP Wizard .plb09 library file. }
   lpwPlb09FilePath := ChangeFileExt(padsFilePath, constExtLpWizardLibrary);
   WriteToDebugFile(' lpwPlb09FilePath is "' + lpwPlb09FilePath + '".');

   { Verify that PADS .asc file and Mentor LP Wizard .plb09 file both exist. }
   if (not FileExists(padsFilePath)) then
      CLF_Abort('Could not find PADS .asc source file "' + padsFilePath + '"!');
   
   if (not FileExists(lpwPlb09FilePath)) then
      CLF_Abort('Could not find LP Wizard .plb09 source file "' + lpwPlb09FilePath + '"!');
   
   { Store (old) names of PADS .asc file and Mentor LP Wizard .plb09 file in galactic string list. }
   cnfGalacticInfo.add(constGilOldPadsFileName + constStringEquals + padsFilePath);
   cnfGalacticInfo.add(constGilOldPlb09FileName + constStringEquals + lpwPlb09FilePath);

   { Read contents of .plb09 library file. }
   lpwPlb09FileStrs := TStringList.Create();
   lpwPlb09FileStrs.LoadFromFile(lpwPlb09FilePath);

   { Figure out how many lines were just read in. }
   lpwPlb09FileLines := lpwPlb09FileStrs.Count;
   WriteToDebugFile(' Read ' + IntToStr(lpwPlb09FileLines) + ' lines from .plb09 file.');

   { Sanity check to make sure that there is only 1 footprint described in this .plb09 file! }
   if (lpwPlb09FileLines > constLpWizardMaxLinesInFile) then
      CLF_Abort('Found too many lines in .plb09 file, indicating multiple footprints.  This script requires 1 footprint per .plb09 file!');    
   
   { Extract field names from LP Wizard file. }
   lpwPlb09FieldNames := TStringList.Create();
   CLF_SplitDelimitedUnquotedStringIntoStringList({delimitedString} lpwPlb09FileStrs[constLpWizardFieldNamesLine],
                                                  {delimiter} constLpWizardFieldSepChar,
                                                  {var stringList} lpwPlb09FieldNames);
   WriteToDebugFile(' Extracted ' + IntToStr(lpwPlb09FieldNames.Count) + ' field names from .plb09 file.');
   

   { Extract field values from LP Wizard file. }
   lpwPlb09FieldValues := TStringList.Create();
   CLF_SplitDelimitedUnquotedStringIntoStringList({delimitedString} lpwPlb09FileStrs[constLpWizardFieldValuesLine],
                                                  {delimiter} constLpWizardFieldSepChar,
                                                  {var stringList} lpwPlb09FieldValues);
   WriteToDebugFile(' Extracted ' + IntToStr(lpwPlb09FieldValues.Count) + ' field values from .plb09 file.');

   { Sanity check. }
   if (lpwPlb09FieldNames.Count <> lpwPlb09FieldValues.Count) then
      CLF_Abort('Unequal number of field names vs. field values in LP Wizard .plb09 file!');

   { Loop over all the name/value pairs. }
   for i := 0 to (lpwPlb09FieldNames.Count - 1) do
   begin

      { Add this name/value pair to the galactic info list, with a prefix on the name. }
      cnfGalacticInfo.add(constGilPrefixLpWizardInfo + lpwPlb09FieldNames[i] + constStringEquals + lpwPlb09FieldValues[i]);

   end; { endfor }

   { Extract the first bit of the LP Wizard description field and use that as our footprint type. }
   lpwPlb09Description   := cnfGalacticInfo.Values(constGilPrefixLpWizardInfo + constLpWizardDescriptionField);
   rc := SplitStringIntoLeftAndRight({splitMe} lpwPlb09Description,
                                     {delimiter} constLpWizardDescFieldSepChar,
                                     {var} leftStr,
                                     {var} rightStr);

   { Make one name substitution to save what little remains of my sanity. }
   { NOTE:  Changing 'SON' to 'DFN'.  Who on earth calls it a 'SON' other than the LP Wizard??? }   
   footprintType         := StringReplace(leftStr, 'SON', 'DFN', MkSet(rfReplaceAll));
   WriteToDebugFile(' Footprint type is "' + footprintType + '".');
   
   { Sanity check }
   if ( (rc <> 0) or (footprintType = '') ) then
      CLF_Abort('Unable to get valid Description out of .plb09 file!  Did you override the Description when in LP Wizard??');

   { Store this to our galactic list of information. }
   cnfGalacticInfo.add(constGilFootprintType + constStringEquals + footprintType);

   {** Extract package dimensions from fields we already got out of .plb09 file. **}
   CLF_ExtractLpWizardPackageDimensions({var} cnfGalacticInfo,
                                        {var} hasEp);

   
   {** Extract build info from fields we already got out of .plb09 file. **}
   CLF_ExtractLpWizardBuildInfo({var} cnfGalacticInfo);

   {** Perform various checks on the build info. **}
   { Check silkscreen-to-pad parameter. }
   CLF_CheckLpWizardParameter({fieldName}     constLpWizardBuildInfoSilkToPadField,
                              {requiredValue} constLpWizardBuildInfoSilkToPadValue,
                              {fieldDesc}     'silk-to-pad',
                              cnfGalacticInfo);

   { Check silkscreen outline mode parameter. }
   CLF_CheckLpWizardParameter({fieldName}     constLpWizardBuildInfoSilkOutlineField,
                              {requiredValue} constLpWizardBuildInfoSilkOutlineValue,
                              {fieldDesc}     'silkscreen outline mode',
                              cnfGalacticInfo);

   { Check assembly outline mode parameter. }
   CLF_CheckLpWizardParameter({fieldName}     constLpWizardBuildInfoAssyOutlineField,
                              {requiredValue} constLpWizardBuildInfoAssyOutlineValue,
                              {fieldDesc}     'assembly outline mode',
                              cnfGalacticInfo);

   {* TODO:  Instead of checking these pre-defined constants against LP Wizard parameters,
    we should probably just use them instead of the constants.  *}
   {* TODO:  We should check that body_outline_width <> assy_outline_width <> silk_line_width. *}
   { Check body line width parameter. }
   CLF_CheckLpWizardParameter({fieldName}     constLpWizardBuildInfoBodyLineWidthField,
                              {requiredValue} FloatToStr(constOldWidthAssyDrawingMm),   { We have been using the body outline as our proto- assembly outline. }
                              {fieldDesc}     'body outline line width',
                              cnfGalacticInfo);

   { Check assy line width parameter. }
   CLF_CheckLpWizardParameter({fieldName}     constLpWizardBuildInfoAssyLineWidthField,
                              {requiredValue} FloatToStr(constOldWidthLinesToKill),     { We have not been using this assembly outline, but we need to know how to recognize it and kill it. }
                              {fieldDesc}     'assembly outline line width',
                              cnfGalacticInfo);

   { Check silk line width parameter. }
   CLF_CheckLpWizardParameter({fieldName}     constLpWizardBuildInfoSilkLineWidthField,
                              {requiredValue} FloatToStr(constWidthSilkMm),
                              {fieldDesc}     'silkscreen outline line width',
                              cnfGalacticInfo);


   {** Perform various checks on the footprint name. **}

   { Presume that the base footprint body is black in color. }
   packageColorCode    := 'Blk';

   { Look for other allowed colors. }
   if (CLF_DoesStringEndWith(padsFileBaseName, '_Gry_BLNK_IPC_LPW')) then
      packageColorCode := 'Gry'
   
   else if (CLF_DoesStringEndWith(padsFileBaseName, '_Yel_BLNK_IPC_LPW')) then
      packageColorCode := 'Yel';
   
   { Call CLF_CalculateLibFileNameSuffixAndDescription() to obtain the appropriate libFileNameSuffix. }
   CLF_CalculateLibFileNameSuffixAndDescription({hasThVias} False,          { Specify that the base footprint has no thermal vias in the EP pad. }                                                      
                                                packageColorCode,           { Specify the color of the base footprint body. }
                                                {packageMarkingMfgName} '', { Specify that the base footprint body is not marked. }
                                                {packageMarkingMfgPn} '',   { " " " }
                                                {packageMarkingText} '',    { " " " }                                                
                                                cnfGalacticInfo,
                                                {var} libFileNameSuffix,
                                                {var} libDescription);

   { Verify that various required extra strings are appended to the PADS file base name. }
   if (not CLF_DoesStringEndWith(padsFileBaseName, libFileNameSuffix) ) then
   begin

      { Issue warning modal dialog box with specified warning message,
       specified reply after clicking Ok, and specified reply after clicking Cancel. }
      IssueWarningWithOkOrCancel('Warning:  PADS .asc file "' + padsFileBaseName + '.asc" did not end in required suffix "' + libFileNameSuffix + '"!!' + constLineBreak +
                                 constLineBreak +
                                 'I will try to correct this if you click Ok.  Otherwise, hit Cancel to Abort',
                                 '',
                                 '');

      { Strip off everything to the right of the first '_' separator.
       This is in case the user tried to put in some of the suffix but got it slightly wrong. }
      SplitStringIntoLeftAndRight({splitMe} padsFileBaseName,
                                  {delimiter} '_',
                                  {var} leftStr,
                                  {var} rightStr);

      
      { Use the base name plus our computed suffix as the library file name. }
      padsFileBaseName := leftStr + libFileNameSuffix;
      WriteToDebugFile(' padsFileBaseName is now "' + padsFileBaseName + '".');

      { Construct new full name/paths for PADS .asc and LPW .plb09 files. }
      padsFilePath := padsFilePathOnly + padsFileBaseName + constExtPadsAscii;
      lpwPlb09FilePath := ChangeFileExt(padsFilePath, constExtLpWizardLibrary);
      WriteToDebugFile(' padsFilePath is now "' + padsFilePath + '".');
      WriteToDebugFile(' lpwPlb09FilePath now is "' + lpwPlb09FilePath + '".');
      
   end;

   { Use the PADS .asc file base name as the library name also. }
   libFileName := padsFileBaseName;
   
   { Store (new) names of PADS .asc file and Mentor LP Wizard .plb09 file in galactic string list. }
   cnfGalacticInfo.add(constGilPadsFileName + constStringEquals + padsFilePath);
   cnfGalacticInfo.add(constGilPlb09FileName + constStringEquals + lpwPlb09FilePath);

   { Store library file name and library file name suffix and library file description. }
   cnfGalacticInfo.add(constGilLibraryFileName + constStringEquals + libFileName);
   cnfGalacticInfo.add(constGilLibraryFileNameSuffix + constStringEquals + libFileNameSuffix);
   cnfGalacticInfo.add(constGilLibraryFileDescription + constStringEquals + libDescription);
   
   { Free string lists. }
   lpwPlb09FileStrs.Free;
   lpwPlb09FieldNames.Free;
   lpwPlb09FieldValues.Free;

   CLF_WriteToSummaryAndDebugFilesWithStepNum('Extracted package and build parameters from Mentor LP Wizard .plb09 file "' + lpwPlb09FilePath + '".');
end; { end CLF_ExtractInfoFromLpWizardFile() }
                                    

{***************************************************************************
 * function CLF_ImportFromPads()
 *  Import the PADS .asc file into a new Altium .PcbDoc file within a new project.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ImportFromPads(    Workspace       : IWorkspace;
                                scriptsPath     : TDynamicString;
                                projectPath     : TDynamicString;
                            var importedProject : IProject;
                            var cnfGalacticInfo : TStringList;
                                )               : Integer;

var
   i                   : Integer;
   padsFilePath        : TString;
   lpwPlb09FilePath    : TString;
   padsOldFilePath     : TString;
   lpwPlb09OldFilePath : TString;
   origProjPath        : TString;
   newProjPath        : TString;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_ImportFromPads()');
   CLF_UpdateGuiStatusMessage('Proceeding to import PADS .asc footprint into Altium.  Click "Next" 8 times.  Then click "Finish".');

   { Record our original focused project full path. }
   origProjPath        := Workspace.DM_FocusedProject.DM_ProjectFullPath;
   WriteToDebugFile(' Current project starts out as "' + origProjPath + '".');

   { Retrieve names of PADS .asc file and Mentor LP Wizard .plb09 file from galactic string list. }
   padsOldFilePath       := cnfGalacticInfo.Values(constGilOldPadsFileName);
   lpwPlb09OldFilePath   := cnfGalacticInfo.Values(constGilOldPlb09FileName);
   padsFilePath          := cnfGalacticInfo.Values(constGilPadsFileName);
   lpwPlb09FilePath      := cnfGalacticInfo.Values(constGilPlb09FileName);
   WriteToDebugFile(' padsFilePath is "' + padsFilePath + '".');
   WriteToDebugFile(' lpwPlb09FilePath is "' + lpwPlb09FilePath + '".');
   WriteToDebugFile(' padsOldFilePath is "' + padsOldFilePath + '".');
   WriteToDebugFile(' lpwPlb09OldFilePath is "' + lpwPlb09OldFilePath + '".');

   { See if there are differences between the old and new filenames. }
   if ( (padsFilePath <> padsOldFilePath) or (lpwPlb09FilePath <> lpwPlb09OldFilePath) ) then
   begin

      WriteToDebugFile(' Have to deal with renaming PADS .asc and LPW .plb09 files!');

      { Call CLF_MoveFileSvnAware() to do a filesystem move or an svn move as needed. }
      CLF_MoveFileSvnAware(scriptsPath,
                           projectPath,
                           {fileOldPath} padsOldFilePath,
                           {fileNewPath} padsFilePath
                           );

      CLF_MoveFileSvnAware(scriptsPath,
                           projectPath,
                           {fileOldPath} lpwPlb09OldFilePath,
                           {fileNewPath} lpwPlb09FilePath
                           );

      CLF_WriteToSummaryAndDebugFilesWithStepNum('Renamed PADS .asc file "' + padsOldFilePath + '" to "' + padsFilePath + '".');
      CLF_WriteToSummaryAndDebugFilesWithStepNum('Renamed Mentor LP Wizard .plb09 file "' + lpwPlb09OldFilePath + '" to "' + lpwPlb09FilePath + '".');
end; { endif }


   { Verify that PADS .asc file exists where it should. }
   if (not FileExists(padsFilePath)) then
      CLF_Abort('Could not find PADS .asc source file "' + padsFilePath + '"!');
   
   { Attempt to open this file. }
   { This will launch the import-from-PADS wizard to which the user will have to click Next 8 times and then Finish 1 time. }
   ResetParameters;
   AddStringParameter('ObjectKind', 'Document');
   AddStringParameter('FileName', padsFilePath);
   RunProcess('WorkspaceManager:OpenObject');

   { Retrieve a reference to the new and newly-focused project. }
   importedProject := Workspace.DM_FocusedProject;

   { Record our new focused project full path. }
   newProjPath        := Workspace.DM_FocusedProject.DM_ProjectFullPath;
   WriteToDebugFile(' After running PADS import, current project is now "' + newProjPath + '".');

   { Sanity check. }
   if (origProjPath = newProjPath) then
      CLF_Abort('Something went wrong with PADS import.  I don''t have a newly imported project to work on!');
   
   CLF_WriteToSummaryAndDebugFilesWithStepNum('Imported PADS .asc file "' + padsFilePath + '".');

end; { end CLF_ImportFromPads() }


{***************************************************************************
 * function CLF_FindImportedPcbDoc()
 *  Find the footprint that has been imported form PADS into a new
 *  Altium project and a PcbDoc file within said project.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_FindImportedPcbDoc(    Project         : IProject;
                                var importedPcbDoc  : IDocument;
                                var cnfGalacticInfo : TStringList;
                                )                   : Integer;

var
   i              : Integer;
   k              : Integer;
   document       : IDocument;
   numPcbDocs     : Integer;
   pcbDocFileName : TString;
   footprintType  : TString;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_FindImportedPcbDoc()');

   { Flag that we have not yet found the imported PcbDoc. }
   numPcbDocs := 0;

   { Look over all project documents looking for PcbDoc files. }
   {** Loop over all logical documents in the project (counting backwards). **}
   for k := (Project.DM_LogicalDocumentCount - 1) downto 0 do
   begin
      document := Project.DM_LogicalDocuments(k);
      WriteToDebugFile('Examining project document ' + document.DM_FullPath);
      WriteToDebugFile('Document kind is ' + document.DM_DocumentKind);
      
      { See if this document is a PcbDoc file. }
      if (AnsiUpperCase(document.DM_DocumentKind) = constKindPcb) then
      begin

         WriteToDebugFile('Identified project document "' + document.DM_FullPath + '" as a PcbDoc file.');

         { Return reference to this document to caller. }
         importedPcbDoc := document;
         
         { Increment the number of PcbDoc files that we've found. }
         numPcbDocs := numPcbDocs + 1;

      end; { endif}

   end; { endfor }

   { See if we have exactly 1 PcbDoc file. }
   if (numPcbDocs <> 1) then
   begin
      CLF_Abort('Expected to find 1 PcbDoc files in focused project.  But I found ' + IntToStr(numPcbDocs) + ' instead.');

   end; { endif }

   CLF_WriteToSummaryAndDebugFilesWithStepNum('Import created .PcbDoc file "' + importedPcbDoc.DM_FullPath + '".');
end; { end CLF_FindImportedPcbDoc() }


{***************************************************************************
 * function CLF_MoveImportedPcbDocAndNukeImportedProject()
 *  Do a filesystem move to move the imported PcbDoc file to live in its
 *  new home in the target project's directory.
 *
 *  Nuke the imported project and its History/ subdir and everything.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_MoveImportedPcbDocAndNukeImportedProject(    Workspace       : IWorkspace;
                                                          Project         : IProject;
                                                          projectPath     : TDynamicString;
                                                          importedProject : IProject;
                                                      var importedPcbDoc  : IDocument;
                                                      var cnfGalacticInfo : TStringList;
                                                          )               : Integer;

var
   i                  : Integer;
   k                  : Integer;
   oldProjPath        : TString;
   newProjPath        : TString;
   projectDoc         : IServerDocument;
   targetProjPath     : TString;
   pcbDocPathOnly     : TString;
   pcbDocFileNameOnly : TString;
   pcbDocOldFilePath  : TString;
   pcbDocNewFilePath  : TString;
   logOldFilePath     : TString;
   logNewFilePath     : TString;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_MoveImportedPcbDocAndNukeImportedProject()');
   WriteToDebugFile(' importedPcbDoc full path is "' + importedPcbDoc.DM_FullPath + '".');
   CLF_UpdateGuiStatusMessage('Proceeding to move imported .PcbDoc file into proper project.');

   { Record our old focused project full path. }
   oldProjPath        := Workspace.DM_FocusedProject.DM_ProjectFullPath;
   WriteToDebugFile(' Before doing anything, current project is "' + oldProjPath + '".');

   { Try to close all project documents for the imported project. }
   ResetParameters;
   AddStringParameter('ObjectKind', 'FocusedProjectDocuments');
   RunProcess('WorkspaceManager:CloseObject');
   
   { Open target project. }
   WriteToDebugFile('Attempting to open target project "' + project.DM_ProjectFullPath + '".');
   targetProjPath := project.DM_ProjectFullPath;
   
   ResetParameters;
   AddStringParameter('ObjectKind', 'Project');
   AddStringParameter('FileName', targetProjPath);
   RunProcess('WorkspaceManager:OpenObject');

   ResetParameters;
   RunProcess('WorkspaceManager:SetCurrentProject');

   
   {* I can't get any method to work to close the old project without crashing Altium! *}
//   ShowMessage('Before attempting to close imported project');

//   { Try to close project. }
//   ResetParameters;
//   AddStringParameter('ObjectKind', 'FocusedProjectAndDocuments');
//   RunProcess('WorkspaceManager:CloseObject');
 
//   projectDoc := Client.OpenDocument('PrjPcb', importedProject.DM_ProjectFullPath);
//   Client.CloseDocument(projectDoc);

//   { Try to close imported project. }
//   ResetParameters;
//   AddStringParameter('ObjectKind', 'Project');
//   AddStringParameter('FileName', oldProjPath);
//   RunProcess('WorkspaceManager:CloseObject');
   
//   ShowMessage('After attempting to close imported project');

   
   { Record our new focused project full path. }
   newProjPath        := Workspace.DM_FocusedProject.DM_ProjectFullPath;
   WriteToDebugFile(' After trying to close imported project, current project is now "' + newProjPath + '".');

   {* Move PcbDoc file to its new location. *}
   pcbDocOldFilePath  := importedPcbDoc.DM_FullPath;
   pcbDocFileNameOnly := ExtractFileName(pcbDocOldFilePath);
   pcbDocNewFilePath  := (projectPath + pcbDocFileNameOnly);

   { Perform filesystem move. }
   CLF_MoveFile({fileOldPath} pcbDocOldFilePath,
                {fileNewPath} pcbDocNewFilePath);
   CLF_WriteToSummaryAndDebugFilesWithStepNum('Moved .PcbDoc file "' + pcbDocOldFilePath + '" to "' + pcbDocNewFilePath + '".');

   { Add PcbDoc (from new location) to project. }
   CLF_ProjectAddFile(Project,
                      {addMePath} pcbDocNewFilePath);

   { Move accompanying log file to its new location. }
   logOldFilePath     := ChangeFileExt(pcbDocOldFilePath, '.LOG');
   logNewFilePath     := ChangeFileExt(pcbDocNewFilePath, '.LOG');
   CLF_WriteToSummaryAndDebugFilesWithStepNum('Moved import log file "' + logOldFilePath + '" to "' + logNewFilePath + '".');
   
   { Perform filesystem move. }
   CLF_MoveFile({fileOldPath} logOldFilePath,
                {fileNewPath} logNewFilePath);

   { Add log file name (at new location) to galactic string list. }
   cnfGalacticInfo.add(constGilLogFileName + constStringEquals + logNewFilePath);
  

   { Nuke remaining files in the original imported directory (eg. useless project file, History/ files, etc.) }
   { FIXME:  Figure out why we are unable to delete the annoying __Previews directory!  Commenting out operation for now. }
//   CLF_DeleteFilesAndDirs(ExtractFilePath(oldProjPath));   

   
   { Retrieve a reference to this "new" PcbDoc document. }
   importedPcbDoc := Workspace.DM_GetDocumentFromPath(pcbDocNewFilePath);
   WriteToDebugFile(' importedPcbDoc full path is now "' + importedPcbDoc.DM_FullPath + '".');
   
end; { end CLF_MoveImportedPcbDocAndNukeImportedProject() }


{***************************************************************************
 * function CLF_AddSourceDocumentsToProjectAndSvn()
 *  Add our source documents to the project and to subversion.  This includes
 *  the Mentor LP Wizard .plb09 file, the PADS .asc file, the .PcbDoc resulting
 *  from the import, the logfile from the import, and an .xml command file
 *  (if the user has created one for this footprint).
 *  
 *  Note:  The .PcbDoc file resulting from the import from PADS has already been
 *  added to project, although not to svn.
 *  
 *  Just do svn add, no commits.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_AddSourceDocumentsToProjectAndSvn(    project           : IProject;
                                                   projectPath       : TDynamicString;
                                                   scriptsPath       : TDynamicString;
                                                   lpWizardFilesPath : TDynamicString;
                                                   importedPcbDoc    : IDocument;
                                               var commandFilePath   : TString;
                                                   cnfGalacticInfo   : TStringList;
                                                   )                 : Integer;
                                                                 
var                                                              
   i                   : Integer;
   rc                  : Integer;
   filesToAdd          : TStringList;
   padsFilePath        : TString;
   lpwPlb09FilePath    : TString;
   padsOldFilePath     : TString;
   lpwPlb09OldFilePath : TString;
   logFilePath         : TString;
   projectDoc          : IServerDocument;
   libFileName         : TString;
   dummy               : TStringList;
   pdfFilePath         : TString;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_AddSourceDocumentsToProjectAndSvn().');
   CLF_UpdateGuiStatusMessage('Proceeding to add source documents to project and to svn.');

   { Retrieve required info from galactic string list. }
   libFileName          := cnfGalacticInfo.Values(constGilLibraryFileName);

   { Add projectPath to galactic string list, for later use. }
   cnfGalacticInfo.add(constGilProjectFileName + constStringEquals + projectPath);
   
   { Retrieve names of PADS .asc file and Mentor LP Wizard .plb09 file from galactic string list. }
   padsOldFilePath       := cnfGalacticInfo.Values(constGilOldPadsFileName);
   lpwPlb09OldFilePath   := cnfGalacticInfo.Values(constGilOldPlb09FileName);
   padsFilePath          := cnfGalacticInfo.Values(constGilPadsFileName);
   lpwPlb09FilePath      := cnfGalacticInfo.Values(constGilPlb09FileName);
   logFilePath           := cnfGalacticInfo.Values(constGilLogFileName);
   pdfFilePath           := lpWizardFilesPath + + '\' + libFileName + '.pdf';
   
   WriteToDebugFile(' padsFilePath is "' + padsFilePath + '".');
   WriteToDebugFile(' lpwPlb09FilePath is "' + lpwPlb09FilePath + '".');
   WriteToDebugFile(' padsOldFilePath is "' + padsOldFilePath + '".');
   WriteToDebugFile(' lpwPlb09OldFilePath is "' + lpwPlb09OldFilePath + '".');
   WriteToDebugFile(' logFilePath is "' + logFilePath + '".');
   WriteToDebugFile(' pdfFilePath is "' + pdfFilePath + '".');
   
   { See if there are differences between the old and new filenames. }
   if ( (padsFilePath <> padsOldFilePath) or (lpwPlb09FilePath <> lpwPlb09OldFilePath) ) then
   begin

      WriteToDebugFile(' Need to remove old PADS .asc and LPW .plb09 files from project!');

      {* Remove PADS .asc file and Mentor LP Wizard .plb09 file from project. }
      CLF_ProjectRemoveFile(Project,
                         {removeMePath} padsOldFilePath);
      
      CLF_ProjectRemoveFile(Project,
                         {removeMePath} lpwPlb09OldFilePath);
      
   end; { endif }

   {* Add PADS .asc file and Mentor LP Wizard .plb09 file to project. }
   CLF_ProjectAddFile(Project,
                      {addMePath} padsFilePath);
   CLF_WriteToSummaryAndDebugFilesWithStepNum('Added PADS .asc file "' + padsFilePath + '" to project.');

   CLF_ProjectAddFile(Project,
                      {addMePath} lpwPlb09FilePath);
   CLF_WriteToSummaryAndDebugFilesWithStepNum('Added Mentor LP Wizard .plb09 file "' + lpwPlb09FilePath + '" to project.');

   { PcbDoc (from new location) has already been added to project. }
   CLF_WriteToSummaryAndDebugFilesWithStepNum('Added .PcbDoc file "' + importedPcbDoc.DM_FullPath + '" to project.');
   
   { Add log file (from new location) to project. }
   CLF_ProjectAddFile(Project,
                      {addMePath} logFilePath);
   CLF_WriteToSummaryAndDebugFilesWithStepNum('Added import log file "' + logFilePath + '" to project.');


   {* Look for a footprint-specific command file (.xml). *}
   commandFilePath     := (projectPath + libFileName + '.xml');

   { See if the file even exists. }
   if (FileExists(commandFilePath)) then
   begin

      { Make sure it's readable (eg. not still open and flocked by Excel). }
      VerifyFileIsReadable(commandFilePath);

      { Add command file to project. }
      CLF_ProjectAddFile(Project,
                         {addMePath} commandFilePath);
      CLF_WriteToSummaryAndDebugFilesWithStepNum('Added footprint command file "' + commandFilePath + '" to project.');
   
   end { endif }

   { Else we do not have a footprint-specific commmand file for this footprint.  Set this field to be null (''). }
   else
      commandFilePath     := '';
   
   
   {* Save modifications to project file. *}
   { Note:  If/when the script crashes here, it means that Altium itself is unhappy and needs to be shut down and restarted! }
   projectDoc := Client.OpenDocument('PrjPcb', project.DM_ProjectFullPath);
   projectDoc.DoFileSave('PrjPcb');
   
   
   {* Svn add the source documents. *}
   WriteToDebugFile('lpWizardFilesPath is "' + lpWizardFilesPath + '".');

   { Create string list. }
   filesToAdd := TStringList.Create();

   { Setup which files to add to svn. }
   { TODO:  If these files are now part of our project, can we use the builtin version control to do the svn add?? }
   filesToAdd.Add(ExtractFileName(padsFilePath)); 
   filesToAdd.Add(ExtractFileName(lpwPlb09FilePath));
   if ( FileExists(pdfFilePath) ) then
      filesToAdd.Add(libFileName + '.pdf');
   filesToAdd.Add(importedPcbDoc.DM_FullPath);
   filesToAdd.Add(logFilePath);

   { If we have a valid command file, then add that to svn too. }
   if (commandFilePath <> '') then
      filesToAdd.Add(commandFilePath);
      
   { If any of these files need to be added to svn, then do so. }
   CLF_SvnAddFiles(scriptsPath,
                   {projectPath} lpWizardFilesPath,
                   {allProjectDocs} filesToAdd);

   { Free string list. }
   filesToAdd.Free();

   CLF_WriteToSummaryAndDebugFilesWithStepNum('Added above files to svn.');
   
   {* Order Altium to refresh the project's svn status for display purposes. *}
   CLF_RefreshProjectSvnStatus(99);

end; { end CLF_AddSourceDocumentsToProjectAndSvn() }


{***************************************************************************
 * function CLF_AddGeneratedDocumentsToProjectAndSvn()
 *  Add our generated documents to the project and to subversion.
 *  This includes the actual .PcbLib file, the summary report file, and the
 *  csv report file.
 *  
 *  Just do svn add, no commits.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_AddGeneratedDocumentsToProjectAndSvn(project           : IProject;
                                                  projectPath       : TDynamicString;
                                                  scriptsPath       : TDynamicString;
                                                  libSubDir         : TString;
                                                  libFileName       : TString;
                                                  pcbLibFileName    : TString;
                                                  csvReportFilePath : TString;
                                                  fcstdFilePath     : TString;
                                                  iniFilePath       : TString;
                                                  stepFilePath      : TString;
                                                  logFilePath       : TString;
                                                  reportFilePath    : TString;
                                                  cnfGalacticInfo   : TStringList;
                                                  )                 : Integer;
                                                                 
var                                                              
   i          : Integer;
   rc         : Integer;
   filesToAdd : TStringList;
   projectDoc : IServerDocument;
   dummy      : TStringList;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_AddGeneratedDocumentsToProjectAndSvn().');
   CLF_UpdateGuiStatusMessage('Proceeding to add generated documents to project and to svn.');

   { Add PcbLib file to project. }
   CLF_ProjectAddFile(Project,
                      {addMePath} pcbLibFileName);
   CLF_WriteToSummaryAndDebugFilesWithStepNum('Added PcbLib file "' + pcbLibFileName + '" to project.');
   

   {** Save a copy of summary file to a per-PcbLib file name and add that file to project and svn. **}

   
   { Write a copy of our summary file to the reportFilePath, which is named similarly to the .PcbLib file. }
   { Note:  We're not done with creating summary messages.  We're doing the project add and svn add operations
    now so we don't have to worry about that part later.  But we will need to re-write the report file itself
    once the script is actually done. }
   if (FileExists(reportFilePath)) then VerifyFileIsWriteable(reportFilePath);
   SummaryMessages.SaveToFile(reportFilePath);

   { Add copy of summary file to project. }
   CLF_ProjectAddFile(Project,
                      {addMePath} reportFilePath);
   CLF_WriteToSummaryAndDebugFilesWithStepNum('Added script report file "' + reportFilePath + '" to project.');

   {** Save the CSV report file and add that file to project and svn. **}
   { Write null file to our CSV report file to the csvReportFilePath, which is named similarly to the .PcbLib file. }
   { Note:  We're not done with creating this report.  We're doing the project add and svn add operations
    now so we don't have to worry about that part later.  But we will need to re-write the csvReport file itself
    once the script is actually done. }
   if (FileExists(csvReportFilePath)) then VerifyFileIsWriteable(csvReportFilePath);

   { Create string list. }
   dummy := TStringList.Create();

   { Write null string list to csv report file. }
   dummy.SaveToFile(csvReportFilePath);

   { Free string list. }
   dummy.Free();
   

   { Add CSV report file to project. }
   CLF_ProjectAddFile(Project,
                      {addMePath} csvReportFilePath);
   CLF_WriteToSummaryAndDebugFilesWithStepNum('Added CSV report file "' + csvReportFilePath + '" to project.');
   

   {* Save modifications to project file. *}
   { Note:  If/when the script crashes here, it means that Altium itself is unhappy and needs to be shut down and restarted! }
   projectDoc := Client.OpenDocument('PrjPcb', project.DM_ProjectFullPath);
   projectDoc.DoFileSave('PrjPcb');
   
   
   {* Svn add the generated files. *}

   { Create string list. }
   filesToAdd := TStringList.Create();

   { Setup which files to add to svn. }
   { TODO:  If these files are now part of our project, can we use the builtin version control to do the svn add?? }
   filesToAdd.Add(pcbLibFileName);
   filesToAdd.Add(reportFilePath);
   filesToAdd.Add(csvReportFilePath);

   { If the fcstdFilePath was not generated, it means that the script did not know how to generate a STEP model for this footprint type.
    Therefore, we don't want to add the FreeCAD files to subversion. }
   if (fcstdFilePath <> '') then
   begin
      filesToAdd.Add(fcstdFilePath);
      filesToAdd.Add(iniFilePath);
      filesToAdd.Add(stepFilePath);
      filesToAdd.Add(logFilePath);
   end;
   
   { If any of these files need to be added to svn, then do so. }
   CLF_SvnAddFiles(scriptsPath,
                   projectPath,
                   {allProjectDocs} filesToAdd);

   { Free string list. }
   filesToAdd.Free();

   { This only works for focused document.  So it works for .PcbLib doc.
    But I don't want to have to go to the trouble of focusing on report file and csv file. }
//   { Add new PcbLib file to svn. }
//   ResetParameters;
//   AddStringParameter('Action', 'Add');
//   AddStringParameter('ObjectKind', 'FocusedDocument');
//   RunProcess('VersionControl:VersionControl');
//   CLF_WriteToSummaryAndDebugFilesWithStepNum('Added .PcbLib file "' + pcbLibFileName + '" to svn.');

   
   CLF_WriteToSummaryAndDebugFilesWithStepNum('Added above files to svn.');
   
   {* Order Altium to refresh the project's svn status for display purposes. *}
   CLF_RefreshProjectSvnStatus(99);

end; { end CLF_AddGeneratedDocumentsToProjectAndSvn() }


{***************************************************************************
 * function CLF_RevertGeneratedFilesIfNeeded()
 *  See if our CSV report file changed as a result of changing .asc file,
 *  improving script code, changing xml file, etc.
 *  
 *  If our CSV report file is unchanged from before this script run, then
 *  proceed to revert CSV report file and .PcbLib file, so that the user
 *  is not tempted to check in effectively unchanged binary files (eg. .PcbLib).
 *
 *  When mode is true, function is in 3d mode. When mode is false, function is
 *  footprint mode.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_RevertGeneratedFilesIfNeeded(    projectPath           : TDynamicString;
                                              scriptsPath           : TDynamicString;
                                              pcbLibOrFcstdFilePath : TString;
                                              reportOrIniFilePath   : TString;
                                              pcbDocOrStepFilePath  : TString;
                                              csvOrLogFilePath      : TString;
                                              mode                  : Boolean;
                                          var csvOrLogFileOld       : TStringList;
                                          var csvOrLogFileOut       : TStringList;
                                          var filesAreReverted      : Boolean;
                                              )                     : Integer;

var                                                              
   i                  : Integer;
   rc                 : Integer;
   parms              : TStringList;
   pcbLib             : TString;
   csv                : TString;
   csvOrLogFileOldStr : TString;
   csvOrLogFileOutStr : TString;
   footprint          : TString;
   shorterList        : Integer;

begin

   { Assume success. }
   result := 0;

   { Assume that we won't have to revert files. }
   filesAreReverted := False;

   { Depending on which mode (either footprint or 3d model) the function
     is in, the notifications and the Debug file must be different}
   if ( mode ) then
   begin
      pcbLib := 'FCStd';
      csv := 'log';
      csvOrLogFileOldStr := 'freeCadLogOld';
      csvOrLogFileOutStr := 'freeCadLogNew';
      footprint := 'model';
   end

   else
   begin
      pcbLib := 'PcbLib';
      csv := 'csv';
      csvOrLogFileOldStr := 'csvReportOld';
      csvOrLogFileOutStr := 'csvReportOut';
      footprint := 'footprint';
   end;
      
      

   WriteToDebugFile('Hello from CLF_RevertGeneratedFilesIfNeeded().');
   CLF_UpdateGuiStatusMessage('Proceeding to see if ' + pcbLib + ' file is changed compared to last svn checkin.');

   { See if the csvOrLogFileOld file is the same as the new csvOrLogFileOut file. }
   { Note:  Previous code already handled cases where csv file was not previously checked in, etc.
    by not populating the csvOrLogFileOld stringlist.  In such a case, comparing a null stringlist
    to our new stringlist is guaranteed to be different. }
   rc := DiffStringLists({listA} csvOrLogFileOld,
                         {listB} csvOrLogFileOut);

   { If they are the same, then inform user. }
   if (rc = 0) then
   begin

      CLF_WriteToSummaryAndDebugFilesWithStepNum('Upon checking, contents of new .' + csv + ' file match previous .' + csv + ' file contents.  Thus, about to revert changes to .' + csv + ' and .' + pcbLib + ' files!');
      ShowMessage('This generated .' + pcbLib + ' ' + footprint + ' is effectively unchanged since last svn checkin.  Therefore, script will revert files so that you don''t accidentally checkin meaningless changes.');

      { Inform caller that we ended up doing a revert. }
      filesAreReverted := True;
      
      {* Svn revert all generated files. *}

      { Create string list. }
      parms := TStringList.Create();

      { Setup which files to revert. }
      parms.Add(pcbLibOrFcstdFilePath);
      parms.Add(reportOrIniFilePath);
      parms.Add(csvOrLogFilePath);

      { If in 3d model mode, delete the STEP file we just created. }
      if ( mode ) then
      begin
         DeleteFileWithVerify(pcbDocOrStepFilePath);
      end

      { Else we're in footprint mode.  If we have a PcbDoc, then revert it. }
      else if (pcbDocOrStepFilePath <> '') then
      begin
         parms.Add(pcbDocOrStepFilePath);
      end;
         
      { Call IssueSvnCommand() to do all the real work. }
      IssueSvnCommand(scriptsPath,
                      projectPath,
                      {command} constSvnCmdRevert,
                      parms);

      { Free string list. }
      parms.Free();

      {* Order Altium to refresh the project's svn status for display purposes. *}
      CLF_RefreshProjectSvnStatus(99);
      
   end { endif }

   { Else report on the differences. }
   else
   begin
      WriteToDebugFile( csv + ' files appear to be different.');
      //WriteToDebugFile( csv + ' report ' + csvOrLogFileOldStr + ' follows:');

      //for i := 0 to (csvOrLogFileOld.Count - 1) do
      //   WriteToDebugFile(csvOrLogFileOld.Strings[i]);

      //WriteToDebugFile( csv + ' report ' + csvOrLogFileOutStr + ' follows:');

      //for i := 0 to (csvOrLogFileOut.Count - 1) do
      //   WriteToDebugFile(csvOrLogFileOut.Strings[i]);

      { Determine which string list is shorter to avoid any array index out of bounds errors. }
      if (csvOrLogFileOld.Count < csvOrLogFileOut.Count) then
      begin
         shorterList := csvOrLogFileOld.Count;
      end
      else
      begin
         shorterList := csvOrLogFileOut.Count;
      end;

      { Dump the files that are different to the debug file. }
      for i := 0 to (shorterList - 1) do
      begin
         if ( csvOrLogFileOld.Strings[i] <> csvOrLogFileOut.Strings[i] ) then
         begin
            WriteToDebugFile(csvOrLogFileOld.Strings[i]);
            WriteToDebugFile(csvOrLogFileOut.Strings[i]);
            WriteToDebugFile('');
         end; { end if }
      end; { end loop }
      
   end; { endelse }
      
   { Free both csv report stringlists. }
   csvOrLogFileOld.Free();
   csvOrLogFileOut.Free();
  
end; { end CLF_RevertGeneratedFilesIfNeeded() }


{***************************************************************************
 * function CLF_GetCommandFileParm()
 *  Get a specified command file parameter and return it as var parm parmValue.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_GetCommandFileParm(    xmlCellValues : TStringList;
                                    commandStr    : TString;
                                    cellLocStr    : TString;
                                    column        : Integer;
                                var parmValue     : TString;
                                    )             : Integer;

var                                                              
   i              : Integer;
   rc             : Integer;
   parmCellLocStr : TString;

begin

   { Assume success. }
   result := 0;
   
   { We require the filename to live in the same row, column specified. }
   parmCellLocStr := StringReplace(cellLocStr, '_Col_1', ('_Col_' + IntToStr(column)), 0);
   
   { Verify that the xml file had an entry for this location. }
   if (xmlCellValues.IndexOfName(parmCellLocStr) < 0) then
      CLF_Abort('Did not find required parameter in column ' + IntToStr(column) + ' following command file command "' + commandStr + '"!');

   { Retrieve parameter. }
   parmValue := xmlCellValues.Values(parmCellLocStr);
   WriteToDebugFile(' Found required command file parameter in column ' + IntToStr(column) + ', value is "' + parmValue + '".');

end; { end CLF_GetCommandFileParm() }


{***************************************************************************
 * function CLF_GetAndStoreCommandFileParm()
 *  Get a specified command file parameter and store it to galactic string list.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_GetAndStoreCommandFileParm(    xmlCellValues   : TStringList;
                                            commandStr      : TString;
                                            cellLocStr      : TString;
                                            column          : Integer;
                                            parmName        : TString;
                                        var parmValue       : TString;
                                        var cnfGalacticInfo : TStringList;
                                            )               : Integer;

var                                                              
   i              : Integer;
   rc             : Integer;
   parmCellLocStr : TString;
                                              
begin

   { Assume success. }
   result := 0;
   
   { Retrieve parm value from command file parameters. }
   CLF_GetCommandFileParm(xmlCellValues,
                          commandStr,
                          cellLocStr,
                          column,
                          {var} parmValue);
   
   { Add parm to galactic string list. }
   cnfGalacticInfo.add(parmName + constStringEquals + parmValue);
   
end; { end CLF_GetAndStoreCommandFileParm() }


{***************************************************************************
 * function CLF_GetAndStoreCommandFileFilePathParm()
 *  Get a specified command file parameter for a filename and store it to galactic string list.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_GetAndStoreCommandFileFilePathParm(    projectPath         : TDynamicString;
                                                    xmlCellValues       : TStringList;
                                                    commandStr          : TString;
                                                    cellLocStr          : TString;
                                                    column              : Integer;
                                                    fileRelPathParmName : TString;
                                                    filePathParmName    : TString;
                                                var filePath            : TString;
                                                var cnfGalacticInfo     : TStringList;
                                                    )                   : Integer;

var                                                              
   i                 : Integer;
   rc                : Integer;
   specifiedFilePath : TString;
   parmValue         : TString;
   
begin

   { Assume success. }
   result := 0;
   
   { Retrieve parameter SpecifiedFileRelPath from command file parameters and store to galactic string list. }
   CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                  commandStr,
                                  cellLocStr,
                                  {column} CLF_VarPlusPlus(column),
                                  {parmName} fileRelPathParmName,
                                  {var} parmValue,
                                  {var} cnfGalacticInfo);

   { Prepend the absolute project path to the relative specified file path given. }
   specifiedFilePath := projectPath + parmValue;

   { Verify that this file exists. }
   if (not FileExists(specifiedFilePath)) then
      CLF_Abort('From command file, specified file name "' + specifiedFilePath + '" does not exist!');

   { Verify that this file is readable. }
   VerifyFileIsReadable(specifiedFilePath);

   { Add this modified parameter (full path) also to galactic string list. }
   cnfGalacticInfo.add(filePathParmName + constStringEquals + specifiedFilePath);

   { Return this as var parm filePath. }
   filePath := specifiedFilePath;
   
end; { end CLF_GetAndStoreCommandFileFilePathParm() }


{***************************************************************************
 * function CLF_CopyParmFromPrevDerivedFootprint()
 *  Copy a specified parameter from the previous derived footprint in this
 *  series to the current derived footprint.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CopyParmFromPrevDerivedFootprint(    parmBaseName     : TString;
                                                  derivedFpNumPrev : Integer;
                                                  derivedFpNum     : Integer;
                                              var cnfGalacticInfo  : TStringList;
                                                  )                : Boolean;
var
   parmNamePrev : TString;
   parmNameCurr : TString;
   parmValPrev  : TString;
                                                     
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_CopyParmFromPrevDerivedFootprint().');

   { Construct string names for previous and current parameter name. }
   parmNamePrev := parmBaseName + IntToStr(derivedFpNumPrev);
   parmNameCurr := parmBaseName + IntToStr(derivedFpNum);

   { Make sure the previous parameter name is in the galactic string list. }
   if (not CLF_IsNameInStringList({name} parmNamePrev, {stringlist} cnfGalacticInfo)) then
      CLF_Abort('Did not find "' + parmNamePrev + '" from previous derived footprint!');
      
   { Retrieve previous parm value. }
   parmValPrev := cnfGalacticInfo.Values(parmNamePrev);
   
   { Add this previous parm value to the current derived footprint. }
   cnfGalacticInfo.add(parmNameCurr + constStringEquals + parmValPrev);
         
   WriteToDebugFile(' parmNamePrev is ' + parmNamePrev + ', parmNameCurr is ' + parmNameCurr + ', parmValPrev is ' + parmValPrev + '.');

end; { end CLF_CopyParmFromPrevDerivedFootprint() }

               
{***************************************************************************
 * function CLF_ParseCommandFile()
 *  Parse an .xml command file that specifies other actions to take or
 *  parameters to add to our component, beyond what we get from the
 *  Mentor LP Wizard .plb09 file and the PADS .asc file.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ParseCommandFile(    projectPath     : TDynamicString;
                                  commandFilePath : TString;
                              var cnfGalacticInfo : TStringList;
                                  )               : Integer;
                                                                 
var                                                              
   i                : Integer;
   rc               : Integer;
   xmlCellValues    : TStringList;
   cellLocStr       : TString;
   commandStr       : TString;
   parmCellLocStr   : TString;
   parmValue        : TString;
   stepFilePath     : TString;
   column           : Integer;
   splitPin         : TString;
   compHeight       : TString;
   derivedFpNum     : Integer;
   derivedFpNumPrev : Integer;
   currDerivedFpNum : Integer;
   filePath         : TString;
   
begin

   { Assume success. }
   result := 0;

   { Initialize key command file parameters to be null. }
   stepFilePath    := '';
   compHeight      := '';
   splitPin        := '';
   derivedFpNum    := 0;
   currDerivedFpNum := 0;

   WriteToDebugFile('Hello from CLF_ParseCommandFile().');

   { If the path is not null, then proceed to read in the file. }
   if (commandFilePath <> '') then
   begin

      { Create string list to hold extracted cell values from xml file. }
      xmlCellValues   := TStringList.Create;

      { Read xml file into xmlCellValues stringlist. }
      CLF_ReadXmlFileIntoStringList({xmlFilePath} commandFilePath,
                                    {var} xmlCellValues);

      { Loop over all the cells. }
      for i := 0 to (xmlCellValues.Count - 1) do
      begin

         { Split the cell into name=value pair. }
         SplitStringIntoLeftAndRight(xmlCellValues[i],
                                     constStringEquals,
                                     {var} cellLocStr,
                                     {var} commandStr);

         { We require all commands to live in Col1.  So look for any cell living in column 1. }
         { We also require that the command not start with '#', which we consider a comment line character. }
         if ( (CLF_DoesStringEndWith(cellLocStr, '_Col_1')) and
             (not CLF_DoesStringStartWith(commandStr, '#')) ) then
         begin

            WriteToDebugFile(' Found command: "' + commandStr + '".');

            { Since command was in column 1, we need to start retrieving parms from col 2. }
            column          := 2;
               
            {* See if it's one of our recognized commands. *}
            { Look for command to specify STEP file name. }
            if (commandStr = 'Specify STEP file name') then
            begin

               { Retrieve parameter constGilStepFileRelPath from command file parameters. }
               CLF_GetAndStoreCommandFileFilePathParm(projectPath,
                                                      xmlCellValues,
                                                      commandStr,
                                                      cellLocStr,
                                                      {column} CLF_VarPlusPlus(column),
                                                      {fileRelPathParmName} constGilStepFileRelPath,
                                                      {filePathParmName}    constGilStepFilePath,
                                                      {var filePath} stepFilePath,
                                                      {var} cnfGalacticInfo);

               { Retrieve parameter XRot from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilStepXRot,
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter YRot from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilStepYRot,
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter ZRot from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilStepZRot,
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter ZOff from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilStepZOff,
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);
               
            end { endif }

            {* FreeCAD autogenerated models related stuff. *}
            { Specific pivot point height ratio. }
            else if (commandStr = 'Specify FC3DM_pivotPointHeightRatio') then
            begin

               { Retrieve parameter CompHeight from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constFC3DM_pivotPointHeightRatio,
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);
               
            end { end elsif }
            
            { Set SOIC P1chamferOffset }
            else if (commandStr = 'Specify FC3DM_P1chamferOffset') then
            begin 
               { Retrieve parameter P1chamferOffset from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constFC3DM_P1chamferOffset,
                                              {var parmValue} parmValue,
                                              {var} cnfGalacticInfo);
               
            end { end elsif }

                        
            { Set FC3DM_epPin1ChamferRadius for QFNs }
            else if (commandStr = 'Specify FC3DM_epPin1ChamferRadius') then
            begin 
               { Retrieve parameter epPin1ChamferRadius from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constFC3DM_epPin1ChamferRadius,
                                              {var parmValue} parmValue,
                                              {var} cnfGalacticInfo);
               
            end { end elsif }
            
            
            { Look for command to specify component height. }
            else if (commandStr = 'Specify component height') then
            begin

               { Retrieve parameter CompHeight from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilSpecCompHeight,
                                              {var parmValue} compHeight,
                                              {var} cnfGalacticInfo);

            end { end elsif }

            { Look for command to specify component color code. }
            else if (commandStr = 'Specify component color code') then
            begin

               { Retrieve parameter CompHeight from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilSpecCompColorCode,
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

            end { end elsif }

            { Look for command to split a pin. }
            else if (commandStr = 'Split pin') then
            begin

               { Retrieve parameter PinName from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilSplitPinPinName,
                                              {var parmValue} splitPin,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter PinName1 from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilSplitPinPinName1,
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter X1center from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilSplitPinX1center,
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter Y1center from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilSplitPinY1center,
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter X1size from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilSplitPinX1size,
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter Y1size from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilSplitPinY1size,
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter PinName2 from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilSplitPinPinName2,
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter X2center from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilSplitPinX2center,
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter Y2center from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilSplitPinY2center,
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter X2size from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilSplitPinX2size,
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter Y2size from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilSplitPinY2size,
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

            end { end elsif }

            { Look for command to derive a new version of the base footprint. }
            else if (commandStr = 'Derive new footprint by marking') then
            begin

               { Add parm to galactic string list just stating what we're doing.  Note derivedFpNum. }
               cnfGalacticInfo.add(constGilDeriveByMethod + IntToStr(derivedFpNum) + constStringEquals + constGilDeriveByMarking);
   
               { Retrieve parameter MfgName from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilDeriveMfgName + IntToStr(derivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter MfgPn from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilDeriveMfgPn + IntToStr(derivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Increment the number of derived footprints we know about. }
               derivedFpNum    := derivedFpNum + 1;
               
            end { end elsif }

            { Look for command to derive the next footprint in a series, by marking. }
            else if (commandStr = 'Derive next footprint by marking') then
            begin

               { Add parm to galactic string list just stating what we're doing.  Note derivedFpNum. }
               cnfGalacticInfo.add(constGilDeriveByMethod + IntToStr(derivedFpNum) + constStringEquals + constGilDeriveByMarking);
   
               { Retrieve parameter MfgPn from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilDeriveMfgPn + IntToStr(derivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter Text from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dTextText + IntToStr(derivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Calculate derivedFpNum for previous footprint. }
               derivedFpNumPrev := derivedFpNum - 1;
               

               { Copy MfgName parameter from previous footprint. }
               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGilDeriveMfgName,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);


               {* Copy 3D text related parameters from previous footprint. *}
               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dTextRotation,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dTextX1,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dTextY1,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dTextX2,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dTextY2,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dTextFontName,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dTextBold,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dTextItalic,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dTextCenterInX,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dTextCenterInY,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dTextColorText,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dTextColorBackground,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               
               {* Copy 3D image related parameters from previous footprint. *}
               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dImageImageFileRelPath,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dImageImageFilePath,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dImageRotation,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dImageX1,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dImageY1,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dImageX2,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dImageY2,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dImageNegative,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dImageCenterInX,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dImageCenterInY,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dImageColorImage,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);

               CLF_CopyParmFromPrevDerivedFootprint({parmBaseName} constGil3dImageColorBackground,
                                                    derivedFpNumPrev,
                                                    derivedFpNum,
                                                    {var} cnfGalacticInfo);


               { Increment the number of derived footprints we know about. }
               derivedFpNum    := derivedFpNum + 1;
               
            end { end elsif }

            { Look for command to derive a new version of the base footprint. }
            else if (commandStr = 'Derive new footprint by marking text') then
            begin

               { Add parm to galactic string list just stating what we're doing.  Note derivedFpNum. }
               cnfGalacticInfo.add(constGilDeriveByMethod + IntToStr(derivedFpNum) + constStringEquals + constGilDeriveByMarkingText);
   
               { Retrieve parameter MarkingText from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilDeriveMarkingText + IntToStr(derivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Increment the number of derived footprints we know about. }
               derivedFpNum    := derivedFpNum + 1;
               
            end { end elsif }

            { Look for command to derive a new version of the base footprint. }
            else if (commandStr = 'Derive new footprint by height') then
            begin

               { Add parm to galactic string list just stating what we're doing.  Note derivedFpNum. }
               cnfGalacticInfo.add(constGilDeriveByMethod + IntToStr(derivedFpNum) + constStringEquals + constGilDeriveByHeight);
   
               { Retrieve parameter CompHeight from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilDeriveHeight + IntToStr(derivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Increment the number of derived footprints we know about. }
               derivedFpNum    := derivedFpNum + 1;
               
            end { end elsif }

            { Look for command to derive a new version of the base footprint. }
            else if (commandStr = 'Derive new footprint by color') then
            begin

               CLF_Abort('Command "' + commandStr + '" not yet supported!');
               
               { Increment the number of derived footprints we know about. }
               derivedFpNum    := derivedFpNum + 1;
               
            end { end elsif }

            { Look for command to add 3D text to derived footprint. }
            else if (commandStr = 'Add 3D text to derived footprint') then
            begin

               { Retrieve parameter Text from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dTextText + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter Rotation from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dTextRotation + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter X1 from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dTextX1 + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter Y1 from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dTextY1 + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter X2 from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dTextX2 + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter Y2 from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dTextY2 + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter FontName from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dTextFontName + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter Bold from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dTextBold + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter Italic from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dTextItalic + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter CenterInX from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dTextCenterInX + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter CenterInY from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dTextCenterInY + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter ColorText from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dTextColorText + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter ColorBackground from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dTextColorBackground + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);
               
            end { end elsif }

            { Look for command to add 3D image to derived footprint. }
            else if (commandStr = 'Add 3D image to derived footprint') then
            begin

               { Retrieve parameter ImageFileRelPath from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileFilePathParm(projectPath,
                                                      xmlCellValues,
                                                      commandStr,
                                                      cellLocStr,
                                                      {column} CLF_VarPlusPlus(column),
                                                      {fileRelPathParmName} constGil3dImageImageFileRelPath + IntToStr(currDerivedFpNum),
                                                      {filePathParmName}    constGil3dImageImageFilePath + IntToStr(currDerivedFpNum),
                                                      {var} filePath,
                                                      {var} cnfGalacticInfo);

               { Retrieve parameter Rotation from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dImageRotation + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter X1 from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dImageX1 + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter Y1 from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dImageY1 + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter X2 from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dImageX2 + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter Y2 from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dImageY2 + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter Negative from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dImageNegative + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter CenterInX from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dImageCenterInX + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter CenterInY from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dImageCenterInY + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter ColorImage from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dImageColorImage + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);

               { Retrieve parameter ColorBackground from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGil3dImageColorBackground + IntToStr(currDerivedFpNum),
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);
               
            end { end elsif }

            { Look for command to specify STEP model for derived footprint. }
            else if (commandStr = 'Specify STEP file name for derived footprint') then
            begin

               { Retrieve parameter ImageFileRelPath from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileFilePathParm(projectPath,
                                                      xmlCellValues,
                                                      commandStr,
                                                      cellLocStr,
                                                      {column} CLF_VarPlusPlus(column),
                                                      {fileRelPathParmName} constGilDerivedStepFileRelPath + IntToStr(currDerivedFpNum),
                                                      {filePathParmName}    constGilDerivedStepFilePath + IntToStr(currDerivedFpNum),
                                                      {var} filePath,
                                                      {var} cnfGalacticInfo);
               
            end { end elsif }

            { Look for command to specify Altium PcbLib library source. }
            else if (commandStr = 'Specify Altium PcbLib library source') then
            begin

               { Retrieve parameter SpecAltiumPcbLibFileRelPath from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileFilePathParm(projectPath,
                                                      xmlCellValues,
                                                      commandStr,
                                                      cellLocStr,
                                                      {column} CLF_VarPlusPlus(column),
                                                      {fileRelPathParmName} constGilSpecAltPcbLibFileRelPath,
                                                      {filePathParmName}    constGilSpecAltPcbLibFilePath,
                                                      {var} filePath,
                                                      {var} cnfGalacticInfo);
               
            end { end elsif }

            { Look for command to specify Altium footprint source. }
            else if (commandStr = 'Specify Altium footprint source') then
            begin

               { Retrieve parameter SpecAltFootprintName from command file parameters and store to galactic string list. }
               CLF_GetAndStoreCommandFileParm(xmlCellValues,
                                              commandStr,
                                              cellLocStr,
                                              {column} CLF_VarPlusPlus(column),
                                              {parmName} constGilSpecAltFootprintName,
                                              {var} parmValue,
                                              {var} cnfGalacticInfo);
               
            end { end elsif }

            { Else we don't recognize this command. }
            else
               CLF_Abort('Unrecognized command file command "' + commandStr + '"!');

            
            { When we're adding properties for a derived footprint, we want to operate on the last defined derived footprint number. }
            currDerivedFpNum := derivedFpNum - 1;
            
         end; { endif is command }

         
      end; { endfor }
      

      { Free string list. }
      xmlCellValues.Free();
   

   end; { endif }

   { If stepFilePath is null, then add it as such to galactic string list. }
   if (stepFilePath = '') then
      cnfGalacticInfo.add(constGilStepFilePath + constStringEquals + stepFilePath);
   
   { If compHeight is null, then add it as such to galactic string list. }
   if (compHeight = '') then
      cnfGalacticInfo.add(constGilSplitPinPinName + constStringEquals + compHeight);

   { If splitPin is null, then add it as such to galactic string list. }
   if (splitPin = '') then
      cnfGalacticInfo.add(constGilSplitPinPinName + constStringEquals + splitPin);
   
end; { end CLF_ParseCommandFile() }

{***************************************************************************
 * END Import related functions.
 ***************************************************************************}

{***************************************************************************
 * BEGIN Footprint-related functions.
 ***************************************************************************}

{***************************************************************************
 * function CLF_ExplodeComponent()
 *  Find the one component in the source PcbDoc file and explode it.
 *  That is to say, unlock its primitives.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ExplodeComponent(    importedPcbDoc : IDocument;
                              var boardSrc       : IPCB_Board;
                                  )              : Integer;

var                                         
   iterator  : IPCB_BoardIterator;
   component : IPCB_Component;
   numComps  : Integer;

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
      CLF_Abort('PCBServer is Nil.  D''oh.');
   
   { Initialize the PCB editor. }
   PCBServer.PreProcess;

   { I cannot get PCBServer.GetPCBBoardByPath(pcbDocPath) to work, so I'm going
    to manually open the PCB document and then use PCBServer.GetCurrentPCBBoard. }
   ResetParameters;
   AddStringParameter('ObjectKind', 'Document');
   AddStringParameter('FileName', importedPcbDoc.DM_FullPath);
   RunProcess('WorkspaceManager:OpenObject');
   
   { Attempt to open the project's PcbDoc file. }
   { FIXME:  Why does this not work?? GetPCBBoardByPath(pcbDocPath); }
   boardSrc := PCBServer.GetCurrentPCBBoard;

   { Sanity check }
   if (boardSrc = Nil) then
      CLF_Abort('Unable to open PcbDoc file ' + pcbDocPath);

   { Setup an iterator so that we can iterate over all PCB components. }
   iterator        := boardSrc.BoardIterator_Create;
   iterator.AddFilter_ObjectSet(MkSet(eComponentObject));
   iterator.AddFilter_LayerSet(AllLayers);
   iterator.AddFilter_Method(eProcessAll);

   { Get a reference to the first PCB object. }
   component := iterator.FirstPCBObject;

   { Loop over all objects in this PcbDoc file. }
   while (component <> nil) do
   begin
      
      WriteToDebugFile('*Found component ' + component.SourceDesignator);

      { Increment the number of components that we've found. }
      numComps := numComps + 1;

      { Advance to next component in this PcbDoc file. }
      component := iterator.NextPCBObject;

   end;  { endwhile loop over all PCB components in this PcbDoc file. }

   { Get a reference to the first PCB object. }
   component := iterator.FirstPCBObject;

   { Free PCB component iterator. }
   boardSrc.BoardIterator_Destroy(iterator);

   { See if we have exactly 1 component. }
   if (numComps <> 1) then
   begin
      CLF_Abort('Looking for exactly 1 component in imported PcbDoc file.  But I found ' + IntToStr(numComps) + ' components.' + constLineBreak +
                'Perhaps I was reading some other PcbDoc file??');

   end; { endif }

   { Attempt to unlock primitives for our one component. }
   component.SetState_PrimitiveLock(False);

end; { end CLF_ExplodeComponent() }


{***************************************************************************
 * function CLF_RetrieveBoundingRectangleBoundaryNames()
 *  Retrieve the boundary names for a specified namePrefix.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_RetrieveBoundingRectangleBoundaryNames(    namePrefix : TString;
                                                    var nameWest   : TString;
                                                    var nameEast   : TString;
                                                    var nameNorth  : TString;
                                                    var nameSouth  : TString;
                                                        )          : Integer;
begin

   { Assume success. }
   result := 0;
         
   { Construct strings needed to store the coordinates in our galactic string list. }
   nameWest   := namePrefix + 'West';
   nameEast   := namePrefix + 'East';
   nameNorth  := namePrefix + 'North';
   nameSouth  := namePrefix + 'South';

end; { end CLF_RetrieveBoundingRectangleBoundaryNames() }


{***************************************************************************
 * function CLF_RetrieveBoundingRectangleByNamePrefixNoAbort()
 *  Retrieve an existing bounding rectangle by specifying the namePrefix
 *  to galactic string list.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_RetrieveBoundingRectangleByNamePrefixNoAbort(    namePrefix      : TString;
                                                          var cnfGalacticInfo : TStringList;
                                                          var boundaryWestMm  : Real;
                                                          var boundaryEastMm  : Real;
                                                          var boundaryNorthMm : Real;
                                                          var boundarySouthMm : Real;
                                                              )               : Integer;
var                                                               
   nameWest        : TString;
   nameEast        : TString;
   nameNorth       : TString;
   nameSouth       : TString;

begin

   { Assume success. }
   result := 0;

   { Retrieve bounding rectangle names. }
   CLF_RetrieveBoundingRectangleBoundaryNames(namePrefix,
                                              {var} nameWest,
                                              {var} nameEast,
                                              {var} nameNorth,
                                              {var} nameSouth);

   { If these coordinates do not already exist in galactic string list, then flag an error. }
   if (not CLF_IsNameInStringList({name} nameWest,
       {stringlist} cnfGalacticInfo)) then
   begin

      WriteToDebugFile('In CLF_RetrieveBoundingRectangleByNamePrefixNoAbort(), unable to find bounds for namePrefix "' + namePrefix + '"!');

      result := 1;
   end

   { Else proceed as normal. }
   else
   begin
   
      WriteToDebugFile('In CLF_RetrieveBoundingRectangleByNamePrefixNoAbort(), found existing bounds for namePrefix "' + namePrefix + '"!');

      { Retrieve existing values of the bounding box. }
      boundaryWestMm  := StrToFloat(cnfGalacticInfo.Values(nameWest));
      boundaryEastMm  := StrToFloat(cnfGalacticInfo.Values(nameEast));
      boundaryNorthMm := StrToFloat(cnfGalacticInfo.Values(nameNorth));
      boundarySouthMm := StrToFloat(cnfGalacticInfo.Values(nameSouth));
      
   end; { endelse }

end; { end CLF_RetrieveBoundingRectangleByNamePrefixNoAbort() }


{***************************************************************************
 * function CLF_RetrieveBoundingRectangleByNamePrefix()
 *  Retrieve an existing bounding rectangle by specifying the namePrefix
 *  to galactic string list.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_RetrieveBoundingRectangleByNamePrefix(    namePrefix      : TString;
                                                   var cnfGalacticInfo : TStringList;
                                                   var boundaryWestMm  : Real;
                                                   var boundaryEastMm  : Real;
                                                   var boundaryNorthMm : Real;
                                                   var boundarySouthMm : Real;
                                                       )               : Integer;
var                                                               
   rc : Integer;

begin

   { Assume success. }
   result := 0;

   { Call CLF_RetrieveBoundingRectangleByNamePrefixNoAbort() to do all the real work. }
   rc := CLF_RetrieveBoundingRectangleByNamePrefixNoAbort(namePrefix,
                                                          {var} cnfGalacticInfo,
                                                          {var} boundaryWestMm,
                                                          {var} boundaryEastMm,
                                                          {var} boundaryNorthMm,
                                                          {var} boundarySouthMm);

   { If these coordinates do not already exist in galactic string list, then abort. }
   if (rc <> 0) then
      CLF_Abort('In CLF_RetrieveBoundingRectangleByNamePrefix(), unable to find bounds for namePrefix "' + namePrefix + '"!');

end; { end CLF_RetrieveBoundingRectangleByNamePrefix() }


{***************************************************************************
 * function CLF_MaintainBoundingRectangle()
 *  Create and maintain a bounding rectangle that holds all coordinates
 *  given to us for a given name prefix.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_MaintainBoundingRectangle(    X1Mm            : Real;
                                           Y1Mm            : Real;
                                           X2Mm            : Real;
                                           Y2Mm            : Real;
                                           namePrefix      : TString;
                                       var cnfGalacticInfo : TStringList;
                                           )               : Integer;

var                                         
   rc              : Integer;
   XminMm          : Real;
   XmaxMm          : Real;
   YminMm          : Real;
   YmaxMm          : Real;
   boundaryWestMm  : Real;
   boundaryEastMm  : Real;
   boundaryNorthMm : Real;
   boundarySouthMm : Real;
   nameWest        : TString;
   nameEast        : TString;
   nameNorth       : TString;
   nameSouth       : TString;
   indexWest       : Integer;
   indexEast       : Integer;
   indexNorth      : Integer;
   indexSouth      : Integer;

begin

   { Assume success. }
   result := 0;
   
   { Digest the two sets of x,y coordinates. }
   XminMm          := CLF_MinReal(X1Mm, X2Mm);
   XmaxMm          := CLF_MaxReal(X1Mm, X2Mm);
   YminMm          := CLF_MinReal(Y1Mm, Y2Mm);
   YmaxMm          := CLF_MaxReal(Y1Mm, Y2Mm);

   { Retrieve bounding rectangle names. }
   CLF_RetrieveBoundingRectangleBoundaryNames(namePrefix,
                                              {var} nameWest,
                                              {var} nameEast,
                                              {var} nameNorth,
                                              {var} nameSouth);

   { Retrieve existing boundaries (if any). }
   rc := CLF_RetrieveBoundingRectangleByNamePrefixNoAbort(namePrefix,
                                                          {var} cnfGalacticInfo,
                                                          {var} boundaryWestMm,
                                                          {var} boundaryEastMm,
                                                          {var} boundaryNorthMm,
                                                          {var} boundarySouthMm);

   { If this is the first time we've been run on this namePrefix, then
    there won't be existing coordinates.  So take our current min & max
    x,y coordinates and use those as the initial coordinates. }
   if (rc <> 0) then
   begin

      WriteToDebugFile('In CLF_MaintainBoundingRectangle(), initializing new bounds for namePrefix "' + namePrefix + '"!');

      cnfGalacticInfo.add(nameWest + constStringEquals + FloatToStr(XminMm));
      cnfGalacticInfo.add(nameEast + constStringEquals + FloatToStr(XmaxMm));
      cnfGalacticInfo.add(nameNorth + constStringEquals + FloatToStr(YmaxMm));
      cnfGalacticInfo.add(nameSouth + constStringEquals + FloatToStr(YminMm));
   end

   { Else run as usual. }
   else
   begin

      WriteToDebugFile('In CLF_MaintainBoundingRectangle(), found existing bounds for namePrefix "' + namePrefix + '"!');

      {* Maintain west boundary *}
      { Look for a new minimum. }
      if (XminMm < boundaryWestMm) then
      begin
         indexWest  := cnfGalacticInfo.IndexOfName(nameWest);
         cnfGalacticInfo[indexWest] := (nameWest + constStringEquals + FloatToStr(XminMm));
      end;

      {* Maintain east boundary *}
      { Look for a new maximum. }
      if (XmaxMm > boundaryEastMm) then
      begin
         indexEast  := cnfGalacticInfo.IndexOfName(nameEast);
         cnfGalacticInfo[indexEast] := (nameEast + constStringEquals + FloatToStr(XmaxMm));
      end;

      {* Maintain north boundary *}
      { Look for a new maximum. }
      if (YmaxMm > boundaryNorthMm) then
      begin
         indexNorth := cnfGalacticInfo.IndexOfName(nameNorth);
         cnfGalacticInfo[indexNorth] := (nameNorth + constStringEquals + FloatToStr(YmaxMm));
      end;

      {* Maintain south boundary *}
      { Look for a new minimum. }
      if (YminMm < boundarySouthMm) then
      begin
         indexSouth := cnfGalacticInfo.IndexOfName(nameSouth);
         cnfGalacticInfo[indexSouth] := (nameSouth + constStringEquals + FloatToStr(YminMm));
      end;

   end; { endelse }

   WriteToDebugFile(nameWest +  ' is ' + cnfGalacticInfo.Values(nameWest));
   WriteToDebugFile(nameEast +  ' is ' + cnfGalacticInfo.Values(nameEast));
   WriteToDebugFile(nameNorth + ' is ' + cnfGalacticInfo.Values(nameNorth));
   WriteToDebugFile(nameSouth + ' is ' + cnfGalacticInfo.Values(nameSouth));
         
end; { end CLF_MaintainBoundingRectangle() }


{***************************************************************************
 * function CLF_MaintainBoundingRectangleForTracks()
 *  Create and maintain a bounding rectangle that holds all tracks
 *  given to us for a given name prefix.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_MaintainBoundingRectangleForTracks(    boardXorigin    : TCoord;
                                                    boardYorigin    : TCoord;
                                                    trackSrc        : IPCB_Track;
                                                    namePrefix      : TString;
                                                var cnfGalacticInfo : TStringList;
                                                    )               : Integer;

var                                         
   X1Mm         : Real;
   Y1Mm         : Real;
   X2Mm         : Real;
   Y2Mm         : Real;

begin

   { Assume success. }
   result := 0;
   
   { Extract the two x,y coordinates that describe this track. }
   X1Mm        := CoordToMMs(trackSrc.X1 - boardXorigin);
   Y1Mm        := CoordToMMs(trackSrc.Y1 - boardYorigin);
   X2Mm        := CoordToMMs(trackSrc.X2 - boardXorigin);
   Y2Mm        := CoordToMMs(trackSrc.Y2 - boardYorigin);

   WriteToDebugFile('X1Mm is ' + FloatToStr(X1Mm));
   WriteToDebugFile('Y1Mm is ' + FloatToStr(Y1Mm));
   WriteToDebugFile('X2Mm is ' + FloatToStr(X2Mm));
   WriteToDebugFile('Y2Mm is ' + FloatToStr(Y2Mm));

   { Now call CLF_MaintainBoundingRectangle() to do the real work. }
   CLF_MaintainBoundingRectangle(X1Mm,
                                 Y1Mm,
                                 X2Mm,
                                 Y2Mm,
                                 namePrefix,
                                 {var} cnfGalacticInfo);
         
end; { end CLF_MaintainBoundingRectangleForTracks() }


{***************************************************************************
 * function CLF_MaintainBoundingRectangleForRegions()
 *  Create and maintain a bounding rectangle that holds all regions
 *  given to us for a given name prefix.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_MaintainBoundingRectangleForRegions(    boardXorigin    : TCoord;
                                                     boardYorigin    : TCoord;
                                                     regionSrc       : IPCB_Region;
                                                     namePrefix      : TString;
                                                 var cnfGalacticInfo : TStringList;
                                                     )               : Integer;

var                                         
   rectSrc : TCoordRect;
   X1Mm    : Real;
   Y1Mm    : Real;
   X2Mm    : Real;
   Y2Mm    : Real;
   
begin

   { Assume success. }
   result  := 0;

   { Extract bounding rectangle from source region. }
   rectSrc := regionSrc.BoundingRectangle;

   { Get coordinates of bounding rectangle, account for origin, and convert to mm. }
   X1Mm    := CoordToMMs(rectSrc.left - boardXorigin);
   X2Mm    := CoordToMMs(rectSrc.right - boardXorigin);
   Y1Mm    := CoordToMMs(rectSrc.top - boardYorigin);
   Y2Mm    := CoordToMMs(rectSrc.bottom - boardYorigin);

   WriteToDebugFile('X1Mm is ' + FloatToStr(X1Mm));
   WriteToDebugFile('Y1Mm is ' + FloatToStr(Y1Mm));
   WriteToDebugFile('X2Mm is ' + FloatToStr(X2Mm));
   WriteToDebugFile('Y2Mm is ' + FloatToStr(Y2Mm));

   { Now call CLF_MaintainBoundingRectangle() to do the real work. }
   CLF_MaintainBoundingRectangle(X1Mm,
                                 Y1Mm,
                                 X2Mm,
                                 Y2Mm,
                                 namePrefix,
                                 {var} cnfGalacticInfo);
         
end; { end CLF_MaintainBoundingRectangleForRegions() }


{***************************************************************************
 * function CLF_IsInsideBoundingRectangle()
 *  Answers a query as to whether a track/region described by two sets of
 *  (x,y) coordinates is within a given bounding rectangle.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_IsInsideBoundingRectangle(    X1Mm            : Real;
                                           Y1Mm            : Real;
                                           X2Mm            : Real;
                                           Y2Mm            : Real;
                                           namePrefix      : TString;
                                       var cnfGalacticInfo : TStringList;
                                       var isInside        : Boolean;
                                           )               : Integer;

var                                         
   boundaryWestMm  : Real;
   boundaryEastMm  : Real;
   boundaryNorthMm : Real;
   boundarySouthMm : Real;

begin

   { Assume success. }
   result := 0;

   { Assume that our candidate is inside the bounding rectangle until we find out otherwise. }
   isInside := True;

   { Retrieve the bounds for this name prefix. }
   CLF_RetrieveBoundingRectangleByNamePrefix(namePrefix,
                                             {var} cnfGalacticInfo,
                                             {var} boundaryWestMm,
                                             {var} boundaryEastMm,
                                             {var} boundaryNorthMm,
                                             {var} boundarySouthMm);

   {* Check west boundary *}
   if (X1Mm < boundaryWestMm) then
      isInside := False;
   if (X2Mm < boundaryWestMm) then
      isInside := False;

   {* Check east boundary *}
   if (X1Mm > boundaryEastMm) then
      isInside := False;
   if (X2Mm > boundaryEastMm) then
      isInside := False;

   {* Check north boundary *}
   if (Y1Mm > boundaryNorthMm) then
      isInside := False;
   if (Y2Mm > boundaryNorthMm) then
      isInside := False;

   {* Check south boundary *}
   if (Y1Mm < boundarySouthMm) then
      isInside := False;
   if (Y2Mm < boundarySouthMm) then
      isInside := False;

   WriteToDebugFile('In CLF_IsInsideBoundingRectangle(), X1Mm is ' + FloatToStr(X1Mm));
   WriteToDebugFile('In CLF_IsInsideBoundingRectangle(), Y1Mm is ' + FloatToStr(Y1Mm));
   WriteToDebugFile('In CLF_IsInsideBoundingRectangle(), X2Mm is ' + FloatToStr(X2Mm));
   WriteToDebugFile('In CLF_IsInsideBoundingRectangle(), Y2Mm is ' + FloatToStr(Y2Mm));
   WriteToDebugFile('In CLF_IsInsideBoundingRectangle(), isInside is ' + BoolToStr(isInside));
   
end; { end CLF_IsInsideBoundingRectangle() }


{***************************************************************************
 * function CLF_IsArcInsideBoundingRectangle()
 *  Answer a query as to whether a specified arc is within a bounding rectangle.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_IsArcInsideBoundingRectangle(    boardXorigin    : TCoord;
                                              boardYorigin    : TCoord;
                                              arcSrc          : IPCB_Arc;
                                              namePrefix      : TString;
                                          var cnfGalacticInfo : TStringList;
                                          var isInside        : Boolean;
                                              )               : Integer;
                                                     
var                                         
   X1Mm         : Real;
   Y1Mm         : Real;
   X2Mm         : Real;
   Y2Mm         : Real;

begin

   { Assume success. }
   result := 0;

   { Calculate bounds of arc.  Assume it's a full 360 degress around. }
   X1Mm := CoordToMMs(arcSrc.XCenter - arcSrc.Radius - (0.5 * arcSrc.LineWidth) - boardXorigin  );
   X2Mm := CoordToMMs(arcSrc.XCenter + arcSrc.Radius + (0.5 * arcSrc.LineWidth) - boardXorigin  );
   Y1Mm := CoordToMMs(arcSrc.YCenter - arcSrc.Radius - (0.5 * arcSrc.LineWidth) - boardYorigin  );
   Y2Mm := CoordToMMs(arcSrc.YCenter + arcSrc.Radius + (0.5 * arcSrc.LineWidth) - boardYorigin  );

   { Call CLF_IsInsideBoundingRectangle() to do all the real work. }
   CLF_IsInsideBoundingRectangle(X1Mm,
                                 Y1Mm,
                                 X2Mm,
                                 Y2Mm,
                                 namePrefix,
                                 {var} cnfGalacticInfo,
                                 {var} isInside);
   
end; { end CLF_IsArcInsideBoundingRectangle() }


{***************************************************************************
 * function CLF_CreateNewText()
 *  Create a new text with the specified properties.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewText(    layer         : Integer;
                               widthMm       : Real;
                               heightMm      : Real;
                               Xmm           : Real;
                               Ymm           : Real;
                               angle         : TAngle;
                               text          : TString;
                           var textQueue     : TInterfaceList;
                               name          : TString;
                           var primNames     : TStringList;
                               )             : Integer;

var                                               
   i          : Integer;
   textDst    : IPCB_Text;
   line       : TString;
   identifier : TString;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_CreateNewText().');

   { Create new blank text. }
   textDst := PcbServer.PCBObjectFactory(eTextObject,eNoDimension,eCreate_Default);

   { Set width and desired mech layer. }
   textDst.Layer                := layer;
   textDst.Size                 := MMsToCoord(heightMm);
   textDst.Width                := MMsToCoord(widthMm);

   { Starting point for writing text. }
   textDst.Xlocation            := MMsToCoord(Xmm);
   textDst.Ylocation            := MMsToCoord(Ymm);

   { Text string itself. }
   textDst.Rotation             := angle;
   textDst.Text                 := text;

   { Name this primitive. }
   CLF_AddPrimNameToList({var prim} textDst,
                         name,
                         {var} primNames);

   { Store this new text in the queue. }
   textQueue.Add(textDst);

end; { end CLF_CreateNewText() }


{***************************************************************************
 * function CLF_CreateNewTextFpInfo()
 *  Create a new hidden text to pass information to future PCB layout helper scripts.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewTextFpInfo(    name      : TString;
                                     value     : TString;
                                 var textQueue : TInterfaceList;
                                 var primNames : TStringList;
                                     )         : Integer;
                                          
var                                       
   i          : Integer;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_CreateNewTextFpInfo().');

   { Create a hidden string (0 height, 0 line width) on courtbody mech layer. }
   CLF_CreateNewText({layer} constLayerCourtBody,
                     {widthMm} 0,
                     {heightMm} 0,
                     {Xmm} 0,
                     {Ymm} 0,
                     {angle} 0,
                     {text} (name + '=' + value),
                     {var} textQueue,
                     name,
                     {var} primNames);

end; { end CLF_CreateNewTextFpInfo() }


{***************************************************************************
 * function CLF_CreateNewTrueTypeTextByBounds()
 *  Create a new true-type text object with specified layer, etc.
 *  Given the specified bounding rectangle, compute the largest text size that
 *  will allow the text to fit.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewTrueTypeTextByBounds(    layer               : Integer;
                                               X1Mm                : Real;
                                               Y1Mm                : Real;
                                               X2Mm                : Real;
                                               Y2Mm                : Real;
                                               angle               : TAngle;
                                               text                : TString;
                                               fontName            : TPCBString;
                                               bold                : Boolean;
                                               italic              : Boolean;
                                               doCenterInX         : Boolean;
                                               doCenterInY         : Boolean;
                                           var textDst             : IPCB_Text;
                                           var textQueue           : TInterfaceList;
                                           var trackQueue          : TInterfaceList;
                                               name                : TString;
                                               identifierRectangle : TString;
                                           var primNames           : TStringList;
                                               )                   : Integer;
                                                         
var                                                      
   i                  : Integer;
   j                  : Integer;
   identifier         : TString;
   heightMm           : Real;
   heightValidMm      : Real;
   haveValidText      : Boolean;
   isInside           : Boolean;
   loopCount          : Integer;
//   AGeometricPolygon  : IPCB_GeometricPolygon;
   regionFoo          : IPCB_Region;
   regionDst          : IPCB_Region;
   bodyDst            : IPCB_ComponentBody;
   dummyRegionQueue   : TInterfaceList;
   dummyBodyQueue     : TInterfaceList;
   boundsList         : TStringList;
   boundaryWestMm     : Real;
   boundaryEastMm     : Real;
   boundaryNorthMm    : Real;
   boundarySouthMm    : Real;
   validBoundsWestMm  : Real;
   validBoundsEastMm  : Real;
   validBoundsNorthMm : Real;
   validBoundsSouthMm : Real;
   usedSpaceXmm       : Real;
   usedSpaceYmm       : Real;
   allottedSpaceXmm   : Real;
   allottedSpaceYmm   : Real;
   extraSpaceXmm      : Real;
   extraSpaceYmm      : Real;
   halfExtraSpaceXmm  : Real;
   halfExtraSpaceYmm  : Real;
   textXmm            : Real;
   textYmm            : Real;
                 
begin

   { Assume success. }
   result := 0;

   { For debug purposes, draw a rectangle around the specified bounds. }
   CLF_CreateNewTrackRectangle2(layer,
                                {widthMm} constNewWidthCourtyardMm,
                                {boundaryWestMm} X1mm,
                                {boundaryEastMm} X2mm,
                                {boundaryNorthMm} Y1mm,
                                {boundarySouthMm} Y2mm,
                                {var} trackQueue,
                                {namePrefix} identifierRectangle + '-boundary',
                                {var} primNames);

   { Compute the allotted space in x,y directions. }
   allottedSpaceXmm   := Abs(X1mm - X2mm);
   allottedSpaceYmm   := Abs(Y1mm - Y2mm);
   
   { Initialize stringlist. }
   boundsList         := TStringList.Create();   
   dummyRegionQueue   := TInterfaceList.Create();

   
   { Flag that we do not yet have a valid text size. }
   haveValidText      := False;
   validBoundsWestMm  := 0;
   validBoundsEastMm  := 0;
   validBoundsNorthMm := 0;
   validBoundsSouthMm := 0;
   
   
   WriteToDebugFile('Hello from CLF_CreateNewTrueTypeTextByBounds().');
   WriteToDebugFile('Hello from CLF_CreateNewTrueTypeTextByBounds(), layer is ' + IntToStr(layer) + ' ' + Layer2String(layer) + '.');

   
//   WriteToDebugFile(' heightMm is ' + IntToStr(heightMm) + '.');
   WriteToDebugFile(' X1mm Is ' + FloatToStr(X1mm) + '.');
   WriteToDebugFile(' Y1mm Is ' + FloatToStr(Y1mm) + '.');
   WriteToDebugFile(' X2mm Is ' + FloatToStr(X2mm) + '.');
   WriteToDebugFile(' Y2mm Is ' + FloatToStr(Y2mm) + '.');
   WriteToDebugFile(' angle is ' + IntToStr(angle) + '.');
   WriteToDebugFile(' text is "' + text + '".');
   WriteToDebugFile(' fontName is "' + fontName + '".');
   WriteToDebugFile(' bold is ' + BoolToStr(bold) + '.');
   WriteToDebugFile(' italic is ' + BoolToStr(italic) + '.');
   WriteToDebugFile(' doCenterInX is ' + BoolToStr(doCenterInX) + '.');
   WriteToDebugFile(' doCenterInY is ' + BoolToStr(doCenterInY) + '.');

   { Create new blank text. }
   textDst := PcbServer.PCBObjectFactory(eTextObject,eNoDimension,eCreate_Default);

   { Set width and desired mech layer. }
   textDst.Layer                := layer;

   { Note that TrueType fonts in Altium have no line width parameter! }
   textDst.Width                := 0;

   { Set starting point for writing text. }
   { Use the origin.  This whole thing is just to figure out the size of the resulting text,
    and it's easier to do the calculation if the text starts out in an easy place. }
   textDst.Xlocation            := MMsToCoord(0);
   textDst.Ylocation            := MMsToCoord(0);
   
   { Text string itself. }
   textDst.Rotation             := angle;
   textDst.Text                 := text;

   { Name this primitive. }
   CLF_AddPrimNameToList({var prim} textDst,
                         name,
                         {var} primNames);

   { Set text to use true type fonts. }
   textDst.SetState_UseTTFonts(True);

   { Set text to use specified true type font. }
   textDst.FontName             := fontName;

   { Set text for bold or italic flags as requested. }
   textDst.Bold                 := bold;
   textDst.Italic               := italic;


   { Initialize loop counter to 0. }
   loopCount := 0;

   { Initialize text height to something "reasonable". }
   heightMm  := const3dTextInitialHeightMm;
   
   { Loop over increasing text sizes until we exceed the specified bounds. }
   repeat
   begin

      { Try setting the text size to the now-current height. }
      textDst.Size                 := MMsToCoord(heightMm);
      WriteToDebugFile('In CLF_CreateNewTrueTypeTextByBounds(), trying heightMm of ' + FloatToStr(heightMm) + '.');
      
      {* Get bounding rectangle for text as it now stands. *}
      { Note:  I can't get textDst.BoundingRectangle or textDst.X1Location, etc.
       to actually work.  So I'm doing a totally overkill thing and actually doing
       the true type font to connected polygons conversion and obtaining a bounding
       rectangle for the union of all connected polygons. }
      
      { Create new dummy region. }
      regionFoo := PcbServer.PCBObjectFactory(eRegionObject,eNoDimension,eCreate_Default);

      { Convert text object to geometric region. }
      regionFoo.GeometricPolygon := textDst.TTTextOutlineGeometricPolygon();

      { Set identifier for regions that will be created. }
      identifier := 'dummy_region';

      { Clear dummyRegionQueue and boundsList so that those start over for this run of the repeat-until loop. }
      dummyRegionQueue.Clear();
      boundsList.Clear();

      { Call CLF_ExtrudeGeometricPolygonInto3d() to convert the geometric polygon into a series of regions. }
      CLF_ExtrudeGeometricPolygonInto3d({boardSide} 0,
                                        layer,
                                        {colorText} 0,
                                        {colorBackground} 0,
                                        {opacity} 0,
                                        {overallHeightMm} 0,
                                        {standoffHeightMm} 0,
                                        {AGeometricPolygon} regionFoo.GeometricPolygon,
                                        identifier,
                                        {doQueueRegions} True,
                                        {doQueueBodies} False,
                                        {var} bodyDst,
                                        {var regionQueue} dummyRegionQueue,
                                        {var bodyQueue} dummyBodyQueue,
                                        primNames,
                                        boundsList);
      

      { Destroy dummy region. }
      PCBServer.DestroyPCBObject(regionFoo);
         
      {* Do a cleanup step where we delete all regions, since we only wanted their bounds anyway. *}
      { Loop over all regions in the queue, backwards. }
      for i := (dummyRegionQueue.Count - 1) downto 0 do
      begin

         { Retrieve reference to queued region. }
         regionDst := dummyRegionQueue.items[i];
         
         { Destroy this primitive. }
         PCBServer.DestroyPCBObject(regionDst);
         
         { Remove this 3D region from the queue, since this was leftover from the last footprint. }
         dummyRegionQueue.Delete(i);
         
      end; { endfor }
      

      { Retrieve the bounding rectangle formed by the union of all the regions needed to make up the text. }
      CLF_RetrieveBoundingRectangleByNamePrefix({namePrefix} identifier,
                                                {var cnfGalacticInfo} boundsList,
                                                {var} boundaryWestMm,
                                                {var} boundaryEastMm,
                                                {var} boundaryNorthMm,
                                                {var} boundarySouthMm);

      WriteToDebugFile(' boundaryWestMm is ' + FloatToStr(boundaryWestMm) + '.');
      WriteToDebugFile(' boundaryEastMm is ' + FloatToStr(boundaryEastMm) + '.');
      WriteToDebugFile(' boundaryNorthMm is ' + FloatToStr(boundaryNorthMm) + '.');
      WriteToDebugFile(' boundarySouthMm is ' + FloatToStr(boundarySouthMm) + '.');

      { Compute whether we have a set of text regions that fit inside the allotted area. }
      isInside := (  ( (boundaryEastMm - boundaryWestMm) <= allottedSpaceXmm ) and
                   ( (boundaryNorthMm - boundarySouthMm) <= allottedSpaceYmm )  );
      
      
      { If the new/modified text object is inside the required bounds, then flag and record some things. }
      if (isInside) then
      begin
         
         WriteToDebugFile(' Text is inside bounding rectangle!');
         
         { Flag that we have a valid text size. }
         haveValidText      := True;

         { Record valid text height. }
         heightValidMm      := heightMm;
         validBoundsWestMm  := boundaryWestMm;
         validBoundsEastMm  := boundaryEastMm;
         validBoundsNorthMm := boundaryNorthMm;
         validBoundsSouthMm := boundarySouthMm;
         
      end;
      
      { Increase text height. }
      heightMm  := heightMm + const3dTextIncrementHeightMm;
      
      { Increment loop counter. }
      loopCount := loopCount + 1;
      
   end
      { Repeat until we have found valid text, but it's too big this time, and we're not timed out. }
   until ( (not isInside) or (loopCount > 200) );

   { Sanity check against our timeout. }
   if (loopCount > 200) then
      CLF_Abort('Unable to find valid text size for specified bounding box (timed out)!');
   
   { Sanity check against finding valid height. }
   if (not haveValidText) then
      CLF_Abort('Unable to find valid text size for specified bounding box (initial check failed)!');
   
   { Set the text size to the best height. }
   textDst.Size                 := MMsToCoord(heightValidMm);

   { Compute the amount of space used by the text. }
   { Note:  From this point forward, we must use "validBoundsXxxMm", not "boundaryXxxMm"! }
   usedSpaceXmm       := (validBoundsEastMm  - validBoundsWestMm);
   usedSpaceYmm       := (validBoundsNorthMm - validBoundsSouthMm);
   
   { Compute the amount of extra space in X & Y between text and boundary, in mm. }
   extraSpaceXmm      := (allottedSpaceXmm - usedSpaceXmm);
   extraSpaceYmm      := (allottedSpaceYmm - usedSpaceYmm);

   halfExtraSpaceXmm  := (extraSpaceXmm * 0.5);
   halfExtraSpaceYmm  := (extraSpaceYmm * 0.5);
   
   { Add x,y offsets to account for TrueType fonts existing beyond initial x,y coordinates.
    Also account for rotation.  Basically we want the left-bottom (west-south) point of the
    text to always be at the left-bottom of the boundary box, after both are rotated.}
   if (angle = 0) then
   begin

      { Calculate for the left-bottom point of the text to start at the left-bottom (west-south) point of the boundary. }
      textXmm            := ((X1mm - validBoundsWestMm));
      textYmm            := ((Y1mm - validBoundsSouthMm));

      { Add offsets in X and/or Y if we've been ordered to center the text in the boundary. }
      if (doCenterInX) then
         textXmm         := textXmm + halfExtraSpaceXmm;
         
      if (doCenterInY) then
         textYmm         := textYmm + halfExtraSpaceYmm;

   end

   else if (angle = 90) then
   begin
      
      { Calculate for the left-bottom point of the text to start at the right-bottom (east-south) point of the boundary. }
      textXmm            := ((X1mm - validBoundsWestMm) + extraSpaceXmm);
      textYmm            := ((Y1mm - validBoundsSouthMm));

      { Add offsets in X and/or Y if we've been ordered to center the text in the boundary. }
      if (doCenterInX) then
         textXmm         := textXmm - halfExtraSpaceXmm;
         
      if (doCenterInY) then
         textYmm         := textYmm + halfExtraSpaceYmm;

   end

   else if (angle = 180) then
   begin
      
      { Calculate for the left-bottom point of the text to start at the right-top (east-north) point of the boundary. }
      { Note:  Operate relative to the north boundary here. }
      textXmm            := ((X1mm - validBoundsWestMm) + extraSpaceXmm);
      textYmm            := ((Y1mm - validBoundsNorthMm));

      { Add offsets in X and/or Y if we've been ordered to center the text in the boundary. }
      if (doCenterInX) then
         textXmm         := textXmm - halfExtraSpaceXmm;
         
      if (doCenterInY) then
         textYmm         := textYmm - halfExtraSpaceYmm;

   end

   else if (angle = 270) then
   begin
      
      { Calculate for the left-bottom point of the text to start at the left-top (west-north) point of the boundary. }
      { Note:  Operate relative to the north boundary here. }
      textXmm            := ((X1mm - validBoundsWestMm));
      textYmm            := ((Y1mm - validBoundsNorthMm));

      { Add offsets in X and/or Y if we've been ordered to center the text in the boundary. }
      if (doCenterInX) then
         textXmm         := textXmm + halfExtraSpaceXmm;
         
      if (doCenterInY) then
         textYmm         := textYmm - halfExtraSpaceYmm;

   end

   else
      CLF_Abort('Unsupported angle ' + IntToStr(angle) + '!');

   { Do one last conversion from Coord to Mm. }
   textDst.Xlocation     := MMsToCoord(textXmm);
   textDst.Ylocation     := MMsToCoord(textYmm);
   
   { Store this new text in the queue. }
   textQueue.Add(textDst);

   { Free stringlist. }
   dummyRegionQueue.Free();
   boundsList.Free();

end; { end CLF_CreateNewTrueTypeTextByBounds() }


{***************************************************************************
 * function CLF_ReportTextPropertiesPartial()
 *  Report most text properties to a line that will be added to csv file.
 *  This is the "partial" function in the sense that calling functions
 *  still need to do some of the work.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportTextPropertiesPartial(    textDst    : IPCB_Text;
                                             identifier : TString;
                                         var line       : TString;
                                             )          : Integer;

var                                               
   i          : Integer;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_ReportTextPropertiesPartial()');

   {* Add information about this new text to CSV file. *}

   { Note:  We rely on calling function to have created line identifier! }

   { TODO:  For any object on top copper or bottom copper, we need to report soldermask & solderpaste expansions! }

   { Report relevant parameters. }
//   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldObjectKind, {valueStr} 'Text',                       {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldString,     {valueStr} textDst.Text,                 {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldIdentifier, {valueStr} identifier,                   {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldLayer,      {valueStr} Layer2String(textDst.Layer),  {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldTextHeight, {valueCoord} textDst.Size,               {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldX1,         {valueCoord} textDst.Xlocation,          {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldY1,         {valueCoord} textDst.Ylocation,          {var} line);
   CLF_ReportCsvPropertyInt  ({name} constCsvRptFieldRotation,   {valueInt} textDst.Rotation,             {var} line);

   { Report parameters that are only relevant for true-type fonts. }
   if (textDst.UseTTFonts) then
   begin
      CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldTextKind,   {valueStr} 'TrueType Font',           {var} line);
      
      CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldTrueTypeFontName, {valueStr} textDst.FontName,    {var} line);
      CLF_ReportCsvPropertyBool ({name} constCsvRptFieldBold,             {valueBool} textDst.Bold,       {var} line);
      CLF_ReportCsvPropertyBool ({name} constCsvRptFieldItalic,           {valueBool} textDst.Italic,     {var} line);

      { TODO:  Support inverted text. }
      CLF_ReportCsvPropertyBool ({name} constCsvRptFieldInverted,         {valueBool} False         ,     {var} line);

   end { endif }

   { Else report parameters that are only relevant for stroke font. }
   else
   begin
      CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldTextKind,   {valueStr} 'Stroke Font',                {var} line);

      CLF_ReportCsvPropertyCoord({name} constCsvRptFieldTextWidth,  {valueCoord} textDst.Width,              {var} line);
      CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldStrokeFont, {valueStr} 'Default',                    {var} line);

   end; { endelse }

   { TODO:  Support mirrored text, barcode text, inverted text. }
   
   { Report various other text related properties as default, etc. }
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldStringType, {valueStr} 'Free',                       {var} line);
   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldMirror,     {valueBool} False,                       {var} line);
   
   { Boilerplate so that things appear similarly to when we look at them in PCBLIB List. }
   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldKeepout,    {valueBool} False,                       {var} line);
   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldLocked,     {valueBool} False,                       {var} line);
   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldHide,       {valueBool} False,                       {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldNet,        {valueStr} '-',                          {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldComponent,  {valueStr} 'Free',                       {var} line);

   { Note:  We rely on calling function to add line to csv report stringlist. }
   
end; { end CLF_ReportTextPropertiesPartial() }


{***************************************************************************
 * function CLF_ReportTextProperties()
 *  Report all parameters for a specified text object.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportTextProperties(    textDst       : IPCB_Text;
                                      primNames     : TStringList;
                                  var csvReportStrs : TStringList;
                                      )             : Integer;

var                                               
   i          : Integer;
   line       : TString;
   identifier : TString;
   
begin

   { Assume success. }
   result := 0;

   {* Add information about this new text to CSV file. *}

   { Retrieve text name. }
   identifier         := CLF_GetPrimName({prim} textDst, primNames);

   { Exclude 3D text from being double reported, once as a 3D body and once as Text. }
   if (identifier <> constMagicalExclude3dTextName) then
   begin
      
      { Create line identifier. }
      line := constCsvRptPrefixText + identifier + '=';

      WriteToDebugFile('Hello from CLF_ReportTextProperties().  Layer is ' + IntToStr(textDst.Layer) + '.');

      { Report relevant parameters. }
      CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldObjectKind, {valueStr} 'Text',                       {var} line);

      { Call CLF_ReportTextPropertiesPartial() to do most of the work for us. }
      CLF_ReportTextPropertiesPartial(textDst,
                                      identifier,
                                      {var} line);

      { Add line to csv report stringlist. }
      csvReportStrs.Add(line);

   end; { endif }
   
end; { end CLF_ReportTextProperties() }


{***************************************************************************
 * function CLF_CreateNewArc()
 *  Create a new arc with the specified properties.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewArc(    layer         : Integer;
                              XCenterMm     : Real;
                              YCenterMm     : Real;
                              RadiusMm      : Real;
                              LineWidthMm   : Real;
                              StartAngleDeg : Integer;
                              EndAngleDeg   : Integer;
                              isKeepout     : Boolean;
                          var arcQueue      : TInterfaceList;
                              name          : TString;
                          var primNames     : TStringList;
                              )             : Integer;

var                                               
   i      : Integer;
   arcDst : IPCB_Arc;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_CreateNewArc().');

   { Create new blank arc. }
   arcDst := PcbServer.PCBObjectFactory(eArcObject,eNoDimension,eCreate_Default);

   { Assign properties to new arc. }
   arcDst.Layer         := layer;
   
   arcDst.XCenter       := MMsToCoord(XCenterMm);
   arcDst.YCenter       := MMsToCoord(YCenterMm);
   arcDst.Radius        := MMsToCoord(RadiusMm);
   arcDst.LineWidth     := MMsToCoord(LineWidthMm);
   arcDst.StartAngle    := StartAngleDeg;
   arcDst.EndAngle      := EndAngleDeg;

   arcDst.IsKeepout     := isKeepout;

   { Name this primitive. }
   CLF_AddPrimNameToList({var prim} arcDst,
                         name,
                         {var} primNames);

   { Store this new arc in the queue. }
   arcQueue.Add(arcDst);

end; { end CLF_CreateNewArc() }


{***************************************************************************
 * function CLF_CreateNewTrack()
 *  Create a new track with the specified properties.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewTrack(    layer      : Integer;
                                widthMm    : Real;
                                X1mm       : Real;
                                X2mm       : Real;
                                Y1mm       : Real;
                                Y2mm       : Real;
                            var trackQueue : TInterfaceList;
                                name       : TString;
                            var primNames  : TStringList;
                                )          : Integer;
                                                  
var                                               
   i        : Integer;
   trackDst : IPCB_Track;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_CreateNewTrack().');

   { Create new blank track. }
   trackDst := PcbServer.PCBObjectFactory(eTrackObject,eNoDimension,eCreate_Default);

   { Set width and desired mech layer. }
   trackDst.Layer               := layer;
   trackDst.Width               := MMsToCoord(widthMm);

   { x,y coordinates of start and end points. }
   trackDst.X1                  := MMsToCoord(X1mm);
   trackDst.X2                  := MMsToCoord(X2mm);
   trackDst.Y1                  := MMsToCoord(Y1mm);
   trackDst.Y2                  := MMsToCoord(Y2mm);

   { Name this primitive. }
   CLF_AddPrimNameToList({var prim} trackDst,
                         name,
                         {var} primNames);

   { Store this new track in the queue. }
   trackQueue.Add(trackDst);

end; { end CLF_CreateNewTrack() }


{***************************************************************************
 * function CLF_CreateNewFill()
 *  Create a new fill with the specified properties.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewFill(    layer           : Integer;
                               boundaryWestMm  : Real;
                               boundaryEastMm  : Real;
                               boundaryNorthMm : Real;
                               boundarySouthMm : Real;
                           var fillQueue       : TInterfaceList;
                               name            : TString;
                           var primNames       : TStringList;
                               )               : Integer;

var                                               
   i       : Integer;
   fillDst : IPCB_Fill;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_CreateNewFill().');

   { Create new blank fill. }
   fillDst := PcbServer.PCBObjectFactory(eFillObject,eNoDimension,eCreate_Default);

   { Set width and desired layer. }
   fillDst.Layer               := layer;

   { x,y coordinates of boundaries. }
   { Note:  By convention, try to make sure that x1,y1 is bottom left (south west)! }
   fillDst.X1Location          := MMsToCoord(boundaryWestMm);
   fillDst.Y1Location          := MMsToCoord(boundarySouthMm);
   fillDst.X2Location          := MMsToCoord(boundaryEastMm);
   fillDst.Y2Location          := MMsToCoord(boundaryNorthMm);

   { Name this primitive. }
   CLF_AddPrimNameToList({var prim} fillDst,
                         name,
                         {var} primNames);

   { Store this new fill in the queue. }
   fillQueue.Add(fillDst);

end; { end CLF_CreateNewFill() }


{***************************************************************************
 * function CLF_CopyRegionAsFill()
 *  Take a source region known to be rectangular and create a fill from it.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CopyRegionAsFill(    boardXorigin : TCoord;
                                  boardYorigin : TCoord;
                                  regionSrc    : IPCB_Region;
                              var fillDst      : IPCB_Fill;
                                  name         : TString;
                              var primNames    : TStringList;
                                  )            : Integer;

var
   i                   : Integer;
   layer               : TLayer;
   rectSrc             : TCoordRect;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_CopyRegionAsFill().  Layer is ' + IntToStr(regionSrc.Layer) + '.');

   WriteToDebugFile(' regionSrc west :      ' + CoordUnitToString(regionSrc.BoundingRectangle.left, eMetric));
   WriteToDebugFile(' regionSrc south :     ' + CoordUnitToString(regionSrc.BoundingRectangle.bottom, eMetric));
   WriteToDebugFile(' regionSrc east :      ' + CoordUnitToString(regionSrc.BoundingRectangle.right, eMetric));
   WriteToDebugFile(' regionSrc north :     ' + CoordUnitToString(regionSrc.BoundingRectangle.top, eMetric));
   WriteToDebugFile(' regionSrc HoleCount : ' + IntToStr(regionSrc.HoleCount));

   { Extract bounding rectangle from source region. }
   rectSrc := regionSrc.BoundingRectangle;

   { Sanity check. }
   if (regionSrc.HoleCount <> 0) then
      CLF_Abort('Sorry, I don''t support copying any regions that have holes!');
   
   { Create new blank fill. }
   fillDst := PcbServer.PCBObjectFactory(eFillObject,eNoDimension,eCreate_Default);

   { Copy region properties from source region to destination region. }
   fillDst.Layer              := regionSrc.Layer;

   { x,y coordinates of start and end points. }
   { Note:  By convention, try to make sure that x1,y1 is bottom left (south west)! }
   fillDst.X1Location                  := (rectSrc.left - boardXorigin);
   fillDst.X2Location                  := (rectSrc.right - boardXorigin);
   fillDst.Y1Location                  := (rectSrc.bottom - boardYorigin);
   fillDst.Y2Location                  := (rectSrc.top - boardYorigin);

   { Name this primitive. }
   CLF_AddPrimNameToList({var prim} fillDst,
                         name,
                         {var} primNames);

end; { end CLF_CopyRegionAsFill() }


{***************************************************************************
 * function CLF_ReportFillProperties()
 *  Report all known fill properties to csv file.
 *  Note:  Assumes that library component is at exact board origin!
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportFillProperties(    fillDst       : IPCB_Fill;
                                      primNames     : TStringList;
                                  var csvReportStrs : TStringList;
                                      )             : Integer;

var
   i          : Integer;
   line       : TString;
   identifier : TString;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_ReportFillProperties().  Layer is ' + IntToStr(fillDst.Layer) + '.');

   { Create line identifier. }
   identifier         := CLF_GetPrimName({prim} fillDst, primNames);
   line := constCsvRptPrefixFill + identifier + '=';

   { TODO:  For any object on top copper or bottom copper, we need to report soldermask & solderpaste expansions! }

   { Report relevant parameters. }
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldObjectKind, {valueStr} 'Fill',                       {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldIdentifier, {valueStr} identifier,                   {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldLayer,      {valueStr} Layer2String(fillDst.Layer),  {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldX1,         {valueCoord} fillDst.X1Location,         {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldY1,         {valueCoord} fillDst.Y1Location,         {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldX2,         {valueCoord} fillDst.X2Location,         {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldY2,         {valueCoord} fillDst.Y2Location,         {var} line);
   CLF_ReportCsvPropertyInt  ({name} constCsvRptFieldRotation,   {valueInt} fillDst.Rotation,             {var} line);

   { Boilerplate so that things appear similarly to when we look at them in PCBLIB List. }
   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldKeepout,    {valueBool} False,                       {var} line);
   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldLocked,     {valueBool} False,                       {var} line);
   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldHide,       {valueBool} False,                       {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldNet,        {valueStr} 'No Net',                     {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldComponent,  {valueStr} 'Free',                       {var} line);

   { Add line to csv report stringlist. }
   csvReportStrs.Add(line);

end; { end CLF_ReportFillProperties() }


{***************************************************************************
 * function CLF_CopyRegionProperties()
 *  Copy all known region properties from source region to destination region.
 *  Why oh why can't Altium provide something resembling a copy constructor???
 *  
 *  FIXME:  This function is not currently capable of copying a region's contour
 *  from regionSrc to regionDst!
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CopyRegionProperties(    boardXorigin : TCoord;
                                      boardYorigin : TCoord;
                                      regionSrc    : IPCB_Region;
                                  var regionDst    : IPCB_Region;
                                  var primNames    : TStringList;
                                      )            : Integer;

var
   i                   : Integer;
   layer               : TLayer;
   rectSrc             : TCoordRect;
   rectDst             : TCoordRect;
   regionOutlinePoints : IPCB_Contour;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_CopyRegionProperties().  Layer is ' + IntToStr(regionSrc.Layer) + '.');

   WriteToDebugFile(' regionSrc west :      ' + CoordUnitToString(regionSrc.BoundingRectangle.left, eMetric));
   WriteToDebugFile(' regionSrc south :     ' + CoordUnitToString(regionSrc.BoundingRectangle.bottom, eMetric));
   WriteToDebugFile(' regionSrc east :      ' + CoordUnitToString(regionSrc.BoundingRectangle.right, eMetric));
   WriteToDebugFile(' regionSrc north :     ' + CoordUnitToString(regionSrc.BoundingRectangle.top, eMetric));
   WriteToDebugFile(' regionSrc HoleCount : ' + IntToStr(regionSrc.HoleCount));

   { Sanity check. }
   if (regionSrc.HoleCount <> 0) then
      CLF_Abort('Sorry, I don''t support copying any regions that have holes!');
   
   { Copy region properties from source region to destination region. }
   regionDst.Layer              := regionSrc.Layer;
   
   { Extract bounding rectangle from source region. }
   rectSrc := regionSrc.BoundingRectangle;

   { These methods don't actually work, despite being documented at http://wiki.altium.com/display/ADOH/PCB+API+Design+Objects+Interfaces . }
//   rectDst.SetRegionData(rectSrc.GetRegionData());
//   rectDst.SetOutlineContour(rectSrc.GetMainContour());
   
   { So do things the harder way...
    Get the bounding rectangle from the source region.
    Then use that as the basis for creating an outline contour for the destination region. }
//   rectDst.Create();
   rectDst := regionSrc.BoundingRectangle;  { This is only necessary in order to alloc rectDst. }

   { Set destination region bounding rectangle after compensating for board origin. }
   rectDst.left   := (rectSrc.left - boardXorigin);
   rectDst.right  := (rectSrc.right - boardXorigin);
   rectDst.top    := (rectSrc.top - boardYorigin);
   rectDst.bottom := (rectSrc.bottom - boardYorigin);

   { Create a new contour to describe the new region. }
   regionOutlinePoints   := PCBServer.PCBContourFactory;

   { Add outline points to the rectangular shape of the source region. }
   regionOutlinePoints.AddPoint(rectDst.left, rectDst.top);
   regionOutlinePoints.AddPoint(rectDst.left, rectDst.bottom);
   regionOutlinePoints.AddPoint(rectDst.right, rectDst.bottom);
   regionOutlinePoints.AddPoint(rectDst.right, rectDst.top);
   
   regionDst.SetOutlineContour(regionOutlinePoints);


   WriteToDebugFile(' regionDst west :  ' + CoordUnitToString(regionDst.BoundingRectangle.left, eMetric));
   WriteToDebugFile(' regionDst south : ' + CoordUnitToString(regionDst.BoundingRectangle.bottom, eMetric));
   WriteToDebugFile(' regionDst east :  ' + CoordUnitToString(regionDst.BoundingRectangle.right, eMetric));
   WriteToDebugFile(' regionDst north : ' + CoordUnitToString(regionDst.BoundingRectangle.top, eMetric));
   
   { TODO:  Are there other properties that we need to copy??? }

   
   { Name this primitive. }
   CLF_AddPrimNameToList({var prim} regionDst,
                         {name} 'Unknown_region_from_LPW_' + Layer2String(regionSrc.Layer),
                         {var} primNames);

end; { end CLF_CopyRegionProperties() }


{***************************************************************************
 * function CLF_ReportRegionProperties()
 *  Report all known region properties to csv file.
 *  Note:  Assumes that library component is at exact board origin!
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportRegionProperties(    regionDst     : IPCB_Region;
                                        primNames     : TStringList;
                                    var csvReportStrs : TStringList;
                                        )             : Integer;

var
   i          : Integer;
   line       : TString;
   identifier : TString;
   rectDst    : TCoordRect;
              
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_ReportRegionProperties().  Layer is ' + IntToStr(regionDst.Layer) + '.');

   { Create line identifier. }
   identifier         := CLF_GetPrimName({prim} regionDst, primNames);
   line := constCsvRptPrefixRegion + identifier + '=';

   { Make sure we're not expected to suppress reporting this region because it's part of a 3D region. }
   if (identifier <> constMagicalExclude3dRegionName) then
   begin
      
      { Extract bounding rectangle from region. }
      rectDst := regionDst.BoundingRectangle;

      { TODO:  For any object on top copper or bottom copper, we need to report soldermask & solderpaste expansions! }

      { Report relevant parameters. }
      CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldObjectKind, {valueStr} 'Region',                     {var} line);
      CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldIdentifier, {valueStr} identifier,                   {var} line);
      CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldLayer,      {valueStr} Layer2String(regionDst.Layer),{var} line);
      CLF_ReportCsvPropertyCoord({name} constCsvRptFieldX1,         {valueCoord} rectDst.left,               {var} line);
      CLF_ReportCsvPropertyCoord({name} constCsvRptFieldY1,         {valueCoord} rectDst.bottom,             {var} line);
      CLF_ReportCsvPropertyCoord({name} constCsvRptFieldX2,         {valueCoord} rectDst.right,              {var} line);
      CLF_ReportCsvPropertyCoord({name} constCsvRptFieldY2,         {valueCoord} rectDst.top,                {var} line);

      { Boilerplate so that things appear similarly to when we look at them in PCBLIB List. }
      CLF_ReportCsvPropertyBool ({name} constCsvRptFieldKeepout,    {valueBool} False,                       {var} line);
      CLF_ReportCsvPropertyBool ({name} constCsvRptFieldLocked,     {valueBool} False,                       {var} line);
      CLF_ReportCsvPropertyBool ({name} constCsvRptFieldHide,       {valueBool} False,                       {var} line);
      CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldNet,        {valueStr} 'No Net',                     {var} line);
      CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldComponent,  {valueStr} 'Free',                       {var} line);

      { Add line to csv report stringlist. }
      csvReportStrs.Add(line);

   end; { endif }

end; { end CLF_ReportRegionProperties() }


{***************************************************************************
 * function CLF_CopyPadProperties()
 *  Copy all known pad properties from source pad to destination pad.
 *  Why oh why can't Altium provide something resembling a copy constructor???
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CopyPadProperties(    boardXorigin : TCoord;
                                   boardYorigin : TCoord;
                                   padSrc       : IPCB_Pad;
                               var padDst       : IPCB_Pad;
                               var primNames    : TStringList;
                                   )            : Integer;

var
   i        : Integer;
   layer    : TLayer;
   padCache : TPadCache;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_CopyPadProperties().  X is ' + IntToStr(CoordToMMs(padSrc.X)) + 'mm, Y is ' + IntToStr(CoordToMMs(padSrc.Y)) + 'mm.');


   { Copy pad properties from source pad to destination pad. }
   padDst.Layer                 := padSrc.Layer;

   padDst.X                 := (padSrc.X - boardXorigin);
   padDst.Y                 := (padSrc.Y - boardYorigin);
//   padDst.PinDescriptor       := PadSrc.PinDescriptor;
//   padDst.IsConnectedToPlane:= PadSrc.IsConnectedToPlane;
   padDst.Mode              := PadSrc.Mode;

   { Copy all the pad cache properties in one fell swoop. }
   padCache := padSrc.GetState_Cache;
   padDst.SetState_Cache := padCache;
   
   
//   padDst.Cache.SolderMaskExpansionValid  := padSrc.Cache.SolderMaskExpansionValid;
//   padDst.Cache.SolderMaskExpansion       := padSrc.Cache.SolderMaskExpansion;
//   padDst.Cache.PasteMaskExpansionValid   := padSrc.Cache.PasteMaskExpansionValid;
//   padDst.Cache.PasteMaskExpansion        := padSrc.Cache.PasteMaskExpansion;

   
//   { Loop over all known layers. }
//   for layer := eTopLayer to eBottomLayer do
//   begin
//      
//      PadDst.XSizeOnLayer[layer]      := PadSrc.XSizeOnLayer[layer];
//      PadDst.YSizeOnLayer[layer]      := PadSrc.YSizeOnLayer[layer];
//      PadDst.ShapeOnLayer[layer]      := PadSrc.ShapeOnLayer[layer];
//      PadDst.XStackSizeOnLayer[layer] := PadSrc.XStackSizeOnLayer[layer];
//      PadDst.YStackSizeOnLayer[layer] := PadSrc.YStackSizeOnLayer[layer];
//      PadDst.StackShapeOnLayer[layer] := PadSrc.StackShapeOnLayer[layer];
//   PadDst.WidthOnLayer        := PadSrc.WidthOnLayer;
//   end;

   { Loop over all known layers. }
   {***************************************************************************
    * BEGIN code borrowed from FormatPaintBrush.pas.
    ***************************************************************************}
   for Layer := eTopLayer to eBottomLayer Do
   begin
      padDst.StackShapeOnLayer[Layer]     := padSrc.StackShapeOnLayer[Layer];
      padDst.StackCRPctOnLayer[Layer]     := padSrc.StackCRPctOnLayer[Layer];

      padDst.XStackSizeOnLayer[Layer]     := padSrc.XStackSizeOnLayer[Layer];
      padDst.YStackSizeOnLayer[Layer]     := padSrc.YStackSizeOnLayer[Layer];

      padDst.XPadOffset[Layer]            := padSrc.XPadOffset[Layer];
      padDst.YPadOffset[Layer]            := padSrc.YPadOffset[Layer];

   end;
   {***************************************************************************
    * END code borrowed from FormatPaintBrush.pas.
    ***************************************************************************}
   
   padDst.TopXSize          := PadSrc.TopXSize;
   padDst.TopYSize          := PadSrc.TopYSize;
   padDst.MidXSize          := PadSrc.MidXSize;
   padDst.MidYSize          := PadSrc.MidYSize;
   padDst.BotXSize          := PadSrc.BotXSize;
   padDst.BotYSize          := PadSrc.BotYSize;
   padDst.TopShape          := PadSrc.TopShape;
   padDst.MidShape          := PadSrc.MidShape;
   padDst.BotShape          := PadSrc.BotShape;
   padDst.HoleSize          := PadSrc.HoleSize;
   padDst.Rotation          := PadSrc.Rotation;
   padDst.Name              := PadSrc.Name;
//   padDst.Width               := PadSrc.Width;
//   padDst.SwapID_Pad      := PadSrc.SwapID_Pad;
//   padDst.SwapID_Gate     := PadSrc.SwapID_Gate;
//   padDst.SwappedPadName  := PadSrc.SwappedPadName;
   padDst.Cache             := PadSrc.Cache;
   padDst.OwnerPart_ID      := PadSrc.OwnerPart_ID;
   padDst.Plated            := PadSrc.Plated;
   padDst.DrillType         := PadSrc.DrillType;
   padDst.HoleType          := PadSrc.HoleType;
   padDst.HoleWidth         := PadSrc.HoleWidth;
   padDst.HoleRotation      := PadSrc.HoleRotation;

end; { end CLF_CopyPadProperties() }


{***************************************************************************
 * function CLF_ReportPadProperties()
 *  Report all known pad properties to csv file.
 *  Note:  Assumes that library component is at exact board origin!
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportPadProperties(    padDst        : IPCB_Pad;
                                     primNames     : TStringList;
                                 var csvReportStrs : TStringList;
                                     )             : Integer;

var
   i          : Integer;
   line       : TString;
   identifier : TString;
   padCache   : TPadCache;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_ReportPadProperties().  Layer is ' + IntToStr(padDst.Layer) + '.');

   { Create line identifier. }
   { Use a sortable (eg. with leading 0's prepended) version of the pad name. }
   identifier         := CLF_GetSortablePadName({padName} padDst.Name);
   line := constCsvRptPrefixPad + identifier + '=';

   { Copy all the pad cache properties in one fell swoop. }
   padCache := padDst.GetState_Cache;

   { Report pad group name in the String field. }
   CLF_ReportCsvPropertyStr        ({name} constCsvRptFieldString,     {valueStr} CLF_GetPadGroupNamePrefix({padGroupNum} CLF_GetPadGroupNum(padDst), primNames),  {var} line);

   { Report relevant parameters. }
   CLF_ReportCsvPropertyStr        ({name} constCsvRptFieldObjectKind, {valueStr} 'Pad',                      {var} line);
   CLF_ReportCsvPropertyStr        ({name} constCsvRptFieldIdentifier, {valueStr} padDst.Name,                {var} line);
   CLF_ReportCsvPropertyStr        ({name} constCsvRptFieldLayer,      {valueStr} Layer2String(padDst.Layer), {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldX1,         {valueCoord} padDst.X,                 {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldY1,         {valueCoord} padDst.Y,                 {var} line);
   CLF_ReportCsvPropertyInt        ({name} constCsvRptFieldRotation,   {valueInt} padDst.Rotation,             {var} line);

   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldHoleSize,   {valueCoord} padDst.HoleSize,            {var} line);
   CLF_ReportCsvPropertyTExtendedDrillType({name} constCsvRptFieldDrillType, {valueCoord} padDst.DrillType,    {var} line);
   CLF_ReportCsvPropertyTExtendedHoleType({name} constCsvRptFieldHoleType,   {valueCoord} padDst.HoleType,    {var} line);
   CLF_ReportCsvPropertyInt        ({name} constCsvRptFieldHoleRotation,     {valueInt} padDst.HoleRotation,             {var} line);

   { TODO: Add HoleWidth for square holes! }
//   constCsvRptFieldHoleWidth                          = '090_HoleWidth';

   
   { Report soldermask and solderpaste expansion properties. }
   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldSolderpasteOverride,      {valueBool} (padCache.PasteMaskExpansionValid = eCacheManual), {var} line);
   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldSoldermaskOverride,       {valueBool} (padCache.SolderMaskExpansionValid = eCacheManual), {var} line);

   CLF_ReportCsvPropertyTCacheState({name} constCsvRptFieldSolderpasteExpansionMode, {valueEnum} padCache.PasteMaskExpansionValid,            {var} line);
   CLF_ReportCsvPropertyTCacheState({name} constCsvRptFieldSoldermaskExpansionMode,  {valueEnum} padCache.SolderMaskExpansionValid,           {var} line);

   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldSolderpasteExpansion,     {valueCoord} padCache.PasteMaskExpansion,                {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldSoldermaskExpansion,      {valueCoord} padCache.SolderMaskExpansion,               {var} line);

   { Report tenting properties. }
//   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldSoldermaskTentingTop,     {valueBool} padDst.IsTentingTop(),                         {var} line);
//   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldSoldermaskTentingBot,     {valueBool} padDst.IsTentingBottom(),                      {var} line);

   { FIXME:  How can I actually read these properties???? }
   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldSoldermaskTentingTop,     {valueBool} False,                                         {var} line);
   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldSoldermaskTentingBot,     {valueBool} False,                                         {var} line);
   
   { Report test point properties. }
   { TODO:  How can I tell the difference between a fab and assembly test point??  There is only one test point class member! }
//   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldFabTestPointTop,          {valueBool} padDst.IsTestPointTop,                         {var} line);
//   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldFabTestPointBot,          {valueBool} padDst.IsTestPointBottom,                      {var} line);
//   
//   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldAssyTestPointTop,         {valueBool} padDst.IsAssyTestPointTop,                     {var} line);
//   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldAssyTestPointBot,         {valueBool} padDst.IsAssyTestPointBottom,                  {var} line);
//   
//   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldAssyTestPointTop,         {valueBool} False,                                         {var} line);
//   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldAssyTestPointBot,         {valueBool} False,                                         {var} line);

   { FIXME:  How can I actually read these properties???? }
   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldFabTestPointTop,          {valueBool} False,                                         {var} line);
   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldFabTestPointBot,          {valueBool} False,                                         {var} line);
   
   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldAssyTestPointTop,         {valueBool} False,                                         {var} line);
   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldAssyTestPointBot,         {valueBool} False,                                         {var} line);
   
   { Report other pad properties. }
   { TODO:  How do I find electrical type in data structure?? }
   { TODO:  How do I find jumper ID in data structure?? }
   CLF_ReportCsvPropertyStr        ({name} constCsvRptFieldElectricalType,           {valueStr} 'Load',                      {var} line);
   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldPlated,                   {valueBool} padDst.Plated,              {var} line);
   CLF_ReportCsvPropertyInt        ({name} constCsvRptFieldPadJumperID,              {valueInt} 0,                           {var} line);

   { Report stack mode. }
   CLF_ReportCsvPropertyTPadMode   ({name} constCsvRptFieldStackMode,  {valueEnum} padDst.Mode,                 {var} line);

   { For simple mode pads, report the pad shapes, etc. as PadAll. }
   if (padDst.Mode = ePadMode_Simple) then
   begin

      CLF_ReportCsvPropertyTShape  ({name} constCsvRptFieldPadShapeAll,{valueEnum} padDst.TopShape,             {var} line);
      CLF_ReportCsvPropertyCoord   ({name} constCsvRptFieldPadXSizeAll,{valueCoord} padDst.TopXSize,            {var} line);
      CLF_ReportCsvPropertyCoord   ({name} constCsvRptFieldPadYSizeAll,{valueCoord} padDst.TopYSize,            {var} line);

   end;

   { Report x,y pad offsets on each layer. }
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetTop,            {valueCoord} padDst.XPadOffset[eTopLayer],   {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetTop,            {valueCoord} padDst.YPadOffset[eTopLayer],   {var} line);

   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid1,           {valueCoord} padDst.XPadOffset[eMidLayer1],  {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid1,           {valueCoord} padDst.YPadOffset[eMidLayer1],  {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid2,           {valueCoord} padDst.XPadOffset[eMidLayer2],  {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid2,           {valueCoord} padDst.YPadOffset[eMidLayer2],  {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid3,           {valueCoord} padDst.XPadOffset[eMidLayer3],  {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid3,           {valueCoord} padDst.YPadOffset[eMidLayer3],  {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid4,           {valueCoord} padDst.XPadOffset[eMidLayer4],  {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid4,           {valueCoord} padDst.YPadOffset[eMidLayer4],  {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid5,           {valueCoord} padDst.XPadOffset[eMidLayer5],  {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid5,           {valueCoord} padDst.YPadOffset[eMidLayer5],  {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid6,           {valueCoord} padDst.XPadOffset[eMidLayer6],  {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid6,           {valueCoord} padDst.YPadOffset[eMidLayer6],  {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid7,           {valueCoord} padDst.XPadOffset[eMidLayer7],  {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid7,           {valueCoord} padDst.YPadOffset[eMidLayer7],  {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid8,           {valueCoord} padDst.XPadOffset[eMidLayer8],  {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid8,           {valueCoord} padDst.YPadOffset[eMidLayer8],  {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid9,           {valueCoord} padDst.XPadOffset[eMidLayer9],  {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid9,           {valueCoord} padDst.YPadOffset[eMidLayer9],  {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid10,          {valueCoord} padDst.XPadOffset[eMidLayer10], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid10,          {valueCoord} padDst.YPadOffset[eMidLayer10], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid11,          {valueCoord} padDst.XPadOffset[eMidLayer11], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid11,          {valueCoord} padDst.YPadOffset[eMidLayer11], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid12,          {valueCoord} padDst.XPadOffset[eMidLayer12], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid12,          {valueCoord} padDst.YPadOffset[eMidLayer12], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid13,          {valueCoord} padDst.XPadOffset[eMidLayer13], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid13,          {valueCoord} padDst.YPadOffset[eMidLayer13], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid14,          {valueCoord} padDst.XPadOffset[eMidLayer14], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid14,          {valueCoord} padDst.YPadOffset[eMidLayer14], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid15,          {valueCoord} padDst.XPadOffset[eMidLayer15], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid15,          {valueCoord} padDst.YPadOffset[eMidLayer15], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid16,          {valueCoord} padDst.XPadOffset[eMidLayer16], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid16,          {valueCoord} padDst.YPadOffset[eMidLayer16], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid17,          {valueCoord} padDst.XPadOffset[eMidLayer17], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid17,          {valueCoord} padDst.YPadOffset[eMidLayer17], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid18,          {valueCoord} padDst.XPadOffset[eMidLayer18], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid18,          {valueCoord} padDst.YPadOffset[eMidLayer18], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid19,          {valueCoord} padDst.XPadOffset[eMidLayer19], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid19,          {valueCoord} padDst.YPadOffset[eMidLayer19], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid20,          {valueCoord} padDst.XPadOffset[eMidLayer20], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid20,          {valueCoord} padDst.YPadOffset[eMidLayer20], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid21,          {valueCoord} padDst.XPadOffset[eMidLayer21], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid21,          {valueCoord} padDst.YPadOffset[eMidLayer21], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid22,          {valueCoord} padDst.XPadOffset[eMidLayer22], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid22,          {valueCoord} padDst.YPadOffset[eMidLayer22], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid23,          {valueCoord} padDst.XPadOffset[eMidLayer23], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid23,          {valueCoord} padDst.YPadOffset[eMidLayer23], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid24,          {valueCoord} padDst.XPadOffset[eMidLayer24], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid24,          {valueCoord} padDst.YPadOffset[eMidLayer24], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid25,          {valueCoord} padDst.XPadOffset[eMidLayer25], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid25,          {valueCoord} padDst.YPadOffset[eMidLayer25], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid26,          {valueCoord} padDst.XPadOffset[eMidLayer26], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid26,          {valueCoord} padDst.YPadOffset[eMidLayer26], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid27,          {valueCoord} padDst.XPadOffset[eMidLayer27], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid27,          {valueCoord} padDst.YPadOffset[eMidLayer27], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid28,          {valueCoord} padDst.XPadOffset[eMidLayer28], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid28,          {valueCoord} padDst.YPadOffset[eMidLayer28], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid29,          {valueCoord} padDst.XPadOffset[eMidLayer29], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid29,          {valueCoord} padDst.YPadOffset[eMidLayer29], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetMid30,          {valueCoord} padDst.XPadOffset[eMidLayer30], {var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetMid30,          {valueCoord} padDst.YPadOffset[eMidLayer30], {var} line);
   
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadXOffsetBot,            {valueCoord} padDst.XPadOffset[eBottomLayer],{var} line);
   CLF_ReportCsvPropertyCoord      ({name} constCsvRptFieldPadYOffsetBot,            {valueCoord} padDst.YPadOffset[eBottomLayer],{var} line);

   
   { Boilerplate so that things appear similarly to when we look at them in PCBLIB List. }
   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldLocked,     {valueBool} False,                       {var} line);
   CLF_ReportCsvPropertyBool       ({name} constCsvRptFieldHide,       {valueBool} False,                       {var} line);
   CLF_ReportCsvPropertyStr        ({name} constCsvRptFieldNet,        {valueStr} 'No Net',                     {var} line);
   CLF_ReportCsvPropertyStr        ({name} constCsvRptFieldComponent,  {valueStr} 'Free',                       {var} line);

   if ( (padDst.X = 0) and (padDst.Y = 0) ) then
      CLF_ReportCsvPropertyInt      ({name} constCsvRptFieldPadCornerRadTop, {valueFloat} padDst.GetState_StackCRPctOnLayer(eTopLayer), {var} line);

   { FIXME:  Support lots and lots of other pad properties!  Support thru-hole pads!! }
   
   { Add line to csv report stringlist. }
   csvReportStrs.Add(line);
   
end; { end CLF_ReportPadProperties() }


{***************************************************************************
 * function CLF_CopyTrackProperties()
 *  Copy all known track properties from source track to destination track.
 *  Why oh why can't Altium provide something resembling a copy constructor???
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CopyTrackProperties(    boardXorigin : TCoord;
                                     boardYorigin : TCoord;
                                     trackSrc     : IPCB_Track;
                                 var trackDst     : IPCB_Track;
                                 var primNames    : TStringList;
                                     )            : Integer;

var
   i     : Integer;
   layer : TLayer;
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_CopyTrackProperties().  Layer is ' + IntToStr(trackSrc.Layer) + '.');

   { Replicate source track. }
//   trackDst := trackSrc.Replicate();

//   { Move trackDst in x,y to remove annoying boardOrigin terms. }
//   trackDst.MoveByXY(-1 * boardXorigin,
//                     -1 * boardYorigin);
   
//   trackDst.MoveToXY(0, 0);
   
   { Copy track properties from source track to destination track. }
   trackDst.Layer               := trackSrc.Layer;

   { Make sure we don't have any annoying rounding errors between mm and coord representations
    of these track points in the future, by forcing an unnecessary rounding now. }
   trackDst.X1                  := MMsToCoord(CoordToMMs(trackSrc.X1 - boardXorigin));
   trackDst.Y1                  := MMsToCoord(CoordToMMs(trackSrc.Y1 - boardYorigin));
   trackDst.X2                  := MMsToCoord(CoordToMMs(trackSrc.X2 - boardXorigin));
   trackDst.Y2                  := MMsToCoord(CoordToMMs(trackSrc.Y2 - boardYorigin));
   trackDst.Width               := trackSrc.Width;

   { Name this primitive. }
   CLF_AddPrimNameToList({var prim} trackDst,
                         {name} 'Unknown_track_from_LPW_' + Layer2String(trackSrc.Layer),
                         {var} primNames);

end; { end CLF_CopyTrackProperties() }


{***************************************************************************
 * function CLF_ReportTrackProperties()
 *  Report all known track properties to csv file.
 *  Note:  Assumes that library component is at exact board origin!
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportTrackProperties(    trackDst      : IPCB_Track;
                                       primNames     : TStringList;
                                   var csvReportStrs : TStringList;
                                       )             : Integer;

var
   i          : Integer;
   line       : TString;
   identifier : TString;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_ReportTrackProperties().  Layer is ' + IntToStr(trackDst.Layer) + '.');

   { Create line identifier. }
   identifier         := CLF_GetPrimName({prim} trackDst, primNames);
   line := constCsvRptPrefixTrack + identifier + '=';

   { TODO:  For any object on top copper or bottom copper, we need to report soldermask & solderpaste expansions! }

   { Report relevant parameters. }
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldObjectKind, {valueStr} 'Track',                      {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldIdentifier, {valueStr} identifier,                   {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldLayer,      {valueStr} Layer2String(trackDst.Layer), {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldWidth,      {valueCoord} trackDst.Width,             {var} line);

   { Note:  By convention, we try to report (x1,y1) as the bottom-left (south-west) corner! }
   { See if x1 is to the west (left) of x2.  If so, report (x1,y1) & (x2,y2) as is. }
   if (  (trackDst.X1 < trackDst.X2) or
       ( (trackDst.X1 = trackDst.X2) and (trackDst.Y1 < trackDst.Y2) )  ) then
   begin
      CLF_ReportCsvPropertyCoord({name} constCsvRptFieldX1,         {valueCoord} trackDst.X1,                {var} line);
      CLF_ReportCsvPropertyCoord({name} constCsvRptFieldY1,         {valueCoord} trackDst.Y1,                {var} line);
      CLF_ReportCsvPropertyCoord({name} constCsvRptFieldX2,         {valueCoord} trackDst.X2,                {var} line);
      CLF_ReportCsvPropertyCoord({name} constCsvRptFieldY2,         {valueCoord} trackDst.Y2,                {var} line);
   end

   { Else swap (x1,y1) and (x2,y2) for purposes of reporting to csv file only. }
   else
   begin
      CLF_ReportCsvPropertyCoord({name} constCsvRptFieldX1,         {valueCoord} trackDst.X2,                {var} line);
      CLF_ReportCsvPropertyCoord({name} constCsvRptFieldY1,         {valueCoord} trackDst.Y2,                {var} line);
      CLF_ReportCsvPropertyCoord({name} constCsvRptFieldX2,         {valueCoord} trackDst.X1,                {var} line);
      CLF_ReportCsvPropertyCoord({name} constCsvRptFieldY2,         {valueCoord} trackDst.Y1,                {var} line);
   end;

   { Boilerplate so that things appear similarly to when we look at them in PCBLIB List. }
   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldKeepout,    {valueBool} False,                       {var} line);
   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldLocked,     {valueBool} False,                       {var} line);
   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldHide,       {valueBool} False,                       {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldNet,        {valueStr} 'No Net',                     {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldComponent,  {valueStr} 'Free',                       {var} line);

   { Add line to csv report stringlist. }
   csvReportStrs.Add(line);

end; { end CLF_ReportTrackProperties() }


{***************************************************************************
 * function CLF_CopyArcProperties()
 *  Copy all known arc properties from source arc to destination arc.
 *  Why oh why can't Altium provide something resembling a copy constructor???
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CopyArcProperties(    boardXorigin : TCoord;
                                   boardYorigin : TCoord;
                                   arcSrc       : IPCB_Arc;
                               var arcDst       : IPCB_Arc;
                               var primNames    : TStringList;
                                   )            : Integer;

var
   i     : Integer;
   layer : TLayer;
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_CopyArcProperties().  Layer is ' + IntToStr(arcSrc.Layer) + '.');

   { Copy arc properties from source arc to destination arc. }
   arcDst.Layer             := arcSrc.Layer;
   
   arcDst.XCenter           := (arcSrc.XCenter - boardXorigin);
   arcDst.YCenter           := (arcSrc.YCenter - boardYorigin);
   arcDst.Radius            := arcSrc.Radius;
   arcDst.LineWidth         := arcSrc.LineWidth;
   arcDst.StartAngle        := arcSrc.StartAngle;
   arcDst.EndAngle          := arcSrc.EndAngle;
   arcDst.IsKeepout         := arcSrc.IsKeepout;
//   arcDst.StartX          := (arcSrc.StartX - boardXorigin);
//   arcDst.StartY          := (arcSrc.StartY - boardYorigin);
//   arcDst.EndX                := (arcSrc.EndX - boardXorigin);
//   arcDst.EndY                := (arcSrc.EndY - boardYorigin);
   
   { Name this primitive. }
   CLF_AddPrimNameToList({var prim} arcDst,
                         {name} 'Unknown_arc_from_LPW_' + Layer2String(arcSrc.Layer),
                         {var} primNames);

end; { end CLF_CopyArcProperties() }


{***************************************************************************
 * function CLF_ReportArcProperties()
 *  Report all known arc properties to csv file.
 *  Note:  Assumes that library component is at exact board origin!
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportArcProperties(    arcDst        : IPCB_Arc;
                                     primNames     : TStringList;
                                 var csvReportStrs : TStringList;
                                     )             : Integer;

var
   i          : Integer;
   line       : TString;
   identifier : TString;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_ReportArcProperties().  Layer is ' + IntToStr(arcDst.Layer) + '.');

   { Create line identifier. }
   identifier         := CLF_GetPrimName({prim} arcDst, primNames);
   line := constCsvRptPrefixArc + identifier + '=';

   { TODO:  For any object on top copper or bottom copper, we need to report soldermask & solderpaste expansions! }

   { Report relevant parameters. }
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldObjectKind, {valueStr} 'Arc',                        {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldIdentifier, {valueStr} identifier,                   {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldLayer,      {valueStr} Layer2String(arcDst.Layer),   {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldWidth,      {valueCoord} arcDst.LineWidth,           {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldX1,         {valueCoord} arcDst.XCenter,             {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldY1,         {valueCoord} arcDst.YCenter,             {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldRadius,     {valueCoord} arcDst.Radius,              {var} line);
   CLF_ReportCsvPropertyInt  ({name} constCsvRptFieldStartAngle, {valueInt} arcDst.StartAngle,            {var} line);
   CLF_ReportCsvPropertyInt  ({name} constCsvRptFieldEndAngle,   {valueInt} arcDst.EndAngle,              {var} line);
   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldKeepout,    {valueBool} arcDst.IsKeepout,            {var} line);

   { Boilerplate so that things appear similarly to when we look at them in PCBLIB List. }
   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldLocked,     {valueBool} False,                       {var} line);
   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldHide,       {valueBool} False,                       {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldNet,        {valueStr} 'No Net',                     {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldComponent,  {valueStr} 'Free',                       {var} line);

   { Add line to csv report stringlist. }
   csvReportStrs.Add(line);

end; { end CLF_ReportArcProperties() }


{***************************************************************************
 * function CLF_ModifyAndSuppressRegions()
 *  Modify any regions that we want to change as we copy things to destination
 *  library component.  Also decide if there are some regions that we wish
 *  to suppress altogether (eg. not copy to destination).
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ModifyAndSuppressRegions(    boardXorigin    : TCoord;
                                          boardYorigin    : TCoord;
                                          regionSrc       : IPCB_Region;
                                      var regionDst       : IPCB_Region;
                                      var primNames       : TStringList;
                                          padQueue        : TInterfaceList;
                                      var fillQueue       : TInterfaceList;
                                      var doCopy          : Boolean;
                                      var numEpRegions    : Integer;
                                      var cnfGalacticInfo : TStringList;
                                          )               : Integer;
var                                                       
   i                            : Integer;
   hasEp                        : Boolean;
   nameValuePair                : TString;
   namePrefix                   : TString;
   rectDst                      : TCoordRect;
   isInside                     : Boolean;
   padDst                       : IPCB_Pad;
   fillDst                      : IPCB_Fill;
   regionXcoord                 : TCoord;
   regionYcoord                 : TCoord;
   regionKnown                  : Boolean;
   epLeftBoundary               : Real;
   epRightBoundary              : Real;
   epBottomBoundary             : Real;
   epTopBoundary                : Real;
   epWidthRounded               : Real;
   epLengthRounded              : Real;
   regionWidth                  : Real;
   regionLength                 : Real;
   xCenter                      : Real;
   yCenter                      : Real;
   solderpastePct               : Real;
   epArea                       : Real;
   expectedSolderpasteArea      : Real;
   regionArea                   : Real;
   epChamfer                    : Real;
   chamferArea                  : Real;
   pasteExpansion               : Real;
   foundSingleSolderPasteRegion : Boolean;
   newSolderPasteBoundaryLeft   : Real;
   newSolderPasteBoundaryRight  : Real;
   newSolderPasteBoundaryTop    : Real;
   newSolderPasteBoundaryBottom : Real;
   hasDshapePads                : Boolean;
   betweenX                     : Boolean;
   betweenY                     : Boolean;
   padRight                     : Real;
   padLeft                      : Real;
   padTop                       : Real;
   padBottom                    : Real;
   regionTotalHeight            : Real;
   regionTotalWidth             : Real;
   
begin

   WriteToDebugFile('Hello from CLF_ModifyAndSuppressRegions()!');
   
   { Assume success. }
   result := 0;
   
   { Assume that we want to copy this object to destination library. }
   doCopy := True;

   { Assume that we will not find a single solderpaste region. }
   foundSingleSolderPasteRegion := False;

   { Flag that until we determine otherwise, we don't know what this region is. }
   regionKnown       := False;
   
   { Extract bounding rectangle from destination region. }
   rectDst := regionDst.BoundingRectangle;

   { Retrieve whether this package is known to have an exposed pad (EP). }
   hasEp            := StrToBool(cnfGalacticInfo.Values(constGilHasEp));


   { We only want to do these next operations for footprints that have an EP pad. }
   if (hasEp) then
   begin
      
      { Retrieve name prefix for the EP pad group. }
      namePrefix        := CLF_GetPadGroupNamePrefix(constPadGroupEp, primNames);

      { See if the region is within the bounds of the EP pad(s) for this footprint. }
      CLF_IsInsideBoundingRectangle({X1Mm} CoordToMMs(rectDst.left),
                                    {Y1Mm} CoordToMMs(rectDst.top),
                                    {X2Mm} CoordToMMs(rectDst.right),
                                    {Y2Mm} CoordToMMs(rectDst.bottom),
                                    namePrefix,
                                    {var} cnfGalacticInfo,
                                    {var} isInside);

      //WriteToDebugFile('In CLF_ModifyAndSuppressRegions(), isInside is ' + BoolToStr(isInside) + '.');
      //WriteToDebugFile('In CLF_ModifyAndSuppressRegions(), regionSrc.Layer is ' + BoolToStr(regionSrc.Layer = eKeepOutLayer) + '.');
      
      
      { Retrieve rounded EP length and width from cnfGalacticInfo to determine which regions we will not copy. }
      epWidthRounded :=  StrToFloat(cnfGalacticInfo.Values(constGilEpWidthRounded));
      epLengthRounded :=  StrToFloat(cnfGalacticInfo.Values(constGilEpLengthRounded));
      
      { Compute the boundaries of EP so that it can be compared to the boundaries of the current region. }
      { FIXME: THIS IS CURRENTLY ASSUMING THE EP IS CENTERED AT (0,0) }
      epLeftBoundary := (-1) * (epWidthRounded / 2.0);
      epRightBoundary := (epWidthRounded / 2.0);
      epBottomBoundary := (-1) * (epLengthRounded / 2.0);
      epTopBoundary := (epLengthRounded / 2.0);
      
      WriteToDebugFile('EP boundaries: ' + FloatToStr(epLeftBoundary) + ' and ' + FloatToStr(epTopBoundary) + ' and ' + FloatToStr(epRightBoundary) + ' and ' + FloatToStr(epBottomBoundary));
      
      {* Perform any changes we want to make to regions before we do the copy to destination library. *}
      { Look for solderpaste regions that are tiled on EP (exposed pad) pad. }
      if ( (regionSrc.Layer = eKeepOutLayer) and (isInside) ) then
      begin

         { Suppress this region as such, since we're either going to suppress it or turn it into a fill. }
         doCopy := False;

         //WriteToDebugFile(FloatToStr(epLeftBoundary) + ' and ' + FloatToStr(epTopBoundary) + ' and ' + FloatToStr(epRightBoundary) + ' and ' + FloatToStr(epBottomBoundary));
         //WriteToDebugFile(FloatToStr(CoordToMMs(rectDst.left)) + ' and ' + FloatToStr(CoordToMMs(rectDst.top)) + ' and ' + FloatToStr(CoordToMMs(rectDst.right)) + ' and ' + FloatToStr(CoordToMMs(rectDst.bottom)));         
         
         { If the boundaries of the current region are the same as the boundaries of the EP, then the current region is the EP region }
         if ( (Abs(epLeftBoundary - CoordToMMs(rectDst.left)) < 0.001)
             and (Abs(epTopBoundary - CoordToMMs(rectDst.top)) < 0.001)
             and (Abs(epRightBoundary - CoordToMMs(rectDst.right)) < 0.001)
             and (Abs(epBottomBoundary - CoordToMMs(rectDst.bottom)) < 0.001)) then
         begin
            WriteToDebugFile('In CLF_ModifyAndSuppressRegions(), found EP region!');
            
            { Flag that we know what this region is. }
            regionKnown     := True;    
         end

         { Else, the current region is a solderpaste region. }
         else
         begin

            WriteToDebugFile('In CLF_ModifyAndSuppressRegions(), found solderpaste region!');
            
            { Flag that we know what this region is. }
            regionKnown     := True; 

            {* Perform necessary calculations to determine if this is a single solderpaste region. *}
            { Determine if the region is centered at the origin. }
            xCenter :=  (CoordToMMs(rectDst.right + rectDst.left)) / 2.0;
            yCenter :=  (CoordToMMs(rectDst.top + rectDst.bottom)) / 2.0;

            { Determine if the area of the region is the percentage of area as determined by LP Wizard. }
            regionWidth := CoordToMMs(rectDst.right - rectDst.left);
            regionLength := CoordToMMs(rectDst.top - rectDst.bottom);
            WriteToDebugFile('regionWidth is: ' + FloatToStr(regionWidth));
            WriteToDebugFile('regionLength is: ' + FloatToStr(regionLength));
            
            epChamfer := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsEpChamfer));            
            chamferArea := (epChamfer * epChamfer) / 2.0;         
            epArea := (epWidthRounded * epLengthRounded) - chamferArea;
            WriteToDebugFile('epArea is: ' + FloatToStr(epArea));
            
            solderpastePct := StrToFloat(cnfGalacticInfo.Values(constLpWizardBuildInfoEpSolderpastePct)) / 100.0;
            WriteToDebugFile('solderpastePct is: ' + FloatToStr(solderpastePct));
            
            { We are subtracting 1% from solderpastePct to allow for rounding errors created by LP Wizard. }
            expectedSolderpasteArea := (solderpastePct - 0.01) * epArea;
            regionArea := regionWidth * regionLength;

            WriteToDebugFile('expectedSolderpasteArea is: ' + FloatToStr(expectedSolderpasteArea));
            WriteToDebugFile('regionArea is: ' + FloatToStr(regionArea));
            WriteToDebugFile('xCenter is: ' + FloatToStr(xCenter) + ' and yCenter is: ' + FloatToStr(yCenter));
            
            { Is this the single solderpaste region that resulted from having a chamfered EP pad
             back in Mentor LPW? }
            if ( (xCenter = 0) and (yCenter = 0) and (regionArea >= expectedSolderpasteArea) ) then
            begin
               WriteToDebugFile('This solderpaste region is the only EP solderpaste region. Flagging to modify pad.');
               foundSingleSolderPasteRegion := True;
            end

            { Else this is a tiled solderpaste region.  Handle as before. }
            else
            begin
               
               { We would rather copy this region as a fill, since a fill is by definition a simple rectangular object. }
               CLF_CopyRegionAsFill(boardXorigin,
                                    boardYorigin,
                                    regionSrc,
                                    {var} fillDst,
                                    {name} ('Top_solderpaste_fill_' + IntToStr(numEpRegions)),
                                    {var} primNames);

               { Move this to the proper layer. }
               fillDst.Layer               := constNewLayerSolderPasteRegion;

               { Queue copied fill for new library component. }
               fillQueue.Add(fillDst);
            
               { Flag that we have found solderpaste tiled regions. }
               nameValuePair    := constParmHaveSolderPasteTiles + constStringEquals + BoolToStr(True);

               { If this flag does not yet exist, then add it. }
               if (cnfGalacticInfo.IndexOfName(constParmHaveSolderPasteTiles) < 0) then
                  cnfGalacticInfo.add(nameValuePair);

               { Increment the number of known EP solderpaste regions. }
               numEpRegions    := numEpRegions + 1;

            end; { endelse }

         end; { endelse was solderpaste region }
         
      end; { endif }

   end; { endif hasEp }

   WriteToDebugFile('Current Region Boundaries (right, left, top, bottom): ' + FloatToStr(CoordToMMs(rectDst.right)) + ' ' +  + FloatToStr(CoordToMMs(rectDst.left)) + ' ' +  + FloatToStr(CoordToMMs(rectDst.top)) + ' ' +  + FloatToStr(CoordToMMs(rectDst.bottom)));
   
   {* Suppress keepout regions around fiducial pads.  We will handle the keepout function later on. *}
   { Loop over all pads and look for "unknown" pads with fiducial names among them. }
   for i := 0 to (padQueue.Count - 1) do
   begin

      { Retrieve reference to queued pad. }
      padDst := padQueue.items[i];
         
      { Compute center of region. }
      regionXcoord        := ((rectDst.right + rectDst.left) / 2);
      regionYcoord        := ((rectDst.top + rectDst.bottom) / 2);

      { Determine whether or not the center of the pad is located within the current region. }
      betweenX := (padDst.X < rectDst.right) and (padDst.X > rectDst.left);
      betweenY := (padDst.Y < rectDst.top) and (padDst.Y > rectDst.bottom);
      
      { See if center of this pad is inside the region. }
      if ( betweenX and betweenY ) then
      begin

         { Suppress this region. }
         doCopy := False;
            
         { Flag that we know what this region is. }
         regionKnown     := True;

         { Check to see if this footprint has D-shape pads. }
         hasDshapePads := StrToBool(cnfGalacticInfo.Values(constGilPkgDimsHasDshapePads));
         
         { See if this is a member of the unknown group and specifically a fiducial pad. }
         if ( (CLF_GetPadGroupNum(padDst) = constPadGroupUnknown) and (CLF_IsPadFiducial(padDst)) ) then
         begin
            CLF_WriteToSummaryAndDebugFilesWithStepNum('Suppressing keepout region around fidicial pad ' + padDst.Name + '.');
         end

         { Else if the footprint has D-shaped pads and the current pad is a member of the west, east, north or south groups,
         then proceed to modify the pad to make it rectangular and to take up the area formerly occupied by the region.
         Basically, we're squaring off the D-shaped pad.  We're doing this because using footprints with non-primitive
         pads (eg. regions) is a pain in the butt when you're actually doing layout. }
         else if ( (hasDshapePads) and ( (CLF_GetPadGroupNum(padDst) = constPadGroupWest) or (CLF_GetPadGroupNum(padDst) = constPadGroupEast) or (CLF_GetPadGroupNum(padDst) = constPadGroupNorth) or (CLF_GetPadGroupNum(padDst) = constPadGroupSouth) ) ) then 
         begin
            WriteToDebugFile('Found D-shaped pad!');          

            { Set the pad to rectangular. }
            WriteToDebugFile('padDst.ShapeOnLayer [' + IntToStr(eTopLayer) + ']: ' + IntToStr(padDst.ShapeOnLayer[eTopLayer]));
            WriteToDebugFile('Changing pads to rectangular');
            padDst.SetState_StackShapeOnLayer(eTopLayer, eRectangular);
            WriteToDebugFile('padDst.ShapeOnLayer [' + IntToStr(eTopLayer) + ']: ' + IntToStr(padDst.ShapeOnLayer[eTopLayer]));

            { Set the center of the pad to the center of the region. }
            WriteToDebugFile('old padDst.X: ' + FloatToStr(CoordToMMs(padDst.X)) + ' and old padDst.Y: ' + FloatToStr(CoordToMMs(padDst.Y)));
            padDst.X := regionXcoord;
            padDst.Y := regionYcoord;            
            WriteToDebugFile('new padDst.X: ' + FloatToStr(CoordToMMs(padDst.X)) + ' and new padDst.Y: ' + FloatToStr(CoordToMMs(padDst.Y)));
            
            { Set the dimensions of the pad to that of the region. }
            WriteToDebugFile('This D-shaped pad was resized.');
            padDst.TopXSize := Abs(rectDst.right - rectDst.left);
            padDst.TopYSize := Abs(rectDst.top - rectDst.bottom);
            WriteToDebugFile('New padDst.TopXSize is: ' + FloatToStr(CoordToMMs(padDst.TopXSize)) + ' and new padDst.TopYSize is: ' + FloatToStr(CoordToMMs(padDst.TopYSize)));

            { New pad boundaries (MMs). }
            padRight := CoordToMMs(padDst.X + (padDst.TopXSize/2.0));
            padLeft := CoordToMMs(padDst.X - (padDst.TopXSize/2.0));
            padTop := CoordToMMs(padDst.Y + (padDst.TopYSize/2.0));
            padBottom := CoordToMMs(padDst.Y - (padDst.TopYSize/2.0));

            WritetoDebugFile('New Pad Boundaries (right, left, top, bottom): ' + FloatToStr(padRight) + ' and ' + FloatToStr(padLeft) + ' and ' + FloatToStr(padTop) + ' and ' + FloatToStr(padBottom));

         end; { end elsif }
  
      end; { endif }

      { If  we found a single solderpaste region earlier, then we will remove the single solderpaste region and replace it with a solderpaste expansion in the the EP. }
      if ( (CLF_GetPadGroupNum(padDst) = constPadGroupEp) and (foundSingleSolderPasteRegion)) then
      begin
         pasteExpansion := epLeftBoundary - CoordToMMs(rectDst.left);
         
         WriteToDebugFile('In CLF_ModifyAndSuppressRegions(), found the EP! Setting solderpaste expansion to: ' + FloatToStr(pasteExpansion));
         CLF_SetSolderPasteExpansion({var pad} padDst,
                                     pasteExpansion);
         
         pasteExpansion := CoordToMMs(padDst.PasteMaskExpansion);
         WriteToDebugFile('EP pasteExpansion is now: ' + FloatToStr(pasteExpansion));

         newSolderPasteBoundaryLeft := epLeftBoundary - pasteExpansion;
         newSolderPasteBoundaryRight := epRightBoundary + pasteExpansion;
         newSolderPasteBoundaryTop := epTopBoundary + pasteExpansion;
         newSolderPasteBoundaryBottom := epBottomBoundary - pasteExpansion;       

         //WriteToDebugFile('newSolderPasteBoundaryLeft is: ' + FloatToStr(newSolderPasteBoundaryLeft) + ' and rectDst.left is: ' + FloatToStr(CoordToMMs(rectDst.left)));
         //WriteToDebugFile('newSolderPasteBoundaryRight is: ' + FloatToStr(newSolderPasteBoundaryRight) + ' and rectDst.right is: ' + FloatToStr(CoordToMMs(rectDst.right)));
         //WriteToDebugFile('newSolderPasteBoundaryTop is: ' + FloatToStr(newSolderPasteBoundaryTop) + ' and rectDst.top is: ' + FloatToStr(CoordToMMs(rectDst.top)));
         //WriteToDebugFile('newSolderPasteBoundaryBottom is: ' + FloatToStr(newSolderPasteBoundaryBottom) + ' and rectDst.bottom is: ' + FloatToStr(CoordToMMs(rectDst.bottom)));
         
         { Sanity Check. Abort if the new solderpaste boundaries do not match the boundaries of the original solderpaste region. }
         if ((Abs(newSolderPasteBoundaryLeft - CoordToMMs(rectDst.left)) < 0.0000001)
             and(Abs(newSolderPasteBoundaryTop - CoordToMMs(rectDst.top)) < 0.0000001)
             and(Abs(newSolderPasteBoundaryRight - CoordToMMs(rectDst.right)) < 0.0000001)
             and(Abs(newSolderPasteBoundaryBottom - CoordToMMs(rectDst.bottom)) < 0.0000001)) then
         begin
            WriteToDebugFile('New solderpaste boundaries match original solderpaste region');
         end { end if }
         else
         begin
            //WriteToDebugFile(BoolToStr(newSolderPasteBoundaryLeft = CoordToMMs(rectDst.left)) + ' and ' + BoolToStr(newSolderPasteBoundaryTop = CoordToMMs(rectDst.top)) + ' and ' + BoolToStr(newSolderPasteBoundaryRight = CoordToMMs(rectDst.right)) + ' and ' + BoolToStr(newSolderPasteBoundaryBottom = CoordToMMs(rectDst.bottom)));
            
            CLF_Abort('New solderpaste boundaries do not match original solderpaste region!');
         end; { end else }

      end; { end if }

   end; { end loop }
   { Sanity check.  The only regions we expect to see are solderpaste tiles on EP pad and keepout regions on fiducial pads.
    If we find anything else, we don't know what it is or how to handle it! }
   if (not regionKnown) then
      CLF_Abort('Found unknown and unhandled region!  Figure out what this is and fix/improve script!');

end; { end CLF_ModifyAndSuppressRegions() }


{***************************************************************************
 * function CLF_CleanupCornerDshapePads()
 *  We previously changed all D-shape pads to rectangular so that the footprint
 *  would be easier for Altium to handle, but this may have caused some pad
 *  to pad clearance violations because the D-shape pads are closer together.
 *  This function checks if that parameter was violated and shrinks all corner
 *  pads just enough to maintain pad to pad clearance.
 *
 *  Note: All variables related to pads are in coordinates, except for those related to pad to pad clearance.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CleanupCornerDshapePads(    cnfGalacticInfo : TStringList;                
                                     var padQueue        : TInterfaceList;
                                     )                   : Integer;

var
   i                     : Integer;
   lastGroupNum          : Integer;
   currentGroupNum       : Integer;
   cornerPadNums         : array[0..7] of Integer;
   nextOpenArrayIndex    : Integer;
   padA                  : IPCB_Pad;
   padB                  : IPCB_Pad;
   tempInt               : Integer;
   tempPad               : IPCB_Pad;
   constPadToPadDistance : Real;
   oldPadToPadDistance   : Real;
   newPadToPadDistance   : Real;
   padAcornerX           : Real;
   padAcornerY           : Real;
   padBcornerX           : Real;
   padBcornerY           : Real;
   newPadAcornerX        : Real;
   newPadBcornerY        : Real;
   padAcornerDifferenceX : Real;
   padBcornerDifferenceY : Real;
   newXYdifference       : Real;
   hasEp                 : Boolean;
   xDiffMMs              : Real;
   yDiffMMs              : Real;
   deltaWithPlus         : Real;
   deltaWithMinus        : Real;
   a                     : Real;
   b                     : Real;
   c                     : Real;
   padAgroupNum          : Integer;
   padBgroupNum          : Integer;
   
begin
   WriteToDebugFile('Hello from CLF_CleanupCornerDshapePads()!');

   { Set lastGroupNum to a null value. }
   lastGroupNum := -1;

   { Initialize array counter to 0. }
   nextOpenArrayIndex := 0;

   { Retrieve boolean hasEp from cnfGalacticInfo. }
   hasEp := StrToBool(cnfGalacticInfo.Values(constGilHasEp));

   { Loop over padQueue, looking for when the pad group changes. }
   for i := 0 to (padQueue.Count - 1) do
   begin
      { Retrieve the pad group number of the current pad. }
      currentGroupNum := CLF_GetPadGroupNum(padqueue.items[i]);

      //WriteToDebugFile('currentGroupNum: ' + IntToStr(currentGroupNum));
      //WriteToDebugFile('lastGroupNum: ' + IntToStr(lastGroupNum));    
      { If the current pad group num does not match the last pad group num add the current pad and the last pad
       to cornerPadNums[] unless the last pad group number was the null value or the current pad is an EP. }
     if ( (currentGroupNum <> lastGroupNum) ) then
     begin
         if (lastGroupNum <> -1 ) then
         begin
            WriteToDebugFile('last pad is a corner pad. Pad Number: ' + IntToStr(i));
            cornerPadNums[nextOpenArrayIndex] := i;
            Inc(nextOpenArrayIndex);
         end;
         if (currentGroupNum <> constPadGroupEp) then
         begin
            WriteToDebugFile('currentPad is a corner pad. Pad Number: ' + IntToStr(i+1));
            cornerPadNums[nextOpenArrayIndex] := i+1;
            Inc(nextOpenArrayIndex);
         end;
     end;

      { currentGroupNum becomes lastGroupNum. }
      lastGroupNum: = currentGroupNum;
      //WriteToDebugFile('New lastGroupNum: ' + IntToStr(lastGroupNum));

   end;

   { If the footprint does not have EP, then the last pad in the queue should also be added to cornerPadNums[]. }
   if (not hasEp) then
   begin
      cornerPadNums[nextOpenArrayIndex] := i;
      WriteToDebugFile('last pad is a corner pad. Pad Number: ' + IntToStr(i));
   end;
   
   { Reorder array so the last pad is adjacent to the first pad in the array. }
   tempInt := cornerPadNums[7];
   for i := 6 {(Length(cornerPadNums)-2)} downto 1 do
   begin
      cornerPadNums[i+1] := cornerPadNums[i]
   end;
   cornerPadNums[1] := tempInt;

   { Print cornerPadNums[] to Debug file to confirm adjacent pads are adjacent in the array. }
   //for i := 0 to 7 {cornerPadNums.length - 1} do
   //begin 
   //   WriteToDebugFile('cornerPadNums Index: ' + IntToStr(cornerPadNums[i]));
   //end;

   { Loop over cornerPadNums[] (increment 2 per loop to skip over the paired pads). }
   for i := 0 to 6 {cornerPadNums.length - 2} do
   begin
      { Extract the pair of corner pads from padQueue. }
      padA := padqueue.items[cornerPadNums[i]-1];
      padB := padqueue.items[cornerPadNums[i+1]-1];

      padAgroupNum := CLF_GetPadGroupNum(padA);
      padBgroupNum := CLF_GetPadGroupNum(padB);

      { If padA is a north or south pad, switch padA and padB to simplify the following steps. }
      if ( (padAgroupNum = constPadGroupNorth) or (padAgroupNum = constPadGroupSouth) ) then
      begin
         tempPad := padA;
         padA := padB;
         padB := tempPad;
         
         tempInt := padAgroupNum;
         padAgroupNum := padBgroupNum;
         padBgroupNum := tempInt
      end;    

      WriteToDebugFile('padA groupNum is: ' +  IntToStr(padAgroupNum) + ' and padB groupNum is: ' +  IntToStr(padBgroupNum));

      { Sanity check that aborts if any pad is not north, south, east or west. }
      if ((padBgroupNum <> constPadGroupNorth) and (padBgroupNum <> constPadGroupSouth) and (padAgroupNum <> constPadGroupWest) and (padAgroupNum <> constPadGroupEast)) then
         CLF_Abort('This pad does not belong to pad group north, south, east or west! Please review the code!');

      { If padA is west, find the coordinates of the corner nearest its paired pad. }
      if (padAgroupNum = constPadGroupWest) then
      begin
         padAcornerX := padA.X + (padA.TopXSize / 2.0);
         padBcornerX := padB.X -(padB.TopXSize / 2.0);
      end

      { Else padA is east, find the coordinates of the corner nearest its paired pad. }
      else
      begin
         padAcornerX := padA.X - (padA.TopXSize / 2.0);
         padBcornerX := padB.X + (padB.TopXSize / 2.0);
      end;
      
      { If padB is south, find the coordinates of the corner nearest its paired pad. }
      if (padBgroupNum = constPadGroupSouth) then
      begin
         padBcornerY := padB.Y + (padB.TopYSize / 2.0);                   
         padAcornerY := padA.Y - (padA.TopYSize / 2.0);
      end

      { Else padB is North, find the coordinates of the corner nearest its paired pad. }
      else
      begin
         padBcornerY := padB.Y - (padB.TopYSize / 2.0);
         padAcornerY := padA.Y + (padA.TopYSize / 2.0);
      end;
      WriteToDebugFile('Old padA.X: ' + FloatToStr(CoordToMMs(padA.X)));
      //WriteToDebugFile('Old padA.Y: ' + FloatToStr(CoordToMMs(padA.Y)));
      //WriteToDebugFile('Old padB.X: ' + FloatToStr(CoordToMMs(padB.X)));
      WriteToDebugFile('Old padB.Y: ' + FloatToStr(CoordToMMs(padB.Y)));
      WriteToDebugFile('Old padA.TopXSize: ' + FloatToStr(CoordToMMs(padA.TopXSize)));
      //WriteToDebugFile('Old padA.TopYSize: ' + FloatToStr(CoordToMMs(padA.TopYSize)));      
      //WriteToDebugFile('Old padB.TopXSize: ' + FloatToStr(CoordToMMs(padB.TopXSize)));      
      WriteToDebugFile('Old padB.TopYSize: ' + FloatToStr(CoordToMMs(padB.TopYSize)));      
      //WriteToDebugFile('Old padAcornerX: ' + FloatToStr(CoordToMMs(padAcornerX)));      
      //WriteToDebugFile('Old padAcornerY: ' + FloatToStr(CoordToMMs(padAcornerY)));
      //WriteToDebugFile('Old padBcornerX: ' + FloatToStr(CoordToMMs(padBcornerX)));      
      //WriteToDebugFile('Old padBcornerY: ' + FloatToStr(CoordToMMs(padBcornerY)));

      { Calculate the current distance between the two closest corners. }
      xDiffMMs := CoordToMMs(Abs(padBcornerX) - Abs(padAcornerX));
      yDiffMMs := CoordToMMs(Abs(padBcornerY) - Abs(padAcornerY));
      //WriteToDebugFile('xDiffMMs: ' + FloatToStr(CoordToMMs(xDiffMMs)));
      //WriteToDebugFile('yDiffMMs: ' + FloatToStr(CoordToMMs(yDiffMMs)));
      oldPadToPadDistance := Sqrt(Sqr(xDiffMMs) + Sqr(yDiffMMs));
      //WriteToDebugFile('oldPadToPadDistance: ' + FloatToStr(oldPadToPadDistance));

      { Retrieve from cnfGalacticInfo the minimum pad to pad distance. }
      constPadToPadDistance := StrToFloat(cnfGalacticInfo.Values(constLpWizardBuildInfoPadToPadField));
      //WriteToDebugFile('constPadToPadDistance is: ' + FloatToStr(constPadToPadDistance));

      { If the pair of pads are closer than LPW allows (given by the constPadToPadDistance extracted from the plb09 file), modify the pads. }
      if ( oldPadToPadDistance < constPadToPadDistance ) then
      begin

         WriteToDebugFile('Pad to pad clearance is not satisfied. About to correct pads...');

         { Use quadratic formula to solve (constPadToPadDistance)^2 = (xDiffMMs + newXYdifference)^2 + (yDiffMMs + newXYdifference)^2 for newXYdifference. 
          In standard form, this equation simplifies to 2*newXYdifference^2 + 2*(xDiffMMs + yDiffMMs)*newXYdifference + (oldPadToPadDistance^2 - constPadToPadDistance^2) = 0. }
         a := 2.0;
         b := 2*(xDiffMMs + yDiffMMs);
         c := Sqr(oldPadToPadDistance) - Sqr(constPadToPadDistance);
         deltaWithPlus := (-b + Sqrt(Sqr(b) - 4*a*c)) / (2*a);
         deltaWithMinus := (-b - Sqrt(Sqr(b) - 4*a*c)) / (2*a);

         //WriteToDebugFile('deltaWithPlus: ' + FloatToStr(deltaWithPlus));
         //WriteToDebugFile('deltaWithMinus: ' + FloatToStr(deltaWithMinus));
         if ( deltaWithPlus > 0 ) then
         begin
            newXYdifference := MMsToCoord(deltaWithPlus);
         end
         else
            newXYdifference := MMsToCoord(deltaWithMinus);

         { Use quadratic formula to solve (constPadToPadDistance)^2 = (xDiffMMs + newXYdifference)^2 + (yDiffMMs + newXYdifference)^2 for newXYdifference. 
          In standard form, this equation simplifies to 2*newXYdifference^2 + 2*(xDiffMMs + yDiffMMs)*newXYdifference + (oldPadToPadDistance^2 - constPadToPadDistance^2) = 0. }
         a := 2.0;
         b := 2*(xDiffMMs + yDiffMMs);
         c := Sqr(oldPadToPadDistance) - Sqr(constPadToPadDistance);
         deltaWithPlus := (-b + Sqrt(Sqr(b) - 4*a*c)) / (2*a);
         deltaWithMinus := (-b - Sqrt(Sqr(b) - 4*a*c)) / (2*a);

         //WriteToDebugFile('deltaWithPlus: ' + FloatToStr(deltaWithPlus));
         //WriteToDebugFile('deltaWithMinus: ' + FloatToStr(deltaWithMinus));
         { We want to set newXYdifference to whichever delta is positive. } 
         if ( deltaWithPlus > 0 ) then
         begin
            newXYdifference := MMsToCoord(deltaWithPlus);
         end
         else
            newXYdifference := MMsToCoord(deltaWithMinus);

         { Calculate how far in the x and y directions the pads should be to maintain constPadToPadDistance. }
         //WriteToDebugFile('newXYdifference: ' + FloatToStr(CoordToMMs(newXYdifference)));

         { If padA is west, find the coordinates of where the new closest corner should be. }
         if (padAgroupNum = constPadGroupWest) then
         begin
            newPadAcornerX := padAcornerX - newXYdifference;
         end         
         { Else padA is east, find the coordinates of where the new closest corner should be. }
         else
         begin
            newPadAcornerX := padAcornerX + newXYdifference;
         end;
         { If padB is south, find the coordinates of where the new closest corner should be. }
         if (padBgroupNum = constPadGroupSouth) then
         begin
			newPadBcornerY := padBcornerY - newXYdifference;
         end
         { Else padB is north, find the coordinates of where the new closest corner should be. }
         else
			newPadBcornerY := padBcornerY + newXYdifference;

         { Calculate the distance the closest corner needs to move. }
         padAcornerDifferenceX := Abs(padAcornerX - newPadAcornerX);
         padBcornerDifferenceY := Abs(padBcornerY - newPadBcornerY);
         //WriteToDebugFile('newPadAcornerX: ' + FloatToStr(CoordToMMs(newPadAcornerX)));
         //WriteToDebugFile('newPadBcornerY: ' + FloatToStr(CoordToMMs(newPadBcornerY)));


         { Decrease the width of padA and height of padB by half the difference calculated previously. }
         padA.TopXSize := padA.TopXSize - (padAcornerDifferenceX / 2.0);
         padB.TopYSize := padB.TopYSize - (padBcornerDifferenceY / 2.0);
         WriteToDebugFile('New padA.TopXSize: ' + FloatToStr(CoordToMMs(padA.TopXSize)));
         WriteToDebugFile('New padB.TopYSize: ' + FloatToStr(CoordToMMs(padB.TopYSize)));

         { If padA is west, move the center so the left boundary lines up with the other west pads. }
         if (padAgroupNum = constPadGroupWest) then
         begin
            padA.X := padA.X - (padAcornerDifferenceX / 4.0);
         end
         { Else padA is east, move the center so the right boundary lines up with the other east pads. }
         else
         begin
            padA.X := padA.X + (padAcornerDifferenceX / 4.0);
         end;
         
         { If padB is south, move the center so the bottom boundary lines up with the other south pads. }
         if (padBgroupNum = constPadGroupSouth) then
         begin
            padB.Y := padB.Y - (padBcornerDifferenceY / 4.0);
         end
         { Else padB is north, move the center so the top boundary lines up with the other north pads. }
         else
         begin
            padB.Y := padB.Y + (padBcornerDifferenceY / 4.0);
         end;
         WriteToDebugFile('New padA.X: ' + FloatToStr(CoordToMMs(padA.X)));
         //WriteToDebugFile('New padA.Y (should be same as old): ' + FloatToStr(CoordToMMs(padA.Y)));
         //WriteToDebugFile('New padB.X (should be same as old): ' + FloatToStr(CoordToMMs(padB.X)));
         WriteToDebugFile('New padB.Y: ' + FloatToStr(CoordToMMs(padB.Y)));      

         { Put the pads back in padQueue where we found them. }
         padqueue.items[cornerPadNums[i]-1] := padA;
         padqueue.items[cornerPadNums[i+1]-1] := padB;


         { Sanity check that the pads are now far enough away from each other. }
         //WriteToDebugFile('New padA left boundary: ' + FloatToStr(CoordToMMs(padA.X - (padA.TopXSize/2.0))));
         //WriteToDebugFile('New padA right boundary: ' + FloatToStr(CoordToMMs(padA.X + (padA.TopXSize/2.0))));
         //WriteToDebugFile('New padB top boundary: ' + FloatToStr(CoordToMMs(padB.Y + (padB.TopYSize/2.0))));
         //WriteToDebugFile('New padB bottom boundary: ' + FloatToStr(CoordToMMs(padB.Y - (padB.TopYSize/2.0))));
         newPadToPadDistance := CoordToMMs(Sqrt(Sqr(MMsToCoord(xDiffMMs) + newXYdifference) + Sqr(MMsToCoord(yDiffMMs) + newXYdifference)));
         WriteToDebugFile('Double check that we correctly moved the pads (This should be ' + FloatToStr(constPadToPadDistance) + '): ' + FloatToStr(newPadtoPadDistance));

         { Sanity Check. }
         if (newPadToPadDistance <> constPadToPadDistance) then
            CLF_Abort('CLF_CleanupCornerDshapePads() has failed to set the pad to pad distance to ' + FloatToStr(constPadToPadDistance) + '! Please review the code!');

      end

      else
         WriteToDebugFile('This pair of pads does not need its D-shape pads modified to maintain pad to pad clearance.');

      { Increment i in addition to the for loop in order to skip over the second pad in the pair. }
      Inc(i);

   end;
   
end; { end CLF_CleanupCornerDshapePads() }


{***************************************************************************
 * function CLF_ModifyAndSuppressPads()
 *  Modify any pads that we want to change as we copy things to destination
 *  library component.  Also decide if there are some pads that we wish
 *  to suppress altogether (eg. not copy to destination).
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ModifyAndSuppressPads(    boardXorigin    : TCoord;
                                       boardYorigin    : TCoord;
                                       padSrc          : IPCB_Pad;
                                   var padDst          : IPCB_Pad;
                                   var primNames       : TStringList;
                                   var doCopy          : Boolean;
                                   var padNew          : IPCB_Pad;
                                   var cnfGalacticInfo : TStringList;
                                       )               : Integer;
var
   nameValuePair          : TString;
   hasEp                  : Boolean;
   splitPin               : TString;
   newPinName1            : TString;
   X1center               : Real;
   Y1center               : Real;
   X1size                 : Real;
   Y1size                 : Real;
   newPinName2            : TString;
   X2center               : Real;
   Y2center               : Real;
   X2size                 : Real;
   Y2size                 : Real;
   solderpasteExpansionMm : Real;
   pkgDimsEpChamfer       : Real;
   pkgDimsEpCornerRadius  : Real;
   pkgDimsEpWidthMax      : Real;
   pkgDimsEpLengthMax     : Real;
   fixExposedPad          : Boolean;
   xyRoundingFactor       : Real;
   epWidthRounded         : Real;
   epLengthRounded        : Real;
   cornerRadiusPct        : Real;
   
begin

   { Assume success. }
   result := 0;
   
   { Assume that we want to copy this object to destination library. }
   doCopy := True;

   { Assume that we will not be creating a new pad. }
   padNew           := Nil;

   { Assume that the exposed pad will not need to be fixed. }
   fixExposedPad := False;

   { Retrieve whether this package is known to have an exposed pad (EP). }
   hasEp            := StrToBool(cnfGalacticInfo.Values(constGilHasEp));

   { Retrieve whether we have been ordered to split a specific pin. }
   splitPin         := (cnfGalacticInfo.Values(constGilSplitPinPinName));

   { If we have been ordered to split a pin, then retrieve other necessary info. }
   if (splitPin <> '') then
   begin
      WriteToDebugFile('Found a split pin');
      newPinName1   := (cnfGalacticInfo.Values(constGilSplitPinPinName1));
      X1center      := StrToFloat(cnfGalacticInfo.Values(constGilSplitPinX1center));
      Y1center      := StrToFloat(cnfGalacticInfo.Values(constGilSplitPinY1center));
      X1size        := StrToFloat(cnfGalacticInfo.Values(constGilSplitPinX1size));
      Y1size        := StrToFloat(cnfGalacticInfo.Values(constGilSplitPinY1size));
      newPinName2   := (cnfGalacticInfo.Values(constGilSplitPinPinName2));
      X2center      := StrToFloat(cnfGalacticInfo.Values(constGilSplitPinX2center));
      Y2center      := StrToFloat(cnfGalacticInfo.Values(constGilSplitPinY2center));
      X2size        := StrToFloat(cnfGalacticInfo.Values(constGilSplitPinX2size));
      Y2size        := StrToFloat(cnfGalacticInfo.Values(constGilSplitPinY2size));
   end;

   {* Perform any changes we want to make to pads before we do the copy to destination library. *}
   { Find our EP (exposed pad) pad, if one exists. }
   { In cases with split pins, neither of the halves will be centered at (0,0). At this point,
    split EP will not have been split yet so it is valid to use this check to identify EP. }
   if ( (padDst.X = 0) and (padDst.Y = 0) and (hasEp) ) then
   begin
      
      WriteToDebugFile('In CLF_ModifyAndSuppressPads(), found EP pad!');

      { Change name of this pad to 'EP'. }
      padDst.Name               := constNewNameOfEp;

      { Retrieve exposed pad chamfer and fillet radius parameters from cnfGalacticInfo to
       determine if the exposed pad will need to be fixed when creating the 3D model. }
      pkgDimsEpChamfer :=  cnfGalacticInfo.Values(constGilPkgDimsEpChamfer);
      pkgDimsEpCornerRadius :=  cnfGalacticInfo.Values(constGilPkgDimsEpCornerRad);

      { Retrieve the exposed pad dimensions as they were in the plb09 file. }
      pkgDimsEpWidthMax :=  cnfGalacticInfo.Values(constGilPkgDimsEpWidthMax);
      pkgDimsEpLengthMax :=  cnfGalacticInfo.Values(constGilPkgDimsEpLengthMax);

      { Round the EP length and width as specified by the settings extracted from the .plb09 file. }
      { TODO:  We are currently assuming that we want to do a floor() function.  We need to investigate this further
       with additional footprints to make sure that this is really true. }
      xyRoundingFactor :=  StrToFloat(cnfGalacticInfo.Values(constLpWizardBuildInfoXYRounding));
      epWidthRounded := ( floor(pkgDimsEpWidthMax / xyRoundingFactor) ) * xyRoundingFactor;
      epLengthRounded := ( floor(pkgDimsEpLengthMax / xyRoundingFactor) ) * xyRoundingFactor;

      { Store the rounded values in cnfGalacticInfo. }
      cnfGalacticInfo.add(constGilEpWidthRounded + constStringEquals + FloatToStr(epWidthRounded));
      cnfGalacticInfo.add(constGilEpLengthRounded + constStringEquals + FloatToStr(epLengthRounded));

      { If either the chamfer or corner radius parameters are non-zero, round the values stored in
       cnfGalacticInfo and use the rounded values as the exposed pad length and width. }
      { FIXME: code needed to support cases when chamfer and fillet radius are non-zero. }
      if ( (pkgDimsEpChamfer <> 0) or ( pkgDimsEpCornerRadius <> 0 ) ) then
      begin
         
         WriteToDebugFile('EP chamfer or corner radius was found to be non-zero! Proceeding to resize exposed pad and flag to fix later.');

         { Set the exposed pad length and width to the rounded version of the original dimensions. }
         padDst.TopXSize := MMsToCoord(epWidthRounded);
         padDst.TopYSize := MMsToCoord(epLengthRounded);

         { Flag that the exposed pad needs to be fixed when creating the 3D model and
          store this in cnfGalacticInfo. }
         fixExposedPad := True;
         cnfGalacticInfo.add(constGilFixExposedPad + constStringEquals + BoolToStr(fixExposedPad));
         
         if ( pkgDimsEpCornerRadius <> 0 ) then
         begin
            WriteToDebugFile('EP corner radius is not zero: ' + FloatToStr(pkgDimsEpCornerRadius));
            
            //WriteToDebugFile('pkgDimsEpCornerRadius: ' + FloatToStr(pkgDimsEpCornerRadius));
            //WriteToDebugFile('epWidthRounded: ' + FloatToStr(epWidthRounded));

            { Set the EP shape to Rounded Rectangular. }
            padDst.SetState_StackShapeOnLayer(eTopLayer, eRoundedRectangular);

            { Set the change pkgDimsEpCornerRadius into a percentage (because Altium measures EP radii in this way. }
            cornerRadiusPct := Round(2*pkgDimsEpCornerRadius/epWidthRounded*100);
            //WriteToDebugFile('cornerRadiusPct: ' + FloatToStr(cornerRadiusPct));

            { Set the EP corner radius percentage to the value just calculated. }
            WriteToDebugFile('Old EP CR Pct: ' + FloatToStr(padDst.GetState_StackCRPctOnLayer(eTopLayer)));
            padDst.SetState_StackCRPctOnLayer(eTopLayer, cornerRadiusPct);
            WriteToDebugFile('New EP CR Pct: ' + FloatToStr(padDst.GetState_StackCRPctOnLayer(eTopLayer)));

         end;
      end;
      
   end {nosemi}

   { Else see if we need to perform special handling for fiducial pads. }
   else if (CLF_IsPadFiducial(padSrc)) then
   begin
      
      { Configure for a solderpaste expansion of -10mm (shrinking it to nothing). }
      CLF_SetSolderPasteExpansion({var pad} padDst,
                                  constPasteExpansionSuppressMm);

   end; { end elsif }

   { See if we have been ordered to split this particular pin. }
   { Note:  This MUST be independent of the above if-elsif construct! }
   if (padDst.Name = splitPin) then
   begin

      WriteToDebugFile('In CLF_ModifyAndSuppressPads(), attempting to split pad ' + padDst.Name + '!');

      {* Morph padDst into newPinName1. }
      { Change name of this pad to newPinName1. }
      padDst.Name              := newPinName1;

      { Override certain other x,y data with parameters read from our command xml file. }
      padDst.X                 := MMsToCoord(X1center);
      padDst.Y                 := MMsToCoord(Y1center);
      padDst.TopXSize          := MMsToCoord(X1size);
      padDst.TopYSize          := MMsToCoord(Y1size);
      padDst.Rotation          := 0;    { This is not specified in command xml file.  Assume/force it to be 0. }

      { If this is an EP pin, then we need to recalculate solderpaste expansion! }
      if (CLF_IsPadEp(padDst)) then
      begin
      
         { Calculate and set solderpaste expansion for this modified EP pin. }
         CLF_CalculateAndSetSolderPasteExpansionForEp({var} cnfGalacticInfo,
                                                      {var pad} padDst);
      end;

      {* Create new blank pad. *}
      { FIXME:  Add new pad to appropriate pad group! }
      padNew := PcbServer.PCBObjectFactory(ePadObject,eNoDimension,eCreate_Default);

      { Copy all pad properties from padDst to padNew. }
      CLF_CopyPadProperties({boardXorigin} 0,
                            {boardYorigin} 0,
                            padDst,
                            {var} padNew,
                            {var} primNames);

      {* Morph padNew into newPinName2. }
      { Change name of this pad to newPinName2. }
      padNew.Name              := newPinName2;

      { Override certain other x,y data with parameters read from our command xml file. }
      padNew.X                 := MMsToCoord(X2center);
      padNew.Y                 := MMsToCoord(Y2center);
      padNew.TopXSize          := MMsToCoord(X2size);
      padNew.TopYSize          := MMsToCoord(Y2size);
      padNew.Rotation          := 0;    { This is not specified in command xml file.  Assume/force it to be 0. }

      { If this is an EP pin, then we need to recalculate solderpaste expansion! }
      if (CLF_IsPadEp(padNew)) then
      begin
      
         { Calculate and set solderpaste expansion for this new EP pin. }
         CLF_CalculateAndSetSolderPasteExpansionForEp({var} cnfGalacticInfo,
                                                      {var pad} padNew);
      end;

   end; { endif }
   
   
end; { end CLF_ModifyAndSuppressPads() }


{***************************************************************************
 * function CLF_ModifyAndSuppressTracks()
 *  Modify any tracks that we want to change as we copy things to destination
 *  library component.  Also decide if there are some tracks that we wish
 *  to suppress altogether (eg. not copy to destination).
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ModifyAndSuppressTracks(    boardXorigin    : TCoord;
                                         boardYorigin    : TCoord;
                                         trackSrc        : IPCB_Track;
                                     var trackDst        : IPCB_Track;
                                     var primNames       : TStringList;
                                     var doCopy          : Boolean;
                                     var cnfGalacticInfo : TStringList;
                                         )               : Integer;
var
   name          : TString;
   i             : Integer;
   footprintType : TString;
   oldLayer1     : Integer;
   oldWidthMm1   : Real;
   newLayer1     : Integer;
   newWidthMm1   : Real;
   oldLayer2     : Integer;
   oldWidthMm2   : Real;
   newLayer2     : Integer;
   newWidthMm2   : Real;
   oldLayer3     : Integer;
   oldWidthMm3   : Real;
   newLayer3     : Integer;
   newWidthMm3   : Real;
   oldLayer4     : Integer;
   oldWidthMm4   : Real;
   newLayer4     : Integer;
   newWidthMm4   : Real;

begin

   { Assume success. }
   result := 0;
   
   { Retrieve footprint type. }
   footprintType         := cnfGalacticInfo.Values(constGilFootprintType);

   { Assume that we want to copy this object to destination library. }
   doCopy := True;

   { Retrieve necessary information re/ old layer / old width / new layer / new width. }
   oldLayer1   := StrToInt(cnfGalacticInfo.Values(constGilModTracksOldLayer1));
   oldWidthMm1 := StrToFloat(cnfGalacticInfo.Values(constGilModTracksOldWidthMm1));
   newLayer1   := StrToInt(cnfGalacticInfo.Values(constGilModTracksNewLayer1));
   newWidthMm1 := StrToFloat(cnfGalacticInfo.Values(constGilModTracksNewWidthMm1));

   oldLayer2   := StrToInt(cnfGalacticInfo.Values(constGilModTracksOldLayer2));
   oldWidthMm2 := StrToFloat(cnfGalacticInfo.Values(constGilModTracksOldWidthMm2));
   newLayer2   := StrToInt(cnfGalacticInfo.Values(constGilModTracksNewLayer2));
   newWidthMm2 := StrToFloat(cnfGalacticInfo.Values(constGilModTracksNewWidthMm2));

   oldLayer3   := StrToInt(cnfGalacticInfo.Values(constGilModTracksOldLayer3));
   oldWidthMm3 := StrToFloat(cnfGalacticInfo.Values(constGilModTracksOldWidthMm3));
   newLayer3   := StrToInt(cnfGalacticInfo.Values(constGilModTracksNewLayer3));
   newWidthMm3 := StrToFloat(cnfGalacticInfo.Values(constGilModTracksNewWidthMm3));

   oldLayer4   := StrToInt(cnfGalacticInfo.Values(constGilModTracksOldLayer4));
   oldWidthMm4 := StrToFloat(cnfGalacticInfo.Values(constGilModTracksOldWidthMm4));
   newLayer4   := StrToInt(cnfGalacticInfo.Values(constGilModTracksNewLayer4));
   newWidthMm4 := StrToFloat(cnfGalacticInfo.Values(constGilModTracksNewWidthMm4));

   
   {* Perform any changes we want to make to tracks before we do the copy to destination library. *}
   { Handle layer/width set 1. }
   if ( (trackSrc.Layer = oldLayer1) and (trackSrc.Width = MMsToCoord(oldWidthMm1)) ) then
   begin

      { Move this track to the desired layer and change its width. }
      trackDst.Layer                := newLayer1;
      trackDst.Width                := MMsToCoord(newWidthMm1);

   end {nosemi}

   { Handle layer/width set 2. }
   else if ( (trackSrc.Layer = oldLayer2) and (trackSrc.Width = MMsToCoord(oldWidthMm2)) ) then
   begin

      { Move this track to the desired layer and change its width. }
      trackDst.Layer                := newLayer2;
      trackDst.Width                := MMsToCoord(newWidthMm2);

   end {nosemi}

   { Handle layer/width set 3. }
   else if ( (trackSrc.Layer = oldLayer3) and (trackSrc.Width = MMsToCoord(oldWidthMm3)) ) then
   begin

      { Move this track to the desired layer and change its width. }
      trackDst.Layer                := newLayer3;
      trackDst.Width                := MMsToCoord(newWidthMm3);

   end {nosemi}

   { Handle layer/width set 4. }
   else if ( (trackSrc.Layer = oldLayer4) and (trackSrc.Width = MMsToCoord(oldWidthMm4)) ) then
   begin

      { Move this track to the desired layer and change its width. }
      trackDst.Layer                := newLayer4;
      trackDst.Width                := MMsToCoord(newWidthMm4);

   end;

   
   {* Perform any special handling now that we know the layer that this track will end up on. *}
   { Special handling for courtyard tracks. }
   if (trackDst.Layer = constNewLayerCourtyard) then
   begin      
   
      { See if this is a courtyard outline line or part of the center cross. }
      if ( (trackDst.X1 = 0) and (trackDst.X2 = 0) ) then
         name := 'Courtyard_cross_vert' {nosemi}
      else if ( (trackDst.Y1 = 0) and (trackDst.Y2 = 0) ) then
         name := 'Courtyard_cross_horiz' {nosemi}
      else
         name := 'Courtyard';
      
      { Change the primitive name now that we know it's a courtyard line. }
      CLF_ChangePrimName({var prim} trackDst,
                         name,
                         {var} primNames);
      
      { Extract and maintain the bounding coordinates of the courtyard rectangle. }
      CLF_MaintainBoundingRectangleForTracks(boardXorigin,
                                             boardYorigin,
                                             trackSrc,
                                             {namePrefix} 'Courtyard',
                                             {var} cnfGalacticInfo);

   end {nosemi}

   { Special handling for AssyDrawing lines. }
   else if (trackDst.Layer = constNewLayerAssyDrawing) then
   begin

      { For chip or molded components, use smaller line width on assembly drawing. }
      if ( (footprintType = 'Inductor') or (footprintType = 'Capacitor') ) then
      begin

         { Change track width. }
         trackDst.Width                := MMsToCoord(constNewWidthAssyDrawingChipMm);

      end;
      
      { Change the primitive name now that we know it's a assembly line. }
      CLF_ChangePrimName({var prim} trackDst,
                         {name} 'Assembly',
                         {var} primNames);

      { Extract and maintain the bounding coordinates of the assembly rectangle. }
      CLF_MaintainBoundingRectangleForTracks(boardXorigin,
                                             boardYorigin,
                                             trackSrc,
                                             {namePrefix} 'Assembly',
                                             {var} cnfGalacticInfo);
   end {nosemi}

   { Special handling for lines that are useless that we don't want to propagate to destination library. }
   else if (trackDst.Layer = 0) then
   begin
      
      { Flag to suppress copying this track. }
      doCopy := False;
      
   end {nosemi}

   { Special handling for silkscreen tracks. }
   else if (trackDst.Layer = constSilkLayer) then
   begin

      { For chip or molded components, suppress silkscreen lines from LPW footprint. }
      if ( (footprintType = 'Inductor') or (footprintType = 'Capacitor') ) then
      begin

         { Flag to suppress copying this track. }
         doCopy := False;

      end;

      { Change the primitive name now that we know it's a silkscreen line. }
      CLF_ChangePrimName({var prim} trackDst,
                         {name} 'Silkscreen',
                         {var} primNames);
      
      { Extract and maintain the bounding coordinates of the silkscreen rectangle. }
      { We are making no changes to these. }
      CLF_MaintainBoundingRectangleForTracks(boardXorigin,
                                             boardYorigin,
                                             trackSrc,
                                             {namePrefix} 'Silkscreen',
                                             {var} cnfGalacticInfo);

   end;

end; { end CLF_ModifyAndSuppressTracks() }


{***************************************************************************
 * function CLF_ModifyAndSuppressArcs()
 *  Modify any arcs that we want to change as we copy things to destination
 *  library component.  Also decide if there are some arcs that we wish
 *  to suppress altogether (eg. not copy to destination).
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ModifyAndSuppressArcs(    boardXorigin    : TCoord;
                                       boardYorigin    : TCoord;
                                       arcSrc          : IPCB_Arc;
                                   var arcDst          : IPCB_Arc;
                                   var primNames       : TStringList;
                                   var doCopy          : Boolean;
                                   var cnfGalacticInfo : TStringList;
                                       )               : Integer;
var
   isInside      : Boolean;
   footprintType : TString;

begin

   { Assume success. }
   result := 0;
   
   { Assume that we want to copy this object to destination library. }
   doCopy := True;

   { Retrieve footprint type. }
   footprintType         := cnfGalacticInfo.Values(constGilFootprintType);

   {* Perform any changes we want to make to arcs before we do the copy to destination library. *}

   { Determine if this arc is inside the assembly outline. }
   CLF_IsArcInsideBoundingRectangle(boardXorigin,
                                    boardYorigin,
                                    arcSrc,
                                    {namePrefix} 'Assembly',
                                    {var} cnfGalacticInfo,
                                    {var} isInside);

   { Suppress all arcs within the assembly outline.  We will manually replace
    the interior pin 1 markers on silkscreen and assembly layers. }
   { For molded components, suppress silkscreen arcs from LPW footprint. }
   if ( (isInside) or (footprintType = 'Capacitor') ) then
   begin
      
      { Among the arcs we will suppress is the arc at origin. }
      { Flag to suppress copying this arc. }
      doCopy := False;
      
   end

   { Else allow all arcs outside the assembly outline.  Hopefully this is just the
    silkscreen external pin 1 marker. }
   else
   begin

      CLF_AddPrimNameToList({var prim} arcDst,
                            {name} 'Silkscreen_marker_pin_1_exterior',
                            {var} primNames);
      
   end { endelse }
   
end; { end CLF_ModifyAndSuppressArcs() }


{***************************************************************************
 * function CLF_CopyAllRegions()
 *  Copy all regions from source document to destination document.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CopyAllRegions(    boardSrc        : IPCB_Board;
                            var cnfGalacticInfo : TStringList;
                            var regionQueue     : TInterfaceList;
                            var fillQueue       : TInterfaceList;
                            var primNames       : TStringList;
                                padQueue        : TInterfaceList;
                                )               : Integer;

var                                         
   boardXorigin : TCoord;
   boardYorigin : TCoord;
   regionSrc    : IPCB_Region;
   regionDst    : IPCB_Region;
   iterator     : IPCB_BoardIterator;
   doCopy       : Boolean;
   numEpRegions : Integer;

begin

   { Assume success. }
   result := 0;

   { Initialize that we don't yet have EP solderpaste regions. }
   numEpRegions := 0;   
   
   { Retrieve origin information for source PcbDoc file. }
   boardXorigin := boardSrc.Xorigin;
   boardYorigin := boardSrc.Yorigin;
   WriteToDebugFile('Hello from CLF_CopyAllRegions().  Board origin:  X is ' + CoordUnitToString(boardXorigin,eMetric) + ', Y is ' + CoordUnitToString(boardYorigin,eMetric));

   {** Copy all regions from source PcbDoc file to destination library component. **}
   { Setup an iterator so that we can iterate over all PCB regions. }
   iterator        := boardSrc.BoardIterator_Create;
   iterator.AddFilter_ObjectSet(MkSet(eRegionObject));
   iterator.AddFilter_LayerSet(AllLayers);
   iterator.AddFilter_Method(eProcessAll);

   { Get a reference to the first PCB object. }
   regionSrc := iterator.FirstPCBObject;

   { Loop over all regions in this PcbDoc file. }
   while (regionSrc <> nil) do
   begin

      WriteToDebugFile(' Region Detail : '             + regionSrc.Detail);
      WriteToDebugFile(' Region Layer : '              + Layer2String(regionSrc.Layer));

      { Create new blank region. }
      regionDst := PcbServer.PCBObjectFactory(eRegionObject,eNoDimension,eCreate_Default);

      { Copy all region properties from this region in source file to new region destined for new library component. }
      CLF_CopyRegionProperties(boardXorigin,
                               boardYorigin,
                               regionSrc,
                               {var} regionDst,
                               {var} primNames);

      {* Perform any changes we want to make to regions before we do the copy to destination library. *}
      CLF_ModifyAndSuppressRegions(boardXorigin,
                                   boardYorigin,
                                   regionSrc,
                                   {var} regionDst,
                                   {var} primNames,
                                   padQueue, 
                                   {var} fillQueue,
                                   {var} doCopy,
                                   {var} numEpRegions,
                                   {var} cnfGalacticInfo);
      
      { Only copy this region to destination if that is so desired. }
      if (doCopy) then
      begin
         
         { Queue copied region for new library component. }
         regionQueue.Add(regionDst);
            
      end;

      { Advance to next region in this PcbDoc file. }
      regionSrc := iterator.NextPCBObject;

   end;  { endwhile loop over all PCB regions in this PcbDoc file. }

   { Free PCB region iterator. }
   boardSrc.BoardIterator_Destroy(iterator);

end; { end CLF_CopyAllRegions() }


{***************************************************************************
 * function CLF_GroupPadsByCompSide()
 *  Group pads by component side (west, east, north, south, etc.). 
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_GroupPadsByCompSide(var cnfGalacticInfo : TStringList;
                                     padDst          : IPCB_Pad;
                                 var state           : Integer;
                                 var lastX           : Real;
                                 var lastY           : Real;
                                     primNames       : TStringList;
                                     )               : Integer;
var                                                  
   i             : Integer;
   footprintType : TString;

begin

   { Assume success. }
   result := 0;

   { Retrieve footprint type. }
   footprintType         := cnfGalacticInfo.Values(constGilFootprintType);

   {** Group pins by which side of the component they are on. **}
   { Kludge for initializing }
   if (state = 0) then
   begin

      { Special case for BGA footprint type. }
      if (footprintType = 'BGA') then
      begin
         state := 6;   { Advance state! }
      end

      { General case for all other footprint types. }
      else
      begin
         lastX := padDst.X;
         lastY := padDst.Y;
         
         state := 1;   { Advance state! }
      end; { endelse }

   end; { endif }
   
   { Examine current state and act appropriately. }
   case state of

     { State 1:  West side of component. }
     1 :
        begin

           WriteToDebugFile(' In state 1, lastX is ' + IntToStr(lastX) + ', padDst.X is ' + IntToStr(padDst.X) + ', lastY is ' + IntToStr(lastY) + ', padDst.Y is ' + IntToStr(padDst.Y) + '.');           
           
           { See if the current pin is also on the west side of the component. }
           if ( (padDst.X = lastX) ) then
           begin
              state := state;   { Hold state }

              { Assign this pad to the west group. }
              CLF_AssignPadToGroupNum(padDst,
                                      {padGroupNum} constPadGroupWest);
              WriteToDebugFile(' Identified this as a west side pad.');
           end

           { Else see if we've skipped over to a east side pad. }
           { TODO:  This is still a bit too simplistic and will fail for SOT223 packages! }
           else if ( (padDst.Y = lastY) or (padDst.X = (-1)*lastX) ) then
           begin
              state := 3;   { Advance state }

              { Assign this pad to the east group. }
              CLF_AssignPadToGroupNum(padDst,
                                      {padGroupNum} constPadGroupEast);
              WriteToDebugFile(' Identified this as a east side pad.');
           end

           { Else hopefully we're on a south side pad. }
           else
           begin
              state := 2;   { Advance state }

              { Assign this pad to the south group. }
              CLF_AssignPadToGroupNum(padDst,
                                      {padGroupNum} constPadGroupSouth);
              WriteToDebugFile(' Identified this as a south side pad.');

           end;
           
        end; { endcase 1 }

     { State 2:  South side of component. }
     2 :
        begin

           { See if the current pin is also on the south side of the component. }
           if ( (padDst.Y = lastY) ) then
           begin
              state := state;   { Hold state }

              { Assign this pad to the south group. }
              CLF_AssignPadToGroupNum(padDst,
                                      {padGroupNum} constPadGroupSouth);
              WriteToDebugFile(' Identified this as a south side pad.');
           end

           { Else hopefully we're on a east side pad. }
           else
           begin
              state := 3;   { Advance state }

              { Assign this pad to the east group. }
              CLF_AssignPadToGroupNum(padDst,
                                      {padGroupNum} constPadGroupEast);
              WriteToDebugFile(' Identified this as a east side pad.');
           end;
           
        end; { endcase 2 }

     { State 3:  East side of component. }
     3 :
        begin

           { See if the current pin is also on the east side of the component. }
           if ( (padDst.X = lastX) ) then
           begin
              state := state;   { Hold state }

              { Assign this pad to the east group. }
              CLF_AssignPadToGroupNum(padDst,
                                      {padGroupNum} constPadGroupEast);
              WriteToDebugFile(' Identified this as a east side pad.');
           end

           { Else see if we've skipped to an EP pad. }
           else if (  CLF_IsPadEp(padDst) or
                    ( (padDst.X = 0.0) and (padDst.Y = 0.0) )  ) then
           begin

              { Advance state to handle EP pads. }
              state := 5;   { Advance state }

              { Assign this pad to the EP group. }
              CLF_AssignPadToGroupNum(padDst,
                                      {padGroupNum} constPadGroupEp);
              WriteToDebugFile(' Identified this as an EP pad.');
           end

           { Else hopefully we're on a north side pad. }
           else
           begin
              state := 4;   { Advance state }

              { Assign this pad to the north group. }
              CLF_AssignPadToGroupNum(padDst,
                                      {padGroupNum} constPadGroupNorth);
              WriteToDebugFile(' Identified this as a north side pad.');
           end;
           
        end; { endcase 3 }

     { State 4:  North side of component. }
     4 :
        begin

           { See if the current pin is also on the north side of the component. }
           if ( (padDst.Y = lastY) ) then
           begin
              state := state;   { Hold state }

              { Assign this pad to the north group. }
              CLF_AssignPadToGroupNum(padDst,
                                      {padGroupNum} constPadGroupNorth);
              WriteToDebugFile(' Identified this as a north side pad.');
           end

           { Else see if we've skipped to an EP pad. }
           else if (  CLF_IsPadEp(padDst) or
                    ( (padDst.X = 0.0) and (padDst.Y = 0.0) )  ) then
           begin

              { Advance state to handle EP pads. }
              state := 5;   { Advance state }

              { Assign this pad to the EP group. }
              CLF_AssignPadToGroupNum(padDst,
                                      {padGroupNum} constPadGroupEp);
              WriteToDebugFile(' Identified this as an EP pad.');
           end

           { Else we're off to the great unknown. }
           else
           begin
              state := 7;   { Advance state }

              { Assign this pad to the unknown group. }
              CLF_AssignPadToGroupNum(padDst,
                                      {padGroupNum} constPadGroupUnknown);
              WriteToDebugFile(' Identified this as a unknown side pad.');
           end;
           
        end; { endcase 4 }

     { State 5:  Exposed pads (EP). }
     5 :
        begin

           { See if the current pad is also an EP pad. }
           if (  CLF_IsPadEp(padDst) or
                    ( (padDst.X = 0.0) and (padDst.Y = 0.0) )  ) then
           begin
              state := state;   { Hold state }

              { Assign this pad to the EP group. }
              CLF_AssignPadToGroupNum(padDst,
                                      {padGroupNum} constPadGroupEp);
              WriteToDebugFile(' Identified this as an EP pad.');
           end

           { Else we're off to the great unknown. }
           else
           begin
              state := 7;   { Advance state }

              { Assign this pad to the unknown group. }
              CLF_AssignPadToGroupNum(padDst,
                                      {padGroupNum} constPadGroupUnknown);
              WriteToDebugFile(' Identified this as a unknown side pad.');
           end;
           
        end; { endcase 5 }

     { State 6:  Center of component (BGA pads). }
     6 :
        begin

           { See if the current pin is also on the center side of the component. }
           if (not CLF_IsPadFiducial(padDst)) then
           begin
              state := state;   { Hold state }

              { Assign this pad to the center group. }
              CLF_AssignPadToGroupNum(padDst,
                                      {padGroupNum} constPadGroupCenter);
              WriteToDebugFile(' Identified this as a center (BGA) pad.');
           end

           { Else we're off to the great unknown. }
           else
           begin
              state := 7;   { Advance state }

              { Assign this pad to the unknown group. }
              CLF_AssignPadToGroupNum(padDst,
                                      {padGroupNum} constPadGroupUnknown);
              WriteToDebugFile(' Identified this as a unknown side pad.');
           end;
           
        end; { endcase 6 }

     { State 7:  Unknown side of component. }
     7 :
        begin

           { See if the current pin is also on the unknown side of the component. }
           state := state;  { Hold state }
           
           { Assign this pad to the unknown group. }
           CLF_AssignPadToGroupNum(padDst,
                                   {padGroupNum} constPadGroupUnknown);
           WriteToDebugFile(' Identified this as a unknown side pad.');

        end; { endcase 7 }

   else CLF_Abort('Unknown state ' + IntToStr(state));
   end; { endcase }          
   
   { Keep history for next time around. }
   lastX := padDst.X;
   lastY := padDst.Y;
   
   WriteToDebugFile(' Stored pad group name is "' + CLF_GetPadGroupNamePrefix({padGroupNum} CLF_GetPadGroupNum(padDst), primNames) + '".');
   
end; { end CLF_GroupPadsByCompSide() }


{***************************************************************************
 * function CLF_CopyAllPads()
 *  Copy all pads from source document to destination document.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CopyAllPads(    boardSrc        : IPCB_Board;
                         var cnfGalacticInfo : TStringList;
                         var padQueue        : TInterfaceList;
                         var primNames       : TStringList;
                             )               : Integer;

var                                         
   boardXorigin : TCoord;
   boardYorigin : TCoord;
   padSrc       : IPCB_Pad;
   padDst       : IPCB_Pad;
   padNew       : IPCB_Pad;
   iterator     : IPCB_BoardIterator;
   doCopy       : Boolean;
   state        : Integer;
   lastX        : Real;
   lastY        : Real;

begin

   { Assume success. }
   result := 0;
   
   { Retrieve origin information for source PcbDoc file. }
   boardXorigin := boardSrc.Xorigin;
   boardYorigin := boardSrc.Yorigin;
   WriteToDebugFile('Hello from CLF_CopyAllPads().  Board origin:  X is ' + CoordUnitToString(boardXorigin,eMetric) + ', Y is ' + CoordUnitToString(boardYorigin,eMetric));

   {** Copy all pads from source PcbDoc file to destination library component. **}
   { Setup an iterator so that we can iterate over all PCB pads. }
   iterator        := boardSrc.BoardIterator_Create;
   iterator.AddFilter_ObjectSet(MkSet(ePadObject));
   iterator.AddFilter_LayerSet(AllLayers);
   iterator.AddFilter_Method(eProcessAll);

   { Get a reference to the first PCB object. }
   padSrc := iterator.FirstPCBObject;

   { Panic if we can't find any pads. }
   if (padSrc = Nil) then
   begin
      boardSrc.BoardIterator_Destroy(iterator);
      CLF_Abort('Unable to find any PCB pads!');
   end;

   { Initialize state variable to the "init" state to trigger CLF_GroupPadsByCompSide() to initialize things. }
   state := 0;
   
   { Loop over all pads in this PcbDoc file. }
   while (padSrc <> nil) do
   begin

      WriteToDebugFile('*Found pad ' + padSrc.Name + ', X is ' + CoordUnitToString(padSrc.X,eMetric) + ', Y is ' + CoordUnitToString(padSrc.Y,eMetric));
//      WriteToDebugFile(' TopXSize is ' + CoordUnitToString(padSrc.TopXSize,eMetric) + ', TopYSize is ' + CoordUnitToString(padSrc.TopYSize,eMetric));

//      { Unfortunately, this doesn't work! }
//      { Replicate the source pad and assign this to the destination pad. }
//      padDst := Nil;
//      padDst := padSrc.Replicate();
//      if (padDst = Nil) then
//         CLF_Abort('Pad replication failed!');

      { Create new blank pad. }
      padDst := PcbServer.PCBObjectFactory(ePadObject,eNoDimension,eCreate_Default);

      { Copy all pad properties from this pad in source file to new pad destined for new library component. }
      CLF_CopyPadProperties(boardXorigin,
                            boardYorigin,
                            padSrc,
                            {var} padDst,
                            {var} primNames);

      {* Perform any changes we want to make to pads before we do the copy to destination library. *}
      CLF_ModifyAndSuppressPads(boardXorigin,
                                boardYorigin,
                                padSrc,
                                {var} padDst,
                                {var} primNames,
                                {var} doCopy,
                                {var} padNew,
                                {var} cnfGalacticInfo);                                                            
      
      { Only copy this pad to destination if that is so desired. }
      if (doCopy) then
      begin
         
         { Queue copied pad for new library component. }
         padQueue.Add(padDst);

         { Group pads by component side (west, east, north, south) for future reference. }
         CLF_GroupPadsByCompSide({var} cnfGalacticInfo,
                                 padDst,
                                 {var} state,
                                 {var} lastX,
                                 {var} lastY,
                                 primNames);
         
      end;

      { If CLF_ModifyAndSuppressPads() has created a new pin for us, then give it the same treatment. }
      if (padNew <> Nil) then
      begin
         
         { Queue new pad for new library component. }
         padQueue.Add(padNew);

         { Group pads by component side (west, east, north, south) for future reference. }
         CLF_GroupPadsByCompSide({var} cnfGalacticInfo,
                                 padNew,
                                 {var} state,
                                 {var} lastX,
                                 {var} lastY,
                                 primNames);
         
      end;

      { Advance to next pad in this PcbDoc file. }
      padSrc := iterator.NextPCBObject;

   end;  { endwhile loop over all PCB pads in this PcbDoc file. }

   { Free PCB pad iterator. }
   boardSrc.BoardIterator_Destroy(iterator);

end; { end CLF_CopyAllPads() }


{***************************************************************************
 * function CLF_CopyAllTracks()
 *  Copy all tracks from source document to destination document.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CopyAllTracks(    boardSrc        : IPCB_Board;
                           var cnfGalacticInfo : TStringList;
                           var trackQueue      : TInterfaceList;
                           var primNames       : TStringList;
                               )               : Integer;

var                                         
   boardXorigin      : TCoord;
   boardYorigin      : TCoord;
   trackSrc          : IPCB_Track;
   trackDst          : IPCB_Track;
   iterator          : IPCB_BoardIterator;
   srcX1Mm           : Real;
   srcY1Mm           : Real;
   srcX2Mm           : Real;
   srcY2Mm           : Real;
   doCopy            : Boolean;

begin

   { Assume success. }
   result := 0;
   
   { Retrieve origin information for source PcbDoc file. }
   boardXorigin := boardSrc.Xorigin;
   boardYorigin := boardSrc.Yorigin;
   WriteToDebugFile('Hello from CLF_CopyAllTracks().  Board origin:  X is ' + CoordUnitToString(boardXorigin,eMetric) + ', Y is ' + CoordUnitToString(boardYorigin,eMetric));

   {** Copy all tracks from source PcbDoc file to destination library component. **}
   { Setup an iterator so that we can iterate over all PCB tracks. }
   iterator        := boardSrc.BoardIterator_Create;
   iterator.AddFilter_ObjectSet(MkSet(eTrackObject));
   iterator.AddFilter_LayerSet(AllLayers);
   iterator.AddFilter_Method(eProcessAll);

   { Get a reference to the first PCB object. }
   trackSrc := iterator.FirstPCBObject;

   { Panic if we can't find any tracks. }
   if (trackSrc = Nil) then
   begin
      boardSrc.BoardIterator_Destroy(iterator);
      CLF_Abort('Unable to find any PCB tracks!');
   end;

   { Loop over all tracks in this PcbDoc file. }
   while (trackSrc <> nil) do
   begin
      
      WriteToDebugFile('X1 is ' + CoordUnitToString(trackSrc.X1,eMetric) + ', Y1 is ' + CoordUnitToString(trackSrc.Y1,eMetric) + ', layer is ' + IntToStr(trackSrc.Layer));

      { Create new blank track. }
      trackDst := PcbServer.PCBObjectFactory(eTrackObject,eNoDimension,eCreate_Default);

      { Copy all track properties from this track in source file to new track destined for new library component. }
      CLF_CopyTrackProperties(boardXorigin,
                              boardYorigin,
                              trackSrc,
                              {var} trackDst,
                              {var} primNames);

      {* Perform any changes we want to make to tracks before we do the copy to destination library. *}
      CLF_ModifyAndSuppressTracks(boardXorigin,
                                  boardYorigin,
                                  trackSrc,
                                  {var} trackDst,
                                  {var} primNames,
                                  {var} doCopy,
                                  {var} cnfGalacticInfo);                            
                                  

      { Only copy this track to destination if that is so desired. }
      if (doCopy) then
      begin
      
         { Queue copied track for new library component. }
         trackQueue.Add(trackDst);
         
      end;
            
      { Advance to next track in this PcbDoc file. }
      trackSrc := iterator.NextPCBObject;

   end;  { endwhile loop over all PCB tracks in this PcbDoc file. }

   { Free PCB track iterator. }
   boardSrc.BoardIterator_Destroy(iterator);

   
end; { end CLF_CopyAllTracks() }


{***************************************************************************
 * function CLF_CopyAllArcs()
 *  Copy all arcs from source document to destination document.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CopyAllArcs(var boardSrc        : IPCB_Board;
                         var cnfGalacticInfo : TStringList;
                         var arcQueue        : TInterfaceList;
                         var primNames       : TStringList;
                             )               : Integer;

var                                         
   boardXorigin : TCoord;
   boardYorigin : TCoord;
   arcSrc       : IPCB_Arc;
   arcDst       : IPCB_Arc;
   iterator     : IPCB_BoardIterator;
   doCopy       : Boolean;
                
begin

   { Assume success. }
   result := 0;
   
   { Retrieve origin information for source PcbDoc file. }
   boardXorigin := boardSrc.Xorigin;
   boardYorigin := boardSrc.Yorigin;
   WriteToDebugFile('Hello from CLF_CopyAllArcs().  Board origin:  X is ' + CoordUnitToString(boardXorigin,eMetric) + ', Y is ' + CoordUnitToString(boardYorigin,eMetric));

   {** Copy all arcs from source PcbDoc file to destination library component. **}
   { Setup an iterator so that we can iterate over all PCB arcs. }
   iterator        := boardSrc.BoardIterator_Create;
   iterator.AddFilter_ObjectSet(MkSet(eArcObject));
   iterator.AddFilter_LayerSet(AllLayers);
   iterator.AddFilter_Method(eProcessAll);

   { Get a reference to the first PCB object. }
   arcSrc := iterator.FirstPCBObject;

   { Panic if we can't find any arcs. }
   if (arcSrc = Nil) then
   begin
      boardSrc.BoardIterator_Destroy(iterator);
      CLF_Abort('Unable to find any PCB arcs!');
   end;

   { Loop over all arcs in this PcbDoc file. }
   while (arcSrc <> nil) do
   begin
      
//      WriteToDebugFile('X1 is ' + CoordUnitToString(arcSrc.X1,eMetric) + ', Y1 is ' + CoordUnitToString(arcSrc.Y1,eMetric));

      { Create new blank arc. }
      arcDst := PcbServer.PCBObjectFactory(eArcObject,eNoDimension,eCreate_Default);

      { Copy all arc properties from this arc in source file to new arc destined for new library component. }
      CLF_CopyArcProperties(boardXorigin,
                            boardYorigin,
                            arcSrc,
                            {var} arcDst,
                            {var} primNames);

      {* Perform any changes we want to make to arcs before we do the copy to destination library. *}
      CLF_ModifyAndSuppressArcs(boardXorigin,
                                boardYorigin,
                                arcSrc,
                                {var} arcDst,
                                {var} primNames,
                                {var} doCopy,
                                {var} cnfGalacticInfo);                                                            
      
      { Only copy this arc to destination if that is so desired. }
      if (doCopy) then
      begin
      
         { Queue copied arc for new library component. }
         arcQueue.Add(arcDst);

      end;

      { Advance to next arc in this PcbDoc file. }
      arcSrc := iterator.NextPCBObject;

   end;  { endwhile loop over all PCB arcs in this PcbDoc file. }

   { Free PCB arc iterator. }
   boardSrc.BoardIterator_Destroy(iterator);

end; { end CLF_CopyAllArcs() }


{***************************************************************************
 * function CLF_FindTrackByEndpoints()
 *  Find a track on a given layer by matching specified endpoints.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_FindTrackByEndpoints(    layer      : Integer;
                                      X1mm       : Real;
                                      Y1mm       : Real;
                                      X2mm       : Real;
                                      Y2mm       : Real;
                                      trackQueue : TInterfaceList;
                                  var trackDst   : IPCB_Track;
                                      )          : Integer;
var                                               
   i        : Integer;
   trackFoo : IPCB_Track;
   X1coord  : TCoord;
   X2coord  : TCoord;
   Y1coord  : TCoord;
   Y2coord  : TCoord;
          
begin

   { Assume success. }
   result := 0;

   { Flag that we have not yet found what we're looking for. }
   trackDst := 0;

   { Convert to coordinate units. }
   X1coord := MMsToCoord(X1mm);
   X2coord := MMsToCoord(X2mm);
   Y1coord := MMsToCoord(Y1mm);
   Y2coord := MMsToCoord(Y2mm);
  
   WriteToDebugFile('Hello from CLF_FindTrackByEndpoints(), looking for ' + FloatToStr(X1mm) + ',' + FloatToStr(Y1mm) + ' ' + FloatToStr(X2mm) + ',' + FloatToStr(Y2mm) + ', layer ' + IntToStr(layer) + '.');
   WriteToDebugFile('Hello from CLF_FindTrackByEndpoints(), looking for ' + IntToStr(X1coord) + ',' + IntToStr(Y1coord) + ' ' + IntToStr(X2coord) + ',' + IntToStr(Y2coord) + ', layer ' + IntToStr(layer) + '.');

   { Loop over all tracks that have already been queued. }
   for i := 0 to (trackQueue.Count - 1) do
   begin

      { Retrieve reference to queued track. }
      trackFoo := trackQueue.items[i];
      
      { Make sure this is a valid track. }
      if (trackFoo <> 0) then
      begin

         WriteToDebugFile(' Examining track with endpoints of ' + IntToStr((trackFoo.X1)) + ',' + IntToStr((trackFoo.Y1)) + ' ' + IntToStr((trackFoo.X2)) + ',' + IntToStr((trackFoo.Y2)) + ', layer ' + IntToStr(trackFoo.Layer) + '.');
         
         { Examine endpoints and layer. }
         if (   ( (
             (trackFoo.X1 = X1coord) and
             (trackFoo.X2 = X2coord) and
             (trackFoo.Y1 = Y1coord) and
             (trackFoo.Y2 = Y2coord) ) or
             
             ( (trackFoo.X2 = X1coord) and
             (trackFoo.X1 = X2coord) and
             (trackFoo.Y2 = Y1coord) and
             (trackFoo.Y1 = Y2coord) ) ) and
             
             (trackFoo.Layer = layer)   ) then
         begin

            { Return found track to caller. }
            trackDst := trackFoo;
            WriteToDebugFile(' Found it!');            
            
         end; { endif }

      end; { endif valid track }

   end; { endfor }

   { Make sure we succeeded.  If not, return error code to caller. }
   if (trackDst = 0) then
      result := 1;
   
end; { end CLF_FindTrackByEndpoints() }


{***************************************************************************
 * function CLF_FindTrackByEndpointsWithAbort()
 *  Find a track on a given layer by matching specified endpoints.
 *  Abort script if we are unable to find said track.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_FindTrackByEndpointsWithAbort(    layer      : Integer;
                                               X1mm       : Real;
                                               Y1mm       : Real;
                                               X2mm       : Real;
                                               Y2mm       : Real;
                                               trackQueue : TInterfaceList;
                                           var trackDst   : IPCB_Track;
                                               )          : Integer;
var                                               
   rc : Integer;
          
begin

   { Assume success. }
   result := 0;

   { Call CLF_FindTrackByEndpoints() to do all the real work. }
   rc := CLF_FindTrackByEndpoints(layer,
                                  X1mm,
                                  Y1mm,
                                  X2mm,
                                  Y2mm,
                                  trackQueue,
                                  {var} trackDst);

   { See if we have an error code.  If so, abort script. }
   if (rc <> 0) then
      CLF_Abort('Could not find the track I was searching for! ' + FloatToStr(X1mm) + ',' + FloatToStr(Y1mm) + ' ' + FloatToStr(X2mm) + ',' + FloatToStr(Y2mm) + ' ');      
   
end; { end CLF_FindTrackByEndpointsWithAbort() }


{***************************************************************************
 * function CLF_ModifyTrackEndpoints()
 *  Modify an existing track by specifying new endpoints.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ModifyTrackEndpoints(    X1mm     : Real;
                                      Y1mm     : Real;
                                      X2mm     : Real;
                                      Y2mm     : Real;
                                  var trackDst : IPCB_Track;
                                      )        : Integer;
var                                               
   i        : Integer;
          
begin

   { Assume success. }
   result := 0;

   { Modify existing assembly outline. }
   trackDst.X1 := MMsToCoord(X1mm);
   trackDst.Y1 := MMsToCoord(Y1mm);
   trackDst.X2 := MMsToCoord(X2mm);
   trackDst.Y2 := MMsToCoord(Y2mm);
   
end; { end CLF_ModifyTrackEndpoints() }


{***************************************************************************
 * function CLF_LookForAndRenameBoundaryTrack()
 *  Look for a specific existing track as one side's boundary track.
 *  If we find such a track, append the side name to its existing name.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_LookForAndRenameBoundaryTrack(    layer      : Integer;
                                               X1mm       : Real;
                                               Y1mm       : Real;
                                               X2mm       : Real;
                                               Y2mm       : Real;
                                               side       : TString;
                                               trackQueue : TInterfaceList;
                                           var primNames  : TStringList;
                                               )          : Integer;

var                                         
   rc              : Integer;
   trackDst        : IPCB_Track;
   name            : TString;
   
begin

   { Assume success. }
   result := 0;
   
   WriteToDebugFile('Hello from CLF_LookForAndRenameBoundaryTrack()');

   { Look for the specified boundary track. }
   rc := CLF_FindTrackByEndpoints(layer,
                                  X1mm,
                                  Y1mm,
                                  X2mm,
                                  Y2mm,
                                  trackQueue,
                                  {var} trackDst);

   { See if we found such a boundary track. }
   if (rc = 0) then
   begin

      WriteToDebugFile(' Found ' + side + ' boundary track!');

      { Retrieve current name of this track. }
      name := CLF_GetPrimName({prim} trackDst,
                              primNames);
      
      { Append the side suffix to the name. }
      name := name + side;
      
      { Update the primitive name. }
      CLF_ChangePrimName({var prim} trackDst,
                         name,
                         {var} primNames);

   end; { endif }

   
end; { end CLF_LookForAndRenameBoundaryTrack() }


{***************************************************************************
 * function CLF_LookForAndModifyWidthOfBoundaryTrack()
 *  Look for a specific existing track as one side's boundary track.
 *  If we find such a track, change its width as specified.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_LookForAndModifyWidthOfBoundaryTrack(    layer      : Integer;
                                                      X1mm       : Real;
                                                      Y1mm       : Real;
                                                      X2mm       : Real;
                                                      Y2mm       : Real;
                                                      newWidthMm : Real;
                                                      trackQueue : TInterfaceList;
                                                      )          : Integer;

var                                         
   rc              : Integer;
   trackDst        : IPCB_Track;
   name            : TString;
   
begin

   { Assume success. }
   result := 0;
   
   WriteToDebugFile('Hello from CLF_LookForAndModifyWidthOfBoundaryTrack(), layer is ' + IntToStr(layer) + '.');

   { Look for the specified boundary track. }
   rc := CLF_FindTrackByEndpointsWithAbort(layer,
                                           X1mm,
                                           Y1mm,
                                           X2mm,
                                           Y2mm,
                                           trackQueue,
                                           {var} trackDst);

   { See if we found such a boundary track. }
   if (rc = 0) then
   begin

      WriteToDebugFile(' Found boundary track!');

      { Modify width of this boundary track. }
      trackDst.Width               := MMsToCoord(newWidthMm);
      
   end; { endif }
   
end; { end CLF_LookForAndModifyWidthOfBoundaryTrack() }


{***************************************************************************
 * function CLF_UpdateTrackNamesFromBoundingRectangle()
 *  Retrieve the bounding rectangle for a given layer.  For that layer,
 *  look for tracks that exactly match each side of that rectangle.
 *  If any such are found, update their names by appending the side.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_UpdateTrackNamesFromBoundingRectangle(    layer           : Integer;
                                                       namePrefix      : TString;
                                                   var cnfGalacticInfo : TStringList;
                                                       trackQueue      : TInterfaceList;
                                                   var primNames       : TStringList;
                                                    )                  : Integer;

var                                         
   rc              : Integer;
   boundaryWestMm  : Real;
   boundaryEastMm  : Real;
   boundaryNorthMm : Real;
   boundarySouthMm : Real;
   trackDst        : IPCB_Track;
   name            : TString;
   
begin

   { Assume success. }
   result := 0;
   
   WriteToDebugFile('Hello from CLF_UpdateTrackNamesFromBoundingRectangle()');

   { Retrieve the bounds for this name prefix. }
   CLF_RetrieveBoundingRectangleByNamePrefix(namePrefix,
                                             {var} cnfGalacticInfo,
                                             {var} boundaryWestMm,
                                             {var} boundaryEastMm,
                                             {var} boundaryNorthMm,
                                             {var} boundarySouthMm);

   { Look for the west boundary track and update its name if found. }
   CLF_LookForAndRenameBoundaryTrack(layer,
                                     {X1mm} boundaryWestMm,
                                     {Y1mm} boundaryNorthMm,
                                     {X2mm} boundaryWestMm,
                                     {Y2mm} boundarySouthMm,
                                     {side} '_outline-west',
                                     trackQueue,
                                     {var} primNames);

   { Look for the east boundary track and update its name if found. }
   CLF_LookForAndRenameBoundaryTrack(layer,
                                     {X1mm} boundaryEastMm,
                                     {Y1mm} boundaryNorthMm,
                                     {X2mm} boundaryEastMm,
                                     {Y2mm} boundarySouthMm,
                                     {side} '_outline-east',
                                     trackQueue,
                                     {var} primNames);

   { Look for the north boundary track and update its name if found. }
   CLF_LookForAndRenameBoundaryTrack(layer,
                                     {X1mm} boundaryWestMm,
                                     {Y1mm} boundaryNorthMm,
                                     {X2mm} boundaryEastMm,
                                     {Y2mm} boundaryNorthMm,
                                     {side} '_outline-north',
                                     trackQueue,
                                     {var} primNames);

   { Look for the south boundary track and update its name if found. }
   CLF_LookForAndRenameBoundaryTrack(layer,
                                     {X1mm} boundaryWestMm,
                                     {Y1mm} boundarySouthMm,
                                     {X2mm} boundaryEastMm,
                                     {Y2mm} boundarySouthMm,
                                     {side} '_outline-south',
                                     trackQueue,
                                     {var} primNames);

end; { end CLF_UpdateTrackNamesFromBoundingRectangle() }


{***************************************************************************
 * function CLF_UpdateTrackWidthFromBoundingRectangle()
 *  Retrieve the bounding rectangle for a given layer.  For that layer,
 *  look for tracks that exactly match each side of that rectangle.
 *  If any such are found, update their width as specified.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_UpdateTrackWidthFromBoundingRectangle(    layer           : Integer;
                                                       namePrefix      : TString;
                                                       newWidthMm      : Real;
                                                   var cnfGalacticInfo : TStringList;
                                                       trackQueue      : TInterfaceList;
                                                    )                  : Integer;

var                                         
   rc              : Integer;
   boundaryWestMm  : Real;
   boundaryEastMm  : Real;
   boundaryNorthMm : Real;
   boundarySouthMm : Real;
   trackDst        : IPCB_Track;
   name            : TString;
   
begin

   { Assume success. }
   result := 0;
   
   WriteToDebugFile('Hello from CLF_UpdateTrackWidthFromBoundingRectangle(), newWidthMm is ' + FloatToStr(newWidthMm) + '.');

   { Retrieve the bounds for this name prefix. }
   CLF_RetrieveBoundingRectangleByNamePrefix(namePrefix,
                                             {var} cnfGalacticInfo,
                                             {var} boundaryWestMm,
                                             {var} boundaryEastMm,
                                             {var} boundaryNorthMm,
                                             {var} boundarySouthMm);

   { Look for the west boundary track and update its width if found. }
   CLF_LookForAndModifyWidthOfBoundaryTrack(layer,
                                            {X1mm} boundaryWestMm,
                                            {Y1mm} boundaryNorthMm,
                                            {X2mm} boundaryWestMm,
                                            {Y2mm} boundarySouthMm,
                                            newWidthMm,
                                            trackQueue);

   { Look for the east boundary track and update its width if found. }
   CLF_LookForAndModifyWidthOfBoundaryTrack(layer,
                                            {X1mm} boundaryEastMm,
                                            {Y1mm} boundaryNorthMm,
                                            {X2mm} boundaryEastMm,
                                            {Y2mm} boundarySouthMm,
                                            newWidthMm,
                                            trackQueue);

   { Look for the north boundary track and update its width if found. }
   CLF_LookForAndModifyWidthOfBoundaryTrack(layer,
                                            {X1mm} boundaryWestMm,
                                            {Y1mm} boundaryNorthMm,
                                            {X2mm} boundaryEastMm,
                                            {Y2mm} boundaryNorthMm,
                                            newWidthMm,
                                            trackQueue);

   { Look for the south boundary track and update its width if found. }
   CLF_LookForAndModifyWidthOfBoundaryTrack(layer,
                                            {X1mm} boundaryWestMm,
                                            {Y1mm} boundarySouthMm,
                                            {X2mm} boundaryEastMm,
                                            {Y2mm} boundarySouthMm,
                                            newWidthMm,
                                            trackQueue);

end; { end CLF_UpdateTrackWidthFromBoundingRectangle() }


{***************************************************************************
 * function CLF_BreakHorizontalTrack()
 *  "Break" a horizontal track by moving endpoint of existing track to end
 *  at the "break" point.  Then create new horizontal track to "resume"
 *  and end at the original endpoint.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_BreakHorizontalTrack(    layer      : Integer;
                                      widthMm    : Real;
                                      X1mm       : Real;
                                      Y1mm       : Real;
                                      X2mm       : Real;
                                      Y2mm       : Real;
                                      XbreakMm   : Real;
                                      XresumeMm  : Real;
                                  var trackQueue : TInterfaceList;
                                  var primNames  : TStringList;
                                      )          : Integer;
var                                               
   i        : Integer;
   trackDst : IPCB_Track;
   name     : TString;
          
begin

   { Assume success. }
   result := 0;

   { Find this existing track. }
   CLF_FindTrackByEndpointsWithAbort(layer,
                                     X1mm,
                                     Y1mm,
                                     X2mm,
                                     Y2mm,
                                     trackQueue,
                                     {var} trackDst);

   { Modify existing track. }
   CLF_ModifyTrackEndpoints(X1mm,
                            Y1mm,
                            {X2mm} XbreakMm,    { New value }
                            Y2mm,
                            {var} trackDst);

   { Retrieve name of existing track. }
   name := CLF_GetPrimName({prim} trackDst,
                           primNames);

   { Append a "-B" to end of name for use by our new track. }
   name := name + '-B';

   { Create new track for the resumed segment. }
   CLF_CreateNewTrack(layer,
                      widthMm,
                      {X1mm} XresumeMm,
                      X2mm,
                      Y1mm,
                      Y2mm,
                      {var} trackQueue,
                      name,
                      {var} primNames
                      );
   
end; { end CLF_BreakHorizontalTrack() }


{***************************************************************************
 * function CLF_BreakVerticalTrack()
 *  "Break" a vertical track by moving endpoint of existing track to end
 *  at the "break" point.  Then create new vertical track to "resume"
 *  and end at the original endpoint.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_BreakVerticalTrack(    layer      : Integer;
                                    widthMm    : Real;
                                    X1mm       : Real;
                                    Y1mm       : Real;
                                    X2mm       : Real;
                                    Y2mm       : Real;
                                    YbreakMm   : Real;
                                    YresumeMm  : Real;
                                var trackQueue : TInterfaceList;
                                var primNames  : TStringList;
                                    )          : Integer;
var                                               
   i        : Integer;
   trackDst : IPCB_Track;
   name     : TString;
          
begin

   { Assume success. }
   result := 0;

   { Find this existing track. }
   CLF_FindTrackByEndpointsWithAbort(layer,
                                     X1mm,
                                     Y1mm,
                                     X2mm,
                                     Y2mm,
                                     trackQueue,
                                     {var} trackDst);

   { Modify existing track. }
   CLF_ModifyTrackEndpoints(X1mm,
                            Y1mm,
                            X2mm, 
                            {Y2mm} YbreakMm,    { New value }
                            {var} trackDst);

   { Retrieve name of existing track. }
   name := CLF_GetPrimName({prim} trackDst,
                           primNames);

   { Append a "-B" to end of name for use by our new track. }
   name := name + '-B';

   { Create new track for the resumed segment. }
   CLF_CreateNewTrack(layer,
                      widthMm,
                      X1mm,
                      X2mm,
                      {Y1mm} YresumeMm,
                      Y2mm,
                      {var} trackQueue,
                      name,
                      {var} primNames
                      );
   
end; { end CLF_BreakVerticalTrack() }


{***************************************************************************
 * function CLF_CreateNewDiagonalTrackBorders()
 *  Create rectangular tracks starting at the outside corners and coming in.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewDiagonalTrackBorders(    layer             : Integer;
                                               widthMm           : Real;
                                               outerBoundWestMm  : Real;
                                               outerBoundEastMm  : Real;
                                               outerBoundNorthMm : Real;
                                               outerBoundSouthMm : Real;
                                               deltaXwestMm      : Real;
                                               deltaXeastMm      : Real;
                                               deltaYmm          : Real;
                                           var trackQueue        : TInterfaceList;
                                               namePrefix        : TString;
                                           var primNames         : TStringList;
                                               )                 : Integer;
var
   i                    : Integer;
                                               
begin

   { Assume success. }
   result := 0;

   { North west }
   CLF_CreateNewTrack(layer,
                      widthMm,
                      (outerBoundWestMm),
                      (outerBoundWestMm + deltaXwestMm),
                      (outerBoundNorthMm),
                      (outerBoundNorthMm - deltaYmm),
                      {var} trackQueue,
                      {name} (namePrefix + '-north-west'),
                      {var} primNames
                      );

   { South west }
   CLF_CreateNewTrack(layer,
                      widthMm,
                      (outerBoundWestMm),
                      (outerBoundWestMm + deltaXwestMm),
                      (outerBoundSouthMm),
                      (outerBoundSouthMm + deltaYmm),
                      {var} trackQueue,
                      {name} (namePrefix + '-south-west'),
                      {var} primNames
                      );

   { South east }
   CLF_CreateNewTrack(layer,
                      widthMm,
                      (outerBoundEastMm),
                      (outerBoundEastMm - deltaXeastMm),
                      (outerBoundSouthMm),
                      (outerBoundSouthMm + deltaYmm),
                      {var} trackQueue,
                      {name} (namePrefix + '-south-east'),
                      {var} primNames
                      );

   { North east }
   CLF_CreateNewTrack(layer,
                      widthMm,
                      (outerBoundEastMm),
                      (outerBoundEastMm - deltaXeastMm),
                      (outerBoundNorthMm),
                      (outerBoundNorthMm - deltaYmm),
                      {var} trackQueue,
                      {name} (namePrefix + '-north-east'),
                      {var} primNames
                      );

end; { end CLF_CreateNewDiagonalTrackBorders() }


{***************************************************************************
 * function CLF_CreateNewTrackPeninsula()
 *  Create a feature consisting of 3 tracks with the specified endpoints.
 *  The missing piece is from (x1,y1) to (x4,y4).
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewTrackPeninsula(    layer      : Integer;
                                         widthMm    : Real;
                                         X1mm       : Real;
                                         Y1mm       : Real;
                                         X2mm       : Real;
                                         Y2mm       : Real;
                                         X3mm       : Real;
                                         Y3mm       : Real;
                                         X4mm       : Real;
                                         Y4mm       : Real;
                                     var trackQueue : TInterfaceList;
                                         namePrefix : TString;
                                     var primNames  : TStringList;
                                         )          : Integer;
var
   i                    : Integer;
                                               
begin

   { Assume success. }
   result := 0;

   {* Create a peninsula. *}
   CLF_CreateNewTrack(layer,
                      widthMm,
                      X1mm,
                      X2mm,
                      Y1mm,
                      Y2mm,
                      {var} trackQueue,
                      {name} (namePrefix + '-1'),
                      {var} primNames
                      );

   CLF_CreateNewTrack(layer,
                      widthMm,
                      X2mm,
                      X3mm,
                      Y2mm,
                      Y3mm,
                      {var} trackQueue,
                      {name} (namePrefix + '-2'),
                      {var} primNames
                      );

   CLF_CreateNewTrack(layer,
                      widthMm,
                      X3mm,
                      X4mm,
                      Y3mm,
                      Y4mm,
                      {var} trackQueue,
                      {name} (namePrefix + '-3'),
                      {var} primNames
                      );

end; { end CLF_CreateNewTrackPeninsula() }


{***************************************************************************
 * function CLF_CreateNewTrackPeninsula2()
 *  Create a peninsula of 3 tracks.
 *  Specify all 4 boundaries, plus the name of the one boundary line to omit.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewTrackPeninsula2(    layer           : Integer;
                                          widthMm         : Real;
                                          boundaryWestMm  : Real;
                                          boundaryEastMm  : Real;
                                          boundaryNorthMm : Real;
                                          boundarySouthMm : Real;
                                          omitSide        : TString;
                                      var trackQueue      : TInterfaceList;
                                          namePrefix      : TString;
                                      var primNames       : TStringList;
                                         )                : Integer;
var
   i                    : Integer;
                                               
begin

   { Assume success. }
   result := 0;

   { Create west boundary. }
   if (omitSide <> 'West') then
      CLF_CreateNewTrack(layer,
                         widthMm,
                         {X1mm} boundaryWestMm,
                         {X2mm} boundaryWestMm,
                         {Y1mm} boundaryNorthMm,
                         {Y2mm} boundarySouthMm,
                         {var} trackQueue,
                         {name} (namePrefix + '-west'),
                         {var} primNames
                         );

   { Create east boundary. }
   if (omitSide <> 'East') then
      CLF_CreateNewTrack(layer,
                         widthMm,
                         {X1mm} boundaryEastMm,
                         {X2mm} boundaryEastMm,
                         {Y1mm} boundaryNorthMm,
                         {Y2mm} boundarySouthMm,
                         {var} trackQueue,
                         {name} (namePrefix + '-east'),
                         {var} primNames
                         );

   { Create north boundary. }
   if (omitSide <> 'North') then
      CLF_CreateNewTrack(layer,
                         widthMm,
                         {X1mm} boundaryWestMm,
                         {X2mm} boundaryEastMm,
                         {Y1mm} boundaryNorthMm,
                         {Y2mm} boundaryNorthMm,
                         {var} trackQueue,
                         {name} (namePrefix + '-north'),
                         {var} primNames
                         );

   { Create south boundary. }
   if (omitSide <> 'South') then
      CLF_CreateNewTrack(layer,
                         widthMm,
                         {X1mm} boundaryWestMm,
                         {X2mm} boundaryEastMm,
                         {Y1mm} boundarySouthMm,
                         {Y2mm} boundarySouthMm,
                         {var} trackQueue,
                         {name} (namePrefix + '-south'),
                         {var} primNames
                         );

end; { end CLF_CreateNewTrackPeninsula2() }


{***************************************************************************
 * function CLF_CreateNewTrackRectangle2()
 *  Create a feature consisting of 4 tracks with the specified boundaries.
 *  
 *  NOTE:  This function was forward declared!  So be sure to change
 *  the forward declaration too if you change the parameter list here!
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewTrackRectangle2(    layer           : Integer;
                                          widthMm         : Real;
                                          boundaryWestMm  : Real;
                                          boundaryEastMm  : Real;
                                          boundaryNorthMm : Real;
                                          boundarySouthMm : Real;
                                      var trackQueue      : TInterfaceList;
                                          namePrefix      : TString;
                                      var primNames       : TStringList;
                                          )               : Integer;
var
   i                    : Integer;
                                               
begin

   { Call CLF_CreateNewTrackPeninsula2() to do all the real work. }
   result := CLF_CreateNewTrackPeninsula2(layer,
                                          widthMm,
                                          boundaryWestMm,
                                          boundaryEastMm,
                                          boundaryNorthMm,
                                          boundarySouthMm,
                                          {omitSide} 'none',
                                          {var} trackQueue,
                                          namePrefix,
                                          {var} primNames);

end; { end CLF_CreateNewTrackRectangle2() }


{***************************************************************************
 * function CLF_GetEffectivePadXYbounds()
 *  Return the effective x,y boundaries of a pad, taking rotation into account.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_GetEffectivePadXYbounds(    padDst : IPCB_Pad;
                                     var X1Mm   : Real;
                                     var Y1Mm   : Real;
                                     var X2Mm   : Real;
                                     var Y2Mm   : Real;
                                         )      : Integer;
var                                               
   i         : Integer;
   XCenterMm : Real;
   YCenterMm : Real;
   XSizeMm   : Real;
   YSizeMm   : Real;
   rotation  : Integer;
          
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_GetEffectivePadXYbounds().');

   { Retrieve relevant parameters from pad itself. }
   XCenterMm         := CoordToMMs(padDst.X);
   YCenterMm         := CoordToMMs(padDst.Y);
   XSizeMm           := CoordToMMs(padDst.TopXSize);
   YSizeMm           := CoordToMMs(padDst.TopYSize);
   rotation          := padDst.Rotation;

   WriteToDebugFile(' XCenterMm is ' + FloatToStr(XCenterMm) + '.');
   WriteToDebugFile(' YCenterMm is ' + FloatToStr(YCenterMm) + '.');
   WriteToDebugFile(' XSizeMm is ' + FloatToStr(XSizeMm) + '.');
   WriteToDebugFile(' YSizeMm is ' + FloatToStr(YSizeMm) + '.');
   WriteToDebugFile(' rotation is ' + IntToStr(rotation) + '.');

   { Examine rotation. }
   if ( (rotation = 90) or (rotation = 270) ) then
   begin

      { We have to account for a 1/4 turn rotation. }
      X1Mm   := XCenterMm - (0.5 * YSizeMm);
      X2Mm   := XCenterMm + (0.5 * YSizeMm);
      Y1Mm   := YCenterMm - (0.5 * XSizeMm);
      Y2Mm   := YCenterMm + (0.5 * XSizeMm);

   end

   { Else no rotation problems. }
   else
   begin

      X1Mm   := XCenterMm - (0.5 * XSizeMm);
      X2Mm   := XCenterMm + (0.5 * XSizeMm);
      Y1Mm   := YCenterMm - (0.5 * YSizeMm);
      Y2Mm   := YCenterMm + (0.5 * YSizeMm);
   end;

   WriteToDebugFile(' X1Mm is ' + FloatToStr(X1Mm) + '.');
   WriteToDebugFile(' X2Mm is ' + FloatToStr(X2Mm) + '.');
   WriteToDebugFile(' Y1Mm is ' + FloatToStr(Y1Mm) + '.');
   WriteToDebugFile(' Y2Mm is ' + FloatToStr(Y2Mm) + '.');
   
end; { end CLF_GetEffectivePadXYbounds() }


{***************************************************************************
 * function CLF_MaintainBoundingRectangleForPadGroup()
 *  Create / update rectangular boundaries for all pads in a particular group.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_MaintainBoundingRectangleForPadGroup(    padQueue        : TInterfaceList;
                                                      padGroupNum     : Integer;
                                                      namePrefix      : TString;
                                                  var cnfGalacticInfo : TStringList;
                                                      )               : Integer;
var                                               
   i         : Integer;
   padDst    : IPCB_Pad;
   X1Mm      : Real;
   Y1Mm      : Real;
   X2Mm      : Real;
   Y2Mm      : Real;
          
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_MaintainBoundingRectangleForPadGroup().');

   { Loop over all pads in the pad queue. }
   for i := 0 to (padQueue.Count - 1) do
   begin

      { Retrieve reference to queued pad. }
      padDst := padQueue.items[i];

      { See if this pad is a member of the specified pad group number. }
      if (CLF_GetPadGroupNum(padDst) = padGroupNum) then
      begin

         { Get effective pad x,y boundaries after taking rotation into account. }
         CLF_GetEffectivePadXYbounds(padDst,
                                     {var} X1Mm,
                                     {var} Y1Mm,
                                     {var} X2Mm,
                                     {var} Y2Mm);
         
         { Now call CLF_MaintainBoundingRectangle() to do the real work. }
         CLF_MaintainBoundingRectangle(X1Mm,
                                       Y1Mm,
                                       X2Mm,
                                       Y2Mm,
                                       namePrefix,
                                       {var} cnfGalacticInfo);
      end; { endif }
      
   end; { endfor }

end; { end CLF_MaintainBoundingRectangleForPadGroup() }


{***************************************************************************
 * function CLF_DrawBoundingRectangleForPadGroup()
 *  Draw the rectangular boundaries for a group of pads.  For debug only.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_DrawBoundingRectangleForPadGroup(    namePrefix      : TString;
                                              var cnfGalacticInfo : TStringList;
                                              var trackQueue      : TInterfaceList;
                                              var primNames       : TStringList;
                                                  )               : Integer;
var                                               
   i               : Integer;
   boundaryWestMm  : Real;
   boundaryEastMm  : Real;
   boundaryNorthMm : Real;
   boundarySouthMm : Real;
          
begin

   { Assume success. }
   result := 0;

   { Retrieve the bounds for this name prefix. }
   CLF_RetrieveBoundingRectangleByNamePrefix(namePrefix,
                                             {var} cnfGalacticInfo,
                                             {var} boundaryWestMm,
                                             {var} boundaryEastMm,
                                             {var} boundaryNorthMm,
                                             {var} boundarySouthMm);

   WriteToDebugFile(' boundaryWestMm is ' + FloatToStr(boundaryWestMm) + '.');
   WriteToDebugFile(' boundaryEastMm is ' + FloatToStr(boundaryEastMm) + '.');
   WriteToDebugFile(' boundaryNorthMm is ' + FloatToStr(boundaryNorthMm) + '.');
   WriteToDebugFile(' boundarySouthMm is ' + FloatToStr(boundarySouthMm) + '.');

   { Create dummy rectangle on courtyard layer for debug purposes. }
   CLF_CreateNewTrackRectangle2({layer} constNewLayerCourtyard, 
                                {widthMm} constNewWidthCourtyardMm,
                                boundaryWestMm,
                                boundaryEastMm,
                                boundaryNorthMm,
                                boundarySouthMm,
                                {var} trackQueue,
                                {namePrefix} 'DUMMY_bounding_rectangle_for_pad_group_' + namePrefix,
                                {var} primNames);

end; { end CLF_DrawBoundingRectangleForPadGroup() }


{***************************************************************************
 * function CLF_CreatePadGroupBounds()
 *  Create rectangular boundaries for all groups of pads (west, east, north, south, etc.). 
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreatePadGroupBounds(var cnfGalacticInfo : TStringList;
                                      padQueue        : TInterfaceList;
                                  var trackQueue      : TInterfaceList;
                                  var primNames       : TStringList;
                                      )               : Integer;
var                                                  
   i                 : Integer;
   namePrefix        : TString;
   padGroupNum       : Integer;

begin

   { Assume success. }
   result := 0;

   { Create bounding rectangles for all relevant pad groups. }
   for padGroupNum := constPadGroupWest to constPadGroupEp do
   begin

      { Retrieve name prefix for the current pad group. }
      namePrefix        := CLF_GetPadGroupNamePrefix(padGroupNum, primNames);
      
      { Create bounding rectangle for this pad group. }
      CLF_MaintainBoundingRectangleForPadGroup(padQueue,
                                               padGroupNum,
                                               namePrefix,
                                               {var} cnfGalacticInfo);

//      { FIXME:  This line should normally be commented out! }
//      { Draw bounding rectangle for this pad group for debugging purposes. }
//      CLF_DrawBoundingRectangleForPadGroup(namePrefix,
//                                           {var} cnfGalacticInfo,
//                                           {var} trackQueue,
//                                           {var} primNames);
   end; { endfor }

end; { end CLF_CreatePadGroupBounds() }


{***************************************************************************
 * function CLF_CalculatePinCoords()
 *  Calculate pin coordinates in a general way, so that we may use the
 *  result of said calculations for assembly drawing, pad landing, and pin
 *  extrusion purposes.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CalculatePinCoords(    cnfGalacticInfo : TStringList;
                                    padDst          : IPCB_Pad;
                                    padGroupNum     : Integer;
                                    bodyWidthMm     : Real;
                                    bodyLengthMm    : Real;
                                    totalWidthMm    : Real;
                                    totalLengthMm   : Real;
                                    pinWidthMm      : Real;
                                    pinLandMm       : Real;
                                var boundaryWestMm  : Real;
                                var boundaryEastMm  : Real;
                                var boundaryNorthMm : Real;
                                var boundarySouthMm : Real;
                                var omitSide        : TString;
                                    )               : Integer;
var
   i                  : Integer;
   padXmm             : Real;
   padYmm             : Real;
   boundaryBeyondAssy : Real;
   footprintType      : TString;

begin

   { Assume success. }
   result := 0;
  
   WriteToDebugFile('Hello from CLF_CalculatePinCoords().');

   { Retrieve footprint type. }
   footprintType         := cnfGalacticInfo.Values(constGilFootprintType);

   { Retrieve pad x,y coordinates. }
   padXmm               := CoordToMMs(padDst.X);
   padYmm               := CoordToMMs(padDst.Y);

   WriteToDebugFile(' padXmm is ' + FloatToStr(padXmm) + '.');
   WriteToDebugFile(' padYmm is ' + FloatToStr(padYmm) + '.');

   {* Perform some common calculations. *}
   { See if this is a west or east side pad. }
   if ( (padGroupNum = constPadGroupWest) or (padGroupNum = constPadGroupEast) ) then
   begin
      
      { Compute delta y as a function of pad Y coordinate. }
      boundaryNorthMm         := (padYmm - (0.5*pinWidthMm));
      boundarySouthMm         := (padYmm + (0.5*pinWidthMm));

      { Compute how far beyond the assembly outline the pin drawing will go. }
      boundaryBeyondAssy    := ((CLF_MaxReal(totalWidthMm, bodyWidthMm) - bodyWidthMm));
   end
   
   { Else see if this is a north or south side pad. }
   else if ( (padGroupNum = constPadGroupNorth) or (padGroupNum = constPadGroupSouth) ) then
   begin
      
      { Compute delta x as a function of pad X coordinate. }
      boundaryWestMm        := (padXmm - (0.5*pinWidthMm));
      boundaryEastMm        := (padXmm + (0.5*pinWidthMm));
      
      { Compute how far beyond the assembly outline the pin drawing will go. }
      boundaryBeyondAssy    := ((CLF_MaxReal(totalLengthMm, bodyLengthMm) - bodyLengthMm));
   end

   { Else panic. }
   else
      CLF_Abort('Don''t know how to compute assembly pin drawing coordinates for pin group ' + IntToStr(padGroupNum) + '!');

   WriteToDebugFile(' boundaryBeyondAssy is ' + FloatToStr(boundaryBeyondAssy) + '.');


   {* Perform side-specific calculations. *}
   { See if this is a west pad. }
   if (padGroupNum = constPadGroupWest) then
   begin
      
      { Handle chip and molded component types. }
      if ( (footprintType = 'Inductor') or (footprintType = 'Capacitor') ) then
      begin

         { This pad is at the west assembly outline. }
         boundaryWestMm       := (-0.5*totalLengthMm);
         boundaryEastMm       := (boundaryWestMm + pinLandMm);

         { Omit no sides. }
         omitSide             := 'none';
         
      end

      { Else handle all other footprint types. }
      else
      begin
         
         { This pad is to the west of assembly outline. }
         boundaryWestMm       := ((-0.5*bodyWidthMm) - (0.5*boundaryBeyondAssy));
         boundaryEastMm       := (boundaryWestMm + pinLandMm);

         { Specify the side of the peninsula we may omit drawing for the assembly drawing. }
         omitSide             := 'East';

      end; { endelse }
   end
   
   { Else see if this is a east pad. }
   else if (padGroupNum = constPadGroupEast) then
   begin
      
      if ( (footprintType = 'Inductor') or (footprintType = 'Capacitor') ) then
      begin

         { This pad is at the east assembly outline. }
         boundaryEastMm       := (0.5*totalLengthMm);
         boundaryWestMm       := (boundaryEastMm - pinLandMm);

         { Omit no sides. }
         omitSide             := 'none';
         
      end

      { Else handle all other footprint types. }
      else
      begin
         
         { This pad is to the east of assembly outline. }
         boundaryEastMm       := ((0.5*bodyWidthMm) + (0.5*boundaryBeyondAssy));
         boundaryWestMm       := (boundaryEastMm - pinLandMm);
         
         { Specify the side of the peninsula we may omit drawing for the assembly drawing. }
         omitSide             := 'West';

      end; { endelse }
   end
   
   { Else see if this is a north pad. }
   else if (padGroupNum = constPadGroupNorth) then
   begin
      
      { This pad is to the north of assembly outline. }
      boundaryNorthMm      := ((0.5*bodyLengthMm) + (0.5*boundaryBeyondAssy));
      boundarySouthMm      := (boundaryNorthMm - pinLandMm);
         
      { Specify the side of the peninsula we may omit drawing for the assembly drawing. }
      omitSide             := 'South';
   end
   
   { Else see if this is a south pad. }
   else if (padGroupNum = constPadGroupSouth) then
   begin
      
      { This pad is to the south of assembly outline. }
      boundarySouthMm      := ((-0.5*bodyLengthMm) - (0.5*boundaryBeyondAssy));
      boundaryNorthMm      := (boundarySouthMm + pinLandMm);
         
      { Specify the side of the peninsula we may omit drawing for the assembly drawing. }
      omitSide             := 'North';      
   end;

   WriteToDebugFile(' boundaryWestMm is ' + FloatToStr(boundaryWestMm) + '.');
   WriteToDebugFile(' boundaryEastMm is ' + FloatToStr(boundaryEastMm) + '.');
   WriteToDebugFile(' boundaryNorthMm is ' + FloatToStr(boundaryNorthMm) + '.');
   WriteToDebugFile(' boundarySouthMm is ' + FloatToStr(boundarySouthMm) + '.');
   
end; { end CLF_CalculatePinCoords() }


{***************************************************************************
 * function CLF_CreateAssyPinDrawings()
 *  Create assembly layer pin drawings.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateAssyPinDrawings(    cnfGalacticInfo : TStringList;
                                       padQueue        : TInterfaceList;
                                   var trackQueue      : TInterfaceList;
                                   var primNames       : TStringList;
                                       )               : Integer;
var
   footprintType         : TString;
   pkgDimsBodyWidthMax   : Real;
   pkgDimsBodyLengthMax  : Real;
   pkgDimsTotalWidthMax  : Real;
   pkgDimsTotalLengthMax : Real;
   pkgDimsPinWidthMax    : Real;
   pinDrawWestMm         : Real;
   pinDrawEastMm         : Real;
   pinDrawNorthMm        : Real;
   pinDrawSouthMm        : Real;
   padDst                : IPCB_Pad;
   padGroupNum           : Integer;
   i                     : Integer;
   pinLandMm             : Real;
   omitSide              : TString;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_CreateAssyPinDrawings().');

   { Retrieve footprint type. }
   footprintType         := cnfGalacticInfo.Values(constGilFootprintType);

   { Retrieve package dimension fields from galactic string list. }
   pkgDimsBodyWidthMax   := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsBodyWidthMax));
   pkgDimsBodyLengthMax  := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsBodyLengthMax));
   pkgDimsTotalWidthMax  := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsTotalWidthMax));
   pkgDimsTotalLengthMax := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsTotalLengthMax));
   pkgDimsPinWidthMax    := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsPinWidthMax));

   { Compute the "pin landing".
    For our purposes, this is the entire distance between the tip of the pin and the assembly drawing. }
   pinLandMm             := (0.5 * (pkgDimsTotalWidthMax - pkgDimsBodyWidthMax));

   { Sanity check.  For quad package types, pin landing must be equal on all 4 sides. }
   if (  not CLF_IsFootprintDualRowGullwingType(footprintType) and
       ( (pkgDimsTotalWidthMax - pkgDimsBodyWidthMax) <> (pkgDimsTotalLengthMax - pkgDimsBodyLengthMax) )  ) then
      CLF_Abort('Sorry, but I do not support different pin landings for width vs. length sides!');
   
   { Create pin drawings for SOIC type footprints. }
   if ( (footprintType = 'SOIC') or (footprintType = 'SOT') or (footprintType = 'SOP') or (footprintType = 'QFP') ) then
   begin

      { Loop over all the pads in the queue. }
      for i := 0 to (padQueue.Count - 1) do
      begin

         { Retrieve reference to queued pad. }
         padDst := padQueue.items[i];

         { Retrieve group number of queued pad. }
         padGroupNum := CLF_GetPadGroupNum(padDst);
         
         { We are only interested in west, east, north, and south side pins. }
         if ( (padGroupNum = constPadGroupWest) or (padGroupNum = constPadGroupEast) or
             (padGroupNum = constPadGroupNorth) or (padGroupNum = constPadGroupSouth) ) then
         begin

            { Compute what the boundaries of the rectangular pin drawing should be,
             as a function of package dimensions and side. }
            CLF_CalculatePinCoords(cnfGalacticInfo,
                                   padDst,
                                   padGroupNum,
                                   {bodyWidthMm} pkgDimsBodyWidthMax,
                                   {bodyLengthMm} pkgDimsBodyLengthMax,
                                   {totalWidthMm} pkgDimsTotalWidthMax,
                                   {totalLengthMm} pkgDimsTotalLengthMax,
                                   {pinWidthMm} pkgDimsPinWidthMax,
                                   pinLandMm,
                                   {var boundaryWestMm} pinDrawWestMm,
                                   {var boundaryEastMm} pinDrawEastMm,
                                   {var boundaryNorthMm} pinDrawNorthMm,
                                   {var boundarySouthMm} pinDrawSouthMm,
                                   {var} omitSide);
            
            { Create a rectangle with the specified boundary. }
            CLF_CreateNewTrackPeninsula2({layer} constNewLayerAssyDrawing,
                                         {widthMm} constWidthThinAssyDrawingMm,
                                         {boundaryWestMm} pinDrawWestMm,
                                         {boundaryEastMm} pinDrawEastMm,
                                         {boundaryNorthMm} pinDrawNorthMm,
                                         {boundarySouthMm} pinDrawSouthMm,
                                         omitSide,
                                         {var} trackQueue,
                                         {namePrefix} 'Assembly_pin_drawing_' + CLF_GetSortablePadName({padName} padDst.Name),
                                         {var} primNames);


         end; { endif }

      end; { endfor i }
      
   end; { endif is SOIC, etc. type. }

end; { end CLF_CreateAssyPinDrawings() }


{***************************************************************************
 * function CLF_CreateNewFeaturesAssembly()
 *  Create all new 2D features on assembly drawing layer.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewFeaturesAssembly(var cnfGalacticInfo : TStringList;
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
   courtyardWestMm             : Real;
   courtyardEastMm             : Real;
   courtyardNorthMm            : Real;
   courtyardSouthMm            : Real;
   assemblyWestMm              : Real;
   assemblyEastMm              : Real;
   assemblyNorthMm             : Real;
   assemblySouthMm             : Real;
   assemblyInnerWestMm         : Real;
   assemblyInnerEastMm         : Real;
   assemblyInnerNorthMm        : Real;
   assemblyInnerSouthMm        : Real;
   deltaXWestAssyToInnerAssyMm : Real;
   deltaXEastAssyToInnerAssyMm : Real;
   deltaYAssyToInnerAssyMm     : Real;
   assyPin1MarkArcRadMm        : Real;
   assyPin1MarkArcWidthMm      : Real;
   assyPin1MarkXcenterMm       : Real;
   assyPin1MarkYcenterMm       : Real;
   footprintType               : TString;
   commentXcoord               : Real;
   refdesXcoord                : Real;
   commentRefDesWestmostXmm    : Real;
   commentRefDesEastmostXmm    : Real;
   commentXmm                  : Real;
   commentYmm                  : Real;
   refdesXmm                   : Real;
   refdesYmm                   : Real;
   commentRotXmm               : Real;
   commentRotYmm               : Real;
   refdesRotXmm                : Real;
   refdesRotYmm                : Real;
   spacing                     : Real;
   assyNorthBreakAtMm          : Real;
   assyNorthResumeAtMm         : Real;
   assySouthBreakAtMm          : Real;
   assySouthResumeAtMm         : Real;
   trackDst                    : IPCB_Track;
   constWidthAssyTextMm        : Real;
   assyTextRotation            : Integer;
   assyTextRotationRot         : Integer;

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
   assyTextRotationRot:= constRotationAssyText - 180; { TODO:  This is hardcoded assuming standard rotation is 270! }


   {* Decide if we have a "small" component that needs to use minimal feature sizes for assembly. *}
   if (  ( (assemblyEastMm - assemblyWestMm) < constThreshSmallAssyOutlineMm ) or
       ( (assemblyNorthMm - assemblySouthMm) < constThreshSmallAssyOutlineMm )  ) then
   begin

      WriteToDebugFile('Hello from CLF_CreateNewFeaturesAssembly().  Using minimal assembly spacings!');

      { Use minimal offset between assembly outline and inner assembly outline. }
      deltaXWestAssyToInnerAssyMm  := constMinDeltaXyAssyToInnerAssyMm;
      deltaXEastAssyToInnerAssyMm := constMinDeltaXyAssyToInnerAssyMm;
      deltaYAssyToInnerAssyMm      := constMinDeltaXyAssyToInnerAssyMm;

      { Use minimal assembly pin 1 marking. }
      assyPin1MarkArcRadMm     := constMinAssyPin1MarkArcRadMm;
      assyPin1MarkArcWidthMm   := constMinAssyPin1MarkArcWidthMm;

      { Compute x,y center position for assembly pin 1 marking. }
      assyPin1MarkXcenterMm    := (assemblyWestMm + deltaXWestAssyToInnerAssyMm + assyPin1MarkArcRadMm + (0.5*assyPin1MarkArcWidthMm));
      assyPin1MarkYcenterMm    := (assemblyNorthMm - deltaYAssyToInnerAssyMm - assyPin1MarkArcRadMm - (0.5*assyPin1MarkArcWidthMm));

   end
   
   else
   begin
      
      WriteToDebugFile('Hello from CLF_CreateNewFeaturesAssembly().  Using nominal assembly spacings.');

      { Use nominal offset between assembly outline and inner assembly outline. }
      { Special handling for SOIC footprint type to create a beveled looking feature on west side of footprint. }
      if (footprintType = 'SOIC') then
         deltaXWestAssyToInnerAssyMm := constSoicDeltaXWestAssyToInnerAssyMm {nosemi}

      else
         deltaXWestAssyToInnerAssyMm  := constNomDeltaXyAssyToInnerAssyMm;

      deltaXEastAssyToInnerAssyMm := constNomDeltaXyAssyToInnerAssyMm;
      deltaYAssyToInnerAssyMm      := constNomDeltaXyAssyToInnerAssyMm;
      
      { Use nominal assembly pin 1 marking. }
      assyPin1MarkArcRadMm     := constNomAssyPin1MarkArcRadMm;
      assyPin1MarkArcWidthMm   := constNomAssyPin1MarkArcWidthMm;

      { Compute x,y center position for assembly pin 1 marking. }
      assyPin1MarkXcenterMm    := (assemblyWestMm + deltaXWestAssyToInnerAssyMm + assyPin1MarkArcRadMm);
      assyPin1MarkYcenterMm    := (assemblyNorthMm - deltaYAssyToInnerAssyMm - assyPin1MarkArcRadMm);

   end;

   { Compute inner assembly outline. }
   assemblyInnerWestMm         := (assemblyWestMm + deltaXWestAssyToInnerAssyMm);
   assemblyInnerEastMm         := (assemblyEastMm - deltaXEastAssyToInnerAssyMm);
   assemblyInnerNorthMm        := (assemblyNorthMm - deltaYAssyToInnerAssyMm);
   assemblyInnerSouthMm        := (assemblySouthMm + deltaYAssyToInnerAssyMm);

   { Secretly store this information in the footprint for later use by PCB helper scripts. }
   CLF_CreateNewTextFpInfo({name} constClfSecretAssyOutlineInner,
                           {value} FloatToStr(assemblyInnerWestMm) + '_' + FloatToStr(assemblyInnerEastMm) + '_' + FloatToStr(assemblyInnerNorthMm) + '_' + FloatToStr(assemblyInnerSouthMm),
                           {var} textQueue,
                           {var} primNames);


   {* Compute locations for .Comment and .Designator text strings. *}
   { Ideally we want enough room for the assembly pin 1 marker, a rotated .Designator string,
    the normal .Comment string, and the normal .Designator string. }
   { See if there's enough room for absolutely everything. }
   commentRefDesWestmostXmm      := (assyPin1MarkXcenterMm + assyPin1MarkArcRadMm + (0.5*assyPin1MarkArcWidthMm) {+ constDeltaYInnerAssyToTextMm} );
   commentRefDesEastmostXmm      := assemblyInnerEastMm;
   WriteToDebugFile('commentRefDesWestmostXmm is ' + FloatToStr(commentRefDesWestmostXmm) + '.');
   WriteToDebugFile('commentRefDesEastmostXmm is ' + FloatToStr(commentRefDesEastmostXmm) + '.');
   WriteToDebugFile('( commentRefDesWestmostXmm + (2.0*(constHeightAssyTextMm)) ) is ' + FloatToStr(( commentRefDesWestmostXmm + (2.0*(constHeightAssyTextMm)) )) + '.');

   { Setup default Y coordinates for both.  We'll override later if absolutely needed. }
   commentYmm                    := (assemblyInnerNorthMm - constDeltaYInnerAssyToTextMm);
   refdesYmm                     := (assemblyInnerNorthMm - constDeltaYInnerAssyToTextMm);

   { Setup default Y coordinates for rotated .Comment and rotated .Designator texts. }
   commentRotYmm                 := (-1.0 * commentYmm);
   refdesRotYmm                  := (-1.0 * refdesYmm);

   { See if there's enough room for the dummy string between pin 1 marker and 0.
    Then see if there's enough room for the 2 real strings between 0 and eastmost. }
   if (  ( ( commentRefDesWestmostXmm + constHeightAssyTextMm + constDeltaYInnerAssyToTextMm) < 0) and
       ( (constDeltaYInnerAssyToTextMm + (2*(constHeightAssyTextMm + constDeltaYInnerAssyToTextMm)) < commentRefDesEastmostXmm) )  ) then
   begin

      WriteToDebugFile('Plenty of room for all 3 lines of text in ideal locations!');

      { Proceed to have .Comment start at x=constDeltaYInnerAssyToTextMm and .Designator start at x=commentXmm + (constHeightAssyTextMm + constDeltaYInnerAssyToTextMm). }
      commentXmm                  := constDeltaYInnerAssyToTextMm;
      refdesXmm                   := commentXmm + (constHeightAssyTextMm + constDeltaYInnerAssyToTextMm);

      { Compute the locations of rotated .Comment and rotated .Designator texts. }
      refdesRotXmm                := commentXmm - (constHeightAssyTextMm + constDeltaYInnerAssyToTextMm);
      commentRotXmm               := commentXmm;
      
   end {nosemi}

   { Else see if there's enough room for all 3 strings, starting at pin 1 marker. }
   else if ( commentRefDesWestmostXmm + constDeltaYInnerAssyToTextMm + (3.0*(constHeightAssyTextMm + constDeltaYInnerAssyToTextMm)) < commentRefDesEastmostXmm ) then
   begin

      WriteToDebugFile('Enough room for all 3 lines of text!');

      { Proceed to have .Comment start at x= . }
      commentXmm                  := commentRefDesWestmostXmm + constDeltaYInnerAssyToTextMm + (constHeightAssyTextMm + constDeltaYInnerAssyToTextMm);
      refdesXmm                   := commentXmm + (constHeightAssyTextMm + constDeltaYInnerAssyToTextMm);

      { Compute the locations of rotated .Comment and rotated .Designator texts. }
      refdesRotXmm                := commentXmm - (constHeightAssyTextMm + constDeltaYInnerAssyToTextMm);
      commentRotXmm               := commentXmm;
      
   end {nosemi}

   { Else see if there's enough room for the basic 2 strings, starting at pin 1 marker. }
   else if ( commentRefDesWestmostXmm + constDeltaYInnerAssyToTextMm + (2.0*(constHeightAssyTextMm + constDeltaYInnerAssyToTextMm)) < commentRefDesEastmostXmm ) then
   begin
      WriteToDebugFile('Enough room for 2 lines of text with proper spacing!');

      commentXmm                  := commentRefDesWestmostXmm + constDeltaYInnerAssyToTextMm;
      refdesXmm                   := commentXmm + (constHeightAssyTextMm + constDeltaYInnerAssyToTextMm);

      { Compute the locations of rotated .Comment and rotated .Designator texts. }
      refdesRotXmm                := commentXmm;
      commentRotXmm               := refdesXmm;
      
   end {nosemi}

   { Else see if we can squeeze in the basic 2 strings, starting at pin 1 marker. }
   else if ( commentRefDesWestmostXmm + (2.0*(constHeightAssyTextMm)) < commentRefDesEastmostXmm ) then
   begin
      WriteToDebugFile('Enough room for 2 lines of text but without proper spacing!');

      { Compute the emergency spacing. }
      spacing                     := ((commentRefDesEastmostXmm - ( commentRefDesWestmostXmm + (2.0*(constHeightAssyTextMm)))) * 0.33);
      
      commentXmm                  := commentRefDesWestmostXmm + spacing;
      refdesXmm                   := commentXmm + (constHeightAssyTextMm + spacing);

      { Compute the locations of rotated .Comment and rotated .Designator texts. }
      refdesRotXmm                := commentXmm;
      commentRotXmm               := refdesXmm;
      
   end {nosemi}

   { Else see if we can squeeze in the basic 2 strings, with 1 string overlapping with the pin 1 marker in x. }
   else if ( assemblyInnerWestMm + (2.0*(constHeightAssyTextMm)) < commentRefDesEastmostXmm ) then
   begin
      WriteToDebugFile('Enough room for 2 lines of text but with overlapping pin 1 marker in x!');

      { Compute the emergency spacing. }
      spacing                   := ((commentRefDesEastmostXmm - ( assemblyInnerWestMm + (2.0*(constHeightAssyTextMm)))) * 0.33);

      { Reverse the normal order and have .Designator (which is shorter when expanded) on the west and .Comment on the east. }
      commentXmm                  := commentRefDesEastmostXmm - (constHeightAssyTextMm + spacing);
      refdesXmm                   := commentXmm - (constHeightAssyTextMm + spacing);

      { Compute emergency y override for refdes. }
      refdesYmm                   := (assyPin1MarkYcenterMm - assyPin1MarkArcRadMm - (0.5*assyPin1MarkArcWidthMm) {+ constDeltaYInnerAssyToTextMm} );
      
      { Compute the locations of rotated .Comment and rotated .Designator texts. }
      refdesRotXmm                := refdesXmm;
      commentRotXmm               := commentXmm;
      
   end {nosemi}

   { Else panic and assume that the user will have to clean up after us. }
   else
   begin
      CLF_WriteToSummaryAndDebugFilesWithStepNum('**Not enough room for .Comment and .Designator assembly text!  User will have to clean up after me!**');

      refdesXmm                   := 0;
      commentXmm                  := 0;
      refdesYmm                   := 0;
      commentYmm                  := 0;
   end;

   
   {* See if we will have enough room for .Comment string within assembly outlines. *}
   WriteToDebugFile('commentYmm is ' + FloatToStr(commentYmm) + '.');
   WriteToDebugFile('constWidthAssyTextMm is ' + FloatToStr(constWidthAssyTextMm) + '.');
   WriteToDebugFile('assemblyInnerSouthMm is ' + FloatToStr(assemblyInnerSouthMm) + '.');
   WriteToDebugFile('( (commentYmm - (constAllowNumCommentChars*constWidthAssyTextMm)) is ' + FloatToStr(( (commentYmm - (constAllowNumCommentChars*constWidthAssyTextMm)))) + '.');
   if ( (commentYmm - (constAllowNumCommentChars*constWidthAssyTextMm)) < assemblyInnerSouthMm ) then
   begin

      WriteToDebugFile('Need to break assembly outlines to accommodate expansion of .Comment!');

      { Flag that inner assembly outline should be broken to accommodate .Comment string expansion. }
      assySouthBreakAtMm             := CLF_MaxReal((commentXmm - constDeltaYInnerAssyToTextMm), assemblyInnerWestMm);
      assySouthResumeAtMm            := CLF_MinReal((commentXmm + constHeightAssyTextMm + constDeltaYInnerAssyToTextMm), assemblyInnerEastMm);

      { Break the north outline in the same way. }
      assyNorthBreakAtMm             := assySouthBreakAtMm;
      assyNorthResumeAtMm            := assySouthResumeAtMm;

      {* Break the outer assembly outline, south line, to accommodate .Comment string expansion. *}
      CLF_BreakHorizontalTrack({layer} constNewLayerAssyDrawing,
                               {widthMm} constNewWidthAssyDrawingMm,
                               {X1mm} assemblyWestMm,
                               {Y1mm} assemblySouthMm,
                               {X2mm} assemblyEastMm,
                               {Y2mm} assemblySouthMm,
                               {XbreakMm} assySouthBreakAtMm,
                               {XresumeMm} assySouthResumeAtMm,
                               {var} trackQueue,
                               {var} primNames
                               );

      {* Break the outer assembly outline, north line, to accommodate .Comment string expansion. *}
      CLF_BreakHorizontalTrack({layer} constNewLayerAssyDrawing,
                               {widthMm} constNewWidthAssyDrawingMm,
                               {X1mm} assemblyWestMm,
                               {Y1mm} assemblyNorthMm,
                               {X2mm} assemblyEastMm,
                               {Y2mm} assemblyNorthMm,
                               {XbreakMm} assyNorthBreakAtMm,
                               {XresumeMm} assyNorthResumeAtMm,
                               {var} trackQueue,
                               {var} primNames
                               );

      {* Move the .Comment text in Y outside the assembly outline to give it as much room as possible to grow. *}
      commentYmm                     := (courtyardNorthMm);
      
   end

   { Else flag that we don't need to break assembly outlines. }
   else
   begin
      assySouthBreakAtMm             := 0.0;
      assySouthResumeAtMm            := 0.0;

      assyNorthBreakAtMm             := 0.0;
      assyNorthResumeAtMm            := 0.0;

   end;


   { Account for the fact that these are rotated with respect to the original. }
   { TODO:  This assumes standard rotation is 270! }
   commentRotXmm                 := (commentRotXmm + constHeightAssyTextMm);
   refdesRotXmm                  := (refdesRotXmm + constHeightAssyTextMm);   
   

   {** Create any new tracks that we wish to create. **}
   
   {* Create 4 diagonal lines between assembly outline and the inner assembly outline. *}
   CLF_CreateNewDiagonalTrackBorders({layer} constNewLayerAssyDrawing,
                                     {widthMm} constWidthThinAssyDrawingMm,
                                     {outerBoundWestMm} assemblyWestMm,
                                     {outerBoundEastMm} assemblyEastMm,
                                     {outerBoundNorthMm} assemblyNorthMm,
                                     {outerBoundSouthMm} assemblySouthMm,
                                     {deltaXwestMm} deltaXWestAssyToInnerAssyMm,
                                     {deltaXeastMm} deltaXEastAssyToInnerAssyMm,
                                     {deltaYmm} deltaYAssyToInnerAssyMm,
                                     {var} trackQueue,
                                     {namePrefix} 'Assembly_border_outer_inner',
                                     {var} primNames);

   {* Create a rectangle to form the inner assembly outline. *}
   { See if we have the standard case of a closed rectangle. }
   if ( (assyNorthBreakAtMm = 0.0) and (assySouthBreakAtMm = 0.0) ) then
   begin
   
      WriteToDebugFile(' Normal case with closed inner assembly outline.');

      CLF_CreateNewTrackRectangle2({layer} constNewLayerAssyDrawing,
                                   {widthMm} constWidthThinAssyDrawingMm,
                                   {boundaryWestMm} assemblyInnerWestMm,
                                   {boundaryEastMm} assemblyInnerEastMm,
                                   {boundaryNorthMm} assemblyInnerNorthMm,
                                   {boundarySouthMm} assemblyInnerSouthMm,
                                   {var} trackQueue,
                                   {namePrefix} 'Assembly_outline_inner',
                                   {var} primNames);

   end

   { Else construct 2 peninsulas (one on west and one on east) to
    accommodate the breaks. }
   else
   begin

      WriteToDebugFile(' About to break inner assembly outline to accommodate expansion of .Comment!');

      CLF_CreateNewTrackPeninsula({layer} constNewLayerAssyDrawing,
                                  {widthMm} constWidthThinAssyDrawingMm,
                                  {X1mm} assyNorthBreakAtMm,
                                  {Y1mm} assemblyInnerNorthMm,
                                  {X2mm} assemblyInnerWestMm,
                                  {Y2mm} assemblyInnerNorthMm,
                                  {X3mm} assemblyInnerWestMm,
                                  {Y3mm} assemblyInnerSouthMm,
                                  {X4mm} assySouthBreakAtMm,
                                  {Y4mm} assemblyInnerSouthMm,
                                  {var} trackQueue,
                                  {namePrefix} 'Assembly_inner_outline-A',
                                  {var} primNames);

      CLF_CreateNewTrackPeninsula({layer} constNewLayerAssyDrawing,
                                  {widthMm} constWidthThinAssyDrawingMm,
                                  {X1mm} assyNorthResumeAtMm,
                                  {Y1mm} assemblyInnerNorthMm,
                                  {X2mm} assemblyInnerEastMm,
                                  {Y2mm} assemblyInnerNorthMm,
                                  {X3mm} assemblyInnerEastMm,
                                  {Y3mm} assemblyInnerSouthMm,
                                  {X4mm} assySouthResumeAtMm,
                                  {Y4mm} assemblyInnerSouthMm,
                                  {var} trackQueue,
                                  {namePrefix} 'Assembly_inner_outline-B',
                                  {var} primNames);

   end; { endelse }
   CLF_WriteToSummaryAndDebugFilesWithStepNum('Created inner assembly outline for new .PcbLib file.');
   

   {* Create pin drawings on assembly layer *}
   CLF_CreateAssyPinDrawings(cnfGalacticInfo,
                             padQueue,
                             {var} trackQueue,
                             {var} primNames);
   

   {** Create any new arcs that we wish to create. **}
   
   {* Create interior pin 1 marker on assembly drawing. *}
   CLF_CreateNewArc({layer} constNewLayerAssyDrawing,
                    {XCenterMm} assyPin1MarkXcenterMm,
                    {YCenterMm} assyPin1MarkYcenterMm,
                    {RadiusMm} assyPin1MarkArcRadMm,
                    {LineWidthMm} assyPin1MarkArcWidthMm,
                    {StartAngleDeg} 0,
                    {EndAngleDeg} 360,
                    {isKeepout} False,
                    {var} arcQueue,
                    {name} 'Assembly_marker_pin_1',
                    {var} primNames
                    );

   { Secretly store this information in the footprint for later use by PCB helper scripts. }
   CLF_CreateNewTextFpInfo({name} constClfSecretAssyArcPin1,
                           {value} FloatToStr(assyPin1MarkXcenterMm) + '_' + FloatToStr(assyPin1MarkYcenterMm) + '_' + FloatToStr(assyPin1MarkArcRadMm) + '_' + FloatToStr(assyPin1MarkArcWidthMm),
                           {var} textQueue,
                           {var} primNames);

   CLF_WriteToSummaryAndDebugFilesWithStepNum('Created interior pin 1 marker on assembly drawing layer for new .PcbLib file.');

   {** Create any new texts that we wish to create. **}

   { .Comment string on assembly drawing. }
   CLF_CreateNewText(constNewLayerAssyDrawing,
                     constLineWidthAssyTextMm,
                     constHeightAssyTextMm,
                     commentXmm,
                     commentYmm,
                     assyTextRotation,                     
                     '.Comment',
                     {var} textQueue,
                     {name} 'Assembly_.Comment',
                     {var} primNames
                     );

   { Secretly store this information in the footprint for later use by PCB helper scripts. }
   CLF_CreateNewTextFpInfo({name} constClfSecretAssyComment,
                           {value} FloatToStr(commentXmm) + '_' + FloatToStr(commentYmm) + '_' + IntToStr(assyTextRotation) + '_' + FloatToStr(constLineWidthAssyTextMm) + '_' + FloatToStr(constHeightAssyTextMm),
                           {var} textQueue,
                           {var} primNames);

   { .Designator string on assembly drawing. }
   CLF_CreateNewText(constNewLayerAssyDrawing,
                     constLineWidthAssyTextMm,
                     constHeightAssyTextMm,
                     refdesXmm,
                     refdesYmm,
                     assyTextRotation,                     
                     '.Designator',
                     {var} textQueue,
                     {name} 'Assembly_.Designator',
                     {var} primNames
                     );

   { Secretly store this information in the footprint for later use by PCB helper scripts. }
   CLF_CreateNewTextFpInfo({name} constClfSecretAssyDesignator,
                           {value} FloatToStr(refdesXmm) + '_' + FloatToStr(refdesYmm) + '_' + IntToStr(assyTextRotation) + '_' + FloatToStr(constLineWidthAssyTextMm) + '_' + FloatToStr(constHeightAssyTextMm),
                           {var} textQueue,
                           {var} primNames);


   {* Store information for rotated .Comment and .Designator strings. }
//   { .Comment string on assembly drawing. }
//   CLF_CreateNewText(constNewLayerAssyDrawing,
//                     constLineWidthAssyTextMm,
//                     constHeightAssyTextMm,
//                     commentRotXmm,
//                     commentRotYmm,
//                     assyTextRotationRot,                     
//                     'CommentRot',
//                     {var} textQueue,
//                     {name} 'Assembly_.CommentRot',
//                     {var} primNames
//                     );

   { Secretly store this information in the footprint for later use by PCB helper scripts. }
   CLF_CreateNewTextFpInfo({name} constClfSecretAssyCommentRot,
                           {value} FloatToStr(commentRotXmm) + '_' + FloatToStr(commentRotYmm) + '_' + IntToStr(assyTextRotationRot) + '_' + FloatToStr(constLineWidthAssyTextMm) + '_' + FloatToStr(constHeightAssyTextMm),
                           {var} textQueue,
                           {var} primNames);

//   { .Designator string on assembly drawing. }
//   CLF_CreateNewText(constNewLayerAssyDrawing,
//                     constLineWidthAssyTextMm,
//                     constHeightAssyTextMm,
//                     refdesRotXmm,
//                     refdesRotYmm,
//                     assyTextRotationRot,                     
//                     'RefDesRot',
//                     {var} textQueue,
//                     {name} 'Assembly_.DesignatorRot',
//                     {var} primNames
//                     );

   { Secretly store this information in the footprint for later use by PCB helper scripts. }
   CLF_CreateNewTextFpInfo({name} constClfSecretAssyDesignatorRot,
                           {value} FloatToStr(refdesRotXmm) + '_' + FloatToStr(refdesRotYmm) + '_' + IntToStr(assyTextRotationRot) + '_' + FloatToStr(constLineWidthAssyTextMm) + '_' + FloatToStr(constHeightAssyTextMm),
                           {var} textQueue,
                           {var} primNames);


   CLF_WriteToSummaryAndDebugFilesWithStepNum('Created .Comment and .Designator text strings on assembly drawing layer for new .PcbLib file.');
   
end; { end CLF_CreateNewFeaturesAssembly() }


{***************************************************************************
 * function CLF_CreateNewFeaturesSilkscreen()
 *  Create all new 2D features on the Silkscreen layer.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewFeaturesSilkscreen(var cnfGalacticInfo : TStringList;
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
   footprintType         : TString;
   silkscreenWestMm      : Real;
   silkscreenEastMm      : Real;
   silkscreenNorthMm     : Real;
   silkscreenSouthMm     : Real;
   assemblyWestMm        : Real;
   assemblyEastMm        : Real;
   assemblyNorthMm       : Real;
   assemblySouthMm       : Real;
   silkPin1MarkXcenterMm : Real;
   silkPin1MarkYcenterMm : Real;

begin
   
   { Retrieve other info from galactic string list. }
   footprintType      := cnfGalacticInfo.Values(constGilFootprintType);

   {* Retrieve coordinate info from galactic string list. *}
   { Retrieve the bounds for the assembly outline. }
   CLF_RetrieveBoundingRectangleByNamePrefix({namePrefix} 'Assembly',
                                             {var} cnfGalacticInfo,
                                             {var boundaryWestMm} assemblyWestMm,
                                             {var boundaryEastMm} assemblyEastMm,
                                             {var boundaryNorthMm} assemblyNorthMm,
                                             {var boundarySouthMm} assemblySouthMm);

   { Retrieve the bounds for the silkscreen outline. }
   { FIXME:  If there is no silkscreen outline, then add code to try to create a silkscreen outline
    based on assembly outline, with breaks to accommodate pad groups. }
   rc := CLF_RetrieveBoundingRectangleByNamePrefixNoAbort({namePrefix} 'Silkscreen',
                                                          {var} cnfGalacticInfo,
                                                          {var boundaryWestMm} silkscreenWestMm,
                                                          {var boundaryEastMm} silkscreenEastMm,
                                                          {var boundaryNorthMm} silkscreenNorthMm,
                                                          {var boundarySouthMm} silkscreenSouthMm);
   { Sanity check. }
   if (rc <> 0) then
      CLF_Abort('Sorry, cannot currently handle a footprint that comes through from LP Wizard with no silkscreen outline!');

   { FIXME:  Add code to create silkscreen outline if it is missing here! }
   
   { Update names of tracks that form existing silkscreen outline. }
   CLF_UpdateTrackNamesFromBoundingRectangle({layer} constSilkLayer,
                                             {namePrefix} 'Silkscreen',
                                             {var} cnfGalacticInfo,
                                             {var} trackQueue,
                                             {var} primNames);
   
   {* Decide if we have a "small" component with respect to silkscreen features. *}
   if ( (silkscreenEastMm - silkscreenWestMm) < constSilkPin1MarkXThreshCenterMm ) then
   begin

      WriteToDebugFile('Hello from CLF_CreateNewFeaturesSilkscreen().  Using minimal silkscreen spacings!');

      { Compute x,y center position for silkscreen pin 1 marking. }
      silkPin1MarkXcenterMm    := 0;

   end
   
   else
   begin
      
      WriteToDebugFile('Hello from CLF_CreateNewFeaturesSilkscreen().  Using nominal silkscreen spacings.');

      { Compute x,y center position for silkscreen pin 1 marking. }
      silkPin1MarkXcenterMm    := (silkscreenWestMm + (0.5*constWidthSilkMm) + (0.5*constIntSilkPin1MarkArcWidthMm) + constIntSilkPin1MarkArcRadMm + constIntSilkPin1MarkSpaceMm);

   end;

   { This coordinate is invariant. }
   silkPin1MarkYcenterMm    := (silkscreenNorthMm - (0.5*constWidthSilkMm) - (0.5*constIntSilkPin1MarkArcWidthMm) - constIntSilkPin1MarkArcRadMm - constIntSilkPin1MarkSpaceMm);

   
   {* Create interior pin 1 marker on silkscreen. *}

   { DFN & QFN components don't get this feature. }
   { FIXME:  Change this to a check of whether proposed arc overlaps with EP group boundary. }
   if ( (footprintType <> 'DFN') and (footprintType <> 'QFN') ) then
      begin

         CLF_CreateNewArc({layer} constSilkLayer,
                          {XCenterMm} silkPin1MarkXcenterMm,
                          {YCenterMm} silkPin1MarkYcenterMm,
                          {RadiusMm} constIntSilkPin1MarkArcRadMm,
                          {LineWidthMm} constIntSilkPin1MarkArcWidthMm,
                          {StartAngleDeg} 0,
                          {EndAngleDeg} 360,
                          {isKeepout} False,
                          {var} arcQueue,
                          {name} 'Silkscreen_marker_pin_1_interior',
                          {var} primNames
                          );

         CLF_WriteToSummaryAndDebugFilesWithStepNum('Created interior silkscreen pin 1 marking for new .PcbLib file.');
      end; { endif }

end; { end CLF_CreateNewFeaturesSilkscreen() }


{***************************************************************************
 * function CLF_ReportBodyPropertiesPartial()
 *  Report common 3D body properties to the CSV file stringlist.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportBodyPropertiesPartial(    bodyDst       : IPCB_ComponentBody;
                                             csvRptPrefix  : TString;
                                             borderWestMm  : Real;
                                             borderEastMm  : Real;
                                             borderNorthMm : Real;
                                             borderSouthMm : Real;
                                             Xrot          : Integer;
                                             Yrot          : Integer;
                                             Zrot          : Integer;
                                         var line          : TString;
                                             )             : Integer;
var
   i          : Integer;
   hidden     : Boolean;
   identifier : TString;
               
begin

   { Assume success. }
   result := 0;

   {* Add information about this new body to CSV file. *}

   { Compute whether this new 3D body will in fact be hidden. }
   hidden := (bodyDst.BodyOpacity3D = 0.0);

   WriteToDebugFile('Hello from CLF_ReportBodyPropertiesPartial().  Layer is ' + IntToStr(bodyDst.Layer) + '.');

   { Create line identifier. }
   identifier := bodyDst.identifier;
   line := csvRptPrefix + identifier + '=';

   { Do the things we need to do for a real 3D body object. }
   if (csvRptPrefix = constCsvRptPrefixBody) then
   begin
      
      { Report relevant parameters. }
      CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldObjectKind, {valueStr} '3D Body',                    {var} line);

   end
   
   { Else do the things we need to do for a 3D regions object. }
   else if (csvRptPrefix = constCsvRptPrefix3dRegions) then
   begin
      
      { Report relevant parameters. }
      CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldObjectKind, {valueStr} '3D Regions',                    {var} line);

   end;
   
   { Do the things we need to do for 3D body or 3D regions. }
   if ( (csvRptPrefix = constCsvRptPrefixBody) or (csvRptPrefix = constCsvRptPrefix3dRegions) ) then
   begin
   
      { Note:  By convention, we try to report (x1,y1) as the bottom-left (south-west) corner! }
      CLF_ReportCsvPropertyMm({name} constCsvRptFieldX1,         {valueMm} borderWestMm,               {var} line);
      CLF_ReportCsvPropertyMm({name} constCsvRptFieldY1,         {valueMm} borderSouthMm,                {var} line);
      CLF_ReportCsvPropertyMm({name} constCsvRptFieldX2,         {valueMm} borderEastMm,              {var} line);
      CLF_ReportCsvPropertyMm({name} constCsvRptFieldY2,         {valueMm} borderNorthMm,                {var} line);

      CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldIdentifier, {valueStr} identifier,                   {var} line);
      CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldLayer,      {valueStr} Layer2String(bodyDst.Layer),  {var} line);

   end

   { Else handle 3D text. }
   else if (csvRptPrefix = constCsvRptPrefix3dText) then
   begin

      { Report relevant parameters. }
      CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldObjectKind, {valueStr} '3D Text',                       {var} line);

      { Note:  Identifier and layer will be reported by CLF_ReportTextPropertiesPartial(). }
   end

   { Else panic }
   else
      CLF_Abort('Unknown object kind in CLF_ReportBodyPropertiesPartial()!');
   
   { Report properties that are common to all 3D objects. }
   CLF_ReportCsvPropertyTBoardSide({name} constCsvRptFieldBodyProjection, {valueEnum} bodyDst.BodyProjection,  {var} line);
   CLF_ReportCsvPropertyInt  ({name} constCsvRptFieldBodyColor,           {valueInt} bodyDst.BodyColor3D,      {var} line);
   CLF_ReportCsvPropertyFloat({name} constCsvRptFieldBodyOpacity,         {valueFloat} bodyDst.BodyOpacity3D,  {var} line);
   CLF_ReportCsvPropertyInt  ({name} constCsvRptFieldModelRotationX,      {valueInt} Xrot,                     {var} line);
   CLF_ReportCsvPropertyInt  ({name} constCsvRptFieldModelRotationY,      {valueInt} Yrot,                     {var} line);
   CLF_ReportCsvPropertyInt  ({name} constCsvRptFieldModelRotationZ,      {valueInt} Zrot,                     {var} line);

   { ModelType, ModelStandoffHeight, ModelOverallHeight, and ModelRadius must be handled by caller functions! }
   
   { Boilerplate so that things appear similarly to when we look at them in PCBLIB List. }
   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldLocked,     {valueBool} False,                       {var} line);

   { bodyDst.IsHidden() doesn't work.  So we have to cheat and use our knowledge of the body's opacity. }
//   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldHide,       {valueBool} bodyDst.IsHidden(),          {var} line);
   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldHide,       {valueBool} hidden,                      {var} line);
   
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldNet,        {valueStr} 'No Net',                     {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldComponent,  {valueStr} 'Free',                       {var} line);

   { Add line to csv report stringlist. }
   { Note:  In this instance, we rely on caller function to do this last step! }
//   csvReportStrs.Add(line);
   
end; { end CLF_ReportBodyPropertiesPartial() }
   

{***************************************************************************
 * function CLF_Create3dRectangularExtrusion()
 *  Create an extruded 3D body in a rectangular prism shape.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_Create3dRectangularExtrusion(    boardSide        : Integer;
                                              layer            : Integer;
                                              color            : Integer;
                                              opacity          : Real;
                                              borderWestMm     : Real;
                                              borderEastMm     : Real;
                                              borderNorthMm    : Real;
                                              borderSouthMm    : Real;
                                              overallHeightMm  : Real;
                                              standoffHeightMm : Real;
                                              identifier       : TString;
                                          var bodyQueue        : TInterfaceList;
                                          var primNames        : TStringList;
                                          var csvReportStrs    : TStringList;                               
                                              )                : Integer;
var
   i           : Integer;
   Model       : IPCB_Model;
   bodyDst     : IPCB_ComponentBody;
   bodyContour : IPCB_Contour;
   line        : TString;
   Xrot        : Integer;
   Yrot        : Integer;
   Zrot        : Integer;
               
begin

   { Assume success. }
   result := 0;

   {***************************************************************************
    * BEGIN code borrowed from CreateComponentBody.pas
    ***************************************************************************}
   bodyDst := PcbServer.PCBObjectFactory(eComponentBodyObject,eNoDimension,eCreate_Default);

   bodyDst.StandoffHeight := MMsToCoord(standoffHeightMm);
   bodyDst.OverallHeight  := MMsToCoord(overallHeightMm);
   bodyDst.BodyProjection := boardSide;
   bodyDst.Layer          := layer;
   bodyDst.Kind           := eRegionKind_Copper;
   bodyDst.BodyColor3D    := color;
   bodyDst.BodyOpacity3D  := opacity;
   bodyDst.SetState_Identifier(identifier);
//   bodyDst.Locked := True;

   { Unfortunately, we can't rotate in x,y,z even if we want to.  ;-( }
   Xrot          := 0;
   Yrot          := 0;
   Zrot          := 0;

   { Allocate an IPCB_Contour object so that we may describe the outline of the 3D extrusion. }
   bodyContour            := PCBServer.PCBContourFactory;

   { Add outline points to the rectangular shape of the assembly outline. }
   bodyContour.AddPoint(MMsToCoord(borderWestMm), MMsToCoord(borderNorthMm));
   bodyContour.AddPoint(MMsToCoord(borderWestMm), MMsToCoord(borderSouthMm));
   bodyContour.AddPoint(MMsToCoord(borderEastMm), MMsToCoord(borderSouthMm));
   bodyContour.AddPoint(MMsToCoord(borderEastMm), MMsToCoord(borderNorthMm));
   
   bodyDst.SetOutlineContour(bodyContour);

   { Queue new body for new library component. }
   bodyQueue.Add(bodyDst);
         
   {***************************************************************************
    * END code borrowed from CreateComponentBody.pas
    ***************************************************************************}

   {* Add information about this new body to CSV file. *}

   { Call CLF_ReportBodyPropertiesPartial() to do most of the work for us. }
   CLF_ReportBodyPropertiesPartial(bodyDst,
                                   {csvRptPrefix} constCsvRptPrefixBody,
                                   borderWestMm,
                                   borderEastMm,
                                   borderNorthMm,
                                   borderSouthMm,
                                   Xrot,
                                   Yrot,
                                   Zrot,
                                   {var} line);

   { Add properties specific to this type of 3D body. }
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldModelType,  {valueStr} 'Extruded rectangle',                   {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldModelStandoffHeight, {valueCoord} bodyDst.StandoffHeight, {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldModelOverallHeight,  {valueCoord} bodyDst.OverallHeight,  {var} line);

   { Add line to csv report stringlist. }
   csvReportStrs.Add(line);
   
end; { end CLF_Create3dRectangularExtrusion() }
   

{***************************************************************************
 * function CLF_ExtrudeGeometricPolygonInto3d()
 *  Extrude a given geometric polygon into 3D.
 *  Split the geometric polygon into connected polygons and their associated
 *   hole contours.
 *  For each connected polygon, create new 3D extrusion(s) and add it/them to the bodyQueue.
 *  Optionally create the corresponding 2D regions and add them to the regionQueue.
 *
 *  Note:  Rotating the contours doesn't work in that rotated contours with
 *  holes (even after I had rotated those hole contours) don't display propertly
 *  in Altium 3D land.  So I am removing all rotation code from this function,
 *  and relying on being able to rotate whatever object generates the geometric
 *  polygon (eg. true type text, image bitmap).
 *  
 *  NOTE:  This function was forward declared!  So be sure to change
 *  the forward declaration too if you change the parameter list here!
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ExtrudeGeometricPolygonInto3d(    boardSide         : Integer;
                                               layer             : Integer;
                                               colorText         : Integer;
                                               colorBackground   : Integer;
                                               opacity           : Real;
                                               overallHeightMm   : Real;
                                               standoffHeightMm  : Real;
                                               AGeometricPolygon : IPCB_GeometricPolygon;
                                               identifier        : TString;
                                               doQueueRegions    : Boolean;
                                               doQueueBodies     : Boolean;
                                           var bodyDst           : IPCB_ComponentBody;
                                           var regionQueue       : TInterfaceList;
                                           var bodyQueue         : TInterfaceList;
                                           var primNames         : TStringList;
                                           var boundsList        : TStringList;
                                               )                 : Integer;

var                                               
   i                 : Integer;
   bodyHole          : IPCB_ComponentBody;
   ConnectedPolygon  : IPCB_GeometricPolygon;
   ConnectedPolygons : TInterfaceList;
   Region            : IPCB_Region;
   j                 : Integer;
   k                 : Integer;
   contour           : IPCB_Contour;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_ExtrudeGeometricPolygonInto3d().');
   WriteToDebugFile('Hello from CLF_ExtrudeGeometricPolygonInto3d(), layer is ' + IntToStr(layer) + ' ' + Layer2String(layer) + '.');

   WriteToDebugFile(' boardSide is ' + IntToStr(boardSide) + '.');
   WriteToDebugFile(' layer is ' + IntToStr(layer) + '.');
   WriteToDebugFile(' colorText is ' + IntToStr(colorText) + '.');
   WriteToDebugFile(' colorBackground is ' + IntToStr(colorBackground) + '.');
   WriteToDebugFile(' opacity is ' + FloatToStr(opacity) + '.');
   WriteToDebugFile(' overallHeightMm is ' + FloatToStr(overallHeightMm) + '.');
   WriteToDebugFile(' standoffHeightMm is ' + FloatToStr(standoffHeightMm) + '.');
   WriteToDebugFile(' identifier is "' + identifier + '".');
   WriteToDebugFile(' doQueueRegions is ' + BoolToStr(doQueueRegions) + '.');
   WriteToDebugFile(' doQueueBodies is ' + BoolToStr(doQueueBodies) + '.');


   {***************************************************************************
    * BEGIN code borrowed from ConstructRegionsFromContourSet.pas
    ***************************************************************************}
   { Create an interface list to hold all connected polygons. }
//   ConnectedPolygons := TInterfaceList.Create();  { This doesn't work for some reason. }
   ConnectedPolygons := CreateInterfaceList;

   { Split the galactic geometric polygon into a series of connected polygons,
    eg. (usually) 1 connected polygon for each letter of text. }
   PCBServer.PCBContourUtilities.SplitIntoConnectedPolygons(AGeometricPolygon, ConnectedPolygons);

   { Loop over all connected polygons. }
   for i := 0 To (ConnectedPolygons.Count - 1) do
   begin

      { Retrieve a reference to the current ConnectedPolygon (eg. next letter of text). }
      { Note:  We are NOT guaranteed that the connected polygons are in order with respect
       to the text string.  In other words, ConnectedPolygons[0] is not guaranteed to be the
       initial character of the text string.  From what I've seen, the ordering is random. }
      { Note:  We are not guaranteed that there is only 1 connected polygon per text char.
       For example, the '%' char (at least in Courier New font) is described by 3 ConnectedPolygons. }
      ConnectedPolygon := ConnectedPolygons[i];
      WriteToDebugFile(' Retrieving connected polygon # ' + IntToStr(i) + '.');

      { Note that we will have issues with any closed character (eg. one that has an interior region)
       that needs a hole contour, eg. a, b, d, e, o, p, q, A, B, D, O, P, Q, R, 0, 4, 6, 8, 9, @, #, %, &. }
      { abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()-_=+[];:,./<>?\| }

      { Get the 0th contour of the connected polygon. }
      contour           := ConnectedPolygon.Contour[0];

      { Create a 3D body with this as its outline contour if desired. }
      if (doQueueBodies) then
      begin

         { Create a new 3D body object. }
         bodyDst := PcbServer.PCBObjectFactory(eComponentBodyObject,eNoDimension,eCreate_Default);

         { Set various standard 3D body parameters. }
         bodyDst.StandoffHeight := MMsToCoord(standoffHeightMm);
         bodyDst.OverallHeight  := MMsToCoord(overallHeightMm);
         bodyDst.BodyProjection := boardSide;
         bodyDst.Layer          := layer;
         bodyDst.Kind           := eRegionKind_Copper;
         bodyDst.BodyColor3D    := colorText;
         bodyDst.BodyOpacity3D  := opacity;
         bodyDst.SetState_Identifier(identifier + '-' + IntToStr(i));
         //   bodyDst.Locked := True;

         { Set the new 3D body object to use the current contour as its outline contour. }
         bodyDst.SetOutlineContour(contour);

         { Report contour points. }
         for j := 0 to (bodyDst.MainContour.Count - 1) do
         begin
            
//           WriteToDebugFile(' main contour point[' + IntToStr(j) + '] is ' + IntToStr(bodyDst.MainContour.x[j]) + ',' + IntToStr(bodyDst.MainContour.y[j]) + '.');
         
         end;

      end; { endif }

      { Create a 2D region with this as its outline contour if desired. }
      if (doQueueRegions) then
      begin

         { Create a new 2D region and assign it the same outline contour and layer as the body. }
         Region := PCBServer.PCBObjectFactory(eRegionObject, eNoDimension, eCreate_Default);
         Region.SetOutlineContour(contour);
         Region.Layer := Layer;

         { Name this primitive. }
         CLF_AddPrimNameToList({var prim} Region,
                               {name} constMagicalExclude3dRegionName,
                               {var} primNames);
         
         { Create & maintain a bounding rectangle for all regions that we create here. }
         CLF_MaintainBoundingRectangleForRegions({boardXorigin} 0,
                                                 {boardYorigin} 0,
                                                 {regionSrc} Region,
                                                 {namePrefix} identifier,
                                                 {var cnfGalacticInfo} boundsList);
         
      end;

      { Loop over any additional contours of ConnectedPolygon. }
      for j := 1 to (ConnectedPolygon.Count - 1) do
      begin

         WriteToDebugFile(' Retrieving hole polygon # ' + IntToStr(j-1) + '.');

         { Get the jth contour of the connected polygon, which is a hole contour. }
         contour           := ConnectedPolygon.Contour[j];

         { Create a 3D body with this as its outline contour if desired. }
         if (doQueueBodies) then
         begin

            { Set the primary 3D body object to use the jth contour of ConnectedPolygon as a hole contour. }
            { This is used, for example, to create the hole in the middle of an 'O' or a little 'e' char. }
            { Note that this is only effective within Altium!  It will display properly in 3D mode, but
             this hole contour will NOT be present in STEP export! }
            bodyDst.GeometricPolygon.AddContourIsHole(contour, True);

            { Since doing AddContourIsHole is ineffective with respect to generating an output STEP
             model, what we're going to do in addition is to create yet another 3D body object, with color
             set to the background color, to hide the hole in the middle of the '0', 'A', etc. chars. }
            
            { Create a new "hole" 3D body object. }
            bodyHole := PcbServer.PCBObjectFactory(eComponentBodyObject,eNoDimension,eCreate_Default);
            
            { Set various standard 3D body parameters. }
            bodyHole.StandoffHeight := MMsToCoord(overallHeightMm);        { Start at the text height. }
            bodyHole.OverallHeight  := (MMsToCoord(overallHeightMm) + 1);  { Cheat and add 1 coordinate unit. }
            bodyHole.BodyProjection := boardSide;
            bodyHole.Layer          := layer;
            bodyHole.Kind           := eRegionKind_Copper;
            bodyHole.BodyColor3D    := colorBackground;
            bodyHole.BodyOpacity3D  := 0.0;                                { Set to be completely transparent (eg. hidden) in Altium.  But it will still export to STEP. }
            bodyHole.SetState_Identifier(identifier + '-' + IntToStr(i) + '-hole-' + IntToStr(j-1));
            //   bodyHole.Locked := True;
            
            { Set the new "hole" 3D body object to use the current contour (a hole contour) as its outline contour. }
            bodyHole.SetOutlineContour(contour);

            { Queue new "hole" 3D body for new library component. }
            bodyQueue.Add(bodyHole);

         end; { endif }
            
         { Create a 2D hole region if desired. }
         if (doQueueRegions) then
         begin
            Region.GeometricPolygon.AddContourIsHole(contour, True);
         end;

         { Report hole contour points. }
         for k := 0 to (ConnectedPolygon.Contour[j].Count - 1) do
         begin
            
//            WriteToDebugFile(' hole contour point[' + IntToStr(k) + '] is ' + IntToStr(ConnectedPolygon.Contour[j].x[k]) + ',' + IntToStr(ConnectedPolygon.Contour[J].y[k]) + '.');
            
         end;
      
      end; { endfor j }

      { Loop over all contours of ConnectedPolygon. }
      for j := 0 to (ConnectedPolygon.Count - 1) do
      begin

         { Get the jth contour of the connected polygon, which is a hole contour. }
         contour           := ConnectedPolygon.Contour[j];

         { Destroy contour to try to minimize memory leaks. }
         PCBServer.DestroyPCBContour(contour);

      end; { endfor j }

      { Queue primary 3D body if desired. }
      if (doQueueBodies) then
      begin
         bodyQueue.Add(bodyDst);
      end;
      
      { Queue region if desired. }
      if (doQueueRegions) then
      begin
         regionQueue.Add(Region);
      end;

   end; { endfor i }

   { Shouldn't we free this instead?? }
//   ConnectedPolygons := Nil;
//   ConnectedPolygons.Free;
//   ConnectedPolygons.Destroy;
   ConnectedPolygons.Clear;
   {***************************************************************************
    * END code borrowed from ConstructRegionsFromContourSet.pas
    ***************************************************************************}

end; { end CLF_ExtrudeGeometricPolygonInto3d() }


{***************************************************************************
 * function CLF_Create3dText()
 *  Create a new 3d text object with the specified properties.
 *  Provide a reference to a 2D true type text object as the starting point.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_Create3dText(    boardSide        : Integer;
                              layer            : Integer;
                              colorText        : Integer;
                              colorBackground  : Integer;
                              opacity          : Real;
                              overallHeightMm  : Real;
                              standoffHeightMm : Real;
                              textDst          : IPCB_Text;
                          var bodyQueue        : TInterfaceList;
                              identifier       : TString;
                          var primNames        : TStringList;
                          var csvReportStrs    : TStringList;                               
                              )                : Integer;

var                                               
   i                 : Integer;
   bodyDst           : IPCB_ComponentBody;
   bodyHole          : IPCB_ComponentBody;
   Xrot              : Integer;
   Yrot              : Integer;
   Zrot              : Integer;
   line              : TString;
   AGeometricPolygon : IPCB_GeometricPolygon;
   dummyRegionQueue  : TInterfaceList;
   boundsList        : TStringList;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_Create3dText().');

   WriteToDebugFile(' boardSide is ' + IntToStr(boardSide) + '.');
   WriteToDebugFile(' layer is ' + IntToStr(layer) + '.');
   WriteToDebugFile(' colorText is ' + IntToStr(colorText) + '.');
   WriteToDebugFile(' colorBackground is ' + IntToStr(colorBackground) + '.');
   WriteToDebugFile(' opacity is ' + FloatToStr(opacity) + '.');
   WriteToDebugFile(' overallHeightMm is ' + FloatToStr(overallHeightMm) + '.');
   WriteToDebugFile(' standoffHeightMm is ' + FloatToStr(standoffHeightMm) + '.');
   WriteToDebugFile(' identifier is "' + identifier + '".');

   { Convert text object to geometric region. }
   AGeometricPolygon := textDst.TTTextOutlineGeometricPolygon();
   
   { Unfortunately, we can't rotate in x,y,z even if we want to.  ;-( }
   Xrot          := 0;
   Yrot          := 0;
   Zrot          := 0;

   { Create useless string list. }
   boundsList        := TStringList.Create();   
   
   { Call CLF_ExtrudeGeometricPolygonInto3d() to extrude the geometric polygon into 3D. }
   CLF_ExtrudeGeometricPolygonInto3d(boardSide,
                                     layer,
                                     colorText,
                                     colorBackground,
                                     opacity,
                                     overallHeightMm,
                                     standoffHeightMm,
                                     AGeometricPolygon,
                                     identifier,
                                     {doQueueRegions} False,
                                     {doQueueBodies} True,
                                     {var} bodyDst,
                                     {var regionQueue} dummyRegionQueue,
                                     {var} bodyQueue,
                                     primNames,
                                     boundsList);
   
   { Free useless string list. }
   boundsList.Free();
   

   {* Add information about this new text to CSV file. *}

   { Create line identifier. }
   bodyDst.SetState_Identifier(identifier);
   
   { Call CLF_ReportBodyPropertiesPartial() to do most of the work for us. }
   CLF_ReportBodyPropertiesPartial(bodyDst,
                                   {csvRptPrefix} constCsvRptPrefix3dText,
                                   {borderWestMm} 0,
                                   {borderEastMm} 0,
                                   {borderNorthMm} 0,
                                   {borderSouthMm} 0,
                                   Xrot,
                                   Yrot,
                                   Zrot,
                                   {var} line);

   { Call CLF_ReportTextPropertiesPartial() to do most of the work for us. }
   CLF_ReportTextPropertiesPartial(textDst,
                                   identifier,
                                   {var} line);

   { Add properties specific to this type of 3D body. }
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldModelType,  {valueStr} 'Extruded text polygons',                   {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldModelStandoffHeight, {valueCoord} bodyDst.StandoffHeight, {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldModelOverallHeight,  {valueCoord} bodyDst.OverallHeight,  {var} line);

   { Add line to csv report stringlist. }
   csvReportStrs.Add(line);
   
end; { end CLF_Create3dText() }


{***************************************************************************
 * BEGIN code borrowed from ConstructContourSetFromPicture.pas.
 ***************************************************************************}
{***************************************************************************
 * procedure CLF_FlipCoords()
 *  Flip x,y coordinates according to various flags.
 *  
 *  This is a slightly modified form of the function found in
 *   ConstructContourSetFromPicture.pas.
 *  
 *  Returns:  Nothing.  Procedure, not a function.
 ***************************************************************************}
Procedure CLF_FlipCoords(    aPicture    : TPicture;
                             doSwapXY    : Boolean;
                             doFlipHoriz : Boolean;
                             doFlipVert  : Boolean;
                             InX         : TCoord;
                             InY         : TCoord;
                         var OutX        : TCoord;
                         var OutY        : TCoord;
                             );
var
   temp      : TCoord;
   effWidth  : TCoord;
   effHeight : TCoord;
   
Begin

   effWidth  := aPicture.Width;
   effHeight := aPicture.Height;

   if (doSwapXY) then
   begin
      temp := InX;
      InX  := InY;
      InY  := temp;

      temp := effWidth;
      effWidth  := effHeight;
      effHeight  := temp;

   end; { endif }
   
    If doFlipHoriz Then
       OutX := effWidth - InX - 1
    Else
       OutX := InX;

    If doFlipVert Then
       OutY := InY
    Else
       OutY := effHeight - InY - 1;
   
End; { end CLF_FlipCoords() }


{***************************************************************************
 * function CLF_CreateContourFromPixel().
 *  Construct a new contour from a single image pixel.
 *  
 *  This is a slightly modified form of the function found in
 *   ConstructContourSetFromPicture.pas.
 *  
 *  Returns:  Reference to new contour.
 ***************************************************************************}
Function CLF_CreateContourFromPixel(X, Y, BaseX, BaseY, PixelToCoordScale : TCoord;
                                    Color                                 : TColor;
                                    doNegateImage                         : Boolean;
                                    )                                     : IPCB_Contour;
Var
   GrayProportion  : Double;
   GrayWidth       : TCoord;
   Left            : TCoord;
   Right           : TCoord;
   Bottom          : TCoord;
   Top             : TCoord;
   RedProportion   : Integer;
   GreenProportion : Integer;
   BlueProportion  : Integer;
Begin
     RedProportion   := (Color And $0000FF);
     GreenProportion := (Color And $00FF00) Shr 8;
     BlueProportion  := (Color And $FF0000) Shr 16;

     GrayProportion := (RedProportion + GreenProportion + BlueProportion) / (3 * 255);

     If (doNegateImage) Then
        GrayProportion := 1 - GrayProportion;

     GrayWidth := Round((1 - sqrt(GrayProportion)) * PixelToCoordScale / 2);

     Left   := X       * PixelToCoordScale + BaseX + GrayWidth;
     Right  := (X + 1) * PixelToCoordScale + BaseX - GrayWidth;
     Bottom := Y       * PixelToCoordScale + BaseY + GrayWidth;
     Top    := (Y + 1) * PixelToCoordScale + BaseY - GrayWidth;

     If GrayProportion = 0.0 Then Exit;

     Result := PCBServer.PCBContourFactory;

     Result.AddPoint(Left , Bottom);
     Result.AddPoint(Right, Bottom);
     Result.AddPoint(Right, Top   );
     Result.AddPoint(Left , Top   );

End; { end CLF_CreateContourFromPixel() }


{***************************************************************************
 * function CLF_ConstructGeometricPolygonFromPicture_UnionOptimized2()
 *  Construct a geometric polygon from an image (bitmap).
 *  
 *  This is a slightly modified form of the function found in
 *   ConstructContourSetFromPicture.pas.
 *  
 *  Returns:  Reference to completed geometric polygon.
 ***************************************************************************}
Function CLF_ConstructGeometricPolygonFromPicture_UnionOptimized2(aImage        : TImage;
                                                                  ProgressBar   : TProgressBar;
                                                                  StatusBar     : TStatusBar;
                                                                  FBaseX        : TCoord;
                                                                  FBaseY        : TCoord;
                                                                  FPixelSize    : TCoord;
                                                                  doNegateImage : Boolean;
                                                                  doSwapXY      : Boolean;
                                                                  doFlipHoriz   : Boolean;
                                                                  doFlipVert    : Boolean;
                                                                  )             : IPCB_GeometricPolygon;
Var
   I, J, K              : Integer;
   X, Y                 : Integer;
   tX, tY               : Integer;
   TempGeometricPolygon : IPCB_GeometricPolygon;
   PixelContour         : IPCB_Contour;
   
Begin
    If ProgressBar <> Nil Then
        ProgressBar.Max := aImage.Picture.Height * 2;

    Result := PcbServer.PCBGeometricPolygonFactory;

    For K := 0 To 3 Do
    Begin
        TempGeometricPolygon := PcbServer.PCBGeometricPolygonFactory;

        If StatusBar <> Nil Then
        Begin
            StatusBar.SimpleText  := ' Converting Pixels To Contours...';
            StatusBar.Update;
        End;

        For J := 0 To aImage.Picture.Height Div 2 Do
        Begin
            If ProgressBar <> Nil Then
            Begin
                ProgressBar.Position := ProgressBar.Position + 1;
                ProgressBar.Update;
            End;

            For I := 0 To aImage.Picture.Width Div 2 Do
            Begin
                X := I * 2 + K Mod 2;
                Y := J * 2 + K Div 2;

                CLF_FlipCoords(aImage.Picture,
                               doSwapXY,
                               doFlipHoriz,
                               doFlipVert,
                               X,
                               Y,
                               tX,
                               tY);

                If (X < aImage.Picture.Width) And (Y < aImage.Picture.Height) Then
                Begin
                  PixelContour := CLF_CreateContourFromPixel(tX, tY, FBaseX, FBaseY, FPixelSize,
                                                             aImage.Canvas.Pixels[X, Y],
                                                             doNegateImage);
                  TempGeometricPolygon.AddContour(PixelContour);
                End;
            End;

        End;

        If StatusBar <> Nil Then
        Begin
            StatusBar.SimpleText := ' Creating Union...';
            StatusBar.Update;
        End;

        PCBServer.PCBContourUtilities.ClipSetSet(eSetOperation_Union, TempGeometricPolygon, Result, Result);
    End;
   
End; { end CLF_ConstructGeometricPolygonFromPicture_UnionOptimized2() }
{***************************************************************************
 * END code borrowed from ConstructContourSetFromPicture.pas.
 ***************************************************************************}


{***************************************************************************
 * function CLF_Create3dRegionsFromImage()
 *  Create new 3d region objects from the specified image (bitmap) file.
 *  This is used for adding a manufacturer's logo to the marking on top of
 *  a 3D component body.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_Create3dRegionsFromImage(    boardSide        : Integer;
                                          layer            : Integer;
                                          X1mm             : Real;
                                          Y1mm             : Real;
                                          X2mm             : Real;
                                          Y2mm             : Real;
                                          rotation         : Integer;
                                          imageFilePath    : TString;
                                          colorImage       : Integer;
                                          colorBackground  : Integer;
                                          opacity          : Real;
                                          overallHeightMm  : Real;
                                          standoffHeightMm : Real;
                                          doNegateImage    : Boolean;
                                          doCenterInX      : Boolean;
                                          doCenterInY      : Boolean;
                                          identifier       : TString;
                                      var trackQueue       : TInterfaceList;
                                      var regionQueue      : TInterfaceList;
                                      var bodyQueue        : TInterfaceList;
                                          cnfGalacticInfo  : TStringList;
                                      var primNames        : TStringList;
                                      var csvReportStrs    : TStringList;                               
                                          )                : Integer;

var                                               
   i                     : Integer;
   bodyDst               : IPCB_ComponentBody;
   bodyHole              : IPCB_ComponentBody;
   Xrot                  : Integer;
   Yrot                  : Integer;
   Zrot                  : Integer;
   borderWestMm          : Real;
   borderEastMm          : Real;
   borderNorthMm         : Real;
   borderSouthMm         : Real;
   line                  : TString;
   AGeometricPolygon     : IPCB_GeometricPolygon;
   boundsList            : TStringList;
   AImage                : TImage;
   projectPath           : TString;
   imageFileRelPath      : TString;
   Xmm                   : Real;
   Ymm                   : Real;
   XSizeMm               : Real;
   YSizeMm               : Real;
   XSizeCoord            : TCoord;
   YSizeCoord            : TCoord;
   imageXSizePixels      : Integer;
   imageYSizePixels      : Integer;
   imageXSizeCoord       : TCoord;
   imageYSizeCoord       : TCoord;
   imageXCoordsPerPixel  : TCoord;
   imageYCoordsPerPixel  : TCoord;
   imageXYCoordsPerPixel : TCoord;
   extraSpaceXCoord      : TCoord;
   extraSpaceYCoord      : TCoord;
   halfExtraSpaceXCoord  : TCoord;
   halfExtraSpaceYCoord  : TCoord;
   doSwapXY              : Boolean;
   doFlipHoriz           : Boolean;
   doFlipVert            : Boolean;

begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_Create3dRegionsFromImage().');
   WriteToDebugFile(' boardSide is ' + IntToStr(boardSide) + '.');
   WriteToDebugFile(' layer is ' + IntToStr(layer) + '.');
   WriteToDebugFile(' X1mm is ' + FloatToStr(X1mm) + '.');
   WriteToDebugFile(' Y1mm is ' + FloatToStr(Y1mm) + '.');
   WriteToDebugFile(' X2mm is ' + FloatToStr(X2mm) + '.');
   WriteToDebugFile(' Y2mm is ' + FloatToStr(Y2mm) + '.');
   WriteToDebugFile(' rotation is ' + IntToStr(rotation) + '.');
   WriteToDebugFile(' colorImage is ' + IntToStr(colorImage) + '.');
   WriteToDebugFile(' colorBackground is ' + IntToStr(colorBackground) + '.');
   WriteToDebugFile(' opacity is ' + FloatToStr(opacity) + '.');
   WriteToDebugFile(' overallHeightMm is ' + FloatToStr(overallHeightMm) + '.');
   WriteToDebugFile(' standoffHeightMm is ' + FloatToStr(standoffHeightMm) + '.');
   WriteToDebugFile(' doNegateImage is ' + BoolToStr(doNegateImage) + '.');
   WriteToDebugFile(' doCenterInX is ' + BoolToStr(doCenterInX) + '.');
   WriteToDebugFile(' doCenterInY is ' + BoolToStr(doCenterInY) + '.');
   WriteToDebugFile(' identifier is "' + identifier + '".');

   { For debug purposes, draw a rectangle around the specified bounds. }
   CLF_CreateNewTrackRectangle2(layer,
                                {widthMm} constNewWidthCourtyardMm,
                                {boundaryWestMm} X1mm,
                                {boundaryEastMm} X2mm,
                                {boundaryNorthMm} Y1mm,
                                {boundarySouthMm} Y2mm,
                                {var} trackQueue,
                                {namePrefix} identifier + '-boundary',
                                {var} primNames);

   
   { Load image bitmap from file. }
   { Note:  Image1 is an evil global variable (part of our dialog box). }
   Image1.Picture.LoadFromFile(imageFilePath);
   AImage := Image1;
   
   {* Figure out scaling factor for image file. *}

   { Examine rotation and compensate as needed. }
   if ( (rotation = 90) or (rotation = 270) ) then
   begin

      { We have to account for a 1/4 turn rotation. }
      imageXSizePixels   := AImage.Picture.Height;
      imageYSizePixels   := AImage.Picture.Width;
      
   end

   else if ( (rotation = 0) or (rotation = 180) ) then
   begin

      { No rotation problems here. }
      imageXSizePixels   := AImage.Picture.Width;
      imageYSizePixels   := AImage.Picture.Height;

   end

   else
      CLF_Abort('In CLF_Create3dRegionsFromImage(), unsupported rotation value ' + IntToStr(rotation) + '!');

   { Retrieve the size of the image in pixels. }
   WriteToDebugFile(' imageXSizePixels is ' + IntToStr(imageXSizePixels) + '.');
   WriteToDebugFile(' imageYSizePixels is ' + IntToStr(imageYSizePixels) + '.');

   { Compute the X & Y allowed sizes of the image on the footprint component body. }
   XSizeMm := Abs(X1mm - X2mm);
   YSizeMm := Abs(Y1mm - Y2mm);
   WriteToDebugFile(' XSizeMm is ' + FloatToStr(XSizeMm) + '.');
   WriteToDebugFile(' YSizeMm is ' + FloatToStr(YSizeMm) + '.');

   { Convert to coordinate units.  We need all available precision for this operation. }
   XSizeCoord             := MMsToCoord(XSizeMm);
   YSizeCoord             := MMsToCoord(YSizeMm);
   WriteToDebugFile(' XSizeCoord is ' + IntToStr(XSizeCoord) + '.');
   WriteToDebugFile(' YSizeCoord is ' + IntToStr(YSizeCoord) + '.');

   { Calculate the pixel size (coord units per pixel) if the image were bound only
    by the X or Y boundaries, respectively. }
   imageXCoordsPerPixel   := (XSizeCoord / imageXSizePixels);
   imageYCoordsPerPixel   := (YSizeCoord / imageYSizePixels);
   WriteToDebugFile(' imageXCoordsPerPixel is ' + IntToStr(imageXCoordsPerPixel) + '.');
   WriteToDebugFile(' imageYCoordsPerPixel is ' + IntToStr(imageYCoordsPerPixel) + '.');

   { Use the minimum of these two values as our X & Y pixel size. }
   imageXYCoordsPerPixel  := Min(imageXCoordsPerPixel, imageYCoordsPerPixel);
   WriteToDebugFile(' imageXYCoordsPerPixel is ' + IntToStr(imageXYCoordsPerPixel) + '.');

   { Compute the image size in Coords now that we have chosen pixel size. }
   imageXSizeCoord       := (imageXYCoordsPerPixel * imageXSizePixels);
   imageYSizeCoord       := (imageXYCoordsPerPixel * imageYSizePixels);

   { Compute the amount of extra space in X & Y between image and boundary, in Coords. }
   extraSpaceXCoord      := (XSizeCoord - imageXSizeCoord);
   extraSpaceYCoord      := (YSizeCoord - imageYSizeCoord);
   
   { Compute half the above amounts, for purposes of centering image in X & Y, in Coords. }
   halfExtraSpaceXCoord  := (extraSpaceXCoord / 2);
   halfExtraSpaceYCoord  := (extraSpaceYCoord / 2);
   

   { Compute the x,y anchor point.  For an unrotated image, this is the bottom-left
    (aka west-south) point.  Also account for the effects of rotations. }
   if ( (rotation = 0) ) then 
   begin

      { Calculate for the left-bottom point of the image to start at the left-bottom (west-south) point of the boundary. }
      Xmm                 := CLF_MinReal(X1mm, X2mm);
      Ymm                 := CLF_MinReal(Y1mm, Y2mm);

      { Add offsets in X and/or Y if we've been ordered to center the image in the boundary. }
      if (doCenterInX) then
         Xmm              := Xmm + CoordToMMs(halfExtraSpaceXCoord);
         
      if (doCenterInY) then
         Ymm              := Ymm + CoordToMMs(halfExtraSpaceYCoord);

      { Set doSwapXY, doFlipHoriz, and doFlipVert flags appropriately to accomplish desired rotation. }
      doSwapXY            := False;
      doFlipHoriz         := False;
      doFlipVert          := False;
   end

   else if ( (rotation = 90) ) then
   begin

      { Calculate for the left-bottom point of the image to start at the right-bottom (east-south) point of the boundary. }
      Xmm                 := CLF_MaxReal(X1mm, X2mm) - CoordToMMs(imageXSizeCoord);
      Ymm                 := CLF_MinReal(Y1mm, Y2mm);
      
      { Add offsets in X and/or Y if we've been ordered to center the image in the boundary. }
      if (doCenterInX) then
         Xmm              := Xmm - CoordToMMs(halfExtraSpaceXCoord);
         
      if (doCenterInY) then
         Ymm              := Ymm + CoordToMMs(halfExtraSpaceYCoord);

      { Set doSwapXY, doFlipHoriz, and doFlipVert flags appropriately to accomplish desired rotation. }
      doSwapXY            := True;
      doFlipHoriz         := False;
      doFlipVert          := True;
   end

   else if ( (rotation = 180) ) then
   begin

      { Calculate for the left-bottom point of the image to start at the right-top (east-north) point of the boundary. }
      Xmm                 := CLF_MaxReal(X1mm, X2mm) - CoordToMMs(imageXSizeCoord);
      Ymm                 := CLF_MaxReal(Y1mm, Y2mm) - CoordToMMs(imageYSizeCoord);
      
      { Add offsets in X and/or Y if we've been ordered to center the image in the boundary. }
      if (doCenterInX) then
         Xmm              := Xmm - CoordToMMs(halfExtraSpaceXCoord);
         
      if (doCenterInY) then
         Ymm              := Ymm - CoordToMMs(halfExtraSpaceYCoord);

      { Set doSwapXY, doFlipHoriz, and doFlipVert flags appropriately to accomplish desired rotation. }
      doSwapXY            := False;
      doFlipHoriz         := True;
      doFlipVert          := True;
   end

   else if ( (rotation = 270) ) then
   begin

      { Calculate for the left-bottom point of the image to start at the left-top (west-north) point of the boundary. }
      Xmm                 := CLF_MinReal(X1mm, X2mm);
      Ymm                 := CLF_MaxReal(Y1mm, Y2mm) - CoordToMMs(imageYSizeCoord);
      
      { Add offsets in X and/or Y if we've been ordered to center the image in the boundary. }
      if (doCenterInX) then
         Xmm              := Xmm + CoordToMMs(halfExtraSpaceXCoord);
         
      if (doCenterInY) then
         Ymm              := Ymm - CoordToMMs(halfExtraSpaceYCoord);

      { Set doSwapXY, doFlipHoriz, and doFlipVert flags appropriately to accomplish desired rotation. }
      doSwapXY            := True;
      doFlipHoriz         := True;
      doFlipVert          := False;
   end;

   
   { Convert image to a geometric polygon. }
   AGeometricPolygon := CLF_ConstructGeometricPolygonFromPicture_UnionOptimized2(AImage,
                                                                                 Nil,
                                                                                 Nil,
                                                                                 {FBaseX} MMsToCoord(Xmm),
                                                                                 {FBaseY} MMsToCoord(Ymm),
                                                                                 {FCoordsPerPixel} imageXYCoordsPerPixel,
                                                                                 doNegateImage,
                                                                                 doSwapXY,
                                                                                 doFlipHoriz,
                                                                                 doFlipVert
                                                                                 );

   
   { Create bounds string list. }
   boundsList             := TStringList.Create();   
   
   { Call CLF_ExtrudeGeometricPolygonInto3d() to extrude the geometric polygon into 3D. }
   CLF_ExtrudeGeometricPolygonInto3d(boardSide,
                                     layer,
                                     {colorText} colorImage,
                                     colorBackground,
                                     opacity,
                                     overallHeightMm,
                                     standoffHeightMm,
                                     AGeometricPolygon,
                                     identifier,
                                     {doQueueRegions} True,
                                     {doQueueBodies} True,
                                     {var} bodyDst,
                                     {var} regionQueue,
                                     {var} bodyQueue,
                                     primNames,
                                     {var} boundsList);
   
   { Retrieve boundary information for all the regions we just created. }
   CLF_RetrieveBoundingRectangleByNamePrefix({namePrefix} identifier,
                                             {var cnfGalacticInfo} boundsList,
                                             {var boundaryWestMm} borderWestMm,
                                             {var boundaryEastMm} borderEastMm,
                                             {var boundaryNorthMm} borderNorthMm,
                                             {var boundarySouthMm} borderSouthMm);

   { Free bounds string list. }
   boundsList.Free();

   
   {* Add information about this new 3D region to CSV file. *}

   { Create line identifier. }
   bodyDst.SetState_Identifier(identifier);
   
   { Unfortunately, we can't rotate in x,y,z even if we want to.  ;-( }
   Xrot          := 0;
   Yrot          := 0;
   Zrot          := 0;

   { Call CLF_ReportBodyPropertiesPartial() to do most of the work for us. }
   CLF_ReportBodyPropertiesPartial(bodyDst,
                                   {csvRptPrefix} constCsvRptPrefix3dRegions,
                                   borderWestMm,
                                   borderEastMm,
                                   borderNorthMm,
                                   borderSouthMm,
                                   Xrot,
                                   Yrot,
                                   Zrot,
                                   {var} line);

   { Add properties specific to this type of 3D body. }
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldModelType,  {valueStr} 'Extruded region polygons',        {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldModelStandoffHeight, {valueCoord} bodyDst.StandoffHeight, {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldModelOverallHeight,  {valueCoord} bodyDst.OverallHeight,  {var} line);
   CLF_ReportCsvPropertyInt  ({name} constCsvRptFieldRotation,   {valueInt} rotation,                          {var} line);

   { Report (unrotated) x,y size of image file in pixels as "TextHeight" and "TextWidth". }
   CLF_ReportCsvPropertyInt({name} constCsvRptFieldTextHeight, {valueInt} AImage.Picture.Height,               {var} line);
   CLF_ReportCsvPropertyInt({name} constCsvRptFieldTextWidth,  {valueInt} AImage.Picture.Width,                {var} line);
   
   { Report negate image flag as the "Inverted" flag. }
   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldInverted,         {valueBool} doNegateImage,              {var} line);

   { Report center-in-X flag as the "UseInvertedRectangle" flag. }
   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldUseInvertedRectangle,      {valueBool} doCenterInX,       {var} line);

   { Report center-in-Y flag as the "InvertedTextJustification" flag. }
   CLF_ReportCsvPropertyBool ({name} constCsvRptFieldInvertedTextJustification, {valueBool} doCenterInY,       {var} line);

   { Retrieve project file name. }
   projectPath := cnfGalacticInfo.Values(constGilProjectFileName);
   
   { Calculate the *relative* path for the image file that we just added to footprint. }
   imageFileRelPath := StringReplace(imageFilePath, projectPath, '', 0);
   imageFileRelPath := StringReplace(imageFileRelPath, AnsiUpperCase(projectPath), '', 0);
   WriteToDebugFile(' imageFileRelPath is "' + imageFileRelPath + '".');
   
   { Report relative path to image file in the String field. }
   CLF_ReportCsvPropertyStr        ({name} constCsvRptFieldString,     {valueStr} imageFileRelPath,  {var} line);

   { Add line to csv report stringlist. }
   csvReportStrs.Add(line);
   
end; { end CLF_Create3dRegionsFromImage() }


{***************************************************************************
 * function CLF_Create3dSphere()
 *  Create a 3D sphere and add it to the queue.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_Create3dSphere(    boardSide     : Integer;
                                layer         : Integer;
                                color         : Integer;
                                opacity       : Real;
                                XCenterMm     : Real;
                                YCenterMm     : Real;
                                radiusMm      : Real;
                                ZoffsetMm     : Real;
                                identifier    : TString;
                            var bodyQueue     : TInterfaceList;
                            var primNames     : TStringList;
                            var csvReportStrs : TStringList;                               
                                )             : Integer;
var
   i             : Integer;
   model         : IPCB_Model;
   bodyDst       : IPCB_ComponentBody;
   borderWestMm  : Real;
   borderEastMm  : Real;
   borderNorthMm : Real;
   borderSouthMm : Real;
   line          : TString;
   Xrot          : Integer;
   Yrot          : Integer;
   Zrot          : Integer;
                 
begin

   { Assume success. }
   result := 0;

   { Create a new component body object. }
   bodyDst := PcbServer.PCBObjectFactory(eComponentBodyObject,eNoDimension,eCreate_Default);

   bodyDst.StandoffHeight := MMsToCoord(ZoffsetMm);
//   bodyDst.OverallHeight  := MMsToCoord(overallHeightMm);
   bodyDst.BodyProjection := boardSide;
   bodyDst.Layer          := layer;
   bodyDst.Kind           := eRegionKind_Copper;
   bodyDst.BodyColor3D    := color;
   bodyDst.BodyOpacity3D  := opacity;
   bodyDst.SetState_Identifier(identifier);
//   bodyDst.Locked := True;

   { We have no need to rotate a sphere. }
   Xrot          := 0;
   Yrot          := 0;
   Zrot          := 0;

   { Create a new model of a sphere. }
   model := bodyDst.ModelFactory_CreateSphere(MMsToCoord(radiusMm), 0);
   bodyDst.SetState_FromModel;

   { Set x,y,z rotation and z offset from board surface. }
   model.SetState(Xrot, Yrot, Zrot, MMsToCoord(ZoffsetMm));
   bodyDst.Model := model;

//   WriteToDebugFile('ModelProperties is "' + model.GetState_ModelPropertiesString + '".');

   { Compute x,y bounds for sphere. }
   borderWestMm    := XCenterMm-radiusMm;
   borderEastMm    := XCenterMm+radiusMm;
   borderNorthMm   := YCenterMm+radiusMm;
   borderSouthMm   := YCenterMm-radiusMm;
   
   { Set x,y coordinates for sphere. }
   { Note:  The MoveToXY() wants the south west coordinate, use that. }
   bodyDst.MoveToXY(MMsToCoord(borderWestMm), MMsToCoord(borderSouthMm));

   { Queue new body for new library component. }
   bodyQueue.Add(bodyDst);

   
   {* Add information about this new body to CSV file. *}

   { Call CLF_ReportBodyPropertiesPartial() to do most of the work for us. }
   CLF_ReportBodyPropertiesPartial(bodyDst,
                                   {csvRptPrefix} constCsvRptPrefixBody,
                                   borderWestMm,
                                   borderEastMm,
                                   borderNorthMm,
                                   borderSouthMm,
                                   Xrot,
                                   Yrot,
                                   Zrot,
                                   {var} line);

   { Add properties specific to this type of 3D body. }
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldModelType,  {valueStr} 'Sphere', {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldModelStandoffHeight, {valueCoord} bodyDst.StandoffHeight, {var} line);
   CLF_ReportCsvPropertyMm   ({name} constCsvRptFieldModelRadius,{valueMm} radiusMm,  {var} line);

   { Add line to csv report stringlist. }
   csvReportStrs.Add(line);
   
end; { end CLF_Create3dSphere() }


{***************************************************************************
 * function CLF_Create3dCylinder()
 *  Create a 3D cylinder and add it to the queue.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_Create3dCylinder(    boardSide     : Integer;
                                  layer         : Integer;
                                  color         : Integer;
                                  opacity       : Real;
                                  XCenterMm     : Real;
                                  YCenterMm     : Real;
                                  radiusMm      : Real;
                                  ZheightMm     : Real;
                                  ZoffsetMm     : Real;
                                  identifier    : TString;
                              var bodyQueue     : TInterfaceList;
                              var primNames     : TStringList;
                              var csvReportStrs : TStringList;                               
                                  )             : Integer;
var
   i             : Integer;
   model         : IPCB_Model;
   bodyDst       : IPCB_ComponentBody;
   borderWestMm  : Real;
   borderEastMm  : Real;
   borderNorthMm : Real;
   borderSouthMm : Real;
   line          : TString;
   Xrot          : Integer;
   Yrot          : Integer;
   Zrot          : Integer;
                 
begin

   { Assume success. }
   result := 0;

   { Create a new component body object. }
   bodyDst := PcbServer.PCBObjectFactory(eComponentBodyObject,eNoDimension,eCreate_Default);

   bodyDst.StandoffHeight := MMsToCoord(ZoffsetMm);
//   bodyDst.OverallHeight  := MMsToCoord(overallHeightMm);
   bodyDst.BodyProjection := boardSide;
   bodyDst.Layer          := layer;
   bodyDst.Kind           := eRegionKind_Copper;
   bodyDst.BodyColor3D    := color;
   bodyDst.BodyOpacity3D  := opacity;
   bodyDst.SetState_Identifier(identifier);
//   bodyDst.Locked := True;

   { In this particular application, we have no need to rotate our cylinder, although we could. }
   Xrot          := 0;
   Yrot          := 0;
   Zrot          := 0;

   { Create a new model of a cylinder. }
   model := bodyDst.ModelFactory_CreateCylinder(MMsToCoord(radiusMm), MMsToCoord(ZheightMm-ZoffsetMm), 0);
   bodyDst.SetState_FromModel;

   { Set x,y,z rotation and z offset from board surface. }
   model.SetState(Xrot, Yrot, Zrot, MMsToCoord(ZoffsetMm));
   bodyDst.Model := model;

//   WriteToDebugFile('ModelProperties is "' + model.GetState_ModelPropertiesString + '".');
   
   { Compute x,y bounds for cylinder. }
   borderWestMm    := XCenterMm-radiusMm;
   borderEastMm    := XCenterMm+radiusMm;
   borderNorthMm   := YCenterMm+radiusMm;
   borderSouthMm   := YCenterMm-radiusMm;
   
   { Set x,y coordinates for cylinder. }
   { Note:  The MoveToXY() wants the south west coordinate, use that. }
   bodyDst.MoveToXY(MMsToCoord(borderWestMm), MMsToCoord(borderSouthMm));

   { Queue new body for new library component. }
   bodyQueue.Add(bodyDst);


   {* Add information about this new body to CSV file. *}

   { Call CLF_ReportBodyPropertiesPartial() to do most of the work for us. }
   CLF_ReportBodyPropertiesPartial(bodyDst,
                                   {csvRptPrefix} constCsvRptPrefixBody,
                                   borderWestMm,
                                   borderEastMm,
                                   borderNorthMm,
                                   borderSouthMm,
                                   Xrot,
                                   Yrot,
                                   Zrot,
                                   {var} line);

   { Add properties specific to this type of 3D body. }
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldModelType,           {valueStr} 'Cylinder',            {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldModelStandoffHeight, {valueCoord} bodyDst.StandoffHeight, {var} line);
   CLF_ReportCsvPropertyMm   ({name} constCsvRptFieldModelOverallHeight,  {valueMm} ZheightMm,              {var} line);
   CLF_ReportCsvPropertyMm   ({name} constCsvRptFieldModelRadius,         {valueMm} radiusMm,               {var} line);
   
   { Add line to csv report stringlist. }
   csvReportStrs.Add(line);
   
end; { end CLF_Create3dCylinder() }


{***************************************************************************
 * function CLF_AddStepModel()
 *  Add a specified STEP model file to the queue.
 *  Place said STEP model in the center of the footprint (x,y).
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_AddStepModel(    boardSide       : Integer;
                              layer           : Integer;
                              opacity         : Real;
                              stepFileName    : TString;
                              Xrot            : Integer;
                              Yrot            : Integer;
                              Zrot            : Integer;
                              ZoffsetMm       : Real;
                          var bodyQueue       : TInterfaceList;
                              cnfGalacticInfo : TStringList;
                          var primNames       : TStringList;
                          var csvReportStrs   : TStringList;                               
                              )               : Integer;
var
   i               : Integer;
   model           : IPCB_Model;
   bodyDst         : IPCB_ComponentBody;
   line            : TString;
   projectPath     : TString;
   stepFileRelPath : TString;
   rectDst         : TCoordRect;
   borderWestMm    : Real;
   borderEastMm    : Real;
   borderNorthMm   : Real;
   borderSouthMm   : Real;
                                                 
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello world from CLF_AddStepModel().');
   WriteToDebugFile('stepFileName is "' + stepFileName + '".');

   {***************************************************************************
    * BEGIN code borrowed from AutoSTEPplacer.pas.
    ***************************************************************************}
   { Create a new component body object. }
   bodyDst := PcbServer.PCBObjectFactory(eComponentBodyObject,eNoDimension,eCreate_Default);
   WriteToDebugFile(' Back from PCBObjectFactory.');

   { Set the board side and board layer. }
   bodyDst.BodyProjection := boardSide;
   bodyDst.Layer          := layer;
   bodyDst.BodyOpacity3D  := opacity;
//   bodyDst.StandoffHeight := MMsToCoord(ZoffsetMm);
   
   { Create a new model from specified STEP model. }
   model := bodyDst.ModelFactory_FromFilename(stepFileName, false);
   bodyDst.SetState_FromModel;

   { Set x,y,z rotation and z offset from board surface. }
   model.SetState(Xrot, Yrot, Zrot, MMsToCoord(ZoffsetMm));
   bodyDst.Model := model;

   { Use the name of the STEP file (without path or extension) as the identifier.
    This is the behavior of the Altium GUI when you choose a STEP model and fly it in. }
   bodyDst.SetState_Identifier(ChangeFileExt(ExtractFileName(stepFileName),''));

//   model.Locked := True;

//   WriteToDebugFile('ModelProperties is "' + model.GetState_ModelPropertiesString + '".');
   
   { Queue new body for new library component. }
   bodyQueue.Add(bodyDst);

   {***************************************************************************
    * END code borrowed from AutoSTEPplacer.pas.
    ***************************************************************************}

   { Try to obtain bounds of STEP model so that we may report this to csv file. }
   rectDst         := bodyDst.BoundingRectangle;

   borderWestMm    := CoordToMMs(rectDst.left);
   borderEastMm    := CoordToMMs(rectDst.right);
   borderNorthMm   := CoordToMMs(rectDst.top);
   borderSouthMm   := CoordToMMs(rectDst.bottom);
                                                 
   {* Add information about this new body to CSV file. *}
   CLF_ReportBodyPropertiesPartial(bodyDst,
                                   {csvRptPrefix} constCsvRptPrefixBody,
                                   borderWestMm,
                                   borderEastMm,
                                   borderNorthMm,
                                   borderSouthMm,
                                   Xrot,
                                   Yrot,
                                   Zrot,
                                   {var} line);


   { Retrieve project file name. }
   projectPath := cnfGalacticInfo.Values(constGilProjectFileName);
   
   { Calculate the *relative* path for the STEP model that we just added to footprint. }
   stepFileRelPath := StringReplace(stepFileName, projectPath, '', 0);
   stepFileRelPath := StringReplace(stepFileRelPath, AnsiUpperCase(projectPath), '', 0);
   WriteToDebugFile(' stepFileRelPath is "' + stepFileRelPath + '".');
   
   { Report relative path to STEP model in the String field. }
   CLF_ReportCsvPropertyStr        ({name} constCsvRptFieldString,     {valueStr} stepFileRelPath,  {var} line);

   { Add properties specific to this type of 3D body. }
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldModelType,           {valueStr} 'STEP model',          {var} line);
   CLF_ReportCsvPropertyMm({name} constCsvRptFieldModelStandoffHeight, {valueMm} ZoffsetMm, {var} line);
   
   { Add line to csv report stringlist. }
   csvReportStrs.Add(line);
   
end; { end CLF_AddStepModel() }


{***************************************************************************
 * function CLF_FindStepModel()
 *  Find a STEP model for the current component.  Go search for it on the filesystem,
 *  based on footprint type, Mfg Name and Mfg Pkg Code.
 *
 *  If one is not available, warn the user and offer to extrude a simple 3D
 *  body (ala Altium IPC footprint wizard).
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_FindStepModel(    scriptsPath                            : TDynamicString;
                               projectPath                            : TDynamicString;
                               cnfGalacticInfo                        : TStringList;
                               ModelsDir                              : TString;
                               expectedStepFileName                   : TString;
                           var highestRevMatchingStepFileNameTrueCase : TString;
                           var highestRevNumber                       : Integer;
                               )                                      : Integer;

var
   i                                      : Integer;
   parms                                  : TStringList;
   stepFileList                           : TStringList;
   stepFileList2                          : TStringList;
   matchingStepFileList                   : TStringList;
   searchSubDirs                          : Boolean;
   footprintType                          : TString;
   mfgName                                : TString;
   mfgPkgCode                             : TString;
   foundStepFileName                      : TString;
   matchingStepFileName                   : TString;
   revNumber                              : Integer;
   highestRevMatchingStepFileName         : Tstring;
   Xrot                                   : Integer;
   Yrot                                   : Integer;
   Zrot                                   : Integer;
   ZoffsetMm                              : Real;
                           
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_FindStepModel().');

   WriteToDebugFile('expectedStepFileName is: ' + expectedStepFileName);
   WriteToDebugFile('ModelsDir is: ' + ModelsDir);

   {** Perform svn update of the directory structure where we will look for STEP models. **}
   { Create string list to hold svn parameters. }
   parms := TStringList.Create();

   { Add the directory we want to be svn updated. }
   parms.Add(ModelsDir);

   { Do an svn update of the directory structure where the 3D models live. }
   IssueSvnCommand(scriptsPath,
                   projectPath,
                   'update',
                   parms);

   
   {** Look for STEP models. **}
   { Create string lists to hold lists of STEP files. }
   stepFileList := TStringList.Create();
   stepFileList2 := TStringList.Create();
   matchingStepFileList := TStringList.Create();

   { Search for all .stp and .step files. }
   searchSubDirs := True;
   FindFiles((projectPath + ModelsDir), ('*' + constExtStepModel1), faAnyFile, searchSubDirs, stepFileList);

   { Re-use parms list for the second search for STEP files. }
   FindFiles((projectPath + ModelsDir), ('*' + constExtStepModel2), faAnyFile, searchSubDirs, stepFileList2);
   stepFileList.AddStrings(stepFileList2);

   { Sort the list of STEP files. }
   stepFileList.Sort();


   { Loop over all the STEP files that we found. }
   for i := 0 to (stepFileList.Count - 1) do
   begin
//      WriteToDebugFile('stepFileList[' + IntToStr(i) + '] is "' + stepFileList[i] + '".');

      { Strip off the path from the found STEP file name. }
      foundStepFileName    := ExtractFileName(stepFileList[i]);
//      WriteToDebugFile(' foundStepFileName is "' + foundStepFileName + '".');
      
      { See if the expected STEP file name matches the start of this found STEP file name. }
      if (CLF_DoesStringStartWith(foundStepFileName, expectedStepFileName)) then
      begin

         { Record that we found a matching STEP file. }
         WriteToDebugFile(' Found matching STEP file name: "' + foundStepFileName + '".');
         matchingStepFileList.Add(stepFileList[i]);
         
      end;
      
   end;

   {** See if we found any matching STEP files at all. **}
   if (matchingStepFileList.Count > 0) then
   begin
   
      {** Attempt to find the highest numbered revision of the STEP file we're looking for. **}
      highestRevNumber     := -1;   
      
      { Loop over all the matching STEP files. }
      for i := 0 to (matchingStepFileList.Count - 1) do
      begin
         
         { Strip off the path from the matching STEP file name. }
         matchingStepFileName := ExtractFileName(matchingStepFileList[i]);

         { Extract trailing revision number (eg. "_SPI17") from matching STEP file name. }
         CLF_ExtractTrailingNumberFromString({getMyTrailingNumber} ChangeFileExt(matchingStepFileName, ''),
                                             {var trailingNumber} revNumber);

         { See if this is the highest rev number so far. }
         if (revNumber > highestRevNumber) then
         begin

            { Record the rev number and the current STEP file name. }
            highestRevNumber     := revNumber;
            highestRevMatchingStepFileName := matchingStepFileList[i];
         end;

      end;

      {** Add the highest rev STEP file to queue. **}
      WriteToDebugFile(' highestRevNumber is ' + IntToStr(highestRevNumber) + '.');
      WriteToDebugFile(' Highest rev STEP file name: "' + highestRevMatchingStepFileName + '".');

      { Since this filename was obtained through Delphi FindFile(), it will be in all
       upper case.  This annoys me.  So run external command to obtain actual file casing. }
      CLF_GetActualCasingOfFileName(scriptsPath,
                                    {fileNameUpperCase} highestRevMatchingStepFileName, 
                                    {var fileNameTrueCase} highestRevMatchingStepFileNameTrueCase
                                    );
   end

   { Else we found no matching STEP files! }
   else
   begin
      { Return error code to caller. }
      result := 1;
   end; { endelse }

   { Free string lists. }
   parms.Free();
   stepFileList.Free();
   stepFileList2.Free();
   matchingStepFileList.Free();

end; { end CLF_FindStepModel() }


{***************************************************************************
 * function CLF_FindAndAddStepModel()
 *  Find an existing STEP model and add it to the queue.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_FindAndAddStepModel(    scriptsPath          : TDynamicString;
                                     projectPath          : TDynamicString;
                                     cnfGalacticInfo      : TStringList;
                                     ModelsDir            : TString;
                                 var expectedStepFileName : TString;
                                 var bodyQueue            : TInterfaceList;
                                 var primNames            : TStringList;
                                 var csvReportStrs        : TStringList;                               
                               )                          : Integer;

var
   Xrot                                   : Integer;
   Yrot                                   : Integer;
   Zrot                                   : Integer;
   ZoffsetMm                              : Real;
   found                                  : Integer;
   footprintType                          : TString;
   mfgName                                : TString;
   mfgPkgCode                             : TString;
   highestRevMatchingStepFileNameTrueCase : TString;
   highestRevNumber                       : Integer;

begin

   WriteToDebugFile('Hello from CLF_FindAndAddStepModel().');
   result := 0;

   { Retrieve necessary info from galactic string list. }
   footprintType        := cnfGalacticInfo.Values(constGilFootprintType);
   mfgName              := cnfGalacticInfo.Values(constGilPrefixLpWizardInfo + constLpWizardManufacturerField);
   mfgPkgCode           := cnfGalacticInfo.Values(constGilPrefixLpWizardInfo + constLpWizardMfgPkgCodeField);

   { Construct the expected STEP file name, based on the footprint type, mfg name, and mfg pkg code. }
   { Note:  We have to convert to all upper case because FindFiles() will give all results as all upper case! }
   expectedStepFileName := AnsiUpperCase(footprintType + '_' + mfgName + '_' + mfgPkgCode + '_' + const3dModelCompanySuffix);
   WriteToDebugFile(' expectedStepFileName is "' + expectedStepFileName + '".');
   
   { Call the find function using same parameters. }
   found := CLF_FindStepModel(scriptsPath,
                              projectPath,
                              cnfGalacticInfo,
                              ModelsDir,
                              expectedStepFileName,
                              {var} highestRevMatchingStepFileNameTrueCase,
                              {var} highestRevNumber);

   { Proceed if found. }
   if ( found = 0 ) then
   begin

      { We must know a priori what the appropriate x,y,z rotation and z offset should be. }
      { FIXME:  This must be a function of footprint type! }
      Xrot                           := 0;
      Yrot                           := 0;
      Zrot                           := 0;
      ZoffsetMm                      := 0.0;

      { Add the STEP model to queue. }
      CLF_AddStepModel({boardSide} constBoardSideCompBody,
                       {layer} constLayerCompBody,
                       {opacity} constCompBodyOpacity,
                       {stepFileName} highestRevMatchingStepFileNameTrueCase, 
                       Xrot,
                       Yrot,
                       Zrot,
                       ZoffsetMm,
                       {var} bodyQueue,
                       cnfGalacticInfo,
                       {var} primNames,
                       {var} csvReportStrs);
 
      CLF_WriteToSummaryAndDebugFilesWithStepNum('Added auto-generated STEP model "' + highestRevMatchingStepFileNameTrueCase + '" to new .PcbLib file.');

   end {end if}

   { Else, we did not find a matching STEP file. }
   else
   begin
      result := 1;
   end; { end else. }

end; { end CLF_FindAndAddStepModel() }


{***************************************************************************
 * function CLF_Create3dFreeCadCompBody()
 *  Setup to call FreeCAD python script to have it generate a 3D STEP model
 *  for this particular package.  Then create all other new 3D features on
 *  the CompBody layer.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_Create3dFreeCadCompBody(    scriptsPath     : TDynamicString;
                                         projectPath     : TDynamicString;
                                     var cnfGalacticInfo : TStringList;
                                     var trackQueue      : TInterfaceList;
                                     var arcQueue        : TInterfaceList;
                                     var textQueue       : TInterfaceList;
                                     var padQueue        : TInterfaceList;
                                     var regionQueue     : TInterfaceList;
                                     var fillQueue       : TInterfaceList;
                                     var bodyQueue       : TInterfaceList;
                                     var primNames       : TStringList;
                                     var csvReportStrs   : TStringList;
                                         )               : Integer;

var
   pkgDimsBallDiamNom             : Real;
   pkgDimsPinWidthMax             : Real;
   pkgDimsPinLandMax              : Real;
   pkgDimsPinLandMin              : Real;
   pkgDimsBodyWidthMax            : Real;
   pkgDimsBodyLengthMax           : Real;
   pkgDimsTotalWidthMax           : Real;
   pkgDimsTotalLengthMax          : Real;
   pkgDimsStandoffMin             : Real;
   pkgDimsEpWidthMax              : Real;
   pkgDimsEpLengthMax             : Real;
   pkgDimsEpChamfer               : Real;
   pkgDimsEpCornerRad             : Real;
   pkgDimsPivotPointRatio         : Real;
   pkgDimsHeightMax               : Real;
   iniFileOut                     : TStringList;
   iniFilePath                    : TString;
   libFileName                    : TString;
   Tp                             : Real;
   Hpph                           : Real;
   Hppl                           : Real;
   Fr                             : Real;
   Hpe                            : Real;
   padGroupName                   : TString;
   padType                        : TString;
   footprintType                  : TString;
   i                              : Integer;
   padDst                         : IPCB_Pad;
   padXmm                         : Real;
   padYmm                         : Real;
   padGroupNum                    : Integer;
   parms                          : TStringList;
   genOut                         : TStringList;
   fcMacrosPath                   : TString;
   genRcPath                      : TString;
   genRc                          : Integer;
   newModelPath                   : TString;
   companySuffix                  : TString;
   stepExt                        : TString;
   stepFilePath                   : TString;
   Xrot                           : Integer;
   Yrot                           : Integer;
   Zrot                           : Integer;
   ZoffsetMm                      : Real;
   freeCadPath                    : TString;
   cmd                            : TString;
   fcFound                        : Integer;
   fcstdFileName                  : TString;
   highestRevMatchingStepFileName : TString;
   modelPath                      : TSTring;
   highestRevNumber               : Integer;
   nextRevNumber                  : Integer;
   freeCadLogOld                  : TStringList;
   freeCadLogNew                  : TStringList;
   fcstdExt                       : TString;
   expectedStepFileName           : TString;
   logFilePath                    : TString;
   mode                           : Boolean;
   iniFileName                    : TString;
   iniPath                        : TString;
   fcstdFilePath                  : TString;
   filesAreReverted               : Boolean;
   P1chamferOffset                : Real;
   stepFileName                   : TString;
   modelDir                       : TString;
   deletedStepFile                : Boolean;
   hasDshapePads                  : Boolean;
   hasEp                          : Boolean;
   maDeg                          : Real;
   epPin1ChamferRadius            : Real;
   X1size                         : Real;
   Y1size                         : Real;
   X2size                         : Real;
   Y2size                         : Real;
   X1center                       : Real;
   Y1center                       : Real;
   X2center                       : Real;
   Y2center                       : Real;
   typeEp1_Ft                     : Real;
   typeEp2_Ft                     : Real;
   typeEp1_epPin1ChamferRadius    : Real;
   typeEp2_epPin1ChamferRadius    : Real;
   splitPin                       : TString;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_Create3dFreeCadCompBody().');

   { Build expected file name for existing STEP model. }
   libFileName := cnfGalacticInfo.Values(constGilLibraryFileName);
   companySuffix := '_' + const3dModelCompanySuffix;
   modelDir := ExtractFileName(StripTrailingBackslash(projectPath)) + '\';

   {* Check if there is an existing STEP model for the footprint. *}
   { Compose name of STEP model. }
   expectedStepFileName := AnsiUpperCase(libFileName + companySuffix);
   modelPath := constSpi3dModelsFcDir + modelDir;

   {* Setting up RevertOld...ReadIn() parameters. *}
   { Setup the paths and names of the FreeCAD related files that we are expecting
    to exist and/or be generated. }

   { FreeCAD Model (native binary FreeCAD file) }
   fcstdExt := '.FCStd';
   fcstdFileName := libFileName + fcstdExt;
   fcstdFilePath := projectPath + modelPath + fcstdFileName;
   WriteToDebugFile('fcstdFilePath: ' + fcstdFilePath);

   { Ini File (ASCII file that this script generates as "source code") }
   iniFileName := libFileName + '.ini';
   iniPath := projectPath + modelPath + iniFileName;
   WriteToDebugFile('iniPath: ' + iniPath);

   { Log File (Features report generated by python script) }
   logFilePath := projectPath + modelPath + libFileName + '.log';
   WriteToDebugFile('logFilePath: ' + logFilePath);

   WriteToDebugFile('About to loop over CLF_FindStepModel() and CLF_RevertOldCsvFileAndReadIn()');
   { Check to see if the highest rev STEP file is added to check-in queue.
    Delete it if it is not and repeat this process until the highest rev
    STEP file is not deleted. }
   repeat
      
      { Call find function to check if there is an existing STEP model. }
      fcFound := CLF_FindStepModel(scriptsPath,
                                   projectPath,
                                   cnfGalacticInfo,
                                   {ModelsDir} modelPath,
                                   expectedStepFileName,
                                   {var} highestRevMatchingStepFileName,
                                   {var} highestRevNumber);

      { If there is an existing STEP file, check the most recent csv file against the new one. }
      if ( fcFound = 0 ) then
      begin
         WriteToDebugFile('CreateFreeCad found the file');

         WriteToDebugFile('highestRevMatchingStepFileName: ' + highestRevMatchingStepFileName);
      end

      { Else create a new STEP model. }
      else
      begin
         WriteToDebugFile('CreateFreeCad did not find file');
         highestRevNumber := 0;
      end; 

      { STEP File (We will use the highest rev version to pass to CLF_RevertOldCsvFileAndReadIn(). If there is not an existing STEP files, we will pass it a null string. }
      stepExt := '.step';
      if (highestRevNumber = 0) then
      begin
         WriteToDebugFile('highestRevNumber: ' + IntToStr(highestRevNumber));
         stepFilePath := '';
         WriteToDebugFile('stepFilePath should be null: ' + stepFilePath + ' end');
      end
      
      else
      begin
         WriteToDebugFile('highestRevNumber: ' + IntToStr(highestRevNumber));
         stepFileName := libFileName + companySuffix + IntToStr(highestRevNumber) + stepExt;
         stepFilePath := projectPath + constSpi3dModelsFcDir + modelDir + stepFileName;
         WriteToDebugFile('stepFilePath: ' + stepFilePath);
      end;

      { Call CLF_RevertOldCsvFileAndReadIn() to revert any modified files and read-in freeCadLogOld. }
      CLF_RevertOldCsvFileAndReadIn(projectPath, 
                                    scriptsPath,
                                    {pcbLibOrFcstdFilePath} fcstdFilePath,
                                    {reportOrIniFilePath} iniPath,
                                    {pcbDocOrStepFilePath} stepFilePath,
                                    {csvOrLogFilePath} logFilePath,
                                    {var csvOrLogFileOld} freeCadLogOld,
                                    {var} deletedStepFile
                                    );

   until ( (not deletedStepFile) or (highestRevNumber = 0) ); { end loop }

   { The rev number of the 3d model we are about to generate will be one more than the current highest rev number. }
   nextRevNumber := highestRevNumber + 1;

   { Retrieve package dimension fields from galactic string list. }
   pkgDimsBallDiamNom   := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsBallDiamNom));
   pkgDimsPinWidthMax   := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsPinWidthMax));
   pkgDimsPinLandMax    := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsPinLandMax));
   pkgDimsPinLandMin    := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsPinLandMin));

   { Retrieve package dimension fields from galactic string list. }
   pkgDimsBodyWidthMax  := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsBodyWidthMax));
   pkgDimsBodyLengthMax := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsBodyLengthMax));
   pkgDimsTotalWidthMax  := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsTotalWidthMax));
   pkgDimsTotalLengthMax := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsTotalLengthMax));
   pkgDimsStandoffMin := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsStandoffMin));

   
   { Retrieve package height, as known by LPW tool. }
   { TODO:  Override this with derived footprint height, as needed! }
   pkgDimsHeightMax   := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsHeightMax));
   footprintType      := cnfGalacticInfo.Values(constGilFootprintType);
  
   { Compute the type of pad based on our footprint type. }

   { Look for packages with gullwing pins. }
   if ( (footprintType = 'SOIC') or (footprintType = 'SOT') or
       (footprintType = 'SOP') or (footprintType = 'QFP') ) then
   begin
      padType                  := 'Gullwing';

      { If the length of the two pins plus the body are longer than the total width,
       set the the length of the pins to be half of the pin width minus the body width plus 0.05.
       Otherwise, keep the pin length as the maximum pin length. }
      if ( ( (2*pkgDimsPinLandMax) + pkgDimsBodyWidthMax) > pkgDimsTotalWidthMax ) then
      begin
         pkgDimsPinLandMax := ( (pkgDimsTotalWidthMax - pkgDimsBodyWidthMax) / 2.0 ) - 0.05;
      end;
   end
   
   else if( (footprintType = 'QFN') ) then
   begin
      padType := 'QFN';
   end

   { Else unsupported. }
   else
   begin
      { If we don't know how to create a STEP model, we will delete the 3d files we just created. }
      DeleteFileWithVerify(fcstdFilePath);
      DeleteFileWithVerify(iniPath);
      DeleteFileWithVerify(logFilePath);
      
      { CLF_Create3dFreeCadCompBody() has failed so we will return and create an extruded model. } 
      result := 1;
      
      Exit; { Exit this function }
   end;

   { Store FreeCAD file paths in cnfGalacticInfo. They are added here (as opposed to after they were constructed
    because we do not want to store them if the package type is not supported. }
   cnfGalacticInfo.add(constFC3DM_fcstdFilePath + constStringEquals + fcstdFilePath);
   cnfGalacticInfo.add(constFC3DM_iniFilePath + constStringEquals + iniPath);
   cnfGalacticInfo.add(constFC3DM_logFilePath + constStringEquals + logFilePath); 

   { See if xml command file specified pivot point height ratio. }
   if ( CLF_IsNameInStringList(constFC3DM_pivotPointHeightRatio, cnfGalacticInfo) ) then
   begin
      pkgDimsPivotPointRatio := StrToFloat(cnfGalacticInfo.Values(constFC3DM_pivotPointHeightRatio));
   end

   { Else if footprint is SOT, set pivot point to two-thirds the body height. }
   else if ( (footprintType = 'SOT') ) then
   begin
      pkgDimsPivotPointRatio := 0.66667;
   end
   
   { Else footprint is not SOT.  Set pivot point to half the body height. }
   else
   begin
      { Hpph = (0.5*pkgDimsHeightMax) + (0.5*Tp) }
      { Hppl = (0.5*pkgDimsHeightMax) - (0.5*Tp) }
      pkgDimsPivotPointRatio := 0.5;
   end;

   { Ratio multiplied by package height. }
   Hpph := ( pkgDimsPivotPointRatio * pkgDimsHeightMax);

   
   { Set the entry point to be at the pivot point. }
   Tp := 0.15;
   Hppl := Hpph;
   Hpe := Hpph + (0.5*Tp);
   
   { Create string list to hold ini file content. }
   iniFileOut := TStringList.Create();

   
   {* Create global ini file. *}   
   { Compute the iniFilePath for the global ini file. }
   iniFilePath := projectPath + constSpi3dModelsFcDir + constSpi3dModelsScriptSubDir + constSpi3dModelsGlobalIniFile;
   fcMacrosPath := projectPath + constSpi3dModelsFcDir + StripTrailingBackslash(constSpi3dModelsScriptSubDir); {iniFilePath}

   iniFileOut.add('# Global ini file for FC3DM scripts.');
   iniFileOut.add('');
   iniFileOut.add('# We currently have no way to pass command line parameters to our python scripts.');
   iniFileOut.add('# Thus, we don''t have an obvious way to specify what component we want to build right now.');
   iniFileOut.add('# So we use this ini file to point to another ini file.');
   iniFileOut.add('');
   iniFileOut.add('# Ini file that describes the component that we actually want to build now.');
   iniFileOut.add('iniFileName = "' + '..\' + modelDir + libFileName + '.ini"');

   { Actually write string list to iniFile. }
   iniFileOut.SaveToFile(iniFilePath);
   
   {* Create component-specific ini file. *}
   { Clear string list in preparation for writing component-specific ini file. }
   iniFileOut.Clear();
   
   { Compute the iniFilePath for the component-specific ini file. }
   newModelPath := projectPath + modelPath;
   iniFilePath  := newModelPath + libFileName + '.ini';
   companySuffix   := companySuffix + IntToStr(nextRevNumber);
   stepFilePath := newModelPath + libFileName + companySuffix + stepExt;

   iniFileOut.add('# Parameters that describe how to generate a specific component''s 3D model');
   iniFileOut.add('');
   iniFileOut.add('###################################');
   iniFileOut.add('## Invariant information');
   iniFileOut.add('###################################');
   iniFileOut.add('');
   iniFileOut.add('# Directory to which to write our finished native and STEP models');
   iniFileOut.add('newModelPath = "' + projectPath + constSpi3dModelsFcDir + modelDir);
   iniFileOut.add('');
   iniFileOut.add('# Suffix to add to STEP models');
   iniFileOut.add('stepSuffix = "' + companySuffix + '"');
   iniFileOut.add('suffix = "_SvnRev_"');
   iniFileOut.add('stepExt = "' + stepExt + '"');
   iniFileOut.add('');
   iniFileOut.add('');

   
   iniFileOut.add('###################################');
   iniFileOut.add('## Parameters for this specific model');
   iniFileOut.add('###################################');
   iniFileOut.add('newModelName = "' + libFileName + '"');
   iniFileOut.add('bodyName = "Body"');
   iniFileOut.add('pinName = "Pin"');
   iniFileOut.add('pin1MarkName = "Pin1Mark"');
   iniFileOut.add('');

   iniFileOut.add('## Parameters off datasheet');
   iniFileOut.add('L=' + FloatToStr(pkgDimsTotalWidthMax) + ' #pkgDimsTotalWidthMax');
   iniFileOut.add('T=' + FloatToStr(pkgDimsPinLandMax) + ' #pkgDimsPinLandMax');
   iniFileOut.add('W=' + FloatToStr(pkgDimsPinWidthMax) + ' #pkgDimsPinWidthMax');
   iniFileOut.add('A=' + FloatToStr(pkgDimsBodyWidthMax) + ' #pkgDimsBodyWidthMax');
   iniFileOut.add('B=' + FloatToStr(pkgDimsBodyLengthMax) + ' #pkgDimsBodyLengthMax');
   iniFileOut.add('H=' + FloatToStr(pkgDimsHeightMax) + ' #pkgDimsHeightMax');
   iniFileOut.add('K=' + FloatToStr(pkgDimsStandoffMin) + ' #pkgDimsStandoffMin');
   iniFileOut.add('');
   
   { Retrieve whether this package is known to have an exposed pad (EP). }
   hasEp := StrToBool(cnfGalacticInfo.Values(constGilHasEp));

   { If the footprint has an exposed pad, we will include the appropriate dimensions in the ini file. }
   if ( hasEp ) then
   begin
      { Retrieve the EP dimensions from cnfGalacticInfo. }
      pkgDimsEpWidthMax  := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsEpWidthMax));
      pkgDimsEpLengthMax := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsEpLengthMax));
      pkgDimsEpChamfer := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsEpChamfer));
      pkgDimsEpCornerRad := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsEpCornerRad));
      
      iniFileOut.add('# Footprint has an EP');
      iniFileOut.add('Tt=' + FloatToStr(pkgDimsEpLengthMax) + ' #pkgDimsEpLengthMax');
      iniFileOut.add('Wt=' + FloatToStr(pkgDimsEpWidthMax) + ' #pkgDimsEpWidthMax');

      { See if xml command file specified . }
      if ( CLF_IsNameInStringList(constFC3DM_epPin1ChamferRadius, cnfGalacticInfo) ) then
      begin

         { Retrieve the value specified by the xml file for epPin1ChamferRadius. }
         epPin1ChamferRadius := StrToFloat(cnfGalacticInfo.Values(constFC3DM_epPin1ChamferRadius));

         { Inform user that we will set pkgDimsEpChamfer that we will be setting pkgDimsEpChamfer to zero. }
         if ( pkgDimsEpChamfer <> 0.0 ) then
         begin
            ShowMessage('epPin1ChamferRadius was found in xml file and pkgDimsEpChamfer is not zero. Proceeding to set pkgDimsEpChamfer to zero');
            pkgDimsEpChamfer := 0.0;
         end;
      end

      { If epPin1ChamferRadius is not specified in the xml file, we shall set it to zero. }
      else
         epPin1ChamferRadius := 0.0;

      iniFileOut.add('Ft=' + FloatToStr(pkgDimsEpChamfer) + ' #pkgDimsEpChamfer');
      iniFileOut.add('Rt=' + FloatToStr(pkgDimsEpCornerRad) + ' #pkgDimsEpCornerRad');
      iniFileOut.add('epPin1ChamferRadius=' + FloatToStr(epPin1ChamferRadius) + ' # Defined in xml file');
      iniFileOut.add('');

      splitPin := cnfGalacticInfo.Values(constGilSplitPinPinName);
   
      { See if xml command file specified a split pin.
      	Extract the parameters and write them to the ini fil. }
      if ( splitPin <> '' ) then
      begin
         
         X1center      := StrToFloat(cnfGalacticInfo.Values(constGilSplitPinX1center));
         Y1center      := StrToFloat(cnfGalacticInfo.Values(constGilSplitPinY1center));
         X1size        := StrToFloat(cnfGalacticInfo.Values(constGilSplitPinX1size));
         Y1size        := StrToFloat(cnfGalacticInfo.Values(constGilSplitPinY1size));
         X2center      := StrToFloat(cnfGalacticInfo.Values(constGilSplitPinX2center));
         Y2center      := StrToFloat(cnfGalacticInfo.Values(constGilSplitPinY2center));
         X2size        := StrToFloat(cnfGalacticInfo.Values(constGilSplitPinX2size));
         Y2size        := StrToFloat(cnfGalacticInfo.Values(constGilSplitPinY2size));
         
         if ( Y1center > Y2center ) then
         begin
            typeEp1_Ft := pkgDimsEpChamfer;
            typeEp2_Ft := 0.0;
            typeEp1_epPin1ChamferRadius := epPin1ChamferRadius;
            typeEp2_epPin1ChamferRadius := 0.0;
         end
         else
         begin
            typeEp1_Ft := 0.0;
            typeEp2_Ft := pkgDimsEpChamfer;
            typeEp1_epPin1ChamferRadius := 0.0;
            typeEp2_epPin1ChamferRadius := epPin1ChamferRadius;
         end;
         
         iniFileOut.add('# Split Pin 1 dimensions as defined by xml file');
         iniFileOut.add('TypeEp1_Tt=' + FloatToStr(Y1size) + ' #Y1size');
         iniFileOut.add('TypeEp1_Wt=' + FloatToStr(X1size) + ' #X1size');
         iniFileOut.add('TypeEp1_Ft=' + FloatToStr(typeEp1_Ft) + ' #typeEp1_Ft');
         iniFileOut.add('TypeEp1_Rt=' + FloatToStr(pkgDimsEpCornerRad) + ' #pkgDimsEpCornerRad');
         iniFileOut.add('TypeEp1_epPin1ChamferRadius=' + FloatToStr(typeEp1_epPin1ChamferRadius));
         iniFileOut.add('');
         
         iniFileOut.add('# Split Pin 2 dimensions as defined by xml file');
         iniFileOut.add('TypeEp2_Tt=' + FloatToStr(Y2size) + ' #Y2size');
         iniFileOut.add('TypeEp2_Wt=' + FloatToStr(X2size) + ' #X2size');
         iniFileOut.add('TypeEp2_Ft=' + FloatToStr(typeEp2_Ft) + ' #typeEp2_Ft');
         iniFileOut.add('TypeEp2_Rt=' + FloatToStr(pkgDimsEpCornerRad) + ' #pkgDimsEpCornerRad');
         iniFileOut.add('TypeEp2_epPin1ChamferRadius=' + FloatToStr(typeEp2_epPin1ChamferRadius));
         iniFileOut.add('');
    
      end;
      
   end;

   { If the footprint is a QFN or DFN, then check if it has D-shape pads and include that in the ini file. }
   if ( (footprintType = 'QFN') or (footprintType = 'DFN') ) then
   begin
      { Retrieve boolean from cnfGalacticInfo. }
      hasDshapePads := StrToBool(cnfGalacticInfo.Values(constGilPkgDimsHasDshapePads));

      iniFileOut.add('# Does the footprint have D-shape pads (-1 has D-shape, 0 does not have D-shape)');
      iniFileOut.add('hasDshapePads=' + BoolToStr(hasDshapePads) + ' #hasDshapePads');
      iniFileOut.add('');
   end;
   
   iniFileOut.add('# Thickness of pin (in Z)');
   iniFileOut.add('Tp = ' + FloatToStr(Tp));
   iniFileOut.add('');

   iniFileOut.add('## Parameters that we have to make up, since they''re not usually specified on datasheet.' );
   iniFileOut.add('');
   
   iniFileOut.add('# Mold angle (in degrees) (not specified in datasheet--make something up)');
   if ( (footprintType = 'QFN') or (footprintType = 'DFN') ) then
   begin
      maDeg := constFC3DM_qfnMoldAngle;
   end
   else
      maDeg := constFC3DM_gullwingMoldAngle;
   iniFileOut.add('maDeg = ' + FloatToStr(maDeg));
   iniFileOut.add('');
   
   iniFileOut.add('# Pivot points (in Z) for chamfering the IC body');
   iniFileOut.add('Hpph = ' + FloatToStr(Hpph) + ' #0.5*pkgDimsHeightMax');
   iniFileOut.add('Hppl = ' + FloatToStr(Hppl) + ' #Hpph');
   iniFileOut.add('');

   iniFileOut.add('# Fillet radius for pin edges');
   Fr := Tp;
   iniFileOut.add('Fr = ' + FloatToStr(Fr) + ' #Tp');
   iniFileOut.add('');
   
   iniFileOut.add('# Height of entry of pin (center) to IC body');
   iniFileOut.add('Hpe = ' + FloatToStr(Hpe) + ' #Hpph + (0.5*Tp)');
   iniFileOut.add('');
   
   iniFileOut.add('# Fillet radius for body');
   iniFileOut.add('Frbody = 0.1');
   iniFileOut.add('');

   { If footprint is SOIC, then find the pin 1 chamfer offset parameter. }
   if ( footprintType = 'SOIC' ) then
   begin
      
      WriteToDebugFile('Footprint is identified as SOIC, proceeding to find P1chamferOffset');
      
      { If the pin 1 chamfer offset is defined in cnfGalacticInfo, use that value. }
      if ( CLF_IsNameInStringList(constFC3DM_p1chamferOffset, cnfGalacticInfo) ) then
      begin
         P1chamferOffset := StrToFloat(cnfGalacticInfo.Values(constFC3DM_p1chamferOffset));
         WriteToDebugFile('P1chamferOffset found in cnfGalacticInfo. P1chamferOffset is: ' + FloatToStr(P1chamferOffset));
      end { end if }
      
      { Else, set the pin 1 chamfer offset to the default value, constP1soicChamferOffset. }
      else
      begin
         P1chamferOffset := constP1soicChamferOffset;
         WriteToDebugFile('P1chamferOffset not found in cnfGalacticInfo. By default, P1chamferOffset is: ' + FloatToStr(P1chamferOffset));
      end; { end else }
      
      { new parameter for SOICs }
      iniFileOut.add('# Chamfer parameter for SOICs');
      iniFileOut.add('P1chamferOffset = ' + FloatToStr(P1chamferOffset));
      iniFileOut.add('');
      
   end; { end if }
   
   iniFileOut.add('# Offset in X and Y from the pin 1 corner to the start of the pin 1 marker cylinder');
   iniFileOut.add('P1markOffset = 0.07');
   iniFileOut.add('');
   
   iniFileOut.add('# Radius of pin 1 marker cylinder');
   iniFileOut.add('P1markRadius = 0.15');
   iniFileOut.add('');
   
   iniFileOut.add('# How much to indent the pin 1 marker into IC body');
   iniFileOut.add('P1markIndent = 0.02');
   iniFileOut.add('');
   
   iniFileOut.add('# Height of marking ink');
   iniFileOut.add('markHeight = 0.001');
   iniFileOut.add('');
   
   {* Write pin information to output ini file. *}
   iniFileOut.add('## Pin numbers, type, side, and x,y coordinates:');

   {* Output pin names, type, groups, and x,y coordinates for all pins. *}
   { Loop over all the pads in the queue. }
   for i := 0 to (padQueue.Count - 1) do
   begin
      
      { Retrieve reference to queued pad. }
      padDst := padQueue.items[i];
      
      { Retrieve group number of queued pad. }
      padGroupNum := CLF_GetPadGroupNum(padDst);

      { Retrieve group name of queued pad. }
      padGroupName := primNames.Strings(padGroupNum);

      { Strip off leading 'pads' from group name. }
      padGroupName := StringReplace(padGroupName, 'pads', '', MkSet(rfReplaceAll));
      
      { Retrieve pad x,y coordinates. }
      padXmm               := CoordToMMs(padDst.X);
      padYmm               := CoordToMMs(padDst.Y);

      { If the current pad is an EP, set padType to EP.
      	If there is a split pin, name the two accordingly. }
      if ( padGroupNum = constPadGroupEp ) then
      begin

         if ( splitPin <> '' ) then
         begin

            WriteToDebugFile('padXmm: ' + FloatToStr(padXmm) + ' padYmm: ' + FloatToStr(padYmm));
            WriteToDebugFile('X1center: ' + FloatToStr(X1center) + ' Y1center: ' + FloatToStr(Y1center));
            if ( (Abs(padXmm - X1center) < 0.000001) and (Abs(padYmm - Y1center) < 0.000001) ) then
            begin
               padType := 'TypeEp1';
            end
            else
               padType := 'TypeEp2';
         end
         else
            padType := 'EP';
         
      end;
         
      { Prepare output line. }
      { Format: 'Pin'[pin_name]=[type],[group],[x],[y] }
      { Example:  'Pin1=Gullwing,West,-3.3,2.925' }
      iniFileOut.add('Pin' + padDst.Name + '=' + padType + ',' + padGroupName + ',' + FloatToStr(padXmm) + ',' + FloatToStr(padYmm));

   end; { endfor }

   { Actually write string list to iniFile. }
   iniFileOut.SaveToFile(iniFilePath);
   
   { Free string list. }
   iniFileOut.free();


   {* Run FreeCAD script. *}   

   { Setup path to rc file that we will use to test if FreeCAD is done. }
   genRcPath := fcMacrosPath + '\python.rc';
   DeleteFileWithVerify(genRcPath);

   { Find FreeCAD.exe on this system. }
   if (FileExists('c:\Program Files\FreeCAD0.13\bin\FreeCAD.exe')) then
   begin
      freeCadPath := 'c:\Program Files\FreeCAD0.13\bin\FreeCAD.exe';
   end
   else if (FileExists('c:\Program Files (x86)\FreeCAD0.13\bin\FreeCAD.exe')) then
   begin
      freeCadPath := 'c:\Program Files (x86)\FreeCAD0.13\bin\FreeCAD.exe';
   end
   else
      CLF_Abort('Could not find path to FreeCAD.exe!');
   WriteToDebugFile('*Path to FreeCAD is "' + freeCadPath + '".');
   
   { Run FreeCAD and tell it to launch our python script. }
   cmd := '"' + freeCadPath + '"' + ' -l ' + fcMacrosPath + '\' + constFreeCadPythonScriptName;
   WriteToDebugFile('*About to call command "' + cmd + '".');
   RunSystemCommand(cmd);

   { Wait for FreeCAD python script to complete and get its return code. }
   AwaitSvnCompletion(genRcPath,
                      constStandardTimeoutLimit,
                      {var} genRc);

   { Check return code from external command. }
   WriteToDebugFile('*Return code from FreeCAD was ' + genRc);
   if (StrToInt(genRc) <> 0) then
   begin
      MyAbort('External FreeCAD command reported error.  Return code was ' + genRc + '!');
   end;
   CLF_WriteToSummaryAndDebugFilesWithStepNum('Used FreeCAD python script to auto-generate custom STEP model for this footprint.');

   { Verify that we have a generated STEP file. }
   if (not FileExists(stepFilePath)) then
      CLF_Abort('Could not find STEP model file "' + stepFilePath + '"!');
   

   {* See if resulting FreeCAD STEP model is any different from a previously-existing version of such. *}

   { Initialize stringlist. }
   freeCadLogNew  := TStringList.Create();
   
   { Read in python-generated features report to stringlist freeCadLogNew }
   { Read old csv report file into stringlist. }
   freeCadLogNew.LoadFromFile(logFilePath);
   
   { Set function mode }
   mode := True;
   
   { Call CLF_RevertGeneratedFilesIfNeeded() to compare old vs new log files.  If those are the same,
   the revert all FreeCAD related files (throwing out newly created version), and delete the newest
   STEP model. }
   CLF_RevertGeneratedFilesIfNeeded(projectPath,
                                    scriptsPath,
                                    {pcbLibOrFcstdFilePath} fcstdFilePath,
                                    {reportOrIniFilePath} iniPath,
                                    {pcbDocOrStepFilePath} stepFilePath,
                                    {csvOrLogFilePath} logFilePath,
                                    mode,
                                    {var csvOrLogFileOld} freeCadLogOld,
                                    {var csvOrLogFileOut} freeCadLogNew,
                                    {var filesAreReverted} filesAreReverted
                                    );
   
   { If that function reverted files, then it also deleted the newly generated STEP model.
   In this case, we want to fall back to the highest rev STEP model that already existed. }
   if ( filesAreReverted ) then
   begin
      stepFilePath := highestRevMatchingStepFileName;
      nextRevNumber := nextRevNumber - 1;
   end;
   
   { Store highestRevNumber and stepFilePath in cnfGalacticInfo. }
   cnfGalacticInfo.add(constFC3DM_highestRevNumber + constStringEquals + IntToStr(nextRevNumber));
   cnfGalacticInfo.add(constFC3DM_stepFilePath + constStringEquals + stepFilePath);   

 
   {* Add resulting STEP model to footprint queue. *}

   { Our auto-generated STEP model will be such that we need no rotation or offset. }
   Xrot                           := 0;
   Yrot                           := 0;
   Zrot                           := 0;
   ZoffsetMm                      := 0.0;
      
   WriteToDebugFile('stepFilePath: ' + stepFilePath);
   { Add the STEP model to queue. }
   CLF_AddStepModel({boardSide} constBoardSideCompBody,
                    {layer} constLayerCompBody,
                    {opacity} constCompBodyOpacity,
                    {stepFileName} stepFilePath,
                    Xrot,
                    Yrot,
                    Zrot,
                    ZoffsetMm,
                    {var} bodyQueue,
                    cnfGalacticInfo,
                    {var} primNames,
                    {var} csvReportStrs);
   CLF_WriteToSummaryAndDebugFilesWithStepNum('Added auto-generated STEP model "' + stepFilePath + '" to new .PcbLib file.');

end; { end CLF_Create3dFreeCadCompBody() }


{***************************************************************************
 * function CLF_Create3dThermalBody()
 *  Create a simple rectangular extrusion (set to invisible).  This is used
 *  for exporting a simpler STEP version of board for thermal modeling.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_Create3dThermalBody(    scriptsPath     : TDynamicString;
                                     projectPath     : TDynamicString;
                                 var cnfGalacticInfo : TStringList;
                                 var bodyQueue       : TInterfaceList;
                                 var primNames       : TStringList;
                                 var csvReportStrs   : TStringList;
                                     )               : Integer;

var
   assemblyWestMm     : Real;
   assemblyEastMm     : Real;
   assemblyNorthMm    : Real;
   assemblySouthMm    : Real;
   borderWestMm       : Real;
   borderEastMm       : Real;
   borderNorthMm      : Real;
   borderSouthMm      : Real;
   pkgDimsStandoffMin : Real;
   pkgDimsHeightMax   : Real;
   libHeightMm        : Real;

begin
   
   { Assume success. }
   result := 0;

   WriteToDebugFile('*Hello world from CLF_Create3dThermalBody()');

   { Retrieve the bounds for the assembly outline. }
   CLF_RetrieveBoundingRectangleByNamePrefix({namePrefix} 'Assembly', 
                                             {var} cnfGalacticInfo,
                                             {var boundaryWestMm} assemblyWestMm,
                                             {var boundaryEastMm} assemblyEastMm,
                                             {var boundaryNorthMm} assemblyNorthMm,
                                             {var boundarySouthMm} assemblySouthMm);

   { This is for gullwing ICs. }
   borderWestMm := assemblyWestMm;
   borderEastMm := assemblyEastMm;
   borderNorthMm := assemblyNorthMm;
   borderSouthMm := assemblySouthMm;
   

   { Retrieve package dimension fields from galactic string list. }
   pkgDimsStandoffMin := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsStandoffMin));

   { Retrieve package height, as known by LPW tool. }
   { TODO:  Override this with derived footprint height, as needed! }
   pkgDimsHeightMax   := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsHeightMax));
   libHeightMm   := pkgDimsHeightMax;
   
   WriteToDebugFile(' pkgDimsStandoffMin is ' + FloatToStr(pkgDimsStandoffMin) + '.');
   WriteToDebugFile(' pkgDimsHeightMax is ' + FloatToStr(pkgDimsHeightMax) + '.');

   { Extrude 3D body for component body. }
   CLF_Create3dRectangularExtrusion({boardSide} constBoardSideCompBody,
                                    {layer} constLayerCompBody,
                                    {color} constCompBodyColor,
                                    {opacity} constCompBodyOpacityThermal,
                                    borderWestMm,
                                    borderEastMm,
                                    borderNorthMm,
                                    borderSouthMm,
                                    {overallHeightMm} libHeightMm,
                                    {standoffHeightMm} pkgDimsStandoffMin,
                                    {identifier} constNameCompBodyThermal,
                                    {var} bodyQueue,
                                    {var} primNames,
                                    {var} csvReportStrs
                                    );
         
end; { end CLF_Create3dThermalBody() }

                                  
{***************************************************************************
 * function CLF_Create3dExtrudedCompBody()
 *  Create all new 3D features on the ComptBody layer.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_Create3dExtrudedCompBody(var cnfGalacticInfo : TStringList;
                                      var trackQueue      : TInterfaceList;
                                      var arcQueue        : TInterfaceList;
                                      var textQueue       : TInterfaceList;
                                      var padQueue        : TInterfaceList;
                                      var regionQueue     : TInterfaceList;
                                      var fillQueue       : TInterfaceList;
                                      var bodyQueue       : TInterfaceList;
                                      var primNames       : TStringList;
                                      var csvReportStrs   : TStringList;                               
                                          )               : Integer;
var
   i                        : Integer;
   rc                       : Integer;
   footprintType            : TString;
   assemblyWestMm           : Real;
   assemblyEastMm           : Real;
   assemblyNorthMm          : Real;
   assemblySouthMm          : Real;
   pkgDimsStandoffMin       : Real;
   pkgDimsHeightMax         : Real;
   libHeightMm              : TString;
   padDst                   : IPCB_Pad;
   padXmm                   : Real;
   padYmm                   : Real;
   pkgDimsBallDiamNom       : Real;
   pkgDimsPinWidthMax       : Real;
   pkgDimsPinLandMax        : Real;
   pkgDimsPinLandMin        : Real;
   pkgDimsBodyWidthMax      : Real;
   pkgDimsBodyLengthMax     : Real;
   pkgDimsTotalWidthMax     : Real;
   pkgDimsTotalLengthMax    : Real;
   compBodyPin1MarkArcRadMm : Real;
   compBodyPinThicknessMm   : Real;
   padGroupNum              : Integer;
   borderWestMm             : Real;
   borderEastMm             : Real;
   borderNorthMm            : Real;
   borderSouthMm            : Real;
   omitSide                 : TString;
   bodyWidthMm              : Real;
   bodyLengthMm             : Real;
   totalWidthMm             : Real;
   totalLengthMm            : Real;
   pinLandMm                : Real;
   ZstartMm                 : Real;
   ZstopMm                  : Real;
   ZpinEscapeMult           : Real;
   padNameSortable          : TString;
   bodyColor                : Integer;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_Create3dExtrudedCompBody().');
   
   { Retrieve package dimension fields from galactic string list. }
   pkgDimsBallDiamNom   := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsBallDiamNom));
   pkgDimsPinWidthMax   := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsPinWidthMax));
   pkgDimsPinLandMax    := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsPinLandMax));
   pkgDimsPinLandMin    := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsPinLandMin));

   { Retrieve package dimension fields from galactic string list. }
   pkgDimsBodyWidthMax  := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsBodyWidthMax));
   pkgDimsBodyLengthMax := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsBodyLengthMax));
   pkgDimsTotalWidthMax  := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsTotalWidthMax));
   pkgDimsTotalLengthMax := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsTotalLengthMax));
   pkgDimsStandoffMin := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsStandoffMin));

   { Retrieve package height, as known by LPW tool. }
   { TODO:  Override this with derived footprint height, as needed! }
   pkgDimsHeightMax   := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsHeightMax));
   libHeightMm   := pkgDimsHeightMax;
   
         
   {** Add 3D extrusion for component body. **}
   
   { Retrieve other info from galactic string list. }
   footprintType      := cnfGalacticInfo.Values(constGilFootprintType);

   { Retrieve the bounds for the assembly outline. }
   CLF_RetrieveBoundingRectangleByNamePrefix({namePrefix} 'Assembly',
                                             {var} cnfGalacticInfo,
                                             {var boundaryWestMm} assemblyWestMm,
                                             {var boundaryEastMm} assemblyEastMm,
                                             {var boundaryNorthMm} assemblyNorthMm,
                                             {var boundarySouthMm} assemblySouthMm);

   {** Compute parameters for extruding component body. **}
   { Handle chip inductors }
   { TODO:  We should probably also check chip vs. molded package types. }
   if (footprintType = 'Inductor') then
   begin
      
      pinLandMm       :=  (pkgDimsPinLandMax + pkgDimsPinLandMin) * 0.5;    { Use nominal termination width. }
      
      borderWestMm := assemblyWestMm + pinLandMm;
      borderEastMm := assemblyEastMm - pinLandMm;
      borderNorthMm := assemblyNorthMm;
      borderSouthMm := assemblySouthMm;

      bodyColor     := constCompBodyChipInd;
      
   end

   { Handle molded capacitors }
   { TODO:  We should probably also check chip vs. molded package types. }
   else if (footprintType = 'Capacitor') then
   begin
      
      { Subtract the pin thickness from the total length. }
      borderWestMm := assemblyWestMm + constCompBodyPinThicknessMoldMm;
      borderEastMm := assemblyEastMm - constCompBodyPinThicknessMoldMm;
      borderNorthMm := assemblyNorthMm;
      borderSouthMm := assemblySouthMm;

      bodyColor     := constCompBodyMoldCap;
      
   end

   { Handle all other footprint types. }
   else
   begin
      borderWestMm := assemblyWestMm;
      borderEastMm := assemblyEastMm;
      borderNorthMm := assemblyNorthMm;
      borderSouthMm := assemblySouthMm;

      bodyColor     := constCompBodyColor;
      
   end; { endelse }      

   
   { Extrude 3D body for component body. }
   CLF_Create3dRectangularExtrusion({boardSide} constBoardSideCompBody,
                                    {layer} constLayerCompBody,
                                    {color} bodyColor,
                                    {opacity} constCompBodyOpacity,
                                    borderWestMm,
                                    borderEastMm,
                                    borderNorthMm,
                                    borderSouthMm,
                                    {overallHeightMm} libHeightMm,
                                    {standoffHeightMm} pkgDimsStandoffMin,
                                    {identifier} constNameCompBody,
                                    {var} bodyQueue,
                                    {var} primNames,
                                    {var} csvReportStrs
                                    );

   {** Create pin 1 marking on extruded 3D component body. }
   { Handle chip inductors }
   { TODO:  We should probably also check chip vs. molded package types. }
   if (footprintType = 'Inductor') then
   begin

      { Do nothing }
   end

   { Handle molded capacitors }
   { TODO:  We should probably also check chip vs. molded package types. }
   else if (footprintType = 'Capacitor') then
   begin

      { Take up the west-most 0.4mm of the body. }
      borderWestMm := assemblyWestMm + constCompBodyPinThicknessMoldMm;
      borderEastMm := borderWestMm + constCompBodyMoldCapAnodeMarkLen;
      borderNorthMm := assemblyNorthMm;
      borderSouthMm := assemblySouthMm;

      { Create 3D rectangular extrusion to model anode bar on top of body. }
      CLF_Create3dRectangularExtrusion({boardSide} constBoardSideCompBody,
                                       {layer} constLayerCompBody,
                                       {color} constCompBodyPin1MarkColor,
                                       {opacity} constCompBodyOpacity,
                                       borderWestMm,
                                       borderEastMm,
                                       borderNorthMm,
                                       borderSouthMm,
                                       {overallHeightMm} (libHeightMm + constCompBodyMarkHeight),
                                       {standoffHeightMm} libHeightMm,
                                       {identifier} 'Component_mark_pin_1',
                                       {var} bodyQueue,
                                       {var} primNames,
                                       {var} csvReportStrs);

   end { end elsif }

   { Handle all other package types. }
   else
      begin
      
      {* Decide if we have a "small" component that needs to use minimal feature sizes for assembly. *}
      if (  ( (assemblyEastMm - assemblyWestMm) < constThreshSmallAssyOutlineMm ) or
          ( (assemblyNorthMm - assemblySouthMm) < constThreshSmallAssyOutlineMm )  ) then
      begin

         WriteToDebugFile('Hello from CLF_Create3dExtrudedCompBody().  Using minimal compbody spacings!');

         { Use minimal compbody pin 1 marking. }
         compBodyPin1MarkArcRadMm     := constMinCompBodyPin1MarkArcRadMm;

      end
      
      else
      begin
         
         WriteToDebugFile('Hello from CLF_Create3dExtrudedCompBody().  Using nominal compbody spacings.');

         { Use nominal compbody pin 1 marking. }
         compBodyPin1MarkArcRadMm     := constNomCompBodyPin1MarkArcRadMm;

      end;

      {* Create 3D cylinder to model compbody pin 1 marking. *}
      CLF_Create3dCylinder({boardSide}  constBoardSideCompBody,
                           {layer} constLayerCompBody,
                           {color} constCompBodyPin1MarkColor,
                           {opacity} constCompBodyOpacity,
                           {XCenterMm} (assemblyWestMm + compBodyPin1MarkArcRadMm + 0.05),
                           {YCenterMm} (assemblyNorthMm - compBodyPin1MarkArcRadMm - 0.05),
                           {radiusMm} compBodyPin1MarkArcRadMm,
                           {ZheightMm} (libHeightMm + constCompBodyMarkHeight),
                           {ZoffsetMm} libHeightMm,
                           {identifier} 'Component_mark_pin_1',
                           {var} bodyQueue,
                           {var} primNames,
                           {var} csvReportStrs);

      //   CLF_WriteToSummaryAndDebugFilesWithStepNum('Created interior pin 1 marker on assembly drawing layer for new .PcbLib file.');

   end; { endif }


   {* Create 3D extrusions to model component pins. *}
   { Loop over all the pads in the queue. }
   for i := 0 to (padQueue.Count - 1) do
   begin
      
      { Retrieve reference to queued pad. }
      padDst := padQueue.items[i];
      
      { Retrieve group number of queued pad. }
      padGroupNum := CLF_GetPadGroupNum(padDst);
      
      { Get the sortable version of the pad name. }
      padNameSortable    := CLF_GetSortablePadName({padName} padDst.Name);
      
      {* Setup some parameters based on package type. *}

      { Handle chip inductors. }
      if (footprintType = 'Inductor') then
      begin

         WriteToDebugFile('Hello from CLF_Create3dExtrudedCompBody().  Setting up for chip component.');

         { "Pin" is the height of the chip component. }
         compBodyPinThicknessMm := pkgDimsHeightMax;

         { Use the standard max values for all these parameters. }
         bodyWidthMm     :=  pkgDimsBodyWidthMax;
         bodyLengthMm    :=  pkgDimsBodyLengthMax;
         totalWidthMm    :=  pkgDimsTotalWidthMax;
         totalLengthMm   :=  pkgDimsTotalLengthMax;
         pinLandMm       :=  (pkgDimsPinLandMax + pkgDimsPinLandMin) * 0.5; { Use nominal termination width. }

         { This parameter is not used for this package type.  Set to 0 to be safe. }
         ZpinEscapeMult  := 0.0;
         
      end

      { Handle molded capacitors }
      { TODO:  We should probably also check chip vs. molded package types. }
      else if (footprintType = 'Capacitor') then
      begin
         
         WriteToDebugFile('Hello from CLF_Create3dExtrudedCompBody().  Setting up for molded component.');

         { "Pin" a fraction of the height of the molded component. }
         compBodyPinThicknessMm := (constCompBodyPinHeightMoldRatio * pkgDimsHeightMax);

         { Use the standard max values for all these parameters. }
         bodyWidthMm     :=  pkgDimsBodyWidthMax;
         bodyLengthMm    :=  pkgDimsBodyLengthMax;
         totalWidthMm    :=  pkgDimsTotalWidthMax;
         totalLengthMm   :=  pkgDimsTotalLengthMax;
         pinLandMm       :=  (pkgDimsPinLandMax + pkgDimsPinLandMin) * 0.5; { Use nominal termination width. }

         { This parameter is not used for this package type.  Set to 0 to be safe. }
         ZpinEscapeMult  := 0.0;
         
      end
      
      { Handle QFN & DFN. }
      else if ( (footprintType = 'QFN') or (footprintType = 'DFN') ) then
      begin

         { For QFN/DFN packages, set predefined pin thickness. }
         compBodyPinThicknessMm := constCompBodyPinThicknessQfnMm;

         { For QFN/DFN packages, we cheat and make the pin extend 0.001 mm beyond
          the body, so that we can actually see it in Altium. }
         bodyWidthMm     :=  (pkgDimsBodyWidthMax + (2*constCompBodyMarkHeight));
         bodyLengthMm    :=  (pkgDimsBodyLengthMax + (2*constCompBodyMarkHeight));
         totalWidthMm    :=  (pkgDimsTotalWidthMax + (2*constCompBodyMarkHeight));
         totalLengthMm   :=  (pkgDimsTotalLengthMax + (2*constCompBodyMarkHeight));
         pinLandMm       :=  (pkgDimsPinLandMax + constCompBodyMarkHeight);

         { This parameter is not used for QFN/DFN.  Set to 0 to be safe. }
         ZpinEscapeMult  := 0.0;
      end

      { Otherwise, setup for SOIC/SOP/QFP/etc. }
      else
      begin
         { For SOIC/SOP/QFP/etc. packages, set predefined pin thickness. }
         compBodyPinThicknessMm := constCompBodyPinThicknessSoicMm;

         { Use the standard max values for all these parameters. }
         bodyWidthMm     :=  pkgDimsBodyWidthMax;
         bodyLengthMm    :=  pkgDimsBodyLengthMax;
         totalWidthMm    :=  pkgDimsTotalWidthMax;
         totalLengthMm   :=  pkgDimsTotalLengthMax;
         pinLandMm       :=  pkgDimsPinLandMax;

         { Choose the multiplier for where in Z the pins escape from the plastic body and come down to land on board. }
         if (footprintType = 'SOT') then
            ZpinEscapeMult := constCompBodyPinEscapeHeightSotMult {nosemi}
         else
            ZpinEscapeMult := constCompBodyPinEscapeHeightSoicMult;
         
      end;
      

      { We only extrude pin models for west, east, north, and south side pins. }
      if ( (padGroupNum = constPadGroupWest) or (padGroupNum = constPadGroupEast) or
          (padGroupNum = constPadGroupNorth) or (padGroupNum = constPadGroupSouth) ) then
      begin

         { Currently we do this for QFN/DFN packages and anything with gullwing pins. }
         if ( (footprintType = 'Inductor') or (footprintType = 'Capacitor') or
             (footprintType = 'QFN') or (footprintType = 'DFN') or
             (footprintType = 'SOIC') or (footprintType = 'SOT') or
             (footprintType = 'SOP') or (footprintType = 'QFP') ) then
         begin

            {* Compute and model the maximal pin shape. *}
            CLF_CalculatePinCoords(cnfGalacticInfo,
                                   padDst,
                                   padGroupNum,
                                   bodyWidthMm,
                                   bodyLengthMm,
                                   totalWidthMm,
                                   totalLengthMm,
                                   {pinWidthMm} pkgDimsPinWidthMax,
                                   pinLandMm,
                                   {var} borderWestMm,
                                   {var} borderEastMm,
                                   {var} borderNorthMm,
                                   {var} borderSouthMm,
                                   {var} omitSide);
            
            { Create 3D rectangular extrusion to model component pin. }
            CLF_Create3dRectangularExtrusion({boardSide} constBoardSideCompBody,
                                             {layer} constLayerCompBody,
                                             {color} constCompBodyBallColor,
                                             {opacity} constCompBodyOpacity,
                                             borderWestMm,
                                             borderEastMm,
                                             borderNorthMm,
                                             borderSouthMm,
                                             {overallHeightMm} compBodyPinThicknessMm,
                                             {standoffHeightMm} 0,
                                             {identifier} 'Component_pin_' + padNameSortable,
                                             {var} bodyQueue,
                                             {var} primNames,
                                             {var} csvReportStrs);

            WriteToDebugFile(' Created 3D extrusion to model ' + CLF_GetPadGroupNamePrefix(constPadGroupEp, primNames) + ' side ' + footprintType + ' component pin "' + padNameSortable + '".');

         end; { endif is QFN/DFN/SOIC/etc.}
         
         { Do these next 2 steps only for packages with gullwing pins. }
         if ( (footprintType = 'SOIC') or (footprintType = 'SOT') or
             (footprintType = 'SOP') or (footprintType = 'QFP') ) then
         begin

            {* Compute and model the vertical part of the gullwing pin. *}

            { Calculate the parameters for the 2nd segment of the gullwing pin,
             the vertical piece that rises from pad landing. }
            bodyWidthMm     := pkgDimsTotalWidthMax - (2*pkgDimsPinLandMax) + (2*compBodyPinThicknessMm);
            bodyLengthMm    := pkgDimsTotalLengthMax - (2*pkgDimsPinLandMax) + (2*compBodyPinThicknessMm);
            totalWidthMm    := bodyWidthMm;
            totalLengthMm   := bodyLengthMm;
            pinLandMm       := compBodyPinThicknessMm;

            { Calculate Z (height) parameters. }
            { The center of mass of the pin should escape at the specified multiplier to the package height. }
            ZstopMm         := ((ZpinEscapeMult * libHeightMm) - (0.5*compBodyPinThicknessMm));
            ZstartMm        := (compBodyPinThicknessMm);
            
            { Calculate the coordinates for this part of each gullwing pin. }
            CLF_CalculatePinCoords(cnfGalacticInfo,
                                   padDst,
                                   padGroupNum,
                                   bodyWidthMm,
                                   bodyLengthMm,
                                   totalWidthMm,
                                   totalLengthMm,
                                   {pinWidthMm} pkgDimsPinWidthMax,
                                   pinLandMm,
                                   {var} borderWestMm,
                                   {var} borderEastMm,
                                   {var} borderNorthMm,
                                   {var} borderSouthMm,
                                   {var} omitSide);
            
            { Create 3D rectangular extrusion to model component pin. }
            CLF_Create3dRectangularExtrusion({boardSide} constBoardSideCompBody,
                                             {layer} constLayerCompBody,
                                             {color} constCompBodyBallColor,
                                             {opacity} constCompBodyOpacity,
                                             borderWestMm,
                                             borderEastMm,
                                             borderNorthMm,
                                             borderSouthMm,
                                             {overallHeightMm} ZstopMm,
                                             {standoffHeightMm} ZstartMm,
                                             {identifier} 'Component_pin_' + padNameSortable + '-B',
                                             {var} bodyQueue,
                                             {var} primNames,
                                             {var} csvReportStrs);

            
            {* Compute and model the final horizontal part of the gullwing pin. *}

            { Calculate the parameters for the final segment of the gullwing pin,
             the horizontal piece that disappears into component body. }
            pinLandMm       := (0.5*(bodyWidthMm - pkgDimsBodyWidthMax));

            { Calculate Z (height) parameters. }
            ZstopMm         := (ZstopMm + compBodyPinThicknessMm);
            ZstartMm        := (ZstopMm - compBodyPinThicknessMm);
            
            { Calculate the coordinates for this part of each gullwing pin. }
            CLF_CalculatePinCoords(cnfGalacticInfo,
                                   padDst,
                                   padGroupNum,
                                   bodyWidthMm,
                                   bodyLengthMm,
                                   totalWidthMm,
                                   totalLengthMm,
                                   {pinWidthMm} pkgDimsPinWidthMax,
                                   pinLandMm,
                                   {var} borderWestMm,
                                   {var} borderEastMm,
                                   {var} borderNorthMm,
                                   {var} borderSouthMm,
                                   {var} omitSide);
            
            { Create 3D rectangular extrusion to model component pin. }
            CLF_Create3dRectangularExtrusion({boardSide} constBoardSideCompBody,
                                             {layer} constLayerCompBody,
                                             {color} constCompBodyBallColor,
                                             {opacity} constCompBodyOpacity,
                                             borderWestMm,
                                             borderEastMm,
                                             borderNorthMm,
                                             borderSouthMm,
                                             {overallHeightMm} ZstopMm,
                                             {standoffHeightMm} ZstartMm,
                                             {identifier} 'Component_pin_' + padNameSortable + '-C',
                                             {var} bodyQueue,
                                             {var} primNames,
                                             {var} csvReportStrs);

         end; { endif is QFN/DFN/SOIC/etc.}
         
      end { endif is west/east/north/south}

      { Handle EP pads. }
      else if (padGroupNum = constPadGroupEp) then
      begin

         { Get effective EP pad x,y bounds after taking rotation into account. }
         CLF_GetEffectivePadXYbounds(padDst,
                                     {var X1Mm} borderWestMm,
                                     {var Y1Mm} borderNorthMm,
                                     {var X2Mm} borderEastMm,
                                     {var Y2Mm} borderSouthMm);

         
         { Create 3D rectangular extrusion to model EP pin. }
         CLF_Create3dRectangularExtrusion({boardSide} constBoardSideCompBody,
                                          {layer} constLayerCompBody,
                                          {color} constCompBodyBallColor,
                                          {opacity} constCompBodyOpacity,
                                          borderWestMm,
                                          borderEastMm,
                                          borderNorthMm,
                                          borderSouthMm,
                                          {overallHeightMm} constCompBodyMarkHeight,
                                          {standoffHeightMm} 0,
                                          {identifier} 'Component_pin_' + padNameSortable,
                                          {var} bodyQueue,
                                          {var} primNames,
                                          {var} csvReportStrs);

         WriteToDebugFile(' Created 3D extrusion to model EP component pin "' + padNameSortable + '".');
         
      end { end elsif}

      { Else see if we have center side pins. }
      else if (padGroupNum = constPadGroupCenter) then
      begin

         {* Create 3D spheres for BGA balls. *}
         { Note:  We don't need any additional conditionals because only BGA parts have "center" pins. }

         { Retrieve pad x,y coordinates. }
         padXmm               := CoordToMMs(padDst.X);
         padYmm               := CoordToMMs(padDst.Y);
         
         //      WriteToDebugFile(' Found center side pad named "' + padDst.Name + '".');
         //      WriteToDebugFile(' padXmm is ' + FloatToStr(padXmm) + '.');
         //      WriteToDebugFile(' padYmm is ' + FloatToStr(padYmm) + '.');

         { Create a sphere to model the BGA ball. }
         CLF_Create3dSphere({boardSide}  constBoardSideCompBody,
                            {layer} constLayerCompBody,
                            {color} constCompBodyBallColor,
                            {opacity} constCompBodyOpacity,
                            {XCenterMm} padXmm,
                            {YCenterMm} padYmm,
                            {radiusMm} (0.5*pkgDimsBallDiamNom),
                            {ZoffsetMm} (0.5*pkgDimsBallDiamNom),
                            {identifier} ('Component_ball_' + padNameSortable),
                            {var} bodyQueue,
                            {var} primNames,
                            {var} csvReportStrs);
         
         WriteToDebugFile(' Created 3D sphere for component ball "' + padNameSortable + '".');

      end { end elsif}

   end; { endfor i }

   CLF_WriteToSummaryAndDebugFilesWithStepNum('Created simple extruded 3D body for new .PcbLib file.');

end; { end CLF_Create3dExtrudedCompBody() }


{***************************************************************************
 * function CLF_CreateNewFeatures3dCompbody()
 *  Create all new 3D features on the CompBody layer.
 *  This will typically be a pre-created STEP model or an extruded 3D model.
 *  It may also include 3D text.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewFeatures3dCompbody(    scriptsPath      : TDynamicString;
                                             projectPath      : TDynamicString;
                                             libHeightMm      : Real;
                                             stepFilePath     : TString;
                                             allow3dExtrusion : Boolean; 
                                             doBaseFp         : Boolean;
                                             currDerivedFpNum : Integer;
                                         var cnfGalacticInfo  : TStringList;
                                         var trackQueue       : TInterfaceList;
                                         var arcQueue         : TInterfaceList;
                                         var textQueue        : TInterfaceList;
                                         var padQueue         : TInterfaceList;
                                         var regionQueue      : TInterfaceList;
                                         var fillQueue        : TInterfaceList;
                                         var bodyQueue        : TInterfaceList;
                                         var primNames        : TStringList;
                                         var csvReportStrs    : TStringList;                                
                                             )                : Integer;
var
   i                    : Integer;
   rc                   : Integer;
   expectedStepFileName : TString;
   pkgDimsStandoffMin   : Real;
   pkgDimsHeightMax     : Real;
   Xrot                 : Integer;
   Yrot                 : Integer;
   Zrot                 : Integer;
   ZoffsetMm            : Real;
   text                 : Tstring;
   rotation             : Integer;
   X1mm                 : Real;
   X2mm                 : Real;
   Y1mm                 : Real;
   Y2mm                 : Real;
   fontName             : TString;
   bold                 : Boolean;
   italic               : Boolean;
   colorText            : Integer;
   colorBackground      : Integer;
   textDst              : IPCB_Text;
   imageFilePath        : TString;
   image                : TImage;
   regionDst            : IPCB_Region;
   regionOutlinePoints  : IPCB_Contour;
   doNegateImage        : Boolean;
   doCenterInX          : Boolean;
   doCenterInY          : Boolean;
   colorImage           : Integer;
   failed               : Integer;
   
begin

   { Assume success. }
   result := 0;

   { Assume we will be able to find or generate a STEP model. }
   failed := 0;

   WriteToDebugFile('Hello from CLF_CreateNewFeatures3dCompbody().');

   WriteToDebugFile('expectedStepFileName: ' + expectedStepFileName);
   
   {** Attempt to find and add auto-generated STEP model to component. **}
   { TODO:  Should user-specified STEP file take precedence over this??? }
   rc := CLF_FindAndAddStepModel(scriptsPath,
                                 projectPath,
                                 cnfGalacticInfo,
                                 {ModelsDir} constSpi3dModelsMeDir,
                                 {var} expectedStepFileName,
                                 {var} bodyQueue,
                                 {var} primNames,
                                 {var} csvReportStrs
                                 );

   { See if that failed. }
   if (rc <> 0) then
   begin

      WriteToDebugFile(' CLF_FindStepModel() failed.  Proceeding to try to find user-specified STEP file.');

      { If this field is non-null, then proceed to use this specified STEP file. }
      if (stepFilePath <> '') then
      begin

         { Retrieve other info from galactic string list. }
         Xrot               := StrToInt(cnfGalacticInfo.Values(constGilStepXRot));
         Yrot               := StrToInt(cnfGalacticInfo.Values(constGilStepYRot));
         Zrot               := StrToInt(cnfGalacticInfo.Values(constGilStepZRot));
         ZoffsetMm          := StrToFloat(cnfGalacticInfo.Values(constGilStepZOff));

         { Add the STEP model to queue. }
         CLF_AddStepModel({boardSide} constBoardSideCompBody,
                          {layer} constLayerCompBody,
                          {opacity} constCompBodyOpacity,
                          {stepFileName} stepFilePath, 
                          Xrot,
                          Yrot,
                          Zrot,
                          ZoffsetMm,
                          {var} bodyQueue,
                          cnfGalacticInfo,
                          {var} primNames,
                          {var} csvReportStrs);
         CLF_WriteToSummaryAndDebugFilesWithStepNum('Added user-specified STEP model "' + stepFilePath + '" to new .PcbLib file.');

         { Call CLF_Create3dThermalBody() to additionally create a simplified 3D extrusion
          that can be used for thermal modeling purposes. }
         CLF_Create3dThermalBody(scriptsPath,
                                 projectPath,
                                 {var} cnfGalacticInfo,
                                 {var} bodyQueue,
                                 {var} primNames,
                                 {var} csvReportStrs);
         
      end { endif }

      { Else create a 3D body. }
      else 
      begin

         { Try to create a FreeCAD model for this footprint. }
         failed: = CLF_Create3dFreeCadCompBody(scriptsPath,
                                               projectPath,
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

         { See if we succeeded in creating a FreeCAD model. }
         if ( failed = 0 ) then
         begin

            { Call CLF_Create3dThermalBody() to additionally create a simplified 3D extrusion
             that can be used for thermal modeling purposes. }
            CLF_Create3dThermalBody(scriptsPath,
                                    projectPath,
                                    {var} cnfGalacticInfo,
                                    {var} bodyQueue,
                                    {var} primNames,
                                    {var} csvReportStrs);
                        
            CLF_WriteToSummaryAndDebugFilesWithStepNum('Created FreeCAD 3D body for new .PcbLib file.');
         end

         { Else we failed to generate a STEP model using FreeCAD.  Create an extruded model. }
         else
         begin 
            { If we're not allowed to extrude a simple 3D body (eg. because we don't have code
             to support this type of footprint), then abort now. }
            if (not allow3dExtrusion) then
               CLF_Abort('Script was flagged to not allow attempt to extrude a simple 3D body for this footprint.  You must specify a STEP file name in .xml file!');
            
            { If this is the base footprint, issue warning modal dialog box with specified warning message,
             specified reply after clicking Ok, and specified reply after clicking Cancel. }
            if (doBaseFp) then
            begin
               IssueWarningWithOkOrCancel('Warning:  Could not find STEP model with filename starting with "' + expectedStepFileName + '"!!' + constLineBreak +
                                          constLineBreak +
                                          'Shall I extrude a simple black 3D body to crudely represent the body and pins of the component?' + constLineBreak +
                                          'Click Ok to do this.  Otherwise, hit Cancel to Abort the script so you can find the missing STEP model.',
                                          'Ok.  Proceeding to extrude crude 3D body and pins.',
                                          '');
            end; { endif }         

            {** Add 3D extrusion for component body. **}
            CLF_Create3dExtrudedCompBody({var} cnfGalacticInfo,
                                         {var} trackQueue,
                                         {var} arcQueue,
                                         {var} textQueue,
                                         {var} padQueue,
                                         {var} regionQueue,
                                         {var} fillQueue,
                                         {var} bodyQueue,
                                         {var} primNames,
                                         {var} csvReportStrs);
            CLF_WriteToSummaryAndDebugFilesWithStepNum('Created simple extruded 3D body for new .PcbLib file.');

         end; { endelse failed to create FreeCAD model }

      end; { endelse create simple 3D body }

   end; { endif find auto-generated STEP model failed }

   
   { See if we have been ordered to create 3D text for this derived footprint. }
   WriteToDebugFile(' doBaseFp is ' + BoolToStr(doBaseFp) + ', currDerivedFpNum is ' + IntToStr(currDerivedFpNum) + '.');
   if ( (not doBaseFp) and
       (CLF_IsNameInStringList({name} constGil3dTextText + IntToStr(currDerivedFpNum),
       {stringlist} cnfGalacticInfo)) ) then
   begin
      
      WriteToDebugFile(' About to create 3D text!');

      { Retrieve 3D text parameters for this derived footprint. }
      text                     := cnfGalacticInfo.Values(constGil3dTextText + IntToStr(currDerivedFpNum));
      rotation                 := StrToInt(cnfGalacticInfo.Values(constGil3dTextRotation + IntToStr(currDerivedFpNum)));
      X1mm                     := StrToFloat(cnfGalacticInfo.Values(constGil3dTextX1 + IntToStr(currDerivedFpNum)));
      Y1mm                     := StrToFloat(cnfGalacticInfo.Values(constGil3dTextY1 + IntToStr(currDerivedFpNum)));
      X2mm                     := StrToFloat(cnfGalacticInfo.Values(constGil3dTextX2 + IntToStr(currDerivedFpNum)));
      Y2mm                     := StrToFloat(cnfGalacticInfo.Values(constGil3dTextY2 + IntToStr(currDerivedFpNum)));
      fontName                 := cnfGalacticInfo.Values(constGil3dTextFontName + IntToStr(currDerivedFpNum));

      WriteToDebugFile(' X1mm Is ' + FloatToStr(X1mm) + '.');
      WriteToDebugFile(' Y1mm Is ' + FloatToStr(Y1mm) + '.');

      WriteToDebugFile(' X2mm Is ' + FloatToStr(X2mm) + '.');
      WriteToDebugFile(' Y2mm Is ' + FloatToStr(Y2mm) + '.');

      
      { If we see the Excel boolean value "True" (aka "1") then flag it as true.  Otherwise, false. }
      bold                     := (cnfGalacticInfo.Values(constGil3dTextBold + IntToStr(currDerivedFpNum)) = '1');
      italic                   := (cnfGalacticInfo.Values(constGil3dTextItalic + IntToStr(currDerivedFpNum)) = '1');
      doCenterInX              := (cnfGalacticInfo.Values(constGil3dTextCenterInX + IntToStr(currDerivedFpNum)) = '1');
      doCenterInY              := (cnfGalacticInfo.Values(constGil3dTextCenterInY + IntToStr(currDerivedFpNum)) = '1');

      { FIXME:  Remove this! }
//      doCenterInX         := False;
//      doCenterInY         := False;

      { If we see the string "Default" then use our default color.  Otherwise, the color integer specified. }
      if (AnsiUpperCase(cnfGalacticInfo.Values(constGil3dTextColorText + IntToStr(currDerivedFpNum))) = 'DEFAULT') then
         colorText             := constCompBodyPin1MarkColor {nosemi}
      else
         colorText             := StrToInt(cnfGalacticInfo.Values(constGil3dTextColorText + IntToStr(currDerivedFpNum)));
         
      { If we see the string "Default" then use our default color.  Otherwise, the color integer specified. }
      if (AnsiUpperCase(cnfGalacticInfo.Values(constGil3dTextColorBackground + IntToStr(currDerivedFpNum))) = 'DEFAULT') then
         colorBackground       := constCompBodyColor {nosemi}
      else
         colorBackground       := StrToInt(cnfGalacticInfo.Values(constGil3dTextColorBackground + IntToStr(currDerivedFpNum)));
         

      { Create the 2D true type text object with the specified bounds. }
      CLF_CreateNewTrueTypeTextByBounds({layer} constLayerCompBody,
                                        X1Mm,
                                        Y1Mm,
                                        X2Mm,
                                        Y2Mm,
                                        {angle} rotation,
                                        text,
                                        fontName,
                                        bold,
                                        italic,
                                        doCenterInX,
                                        doCenterInY,
                                        {var} textDst,
                                        {var} textQueue,
                                        {var} trackQueue,
                                        {name} constMagicalExclude3dTextName,
                                        {identifierRectangle} 'Component_mfg_marking_code',
                                        {var} primNames);
      
      
      { Create the 3D text object(s), extruded up from the 2D true type text object. }
      CLF_Create3dText({boardSide}  constBoardSideCompBody,
                       {layer} constLayerCompBody,
                       colorText,
                       colorBackground,
                       {opacity} constCompBodyOpacity,
                       {overallHeightMm} (libHeightMm + constCompBodyMarkHeight),
                       {standoffHeightMm} libHeightMm,
                       textDst,
                       {var} bodyQueue,
                       {identifier} 'Component_mfg_marking_code',
                       {var} primNames,
                       {var} csvReportStrs);
   end;


   { See if we have been ordered to create 3D image for this derived footprint. }
   if ( (not doBaseFp) and
       (CLF_IsNameInStringList({name} constGil3dImageImageFilePath + IntToStr(currDerivedFpNum),
       {stringlist} cnfGalacticInfo)) ) then
   begin

      WriteToDebugFile(' About to create 3D regions for company logo!');

      { Retrieve 3D text parameters for this derived footprint. }
      imageFilePath            := cnfGalacticInfo.Values(constGil3dImageImageFilePath + IntToStr(currDerivedFpNum));
      rotation                 := StrToInt(cnfGalacticInfo.Values(constGil3dImageRotation + IntToStr(currDerivedFpNum)));
      X1mm                     := StrToFloat(cnfGalacticInfo.Values(constGil3dImageX1 + IntToStr(currDerivedFpNum)));
      Y1mm                     := StrToFloat(cnfGalacticInfo.Values(constGil3dImageY1 + IntToStr(currDerivedFpNum)));
      X2mm                     := StrToFloat(cnfGalacticInfo.Values(constGil3dImageX2 + IntToStr(currDerivedFpNum)));
      Y2mm                     := StrToFloat(cnfGalacticInfo.Values(constGil3dImageY2 + IntToStr(currDerivedFpNum)));

      { If we see the Excel boolean value "True" (aka "1") then flag it as true.  Otherwise, false. }
      doNegateImage            := (cnfGalacticInfo.Values(constGil3dImageNegative + IntToStr(currDerivedFpNum)) = '1');
      doCenterInX              := (cnfGalacticInfo.Values(constGil3dImageCenterInX + IntToStr(currDerivedFpNum)) = '1');
      doCenterInY              := (cnfGalacticInfo.Values(constGil3dImageCenterInY + IntToStr(currDerivedFpNum)) = '1');

      { If we see the string "Default" then use our default color.  Otherwise, the color integer specified. }
      if (AnsiUpperCase(cnfGalacticInfo.Values(constGil3dImageColorImage + IntToStr(currDerivedFpNum))) = 'DEFAULT') then
         colorImage             := constCompBodyPin1MarkColor {nosemi}
      else
         colorImage             := StrToInt(cnfGalacticInfo.Values(constGil3dImageColorImage + IntToStr(currDerivedFpNum)));
         
      { If we see the string "Default" then use our default color.  Otherwise, the color integer specified. }
      if (AnsiUpperCase(cnfGalacticInfo.Values(constGil3dImageColorBackground + IntToStr(currDerivedFpNum))) = 'DEFAULT') then
         colorBackground       := constCompBodyColor {nosemi}
      else
         colorBackground       := StrToInt(cnfGalacticInfo.Values(constGil3dImageColorBackground + IntToStr(currDerivedFpNum)));

      { Call CLF_Create3dRegionsFromImage() to create 2D regions and extrude them to 3D. }
      CLF_Create3dRegionsFromImage({boardSide}  constBoardSideCompBody,
                                   {layer} constLayerCompBody,
                                   X1mm,
                                   Y1mm,
                                   X2mm,
                                   Y2mm,
                                   rotation,
                                   imageFilePath,
                                   colorImage,
                                   colorBackground,
                                   {opacity} constCompBodyOpacity,
                                   {overallHeightMm} (libHeightMm + constCompBodyMarkHeight),
                                   {standoffHeightMm} libHeightMm,
                                   doNegateImage,
                                   doCenterInX,
                                   doCenterInY,
                                   {identifier} 'Component_mfg_logo',
                                   {var} trackQueue,
                                   {var} regionQueue,
                                   {var} bodyQueue,
                                   cnfGalacticInfo,
                                   {var} primNames,
                                   {var} csvReportStrs);

   end;
   
end; { end CLF_CreateNewFeatures3dCompbody() }


{***************************************************************************
 * function CLF_CreateNewFeaturesPinLand()
 *  Create all new 2D features on the PinLand layer.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewFeaturesPinLand(var cnfGalacticInfo : TStringList;
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
   i                     : Integer;
   pkgDimsPins           : Integer;
   pkgDimsPitch          : Real;
   pkgDimsStandoffMin    : Real;
   pkgDimsHeightMax      : Real;
   pkgDimsEpWidthMax     : Real;
   pkgDimsEpLengthMax    : Real;
   pkgDimsTotalWidthMin  : Real;
   pkgDimsTotalWidthMax  : Real;
   pkgDimsTotalLengthMin : Real;
   pkgDimsTotalLengthMax : Real;
   pkgDimsBodyWidthMin   : Real;
   pkgDimsBodyWidthMax   : Real;
   pkgDimsBodyLengthMin  : Real;
   pkgDimsBodyLengthMax  : Real;
   pkgDimsPinWidthMin    : Real;
   pkgDimsPinWidthMax    : Real;
   pkgDimsPinLandMin     : Real;
   pkgDimsPinLandMax     : Real;
   pkgDimsBallDiamNom    : Real;
   padDst                : IPCB_Pad;
   padXmm                : Real;
   padYmm                : Real;
   pinLandBeyondAssy     : Real;
   pinLandWestMm         : Real;
   pinLandEastMm         : Real;
   pinLandNorthMm        : Real;
   pinLandSouthMm        : Real;
   footprintType         : TString;
   padGroupNum           : Integer;
   omitSide              : TString;
   padNameSortable       : TString;

begin

   { Assume success. }
   result := 0;

   { Retrieve other info from galactic string list. }
   footprintType      := cnfGalacticInfo.Values(constGilFootprintType);

   { Retrieve package dimension fields from galactic string list. }
   pkgDimsPins          := StrToInt(cnfGalacticInfo.Values(constGilPkgDimsPins));
   pkgDimsPitch         := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsPitch));
   pkgDimsStandoffMin   := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsStandoffMin));
   pkgDimsHeightMax     := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsHeightMax));
   pkgDimsEpWidthMax    := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsEpWidthMax));
   pkgDimsEpLengthMax   := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsEpLengthMax));
   pkgDimsTotalWidthMin := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsTotalWidthMin));
   pkgDimsTotalWidthMax := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsTotalWidthMax));
   pkgDimsTotalLengthMin:= StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsTotalLengthMin));
   pkgDimsTotalLengthMax:= StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsTotalLengthMax));
   pkgDimsBodyWidthMin  := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsBodyWidthMin));
   pkgDimsBodyWidthMax  := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsBodyWidthMax));
   pkgDimsBodyLengthMin := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsBodyLengthMin));
   pkgDimsBodyLengthMax := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsBodyLengthMax));
   pkgDimsPinWidthMin   := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsPinWidthMin));
   pkgDimsPinWidthMax   := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsPinWidthMax));
   pkgDimsPinLandMin    := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsPinLandMin));
   pkgDimsPinLandMax    := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsPinLandMax));
   pkgDimsBallDiamNom   := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsBallDiamNom));

   WriteToDebugFile('Hello from CLF_CreateNewFeaturesPinLand().');

   { Loop over all the pads in the queue. }
   for i := 0 to (padQueue.Count - 1) do
   begin

      { Retrieve reference to queued pad. }
      padDst := padQueue.items[i];

      { Retrieve group number of queued pad. }
      padGroupNum := CLF_GetPadGroupNum(padDst);
      
      { Get the sortable version of the pad name. }
      padNameSortable    := CLF_GetSortablePadName({padName} padDst.Name);
      
      { We only draw pin landing rectangles on west, east, north, and south side pins. }
      if ( (padGroupNum = constPadGroupWest) or (padGroupNum = constPadGroupEast) or
          (padGroupNum = constPadGroupNorth) or (padGroupNum = constPadGroupSouth) ) then
      begin

         {* Compute and draw the maximal pin landing rectangle. *}
         CLF_CalculatePinCoords(cnfGalacticInfo,
                                padDst,
                                padGroupNum,
                                {bodyWidthMm} pkgDimsBodyWidthMax,
                                {bodyLengthMm} pkgDimsBodyLengthMax,
                                {totalWidthMm} pkgDimsTotalWidthMax,
                                {totalLengthMm} pkgDimsTotalLengthMax,
                                {pinWidthMm} pkgDimsPinWidthMax,
                                {pinLandMm} pkgDimsPinLandMax,
                                {var boundaryWestMm} pinLandWestMm,
                                {var boundaryEastMm} pinLandEastMm,
                                {var boundaryNorthMm} pinLandNorthMm,
                                {var boundarySouthMm} pinLandSouthMm,
                                {var} omitSide);
         
         { Create a rectangle with the specified boundary. }
         CLF_CreateNewTrackRectangle2({layer} constLayerPinLand,
                                      {widthMm} constWidthPinLand,
                                      {boundaryWestMm} pinLandWestMm,
                                      {boundaryEastMm} pinLandEastMm,
                                      {boundaryNorthMm} pinLandNorthMm,
                                      {boundarySouthMm} pinLandSouthMm,
                                      {var} trackQueue,
                                      {namePrefix} 'Pin_land_maximal_' + padNameSortable,
                                      {var} primNames);

         {* Compute and draw the minimal pin landing rectangle. *}
         CLF_CalculatePinCoords(cnfGalacticInfo,
                                padDst,
                                padGroupNum,
                                {bodyWidthMm} pkgDimsBodyWidthMin,
                                {bodyLengthMm} pkgDimsBodyLengthMin,
                                {totalWidthMm} pkgDimsTotalWidthMin,
                                {totalLengthMm} pkgDimsTotalLengthMin,
                                {pinWidthMm} pkgDimsPinWidthMin,
                                {pinLandMm} pkgDimsPinLandMax,   { This one stays at max! }
                                {var boundaryWestMm} pinLandWestMm,
                                {var boundaryEastMm} pinLandEastMm,
                                {var boundaryNorthMm} pinLandNorthMm,
                                {var boundarySouthMm} pinLandSouthMm,
                                {var} omitSide);
         
         { Create a rectangle with the specified boundary. }
         CLF_CreateNewTrackRectangle2({layer} constLayerPinLand,
                                      {widthMm} constWidthPinLand,
                                      {boundaryWestMm} pinLandWestMm,
                                      {boundaryEastMm} pinLandEastMm,
                                      {boundaryNorthMm} pinLandNorthMm,
                                      {boundarySouthMm} pinLandSouthMm,
                                      {var} trackQueue,
                                      {namePrefix} 'Pin_land_minimal_' + padNameSortable,
                                      {var} primNames);

      end { endif }

      { Else see if we have center side pins. }
      else if (padGroupNum = constPadGroupCenter) then
      begin

         { Retrieve pad x,y coordinates. }
         padXmm               := CoordToMMs(padDst.X);
         padYmm               := CoordToMMs(padDst.Y);
         
         WriteToDebugFile(' Found center side pad named "' + padNameSortable + '".');
         WriteToDebugFile(' padXmm is ' + FloatToStr(padXmm) + '.');
         WriteToDebugFile(' padYmm is ' + FloatToStr(padYmm) + '.');

         { Create a new arc to represent the BGA ball. }
         CLF_CreateNewArc({layer} constLayerPinLand,
                          {XCenterMm} padXmm,
                          {YCenterMm} padYmm,
                          {RadiusMm} (0.5*pkgDimsBallDiamNom),
                          {LineWidthMm} constWidthPinLand,
                          {StartAngleDeg} 0,
                          {EndAngleDeg} 360,
                          {isKeepout} False,
                          {var} arcQueue,
                          {name} 'Pin_land_nominal_' + padNameSortable,
                          {var} primNames);

      end { end elsif }
      
      { Else see if we have ep side pins. }
      else if (padGroupNum = constPadGroupEp) then
      begin

         {* Compute and draw the maximal pin landing rectangle. *}

         { If this EP pad is at 0,0 then we may compute its bounds
          based on package dimensions only. }
         if ( (padDst.X = 0.0) and (padDst.Y = 0.0) ) then
         begin
            
            { Retrieve pad x,y coordinates. }
            padXmm               := CoordToMMs(padDst.X);
            padYmm               := CoordToMMs(padDst.Y);
            
            WriteToDebugFile(' Found EP pad named "' + padNameSortable + '".');
            WriteToDebugFile(' padXmm is ' + FloatToStr(padXmm) + '.');
            WriteToDebugFile(' padYmm is ' + FloatToStr(padYmm) + '.');

            { Compute maximum bounds of EP "pin". }
            pinLandWestMm        := (padXmm - (0.5*pkgDimsEpWidthMax));
            pinLandEastMm        := (padXmm + (0.5*pkgDimsEpWidthMax));
            pinLandNorthMm       := (padYmm - (0.5*pkgDimsEpLengthMax));
            pinLandSouthMm       := (padYmm + (0.5*pkgDimsEpLengthMax));
         end

         { Otherwise, we need to look up actual EP pad location and x,y sizes from padDst. }
         else
         begin

            { Get effective EP pad x,y bounds after taking rotation into account. }
            CLF_GetEffectivePadXYbounds(padDst,
                                        {var X1Mm} pinLandWestMm,
                                        {var Y1Mm} pinLandNorthMm,
                                        {var X2Mm} pinLandEastMm,
                                        {var Y2Mm} pinLandSouthMm);

         end; { endelse }
         
         { Create 4 tracks. }
         CLF_CreateNewTrackRectangle2({layer} constLayerPinLand,
                                      {widthMm} constWidthPinLand,
                                      {boundaryWestMm} pinLandWestMm,
                                      {boundaryEastMm} pinLandEastMm,
                                      {boundaryNorthMm} pinLandNorthMm,
                                      {boundarySouthMm} pinLandSouthMm,
                                      {var} trackQueue,
                                      {namePrefix} 'Pin_land_maximal_' + padNameSortable,
                                      {var} primNames);

      end { end elsif }            

   end; { endfor i }

   CLF_WriteToSummaryAndDebugFilesWithStepNum('Created pin landing drawings on pinland layer for new .PcbLib file.');
end; { end CLF_CreateNewFeaturesPinLand() }


{***************************************************************************
 * function CLF_CreateNewFeaturesCourtyard()
 *  Create all new 2D features on the Courtyard layer.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewFeaturesCourtyard(var cnfGalacticInfo : TStringList;
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
   i          : Integer;

begin
   
   { Update names of tracks that form existing courtyard outline. }
   CLF_UpdateTrackNamesFromBoundingRectangle({layer} constNewLayerCourtyard,
                                             {namePrefix} 'Courtyard',
                                             {var} cnfGalacticInfo,
                                             {var} trackQueue,
                                             {var} primNames);

end; { end CLF_CreateNewFeaturesCourtyard() }


{***************************************************************************
 * function CLF_CreateNewFeaturesCourtbody()
 *  Create all new 2D features on the CourtBody layer.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewFeaturesCourtbody(var cnfGalacticInfo : TStringList;
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
   rc                  : Integer;
   courtyardWestMm     : Real;
   courtyardEastMm     : Real;
   courtyardNorthMm    : Real;
   courtyardSouthMm    : Real;
   namePrefix          : TString;
   padGroupNum         : Integer;
   boundaryWestMm      : Real;
   boundaryEastMm      : Real;
   boundaryNorthMm     : Real;
   boundarySouthMm     : Real;
   haveSoldermaskFills : Boolean;
                    
begin
   
   { Retrieve the bounds for the courtyard outline. }
   CLF_RetrieveBoundingRectangleByNamePrefix({namePrefix} 'Courtyard',
                                             {var} cnfGalacticInfo,
                                             {var boundaryWestMm} courtyardWestMm,
                                             {var boundaryEastMm} courtyardEastMm,
                                             {var boundaryNorthMm} courtyardNorthMm,
                                             {var boundarySouthMm} courtyardSouthMm);
   
   { Secretly store this information in the footprint for later use by PCB helper scripts. }
   CLF_CreateNewTextFpInfo({name} constClfSecretCourtyardOutline,
                           {value} FloatToStr(courtyardWestMm) + '_' + FloatToStr(courtyardEastMm) + '_' + FloatToStr(courtyardNorthMm) + '_' + FloatToStr(courtyardSouthMm),
                           {var} textQueue,
                           {var} primNames);

   {* Create 4 diagonal lines between courtyard outline and what will become the courtbody 3D region. *}
   CLF_CreateNewDiagonalTrackBorders({layer} constLayerCourtBody,
                                     {widthMm} constWidthCourtBodyBorderMm,
                                     {outerBoundWestMm} courtyardWestMm,
                                     {outerBoundEastMm} courtyardEastMm,
                                     {outerBoundNorthMm} courtyardNorthMm,
                                     {outerBoundSouthMm} courtyardSouthMm,
                                     {deltaXwestMm} constDeltaXyCourtBodyBorderMm,
                                     {deltaXeastMm} constDeltaXyCourtBodyBorderMm,
                                     {deltaYmm} constDeltaXyCourtBodyBorderMm,
                                     {var} trackQueue,
                                     {namePrefix} 'Courtbody_border',
                                     {var} primNames);

   CLF_WriteToSummaryAndDebugFilesWithStepNum('Created track borders on courtyard body layer for new .PcbLib file.');


   {* Create a fill on courtbody mech layer for all relevant pad groups that exist in this footprint. *}
   { Later on, a layout-level script will switch these to soldermask top as needed to suppress
    soldermask sliver DRCs for this footprint.  But we don't want to bake this all the way into the
    footprint, since we don't know in advance what soldermask expansion value will be used, and thus
    how twitchy we should be about when we should draw these fills.  So we're going to create them all now,
    but on a mech layer, and leave it for a later script to turn them on (move them to soldermask layer)
    as needed. }

   { Flag that we have not yet created any of these. }
   haveSoldermaskFills := False;
   
   { Create bounding rectangles for all relevant pad groups. }
   for padGroupNum := constPadGroupWest to constPadGroupEp do
   begin

      { Don't do this for center group (BGA) pads.  The last thing
       we want to do is to make it easier to create soldermask apertures
       under a BGA component! }
      if (padGroupNum <> constPadGroupCenter) then
      begin
         
         { Retrieve name prefix for the current pad group. }
         namePrefix        := CLF_GetPadGroupNamePrefix(padGroupNum, primNames);

         { Retrieve the boundaries of this pad group (if it exists). }
         rc := CLF_RetrieveBoundingRectangleByNamePrefixNoAbort(namePrefix,
                                                                {var} cnfGalacticInfo,
                                                                {var} boundaryWestMm,
                                                                {var} boundaryEastMm,
                                                                {var} boundaryNorthMm,
                                                                {var} boundarySouthMm);

         { See if we succeeded. }
         if (rc = 0) then
         begin
            WriteToDebugFile('In CLF_CreateNewFeaturesCourtbody(), about to create mech layer fill for pad group "' + namePrefix + '".');

            { Create new fill on courtbody mech layer. }
            CLF_CreateNewFill({layer} constLayerCourtBody,
                              boundaryWestMm,
                              boundaryEastMm,
                              boundaryNorthMm,
                              boundarySouthMm,
                              {var} fillQueue,
                              {name} ('Courtbody_fill_for_soldermask_slivers_' + namePrefix),
                              {var} primNames);

            { Secretly store this information in the footprint for later use by PCB helper scripts. }
            CLF_CreateNewTextFpInfo({name} constClfSecretFillSoldermaskSliver + namePrefix,
                                    {value} FloatToStr(boundaryWestMm) + '_' + FloatToStr(boundaryEastMm) + '_' + FloatToStr(boundaryNorthMm) + '_' + FloatToStr(boundarySouthMm),
                                    {var} textQueue,
                                    {var} primNames);

            { Flag that we have created at least one of these. }
            haveSoldermaskFills := True;
            
         end
         
         { Else report that we failed, hopefully just due to the fact that there were no pads in said pad group. }
         else
         begin
            WriteToDebugFile('In CLF_CreateNewFeaturesCourtbody(), did not find any pads belonging to pad group "' + namePrefix + '".');
            
         end; { endelse }
         
      end; { endif }
      
   end; { endfor }

   { Report this status, but only if we actually created any of these fills. }
   if (haveSoldermaskFills) then
      CLF_WriteToSummaryAndDebugFilesWithStepNum('Created fills to solve future soldermask sliver problems for new .PcbLib file.');
end; { end CLF_CreateNewFeaturesCourtbody() }


{***************************************************************************
 * function CLF_CreateNewFeatures3dCourtbody()
 *  Create all new 3D features on the CourtBody layer.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewFeatures3dCourtbody(var cnfGalacticInfo : TStringList;
                                        var trackQueue      : TInterfaceList;
                                        var arcQueue        : TInterfaceList;
                                        var textQueue       : TInterfaceList;
                                        var padQueue        : TInterfaceList;
                                        var regionQueue     : TInterfaceList;
                                        var fillQueue       : TInterfaceList;
                                        var bodyQueue       : TInterfaceList;
                                        var primNames       : TStringList;
                                        var csvReportStrs   : TStringList;                               
                                            )               : Integer;
var
   rc               : Integer;
   courtyardWestMm  : Real;
   courtyardEastMm  : Real;
   courtyardNorthMm : Real;
   courtyardSouthMm : Real;
   courtbodyWestMm  : Real;
   courtbodyEastMm  : Real;
   courtbodyNorthMm : Real;
   courtbodySouthMm : Real;
                    
begin
   
   { Retrieve the bounds for the courtyard outline. }
   CLF_RetrieveBoundingRectangleByNamePrefix({namePrefix} 'Courtyard',
                                             {var} cnfGalacticInfo,
                                             {var boundaryWestMm} courtyardWestMm,
                                             {var boundaryEastMm} courtyardEastMm,
                                             {var boundaryNorthMm} courtyardNorthMm,
                                             {var boundarySouthMm} courtyardSouthMm);
   
   { Compute the borders of the courtbody extrusion. }
   courtbodyWestMm := (courtyardWestMm + constDeltaXyCourtBodyBorderMm);
   courtbodyEastMm := (courtyardEastMm - constDeltaXyCourtBodyBorderMm);
   courtbodyNorthMm := (courtyardNorthMm - constDeltaXyCourtBodyBorderMm);
   courtbodySouthMm := (courtyardSouthMm + constDeltaXyCourtBodyBorderMm);

   {** Add 3D extrusion for courtyard body. **}
   CLF_Create3dRectangularExtrusion({boardSide} constBoardSideCourtBody,
                                    {layer} constLayerCourtBody,
                                    {color} constCourtBodyColor,
                                    {opacity} constCourtBodyOpacity,
                                    {borderWestMm} courtbodyWestMm,
                                    {borderEastMm} courtbodyEastMm,
                                    {borderNorthMm} courtbodyNorthMm,
                                    {borderSouthMm} courtbodySouthMm,
                                    {overallHeightMm} constOverallHeightCourtBodyMm,
                                    {standoffHeightMm} constStandoffHeightCourtBodyMm,
                                    {identifier} constNameCourtBody,
                                    {var} bodyQueue,
                                    {var} primNames,
                                    {var} csvReportStrs);

   { Secretly store this information in the footprint for later use by PCB helper scripts. }
   CLF_CreateNewTextFpInfo({name} constClfSecretCourtbodyOutline,
                           {value} FloatToStr(courtbodyWestMm) + '_' + FloatToStr(courtbodyEastMm) + '_' + FloatToStr(courtbodyNorthMm) + '_' + FloatToStr(courtbodySouthMm),
                           {var} textQueue,
                           {var} primNames);

   CLF_WriteToSummaryAndDebugFilesWithStepNum('Created 3D extrusion on courtyard body layer for new .PcbLib file.');

end; { end CLF_CreateNewFeatures3dCourtbody() }


{***************************************************************************
 * function CLF_CreateNewFeatures()
 *  Create all new 2D features that we want to have in the destination footprint.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewFeatures(    scriptsPath     : TDynamicString;
                                   projectPath     : TDynamicString;
                               var cnfGalacticInfo : TStringList;
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
   i                    : Integer;
   footprintType         : TString;
   allowNumCommentChars : Integer;
   
begin

   { Assume success. }
   result := 0;
     
   { Retrieve footprint type. }
   footprintType         := cnfGalacticInfo.Values(constGilFootprintType);

   
   { Handle chip components differently (use code from CALF script. }
   if ( (footprintType = 'Inductor') or (footprintType = 'Capacitor') ) then
   begin

      if (footprintType = 'Inductor') then
         allowNumCommentChars := 13   { eg. "BLM21BD222SN1" }
      else if (footprintType = 'Capacitor') then
         allowNumCommentChars := 5;   { eg. "100nF" }

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

   end

   { Else handle all IC type package. }
   else
   begin
      
      {* Create all new features on silkscreen layer. *}
      CLF_CreateNewFeaturesSilkscreen({var} cnfGalacticInfo,
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
      CLF_CreateNewFeaturesAssembly({var} cnfGalacticInfo,
                                    {var} trackQueue,
                                    {var} arcQueue,
                                    {var} textQueue,
                                    {var} padQueue,
                                    {var} regionQueue,
                                    {var} fillQueue,
                                    {var} bodyQueue,
                                    {var} primNames
                                    );

   end;

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

end; { end CLF_CreateNewFeatures() }


{***************************************************************************
 * function CLF_CreateNewFeatures3d()
 *  Create all new 3D features that we want to have in the destination footprint.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewFeatures3d(    scriptsPath       : TDynamicString;
                                     projectPath       : TDynamicString;
                                     libHeightMm       : Real;
                                     stepFilePath      : TString;                                         
                                     allow3dExtrusion  : Boolean;
                                     doBaseFp          : Boolean;
                                     currDerivedFpNum  : Integer;
                                     trackQueue2dCount : Integer;
                                     textQueue2dCount  : Integer;
                                 var cnfGalacticInfo   : TStringList;
                                 var trackQueue        : TInterfaceList;
                                 var arcQueue          : TInterfaceList;
                                 var textQueue         : TInterfaceList;
                                 var padQueue          : TInterfaceList;
                                 var regionQueue       : TInterfaceList;
                                 var fillQueue         : TInterfaceList;
                                 var bodyQueue         : TInterfaceList;
                                 var primNames         : TStringList;
                                 var csvReportStrs     : TStringList;                               
                                     )                 : Integer;
var
   i          : Integer;
   textDst    : IPCB_Text;
   regionDst  : IPCB_Region;
   trackDst   : IPCB_Track;
   identifier : TString;

begin

   { Assume success. }
   result := 0;

   {* Do a cleanup step where we delete any existing 3D text from the text queue. *}
   { Loop over all texts in the queue, backwards. }
   for i := (textQueue.Count - 1) downto 0 do
   begin

      { Retrieve reference to queued text. }
      textDst := textQueue.items[i];
      
      { Make sure this is a valid text object. }
      if (textDst <> 0) then
      begin

         { Retrieve text name. }
         identifier         := CLF_GetPrimName({prim} textDst, primNames);
         
         { See if this is 3D text. }
         if (identifier = constMagicalExclude3dTextName) then
         begin

            { Destroy this primitive. }
            PCBServer.DestroyPCBObject(textDst);
            
            { Remove this 3D text from the queue, since this was leftover from the last footprint. }
            textQueue.Delete(i);

         end; { endif }

      end; { endif }

   end; { endfor }
   
   
   {* Do a cleanup step where we delete any existing 3D region from the region queue. *}
   { Loop over all regions in the queue, backwards. }
   for i := (regionQueue.Count - 1) downto 0 do
   begin

      { Retrieve reference to queued region. }
      regionDst := regionQueue.items[i];
      
      { Make sure this is a valid region object. }
      if (regionDst <> 0) then
      begin

         { Retrieve region name. }
         identifier         := CLF_GetPrimName({prim} regionDst, primNames);
         
         { See if this is 3D region. }
         if (identifier = constMagicalExclude3dRegionName) then
         begin

            { Destroy this primitive. }
            PCBServer.DestroyPCBObject(regionDst);

            { Remove this 3D region from the queue, since this was leftover from the last footprint. }
            regionQueue.Delete(i);

         end; { endif }

      end; { endif }

   end; { endfor }
   

   {* Do a cleanup step where we delete any existing tracks that were part of 3D objects from a previous derived footprint. *}
   { Loop over all tracks in the queue that were added by 3D operations, backwards. }
   for i := (trackQueue.Count - 1) downto (trackQueue2dCount) do
   begin

      { Retrieve reference to queued track. }
      trackDst := trackQueue.items[i];
      
      { Make sure this is a valid track object. }
      if (trackDst <> 0) then
      begin

         { Destroy this primitive. }
         PCBServer.DestroyPCBObject(trackDst);

         { Remove this track from the queue, since this was leftover from the last derived footprint. }
         trackQueue.Delete(i);

      end; { endif }

   end; { endfor }
   
   {* Do a cleanup step where we delete any existing texts that were part of 3D objects from a previous derived footprint. *}
   { Loop over all texts in the queue that were added by 3D operations, backwards. }
   for i := (textQueue.Count - 1) downto (textQueue2dCount) do
   begin

      { Retrieve reference to queued text. }
      textDst := textQueue.items[i];
      
      { Make sure this is a valid text object. }
      if (textDst <> 0) then
      begin

         { Destroy this primitive. }
         PCBServer.DestroyPCBObject(textDst);

         { Remove this text from the queue, since this was leftover from the last derived footprint. }
         textQueue.Delete(i);

      end; { endif }

   end; { endfor }
   

   {* Create all new 3D features on component body layer. *}
   CLF_CreateNewFeatures3dCompbody(scriptsPath, 
                                   projectPath,
                                   libHeightMm,
                                   stepFilePath,                                         
                                   allow3dExtrusion,
                                   doBaseFp,
                                   currDerivedFpNum,
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
   
   {* Create all new 3D features on courtbody layer. *}
   CLF_CreateNewFeatures3dCourtbody({var} cnfGalacticInfo,
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

end; { end CLF_CreateNewFeatures3d() }


{***************************************************************************
 * function CLF_FinalModifyFeaturesSilkscreen()
 *  Do a final pass through silkscreen features and see if there's anything we need
 *  to modify.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_FinalModifyFeaturesSilkscreen(var cnfGalacticInfo : TStringList;
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
   i                     : Integer;
   j                     : Integer;
   trackDst              : IPCB_Track;
   assemblyWestMm        : Real;
   assemblyEastMm        : Real;
   assemblyNorthMm       : Real;
   assemblySouthMm       : Real;
   silkscreenWestMm      : Real;
   silkscreenEastMm      : Real;
   silkscreenNorthMm     : Real;
   silkscreenSouthMm     : Real;
   assyToSilkWestMm      : Real;
   assyToSilkEastMm      : Real;
   assyToSilkNorthMm     : Real;
   assyToSilkSouthMm     : Real;
   pkgDimsTotalWidthMin  : Real;
   pkgDimsTotalWidthMax  : Real;
   pkgDimsTotalLengthMin : Real;
   pkgDimsTotalLengthMax : Real;
   tweakToWest           : Boolean;
   tweakToEast           : Boolean;
   tweakToNorth          : Boolean;
   tweakToSouth          : Boolean;
   footprintType         : TString;
   identifier            : TString;
   silkLinesWest         : Integer;
   silkLinesEast         : Integer;
   silkLinesNorth        : Integer;
   silkLinesSouth        : Integer;
   X1mm                  : Real;
   Y1mm                  : Real;
   X2mm                  : Real;
   Y2mm                  : Real;
             
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_FinalModifyFeaturesSilkscreen().');

   { Initialize that we have no named outline lines. }
   silkLinesWest         := 0;
   silkLinesEast         := 0;
   silkLinesNorth        := 0;
   silkLinesSouth        := 0;
             
   { Retrieve footprint type. }
   footprintType         := cnfGalacticInfo.Values(constGilFootprintType);

   { Retrieve the bounds for the assembly outline. }
   CLF_RetrieveBoundingRectangleByNamePrefix({namePrefix} 'Assembly',
                                             {var} cnfGalacticInfo,
                                             {var boundaryWestMm} assemblyWestMm,
                                             {var boundaryEastMm} assemblyEastMm,
                                             {var boundaryNorthMm} assemblyNorthMm,
                                             {var boundarySouthMm} assemblySouthMm);
   
   { Retrieve the bounds for the silkscreen outline. }
   CLF_RetrieveBoundingRectangleByNamePrefix({namePrefix} 'Silkscreen',
                                             {var} cnfGalacticInfo,
                                             {var boundaryWestMm} silkscreenWestMm,
                                             {var boundaryEastMm} silkscreenEastMm,
                                             {var boundaryNorthMm} silkscreenNorthMm,
                                             {var boundarySouthMm} silkscreenSouthMm);
   
   { Retrieve coordinate info from galactic string list. }
   pkgDimsTotalWidthMin  := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsTotalWidthMin));
   pkgDimsTotalWidthMax  := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsTotalWidthMax));
   pkgDimsTotalLengthMin := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsTotalLengthMin));
   pkgDimsTotalLengthMax := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsTotalLengthMax));

   { Compute how much different the assembly and silkscreen bounding boxes actually are. }
   assyToSilkWestMm      := assemblyWestMm - silkscreenWestMm;
   assyToSilkEastMm      := assemblyEastMm - silkscreenEastMm;
   assyToSilkNorthMm     := assemblyNorthMm - silkscreenNorthMm;
   assyToSilkSouthMm     := assemblySouthMm - silkscreenSouthMm;
   
   { Loop over all queued tracks. }
   for i := 0 to (trackQueue.Count - 1) do
   begin

      { Retrieve reference to queued track. }
      trackDst := trackQueue.items[i];

      { See if this is a silkscreen track. }
      if (trackDst.Layer = constSilkLayer) then
      begin

         { Retrieve identifier. }
         identifier := CLF_GetPrimName({prim} trackDst, primNames);
         
         { See if this is a non-outline line that hasn't had its name updated. }
         if (identifier = 'Silkscreen') then
         begin

            { We want to kill off non-outline silkscreen lines (eg. pin 1 markers)
             on SOT & SOP type footprints. }
            if ( (footprintType = 'SOT') or (footprintType = 'SOP') ) then
            begin
               
               { Suppress this track. }
               trackQueue.items[i] := 0;
               WriteToDebugFile(' Suppressing track named "' + identifier + '".');               

            end { endif }

            { Else try to improve the name of this track. }
            else
            begin

               { Retrieve x1,y1 and x2,y2 coordinates for this track, in mm. }
               X1mm                  := CoordToMMs(trackDst.X1);
               Y1mm                  := CoordToMMs(trackDst.Y1);
               X2mm                  := CoordToMMs(trackDst.X2);
               Y2mm                  := CoordToMMs(trackDst.Y2);

               { Look for a line sitting on the west boundary. }
               if ( (X1mm = silkscreenWestMm) and (X2mm = silkscreenWestMm) ) then

                  { Rename it to append '_outline-west-x', where is is the number of lines on the west boundary. }
                  CLF_ChangePrimName({var prim} trackDst,
                                     {name} identifier + '_outline-west-' + IntToStr(CLF_VarPlusPlus(silkLinesWest)),
                                     {var} primNames) {nosemi}
                  
               { Else look for a line sitting on the east boundary. }
               else if ( (X1mm = silkscreenEastMm) and (X2mm = silkscreenEastMm) ) then

                  { Rename it to append '_outline-east-x', where is is the number of lines on the east boundary. }
                  CLF_ChangePrimName({var prim} trackDst,
                                     {name} identifier + '_outline-east-' + IntToStr(CLF_VarPlusPlus(silkLinesEast)),
                                     {var} primNames) {nosemi}
                  
               { Else look for a line sitting on the north boundary. }
               else if ( (Y1mm = silkscreenNorthMm) and (Y2mm = silkscreenNorthMm) ) then

                  { Rename it to append '_outline-north-x', where is is the number of lines on the north boundary. }
                  CLF_ChangePrimName({var prim} trackDst,
                                     {name} identifier + '_outline-north-' + IntToStr(CLF_VarPlusPlus(silkLinesNorth)),
                                     {var} primNames) {nosemi}
                  
               { Else look for a line sitting on the south boundary. }
               else if ( (Y1mm = silkscreenSouthMm) and (Y2mm = silkscreenSouthMm) ) then

                  { Rename it to append '_outline-south-x', where is is the number of lines on the south boundary. }
                  CLF_ChangePrimName({var prim} trackDst,
                                     {name} identifier + '_outline-south-' + IntToStr(CLF_VarPlusPlus(silkLinesSouth)),
                                     {var} primNames) {nosemi}

            end; { endelse }

         end; { endif is identifier 'Silkscreen' }         

      end; { endif is silkscreen track }
   
   end; { endfor i }

end; { end CLF_FinalModifyFeaturesSilkscreen() }


{***************************************************************************
 * function CLF_FinalModifyFeaturesPads()
 *  Do a final pass through pads and see if there's anything we need
 *  to modify.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_FinalModifyFeaturesPads(var cnfGalacticInfo : TStringList;
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
   i         : Integer;
   j         : Integer;
   trackDst  : IPCB_Track;
   arcDst    : IPCB_Arc;
   textDst   : IPCB_Text;
   padDst    : IPCB_Pad;
   regionDst : IPCB_Region;
   bodyDst   : IPCB_ComponentBody;
   padCache  : TPadCache;
   radius    : TCoord;
   XCenterMm : Real;
   YCenterMm : Real;
   RadiusMm  : Real;
         
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_FinalModifyFeaturesPads().');

   { Loop over all queued pads. }
   for i := 0 to (padQueue.Count - 1) do
   begin

      { Retrieve reference to queued pad. }
      padDst := padQueue.items[i];

      { See if this is a fiducial pad. }
      if (CLF_IsPadFiducial(padDst)) then
      begin

         { Create a topside keepout arc around this fiducial pin. }
         CLF_WriteToSummaryAndDebugFilesWithStepNum('Creating keepout arc around fidicial pad ' + padDst.Name + '.');

         { Retrieve the soldermask expansion for this fiducial pin. }
         padCache                           := padDst.GetState_Cache;
         radius                             := (2 * padCache.SolderMaskExpansion);

         { Compute parameters for new arc. }
         XCenterMm := CoordToMMs(padDst.X);
         YCenterMm := CoordToMMs(padDst.Y);
         RadiusMm :=  CoordToMMs(radius);
         
         CLF_CreateNewArc({layer} constFiducialLayer,
                          XCenterMm,
                          YCenterMm,
                          RadiusMm,
                          {LineWidthMm} constFiducialKeepoutWidthMm,
                          {StartAngleDeg} 0,
                          {EndAngleDeg} 360,
                          {isKeepout} True,
                          {var} arcQueue,
                          {name} 'Top_keepout_fiducial_' + padDst.Name,
                          {var} primNames
                          );

         { Secretly store this information in the footprint for later use by PCB helper scripts. }
         CLF_CreateNewTextFpInfo({name} constClfSecretPinFiducial + padDst.Name,
                                 {value} FloatToStr(XCenterMm) + '_' + FloatToStr(YCenterMm) + '_' + FloatToStr(CoordToMMs(padDst.TopXSize)) + '_' + FloatToStr(RadiusMm),
                                 {var} textQueue,
                                 {var} primNames);

         WriteToDebugFile('Checking for keepout region around fidicial pad ' + padDst.Name + '.');

      end { endif is fiducial pad }
      
      { Else if this is an EP pin, then we may need to recalculate solderpaste expansion. }
      else if (CLF_IsPadEp(padDst)) then
      begin

         { TODO:  We don't yet support split EP pads with solderpaste tiles! }
         
         { If we've been flagged that LP Wizard created solderpaste regions, then we need to
          suppress solderpaste on this pad itself. }
         if (CLF_IsNameInStringList({name} constParmHaveSolderPasteTiles,
             {stringlist} cnfGalacticInfo)) then
         begin

            { Configure for a solderpaste expansion of -10mm (shrinking it to nothing). }
            CLF_SetSolderPasteExpansion({var pad} padDst,
                                        constPasteExpansionSuppressMm);
            
            WriteToDebugFile('In CLF_FinalModifyFeaturesPads(), we have been flagged that LP Wizard created solderpaste regions.');
            WriteToDebugFile(' Therefore, suppresing solderpaste on EP pad itself.');

         end;

      end; { end elsif is EP pad }
   
   end; { endfor i }

end; { end CLF_FinalModifyFeaturesPads() }


{***************************************************************************
 * function CLF_FinalModifyOfFeatures()
 *  Do a final pass through all features and see if there's anything we need
 *  to modify.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_FinalModifyOfFeatures(var cnfGalacticInfo : TStringList;
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
   i         : Integer;
             
begin

   { Assume success. }
   result := 0;

   { Modify any pads. }
   CLF_FinalModifyFeaturesPads({var} cnfGalacticInfo,
                               {var} trackQueue,
                               {var} arcQueue,
                               {var} textQueue,
                               {var} padQueue,
                               {var} regionQueue,
                               {var} fillQueue,
                               {var} bodyQueue,
                               {var} primNames);

   { Modify any silkscreen features. }
   CLF_FinalModifyFeaturesSilkscreen({var} cnfGalacticInfo,
                                     {var} trackQueue,
                                     {var} arcQueue,
                                     {var} textQueue,
                                     {var} padQueue,
                                     {var} regionQueue,
                                     {var} fillQueue,
                                     {var} bodyQueue,
                                     {var} primNames);
   
end; { end CLF_FinalModifyOfFeatures() }


{***************************************************************************
 * function CLF_ProcessQueuedFeatures()
 *  Process all the queued features (tracks, arcs, etc.) and create them
 *  in new library component.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ProcessQueuedFeatures(    trackQueue    : TInterfaceList;
                                       arcQueue      : TInterfaceList;
                                       textQueue     : TInterfaceList;
                                       padQueue      : TInterfaceList;
                                       regionQueue   : TInterfaceList;
                                       fillQueue     : TInterfaceList;
                                       bodyQueue     : TInterfaceList;
                                   var primNames     : TStringList;
                                   var csvReportStrs : TStringList;
                                   var newLibComp    : IPCB_LibComponent;
                                       )             : Integer;

var                                               
   i           : Integer;
   trackDst    : IPCB_Track;
   arcDst      : IPCB_Arc;
   textDst     : IPCB_Text;
   padDst      : IPCB_Pad;
   regionDst   : IPCB_Region;
   fillDst     : IPCB_Fill;
   bodyDst     : IPCB_ComponentBody;
   trackNew    : IPCB_Track;
   arcNew      : IPCB_Arc;
   textNew     : IPCB_Text;
   padNew      : IPCB_Pad;
   regionNew   : IPCB_Region;
   fillNew     : IPCB_Fill;
   
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_ProcessQueuedFeatures().');

   { Loop over all tracks that we wish to create. }
   for i := 0 to (trackQueue.Count - 1) do
   begin

      { Retrieve reference to queued track. }
      trackDst := trackQueue.items[i];
      
      { Create this new track in new library component. }
      if (trackDst <> 0) then
      begin

         { Replicate the queued track and operate on that track. }
         trackNew := trackDst.Replicate();

         { Add new object to PcbLib footprint. }
         newLibComp.AddPCBObject(trackNew);
         PCBServer.SendMessageToRobots(newLibComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,trackNew.I_ObjectAddress);

         WriteToDebugFile('Hello from CLF_ProcessQueuedFeatures().  name is "' + primNames.Strings(trackNew.Index) + '".');

         { Report track properties to the stringlist that will become our .csv file report. }
         CLF_ReportTrackProperties(trackNew,
                                   primNames,
                                   {var} csvReportStrs);
      end;

   end; { endfor }
   
   { Loop over all arcs that we wish to create. }
   for i := 0 to (arcQueue.Count - 1) do
   begin

      { Retrieve reference to queued arc. }
      arcDst := arcQueue.items[i];
      
      { Create this new arc in new library component. }
      if (arcDst <> 0) then
      begin

         { Replicate the queued arc and operate on that arc. }
         arcNew := arcDst.Replicate();

         { Add new object to PcbLib footprint. }
         newLibComp.AddPCBObject(arcNew);
         PCBServer.SendMessageToRobots(newLibComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,arcNew.I_ObjectAddress);

         WriteToDebugFile('Hello from CLF_ProcessQueuedFeatures().  name is "' + primNames.Strings(arcNew.Index) + '".');
         
         { Report arc properties to the stringlist that will become our .csv file report. }
         CLF_ReportArcProperties(arcNew,
                                 primNames,
                                 {var} csvReportStrs);
      end;

   end; { endfor }
   
   { Loop over all texts that we wish to create. }
   for i := 0 to (textQueue.Count - 1) do
   begin

      { Retrieve reference to queued text. }
      textDst := textQueue.items[i];
      
      { Create this new text in new library component. }
      if (textDst <> 0) then
      begin

         { Replicate the queued text and operate on that text. }
         textNew := textDst.Replicate();

         { Add new object to PcbLib footprint. }
         newLibComp.AddPCBObject(textNew);
         PCBServer.SendMessageToRobots(newLibComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,textNew.I_ObjectAddress);

         WriteToDebugFile('Hello from CLF_ProcessQueuedFeatures().  name is "' + primNames.Strings(textNew.Index) + '".');

         { Report text properties to the stringlist that will become our .csv file report. }
         CLF_ReportTextProperties(textNew,
                                 primNames,
                                 {var} csvReportStrs);
      end;

   end; { endfor }
   
   { Loop over all pads that we wish to create. }
   for i := 0 to (padQueue.Count - 1) do
   begin

      { Retrieve reference to queued pad. }
      padDst := padQueue.items[i];
      
      { Create this new pad in new library component. }
      if (padDst <> 0) then
      begin

         { Replicate the queued pad and operate on that pad. }
         padNew := padDst.Replicate();

         { Add new object to PcbLib footprint. }
         newLibComp.AddPCBObject(padNew);
         PCBServer.SendMessageToRobots(newLibComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,padNew.I_ObjectAddress);

         { Report pad properties to the stringlist that will become our .csv file report. }
         CLF_ReportPadProperties(padNew,
                                 primNames,
                                 {var} csvReportStrs);
      end;

   end; { endfor }
   
   { Loop over all regions that we wish to create. }
   for i := 0 to (regionQueue.Count - 1) do
   begin

      { Retrieve reference to queued region. }
      regionDst := regionQueue.items[i];
      
      { Create this new region in new library component. }
      if (regionDst <> 0) then
      begin

         { Replicate the queued region and operate on that region. }
         regionNew := regionDst.Replicate();

         { Add new object to PcbLib footprint. }
         newLibComp.AddPCBObject(regionNew);
         PCBServer.SendMessageToRobots(newLibComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,regionNew.I_ObjectAddress);

         { Report region properties to the stringlist that will become our .csv file report. }
         CLF_ReportRegionProperties(regionNew,
                                    primNames,
                                    {var} csvReportStrs);
      end;

   end; { endfor }
   
   { Loop over all fills that we wish to create. }
   for i := 0 to (fillQueue.Count - 1) do
   begin

      { Retrieve reference to queued fill. }
      fillDst := fillQueue.items[i];
      
      { Create this new fill in new library component. }
      if (fillDst <> 0) then
      begin

         { Replicate the queued fill and operate on that fill. }
         fillNew := fillDst.Replicate();

         { Add new object to PcbLib footprint. }
         newLibComp.AddPCBObject(fillNew);
         PCBServer.SendMessageToRobots(newLibComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,fillNew.I_ObjectAddress);

         { Report fill properties to the stringlist that will become our .csv file report. }
         CLF_ReportFillProperties(fillNew,
                                  primNames,
                                  {var} csvReportStrs);
      end;

   end; { endfor }
   
   { Loop over all bodies that we wish to create. }
   for i := (bodyQueue.Count - 1) downto 0 do
   begin

      { Retrieve reference to queued body. }
      bodyDst := bodyQueue.items[i];
      
      { Create this new body in new library component. }
      if (bodyDst <> 0) then
      begin
         
         { Note:  No .Replicate() here!  We re-do 3D features for each footprint! }         

         { Add new object to PcbLib footprint. }
         newLibComp.AddPCBObject(bodyDst);
         PCBServer.SendMessageToRobots(newLibComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,bodyDst.I_ObjectAddress);

         { Note:  The equivalent of CLF_ReportBodyProperties() has already been done within the various calls
          to create extruded rectangles, spheres, cylinders, 3D text, and add STEP models. }
         
      end;

      { Remove this body from the queue, in preparation for the next derived footprint. }
      bodyQueue.Delete(i);

   end; { endfor }
   
end; { end CLF_ProcessQueuedFeatures() }


{***************************************************************************
 * procedure CLF_ReportMechLayerStatus()
 *  Enable and set visible a mechanical layer.
 *  Borrowed from buggy example in http://wiki.altium.com/display/ADOH/PCB+API+System+Interfaces .
 *  
 *  Returns:  Nothing.  Procedure, not a function.
 ***************************************************************************}
procedure CLF_ReportMechLayerStatus(newPcbLib      : IPCB_Library);
var
   TheLayerStack   : IPCB_LayerStack;
   i               : Integer;
   LayerObj        : IPCB_MechanicalLayer;
   Layer           : TLayer;
   LS              : String;
begin

//   ShowMessage('Hello world from CLF_ReportMechLayerStatus()');

   TheLayerStack := newPcbLib.Board.LayerStack;
   If TheLayerStack = Nil Then Exit;
   LS := '';
   For Layer := eMechanical1 to eMechanical16 Do
   Begin
      LayerObj := TheLayerStack.LayerObject[Layer];

      WriteToDebugFile(' ' + Layer2String(Layer) + ' linked: ' + BoolToStr(LayerObj.LinkToSheet) + 
                       ', enabled: ' + BoolToStr(LayerObj.MechanicalLayerEnabled) +
                       ', displayed in single layer mode: ' + BoolToStr(LayerObj.DisplayInSingleLayerMode));
   End;
   
end; { end CLF_ReportMechLayerStatus() }


{***************************************************************************
 * function CLF_ReportLayersUsed()
 *  Report to csv file which layers are used by this footprint.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ReportLayersUsed(    newPcbLib     : IPCB_Library;
                              var csvReportStrs : TStringList;
                                  )         : Integer;

var
   i          : Integer;
   line       : TString;
   identifier : TString;
   layer      : TLayer;
   enabled    : Boolean;
   PCBBoard   : IPCB_Board;

begin

   { Assume success. }
   result := 0;

   {eTopLayer ,
    eMidLayer1 ,
    eMidLayer2 ,
    eMidLayer3 ,
    eMidLayer4 ,
    eMidLayer5 ,
    eMidLayer6 ,
    eMidLayer7 ,
    eMidLayer8 ,
    eMidLayer9 ,
    eMidLayer10 ,
    eMidLayer11 ,
    eMidLayer12 ,
    eMidLayer13 ,
    eMidLayer14 ,
    eMidLayer15 ,
    eMidLayer16 ,
    eMidLayer17 ,
    eMidLayer18 ,
    eMidLayer19 ,
    eMidLayer20 ,
    eMidLayer21 ,
    eMidLayer22 ,
    eMidLayer23 ,
    eMidLayer24 ,
    eMidLayer25 ,
    eMidLayer26 ,
    eMidLayer27 ,
    eMidLayer28 ,
    eMidLayer29 ,
    eMidLayer30 ,
    eBottomLayer ,
    eTopOverlay ,
    eBottomOverlay ,
    eTopPaste ,
    eBottomPaste ,
    eTopSolder ,
    eBottomSolder ,
    eInternalPlane1 ,
    eInternalPlane2 ,
    eInternalPlane3 ,
    eInternalPlane4 ,
    eInternalPlane5 ,
    eInternalPlane6 ,
    eInternalPlane7 ,
    eInternalPlane8 ,
    eInternalPlane9 ,
    eInternalPlane10 ,
    eInternalPlane11 ,
    eInternalPlane12 ,
    eInternalPlane13 ,
    eInternalPlane14 ,
    eInternalPlane15 ,
    eInternalPlane16 ,
    eDrillGuide ,
    eKeepOutLayer ,
    eMechanical1 ,
    eMechanical2 ,
    eMechanical3 ,
    eMechanical4 ,
    eMechanical5 ,
    eMechanical6 ,
    eMechanical7 ,
    eMechanical8 ,
    eMechanical9 ,
    eMechanical10 ,
    eMechanical11 ,
    eMechanical12 ,
    eMechanical13 ,
    eMechanical14 ,
    eMechanical15 ,
    eMechanical16 ,
    eDrillDrawing ,
    eMultiLayer ,}
   
//   ShowMessage('Hello world from CLF_ReportLayersUsed()');

   { Get a reference to the current PCB. }
   PCBBoard  :=  PCBServer.GetCurrentPCBBoard;
   If  PCBBoard  =  Nil  Then
      CLF_Abort('Unable to get reference to current PCB.');
   
   { Loop over all layers we could possibly care about. }
   for layer := eTopLayer to eMultiLayer do
   begin

      { Retrieve whether this layer is enabled or not. }
      enabled := PCBBoard.LayerIsUsed(layer);
      
      WriteToDebugFile(' ' + Layer2String(layer) + ' enabled: ' + BoolToStr(enabled) + '.');

      { If this layer is enabled, then report it as such to csv file. }
      if (enabled) then
      begin

         WriteToDebugFile('Hello from CLF_ReportLayers Used().  Layer is ' + IntToStr(layer) + '.');

         { Create line identifier. }
         identifier         := CLF_GetSortablePadName({padName} IntToStr(layer));
         line := constCsvRptPrefixLayerUsed + identifier + '=';

         { Report relevant parameters. }
         CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldObjectKind, {valueStr} 'LayerIsUsed',                {var} line);
         CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldIdentifier, {valueStr} Layer2String(layer),          {var} line);
         CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldLayer,      {valueStr} Layer2String(layer),          {var} line);

         { Add line to csv report stringlist. }
         csvReportStrs.Add(line);

      end; { endif enabled }
      
   end; { endfor layer }

end; { end CLF_ReportLayersUsed() }


{***************************************************************************
 * procedure CLF_EnableAndShowMechanicalLayer()
 *  Enable and set visible a mechanical layer.
 *  
 *  Returns:  Nothing.  Procedure, not a function.
 ***************************************************************************}
procedure CLF_EnableAndShowMechanicalLayer(newPcbLib      : IPCB_Library;
                                           AMechanicalLayer : TLayer);
var
   MechanicalLayer : IPCB_MechanicalLayer;
begin

//   ShowMessage('Hello world from CLF_EnableAndShowMechanicalLayer()');

   {***************************************************************************
    * BEGIN code borrowed from ComponentBodyExample.pas.
    ***************************************************************************}
   MechanicalLayer := newPcbLib.Board.LayerStack.LayerObject[AMechanicalLayer];
   MechanicalLayer.MechanicalLayerEnabled := True;
   newPcbLib.Board.LayerIsDisplayed[AMechanicalLayer] := True;
   {***************************************************************************
    * END code borrowed from ComponentBodyExample.pas.
    ***************************************************************************}

   newPcbLib.Board.LayerIsUsed[AMechanicalLayer] := True;

end; { end CLF_EnableAndShowMechanicalLayer() }


{***************************************************************************
 * function CLF_CreateNewLibComp()
 *  Create a new component in a new PcbLib library.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewLibComp(var newPcbLib       : IPCB_Library;
                              var newLibComp      : IPCB_LibComponent;
                                  cnfGalacticInfo : TStringList;
                                  )               : Integer;

var                                                  
   i                 : Integer;
   FootprintIterator : IPCB_LibraryIterator;
   Footprint         : IPCB_LibComponent;   
begin

   { Assume success. }
   result := 0;

   { Since this is a new PcbLib, there is exactly one footprint already
    here, usually called "PCB_COMPONENT".  Kill it off. }
   FootprintIterator  :=  newPcbLib.LibraryIterator_Create;
   FootprintIterator.SetState_FilterAll;
   Footprint  :=  FootprintIterator.FirstPCBObject;
   newPcbLib.RemoveComponent(Footprint);
   
   { Create new footprint in new PCB library. }
   { Note:  We'll rename this later. }
   newLibComp := PCBServer.CreatePCBLibComp;
   newLibComp.Name := 'FOO';
   newPcbLib.RegisterComponent(newLibComp);

end; { end CLF_CreateNewLibComp() }


{***************************************************************************
 * function CLF_ConfigureNewLibComp()
 *  Configure miscellaneous things about the new library component.
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_ConfigureNewLibComp(    libFileName    : TString;
                                     libDescription : TString;
                                     libHeightMm    : Real;
                                 var newPcbLib      : IPCB_Library;
                                 var newLibComp     : IPCB_LibComponent;
                                     csvReportStrs  : TStringList;                               
                                     )              : Integer;

var                                                  
   i                : Integer;
   line             : TString;
   
begin

   { Assume success. }
   result := 0;

   CLF_WriteToSummaryAndDebugFilesWithStepNum('Created new footprint "' + libFileName + '" within new .PcbLib file.');

   {* Set new library to display in metric units. *}
   { Due to a bug in Altium we must request imperial to get metric.  Naturally. }
   { Thank you John Go-Soco for your forum post!  http://forum.live.altium.com/#posts/76896 }
   newPcbLib.Board.DisplayUnit := eImperial; //eMetric;

   {* Set grid spacing in new library. *}
   newPcbLib.Board.SnapGridSize := MMsToCoord(constGridSpacingMm);

   {* Set library component name. *}
   newLibComp.Name := libFileName;
  
   {* Set library component description field. *}
   newLibComp.Description := libDescription;

   {* Set library component height field. *}
   newLibComp.Height := MMsToCoord(libHeightMm);

   CLF_WriteToSummaryAndDebugFilesWithStepNum('Set description field, height field, metric units, and grid spacing within new .PcbLib file.');

   {* Report library component description and height to csv file. *}
   { Create line identifier. }
   line := constCsvRptPrefixFootprintProps + 'foo' + '=';

   { Report library name, library description, and package height. }
   { Note that we have to transform all ',' chars in description to something else, in this case '|' chars. }
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldObjectKind, {valueStr} 'FootprintProps',             {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldIdentifier, {valueStr} libFileName,                  {var} line);
   CLF_ReportCsvPropertyStr  ({name} constCsvRptFieldString,     {valueStr} StringReplace(libDescription, ',', '|', MkSet(rfReplaceAll)),  {var} line);
   CLF_ReportCsvPropertyCoord({name} constCsvRptFieldModelOverallHeight,  {valueCoord} newLibComp.Height, {var} line);

   { Add line to csv report stringlist. }
   csvReportStrs.Add(line);

end; { end CLF_ConfigureNewLibComp() }


{***************************************************************************
 * function CLF_CopyComponentFromImportedPcbDoc()
 *  Copy, modify, and customize the footprint that was imported from LP Wizard
 *  into a .PcbDoc file.  Queue all these features up for later use
 *  (eg. writing to one or more new .PcbLib files).
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CopyComponentFromImportedPcbDoc(    scriptsPath     : TDynamicString;
                                                 projectPath     : TDynamicString;
                                                 importedPcbDoc  : IDocument;
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
   i                   : Integer;
   k                   : Integer;
   rc                  : Integer;
   boardSrc            : IPCB_Board;
//   iterator          : IPCB_BoardIterator;
//   targetProjPath    : WideString;
//   Workspace         : IWorkspace;
//   newPcbLib         : IPCB_Library;
//   libFileName       : TString;
//   newLibComp        : IPCB_LibComponent;
//   pcbLibDoc         : IServerDocument;
//   projectDoc        : IServerDocument;
//   pcbLibFileName    : TString;
   lpwSilkTrackWidthMm : TString;
   footprintType       : TString;
   hasDshapePads       : Boolean;
                                                     
begin

   { Assume success. }
   result := 0;

   WriteToDebugFile('Hello from CLF_CopyComponentFromImportedPcbDoc()');
   CLF_UpdateGuiStatusMessage('Proceeding to copy and copy-modify features from imported .PcbDoc file.');

   {* Configure old layer / old width and new layer / new width for all LPW tracks. *}
   { Retrieve required info from galactic string list. }
   lpwSilkTrackWidthMm          := cnfGalacticInfo.Values(constLpWizardBuildInfoSilkLineWidthField);
   
   { Destination is courtyard. }
   cnfGalacticInfo.add(constGilModTracksOldLayer1   + constStringEquals + IntToStr(constSilkLayer));
   cnfGalacticInfo.add(constGilModTracksOldWidthMm1 + constStringEquals + FloatToStr(constOldWidthCourtyardMm));
   cnfGalacticInfo.add(constGilModTracksNewLayer1   + constStringEquals + IntToStr(constNewLayerCourtyard));
   cnfGalacticInfo.add(constGilModTracksNewWidthMm1 + constStringEquals + FloatToStr(constNewWidthCourtyardMm));
   
   { Destination is assembly drawing. }
   cnfGalacticInfo.add(constGilModTracksOldLayer2   + constStringEquals + IntToStr(constSilkLayer));
   cnfGalacticInfo.add(constGilModTracksOldWidthMm2 + constStringEquals + FloatToStr(constOldWidthAssyDrawingMm));
   cnfGalacticInfo.add(constGilModTracksNewLayer2   + constStringEquals + IntToStr(constNewLayerAssyDrawing));
   cnfGalacticInfo.add(constGilModTracksNewWidthMm2 + constStringEquals + FloatToStr(constNewWidthAssyDrawingMm));
   
   { Destination is none (being deleted). }
   cnfGalacticInfo.add(constGilModTracksOldLayer3   + constStringEquals + IntToStr(constSilkLayer));
   cnfGalacticInfo.add(constGilModTracksOldWidthMm3 + constStringEquals + FloatToStr(constOldWidthLinesToKill));
   cnfGalacticInfo.add(constGilModTracksNewLayer3   + constStringEquals + IntToStr(0));
   cnfGalacticInfo.add(constGilModTracksNewWidthMm3 + constStringEquals + FloatToStr(0));

   { Destination is silkscreen. }
   cnfGalacticInfo.add(constGilModTracksOldLayer4   + constStringEquals + IntToStr(constSilkLayer));
   cnfGalacticInfo.add(constGilModTracksOldWidthMm4 + constStringEquals + lpwSilkTrackWidthMm);
   cnfGalacticInfo.add(constGilModTracksNewLayer4   + constStringEquals + IntToStr(constSilkLayer));
   cnfGalacticInfo.add(constGilModTracksNewWidthMm4 + constStringEquals + FloatToStr(constWidthSilkMm));
   
   
   {** Explode (unlock primitives) of the component in source PcbDoc file. **}
   CLF_ExplodeComponent(importedPcbDoc,
                        {var} boardSrc);

   {** Copy all pads from source PcbDoc file to destination library component. **}
   rc := CLF_CopyAllPads(boardSrc,
                         {var} cnfGalacticInfo,
                         {var} padQueue,
                         {var} primNames
                         );                        

   { Create bounds for all groups of pads.  These will be used later for finishing silkscreen touches, etc. }
   CLF_CreatePadGroupBounds({var} cnfGalacticInfo, 
                            padQueue,
                            {var} trackQueue,
                            {var} primNames
                            );

   {** Copy all regions from source PcbDoc file to destination library component. **}
   rc := CLF_CopyAllRegions(boardSrc,
                            {var} cnfGalacticInfo,
                            {var} regionQueue,
                            {var} fillQueue,
                            {var} primNames,
                            {var} padQueue
                            );                            
   
   { If footprint is QFN and has D-shape pads, then clean-up corner pads to maintain pad to pad clearance. }
   footprintType := cnfGalacticInfo.Values(constGilFootprintType);
   hasDshapePads := StrToBool(cnfGalacticInfo.Values(constGilPkgDimsHasDshapePads));
   if ( (footprintType = 'QFN') and (hasDshapePads) ) then
   begin
      CLF_CleanupCornerDshapePads(cnfGalacticInfo,
                                  {var} padQueue
                                  );
   end;

   {** Copy all tracks from source PcbDoc file to destination library component. **}
   rc := CLF_CopyAllTracks(boardSrc,
                           {var} cnfGalacticInfo,
                           {var} trackQueue,
                           {var} primNames
                           );

   {** Copy all arcs from source PcbDoc file to destination library component. **}
   rc := CLF_CopyAllArcs(boardSrc,
                         {var} cnfGalacticInfo,
                         {var} arcQueue,
                         {var} primNames
                         );

   CLF_WriteToSummaryAndDebugFilesWithStepNum('Copied and copy-modified all regions, pads, tracks, and arcs from .PcbDoc to queue for new .PcbLib file.');

   { Note:  We're intentionally NOT copying any text, since the only text that
    Mentor LP Wizard generates is one that we want to nuke anyway! }

   { Close the PCB document. }
   { Note:  We've actually made one small change to the PcbDoc.  But apparently we've
    failed to properly notify the PCB server of said change.  So it willingly closes
    the document without prompting us to save.  Since this happens to be our desired
    behavior, we're calling it good.  The more proper way would be to explicitly
    undo the change we've made and then close the PcbDoc file. }
   ResetParameters;
   AddStringParameter('ObjectKind', 'Document');
   AddStringParameter('FileName', importedPcbDoc.DM_FullPath);
   RunProcess('WorkspaceManager:CloseObject');

   { Try to close all project documents. }
   ResetParameters;
   AddStringParameter('ObjectKind', 'FocusedProjectDocuments');
   RunProcess('WorkspaceManager:CloseObject');


   {** Create all new 2D features that we want in destination library component. **}
   CLF_UpdateGuiStatusMessage('Proceeding to add new 2D SPI-specific features to new footprint.');
   rc := CLF_CreateNewFeatures(scriptsPath,
                               projectPath,
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

end; { end CLF_CopyComponentFromImportedPcbDoc() }


{***************************************************************************
 * function CLF_CreateNewFootprintFromQueuedFeatures()
 *  Create a new footprint within a new footprint library (.PcbLib) using
 *  all of the queued features (tracks, arcs, pads, etc.) that we previously
 *  copied and copy-modified from the footprint imported from LP Wizard.
 *
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateNewFootprintFromQueuedFeatures(    project         : IProject;
                                                      trackQueue      : TInterfaceList;
                                                      arcQueue        : TInterfaceList;
                                                      textQueue       : TInterfaceList;
                                                      padQueue        : TInterfaceList;
                                                      regionQueue     : TInterfaceList;
                                                      fillQueue       : TInterfaceList;
                                                      bodyQueue       : TInterfaceList;
                                                      libFileName     : TString;
                                                      libDescription  : TString;
                                                      libHeightMm     : Real;
                                                      pcbLibFileName  : TString;
                                                  var primNames       : TStringList;
                                                  var csvReportStrs   : TStringList;
                                                  var cnfGalacticInfo : TStringList;
                                                      )               : Integer;

var
   i                        : Integer;
   k                        : Integer;
   rc                       : Integer;
   targetProjPath           : WideString;
   targetPath               : TString;
   Workspace                : IWorkspace;
   newLibComp               : IPCB_LibComponent;
   oldLibComp               : IPCB_LibComponent;
   pcbLibDoc                : IServerDocument;
   projectDoc               : IServerDocument;
   oldPcbLib                : IPCB_Library;
   newPcbLib                : IPCB_Library;
   vaultGUIDfoo             : TString;
   vaultGUID                : TString;
   folderGUID               : TString;
   lifeCycleDefinitionGUID  : TString;
   revisionNamingSchemeGUID : TString;
   itemGUID                 : TString;
   itemRevisionGUID         : TString;
   footprintType            : TString;

begin

   { Assume success. }
   result := 0;

   folderGUID               := '';
   vaultGUID                := '';
   lifeCycleDefinitionGUID  := '';
   revisionNamingSchemeGUID := '';
   itemGUID                 := '';
   itemRevisionGUID         := '';

   { Attempt to get reference to current workspace. }
   Workspace  := GetWorkspace;
   if (Workspace = nil) then
   begin
      CLF_Abort('Unable to find current workspace.');
   end;

   {* Attempt to retrieve Vault-related info from any previously existing version of this file. *}
   { See if the file even exists. }
   { TODO:  Possibly we should check for this file being in svn but currently missing from the working copy? }
   if (FileExists(pcbLibFileName)) then
   begin

      { Open existing version of this file. }
      AddStringParameter('ObjectKind', 'Document');
      AddStringParameter('FileName', pcbLibFileName);
      RunProcess('WorkspaceManager:OpenObject');

      { Attempt to get reference to new PCB library. }
      If PCBServer = Nil Then CLF_Abort('PCB Server reports Nil!');
      oldPcbLib := PcbServer.GetCurrentPCBLibrary;
      If oldPcbLib = Nil Then CLF_Abort('Unable to get reference to old PCB library!');

      { Retrieve PCBLib level Vault info. }
      vaultGUID                := oldPcbLib.VaultGUID;
      folderGUID               := oldPcbLib.FolderGUID;
      lifeCycleDefinitionGUID  := oldPcbLib.LifeCycleDefinitionGUID;
      revisionNamingSchemeGUID := oldPcbLib.RevisionNamingSchemeGUID;

      { Retrieve footprint level Vault info. }
      { TODO:  Here we assume/hope/pray that there is only 1 footprint in existing PcbLib file. }
      oldLibComp               := oldPcbLib.GetComponent(0);
      itemGUID                 := oldLibComp.ItemGUID;
      itemRevisionGUID         := oldLibComp.ItemRevisionGUID;
  
      WriteToDebugFile('oldPcbLib.vaultGUID is "' + vaultGUID + '".');
      WriteToDebugFile('oldPcbLib.folderGUID is "' + folderGUID + '".');
      WriteToDebugFile('oldPcbLib.lifeCycleDefinitionGUID is "' + lifeCycleDefinitionGUID + '".');
      WriteToDebugFile('oldPcbLib.revisionNamingSchemeGUID is "' + revisionNamingSchemeGUID + '".');
      WriteToDebugFile('oldPcbLib.itemGUID is "' + itemGUID + '".');
      WriteToDebugFile('oldPcbLib.itemRevisionGUID is "' + itemRevisionGUID + '".');

      { Attempt to close old PCB library. }
      ResetParameters;
      AddStringParameter('ObjectKind', 'FocusedProjectDocuments');
      RunProcess('WorkspaceManager:CloseObject');

   end; { endif }
   
   
   { Open target project. }
   WriteToDebugFile('Attempting to open target project "' + project.DM_ProjectFullPath + '".');
   targetProjPath := project.DM_ProjectFullPath;
   targetPath := ExtractFilePath(project.DM_ProjectFullPath);

   ResetParameters;
   AddStringParameter('ObjectKind', 'Project');
   AddStringParameter('FileName', targetProjPath);
   RunProcess('WorkspaceManager:OpenObject');

   ResetParameters;
   RunProcess('WorkspaceManager:SetCurrentProject');


   { See if we succeeded in shifting focus to our target project. }
   WriteToDebugFile('Current project is now "' + Workspace.DM_FocusedProject.DM_ProjectFullPath + '".');


   {* Store Vault linking info to secret text. *}
   { FIXME:  It's not clear that I want to actually store the Vault info in secret text.
    Leaving this code here for now, for compatibility with extant footprints. }
   vaultGUIDfoo      := '(none)';

   { Secretly store this information in the footprint for later use by PCB helper scripts. }
   CLF_CreateNewTextFpInfo({name} constClfSecretVaultGUID,
                           {value} vaultGUIDfoo,
                           {var} textQueue,
                           {var} primNames);

   
   { Retrieve footprint type. }
   footprintType         := cnfGalacticInfo.Values(constGilFootprintType);

   { Secretly store this information in the footprint for later use by PCB helper scripts. }
   CLF_CreateNewTextFpInfo({name} constClfSecretInfoFootprintType,
                           {value} footprintType,
                           {var} textQueue,
                           {var} primNames);


   { Delete this file from the filesystem if it currently exists.}
   { This way, we won't inherit any grid spacing, mech layer, etc. weirdness from an existing PcbLib file. }
   DeleteFileWithVerify(pcbLibFileName);

   { Create a new PcbLib file. }
   pcbLibDoc      := Client.OpenNewDocument ('PCBLIB', pcbLibFileName, '', False);
   Client.ShowDocument(pcbLibDoc);
   CLF_WriteToSummaryAndDebugFilesWithStepNum('Created new .PcbLib file "' + pcbLibFileName + '".');


   { Attempt to get reference to new PCB library. }
   If PCBServer = Nil Then CLF_Abort('PCB Server reports Nil!');
   newPcbLib := PcbServer.GetCurrentPCBLibrary;
   If newPcbLib = Nil Then CLF_Abort('Unable to get reference to current PCB library!');

   { Create a new component within this new PcbLib file. }
   CLF_CreateNewLibComp({var} newPcbLib,
                        {var} newLibComp,
                        cnfGalacticInfo);

   { Notify PCB server that we're going to start doing things. }
   PCBServer.PreProcess;

   { Restore PcbLib and footprint level Vault parameters. }
   if (vaultGUID <> '') then
   begin
      newPcbLib.VaultGUID                := vaultGUID;
      newPcbLib.FolderGUID               := folderGUID;
      newPcbLib.LifeCycleDefinitionGUID  := lifeCycleDefinitionGUID;
      newPcbLib.RevisionNamingSchemeGUID := revisionNamingSchemeGUID;

      newLibComp.ItemGUID                := itemGUID;
      newLibComp.ItemRevisionGUID        := itemRevisionGUID;
   end; { endif }

   { Report status of mechanical layers before. }
   WriteToDebugFile('Reporting mechanical layer status before we do anything:');
   CLF_ReportMechLayerStatus(newPcbLib);

   { Enable the various mechanical layers that we will use. }
   CLF_EnableAndShowMechanicalLayer(newPcbLib,
                                    {AMechanicalLayer} constNewLayerCourtyard);

   CLF_EnableAndShowMechanicalLayer(newPcbLib,
                                    {AMechanicalLayer} constLayerCourtBody);

   CLF_EnableAndShowMechanicalLayer(newPcbLib,
                                    {AMechanicalLayer} constNewLayerAssyDrawing);

   { Report status of mechanical layers after. }
   WriteToDebugFile('Reporting mechanical layer status after we have enabled required mechanical layers:');
   CLF_ReportMechLayerStatus(newPcbLib);

   {** Process all queued features that we wish to add to destination library component. **}
   CLF_UpdateGuiStatusMessage('Proceeding to create new footprint....');
   rc := CLF_ProcessQueuedFeatures(trackQueue,
                                   arcQueue,
                                   textQueue,
                                   padQueue,
                                   regionQueue,
                                   fillQueue,
                                   bodyQueue,
                                   {var} primNames,
                                   {var} csvReportStrs,
                                   {var} newLibComp
                                   );

   {** Perform final configuration of new library component. **}
   CLF_ConfigureNewLibComp(libFileName,
                           libDescription,
                           libHeightMm,
                           {var} newPcbLib,
                           {var} newLibComp,
                           {var} csvReportStrs);

   { Finish updating new component in new library. }
   PCBServer.SendMessageToRobots(newPcbLib.Board.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,newLibComp.I_ObjectAddress);
   PCBServer.PostProcess;
   newPcbLib.CurrentComponent := newLibComp;
   newPcbLib.Board.ViewManager_FullUpdate;

   { Refresh the PcbLib document. }
   ResetParameters;
   AddStringParameter('Action', 'All');
   RunProcess('PCB:Zoom');

   { Save PcbLib document. }
   pcbLibDoc.DoFileSave('PcbLib');

   { Report which layers are used in new footprint in new PcbLib file. }
   CLF_ReportLayersUsed(newPcbLib,
                        {var} csvReportStrs);

   { Refresh svn status of target project. }
   CLF_RefreshProjectSvnStatus(0);

end; { end CLF_CreateNewFootprintFromQueuedFeatures() }


{***************************************************************************
 * function CLF_CreateAllNewFootprints()
 *  Create all new footprints.  This is typically just a single, unmarked
 *  footprint.  But it may also be the unmarked footprint plus one or more
 *  derived, marked footprints.
 *
 *  Call CLF_CopyComponentFromImportedPcbDoc() to copy and copy-modify all features
 *  (tracks, pads, arcs, etc.) from footprint (PcbDoc) that gets imported from
 *  LP Wizard.  Also add various features of our own choosing.
 *
 *  Call CLF_CreateNewFootprintFromQueuedFeatures() to create a new footprint based
 *  on all the features (tracks, pads, arcs, etc.) that we already have queued, plus
 *  the specified libFileName and pcbLibFileName. 
 *  
 *  Returns:  0 on success, 1 if not successful.
 ***************************************************************************}
function CLF_CreateAllNewFootprints(    scriptsPath     : TDynamicString;
                                        projectPath     : TDynamicString;
                                        importedPcbDoc  : IDocument;
                                        project         : IProject;
                                    var cnfGalacticInfo : TStringList;
                                        )               : Integer;

var
   i                                      : Integer;
   k                                      : Integer;
   rc                                     : Integer;
   trackQueue                             : TInterfaceList;
   arcQueue                               : TInterfaceList;
   textQueue                              : TInterfaceList;
   padQueue                               : TInterfaceList;
   regionQueue                            : TInterfaceList;
   fillQueue                              : TInterfaceList;
   bodyQueue                              : TInterfaceList;
   primNames                              : TStringList;
   libSubDir                              : TString;
   libFileName                            : TString;
   libFileNameSuffix                      : TString;
   libFileNameSuffixOld                   : TString;
   libDescription                         : TString;
   libHeightMm                            : Real;
   pcbLibFileName                         : TString;
   csvReportStrs                          : TStringList;
   csvReportOld                           : TStringList;
   csvReportOut                           : TStringList;
   reportFilePath                         : TString;
   csvReportFilePath                      : TString;
   derivedFpNum                           : Integer;
   currDerivedFpNum                       : Integer;
   doBaseFp                               : Boolean;
   haveDerivedFp                          : Boolean;
   hasThVias                              : Boolean;
   packageColorCode                       : TString;
   packageMarkingMfgName                  : TString;
   packageMarkingMfgPn                    : TString;
   packageMarkingText                     : TString;
   trackQueue2dCount                      : Integer;
   textQueue2dCount                       : Integer;
   pkgDimsHeightMax                       : Real;
   stepFilePath                           : TString;
   derivedByMethod                        : TString;
   mode                                   : Boolean;
   pcbDocFileName                         : TString;
   filesAreReverted                       : Boolean;
   fcstdFilePath                          : TString;
   iniFilePath                            : TString;
   logFilePath                            : TString;
   modelDir                               : TString;
   modelPath                              : TString;
   highestRevNumber                       : Integer;
   highestRevMatchingStepFileNameTrueCase : TString;
   modelFolder                            : TString;
   expectedStepFileName                   : TString;
   deletedStepFile                        : Boolean;
                            
begin

   { Assume success. }
   result := 0;

   { Until we find out otherwise, we know about 0 derived footprints. }
   derivedFpNum    := 0;
   currDerivedFpNum:= 0;

   { Flag that we are working on the base footprint. }
   doBaseFp        := True;
   
   WriteToDebugFile('Hello from CLF_CreateAllNewFootprints()');
   
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

   { Call CLF_CopyComponentFromImportedPcbDoc() to copy and copy-modify all features
    (tracks, pads, arcs, etc.) from footprint (PcbDoc) that gets imported from
    LP Wizard.  Also add various 2D features of our own choosing. }
   CLF_CopyComponentFromImportedPcbDoc(scriptsPath,
                                       projectPath,
                                       importedPcbDoc,
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
      
      { Retrieve required info from galactic string list. }
      libFileName          := cnfGalacticInfo.Values(constGilLibraryFileName);
      libFileNameSuffixOld := cnfGalacticInfo.Values(constGilLibraryFileNameSuffix);
      libDescription       := cnfGalacticInfo.Values(constGilLibraryFileDescription);

      { Retrieve package dimension fields from galactic string list. }
      { TODO:  Override this with derived footprint height, as needed! }
      pkgDimsHeightMax     := StrToFloat(cnfGalacticInfo.Values(constGilPkgDimsHeightMax));
      libHeightMm          := pkgDimsHeightMax;
      
      { Retrieve user-specified STEP file, via command .xml file. }
      { TODO:  Override this with derived footprint STEP file name, as needed! }
      stepFilePath       := cnfGalacticInfo.Values(constGilStepFilePath);

      { See if this is the base footprint that we're working on. }
      if (doBaseFp) then
      begin

         { We have already retrieved the results of the previous call to CLF_CalculateLibFileNameSuffixAndDescription(). }

         { libFileName and libDescription are already valid for the base footprint, so nothing to do for them. }

         { The base footprint shall live in the project directory, not any subdir thereof. }
         libSubDir      := '';
      
      end { endif }

      { Else create values appropriate to derived footprint. }
      else
      begin

         { Call CLF_CalculateLibFileNameSuffixAndDescription() to obtain the appropriate libFileNameSuffix and libDescription. }
         CLF_CalculateLibFileNameSuffixAndDescription(hasThVias,
                                                      packageColorCode,
                                                      packageMarkingMfgName,
                                                      packageMarkingMfgPn,
                                                      packageMarkingText,
                                                      cnfGalacticInfo,
                                                      {var} libFileNameSuffix,
                                                      {var} libDescription);

         
         { Replace the suffix of the base footprint with the new suffix (eg. replace "BLNK" with "Marked_TI_74LVC04"). }
         libFileName          := StringReplace(libFileName, libFileNameSuffixOld, libFileNameSuffix, 0);

         { TODO:  Configure any subdirectory (eg. "Derived/") that we want to use for this footprint. }
         libSubDir      := 'Derived\';
      
      end; { endelse }

      { Construct the full path and name and extension for new PcbLib file that we are about to create. }
      pcbLibFileName := (projectPath + libSubDir + libFileName + '.PcbLib');
      pcbDocFileName := (projectPath + libSubDir + libFileName + '.PcbDoc'); 

      { Construct the full path and name and extension for new csvReport file that we are about to create. }
      csvReportFilePath       := (projectPath + libSubDir + libFileName + '_Features_Report.csv');
     
      { Construct the full path and name and extension for new script report file that we are about to create. }
      reportFilePath       := (projectPath + libSubDir + libFileName + '_Script_Report.txt');

      {** Read in old CSV report file for later comparison with new CSV report file. **}
      CLF_RevertOldCsvFileAndReadIn(projectPath,
                                    scriptsPath,
                                    {pcbLibOrFcstdFilePath} pcbLibFileName,
                                    {reportOrIniFilePath} reportFilePath,
                                    {stepFilePath} '',
                                    {csvOrLogFilePath} csvReportFilePath,
                                    {var} csvReportOld,
                                    {var} deletedStepFile);
      
      { Write name of upcoming footprint to summary file. }
      CLF_WriteToSummaryAndDebugFilesWithStepNum('About to start work on footprint "' + libFileName + '".');
      
      {** Create all new 3D features that we want in destination library component. **}
      CLF_UpdateGuiStatusMessage('Proceeding to add new 3D SPI-specific features to new footprint.');
      rc := CLF_CreateNewFeatures3d(scriptsPath,
                                    projectPath,
                                    libHeightMm,
                                    stepFilePath,
                                    {allow3dExtrusion} True,
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
                                               libHeightMm,
                                               pcbLibFileName,
                                               {var} primNames,
                                               {var} csvReportStrs,
                                               {var} cnfGalacticInfo);

      { Retrieve 3D model file paths from cnfGalacticInfo so they can be added to subversion. }      
      fcstdFilePath := cnfGalacticInfo.Values(constFC3DM_fcstdFilePath);
      WriteToDebugFile('fcstdFilePath: ' + fcstdFilePath);
      
      iniFilePath := cnfGalacticInfo.Values(constFC3DM_iniFilePath);
      WriteToDebugFile('iniFilePath: ' + iniFilePath);

      stepFilePath := cnfGalacticInfo.Values(constFC3DM_stepFilePath);
      WriteToDebugFile('stepFilePath: ' + stepFilePath);
      
      logFilePath :=  cnfGalacticInfo.Values(constFC3DM_logFilePath);
      WriteToDebugFile('logFilePath: ' + logFilePath);
 
      { Add all generated files (PcbLib + report file + csv file) to project and to svn. }
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
      CLF_UpdateGuiStatusMessage('Proceeding to write CSV report file to disk....');
      CLF_CsvReportFileWrite(csvReportStrs,
                             {var} csvReportOut,
                             csvReportFilePath);

      
      {** Save a copy of summary file to a per-PcbLib file name. **}
      { Note that this file has already been touched and added to project and svn, so we only need to write it now. }

      { (Re-)Write a copy of our summary file to the reportFileName, which is named similarly to the .PcbLib file. }
      SummaryMessages.SaveToFile(reportFilePath);


      { See if the CSV report file has changed compared to before this script run.  If it has not,
       then proceed to revert all generated files, so that user is not tempted to checkin effectively
       unchanged binary file .PcbLib. Mode=False indicates footprint mode. }
      mode := False;
      CLF_RevertGeneratedFilesIfNeeded(projectPath,
                                       scriptsPath,
                                       {pcbLibOrFcstdFilePath} pcbLibFileName,
                                       {reportOrIniFilePath} reportFilePath,
                                       {pcbDocOrStepFilePath} pcbDocFileName,
                                       {csvOrLogFilePath} csvReportFilePath,
                                       mode,
                                       {var csvOrLogFileOld} csvReportOld,
                                       {var csvOrLogFileOut} csvReportOut,
                                       {var} filesAreReverted
                                       );
      
      {** See if we have a(nother) derived footprint that we have been ordered to create. **}
      WriteToDebugFile('Checking for order to derive new footprint # ' + IntToStr(derivedFpNum) + '.');
      haveDerivedFp       :=  CLF_IsNameInStringList({name} (constGilDeriveByMethod + IntToStr(derivedFpNum)),
                                                     {stringlist} cnfGalacticInfo);

      { Configure for black, unmarked derived footprint until & unless we find out otherwise. }
      hasThVias             := False;
      packageColorCode      := 'Blk';   { Configure for black component body. }
      packageMarkingMfgName := '';
      packageMarkingMfgPn   := '';
      packageMarkingText    := '';
      
      { If needed, increment the number of derived footprints we know about. }
      if (haveDerivedFp) then
      begin

         WriteToDebugFile(' Setting up to derive new footprint # ' + IntToStr(derivedFpNum) + '.');

         { Retrieve the type of derived footprint that we need to deal with. }
         derivedByMethod    := cnfGalacticInfo.Values(constGilDeriveByMethod + IntToStr(derivedFpNum));

         { Handle the different types of derived footprints. }
         if (derivedByMethod = constGilDeriveByMarking) then
         begin

            { Retrieve info on the next derived footprint from galactic string list. }
            packageMarkingMfgName := cnfGalacticInfo.Values(constGilDeriveMfgName + IntToStr(derivedFpNum));
            packageMarkingMfgPn   := cnfGalacticInfo.Values(constGilDeriveMfgPn + IntToStr(derivedFpNum));

         end

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

      end;   
      
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
end; { end CLF_CreateAllNewFootprints() }

{***************************************************************************
 * END Footprint-related functions.
 ***************************************************************************}


{***************************************************************************
 * procedure CNF_CleanupLpwFootprint()
 *  This is the effective main program for the script.
 *
 *  Returns:  (nothing--procedure, not a function).
 ***************************************************************************}
procedure CNF_CleanupLpwFootprint(foo : Integer);
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
   importedPcbDoc    : IDocument;
   importedProject   : IProject;
   timestamp         : TDynamicString;
   startTime         : TDateTime;
   endTime           : TDateTime;
   rc                : Integer;
   cnfGalacticInfo   : TStringList;
   i                 : Integer;
   padsFilePath      : TString;
   reportFilePath    : TString;
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
   whichScriptIsThis := constWhichScriptClf;

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

   { Assign the path where we expect the Mentor LP Wizard generated .plb09 and .asc files to live. }
   lpWizardFilesPath := projectPath + StripTrailingBackslash(constLpWizardFilesDir);
   WriteToDebugFile(' lpWizardFilesPath is "' + lpWizardFilesPath + '".');
   
   { Try to close all project documents for the actual project. }
   CLF_UpdateGuiStatusMessage('Closing all documents in project before starting script.');
   ResetParameters;
   AddStringParameter('ObjectKind', 'FocusedProjectDocuments');
   RunProcess('WorkspaceManager:CloseObject');
   
   { Prompt user to identify PADS .asc file that he/she just exported from Mentor LP Wizard. }
   CLF_ChoosePadsFile(lpWizardFilesPath,
                      {var} cnfGalacticInfo,
                      {var} padsFilePath);

   {** Extract information from LP Wizard-generated .plb09 file that should exist alongside PADS .asc file. **}
   CLF_ExtractInfoFromLpWizardFile(padsFilePath,
                                   {var} cnfGalacticInfo);
   
   { Import said PADS .asc file into an Altium .PcbDoc file in a new imported project. }
   CLF_ImportFromPads(Workspace, 
                      scriptsPath,
                      projectPath,
                      {var} importedProject,
                      {var} cnfGalacticInfo);
      
   { Find the 1 PcbDoc file that was our footprint imported from PADS. }
   rc := CLF_FindImportedPcbDoc(importedProject,
                                {var} importedPcbDoc,
                                {var} cnfGalacticInfo);

   { Move the imported PcbDoc to live in the (target) project.
    Nuke its associated project file and other crap. }
   CLF_MoveImportedPcbDocAndNukeImportedProject(Workspace, 
                                                project,
                                                projectPath,
                                                importedProject,
                                                {var} importedPcbDoc,
                                                {var} cnfGalacticInfo);

   
   {** Add source documents (.asc file, .plb09 file, and imported PcbDoc and its project) to project and to svn. **}
   CLF_AddSourceDocumentsToProjectAndSvn(project,
                                         projectPath,
                                         scriptsPath,
                                         lpWizardFilesPath,
                                         importedPcbDoc,
                                         {var} commandFilePath,
                                         cnfGalacticInfo);

   {** Parse the footprint-specific .xml command file (if one exists) **}
   CLF_ParseCommandFile(projectPath,
                        commandFilePath,
                        {var} cnfGalacticInfo);
   
   {** Copy the footprint from LP Wizard and PADS from the imported PcbDoc file. **}
   { We also do a number of modifications and additions to the footprint(s). }
   { Call CLF_CreateAllNewFootprints() to do all the work. }
   rc := CLF_CreateAllNewFootprints(scriptsPath, 
                                    projectPath,
                                    importedPcbDoc,
                                    project,
                                    {var} cnfGalacticInfo);

   
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
   CLF_WriteToSummaryAndDebugFiles('1.  Review new footprint(s).');
   CLF_WriteToSummaryAndDebugFiles('2.  Perform svn checkin.');
   CLF_WriteToSummaryAndDebugFiles('3.  Right click, close project to imported project "' + ExtractFileName(importedProject.DM_ProjectFullPath) + '".');


   {****** Wrap things up ******}
   { Call AtExit() procedure to write debug outputs to file. }
   
   WriteToDebugFile('**About to exit script.');
//   ShowMessage('About to call AtExit()');
   AtExit(0);                   { Report success at exit }
//   ShowMessage('Back from AtExit()');

end; { end CNF_CleanupLpwFootprint() }


{***************************************************************************
 * procedure TCleanupLpwFootprintForm.clickedOk()
 *  This is the handler for primary dialog box "OK" click.
 ***************************************************************************}
procedure TCleanupLpwFootprintForm.clickedOk(Sender : TPanel);

begin

//   ShowMessage('Hello world from TCleanupLpwFootprintForm.clickedOk()');

   { Figure out if we got here from the initial Ok click to start the script or
    the final Ok click to end the script. }
   
   { See if this is the initial Ok click. }
   if (CleanupLpwFootprintForm.formButtonOk.Left <> 450) then
   begin
   
      { Tell the user that we are proceeding to run script. }
      formButtonsLabel1.Caption := 'OK.  Proceeding to run script.';
      formButtonsLabel1.Update;

      { Disable (grey out) OK and Cancel buttons on primary form. }
      formButtonOk.Enabled := False;
      formButtonOk.Update;

      formButtonCancel.Enabled := False;
      formButtonCancel.Update;

      { Call CNF_CleanupLpwFootprint() to do all the actual work. }
      CNF_CleanupLpwFootprint(99);

   end

   { Else this is the final ok to end the script.
    Close the modal window. }
   else
   begin
      ModalResult := mrOK;
      CleanupLpwFootprintForm.Close;
   end;
   
   { Return to caller. }
   Exit;

end; { end TCleanupLpwFootprintForm.clickedOk() }


{***************************************************************************
 * procedure TCleanupLpwFootprintForm.clickedCancel()
 *  This is the handler for primary dialog box "Cancel" click.
 ***************************************************************************}
procedure TCleanupLpwFootprintForm.clickedCancel(Sender : TPanel);

var
   Action : TCloseAction;
        
begin

   { Close dialog box. }
   ModalResult := mrCancel;
   CleanupLpwFootprintForm.Close;

   ShowError('Script canceled at User request.');

   { Exit script now. }
   Exit;
   
end; { end TCleanupLpwFootprintForm.clickedCancel() }


{***************************************************************************
 * procedure SPI_Cleanup_LPW_Footprint()
 *  Script entry point.
 *
 *  Returns:  (nothing--procedure, not a function).
 ***************************************************************************}
procedure SPI_Cleanup_LPW_Footprint;

//var

begin

   { Override GUI text entries. }
   { Note:  Don't condense this onto fewer lines or you will have svn replacing $Revision.*$
    with svn rev number on checkin! }
   CleanupLpwFootprintForm.Caption := 'Welcome to ' + constThisScriptNameNoExt + ' ' +
   StringReplace(StringReplace(constScriptVersion, '$Revision:', ', svn rev', ''),
                 ' $', '', '') + ' script main menu!';
 
   { Override GUI text entries. }
   CleanupLpwFootprintForm.formText01.Caption := 'You have launched script ' + constThisScriptName + '.';
   CleanupLpwFootprintForm.formText01.Font.Style := MkSet(fsBold);
   CleanupLpwFootprintForm.formText02.Caption := '';

   CleanupLpwFootprintForm.formText03.Caption := 'This script is intended to import a footprint from Mentor LP Wizard, clean it up, and add SPI-specific features.';
   CleanupLpwFootprintForm.formText03.Font.Style := MkSet(fsBold);
   CleanupLpwFootprintForm.formText04.Caption := '';
   CleanupLpwFootprintForm.formText05.Caption := '';
   CleanupLpwFootprintForm.formText06.Caption := '';
   CleanupLpwFootprintForm.formText07.Caption := '';

   CleanupLpwFootprintForm.formText08.Caption := ''; {This script will also:';}
   CleanupLpwFootprintForm.formText08.Font.Style := MkSet(fsBold);
   CleanupLpwFootprintForm.formText09.Caption := '';
   CleanupLpwFootprintForm.formText10.Caption := '';
   CleanupLpwFootprintForm.formText11.Caption := '';
   CleanupLpwFootprintForm.formText12.Caption := '';
   CleanupLpwFootprintForm.formText13.Caption := '';

   CleanupLpwFootprintForm.formText14.Caption := 'Preconditions:';
   CleanupLpwFootprintForm.formText14.Font.Style := MkSet(fsBold);
   CleanupLpwFootprintForm.formText15.Caption := '1.  IC footprint must have been created in Mentor LP Wizard and saved to its own .plb09 file.';
   CleanupLpwFootprintForm.formText16.Caption := '2.  IC footprint must also have been exported from Mentor LP Wizard to PADS .asc file.';
   CleanupLpwFootprintForm.formText17.Caption := '3.  If overrides or information for derived footprints is needed, .xml command file must have been already created.';
   CleanupLpwFootprintForm.formText18.Caption := '';
   CleanupLpwFootprintForm.formText19.Caption := '';
   CleanupLpwFootprintForm.formText20.Caption := '';

   CleanupLpwFootprintForm.formText21.Caption := 'Notes:';
   CleanupLpwFootprintForm.formText21.Font.Style := MkSet(fsBold);
   CleanupLpwFootprintForm.formText22.Caption := '';
   CleanupLpwFootprintForm.formText23.Caption := '';
   CleanupLpwFootprintForm.formText24.Caption := '';
   CleanupLpwFootprintForm.formText25.Caption := '';
   CleanupLpwFootprintForm.formText26.Caption := '';
   
   CleanupLpwFootprintForm.formText27.Caption := '';
   CleanupLpwFootprintForm.formText28.Caption := '';
   CleanupLpwFootprintForm.formText29.Caption := '';
   
   CleanupLpwFootprintForm.formText30.Caption := ''; //'Options:';
   CleanupLpwFootprintForm.formText30.Font.Style := MkSet(fsBold);
   
   CleanupLpwFootprintForm.formButtonsLabel1.Caption := 'Shall I run (OK), or shall I Cancel running this script?';
   CleanupLpwFootprintForm.formButtonsLabel1.Font.Style := MkSet(fsBold);

   { Set initial status message. }
   CleanupLpwFootprintForm.formStatusBar1.SimpleText := 'Status:  Awaiting user permission to run script.';

   
   { Run GUI dialog box asking user for permission to run. }
   CleanupLpwFootprintForm.ShowModal;

   { Note:  Control now passes to one of the handler functions above. }
   
end; { end SPI_Cleanup_LPW_Footprint() }




end.
