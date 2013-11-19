#================================================================================================
#
#	@file			FC3DM_chip_resistor.py
#
#	@brief			Python script to create an SMT fuse 3D model in FreeCAD.
#
#	@details		
#
#    @version		0.2.1
#					   $Rev::                                                                        $:
#	@date			  $Date::                                                                        $:
#	@author			$Author::                                                                        $:
#					    $Id::                                                                               $:
#
#	@copyright      Copyright (c) 2012 Sierra Photonics, Inc.  All rights reserved.
#	
#***************************************************************************
# * The Sierra Photonics, Inc. Software License, Version 1.0:
# *  
# * Copyright (c) 2012 by Sierra Photonics Inc.  All rights reserved.
# *  Author:        Jeff Collins, jcollins@sierraphotonics.com
# *  Author:        $Author$
# *  Check-in Date: $Date$ 
# *  Version #:     $Revision$
# *  
# * Redistribution and use in source and binary forms, with or without
# * modification, are permitted provided that the following conditions
# * are met and the person seeking to use or redistribute such software hereby
# * agrees to and abides by the terms and conditions below:
# *
# * 1. Redistributions of source code must retain the above copyright
# * notice, this list of conditions and the following disclaimer.
# *
# * 2. Redistributions in binary form must reproduce the above copyright
# * notice, this list of conditions and the following disclaimer in
# * the documentation and/or other materials provided with the
# * distribution.
# *
# * 3. The end-user documentation included with the redistribution,
# * if any, must include the following acknowledgment:
# * "This product includes software developed by Sierra Photonics Inc." 
# * Alternately, this acknowledgment may appear in the software itself,
# * if and wherever such third-party acknowledgments normally appear.
# *
# * 4. The Sierra Photonics Inc. names or marks must
# * not be used to endorse or promote products derived from this
# * software without prior written permission. For written
# * permission, please contact:
# *  
# *  Sierra Photonics Inc.
# *  attn:  Legal Department
# *  7563 Southfront Rd.
# *  Livermore, CA  94551  USA
# * 
# * IN ALL CASES AND TO THE FULLEST EXTENT PERMITTED UNDER APPLICABLE LAW,
# * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
# * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# * DISCLAIMED.  IN NO EVENT SHALL SIERRA PHOTONICS INC. OR 
# * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
# * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
# * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# * SUCH DAMAGE.
# *
# * This software consists of voluntary contributions made by many
# * individuals on behalf of the Altium Community Software.
# *
# * See also included file SPI_License.txt.
# ***************************************************************************

import FreeCAD
import Part
import math
import string
import sys

# Add our path to the python system path
## Get path to this script
#scriptPath, fr = os.path.split(sys.argv[0]) #__path__)
#scriptPath = os.getcwd()
#scriptPath = "c:\\projects\\libraries\\Design-Electrical\\trunk\\Mechanical\\3D-models\\TRT_Created\\FreeCAD_macros"
scriptPath = "c:\\projects\\altium-designer-addons\\trunk\\SPI_Footprint_and_Vault_Scripts\\Mechanical_scripts\\mechanical\\3D-models\\SPI_Created\\FreeCAD\\FreeCAD_macros"
sys.path.append(scriptPath)


# Import our utilities module
import FC3DM_utils

# Reload utilities module, since this changes often!
reload(FC3DM_utils)

# Explicitly load all functions within it
from FC3DM_utils import *



###################################
#### Main function
###################################

# Invariant information
#newModelPath = "R:/trunk/mechanical/3D-models/SPI_Created/FreeCAD/Fuse_SMT/"
newModelPath = "c:/projects/libraries/Design-Electrical/trunk/Mechanical/3D-models/TRT_Created/Resistor_chip/"
stepSuffix = "_TRT1"
suffix = "_SvnRev_"


###################################
#### Read ini files to get all our parameters.
###################################

# Clear the parms associative array
parms = {}

# Workaround needed because otherwise parms doesn't actually get created until
# after the function call, and then we end up with a reference to a blank parms dict.
parms["foo"] = "bar"

# Read both the global and component-specific ini files.
FC3DM_ReadIniFiles(scriptPath, parms)

# Write parms to console window
print "Back in main(), Parms are:"
print parms


###################################
#### Main function
###################################

# Open the debug file
FC3DM_OpenDebugFile(parms)


###################################
## Prepare for next model
###################################
bodyName = parms["bodyName"]
moldName = parms["moldName"]
pinName = parms["pinName"]
pinsName = parms["pinsName"]
pin2Name = parms["pin2Name"]

docName = parms["docName"]


# Create new document
App.newDocument(docName)
App.setActiveDocument(docName)
App.ActiveDocument=App.getDocument(docName)
Gui.ActiveDocument=Gui.getDocument(docName)


# Setup parameters related to the chip component body
L = parms["L"] # Length of chip resistor
W = parms["W"] # Width of chip resistor
T = parms["T"] # Distance from furthest length to end of termination
H = parms["H"] # Height of chip resistor
K = parms["K"] # Should be 0 always
termThickness = parms["termThickness"]

# Tweak these to reflect only the white substrate
bodyH = H-(termThickness)
bodyL = L-(2*termThickness)
bodyK = termThickness

# Create a box to represent the white substrate of the SMT fuse
rotDeg = 0
FC3DM_CreateAndCenterBox(App, Gui,
                         bodyL, W, bodyH, bodyK,
                         rotDeg, 
                         docName,
                         bodyName)


# Create left side chip component termination
termL = T
termX = -1*(L/2)
termY = -1*(W/2)
FC3DM_CreateBox(App, Gui,
                termL, W, H, K,
                termX, termY, rotDeg, 
                docName,
                pinsName)
    
# Cut out the part of the termination that overlaps with the body
FC3DM_CutObjectWithToolAndKeepTool(App, Gui,
                                   docName, pinsName, bodyName)

# Fillet the outside edges of this termination
edges = ["Edge2","Edge4"]
radius = (termThickness)
FC3DM_FilletObjectEdges(App, Gui,
                        docName, pinsName, edges, radius)

# Copy pin 1 to pin 2
FC3DM_CopyObject(App, Gui,
                 0, 0, 180, 
                 docName,
                 pinsName,
                 pin2Name)

# Create a box to represent the overmold
moldL = L-(2*T)
moldH = bodyH + (termThickness)
moldK = bodyH
rotDeg = 0
FC3DM_CreateAndCenterBox(App, Gui,
                         moldL, W, moldH, moldK,
                         rotDeg, 
                         docName,
                         moldName)


## Fillet the outside edges of this overmold
#edges = ["Edge2","Edge6","Edge10","Edge12"]
#radius = termThickness
#FC3DM_FilletObjectEdges(App, Gui,
#                        docName, moldName, edges, radius)

## Fuse all the pins together
#FC3DM_FuseObjects(App, Gui,
#                  docName, pinsName, pin2Name)


## Wrap up
# Color body white
#FreeCADGui.getDocument(docName).getObject(bodyName).ShapeColor = (1.00,1.00,1.00)

# Color pins bright tin
#FreeCADGui.getDocument(docName).getObject(pinsName).ShapeColor = (0.80,0.80,0.75)

# Color overmold black
#FreeCADGui.getDocument(docName).getObject(moldName).ShapeColor = (0.10,0.10,0.10)

# The FC3DM_FuseSetOfObjects() function expects a "pin1MarkName", which we don't have.
# But we do have a "moldName", so trick it.
parms["pin1MarkName"] = parms["moldName"]

# Fuse all objects together & retain proper coloring
objNameList = []
objNameList.append(pinsName)
objNameList.append(pin2Name)
objNameList.append(bodyName)
objNameList.append(moldName)
fusionName = docName
FC3DM_FuseSetOfObjects(App, Gui,
                       parms,
                       docName, objNameList, fusionName)

# Zoom in
App.ActiveDocument.recompute()
Gui.SendMsgToActiveView("ViewFit")

# Save file to native format and export to STEP
#objNameList = [bodyName, pinsName, moldName]
objNameList = [fusionName]
FC3DM_SaveAndExport(App, Gui,
                    docName,
                    parms,
                    objNameList)

# Save and close debug file.
FC3DM_CloseDebugFile()

# Exit with success return code.
#FC3DM_MyExit(0)
