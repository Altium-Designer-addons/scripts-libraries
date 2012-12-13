#================================================================================================
#
#	@file			FC3DM_TI_PW-20.py
#
#	@brief			Python script to create an SMT fuse 3D model in FreeCAD.
#
#	@details		
#
#    @version		0.1.12
#					   $Rev::                                                                        $:
#	@date			  $Date::                                                                        $:
#	@author			$Author::                                                                        $:
#					    $Id::                                                                          $:
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
sys.path.append("r:\\trunk\\mechanical\\3D-models\\SPI_Created\\FreeCAD\\FreeCAD_macros")

# Import our utilities module
from FC3DM_utils import *



###################################
#### Main function
###################################

# Invariant information
newModelPath = "R:/trunk/mechanical/3D-models/SPI_Created/FreeCAD/IC_Gullwing/"
stepSuffix = "_SPI1"
suffix = "_SvnRev_"



###################################
## Prepare for next model
###################################
newModelName = "SOP65P640X120-20N_TI_PW-20_Blk_BLNK_IPC_LPW"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = "Body"
moldName = "Mold"
pinName = "Pin"
pin2Name = "Pin2"
pinsName = "Pins"
pin1MarkName = "Pin1Mark"

# Strip out all "-" characters for use as the FreeCAD document name
docName = string.replace(newModelName, "-", "_")

# Create new document
App.newDocument(docName)
App.setActiveDocument(docName)
App.ActiveDocument=App.getDocument(docName)
Gui.ActiveDocument=Gui.getDocument(docName)


## Parameters off datasheet
L=6.6
T=0.75
W=0.3
A=4.5
B=6.6
H=1.2
K=0.05

# Thickness of pin (in Z)
Tp = 0.15

## Parameters that we have to make up, since they're not usually specified on datasheet

# Mold angle (in degrees) (not specified in datasheet--make something up)
maDeg = 12

# Pivot points (in Z) for chamfering the IC body
#Hpph = (0.5*H) + (0.5*Tp)
#Hppl = (0.5*H) - (0.5*Tp)

Hpph = (0.5*H)
Hppl = Hpph


# Fillet radius for pin edges
Fr = Tp

# Height of entry of pin (center) to IC body
Hpe = Hpph + (0.5*Tp)

# Fillet radius for body
Frbody = 0.1

# Offset in X and Y from the pin 1 corner to the start of the pin 1 marker cylinder
P1markOffset = 0.07

# Radius of pin 1 marker cylinder
P1markRadius = 0.15

# How much to indent the pin 1 marker into IC body
P1markIndent = 0.02

# Height of marking ink
markHeight = 0.001


## Start creating the component model.

# Call CreateIcBody() to create the plastic molded IC body
FC3DM_CreateIcBody(App, Gui,
                   A, B, H, K,
                   maDeg, Hpph, Hppl,
                   Frbody, P1markOffset, P1markRadius, P1markIndent, markHeight, 
                   docName,
                   bodyName,
                   pin1MarkName)


# Call CreateIcPin() to create first (template) IC pin
FC3DM_CreateIcPin(App, Gui,
                  L, A, B, 
                  W, T, Tp, Fr, Hpe,
                  maDeg, Hpph, Hppl,
                  docName,
                  pinName,
                  bodyName)

# Copy IC pins to other locations
rotDeg = 0.0
x = 0.0
y = (0 * 0.65) + (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin16")

y = (1 * 0.65) + (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin17")

y = (2 * 0.65) + (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin18")

y = (3 * 0.65) + (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin19")

y = (4 * 0.65) + (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin20")


y = (0 * 0.65) - (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin15")

y = (-1 * 0.65) - (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin14")

y = (-2 * 0.65) - (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin13")

y = (-3 * 0.65) - (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin12")

y = (-4 * 0.65) - (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin11")


rotDeg = 180.0
x = 0.0
y = (0 * 0.65) + (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin5")

y = (1 * 0.65) + (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin4")

y = (2 * 0.65) + (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin3")

y = (3 * 0.65) + (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin2")

y = (4 * 0.65) + (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin1")


y = (0 * 0.65) - (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin6")

y = (-1 * 0.65) - (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin7")

y = (-2 * 0.65) - (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin8")

y = (-3 * 0.65) - (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 "Pin9")

y = (-4 * 0.65) - (0.5*0.65)
FC3DM_CopyObject(App, Gui,
                 x, y, rotDeg,
                 docName,
                 pinName,
                 pinsName) #"Pin10")


## Fuse all the pins together
FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin1")

FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin2")

FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin3")

FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin4")

FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin5")

FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin6")

FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin7")

FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin8")

FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin9")

FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin11")

FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin12")

FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin13")

FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin14")

FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin15")

FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin16")

FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin17")

FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin18")

FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin19")

FC3DM_FuseObjects(App, Gui,
                  docName, pinsName, "Pin20")


# Remove the objects that made up the fillet
App.getDocument(docName).removeObject(pinName)


## Wrap up
# Color body black
FreeCADGui.getDocument(docName).getObject(bodyName).ShapeColor = (0.10,0.10,0.10)

# Color pins bright tin
FreeCADGui.getDocument(docName).getObject(pinsName).ShapeColor = (0.80,0.80,0.75)

# Color Pin1Mark white
FreeCADGui.getDocument(docName).getObject(pin1MarkName).ShapeColor = (1.00,1.00,1.00)


# Zoom in
App.ActiveDocument.recompute()
Gui.SendMsgToActiveView("ViewFit")

## Save file to native format and export to STEP
objNameList = [bodyName, pinsName, pin1MarkName]
FC3DM_SaveAndExport(App, Gui,
                    docName,
                    newModelPathNameExt,
                    newStepPathNameExt,
                    objNameList)
