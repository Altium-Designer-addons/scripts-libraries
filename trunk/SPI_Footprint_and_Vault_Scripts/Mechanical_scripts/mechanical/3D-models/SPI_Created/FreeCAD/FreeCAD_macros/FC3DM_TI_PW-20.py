#================================================================================================
#
#	@file			FC3DM_TI_PW-20.py
#
#	@brief			Python script to create a 3D model of TI's 20 pin TSSOP IC package in FreeCAD.
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

###################################
#### Load external libraries.
###################################
import FreeCAD
import Part
import math
import string
import sys
import os

## Get cwd
cwd = os.getcwd()
print("cwd is :" + cwd + ":")

# Replace all "\" chars with "\\" so that we can use it as a path in sys.path.append()
# FIXME:  Probably this is a Windows only thing!
cwd = cwd.replace("\\", "\\\\")
print("cwd is :" + cwd + ":")

# Add cwd to system path so that we can pick up our FC3DM_Utils.py file.
# Note:  In absence of this file being in our cwd, FreeCAD may need to be passed a
# "-p path" command line parameter!
sys.path.append(cwd)
    
# Import our utilities module
import FC3DM_utils

# Reload utilities module, since this changes often!
reload(FC3DM_utils)

# Explicitly load all functions within it
from FC3DM_utils import *


###################################
#### Read ini files to get all our parameters.
###################################

# Clear the parms associative array
parms = {}

# Workaround needed because otherwise parms doesn't actually get created until
# after the function call, and then we end up with a reference to a blank parms dict.
parms["foo"] = "bar"

# Read both the global and component-specific ini files.
FC3DM_ReadIniFiles(parms)

# Write parms to console window
print "Back in main(), Parms are:"
print parms


###################################
#### Main function
###################################

# Extract relevant parameter values from parms associative array
# TODO:  Currently no error checking!
newModelPath = parms["newModelPath"]
newModelName = parms["newModelName"]
stepSuffix = parms["stepSuffix"]
pinName = parms["pinName"]
pin1MarkName = parms["pin1MarkName"]
bodyName = parms["bodyName"]
stepExt = parms["stepExt"]

# Calculate derived strings
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + stepExt

# Strip out all "-" characters for use as the FreeCAD document name
docName = string.replace(newModelName, "-", "_")

# Create new document
App.newDocument(docName)
App.setActiveDocument(docName)
App.ActiveDocument=App.getDocument(docName)
Gui.ActiveDocument=Gui.getDocument(docName)


## Start creating the component model.

# Call CreateIcBody() to create the plastic molded IC body
FC3DM_CreateIcBody(App, Gui,
                   parms,
                   docName)

# Call CreateIcPin() to create first (template) IC pin
FC3DM_CreateIcPin(App, Gui,
                  parms,
                  docName)

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
                 "Pin10")


# Remove the pin template object
App.getDocument(docName).removeObject(pinName)

# Color body black FIXME!
FreeCADGui.getDocument(docName).getObject(bodyName).ShapeColor = (0.10,0.10,0.10)
#FreeCADGui.getDocument(docName).getObject(bodyName).ShapeColor = (0.0,1.0,0.0)
App.ActiveDocument.recompute()

# Color pins
FreeCADGui.getDocument(docName).getObject("Pin1").ShapeColor = (0.0,0.0,1.0)
App.ActiveDocument.recompute()


# Fuse all objects together & retain proper coloring
objNameList = [bodyName, pin1MarkName, "Pin1", "Pin2", "Pin3", "Pin4", "Pin5", "Pin6", "Pin7", "Pin8", "Pin9", "Pin10", "Pin11", "Pin12", "Pin13", "Pin14", "Pin15", "Pin16", "Pin17", "Pin18", "Pin19", "Pin20"]
fusionName = "Final"
FC3DM_FuseSetOfObjects(App, Gui,
                       parms,
                       docName, objNameList, fusionName)


# Zoom in
App.ActiveDocument.recompute()
Gui.SendMsgToActiveView("ViewFit")

## Save file to native format and export to STEP
objNameList = [fusionName]
FC3DM_SaveAndExport(App, Gui,
                    docName,
                    newModelPathNameExt,
                    newStepPathNameExt,
                    objNameList)

