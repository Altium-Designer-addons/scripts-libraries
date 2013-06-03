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

import FreeCAD
import Part
import math
import string
import sys
import csv

# Function below was stolen from Daniel Goldberg's post at:
#  http://stackoverflow.com/questions/354038/how-do-i-check-if-a-string-is-a-number-in-python
def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False
    
## Prepare to read ini file for this component.
iniFileName = "c:\\projects\\altium-designer-addons\\trunk\\SPI_Footprint_and_Vault_Scripts\\Mechanical_scripts\\mechanical\\3D-models\\SPI_Created\\FreeCAD\\FreeCAD_macros\\TI_PW-20.ini"

# Clear the parms associative array
parms = {}

# Open ini file with our paths and parameters
print "About to open ini file"

#ins = open(iniFileName, "rb" )
lines = [line.strip() for line in open(iniFileName, "r")]

array = []
for line in lines:
    array.append( line )

    # Exclude all lines beginning with '#' comment character
    if (not (line.startswith('#'))):
#        print line

        # Split at '#' char to strip off any within-line comments
        tup = line.partition('#')
        line = tup[0];

        # Look for '=' sign to indicate name=value pair
        if (line.find('=') > -1):
#            print "Found name=value pair!"

            # Split at '=' sign and strip off leading/trailing whitespace
            tup = line.partition('=')
            name = tup[0].strip()
            value = tup[2].strip()
#            print("name=:" + name + ":")
#            print("value=:" + value + ":")

            # Determine if this a numeric or string value
            if (is_number(value)):

                print "Found numeric value! " + value
                
                # Add name=value pair (numeric value) to our parms associative array 
                parms[name] = float(value)

            else:

                # Add name=value pair (string value) to our parms associative array
                # Strip off '"' chars that have somehow propagated to this point
                parms[name] = value.replace("\"", "")
    

# Write parms to console window
print "Parms are:"
print parms


# Add our path to the python system path
#sys.path.append("r:\\trunk\\mechanical\\3D-models\\SPI_Created\\FreeCAD\\FreeCAD_macros")
sys.path.append("c:\\projects\\altium-designer-addons\\trunk\\SPI_Footprint_and_Vault_Scripts\\Mechanical_scripts\\mechanical\\3D-models\\SPI_Created\\FreeCAD\\FreeCAD_macros")

# Import our utilities module
import FC3DM_utils

# Reload utilities module, since this changes often!
reload(FC3DM_utils)

# Explicitly load all functions within it
from FC3DM_utils import *

#FreeCAD.Console.PrintMessage("Hello World!\n")


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

# Calculate derived strings
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"

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


# Fuse all objects together
objNameList = [bodyName, pin1MarkName, "Pin1", "Pin2", "Pin3", "Pin4", "Pin5", "Pin6", "Pin7", "Pin8", "Pin9", "Pin10", "Pin11", "Pin12", "Pin13", "Pin14", "Pin15", "Pin16", "Pin17", "Pin18", "Pin19", "Pin20"]
fusionName = docName
FC3DM_FuseSetOfObjects(App, Gui,
                       docName, objNameList, fusionName)

# Color fusion red.  FIXME:  Change to bright tin!
#Gui.getDocument(docName).getObject(fusionName).ShapeColor = (0.0,1.0,0.0)

# Color fusion bright tin
Gui.getDocument(docName).getObject(fusionName).ShapeColor = (0.80,0.80,0.75)
App.ActiveDocument.recompute()

# TODO:  I've been unable to find a way in python to go in and change the color of individual
# faces with the fused shape.  The plan is to fuse everything together, so that it will be
# a STEP part, rather than a STEP assembly.  But I need to change the color of the faces
# that were part of the body back to black, and the color of the pin 1 mark back to white.


## Wrap up
# Color body black
#FreeCADGui.getDocument(docName).getObject(bodyName).ShapeColor = (0.10,0.10,0.10)

# Color pins bright tin
#FreeCADGui.getDocument(docName).getObject(pinsName).ShapeColor = (0.80,0.80,0.75)

# Color Pin1Mark white
#FreeCADGui.getDocument(docName).getObject(pin1MarkName).ShapeColor = (1.00,1.00,1.00)


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

