#================================================================================================
#
#	@file			FC3DM_IC_Gullwing.py
#
#	@brief			Python script to create a 3D model of a parameterized Gullwing IC in FreeCAD.
#
#	@details		
#
#    @version		0.2.1
#					   $Rev::                                                                        $:
#	@date			  $Date::                                                                        $:
#	@author			$Author::                                                                        $:
#					    $Id::                                                                             $:
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
pin1MarkName = parms["pin1MarkName"]
bodyName = parms["bodyName"]
docName = parms["docName"]

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

# Create all IC gullwing pins
pinNames = list()
FC3DM_CreateIcPins(App, Gui,
                   parms, pinNames,
                   docName)

# Describe all objects in this component to a logfile.
FC3DM_DescribeObjectsToLogFile(App, Gui,
                               parms, pinNames,
                               docName)

# Fuse all objects together & retain proper coloring
objNameList = list(pinNames)
objNameList.append(bodyName)
objNameList.append(pin1MarkName)
fusionName = docName
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
                    parms,
                    objNameList)

