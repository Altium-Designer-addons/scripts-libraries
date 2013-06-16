#================================================================================================
#
#	@file			FC3DM_utils.py
#
#	@brief			Python script functions to help create electronics 3D models in FreeCAD.
#
#	@details		
#
#    @version		0.3.6
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
import cStringIO
import sys
import os
import re

scriptPathUtils = ""

###################################################################
# is_number()
# 	Function below was stolen from Daniel Goldberg's post at:
#  http://stackoverflow.com/questions/354038/how-do-i-check-if-a-string-is-a-number-in-python
###################################################################
def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False


###################################################################
# FC3DM_SortPinNames()
#	Function to do custom comparison for pin names.
###################################################################
def FC3DM_SortPinNames(a, b):

    # See if we're even sorting a pair of valid pin names
    if (a.startswith("Pin") and b.startswith("Pin")):

        # Strip off anything after an '=' char.
        tup = a.partition('=')
        a = tup[0];
        tup = b.partition('=')
        b = tup[0];

        # Strip off leading "Pin" from a & b pin names
        aStr = a.replace("Pin", "");
        bStr = b.replace("Pin", "");

        ## TODO:  Support BGA pin names, as well as "EP"!

        # Convert remaining name to integer
        aInt = int(aStr);
        bInt = int(bStr);

        if aInt > bInt:
            return 1
        elif aInt == bInt:
            return 0
        else:
            return -1

    # Else at least one of these is not a valid pin name.  Do simple comparison using builtin.
    else:
        return cmp(a,b)
    

###################################################################
# FC3DM_ReadIniFile()
#	Function to read an ini file.  File format is "key=value".
###################################################################
def FC3DM_ReadIniFile(iniFileName,
                      parms):

    # Open ini file with our paths and parameters
    print ("About to open ini file :" + iniFileName + ":")

    #ins = open(iniFileName, "rb" )
    lines = [line.strip() for line in open(iniFileName, "r")]

    array = []
    for line in lines:
        array.append( line )

        # Exclude all lines beginning with '#' comment character
        if (not (line.startswith('#'))):
#            print line

            # Split at '#' char to strip off any within-line comments
            tup = line.partition('#')
            line = tup[0];

            # Look for '=' sign to indicate name=value pair
            if (line.find('=') > -1):
#                print "Found name=value pair!"

                # Split at '=' sign and strip off leading/trailing whitespace
                tup = line.partition('=')
                name = tup[0].strip()
                value = tup[2].strip()
#                print("name=:" + name + ":")
#                print("value=:" + value + ":")

                # Determine if this a numeric or string value
                if (is_number(value)):

                    print "Found numeric value! " + value

                    # Add name=value pair (numeric value) to our parms associative array 
                    parms[name] = float(value)

                else:

                    # Add name=value pair (string value) to our parms associative array
                    # Strip off '"' chars that have somehow propagated to this point
                    parms[name] = value.replace("\"", "")

    return 0

                
###################################################################
# FC3DM_ReadIniFiles()
#	Function to read both global and component-specific ini files.
###################################################################
def FC3DM_ReadIniFiles(scriptPath, parms):

    # Store to global variable
    global scriptPathUtils
    scriptPathUtils = scriptPath

    ## Prepare to read global ini file.
    # Append ini file name.
    iniFileName = scriptPath + "\\" + "FC3DM_global.ini"

    # Read global ini file
    FC3DM_ReadIniFile(iniFileName,
                      parms)

    # Write parms to console window
    print "Parms are:"
    print parms

    ## Prepare to read component-specific ini file.
    # Extract relevant parameter values from parms associative array
    # TODO:  Currently no error checking!
    # Note:  Assumes that iniFileName from file is a relative directory!
    #  Thus, we must pre-pend our path to this.
    iniFileName = scriptPath + "\\" + parms["iniFileName"]

    # Read component-specific ini file
    FC3DM_ReadIniFile(iniFileName,
                      parms)

    ## Set standard colors for our component
    # TODO:  We should be reading these from ini file also!
    parms["colorPin1Mark"] = ((1.00,1.00,1.00)) # White for pin1Mark
    parms["colorPins"] = ((0.80,0.80,0.75)) 	# Bright tin for all pins
    parms["colorBody"] = ((0.10,0.10,0.10))	# Black for body    


    ## Extract relevant parameter values from parms associative array
    # TODO:  Currently no error checking!
    newModelPath = parms["newModelPath"]
    newModelName = parms["newModelName"]
    stepSuffix = parms["stepSuffix"]
    stepExt = parms["stepExt"]

    ## Calculate derived strings
    newModelPathNameExt = newModelPath + newModelName + ".FCStd"
    newStepPathNameExt = newModelPath + newModelName + stepSuffix + stepExt
    logFilePathNameExt = newModelPath + newModelName + stepSuffix + ".log"

    # Strip out all "-" characters for use as the FreeCAD document name
    docName = string.replace(newModelName + stepSuffix, "-", "_")

    ## Store derived strings to parms
    parms["newModelPathNameExt"] = newModelPathNameExt
    parms["newStepPathNameExt"] = newStepPathNameExt
    parms["logFilePathNameExt"] = logFilePathNameExt
    parms["docName"] = docName

    ## Remove bogus parms
    del parms["foo"]

    # Write parms to console window
    print "Parms are:"
    print parms

    return 0


###################################################################
# FC3DM_FilletObjectEdges()
# 	Function to fillet edges of a given object.
###################################################################
def FC3DM_FilletObjectEdges(App, Gui,
                            docName, filletMe, edges, radius):

    # Init
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(docName)
    App.ActiveDocument=App.getDocument(docName)
    Gui.ActiveDocument=Gui.getDocument(docName)

    # Fillet the sharp edges of the termination sheet metal
    Gui.activateWorkbench("PartDesignWorkbench")
    App.activeDocument().addObject("PartDesign::Fillet","Fillet")
    App.activeDocument().Fillet.Base = (App.ActiveDocument.getObject(filletMe),edges)
    Gui.activeDocument().hide(filletMe)
#    Gui.activeDocument().Fusion.Visibility=False
    Gui.activeDocument().setEdit('Fillet')
    App.ActiveDocument.Fillet.Radius = radius
    App.ActiveDocument.recompute()
    Gui.activeDocument().resetEdit()

    # Remove the objects that made up the fillet
    App.getDocument(docName).removeObject(filletMe)

    # Copy the cut object and call it the original cutMe name
    newTermShape = FreeCAD.getDocument(docName).getObject("Fillet").Shape.copy()
    newTermObj = App.activeDocument().addObject("Part::Feature",filletMe)
    newTermObj.Shape = newTermShape
    
    # Remove the fillet itself
    App.getDocument(docName).removeObject("Fillet")

    return 0


###################################################################
# FC3DM_ChamferObjectEdges()
# 	Function to chamfer edges of a given object.
# NOTE:  This function is less useful than you may think.  If you
#  want a 45 deg chamfer, then the two planes meeting at the edge
#  that you want to chamfer must be perpendicular!  If, on the
#  other hand, one is angled by the mold angle, then calling this
#  function will NOT give you a 45 degree absolute chamfer!
###################################################################
def FC3DM_ChamferObjectEdges(App, Gui,
                             docName, chamferMe, edges, size):

    # Init
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(docName)
    App.ActiveDocument=App.getDocument(docName)
    Gui.ActiveDocument=Gui.getDocument(docName)

    # Chamfer the sharp edges of the termination sheet metal
    Gui.activateWorkbench("PartDesignWorkbench")
    App.activeDocument().addObject("PartDesign::Chamfer","Chamfer")
    App.activeDocument().Chamfer.Base = (App.ActiveDocument.getObject(chamferMe),edges)
    Gui.activeDocument().hide(chamferMe)
#    Gui.activeDocument().Fusion.Visibility=False
    Gui.activeDocument().setEdit('Chamfer')
    App.ActiveDocument.Chamfer.Size = size
    App.ActiveDocument.recompute()
    Gui.activeDocument().resetEdit()

    # Remove the objects that made up the chamfer
    App.getDocument(docName).removeObject(chamferMe)

    # Copy the cut object and call it the original cutMe name
    newTermShape = FreeCAD.getDocument(docName).getObject("Chamfer").Shape.copy()
    newTermObj = App.activeDocument().addObject("Part::Feature",chamferMe)
    newTermObj.Shape = newTermShape
    
    # Remove the chamfer itself
    App.getDocument(docName).removeObject("Chamfer")

    return 0


###################################################################
# FC3DM_DescribeObjectsToLogFile()
#	Function to describe all objects to a log file.
###################################################################
def FC3DM_DescribeObjectsToLogFile(App, Gui,
                                   parms, pinNames,
                                   docName):

    # Extract relevant parameter values from parms associative array
    # TODO:  Currently no error checking!
    bodyName = parms["bodyName"]
    pin1MarkName = parms["pin1MarkName"]
    logFilePathNameExt = parms["logFilePathNameExt"]

    # Init
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(docName)
    App.ActiveDocument=App.getDocument(docName)
    Gui.ActiveDocument=Gui.getDocument(docName)

    ## Dump all parms to string list
    strList = list()

    # Loop over all parms
    for parm in parms:

        # Append this to string list
        strList.append(parm + "=" + str(parms[parm]))

    # Sort the string list
    strList.sort(FC3DM_SortPinNames)

    ## Open the logfile
    fileP = open(logFilePathNameExt, 'w')

    # Log all the parms to logfile
    fileP.write("Parms:\n")
    for i in strList:
        fileP.write(i + '\n')

    ## Log all pin vertices to logfile.
    # Loop over all the pin names.
    for pin in pinNames:

        # Declare the name of this object
        fileP.write("\n" + pin + ':\n')

        # Declare the soon-to-be color of this object
        fileP.write("Color " + str(parms["colorPins"]) + "\n")

        # Loop over all the faces in this pin.
        for face in App.ActiveDocument.getObject(pin).Shape.Faces:

            # Loop over all the vertexes in this pin
            for vertex in face.Vertexes:

                # Write this vertex to file
                fileP.write(str(vertex.Point) + '\n')

    ## Log all body vertices to logfile.
    # Declare the name of this object
    fileP.write("\n" + bodyName + ':\n')

    # Declare the soon-to-be color of this object
    fileP.write("Color " + str(parms["colorBody"]) + "\n")

    # Loop over all the faces in this body.
    for face in App.ActiveDocument.getObject(bodyName).Shape.Faces:

        # Loop over all the vertexes in this body
        for vertex in face.Vertexes:

            # Write this vertex to file
            fileP.write(str(vertex.Point) + '\n')

    ## Log all pin1Mark vertices to logfile.
    # Declare the name of this object
    fileP.write("\n" + pin1MarkName + ':\n')

    # Declare the soon-to-be color of this object
    fileP.write("Color " + str(parms["colorPin1Mark"]) + "\n")

    # Loop over all the faces in this pin1Mark.
    for face in App.ActiveDocument.getObject(pin1MarkName).Shape.Faces:

        # Loop over all the vertexes in this pin1Mark
        for vertex in face.Vertexes:

            # Write this vertex to file
            fileP.write(str(vertex.Point) + '\n')


    # Close the logfile
    fileP.close()
        
    return 0


###################################################################
# FC3DM_FuseObjects()
#	Function to fuse two objects together.
###################################################################
def FC3DM_FuseObjects(App, Gui,
                      docName, fuseMe, addMeToFusion):

    # Fuse two objects
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(docName)
    App.ActiveDocument=App.getDocument(docName)
    Gui.ActiveDocument=Gui.getDocument(docName)
    App.activeDocument().addObject("Part::MultiFuse","Fusion")
    App.activeDocument().Fusion.Shapes = [App.ActiveDocument.getObject(fuseMe), App.ActiveDocument.getObject(addMeToFusion)]
    App.ActiveDocument.recompute()

    # Remove the objects that made up the fusion
    App.getDocument(docName).removeObject(fuseMe)
    App.getDocument(docName).removeObject(addMeToFusion)

    # Copy the fusion object and call it the original fuseMe name
    newTermShape = FreeCAD.getDocument(docName).getObject("Fusion").Shape.copy()
    newTermObj = App.activeDocument().addObject("Part::Feature",fuseMe)
    newTermObj.Shape = newTermShape
    
    # Remove the fusion itself
    App.getDocument(docName).removeObject("Fusion")

    return 0


###################################################################
# FC3DM_FuseSetOfObjects()
#	Function to fuse a set of objects together and preserve face colors.
###################################################################
def FC3DM_FuseSetOfObjects(App, Gui,
                           parms,
                           docName, objNameList, fusionName):

    print("Hello world from FuseSetOfObjects!")

    # Extract relevant parameter values from parms associative array
    # TODO:  Currently no error checking!
    bodyName = parms["bodyName"]
    pin1MarkName = parms["pin1MarkName"]
    print "pin1MarkName is :" + pin1MarkName + ":"


    # Configure active document
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(docName)
    App.ActiveDocument=App.getDocument(docName)
    Gui.ActiveDocument=Gui.getDocument(docName)

    # Create list of objects, starting with object names
    objs=[]
    for i in objNameList:
        
        objs.append(App.activeDocument().getObject(i))

    # Do the multi-fusion.  Write to "Temp".
    Gui.activateWorkbench("PartWorkbench")
    App.activeDocument().addObject("Part::MultiFuse","Temp")
    App.activeDocument().getObject("Temp").Shapes = objs
    App.ActiveDocument.recompute()

    # Copy the temp fusion object and call it the desired fusion name
    newTermShape = FreeCAD.getDocument(docName).getObject("Temp").Shape.copy()
    newTermObj = App.activeDocument().addObject("Part::Feature", fusionName)
    newTermObj.Shape = newTermShape
    App.ActiveDocument.recompute()


    ### Preserve face colors for the body and pin1Mark!
    # Cache vertex lists for the body and pin1Mark, so that we may find them later
    # in the fused object.

    ## Loop over all faces in the pin1Mark object
    pin1MarkVerts=[]
    for i in App.ActiveDocument.getObject(pin1MarkName).Shape.Faces:
        print("Found face in pin1Mark object!")

        # Loop over all the vertexes in this face
        buf = cStringIO.StringIO()
        for j in i.Vertexes:

            print >> buf, j.Point

        # Store all the vertexes for this face as a string array
        pin1MarkVerts.append(buf.getvalue())

        print "Vertexes for this face are:"
        print buf.getvalue()

    ## Loop over all faces in the body object
    bodyVerts=[]
    for i in App.ActiveDocument.getObject(bodyName).Shape.Faces:
        print("Found face in body object!")

        # Loop over all the vertexes in this face
        buf = cStringIO.StringIO()
        for j in i.Vertexes:

            print >> buf, j.Point

        # Store all the vertexes for this face as a string array
        bodyVerts.append(buf.getvalue())

        print "Vertexes for this face are:"
        print buf.getvalue()


    ## Loop over all faces in the pin objects
    pinVerts=[]
    for k in objNameList:

        # See if this object name is the body or pin 1 marker.
        if ( (k != bodyName) and (k != pin1MarkName) ):

            print("Found pin object named " + k + ".")

            # Loop over all the vertexes in this face
            for i in App.ActiveDocument.getObject(k).Shape.Faces:
                print("Found face in pin object!")

                # Loop over all the vertexes in this face
                buf = cStringIO.StringIO()
                for j in i.Vertexes:

                    print >> buf, j.Point

                # Store all the vertexes for this face as a string array
                pinVerts.append(buf.getvalue())

                print "Vertexes for this face are:"
                print buf.getvalue()


    ## Prepare to compare all faces in the fusion with pin1Mark, pin, and body faces
    faceColors=[]

    # Loop over all the faces in the fusion object
    for xp in App.ActiveDocument.getObject(fusionName).Shape.Faces:
        print("Found face in fusion object!")

        # Clear found flags for pin1Mark, body, and pin
        isPin1Mark = False;
        isBody = False;
        isPin = False;

        # Loop over all the vertexes in this fusion shape face
        buf = cStringIO.StringIO()
        for j in xp.Vertexes:

            # Record this vertex in a string buffer
            print >> buf, j.Point

        print "Vertexes for this face are:"
        print buf.getvalue()

        # See if this face is the same as a face from the pin1Mark object
        for i in pin1MarkVerts:

            # I can't get Faces.isSame() to actually work.  That's why I'm doing this
            # kludge with sprint'ing vertex info to a string.
#            print xp.isSame(i)
        
            # See if the i string (from a pin1Mark vertex string) equals our current vertex string
            if (buf.getvalue() == i):
                isPin1Mark = True;

        # See if this face is the same as a face from the pin1Mark object
        for i in pinVerts:

            # I can't get Faces.isSame() to actually work.  That's why I'm doing this
            # kludge with sprint'ing vertex info to a string.
#            print xp.isSame(i)
        
            # See if the i string (from a pin vertex string) equals our current vertex string
            if (buf.getvalue() == i):
                isPin = True;

        # See if this face is the same as a face from the body object
        for i in bodyVerts:

            # I can't get Faces.isSame() to actually work.  That's why I'm doing this
            # kludge with sprint'ing vertex info to a string.
#            print xp.isSame(i)
        
            # See if the i string (from a body vertex string) equals our current vertex string
            if (buf.getvalue() == i):
                isBody = True;


        # See if we found a face that derives from our pin1Mark
        if (isPin1Mark):

            print("Found face in fusion that exactly matched a pin1Mark face.")

            # Color Pin1Mark white
            faceColors.append(parms["colorPin1Mark"])

        # See if we found a face that derives from a pin
        elif (isPin):

            print("Found face in fusion that exactly matched a pin face.")

            # Color pin bright tin.
            faceColors.append(parms["colorPins"])
        
        # See if we found a face that derives from our body
        elif (isBody):

            print("Found face in fusion that exactly matched a body face.")

            # Color body black
            faceColors.append(parms["colorBody"])

        # Else we're not sure what it is.  Assume that it's part of the body
        # that got modified as pins fused to it, and thus wasn't an exact match.
        else:

            print("Found face in fusion that didn't match a known face!")

            # Count the number of vertex lines in this string.
            numLines = len(buf.getvalue().splitlines())

            # Empirically I've seen that unmatched pin faces have only 4 vertexes.
            # TODO:  Will this simple distinction be true for non-gullwing ICs???
            if (numLines <= 4):
                print(" numLines is " + str(numLines) + ".  Hoping it's part of a pin!")

                # Color pin bright tin.
                faceColors.append(parms["colorPins"])
        
            # Else we assume that it's part of the body.
            else:
           
                print(" numLines is " + str(numLines) + ".  Hoping it's part of the body!")

                # Color body black
                faceColors.append(parms["colorBody"])


    # Now that we have a list of all the face colors that we want, proceed to apply it
    # to the fusion shape.
    # A special thanks here to FC guru/author WMayer for his forum post at
    # https://sourceforge.net/apps/phpbb/free-cad/viewtopic.php?f=19&t=4117
    # which pointed me in the right direction to make this actually work.                
    print("Attempting to set fusion face colors")
    Gui.ActiveDocument.getObject(fusionName).DiffuseColor=faceColors
    App.ActiveDocument.recompute()
    print("Attempted to set fusion face colors")

    # Delete the original objects that comprised the fusion
    for i in objNameList:

        App.activeDocument().removeObject(i)

    # Remove the temp fusion
    App.getDocument(docName).removeObject("Temp")
    App.ActiveDocument.recompute()

    # Deallocate list
    del objs

    return 0


###################################################################
# FC3DM_CutWithFilletedBox()
#	Function to cut an object with a filleted box that we create for this purpose.
###################################################################
def FC3DM_CutWithFilletedBox(App, Gui,
                             docName, cutMe,
                             L, W, H, x, y, ppH,
                             r0, r1, r2, r3,
                             edges, radius):
    
    # Create box to use to cut away at body
    App.ActiveDocument.addObject("Part::Box","Cutter")
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")
    FreeCAD.getDocument(docName).getObject("Cutter").Length = L
    FreeCAD.getDocument(docName).getObject("Cutter").Width = W
    FreeCAD.getDocument(docName).getObject("Cutter").Height = H
    FreeCAD.getDocument(docName).getObject("Cutter").Placement = App.Placement(App.Vector(x, y, ppH),App.Rotation(r0, r1, r2, r3))

    Gui.activateWorkbench("PartWorkbench")

    # See if we need to filet the cutting box
    if (radius != 0.0) :

        # Fillet the selected edges of this cutting box
        FC3DM_FilletObjectEdges(App, Gui,
                                docName, "Cutter", edges, radius)

    # Perform cut
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(docName)
    App.ActiveDocument=App.getDocument(docName)
    Gui.ActiveDocument=Gui.getDocument(docName)
    App.activeDocument().addObject("Part::Cut","Cut000")
    App.activeDocument().Cut000.Base = FreeCAD.getDocument(docName).getObject(cutMe)
    App.activeDocument().Cut000.Tool = App.activeDocument().Cutter
    Gui.activeDocument().hide(cutMe)
    Gui.activeDocument().hide("Cutter")
    App.ActiveDocument.recompute()

    # Remove the objects that made up the cut
    App.getDocument(docName).removeObject(cutMe)
    App.getDocument(docName).removeObject("Cutter")

    # Copy the cut object and call it the original cutMe name
    newTermShape = FreeCAD.getDocument(docName).getObject("Cut000").Shape.copy()
    newTermObj = App.activeDocument().addObject("Part::Feature",cutMe)
    newTermObj.Shape = newTermShape

    # Remove the cut itself
    App.getDocument(docName).removeObject("Cut000")

    return 0


###################################################################
# FC3DM_CutWithBox()
#	Function to cut an object with a box that we create for this purpose.
###################################################################
def FC3DM_CutWithBox(App, Gui,
                     docName, cutMe,
                     L, W, H, x, y, ppH,
                     r0, r1, r2, r3):


    edges = []
    radius = 0.0

    # Call FC3DM_CutWithFilletedBox() to do all the real work
    FC3DM_CutWithFilletedBox(App, Gui,
                             docName, cutMe,
                             L, W, H, x, y, ppH,
                             r0, r1, r2, r3,
                             edges, radius)
    
    return 0


###################################################################
# FC3DM_CutObjectWithToolAndKeepTool()
#	Function to cut an object with a tool object, but keep tool
# object when we're all done.
###################################################################
def FC3DM_CutObjectWithToolAndKeepTool(App, Gui,
                                       docName, cutMe, cuttingTool):
    
    # Copy the tool object
    newTermShape = FreeCAD.getDocument(docName).getObject(cuttingTool).Shape.copy()
    newTermObj = App.activeDocument().addObject("Part::Feature","newCuttingTool")
    newTermObj.Shape = newTermShape

    # Perform cut
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(docName)
    App.ActiveDocument=App.getDocument(docName)
    Gui.ActiveDocument=Gui.getDocument(docName)
    App.activeDocument().addObject("Part::Cut","Cut000")
    App.activeDocument().Cut000.Base = FreeCAD.getDocument(docName).getObject(cutMe)
    App.activeDocument().Cut000.Tool = FreeCAD.getDocument(docName).getObject("newCuttingTool")
    Gui.activeDocument().hide(cutMe)
    Gui.activeDocument().hide("newCuttingTool")
    App.ActiveDocument.recompute()

    # Remove the objects that made up the cut
    App.getDocument(docName).removeObject(cutMe)
    App.getDocument(docName).removeObject("newCuttingTool")

    # Copy the cut object and call it the original cutMe name
    newTermShape = FreeCAD.getDocument(docName).getObject("Cut000").Shape.copy()
    newTermObj = App.activeDocument().addObject("Part::Feature",cutMe)
    newTermObj.Shape = newTermShape

    # Remove the cut itself
    App.getDocument(docName).removeObject("Cut000")

    return 0


###################################################################
# FC3DM_CutObjectWithToolAndDiscardTool
#	Function to cut an object with a tool object, and discard
# tool object when we're all done.
###################################################################
def FC3DM_CutObjectWithToolAndDiscardTool(App, Gui,
                                          docName, cutMe, cuttingTool):

    # Call FC3DM_CutObjectWithToolAndKeepTool() to do all the real work
    FC3DM_CutObjectWithToolAndKeepTool(App, Gui,
                                       docName, cutMe, cuttingTool)

    # Remove the tool
    App.getDocument(docName).removeObject(cuttingTool)

    return 0
    


###################################################################
# FC3DM_TranslateObjectAndRotateAboutZ()
#	Function to translate an object in x,y and then rotate it about Z axis.
###################################################################
def FC3DM_TranslateObjectAndRotateAboutZ(App, Gui,
                                         docName, rotMe,
                                         x, y, rotDeg):

    # Convert to radians
    rot = math.radians(rotDeg)

    # Rotate about the Z-axis.  Do the specified x,y translations.
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(docName)
    App.ActiveDocument=App.getDocument(docName)
    Gui.ActiveDocument=Gui.getDocument(docName)
    FreeCAD.getDocument(docName).getObject(rotMe).Placement = App.Placement(App.Vector(x,y,0),App.Rotation(0,0,math.sin(rot/2),math.cos(rot/2)))

    return 0


###################################################################
# FC3DM_RotateObjectAboutZ()
#	Function to rotate an object about Z axis.
###################################################################
def FC3DM_RotateObjectAboutZ(App, Gui,
                             docName, rotMe, rotDeg):

    # Call FC3DM_TranslateObjectAndRotateAboutZ() to do all the real work
    FC3DM_TranslateObjectAndRotateAboutZ(App, Gui,
                                         docName, rotMe,
                                         0, 0, rotDeg)

    return 0


###################################################################
# FC3DM_CreateBox()
#	Function to create and place a box.
#
# Parameter names are per Mentor LP Wizard tool:
# L == width of body
# W == length of body
# H == height of body
# K == standoff height of body
#
# Other parameters
# x == x coordinate of body
# y == y coordinate of body
# rotDeg == rotation about Z axis, in degrees
###################################################################
def FC3DM_CreateBox(App, Gui,
                    L, W, H, K,
                    x, y, rotDeg, 
                    docName,
                    bodyName):
    
    # Constant pi
    pi = 3.141592654

    # Create box to model IC body
    App.ActiveDocument.addObject("Part::Box",bodyName)
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")

    # Set body size
    FreeCAD.getDocument(docName).getObject(bodyName).Length = L
    FreeCAD.getDocument(docName).getObject(bodyName).Width = W
    FreeCAD.getDocument(docName).getObject(bodyName).Height = (H-K)

    # Compute initial rotation about z axis
    rot = math.radians(rotDeg)

    # Center the body at (0,0), set standoff height, and set initial rotation about Z-axis
    FreeCAD.getDocument(docName).getObject(bodyName).Placement = App.Placement(App.Vector(x, y, K),App.Rotation(0,0,math.sin(rot/2),math.cos(rot/2)))
    
    return 0


###################################################################
# FC3DM_CreateAndCenterBox()
#	Function to create and center a box
#
# Parameter names are per Mentor LP Wizard tool:
# L == width of body
# W == length of body
# H == height of body
# K == standoff height of body
#
# Other parameters:
# rotDeg == rotation about Z axis, in degrees
###################################################################
def FC3DM_CreateAndCenterBox(App, Gui,
                             L, W, H, K,
                             rotDeg, 
                             docName,
                             bodyName):
    
    # Compute x and y coordinates
    # FIXME:  Do we have to account for rotation as we do this???
    x = -1*(L/2)
    y = -1*(W/2)

    # Call FC3DM_CreateBox() to do all the real work.
    FC3DM_CreateBox(App, Gui,
                    L, W, H, K,
                    x, y, rotDeg, 
                    docName,
                    bodyName)
    
    return 0


###################################################################
# FC3DM_CreateCylinderVert()
#	Function to create a vertical (oriented in Z-axis) cylinder.
###################################################################
def FC3DM_CreateCylinderVert(App, Gui,
                             docName, cylName, x, y, z, radius, height):

    Gui.activateWorkbench("PartWorkbench")
    App.ActiveDocument.addObject("Part::Cylinder",cylName)
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")
    FreeCAD.getDocument(docName).getObject(cylName).Radius = radius
    FreeCAD.getDocument(docName).getObject(cylName).Height = height
    FreeCAD.getDocument(docName).getObject(cylName).Placement = App.Placement(App.Vector(x,y,z),App.Rotation(0,0,0,1))

    return 0


###################################################################
# FC3DM_CreateIcBody()
# 	Function to create an IC body
#
# Parameter names are per Mentor LP Wizard tool:
# A == width of body
# B == length of body
# H == height of body
# K == standoff height of body
#
# Other parameters:
# maDeg == mold angle in degrees
# Hpph == height of high pivot point
# Hppl == height of low pivot point
# Frbody == fillet radius (for top and bottom faces)
# P1markOffset == Offset in X and Y from pin1 corner to start of pin 1 marker
###################################################################
def FC3DM_CreateIcBody(App, Gui,
                       parms,
                       docName):

    # Extract relevant parameter values from parms associative array
    # TODO:  Currently no error checking!
    A = parms["A"]
    B = parms["B"]
    H = parms["H"]
    K = parms["K"]
    maDeg = parms["maDeg"]
    Hpph = parms["Hpph"]
    Hppl = parms["Hppl"]
    Frbody = parms["Frbody"]
    P1markOffset = parms["P1markOffset"]
    P1markRadius = parms["P1markRadius"]
    P1markIndent = parms["P1markIndent"]
    markHeight = parms["markHeight"]
    bodyName = parms["bodyName"]
    pin1MarkName = parms["pin1MarkName"]
    newModelName = parms["newModelName"]

    # Figure out the footprintType for the current gullwing model.
    # Do this by extracting just the leading characters in the newModelName.
    # footprintType = echo $newModelName | sed 's/[0-9]+.*//g'
    footprintType = re.sub('[0-9]+.*', '', newModelName)
    print footprintType

    # For SOIC packages, chamfer the upper long edge along pin 1        
    if (footprintType == "SOIC"):

        # Retrieve chamfer size
        # FIXME:  Retrieve from ini file, rather than hardcoding here!
        P1chamferOffset = 0.25

    # Other packages have no chamfer of the body upper long edge along pin 1        
    else:
        P1chamferOffset = 0


    print "docName is :" + docName + ":"
    print "bodyName is:" + bodyName + ":"
    print "B is       :" + str(B) + ":"

    # Constant pi
    pi = 3.141592654

    # Mold angle (in radians)
    ma = math.radians(maDeg)

    # Chamfer angle in degrees and radians
    caDeg = 45
    ca = math.radians(caDeg)

    # The pin 1 chamfer is referenced to the cut-away body, not at the rectangular prism prior to all the cuts.
    # moldOffset === offset due to mold angle
    # tan (maDeg) = moldOffset / (H-Hpph)
    # moldOffset = (H-Hpph) * tan(maDeg)
    moldOffset = (H-Hpph) * math.tan(ma)

    # Configure active document
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(docName)
    App.ActiveDocument=App.getDocument(docName)
    Gui.ActiveDocument=Gui.getDocument(docName)

    # Create box to model IC body
    App.ActiveDocument.addObject("Part::Box",bodyName)
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")

    # Set body size
    App.ActiveDocument.getObject(bodyName).Length = B
    App.ActiveDocument.getObject(bodyName).Width = A
    App.ActiveDocument.getObject(bodyName).Height = (H-K)

    # Choose initial rotation of 90 degrees about z axis
    # We want pin 1 to be in the upper left corner to match the assumptions in Mentor LP Wizard
    rot = math.radians(90)

    # Center the body at (0,0), set standoff height, and set initial rotation of 90 degrees about Z-axis
    FreeCAD.getDocument(docName).getObject(bodyName).Placement = App.Placement(App.Vector(1*(A/2),-1*(B/2),K),App.Rotation(0,0,math.sin(rot/2),math.cos(rot/2)))


    ## Make 2 cuts to each side of body, to model mold angle

    # Perform a cut at the north side of the IC body (pivot point high)
    FC3DM_CutWithBox(App, Gui,
                     docName, bodyName,
                     B, B, B, -1*(A/2), 1*(B/2), Hpph,
                     math.sin(ma/2),0,0,math.cos(ma/2))

    # Perform a cut at the north side of the IC body (pivot point low)
    FC3DM_CutWithBox(App, Gui,
                     docName, bodyName,
                     B, B, B, -1*(A/2), 1*(B/2), Hppl,
                     math.sin(((3*pi)/4)-(ma/2)),0,0,math.cos(((3*pi)/4)-(ma/2)))


    # Rotate the IC body 180 degrees about the z axis
    FC3DM_RotateObjectAboutZ(App, Gui,
                             docName, bodyName, 180)

    # Perform a cut at the north side of the IC body (pivot point high)
    FC3DM_CutWithBox(App, Gui,
                     docName, bodyName,
                     B, B, B, -1*(A/2), 1*(B/2), Hpph,
                     math.sin(ma/2),0,0,math.cos(ma/2))

    # Perform a cut at the north side of the IC body (pivot point low)
    FC3DM_CutWithBox(App, Gui,
                     docName, bodyName,
                     B, B, B, -1*(A/2), 1*(B/2), Hppl,
                     math.sin(((3*pi)/4)-(ma/2)),0,0,math.cos(((3*pi)/4)-(ma/2)))


    # Rotate the IC body 90 degrees about the z axis
    FC3DM_RotateObjectAboutZ(App, Gui,
                             docName, bodyName, 90)

    # Perform a cut at the north side of the IC body (pivot point high)
    FC3DM_CutWithBox(App, Gui,
                     docName, bodyName,
                     B, B, B, -1*(B/2), 1*(A/2), Hpph,
                     math.sin(ma/2),0,0,math.cos(ma/2))

    # Perform a cut at the north side of the IC body (pivot point low)
    FC3DM_CutWithBox(App, Gui,
                     docName, bodyName,
                     B, B, B, -1*(B/2), 1*(A/2), Hppl,
                     math.sin(((3*pi)/4)-(ma/2)),0,0,math.cos(((3*pi)/4)-(ma/2)))

    
    # Rotate the IC body 180 degrees about the z axis
    FC3DM_RotateObjectAboutZ(App, Gui,
                             docName, bodyName, 180)

    # Perform a cut at the north side of the IC body (pivot point high)
    FC3DM_CutWithBox(App, Gui,
                     docName, bodyName,
                     B, B, B, -1*(B/2), 1*(A/2), Hpph,
                     math.sin(ma/2),0,0,math.cos(ma/2))


    # See if we need to do the pin 1 chamfer on the body
    if (P1chamferOffset > 0):

        # TODO:  Add sanity check to ensure that the chamfer doesn't cut into the body below the
        # level of where the top-most part of the pin enters the body.  If it does, then we
        # would break our underlying assumption that the pins are symmetric side-to-side.
        # Detect this and abort on error!

        # Perform a cut at the north side of the IC body (chamfer on pin 1 long side of body)
        FC3DM_CutWithBox(App, Gui,
                         docName, bodyName,
                         B, B, B, -1*(B/2), (1*(A/2) - moldOffset - P1chamferOffset), H,
                         math.sin(-1*(ca/2)),0,0,math.cos(ca/2))

    # Perform a cut at the north side of the IC body (pivot point low)
    FC3DM_CutWithBox(App, Gui,
                     docName, bodyName,
                     B, B, B, -1*(B/2), 1*(A/2), Hppl,
                     math.sin(((3*pi)/4)-(ma/2)),0,0,math.cos(((3*pi)/4)-(ma/2)))

    
    # Do final rotation of the IC body 90 degrees about the z axis
    FC3DM_RotateObjectAboutZ(App, Gui,
                             docName, bodyName, 90)


    ## Attempt to analyze the faces in the body, to find which ones to fillet.
    # Loop over all the faces in this pin.
    print("Here are the edges!")
    numEdges = len(App.ActiveDocument.getObject(bodyName).Shape.Edges)
    print(" Number of edges is " + str(numEdges))

    # Workaround for the fact that edge.Name doesn't work.
    # Since we now know the number of edges, and we know FC's naming convention, we shall
    # iterate through the edges by name, rather than by reference.
#    for edgeNum in range(1, numEdges):

#        edgeName = "Edge" + str(edgeNum)
#        print("Examining " + edgeName)

#        edge = App.ActiveDocument.getObject(edgeName)


    # Attempt to iterate over all the edges in the shape.
    # For each edge, analyze its vertexes and find ones that are on the top or bottom faces.
    # Then select such edges for filleting.
    # The problem is that I can't find a way to extract the edge name.
    # Thus, this is currently useless.
    for edge in App.ActiveDocument.getObject(bodyName).Shape.Edges:
        print edge #.PropertiesList #Label #str(edge)

        # Loop over all the vertexes in this edge
        for vertex in edge.Vertexes:
            
            # Write this vertex
            print(str(vertex.Point))

    # If we had a pin 1 chamfer, we only have 7 edges to fillet, not 8.
    if (P1chamferOffset > 0):

        # Set the faces that need to be filleted
        edges=["Edge4","Edge6","Edge9","Edge11","Edge14","Edge17","Edge21"]

    else:

        # Set the faces that need to be filleted
        edges=["Edge4","Edge6","Edge14","Edge11","Edge9","Edge17","Edge19","Edge20"]

    ## Fillet all non-chamfered edges on the top & bottom faces
    # Note:  Do them all at once, because I've had a hard time with finding edge names on
    # the top face after filleting the bottom face, and vice versa.
    # TODO:  This is currently hardcoded!
    # TODO:  This must be revisited for QFN, BGA, etc.!
    FC3DM_FilletObjectEdges(App, Gui,
                            docName, bodyName, edges, Frbody)


    ## Prepare to make pin 1 marker

    # Create a cylinder to use for cutting out for pin 1 marker
    cylName = "CylCuttingTool"
    FC3DM_CreateCylinderVert(App, Gui,
                             docName, cylName, (-1*(A/2))+moldOffset+P1chamferOffset+Frbody+P1markOffset+P1markRadius, (B/2)-moldOffset-Frbody-P1markOffset-P1markRadius, (H-P1markIndent), P1markRadius, H)

    # Use this dummy cylinder to cut into the IC body
    FC3DM_CutObjectWithToolAndDiscardTool(App, Gui,
                                          docName, bodyName, cylName)

    # Apply pin 1 marker ink inside cut
    FC3DM_CreateCylinderVert(App, Gui,
                             docName, pin1MarkName, (-1*(A/2))+moldOffset+P1chamferOffset+Frbody+P1markOffset+P1markRadius, (B/2)-moldOffset-Frbody-P1markOffset-P1markRadius, (H-P1markIndent), P1markRadius, markHeight)


    return 0


###################################################################
# FC3DM_CreateIcPin()
#	Function to create a gullwing IC pin.
#
# Parameter names are per Mentor LP Wizard tool:
# L == Overall component width (pin tip to pin tip)
# A == Overall body width
# B == Overall body length
# W == Pin width
# T == Pin landing length
#
# Other parameters:
# Tp == Pin thickness (z dimension)
# Fr == Fillet radius for pin edges
# Hpe == Height of pin entry to body (center)
###################################################################
def FC3DM_CreateIcPin(App, Gui,
                      parms,
                      docName):
                
    # Extract relevant parameter values from parms associative array
    # TODO:  Currently no error checking!
    L = parms["L"]
    A = parms["A"]
    B = parms["B"]
    W = parms["W"]
    T = parms["T"]
    Tp = parms["Tp"]
    Fr = parms["Fr"]
    Hpe = parms["Hpe"]
    maDeg = parms["maDeg"]
    Hpph = parms["Hpph"]
    Hppl = parms["Hppl"]
    pinName = parms["pinName"]
    bodyName = parms["bodyName"]
    
    # Configure active document
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(docName)
    App.ActiveDocument=App.getDocument(docName)
    Gui.ActiveDocument=Gui.getDocument(docName)

    # Create box to model IC pin
    App.ActiveDocument.addObject("Part::Box",pinName)
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")

    # Set pin size
    FreeCAD.getDocument(docName).getObject(pinName).Length = (L/2.0)
    FreeCAD.getDocument(docName).getObject(pinName).Width = W
    FreeCAD.getDocument(docName).getObject(pinName).Height = (Hpe + (Tp/2.0))

    # Center the pin at (0,0), set standoff height, and set initial rotation of 0 degrees about Z-axis
    rot = math.radians(0)
    FreeCAD.getDocument(docName).getObject(pinName).Placement = App.Placement(App.Vector(0,-1*(W/2),0),App.Rotation(0,0,math.sin(rot/2),math.cos(rot/2)))

    # Cut away top-right part of the IC pin solid
    edges=["Edge4"]
    radius=0.3*Fr	# FIXME:  How to compute the inner radius (here) as a function of outer radius (Fr)???
    FC3DM_CutWithFilletedBox(App, Gui,
                             docName, pinName,
                             L, L, L, (L/2)-T+Tp, -1*(W/2), Tp,
                             0, 0, 0, 0,
                             edges, radius)
    
    # Cut away lower-left part of the IC pin solid
    edges=["Edge6"]
    FC3DM_CutWithFilletedBox(App, Gui,
                             docName, pinName,
                             (L/2)-T, W, Hpe-(Tp/2.0), 0, -1*(W/2), 0,
                             0, 0, 0, 0,
                             edges, radius)

    # Fillet (round) some of the gullwing pin edges
    edges=["Edge4","Edge30"]
    FC3DM_FilletObjectEdges(App, Gui,
                            docName, pinName, edges, Fr)

    # Cut away the part of the pin that disappears inside IC body
    FC3DM_CutObjectWithToolAndKeepTool(App, Gui,
                                       docName, pinName, bodyName)    

    # Zoom in on pin model
    Gui.SendMsgToActiveView("ViewFit")

    # Color pin red.  FIXME--remove this!
    Gui.getDocument(docName).getObject(pinName).ShapeColor = (1.00,0.00,0.00)

    return 0


###################################################################
# FC3DM_CreateIcPins()
#	Function to create all gullwing IC pins.
###################################################################
def FC3DM_CreateIcPins(App, Gui,
                       parms, pinNames,
                       docName):
                
    print("Hello from FC3DM_CreateIcPins()!")

    # Call CreateIcPin() to create first (template) IC pin
    FC3DM_CreateIcPin(App, Gui,
                      parms,
                      docName)

    print("Back from FC3DM_CreateIcPin()")

    # Extract relevant parameter values from parms associative array
    # TODO:  Currently no error checking!
    pinName = parms["pinName"]

    # Configure active document
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(docName)
    App.ActiveDocument=App.getDocument(docName)
    Gui.ActiveDocument=Gui.getDocument(docName)

    # Loop over all the entries in the parms array
    for parm in parms:

        print("Examining parm " + parm + ".")

        # See if this parm is a pin definition
        if parm.startswith("Pin"):

            # Add this pin to the pinNames list.
            print("Found pin named " + parm + ".")
            pinNames.append(parm)

    # Sort the list of pinNames, using our custom pin name sort function.
    pinNames.sort(FC3DM_SortPinNames)

    print("pinNames is:")
    print(pinNames)

    # Loop over all the pin names.
    for pin in pinNames:

        # Split the pin definition string on ',' chars.
        # Format is "type, side, x, y"
        lis = []
        lis = parms[pin].rsplit(",")

        # Sanity check that we have exactly 4 fields in the list
        if (len(lis) != 4):
            print("Expected to find 4 fields in pin description.  Actually saw " + str(len(lis)) + "!")
            FC3DM_MyExit(-1)

        print("Found pin named " + pin + ", lis is:")
        print(lis)

        ## Examine pin type
        if (lis[0] != "Gullwing"):
            print("Unsupported pin type " + lis[0])
            FC3DM_MyExit(-1)

        ## Examine pin side
        # Look for an east side pin.
        if (lis[1] == "East"):

            # Our template pin was created on the east side, so no rotation required.
            rotDeg = 0.0

        # Else see if this is a west side pin.
        elif (lis[1] == "West"):

            # Our template pin was created on the east side, so rotate 180 degrees
            rotDeg = 180.0

        # Else unsupported!
        else:
            print("Unsupported pin side " + lis[1])
            FC3DM_MyExit(-1)

        ## Get pin x coordinate
        # Currently we're ignoring this datum from the ini file.
        # The way we currently have things arranged, x is either 0.0 because this pin
        # is on the same (East) side as our template pin.  Or else it's 0.0 because this
        # pin will get rotated 180deg around to the West side.
        # TODO:  Revisit this for QFNs, QFPs, BGAs, etc.!
        x = 0.0

        ## Get pin y coordinate
        y = float(lis[3])

        ## Copy template pin to the commanded location
        FC3DM_CopyObject(App, Gui,
                         x, y, rotDeg,
                         docName,
                         pinName,
                         pin)

    # Remove the pin template object
    App.getDocument(docName).removeObject(pinName)

    return 0


###################################################################
# FC3DM_CopyObject()
#	Function to copy a FreeCAD object, then translate in x,y
# and rotate in z.
###################################################################
def FC3DM_CopyObject(App, Gui,
                     x, y, rotDeg,
                     docName,
                     objName,
                     newObjName):
    
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(docName)
    App.ActiveDocument=App.getDocument(docName)
    Gui.ActiveDocument=Gui.getDocument(docName)

    # Copy the object
    newTermShape = FreeCAD.getDocument(docName).getObject(objName).Shape.copy()
    newTermObj = App.activeDocument().addObject("Part::Feature",newObjName)
    newTermObj.Shape = newTermShape

    # Translate the copy in x,y and rotate about the z axis as needed.
    FC3DM_TranslateObjectAndRotateAboutZ(App, Gui,
                                         docName, newObjName,
                                         x, y, rotDeg)

    # Copy the original object's color to the new object
    Color = Gui.getDocument(docName).getObject(objName).ShapeColor
    Gui.getDocument(docName).getObject(newObjName).ShapeColor = Color
    
    return 0


###################################################################
# FC3DM_SaveAndExport()
#	Function to save a FreeCAD native CAD file, as well as export
# the specified objects to a STEP file.
###################################################################
def FC3DM_SaveAndExport(App, Gui,
                        docName,
                        parms,
                        objNameList):
    
    # Extract relevant parameter values from parms associative array
    # TODO:  Currently no error checking!
    newModelPathNameExt = parms["newModelPathNameExt"]
    newStepPathNameExt = parms["newStepPathNameExt"]

    ## Save to disk in native format
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(docName)
    App.ActiveDocument=App.getDocument(docName)
    Gui.ActiveDocument=Gui.getDocument(docName)
    App.getDocument(docName).FileName = newModelPathNameExt
    App.getDocument(docName).Label = docName
    Gui.SendMsgToActiveView("Save")
    App.getDocument(docName).save()
    
    ## Export to STEP
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(docName)
    App.ActiveDocument=App.getDocument(docName)
    Gui.ActiveDocument=Gui.getDocument(docName)
    App.getDocument(docName).save()

    # Create list of objects, starting with object names
    objs=[]
    for i in objNameList:
        
        objs.append(FreeCAD.getDocument(docName).getObject(i))

    # Do export to STEP
    import ImportGui
    ImportGui.export(objs,newStepPathNameExt)
    del objs
    
    return 0


###################################################################
# FC3DM_MyExit()
#	Function to write our return code to "rc file" and then exit.
###################################################################
def FC3DM_MyExit(rc):    

    ## Open the rc file
    fileP = open(scriptPathUtils + '\\python.rc', 'w')

    # Write return code to file
    fileP.write(str(rc))

    # Close the rc file
    fileP.close()

    # Actually do the sys.exit() call
    sys.exit(rc)

    return 0

