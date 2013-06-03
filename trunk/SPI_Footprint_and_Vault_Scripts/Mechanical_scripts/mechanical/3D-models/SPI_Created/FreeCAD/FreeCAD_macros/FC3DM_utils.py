#================================================================================================
#
#	@file			FC3DM_utils.py
#
#	@brief			Python script functions to help create electronics 3D models in FreeCAD.
#
#	@details		
#
#    @version		0.2.0
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

###################################################################
# Function to fillet edges of a given object
#
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
# Function to fuse two objects together
#
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
# Function to fuse a set of objects together
#
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
    __objs__=[]
    for i in objNameList:
        
        __objs__.append(App.activeDocument().getObject(i))

    # Prepare to do the multi-fusion
    Gui.activateWorkbench("PartWorkbench")
    App.activeDocument().addObject("Part::MultiFuse","Temp")
    App.activeDocument().getObject("Temp").Shapes = __objs__
    App.ActiveDocument.recompute()

    # TODO:  Preserve face colors for the body and pin1Mark!
    # Figure out which faces came from the body and pin1Mark and set colors accordingly.

    # Loop over all faces in the pin1Mark object
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

    # Loop over all faces in the body object
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


    ## Prepare to compare all faces in the fusion with pin1Mark and body faces
    faceColors=[]

    # Loop over all the faces in the fusion object
    for xp in App.ActiveDocument.getObject("Temp").Shape.Faces:
        print("Found face in temp object!")

        isPin1Mark = False;
        isBody = False;

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

        # See if this face is the same as a face from the body object
        for i in bodyVerts:

            # I can't get Faces.isSame() to actually work.  That's why I'm doing this
            # kludge with sprint'ing vertex info to a string.
#            print xp.isSame(i)
        
            # See if the i string (from a body vertex string) equals our current vertex string
            if (buf.getvalue() == i):
                isBody = True;

            # TODO:  We need a fuzzy match to compare the IC body sides to the fusion sides.
            # Currently this doesn't fit an exact match, since the body doesn't have vertexes
            # for where all the pins hit, but the fusion does!
                

        # See if we found a face that derives from our pin1Mark
        if (isPin1Mark):

            # Color Pin1Mark white
            faceColors.append((1.00,1.00,1.00))

        # Else it's not derived from our pin1Mark
        else:

            # See if we found a face that derives from our body
            if (isBody):

                # Color body black
                faceColors.append((0.10,0.10,0.10))

            # Else this face is part of a pin.
            else:

                # Color pin bright tin.
                faceColors.append((0.80,0.80,0.75))
            

    # Now that we have a list of all the face colors that we want, proceed to apply it
    # to the fusion shape.
    # A special thanks here to FC guru wmayer for his forum post at
    # https://sourceforge.net/apps/phpbb/free-cad/viewtopic.php?f=19&t=4117
    # which pointed me in the right direction to make this actually work.                
    print("Attempting to set temp face colors")
    Gui.ActiveDocument.getObject("Temp").DiffuseColor=faceColors
    App.ActiveDocument.recompute()
    print("Attempted to set temp face colors")

    # Copy the temp fusion object and call it the desired fusion name
    newTermShape = FreeCAD.getDocument(docName).getObject("Temp").Shape.copy()
    newTermObj = App.activeDocument().addObject("Part::Feature", fusionName)
    newTermObj.Shape = newTermShape
    App.ActiveDocument.recompute()

    Gui.ActiveDocument.getObject(fusionName).DiffuseColor=faceColors
    App.ActiveDocument.recompute()


    # Delete the original objects that comprised the fusion
    for i in objNameList:

        App.activeDocument().removeObject(i)

    # Remove the temp fusion
    App.getDocument(docName).removeObject("Temp")
    App.ActiveDocument.recompute()

    # Deallocate list
    del __objs__

    return 0


###################################################################
# Function to cut an object with a filleted box that we create for this purpose.
#
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
# Function to cut an object with a box that we create for this purpose.
#
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
# Function to cut an object with a tool object, but keep tool object
#  when we're all done.
#
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
# Function to cut an object with a tool object, but keep tool object
#  when we're all done.
#
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
# Function to translate an object in x,y and then rotate it about Z axis
#
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
# Function to rotate an object about Z axis
#
###################################################################
def FC3DM_RotateObjectAboutZ(App, Gui,
                             docName, rotMe, rotDeg):

    # Call FC3DM_TranslateObjectAndRotateAboutZ() to do all the real work
    FC3DM_TranslateObjectAndRotateAboutZ(App, Gui,
                                         docName, rotMe,
                                         0, 0, rotDeg)

    return 0


###################################################################
# Function to create and place a box
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
#
# def FC3DM_CreateBox
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
# Function to create and place a box
#
# Parameter names are per Mentor LP Wizard tool:
# L == width of body
# W == length of body
# H == height of body
# K == standoff height of body
#
# Other parameters
# rotDeg == rotation about Z axis, in degrees
#
# def FC3DM_CreateAndCenterBox
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
# Function to create a vertical (oriented in Z-axis) cylinder
#
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
# Function to create an IC body
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
#
# def CreateIcBody
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

#    markHeight = parms["foo"]

    print "docName is :" + docName + ":"
    print "bodyName is:" + bodyName + ":"
    print "B is       :" + str(B) + ":"

    # Constant pi
    pi = 3.141592654

    # Mold angle (in radians)
    ma = math.radians(maDeg)

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

    # Perform a cut at the north side of the IC body (pivot point low)
    FC3DM_CutWithBox(App, Gui,
                     docName, bodyName,
                     B, B, B, -1*(B/2), 1*(A/2), Hppl,
                     math.sin(((3*pi)/4)-(ma/2)),0,0,math.cos(((3*pi)/4)-(ma/2)))

    
    # Do final rotation of the IC body 90 degrees about the z axis
    FC3DM_RotateObjectAboutZ(App, Gui,
                             docName, bodyName, 90)


    # Fillet all edges on the bottom faces
    edges=["Edge4","Edge6","Edge14","Edge11"]
    FC3DM_FilletObjectEdges(App, Gui,
                            docName, bodyName, edges, Frbody)



## I can't get the filleting of the top faces to work at all right now.  Commenting out.

##	edges=[]
##    edges.append("Edge20")
#
#    edges=["Edge16"]
#
#    # Fillet edges on top faces
##    FC3DM_FilletObjectEdges(App, Gui,
##                            docName, bodyName, edges, Frbody)
##    del __edges__
#
##    edges=["Edge1"]
#
#    # Fillet edges on top faces
##    FC3DM_FilletObjectEdges(App, Gui,
##                            docName, bodyName, edges, Frbody)
#
#
#    ## Fillet all edges on the top faces
#    # If the 2 pivot points are the same Z coordinate, then we have fewer edges than normal
#    if (Hpph == Hppl) :
#        edges2=["Edge24"]
#
#    else :
#        edges2=["Edge27"] #["Edge28","Edge27","Edge12"] #["Edge20"] #["Edge25"] #,"Edge20","Edge28","Edge29"] # FIXME:  These are certainly wrong!
#
#    # Fillet edges on top faces
#    FC3DM_FilletObjectEdges(App, Gui,
#                            docName, bodyName, edges2, Frbody)
#
#
##    del edges
##	edges=[]
##    edges.append("Edge5") #["Edge25"] #,"Edge20","Edge28","Edge29"] # FIXME:  These are certainly wrong!
#
#    # Fillet edges on top faces
#    edges=["Edge5"]
##    FC3DM_FilletObjectEdges(App, Gui,
##                            docName, bodyName, edges, Frbody)



    ## Prepare to make pin 1 marker
    # The pin 1 marker is referenced to the cut-away body, not at the rectangular prism prior to all the cuts.
    # off === offset due to mold angle
    # tan (maDeg) = off / (H-Hpph)
    # off = (H-Hpph) * tan(maDeg)
    off = (H-Hpph) * math.tan(ma)

    # Create a cylinder to use for cutting out for pin 1 marker
    cylName = "CylCuttingTool"
    FC3DM_CreateCylinderVert(App, Gui,
                             docName, cylName, (-1*(A/2))+off+Frbody+P1markOffset+P1markRadius, (B/2)-off-Frbody-P1markOffset-P1markRadius, (H-P1markIndent), P1markRadius, H)

    # Use this dummy cylinder to cut into the IC body
    FC3DM_CutObjectWithToolAndDiscardTool(App, Gui,
                                          docName, bodyName, cylName)

    # Apply pin 1 marker ink inside cut
    FC3DM_CreateCylinderVert(App, Gui,
                             docName, pin1MarkName, (-1*(A/2))+off+Frbody+P1markOffset+P1markRadius, (B/2)-off-Frbody-P1markOffset-P1markRadius, (H-P1markIndent), P1markRadius, markHeight)


    return 0


###################################################################
# L == Overall component width (pin tip to pin tip)
# A == Overall body width
# B == Overall body length
# W == Pin width
# T == Pin landing length
# Tp == Pin thickness (z dimension)
# Fr == Fillet radius for pin edges
# Hpe == Height of pin entry to body (center)
#
# def CreateIcPin
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

    # Color pin bright tin
#    Gui.getDocument(docName).getObject(pinsName).ShapeColor = (0.80,0.80,0.75)

    return 0


###################################################################
# def CopyObject
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
    
#    Color = App.ActiveDocument.getObject(objName).ShapeColor
#    App.ActiveDocument.getObject(newObjName).ShapeColor = Color
    

    return 0


###################################################################
# def SaveAndExport
###################################################################
def FC3DM_SaveAndExport(App, Gui,
                        docName,
                        newModelPathNameExt,
                        newStepPathNameExt,
                        objNameList):
    
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
    __objs__=[]
    for i in objNameList:
        
        __objs__.append(FreeCAD.getDocument(docName).getObject(i))

    # Do export to STEP
    import ImportGui
    ImportGui.export(__objs__,newStepPathNameExt)
    del __objs__
    
    return 0
