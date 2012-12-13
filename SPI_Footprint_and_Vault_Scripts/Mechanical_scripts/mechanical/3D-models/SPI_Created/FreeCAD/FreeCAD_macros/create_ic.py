#================================================================================================
#
#	@file			create_ic.py
#
#	@brief			Python script to create a gull-wing IC model in FreeCAD.
#
#	@details		
#
#    @version		0.1.11
#					   $Rev::                                                                        $:
#	@date			  $Date::                                                                        $:
#	@author			$Author::                                                                        $:
#					    $Id::                                                                        $:
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

###################################################################
# Function to fillet edges of a given object
#
###################################################################
def FilletObjectEdges(docName, filletMe, edges, radius):

    # Fillet the sharp edges of the termination sheet metal
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(docName)
    App.ActiveDocument=App.getDocument(docName)
    Gui.ActiveDocument=Gui.getDocument(docName)
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
def FuseObjects(docName, fuseMe, addMeToFusion):

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
# Function to cut an object with a box that we create for this purpose.
#
###################################################################
def CutWithFilletedBox(docName, cutMe,
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
        FilletObjectEdges(docName, "Cutter", edges, radius)

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
def CutWithBox(docName, cutMe,
               L, W, H, x, y, ppH,
               r0, r1, r2, r3):


    edges = []
    radius = 0.0

    # Call CutWithFilletedBox() to do all the real work
    CutWithFilletedBox(docName, cutMe,
                       L, W, H, x, y, ppH,
                       r0, r1, r2, r3,
                       edges, radius)
    
    return 0


###################################################################
# Function to cut an object with a tool object, but keep tool object
#  when we're all done.
#
###################################################################
def CutObjectWithToolAndKeepTool(docName, cutMe, cuttingTool):
    
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
def CutObjectWithToolAndDiscardTool(docName, cutMe, cuttingTool):

    # Call CutObjectWithToolAndKeepTool() to do all the real work
    CutObjectWithToolAndKeepTool(docName, cutMe, cuttingTool)

    # Remove the tool
    App.getDocument(docName).removeObject(cuttingTool)

    return 0
    


###################################################################
# Function to translate an object in x,y and then rotate it about Z axis
#
###################################################################
def TranslateObjectAndRotateAboutZ(docName, rotMe,
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
def RotateObjectAboutZ(docName, rotMe, rotDeg):

    # Call TranslateObjectAndRotateAboutZ() to do all the real work
    TranslateObjectAndRotateAboutZ(docName, rotMe,
                                   0, 0, rotDeg)

    return 0


###################################################################
# Function to create a vertical (oriented in Z-axis) cylinder
#
###################################################################
def CreateCylinderVert(docName, cylName, x, y, z, radius, height):

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
def CreateIcBody(A, B, H, K,
                 maDeg, Hpph, Hppl,
                 Frbody, P1markOffset, P1markRadius, P1markIndent, markHeight, 
                 docName,
                 bodyName,
                 pin1MarkName):

    # Constant pi
    pi = 3.141592654

    # Mold angle (in radians)
    ma = math.radians(maDeg)

    # Create new document
    App.newDocument(docName)
    App.setActiveDocument(docName)
    App.ActiveDocument=App.getDocument(docName)
    Gui.ActiveDocument=Gui.getDocument(docName)
    
    # Create box to model IC body
    App.ActiveDocument.addObject("Part::Box",bodyName)
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")

    # Set body size
    FreeCAD.getDocument(docName).getObject(bodyName).Length = B
    FreeCAD.getDocument(docName).getObject(bodyName).Width = A
    FreeCAD.getDocument(docName).getObject(bodyName).Height = (H-K)

    # Choose initial rotation of 90 degrees about z axis
    # We want pin 1 to be in the upper left corner to match the assumptions in Mentor LP Wizard
    rot = math.radians(90)

    # Center the body at (0,0), set standoff height, and set initial rotation of 90 degrees about Z-axis
    FreeCAD.getDocument(docName).getObject(bodyName).Placement = App.Placement(App.Vector(1*(A/2),-1*(B/2),K),App.Rotation(0,0,math.sin(rot/2),math.cos(rot/2)))


    ## Make 2 cuts to each side of body, to model mold angle

    # Perform a cut at the north side of the IC body (pivot point high)
    CutWithBox(docName, bodyName,
               B, B, B, -1*(A/2), 1*(B/2), Hpph,
               math.sin(ma/2),0,0,math.cos(ma/2))

    # Perform a cut at the north side of the IC body (pivot point low)
    CutWithBox(docName, bodyName,
               B, B, B, -1*(A/2), 1*(B/2), Hppl,
               math.sin(((3*pi)/4)-(ma/2)),0,0,math.cos(((3*pi)/4)-(ma/2)))


    # Rotate the IC body 180 degrees about the z axis
    RotateObjectAboutZ(docName, bodyName, 180)

    # Perform a cut at the north side of the IC body (pivot point high)
    CutWithBox(docName, bodyName,
               B, B, B, -1*(A/2), 1*(B/2), Hpph,
               math.sin(ma/2),0,0,math.cos(ma/2))

    # Perform a cut at the north side of the IC body (pivot point low)
    CutWithBox(docName, bodyName,
               B, B, B, -1*(A/2), 1*(B/2), Hppl,
               math.sin(((3*pi)/4)-(ma/2)),0,0,math.cos(((3*pi)/4)-(ma/2)))


    # Rotate the IC body 90 degrees about the z axis
    RotateObjectAboutZ(docName, bodyName, 90)

    # Perform a cut at the north side of the IC body (pivot point high)
    CutWithBox(docName, bodyName,
               B, B, B, -1*(B/2), 1*(A/2), Hpph,
               math.sin(ma/2),0,0,math.cos(ma/2))

    # Perform a cut at the north side of the IC body (pivot point low)
    CutWithBox(docName, bodyName,
               B, B, B, -1*(B/2), 1*(A/2), Hppl,
               math.sin(((3*pi)/4)-(ma/2)),0,0,math.cos(((3*pi)/4)-(ma/2)))

    
    # Rotate the IC body 180 degrees about the z axis
    RotateObjectAboutZ(docName, bodyName, 180)

    # Perform a cut at the north side of the IC body (pivot point high)
    CutWithBox(docName, bodyName,
               B, B, B, -1*(B/2), 1*(A/2), Hpph,
               math.sin(ma/2),0,0,math.cos(ma/2))

    # Perform a cut at the north side of the IC body (pivot point low)
    CutWithBox(docName, bodyName,
               B, B, B, -1*(B/2), 1*(A/2), Hppl,
               math.sin(((3*pi)/4)-(ma/2)),0,0,math.cos(((3*pi)/4)-(ma/2)))

    
    # Do final rotation of the IC body 90 degrees about the z axis
    RotateObjectAboutZ(docName, bodyName, 90)


    ## Fillet all edges on the top and bottom faces
    # If the 2 pivot points are the same Z coordinate, then we have fewer edges than normal
    if (Hpph == Hppl) :
        edges=["Edge15","Edge18","Edge19","Edge9","Edge4","Edge6","Edge14","Edge11"]

    else :
        edges=["Edge24","Edge27","Edge19","Edge26","Edge4","Edge6","Edge14","Edge11"]

    # Fillet edges on top and bottom faces
    FilletObjectEdges(docName, bodyName, edges, Frbody)


    ## Prepare to make pin 1 marker
    # The pin 1 marker is referenced to the cut-away body, not at the rectangular prism prior to all the cuts.
    # off === offset due to mold angle
    # tan (maDeg) = off / (H-Hpph)
    # off = (H-Hpph) * tan(maDeg)
    off = (H-Hpph) * math.tan(ma)

    # Create a cylinder to use for cutting out for pin 1 marker
    cylName = "CylCuttingTool"
    CreateCylinderVert(docName, cylName, (-1*(A/2))+off+Frbody+P1markOffset+P1markRadius, (B/2)-off-Frbody-P1markOffset-P1markRadius, (H-P1markIndent), P1markRadius, H)

    # Use this dummy cylinder to cut into the IC body
    CutObjectWithToolAndDiscardTool(docName, bodyName, cylName)

    # Apply pin 1 marker ink inside cut
    CreateCylinderVert(docName, pin1MarkName, (-1*(A/2))+off+Frbody+P1markOffset+P1markRadius, (B/2)-off-Frbody-P1markOffset-P1markRadius, (H-P1markIndent), P1markRadius, markHeight)
    
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
def CreateIcPin(L, A, B, 
                W, T, Tp, Fr, Hpe,
                maDeg, Hpph, Hppl,
                docName,
                pinName,
                bodyName):
                
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
    CutWithFilletedBox(docName, pinName,
                       L, L, L, (L/2)-T+Tp, -1*(W/2), Tp,
                       0, 0, 0, 0,
                       edges, radius)

    # Cut away lower-left part of the IC pin solid
    edges=["Edge6"]
    CutWithFilletedBox(docName, pinName,
                       (L/2)-T, W, Hpe-(Tp/2.0), 0, -1*(W/2), 0,
                       0, 0, 0, 0,
                       edges, radius)

    # Fillet (round) some of the gullwing pin edges
    edges=["Edge4","Edge30"]
    FilletObjectEdges(docName, pinName, edges, Fr)

    # Cut away the part of the pin that disappears inside IC body
    CutObjectWithToolAndKeepTool(docName, pinName, bodyName)    

    # Zoom in on pin model
    Gui.SendMsgToActiveView("ViewFit")
   
    return 0


###################################################################
# def CopyIcPin
###################################################################
def CopyIcPin(A, B, x, y, rotDeg,
              docName,
              pinName,
              newPinName):

    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(docName)
    App.ActiveDocument=App.getDocument(docName)
    Gui.ActiveDocument=Gui.getDocument(docName)

    # Copy the object
    newTermShape = FreeCAD.getDocument(docName).getObject(pinName).Shape.copy()
    newTermObj = App.activeDocument().addObject("Part::Feature",newPinName)
    newTermObj.Shape = newTermShape

    # Translate the copy in x,y and rotate about the z axis as needed.
    TranslateObjectAndRotateAboutZ(docName, newPinName,
                                   x, y, rotDeg)

    return 0


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
newModelName = "SOT95P280X145-5N_Fairchild_MA05B_Blk_BLNK_IPC_LPW"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = "Body"
pinName = "Pin"
pinsName = "Pins"
pin1MarkName = "Pin1Mark"

# Strip out all "-" characters for use as the FreeCAD document name
docName = string.replace(newModelName, "-", "_")

## Parameters off datasheet
L = 3.00
T = 0.55
W = 0.50
A = 1.70
B = 3.00
H = 1.45
K = 0.05

## Parameters that we have to make up, since they're not usually specified on datasheet

# Mold angle (in degrees) (not specified in datasheet--make something up)
maDeg = 12

# Pivot points (in Z) for chamfering the IC body
Hpph = (0.6*H)
Hppl = Hpph #(0.5*H)

# Thickness of pin (in Z)
Tp = 0.2

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

# Call CreateIcBody() to do create the plastic molded IC body
CreateIcBody(A, B, H, K,
             maDeg, Hpph, Hppl,
             Frbody, P1markOffset, P1markRadius, P1markIndent, markHeight, 
             docName,
             bodyName,
             pin1MarkName)


# Call CreateIcPin() to create first (template) IC pin
CreateIcPin(L, A, B, 
            W, T, Tp, Fr, Hpe,
            maDeg, Hpph, Hppl,
            docName,
            pinName,
            bodyName)

# Copy IC pins to other locations
x = 0.0
y = 0.95
rotDeg = 0.0
CopyIcPin(A, B, x, y, rotDeg,
          docName,
          pinName,
          "Pin5")

x = 0.0
y = -0.95
rotDeg = 0.0
CopyIcPin(A, B, x, y, rotDeg,
          docName,
          pinName,
          "Pin4")

x = 0.0
y = 0
rotDeg = 180
CopyIcPin(A, B, x, y, rotDeg,
          docName,
          pinName,
          "Pin2")

x = 0.0
y = 0.95
rotDeg = 180
CopyIcPin(A, B, x, y, rotDeg,
          docName,
          pinName,
          pinsName) # "Pin1"

x = 0.0
y = -0.95
rotDeg = 180
CopyIcPin(A, B, x, y, rotDeg,
          docName,
          pinName,
          "Pin3")


## Fuse all the pins together
FuseObjects(docName, pinsName, "Pin2")
FuseObjects(docName, pinsName, "Pin3")
FuseObjects(docName, pinsName, "Pin4")
FuseObjects(docName, pinsName, "Pin5")

## Remove original pin template
App.getDocument(docName).removeObject(pinName)


## Wrap up
# Color body black
FreeCADGui.getDocument(docName).getObject(bodyName).ShapeColor = (0.10,0.10,0.10)

# Color pins bright tin
FreeCADGui.getDocument(docName).getObject(pinsName).ShapeColor = (0.80,0.80,0.75)

# Color Pin 1 marker white
FreeCADGui.getDocument(docName).getObject(pin1MarkName).ShapeColor = (1.00,1.00,1.00)

# Save to disk in native format
App.ActiveDocument=None
Gui.ActiveDocument=None
App.setActiveDocument(docName)
App.ActiveDocument=App.getDocument(docName)
Gui.ActiveDocument=Gui.getDocument(docName)
App.getDocument(docName).FileName = newModelPathNameExt
App.getDocument(docName).Label = docName
Gui.SendMsgToActiveView("Save")
App.getDocument(docName).save()

# Export to STEP
App.ActiveDocument=None
Gui.ActiveDocument=None
App.setActiveDocument(docName)
App.ActiveDocument=App.getDocument(docName)
Gui.ActiveDocument=Gui.getDocument(docName)
App.getDocument(docName).save()
__objs__=[]
__objs__.append(FreeCAD.getDocument(docName).getObject(bodyName))
__objs__.append(FreeCAD.getDocument(docName).getObject(pinsName))
__objs__.append(FreeCAD.getDocument(docName).getObject(pin1MarkName))
import ImportGui
ImportGui.export(__objs__,newStepPathNameExt)
del __objs__


####    # Close all documents
#####    App.closeDocument(newModelName)
#####    App.setActiveDocument("")
#####    App.ActiveDocument=None
#####    Gui.ActiveDocument=None

