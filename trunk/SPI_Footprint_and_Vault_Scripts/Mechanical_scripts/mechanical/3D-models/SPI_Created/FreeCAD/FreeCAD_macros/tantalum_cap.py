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


###################################################################
# Function to create a tantalum capacitor body
# def CreateTantalumCapacitor
###################################################################
def CreateTantalumCapacitor(L, W, H, F, S, B, X, P, R, T, A, G, E,
                            Tf, maDeg, Lamb, Hamb,
                            newModelName,
                            newModelPathNameExt,
                            newStepPathNameExt,
                            bodyName,
                            pinsName,
                            pin1MarkName):
    
    # Constant pi
    pi = 3.141592654

    # Mold angle (in radians)
    ma = math.radians(maDeg)

    # Length of body after we subtract off 2 termination sheet metal thicknesses
    Lb = (L - (2*T))

    # Pivot point height (not specified in datasheet--make something up)
    ppH = (H * 0.45)

    # Create new document
    App.newDocument(newModelName)
    App.setActiveDocument(newModelName)
    App.ActiveDocument=App.getDocument(newModelName)
    Gui.ActiveDocument=Gui.getDocument(newModelName)
    
    # Create box to model tantalum capacitor body
    App.ActiveDocument.addObject("Part::Box","Box")
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")

    # Set body size
    FreeCAD.getDocument(newModelName).getObject("Box").Length = Lb
    FreeCAD.getDocument(newModelName).getObject("Box").Width = W
    FreeCAD.getDocument(newModelName).getObject("Box").Height = H
    FreeCAD.getDocument(newModelName).getObject("Box").Placement = App.Placement(App.Vector(0,0,0),App.Rotation(0,0,0,1))

    # Center the body at (0,0)
    FreeCAD.getDocument(newModelName).getObject("Box").Placement = App.Placement(App.Vector(-1*(Lb/2),-1*(W/2),0),App.Rotation(0,0,0,1))
    
    # Create box to use to cut away at body
    App.ActiveDocument.addObject("Part::Box","Box001")
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")
    FreeCAD.getDocument(newModelName).getObject("Box001").Length = L
    FreeCAD.getDocument(newModelName).getObject("Box001").Width = L
    FreeCAD.getDocument(newModelName).getObject("Box001").Height = L
    FreeCAD.getDocument(newModelName).getObject("Box001").Placement = App.Placement(App.Vector(-1*(Lb/2),(W/2),ppH),App.Rotation(math.sin(ma/2),0,0,math.cos(ma/2)))

    # Perform 1st cut
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(newModelName)
    App.ActiveDocument=App.getDocument(newModelName)
    Gui.ActiveDocument=Gui.getDocument(newModelName)
    App.activeDocument().addObject("Part::Cut","Cut")
    App.activeDocument().Cut.Base = App.activeDocument().Box
    App.activeDocument().Cut.Tool = App.activeDocument().Box001
    Gui.activeDocument().hide('Box')
    Gui.activeDocument().hide('Box001')
    App.ActiveDocument.recompute()

    # Create box to use to cut away at body
    App.ActiveDocument.addObject("Part::Box","Box002")
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")
    FreeCAD.getDocument(newModelName).getObject("Box002").Length = L
    FreeCAD.getDocument(newModelName).getObject("Box002").Width = L
    FreeCAD.getDocument(newModelName).getObject("Box002").Height = L
    FreeCAD.getDocument(newModelName).getObject("Box002").Placement = App.Placement(App.Vector(-1*(Lb/2),(W/2),ppH),App.Rotation(math.sin(((3*pi)/4)-(ma/2)),0,0,math.cos(((3*pi)/4)-(ma/2))))

    # Perform 2nd cut
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(newModelName)
    App.ActiveDocument=App.getDocument(newModelName)
    Gui.ActiveDocument=Gui.getDocument(newModelName)
    App.activeDocument().addObject("Part::Cut","Cut001")
    App.activeDocument().Cut001.Base = App.activeDocument().Cut
    App.activeDocument().Cut001.Tool = App.activeDocument().Box002
    Gui.activeDocument().hide('Cut')
    Gui.activeDocument().hide('Box002')
    App.ActiveDocument.recompute()

    ###################################################################
    # Rotate body to 180 degrees and get ready for next 2 cuts
    FreeCAD.getDocument(newModelName).getObject("Cut001").Placement = App.Placement(App.Vector(0,0,0),App.Rotation(0,0,1,0))
    ###################################################################

    # Create box to use to cut away at body
    App.ActiveDocument.addObject("Part::Box","Box003")
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")
    FreeCAD.getDocument(newModelName).getObject("Box003").Length = L
    FreeCAD.getDocument(newModelName).getObject("Box003").Width = L
    FreeCAD.getDocument(newModelName).getObject("Box003").Height = L
    FreeCAD.getDocument(newModelName).getObject("Box003").Placement = App.Placement(App.Vector(-1*(Lb/2),(W/2),ppH),App.Rotation(math.sin(ma/2),0,0,math.cos(ma/2)))

    ## Perform 3rd cut
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(newModelName)
    App.ActiveDocument=App.getDocument(newModelName)
    Gui.ActiveDocument=Gui.getDocument(newModelName)
    App.activeDocument().addObject("Part::Cut","Cut002")
    App.activeDocument().Cut002.Base = App.activeDocument().Cut001
    App.activeDocument().Cut002.Tool = App.activeDocument().Box003
    Gui.activeDocument().hide('Cut001')
    Gui.activeDocument().hide('Box003')
    App.ActiveDocument.recompute()

    # Create box to use to cut away at body
    App.ActiveDocument.addObject("Part::Box","Box004")
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")
    FreeCAD.getDocument(newModelName).getObject("Box004").Length = L
    FreeCAD.getDocument(newModelName).getObject("Box004").Width = L
    FreeCAD.getDocument(newModelName).getObject("Box004").Height = L
    FreeCAD.getDocument(newModelName).getObject("Box004").Placement = App.Placement(App.Vector(-1*(Lb/2),(W/2),ppH),App.Rotation(math.sin(((3*pi)/4)-(ma/2)),0,0,math.cos(((3*pi)/4)-(ma/2))))

    # Perform 4th cut
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(newModelName)
    App.ActiveDocument=App.getDocument(newModelName)
    Gui.ActiveDocument=Gui.getDocument(newModelName)
    App.activeDocument().addObject("Part::Cut","Cut003")
    App.activeDocument().Cut003.Base = App.activeDocument().Cut002
    App.activeDocument().Cut003.Tool = App.activeDocument().Box004
    Gui.activeDocument().hide('Cut002')
    Gui.activeDocument().hide('Box004')
    App.ActiveDocument.recompute()

    ###################################################################
    # Rotate body to 90 degrees and get ready for next 2 cuts
    FreeCAD.getDocument(newModelName).getObject("Cut003").Placement = App.Placement(App.Vector(0,0,0),App.Rotation(0,0,0.707107,0.707107))
    ###################################################################

    # Create box to use to cut away at body
    App.ActiveDocument.addObject("Part::Box","Box005")
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")
    FreeCAD.getDocument(newModelName).getObject("Box005").Length = L
    FreeCAD.getDocument(newModelName).getObject("Box005").Width = L
    FreeCAD.getDocument(newModelName).getObject("Box005").Height = L
    FreeCAD.getDocument(newModelName).getObject("Box005").Placement = App.Placement(App.Vector(-1*(W/2),(Lb/2),ppH),App.Rotation(math.sin(ma/2),0,0,math.cos(ma/2)))

    ## Perform 5th cut
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(newModelName)
    App.ActiveDocument=App.getDocument(newModelName)
    Gui.ActiveDocument=Gui.getDocument(newModelName)
    App.activeDocument().addObject("Part::Cut","Cut004")
    App.activeDocument().Cut004.Base = App.activeDocument().Cut003
    App.activeDocument().Cut004.Tool = App.activeDocument().Box005
    Gui.activeDocument().hide('Cut003')
    Gui.activeDocument().hide('Box005')
    App.ActiveDocument.recompute()

    # Create box to use to cut away at body
    App.ActiveDocument.addObject("Part::Box","Box006")
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")
    FreeCAD.getDocument(newModelName).getObject("Box006").Length = L
    FreeCAD.getDocument(newModelName).getObject("Box006").Width = L
    FreeCAD.getDocument(newModelName).getObject("Box006").Height = L
    FreeCAD.getDocument(newModelName).getObject("Box006").Placement = App.Placement(App.Vector(-1*(W/2),(Lb/2),ppH),App.Rotation(math.sin(((3*pi)/4)-(ma/2)),0,0,math.cos(((3*pi)/4)-(ma/2))))

    # Perform 6th cut
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(newModelName)
    App.ActiveDocument=App.getDocument(newModelName)
    Gui.ActiveDocument=Gui.getDocument(newModelName)
    App.activeDocument().addObject("Part::Cut","Cut005")
    App.activeDocument().Cut005.Base = App.activeDocument().Cut004
    App.activeDocument().Cut005.Tool = App.activeDocument().Box006
    Gui.activeDocument().hide('Cut004')
    Gui.activeDocument().hide('Box006')
    App.ActiveDocument.recompute()


    ###################################################################
    # Rotate body to 270 degrees and get ready for next 2 cuts
    FreeCAD.getDocument(newModelName).getObject("Cut005").Placement = App.Placement(App.Vector(0,0,0),App.Rotation(0,0,1,0))
    ###################################################################

    # Create box to use to cut away at body
    App.ActiveDocument.addObject("Part::Box","Box007")
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")
    FreeCAD.getDocument(newModelName).getObject("Box007").Length = L
    FreeCAD.getDocument(newModelName).getObject("Box007").Width = L
    FreeCAD.getDocument(newModelName).getObject("Box007").Height = L
    FreeCAD.getDocument(newModelName).getObject("Box007").Placement = App.Placement(App.Vector(-1*(W/2),(Lb/2),ppH),App.Rotation(math.sin(ma/2),0,0,math.cos(ma/2)))

    ## Perform 7th cut
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(newModelName)
    App.ActiveDocument=App.getDocument(newModelName)
    Gui.ActiveDocument=Gui.getDocument(newModelName)
    App.activeDocument().addObject("Part::Cut","Cut006")
    App.activeDocument().Cut006.Base = App.activeDocument().Cut005
    App.activeDocument().Cut006.Tool = App.activeDocument().Box007
    Gui.activeDocument().hide('Cut005')
    Gui.activeDocument().hide('Box007')
    App.ActiveDocument.recompute()

    # Create box to use to cut away at body
    App.ActiveDocument.addObject("Part::Box","Box008")
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")
    FreeCAD.getDocument(newModelName).getObject("Box008").Length = L
    FreeCAD.getDocument(newModelName).getObject("Box008").Width = L
    FreeCAD.getDocument(newModelName).getObject("Box008").Height = L
    FreeCAD.getDocument(newModelName).getObject("Box008").Placement = App.Placement(App.Vector(-1*(W/2),(Lb/2),ppH),App.Rotation(math.sin(((3*pi)/4)-(ma/2)),0,0,math.cos(((3*pi)/4)-(ma/2))))

    # Perform 8th cut
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(newModelName)
    App.ActiveDocument=App.getDocument(newModelName)
    Gui.ActiveDocument=Gui.getDocument(newModelName)
    App.activeDocument().addObject("Part::Cut","Cut007")
    App.activeDocument().Cut007.Base = App.activeDocument().Cut006
    App.activeDocument().Cut007.Tool = App.activeDocument().Box008
    Gui.activeDocument().hide('Cut006')
    Gui.activeDocument().hide('Box008')
    App.ActiveDocument.recompute()


    ###################################################################
    # Prepare to make the bevel cut at the anode end.
    ###################################################################

    # The anode bevel needs to start at the cut-away body, not at the rectangular prism prior to all the cuts.
    # x === offset due to 8 degree mold angle
    # tan (8deg) = x / (H-ppH)
    # x = (H-pph) * tan(8deg)
    x = (H-ppH) * math.tan(ma)

    # Create box to use to cut away at body
    App.ActiveDocument.addObject("Part::Box","Box009")
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")
    FreeCAD.getDocument(newModelName).getObject("Box009").Length = L
    FreeCAD.getDocument(newModelName).getObject("Box009").Width = L
    FreeCAD.getDocument(newModelName).getObject("Box009").Height = L
    FreeCAD.getDocument(newModelName).getObject("Box009").Placement = App.Placement(App.Vector(-1*(W/2),((Lb/2)-x-B),H),App.Rotation(-0.382683,0,0,0.92388))

    # Perform 9th cut
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(newModelName)
    App.ActiveDocument=App.getDocument(newModelName)
    Gui.ActiveDocument=Gui.getDocument(newModelName)
    App.activeDocument().addObject("Part::Cut","Cut008")
    App.activeDocument().Cut008.Base = App.activeDocument().Cut007
    App.activeDocument().Cut008.Tool = App.activeDocument().Box009
    Gui.activeDocument().hide('Cut007')
    Gui.activeDocument().hide('Box009')
    App.ActiveDocument.recompute()


    ###################################################################
    # Rotate cap body into final position and prepare to add terminations.
    ###################################################################

    # Rotate into final orientation
    FreeCAD.getDocument(newModelName).getObject("Cut008").Placement = App.Placement(App.Vector(0,0,0),App.Rotation(0,0,0.707107,0.707107))

    # Create box to use to cut away at underside of body
    App.ActiveDocument.addObject("Part::Box","Box010")
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")
    FreeCAD.getDocument(newModelName).getObject("Box010").Length = Lb
    FreeCAD.getDocument(newModelName).getObject("Box010").Width = W
    FreeCAD.getDocument(newModelName).getObject("Box010").Height = T
    FreeCAD.getDocument(newModelName).getObject("Box010").Placement = App.Placement(App.Vector(-1*(Lb/2),-1*(W/2),(-1*T)),App.Rotation(0,0,0,1))

    # Create box to use to cut a hole in that box to accommodate the G dimension
    App.ActiveDocument.addObject("Part::Box","Box011")
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")
    FreeCAD.getDocument(newModelName).getObject("Box011").Length = G
    FreeCAD.getDocument(newModelName).getObject("Box011").Width = E
    FreeCAD.getDocument(newModelName).getObject("Box011").Height = T
    FreeCAD.getDocument(newModelName).getObject("Box011").Placement = App.Placement(App.Vector(-1*(G/2),-1*(E/2),(-1*T)),App.Rotation(0,0,0,1))

    # Make cut
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(newModelName)
    App.ActiveDocument=App.getDocument(newModelName)
    Gui.ActiveDocument=Gui.getDocument(newModelName)
    App.activeDocument().addObject("Part::Cut","Cut009")
    App.activeDocument().Cut009.Base = App.activeDocument().Box010
    App.activeDocument().Cut009.Tool = App.activeDocument().Box011
    Gui.activeDocument().hide('Box010')
    Gui.activeDocument().hide('Box011')
    App.ActiveDocument.recompute()

    # Now that we have a template to use for all the material we need to remove underneath the cap, proceed to do so.

    # Position template coincident with cap
    FreeCAD.getDocument(newModelName).getObject("Cut009").Placement = App.Placement(App.Vector(0,0,T),App.Rotation(0,0,0,1))

    # Make cut
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(newModelName)
    App.ActiveDocument=App.getDocument(newModelName)
    Gui.ActiveDocument=Gui.getDocument(newModelName)
    App.activeDocument().addObject("Part::Cut","Cut011")
    App.activeDocument().Cut011.Base = App.activeDocument().Cut008
    App.activeDocument().Cut011.Tool = App.activeDocument().Cut009
    Gui.activeDocument().hide('Cut008')
    Gui.activeDocument().hide('Cut009')
    App.ActiveDocument.recompute()

    # Copy this object and call the copy "Body"
    newTermShape = FreeCAD.getDocument(newModelName).getObject("Cut011").Shape.copy()
    newTermObj = App.activeDocument().addObject("Part::Feature",bodyName)
    newTermObj.Shape = newTermShape


    ###################################################################
    # Add terminations
    ###################################################################

    # Create box to be left side (anode) termination
    App.ActiveDocument.addObject("Part::Box","Box012")
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")
    FreeCAD.getDocument(newModelName).getObject("Box012").Length = S
    FreeCAD.getDocument(newModelName).getObject("Box012").Width = F
    FreeCAD.getDocument(newModelName).getObject("Box012").Height = T
    FreeCAD.getDocument(newModelName).getObject("Box012").Placement = App.Placement(App.Vector(-1*(L/2),-1*(F/2),0),App.Rotation(0,0,0,1))

    # Create box to be left side (anode) termination
    App.ActiveDocument.addObject("Part::Box","Box013")
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")
    FreeCAD.getDocument(newModelName).getObject("Box013").Length = T
    FreeCAD.getDocument(newModelName).getObject("Box013").Width = F
    FreeCAD.getDocument(newModelName).getObject("Box013").Height = (ppH-T)
    FreeCAD.getDocument(newModelName).getObject("Box013").Placement = App.Placement(App.Vector(-1*(L/2),-1*(F/2),T),App.Rotation(0,0,0,1))

    # Fuse the two parts of termination together
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(newModelName)
    App.ActiveDocument=App.getDocument(newModelName)
    Gui.ActiveDocument=Gui.getDocument(newModelName)
    App.activeDocument().addObject("Part::MultiFuse","Fusion")
    App.activeDocument().Fusion.Shapes = [App.activeDocument().Box013,App.activeDocument().Box012,App.activeDocument().Box012]
    Gui.activeDocument().Box013.Visibility=False
    Gui.activeDocument().Box012.Visibility=False
    Gui.activeDocument().Box012.Visibility=False
    App.ActiveDocument.recompute()

    # Fillet the sharp edges of the termination sheet metal
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(newModelName)
    App.ActiveDocument=App.getDocument(newModelName)
    Gui.ActiveDocument=Gui.getDocument(newModelName)
    Gui.activateWorkbench("PartDesignWorkbench")
    App.activeDocument().addObject("PartDesign::Fillet","Fillet")
    App.activeDocument().Fillet.Base = (App.ActiveDocument.Fusion,["Edge1","Edge14"])
    Gui.activeDocument().hide("Fusion")
    Gui.activeDocument().Fusion.Visibility=False
    Gui.activeDocument().setEdit('Fillet')
    App.ActiveDocument.Fillet.Radius = Tf
    App.ActiveDocument.recompute()
    Gui.activeDocument().resetEdit()

    # Copy the termination to cathode
    newTermShape = FreeCAD.getDocument(newModelName).getObject("Fillet").Shape.copy()
    newTermObj = App.activeDocument().addObject("Part::Feature","Pin2")
    newTermObj.Shape = newTermShape
    FreeCAD.getDocument(newModelName).getObject("Pin2").Placement = App.Placement(App.Vector(0,0,0),App.Rotation(0,0,1,0))


    ###################################################################
    # Cut a notch in anode termination
    ###################################################################

    # If P parameter is 0, then no notch to cut
    if (P == 0) :
        
        # Copy fillet object and call the copy "Pin1"
        newTermShape = FreeCAD.getDocument(newModelName).getObject("Fillet").Shape.copy()
        newTermObj = App.activeDocument().addObject("Part::Feature","Pin1")
        newTermObj.Shape = newTermShape

    else:
    
        # Create box to use to cut away at anode termination
        App.ActiveDocument.addObject("Part::Box","Box014")
        App.ActiveDocument.recompute()
        Gui.SendMsgToActiveView("ViewFit")
        FreeCAD.getDocument(newModelName).getObject("Box014").Length = T
        FreeCAD.getDocument(newModelName).getObject("Box014").Width = R
        FreeCAD.getDocument(newModelName).getObject("Box014").Height = H
        FreeCAD.getDocument(newModelName).getObject("Box014").Placement = App.Placement(App.Vector(-1*(L/2),-1*(R/2),P),App.Rotation(0,0,0,1))

        # Perform cut
        App.ActiveDocument=None
        Gui.ActiveDocument=None
        App.setActiveDocument(newModelName)
        App.ActiveDocument=App.getDocument(newModelName)
        Gui.ActiveDocument=Gui.getDocument(newModelName)
        App.activeDocument().addObject("Part::Cut","Cut010")
        App.activeDocument().Cut010.Base = App.activeDocument().Fillet
        App.activeDocument().Cut010.Tool = App.activeDocument().Box014
        Gui.activeDocument().hide('Fillet')
        Gui.activeDocument().hide('Box014')
        App.ActiveDocument.recompute()

        # Copy this object and call the copy "Pin1"
        newTermShape = FreeCAD.getDocument(newModelName).getObject("Cut010").Shape.copy()
        newTermObj = App.activeDocument().addObject("Part::Feature","Pin1")
        newTermObj.Shape = newTermShape


    ###################################################################
    # Fuse the pins together to minimize the number of objects in the final assembly
    ###################################################################

    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(newModelName)
    App.ActiveDocument=App.getDocument(newModelName)
    Gui.ActiveDocument=Gui.getDocument(newModelName)
    Gui.activateWorkbench("PartWorkbench")
    App.activeDocument().addObject("Part::MultiFuse","Fusion001")
    App.activeDocument().Fusion001.Shapes = [App.activeDocument().Pin1,App.activeDocument().Pin2]
    App.ActiveDocument.recompute()

    # Copy this object and call the copy "Pins"
    newTermShape = FreeCAD.getDocument(newModelName).getObject("Fusion001").Shape.copy()
    newTermObj = App.activeDocument().addObject("Part::Feature",pinsName)
    newTermObj.Shape = newTermShape


    ###################################################################
    # Add anode marker bar
    ###################################################################

    # Create box to be anode marker bar
    App.ActiveDocument.addObject("Part::Box",pin1MarkName)
    App.ActiveDocument.recompute()
    Gui.SendMsgToActiveView("ViewFit")
    FreeCAD.getDocument(newModelName).getObject(pin1MarkName).Length = Lamb
    FreeCAD.getDocument(newModelName).getObject(pin1MarkName).Width = W-(2*x)
    FreeCAD.getDocument(newModelName).getObject(pin1MarkName).Height = Hamb
    FreeCAD.getDocument(newModelName).getObject(pin1MarkName).Placement = App.Placement(App.Vector((-1*(Lb/2))+x+B,((-1*(W/2))+x),H),App.Rotation(0,0,0,1))



    ###################################################################
    # Wrap things up
    ###################################################################

    # Kill off various no-longer-needed objects
    App.getDocument(newModelName).removeObject("Pin1")
    App.getDocument(newModelName).removeObject("Pin2")
    App.getDocument(newModelName).removeObject("Cut")
    App.getDocument(newModelName).removeObject("Cut001")
    App.getDocument(newModelName).removeObject("Cut002")
    App.getDocument(newModelName).removeObject("Cut003")
    App.getDocument(newModelName).removeObject("Cut004")
    App.getDocument(newModelName).removeObject("Cut005")
    App.getDocument(newModelName).removeObject("Cut006")
    App.getDocument(newModelName).removeObject("Cut007")
    App.getDocument(newModelName).removeObject("Cut008")
    App.getDocument(newModelName).removeObject("Cut009")
    App.getDocument(newModelName).removeObject("Cut011")
    App.getDocument(newModelName).removeObject("Fusion")
    App.getDocument(newModelName).removeObject("Fusion001")
    App.getDocument(newModelName).removeObject("Box")
    App.getDocument(newModelName).removeObject("Box001")
    App.getDocument(newModelName).removeObject("Box002")
    App.getDocument(newModelName).removeObject("Box003")
    App.getDocument(newModelName).removeObject("Box004")
    App.getDocument(newModelName).removeObject("Box005")
    App.getDocument(newModelName).removeObject("Box006")
    App.getDocument(newModelName).removeObject("Box007")
    App.getDocument(newModelName).removeObject("Box008")
    App.getDocument(newModelName).removeObject("Box009")
    App.getDocument(newModelName).removeObject("Box010")
    App.getDocument(newModelName).removeObject("Box011")
    App.getDocument(newModelName).removeObject("Box012")
    App.getDocument(newModelName).removeObject("Box013")
    App.getDocument(newModelName).removeObject("Fillet")

    # If P parameter wasn't 0, clean up objects that were created
    if (P != 0) :

        App.getDocument(newModelName).removeObject("Box014")
        App.getDocument(newModelName).removeObject("Cut010")
        

    # Set view and zoom in
    Gui.activeDocument().activeView().viewFront()
    Gui.SendMsgToActiveView("ViewFit")
    #Gui.activateWorkbench("DraftWorkbench")

    # Color body yellow
    FreeCADGui.getDocument(newModelName).getObject(bodyName).ShapeColor = (0.89,0.96,0.33)

    # Color pins bright tin
    FreeCADGui.getDocument(newModelName).getObject(pinsName).ShapeColor = (0.80,0.80,0.75)

    # Color anode marker rust orange
    FreeCADGui.getDocument(newModelName).getObject(pin1MarkName).ShapeColor = (0.67,0.42,0.22)

    # Save to disk in native format
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(newModelName)
    App.ActiveDocument=App.getDocument(newModelName)
    Gui.ActiveDocument=Gui.getDocument(newModelName)
    App.getDocument(newModelName).FileName = newModelPathNameExt
    App.getDocument(newModelName).Label = newModelName
    Gui.SendMsgToActiveView("Save")
    App.getDocument(newModelName).save()

    # Export to STEP
    App.ActiveDocument=None
    Gui.ActiveDocument=None
    App.setActiveDocument(newModelName)
    App.ActiveDocument=App.getDocument(newModelName)
    Gui.ActiveDocument=Gui.getDocument(newModelName)
    App.getDocument(newModelName).save()
    __objs__=[]
    __objs__.append(FreeCAD.getDocument(newModelName).getObject(bodyName))
    __objs__.append(FreeCAD.getDocument(newModelName).getObject(pinsName))
    __objs__.append(FreeCAD.getDocument(newModelName).getObject(pin1MarkName))
    import ImportGui
    ImportGui.export(__objs__,newStepPathNameExt)
    del __objs__

##    # Close all documents
###    App.closeDocument(newModelName)
###    App.setActiveDocument("")
###    App.ActiveDocument=None
###    Gui.ActiveDocument=None

    return 0




###################################
#### Main function
###################################

# Invariant information
newModelPath = "R:/trunk/mechanical/3D-models/SPI_Created/FreeCAD/CAPMP/"
stepSuffix = "_SPI1"
suffix = "_SvnRev_"

# Mold angle (in degrees) (not specified in datasheet--make something up)
maDeg = 8

# Length of anode marker bar
Lamb = 0.4

# Marking height
Hamb = 0.001


###################################
## Prepare for TANT_A model
###################################
# Object names
newModelName = "CAPMP_Kemet_A"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"

bodyName = newModelName + "_Body_" + suffix
pinsName = newModelName + "_Pins_" + suffix
pin1MarkName = newModelName + "_Pin1Mark_" + suffix

# Dimensions per http://www.kemet.com/kemet/web/homepage/kechome.nsf/vapubfiles/KEM_TC102_LOWESR.pdf/$file/KEM_TC102_LOWESR.pdf
# Length, width, height of the tantalum cap
L = 3.4
W = 1.8
H = 1.8

# Beveling, measured as horizontal distance from end of cap
B = 0.4

# Thickness of termination sheet metal
T = 0.13

# Width of cutout in anode termination
R = 0.4

# Height off PCB where cutout begins in anode termination
P = 0.4

# Fillet radius for termination sheet metal (make something up)
Tf = (0.5 * T)

# Length of bump underneath cap
G = 1.1

# Width of the bump underneath cap
#E = (0.9*(W-(2*ppH*math.tan(ma))))
E = 1.3

# Length of each termination
S = 0.8

# Width of each termination
F = 1.2

# Parameters specified on Kemet datasheet, but which we don't use.
X = 0
A = 0


###################################
## Prepare for next model
###################################
newModelName = "CAPMP_Kemet_A_3216_18"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = newModelName + "_Body_" + suffix
pinsName = newModelName + "_Pins_" + suffix
pin1MarkName = newModelName + "_Pin1Mark_" + suffix

# Parameters off Kemet datasheet (as processed through Excel)
L = 3.40
W = 1.80
H = 1.80
F = 1.20
S = 0.80
B = 0.40
X = 0.10
P = 0.40
R = 0.40
T = 0.13
A = 1.40
G = 1.10
E = 1.30

# Call CreateTantalumCapacitor() to do all the real work
CreateTantalumCapacitor(L, W, H, F, S, B, X, P, R, T, A, G, E,
                        Tf, maDeg, Lamb, Hamb, 
                        newModelName,
                        newModelPathNameExt,
                        newStepPathNameExt,
                        bodyName,
                        pinsName,
                        pin1MarkName)


###################################
## Prepare for next model
###################################
newModelName = "CAPMP_Kemet_B_3528_21"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = newModelName + "_Body_" + suffix
pinsName = newModelName + "_Pins_" + suffix
pin1MarkName = newModelName + "_Pin1Mark_" + suffix

# Parameters off Kemet datasheet (as processed through Excel)
L = 3.70
W = 3.00
H = 2.10
F = 2.20
S = 0.80
B = 0.40
X = 0.10
P = 0.50
R = 1.00
T = 0.13
A = 2.10
G = 1.80
E = 2.20

# Call CreateTantalumCapacitor() to do all the real work
CreateTantalumCapacitor(L, W, H, F, S, B, X, P, R, T, A, G, E,
                        Tf, maDeg, Lamb, Hamb, 
                        newModelName,
                        newModelPathNameExt,
                        newStepPathNameExt,
                        bodyName,
                        pinsName,
                        pin1MarkName)


###################################
## Prepare for next model
###################################
newModelName = "CAPMP_Kemet_C_6032_28"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = newModelName + "_Body_" + suffix
pinsName = newModelName + "_Pins_" + suffix
pin1MarkName = newModelName + "_Pin1Mark_" + suffix

# Parameters off Kemet datasheet (as processed through Excel)
L = 6.30
W = 3.50
H = 2.80
F = 2.20
S = 1.30
B = 0.50
X = 0.10
P = 0.90
R = 1.00
T = 0.13
A = 3.10
G = 2.80
E = 2.40

# Call CreateTantalumCapacitor() to do all the real work
CreateTantalumCapacitor(L, W, H, F, S, B, X, P, R, T, A, G, E,
                        Tf, maDeg, Lamb, Hamb, 
                        newModelName,
                        newModelPathNameExt,
                        newStepPathNameExt,
                        bodyName,
                        pinsName,
                        pin1MarkName)


###################################
## Prepare for next model
###################################
newModelName = "CAPMP_Kemet_D_7343_31"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = newModelName + "_Body_" + suffix
pinsName = newModelName + "_Pins_" + suffix
pin1MarkName = newModelName + "_Pin1Mark_" + suffix

# Parameters off Kemet datasheet (as processed through Excel)
L = 7.60
W = 4.60
H = 3.10
F = 2.40
S = 1.30
B = 0.50
X = 0.10
P = 0.90
R = 1.00
T = 0.13
A = 3.80
G = 3.50
E = 3.50

# Call CreateTantalumCapacitor() to do all the real work
CreateTantalumCapacitor(L, W, H, F, S, B, X, P, R, T, A, G, E,
                        Tf, maDeg, Lamb, Hamb, 
                        newModelName,
                        newModelPathNameExt,
                        newStepPathNameExt,
                        bodyName,
                        pinsName,
                        pin1MarkName)


###################################
## Prepare for next model
###################################
newModelName = "CAPMP_Kemet_H_7360_20"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = newModelName + "_Body_" + suffix
pinsName = newModelName + "_Pins_" + suffix
pin1MarkName = newModelName + "_Pin1Mark_" + suffix

# Parameters off Kemet datasheet (as processed through Excel)
L = 7.60
W = 6.30
H = 2.00
F = 4.10
S = 1.30
B = 0.00
X = 0.10
P = 0.90
R = 1.00
T = 0.13
A = 3.30
G = 3.50
E = 3.50

# Call CreateTantalumCapacitor() to do all the real work
CreateTantalumCapacitor(L, W, H, F, S, B, X, P, R, T, A, G, E,
                        Tf, maDeg, Lamb, Hamb, 
                        newModelName,
                        newModelPathNameExt,
                        newStepPathNameExt,
                        bodyName,
                        pinsName,
                        pin1MarkName)


###################################
## Prepare for next model
###################################
newModelName = "CAPMP_Kemet_L_6032_19"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = newModelName + "_Body_" + suffix
pinsName = newModelName + "_Pins_" + suffix
pin1MarkName = newModelName + "_Pin1Mark_" + suffix

# Parameters off Kemet datasheet (as processed through Excel)
L = 6.30
W = 3.40
H = 1.90
F = 2.20
S = 1.30
B = 0.00
X = 0.05
P = 0.00
R = 0.00
T = 0.13
A = 3.10
G = 2.80
E = 2.40

# Call CreateTantalumCapacitor() to do all the real work
CreateTantalumCapacitor(L, W, H, F, S, B, X, P, R, T, A, G, E,
                        Tf, maDeg, Lamb, Hamb, 
                        newModelName,
                        newModelPathNameExt,
                        newStepPathNameExt,
                        bodyName,
                        pinsName,
                        pin1MarkName)


###################################
## Prepare for next model
###################################
newModelName = "CAPMP_Kemet_M_3528_15"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = newModelName + "_Body_" + suffix
pinsName = newModelName + "_Pins_" + suffix
pin1MarkName = newModelName + "_Pin1Mark_" + suffix

# Parameters off Kemet datasheet (as processed through Excel)
L = 3.70
W = 3.00
H = 1.50
F = 2.20
S = 0.80
B = 0.00
X = 0.05
P = 0.00
R = 0.00
T = 0.13
A = 2.10
G = 1.80
E = 2.20

# Call CreateTantalumCapacitor() to do all the real work
CreateTantalumCapacitor(L, W, H, F, S, B, X, P, R, T, A, G, E,
                        Tf, maDeg, Lamb, Hamb, 
                        newModelName,
                        newModelPathNameExt,
                        newStepPathNameExt,
                        bodyName,
                        pinsName,
                        pin1MarkName)


###################################
## Prepare for next model
###################################
newModelName = "CAPMP_Kemet_T_3528_12"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = newModelName + "_Body_" + suffix
pinsName = newModelName + "_Pins_" + suffix
pin1MarkName = newModelName + "_Pin1Mark_" + suffix

# Parameters off Kemet datasheet (as processed through Excel)
L = 3.70
W = 3.00
H = 1.20
F = 2.20
S = 0.80
B = 0.00
X = 0.05
P = 0.00
R = 0.00
T = 0.13
A = 2.10
G = 1.80
E = 2.20

# Call CreateTantalumCapacitor() to do all the real work
CreateTantalumCapacitor(L, W, H, F, S, B, X, P, R, T, A, G, E,
                        Tf, maDeg, Lamb, Hamb, 
                        newModelName,
                        newModelPathNameExt,
                        newStepPathNameExt,
                        bodyName,
                        pinsName,
                        pin1MarkName)


###################################
## Prepare for next model
###################################
newModelName = "CAPMP_Kemet_U_6032_15"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = newModelName + "_Body_" + suffix
pinsName = newModelName + "_Pins_" + suffix
pin1MarkName = newModelName + "_Pin1Mark_" + suffix

# Parameters off Kemet datasheet (as processed through Excel)
L = 6.30
W = 3.40
H = 1.50
F = 2.20
S = 1.30
B = 0.00
X = 0.05
P = 0.00
R = 0.00
T = 0.13
A = 3.10
G = 2.80
E = 2.40

# Call CreateTantalumCapacitor() to do all the real work
CreateTantalumCapacitor(L, W, H, F, S, B, X, P, R, T, A, G, E,
                        Tf, maDeg, Lamb, Hamb, 
                        newModelName,
                        newModelPathNameExt,
                        newStepPathNameExt,
                        bodyName,
                        pinsName,
                        pin1MarkName)


###################################
## Prepare for next model
###################################
newModelName = "CAPMP_Kemet_V_7343_19"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = newModelName + "_Body_" + suffix
pinsName = newModelName + "_Pins_" + suffix
pin1MarkName = newModelName + "_Pin1Mark_" + suffix

# Parameters off Kemet datasheet (as processed through Excel)
L = 7.60
W = 4.60
H = 1.90
F = 2.40
S = 1.30
B = 0.00
X = 0.05
P = 0.00
R = 0.00
T = 0.13
A = 3.80
G = 3.50
E = 3.50

# Call CreateTantalumCapacitor() to do all the real work
CreateTantalumCapacitor(L, W, H, F, S, B, X, P, R, T, A, G, E,
                        Tf, maDeg, Lamb, Hamb, 
                        newModelName,
                        newModelPathNameExt,
                        newStepPathNameExt,
                        bodyName,
                        pinsName,
                        pin1MarkName)


###################################
## Prepare for next model
###################################
newModelName = "CAPMP_Kemet_W_7343_15"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = newModelName + "_Body_" + suffix
pinsName = newModelName + "_Pins_" + suffix
pin1MarkName = newModelName + "_Pin1Mark_" + suffix

# Parameters off Kemet datasheet (as processed through Excel)
L = 7.60
W = 4.60
H = 1.50
F = 2.40
S = 1.30
B = 0.00
X = 0.05
P = 0.00
R = 0.00
T = 0.13
A = 3.80
G = 3.50
E = 3.50

# Call CreateTantalumCapacitor() to do all the real work
CreateTantalumCapacitor(L, W, H, F, S, B, X, P, R, T, A, G, E,
                        Tf, maDeg, Lamb, Hamb, 
                        newModelName,
                        newModelPathNameExt,
                        newStepPathNameExt,
                        bodyName,
                        pinsName,
                        pin1MarkName)


###################################
## Prepare for next model
###################################
newModelName = "CAPMP_Kemet_X_7343_43"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = newModelName + "_Body_" + suffix
pinsName = newModelName + "_Pins_" + suffix
pin1MarkName = newModelName + "_Pin1Mark_" + suffix

# Parameters off Kemet datasheet (as processed through Excel)
L = 7.60
W = 4.60
H = 4.30
F = 2.40
S = 1.30
B = 0.50
X = 0.10
P = 1.70
R = 1.00
T = 0.13
A = 3.80
G = 3.50
E = 3.50

# Call CreateTantalumCapacitor() to do all the real work
CreateTantalumCapacitor(L, W, H, F, S, B, X, P, R, T, A, G, E,
                        Tf, maDeg, Lamb, Hamb, 
                        newModelName,
                        newModelPathNameExt,
                        newStepPathNameExt,
                        bodyName,
                        pinsName,
                        pin1MarkName)


###################################
## Prepare for next model
###################################
newModelName = "CAPMP_Kemet_Y_7343_40"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = newModelName + "_Body_" + suffix
pinsName = newModelName + "_Pins_" + suffix
pin1MarkName = newModelName + "_Pin1Mark_" + suffix

# Parameters off Kemet datasheet (as processed through Excel)
L = 7.60
W = 4.60
H = 4.00
F = 2.40
S = 1.30
B = 0.50
X = 0.10
P = 1.70
R = 1.00
T = 0.13
A = 3.80
G = 3.50
E = 3.50

# Call CreateTantalumCapacitor() to do all the real work
CreateTantalumCapacitor(L, W, H, F, S, B, X, P, R, T, A, G, E,
                        Tf, maDeg, Lamb, Hamb, 
                        newModelName,
                        newModelPathNameExt,
                        newStepPathNameExt,
                        bodyName,
                        pinsName,
                        pin1MarkName)


###################################
## Prepare for next model
###################################
newModelName = "CAPMP_Kemet_E_7360_38"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = newModelName + "_Body_" + suffix
pinsName = newModelName + "_Pins_" + suffix
pin1MarkName = newModelName + "_Pin1Mark_" + suffix

# Parameters off Kemet datasheet (as processed through Excel)
L = 7.60
W = 6.30
H = 3.80
F = 4.10
S = 1.30
B = 0.50
X = 0.10
P = 0.00
R = 0.00
T = 0.13
A = 3.30
G = 3.50
E = 3.50

# Call CreateTantalumCapacitor() to do all the real work
CreateTantalumCapacitor(L, W, H, F, S, B, X, P, R, T, A, G, E,
                        Tf, maDeg, Lamb, Hamb, 
                        newModelName,
                        newModelPathNameExt,
                        newStepPathNameExt,
                        bodyName,
                        pinsName,
                        pin1MarkName)


###################################
## Prepare for next model
###################################
newModelName = "CAPMP_Kemet_R_2012_12"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = newModelName + "_Body_" + suffix
pinsName = newModelName + "_Pins_" + suffix
pin1MarkName = newModelName + "_Pin1Mark_" + suffix

# Parameters off Kemet datasheet (as processed through Excel)
L = 2.20
W = 1.50
H = 1.20
F = 0.90
S = 0.50
B = 0.00
X = 0.05
P = 0.00
R = 0.00
T = 0.13
A = 0.80
G = 0.50
E = 0.80

# Call CreateTantalumCapacitor() to do all the real work
CreateTantalumCapacitor(L, W, H, F, S, B, X, P, R, T, A, G, E,
                        Tf, maDeg, Lamb, Hamb, 
                        newModelName,
                        newModelPathNameExt,
                        newStepPathNameExt,
                        bodyName,
                        pinsName,
                        pin1MarkName)


###################################
## Prepare for next model
###################################
newModelName = "CAPMP_Kemet_S_3216_12"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = newModelName + "_Body_" + suffix
pinsName = newModelName + "_Pins_" + suffix
pin1MarkName = newModelName + "_Pin1Mark_" + suffix

# Parameters off Kemet datasheet (as processed through Excel)
L = 3.40
W = 1.80
H = 1.20
F = 1.20
S = 0.80
B = 0.00
X = 0.05
P = 0.00
R = 0.00
T = 0.13
A = 1.40
G = 1.10
E = 1.30

# Call CreateTantalumCapacitor() to do all the real work
CreateTantalumCapacitor(L, W, H, F, S, B, X, P, R, T, A, G, E,
                        Tf, maDeg, Lamb, Hamb, 
                        newModelName,
                        newModelPathNameExt,
                        newStepPathNameExt,
                        bodyName,
                        pinsName,
                        pin1MarkName)


###################################
## Prepare for next model
###################################
newModelName = "CAPMP_Kemet_V_7343_20"
newModelPathNameExt = newModelPath + newModelName + ".FCStd"
newStepPathNameExt = newModelPath + newModelName + stepSuffix + ".step"
bodyName = newModelName + "_Body_" + suffix
pinsName = newModelName + "_Pins_" + suffix
pin1MarkName = newModelName + "_Pin1Mark_" + suffix

# Parameters off Kemet datasheet (as processed through Excel)
L = 7.60
W = 4.60
H = 2.00
F = 2.40
S = 1.30
B = 0.00
X = 0.05
P = 0.00
R = 0.00
T = 0.13
A = 3.80
G = 3.50
E = 3.50

# Call CreateTantalumCapacitor() to do all the real work
CreateTantalumCapacitor(L, W, H, F, S, B, X, P, R, T, A, G, E,
                        Tf, maDeg, Lamb, Hamb, 
                        newModelName,
                        newModelPathNameExt,
                        newStepPathNameExt,
                        bodyName,
                        pinsName,
                        pin1MarkName)



