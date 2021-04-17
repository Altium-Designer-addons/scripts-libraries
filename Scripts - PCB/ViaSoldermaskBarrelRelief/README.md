# ViaSoldermaskBarrelRelief
Soldermask Barrel Relief for VIAs

Barrel Relief Equals Drill Hole Size + Relief\
PCB Fabricators typically request Drill + 5mil when Via Hole > 13 mils

![Example](Via%20Soldermask%20Barrel%20Relief%20screenshot.png)


## Usage
In Altium Soldermask Expansion is Based on the Pad Size.\
That works great for Pads, but not so good for Vias.

Calculate the Soldermask Expansion for Barrel Relief\
Soldermask Expansion = ((PadSize - (DrillSize + Relief))/2)*-1


## Credits
Author: Randy Clemmons\
September 8, 2013


## Dev notes
reference material "AddPCBObject method" (http://wiki.altium.com/display/ADOH/PCB+API+System+Interfaces)

Note we convert values in Mils to internal coordinates using the MilsToCoord function. All PCB objects locations and sizes have internal coordinate units where 1 mil = 10000 internal units.

Debug Code removed from main loop:

```
ShowMessage (DrillSize);
ShowMessage (PadSize);
ShowMessage (Via.SolderMaskExpansion);
```
