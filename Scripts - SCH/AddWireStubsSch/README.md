# AddWireStubsSch
Add Wire Stubs to Schematic Pins

This scripts aims to add small segments of wires to unconnected pins in current schematic sheet equiped by net labels related to each pin. If there is a wire, a power port or a No ERC symbol existing on pin, then no action is done for such pin.\
The script was developed for easier drawing of schematics with FPGA devices where small segments of wires are commonly used to wire down the schematic symbol for easier pin swapping.\
Net labels can be easily replaced by final names in SCH List panel (Smart Grid Paste) from pin file or by importing changes from FPGA project.


## Usage
Startup procedure: AddWireStubsSchRun

Customization can by done with some options given through a simple GUI.

Option | Description
--- | ---
OnlySelectedComps | script process only selected or all components
StubLength | length of wire segments in multiple of visible grid units
AddNetLabels | script creates Net labels for new wire segments
DesToLabels | pin designator will be copied to net label (otherwise pin name is used ad net label)
LabelOffset | offset of new net labels from pin in up, down, left, right directions separately


## Credits
Petr Tosovsky, Retry, www.edatools.cz \
Cyril Andreatta, extension with GUI

Script was developed from Altium script `PlaceSchObjects.PAS` from Scripting Gallery and with inspiration of `FormatPaintBrush.pas` from Petar Perisin, Altium Designer addons page.