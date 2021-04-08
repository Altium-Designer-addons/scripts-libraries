# VendorTools
This Script is made to help people working with FPGA vendor tools.
It can import and export pin swap info for specific vendor.

Script should be executed by starting the "Start" procedure.

It then shows small dialog in which user should choose Vendor
tool, and choose weather he will do import or export.

It currently supports Altera, Lattice and Xilinx, but soon I will
 try to support Actel (but I need help from Actel user for this).

- Altera  export generates .tcl file
- Altera  import needs .pin file
- Xilinx  export generates .ucf file
- Xilinx  import needs .ucf file
- Lattice export generates .csv file
- Lattice import needs .pad file

Aditionally ther is custom import-export that works with simple
csv files that contain only "PinName, NetName" data. On import
data can be separated with '=', ',' or ';'

Known issues (on import only):
- If you place wire stub shorter than the one already on the PCB,
  maybe script will not be able to remove positioned netlabel.
  But anyway, this netlabel will always be floating, so ERC
  should catch it. This is very rare case. To avoid this best
  practice is to use default value.


## Credits
Created by:    Petar Perisin\
Fixes from:    Claudio Verones\
               - Issue with Unconnected net labels on same coordinat\
               - Issue with Alternate part vie