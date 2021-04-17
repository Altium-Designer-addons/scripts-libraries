# LengthTuningHelper
This script creates aditional temporary tracks on a PCB Document that should be used during length tuning.

Primar use of the script is to do length tuning of DDR3-FPGA interface. Entire interface should be added to a net class.


## Usage
Upon executing the script, user should choose IC (FPGA) component and a file that contains info about length of nets inside it.\
Currently only Xilinx *.pkg file is supported. If you want another file type, for another FPGA family, please let me know.

In case you do not work with Xilinx, you can load a CSV file that will contain this info. It is easily made in excel. CSV file should hold info in this format:

`PadDesignator;Length`

also, you should have one aditional line for units. It should be (example):

`Units;um`

units supported are um, cm, mm, mil, th, inch


If you just want to include via length in length tuning, without any file load, it is also supported. Just choose net class and and check the "include via length" checkBox.


## Credits
Created by: Petar Perisin