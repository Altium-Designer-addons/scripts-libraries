NetTieLib for Altin Designer (created on AD 2013, file format IntLib, sources and sample files included)
========================================================================================================

The library NetTieLib is distributed "as is" with no warranty. The library contains special Net Tie components which are used in Altium Designer to split one signal/net into several signals/nets or join several signals/nets into one signal/net. The Net Tie component is in fact creates short circuit connection of the signals. Altium Designer do not reports those short circuit connections as DRC violation if the connection is done by the cooper parts of the Net tie component.
You can find detailed description of Net Tie components in Documentation\NetTies-and-How-to-Use-Them.pdf. The document was not written by me.
The library was created as a template. Please feel free to modify the library components according to your design standards.

The library is distributed free of charge. 

NetTieLib.IntLib 		- integrated library NetTieLib which can be installed and used in Altium Designer
Documentation\ 			- documentation to NetTieLib and Net Tie components in general
DRC exceptions\ 		- RUL files with DRC exception rules which suppress violations in PcbDoc when NetTieLib components are used
NetTieLibSampleBoard\	- sample PCB project with all components from the NetTieLib, DRC exception rules applied
NetTieLibSource\		- source files used for building NetTieLib.IntLib, you can open NetTieLib.LibPkg file in Altium Designer,
						  modify the library and compile it back into IntLib file which will be located in NetTieLibSource\Outpupt\

Please send your comments and error reports to PetrTos (a) gmail.com .
I hope the library will be usefull for you.

Petr Tosovsky
2013-07-25