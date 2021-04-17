# AutoRouter_Interface
This scripts can be used to have better interface to autorouters.\
It has DSNexporter and SESimporter. Here are fixes in them.

SESimporter:
- imports ses file including component placement and routings.
- uses built-in rte importer for routings, but fixes overlaping components and free primitives.

DSNexporter:
- fixes issue with multiple pads of the same name in component.
- fixed rounded pads issue with FreeRouter (I made them rectangular). However, you need to set FreeRouter variable to true for this.


## Credits
Created by: Petar Perisin