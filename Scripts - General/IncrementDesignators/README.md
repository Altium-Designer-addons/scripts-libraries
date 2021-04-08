# IncrementDesignators
This script could be useful to modify designators of components (in SCH or
PCB), Pins (In SCHLIB) or Pads (ib PCBLIB). Component Prefix letters are
not changed.

Script has two modes of working:

- Incrementing: This mode is used if "Swap Designators" is unchecked.
                It assigns "Next Index Number" to clicked primitive. In
                this mode "Increment Number" is also used ad increment step.

- Swapping:     This mode is used if "Swap Designators" is unchecked.
                User needs to select two primitives and their designators
                will be swapped. Whe in SCHLIB, usre can shoose to move pin
                names too (if "Move Pin Name" is checked)


## Credits
Created by: Petar Perisin