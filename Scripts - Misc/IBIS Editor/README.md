# IBIS_Editor
*In July 2011 altium implemented support for "Model Selector", so you can use this script only if you need to override "Add Submodel"*

This scripts can be used to edit IBIS files so that they can be
used correctly by Altium.

Altium can import IBIS files, but "Model Selector" keyword is not
supported by Altium Signal integrity. This script asks user to
select models for each pin, and after that it modifies IBIS file
so that it can be used with Altium.

This script also attempts to override "Add Submodel" keyword, that
is also not supported by Altium


## Credits
Created by:    Petar Perisin