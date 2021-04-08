# MultiPCBProject
This Script enables Project to be used with multiple PCB
documents, so that project is split among multiple PCB Documents.

It uses Balnkets + Parameter Sets (Place -> Directive -> Parameter
Set) which have parameter "PcbDoc", and it's value is name of the
PCB Document.

If the above is satisfied, this script replaces normal "Update PCB"
menu entries. This script will call "Update PCB", but before that
it will Place Compile mask over all blankets that do not point to
the PCB that is currently being updated. After "Update PCB" is
done, it will remove compile masks and re-compile the project.

There are extra functions you can use for pin swapping, and by
and by doing it connector setup like in examples above you can
have automated pin swapping on connectors on one PCB automaically
update to second PCB.


## Credits
Created by:    Petar Perisin