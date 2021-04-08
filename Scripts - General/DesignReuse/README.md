# DesignReuse
This Script enables true design reuse in Altium Designer

Read "How to use this script.odt" document that comes with this
script for more info.

It uses Balnkets + Parameter Sets (Place -> Directive -> Parameter
Set) which have parameter "Snippet", and it's value is name of the
PCB snippet.

User should have this directives placed on schematic, and he
should also have PCB snippets with the same name.

If the above is satisfied, user should do regular PCB update first,
and after that run this script. This script will then place PCB
snippet(s), modify component placement, update net info in
snippet and put all snippet objects in a union, for easier
movement.

NOTE - since I can not access where snippets are stored from a
script, You need to fill in folders of your snippets by modifying
the script. Cntinue reading on line 35, in "WriteSnippetsFolders"
procedure.


## Credits
Created by:    Petar Perisin