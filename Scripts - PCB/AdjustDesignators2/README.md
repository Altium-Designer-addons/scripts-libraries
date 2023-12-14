# [DOWNLOAD](https://altium-designer-addons.github.io/DownGit/#/home?url=https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20PCB/AdjustDesignators2) script

# AdjustDesignators2
This scripts can be used to adjust designators on mech layers or on silkscreen. The designators are centred, rotated at 0 or 90 deg depending on the component orientation and scaled appropriately.

To create these mechanical designators you can:
- add special strings to PcbLib footprints
- add strings in the PcbDoc component footprints with script CopyDesignatorsToMechLayerPair

## Credits
Created by: Mattias Ericson\
Reviewed by: Petar Perisin\
Improvements: Miroslav Dobrev, Stanislav Popelka, Brett Miller


## Changelog
Update 28/09/2021 (Brett Miller)
- Resize text bounding box before move() to fix multi-line text offsets.

Update 25/04/2021 (Brett Miller)
- Stop Comment moving with Designator AutoCenterCenter method.
- Support AD19+ mech layers, handle any existing multiline text
- Add constants for text widths for overlay & non-overlay

Update 30/09/2018 - added stroke font option

Update 15/03/2016 (Miroslav Dobrev)
- The script now works with Altium Designer version 14 and greater
- The script now also works with hidden designator components normally, without the need to permanently un-hide the designators first
- Broken requests to interface elements fixed
- Other small fixes
