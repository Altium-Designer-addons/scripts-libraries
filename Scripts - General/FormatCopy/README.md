# [Download](https://altium-designer-addons.github.io/DownGit/#/home?url=https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts+-+General/FormatCopy)

# FormatCopy
*From FormatPaintBrush.pas*

Used to copy some formatting properties from one (source) primitive to other (destination) primitive(s) of the same or similar type.

Works with almost all schematic objects (in SchDoc and SchLib) and some objects in PCB Document.
* SchDoc:
  * Cross object format copying is possible.
  * `CTRL` key modifier prevent Text colour pasting (C==Colour)
  * `ALT` key modifier prevents Area colour pasting (A==Area)
  * `SHIFT` key could prevent text Size change (S==Size)
* PcbDoc:
  * No cross object
  * `CTRL` key modifier Current Layer only pick
  * `SHIFT` key modifier same Layer pick as Source Prim Layer
  * `+`, `-`  changes current layer.

While script is running:
- `CTRL + Z` is active/usable.
- Current Layer (PCB) can be changed with `+` & `-` keys to influence layer pick bias.
- Ambiguious UI Dialog ignores modifier keys if Layer > MaxLayers


## Credits
Created by : Petar Perisin\
Modified   : B. Miller


## Changelog
```
08/08/2019 v0.1  fn() nGetObjectAtCursor from ModifyFootprintTracks.pas, handles layers beyond eMech16\
09/08/2019 v0.2  Refactored "repeat" loops\
10/08/2019 v0.3  Via & Pad format copying implemented/fixed.\
           v0.4  Recoded Sch obj selector with "sub" object priority/bias\
15/08/2019 v0.5  Recoded PCB obj pick with current layer & obj bias.\
19/08/2019 v0.6  De-select all Sch objects, *fix* PCB layer bias\
28/08/2019 v0.7  Group Sch Obj formatting operations by ancestor obj.\
31/08/2019 v0.71 Allow basic cross object type copy for text\
01/09/2019 v0.72 Sch pick object set based biasing.\
03/09/2019 v0.73 Sch: Move more operations to ancestor objects.\
14/09/2019 v0.74 Sch: Add modifier <cntl><alt> support\
14/09/2019 v0.75 PCB: remove layerset, use Source object & current layer as bias.\
16/09/2019 v0.76 PCB: Spatial iterator does NOT work with groups: dimensions & components.\
                      Pick UI does not cancel out from missed picks; more like the SchDoc picking\
17/09/2019 v0.80 PCB: Use Ambiguious object UI Dialog when possible, else uses prev. method\
21/09/2019 v0.81 SCH: eTextFrame (& eNote?) was not copying IsSolid, Transparent or LineWidth\
22/09/2019 v0.82 SCH: Use CreateHitTest in place of SpatialIterator\
27/09/2019 v0.83 SCH: Refactor out nested InSet & MkSet to avoid weirdness.\
17/10/2019 v0.84 SCH: Refactor out more nested Inset MkSet around line 600 in just in case.\
18/02/2020 v0.85 PCB: Improve Pad & Via expansion rule & value copying\
07/05/2020 v0.86 SCH: Graphically.Invalidate after each copy; trigger bounding box resize for components; fix bug in Comp desc.\
07/05/2020 v0.87 SCH: simplied pick ranking/weighting.
26/12/2020 v0.88 PCB: remove 2 useless lines in Dimensions. Change MessageDlg to mtCustom. Does it beep?
24/06/2022 v0.89 PCB: add pad & via template link copying.
24/06/2022 v0.90 PCB: copy over primitive keepout restrictions.
17/06/2023 v0.91 PCB: missed the simple padstack radius corners, refactor pad & via to layer iterators.
```

tbd: `SHIFT` modifier key was intended to prevent font size change but FontManager is borked in AD19.
     special SchLib filters disabled.


## Dev notes
Current API enumerations:  (in sad need of work)\
AllLayer = [MinLayer..eConnectLayer] , Set of TLayer\
MinLayer = eTopLayer;\
MaxLayer = eViaHoleLayer;              // 82 Mechanical 26 Via Holes\
eConnectLayer = eMechanical16 + 3;     // 75 Mechanical 19 Connections\
MaxMechanicalLayer = eMechanical16;

i=17, eMech17 : LayerID = 67108881   returned by AD17 Obj.Layer; but crashes GetObjectAtCursor(TLayer_V6)\
i=32, eMech32 : LayerID = 67108896   LayerUtils.MechanicalLayer(i) returns correct values TV7_Layer\
Delphi Sets can only have max 256 elements.\
The values 67108### above crash MkSet()
