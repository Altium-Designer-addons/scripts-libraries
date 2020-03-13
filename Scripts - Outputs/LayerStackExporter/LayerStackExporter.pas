{..............................................................................}
{ Summary   This Script saves Layer Stack information to CSV file, which can   }
{           be easily opened in excel.                                         }
{                                                                              } 
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
Procedure LayerStackExporter;
var

    Board         : IPCB_Board;
    TheLayerStack : IPCB_LayerStack;
    i             : Integer;
    LayerObj      : IPCB_LayerObject;
    LS            : String;
    SaveDialog    : TSaveDialog;
    Flag          : Integer;
    FileName      : String;
    Line          : String;
    StackFile     : TStringList;

begin

   Board := PCBServer.GetCurrentPCBBoard;

   If (Board = nil) then  exit;


   // Here I need to cycle through all Parts and see what component parameters
   // are selected.


   SaveDialog        := TSaveDialog.Create(Application);
   SaveDialog.Title  := 'Save Variants to CSV';
   SaveDialog.Filter := 'CSV file (*.csv)|*.csv';

   Flag := SaveDialog.Execute;
   if (not Flag) then exit;

   FileName := SaveDialog.FileName;


   // Set file extension
   FileName := ChangeFileExt(FileName, '.csv');

   StackFile := TStringList.Create;

   // Header
   Line := 'Copper Layer Name;Copper Thickness;Dielectric Height;Dielectric Material;Dielectric Constant;Dielectric Type';

   // Add header line
   StackFile.Add(Line);


    TheLayerStack := Board.LayerStack;
    If TheLayerStack = Nil Then Exit;

    if TheLayerStack.ShowDielectricTop then
        StackFile.Add(';;' + FloatToStr(CoordToMils(TheLayerStack.DielectricTop.DielectricHeight)) + 'mil;Solder Resist;' +
                      FloatToStr(TheLayerStack.DielectricTop.DielectricConstant) + ';Top Solder Mask');


    LayerObj := TheLayerStack.FirstLayer;
    Repeat
        StackFile.Add(LayerObj.Name + ';' + FloatToStr(CoordToMils(LayerObj.CopperThickness)) + 'mil');
        Line := ';;' + FloatToStr(CoordToMils(LayerObj.Dielectric.DielectricHeight)) + 'mil;' + LayerObj.Dielectric.DielectricMaterial + ';' +
                      FloatToStr(LayerObj.Dielectric.DielectricConstant);

        if      LayerObj.Dielectric.DielectricType = eCore    then Line := Line + ';Core'
        else if LayerObj.Dielectric.DielectricType = ePrePreg then Line := Line + ';PrePreg';

        LayerObj := TheLayerStack.NextLayer(LayerObj);

        If LayerObj <> nil then
           StackFile.Add(Line);
    Until LayerObj = Nil;


    if TheLayerStack.ShowDielectricBottom then
        StackFile.Add(';;' + FloatToStr(CoordToMils(TheLayerStack.DielectricBottom.DielectricHeight)) + 'mil;Solder Resist;' +
                      FloatToStr(TheLayerStack.DielectricBottom.DielectricConstant) + ';Bottom Solder Mask');



   StackFile.SaveToFile(FileName);
   StackFile.Free;
end;

