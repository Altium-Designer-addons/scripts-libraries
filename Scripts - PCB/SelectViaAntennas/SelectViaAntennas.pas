{..............................................................................}
{ Summary   This script checks weather Vias are connected on only one layer.   }
{           If not, it is masked.                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}
{ Modified by Randy Clemmons                                                   }
{ Added code to support for AD14                                               }
{..............................................................................}

Procedure SelectViaAntennas;
var
   Board         : IPCB_Board;
   Iter          : IPCB_BoardIterator;
   SpIter        : IPCB_SpatialIterator;
   Via           : IPCB_Via;
   Prim          : IPCB_Primitive;
   TheLayerStack : IPCB_LayerStack;
   LayerObj      : IPCB_LayerObject;
   Rectangle     : TCoordRect;
   Connected     : Integer;

   S, VersionStr : String;
   MajorADVersion : Integer;
   MechEnabled : Boolean;

// Altium Ver Code - Mattias Ericson
{procedure ReadStringFromIniFile read settings from the ini-file.....................}
function ReadStringFromIniFile(Section,Name:String,FilePath:String,IfEmpty:String):String;
var
  IniFile     : TIniFile;
begin
     result := IfEmpty;
     if FileExists(FilePath) then
     begin
          try
             IniFile := TIniFile.Create(FilePath);

             Result := IniFile.ReadString(Section,Name,IfEmpty);

          finally
                 Inifile.Free;
          end;
     end;

end;  {ReadFromIniFile end....................................................}

begin
   Board := PCBServer.GetCurrentPCBBoard;

   if Board = nil then exit;

   ResetParameters;
   AddStringParameter('Scope', 'All');
   RunProcess('PCB:DeSelect');

   Iter := Board.BoardIterator_Create;
   Iter.AddFilter_ObjectSet(MkSet(eViaObject));
   Iter.AddFilter_AllLayers;

   Via := Iter.FirstPCBObject;

   //Check AD version for layer stack version
   VersionStr:= ReadStringFromIniFile('Preference Location','Build',SpecialFolder_AltiumSystem+'\PrefFolder.ini','14');
   S := Copy(VersionStr,0,2);
   //ShowMessage(S);
   MajorADVersion := StrToInt(S);
       
   if MajorADVersion >= 14 then
      TheLayerStack := Board.LayerStack_V7;   // v1.8 for AD14

   if MajorADVersion < 14 then
      TheLayerStack := Board.LayerStack;      // v1.8 for Older Versions of AD

    While (Via <> nil) do
    begin
       LayerObj := TheLayerStack.FirstLayer;
       Connected := 0;
       Rectangle := Via.BoundingRectangle;

       While LayerObj <> nil do
       begin
          if Via.IntersectLayer(LayerObj.LayerID) then  // v1.8
             if ILayer.IsSignalLayer(LayerObj.LayerID) then   // v1.8
             begin

                SpIter := Board.SpatialIterator_Create;
                SpIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject, ePadObject, eFillObject, eRegionObject));
                SpIter.AddFilter_Area(Rectangle.Left, Rectangle.Bottom, Rectangle.Right, Rectangle.Top);
                SpIter.AddFilter_LayerSet(MkSet(LayerObj.LayerID));

                Prim := SpIter.FirstPCBObject;

                While (Prim <> Nil) Do
                Begin
                   if Board.PrimPrimDistance(Prim, Via) = 0 then
                   begin
                      Inc(Connected);
                      break;
                   end;

                   Prim := SpIter.NextPCBObject;
                End;
                Board.SpatialIterator_Destroy(SpIter);
             end
             else
             begin
                if Via.IsConnectedToPlane[LayerObj.LayerID] then
                   Inc(Connected);
             end;
          LayerObj := TheLayerStack.NextLayer(LayerObj);
       end;

       if Connected = 1 then Via.Selected := True;

       Via := Iter.NextPCBObject;
    end;


   Board.BoardIterator_Destroy(Iter);

   Client.PostMessage('PCB:RunQuery','Apply=True|Expr=IsSelected|Mask=True', Length('Apply=True|Expr=IsSelected|Mask=True'), Client.CurrentView);

end;
