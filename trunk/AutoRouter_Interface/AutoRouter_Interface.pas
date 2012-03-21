{..............................................................................}
{ Summary   This scripts can be used to have better interface to autorouters.  }
{           It has DSNexporter and SESimporter. Here are fixes in them.        }
{                                                                              }
{           SESimporter:                                                       }
{           - imports ses file including component placement and routings.     }
{           - uses built-in rte importer for routings, but fixes overlaping    }
{             components and free primitives.                                  }
{                                                                              }
{           DSNexporter:                                                       }
{           - fixes issue with multiple pads of the same name in component.    }
{           - fixed rounded pads issue with FreeRouter (I made them            }
{             rectangular). However, you need to set FreeRouter variable to    }
{             true for this (line 20, just below)                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}

const
   FreeRouter = False;


//Following function removes multiple and leading spaces
function RemoveMultipleSpaces(Tekst): string;

var
   i: Integer;
   StringLen: Integer;
begin
   if Tekst = '' then
   begin
      Result := '';
      exit;
   end;
   i:=1;
   StringLen := Length(Tekst);
   while i<=StringLen do
   begin
      if (Tekst[i]=' ') And ((Tekst[i+1]=' ') or (i = 1) or (i = StringLen)) then Delete(Tekst, i, 1)
      else Inc(i);
      StringLen := Length(Tekst);
   end;
   Result := Tekst;
end;

// This function returns n-th word in a string
function GetToken(Tekst : String, TokenNum: Integer, SepChar: String): string;

var
  Token: string;
  StringLen: Integer;
  Num: Integer;
  EndofToken: Integer;
  i: Integer;
begin
  {Delete multiple spaces}
  StringLen := Length(Tekst);

  Num := 1;
  EndofToken := StringLen;
  while ((Num <= TokenNum) and (EndofToken <> 0)) do
  begin
    EndofToken := Pos(SepChar, Tekst);
    if EndofToken <> 0 then
    begin
      Token := Copy(Tekst, 1, EndofToken - 1);
      Delete(Tekst, 1, EndofToken);
      Inc(Num);
    end
    else
      Token := Tekst;
  end;
  if Num >= TokenNum then
    Result := Token
  else
    Result := '';
end;

Procedure SESimporter;
var
   Board         : IPCB_Board;
   OpenFile      : TOpenDialog;
   SesFile       : TStringList;
   TempSList     : TStringList;
   i, j, k       : Integer;
   OpenBracket   : Integer;
   Line          : String;
   TempLine      : String;
   StartRTEWrite : Boolean;
   FileName      : String;

   Component     : IPCB_Component;
   Iter          : IPCB_BoardIterator;
   GroupIter     : IPCB_GroupIterator;
   SpIter        : IPCB_SpatialIterator;
   Prim1, Prim2  : IPCB_Primitive;

   Resolution    : Integer;
begin
   Board := PCBServer.GetCurrentPCBBoard;

   if Board = nil then exit;

   OpenFile := TOpenDialog.Create(Application);

   OpenFile.Title  := 'Import SES file';
   OpenFile.Filter := 'SES file (*.ses)|*.ses';

   if not OpenFile.Execute then exit;

   FileName := OpenFile.FileName;

   SesFile := TStringList.Create;
   SesFile.LoadFromFile(FileName);

   i := 0;
   StartRTEWrite := False;
   TempSList := TStringList.Create;

   While i < SesFile.count do
   begin
      Line := SesFile[i];
      j := 0;

      for j := 1 to Length(Line) do
         if Line[j] <> ' ' then break;

      Dec(j);

      Line := RemoveMultipleSpaces(Line);
      if Line = '(routes' then
         StartRTEWrite := True;

      If StartRTEwrite then
      begin
         // Here we setup info for RTE File
         if (Line = '(shape') or (Line = '(wire') then
         begin
            OpenBracket := 1;
            repeat
               TempLine := SesFile[i + 1];
               TempLine := RemoveMultipleSpaces(TempLine);
               k := 1;

               for k := 1 to Length(TempLine) do
                  if      TempLine[k] = '(' then Inc(OpenBracket)
                  else if TempLine[k] = ')' then Dec(OpenBracket);

               if TempLine <> ')' then
                  TempLine := ' ' + TempLine;

               Line := Line + TempLine;
               Inc(i);
            until OpenBracket = 0;
         end;

         if Length(Line) <> 0 then
         begin
            Line := AdjustLineBreaks(Line,tlbsCRLF);
            while j > 0 do
            begin
               Line := ' ' + Line;
               Dec(j);
            end;
            if StartRTEWrite then
            begin
               TempSList.Add(Line);
            end;
         end;
      end
      else
      begin
         // Here we find info for component placement
         TempLine := Line;
         if Length(TempLine) > 7 then
            SetLength(TempLine, 7);

         if TempLine = '(place ' then
         begin
            // Get designator in TempLine
            TempLine := GetToken(Line, 2, ' ');

            Iter := Board.BoardIterator_Create;
            Iter.AddFilter_AllLayers;
            Iter.AddFilter_ObjectSet(mkset(eComponentObject));

            Component := Iter.FirstPCBObject;
            while Component <> nil do
            begin
               if Component.Name.Text = TempLine then
               begin
                  Component.BeginModify;

                  TempLine := GetToken(Line, 5, ' ');
                  if (TempLine = 'back') then
                  begin
                     if (Component.Layer = eBottomLayer) then
                        Component.Layer := eTopLayer
                     else
                        Component.Layer := eBottomLayer;

                     Component.Rotation := 0 - Component.Rotation;
                  end;

                  TempLine := GetToken(Line, 6, ' ');
                  while TempLine[Length(TempLine)] = ')' do
                     SetLength(TempLine, Length(TempLine) - 1);

                  Component.Rotation := Component.Rotation + StrToFloat(TempLine);

                  TempLine := GetToken(Line, 3, ' ');
                  Component.x := StrToInt(TempLine) * Resolution;

                  TempLine := GetToken(Line, 4, ' ');
                  Component.y := StrToInt(TempLine) * Resolution;


                  Component.EndModify;
                  Component.GraphicallyInvalidate;

                  break;
               end;

               Component := Iter.NextPCBObject;
            end;
            Board.BoardIterator_Destroy(Iter);

         end
         else if TempLine = '(resolu' then
         begin
            TempLine := GetToken(Line, 2, ' ');

            if TempLine = 'mil' then
            begin
               TempLine   := GetToken(Line, 3, ' ');
               while TempLine[Length(TempLine)] = ')' do
                  SetLength(TempLine, Length(TempLine) - 1);
               Resolution := StrToInt(TempLine) / MilsToCoord(1);
            end
            else
            begin
               TempLine   := GetToken(Line, 3, ' ');
               while TempLine[Length(TempLine)] = ')' do
                  SetLength(TempLine, Length(TempLine) - 1);
               Resolution := StrToInt(TempLine) / MmsToCoord(1);
            end;
         end;
      end;

      inc(i);
   end;

   Line := ChangeFileExt(FileName,'');
   Line := Line + 'ThisIsATempFileAndWillBeDeleted.rte';

   TempSList.SaveToFile(Line);

   ResetParameters;
   AddStringParameter('Format','SPECCTRA ROUTE FILE');
   AddStringParameter('FileName',Line);
   RunProcess('PCB:Import');

   DeleteFile(Line);

   Board.ViewManager_FullUpdate;

   // I will clear TempSList String List
   // And use it for deleting objects below
   TempSList.Clear;

   // Now I need to check if there are overlapping prims. vias are easy, but
   // Tracks will be hard

   Iter := Board.BoardIterator_Create;
   Iter.AddFilter_AllLayers;
   Iter.AddFilter_ObjectSet(mkset(eComponentObject));

   Component := Iter.FirstPCBObject;
   while Component <> nil do
   begin
      GroupIter := Component.GroupIterator_Create;
      GroupIter.AddFilter_AllLayers;
      GroupIter.AddFilter_ObjectSet(mkset(eViaObject));

      Prim1 := GroupIter.FirstPCBObject;
      while Prim1 <> nil do
      begin
         SpIter := Board.SpatialIterator_Create;
         SpIter.AddFilter_AllLayers;
         SpIter.AddFilter_Area(Prim1.BoundingRectangle.Left, Prim1.BoundingRectangle.Bottom, Prim1.BoundingRectangle.Right, Prim1.BoundingRectangle.Top);
         SpIter.AddFilter_ObjectSet(mkset(eViaObject));

         Prim2 := SpIter.FirstPCBObject;
         while Prim2 <> nil do
         begin
            if not Prim2.InComponent and (Prim1.HighLayer = Prim2.HighLayer) and (Prim1.LowLayer = Prim2.LowLayer) and ((Prim2.x div 100) = (Prim1.x div 100)) and ((Prim2.y div 100) = (Prim1.y div 100)) then
               TempSList.AddObject('DeleteThis', Prim2); // TempSList is used to store objects for deleting

            Prim2 := SpIter.NextPCBObject;
         end;
         Board.SpatialIterator_Destroy(SpIter);

         Prim1 := GroupIter.NextPCBObject;
      end;
      Component.GroupIterator_Destroy(GroupIter);

      GroupIter := Component.GroupIterator_Create;
      GroupIter.AddFilter_ObjectSet(mkset(eTrackObject));
      GroupIter.AddFilter_IPCB_LayerSet(LayerSet.SignalLayers);

      Prim1 := GroupIter.FirstPCBObject;
      while Prim1 <> nil do
      begin
         SpIter := Board.SpatialIterator_Create;
         SpIter.AddFilter_LayerSet(MkSet(Prim1.Layer));
         SpIter.AddFilter_Area(Prim1.BoundingRectangle.Left, Prim1.BoundingRectangle.Bottom, Prim1.BoundingRectangle.Right, Prim1.BoundingRectangle.Top);
         SpIter.AddFilter_ObjectSet(mkset(eTrackObject));

         Prim2 := SpIter.FirstPCBObject;
         while Prim2 <> nil do
         begin
            if not Prim2.InComponent and ((((Prim2.x1 div 100) = (Prim1.x1 div 100)) and ((Prim2.y1 div 100) = (Prim1.y1 div 100)) and ((Prim2.x2 div 100) = (Prim1.x2 div 100)) and ((Prim2.y2 div 100) = (Prim1.y2 div 100))) or (((Prim2.x1 div 100) = (Prim1.x2 div 100)) and ((Prim2.y1 div 100) = (Prim1.y2 div 100)) and ((Prim2.x2 div 100) = (Prim1.x1 div 100)) and ((Prim2.y2 div 100) = (Prim1.y1 div 100)))) then
               TempSList.AddObject('DeleteThis', Prim2); // TempSList is used to store objects for deleting

            Prim2 := SpIter.NextPCBObject;
         end;
         Board.SpatialIterator_Destroy(SpIter);

         Prim1 := GroupIter.NextPCBObject;
      end;
      Component.GroupIterator_Destroy(GroupIter);

      Component := Iter.NextPCBObject;
   end;
   Board.BoardIterator_Destroy(Iter);

   For i := 0 to TempSList.Count - 1 do
   begin
      Prim1 := TempSList.GetObject(i);
      Board.RemovePCBObject(Prim1);
   end;

   Board.ViewManager_FullUpdate;
end;



Procedure DSNexporter;
var
   i, j, nr   : Integer;
   flag       : Boolean;
   Board      : IPCB_Board;
   SaveFile   : TSaveDialog;
   FileName   : String;
   Iter       : IPCB_BoardIterator;
   Component  : IPCB_Component;
   GroupIter  : IPCB_GroupIterator;
   Pad        : IPCB_Pad;
   PadList    : TStringList;
   Modified   : TStringList;
   Rounded    : TStringList;
   Octagonal  : TStringList;
   PadString  : String;
   LayerString: String;
begin
   // This will fix error when DSN export does not export multiple component pads with same designator

   Board := PCBServer.GetCurrentPCBBoard;

   if Board = nil then exit;

   SaveFile := TSaveDialog.Create(Application);

   SaveFile.Title  := 'Export DSN file';
   SaveFile.Filter := 'DSN file (*.dsn)|*.dsn';

   if not SaveFile.Execute then exit;

   FileName := SaveFile.FileName;
   ChangeFileExt(FileName,'.dsn');

   PadList  := TStringList.Create;
   Modified := TStringList.Create;
   Rounded  := TStringList.Create;
   Octagonal:= TStringList.Create;

   Iter := Board.BoardIterator_Create;
   Iter.AddFilter_AllLayers;
   Iter.AddFilter_ObjectSet(mkset(eComponentObject));

   Component := Iter.FirstPCBObject;
   while Component <> nil do
   begin
      GroupIter := Component.GroupIterator_Create;
      GroupIter.AddFilter_AllLayers;
      GroupIter.AddFilter_ObjectSet(mkset(ePadObject));

      j := 0;
      PadList.Clear;

      Pad := GroupIter.FirstPCBObject;
      while Pad <> nil do
      begin
         Flag := False;
         For i := 0 to PadList.Count - 1 do
             if Pad.Name = PadList[i] then
                Flag := True;

         if Flag then
         begin
            // Here we need to modify Designator
            Modified.AddObject(Pad.Name,Pad);
            Inc(j);
            Pad.Name := Pad.Name + 'COUNT' + IntToStr(j);
         end
         else
            PadList.Add(Pad.Name);

         if FreeRouter then
         begin
            if Pad.Mode = ePadMode_Simple then
            begin
               if (Pad.TopShape = eRounded) and (Pad.TopXSize <> Pad.TopYSize) then
               begin
                  Pad.TopShape := eRectangular;
                  Rounded.AddObject('',Pad);
               end
               else if (Pad.TopShape = eOctagonal) and (Pad.TopXSize <> Pad.TopYSize) then
               begin
                  Pad.TopShape := eRectangular;
                  Octagonal.AddObject('',Pad);
               end
            end
            else if Pad.Mode = ePadMode_LocalStack then
            begin
               PadString := '';

               if (Pad.TopShape = eRounded) and (Pad.TopXSize <> Pad.TopYSize) then
               begin
                  Pad.TopShape := eRectangular;
                  PadString := 'Top,';
               end;

               if (Pad.MidShape = eRounded) and (Pad.MidXSize <> Pad.MidYSize) then
               begin
                  Pad.MidShape := eRectangular;
                  PadString := PadString + 'Mid,';
               end;

               if (Pad.BotShape = eRounded) and (Pad.BotXSize <> Pad.BotYSize) then
               begin
                  Pad.BotShape := eRectangular;
                  PadString := PadString + 'Bot,';
               end;

               if PadString <> '' then
               begin
                  SetLength(PadString,Length(PadString) - 1);
                  Rounded.AddObject(PadString,Pad);
               end;

               PadString := '';

               if (Pad.TopShape = eOctagonal) and (Pad.TopXSize <> Pad.TopYSize) then
               begin
                  Pad.TopShape := eRectangular;
                  PadString := 'Top,';
               end;

               if (Pad.MidShape = eOctagonal) and (Pad.MidXSize <> Pad.MidYSize) then
               begin
                  Pad.MidShape := eRectangular;
                  PadString := PadString + 'Mid,';
               end;

               if (Pad.BotShape = eOctagonal) and (Pad.BotXSize <> Pad.BotYSize) then
               begin
                  Pad.BotShape := eRectangular;
                  PadString := PadString + 'Bot,';
               end;

               if PadString <> '' then
               begin
                  SetLength(PadString,Length(PadString) - 1);
                  Octagonal.AddObject(PadString,Pad);
               end;
            end
            else if Pad.Mode = ePadMode_ExternalStack then
            begin


            end;
         end;

         Pad := GroupIter.NextPCBObject;
      end;
      Component.GroupIterator_Destroy(GroupIter);

      Component := Iter.NextPCBObject;
   end;
   Board.BoardIterator_Destroy(Iter);

   ResetParameters;
   AddStringParameter('Format','SPECCTRA DESIGN');
   AddStringParameter('FileName',Filename);
   RunProcess('PCB:Export');

   For i := 0 to Modified.Count - 1 do
   begin
      Pad := Modified.GetObject(i);
      Pad.Name := Modified[i];
   end;

   for i := 0 to Rounded.Count - 1 do
   begin
      Pad := Rounded.GetObject(i);
      PadString := Rounded[i];

      if PadString = '' then
         Pad.TopShape := eRounded
      else
      begin
         nr := (Length(PadString) + 1) / 4;

         for j := 1 to nr do
         begin
            LayerString := GetToken(PadString,j,',');

            if LayerString = 'Top' then
            begin
               Pad.TopShape := eRounded;
            end
            else if LayerString = 'Mid' then
            begin
               Pad.MidShape := eRounded;
            end
            else if LayerString = 'Bot' then
            begin
               Pad.BotShape := eRounded;
            end;
         end;
      end;
   end;

   for i := 0 to Octagonal.Count - 1 do
   begin
      Pad := Octagonal.GetObject(i);
      PadString := Octagonal[i];

      if PadString = '' then
         Pad.TopShape := eOctagonal
      else
      begin
         nr := (Length(PadString) + 1) / 4;

         for j := 1 to nr do
         begin
            LayerString := GetToken(PadString,j,',');

            if LayerString = 'Top' then
            begin
               Pad.TopShape := eOctagonal;
            end
            else if LayerString = 'Mid' then
            begin
               Pad.MidShape := eOctagonal;
            end
            else if LayerString = 'Bot' then
            begin
               Pad.BotShape := eOctagonal;
            end;
         end;
      end;
   end;
end;
