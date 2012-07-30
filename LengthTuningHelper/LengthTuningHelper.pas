{..............................................................................}
{ Summary   This script creates aditional temporary tracks on a PCB Document   }
{           that should be used during length tuning.                          }
{                                                                              }
{           Primar use of the script is to do length tuning of DDR3-FPGA       }
{           interface. Entire interface should be added to a net class.        }
{                                                                              }
{           Upon executing a script, user should choose IC (FPGA) component    }
{           and a file that contains info about length of nets inside it.      }
{           Currently only xilinx *.pkg file is supported. If you want another }
{           file type, for another FPGA family, please let me know.            }
{                                                                              }
{           CSV File:                                                          }
{           in case you do not work with xilinx, you can load csv file that    }
{           will contain this info. It is easily made in excel. CSV file       }
{           should hold info in this format:                                   }
{                                                                              }
{                PadDesignator;Length                                          }
{                                                                              }
{           also, you should have one aditional line for units. It should be   }
{           (example):                                                         }
{                                                                              }
{                Units;um                                                      }
{                                                                              }
{           units supported are um, cm, mm, mil, th, inch                      }
{                                                                              }
{                                                                              }
{           if you just want to include via length in length tuning, without   }
{           any file load, it is also supported. Just choose net class and     }
{           and check the "include via length" checkBox.                       }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}



var
   Board      : IPCB_Board;
   FileName   : String;
   ParsedFile : TStringList;
   NetInfo    : TStringList;


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
   i:=0;
   StringLen := Length(Tekst);

   while i<=StringLen do
   begin
      if ((Tekst[i]=' ') And (Tekst[i+1]=' ')) then Delete(Tekst, i, 1)
      else Inc(i);
   end;
   Result := Trim(Tekst);
end;


// Parser for Xilinx *.pkg files
Procedure ImportXilinxPkgFile(dummy : Integer = 0);
var
   i, j, ACode : Integer;
   PkgFile     : TStringList;
   Line        : String;
   PinDes      : String;
   Len         : String;

begin
   PkgFile := TStringList.Create;
   PkgFile.LoadFromFile(FileName);

   for i := 0 to PkgFile.Count - 1 do
   begin
      Line := PkgFile[i];
      Line := StringReplace(Line, #9, #32, mkset(rfReplaceAll));
      Line := RemoveMultipleSpaces(Line);

      if (AnsiCompareText(GetToken(Line,1,' '),'pin') = 0) or (AnsiCompareText(GetToken(Line,1,' '),'pkgpin') = 0) then
      begin
         // pin info
         PinDes := GetToken(Line, 3, ' ');
         Len    := GetToken(Line, 9, ' ');

         if (AnsiCompareText(Len,'N.A.') <> 0) then
             ParsedFile.Add(PinDes + ';' + Len);
      end
      else
      begin
          // finding units here
          if (AnsiCompareText(GetToken(Line,9,' '),'tracelength') = 0) then
          begin
             Inc(i);
             Line := PkgFile[i];
             Line := StringReplace(Line, #9, #32, mkset(rfReplaceAll));
             Line := RemoveMultipleSpaces(Line);
             Line := GetToken(Line,9,' ');

             j := 1;
             while j <= Length(Line) do
             begin
                // check if there are only letters here
                ACode := Ord(Line[j]);

                if ((ACode < 65) or ((ACode > 90) and (ACode < 97)) or (ACode > 122)) then
                   Delete(Line, j, 1);

                Inc(j);
             end;

             ParsedFile.Add('Units;' + Line);
          end;
      end;
   end;

   // ParsedFile.SaveToFile(FileName + 'temp1');
end;

// procedure to generate NetInfo String List, and to parse info from ParsedFile
// stringList inside. It includes convert length value to TCoord units, and
// adding this length together with a link to a net connected to it. Initially
// it will be created from a NetClass, and length info will be added to it from
// FileParser string list.
Procedure GenerateNetInfoSL(dummy : Integer = 0);
var
   Iterator : IPCB_BoardIterator;
   NetClass : IPCB_ObjectClass;
   Net      : IPCB_Net;
   PadIter  : IPCB_GroupIterator;
   Comp     : IPCB_Component;
   Pad      : IPCB_Pad2;
   StrUnits : String;
   Units    : TCoord;
   LengthS  : String;
   LengthF  : Double;
   Length   : TCoord;
   IsAdded  : Boolean;
begin
   // Get net class and comonent from Combo boxes
   NetClass := ComboBoxNetClass.Items.GetObject(ComboBoxNetClass.ItemIndex);

   if ComboBoxIC.ItemIndex = -1 then
      Comp := nil
   else
      Comp := ComboBoxIC.Items.GetObject(ComboBoxIC.ItemIndex);

   StrUnits := ParsedFile.Values['Units'];
   if      (StrUnits = 'um')   or (StrUnits = 'UM')   then Units := mmsToCoord(0.001)
   else if (StrUnits = 'mm')   or (StrUnits = 'MM')   then Units := mmsToCoord(1)
   else if (StrUnits = 'cm')   or (StrUnits = 'CM')   then Units := mmsToCoord(10)
   else if (StrUnits = 'th')   or (StrUnits = 'TH')   then Units := milsToCoord(1)
   else if (StrUnits = 'mil')  or (StrUnits = 'MIL')  then Units := milsToCoord(1)
   else if (StrUnits = 'inch') or (StrUnits = 'INCH') then Units := milsToCoord(1000)
   else                                                    Units := mmsToCoord(0.001);

   Iterator := Board.BoardIterator_Create;
   Iterator.SetState_FilterAll;
   Iterator.AddFilter_ObjectSet(MkSet(eNetObject));

   Net := Iterator.FirstPCBObject;
   While (Net <> nIl) Do
   Begin
      If (NetClass.IsMember(Net)) Then
      begin
         // Now we have got one net. We need to go through all pads in IC package,
         // and find the pad that is on this net

         IsAdded := False;

         if Comp <> nil then
         begin
            PadIter := Comp.GroupIterator_Create;
            PadIter.SetState_FilterAll;
            PadIter.AddFilter_ObjectSet(MkSet(ePadObject));

            Pad := PadIter.FirstPCBObject;
            while (Pad <> nil) do
            begin
               if Pad.InNet then
               if Pad.Net.I_ObjectAddress = Net.I_ObjectAddress then
               begin
                  // Now get length
                  LengthS := Pad.Name;
                  LengthS := ParsedFile.Values[LengthS];

                  if LengthS <> '' then
                  begin
                     LengthF := StrTofloat(LengthS);
                     Length := Units * LengthF;

                     NetInfo.AddObject(IntToStr(Length),Net);
                     IsAdded := True;
                  end;
               end;
               Pad := PadIter.NextPCBObject;
            end;
            Comp.GroupIterator_Destroy(PadIter);
         end;

         if not IsAdded then NetInfo.AddObject('0',Net);
      end;
      Net := Iterator.NextPCBObject;
   End;
   Board.BoardIterator_Destroy(Iterator);

  // NetInfo.SaveToFile(FileName + 'temp');
end;


// Procedure to calculate Via length - it calculates distance from upper most
// layer via is connected to, to lowest layer Via is connected to.
// I might need to reuse this, so I will check planes too.
function GetViaLength(Via : IPCB_Via) : Integer;
var
   i, j : integer;
   LayerObj    : IPCB_LayerObject;
   SpIter      : IPCB_SpatialIterator;
   Rectangle   : TCoordRect;
   Prim        : IPCB_Primitive;
   FoundFirst  : Boolean;
   IsConnected : Boolean;
   TempLength  : Integer;
   Length      : Integer;
begin
   LayerObj   := Board.LayerStack.FirstLayer;
   Rectangle  := Via.BoundingRectangle;
                 
   FoundFirst := False;
   TempLength := 0;
   Length     := 0;

   While LayerObj <> nil do
   begin
      if Via.IntersectLayer(LayerObj.V7_LayerID) then
      begin
         if ILayer.IsSignalLayer(LayerObj.V7_LayerID) then
         begin

            SpIter := Board.SpatialIterator_Create;
            SpIter.AddFilter_ObjectSet(MkSet(eTrackObject, eArcObject, ePadObject, eFillObject, eRegionObject));
            SpIter.AddFilter_Area(Rectangle.Left, Rectangle.Bottom, Rectangle.Right, Rectangle.Top);
            SpIter.AddFilter_LayerSet(MkSet(LayerObj.LayerID));

            Prim := SpIter.FirstPCBObject;
            IsConnected := False;
            While (Prim <> Nil) Do
            Begin
               if (Prim.InNet and Via.InNet) then
               if (Prim.Net.I_ObjectAddress = Via.Net.I_ObjectAddress) and (Board.PrimPrimDistance(Prim, Via) = 0) then
               begin
                  if not FoundFirst then
                  begin
                     TempLength := LayerObj.CopperThickness / 2 + LayerObj.Dielectric.DielectricHeight;
                     FoundFirst := True;
                  end
                  else
                  begin
                     Length := Length + TempLength + LayerObj.CopperThickness / 2;
                     TempLength := LayerObj.CopperThickness / 2 + LayerObj.Dielectric.DielectricHeight;
                  end;
                  IsConnected := True;
                  break;
               end;

               Prim := SpIter.NextPCBObject;
            End;
            Board.SpatialIterator_Destroy(SpIter);

            if (not IsConnected) and FoundFirst then
               TempLength := TempLength + LayerObj.CopperThickness + LayerObj.Dielectric.DielectricHeight;
         end
         else
         begin
            if Via.IsConnectedToPlane[LayerObj.LayerID] then
            begin
               if not FoundFirst then
               begin
                  TempLength := LayerObj.CopperThickness / 2 + LayerObj.Dielectric.DielectricHeight;
                  FoundFirst := True;
               end
               else
               begin
                  Length     := Length + TempLength + LayerObj.CopperThickness / 2;
                  TempLength := LayerObj.CopperThickness / 2 + LayerObj.Dielectric.DielectricHeight;
               end;
            end
            else if FoundFirst then
            begin
               TempLength := TempLength + LayerObj.CopperThickness + LayerObj.Dielectric.DielectricHeight;
            end;
         end;
      end
      else if FoundFirst Then break;
      LayerObj := Board.LayerStack.NextLayer(LayerObj);
   end;

   Result := Length;
end;

// Procedure to add ViaLength info to length calculation
Procedure AddViaLengthInfo(dummy : Integer = 0);
var
   Net      : IPCB_Net;
   Via      : IPCB_Via;
   Iterator : IPCB_GroupIterator;
   i        : Integer;
   Length   : TCoord;
begin
   for i := 0 to NetInfo.Count - 1 do
   begin
      Net    := NetInfo.GetObject(i);
      Length := StrToInt(NetInfo[i]);

      Iterator := Net.GroupIterator_Create;
      Iterator.SetState_FilterAll;
      Iterator.AddFilter_ObjectSet(MkSet(eViaObject));

      Via := Iterator.FirstPCBObject;
      while (Via <> nil) do
      begin
         Length := Length + GetViaLength(Via);

         Via := Iterator.NextPCBObject;
      end;

      Net.GroupIterator_Destroy(Iterator);
      NetInfo[i] := IntToStr(Length);
   end;
end;

// Final procedure to generate dummy component and add length info to tracks inside this component
Procedure GenerateGroup(dummy : Integer = 0);
var
   i     : integer;
   Track : IPCB_Track;
   Net   : IPCB_Net;
   Rule  : IPCB_Rule;
begin
   ResetParameters;
   RunProcess('PCB:Clear');
   for i := 0 to NetInfo.Count - 1 do
   if NetInfo[i] <> '0' then
   begin
      PCBServer.PreProcess;
      Track    := PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default);
      If Track  = Nil Then Exit;

      Track.x1 := Board.BoardOutline.BoundingRectangle.Left + (i * milsToCoord(50));
      Track.x2 := Track.x1;
      Track.y1 := Board.BoundingRectangle.Top + MilsToCoord(200);
      Track.y2 := Track.y1 + StrToInt(NetInfo[i]);

      Track.Layer := eTopLayer;

      PCBServer.SendMessageToRobots(Board.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,Track.I_ObjectAddress);
      Board.AddPCBObject(Track);
      PCBServer.PostProcess;

      Net   := NetInfo.GetObject(i);
      if Net = nil then continue;

      Net.AddPCBObject(Track);
      Rule := Board.FindDominantRuleForObject(Track,eRule_MaxMinWidth);
      Track.Width := Rule.FavoredWidth(eTopLayer);

      Track.Selected := True;
   end;
   ResetParameters;
   AddStringParameter('Action','CreateUnion');
   RunProcess('PCB:ManageUnions');
end;


procedure Start;
var
   iterator : IPCB_BoardIterator;
   NetClass : IPCB_ObjectClass;
   comp     : IPCB_Component;
begin
   Board := PCBServer.GetCurrentPCBBoard;
   if Board = nil then exit;

   Iterator := Board.BoardIterator_Create;
   Iterator.SetState_FilterAll;
   Iterator.AddFilter_ObjectSet(MkSet(eComponentObject));
   comp := Iterator.FirstPCBObject;
   While comp <> NIl Do
   Begin
       ComboBoxIC.Items.AddObject(comp.Name.Text, comp);

       comp := Iterator.NextPCBObject;
   End;
   Board.BoardIterator_Destroy(Iterator);

   Iterator := Board.BoardIterator_Create;
   Iterator.SetState_FilterAll;
   Iterator.AddFilter_ObjectSet(MkSet(eClassObject));
   NetClass := Iterator.FirstPCBObject;
   While NetClass <> NIl Do
   Begin
       If ((NetClass.MemberKind = eClassMemberKind_Net) and (NetClass.Name <> 'All Nets')) Then
           ComboBoxNetClass.Items.AddObject(NetClass.Name, NetClass);

       NetClass := Iterator.NextPCBObject;
   End;
   Board.BoardIterator_Destroy(Iterator);

   FormLengthTuning.Show;
end;


procedure TFormLengthTuning.ButtonCancelClick(Sender: TObject);
begin
   close;
end;


procedure TFormLengthTuning.ButtonLoadFileClick(Sender: TObject);
begin
   if (OpenFileDialog.Execute) then
   begin
      FileName := OpenFileDialog.FileName;
      LabelFileName.Caption := ExtractFileName(FileName);
   end;
end;

procedure TFormLengthTuning.ButtonOKClick(Sender: TObject);
begin
   if ComboBoxNetClass.ItemIndex = -1 then
   begin
      ShowMessage('You must choose net class');
      exit;
   end;

   // 1 - FileParser - it should result in a "ParsedFile" StringList with
   // PinDesignator;Length. It should be handled as Name/Value string list, with
   // semicolon as separator. It should have "Units", together with units
   // used, i.e. "Units;um".
   ParsedFile := TStringList.Create;
   ParsedFile.NameValueSeparator := ';';

   if (LabelfileName.Caption <> 'No File Choosen') then
   begin
      if (ExtractFileExt(LabelFileName.Caption) = '.csv') then
      begin
         ParsedFile.LoadFromFile(FileName);
      end
      else if (ExtractFileExt(LabelFileName.Caption) = '.pkg') then
      begin
         ImportXilinxPkgFile;
      end;
   end;

   // 2 - NetInfo StringList will contain pure length, in TCoord units.
   // Every line in it shall have a reference to a net that it represents.

   NetInfo := TStringList.Create;
   GenerateNetInfoSL;

   // 3 - Via length calculation - if this CB is checked, we should add via stack size
   // to the Length.
   if CheckBoxViaLength.Checked then
      AddViaLengthInfo;

   // 4 - After all this is done, we should create dummy component and add
   // Tracks to it, that will contain length of this lines. They will be added
   // Above the board, with appropriate net, length and width info.
   GenerateGroup;

   // Deselect All
   ResetParameters;
   AddStringParameter('Scope','All');
   RunProcess('PCB:DeSelect');

   Close;
end;


procedure TFormLengthTuning.ComboBoxICChange(Sender: TObject);
begin
   if ComboBoxIC.ItemIndex <> -1 then ButtonLoadFile.Enabled := True
   else                               ButtonLoadFile.Enabled := False;
end;


