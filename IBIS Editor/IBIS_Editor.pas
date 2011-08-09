{..............................................................................}
{ Summary   This scripts can be used to edit IBIS files so that they can be    }
{           used correctly by Altium.                                          }
{                                                                              }
{           Altium can import IBIS files, but "Model Selector" keyword is not  }
{           supported by Altium Signal integrity. This script asks user to     }
{           select models for each pin, and after that it modifies IBIS file   }
{           so that it can be used with Altium.                                }
{                                                                              }
{           This script also attempts to override "Add Submodel" keyword, that }
{           is also not supported by Altium                                    }
{                                                                              }
{           UPDATE: in July 2011 altium implemented support for "Model         }
{           Selector", so you can use this script only if you need to override }
{           "Add Submodel"                                                     }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}
var
   FileArray : TStringList;         //String List that contains entire IBIS file
   ModelSelector : TStringList;     //String List that contalins model selectors ()
   ModelSelectorPins : TStringList; //String List that contains list of Model selectors
   PinIndex : Integer;
   //This global variable is used as helper on pins with no model selector
   NoSelectorPinDescription : String;
   SystemPinDescription : String;

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
      if ((Tekst[i]=' ') And (Tekst[i+1]=' ')) then Delete(Tekst, i, 1)
      else Inc(i);
   Result := Trim(Tekst);
end;



function CountPos(const subtext: string; Text: string): Integer;
begin
  if (Length(subtext) = 0) or (Length(Text) = 0) or (Pos(subtext, Text) = 0) then
    Result := 0
  else
    Result := (Length(Text) - Length(StringReplace(Text, subtext, '', '[rfReplaceAll]'))) div
      Length(subtext);
end;



procedure TIBISEditor.IBISEditorCreate(Sender: TObject);
begin
   StringGridPins.Cells[0,0] := 'Pin';
   StringGridPins.Cells[1,0] := 'Signal';
   StringGridPins.Cells[2,0] := 'Model';
   StringGridPins.Cells[3,0] := 'Model Description';
   StringGridModelSelectors.Cells[0,0] := 'Model Selector Name';
   StringGridModelSelectors.Cells[1,0] := 'Model Name';
   StringGridModelSelectors.Cells[2,0] := 'Model Description';
end;



procedure TIBISEditor.ButtonOpenClick(Sender: TObject);
var
  Line : String;
  i : Integer;
  j : Integer;
begin
   OpenDialog1.Filter := 'IBIS Model (*.ibs)|*.ibs|All files (*.*)|*.*';
   if OpenDialog1.Execute then
   begin
      FileArray := TStringList.Create;
      LabelOpen.Caption := OpenDialog1.FileName; //Path of File selected in dialog box is written in Label
      LabelOpen.Font.Color := clBlack;
      LabelComponent.Enabled := True;
      LabelComponent.Font.Color := clRed;
      ComboBoxComponent.Enabled := True;
      ComboBoxComponent.Items.Clear;
      StringGridPins.RowCount := 29;
      for i := 0 to 3 do
         for j:= 1 to 28 do
            StringGridPins.Cells[i,j] := '';
      //Loading IBIS file to "FileArray" String List
      FileArray.LoadFromFile(OpenDialog1.FileName);
      //Next loop loads all components to Combo Box "ComboBoxComponent"
      i := 0;
      while  i < FileArray.Count-1 do
      begin
         if (FileArray[i] <> '') then
         begin
            Line := FileArray[i];
            if (Line[1] = '[') then
               if (AnsiCompareText(GetToken(Line,1,' '),'[Component]') = 0) then
                  ComboBoxComponent.Items.Add(GetToken(RemoveMultipleSpaces(Line),2,' '));
         end
         else
         begin
            // Remove empty lines - much easier that way, but loading is slower.
            FileArray.Delete(i);
            Dec(i);
         end;
         Inc(i);
      end;
      ComboBoxComponent.SetFocus;
   end;
end;



procedure TIBISEditor.ComboBoxComponentSelect(Sender: TObject);
var
   ComponentName : String;
   Line : String;
   ModelName : String;
   TempModels : TStringList;
   Name: String;
   flag : Integer;
   i : Integer;
   j : Integer;
   k : Integer;
   br : Integer;
begin
   LabelComponent.Font.Color := clBlack;
   StringGridPins.Enabled := True;
   ButtonSave.Enabled := True;
   StringGridPins.RowCount := 2;
   WinXPTabControl.Enabled := True;
   // Name of selected component
   ComponentName := ComboBoxComponent.Text;

   //We will read FileArray and position ourselves at the place of selected component
   i := 0;
   while i < FileArray.Count-1 do
   begin
      if (FileArray[i] <> '') then
      begin
         Line := FileArray[i];
         if (Line[1] = '[') then
            if (AnsiCompareText(GetToken(Line,1,' '),'[Component]') = 0) then
               //  If selected component is found, then break. Line number will be in variable "i"
               if (AnsiCompareText(ComponentName, GetToken(RemoveMultipleSpaces(Line),2,' ')) = 0) then break;
      end;
      Inc(i);
   end;
   Inc(i);
   // We are positioned where component name is.

   //We need to populate Pin info to table
   While i <FileArray.Count-1 do  {Read until end of file and search where is this component}
   begin
      if (FileArray[i] <> '') then
      begin
         Line := FileArray[i];
         if (AnsiCompareText(GetToken(Line,1,' '),'[Pin]') = 0)  then
         begin
            // This is needed most times since there can be empty lines in IBIS
            repeat
               Inc(i);
            until (FileArray[i] <> '');
            break;
         end;
      end;
      Inc(i);
   end;

   //We are positioned where pin names are. We need to populate them to table
   PinIndex := i; //Global variable - used when clicking "save" button
   j := 1;
   TempModels := TStringList.Create;
   ModelSelectorPins := TStringList.Create;
   Line := FileArray[i];
   while (Line[1] <> '[') do
   begin
      if (Line[1] <> '|') then
      begin
         Line := RemoveMultipleSpaces(Line);
         StringGridPins.RowCount := StringGridPins.RowCount + 1;
         //Write pin name and signal name to string grid
         StringGridPins.Cells[0,j] := GetToken(Line,1,' '); 
         StringGridPins.Cells[1,j] := GetToken(Line,2,' ');

         ModelName := GetToken(Line,3,' ');
         StringGridPins.Cells[2,j] := ModelName;

         // We will full "ModelSelectorPins" string list for easier model choosing
         // This will be combiled with "ModelSelector" string list when populating combo box
         ModelSelectorPins.Add(ModelName);
         if ((ModelName = 'GND') or (ModelName = 'POWER') or (ModelName = 'NC')) then
            StringGridPins.Cells[3,j] := EditSystemPin.Text
         else
         begin
            flag := 1;
            for br := 0 to TempModels.Count - 1 do
            begin
               if ModelName = TempModels[br] then flag := 0;
            end;
            if flag = 1 then TempModels.Add(ModelName);
         end;
         Inc(j);
      end;
      repeat
         Inc(i);
      until (FileArray[i] <> '');
      Line := FileArray[i];
   end;
   StringGridPins.RowCount := StringGridPins.RowCount - 1;

   // Now we have set all values to the String Grid, but we still need to modify
   // model selectors and populate them in combo box on selection.

   // Model selector is actually a string list that is populated like this
   // **************************************************************************
   //                      ModelSelector
   // **************************************************************************
   //
   //   First Model Selector;Number of models;Model Name1;Model Description1;Model Name2;Model Description2 ...
   //   Second Model Selector;Number of models;Model Name1;Model Description1;Model Name2;Model Description2 ...
   //   ............
   //
   // We will be using String 'ModelName' as our temporary list that will be inserted
   // in 'ModelSelector' TStringList.
   // We will also use string 'List' to read into

   ModelSelector := TStringList.Create;
   while (i < FileArray.Count - 1) do
   begin
      if (AnsiCompareText(GetToken(Line,1,' '), '[Model') = 0) then
         if (AnsiCompareText(GetToken(Line,2,' '), 'Selector]') = 0) then
         begin         // model selector found. Save it's name to 'ModelName'
            ModelName := GetToken(RemoveMultipleSpaces(Line),3,' ');
            repeat
               Inc(i);
            until (FileArray[i] <> '');
            Line := FileArray.Strings[i];
            br := 0;
            while (Line[1] <> '[') do
            begin
               if (Line[1] <> '|') then
               begin    // models from selector. Append them to 'ModelName' and count them
                  Line := RemoveMultipleSpaces(Line);
                  Name := GetToken(Line,1,' ');
                  Delete (Line, 1, Length(Name) + 1); // temporarily use 'Line' String for description
                  ModelName := ModelName + ';' + Name + ';' + Line; // Append name and description to model name
                  Inc(br);  // count models in model selector
               end;
               repeat
                  Inc(i);
               until (FileArray[i] <> '');
               Line := FileArray[i];
            end;
            Line := GetToken(ModelName,1,';'); // temporarily save model selector name to this string
            Insert(';'+IntToStr(br), ModelName, Length(Line) + 1);
            ModelSelector.Add(ModelName);

            Dec(i);
         end;
      repeat
         Inc(i);
      until (FileArray[i] <> '');
      Line := FileArray[i];
      // following line speeds up the process, but it can be used only if all
      // model selectors are defined before first Model is defined
      // (it should be like that by the definition)
      if (AnsiCompareText(GetToken(Line,1,' '), '[Model]') = 0) then break;
   end;

   // we will display pin models that are first on the list and their description

   for i := 0 to ModelSelectorPins.Count - 1 do // StringGridPins has one extra fixed top row
   begin
      for j := 0 to ModelSelector.Count - 1 do
      begin
          if ModelSelectorPins[i] = GetToken(ModelSelector[j],1,';') then
          begin
             StringGridPins.Cells[2,i + 1] := GetToken(ModelSelector[j],3,';');
             StringGridPins.Cells[3,i + 1] := GetToken(ModelSelector[j],4,';');
          end;
      end;
   if StringGridPins.Cells[3,i + 1] = '' then      // Comment this two lines if you do not want Descriptions
      StringGridPins.Cells[3,i + 1] := EditModelPin.Text // for pins without model selector
   end;

   // Here we will populate second string grid that is used for group model selecting
   StringGridmodelSelectors.RowCount := ModelSelector.Count + 1;
   for i :=0 to ModelSelector.Count - 1 do
   begin
      StringGridModelSelectors.Cells[0,i+1] := GetToken(ModelSelector[i],1,';');
      StringGridModelSelectors.Cells[1,i+1] := GetToken(ModelSelector[i],3,';');
      StringGridModelSelectors.Cells[2,i+1] := GetToken(ModelSelector[i],4,';');
   end;

   if WinXPTabControl.TabIndex = 0 then StringGridPins.SetFocus;
end;



procedure TIBISEditor.StringGridPinsClick(Sender: TObject);
var
   Rct : TCoordRect;
   SelectorName : String;
   NumOfModels : Integer;
   i : Integer;
   flag : Integer;
begin
   if (ModelSelector.Count = 0) {or (CheckBoxModelSelectorGroups.Checked = True)} then exit;
   flag := -1;
   if ((StringGridPins.Col = 2) or (StringGridPins.Col = 3)) then
   begin
      // First we will check weather tedt in selected column appears among Model Selectors
      // If it is only a model, GND, POWER or NC we will not display combobox
      for i := 0 to ModelSelector.Count-1 do
      begin
         SelectorName := GetToken(ModelSelector[i],1,';');
         if SelectorName = ModelSelectorPins[StringGridPins.Row-1] then
             flag := i;
      end;
      if flag = -1 then
      begin
         ComboBoxModelSelect.Visible := False;
         exit;
      end;

      // Now we need to display combo box
      Rct := StringGridPins.CellRect(StringGridPins.Col, StringGridPins.Row);
      ComboBoxModelSelect.Top := Rct.Top + StringGridPins.Top + 2;
      ComboBoxModelSelect.Left := Rct.Left + StringGridPins.Left + 2;
      ComboBoxModelSelect.Height := Rct.Bottom - Rct.Top;
      ComboBoxModelSelect.Width := Rct.Right - Rct.Left;

      // now we will populate combo box with model selector values
      ComboBoxModelSelect.Items.Clear;
      NumOfModels := StrToInt(GetToken(ModelSelector[flag],2,';'));
      for i := 1 to NumOfModels do
         ComboBoxModelSelect.Items.Add(GetToken(ModelSelector[flag],(i*2+StringGridPins.Col-1),';'));
      ComboBoxModelSelect.Text := StringGridPins.Cells[StringGridPins.Col, StringGridPins.Row];
      ComboBoxModelSelect.Visible := True;


   end
   else
      ComboBoxModelSelect.Visible := False;
end;



procedure TIBISEditor.ComboBoxModelSelectChange(Sender: TObject);
var
   Tekst : String;
   ModelSelectorName : String;
   NumOfModels : Integer;
   i : Integer;
   j : Integer;
begin
   // Take text from combo box
   Tekst := ComboBoxModelSelect.Text;

   // Place Text from combo box inside String Grid (Model or Description)
   StringGridPins.Cells[StringGridPins.Col, StringGridPins.Row] := Tekst;

   // Placing Description inside String Grid if Model was chosen or
   // Placing Model inside String Grid if Description was chosen.
   ModelSelectorName := ModelSelectorPins[StringGridPins.Row - 1];
   for i := 0 to ModelSelector.Count - 1 do
   begin
      if (AnsiCompareText(GetToken(ModelSelector[i],1,';'), ModelSelectorName) = 0) then
      begin
         NumOfModels := StrToInt(GetToken(ModelSelector[i],2,';'));
         for j := 1 to NumOfModels do
         begin
            if ((StringGridPins.Col = 2) and (GetToken(ModelSelector[i],(j*2+1),';') =  Tekst)) then
               StringGridPins.Cells[StringGridPins.Col + 1, StringGridPins.Row] :=
                  GetToken(ModelSelector[i],(j*2+2),';');
            if ((StringGridPins.Col = 3) and (GetToken(ModelSelector[i],(j*2+2),';') =  Tekst)) then
               StringGridPins.Cells[StringGridPins.Col - 1, StringGridPins.Row] :=
                  GetToken(ModelSelector[i],(j*2+1),';');
         end;
      end;
   end;
   ComboBoxModelSelect.Visible := False;
end;



function ModelSelectorOverride();
{...............................................................................

-----------------------     Model Selector Override     ------------------------

................................................................................

This is one of the most important functions. It will modify FileArray, and remove
links on Model selectors from them. It will replace links to Model Selectors with
Links to Models that are selected in the Table on the Form.
}

var
   Line : String;
   i, j : Integer;
   FirstString   : String;
   FirstPosition : Integer;
   NextString    : String;
   NextPosition  : Integer;
   CellModel     : String;
   temp : Integer;
begin
   Line := FileArray[PinIndex];
   i := 0;
   j := 0;
   while (Line[1] <> '[') do
   begin
      if(Line[1] <> '|')then
      begin
         if (ModelSelectorPins.Strings[j] <> StringGridPins.Cells[2,j+1]) then
         begin
            // FirstString is model name, and FirstPosition is his position in line
            FirstString := ModelSelectorPins.Strings[j];
            FirstPosition := AnsiPos(' ' + FirstString + ' ', Line);
            Inc(FirstPosition);

            // Next segment takes care if pin name/number/model are the same
            temp := CountPos(' ' + FirstString + ' ', Line);
            CellModel := Line;
            while (CountPos(' ' + FirstString + ' ', CellModel) > 1) do
            begin
               Delete(CellModel, FirstPosition, 1);
               FirstPosition := AnsiPos(' ' + FirstString + ' ', CellModel);
               Inc(FirstPosition);
            end;
            FirstPosition := FirstPosition + temp - 1;

            // NextString is R_Pin and NextPosition is his position in line
            NextString := GetToken(RemoveMultipleSpaces(Line), 4, ' ');
            NextPosition := AnsiPos(' ' + NextString + ' ', Line);
            Inc(NextPosition);
            CellModel := StringGridPins.Cells[2,j+1];

            if (Length(CellModel) < NextPosition - FirstPosition) then
            begin
               while (Length(CellModel) < NextPosition - FirstPosition) do
                  CellModel := CellModel + ' ';
               while (Length(FirstString) <= NextPosition - FirstPosition) do
                  FirstString := FirstString + ' ';
               Delete(Line, FirstPosition, NextPosition - FirstPosition);
               Insert(CellModel, Line, FirstPosition);
            end
            else
            begin
               while (Length(FirstString) <= NextPosition - FirstPosition) do
                  FirstString := FirstString + ' ';

               CellModel := CellModel + ' ';

               Delete(Line, FirstPosition, NextPosition - FirstPosition);
               Insert(CellModel, Line, FirstPosition);
            end;
            FileArray.Delete(PinIndex + i);
            FileArray.Insert(PinIndex + i, Line);
         end;
         Inc(j);
      end;
      repeat
         Inc(i);
      until (FileArray[PinIndex+i] <> '');
      Line := FileArray[PinIndex+i];
   end;
end;



function ModifyFileName();
{...............................................................................

--------------     Modifying File Name and IBIS Version     --------------------

................................................................................

This function will modify File name that appears in IBIS file with filename
selected in File Save dialog.

It will also modify version number to 3.2 if it is higher version if IBIS.
}

var
   i : Integer;
   Line : String;
   Position : Integer;
   TempString : String;
   flag : Integer;
begin
   flag := 0;
   for i := 0 to FileArray.Count-1 do
   begin
      Line := FileArray[i];
      if (Line <> '') then
      begin
         if (Line[1] = '[') then
         begin
            If (flag = 0) then // Here is [IBIS Version] or [IBIS_Version]
            begin
               if (AnsiCompareText(GetToken(Line,1,' '), '[IBIS') = 0) then
               begin
                  TempString := GetToken(RemoveMultipleSpaces(Line),3,' ');
                  Position := AnsiPos(TempString, Line);
               end
               else
               begin
                  TempString := GetToken(RemoveMultipleSpaces(Line),2,' ');
                  Position := AnsiPos(TempString, Line);
               end;
               // Now in TempString we have IBIS version - we will modify it
               if ((StrToFloat(TempString[1]) > 3.5) and (RadioButtonSummer.Checked = True)) then
               begin
                  SetLength(Line, Position - 1);
                  Line := Line + '3.2';
               end;
               flag := 1;
               FileArray.Delete(i);
               FileArray.Insert(i, Line);
            end
            else if (flag = 1) then
            begin       //Check are we at [File Name] or [File_Name]
               TempString := GetToken(RemoveMultipleSpaces(Line),1,' ');
               if (AnsiCompareText(TempString, '[File') = 0) then
               begin
                  TempString := GetToken(RemoveMultipleSpaces(Line),3,' ');
                  flag := 2;
               end;
               if (AnsiCompareText(TempString, '[File_Name]') = 0) then
               begin
                  TempString := GetToken(RemoveMultipleSpaces(Line),2,' ');
                  flag := 2;
               end;
               if flag = 2 then
               begin  // Now we have File name in 'TempString' - we will modify it
                  Position := AnsiPos(TempString, Line);
                  SetLength(Line, Position - 1);
                  TempString := SaveDialog1.FileName;
                  repeat
                     delete(TempString, Length(TempString),1);
                  until TempString[Length(TempString)] = '\';

                  Position := Length(TempString);
                  TempString := SaveDialog1.FileName;
                  delete(TempString, 1,Position);

                  Line := Line + TempString;

                  FileArray.Delete(i);
                  FileArray.Insert(i, Line);

                  // Will exit if you use AD10 since it is faster
                  if RadioButtonRelease.Checked = True then
                     break;
               end;
            end
            else
            begin
               // Over here we will be checking weather we have [Receiver Thresholds]
               // keyword, and we will disable it.
               if (AnsiCompareText(GetToken(Line,1,' '), '[Receiver') = 0) then
               begin
                  TempString := '|' + Line;
                  FileArray.Delete(i);
                  FileArray.Insert(i, TempString);
                  Line := FileArray[i+1];
                  while (Line[1] <> '[') do
                  begin
                     Inc(i);
                     Line := FileArray[i];
                     if (Line[1] <> '|') and (Line[1] <> '[') then
                     begin
                        TempString := '|' + Line;
                        FileArray.Delete(i);
                        FileArray.Insert(i, TempString);
                     end;
                  end;
               end;
            end;
         end
         else if (AnsiCompareText(GetToken(Line,1,' '), 'Vref') = 0) then
         begin
            if (RemoveMultipleSpaces(GetToken(Line,2,' ')) <> '=') then
            begin
               TempString := '|' + Line;
               FileArray.Delete(i);
               FileArray.Insert(i, TempString);
            end;
         end;
      end;
   end;
end;


function LineCheck(Line : String):String;
var
   TempFloat : Float;
   TempString: String;
   Voltage   : String;
   Current   : String;
   CurrentMin: String;
   CurrentMax: String;
   flag      : integer;
begin
   // In all this Try-except I check weather all current entries are there.
   // If they are not I will populate them
   flag := 0;

   TempString := RemoveMultipleSpaces(Line);
   Voltage    := GetToken(TempString,1,' ');
   Current    := GetToken(TempString,2,' ');
   CurrentMin := GetToken(TempString,3,' ');
   CurrentMax := GetToken(TempString,4,' ');

   if (AnsiCompareText(Voltage, 'NA') = 0) then
   begin
      Result := '';
      exit;
   end;

   if (AnsiCompareText(Current, 'NA') = 0) then
   begin
      if (AnsiCompareText(CurrentMax, 'NA') = 0) then
      begin
         if (AnsiCompareText(CurrentMin, 'NA') = 0) then
         begin
            Result := '';
            exit;
         end
         else
         begin
            Current := CurrentMin;
         end;
      end
      else
      begin
         Current := CurrentMax;
      end;
   end;

   if (AnsiCompareText(CurrentMin, 'NA') = 0) then
   begin
      CurrentMin := Current;
   end;

   if (AnsiCompareText(CurrentMax, 'NA') = 0) then
   begin
      CurrentMax := Current;
   end;

   Result := Voltage + ';' + Current + ';' + CurrentMin + ';' + CurrentMax;
end;



function CountTables(Model : TStringList, Submodel : TStringList) : TStringList;
// Pokuaj ubrzavanja funkcije u verziji 465
var
   i           : Integer;
   j           : Integer;
   a : Integer;
   b : integer;
   Vmod1       : Float;
   Imod1       : Float;
   Imod1min    : Float;
   Imod1max    : Float;
   Vmod2       : Float;
   Imod2       : Float;
   Imod2min    : Float;
   Imod2max    : Float;
   Vsub1       : Float;
   Isub1       : Float;
   Isub1min    : Float;
   Isub1max    : Float;
   Vsub2       : Float;
   Isub2       : Float;
   Isub2min    : Float;
   Isub2max    : Float;
   Voltage     : String;
   Current     : String;
   CurrentMin  : String;
   CurrentMax  : String;
   space1      : String;
   space2      : String;
   space3      : String;
   space4      : String;
   Final       : TStringList;
begin
   i := 0;
   j := 0;

   Final := TstringList.Create;

   while (i < Model.Count) or (j < Submodel.Count) do
   begin
      Vmod1 := StrToFloat(GetToken(Model[i],1,';'));
      VSub1 := StrToFloat(GetToken(Submodel[j],1,';'));

      Imod1 := StrToFloat(GetToken(Model[i],2,';'));
      Isub1 := StrToFloat(GetToken(Submodel[j],2,';'));

      Imod1min := StrToFloat(GetToken(Model[i],3,';'));
      Isub1min := StrToFloat(GetToken(Submodel[j],3,';'));

      Imod1max := StrToFloat(GetToken(Model[i],4,';'));
      Isub1max := StrToFloat(GetToken(Submodel[j],4,';'));

      if Vmod1 = Vsub1 then // If they are equal count them
      begin

         // Count Currents
         Imod1 := Imod1 + Isub1;
         Imod1min := Imod1min + Isub1min;
         Imod1max := Imod1max + Isub1max;

         // Format current values to strings
         Voltage := FormatFloat('0.0000000E+00', Vmod1);
         Current := FormatFloat('0.0000000E+00', Imod1);
         CurrentMin := FormatFloat('0.0000000E+00', Imod1min);
         CurrentMax := FormatFloat('0.0000000E+00', Imod1max);

         // Finalizing - adding
         Final.Add(Voltage + ';' + Current + ';' + CurrentMin + ';' + CurrentMax);
      end
      else if Vmod1 < VSub1 then
      begin
         // This adds Submodel Voltage value.

         // We need next voltage value from model
         Vmod2 := StrToFloat(GetToken(Model[i+1],1,';'));

         // Next point of current in Model
         Imod2 := StrToFloat(GetToken(Model[i+1],2,';'));
         Imod2min := StrToFloat(GetToken(Model[i+1],3,';'));
         Imod2max := StrToFloat(GetToken(Model[i+1],4,';'));

         // Calculating Currents
         Imod1 := ((Imod2 - Imod1)/(Vmod2 - Vmod1))*(Vsub1 - Vmod1) + Imod1 + Isub1;
         Imod1min := ((Imod2min - Imod1min)/(Vmod2 - Vmod1))*(Vsub1 - Vmod1) + Imod1min + Isub1min;
         Imod1max := ((Imod2max - Imod1max)/(Vmod2 - Vmod1))*(Vsub1 - Vmod1) + Imod1max + Isub1max;


         // Format current values to strings
         Voltage := FormatFloat('0.0000000E+00', Vsub1);
         Current := FormatFloat('0.0000000E+00', Imod1);
         CurrentMin := FormatFloat('0.0000000E+00', Imod1min);
         CurrentMax := FormatFloat('0.0000000E+00', Imod1max);

         // Finalizing - adding
         Final.Add(Voltage + ';' + Current + ';' + CurrentMin + ';' + CurrentMax);
      end
      else if Vmod1 > VSub1 then
      begin
         // This adds model voltage value

         // We need next voltage value from submodel
         Vsub2 := StrToFloat(GetToken(Submodel[j+1],1,';'));

         // Next point in current of Submodel
         ISub2 := StrToFloat(GetToken(Submodel[j+1],2,';'));
         ISub2min := StrToFloat(GetToken(Submodel[j+1],3,';'));
         ISub2max := StrToFloat(GetToken(Submodel[j+1],4,';'));

         // Calculating Currents
         Isub1 := ((Isub2 - Isub1)/(Vsub2 - Vsub1))*(Vmod1 - Vsub1) + Isub1 + Imod1;
         Isub1min := ((Isub2min - Isub1min)/(Vsub2 - Vsub1))*(Vmod1 - Vsub1) + Isub1min + Imod1min;
         Isub1max := ((Isub2max - Isub1max)/(Vsub2 - Vsub1))*(Vmod1 - Vsub1) + Isub1max + Imod1max;

         // Format current values to strings
         Voltage := FormatFloat('0.0000000E+00', Vmod1);
         Current := FormatFloat('0.0000000E+00', Isub1);
         CurrentMin := FormatFloat('0.0000000E+00', Isub1min);
         CurrentMax := FormatFloat('0.0000000E+00', Isub1max);

         // Finalizing - adding
         Final.Add(Voltage + ';' + Current + ';' + CurrentMin + ';' + CurrentMax);
      end;

      // I have to check weather one table is finished. If that is the case,
      // I need to copy the rest of the remaining table

      if i = Model.Count-1 then
      begin
         // In this case i need to copy remaining of Submodel table
         // IMO this will always happen happen, but it might not be
         while j < Submodel.Count do
         begin
            VSub1 := StrToFloat(GetToken(Submodel[j],1,';'));
            Isub1 := StrToFloat(GetToken(Submodel[j],2,';'));
            Isub1min := StrToFloat(GetToken(Submodel[j],3,';'));
            Isub1max := StrToFloat(GetToken(Submodel[j],4,';'));

            Voltage := FormatFloat('0.0000000E+00', Vsub1);
            Current := FormatFloat('0.0000000E+00', Isub1);
            CurrentMin := FormatFloat('0.0000000E+00', Isub1min);
            CurrentMax := FormatFloat('0.0000000E+00', Isub1max);

            // Finalizing - adding
            Final.Add(Voltage + ';' + Current + ';' + CurrentMin + ';' + CurrentMax);

            Inc(j);
         end;

         // Set result and exit
         Result := Final;
         exit;
      end;

      if j = Submodel.Count-1 then
      begin
         // In this case i need to copy remaining of Model table
         // IMO this will never happen, but anyway :-)
         while i < Model.Count do
         begin

            Vmod1 := StrToFloat(GetToken(Model[i],1,';'));
            Imod1 := StrToFloat(GetToken(Model[i],2,';'));
            Imod1min := StrToFloat(GetToken(Model[i],3,';'));
            Imod1max := StrToFloat(GetToken(Model[i],4,';'));

            Voltage := FormatFloat('0.0000000E+00', Vmod1);
            Current := FormatFloat('0.0000000E+00', Imod1);
            CurrentMin := FormatFloat('0.0000000E+00', Imod1min);
            CurrentMax := FormatFloat('0.0000000E+00', Imod1max);

            // Finalizing - adding
            Final.Add(Voltage + ';' + Current + ';' + CurrentMin + ';' + CurrentMax);

            Inc(i);
         end;

         // Set result and exit
         Result := Final;
         exit;
      end;


      // We repeat this values because we want to avoid overflow, and it will not
      // happen if in previous step we exit the function

      Vmod2 := StrToFloat(GetToken(Model[i+1],1,';'));
      VSub2 := StrToFloat(GetToken(Submodel[j+1],1,';'));

      // increment step
      if Vmod2 = Vsub2 then
      begin
         Inc(i);
         Inc(j);
      end
      else if Vmod2 < Vsub2 then
      begin
         Inc(i);
      end
      else if Vmod2 > Vsub2 then
      begin
         Inc(j);
      end;
   end;
end;



function DeleteExtras(Table : TStringList) : TStringList;
var
   i         : Integer;
   j         : Integer;
   PrevValue : Float;
   NextValue : Float;
   Distance  : Float;
begin
   // Now we need to delete multiple entries.
   i := 1;
   while i < Table.Count do
   begin
      if GetToken(Table[i],1,';') =
         GetToken(Table[i - 1],1,';') then
         begin
            Table.Delete(i);
            Dec(i);
         end;
      Inc(i);
   end;
   i := 1;
   // We need to make sure that when we have more than 100 entries, we make them be only 100
   // To do this we need to delete some
   while Table.Count > 100 do
   begin

      // PrevValue - current value - 1
      // NextValue - current value + 1
      // Distance - distance between PrevValue and NextValue
      // j - index

      Distance := 99.9;
      i := 0;
      j := 0;
      for i := 1 to Table.Count - 2 do
      begin
         PrevValue := StrtoFloat(GetToken(Table[i-1],1,';'));
         NextValue := StrtoFloat(GetToken(Table[i+1],1,';'));
         if Distance > NextValue - PrevValue then
         begin
            Distance := NextValue - PrevValue;
            j := i;
         end;
      end;
      Table.Delete(j);
   end;
   Result := Table;
end;



function FormatTables(Table : TStringList) : TStringList;
var
   i           : Integer;
   Voltage     : String;
   Current     : String;
   CurrentMin  : String;
   CurrentMax  : String;
   space1      : String;
   space2      : String;
   space3      : String;
   space4      : String;
   Final       : TStringList;
begin
   Final := TStringList.Create;
   For i:= 0 to Table.Count - 1 do
   begin
      Voltage    := GetToken(Table[i],1,';');
      Current    := GetToken(Table[i],2,';');
      CurrentMin := GetToken(Table[i],3,';');
      CurrentMax := GetToken(Table[i],4,';');

      space1 := '     ';
      space2 := '     ';
      space3 := '     ';
      space4 := '     ';

      if Voltage[1]    = '-' then space1 := '    ';
      if Current[1]    = '-' then space2 := '    ';
      if CurrentMin[1] = '-' then space3 := '    ';
      if CurrentMax[1] = '-' then space4 := '    ';

      Final.Add(space1 + Voltage + space2 + Current + space3 + CurrentMin + space4 + CurrentMax);
   end;
   Result := Final;
end;



function SubmodelOverride();
   //*************************************************************************//
   //                                                                         //
   //-----------------    Submodel Override    -------------------------------//
   //                                                                         //
   //*************************************************************************//
   {
   This part of the code is added to support submodel. information from Submodel
   will be added to the main model.

   Add Submodel mode can be driving or non-driving (or, rarely, all). Only way to
   override submodel is to import it to main model. To do this user has to choose
   weather he wants to get IBIS file for input or output pins.

   User has to be chareful to use IBIS file based on radio-buttons selection. If
   you create IBIS files with "Input pins" radio button selected, do not place pins
   for this components as outputs during analysis, since you will get wrong results

   Sorry - I do not know how to explain this better, but - if you checked "Input pins"
   radio button, do not use this IBIS model if you want to have output signals
   on pins from this component during analysis.

   One more limit - this works only if you have [GND Clamp] and [POWER Clamp] in
   Submodel. If you have anything else, this will not work. However, in this case
   contact me and we will see what we can do :-)
   }

var
   i           : Integer;
   j           : Integer;
   k           : Integer;
   StepCounter : Integer;
   GNDClampLine: Integer;
   PWRClampLine: Integer;
   Line        : String;
   TempString  : String;
   ModelName   : String;
   SubmodelName: String;
   TempArray   : TStringList;
   ModelPWR    : TStringList;
   ModelGND    : TStringList;
   SubPWR      : TStringList;
   SubGND      : TStringList;
   FinalPWR    : TStringList;
   FinalGND    : TStringList;
   Voltage     : String;
   Current     : String;
   CurrentMin  : String;
   CurrentMax  : String;
   PrevSubmodel: String;
begin

   // We will temporarely copy FileArray TStringList to new list, if something goes wrong;
   TempArray := TStringList.Create;
   TempArray := FileArray;

   // Tables will be populated here
   ModelPWR := TStringList.Create;
   ModelGND := TStringList.Create;
   SubPWR   := TStringList.Create;
   SubGND   := TStringList.Create;

   i := 0;

   while i < FileArray.Count-1 do
   begin
      Line := FileArray[i];
      if (Line[1] = '[') then
      begin
         if (Line[2] = 'A') and (Line[3] = 'd') then
         begin
            // Found [Add Submodel] keyword

            // First we will position ourselves next few lines where info about submodel is
            while (Line[1] = '[') or (Line [1] = '|') do
            begin
               Inc(i);
               Line := FileArray[i];
            end;

            // Get Submodel name
            SubmodelName := GetToken(Line,1,' ');

            // if driving mode is good
            if ((AnsiCompareText(GetToken(RemoveMultipleSpaces(Line),2,' '), 'Driving') = 0) and (RadioButtonOutputs.Checked = True)) or
               ((AnsiCompareText(GetToken(RemoveMultipleSpaces(Line),2,' '), 'Non-Driving') = 0) and (RadioButtonInputs.Checked = True)) then
               begin
                  // Here we need to copy this tables and delete them from FileArray

                  // StepCounter gets incremented when a table is saved
                  StepCounter := 0;
                  While (StepCounter < 2) do
                  begin
                     Line := FileArray[i];
                     inc(i);
                     if Line[1] = '[' then
                     begin
                        if (AnsiCompareText(GetToken(Line,1,' '), '[GND') = 0) and
                           (AnsiCompareText(GetToken(RemoveMultipleSpaces(Line),2,' '), 'Clamp]') = 0) then
                           begin
                              Line := FileArray[i];
                              GNDClampLine := 0;
                              k := 0;  // temp variable used in showmessage
                              while (Line[1] <> '[') do
                              begin
                                 if (Line [1] <> '|') then
                                 begin
                                    TempString := LineCheck(Line);
                                    if TempString = '' then
                                    begin
                                       ShowMessage('Error in line ' + IntToStr(i+k+1) + '. No IBIS Generated.');
                                       FileArray := TempArray;
                                       exit;
                                    end;

                                    ModelGND.Add(TempString);

                                    // We will delete this line from FileArray,
                                    //and decrement "i" to get correct value next
                                    FileArray.Delete(i);
                                    GNDClampLine := i;
                                    Dec(i);
                                    Inc(k);
                                 end;
                                 inc(i);
                                 Line := FileArray[i];
                              end;
                              Inc(StepCounter);
                           end
                        else if (AnsiCompareText(GetToken(Line,1,' '), '[POWER') = 0) and
                           (AnsiCompareText(GetToken(RemoveMultipleSpaces(Line),2,' '), 'Clamp]') = 0) then
                           begin
                              Line := FileArray[i];
                              PWRClampLine := 0;
                              k := 0;   // temp variable used in showmessage
                              while (Line[1] <> '[') do
                              begin
                                 if (Line [1] <> '|') then
                                 begin
                                    TempString := LineCheck(Line);
                                    if TempString = '' then
                                    begin
                                       ShowMessage('Error in line ' + IntToStr(i+k+1) + '. No IBIS Generated.');
                                       FileArray := TempArray;
                                       exit;
                                    end;

                                    ModelPWR.Add(TempString);

                                    // We will delete this line from FileArray,
                                    // and decrement i to get correct value next
                                    FileArray.Delete(i);
                                    PWRClampLine := i;
                                    Dec(i);
                                    Inc(k);
                                 end;
                                 inc(i);
                                 Line := FileArray[i];
                              end;
                              Inc(StepCounter);
                           end;
                     end;
                  end;

                  // Now we have ModelGND and ModelPWR


                  // next we need SubGND and SubPWR

                  StepCounter := 0;
                  j := i;

                  while (StepCounter < 2) do
                  begin
                     Line := FileArray[j];
                     Inc(j);
                     if Line[1] = '[' then
                     begin
                        if (AnsiCompareText(GetToken(Line,1,' '), '[Submodel]') = 0) then
                        begin
                           if (AnsiCompareText(GetToken(RemoveMultipleSpaces(Line),2,' '), SubmodelName) = 0) then
                           begin
                              while (StepCounter < 2) do
                              begin
                                 Line := FileArray[j];
                                 inc(j);
                                 if Line[1] = '[' then
                                 begin
                                    if (AnsiCompareText(GetToken(Line,1,' '), '[GND') = 0) and
                                       (AnsiCompareText(GetToken(RemoveMultipleSpaces(Line),2,' '), 'Clamp]') = 0) then
                                       begin
                                          Line := FileArray[j];
                                          k := 0;   // temp variable used in showmessage
                                          while (Line[1] <> '[') do
                                          begin
                                             if (Line [1] <> '|') then
                                             begin
                                                TempString := LineCheck(Line);
                                                if TempString = '' then
                                                begin
                                                   ShowMessage('Error in line' + IntToStr(j+k+1) + '. No IBIS Generated.');
                                                   FileArray := TempArray;
                                                   exit;
                                                end;

                                                SubGND.Add(TempString);
                                             end;
                                             inc(j);
                                             Line := FileArray[j];
                                             inc(k);
                                          end;
                                          Inc(StepCounter);
                                       end
                                    else if (AnsiCompareText(GetToken(Line,1,' '), '[POWER') = 0) and
                                       (AnsiCompareText(GetToken(RemoveMultipleSpaces(Line),2,' '), 'Clamp]') = 0) then
                                       begin
                                          Line := FileArray[j];
                                          k := 0;   // temp variable used in showmessage
                                          while (Line[1] <> '[') do
                                          begin
                                             if (Line [1] <> '|') then
                                             begin
                                                TempString := LineCheck(Line);
                                                if TempString = '' then
                                                begin
                                                   ShowMessage('Error in line' + IntToStr(j+k+1) + '. No IBIS Generated.');
                                                   FileArray := TempArray;
                                                   exit;
                                                end;

                                                SubPWR.Add(TempString);
                                             end;
                                             Inc(j);
                                             Line := FileArray[j];
                                             inc(k);
                                          end;
                                          Inc(StepCounter);
                                       end;
                                 end;
                              end;
                           end;
                        end;
                     end;
                  end;


                  // Now we have tables in 4 string lists. We need to add those.
                  // We will populate new Tables to two new string Lists - FinalPWR and FinalGND

                  FinalPWR := TStringList.Create;
                  FinalGND := TStringList.Create;

                  // Tables are added in external function
                  FinalPWR := FormatTables(DeleteExtras(CountTables(ModelPWR, SubPWR)));
                  FinalGND := FormatTables(DeleteExtras(CountTables(ModelGND, SubGND)));


                  // Last we need to place this tables in IBIS model.

                  for j := 0 to FinalPWR.Count - 1 do
                     FileArray.Insert(PWRClampLine + j, FinalPWR[j]);

                  for j := 0 to FinalGND.Count - 1 do
                     FileArray.Insert(GNDClampLine + j, FinalGND[j]);

                  // Empty all string lists
                  ModelPWR.Clear;
                  ModelGND.Clear;
                  SubPWR.Clear;
                  SubGND.Clear;
                  FinalPWR.Clear;
                  FinalGND.Clear;

               end;
         end;
      end;
      Inc(i);
   end;
end;



procedure TIBISEditor.ButtonSaveClick(Sender: TObject);
var
   i        : Integer;
   flag     : Integer;
   FileName : String;
   Position : Integer;
begin

   SaveDialog1.Filter := 'IBIS Model (*.ibs)|*.ibs';
   // We need to repeat this if filename is same as original IBIS file
   // Or if we have white spaces in ibis file name
   repeat
      flag := 1;
      i := SaveDialog1.Execute;
      if (not i) then exit;
      if (SaveDialog1.FileName = LabelOpen.Caption) then
      begin
         ShowMessage('Source and Destination files MUST be different');
         flag := 0;
      end;

      if flag = 1 then
      begin
         FileName := SaveDialog1.FileName;
         repeat
            delete(FileName, Length(FileName),1);
         until FileName[Length(FileName)] = '\';

         Position := Length(FileName);
         FileName := SaveDialog1.FileName;
         delete(FileName, 1,Position);

         for i := 1 to Length(FileName) do
            if FileName[i] = ' ' then
            begin
               Showmessage('File name must not contain white spaces');
               flag := 0;
               break;
            end;
      end;
   until (flag = 1);



   // Model Selector override
   if CheckBoxModelSelector.State = cbChecked then
      ModelSelectorOverride;


   // Add Submodel Override
   if CheckBoxSubmodel.State = cbChecked then
      SubmodelOverride;


   // Modify the IBIS file name and version number
   ModifyFileName;


   // Save FileArray TStringList to Choosen File
   FileArray.SaveToFile(SaveDialog1.FileName);
   ShowMessage('Done');
end;



procedure TIBISEditor.CheckBoxSubmodelClick(Sender: TObject);
begin
   if CheckBoxSubmodel.State = cbChecked then
   begin
      RadioButtonInputs.Enabled := True;
      RadioButtonOutputs.Enabled := True;
      ShowMessage('This will work only if you have [GND Clamp] and [POWER Clamp] in [Submodel]' +
          ' (and nothing else). Most Micron DDR IBIS files that I have seen are like this.' +
          ' Contact me if you need other keywords, so we will see what we can do.');
   end
   else
   begin
      RadioButtonInputs.Enabled := False;
      RadioButtonOutputs.Enabled := False;
   end;
end;



procedure TIBISEditor.IBISEditorResize(Sender: TObject);
begin
   ButtonSave.Left := IBISEditor.Width - 130;
   StringGridPins.ColWidths[3] := StringGridPins.width - StringGridPins.ColWidths[0]
                   - StringGridPins.ColWidths[1] - StringGridPins.ColWidths[2] - 24;
   if StringGridPins.ColWidths[3] < 100 then
      StringGridPins.ColWidths[3] :=100;
   If StringGridPins.Enabled = False then
   begin
      StringGridPins.RowCount := (IBISEditor.Height - 88) / StringGridPins.DefaultRowHeight;
   end;
   WinXPTabControl.Height:=IBISEditor.Height-110;
end;



procedure TIBISEditor.WinXPTabControlChange(Sender: TObject);
var
   i : integer;
begin
   ComboBoxModelSelect.Visible := 0;

   // If first tab is selected
   if WinXPTabControl.TabIndex = 0 then
   begin
      StringGridPins.Visible := True;
      GroupBoxOverride.Visible := False;
      GroupBoxDescription.Visible := False;
      GroupBoxModelSelectorGroup.Visible := False;
      RadioGroupAltiumVersion.Visible := False;
      RadioButtonRelease.Visible := False;
      RadioButtonSummer.Visible := False;
      for i := 0 to ModelSelectorPins.Count - 1 do // StringGridPins has one extra fixed top row
      begin
         if StringGridPins.Cells[3,i + 1] = NoSelectorPinDescription then
            StringGridPins.Cells[3,i + 1] := EditModelPin.Text
         else if StringGridPins.Cells[3,i + 1] = SystemPinDescription then
            StringGridPins.Cells[3,i + 1] := EditSystemPin.Text;
      end;
   end
   else
   // if second tab is selected
   begin
      StringGridPins.Visible := False;
      GroupBoxOverride.Visible := True;
      GroupBoxDescription.Visible := True;
      GroupBoxModelSelectorGroup.Visible := True;
      RadioGroupAltiumVersion.Visible := True;
      RadioButtonRelease.Visible := True;
      RadioButtonSummer.Visible := True;
      //This global variable is used as helper on pins with no model selector
      NoSelectorPinDescription := EditModelPin.Text;
      SystemPinDescription := EditSystemPin.Text;
   end;
end;



procedure TIBISEditor.CheckBoxModelSelectorGroupsClick(Sender: TObject);
begin
   if CheckBoxModelSelectorGroups.Checked = False then
      StringGridModelSelectors.Enabled := True
   else
      StringGridModelSelectors.Enabled := True;
end;



procedure TIBISEditor.StringGridModelSelectorsClick(Sender: TObject);
var
   Rct : TCoordRect;
   SelectorName : String;
   NumOfModels : Integer;
   i : Integer;
   flag : Integer;
begin
   if (ModelSelector.Count = 0) or (CheckBoxModelSelectorGroups.Checked = False) then exit;
   flag := -1;
   if ((StringGridModelSelectors.Col = 1) or (StringGridModelSelectors.Col = 2)) then
   begin

      // First we will check weather tedt in selected column appears among Model Selectors
      // If it is only a model, GND, POWER or NC we will not display ComboBox
      for i := 0 to ModelSelector.Count-1 do
      begin
         SelectorName := GetToken(ModelSelector[i],1,';');
         if SelectorName = StringGridModelSelectors.Cells[0,StringGridModelSelectors.Row] then
             flag := i;
      end;
      if flag = -1 then
      begin
         ComboBoxModelGroup.Visible := False;
         exit;
      end;

      // Now we need to set combo box position
      Rct := StringGridModelSelectors.CellRect(StringGridModelSelectors.Col, StringGridModelSelectors.Row);
      ComboBoxModelGroup.Top := Rct.Top + StringGridModelSelectors.Top + 2;
      ComboBoxModelGroup.Left := Rct.Left + StringGridModelSelectors.Left + 2;
      ComboBoxModelGroup.Height := Rct.Bottom - Rct.Top;
      ComboBoxModelGroup.Width := Rct.Right - Rct.Left;

      // now we will populate combo box with model selector values
      ComboBoxModelGroup.Items.Clear;
      NumOfModels := StrToInt(GetToken(ModelSelector[flag],2,';'));
      for i := 1 to NumOfModels do
         ComboBoxModelGroup.Items.Add(GetToken(ModelSelector[flag],(i*2 + StringGridModelSelectors.Col),';'));

      // Set Text to current text
      ComboBoxModelGroup.Text := StringGridModelSelectors.Cells[StringGridModelSelectors.Col, StringGridModelSelectors.Row];

      // Set combo box visible
      ComboBoxModelGroup.Visible := True;
      // GroupBoxModelSelectorGroup.Visible := False;
   end
   else
      ComboBoxModelSelect.Visible := False;
end;



procedure TIBISEditor.ComboBoxModelGroupChange(Sender: TObject);
var
   i     : Integer;
   j     : Integer;
   Tekst : String;
   ModelSelectorName : String;
   NumOfModels       : Integer;
begin
   // Take text from combo box
   Tekst := ComboBoxModelGroup.Text;

   // Place Text from combo box inside String Grid (Model or Description)
   StringGridModelSelectors.Cells[StringGridModelSelectors.Col, StringGridModelSelectors.Row] := Tekst;

   // Placing Description inside String Grid if Model was chosen or
   // Placing Model inside String Grid if Description was chosen.
   ModelSelectorName := StringGridModelSelectors.Cells[0, StringGridModelSelectors.Row];
   for i := 0 to ModelSelector.Count - 1 do
   begin
      if (AnsiCompareText(GetToken(ModelSelector[i],1,';'), ModelSelectorName) = 0) then
      begin
         NumOfModels := StrToInt(GetToken(ModelSelector[i],2,';'));
         for j := 1 to NumOfModels do
         begin
            if ((StringGridModelSelectors.Col = 1) and (GetToken(ModelSelector[i],(j*2+1),';') =  Tekst)) then
               StringGridModelSelectors.Cells[StringGridModelSelectors.Col + 1, StringGridModelSelectors.Row] :=
                  GetToken(ModelSelector[i],(j*2+2),';');
            if ((StringGridModelSelectors.Col = 2) and (GetToken(ModelSelector[i],(j*2+2),';') =  Tekst)) then
               StringGridModelSelectors.Cells[StringGridModelSelectors.Col - 1, StringGridModelSelectors.Row] :=
                  GetToken(ModelSelector[i],(j*2+1),';');
         end;
      end;
   end;
   ComboBoxModelGroup.Visible := False;

   // Here we need to modify StringGridPins to have pin models we have chosen
   // For that we need StringGridPins - List of model selectors that we use
   // to populate StringGridPins Table.

   for i := 1 to ModelSelectorPins.Count - 1 do
      for j := 1 to StringGridModelSelectors.RowCount - 1 do
      begin
         ModelSelectorName := ModelSelectorPins[i];
         Tekst := StringGridModelSelectors.Cells[0,j];
         if (AnsiCompareText(ModelSelectorPins[i], StringGridModelSelectors.Cells[0,j]) = 0) then
         begin
            StringGridPins.Cells[2, i + 1] := StringGridModelSelectors.Cells[1, j];
            StringGridPins.Cells[3, i + 1] := StringGridModelSelectors.Cells[2, j];
         end;
      end;
end;
