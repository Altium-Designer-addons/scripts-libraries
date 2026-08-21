{..............................................................................}
{ Summary   Imports parameter overrides for a SINGLE project VARIANT from a     }
{           delimited text file (.txt / .csv) into the focused PCB project.    }
{                                                                              }
{           The script:                                                         }
{             - Lists all variants defined in the project.                      }
{             - Supports variant names in the format "00", "01", "02", ...     }
{             - Can create a new variant, named as the next consecutive         }
{               two-digit number after the current highest, inheriting the      }
{               configuration of the previous highest variant.                 }
{             - Imports the CSV parameters ONLY into the selected variant      }
{               (not into the whole project).                                   }
{                                                                              }
{           CSV layout (two rows):                                              }
{             - Row 1 contains the parameter names (headers).                  }
{             - Row 2 contains the corresponding values.                       }
{           Header names must match the parameter names in the project.        }
{           Fields may be quoted with double quotes (e.g. "Doe, John").        }
{                                                                              }
{           Based on ImportProjectParametersFromCSV.                           }
{                                                                              }
{ Created by:    GitHub Copilot                                                }
{..............................................................................}

{..............................................................................}

uses
SysUtils;

Interface

type
   TImportVariantParamsForm = class(TForm)
      LabelVariant      : TLabel;
      ComboBoxVariants  : TComboBox;
      ButtonNewVariant  : TButton;
      CheckBoxCopyParams: TCheckBox;
      LabelFile         : TLabel;
      EditFile          : TEdit;
      ButtonBrowse      : TButton;
      ButtonOK          : TButton;
      ButtonCancel      : TButton;
      procedure FormVariantShow(Sender: TObject);
      procedure ButtonBrowseClick(Sender: TObject);
      procedure ButtonNewVariantClick(Sender: TObject);
      procedure ButtonOKClick(Sender: TObject);
      procedure ButtonCancelClick(Sender: TObject);
   end;

var
   ImportVariantParamsForm : TImportVariantParamsForm;
   FocusedProject          : IProject;
   ProjLines               : TStringList;
   VariantHeaders          : TStringList;
   VariantStart            : TStringList;
   VariantEnd              : TStringList;

{..............................................................................}
{ Case-insensitive search for a string in a TStringList. Returns -1 if not     }
{ found.                                                                       }
{..............................................................................}
function FindNameCI(List : TStringList; Name : String) : Integer;
var
   i : Integer;
begin
   Result := -1;
   for i := 0 to List.Count - 1 do
   begin
      if AnsiUpperCase(List[i]) = AnsiUpperCase(Name) then
      begin
         Result := i;
         Exit;
      end;
   end;
end;

{..............................................................................}
{ Returns True if S is non-empty and contains only digits 0..9.                }
{..............................................................................}
function IsAllDigits(S : String) : Boolean;
var
   i : Integer;
begin
   Result := (S <> '');
   for i := 1 to Length(S) do
   begin
      if (S[i] < '0') or (S[i] > '9') then
      begin
         Result := False;
         Exit;
      end;
   end;
end;

{..............................................................................}
{ Formats a number as a two-digit zero-padded variant name ("0" -> "00").      }
{ Values >= 100 are returned without extra leading zeros.                      }
{..............................................................................}
function FormatVariantName(N : Integer) : String;
begin
   if N < 10 then
      Result := '0' + IntToStr(N)
   else
      Result := IntToStr(N);
end;

{..............................................................................}
{ Returns a single lowercase hex digit for N (0..15).                          }
{..............................................................................}
function HexDigit(N : Integer) : String;
const
   HexChars = '0123456789abcdef';
begin
   Result := Copy(HexChars, N + 1, 1);
end;

{..............................................................................}
{ Generates a random GUID string (version 4, like Altium's variant UniqueId).  }
{..............................................................................}
function GenerateGuid : String;
var
   i : Integer;
begin
   Result := '';
   for i := 1 to 8 do Result := Result + HexDigit(Random(16));
   Result := Result + '-';
   for i := 1 to 4 do Result := Result + HexDigit(Random(16));
   Result := Result + '-4';
   for i := 1 to 3 do Result := Result + HexDigit(Random(16));
   Result := Result + '-';
   Result := Result + HexDigit(8 + Random(4));
   for i := 1 to 3 do Result := Result + HexDigit(Random(16));
   Result := Result + '-';
   for i := 1 to 12 do Result := Result + HexDigit(Random(16));
end;

{..............................................................................}
{ Detects the field delimiter used in a CSV line.                              }
{                                                                              }
{ Supports comma (','), semicolon (';') and tab. Whichever occurs most often   }
{ (outside double-quoted fields) wins, so regional settings that use           }
{ semicolons instead of commas are handled automatically.                      }
{..............................................................................}
function DetectDelimiter(Line : String) : Char;
var
   i         : Integer;
   inQuotes  : Boolean;
   commaCnt  : Integer;
   semiCnt   : Integer;
   tabCnt    : Integer;
   ch        : Char;
begin
   Result := ',';

   commaCnt := 0;
   semiCnt  := 0;
   tabCnt   := 0;
   inQuotes := False;

   for i := 1 to Length(Line) do
   begin
      ch := Line[i];

      if ch = '"' then
      begin
         inQuotes := not inQuotes;
         Continue;
      end;

      if not inQuotes then
      begin
         if ch = ',' then commaCnt := commaCnt + 1
         else if ch = ';' then semiCnt := semiCnt + 1
         else if ch = #9 then tabCnt := tabCnt + 1;
      end;
   end;

   if (semiCnt > commaCnt) and (semiCnt >= tabCnt) then
      Result := ';'
   else if (tabCnt > commaCnt) and (tabCnt > semiCnt) then
      Result := #9
   else
      Result := ',';
end;

{..............................................................................}
{ Reads a delimited text file (two rows).                                      }
{ Row 1 -> parameter names, Row 2 -> parameter values.                         }
{                                                                              }
{ The delimiter is detected automatically (comma, semicolon or tab).           }
{ Fields may be enclosed in double quotes; quotes are stripped.                }
{ Names and Values are filled with one entry per non-empty header cell.        }
{ Returns True on success.                                                     }
{..............................................................................}
function ReadCsvFile(FileName : String; Names, Values : TStringList) : Boolean;
var
   Lines  : TStringList;
   HeaderFields : TStringList;
   ValueFields  : TStringList;
   Delim  : Char;
   i      : Integer;
begin
   Result := False;
   Names.Clear;
   Values.Clear;

   if not FileExists(FileName) then Exit;

   Lines := TStringList.Create;
   HeaderFields := TStringList.Create;
   ValueFields  := TStringList.Create;
   try
      Lines.LoadFromFile(FileName);

      { We need at least two rows. }
      if Lines.Count < 2 then Exit;

      { Detect the field delimiter from the first row. }
      Delim := DetectDelimiter(Lines[0]);

      { Parse row 1 (names) and row 2 (values), honouring double-quoted fields. }
      { StrictDelimiter=True means only the delimiter separates fields;         }
      { spaces inside a field (e.g. "test 4") are preserved.                    }
      HeaderFields.Delimiter := Delim;
      HeaderFields.QuoteChar := '"';
      HeaderFields.StrictDelimiter := True;
      HeaderFields.DelimitedText := Lines[0];

      ValueFields.Delimiter := Delim;
      ValueFields.QuoteChar := '"';
      ValueFields.StrictDelimiter := True;
      ValueFields.DelimitedText := Lines[1];

      { Pair each header with the matching value. }
      for i := 0 to HeaderFields.Count - 1 do
      begin
         if Trim(HeaderFields[i]) = '' then Continue;

         Names.Add(Trim(HeaderFields[i]));

         if i < ValueFields.Count then
            Values.Add(Trim(ValueFields[i]))
         else
            Values.Add('');
      end;

      Result := Names.Count > 0;
   finally
      Lines.Free;
      HeaderFields.Free;
      ValueFields.Free;
   end;
end;

{..............................................................................}
{ Loads the project file and locates all "[ProjectVariant...]" blocks.         }
{                                                                              }
{ Populates the globals:                                                       }
{   VariantHeaders  - the header line of each variant block                    }
{   VariantStart    - line index where each block starts (as string)           }
{   VariantEnd      - line index where each block ends (exclusive, as string)  }
{..............................................................................}
procedure LoadProjectStructure;
var
   i        : Integer;
   line     : String;
   trimmed  : String;
   inVariant: Boolean;
begin
   ProjLines.LoadFromFile(FocusedProject.DM_ProjectFullPath);

   VariantHeaders.Clear;
   VariantStart.Clear;
   VariantEnd.Clear;

   inVariant := False;
   for i := 0 to ProjLines.Count - 1 do
   begin
      line := ProjLines[i];
      trimmed := Trim(line);

      if Copy(trimmed, 1, 15) = '[ProjectVariant' then
      begin
         { Start of a new variant block. Close the previous one first. }
         if inVariant then VariantEnd.Add(IntToStr(i));
         VariantHeaders.Add(trimmed);
         VariantStart.Add(IntToStr(i));
         inVariant := True;
      end
      else if inVariant and (Copy(trimmed, 1, 1) = '[') then
      begin
         { A non-variant section header ends the current block. }
         VariantEnd.Add(IntToStr(i));
         inVariant := False;
      end;
   end;

   { If the file ends while still inside a block. }
   if inVariant then VariantEnd.Add(IntToStr(ProjLines.Count));
end;

{..............................................................................}
{ Returns the description (name) of the variant block at the given index.      }
{..............................................................................}
function GetVariantDescription(BlockIdx : Integer) : String;
var
   i, s, e : Integer;
   trimmed : String;
begin
   Result := '';
   s := StrToInt(VariantStart[BlockIdx]);
   e := StrToInt(VariantEnd[BlockIdx]);
   for i := s to e - 1 do
   begin
      trimmed := Trim(ProjLines[i]);
      if Copy(trimmed, 1, 12) = 'Description=' then
      begin
         Result := Trim(Copy(trimmed, 13, Length(trimmed)));
         Exit;
      end;
   end;
end;

{..............................................................................}
{ Returns the block index of the variant whose description matches Desc        }
{ (case-insensitive), or -1 if not found.                                      }
{..............................................................................}
function GetBlockIndexByDescription(Desc : String) : Integer;
var
   k : Integer;
begin
   Result := -1;
   for k := 0 to VariantHeaders.Count - 1 do
   begin
      if AnsiUpperCase(GetVariantDescription(k)) = AnsiUpperCase(Desc) then
      begin
         Result := k;
         Exit;
      end;
   end;
end;

{..............................................................................}
{ Refreshes the variant combo box from the current project structure.          }
{..............................................................................}
procedure PopulateVariantCombo;
var
   k : Integer;
begin
   ComboBoxVariants.Clear;
   for k := 0 to VariantHeaders.Count - 1 do
      ComboBoxVariants.Items.Add(GetVariantDescription(k));

   if ComboBoxVariants.Items.Count > 0 then
      ComboBoxVariants.ItemIndex := 0;
end;

{..............................................................................}
{ Returns the numeric index of a variant block (e.g. "[ProjectVariant2]" -> 2) }
{..............................................................................}
function GetVariantNumber(BlockIdx : Integer) : Integer;
var
   hdr : String;
begin
   hdr := VariantHeaders[BlockIdx];
   hdr := StringReplace(hdr, '[ProjectVariant', '', MkSet(rfReplaceAll));
   hdr := StringReplace(hdr, ']', '', MkSet(rfReplaceAll));
   Result := StrToInt(hdr);
end;

{..............................................................................}
{ Returns True if the given trimmed line is a variant parameter section        }
{ header belonging to variant block number N, i.e. "[Parameter(N+1)_Y]"       }
{ with Y all digits.                                                           }
{..............................................................................}
function IsVariantParamHeaderFor(Line : String; N : Integer) : Boolean;
var
   prefix : String;
   ys     : String;
begin
   Result := False;
   prefix := '[Parameter' + IntToStr(N + 1) + '_';
   if Copy(Line, 1, Length(prefix)) <> prefix then Exit;

   ys := Copy(Line, Length(prefix) + 1, Length(Line) - Length(prefix) - 1);
   Result := IsAllDigits(ys);
end;

{..............................................................................}
{ Reads the per-variant parameters of the variant at BlockIdx.                 }
{                                                                              }
{ Variant parameters are stored as separate "[ParameterX_Y]" sections          }
{ following the variant block, where X = variant number + 1 and Y is the       }
{ parameter index (1-based). Each section has a Name= and a Value= line.       }
{ Names and Values are filled in order.                                        }
{..............................................................................}
procedure ReadVariantParameters(BlockIdx : Integer; Names, Values : TStringList);
var
   n       : Integer;
   i       : Integer;
   trimmed : String;
   inside  : Boolean;
   curName : String;
begin
   Names.Clear;
   Values.Clear;

   n := GetVariantNumber(BlockIdx);

   inside := False;
   curName := '';
   for i := 0 to ProjLines.Count - 1 do
   begin
      trimmed := Trim(ProjLines[i]);

      if Copy(trimmed, 1, 1) = '[' then
      begin
         { Enter a section only if it is one of this variant's parameters. }
         inside := IsVariantParamHeaderFor(trimmed, n);
         curName := '';
         Continue;
      end;

      if inside then
      begin
         if Copy(trimmed, 1, 5) = 'Name=' then
            curName := Copy(trimmed, 6, Length(trimmed))
         else if Copy(trimmed, 1, 6) = 'Value=' then
         begin
            Names.Add(curName);
            Values.Add(Copy(trimmed, 7, Length(trimmed)));
            curName := '';
         end;
      end;
   end;
end;

{..............................................................................}
{ Builds the complete project file content with variant BlockIdx's parameters }
{ replaced by Names/Values.                                                    }
{                                                                              }
{ This removes the variant's existing "[Parameter(N+1)_Y]" sections, updates   }
{ the ParameterCount line inside the variant block, and re-inserts fresh       }
{ parameter sections right after the variant block.                            }
{                                                                              }
{ Returns a new TStringList (caller must free), or Nil on error.               }
{..............................................................................}
function BuildFileWithVariantParams(BlockIdx : Integer; Names, Values : TStringList) : TStringList;
var
   resultList : TStringList;
   n          : Integer;
   s, e       : Integer;
   i          : Integer;
   inVariantBlock : Boolean;
   insertAt   : Integer;
   trimmed    : String;
begin
   Result := Nil;
   n := GetVariantNumber(BlockIdx);

   s := StrToInt(VariantStart[BlockIdx]);
   e := StrToInt(VariantEnd[BlockIdx]);

   resultList := TStringList.Create;

   { Pass 1: copy the file, updating ParameterCount and dropping this          }
   { variant's existing parameter sections.                                    }
   inVariantBlock := False;
   i := 0;
   while i < ProjLines.Count do
   begin
      trimmed := Trim(ProjLines[i]);

      { Track whether we are inside the target variant block. }
      if i = s then inVariantBlock := True;
      if (i > s) and (Copy(trimmed, 1, 1) = '[') then inVariantBlock := False;

      { Drop this variant's existing parameter sections entirely. }
      if IsVariantParamHeaderFor(trimmed, n) then
      begin
         i := i + 1;
         while (i < ProjLines.Count) and (Copy(Trim(ProjLines[i]), 1, 1) <> '[') do
            i := i + 1;
         while (i < ProjLines.Count) and (Trim(ProjLines[i]) = '') do
            i := i + 1;
         Continue;
      end;

      { Update the ParameterCount line inside the target variant block. }
      if inVariantBlock and (Copy(trimmed, 1, 15) = 'ParameterCount=') then
         resultList.Add('ParameterCount=' + IntToStr(Names.Count))
      else
         resultList.Add(ProjLines[i]);

      i := i + 1;
   end;

   { Pass 2: find the end of the target variant block (next section header)    }
   { and insert the new parameter sections there.                              }
   insertAt := -1;
   for i := s + 1 to resultList.Count - 1 do
   begin
      if Copy(Trim(resultList[i]), 1, 1) = '[' then
      begin
         insertAt := i;
         Break;
      end;
   end;
   if insertAt = -1 then insertAt := resultList.Count;

   for i := Names.Count - 1 downto 0 do
   begin
      resultList.Insert(insertAt, '');
      resultList.Insert(insertAt, 'Value=' + Values[i]);
      resultList.Insert(insertAt, 'Name=' + Names[i]);
      resultList.Insert(insertAt, '[Parameter' + IntToStr(n + 1) + '_' + IntToStr(i + 1) + ']');
   end;

   Result := resultList;
end;

{..............................................................................}
{ Builds the complete project file content for creating a NEW variant: the new }
{ variant block (with fresh UniqueId and Description) plus parameter sections }
{ inherited from the source variant, numbered with the new variant's prefix.  }
{                                                                              }
{ NewBlockN = the "[ProjectVariantN]" number of the new variant (1-based).     }
{ Returns a new TStringList (caller must free), or Nil on error.               }
{..............................................................................}
function BuildFileWithNewVariant(SrcBlockIdx : Integer; NewBlockN : Integer; NewDesc : String; CopyParams : Boolean) : TStringList;
var
   resultList : TStringList;
   newHeader  : String;
   srcNames   : TStringList;
   srcValues  : TStringList;
   k          : Integer;
   insertAt   : Integer;
   i          : Integer;
begin
   Result := Nil;

   { Read the source variant's parameters (may be empty). }
   srcNames  := TStringList.Create;
   srcValues := TStringList.Create;
   try
      if CopyParams and (SrcBlockIdx >= 0) then
         ReadVariantParameters(SrcBlockIdx, srcNames, srcValues)
      else
      begin
         srcNames.Clear;
         srcValues.Clear;
      end;
   except
      srcNames.Free;
      srcValues.Free;
      Exit;
   end;

   resultList := TStringList.Create;
   newHeader := '[ProjectVariant' + IntToStr(NewBlockN) + ']';

   { Copy all existing lines. }
   for i := 0 to ProjLines.Count - 1 do
      resultList.Add(ProjLines[i]);

   { Find the insertion point: right after the last variant's parameter         }
   { sections, i.e. at the next top-level section header after the last         }
   { "[ProjectVariant...]" block (or end of file).                              }
   insertAt := resultList.Count;
   for i := 0 to resultList.Count - 1 do
   begin
      if Copy(Trim(resultList[i]), 1, 15) = '[ProjectVariant' then
      begin
         { Walk forward until the next top-level header (no underscore). }
         insertAt := resultList.Count;
         k := i + 1;
         while k < resultList.Count do
         begin
            if Copy(Trim(resultList[k]), 1, 1) = '[' then
            begin
               if Pos('_', Trim(resultList[k])) = 0 then
               begin
                  insertAt := k;
                  Break;
               end;
            end;
            k := k + 1;
         end;
         Break;
      end;
   end;

   { Ensure a blank line before the new block. }
   if (insertAt > 0) and (Trim(resultList[insertAt - 1]) <> '') then
   begin
      resultList.Insert(insertAt, '');
      insertAt := insertAt + 1;
   end;

   { Build the new variant block with inherited parameters count. }
   resultList.Insert(insertAt + 0, newHeader);
   resultList.Insert(insertAt + 1, 'UniqueId=' + GenerateGuid);
   resultList.Insert(insertAt + 2, 'Description=' + NewDesc);
   resultList.Insert(insertAt + 3, 'AllowFabrication=0');
   resultList.Insert(insertAt + 4, 'ParameterCount=' + IntToStr(srcNames.Count));
   resultList.Insert(insertAt + 5, 'VariationCount=0');
   resultList.Insert(insertAt + 6, 'ParamVariationCount=0');
   resultList.Insert(insertAt + 7, '');

   { Inherit the source variant's parameter sections under the new prefix. }
   { New variant's parameter prefix = NewBlockN + 1.                        }
   insertAt := insertAt + 8;
   for k := srcNames.Count - 1 downto 0 do
   begin
      resultList.Insert(insertAt, '');
      resultList.Insert(insertAt, 'Value=' + srcValues[k]);
      resultList.Insert(insertAt, 'Name=' + srcNames[k]);
      resultList.Insert(insertAt, '[Parameter' + IntToStr(NewBlockN + 1) + '_' + IntToStr(k + 1) + ']');
   end;

   srcNames.Free;
   srcValues.Free;

   Result := resultList;
end;

{..............................................................................}
{ Writes a list of lines to the project file.                                  }
{..............................................................................}
procedure WriteLinesToProjectFile(Lines : TStringList);
var
   f : TextFile;
   i : Integer;
begin
   AssignFile(f, FocusedProject.DM_ProjectFullPath);
   Rewrite(f);
   for i := 0 to Lines.Count - 1 do WriteLn(f, Lines[i]);
   CloseFile(f);
end;

{..............................................................................}
{ Finds the highest purely-numeric variant description and returns its block   }
{ index and numeric value. Returns -1 for both if none found.                  }
{..............................................................................}
procedure FindHighestNumericVariant(var BlockIdx : Integer; var Value : Integer);
var
   k   : Integer;
   desc: String;
   n   : Integer;
begin
   BlockIdx := -1;
   Value := -1;
   for k := 0 to VariantHeaders.Count - 1 do
   begin
      desc := GetVariantDescription(k);
      if IsAllDigits(desc) then
      begin
         n := StrToInt(desc);
         if n > Value then
         begin
            Value := n;
            BlockIdx := k;
         end;
      end;
   end;
end;

{..............................................................................}
{ Event handler: form is shown. Populate the variant list.                     }
{..............................................................................}
procedure TImportVariantParamsForm.FormVariantShow(Sender: TObject);
begin
   LoadProjectStructure;
   PopulateVariantCombo;
end;

{..............................................................................}
{ Event handler: Browse button opens a file dialog filtered to CSV/text files. }
{..............................................................................}
procedure TImportVariantParamsForm.ButtonBrowseClick(Sender: TObject);
var
   dlg : TOpenDialog;
begin
   dlg := TOpenDialog.Create(Application);
   try
      dlg.Title  := 'Select the CSV file to import';
      dlg.Filter := 'CSV files (*.csv)|*.csv|Text files (*.txt)|*.txt|All files (*.*)|*.*';
      if dlg.Execute then
         EditFile.Text := dlg.FileName;
   finally
      dlg.Free;
   end;
end;

{..............................................................................}
{ Event handler: Cancel button closes the dialog.                               }
{..............................................................................}
procedure TImportVariantParamsForm.ButtonCancelClick(Sender: TObject);
begin
   Close;
end;

{..............................................................................}
{ Event handler: Create a new variant named as the next consecutive two-digit  }
{ number after the current highest, inheriting that variant's configuration.   }
{..............................................................................}
procedure TImportVariantParamsForm.ButtonNewVariantClick(Sender: TObject);
var
   highIdx : Integer;
   highVal : Integer;
   newVal  : Integer;
   newName : String;
   srcIdx  : Integer;
   newFile : TStringList;
   k       : Integer;
begin
   LoadProjectStructure;

   FindHighestNumericVariant(highIdx, highVal);

   { Determine the source block to base the new variant on. }
   if highIdx >= 0 then
   begin
      srcIdx := highIdx;
      newVal := highVal + 1;
   end
   else
   begin
      { No numeric variants yet: base on the last variant block and start at 0. }
      srcIdx := VariantHeaders.Count - 1;
      newVal := 0;
   end;

   { If somehow there are no variant blocks at all, nothing to base on. }
   if srcIdx < 0 then
   begin
      ShowMessage('The project has no variant blocks to base a new variant on.');
      Exit;
   end;

   { Ensure the new name does not collide with an existing variant. }
   newName := FormatVariantName(newVal);
   while GetBlockIndexByDescription(newName) >= 0 do
   begin
      newVal := newVal + 1;
      newName := FormatVariantName(newVal);
   end;

   newFile := BuildFileWithNewVariant(srcIdx, VariantHeaders.Count + 1, newName, CheckBoxCopyParams.Checked);
   if newFile = Nil then
   begin
      ShowMessage('Failed to build the new variant.');
      Exit;
   end;

   try
      WriteLinesToProjectFile(newFile);
   finally
      newFile.Free;
   end;

   { Refresh the variant list and select the newly created variant. }
   LoadProjectStructure;
   PopulateVariantCombo;
   for k := 0 to ComboBoxVariants.Items.Count - 1 do
   begin
      if ComboBoxVariants.Items[k] = newName then
      begin
         ComboBoxVariants.ItemIndex := k;
         Break;
      end;
   end;

   ShowMessage('Created new variant "' + newName + '".');
end;

{..............................................................................}
{ Event handler: Import button imports the CSV parameters into the selected    }
{ variant only.                                                                }
{..............................................................................}
procedure TImportVariantParamsForm.ButtonOKClick(Sender: TObject);
var
   FileName     : String;
   VariantName  : String;
   BlockIdx     : Integer;
   ImportNames  : TStringList;
   ImportValues : TStringList;
   VarNames     : TStringList;
   VarValues    : TStringList;
   NewFile      : TStringList;
   i, idx       : Integer;
   updatedCount : Integer;
   addedCount   : Integer;
   msg          : String;
begin
   VariantName := Trim(ComboBoxVariants.Text);
   if VariantName = '' then
   begin
      ShowMessage('Please select a variant.');
      Exit;
   end;

   FileName := Trim(EditFile.Text);
   if FileName = '' then
   begin
      ShowMessage('Please enter the path and file name of the CSV file to import.');
      Exit;
   end;

   if not FileExists(FileName) then
   begin
      ShowMessage('The specified file was not found:' + #13#10 + FileName);
      Exit;
   end;

   ImportNames  := TStringList.Create;
   ImportValues := TStringList.Create;
   VarNames     := TStringList.Create;
   VarValues    := TStringList.Create;
   try
      if not ReadCsvFile(FileName, ImportNames, ImportValues) then
      begin
         ShowMessage('Failed to read the file. Ensure it is a two-row delimited text file.' + #13#10 +
                     'Row 1 = parameter names, row 2 = parameter values.');
         Exit;
      end;

      if ImportNames.Count = 0 then
      begin
         ShowMessage('No parameters found in the file.' + #13#10 +
                     'Row 1 must contain the parameter names and row 2 the parameter values.');
         Exit;
      end;

      { Re-read the project structure to get the latest state. }
      LoadProjectStructure;

      BlockIdx := GetBlockIndexByDescription(VariantName);
      if BlockIdx < 0 then
      begin
         ShowMessage('Variant "' + VariantName + '" was not found in the project.');
         Exit;
      end;

      ReadVariantParameters(BlockIdx, VarNames, VarValues);

      { Merge imported values into the variant parameters. }
      updatedCount := 0;
      addedCount   := 0;
      for i := 0 to ImportNames.Count - 1 do
      begin
         idx := FindNameCI(VarNames, ImportNames[i]);
         if idx >= 0 then
         begin
            VarValues[idx] := ImportValues[i];
            updatedCount := updatedCount + 1;
         end
         else
         begin
            VarNames.Add(ImportNames[i]);
            VarValues.Add(ImportValues[i]);
            addedCount := addedCount + 1;
         end;
      end;

      NewFile := BuildFileWithVariantParams(BlockIdx, VarNames, VarValues);
      if NewFile = Nil then
      begin
         ShowMessage('Failed to update the variant parameters.');
         Exit;
      end;

      try
         WriteLinesToProjectFile(NewFile);
      finally
         NewFile.Free;
      end;

      { NOTE: Do NOT call DoFileLoad here. Altium automatically notices the    }
      { project file changed on disk and reloads it. Explicitly reloading      }
      { (DoFileLoad) causes Altium to KEEP its old in-memory data and          }
      { re-add the on-disk data, producing duplicates.                         }
      { (Same finding as documented in the XIA_Release_Manager script.)        }

      msg := 'Import complete for variant "' + VariantName + '".' + #13#10#13#10 +
             'Updated: ' + IntToStr(updatedCount) + #13#10 +
             'Added:   ' + IntToStr(addedCount) + #13#10#13#10 +
             'Variant parameters:' + #13#10;
      for i := 0 to VarNames.Count - 1 do
         msg := msg + '  ' + VarNames[i] + ' = ' + VarValues[i] + #13#10;

      ShowMessage(msg);
      Close;
   finally
      ImportNames.Free;
      ImportValues.Free;
      VarNames.Free;
      VarValues.Free;
   end;
end;

{..............................................................................}
{ Entry point. Validates the focused project, then shows the import dialog.    }
{..............................................................................}
procedure ImportVariantParametersFromCSV;
var
   WS : IWorkspace;
begin
   WS := GetWorkspace;
   FocusedProject := WS.DM_FocusedProject;

   if FocusedProject = nil then
   begin
      ShowMessage('No project is currently focused.');
      Exit;
   end;

   if AnsiUpperCase(ExtractFileExt(FocusedProject.DM_ProjectFileName)) <> '.PRJPCB' then
   begin
      ShowMessage('The focused project is not a PCB project (.PrjPcb).');
      Exit;
   end;

   { Initialise the global string lists used to hold the project structure. }
   ProjLines      := TStringList.Create;
   VariantHeaders := TStringList.Create;
   VariantStart   := TStringList.Create;
   VariantEnd     := TStringList.Create;

   try
      ImportVariantParamsForm.EditFile.Text := '';
      ImportVariantParamsForm.ShowModal;
   finally
      ProjLines.Free;
      VariantHeaders.Free;
      VariantStart.Free;
      VariantEnd.Free;
   end;
end;
