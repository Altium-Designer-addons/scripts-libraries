{..............................................................................}
{ Summary   Imports project parameters into the focused PCB project (.PrjPcb)  }
{           from a delimited text file (.txt / .csv).                          }
{                                                                              }
{           File layout (two rows):                                            }
{             - Row 1 contains the parameter names (headers).                  }
{             - Row 2 contains the corresponding values.                       }
{           Header names must match the parameter names in the project.        }
{           Fields may be quoted with double quotes (e.g. "Doe, John").        }
{                                                                              }
{           Existing project parameters are updated in place, parameters that  }
{           do not yet exist in the project are added.                         }
{                                                                              }
{           The script prompts for the path and file name of the file to use.  }
{                                                                              }
{           Excel/Access files must first be converted to this CSV format      }
{           using the supplied Python helper script (see README).              }
{                                                                              }
{ Created by:    GitHub Copilot                                                }
{..............................................................................}

{..............................................................................}

uses
SysUtils;

Interface

type
   TImportProjectParamsForm = class(TForm)
      LabelFile    : TLabel;
      EditFile     : TEdit;
      ButtonBrowse : TButton;
      ButtonOK     : TButton;
      ButtonCancel : TButton;
      procedure ButtonBrowseClick(Sender: TObject);
      procedure ButtonOKClick(Sender: TObject);
      procedure ButtonCancelClick(Sender: TObject);
   end;

var
   ImportProjectParamsForm : TImportProjectParamsForm;
   FocusedProject          : IProject;

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
{ Reads the current project parameters from the .PrjPcb file.                   }
{                                                                              }
{ Names and Values are filled in file order. Returns True on success.          }
{..............................................................................}
function ReadProjectParameters(Project; Names, Values : TStringList) : Boolean;
var
   lines    : TStringList;
   projPath : String;
   i, k     : Integer;
   inParam  : Boolean;
   curName  : String;
   line     : String;
begin
   Result := False;
   Names.Clear;
   Values.Clear;

   projPath := Project.DM_ProjectFullPath;
   lines := TStringList.Create;
   try
      lines.LoadFromFile(projPath);

      { Parse every "[Parameter...]" block in the file. }
      inParam := False;
      curName := '';
      for i := 0 to lines.Count - 1 do
      begin
         line := Trim(lines[i]);

         if Copy(line, 1, 10) = '[Parameter' then
         begin
            inParam := True;
            curName := '';
            Continue;
         end;

         if inParam then
         begin
            { End of this parameter block when another section starts. }
            if Copy(line, 1, 1) = '[' then
            begin
               inParam := False;
               curName := '';
               Continue;
            end;

            if Copy(line, 1, 5) = 'Name=' then
               curName := Copy(line, 6, Length(line))
            else if Copy(line, 1, 6) = 'Value=' then
            begin
               Names.Add(curName);
               Values.Add(Copy(line, 7, Length(line)));
               curName := '';
            end;
         end;
      end;

      { Deduplicate: keep only the first occurrence of each parameter name. }
      { This removes stale duplicates that can accumulate after re-runs.    }
      i := 0;
      while i < Names.Count - 1 do
      begin
         k := i + 1;
         while k < Names.Count do
         begin
            if AnsiUpperCase(Names[k]) = AnsiUpperCase(Names[i]) then
            begin
               Names.Delete(k);
               Values.Delete(k);
            end
            else
               k := k + 1;
         end;
         i := i + 1;
      end;

      Result := True;
   finally
      lines.Free;
   end;
end;

{..............................................................................}
{ Writes the project parameters (Names/Values) back to the .PrjPcb file.       }
{                                                                              }
{ The existing "[Parameter...]" block is replaced, preserving the rest of the  }
{ file. Returns True on success.                                               }
{..............................................................................}
function WriteProjectFile(Project; Names, Values : TStringList) : Boolean;
var
   lines      : TStringList;
   outLines   : TStringList;
   projPath   : String;
   f          : TextFile;
   i, k       : Integer;
   inParam    : Boolean;
   insertAt   : Integer;
   line       : String;
   sawAnyParam: Boolean;
begin
   Result := False;
   projPath := Project.DM_ProjectFullPath;

   lines := TStringList.Create;
   outLines := TStringList.Create;
   try
      lines.LoadFromFile(projPath);

      { Remove ALL "[Parameter...]" blocks from the file (wherever they are),  }
      { so stale duplicate blocks left by earlier runs are cleaned up too.     }
      inParam := False;
      insertAt := -1;
      sawAnyParam := False;
      for i := 0 to lines.Count - 1 do
      begin
         line := Trim(lines[i]);

         if Copy(line, 1, 10) = '[Parameter' then
         begin
            inParam := True;
            if insertAt = -1 then insertAt := outLines.Count;
            sawAnyParam := True;
            Continue;
         end;

         if inParam then
         begin
            { A non-parameter section header ends the parameter block. }
            if Copy(line, 1, 1) = '[' then
            begin
               inParam := False;
               outLines.Add(lines[i]);
            end;
            { Otherwise skip the line (Name= / Value= / blank inside block). }
            Continue;
         end;

         outLines.Add(lines[i]);
      end;

      { If no parameter block was found, append the new block at the end,     }
      { after a blank line.                                                    }
      if not sawAnyParam then
      begin
         if (outLines.Count > 0) and (Trim(outLines[outLines.Count - 1]) <> '') then
            outLines.Add('');
         insertAt := outLines.Count;
      end;

      { Build the (merged) parameter block as a separate list. }
      k := 0;
      while k < Names.Count do
      begin
         outLines.Insert(insertAt + k * 4 + 0, '[Parameter' + IntToStr(k + 1) + ']');
         outLines.Insert(insertAt + k * 4 + 1, 'Name=' + Names[k]);
         outLines.Insert(insertAt + k * 4 + 2, 'Value=' + Values[k]);
         outLines.Insert(insertAt + k * 4 + 3, '');
         k := k + 1;
      end;

      { Write the modified project file back to disk. }
      AssignFile(f, projPath);
      Rewrite(f);
      for i := 0 to outLines.Count - 1 do WriteLn(f, outLines[i]);
      CloseFile(f);

      Result := True;
   finally
      lines.Free;
      outLines.Free;
   end;
end;

{..............................................................................}
{ Event handler: Browse button opens a file dialog filtered to CSV/text files. }
{..............................................................................}
procedure TImportProjectParamsForm.ButtonBrowseClick(Sender: TObject);
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
procedure TImportProjectParamsForm.ButtonCancelClick(Sender: TObject);
begin
   Close;
end;

{..............................................................................}
{ Event handler: Import button performs the actual import.                      }
{..............................................................................}
procedure TImportProjectParamsForm.ButtonOKClick(Sender: TObject);
var
   FileName     : String;
   ImportNames  : TStringList;
   ImportValues : TStringList;
   ProjNames    : TStringList;
   ProjValues   : TStringList;
   i, idx       : Integer;
   updatedCount : Integer;
   addedCount   : Integer;
   msg          : String;
begin
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
   ProjNames    := TStringList.Create;
   ProjValues   := TStringList.Create;
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

      if not ReadProjectParameters(FocusedProject, ProjNames, ProjValues) then
      begin
         ShowMessage('Failed to read the current project parameters.');
         Exit;
      end;

      { Merge imported values into the project parameters. }
      updatedCount := 0;
      addedCount   := 0;
      for i := 0 to ImportNames.Count - 1 do
      begin
         idx := FindNameCI(ProjNames, ImportNames[i]);
         if idx >= 0 then
         begin
            ProjValues[idx] := ImportValues[i];
            updatedCount := updatedCount + 1;
         end
         else
         begin
            ProjNames.Add(ImportNames[i]);
            ProjValues.Add(ImportValues[i]);
            addedCount := addedCount + 1;
         end;
      end;

      if not WriteProjectFile(FocusedProject, ProjNames, ProjValues) then
      begin
         ShowMessage('Failed to write the updated project parameters to the project file.');
         Exit;
      end;

      { NOTE: Do NOT call DoFileLoad here. Altium automatically notices the    }
      { project file changed on disk and reloads it. Explicitly reloading      }
      { (DoFileLoad) causes Altium to KEEP its old in-memory parameters and    }
      { re-add the on-disk ones, producing duplicates on the next run.         }
      { (This is the same finding documented in the XIA_Release_Manager        }
      { script in this repository.)                                            }

      msg := 'Import complete.' + #13#10#13#10 +
             'Updated: ' + IntToStr(updatedCount) + #13#10 +
             'Added:   ' + IntToStr(addedCount) + #13#10#13#10 +
             'Project parameters:' + #13#10;
      for i := 0 to ProjNames.Count - 1 do
         msg := msg + '  ' + ProjNames[i] + ' = ' + ProjValues[i] + #13#10;

      ShowMessage(msg);
      Close;
   finally
      ImportNames.Free;
      ImportValues.Free;
      ProjNames.Free;
      ProjValues.Free;
   end;
end;

{..............................................................................}
{ Entry point. Validates the focused project, then shows the import dialog.     }
{..............................................................................}
procedure ImportProjectParametersFromCSV;
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

   ImportProjectParamsForm.EditFile.Text := '';
   ImportProjectParamsForm.ShowModal;
end;
