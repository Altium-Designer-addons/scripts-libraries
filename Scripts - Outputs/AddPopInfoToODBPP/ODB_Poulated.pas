uses ShellApi;

Const
     ArraySize = 1024;
     ParamStringSeperator = '|'

Var
   TextLine         : string;
   FileNameInput    : string;
   FileNameOutput   : string;
   FileNameBom      : string;
   PartNumber       : string;
   PartDesignator   : string;
   TestText         : string;
   NewString        : string;
   BomString        : AnsiString;     // max. 2GB
   TextSegment      : array [0..11] of string;  // create Array for component definition
   FileInput        : Variant;
   StartConvert     : boolean;
   TextLineLength   : integer;
   SemicolonPosition: integer;
   CommaPosition    : integer;
   SegmentLength    : integer;
   ErrorCode        : integer;
   StartParameters  : Array[0..5] of string;

   BOMFilePath        : string;
   ODBTopCompFilePath : string;
   ODBBotCompFilePath : string;

Procedure ProcessParameters(Paramstring : string);
Var
   Position           : integer;
   TempString         : string;
Begin
     // Target Path
     Position := AnsiPos(ParamStringSeperator, ParamString);
     TempString := AnsiMidStr(ParamString, 14, Position - 21);//14);
     StartParameters[1] := TempString;
     ParamString := AnsiRightStr(ParamString, length(ParamString) - Position);

     // Variant Name
     Position := AnsiPos(ParamStringSeperator, ParamString);
     TempString := AnsiMidStr(ParamString, 16, Position - 16);
     StartParameters[2] := TempString;
     ParamString := AnsiRightStr(ParamString, length(ParamString) - Position);

     Position := AnsiPos(ParamStringSeperator, ParamString);
     TempString := AnsiMidStr(ParamString, 14, Position - 14);
     StartParameters[3] := TempString;
     ParamString := AnsiRightStr(ParamString, length(ParamString) - Position);

     //Open Output Files
     Position := AnsiPos(ParamStringSeperator, ParamString);
     TempString := AnsiMidStr(ParamString, 13, Position - 13);
     StartParameters[4] := TempString;
     ParamString := AnsiRightStr(ParamString, length(ParamString) - Position);

     // Add Output Files to project
     Position := AnsiPos('=', ParamString);
     TempString := AnsiMidStr(ParamString, Position+1, length(ParamString)-Position);
     StartParameters[5] := TempString;
End;

Procedure GetBOMData;
var
   BomFile           : TextFile;
   i                 : integer;
   InputString       : variant;
Begin
     // Open Bom File
     AssignFile(BomFile, BOMFilePath);
     Reset(BomFile);     // open for read

     // clear Variables
     BomString := '';   // clear String

     // Read Bom File
     i := 0;
     while not (Eof(BomFile)) do
     begin
          ReadLN(BomFile, InputString);
          if (InputString <> NULL) then
          begin
               InputString := AnsiMidStr(InputString,2,length(InputString)-2);            // Remove "" 06.02.2020
               InputString := InputString + ',';                                          // Add comma to each line 06.02.2020
               BomString := BomString + InputString;           // Create String with all designators
               i := i + 1;
          end
     end;
     CloseFile(BomFile);
End;

Procedure ProcessODBTop;
Var
    InputFile           : TextFile;
    OutputFile          ; TextFile;
    FileNameInput       : string;
    FileNameOutput      : string;
    InputString         : string;
    i                   : integer;
Begin
     // Duplicate original ODB file
     try  // don't know if file exists
        Begin
             CopyFile(ODBTopCompFilePath,ODBTopCompFilePath+'.org', False);
            // Open Input Text File
            FileNameInput := ODBTopCompFilePath + '.org';
            AssignFile(InputFile, FileNameInput);
            Reset(InputFile);

            // Open Output Text File
            FileNameOutput := ODBTopCompFilePath + '.txt';
            AssignFile(OutputFile, FileNameOutput);
            Rewrite(OutputFile);

            // Start writing new odb file header
            WriteLN(OutputFile, '#');
            WriteLN(OutputFile, '#Component attribute names');
            WriteLN(OutputFile, '#');
            WriteLN(OutputFile, '@0 .no_pop');
            WriteLN(OutputFile, '@1 .comp_mount_type');
            WriteLN(OutputFile, '@2 .comp_height');

            // Read InputFile Content
            //Text := '';
            StartConvert := False;
            // Skip header
            repeat
                  ReadLN(InputFile, InputString);
                  if (InputString = NULL) then   // Search for first empty line
                                                 // Component definitions start here
                  begin
                       WriteLN(OutputFile, '');
                       StartConvert := True;
                  end;
            until (StartConvert = True);
            // Process Components
            while not Eof(InputFile) do
            begin
                    ReadLN(InputFile, FileInput);
                    if (FileInput = NULL) then     // Search for empty lines
                    begin
                       //Text := Text + #13 + #10;
                    end
                    else begin
                         // check if the line is a component definition
                         TextLine := FileInput;
                         if (AnsiStartsStr('CMP', TextLine)) then
                         begin
                              // if yes ...
                              for i:=1 to 8 do
                              begin
                                 TextLineLength := length(TextLine);   // get text length
                                 SegmentLength := AnsiPos(' ', TextLine);  // Search for first space
                                 TextSegment[i] := AnsiLeftStr(TextLine, SegmentLength-1);       // copy Segment here
                                 TextLine := AnsiRightStr(TextLine, TextLineLength - SegmentLength);  // shorten text
                              end;
                              TextLine := AnsiRightStr(TextLine, length(TextLine)-1);
                              CommaPosition := AnsiPos(',', TextLine);
                              TextSegment[10] := AnsiLeftStr(TextLine, CommaPosition-1);
                              TextSegment[10] := AnsiRightStr(TextSegment[10], 1);
                              TextSegment[11] := AnsiRightStr(TextLine, length(TextLine)-CommaPosition);
                              TestText := TextSegment[11];
                              i := length(TestText);
                              TextSegment[11] := AnsiRightStr(TextSegment[11], i-2);
                              PartDesignator := TextSegment[7] + ',';      // Add comma here too 06.02.2020
                              PartNumber := TextSegment[8];
                              if (AnsiContainsText(BomString, PartDesignator)) then
                              begin
                                  TextSegment[9]:='';
                              end
                              else begin
                                 TextSegment[9]:='0,';
                              end;
                              NewString := TextSegment[1] + ' ' + TextSegment[2] + ' ' + TextSegment[3] + ' ' + TextSegment[4] + ' ' + TextSegment[5] + ' ' + TextSegment[6] + ' ' + TextSegment[7] + ' ' + TextSegment[8] + ' ;' + TextSegment[9] + '1=' + TextSegment[10] + ',2=' + TextSegment[11];
                              WriteLN(OutputFile, NewString);
                              //TextLineLength := length(TextLine);
                              //SemicolonPosition := AnsiPos(';', TextLine);
                              //PartDesignator
                              //PartNumber := AnsiMidStr(TextLine, SemicolonPosition-12, 11);
                              //WriteLN(MyOutputFile, AnsiLeftStr(TextLine, SemicolonPosition));
                              //WriteLN(MyOutputFile, 'Do something here' + ' ' + TextLine);
                         end
                         else begin
                              // if not write text without modification
                            WriteLN(OutputFile, TextLine);
                            //Text := Text + TextLine;
                            //Text := Text + #13 + #10;
                         end;
                    end;
            end;


            // Close both files when done
            CloseFile(InputFile);
            CloseFile(OutputFile);
            deletefile(ODBTopCompFilePath);    // Delete original file
            CopyFile(ODBTopCompFilePath+'.txt',ODBTopCompFilePath, False);  //replace it with the modified copy
            deletefile(ODBTopCompFilePath+'.txt');    // Delete temorary copy
            deletefile(ODBTopCompFilePath+'.org');    // Delete temorary copy

        End;
     except
        //Begin
        Showmessage('Datei Top existiert nicht');
        CloseFile(InputFile);
        CloseFile(OutputFile);
     End;
End;

Procedure ProcessODBBottom;
Var
    InputFile           : TextFile;
    OutputFile          ; TextFile;
    FileNameInput       : string;
    FileNameOutput      : string;
    InputString         : string;
    i                   : integer;
Begin
     // Duplicate original ODB file
     try  // don't know if file exists
        Begin
             CopyFile(ODBBotCompFilePath,ODBBotCompFilePath+'.org', False);
            // Open Input Text File
            FileNameInput := ODBBotCompFilePath + '.org';
            AssignFile(InputFile, FileNameInput);
            Reset(InputFile);

            // Open Output Text File
            FileNameOutput := ODBBotCompFilePath + '.txt';
            AssignFile(OutputFile, FileNameOutput);
            Rewrite(OutputFile);

            // Start writing new odb file header
            WriteLN(OutputFile, '#');
            WriteLN(OutputFile, '#Component attribute names');
            WriteLN(OutputFile, '#');
            WriteLN(OutputFile, '@0 .no_pop');
            WriteLN(OutputFile, '@1 .comp_mount_type');
            WriteLN(OutputFile, '@2 .comp_height');

            // Read InputFile Content
            //Text := '';
            StartConvert := False;
            // Skip header
            repeat
                  ReadLN(InputFile, InputString);
                  if (InputString = NULL) then   // Search for first empty line
                                                 // Component definitions start here
                  begin
                       WriteLN(OutputFile, '');
                       StartConvert := True;
                  end;
            until (StartConvert = True);
            // Process Components
            while not Eof(InputFile) do
            begin
                    ReadLN(InputFile, FileInput);
                    if (FileInput = NULL) then     // Search for empty lines
                    begin
                       //Text := Text + #13 + #10;
                    end
                    else begin
                         // check if the line is a component definition
                         TextLine := FileInput;
                         if (AnsiStartsStr('CMP', TextLine)) then
                         begin
                              // if yes ...
                              for i:=1 to 8 do
                              begin
                                 TextLineLength := length(TextLine);   // get text length
                                 SegmentLength := AnsiPos(' ', TextLine);  // Search for first space
                                 TextSegment[i] := AnsiLeftStr(TextLine, SegmentLength-1);       // copy Segment here
                                 TextLine := AnsiRightStr(TextLine, TextLineLength - SegmentLength);  // shorten text
                              end;
                              TextLine := AnsiRightStr(TextLine, length(TextLine)-1);
                              CommaPosition := AnsiPos(',', TextLine);
                              TextSegment[10] := AnsiLeftStr(TextLine, CommaPosition-1);
                              TextSegment[10] := AnsiRightStr(TextSegment[10], 1);
                              TextSegment[11] := AnsiRightStr(TextLine, length(TextLine)-CommaPosition);
                              TestText := TextSegment[11];
                              i := length(TestText);
                              TextSegment[11] := AnsiRightStr(TextSegment[11], i-2);
                              PartDesignator := TextSegment[7] + ','; // Add comma here also 06.02.2020
                              PartNumber := TextSegment[8];
                              if (AnsiContainsText(BomString, PartDesignator)) then
                              begin
                                  TextSegment[9]:='';
                              end
                              else begin
                                 TextSegment[9]:='0,';
                              end;
                              NewString := TextSegment[1] + ' ' + TextSegment[2] + ' ' + TextSegment[3] + ' ' + TextSegment[4] + ' ' + TextSegment[5] + ' ' + TextSegment[6] + ' ' + TextSegment[7] + ' ' + TextSegment[8] + ' ;' + TextSegment[9] + '1=' + TextSegment[10] + ',2=' + TextSegment[11];
                              WriteLN(OutputFile, NewString);
                         end
                         else begin
                              // if not write text without modification
                            WriteLN(OutputFile, TextLine);
                            //Text := Text + TextLine;
                            //Text := Text + #13 + #10;
                         end;
                    end;
            end;


            // Close both files when done
            CloseFile(InputFile);
            CloseFile(OutputFile);
            deletefile(ODBBotCompFilePath);    // Delete original file
            CopyFile(ODBBotCompFilePath+'.txt',ODBBotCompFilePath, False);  //replace it with the modified copy
            deletefile(ODBBotCompFilePath+'.txt');    // Delete temorary copy
            deletefile(ODBBotCompFilePath+'.org');    // Delete temorary copy

        End;
     except
        //Begin
        Showmessage('Datei Bot existiert nicht');
        CloseFile(InputFile);
        CloseFile(OutputFile);
     End;
End;

Procedure AddBomToODB;
Begin
    GetBomData;
    // Check for Top-Layer components
    If (FileExists(ODBTopCompFilePath) = True) Then
       ProcessODBTop;
    // Check for Bottom Layer components
    If (FileExists(ODBBotCompFilePath) = True) Then
       ProcessODBBottom;
End;

Procedure PredictOutputFileNames(Parameters : string);
Begin

End;

Procedure Generate(Parameters : string);
Var
   FileInfo      : string;
Begin

     ProcessParameters(Parameters);

     BOMFilePath := StartParameters[1] + 'BOM\' + StartParameters[2] + '.txt';
     ODBTopCompFilePath := StartParameters[1] + 'ODB\odb\steps\pcb1\layers\comp_+_top\components';
     ODBBotCompFilePath := StartParameters[1] + 'ODB\odb\steps\pcb1\layers\comp_+_bot\components';

     AddBomToODB;

     FileInfo := StartParameters[1] + 'ODB\' + StartParameters[2] + '.tgz';         // Path to .tgz File
     deletefile(FileInfo);                                              // Delete tgz file
     FileInfo := StartParameters[1] + 'ODB\' + StartParameters[2] + '.zip';         // Path to .zip File
     deletefile(FileInfo);                                              // Delete zip file
     FileInfo := StartParameters[1] + '\ODB' + StartParameters[2] + '.rep';         // Path to Report File
     deletefile(FileInfo);                                              // Delete Report file
     FileInfo := '"C:\Program Files\7-Zip\7z.exe" a' + ' "' + StartParameters[1] + 'ODB\' + StartParameters[2] + '.tar"' + ' "' + StartParameters[1] + 'ODB\odb"';
     ErrorCode := RunApplication(FileInfo);
     sleep(3000); // wait some time
     FileInfo := '"C:\Program Files\7-Zip\7z.exe" a' + ' "' + StartParameters[1] + 'ODB\' + StartParameters[2] + '.tgz"' + ' "' + StartParameters[1] + 'ODB\' + StartParameters[2] + '.tar"';
     ErrorCode := RunApplication(FileInfo);
End;

End.
