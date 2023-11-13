{ Created by:  Ryan Rutledge                                                }
{ Rebuilds all unshelved polygons on the active PcbDoc and displays results }
{ For documentation see README.md                                           }
var
    Board : IPCB_Board;

const
    ScriptVersion = '0.32';
    ScriptTitle = 'PolygonBenchmark';


{......................................................................................................................}
procedure About; forward;
procedure ClearPolygonRegions(PolyObj : IPCB_Polygon); forward;
procedure RefreshPolygonBenchmarkResults(const ResultString : string); forward;
procedure RepourPolygonsAndBenchmark(const dummy : Integer = 0); forward;
procedure SortPolygonsByPourIndex(PolyList : TInterfaceList); forward;
procedure Start; forward;
{......................................................................................................................}


{......................................................................................................................}
{ About information }
procedure About;
var
    MsgText : string;
begin
    MsgText := '"' + ScriptTitle + '" script version ' + ScriptVersion + sLineBreak + sLineBreak +
        'Updated versions may be found here:' + sLineBreak +
        'https://github.com/Altium-Designer-addons/scripts-libraries';

    ShowInfo(MsgText);
end;
{......................................................................................................................}


{......................................................................................................................}
{ to clear polygon regions before repouring, like if you modified the polygon manually }
procedure ClearPolygonRegions(PolyObj : IPCB_Polygon);
var
    RegionIter  : IPCB_GroupIterator;
    RegionObj   : IPCB_Region;
    RegionList  : TInterfaceList;
    i           : Integer;
begin
    if PolyObj = nil then exit;

    RegionIter := PolyObj.GroupIterator_Create;
    RegionIter.AddFilter_ObjectSet(MkSet(eRegionObject));

    RegionList := CreateObject(TInterfaceList);

    RegionObj := RegionIter.FirstPCBObject;

    while RegionObj <> nil do
    begin
        RegionList.Add(RegionObj);
        RegionObj := RegionIter.NextPCBObject;
    end;

    PolyObj.GroupIterator_Destroy(RegionIter);

    for i := 0 to RegionList.Count - 1 do
    begin
        RegionObj := RegionList.items[i];
        Board.RemovePCBObject(RegionObj);
    end;

end;
{......................................................................................................................}


{......................................................................................................................}
{ Refreshes a .PolygonBenchmarkResults special string if one is found in the PcbDoc }
procedure RefreshPolygonBenchmarkResults(const ResultString : string);
var
    TextIter    : IPCB_BoardIterator;
    TextObj     : IPCB_Text;

begin
    TextIter := Board.BoardIterator_Create;
    TextIter.AddFilter_ObjectSet(MkSet(eTextObject));
    TextIter.AddFilter_LayerSet(AllLayers);
    TextIter.AddFilter_Method(eProcessAll);

    TextObj := TextIter.FirstPCBObject;

    while TextObj <> nil do
    begin
        if Pos('PolygonBenchmark', TextObj.UnderlyingString) > 0 then
        begin
        	TextObj.BeginModify;
            TextObj.Text := ResultString;
            TextObj.EndModify;
            TextObj.GraphicallyInvalidate;
            break;
        end;

        TextObj := TextIter.NextPCBObject;
    end;
    Board.BoardIterator_Destroy(TextIter);

end;
{......................................................................................................................}


{......................................................................................................................}
{ main function to repour polygons and benchmark pour times }
procedure RepourPolygonsAndBenchmark(const dummy : Integer = 0);
var
    StartTime, EndTime, TimeTaken, TotalTimeTaken   : TDateTime;
    MessageText         : TStringList;
    ResultFile          : String;
    PolyIter            : IPCB_BoardIterator;
    PolyObj             : IPCB_Polygon;
    PolyList            : TInterfaceList;
    i, UserChoice       : Integer;
begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then Exit;

    PolyList := CreateObject(TInterfaceList);

    TotalTimeTaken := 0;

    MessageText := CreateObject(TStringList);
    MessageText.Add('Altium Designer ' + Client.GetProductVersion);
    MessageText.Add(ExtractFileName(Board.FileName));
    MessageText.Add('PolygonBenchmark Polygon repour times:');
    MessageText.Add('-------------------------');

    // Create an iterator to find all polygon regions on the board
    PolyIter := Board.BoardIterator_Create;
    PolyIter.AddFilter_ObjectSet(MkSet(ePolyObject));
    PolyIter.AddFilter_IPCB_LayerSet(LayerSet.AllLayers);
    PolyIter.AddFilter_Method(eProcessAll);

    PolyObj := PolyIter.FirstPCBObject;

    // iterate through all polygons to add them to a list and clear unshelved polygons' regions
    while PolyObj <> nil do
    begin
        PolyList.Add(PolyObj);
        PolyObj := PolyIter.NextPCBObject;
    end;

    Board.BoardIterator_Destroy(PolyIter);

    if PolyList.Count = 0 then
    begin
        ShowInfo('No polygons found. Exiting.');
        exit;
    end;

    if not ConfirmNoYesWithCaption('Warning', 'This will rebuild ALL ' + IntToStr(PolyList.Count) + ' polygons on the board. This operation cannot be undone!' + sLineBreak + sLineBreak + 'Do you want to continue?') then exit;

    // Start undo (might not be fully undoable anyway though) - move after prompt else leaves undo hanging
    PCBServer.PreProcess;
    try
        BeginHourGlass;
        // clear existing poured regions in list
        for i := 0 to PolyList.Count - 1 do
        begin
            PolyObj := PolyList.items[i];
            if PolyObj.Poured then
            begin
                PolyObj.BeginModify;
                ClearPolygonRegions(PolyObj);
                PolyObj.EndModify;
            end;
        end;

        Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);  // refresh the view so you see the polygons cleared for repouring

        SortPolygonsByPourIndex(PolyList);

        for i := 0 to PolyList.Count - 1 do
        begin
            PolyObj := PolyList.items[i];
            if PolyObj.Poured then
            begin
                // Record the start time
                StartTime := Now;

                // Repour the current polygon
                PolyObj.BeginModify;
                PolyObj.Rebuild;
                PolyObj.EndModify;

                // Record the end time
                EndTime := Now;

                PolyObj.GraphicallyInvalidate;

                // Calculate the time taken and convert it to seconds
                TimeTaken := (EndTime - StartTime) * 24 * 60 * 60;
                TotalTimeTaken := TotalTimeTaken + TimeTaken;   // accumulate time before rounding

                // Add the time taken to the message
                MessageText.Add(Format('%s (idx%d) : %.2f s', [PolyObj.Name, PolyObj.PourIndex, TimeTaken]));

            end
            else
            begin
                MessageText.Add(Format('%s (idx%d) is shelved', [PolyObj.Name, PolyObj.PourIndex]));
            end;

        end;

        Board.ViewManager_FullUpdate;   // doesn't seem to be all it's cracked up to be, or at least it doesn't seem to apply instantaneously

        MessageText.Add('-------------------------');
        MessageText.Add(Format('Total repour time: %.2f s', [TotalTimeTaken]));

        EndHourGlass;

        // Show the message
        ShowInfo(MessageText.Text, 'PolygonBenchmark Results');

        // save the results
        if ConfirmNoYesWithCaption('Update Results String', 'Do you want to update any placed text containing "PolygonBenchmark" and save results to a file named "<PcbDoc>_<ADversion>_repourtimes.txt"?') then
        begin
            RefreshPolygonBenchmarkResults(MessageText.Text);
            ResultFile := ChangeFileExt(Board.FileName, '_v' + Client.GetProductVersion + '_repourtimes.txt');
            MessageText.SaveToFile(ResultFile);
        end;

    finally
        // End undo
        PCBServer.PostProcess;
    end;

end;
{......................................................................................................................}


{......................................................................................................................}
{ to sort the polygon list by PourIndex else the pour order will be whatever the polygon iterator returned }
procedure SortPolygonsByPourIndex(PolyList : TInterfaceList);
var
    i, j : Integer;
    Temp : IPCB_Polygon;
begin
    for i := 0 to PolyList.Count - 1 do
    begin
        for j := 0 to PolyList.Count - 2 - i do
        begin
            if PolyList[j].PourIndex > PolyList[j + 1].PourIndex then
            begin
                Temp := PolyList[j];
                PolyList[j] := PolyList[j + 1];
                PolyList[j + 1] := Temp;
            end;
        end;
    end;
end;
{......................................................................................................................}


{......................................................................................................................}
procedure Start;
begin
    RepourPolygonsAndBenchmark;
end;
{......................................................................................................................}
