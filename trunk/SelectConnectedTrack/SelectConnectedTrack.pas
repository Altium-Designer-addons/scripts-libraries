{==============================================================================}
{ ---- New vertices compare function  -----------------------------------------}
{------------------------------------------------------------------------------}
{- Function is comparing given vertices VertX and VertY against Track begin   -}
{- and end points X1, Y1, X2 and Y2                                           -}
{- Return value is 0 for no match and oposite index of for match of one       -}
{- point. Both matched points returns 0 - it is not needed to add any point   -}
{==============================================================================}
Function CompareVertices
            (VertX : Integer; VertY : Integer; Track : IPCB_Track): integer;
Begin
  If ((Track.X1 = VertX) AND (Track.Y1 = VertY)) Then
     Begin
          If ((Track.X2 = VertX) AND (Track.Y2 = VertY)) Then
            Result := 0   // both points equal, don't add any vertex to list
          Else
            Result := 2;  // point 1 equal, add point 2
     End
  Else
     Begin
         If ((Track.X2 = VertX) AND (Track.Y2 = VertY)) Then
            Result := 1   // point 2 equal, add point 1
         Else
            Result := 0   // don't add any vertex to list
     End;
End;

{==============================================================================}
{ ---- Main Select Connected Track procedure --(default hotkey SR) ------------}
{------------------------------------------------------------------------------}
{- After manual selection of a track object on PCB iteration algorithm starts -}
{- searching for connected segments of the track on the same layer - segments -}
{- with at least one equal end point coordinates. Every segment with equal    -}
{- end point coordinates is added to selection. Iterative approach allows to  -}
{- select entire track in few cycles.                                         -}
{- Procedure is not clearing selection at start, new selection is added to    -}
{- previous selection in PCB document.                                        -}
{==============================================================================}
Procedure SelectConnectedTrack;
const
    MAX_CYCLES = 100;     // Limit of iteration cycles
    ARRAY_MAX = 1000;     // Limit of track vertices
var
    Board                          : IPCB_Board; // document board object
    Iterator                       : IPCB_BoardIterator; // object iterator
    Track                          : IPCB_Track; // temp track object
    CurrentLayer                   : TLayer;     // layer of selected track
    VertX, VertY                   : array [1..ARRAY_MAX];
                                     // list of selected track vertices
    VertTop                        : Integer;
                                     // top index of vertices list
    VertBot                        : Integer;
                                     // bottom index of vertices list
    OldVertTop                     : Integer;
                                     // top index of vertices list at the begin
                                     // of current iteration
    NewSegment                     : Boolean;
                                     // new (non selected) and connected
                                     // segment flag
    ConnectedSegment               : Integer;
                                     // command/flag of segment connection test
    Cycles                         : Integer;
                                     // current count of iteration cycles
    i                              : Integer;
                                     // current index of vertices list

Begin
 Try
 Board := PCBServer.GetCurrentPCBBoard;
 If Not Assigned(Board) Then  // check of active document
    Begin
       ShowMessage('The Current Document is not a PCB Document.');
       Exit;
    End;
                        // User is asked to select track segment
 Track := Board.GetObjectAtCursor(MkSet(eTrackObject),AllLayers,
                                                    eEditAction_Select);
 If Not Assigned(Track) Then
    Begin
       ShowMessage('Not a track object was selected.');
       Exit;
    End;

 Track.Selected := True;          // The selected segment is used as a first two
 CurrentLayer := Track.Layer;     // vertices in the list
 VertX[1] := Track.X1;
 VertY[1] := Track.Y1;
 VertX[2] := Track.X2;
 VertY[2] := Track.Y2;
 VertTop  := 2;
 VertBot  := 1;


 Iterator := Board.BoardIterator_Create;
 Cycles   := 0;
   Repeat
    NewSegment := False;
    OldVertTop := VertTop;
    Inc(Cycles);                   // load all relevant objects
    Iterator.AddFilter_ObjectSet(MkSet(eTrackObject));
    Iterator.AddFilter_LayerSet(MkSet(CurrentLayer));
    Iterator.AddFilter_Method(eProcessFree);
    Track := Iterator.FirstPCBObject;       // new iteration
    While (Track <> Nil) Do
    Begin
         If (Track.Selected = False) Then   // check if the track is new
         Begin
          For i := VertBot to VertTop Do
          Begin
                 ConnectedSegment := CompareVertices(VertX[i], VertY[i], Track);
                 If (ConnectedSegment<>0) Then
                 Begin                     // check if the track is connected
                        // record vertices of this track
                        Case ConnectedSegment Of
                          1 : Begin              // add vertex 1 to list
                               Inc(VertTop);
                               If VertTop >= ARRAY_MAX Then Break;
                               VertX[VertTop] := Track.X1;
                               VertY[VertTop] := Track.Y1;
                              End;
                          2 : Begin             // add vertex 2 to list
                               Inc(VertTop);
                               If VertTop >= ARRAY_MAX Then Break;
                               VertX[VertTop] := Track.X2;
                               VertY[VertTop] := Track.Y2;
                              End;
                        Else
                            Break;
                        End;

                        // select current track
                        Track.Selected := True;
                        NewSegment := True;
                        Break;

                End;
          End;
        End;
        Track := Iterator.NextPCBObject;  // next object in the iteration
    End;
    VertBot := OldVertTop;
    If (VertTop = VertBot) Then Break;
  Until ((NewSegment <> True) AND (Cycles <= MAX_CYCLES));
  Board.BoardIterator_Destroy(Iterator);

 Finally
// Pcbserver.PostProcess;     // reflect changes into the current PCB document
 Client.SendMessage('PCB:Zoom', 'Action=Selected', 255, Client.CurrentView);
 End;
End;


