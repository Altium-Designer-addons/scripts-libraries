
{................................................. ............................}
{Summary: Connects two arcs or an arc with a pad or via                        }
{                                                                              }
{    To connect two arcs with a line up where tangent points                   }
{    Or an arc and a via or pad                                                }
{                                                                              }
{    Select a arc and an arc / pad / via and run the script (Arc8.Arc8)        }
{                                                                              }
{    Arcs will be adjusted to fit the new line                                 }
{                                                                              }
{                                                                              }
{         Version 0.3                                                          }
{..............................................................................}



{ calculate the 4 posible connections}
{ ReturnArray is groupes Of  |: x1,y1,x2,y2 :|  }
Procedure FindTangents(
                       Arc1: IPCB_Arc;
                       Arc2: IPCB_Arc;
                       Var ReturnArray: Array// [1..16];// [0..3][1..4] Of TCoord;
                        );
Var
  d_sqr    : TCoord;
  d        : TCoord;
  h        : TCoord;
  vx       : TCoord;
  vy       : TCoord;
  nx       : TCoord;
  ny       : TCoord;
  c        : TCoord;
  i        : integer;
  sign1    : integer;
  sign2    : integer;

Begin

  // clear return array
  For i := 1 To 16 Do
  Begin
    ReturnArray[i] := 0;
  End;

  vx := Arc1.XCenter/10000-Arc2.XCenter/10000;
  vy := Arc1.YCenter/10000-Arc2.YCenter/10000;
  c  := Arc2.Radius/10000 - Arc1.Radius/10000;
  d_sqr := vx*vx + vy*vy;
  If d_sqr > c*c Then
  Begin
    d  := sqrt(d_sqr);
    vx := (Arc2.XCenter/10000 - Arc1.XCenter/10000) / d;
    vy := (Arc2.YCenter/10000 - Arc1.YCenter/10000) / d;

    i := 0;
    sign1 := 1;
    Repeat
      c := (Arc1.Radius/10000 - sign1 * Arc2.Radius/10000) / d;
      If c*c <= 1 Then
      Begin
        h := sqrt(1.0 - c*c);
        sign2 := 1;
        Repeat
          nx := vx * c - sign2 * h * vy;
          ny := vy * c + sign2 * h * vx;

          ReturnArray[i*4+1] := Arc1.XCenter + Arc1.Radius * nx;
          ReturnArray[i*4+2] := Arc1.YCenter + Arc1.Radius * ny;
          ReturnArray[i*4+3] := Arc2.XCenter + sign1 * Arc2.Radius * nx;
          ReturnArray[i*4+4] := Arc2.YCenter + sign1 * Arc2.Radius * ny;
          i := i + 1;

          sign2 := sign2 -2;
        Until Not(sign2 > -2);
      End;

      sign1 := sign1 -2;
    Until Not(sign1 > -2);

  End


End; //FindTangents

{..............................................................................}
Procedure Arc8;
Var
    Board     : IPCB_Board;
    Track     : IPCB_Track;
    ViaPad    : IPCB_Via;  // or pad
    Arc       : IPCB_Arc;
    Arc1      : IPCB_Arc;
    Arc2      : IPCB_Arc;
    Arc3      : IPCB_Arc;
    i,j       : integer;
    tempC     : Coord;
    tempA     : Coord;

    Iterator  : IPCB_BoardIterator;

    ReturnArray:Array[1..16];//[0..3][1..4] Of TCoord; // [vector][x1,y1,x2,y2]
    DiffArray:Array[1..4] Of TCoord;    // [vector]

Begin

    Pcbserver.PreProcess;
      Try
        Board := PCBServer.GetCurrentPCBBoard;
        If Not Assigned(Board) Then
        Begin
          ShowMessage('The Current Document is not a Protel PCB Document.');
          i:=1/0;
          Exit;
        End;

      // Retrieve the current board
      Board := PCBServer.GetCurrentPCBBoard;
      If Board = Nil Then Exit;

      If Board.SelectecObjectCount > 2 Then
      Begin
        ShowMessage(' Only 2 Arcs selected OR one Arc and one Pad/Via, please');
          i:=1/0;
        Exit;
      End;

      If Board.SelectecObjectCount < 2 Then
      Begin
        ShowMessage(' Please, select 2 Arcs OR an Arc and a Pad/Via');
          i:=1/0;
        Exit;
      End;

      Arc1 := Nil;
      Arc2 := Nil;
      ViaPad := Nil;

      // first object
      If Board.SelectecObject[1].ObjectId = eArcObject Then
        Arc1 := Board.SelectecObject[1]
      Else
      If  Board.SelectecObject[1].ObjectId = eViaObject Then
        ViaPad := Board.SelectecObject[1]
      Else
      If  Board.SelectecObject[1].ObjectId = ePadObject Then
        ViaPad := Board.SelectecObject[1];

      // 2:nd object
      If Board.SelectecObject[0].ObjectId = eArcObject Then
        Arc2 := Board.SelectecObject[0]
      Else
      If  Board.SelectecObject[0].ObjectId = eViaObject Then
        ViaPad := Board.SelectecObject[0]
      Else
      If  Board.SelectecObject[0].ObjectId = ePadObject Then
        ViaPad := Board.SelectecObject[0];

      If Arc1 = NIL Then
      Begin
        Arc1 := Arc2;
        Arc2 := NIL;
      End;

      // If only one arc, seartch for pad/via
      If Arc2 = Nil Then
      Begin
        if (ViaPad<>Nil) Then
        Begin
          // make a fake arc from the pad/via center
          Arc2 := PCBServer.PCBObjectFactory(eArcObject,eNoDimension,eCreate_Default);
          Arc2.XCenter := ViaPad.x;
          Arc2.YCenter := ViaPad.y;
          Arc2.Radius := 0;
        End;
      End;

      // check no. Of found objects
      If (Arc1 = Nil) Or (Arc2 = Nil) Then
        ShowMessage(' Please, select 2 Arcs OR an Arc and a Pad/Via')
      Else
      Begin
        // we have 2 arcs

        // find posible tracks
        FindTangents(Arc1,Arc2,ReturnArray);

        // find best fit
        tempC := -1;  // invalidate best fit error
        For i := 0 To 3 Do
        If  (ReturnArray[i*4+1] = 0)
        And (ReturnArray[i*4+3] = 0)
        And (ReturnArray[i*4+2] = 0)
        And (ReturnArray[i*4+4] = 0)
        Then
          DiffArray[i+1] := -99  //  this is not a posible track
        Else
        Begin

        //     Arc1.      Arc2.
        // [0] start_1 -> end_2
        // [1] end_1   -> start_2
        // [2] start_1 -> start_2
        // [3] end_1   -> end_2

          // Arc 1 difference
          If (i = 0) Or (i = 2) Then
            tempA := Arc1.StartAngle * pi /180
          Else
            tempA := Arc1.EndAngle * pi /180;

          DiffArray[i+1]
            := SQR((Arc1.XCenter/10000+Arc1.Radius/10000*cos(tempA))-ReturnArray[i*4+1]/10000)
             + SQR((Arc1.YCenter/10000+Arc1.Radius/10000*sin(tempA))-ReturnArray[i*4+2]/10000);

          // And Arc 2 difference
          If (i = 1) Or (i = 2) Then
            tempA := Arc2.StartAngle * pi /180
          Else
            tempA := Arc2.EndAngle * pi /180;

          DiffArray[i+1] := DiffArray[i+1]
             + SQR((Arc2.XCenter/10000+Arc2.Radius/10000*cos(tempA))-ReturnArray[i*4+3]/10000)
             + SQR((Arc2.YCenter/10000+Arc2.Radius/10000*sin(tempA))-ReturnArray[i*4+4]/10000);

          // is this one better?
          If tempC = -1 Then
            tempC := DiffArray[i+1] // shortest error
          Else
            If tempC > DiffArray[i+1] Then
              tempC := DiffArray[i+1]; // shortest error

        End; // i := 0 To 3 // find shortest error

        // find the one
        For i := 0 To 3 Do
        If tempC = DiffArray[i+1] Then
        Begin

          tempA := ArcTan2( ReturnArray[i*4+2] - Arc1.YCenter,
                            ReturnArray[i*4+1] - Arc1.XCenter) / pi * 180;

          //  notify  the  event  manager  that  the  pcb  objects  is  going  To  be  modified
          PCBServer.SendMessageToRobots(Arc1.I_ObjectAddress  ,c_Broadcast,  PCBM_BeginModify  ,  c_NoEventData);
          PCBServer.SendMessageToRobots(Arc2.I_ObjectAddress  ,c_Broadcast,  PCBM_BeginModify  ,  c_NoEventData);

          // modify the arc:s angle To fit the new track
          Case i Of
          0: Begin
            Arc1.StartAngle := tempA;
            Arc2.EndAngle :=  tempA;
            End;
          1:Begin
            Arc1.EndAngle := tempA ;
            Arc2.StartAngle := tempA;
            End;
          2:Begin
            Arc1.StartAngle := tempA;
            Arc2.StartAngle := 180 + tempA;
            End;
          3:Begin
            Arc1.EndAngle := tempA;
            Arc2.EndAngle := 180 + tempA;
            End;
          End; //  Case i Of

          // deselect the arc:s
          Arc1.Selected := false;
          If ViaPad <> Nil Then ViaPad.Selected := false
          Else Arc2.Selected := false;

          //  notify  the  event  manager  that  the  pcb  objects  has  been  modified
          PCBServer.SendMessageToRobots(Arc1.I_ObjectAddress,  c_Broadcast,  PCBM_EndModify  ,  c_NoEventData);
          PCBServer.SendMessageToRobots(Arc2.I_ObjectAddress,  c_Broadcast,  PCBM_EndModify  ,  c_NoEventData);

          // make a new track, To connect the arc:s
          Track := PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default);
          Track.Layer    := Arc1.Layer;
          Track.Width    := Arc1.LineWidth;
          Track.Net      := Arc1.Net;

          Track.X1       := ReturnArray[i*4+1];
          Track.Y1       := ReturnArray[i*4+2];
          Track.X2       := ReturnArray[i*4+3];
          Track.Y2       := ReturnArray[i*4+4];

          Board.AddPCBObject(Track);

          break; // the For loop - there can be only one
        End; // If tempC = DiffArray[i+1] Then

      End; // found 2 arcs

    Finally
        If ViaPad <> Nil Then PCBServer.DestroyPCBObject(Arc2); // avoid memmory leakage
        // update ....
        PCBServer.PostProcess;

        // Refresh PCB workspace.
        Client.SendMessage('PCB:Zoom', 'Action=Redraw', 255, Client.CurrentView);
    End;
End;
