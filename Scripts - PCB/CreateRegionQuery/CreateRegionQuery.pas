{..............................................................................}
{ Summary                                                                      }
{ Based on Altium example script SpatialIterator.pas with modifications.       }
{ Prompts the user to select a rectangle on screen and builds InRegionAbsolute }
{ query for use in expressions.                                                }
{                                                                              }
{ v1.0 - 2022-03-21                                                            }
{ Created by: Ryan Rutledge                                                    }
{ Licensed under GNU GPL - if you like it, you're welcome to it!               }

{..............................................................................}
procedure Start;
var
    Board            : IPCB_Board;
begin
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;
    RegionQueryForm.ShowModal;
    btnSelectArea.Click;
end;

procedure TRegionQueryForm.btnSelectAreaClick(Sender: TObject);
var
    Board            : IPCB_Board;
    X1, X2, Y1, Y2   : TCoord;
    myImperialQuery;
    myMetricQuery;
begin
    Board := PCBServer.GetCurrentPCBBoard;
    if Board = nil then exit;
    RegionQueryForm.Hide;
    (* The ChooseRectangleByCorners fn is an interactive function where you are prompted to choose two points on a PCB document.*)
    if not (Board.ChooseRectangleByCorners( 'Choose first corner', 'Choose second corner', x1,y1,x2,y2)) then
    begin
         RegionQueryForm.Show;
         exit;
    end;

    try
        myImperialQuery := format('InRegionAbsolute(%g,%g,%g,%g)',[CoordToMils(X1),CoordToMils(Y1),CoordToMils(X2),CoordToMils(Y2));
        txtImperial.Text := myImperialQuery;
        myMetricQuery := format('InRegionAbsolute(%g,%g,%g,%g)',[CoordToMMs(X1),CoordToMMs(Y1),CoordToMMs(X2),CoordToMMs(Y2));
        txtMetric.Text := myMetricQuery;
        RegionQueryForm.Show;
        (* Showmessage(myImperialQuery); *)
    except
        Showmessage('Something bad happened');
        Region.QueryForm.Close;
    end;
end;


procedure TRegionQueryForm.btnCancelClick(Sender: TObject);
begin
     Close;
end;

procedure TRegionQueryForm.btnImperialCopyClick(Sender: TObject);
begin
    Clipboard.AsText := txtImperial.Text;
end;

procedure TRegionQueryForm.btnImperialCopyCloseClick(Sender: TObject);
begin
    Clipboard.AsText := txtImperial.Text;
    Close;
end;

procedure TRegionQueryForm.btnMetricCopyClick(Sender: TObject);
begin
    Clipboard.AsText := txtMetric.Text;
end;

procedure TRegionQueryForm.btnMetricCopyCloseClick(Sender: TObject);
begin
    Clipboard.AsText := txtMetric.Text;
    Close;
end;
