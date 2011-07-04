{******************************************************************************}
{ AdjustDesignators script - written by Mattias Ericson, Omnisys Instruments AB}
{                                                                              }
{ Written for Altium Designer 10                                               }
{                                                                              }
{ This script will center the designator in top and bottom overlay. It will    }
{ adjust the size base on objects (bounding rectagle) in layers Mechanical13   }
{ or Mecanical15.                                                              }
{                                                                              }
{ Note: Only works in millimeters at the moment.                               }
{                                                                              }
{ Change log:                                                                  }
{ Ver 1.0 - Initial release                                                    }
{                                                                              }

{******************************************************************************}

//Enter settings here
const
     MaximumHeight = 5;   // millimeters
     DefaultHeight = 1;  // milimeters
     UnHideDesignators = True; //Unhides all designators
     LockStrings = True; // Lock all strings
     BoundingLayers = True; // Look for bounding rectangle in selected layers
     Layer1 = eTopOverlay;                // Change this to the layer/layers that best represent the component
     Layer2 = eBottomOverlay;             // In many cases eTopOverlay OR eBottomOverLay will be used
     Layer3 = eMechanical13;              // Layers not used must be set to false e.g Layer3=false;
     Layer4 = eMechanical15;

//Calculate the hight of the true type text to best fit for Microsoft Sans Serif
function CalculateSize (Size:Integer,S:String,TextLength:Integer):Integer;
begin
     case TextLength of
          1 : Result := MMsToCoord(1.3013*CoordToMMs(Size)-0.0597);
          2 : Result := MMsToCoord(0.7201*CoordToMMs(Size)+0.0612);
          3 : Result := MMsToCoord(0.4319*CoordToMMs(Size)+0.1116);
          4 : Result := MMsToCoord(0.3265*CoordToMMs(Size)+0.1327);
          5 : Result := MMsToCoord(0.2622*CoordToMMs(Size)+0.1508);
          6 : Result := MMsToCoord(0.2194*CoordToMMs(Size)+0.1519);
          7 : Result := MMsToCoord(0.1957*CoordToMMs(Size)-0.2201);
          else ShowWarning('The length of the designator'+ S +' is '+IntToStr(TextLength) +' characters. Designator must be between 1 and 7 characters to be auto adjusted in size.');
     end;
end;


{..............................................................................}
Procedure AdjustDesignators;
Var
    Track                   : IPCB_Primitive;
    TrackIteratorHandle     : IPCB_GroupIterator;
    Component               : IPCB_Component;
    ComponentIteratorHandle : IPCB_BoardIterator;
    S                       : TPCBString;
    TrackCount              : Integer;
    MaxX                    : Integer;
    MinX                    : Integer;
    MaxY                    : Integer;
    MinY                    : Integer;
    X                       : Integer;
    Y                       : Integer;
    Size                    : Integer;
    TextLength              : Integer;
    Designator              : IPCB_Text;
    PCBSystemOptions        : IPCB_SystemOptions;
    DRCSetting              : boolean;
    R                       : TCoordRect;
begin
     // Verify that the document is a PcbDoc
     if PCBServer.GetCurrentPCBBoard = Nil Then Exit;

     // Disables Online DRC during designator movement to improve speed
     PCBSystemOptions := PCBServer.SystemOptions;

     If PCBSystemOptions = Nil Then Exit;

     DRCSetting := PCBSystemOptions.DoOnlineDRC;
     PCBSystemOptions.DoOnlineDRC := false;

     try

        // Notify the pcbserver that we will make changes
        PCBServer.PreProcess;
        ComponentIteratorHandle := PCBServer.GetCurrentPCBBoard.BoardIterator_Create;
        ComponentIteratorHandle.AddFilter_ObjectSet(MkSet(eComponentObject));
        ComponentIteratorHandle.AddFilter_IPCB_LayerSet(LayerSet.AllLayers);
        ComponentIteratorHandle.AddFilter_Method(eProcessAll);

        S := '';

        Component := ComponentIteratorHandle.FirstPCBObject;
        while (Component <> Nil) Do
        begin

             MaxX:= 0;
             MinX:= 999999999;

             MaxY:= 0;
             MinY:= 999999999;

             TrackCount :=0;

             //Show hidden designators?
             if UnHideDesignators = true then
                Component.NameOn := true;
             //Lock all strings?
             if LockStrings = true then
                Component.LockStrings := true;

             TrackIteratorHandle := Component.GroupIterator_Create;
             TrackIteratorHandle.AddFilter_ObjectSet(MkSet(eTrackObject));

             Track := TrackIteratorHandle.FirstPCBObject;
             while (Track <> Nil) Do
             begin
                  // Look for component's tracks on the layers choosen under settings only when BoundingLayers is true
                  If (((Track.Layer = Layer1) OR
                  (Track.Layer = Layer2) OR
                  (Track.Layer = Layer3) OR
                  (Track.Layer = Layer4)) AND (BoundingLayers = True))Then
                  begin
                       Inc(TrackCount);

                       if Track.X1>= MaxX then MaxX:=Track.X1;
                       if Track.X1<= MinX then MinX:=Track.X1;

                       if Track.X2>= MaxX then MaxX:=Track.X2;
                       if Track.X2<= MinX then MinX:=Track.X2;

                       if Track.Y1>= MaxY then MaxY:=Track.Y1;
                       if Track.Y1<= MinY then MinY:=Track.Y1;

                       if Track.Y2>= MaxY then MaxY:=Track.Y2;
                       if Track.Y2<= MinY then MinY:=Track.Y2;
                  end;

                  Track := TrackIteratorHandle.NextPCBObject;
             end;

            // Get the designator handle
            Designator := Component.Name;

            // Find text length so choose equation for size calcualtion
            S := Designator.Text;
            TextLength := Length(S);

            // notify that the pcb object is going to be modified
            PCBServer.SendMessageToRobots(Designator.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_NoEventData);


           //Calculate the width and hegith of the bounding rectangle
            if TrackCount > 0 then
            begin
                 Y:=MaxY-MinY;
                 X:=MaxX-MinX;
            end
            else
            begin
                 R := Component.BoundingRectangleNoNameComment;
                 if R.left < MinX then MinX := R.left;
                 if R.bottom < MinY then MinY := R.bottom;
                 if R.right > MaxX then MaxX := R.right;
                 if R.top > MaxY then MaxY := R.top;

                 Y:=MaxY-MinY;
                 X:=MaxX-MinX;
            end;



            // Set the size based on the bounding rectangle
            if Y >= X then
            begin
                 Size := CalculateSize(Y,S,TextLength);
                 if Size >= X then
                      Designator.Size := CalculateSize(X,S,TextLength)
                 else
                     Designator.Size := Size;
            end
            else
            begin
                  Size := CalculateSize(X,S,TextLength);
                  if Size >= Y then
                     Designator.Size := CalculateSize(Y,S,TextLength)
                  else
                      Designator.Size := Size;

            end;


            // Setup the text properties
            Designator.UseTTFonts := True;
            Designator.Italic := False;
            Designator.Bold := True;
            Designator.Inverted := False;
            Designator.FontName := 'Microsoft Sans Serif';


            // Rotate the designator to increase the readabillity
            if Y > X then
            begin
                 if Designator.Layer = eTopOverlay then
                    Designator.Rotation := 90
                 else
                     Designator.Rotation := 270;
            end
            else
            begin
                 Designator.Rotation := 0;
            end;


            // Trim down designator if its size is bigger then the MaximumHeight constant
            if Designator.Size > MMsToCoord(MaximumHeight) then
               Designator.Size := MMsToCoord(MaximumHeight);

            // Set the autoposition to the center-center
            Component.ChangeNameAutoposition  :=  eAutoPos_CenterCenter;

           // notify that the pcb object is modified
            PCBServer.SendMessageToRobots(Designator.I_ObjectAddress, c_Broadcast, PCBM_EndModify , c_NoEventData);

           // Destroy the track interator
            Component.GroupIterator_Destroy(TrackIteratorHandle);
           // Get the next component handle
            Component := ComponentIteratorHandle.NextPCBObject;

        end;

        // Notify the pcbserver that all changes have been made
        PCBServer.PostProcess;

        //Refresh the screen
        Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

        // Destroy the component handle
        PCBServer.GetCurrentPCBBoard.BoardIterator_Destroy(ComponentIteratorHandle);

    finally
           // Restore DRC setting
           PCBSystemOptions.DoOnlineDRC :=  DRCSetting;
    end;
end;

{..............................................................................}

