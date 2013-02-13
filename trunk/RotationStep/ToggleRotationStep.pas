
//Toggle Rotation Step script
// - John Michael Go-Soco [24/08/2012]
// - Procedure to Set step and code to save step
//      in PCBServer.SystemOptions.DraftTrackThreshold
//      added by Tony Chilco [14/12/2012]
// - Two procedure are exported;  ToggleRotationStep switches between the stored
//    angle and 90 degrees, SetRotationStep prompts for a value to to set the
//    set the rotation angle to and stores that value.

  Procedure ToggleRotationStep;
            var a,b : TAngle;
  Begin
         a := PCBServer.SystemOptions.RotationStep;
            if a = 90 then Begin
               b := PCBServer.SystemOptions.DraftTrackThreshold / 100;
               // Comment out the next line for silent operation
               ShowMessage ('Rotation Angle set to: ' + FloatToStr(b) + '°');
            End Else Begin
              b := 90;
              // The system variable takes an integer value. *100 allows
              //    an angle with two decimal places.
              PCBServer.SystemOptions.DraftTrackThreshold:= Trunc(a*100);
            End;
         PCBServer.SystemOptions.RotationStep := b;
          // ShowMessage ('Rotation Angle set to: ' + FloatToStr(b) + '°');
  End;
                //DraftTrackThreshold

  Procedure SetRotationStep;
            var a,b : TAngle;
  Begin
         a := PCBServer.SystemOptions.RotationStep;
         RotStepForm.RotStepInput.Text:= a;
         RotStepForm.Show;
          // ShowMessage ('Rotation Angle set to: ' + FloatToStr(b) + '°');
  End;

Procedure UpdateRotStep(Dummy : Integer);
Begin
  PCBServer.SystemOptions.RotationStep := RotStepForm.RotStepInput.Text;
  RotStepForm.Close;
End;

procedure TRotStepForm.OKButtonClick(Sender: TObject);
begin
  UpdateRotStep(5);
end;

procedure TRotStepForm.CancelButtonClick(Sender: TObject);
begin
  RotStepForm.Close;
end;


procedure TRotStepForm.RotStepInputKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If Key = 13 then   UpdateRotStep(5);
end;

