procedure TTweakDesForm.OKButtonClick(Sender: TObject);
begin
  TweakDesForm.Close;
end;

procedure TTweakDesForm.MMmilButtonClick(Sender: TObject);
begin
  If MMmilButton.Caption = 'MM' then Begin
    MMmilButton.Caption:= 'Mil';
    AdjustAmtEdit.Text:=Trunc(0.5 + 10000 * AdjustAmtEdit.Text / 25.4)/10;
  End else Begin
    MMmilButton.Caption:= 'MM';
    AdjustAmtEdit.Text:= Trunc(0.5 + 100 * AdjustAmtEdit.Text * 0.0254)/100;
  End;
  AdjustAmtEdit.Update;
end;


procedure TTweakDesForm.CancelButtonClick(Sender: TObject);
begin
  TweakDesForm.Close;
  AbortScript:= True;
end;

