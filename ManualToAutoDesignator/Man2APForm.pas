procedure TAPDesForm.OKButtonClick(Sender: TObject);
begin
  APDesForm.Close;
end;


procedure TAPDesForm.CancelButtonClick(Sender: TObject);
begin
  APDesForm.Close;
  AbortScript:= True;
end;

