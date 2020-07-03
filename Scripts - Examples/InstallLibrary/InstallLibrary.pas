{
  Original code from Jelle LAARMAN
  https://forum.live.altium.com/#posts/241605/746573
}

procedure uninstallAndInstallLib();
var
  CurrLibPath : String;
  ErrorCode : Integer;
  NumberOfLibraries : Integer;
  Done      : string;
  IsPCBLibraryAvailable : Boolean;
begin
  IsPCBLibraryAvailable := False;
  NumberOfLibraries := 0;
  while (NumberOfLibraries < IntegratedLibraryManager.AvailableLibraryCount) do
  Begin
    if(AnsiContainsText(IntegratedLibraryManager.InstalledLibraryPath(NumberOfLibraries),'*Libraryname!*')) then
    Begin
      IsPCBLibraryAvailable := True;
      CurrLibPath := IntegratedLibraryManager.InstalledLibraryPath(NumberOfLibraries);
       
      Break;
    End;
    inc(NumberOfLibraries);
  End;
  if(IsPCBLibraryAvailable) then
  Begin
    RunProcess('IntegratedLibrary:RefreshInstalledLibraries');
    IntegratedLibraryManager.UninstallLibrary(CurrLibPath);

    //do something....    

    IntegratedLibraryManager.InstallLibrary(CurrLibPath);
  End;
End;