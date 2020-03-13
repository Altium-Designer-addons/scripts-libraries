unit InvalidObjectFinder;

interface


Type
{..............................................................................}
TFormStatus = class (TForm)
  ProgressBar : TProgressBar;
  statusLabel : TLabel;
End;
{..............................................................................}



Var
  FormStatus: TFormStatus;

{..............................................................................}
Implementation

{$R *.DFM}

{..............................................................................}
Procedure InitStatus (aCaption : TDynamicString; anItemCount : Integer);
Begin
    FormStatus.statusLabel.Caption  := aCaption;
    FormStatus.ProgressBar.Max      := anItemCount;

    FormStatus.Show;
End;
{..............................................................................}

{..............................................................................}
Procedure UpdateStatus(Increment : Integer);
Begin
    If FormStatus.ProgressBar.Position + Increment > FormStatus.ProgressBar.Max Then
       FormStatus.ProgressBar.Position := 1
    Else
       FormStatus.ProgressBar.Position := ProgressBar.Position + Increment;

    FormStatus.Refresh;
End;
{..............................................................................}

{..............................................................................}
Procedure FinishStatus (dummy_not_used : Integer);
Begin
    FormStatus.ProgressBar.Position := ProgressBar.Max;
    FormStatus.statusLabel.Visible  := True;
    FormStatus.statusLabel.Caption  := 'Finished!';

    Sleep (1000);
    FormStatus.Hide;
End;
{..............................................................................}


End.
