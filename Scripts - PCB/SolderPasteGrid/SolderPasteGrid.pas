{..............................................................................}
{ Summary Solder Paste Mask Grid script.                                       }
{                 aka: Paste Mask Lattice, Paste Mask Hatch                    }
{         Creates a Footprint's Paste Mask as a Grid instead of                }
{         just matching the pad size w/ expansion setting                      }
{                                                                              }
{                                                                              }
{ To use the script:                                                           }
{  1/ Select the footprint's center pad.                                       }
{  2/ Execute the script                                                       }
{..............................................................................}
Interface

Type
  TTextForm = class(TForm)
    bRun          : TButton;
    txtMinGridSize  : TEdit;
    txtMinGap : TEdit;
    txtMinCover : TEdit;
    lblMinGridSize: TLabel;
    lblMinGap: TLabel;
    lblMinCover: TLabel;
  End;

Var
  TextForm: TTextForm;

Implementation

{***********************************************************************************
 * Function: RemoveExistingPaste
 *
 * Description: Using the given pad object, set the paste mask expansion negative
 *              enough that there is no paste mask associated with the pad except
 *              for what may be added later as Fills on the Top Paste layer.
 *
 * Input Parameters:    Pad - Center Pad object of Footprint
 *
 * Returns: None
 *
 ************************************************************************************}
function RemoveExistingPaste(Pad: IPCB_Primitive);
var
    Padcache      : TPadCache;
    Pad_h, Pad_w  : Double;
    Paste_Exp     : Double;
begin
    Pad_h := Pad.TopXSize;
    Pad_w := Pad.TopYSize;
    Paste_Exp := Pad.PasteMaskExpansion;

    Padcache := Pad.GetState_Cache;
    Padcache.PasteMaskExpansionValid := eCacheManual;

    If (Paste_Exp > -Pad_w) or (Paste_Exp > -Pad_h) Then
    Begin
         If (Pad_w > Pad_h) Then
         Begin
              Padcache.PasteMaskExpansion := -Pad_w;
         End
         Else
         Begin
              Padcache.PasteMaskExpansion := -Pad_h;
         End;
    End;

    Pad.SetState_Cache := Padcache;
end;

{***********************************************************************************
 * Function: CreatePasteGrid
 *
 * Description: Calculates the best size paste mask block to cover the minimum
 *              area using the minimum gap size with n blocks in the x direction
 *              and n blocks in the y direction. After calculating this, it
 *              adds the fills to the Top Paste layer.
 *
 * Input Parameters:    Board - Current Board object
 *                      Pad - Center Pad object of Footprint
 *                      Min_Grid_Size - Minimum paste mask grid size in mils, where
 *                                      size = width = height.
 *                      Min_Gap - Minimum distance between paste mask grid blocks
 *                                in mils.
 *                      Min_Cover - Minimum percentage of paste mask coverage w/
 *                                  respect to the pad area.
 *
 * Returns: None
 *
 ************************************************************************************}
function CreatePasteGrid(Board: IPCB_Board, Pad: IPCB_Primitive, Min_Grid_Size: Integer, Min_Gap: Integer, Min_Cover: Float);
const
    INTERN2MIL = 10000;  // Internal Unit Conversion to mils
    GRID_SIZE_INC = 5;   // Grid Size Increments in mils
var
    Grid_x_cnt, Grid_y_cnt                : Integer;
    Grid_x_pad, Grid_y_pad                : Integer;
    Pad_h, Pad_w                          : Double;
    Pad_Area, Paste_Area                  : Int64;
    Grid_Size                             : Integer;
    i, j                                  : Integer;
    Fill                                  : IPCB_Fill;
    fill_x1, fill_x2, fill_y1, fill_y2    : Double;
    xorigin, yorigin                      : Double;
    pct_cover                             : Float;
begin
    xorigin := Board.XOrigin;
    yorigin := Board.YOrigin;

    If (Pad.Rotation = 90) or (Pad.Rotation = 270) Then
    Begin
        Pad_h := Pad.TopXSize;
        Pad_w := Pad.TopYSize;
    End
    Else
    Begin
        Pad_h := Pad.TopYSize;
        Pad_w := Pad.TopXSize;
    End;

    Pad_Area := (Pad_w/INTERN2MIL)*(Pad_h/INTERN2MIL);
    Grid_Size := Min_Grid_Size;

    // Get grid counts
    Grid_x_cnt := Floor((Pad_w/INTERN2MIL)/Grid_Size);
    Grid_y_cnt := Floor((Pad_h/INTERN2MIL)/Grid_Size);

    Paste_Area := 0;
    While (pct_cover < Min_Cover) Do
    Begin

         Grid_x_pad := 0; Grid_y_pad := 0;
         While (Grid_x_pad < Min_Gap*INTERN2MIL) or (Grid_y_pad < Min_Gap*INTERN2MIL) Do
         Begin
              // Get Grid padding
              Grid_x_pad := (Pad_w - (Grid_x_cnt*Grid_Size*INTERN2MIL))/(Grid_x_cnt+1);
              Grid_y_pad := (Pad_h - (Grid_y_cnt*Grid_Size*INTERN2MIL))/(Grid_y_cnt+1);

              If (Grid_x_pad < Min_Gap*INTERN2MIL) Then
              Begin
                   Grid_x_cnt := Grid_x_cnt - 1;
              End;
              If (Grid_y_pad < Min_Gap*INTERN2MIL) Then
              Begin
                   Grid_y_cnt := Grid_y_cnt - 1;
              End;

              // Error Exit
              If Grid_x_cnt <= 0 Then Exit;
              If Grid_y_cnt <= 0 Then Exit;
         End;
         Paste_Area := Grid_x_cnt*Grid_y_cnt*Grid_Size*Grid_Size;
         pct_cover := (Paste_Area/Pad_Area)*100;

         If (pct_cover < Min_Cover) Then
         Begin
              Grid_Size := Grid_Size + GRID_SIZE_INC;
         End;
    End;



    For i := 0 To Grid_x_cnt - 1 Do
    Begin
         For j := 0 To Grid_y_cnt - 1 Do
         Begin
              // Create Fill
              fill_x1 := Grid_x_pad*(i+1) + i*Grid_Size*INTERN2MIL - (Pad_w/2);
              fill_x2 := Grid_x_pad*(i+1) + (i+1)*Grid_Size*INTERN2MIL - (Pad_w/2);
              fill_y1 := Grid_y_pad*(j+1) + j*Grid_Size*INTERN2MIL - (Pad_h/2);
              fill_y2 := Grid_y_pad*(j+1) + (j+1)*Grid_Size*INTERN2MIL - (Pad_h/2);

              Fill := PCBServer.PCBObjectFactory(eFillObject, eNoDimension, eCreate_Default);
              Fill.X1Location := fill_x1 + xorigin;
              Fill.Y1Location := fill_y1 + yorigin;
              Fill.X2Location := fill_x2 + xorigin;
              Fill.Y2Location := fill_y2 + yorigin;
              Fill.Layer := eTopPaste;
              Fill.Rotation := 0;

              // Add a new Fill into the PCB design database.
              Board.AddPCBObject(Fill);
         End;
    End;

    // Refresh the PCB document
    ResetParameters;
    AddStringParameter('Action', 'All');
    RunProcess('PCB:Zoom');
End;

{***********************************************************************************
 * Function: SolderPasteGrid
 *
 * Description: Gets values passed from GUI text boxes. It calls
 *              the appropriate functions when it finds the selected center pad
 *              object for the Footprint that is on the Top Layer.
 *
 * Input Parameters:    Min_Grid_Size - Minimum paste mask grid size in mils, where
 *                                      size = width = height.
 *                      Min_Gap - Minimum distance between paste mask grid blocks
 *                                in mils.
 *                      Min_Cover - Minimum percentage of paste mask coverage w/
 *                                  respect to the pad area.
 *
 * Returns: None
 *
 ************************************************************************************}
function SolderPasteGrid(Min_Grid_Size: Integer, Min_Gap: Integer, Min_Cover: Integer);
var
    Board         : IPCB_Board;
    Iterator      : IPCB_SpatialIterator;
    Pad           : IPCB_Primitive;
    Pad_Layer     : TPCBString;
    Pad_x, Pad_y  : Double;
    xorigin, yorigin : Double;

begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // Create the iterator that will look for Component Body objects only
    Iterator := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(ePadObject));
    Iterator.AddFilter_LayerSet(MkSet(eTopLayer));
    Iterator.AddFilter_Method(eProcessAll);

    xorigin := Board.XOrigin;
    yorigin := Board.YOrigin;

    Pad := Iterator.FirstPCBObject;
    While (Pad <> Nil) Do
    Begin

        If (Pad.Selected) Then
        Begin
            Pad_x := Pad.x - xorigin;
            Pad_y := Pad.y - yorigin;
            Pad_Layer := Layer2String(Pad.Layer);

            If (Pad_Layer = 'Top Layer') and (Pad_x = 0) and (Pad_y = 0) Then
            Begin
                 // Set Paste Mask Expansion To Remove Current Paste Mask
                 RemoveExistingPaste(Pad);

                 CreatePasteGrid(Board, Pad, Min_Grid_Size, Min_Gap, Min_Cover);
            End;
        End;

        Pad := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);
end;



{***********************************************************************************
 * Function: bRunClick
 *
 * Description: Button Run Handler. Calls the function that runs the script.
 *
 * Input Parameters:    Sender object.
 *
 * Returns: None
 *
 ************************************************************************************}
procedure TTextForm.bRunClick(Sender: TObject);
begin
     Close;
     SolderPasteGrid(txtMinGridSize.Text, txtMinGap.Text, txtMinCover.Text);
end;

{***********************************************************************************
 * Function: RunSolderPasteGrid
 *
 * Description: Main. Calls GUI.
 *
 * Input Parameters:    None.
 *
 * Returns: None
 *
 ************************************************************************************}
Procedure RunSolderPasteGrid;
Begin
    TextForm.ShowModal;
End;

