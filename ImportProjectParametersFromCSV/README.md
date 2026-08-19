# ImportProjectParametersFromCSV

Imports project parameters into the currently focused PCB project (`.PrjPcb`)
from a simple two-row, comma-delimited text file (`.txt` / `.csv`).

> **Why not read Excel directly?**
> Altium's DelphiScript engine does **not** support COM/OLE automation
> (`CreateOleObject` is an "Undeclared identifier"). So Excel/Access files must
> be converted to the CSV format first

## CSV file layout

Two rows:

- **Row 1** – parameter names (headers). Must match the parameter names used in the project.
- **Row 2** – the corresponding parameter values.

Example (`params.csv`):

```csv
Title,Author,Revision,ApprovedBy
My Board,"J. Smith",B,"A. Jones"
```

Fields containing commas can be quoted with double quotes (`"J. Smith, Jr."`).

## What the Altium script does

1. Prompts for the path and file name of the CSV file (Browse button or type it in).
2. Reads row 1 (names) and row 2 (values).
3. Reads the current project parameters from the focused `.PrjPcb` project file.
4. For each CSV header:
   - if the parameter already exists, its value is **updated**;
   - if it does not exist, the parameter is **added**.
5. Writes the merged parameters back and reloads the project file.

## Requirements

- Altium Designer (DelphiScript).
- A PCB project (`.PrjPcb`) must be focused when the script is run.

## Running the Altium script

1. Open `ImportProjectParametersFromCSV.PrjScr` in Altium Designer.
2. Focus the PCB project you want to update.
3. Run the `ImportProjectParametersFromCSV` procedure (File » Run Script).

## Notes / limitations

- Project-level parameters cannot be added/updated through the standard
  DelphiScript API, so the script edits the `.PrjPcb` file directly (the same
  approach used by the `XIA_Release_Manager` script) and reloads it.
- The `[Parameter…]` section of the project file is rewritten; the rest of the
  file is preserved unchanged.
- Test on a copy of a project first.
