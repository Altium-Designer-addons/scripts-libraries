# ImportVariantParametersFromCSV

Imports parameter overrides for a **single project variant** into the focused
PCB project (`.PrjPcb`) from a two-row delimited text file (`.txt` / `.csv`).

Based on `ImportProjectParametersFromCSV`, but variant-aware.

## Features

- Lists all variants defined in the project.
- Supports variant names in the `00`, `01`, `02`, … format.
- **New Variant** button creates a new variant named as the next consecutive
  two-digit number after the current highest (e.g. after `02` it creates `03`),
  inheriting the configuration of the previous highest variant.
- Imports CSV parameters **only into the selected variant** — the rest of the
  project (and other variants) are left untouched.

## CSV file layout

Two rows:

- **Row 1** – parameter names (headers).
- **Row 2** – the corresponding parameter values.

Example (`params.csv`):

```csv
Title,Author,Revision
My Board,"J. Smith",B
```

Fields containing commas can be quoted (`"J. Smith, Jr."`). The delimiter is
auto-detected (`,` `;` or tab).

## How it works

Altium's DelphiScript has no API to create variants or edit variant parameters,
so the script rewrites the `[ProjectVariant…]` section of the `.PrjPcb` file
directly (same approach as `XIA_Release_Manager`). Within a variant block it
replaces `ParameterCount` / `ParamNameN` / `ParamValueN` lines and leaves
component variations untouched.

## Running the script

1. Open `ImportVariantParametersFromCSV.PrjScr` in Altium Designer.
2. Focus the PCB project you want to update.
3. Run the `ImportVariantParametersFromCSV` procedure (File » Run Script).

## Notes / limitations

- Test on a copy of a project first — the script rewrites the project file.
- After importing, Altium auto-detects the file change; do not force a reload
  (that causes duplicated parameters).
