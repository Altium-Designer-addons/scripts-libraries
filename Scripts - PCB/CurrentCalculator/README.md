# CurrentCalculator
Calculates the allowable current in a track according to IPC-2221 (from CircuitCalculator blog).


## Usage
Click the button and select a track or arc on the PCB. A dialog box will appear and tell you what that track's current capacity is, for 1°C rise, 5°C rise, and 10°C rise.


It is best used when assigned to a shortcut key.\
In PCB Editor, create a button/icon:

`Process: ScriptingSystem:RunScriptFile`

`Parameters: Filename = <insert file name/path here> | Procname = CurrentCalc`

then assign it a shortcut key.


## Changelog
v1.0: 21/07/11
v1.2: 22/07/11 Removed 0.5oz plating section
v2.0: 25/05/12 Added 0.5oz Plating Section back in for clarity, made it removable via a switch
v2.1: 25/05/12 Added resistance calculations!


## Credits
Written by John Michael Go-Soco (ETL Systems Ltd)