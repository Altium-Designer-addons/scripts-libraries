#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
SetMouseDelay, 0
Menu, Tray, Icon, %A_AhkPath%, 6

/*
[Altium Designer UI Tweaks]
version = 0.006

Description:
This is an AutoHotkey script, and thus requires that AutoHotkey be installed. Tested with
AutoHotkey_L, which can be found at http://l.autohotkey.net/
This script provides several enhancements to Altium Designer, such as limited autocomplete
for filter expressions, improved mouse wheel support, and various tweaks to save time on
frequently-used tasks.


Definitions:
LMB : Left Mouse Button
MMB : Middle Mouse Button
RMB : Right Mouse Button

Features:
* Autocomplete section for filter expressions or other uses
* Wheel Selector script for PCB/PCBLib selection picker
* MMB click helper (see Petar Perisin section)
* Teardrop speeder and primitive reselector
* Mouse wheel can scroll through Shift+W routing width dialog
* Rubber-stamping with net preservation (see comments in code)
* Every 15 minutes, check for DXP.EXE and close Altium Download Manager if DXP is not found

*/

Loop 
{
	Process, Wait, DXP.EXE
	Process, WaitClose, DXP.EXE
	FoundPID = %ErrorLevel%  ; Save the value immediately since ErrorLevel is often changed.
	if FoundPID = 0
	{
		Process, Exist, AltiumDownloadManager.exe
		FoundPID = %ErrorLevel%  ; Save the value immediately since ErrorLevel is often changed.
		if FoundPID <> 0
		{
			Process, Close, AltiumDownloadManager.exe
		}
	}
}

; Autocomplete section for filter expressions. If you add to this, be mindful of adding enough
; characters to distinguish from common words. For example, "incomp" could start the words
; incomplete, incompetent, or the filter expression InComponent(''), whereas "incompo" is not 
; the start of any regular word in my vocabulary.
; start autocomplete section
:*:InCompo::InComponent(''){left 2}

:*:HasFoot::HasFootprint(''){left 2}

;end autocomplete section

; This section makes it so that the object picker in the PCB editor can be scrolled
; with the mouse wheel, and LMB or MMB will select, and RMB will cancel the picker
#IfWinActive, InsightForm ahk_class TInsightForm
*WheelUp::Send,{Up}
*WheelDown::Send,{Down}
; *LButton::Send,{Space}
*LButton:: 
; Added a differentiation between a LMB click and MMB click: Left click will now make your
; mouse return to where it was when you confirmed the picker (and bringing the picked object
; with it) rather than snapping back to the object you picked. This is mostly useful if you
; regularly drag components quickly, and resent that your drag is mostly wasted if the picker
; pops up. If you prefer to keep the default behavior, comment out this *LButton:: section
; and uncomment the  *LButton::Send,{Space}  line above it
{
	CoordMode, Mouse, Screen
	MouseGetPos, xpos, ypos
	Send, {Space}
	KeyWait, LButton
	Sleep, 30
	MouseMove, xpos, ypos, 0
	CoordMode, Mouse, Relative
}
return
*MButton::Send,{Space}
*RButton::Send,{Esc}
#IfWinActive

; If routing width selector is open, let it be scrolled with mouse wheel
; MMB will confirm selection, RMB will cancel
#IfWinActive, Choose Width ahk_class TChoosePreferredWidth
*WheelUp::Send, {Up}
*WheelDown::Send, {Down}
*MButton::Send, {Enter}
*RButton::Send, {Esc}
#IfWinActive

; Teardrop speeder: when teardrop dialog is open, mouse wheel will toggle between add/remove teardrops
; and middle-clicking anywhere will confirm the dialog (presses enter) while right-clicking cancels
#IfWinActive, Teardrop Options ahk_class TTearDropForm
WheelUp::Send, !a{Down}
WheelDown::Send, !a{Down}
MButton::Send, {Enter}
RButton::Send, {Esc}
#IfWinActive

; Teardrop reselector: after teardrop operation, reselects primitives that were selected before
; This is one of my favorite scripts. Note that this uses selection memory slot 8.
#If GetControlUnderMouse("View_Graphical1")
$^t::
	Send, ^8
	Send, ^t
	WinWaitActive, Teardrop Options ahk_class TTearDropForm, , 2
	WinWaitClose, Teardrop Options ahk_class TTearDropForm
	Sleep, 100
	Loop 
	{
		Sleep, 50
		ControlGet, progbar, Visible, , TProgressBar1, ahk_class TDocumentForm
	} Until, progbar = 0
	Sleep, 100
	Send, !8 ; Sometimes this attempt is missed esp. in the case of long teardrop operations
	Sleep, 400
	Send, !8 ; Send second time after a longer delay in case the first one was missed
#IfWinActive
Return
; End Teardrop dialog mod


; This section was requested by Petar Perisin. It makes the MMB function like the LMB
; in the PCB workspace except in the case of a drag operation
; Begin Petar Perisin section
#If GetControlUnderMouse("View_Graphical1")
~*MButton::
	Shift_pressed := GetKeyState("Shift", "P")
	; SetTimer already started, so just double-Click and get out
	If MButton_presses > 0 
	{
		If Shift_pressed
			Send, +{LButton 2}
		Else Send, {LButton 2}
		SetTimer, ClickMButton, off
		MButton_presses = 0
		Return
	}
	; Otherwise this is the first press of a new series. Set count to 1 and start timer:
	MButton_presses = 1
	SetTimer, ClickMButton, -250 ; time to wait for additional Clicks
Return

ClickMButton:
	; SetTimer, ClickMButton, off
	If !GetKeyState("MButton", "P") 
	{
		If Shift_pressed
			Send, +{LButton}
		Else Send, {LButton}
	}
	MButton_presses = 0
Return
; End Petar Perisin section

; Rubber stamp-like behavior on special paste with nets shortcut
; This is a custom AD shortcut of process "PCB:Paste" with parameters "Mode=Special | KeepNetName=True"
; Assigned to shortcut Ctrl+Alt+V
; This one is a bit rough, but I've found it useful for rubber-stamping Ground vias, for example.
$~^!v::
KeyWait V 
Loop
{
	Tooltip, Rubber-stamping
	Sleep, 100
	If GetKeyState("LButton", "P")
	{
		Loop
		{
			Sleep, 50
			If !GetKeyState("LButton", "P")
			{
				Loop 
				{
					Sleep, 50
					ControlGet, progbar, Visible, , TProgressBar1, ahk_class TDocumentForm
				} Until, progbar = 0
				Sleep, 100
				Send, ^!v
				Break
			}
		}
	}
	Else If GetKeyState("RButton", "P")
		Break
	Else If GetKeyState("Escape", "P")
		Break
}
Tooltip
Return
; End section

;Rolling the mouse wheel while pressing alt will try to bring up the routing width picker
!WheelUp::
IfWinActive, Choose Width ahk_class TChoosePreferredWidth
	Send, {Up}
Else, Send, +w
Return

!WheelDown::
IfWinActive, Choose Width ahk_class TChoosePreferredWidth
	Send, {Down}
Else, Send, +w
Return
; End routing width mod


; Finds the control under mouse cursor (as one might expect) - called by multiple routines
GetControlUnderMouse(target)
	{
		MouseGetPos, xpos, ypos, , found
		If (target = found)
			Return true
		Else
			Return false
	}
#If 
; End of function



