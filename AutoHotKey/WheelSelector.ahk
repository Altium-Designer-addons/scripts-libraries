#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
SetMouseDelay, 0
Menu, Tray, Icon, %A_AhkPath%, 6

/*
[Altium Designer UI Tweaks]
version = 0.001

Definitions:
LMB : Left Mouse Button
MMB : Middle Mouse Button
RMB : Right Mouse Button
*/

; This section makes it so that the object picker in the PCB editor can be scrolled
; with the mouse wheel, and LMB or MMB will select, and RMB will cancel the picker
#IfWinActive, InsightForm ahk_class TInsightForm
*WheelUp::Send,{Up}
*WheelDown::Send,{Down}
*LButton::Send,{Space}
*MButton::Send,{Space}
*RButton::Send,{Esc}
#IfWinActive

; This section was requested by Robert Brekalo. It makes the MMB function like the LMB
; in the PCB workspace except in the case of a drag operation
; Begin Robert Brekalo section
#If GetControlUnderMouse("View_Graphical1")
~*MButton::
	GetKeyState, Shift_state, Shift, P
	If MButton_presses > 0 ; SetTimer already started, so just double-Click and get out
	{
		IfEqual, Shift_state, D, Send, +{LButton 2}
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
	GetKeyState, MButton_state, MButton, P
	IfEqual, MButton_state, U
	{
		IfEqual, Shift_state, D, Send, +{LButton}
		Else Send, {LButton}
	}
	MButton_presses = 0
Return

GetControlUnderMouse(target)
	{
		MouseGetPos, , , , found
		If (target = found)
			Return true
		Else
			Return false
	}
#If 
; End Robert Brekalo section

