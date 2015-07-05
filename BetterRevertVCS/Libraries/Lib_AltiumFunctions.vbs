
' Copyright (C) 2015 Justin MASSIOT
' Author: Justin MASSIOT ( m [dot] just1 !at! free [dot] fr )

' ============================================================================ '
'                                                                              '
'    This program is free software: you can redistribute it and/or modify      '
'    it under the terms of the GNU General Public License as published by      '
'    the Free Software Foundation, either version 3 of the License, or         '
'    (at your option) any later version.                                       '
'                                                                              '
'    This program is distributed in the hope that it will be useful,           '
'    but WITHOUT ANY WARRANTY; without even the implied warranty of            '
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the              '
'    GNU General Public License for more details.                              '
'                                                                              '
'    You should have received a copy of the GNU General Public License         '
'    along with this program. If not, see <http://www.gnu.org/licenses/>.      '
'                                                                              '
' ============================================================================ '

' Altium®, Altium Designer®, Altium Vault®, Autotrax®, Camtastic®, CircuitMaker®, CircuitStudio®, Codemaker™, DXP™, Easytrax®, LiveDesign®, NanoBoard®, PCBWORKS™, P-CAD®, Protel®, TASKING® and their respective logos are trademarks or registered trademarks of Altium Limited or its subsidiaries. See the full Copyright at http://www.altium.com/copyrights-and-trademarks and the EULA at http://www.altium.com/eula .
' All other registered or unregistered trademarks referenced herein are the property of their respective owners.

' ------------------------------------------------------------------------------



' Altium Designer API related functions
' To use them, you need to include this file in your script, or simply copy/paste the functions in the content of your file.



' Run a Server Process, with "PostMessage" (non-blocking version)
' Use it carefully because the Server does not allow the execution of multiple scripts at the same time.
' Documentation on Server Processes are available at http://wiki.altium.com/display/ADOH/Server+Process+Routines
' Client.PostMessage() example: http://wiki.altium.com/display/ADOH/Using+the+Altium+Designer+RTL#UsingtheAltiumDesignerRTL-RefreshaPCBDocumentinaScript
Sub ServerRunProcessPost (Process, Parameters)
  If TypeName(Process) <> "String" Then Exit Sub
  If TypeName(Parameters) <> "String" Then Exit Sub
  If Client Is Nothing Then Exit Sub
  Call Client.PostMessage(Process, Parameters, Len(Parameters), Client.CurrentView)
  ' PostMessage returns even before the end of the execution (new thread)
End Sub



' Run a Server Process, with "SendMessage" (blocking version)
' Documentation on Server Processes are available at http://wiki.altium.com/display/ADOH/Server+Process+Routines
' Client.SendMessage() example: http://wiki.altium.com/display/ADOH/Using+the+Altium+Designer+RTL#UsingtheAltiumDesignerRTL-RefreshaPCBDocumentinaScript
Sub ServerRunProcessSend (Process, Parameters)
  If TypeName(Process) <> "String" Then Exit Sub
  If TypeName(Parameters) <> "String" Then Exit Sub
  If Client Is Nothing Then Exit Sub
  Call Client.SendMessage(Process, Parameters, Len(Parameters), Client.CurrentView)
  ' SendMessage returns when the message has been processed (same thread)
End Sub



' This function tells if a document is modified (respectively visible) without having to open the document if it is not open.
' The "mode" parameter must have the value "modified" or "visible". All other values are invalid.
' Example call: Call DocObj_IsModifiedVisible ("modified", Client, GetWorkspace.DM_FocusedDocument)
Function DocObj_IsModifiedVisible (mode, Client, DM_Object)
  If TypeName(mode) <> "String" Then Exit Function
  If mode <> "modified" and mode <> "visible" Then Exit Function
  If Client Is Nothing Then Exit Function
  If TypeName(Client) <> "Object" Then Exit Function
  If DM_Object Is Nothing Then Exit Function
  If TypeName(DM_Object) <> "Object" Then Exit Function
  
  Dim IsValidDocument
  Dim IsValidProject
  Dim CurDoc
  
  ' determine if we are working on a project or on a document object
  IsValidDocument = False
  IsValidProject = False
  On Error Resume Next ' if an error occurs, discard it and go to the next instruction (= enable errors handling)
  Err.Clear ' clear previous errors
  DM_Object.DM_FullPath ' check if the property DM_FullPath exists
  If Err.Number = 0 Then ' no error
    IsValidDocument = True ' if it exists, it is a document object
    'ShowMessage "Valid: " & DM_Object.DM_FullPath
  End If
  Err.Clear ' clear previous errors
  DM_Object.DM_ProjectFullPath ' check if the property DM_ProjectFullPath exists
  If Err.Number = 0 Then ' no error
    IsValidProject = True ' if it exists, it is a project object
    'ShowMessage "Valid: " & DM_Object.DM_ProjectFullPath
  End If
  On Error Goto 0 ' disables the 'On Error Resume Next' statement (= disable errors handling)
  
  ' Whatever the mode, return false if the file has never been saved (the file does not exist).
  ' This is because all the below functions need a valid path to the file to work correctly.
  If IsValidDocument Then
    If Not FileExists(DM_Object.DM_FullPath) Then
      DocObj_IsModifiedVisible = False
      Exit Function
    End If
  ElseIf IsValidProject Then
    If Not IsProjectPathValid(DM_Object, "both") Then
      DocObj_IsModifiedVisible = False
      Exit Function
    End If
  End If
  
  ' From the Altium Designer API documentation, we can use Client.GetDocumentByPath().Modified to check if
  ' the document contains unsaved modifications but this is not truly reliable, so we prefer
  ' to use Client.OpenDocument().Modified .
  ' In order to keep the performances at a high level, we must not open all documents!
  ' That is why we check with Client.IsDocumentOpen() if the document is already open, and considered
  ' the document as not modified if it is closed.
  
  If IsValidProject Then ' project mode
    If mode="visible" Then
      DocObj_IsModifiedVisible = False ' a project file cannot be visible!
    ElseIf Not Client.IsDocumentOpen(DM_Object.DM_ProjectFullPath) Then ' the document is not open, and consequently there is no unsaved modification on it
      DocObj_IsModifiedVisible = False ' modified = false
    Else ' the document is open
      DocObj_IsModifiedVisible = Client.OpenDocument("", DM_Object.DM_ProjectFullPath).Modified ' the object kind is not filled in ("") but it works!
      ' the document was already open so we left it open
    End If
    'ShowMessage DocObj_IsModifiedVisible & " -> " & DM_Object.DM_ProjectFullPath
  ElseIf IsValidDocument Then ' document mode
      If Not Client.IsDocumentOpen(DM_Object.DM_FullPath) Then ' the document is not open, and consequently it cannot be visible and there is no unsaved modification on it
        DocObj_IsModifiedVisible = False ' modified/visible = false
      Else ' the document is open
        CurDoc = Client.OpenDocument("", DM_Object.DM_FullPath) ' the object kind is not filled in ("") but it works!
        DocObj_IsModifiedVisible = iIf(mode="modified", CurDoc.Modified, CurDoc.IsShown) ' Doc.Modified when mode="modified", Doc.IsShown when mode="visible"
        ' the document was already open so we left it open
      End If
      'ShowMessage DocObj_IsModifiedVisible & " -> " & DM_Object.DM_FullPath
  Else ' no valid project, no valid document
    DocObj_IsModifiedVisible = False
    Exit Function
  End If
End Function

' alias for DocObj_IsModifiedVisible("modified", Client, DM_Object)
Function DocObj_IsModified (Client, DM_Object)
  DocObj_IsModified = DocObj_IsModifiedVisible("modified", Client, DM_Object)
End Function

' alias for DocObj_IsModifiedVisible("visible", Client, DM_Object)
Function DocObj_IsVisible (Client, DM_Object)
  DocObj_IsVisible = DocObj_IsModifiedVisible("visible", Client, DM_Object)
End Function



' Function that checks if the project file path is valid
' 3 modes of verification: based on Altium Designer ("properties"), based on file existence ("existence"), or both of them ("both")
Function IsProjectPathValid (Project, CheckMode)
  If Project Is Nothing Then Exit Function
  If TypeName(Project) <> "Object" Then Exit Function
  If TypeName(CheckMode) <> "String" Then Exit Function
  If CheckMode <> "properties" and CheckMode <> "existence" and CheckMode <> "both" Then Exit Function
  
  Dim PropertiesValid
  Dim ExistenceValid
  
  On Error Resume Next ' if an error occurs, discard it and go to the next instruction (= enable errors handling)
  Err.Clear ' clear previous errors
  Project.DM_ProjectFullPath ' check if the property DM_ProjectFullPath exists
  If Err.Number <> 0 Then ' there is an error
    IsProjectPathValid = False ' if it does not exist, the project object is invalid
    Exit Function
  End If
  On Error Goto 0 ' disables the 'On Error Resume Next' statement (= disable errors handling)
  
  If (CheckMode = "properties" or CheckMode = "both") Then
    ' The following conditions give an invalid path: empty path, fullpath = filename, fullpath = free doc project path
    If Project.DM_ProjectFullPath <> "" and _
       Project.DM_ProjectFullPath <> Project.DM_ProjectFileName and _
       Project.DM_ProjectFullPath <> GetWorkspace.DM_FreeDocumentsProject.DM_ProjectFullPath _
    Then
      PropertiesValid = True
    Else
      PropertiesValid = False
    End If
  End If
  If (CheckMode = "existence" or CheckMode = "both") Then
    ExistenceValid = FileExists(Project.DM_ProjectFullPath)
  End If
      
  Select Case CheckMode
  Case "properties"
    IsProjectPathValid = PropertiesValid
  Case "existence"
    IsProjectPathValid = ExistenceValid
  Case "both"
    IsProjectPathValid = PropertiesValid And ExistenceValid
  Case Else
    IsProjectPathValid = False
  End Select
End Function
