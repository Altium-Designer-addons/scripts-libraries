{
**  SCH_GetSelectedComponents
*     This script returns the selected components on a sheet as a TObjectList
*
}
Function SCH_GetSelectedComponents(FilterSet: TObjectSet = nil): TObjectList;
Var
    iterator    :   ISch_Iterator;
    MySCH       :   ISch_Sheet;
    s           :   ISch_GraphicalObject;
    ResultList  :   TObjectList;
Begin
    // ensure we're in a schematic doc
    MySCH := SCHServer.GetCurrentSchDocument;
    If MySCH = Nil then Exit;

    // Initialize iterator
    try
        iterator := MySCH.SchIterator_Create;
        ResultList := TObjectList.Create;
        iterator.SetState_FilterAll;
        iterator.SetState_IterationDepth(eIterateAllLevels);

        if (FilterSet = Nil) then iterator.AddFilter_CurrentPartPrimitives
        else iterator.AddFilter_ObjectSet(FilterSet);

        s := iterator.FirstSCHObject;

        while (s <> Nil) do
        begin
            if (s.Selection = true) then
            begin
                ResultList.Add(s);
            end;
            s := iterator.NextSchObject;
        end;

        Result := ResultList;
    finally
        ResultList.Destroy;
        MySCH.SchIterator_Destroy(iterator);
    end;
End;

{
GetPinData Script:
This script for Altium Designer pastes to clipboard all pin designators, names, and nets for the (first) selected component

Copyright (C) 2015  John Michael Go-Soco

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>
}

Function PinDataToText(pin: INetItem): String;
var
    i           : Integer;
    netName     : String;
Begin
    if (pin.DM_OwnerNetLogical.DM_PinCount  = 1)  AND (pin.DM_OwnerNetLogical.DM_IsLocal)
        then netName := ''
        else netName := pin.DM_NetName;
    Result := pin.DM_PinNumber + ','
            + pin.DM_PinName + ','
            + netName;
End;


Procedure GetPinData;
Var
    project             :   IProject;
    document            :   IDocument;
    net                 :   INet;
    pin                 :   INetItem;
    component           :   ISch_Component;
    objectList          :   TObjectList;
    uniqueID            :   String;
    str, tmpstr         :   String;
    projectName         :   String;
    componentName       :   String;
    componentDesignator :   String;
    i, j, k             :   Integer;
    pinCounter          :   Integer;

Begin
    project := GetWorkspace.DM_FocusedProject;
    project.DM_Compile;
    projectName := ExtractFileNameFromPath(project.DM_ProjectFileName); // get the project name	
    objectList := SCH_GetSelectedComponents;
    if objectList.Count = 0 then
    begin
        showMessage ('Please select a component');
        Exit;
    end;

    pinCounter := 0;

    // get the designator of the component in question
    component := objectList.First;
    componentName := component.Comment.CalculatedValueString;
    componentDesignator := component.FullPartDesignator(0);

    str := ',' + componentName + ',' + projectName;

    // for each document in project...
    for i := 0 to project.DM_LogicalDocumentCount - 1 do
    begin
        document := project.DM_LogicalDocuments(i);

        // for each DM_Nets in document...
        for j := 0 to document.DM_NetCount - 1 do
        begin
            net := document.DM_Nets(j);

            // for each DM_Pins in nets...
            for k := 0 to net.DM_PinCount - 1 do
            begin
                pin := net.DM_Pins(k);

                // if DM_LogicalPartDesignator matches our target designator
                if pin.DM_LogicalPartDesignator = componentDesignator then
                begin
                    str := str + #13#10 + PinDataToText(pin);
                    pinCounter := pinCounter + 1;
                end;
            end;
        end;
    end;
    showmessage('Pin data copied to clipboard: ' + IntToStr(pinCounter));
    Clipboard.AsText := str;
End;




