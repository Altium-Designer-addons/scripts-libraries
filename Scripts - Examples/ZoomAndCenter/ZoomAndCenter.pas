{..............................................................................}
{ Summary   This script moves corsor location to center on zoom in and out.    }
{                                                                              }
{                                                                              }
{ Created by:    Petar Perisin                                                 }
{..............................................................................}

{..............................................................................}


Procedure ZoomInAndCenterSch;
begin
   if SchServer.GetCurrentSchDocument = nil then exit;

   ResetParameters;
   AddStringParameter('Action','Pan');
   RunProcess('Sch:Zoom');
   ResetParameters;
   AddStringParameter('Action','ZoomIn');
   RunProcess('Sch:Zoom');
end;


Procedure ZoomOutAndCenterSch;
begin
   if SchServer.GetCurrentSchDocument = nil then exit;

   ResetParameters;
   AddStringParameter('Action','Pan');
   RunProcess('Sch:Zoom');
   ResetParameters;
   AddStringParameter('Action','ZoomOut');
   RunProcess('Sch:Zoom');
end;


Procedure ZoomInAndCenterPCB;
begin
   if PCBServer.GetCurrentPCBBoard = nil then exit;

   ResetParameters;
   AddStringParameter('Action','In');
   RunProcess('PCB:Zoom');
   ResetParameters;
   AddStringParameter('Action','Pan');
   RunProcess('PCB:Zoom');
end;


Procedure ZoomOutAndCenterPCB;
begin
   if PCBServer.GetCurrentPCBBoard = nil then exit;

   ResetParameters;
   AddStringParameter('Action','Out');
   RunProcess('PCB:Zoom');
   ResetParameters;
   AddStringParameter('Action','Pan');
   RunProcess('PCB:Zoom');
end;

