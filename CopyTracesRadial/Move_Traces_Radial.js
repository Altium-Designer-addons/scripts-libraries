/*
  Title : Radial Trace Placement Script
  Author : Edward Catley
  Description : Script for the cloning and placement of traces and vias radially.
  Version : 0.1
  Usage : Lay the initial traces and vias. Select them on the PCB and run the script. Select the centre around which the objects will be rotated.
  Enter the start suffix and end suffix of the net names (the number of steps is derived from this so it must be entered even if the net names will
  not be automatically incremented. For example if your template traces have the net names "netLED5" and you want these replicated and rotated 10
  more times then 5 would go in the start suffix and 15 would go in the end suffix. Select the angle step between each iteration and then tick the
  "Auto increment net name" box and the "Flip layer" box if desired.
 */


function bEnterClick(Sender)
{
var centreX = MMsToCoord(parseFloat(CentreX_TEdit.Text));
var centreY = MMsToCoord(parseFloat(CentreY_TEdit.Text));
var board = PCBServer.GetCurrentPCBBoard();
var originX1;
var originX2;
var originY1;
var originY2;
var angleStep = parseFloat(Angle_Step_TEdit.Text);
var startN = parseInt(Start_N_TEdit.Text);
var endN = parseInt(End_N_TEdit.Text);
var netSuffix = "";
var splitString = [];
var angle = angleStep;
for(i = startN+1; i<=endN; i++)
{
       ComponentIteratorHandle = board.BoardIterator_Create;
       ComponentIteratorHandle.AddFilter_ObjectSet(MkSet(eTrackObject));
       ComponentIteratorHandle.AddFilter_IPCB_LayerSet(AllLayers);
       ComponentIteratorHandle.AddFilter_Method(eProcessAll);
       Component = ComponentIteratorHandle.FirstPCBObject;
       while (Component != null) {
           if (Component.Selected == true) {
              netSuffix="";
              netString = "";
              splitString.length = 0;
              firstElementPrefix = "";
                 splitString = Component.GetState_Net.GetState_Name.split('_');
                 firstElementPrefix = Component.GetState_Net.GetState_Name.split(/[0-9]/)[0];
                 for(j = 1; j <splitString.length; j++)
                 {
                 netSuffix =netSuffix+"_"+splitString[j];
                 }
                 if(autoincrementBox.State == cbChecked)
                 {
                 netString = firstElementPrefix+i+netSuffix;
                 }
                 else
                 {
                  netString = Component.Net.GetState_Name;
                 }
                 Track = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default);
                 originX1 = Component.X1;
                 originX2 = Component.X2;
                 originY1 = Component.Y1;
                 originY2 = Component.Y2;

                 node1radius = Math.sqrt(Math.pow((originX1 - centreX),2)+Math.pow((originY1 - centreY),2));
                 node2radius = Math.sqrt(Math.pow((originX2 - centreX),2)+Math.pow((originY2 - centreY),2));
                 node1angle = Math.atan((originY1-centreY)/(originX1-centreX));
                 node2angle = Math.atan((originY2-centreY)/(originX2-centreX));
                 if(originX1 > MMsToCoord(200))
                 {
                  if(originY1 < MMsToCoord(200)){
                     node1angle = node1angle + 2*Math.PI;
                     }
                 }
                 else
                 {
                       node1angle = node1angle + Math.PI;
                 }

                 if(originX2 > MMsToCoord(200))
                 {
                  if(originY2 < MMsToCoord(200)){
                     node2angle = node2angle + 2*Math.PI;
                     }
                 }
                 else
                 {
                       node2angle = node2angle + Math.PI;
                 }
                 Track.X1 = node1radius*cos(node1angle+(angle*Math.PI/180))+centreX;
                 Track.Y1 = node1radius*sin(node1angle+(angle*Math.PI/180))+centreY;
                 Track.X2 = node2radius*cos(node2angle+(angle*Math.PI/180))+centreX;
                 Track.Y2 = node2radius*sin(node2angle+(angle*Math.PI/180))+centreY;
                 NetIteratorHandle = board.BoardIterator_Create;
                 NetIteratorHandle.AddFilter_ObjectSet(MkSet(eNetObject));
                 NetIteratorHandle.AddFilter_IPCB_LayerSet(AllLayers);
                 NetIteratorHandle.AddFilter_Method(eProcessAll);
                 Net = NetIteratorHandle.FirstPCBObject;
                 while(Net != null)
                 {
                           if(Net.GetState_Name == netString)
                           {
                           Track.SetState_Net(Net);
                           Track.Layer = Component.Layer;
                                if(FlipLayerBox.State == cbChecked)
                                {
                                          Track.FlipXY(MMsToCoord(200),eHMirror);
                                }

                           board.AddPCBObject(Track);
                           }
                 Net = NetIteratorHandle.NextPCBObject;
                 }
                 board.BoardIterator_Destroy(NetIteratorHandle);
                }
            Component = ComponentIteratorHandle.NextPCBObject;
        }
        board.BoardIterator_Destroy(ComponentIteratorHandle);

       ViaIteratorHandle = board.BoardIterator_Create;
       ViaIteratorHandle.AddFilter_ObjectSet(MkSet(eViaObject));
       ViaIteratorHandle.AddFilter_IPCB_LayerSet(AllLayers);
       ViaIteratorHandle.AddFilter_Method(eProcessAll);
       SourceVia = ViaIteratorHandle.FirstPCBObject;


       while (SourceVia != null) {
            if (SourceVia.Selected == true) {
            netSuffix="";
              netString = "";
              splitString.length = 0;
                 splitString = SourceVia.GetState_Net.GetState_Name.split('_');
                 for(j = 1; j <splitString.length; j++)
                 {
                 netSuffix =netSuffix+"_"+splitString[j];
                 }
                 if(autoincrementBox.State == cbChecked)
                 {
                 netString = firstElementPrefix+i+netSuffix;
                 }
                 else
                 {
                       netString = SourceVia.Net.GetState_Name;
                 }
                 Via = PCBServer.PCBObjectFactory(eViaObject, eNoDimension, eCreate_Default);
                 Via.X = SourceVia.X;
                 Via.Y = SourceVia.Y;
                 Via.RotateAroundXY(MMsToCoord(200),MMsToCoord(200),angle);
                 NetIteratorHandle = board.BoardIterator_Create;
                 NetIteratorHandle.AddFilter_ObjectSet(MkSet(eNetObject));
                 NetIteratorHandle.AddFilter_IPCB_LayerSet(AllLayers);
                 NetIteratorHandle.AddFilter_Method(eProcessAll);
                 Net = NetIteratorHandle.FirstPCBObject;
                 while(Net != null)
                 {
                           if(Net.GetState_Name == netString)
                           {
                           Via.SetState_Net(Net);
                            Via.Size      = SourceVia.Size;
                            Via.HoleSize  = SourceVia.HoleSize;
                            Via.LowLayer  = SourceVia.LowLayer;
                            Via.HighLayer = SourceVia.HighLayer;
                           Via.Layer = SourceVia.Layer;
                           if(FlipLayerBox.State == cbChecked)
                                {
                                          Via.FlipXY(MMsToCoord(200),eHMirror);
                                }
                           board.AddPCBObject(Via);
                           }
                 Net = NetIteratorHandle.NextPCBObject;
                 }
                 board.BoardIterator_Destroy(NetIteratorHandle);
                }
            SourceVia = ViaIteratorHandle.NextPCBObject;
            }
            board.BoardIterator_Destroy(ViaIteratorHandle);
            angle = angle+angleStep;
          }
    PCBServer.PostProcess;
    //Full PCB system update
    board.ViewManager_FullUpdate;

        close();
}

function bExitClick(Sender)
{
 close();
}
