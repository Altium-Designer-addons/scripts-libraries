/*
  Title : Radial Component Placement Script
  Author : Edward Catley
  Description : Script for the automatic placement of components radially. Support for heirarchal designs and movement of Name and Designator to match footprint
  Version : 0.1
 */

function bDrawClick(Sender){
var sourceX,sourceY;
var Source;
var board = PCBServer.GetCurrentPCBBoard();
var centerX = MMsToCoord(parseInt(Center_X_TEdit.Text));
var centerY = MMsToCoord(parseInt(Center_Y_TEdit.Text));
var startAngle = Start_Angle_TEdit.Text;
var endAngle = End_Angle_TEdit.Text;
var radius = MMsToCoord(Radius_TEdit.Text);
var firstElementPrefix = First_Element_TEdit.Text.split(/[0-9]/);
var firstElementSuffix = First_Element_TEdit.Text.split(/[A-Z,a-z]/);
var lastElementPrefix = Last_Element_TEdit.Text.split(/[0-9]/);
var lastElementSuffix =  Last_Element_TEdit.Text.split(/[A-Z,a-z]/);

var firstRoomNamePrefix = First_Room_Name_TEdit.Text.split(/[0-9]/);
var firstRoomSuffix = First_Room_Name_TEdit.Text.split(/[A-Z,a-z]/);
var lastRoomNamePrefix = Last_Room_Name_TEdit.Text.split(/[0-9]/);
var lastRoomSuffix =  Last_Room_Name_TEdit.Text.split(/[A-Z,a-z]/);

var layer = String2Layer(Layer_TEdit.Text);
var angleOffset = parseFloat(Angle_Offset_TEdit.Text);
var componentString;
var angleStep = parseFloat(Angle_Step_TEdit.Text);
var commentAngle, xCommentOffset;
var nameAngle, xNameOffset;
var angle = parseFloat(startAngle);
var nameDistance, commentDistance;


for (j = parseInt(firstRoomSuffix); j <= parseInt(lastRoomSuffix); j++)
{
    for(i = parseInt(firstElementSuffix); i<=parseInt(lastElementSuffix); i++)
        {
        //Check to see if room names are used
        if(firstRoomNamePrefix != ""){
        componentString = firstRoomNamePrefix+j+"_"+firstElementPrefix+i;
        }
        else{
        componentString =firstElementPrefix+i;
        }
        Source = board.GetPcbComponentByRefDes(componentString);

        //Check to see if component exists
        if(Source != null)
        {

        //required to prevent iterative name & comment drift
        Source.Rotation=0;
        Source.Comment.Rotation = 0;
        Source.Name.Rotation= 0;
        Source.AutoPosition_NameComment();

        //Calculate Source XY
        sourceX = radius*cos(angle* (Math.PI/180))+centerX;
        sourceY = radius*sin(angle* (Math.PI/180))+centerY;

        //Calculate x text offset
        xNameOffset = Source.X  - Source.Name.XLocation;
        xCommentOffset = Source.X  - Source.Comment.XLocation;

        //Calculate text Y Distance
        nameDistance = Math.sqrt(Math.pow(Source.Name.YLocation - Source.Y,2));
        commentDistance = Math.sqrt(Math.pow(Source.Comment.YLocation - Source.Y,2));

        //Calculate angle from origin for name and comment from arcos of the Y distance
        commentAngle = Math.acos((Math.pow(commentDistance,2) - 2*(Math.pow(radius,2)))/(-2*radius*radius));
        nameAngle = Math.acos((Math.pow(nameDistance,2) - 2*(Math.pow(radius,2)))/(-2*radius*radius));

        //Calculate Name XY
        NameX = (radius-xNameOffset)*cos(nameAngle+(angle*Math.PI/180))+centerX;
        NameY =(radius-xNameOffset)*sin(nameAngle+(angle*Math.PI/180))+centerY;

        //Calculate comment XY
        CommentX = (radius-xCommentOffset)*cos(commentAngle+(angle*Math.PI/180))+centerX;
        CommentY =(radius-xCommentOffset)*sin(commentAngle+(angle*Math.PI/180))+centerY;

        //Move Source
        Source.Layer = layer;
        Source.Rotation = angle+angleOffset;
        Source.MoveToXY(sourceX,sourceY);


        //Move Name
        Source.Name.MoveToXY(NameX,NameY);
        Source.Name.Rotation = angle+angleOffset;

        // Move comment
        Source.Comment.MoveToXY(CommentX,CommentY);
        Source.Comment.Rotation = angle+angleOffset;

        // Increment angle
        angle = angle + angleStep;
        }

    }
    }
}


function bCloseClick(Sender)
{
    Close();
}


















