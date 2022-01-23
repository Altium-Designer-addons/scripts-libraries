# Round Polygon Corners
Iterates through each polygon and make it corners round.

## Motivation
In my company one of pcb best practice is all polygon corners must be round. And it's real hard to do because: no way to round all corner by click, no way to set round radius (or chord) value, no way to move sides after corner round (use need unround it, move side, round again). This script helps.

## How to Run
1. Run script from pcb layout.
2. A GUI will open. Select options and run.

![GUI Screenshot](GUI_Screenshot.png)

## Features
Basic features could be illustrated with this screens step by step:
1. Original Polygon

![GUI Step 1](GUI_Step0_POI.png)

2. Remove Small Edges

![GUI Step 2](GUI_Step1_POI.png)

3. Remove Redundant Vertices

![GUI Step 3](GUI_Step2_POI.png)

4. Unwrap Bevels

![GUI Step 4](GUI_Step3_POI.png)

5. Aling To Axis

![GUI Step 5](GUI_Step4_POI.png)

6. Round Corners

![GUI Step 6](GUI_Step5_POI.png)

7. Equalize Radius

![GUI Step 7](GUI_Step6_POI.png)
