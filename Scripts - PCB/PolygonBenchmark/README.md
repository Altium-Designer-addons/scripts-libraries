# Polygon Repour Benchmark Script

## What is it
This script is a utility tool to benchmark the relative repour times of the polygons on a .PcbDoc.

## How to install and use
_Step 1_: [DOWNLOAD](https://minhaskamal.github.io/DownGit/#/home?url=https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts%20-%20PCB/PolygonBenchmark) script

_Step 2_: integrate the script into Altium Designer and execute it.\
If you are a newcomer to Altium scripts, [please read the "how to" wiki page](https://github.com/Altium-Designer-addons/scripts-libraries/wiki/HowTo_execute_scripts).

## Usage guide
### Select Assembly Designators
Make a .PcbDoc active and run the _Start_ script procedure.

## Features
* ### Can save results to file and string in .PcbDoc
Results will be saved to a file named "*PcbDoc Name*_repourtimes.txt"\
Place a string on the PcbDoc containing "PolygonBenchmark" to show the results on a layer.\
**NOTE:** if confirmed, script will replace the contents of the first string it finds that contains "PolygonBenchmark".

* ### Reports polygon repour time of each polygon, and total repour time

* ### Repours polygons in their proper order

## Known Issues
* ### Serializes pour order. No multi-threading here.

## Change log
2023-05-26 by Ryan Rutledge : v0.3 - initial release
