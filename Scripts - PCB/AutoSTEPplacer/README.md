# AutoSTEPplacer
This scripts can be used to Auto load STEP files in PcbLib file.


## Usage
STEP files need to have the same name as footprints.\
If names are not 100% same, you can use "Smart Name Detection" that will try to find corrent 3D model by removing last characters in footprint name.

"Include Subfolders" option will include subfolders into detection if 3D model is found, script will remove existing 3D models from footprint if "Delete Existing models is checked".

Most 3D CAD tools have Z axis that points up - except SolidWorks, which has Y pointing up. You can choose up axis in popup menu.


## Credits
Created by: Petar Perisin