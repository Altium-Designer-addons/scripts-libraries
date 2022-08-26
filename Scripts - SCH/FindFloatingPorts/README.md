# Find Floating Ports
Find floating ports within project schematic sheets.      

## Usage
Altium compiler does not report floating ports if they are electrically connected on other schematic sheets.

Script will search all schematic sheets within the project and report floating ports.  
Floating ports are reported in the Messages tab.

Ports are considered floating on a given schematic sheet if they are not connected to either a component pin or sheet entry.

## Changelog
Version 1.0 - Corey Beyer - Initial Release  
Version 1.1 - Corey Beyer - Hid private functions; Added to output message  
