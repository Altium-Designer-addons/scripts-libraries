# FindUnmatchedPorts Script

# DISCLAIMER
This script is provided "AS IS" in the hopes that it will be useful, but comes with no guarantees or warranties. Use of this script is conditional on accepting it as-is, and the user is responsible for any issues that may arise from its use, including failure to detect a critical problem that results in scrap boards. Please thoroughly verify its fitness for your particular use case.

## [DOWNLOAD](https://altium-designer-addons.github.io/DownGit/#/home?url=https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts+-+SCH/FindUnmatchedPorts)

# What This Script Is
This script helps find unmatched or floating ports within project schematic sheets.

## How to install and use
_Step 1_: [DOWNLOAD](https://altium-designer-addons.github.io/DownGit/#/home?url=https://github.com/Altium-Designer-addons/scripts-libraries/tree/master/Scripts+-+SCH/FindUnmatchedPorts) script

_Step 2_: integrate the script into Altium Designer and execute it.\
If you are a newcomer to Altium scripts, [please read the "how to" wiki page](https://github.com/Altium-Designer-addons/scripts-libraries/wiki/HowTo_execute_scripts).

## Usage
- Run script by launching `Start` script procedure
- Altium compiler may not report floating ports if they are electrically connected on other schematic sheets
- Port connection matrix also doesn't explicitly explain potential problems and may be misconfigured
- Also reports ports that don't have at least one match. Some strict type checking is applied:
  - If using input and output ports, net must have **EXACTLY** one output port, **AT LEAST** 1 input port, and **NO** bidirectional ports
  - If using bidirectional ports, net must have **NO** output ports, **NO** input ports, and **AT LEAST** 2 bidirectional ports
  - All other mixes of port types are *right out*

Script will search all schematic sheets within the project and report floating ports. Floating ports are reported in the Messages tab.\
Ports are considered floating on a given schematic sheet if they are not connected to either a component pin or sheet entry.\
\
Script will also report any nets that have an "invalid" combination of port directions per criteria above, or which only have a single port in the net.


## Changelog
- 2023-12-05 by Corey Beyer & Ryan Rutledge : v1.00 - Initial release based on Corey's FindFloatingPorts v1.1
- 2023-12-07 by Ryan Rutledge : v1.10 - added support for detecting when a net has mixed port names (sometimes this is intentional!); unmatched ports list will now ignore floating ports (if they were previously identified)
- 2023-12-07 by Ryan Rutledge : v1.11 - hotfix to allow nets to have all unspecified-direction ports
