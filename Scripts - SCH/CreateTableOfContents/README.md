# CreateTableOfContents
Procedure that compiles the focused project and creates a table of contents page automatically.


## Usage
Note that the project must already have a page with the filename of "Table of Contents" and have a hierarchical block pointing to the top level sheet.

1. Initially I wanted to delete all text on the page except for ones which were locked. I could not find a way to do this, as it appears the 'locked' attribute is not visible to the scripting system. Instead, all text which has the same color as the TOC defined colour will be deleted.\
If there is any text you don't want deleted from the page, I suggest incrementing or decrementing one of the RGB values by one. This will make it unique enough so that it will not get caught up in the deletions.
2. The TOC page is the top document in your logical schematic project. This script will compile the project before creating the TOC so as to ensure it has the correct document where it places the data. If your project does not sucessfully compile because of heirarchical problems, this script will fail.
3. Offsets and separation units are in DXP default. Play with the numbers to see what works best for you. The defaults are what I selected based on my default TOC page being 11 X 17 (ANSI) sized.
4. The default font set in the constants section will be used only if there was no TOC on the page previous to running this. If a TOC was already on the page, the script will scrape that data from the parameters defined on the sheet from the previous run and use it instead.
5. If (like me) you want to use this without the GUI because you use the same defaults all the time, you can change the constant UseGUI to false. This will compile and annotate the TOC sheet without a GUI prompt.


## Credits
Written by: Ron Wierckx, C.E.T, C.I.D

If you end up using this script, please let me know!  You can email me at ron@rtds.com, or better yet, send me a postcard. My daughter loves looking at postcards from around the world!
I'm curious to find out how many people find this useful.


## License
Released as freeware/open source with the following exceptions:
1. Any changes/additions be released also as freeware/open source so that others will benefit
2. the script cannot be sold or traded for capital gain.


## Changelog
Changes in version 5:
- TOC sheet will now be marked dirty so it will be saved when you quit Altium *WORKS
- Correctly remove old text when the new text color has changed
- Small bug fixes

Changes in version 4:
- Moved scrubbing of old data until *after* new TOC parameters are set.  This is so
  you can see what you want changed as you're entering in the new data
- Added parameter to set the width of the column.  This allows for varying font size,
  sheets and column amounts.
- TOC sheet will now be marked dirty so it will be saved when you quit Altium *NOTE may not work
- Consolidated some procedures as they did the exact same thing (just different parameters passed)
