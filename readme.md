# BBS-Screen-Menu-Creator-for-All-BBSs-Version-18-Alpha-Release
 Version 16 with up to 4 inner boxes, each one a different color
This is is version 18 of my BBS Color Screen Scheme Creator, a very easy to use program with data files included.

The program itself is very easy to use , all you do is run the exe file in the zip folder and go through a few opening screens then you get to the main screen where you can select the file to load by typing in the names on the list. You then choose which BBS type that you want to write the output file for.

The program does the rest. All you have to do to set up the program is to copy the exe and basic files to a folder that you create anywhere on your hard drive, then you copy the display folder to the root drive on drive C. 

This location was chosen so that it would be simple for anyone to put the data files in the right location, and the root drive may not be the ideal location but it is simple to do. 

If you want to change the location of the display folder to a different area, then look for the following lines in the basic source , change them and resave and recompile with Quick Basic 64:

asciidisp = "c:\display\bbsdispascii\" + file1

type1$ = asciidisp
ansidisp = "c:\display\bbsdispansi\" + file2
type2$ = ansidisp
wc8disp = "c:\wcat\disp\" + file3
type2$ = wc8disp
pcbdisp = "c:\display\bbsdisppcb\" + file4 ' all path, drive and filename variables
type2$ = pcbdisp
syndisp = "c:\display\bbsdispsyncro\" + file5
type2$ = syndisp
mysdisp = "c:\display\bbsdispmystic\" + file6
type2$ = mysdisp
asciimenu = "c:\display\bbsmenuascii\" + file1
type1$ = asciimenu
ansimenu = "c:\display\bbsmenuansi\" + file2 ' for various screen ouputs
type3$ = ansimenu
wc8menu = "c:\wcat\menu\" + file3
type3$ = wc8menu
pcbmenu = "c:\display\bbsmenupcb\" + file4
type3$ = pcbmenu
synmenu = "c:\display\bbsmenusyncro\" + file5
type3$ = synmenu
mysmenu = "C:\display\bbsmenumystic\" + file6
type3$ = mysmenu
asciipromptmenu = "c:\display\bbsmenuascii\" + file7

These statements can be found precisely at line 522 of this version of the basic program.
If you do not know how to use Quick Basic 64, then email me and 
I will try to help you with a custom copy of the exe file.
