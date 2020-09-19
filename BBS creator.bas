DIM SHARED x AS INTEGER ' x is used in my program as a variable for the background color
DIM SHARED y AS INTEGER ' y is used as a variable for the foreground color
DIM SHARED i AS INTEGER ' i is used as a variable for the length of the line of display
DIM SHARED b AS STRING * 1
' above variables are used for the reading and writing of each character of info
' in each line of the ascii screens that are read into an array.

DIM SHARED lnumber AS STRING ' used as a variable for the line number of the ascii screen
DIM SHARED file AS STRING ' used as a variable for the filename entered in the program
DIM answer AS STRING ' used as a variable for the answer to a question
DIM SHARED asciidisp AS STRING ' ascii display variable used as the ascii display path
DIM SHARED ansidisp AS STRING ' ansi display variable used as the ansi path and filename
DIM SHARED wc8disp AS STRING 'Winserver 8 variable used as the wc8 BBS display path
DIM SHARED pcbdisp AS STRING 'Pc Board display variable used as the pcb display path
DIM SHARED asciipromptmenu AS STRING
DIM SHARED syndisp AS STRING 'Syncronet display variabe used as the Syncronet display path
DIM SHARED asciimenu AS STRING 'ascii menu variable used as the ascii menu path for input
DIM SHARED ansimenu AS STRING 'ansi menu variable used as the ansi path for output
DIM SHARED wc8menu AS STRING 'Winserver 8 variable used as the wc8 menu bbs path for output
DIM SHARED pcbmenu AS STRING 'PC Board variable used as the PcB BBS menu display for output
DIM SHARED synmenu AS STRING 'Syncronet variable used as the Syncronet Menu Display for output
DIM SHARED mysmenu AS STRING 'Mystic variable
DIM SHARED mysdisp AS STRING 'Mystic variable

'above variables are all string variables that are used for storing different information that in the
'Wildcat, Winserver, Ascii, and Ansi screens that are either read in , or written out to different filenames
'in my program. The *120, or *16 etc are just string lenghths.

DIM SHARED file1 AS STRING ' file1 is the ascii input file for the displays or menus that either other people or I create
DIM SHARED file2 AS STRING ' file2 is the ansi file created by he program from the ascii file above
DIM SHARED file3 AS STRING ' file3 is the Wildcator WInserver .BBS file that is created when the user selects wc8
DIM SHARED file4 AS STRING ' file4 is the PCBoard .BBS file created when the user selects pcb as a display
DIM SHARED file5 AS STRING ' file5 is reserved for Synchronet BBS when the user selects syncronet as a display
DIM SHARED file6 AS STRING ' file6 is reserved for Mystic BBS in the user selects mystic as a display
DIM SHARED file7 AS STRING ' file7 is reserved for the prompt files that follow any menu in any BBS

'ALL of the above are actual filenames without the drive letter and path for my program.


DIM SHARED horizontalines AS STRING
DIM SHARED verticalines AS STRING
DIM SHARED foregroundcoloransi AS STRING
DIM SHARED backgroundcoloransi AS STRING
DIM SHARED foregroundcolorbbs AS STRING
DIM SHARED backgroundcolorbbs AS STRING
DIM SHARED foregroundcolorpcb AS STRING
DIM SHARED backgroundcolorpcb AS STRING
DIM SHARED foregroundcolorsynchro AS STRING
DIM SHARED backgroundcolorsynchro AS STRING
DIM SHARED foregroundcolormystic AS STRING
DIM SHARED backgroundcolormystic AS STRING

DIM SHARED code0b AS STRING ' Code Setup for Black   Background
DIM SHARED code0f AS STRING ' Code Setup for Black   Foreground
DIM SHARED code1b AS STRING ' Code Setup for Blue    Background
DIM SHARED code1f AS STRING ' Code Setup for Blue    Foreground
DIM SHARED code2b AS STRING ' Code Setup for Green   Background
DIM SHARED code2f AS STRING ' Code Setup for Green   Foreground
DIM SHARED code3b AS STRING ' Code Setup for Cyan    Background
DIM SHARED code3f AS STRING ' Code Setup for Cyan    Foreground
DIM SHARED code4b AS STRING ' Code Setup for Red     Background
DIM SHARED code4f AS STRING ' Code Setup for Red     Foreground
DIM SHARED code5b AS STRING ' Code Setup for Magenta Background
DIM SHARED code5f AS STRING ' Code Setup for Magenta Foreground
DIM SHARED code6b AS STRING ' Code Setup for Brown   Background
DIM SHARED code6f AS STRING ' Code Setup for Yellow  Foreground
DIM SHARED code7b AS STRING ' Code Setup for White   Background
DIM SHARED code7f AS STRING ' Code Setup for White   Foreground
DIM SHARED corner1 AS STRING
DIM SHARED corner2 AS STRING
DIM SHARED corner3 AS STRING
DIM SHARED corner4 AS STRING
DIM SHARED connector1 AS STRING
DIM SHARED connector2 AS STRING
DIM SHARED connector3 AS STRING
DIM SHARED connector4 AS STRING
DIM SHARED lineconnector AS STRING
DIM SHARED menuchoice AS STRING
DIM SHARED flag AS STRING
DIM SHARED verticalline AS STRING
DIM SHARED display AS STRING
DIM SHARED pick AS INTEGER ' I add this to convert pick into a function

' general settings
RANDOMIZE TIMER
_FULLSCREEN
SCREEN 12

' start  program
SplashScreen
WaitAKey
'setup:
Info
WaitAKey
MenuMessage
WaitAKey
CLS
PRINT
rand
inputscreen1
randomchoices
inputscreen
Menu
displays
prompts
filen
combos
back
GOSUB enteryourbackground
whatyouget
create
Menu
displays

startit:
selector




'----------------------------------------------------------------------------------------------------------------------------




Menu

'-------------------------------------------------------------------------------------------------------------
' Start of actual program , continues until ended
'Start of menu selection decisions by the program
file1 = file + ".txt"
file2 = file + ".ans" ' adds file extension to each type of file
file3 = file + ".bbs"
file4 = file + ".bbs"
file5 = file + ".txt"
file6 = file + ".txt"
file7 = file + "a.txt"
asciidisp = "c:\display\bbsdispascii\" + file1

ansidisp = "c:\display\bbsdispansi\" + file2
wc8disp = "c:\display\bbsdispwildcat\" + file3
pcbdisp = "c:\display\bbsdisppcb\" + file4 ' all path, drive and filename variables
syndisp = "c:\display\bbsdispsyncro\" + file5
mysdisp = "c:\display\bbsdispmystic\" + file6
asciimenu = "c:\display\bbsmenuascii\" + file1
ansimenu = "c:\display\bbsmenuansi\" + file2 ' for various screen ouputs
wc8menu = "c:\display\bbsmenuWildcat\" + file3
pcbmenu = "c:\display\bbsmenupcb\" + file4
synmenu = "c:\display\bbsmenusyncro\" + file5
mysmenu = "C:\display\bbsmenumystic\" + file6
asciipromptmenu = "c:\display\bbsmenuascii\" + file7

IF menuchoice = "D" THEN
    contlne = 0
    picks
    selector
    OPEN asciidisp FOR INPUT AS #1

    IF display = "ansi" THEN
        OPEN ansidisp FOR OUTPUT AS #2
    END IF

    IF display = "wc8" THEN
        OPEN wc8disp FOR OUTPUT AS #2
    END IF

    IF display = "pcb" THEN
        OPEN pcbdisp FOR OUTPUT AS #2
    END IF
    IF display = "synchro" THEN
        OPEN syndisp FOR OUTPUT AS #2
    END IF
    IF display = "mystic" THEN
        OPEN mysdisp FOR OUTPUT AS #2
    END IF

END IF

IF menuchoice = "M" THEN

    contlne = 0
    picks
    selector
    OPEN asciimenu FOR INPUT AS #1
    OPEN asciipromptmenu FOR INPUT AS #3

    IF display = "ansi" THEN
        OPEN ansimenu FOR OUTPUT AS #2
    END IF

    IF display = "wc8" THEN
        OPEN wc8menu FOR OUTPUT AS #2
    END IF

    IF display = "pcb" THEN
        OPEN pcbmenu FOR OUTPUT AS #2
    END IF
    IF display = "synchro" THEN
        OPEN synmenu FOR OUTPUT AS #2
    END IF
    IF display = "mystic" THEN
        OPEN mysmenu FOR OUTPUT AS #2
    END IF
END IF ' end of menu selection decisions by the program.
IF display = "ansi" THEN
    ' Clear screen
    CLS
    ' Set background to black
    COLOR 0, 0
END IF
IF display = "wc8" THEN
    ' clear screen
    CLS
    PRINT #2, "@0" + "0@" + "@CLS@"
    ' set background to black
    COLOR 7, 0
END IF
IF display = "pcb" THEN
    ' clear screen
    CLS
    ' set background to black
    COLOR 0, 0
END IF
IF display = "synchro" THEN
    ' clear screen
    CLS
    ' set background to black
    COLOR 7, 0
END IF
IF display = "mystic" THEN
    ' clear screen
    CLS
    ' set background to black
    COLOR 7, 0
END IF

DO UNTIL EOF(1)
    linecounter = linecounter + 1
    LINE INPUT #1, lnumber

    FOR i = 1 TO LEN(lnumber)
        LOCATE linecounter + 2, (i + 2) + 1
        b = MID$(lnumber, i, 1) ' Calculates the ascii value of every character in the line
        IF display = "ansi" THEN
            specialcharacters3
            specialcharacters2
            IF flag$ = "Y1" THEN
                colorchange
                COLOR t1, g1
                x = g1
                y = t1
                displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                    PRINT b;
                ELSEIF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
            IF flag$ = "Y2" THEN
                colorchange
                COLOR t2, g2
                x = g2
                y = t2
                displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                    PRINT b;
                ELSEIF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
            IF flag$ = "Y3" THEN
                colorchange
                COLOR t, g
                x = g
                y = t
                displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                    PRINT b;
                END IF
            ELSE
                IF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
        END IF

        IF display = "wc8" THEN
            specialcharacters3
            specialcharacters2
            IF flag$ = "Y1" THEN
                colorchange
                x = g1
                y = t1
                COLOR t1, g1
                displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcolorbbs + foregroundcolorbbs + b;
                    PRINT b;
                ELSEIF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
            IF flag$ = "Y2" THEN
                colorchange
                x = g2
                y = t2
                COLOR t2, g2
                displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcolorbbs + foregroundcolorbbs + b;
                    PRINT b;
                ELSEIF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
            IF flag$ = "Y3" THEN
                colorchange
                x = g
                y = t
                COLOR t, g
                displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcolorbbs + foregroundcolorbbs + b;
                    PRINT b;
                END IF
            ELSE
                IF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
        END IF
        IF display = "pcb" THEN
            specialcharacters3
            specialcharacters2
            IF flag$ = "Y1" THEN
                colorchange
                COLOR t1, g1
                x = g1
                y = t1
                displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcolorpcb + foregroundcolorpcb + b;
                    PRINT b;
                ELSEIF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
            IF flag$ = "Y2" THEN
                colorchange
                COLOR t2, g2
                x = g2
                y = t2
                displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcolorpcb + foregroundcolorpcb + b;
                    PRINT b;
                ELSEIF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
            IF flag$ = "Y3" THEN
                colorchange
                COLOR t, g
                x = g
                y = t
                displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcolorpcb + foregroundcolorpcb + b;
                    PRINT b;
                END IF
            ELSE
                IF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
        END IF
        IF display = "synchro" THEN
            specialcharacters3
            specialcharacters2
            IF flag$ = "Y1" THEN
                colorchange
                COLOR t1, g1
                x = g1
                y = t1
                displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcolorsynchro + foregroundcolorsynchro + b;
                    PRINT b;
                ELSEIF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
            IF flag$ = "Y2" THEN
                colorchange
                COLOR t2, g2
                x = g2
                y = t2
                displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcolorsynchro + foregroundcolorsynchro + b;
                    PRINT b;
                ELSEIF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
            IF flag$ = "Y3" THEN
                colorchange
                COLOR t, g
                x = g
                y = t
                displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcolorsynchro + foregroundcolorsynchro + b;
                    PRINT b;
                END IF
            ELSE
                IF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
        END IF
        IF display = "mystic" THEN
            specialcharacters3
            specialcharacters2
            IF flag$ = "Y1" THEN
                colorchange
                COLOR t1, g1
                x = g1
                y = t1
                displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcolormystic + foregroundcolormystic + b;
                    PRINT b;
                ELSEIF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
            IF flag$ = "Y2" THEN
                colorchange
                COLOR t2, g2
                x = g2
                y = t2
                displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcolormystic + foregroundcolormystic + b;
                    PRINT b;
                ELSEIF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
            IF flag$ = "Y3" THEN
                colorchange
                COLOR t, g
                x = g
                y = t
                displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcolormystic + foregroundcolormystic + b;
                    PRINT b;
                END IF
            ELSE
                IF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
        END IF
    NEXT i
    PRINT #2, ""
    PRINT
LOOP
fini:
closeit:
CLOSE #1
PRINT #2, "@0" + "7@";
COLOR 7, 0
IF menuchoice = "M" THEN
    DO UNTIL EOF(3)
        LINE INPUT #3, lnumber
        PRINT #2, lnumber
        PRINT lnumber
    LOOP
ELSE
END IF
PRINT #2, "@0" + "7@"
CLOSE #2
CLOSE #3
COLOR 7, 0
PRINT
PRINT " Create another screen ? [Defaults to [Y]es : "
answer = INPUT$(1)
answer = UCASE$(answer)
IF answer = "N" THEN
    finish
ELSE
    contlne = 0
    CLEAR
    rand
END IF



'specialcharacters2:

'specialcharacters3:

randomsettings:

RETURN

'fixedsettings:
load
RETURN



errorhandle:
CLS
CLOSE #1
PRINT
PRINT "Most likely there was a filename that you entered"
PRINT "That did not exist or you have not created the"
PRINT "prompt file for the Menu item desired. Or you picked"
PRINT "a Menu filename and selected a display output instead "
PRINT "a Menu display, the difference is that Menu output "
PRINT "files have a prompt file, display files do not. "
PRINT
PRINT "Your File "; file1; " Has Not Been Found .... Please Try Again."
PRINT "Press any key to try again......"
WaitAKey
inputscreen



enteryourbackground:

etry1
etry2
etry3
etry4
etry5
etry6
etry7
etry8
whatyouget
load

END

'-------------------------------------------
SUB SplashScreen
    CLS
    COLOR 14, 12
    LOCATE 10, 24
    PRINT "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    LOCATE 11, 24
    PRINT "%                 BBS Screen Creator Written By Russ Campbell                  %"
    LOCATE 12, 24
    PRINT "%                                                                              %"
    LOCATE 13, 24
    PRINT "%                   Beta Version 13  Release Candidate RC2                     %"
    LOCATE 14, 24
    PRINT "%                                                                              %"
    LOCATE 15, 24
    PRINT "%                This is a beta release, not everything is working yet         %"
    LOCATE 16, 24
    PRINT "%                                                                              %"
    LOCATE 17, 24
    PRINT "%             With fully customisable color sets added to the program.         %"
    LOCATE 18, 24
    PRINT "%                                                                              %"
    LOCATE 19, 24
    PRINT "%                    Press any key to continue.............                    %"
    LOCATE 20, 24
    PRINT "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
END SUB

SUB WaitAKey
    DO WHILE INKEY$ = ""
    LOOP

END SUB

SUB Info
    COLOR 7, 0
    CLS
    PRINT
    PRINT "BBS Menu Color Scheme Creator Version 13 Final Version RC1"
    PRINT "Written by Russ Campbell Updated September 14 2020"
    PRINT
    PRINT "First of all design your menus in straight text no colors, etc"
    PRINT "Do not put any background colors in either as the program will"
    PRINT "Do that for you automatically. Use the following convention"
    PRINT "that is laid out in the next display."
    PRINT
    PRINT "This program will then take your plain text menu and turn it into"
    PRINT "a random colored display, methods will be taken to ensure that"
    PRINT "the backgound and text colors do not print the same colors"
    PRINT
    PRINT "Take a look and see what you think, it will of course write"
    PRINT "out your menus in .BBS and .ANS formats, kind of a neat feature."
    PRINT
    PRINT "Don't like what you get? Then run the program again and again"
    PRINT "until you get what you like as a menu screen."
    PRINT
    PRINT "This is a complicated program, you don't have to use boxes"
    PRINT "but for now that is the only shape that my program will"
    PRINT "accept. I will print an example of a menu on the screen so"
    PRINT "that you know exactly how to use my program with a text drawing."
    PRINT ""
    PRINT "Press any key to continue..."
END SUB

SUB MenuMessage
    CLS
    PRINT
    PRINT " ^-------------------------------------------------+"
    PRINT " !               Main Message Menu                 !"
    PRINT " *-------------------&-----------------------------="
    PRINT " !       $           !                             !"
    PRINT " *-------------------#-----------------------------="
    PRINT " ! [C] Check Your Mail   [S] Scan Messages         !"
    PRINT " ! [E] Enter New Message [J] Join Conference       !"
    PRINT " ! [F] FILE MENU         [U] Update Settings       !"
    PRINT " ! [G] Goodbye           [H] Help Level            !"
    PRINT " ! [Q] Quit to Main Menu [?] Command Help          !"
    PRINT " ! [R] Read Messages                               !"
    PRINT " ~-------------------------------------------------%"
    PRINT
    PRINT "Pay special attention to the symbols on each corner"
    PRINT "They are all different, one for each corner, to work"
    PRINT "properly with my program , they must be right. The "
    PRINT "rest of the characters also must be right except for"
    PRINT "whats in the menu. You can see the other characters"
    PRINT "that I use lines and connectors, the program turns"
    PRINT "into nice corners and borders, for double corners "
    PRINT "and borders, well , you can figure that one out."
    PRINT
    PRINT "Press any key to continue..."

END SUB

SUB rand
    DIM answer AS STRING
    ' this is a local variable visible only in this block of code
    DO
        CLS
        PRINT
        PRINT "Do you wish to have multi colors randomly chosen for everything"
        PRINT "or user set colors for the entire area that makes up all of the "
        PRINT "the menus. This can all be used in either displays or menus"
        PRINT
        PRINT "Choose [R]andom  or [U]ser set Patterns"
        PRINT "Choose [F]ixed Screens or Choose [X] to exit program"
        answer = INPUT$(1)
        answer = UCASE$(answer)
        IF answer = "R" THEN randomchoices: inputscreen
        IF answer = "F" THEN inputscreen
        IF answer = "U" THEN inputscreen1
        IF answer = "X" THEN finish

    LOOP
END SUB

SUB inputscreen1
    DO
        CLS
        PRINT "Do you wish to use pre-selected colors or use your own sets "
        PRINT "Of color selections: (10) color schemes can be customised."

        PRINT "This section is for entering your own color schemes , it is  "
        PRINT "for selecting either Preselected or your own colors."
        PRINT
        PRINT "If you want random colors, choose [Y]es as your answer."
        PRINT
        PRINT "However if you want to enter your own colors , then enter"
        PRINT " N as your choice of color selection"
        PRINT
        PRINT "Do you wish to use random colors? [Y]es  or [N]o  [Defaults to [N]o   "
        answer$ = INPUT$(1)
        answer$ = UCASE$(answer$)
        IF answer$ = "N" THEN prompts
        IF answer$ = "" THEN prompts
        IF answer$ = "Y" THEN randomchoices: inputscreen
    LOOP
END SUB

SUB prompts
    DO
        CLS
        PRINT
        PRINT "This section will either load or create a new user data file with all used"
        PRINT "variables saved in a filename that you entered. It will give you the option"
        PRINT "of loading a saved file or creating a totally new filename. You do not have"
        PRINT "to write over any existing files, and the program will be fool proof."
        PRINT
        PRINT "[L]oad or [C]reate User Data file or [X]xit Program"
        answer$ = INPUT$(1)
        database$ = UCASE$(answer$)
        IF database$ = "L" THEN filen
        IF database$ = "C" THEN combos
        IF database$ = "X" THEN finish
    LOOP
END SUB

SUB finish
    CLS
    PRINT "My program is now finished and I"
    PRINT "am looking for suggestions on ways"
    PRINT "To improve it. The program has been"
    PRINT "tested with Winserver 8.0 but has"
    PRINT "not been tested with any other BBS"
    PRINT "programs at this moment."
    PRINT
    PRINT "By now you will have noticed custom"
    PRINT "Color sets, if you have not tried "
    PRINT "this out , then give it a try."
    PRINT
    PRINT "The option to load and save and create"
    PRINT "data sets is now here , if you have not"
    PRINT "tried this feature out then give it a try."
    PRINT
    PRINT "Program written by Russ Campbell"
    PRINT "For more information on how I"
    PRINT "wrote this program, contact me"
    PRINT "on Facebook at many of the groups"
    PRINT "I am in , or email me at"
    PRINT "rcamp48@rogers.com"
    PRINT
    PRINT "Thank you for using BBS Menu Color Scheme Creator 13 RC3"
    PRINT
    END
END SUB

SUB displ

    IF display = "ansi" THEN
        IF x = 0 THEN code0b = CHR$(27) + CHR$(91) + CHR$(52) + CHR$(48) + CHR$(109) ' Black     Background
        IF y = 0 THEN code0f = CHR$(27) + CHR$(91) + CHR$(51) + CHR$(48) + CHR$(109) ' Black     Foreground
        IF x = 1 THEN code1b = CHR$(27) + CHR$(91) + CHR$(52) + CHR$(52) + CHR$(109) ' Blue      Background
        IF y = 1 THEN code1f = CHR$(27) + CHR$(91) + CHR$(51) + CHR$(52) + CHR$(109) ' Blue      Foreground
        IF x = 2 THEN code2b = CHR$(27) + CHR$(91) + CHR$(52) + CHR$(50) + CHR$(109) ' Green     Background
        IF y = 2 THEN code2f = CHR$(27) + CHR$(91) + CHR$(51) + CHR$(50) + CHR$(109) ' Green     Foreground
        IF x = 3 THEN code3b = CHR$(27) + CHR$(91) + CHR$(52) + CHR$(54) + CHR$(109) ' Cyan      Background
        IF y = 3 THEN code3f = CHR$(27) + CHR$(91) + CHR$(51) + CHR$(54) + CHR$(109) ' Cyan      Foreground
        IF x = 4 THEN code4b = CHR$(27) + CHR$(91) + CHR$(52) + CHR$(49) + CHR$(109) ' Red       Background
        IF y = 4 THEN code4f = CHR$(27) + CHR$(91) + CHR$(51) + CHR$(49) + CHR$(109) ' Red       Foreground
        IF x = 5 THEN code5b = CHR$(27) + CHR$(91) + CHR$(52) + CHR$(53) + CHR$(109) ' Magenta   Background
        IF y = 5 THEN code5f = CHR$(27) + CHR$(91) + CHR$(51) + CHR$(53) + CHR$(109) ' Magenta   Foreground
        IF x = 6 THEN code6b = CHR$(27) + CHR$(91) + CHR$(52) + CHR$(51) + CHR$(109) ' Brown     Background
        IF y = 6 THEN code6f = CHR$(27) + CHR$(91) + CHR$(51) + CHR$(51) + CHR$(109) ' Brown     Foreground
        IF x = 7 THEN code7b = CHR$(27) + CHR$(91) + CHR$(52) + CHR$(55) + CHR$(109) ' White     Background
        IF y = 7 THEN code7f = CHR$(27) + CHR$(91) + CHR$(51) + CHR$(55) + CHR$(109) ' White     Foreground
        IF x = 0 THEN backgroundcoloransi = code0b
        IF x = 1 THEN backgroundcoloransi = code1b
        IF x = 2 THEN backgroundcoloransi = code2b
        IF x = 3 THEN backgroundcoloransi = code3b
        IF x = 4 THEN backgroundcoloransi = code4b
        IF x = 5 THEN backgroundcoloransi = code5b
        IF x = 6 THEN backgroundcoloransi = code6b
        IF x = 7 THEN backgroundcoloransi = code7b
        IF y = 0 THEN foregroundcoloransi = code0f
        IF y = 1 THEN foregroundcoloransi = code1f
        IF y = 2 THEN foregroundcoloransi = code2f
        IF y = 3 THEN foregroundcoloransi = code3f
        IF y = 4 THEN foregroundcoloransi = code4f
        IF y = 5 THEN foregroundcoloransi = code5f
        IF y = 6 THEN foregroundcoloransi = code6f
        IF y = 7 THEN foregroundcoloransi = code7f
    END IF
    IF display = "wc8" THEN
        IF x = 0 THEN backgroundcolorbbs = "@0"
        IF x = 1 THEN backgroundcolorbbs = "@1"
        IF x = 2 THEN backgroundcolorbbs = "@2"
        IF x = 3 THEN backgroundcolorbbs = "@3"
        IF x = 4 THEN backgroundcolorbbs = "@4"
        IF x = 5 THEN backgroundcolorbbs = "@5"
        IF x = 6 THEN backgroundcolorbbs = "@6"
        IF x = 7 THEN backgroundcolorbbs = "@7"
        IF x = 8 THEN backgroundcolorbbs = "@8"
        IF x = 9 THEN backgroundcolorbbs = "@9"
        IF x = 10 THEN backgroundcolorbbs = "@A"
        IF x = 11 THEN backgroundcolorbbs = "@B"
        IF x = 12 THEN backgroundcolorbbs = "@C"
        IF x = 13 THEN backgroundcolorbbs = "@D"
        IF x = 14 THEN backgroundcolorbbs = "@E"
        IF x = 15 THEN backgroundcolorbbs = "@F"
        IF y = 0 THEN foregroundcolorbbs = "0@"
        IF y = 1 THEN foregroundcolorbbs = "1@"
        IF y = 2 THEN foregroundcolorbbs = "2@"
        IF y = 3 THEN foregroundcolorbbs = "3@"
        IF y = 4 THEN foregroundcolorbbs = "4@"
        IF y = 5 THEN foregroundcolorbbs = "5@"
        IF y = 6 THEN foregroundcolorbbs = "6@"
        IF y = 7 THEN foregroundcolorbbs = "7@"
        IF y = 8 THEN foregroundcolorbbs = "8@"
        IF y = 9 THEN foregroundcolorbbs = "9@"
        IF y = 10 THEN foregroundcolorbbs = "A@"
        IF y = 11 THEN foregroundcolorbbs = "B@"
        IF y = 12 THEN foregroundcolorbbs = "C@"
        IF y = 13 THEN foregroundcolorbbs = "D@"
        IF y = 14 THEN foregroundcolorbbs = "E@"
        IF y = 15 THEN foregroundcolorbbs = "F@"
    END IF
    IF display = "pcb" THEN
        IF x = 0 THEN backgroundcolorpcb$ = "@X0"
        IF x = 1 THEN backgroundcolorpcb$ = "@X1"
        IF x = 2 THEN backgroundcolorpcb$ = "@X2"
        IF x = 3 THEN backgroundcolorpcb$ = "@X3"
        IF x = 4 THEN backgroundcolorpcb$ = "@X4"
        IF x = 5 THEN backgroundcolorpcb$ = "@X5"
        IF x = 6 THEN backgroundcolorpcb$ = "@X6"
        IF x = 7 THEN backgroundcolorpcb$ = "@X7"
        IF x = 8 THEN backgroundcolorpcb$ = "@X8"
        IF x = 9 THEN backgroundcolorpcb$ = "@X9"
        IF x = 10 THEN backgroundcolorpcb$ = "@XA"
        IF x = 11 THEN backgroundcolorpcb$ = "@XB"
        IF x = 12 THEN backgroundcolorpcb$ = "@XC"
        IF x = 13 THEN backgroundcolorpcb$ = "@XD"
        IF x = 14 THEN backgroundcolorpcb$ = "@XE"
        IF x = 15 THEN backgroundcolorpcb$ = "@XF"
        IF y = 0 THEN foregroundcolorpcb$ = "0@"
        IF y = 1 THEN foregroundcolorpcb$ = "1@"
        IF y = 2 THEN foregroundcolorpcb$ = "2@"
        IF y = 3 THEN foregroundcolorpcb$ = "3@"
        IF y = 4 THEN foregroundcolorpcb$ = "4@"
        IF y = 5 THEN foregroundcolorpcb$ = "5@"
        IF y = 6 THEN foregroundcolorpcb$ = "6@"
        IF y = 7 THEN foregroundcolorpcb$ = "7@"
        IF y = 8 THEN foregroundcolorpcb$ = "8@"
        IF y = 9 THEN foregroundcolorpcb$ = "9@"
        IF y = 10 THEN foregroundcolorpcb$ = "A@"
        IF y = 11 THEN foregroundcolorpcb$ = "B@"
        IF y = 12 THEN foregroundcolorpcb$ = "C@"
        IF y = 13 THEN foregroundcolorpcb$ = "D@"
        IF y = 14 THEN foregroundcolorpcb$ = "E@"
        IF y = 15 THEN foregroundcolorpcb$ = "F@"
    END IF
    IF display = "synchro" THEN
        IF x = 0 THEN backgroundcolorsyncro$ = CHR$(1) + "0"
        IF x = 1 THEN backgroundcolorsyncro$ = CHR$(1) + "1"
        IF x = 2 THEN backgroundcolorsyncro$ = CHR$(1) + "2"
        IF x = 3 THEN backgroundcolorsyncro$ = CHR$(1) + "3"
        IF x = 4 THEN backgroundcolorsyncro$ = CHR$(1) + "4"
        IF x = 5 THEN backgroundcolorsyncro$ = CHR$(1) + "5"
        IF x = 6 THEN backgroundcolorsyncro$ = CHR$(1) + "6"
        IF x = 7 THEN backgroundcolorsyncro$ = CHR$(1) + "7"
        IF y = 0 THEN foregroundcolorsyncro$ = "k"
        IF y = 1 THEN foregroundcolorsyncro$ = "b"
        IF y = 2 THEN foregroundcolorsyncro$ = "g"
        IF y = 3 THEN foregroundcolorsyncro$ = "c"
        IF y = 4 THEN foregroundcolorsyncro$ = "r"
        IF y = 5 THEN foregroundcolorsyncro$ = "m"
        IF y = 6 THEN foregroundcolorsyncro$ = "y"
        IF y = 7 THEN foregroundcolorsyncro$ = "w"
    END IF
    IF display = "mystic" THEN
        IF x = 0 THEN backgroundcolormystic$ = CHR$(254) + "16"
        IF x = 1 THEN backgroundcolormystic$ = CHR$(254) + "17"
        IF x = 2 THEN backgroundcolormystic$ = CHR$(254) + "18"
        IF x = 3 THEN backgroundcolormystic$ = CHR$(254) + "19"
        IF x = 4 THEN backgroundcolormystic$ = CHR$(254) + "20"
        IF x = 5 THEN backgroundcolormystic$ = CHR$(254) + "21"
        IF x = 6 THEN backgroundcolormystic$ = CHR$(254) + "22"
        IF x = 7 THEN backgroundcolormystic$ = CHR$(254) + "23"
        IF x = 8 THEN backgroundcolormystic$ = CHR$(254) + "24"
        IF x = 9 THEN backgroundcolormystic$ = CHR$(254) + "25"
        IF x = 10 THEN backgroundcolormystic$ = CHR$(254) + "26"
        IF x = 11 THEN backgroundcolormystic$ = CHR$(254) + "27"
        IF x = 12 THEN backgroundcolormystic$ = CHR$(254) + "28"
        IF x = 13 THEN backgroundcolormystic$ = CHR$(254) + "29"
        IF x = 14 THEN backgroundcolormystic$ = CHR$(254) + "30"
        IF x = 15 THEN backgroundcolormystic$ = CHR$(254) + "31"
        IF y = 0 THEN foregroundcolormystic$ = CHR$(254) + "00"
        IF y = 1 THEN foregroundcolormystic$ = CHR$(254) + "01"
        IF y = 2 THEN foregroundcolormystic$ = CHR$(254) + "02"
        IF y = 3 THEN foregroundcolormystic$ = CHR$(254) + "03"
        IF y = 4 THEN foregroundcolormystic$ = CHR$(254) + "04"
        IF y = 5 THEN foregroundcolormystic$ = CHR$(254) + "05"
        IF y = 6 THEN foregroundcolormystic$ = CHR$(254) + "06"
        IF y = 7 THEN foregroundcolormystic$ = CHR$(254) + "07"
        IF y = 8 THEN foregroundcolormystic$ = CHR$(254) + "08"
        IF y = 9 THEN foregroundcolormystic$ = CHR$(254) + "09"
        IF y = 10 THEN foregroundcolormystic$ = CHR$(254) + "10"
        IF y = 11 THEN foregroundcolormystic$ = CHR$(254) + "11"
        IF y = 12 THEN foregroundcolormystic$ = CHR$(254) + "12"
        IF y = 13 THEN foregroundcolormystic$ = CHR$(254) + "13"
        IF y = 14 THEN foregroundcolormystic$ = CHR$(254) + "14"
        IF y = 15 THEN foregroundcolormystic$ = CHR$(254) + "15"
    END IF
END SUB

SUB load
    CLS
    PRINT
    PRINT "Do you wish to load a configuration file for later on [Y] or [N] : "
    answer$ = UCASE$(answer$)
    IF answer$ = "N" THEN inputscreen1
    INPUT "Enter Filename [No extender :  ", FILES$
    PRINT
    FILES$ = FILES$ + ".dat"
    filename$ = "c:\display\data\" + file$
    PRINT
    PRINT "Loading data..........."
    PRINT
    OPEN filename$ FOR INPUT AS #4
    INPUT #4, usercolorentry_g1
    INPUT #4, usercolorentry_t1
    INPUT #4, usercolorentry_g2
    INPUT #4, usercolorentry_t2
    INPUT #4, color1background
    INPUT #4, color1foreground
    INPUT #4, color2background
    INPUT #4, color2foreground
    INPUT #4, color3background
    INPUT #4, color3foreground
    INPUT #4, color4background
    INPUT #4, color4foreground
    INPUT #4, color5background
    INPUT #4, color5foreground
    INPUT #4, color6background
    INPUT #4, color6foreground
    INPUT #4, color7background
    INPUT #4, color7foreground
    INPUT #4, color8background
    INPUT #4, color8foreground
    CLOSE #4
    whatyouget
END SUB

SUB whatyouget
    CLS
    PRINT
    COLOR 7, 0
    PRINT
    PRINT "This is what you get for colors [Is it still acceptable] [Y] or [N] : "

    COLOR usercolorentry_t1, usercolorentry_g1
    PRINT "[------------------------------------------------------]"
    COLOR color1foreground, color1background
    PRINT "[This is line 1 of 8 printed to see what it looks like.]"
    COLOR usercolorentry_t1, usercolorentry_g1

    PRINT "[------------------------------------------------------]"
    COLOR color2foreground, color2background
    PRINT "[This is line 2 of 8 printed to see what it looks like.]"
    COLOR usercolorentry_t1, usercolorentry_g1

    PRINT "[------------------------------------------------------]"
    COLOR color3foreground, color3background
    PRINT "[This is line 3 of 8 printed to see what it looks like.]"
    COLOR usercolorentry_t1, usercolorentry_g1

    PRINT "[------------------------------------------------------]"
    COLOR color4foreground, color4background
    PRINT "[This is line 4 of 8 printed to see what it looks like.]"
    COLOR usercolorentry_t1, usercolorentry_g1

    PRINT "[------------------------------------------------------]"
    COLOR color5foreground, color5background
    PRINT "[This is line 5 of 8 printed to see what it looks like.]"
    COLOR usercolorentry_t1, usercolorentry_g1

    PRINT "[------------------------------------------------------]"
    COLOR color6foreground, color6background

    PRINT "[This is line 6 of 8 printed to see what it looks like.]"
    COLOR usercolorentry_t1, usercolorentry_g1

    PRINT "[------------------------------------------------------]"
    COLOR color7foreground, color7background

    PRINT "[This is line 7 of 8 printed to see what it looks like.]"
    COLOR usercolorentry_t1, usercolorentry_g1

    PRINT "[------------------------------------------------------]"
    COLOR color8foreground, color8background
    PRINT "[This is line 8 of 8 printed to see what it looks like.]"
    COLOR usercolorentry_t1, usercolorentry_g1

    PRINT "[------------------------------------------------------]"

    COLOR 7, 0
    PRINT
    PRINT "Acceptable ???? [Y]es or [N]o : "
    PRINT
    answer$ = INPUT$(1)
    answer$ = UCASE$(answer$)
    IF answer$ = "N" THEN inputscreen1
    create
END SUB

SUB colorchange
    IF flag$ = "Y1" THEN
        g1 = 3
        t1 = 6
        colorchanges = 1
    ELSEIF flag$ = "Y2" THEN
        g2 = 6
        t2 = 7
        colorchanges = 1
    ELSEIF flag$ = "Y3" THEN
        g = 4
        t = 7
        colorchanges = 1
    ELSE
        g = 4
        t = 7
        colorchanges = 0
    END IF
END SUB

SUB picks
    pick = INT(RND(1) * 4) + 1
END SUB

SUB etry1
    DO
        background
        INPUT "Color # 1 Background [From 0 to  7] : ", color1background
        INPUT "Color # 1 Foreground [From 0 to 15] : ", color1foreground
        PRINT
        COLOR color1foreground, color1background
        PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
        PRINT
        PRINT "Is This acceptable [Y]es or [N]o : "
        answer$ = INPUT$(1)
        answer$ = UCASE$(answer$)
    LOOP WHILE answer$ = "N"
END SUB

SUB background
    CLS
    PRINT
    PRINT "Please enter the color of your Background "
    PRINT
    PRINT "Black        is color code 0 "
    PRINT "Blue         is color code 1 "
    PRINT "Green        is color code 2 " ' ask for and show the various colors of
    PRINT "Cyan         is color code 3 " ' background codes.
    PRINT "Red          is color code 4 "
    PRINT "Magenta      is color code 5 "
    PRINT "Brown        is color code 6 "
    PRINT "White        is color code 7 "
    IF display = "syn" OR display = "ansi" THEN
        PRINT
        PRINT "Don't worry, the program will translate from decimal to hex for this."
        PRINT "Make your Background selection from [0] to [7] for Syncronet and Ansi."
        PRINT "And [0] to [15] for everything else"
        PRINT
    ELSE
        PRINT "Black        is color code 8 "
        PRINT "Blue         is color code 9 "
        PRINT "Green        is color code 10"
        PRINT "Cyan         is color code 11"
        PRINT "Red          is color code 12"
        PRINT "Magenta      is color code 13"
        PRINT "Yellow       is color code 14"
        PRINT "White        is color code 15"
        PRINT
    END IF
END SUB

SUB create
    CLS
    PRINT
    PRINT "Do you wish to save a configuration file for later on [Y] or [N] : "
    answer$ = UCASE$(answer$)
    IF answer$ = "N" THEN inputscreen
    PRINT
    PRINT "Enter Data for program."
    PRINT
    INPUT "Enter Filename [No extender :  ", FILES$
    PRINT
    FILES$ = FILES$ + ".dat"
    filename$ = "c:\display\data\" + FILES$
    PRINT "Saving data............"
    PRINT
    OPEN filename$ FOR OUTPUT AS #4
    PRINT #4, usercolorentry_g1
    PRINT #4, usercolorentry_g1
    PRINT #4, usercolorentry_g2
    PRINT #4, usercolorentry_g2
    PRINT #4, color1background
    PRINT #4, color1foreground
    PRINT #4, color2background
    PRINT #4, color2foreground
    PRINT #4, color3background
    PRINT #4, color3foreground
    PRINT #4, color4background
    PRINT #4, color4foreground
    PRINT #4, color5background
    PRINT #4, color5foreground
    PRINT #4, color6background
    PRINT #4, color6foreground
    PRINT #4, color7background
    PRINT #4, color7foreground
    PRINT #4, color8background
    PRINT #4, color8foreground
    CLOSE #4
END SUB

SUB back
    ' back1
    DO
        background
        INPUT "Outside Lines Background [From 0 to  7] : ", usercolorentry_g1
        INPUT "Outside Lines Foreground [From 0 to 15] : ", usercolorentry_t1
        PRINT
        COLOR usercolorentry_t1, usercolorentry_g1
        PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
        PRINT
        PRINT "Is This acceptable [Y]es or [N]o : "
        answer$ = INPUT$(1)
        answer$ = UCASE$(answer$)
    LOOP WHILE answer$ = "N"
    ' back2
    DO
        background
        INPUT "Brackets  Background [From 0 to  7] : ", usercolorentry_g2
        INPUT "Brackets  Foreground [From 0 to 15] : ", usercolorentry_t2
        PRINT
        PRINT
        COLOR usercolorentry_t2, usercolorentry_g2
        PRINT "[";: COLOR 7, 0: COLOR 7, 0
        PRINT "A";
        COLOR usercolorentry_t2, usercolorentry_g2
        PRINT "]";: COLOR 7, 0: COLOR 7, 0
        PRINT
        PRINT "Is This acceptable [Y]es or [N]o : "
        answer$ = INPUT$(1)
        answer$ = UCASE$(answer$)
    LOOP WHILE answer$ = "N"
END SUB

SUB etry2
    DO
        background
        INPUT "Color # 2 Background [From 0 to  7] : ", color2background
        INPUT "Color # 2 Foreground [From 0 to 15] : ", color2foreground
        PRINT
        COLOR color2foreground, color2background
        PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
        PRINT
        PRINT "Is This acceptable [Y]es or [N]o : "
        answer$ = INPUT$(1)
        answer$ = UCASE$(answer$)
    LOOP WHILE answer$ = "N"
END SUB

SUB etry8
    DO
        background
        INPUT "Color # 8 Background [From 0 to  7] : ", color8background
        INPUT "Color # 8 Foreground [From 0 to 15] : ", color8foreground
        PRINT
        COLOR color8foreground, color8background
        PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
        PRINT
        PRINT "Is This acceptable [Y]es or [N]o : "
        answer$ = INPUT$(1)
        answer$ = UCASE$(answer$)
    LOOP WHILE answer$ = "N"
END SUB

SUB etry7
    DO
        background
        INPUT "Color # 7 Background [From 0 to  7] : ", color7background
        INPUT "Color # 7 Foreground [From 0 to 15] : ", color7foreground
        PRINT
        COLOR color7foreground, color7background
        PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
        PRINT
        PRINT "Is This acceptable [Y]es or [N]o : "
        answer$ = INPUT$(1)
        answer$ = UCASE$(answer$)
    LOOP WHILE answer$ = "N"
END SUB

SUB displays
    DO
        CLS
        PRINT "Output display [A] Any Ansi BBS    [B] Wildcat 5-8 BBS  [C] PCboard BBS   "
        PRINT "Output Display [D] Synchronet BBS  [E] Mystic BBS                       : " 'which BBS display the user wants
        displaychoice$ = INPUT$(1)
        displaychoice$ = UCASE$(displaychoice$)
        IF displaychoice$ = "A" THEN display = "ansi": Menu
        IF displaychoice$ = "B" THEN display = "wc8": Menu ' select display coding by what the user types in
        IF displaychoice$ = "C" THEN display = "pcb": Menu
        IF displaychoice$ = "D" THEN display = "synchro": Menu
        IF displaychoice$ = "E" THEN display = "mystic": Menu
    LOOP
END SUB

SUB etry3
    DO
        background
        INPUT "Color # 3 Background [From 0 to  7] : ", color3background
        INPUT "Color # 3 Foreground [From 0 to 15] : ", color3foreground
        PRINT
        COLOR color3foreground, color3background
        PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
        PRINT
        PRINT "Is This acceptable [Y]es or [N]o : "
        answer$ = INPUT$(1)
        answer$ = UCASE$(answer$)
    LOOP WHILE answer$ = "N"
END SUB

SUB etry4
    DO
        background
        INPUT "Color # 4 Background [From 0 to  7] : ", color4background
        INPUT "Color # 4 Foreground [From 0 to 15] : ", color4foreground
        PRINT
        COLOR color4foreground, color4background
        PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
        PRINT
        PRINT "Is This acceptable [Y]es or [N]o : "
        answer$ = INPUT$(1)
        answer$ = UCASE$(answer$)
    LOOP WHILE answer$ = "N"
END SUB

SUB etry5
    DO
        background
        INPUT "Color # 5 Background [From 0 to  7] : ", color5background
        INPUT "Color # 5 Foreground [From 0 to 15] : ", color5foreground
        PRINT
        COLOR color5foreground, color5background
        PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
        PRINT
        PRINT "Is This acceptable [Y]es or [N]o : "
        answer$ = INPUT$(1)
        answer$ = UCASE$(answer$)
    LOOP WHILE answer$ = "N"
END SUB

SUB etry6
    DO
        background
        INPUT "Color # 6 Background [From 0 to  7] : ", color6background
        INPUT "Color # 6 Foreground [From 0 to 15] : ", color6foreground
        PRINT
        COLOR color6foreground, color6background
        PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
        PRINT
        PRINT "Is This acceptable [Y]es or [N]o : "
        answer$ = INPUT$(1)
        answer$ = UCASE$(answer$)
    LOOP WHILE answer$ = "N"
END SUB

SUB inputscreen

    'CLEAR
    CLS
    PRINT "Demo files that are available are: "
    PRINT
    PRINT "In the Menu selector"
    PRINT
    PRINT "    inet5     inet6     inet9"
    PRINT "    msg1      msg5      msg6     msg9"
    PRINT "    main1     main5     main6    main9"
    PRINT "    file1     file5     file6    file9"
    PRINT "    sysop8    sysop9" '                  display demo files that are available
    PRINT
    PRINT "In the Disp selector"
    PRINT
    PRINT "    prelog goodbye"
    PRINT
    PRINT "More are coming soon. Custom files are up to you to upload and then use"
    PRINT
    PRINT "Filename : [Do not put in the extender ]"
    INPUT "           [Enter defaults to main5    ] : ", file ' ask user for filename

END SUB

SUB filen
    PRINT
    INPUT "Filename  [Please no extenders I will put them in ] : ", filename$
    filename$ = "c:\display\data\" + filename$ + ".txt"
    IF database$ = "L" THEN
        load
    END IF
END SUB

SUB combos
    CLS
    PRINT
    PRINT "Please enter 8 different color combinations for the display , be creative : "
    PRINT "the program will pick randomly from those colors or will use them 1 at a time,"
    PRINT "depending on what you choose, either you will use the colors one at a time or "
    PRINT "you will use the same colors randomly. This can be fun to do and interesting."
    PRINT "depending on what you choose."
    PRINT
    PRINT "Press any key...."
    WaitAKey
END SUB

SUB specialcharacters3
    IF b = CHR$(94) OR b = CHR$(43) OR b = CHR$(37) OR b = CHR$(126) OR b = CHR$(45) OR b = CHR$(38) OR b = CHR$(35) OR b = CHR$(42) OR b = CHR$(33) OR b = CHR$(61) THEN
        flag$ = "Y1"
    ELSEIF b = CHR$(91) OR b = CHR$(93) THEN
        flag$ = "Y2"
    ELSE
        flag$ = "Y3"
    END IF
END SUB

SUB specialcharacters2
    IF b = CHR$(94) THEN b = corner1
    IF b = CHR$(43) THEN b = corner2
    IF b = CHR$(37) THEN b = corner3
    IF b = CHR$(126) THEN b = corner4
    IF b = CHR$(45) THEN
        b = lineconnector
    END IF
    IF b = CHR$(38) THEN
        b = connector1
    END IF

    IF b = CHR$(61) THEN
        b = connector2
    END IF
    IF b = CHR$(35) THEN
        b = connector3
    END IF
    IF b = CHR$(42) THEN
        b = connector4
    END IF
    IF b = CHR$(33) THEN
        b = verticalline
    END IF
END SUB

SUB selector
    IF pick = 1 THEN
        horizontalines = "double": verticalines$ = "double"
        corner1 = CHR$(201)
        corner2 = CHR$(187) ' corner character for new corners of new box output
        corner3 = CHR$(188)
        corner4 = CHR$(200)
        connector1 = CHR$(203)
        connector2 = CHR$(185) 'various connectors for lines in boxes
        connector3 = CHR$(202)

        connector4 = CHR$(204)
        lineconnector = CHR$(205) ' Horizontal line character for new box output
        verticalline = CHR$(186)
    END IF
    IF pick = 2 THEN
        horizontalines = "double": verticalines = "single"
        corner1 = CHR$(213)
        corner2 = CHR$(184) ' corner character for new corners of new box output
        corner3 = CHR$(190)
        corner4 = CHR$(212)
        connector1 = CHR$(209)
        connector2 = CHR$(181) 'various connectors for lines in boxes
        connector3 = CHR$(207)
        connector4 = CHR$(198)
        lineconnector = CHR$(205) ' Horizontal line character for new box output
        verticalline = CHR$(179)
    END IF
    IF pick = 3 THEN
        horizontalines = "single": verticalines = "double"
        corner1 = CHR$(214)
        corner2 = CHR$(183) ' corner character for new corners of new box output
        corner3 = CHR$(189)
        corner4 = CHR$(211)
        connector1 = CHR$(210)
        connector2 = CHR$(182) 'various connectors for lines in boxes
        connector3 = CHR$(208)
        connector4 = CHR$(199)
        lineconnector = CHR$(196) ' Horizontal line character for new box output
        verticalline = CHR$(186)

    END IF
    IF pick = 4 THEN
        horizontalines = "single": verticalines = "single"
        corner1 = CHR$(218)
        corner2 = CHR$(191) ' corner character for new corners of new box output
        corner3 = CHR$(217)
        corner4 = CHR$(192)
        connector1 = CHR$(194)
        connector2 = CHR$(180) 'various connectors for lines in boxes
        connector3 = CHR$(193)
        connector4 = CHR$(195)
        lineconnector = CHR$(196) ' Horizontal line character for new box output
        verticalline = CHR$(179)
    END IF
END SUB

SUB Menu
    DIM answer AS STRING
    DO
        PRINT "[M]enu file or [D]isplay file : "
        answer$ = INPUT$(1)
        answer = UCASE$(answer) ' ask the user what type of display they want, display file
        menuchoice = answer ' or menu file , gives two choices
        IF menuchoice = "M" OR menuchoice = "D" OR menuchoice = "" THEN EXIT DO
    LOOP
    fileit:
    IF file = "" THEN
        IF menuchoice = "D" THEN ' default file for display files
            file = "goodbye"
        ELSEIF menuchoice = "M" THEN ' default file for menu files
            file = "main5"
        END IF
    END IF
END SUB
SUB randomchoices
    RANDOMIZE TIMER
    trulyrandom:
    g1 = INT(RND(1) * 7) + 1
    t1 = INT(RND(1) * 15) + 1
    g2 = INT(RND(1) * 7) + 1
    t2 = INT(RND(1) * 15) + 1
    g = INT(RND(1) * 7) + 1
    t = INT(RND(1) * 15) + 1
    IF g = t THEN GOTO trulyrandom
    IF g1 = t1 THEN GOTO trulyrandom
    IF g2 = t2 THEN GOTO trulyrandom
    IF g = g1 OR g = g2 THEN GOTO trulyrandom
    IF t = t1 OR t = t2 THEN GOTO trulyrandom
END SUB

