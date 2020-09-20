DIM x AS INTEGER ' x is used in my program as a variable for the background color
DIM y AS INTEGER ' y is used as a variable for the foreground color
DIM i AS INTEGER ' i is used as a variable for the length of the line of display
DIM b AS STRING * 1
' above variables are used for the reading and writing of each character of info
' in each line of the ascii screens that are read into an array.

DIM lnumber AS STRING ' used as a variable for the line number of the ascii screen
DIM file AS STRING ' used as a variable for the filename entered in the program
DIM answer AS STRING ' used as a variable for the answer to a question
DIM asciidisp AS STRING ' ascii display variable used as the ascii display path
DIM ansidisp AS STRING ' ansi display variable used as the ansi path and filename
DIM wc8disp AS STRING 'Winserver 8 variable used as the wc8 BBS display path
DIM pcbdisp AS STRING 'Pc Board display variable used as the pcb display path
DIM asciipromptmenu AS STRING
DIM syndisp AS STRING 'Syncronet display variabe used as the Syncronet display path
DIM asciimenu AS STRING 'ascii menu variable used as the ascii menu path for input
DIM ansimenu AS STRING 'ansi menu variable used as the ansi path for output
DIM wc8menu AS STRING 'Winserver 8 variable used as the wc8 menu bbs path for output
DIM pcbmenu AS STRING 'PC Board variable used as the PcB BBS menu display for output
DIM synmenu AS STRING 'Syncronet variable used as the Syncronet Menu Display for output
DIM mysmenu AS STRING 'Mystic variable
DIM mysdisp AS STRING 'Mystic variable
DIM filess AS STRING
DIM fnames AS STRING
'above variables are all string variables that are used for storing different information that in the
'Wildcat, Winserver, Ascii, and Ansi screens that are either read in , or written out to different filenames
'in my program. The *120, or *16 etc are just string lengths.

DIM file1 AS STRING ' file1 is the ascii input file for the displays or menus that either other people or I create
DIM file2 AS STRING ' file2 is the ansi file created by he program from the ascii file above
DIM file3 AS STRING ' file3 is the Wildcator WInserver .BBS file that is created when the user selects wc8
DIM file4 AS STRING ' file4 is the PCBoard .BBS file created when the user selects pcb as a display
DIM file5 AS STRING ' file5 is reserved for Synchronet BBS when the user selects syncronet as a display
DIM file6 AS STRING ' file6 is reserved for Mystic BBS in the user selects mystic as a display
DIM file7 AS STRING ' file7 is reserved for the prompt files that follow any menu in any BBS

'ALL of the above are actual filenames without the drive letter and path for my program.


DIM horizontalines AS STRING
DIM verticalines AS STRING
DIM foregroundcoloransi AS STRING
DIM backgroundcoloransi AS STRING
DIM foregroundcolorbbs AS STRING
DIM backgroundcolorbbs AS STRING ' Background and foreground string settings
DIM foregroundcolorpcb AS STRING
DIM backgroundcolorpcb AS STRING
DIM foregroundcolorsynchro AS STRING
DIM backgroundcolorsynchro AS STRING
DIM foregroundcolormystic AS STRING
DIM backgroundcolormystic AS STRING

DIM code0b AS STRING ' Code Setup for Black   Background
DIM code0f AS STRING ' Code Setup for Black   Foreground
DIM code1b AS STRING ' Code Setup for Blue    Background
DIM code1f AS STRING ' Code Setup for Blue    Foreground
DIM code2b AS STRING ' Code Setup for Green   Background
DIM code2f AS STRING ' Code Setup for Green   Foreground
DIM code3b AS STRING ' Code Setup for Cyan    Background
DIM code3f AS STRING ' Code Setup for Cyan    Foreground
DIM code4b AS STRING ' Code Setup for Red     Background
DIM code4f AS STRING ' Code Setup for Red     Foreground
DIM code5b AS STRING ' Code Setup for Magenta Background
DIM code5f AS STRING ' Code Setup for Magenta Foreground
DIM code6b AS STRING ' Code Setup for Brown   Background
DIM code6f AS STRING ' Code Setup for Yellow  Foreground
DIM code7b AS STRING ' Code Setup for White   Background
DIM code7f AS STRING ' Code Setup for White   Foreground
DIM corner1 AS STRING
DIM corner2 AS STRING
DIM corner3 AS STRING
DIM corner4 AS STRING
DIM connector1 AS STRING
DIM connector2 AS STRING
DIM connector3 AS STRING
DIM connector4 AS STRING
DIM toplineconnector AS STRING
DIM middlelineconnector AS STRING
DIM bottomlineconnector AS STRING
DIM menuchoice AS STRING
DIM flag AS STRING
DIM leftverticalline AS STRING
DIM rightverticalline AS STRING
DIM display AS STRING
DIM ran AS STRING
DIM randome AS STRING
_FULLSCREEN
CLS
SCREEN 12
COLOR 12, 1

LOCATE 10, 24
PRINT "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
LOCATE 11, 24
PRINT "%                 BBS Screen Creator Written By Russ Campbell                  %"
LOCATE 12, 24
PRINT "%                                                                              %"
LOCATE 13, 24
PRINT "%           ***       Alpha Version 18 Release Candidate RC8      ***          %"
LOCATE 14, 24
PRINT "%                                                                              %"
LOCATE 15, 24
PRINT "%                All parts of the program are: fully functional                %"
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
DO WHILE INKEY$ = ""
LOOP


COLOR 7, 0
setup:
CLS
PRINT
PRINT "BBS Menu Color Scheme Creator Version 18 Alpha Version RC8"
PRINT "Written by Russ Campbell Updated September 15 2020 (c) 2020"
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
DO WHILE INKEY$ = ""
LOOP
CLS
PRINT
PRINT "The menus have changed , see below for instuctions"
PRINT "Especiallly the symbols for Top, Middle and Bottom"
PRINT "Lines as well as Vertical left and right lines    "
PRINT "This gives me more control over screen making.    "
PRINT
PRINT " ^{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{$"
PRINT " !               Main Message Menu                 <"
PRINT " *}}}}}}}}}}}}}}}}}}}<}}}}}}}}}}}}}}}}}}}}}}}}}}}}}="
PRINT " !                   <                             <"
PRINT " *}}}}}}}}}}}}}}}}}}}#}}}}}}}}}}}}}}}}}}}}}}}}}}}}}="
PRINT " ! [C] Check Your Mail   [S] Scan Messages         <"
PRINT " ! [E] Enter New Message [J] Join Conference       <"
PRINT " ! [F] FILE MENU         [U] Update Settings       <"
PRINT " ! [G] Goodbye           [H] Help Level            <"
PRINT " ! [Q] Quit to Main Menu [?] Command Help          <"
PRINT " ! [R] Read Messages                               <"
PRINT " ~>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>%"
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
DO WHILE INKEY$ = ""
LOOP
CLS
PRINT
rand:
CLS
PRINT
PRINT "Do you wish to have multi colors randomly chosen for everything"
PRINT "or user set colors for the entire area that makes up all of the "
PRINT "the menus. This can all be used in either displays or menus"
PRINT
PRINT "Choose [R]andom or [U]ser Set Patterns or"
PRINT "[F]ixed Display Screens [L]oad Data File or E[X]it Program : "
answer = UCASE$(INPUT$(1))

ran = answer
IF ran = "R" THEN GOSUB randomchoices: GOTO inputscreen
IF ran = "F" THEN GOSUB fixedcolors: GOTO inputscreen
IF ran = "L" THEN GOSUB load: GOTO inputscreen
IF ran = "U" THEN GOTO inputscreen1
IF ran = "X" THEN GOTO finish
GOTO rand
inputscreen1:
CLS
PRINT "Do you wish to use pre-selected colors or use your own sets "
PRINT "Of color selections: color schemes can be customised."

PRINT "This section is for entering your own color schemes , it is  "
PRINT "for selecting either Preselected or your own colors."
PRINT
PRINT "If you want random colors, choose [Y]es as your answer."
PRINT
PRINT "However if you want to enter your own colors , then enter"
PRINT " N as your choice of color selection"
PRINT
PRINT "Do you wish to use random colors? [Y]es  or [N]o  [Defaults to [N]o   "
answer = UCASE$(INPUT$(1))

IF answer = "N" THEN randome = "N"
IF answer = "" THEN randome = "N": GOTO prompts
IF answer = "N" THEN GOTO prompts
IF answer = "Y" THEN GOSUB randomchoices: GOTO inputscreen
GOTO inputscreen1
prompts:
CLS
PRINT
PRINT "This section will either load or create a new user data file with all used"
PRINT "variables saved in a filename that you entered. It will give you the option"
PRINT "of loading a saved file or creating a totally new filename. You do not have"
PRINT "to write over any existing files, and the program will be fool proof."
PRINT
PRINT "[L]oad or [C]reate User Data file or [X]xit Program"
answer = UCASE$(INPUT$(1))

IF answer = "L" THEN GOTO filen
IF answer = "C" THEN GOTO combos
IF answer = "X" THEN GOTO finish
GOTO prompts
filen:
PRINT
IF answer = "L" THEN
    GOSUB load
    GOTO inputscreen
END IF
combos:
CLS
PRINT
PRINT "Please enter 6 different color combinations for the display , be creative : "
PRINT "the program will use the colors that you have selected as output for your,"
PRINT "display based on what you choose, you will be given the option to load "
PRINT "and save yout colors every time you run the program."
PRINT
PRINT "Press any key...."
DO WHILE INKEY$ = ""
LOOP
GOSUB back
GOSUB create
GOTO inputscreen
back:
back1a:
GOSUB background
INPUT "Outside Lines Background 1a [From 0 to  7] : ", g1a
INPUT "Outside Lines Foreground 1a [From 0 to 15] : ", t1a
PRINT
COLOR t1a, g1a

PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
PRINT
PRINT "Is This readable [Y]es or [N]o : "
answer = INPUT$(1)
answer = UCASE$(answer)
IF answer = "N" THEN GOTO back1a
IF answer = "Y" THEN GOTO back1b
GOTO back1a
back1b:
GOSUB background
INPUT "Outside Lines Background 1b [From 0 to  7] : ", g1b
INPUT "Outside Lines Foreground 1b [From 0 to 15] : ", t1b
PRINT
COLOR t1b, g1b

PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
PRINT
PRINT "Is This readable [Y]es or [N]o : "
answer = INPUT$(1)
answer = UCASE$(answer)
IF answer = "N" THEN GOTO back1b
IF answer = "Y" THEN GOTO back1c
GOTO back1b

back1c:
GOSUB background
INPUT "Outside Lines Background 1c [From 0 to  7] : ", g1c
INPUT "Outside Lines Foreground 1c [From 0 to 15] : ", t1c
PRINT
COLOR t1c, g1c

PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
PRINT
PRINT "Is This readable [Y]es or [N]o : "
answer = INPUT$(1)
answer = UCASE$(answer)
IF answer = "N" THEN GOTO back1c
IF answer = "Y" THEN GOTO back1d
GOTO back1c

back1d:
GOSUB background
INPUT "Outside Lines Background 1d [From 0 to  7] : ", g1d
INPUT "Outside Lines Foreground 1d [From 0 to 15] : ", t1d
PRINT
COLOR t1d, g1d

PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
PRINT
PRINT "Is This readable [Y]es or [N]o : "
answer = INPUT$(1)
answer = UCASE$(answer)
IF answer = "N" THEN GOTO back1d
IF answer = "Y" THEN GOTO back2
GOTO back1d

back2:
GOSUB background
INPUT "Brackets  Background [From 0 to  7] : ", g2
INPUT "Brackets  Foreground [From 0 to 15] : ", t2
PRINT
PRINT
COLOR t2, g2
PRINT "[";: COLOR 7, 0: COLOR 7, 0
PRINT "A";
COLOR t2, g2
PRINT "]";: COLOR 7, 0: COLOR 7, 0
PRINT
PRINT "Is This readable [Y]es or [N]o : "
answer = UCASE$(INPUT$(1))
IF answer = "N" THEN GOTO back2
IF answer = "Y" THEN GOTO back3a
GOTO back2

back3a:
GOSUB background
INPUT "Inside Lines Background  [From 0 to  7] : ", g
INPUT "Inside Lines Foreground  [From 0 to 15] : ", t
PRINT
COLOR t, g

PRINT "[------------------]";: COLOR 7, 0: COLOR 7, 0
PRINT
PRINT "Is This readable [Y]es or [N]o : "
answer = UCASE$(INPUT$(1))
IF answer = "N" THEN GOTO back3a
IF answer = "Y" THEN GOTO fnd

GOTO back3a
fnd:
RETURN

inputscreen:


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
IF file = "a" OR file = "A" THEN GOTO inputscreen
IF file = "b" OR file = "B" THEN GOTO inputscreen
IF file = "c" OR file = "C" THEN GOTO inputscreen
IF file = "d" OR file = "D" THEN GOTO inputscreen
IF file = "e" OR file = "E" THEN GOTO inputscreen
displays:
CLS
PRINT "Output display [A] Any Ansi BBS    [B] Wildcat 5-8 BBS  [C] PCboard BBS   "
PRINT "Output Display [D] Synchronet BBS  [E] Mystic BBS                       : " 'which BBS display the user wants
answer = UCASE$(INPUT$(1))
IF answer = "A" THEN display = "ansi": GOTO menu
IF answer = "B" THEN display = "wc8": GOTO menu ' select display coding by what the user types in
IF answer = "C" THEN display = "pcb": GOTO menu
IF answer = "D" THEN display = "synchro": GOTO menu
IF answer = "E" THEN display = "mystic": GOTO menu
GOTO displays
startit:

selector:
IF pick = 1 THEN
    horizontalines = "double": verticalines = "double"
    corner1 = CHR$(201)
    corner2 = CHR$(187) ' corner character for new corners of new box output
    corner3 = CHR$(188)
    corner4 = CHR$(200)
    connector1 = CHR$(203)
    connector2 = CHR$(185) 'various connectors for lines in boxes
    connector3 = CHR$(202)

    connector4 = CHR$(204)
    toplineconnector = CHR$(205) ' Horizontal line character for new box output
    middlelineconnector = CHR$(205)
    bottomlineconnector = CHR$(205)
    rightverticalline = CHR$(186)
    leftverticalline = CHR$(186)
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
    toplineconnector = CHR$(205) ' Horizontal line character for new box output
    middlelineconnector = CHR$(205)
    bottomlineconnector = CHR$(205)
    rightverticalline = CHR$(179)
    leftverticalline = CHR$(179)
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
    toplineconnector = CHR$(196) ' Horizontal line character for new box output
    middlelineconnector = CHR$(196) ' Horizontal line character for new box output
    bottomlineconnector = CHR$(196) ' Horizontal line character for new box output
    rightverticalline = CHR$(186)
    leftverticalline = CHR$(186)

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
    toplineconnector = CHR$(196) ' Horizontal line character for new box output
    middlelineconnector = CHR$(196) ' Horizontal line character for new box output
    bottomlineconnector = CHR$(196) ' Horizontal line character for new box output
    rightverticalline = CHR$(179)
    leftverticalline = CHR$(179)
END IF
RETURN

RETURN


'----------------------------------------------------------------------------------------------------------------------------





menu:
PRINT "[M]enu file or [D]isplay file : "
answer = UCASE$(INPUT$(1))
answer = UCASE$(answer) ' ask the user what type of display they want, display file
menuchoice = answer ' or menu file , gives two choices
IF menuchoice = "M" OR menuchoice = "D" OR menuchoice = "" THEN GOTO fileit
GOTO menu
fileit:
IF file = "" AND menuchoice = "D" THEN ' default file for display files
    file = "goodbye"
END IF
IF file = "" AND menuchoice = "M" THEN ' default file for menu files
    file = "main5"
END IF




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
    GOSUB pick0
    GOSUB selector
    ON ERROR GOTO errorhandle
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
    GOSUB pick0
    GOSUB selector
    ON ERROR GOTO errorhandle
    OPEN asciimenu FOR INPUT AS #1
    ON ERROR GOTO errorhandle
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
    LINE INPUT #1, lnumber

    FOR i = 1 TO LEN(lnumber)

        b = MID$(lnumber, i, 1) ' Calculates the ascii value of every character in the line
        IF display = "ansi" THEN
            GOSUB specialcharacters3
            GOSUB specialcharacters2
            IF flag = "Y1" THEN
                IF b1 = 1 THEN
                    GOSUB colorchange
                    COLOR t1a, g1a
                    x = g1a
                    y = t1a
                    GOSUB displ
                    IF colorchanges = 1 THEN
                        PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                        PRINT b;
                    ELSEIF colorchanges = 0 THEN
                        PRINT #2, b;
                        PRINT b;
                    END IF

                ELSE IF b1 = 2 THEN
                        GOSUB colorchange
                        COLOR t1b, g1b
                        x = t1b
                        y = g1b
                        GOSUB displ
                        IF colorchanges = 1 THEN
                            PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                            PRINT b;
                        ELSEIF colorchanges = 0 THEN
                            PRINT #2, b;
                            PRINT b;
                        END IF

                    ELSEIF b1 = 3 THEN
                        GOSUB colorchange
                        COLOR t1c, g1c
                        x = g1c
                        y = t1c
                        GOSUB displ
                        IF colorchanges = 1 THEN
                            PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                            PRINT b;
                        ELSEIF colorchanges = 0 THEN
                            PRINT #2, b;
                            PRINT b;
                        END IF

                    ELSEIF b1 = 4 THEN
                        GOSUB colorchange
                        COLOR t1d, g1d
                        x = g1d
                        y = t1d
                        GOSUB displ
                        IF colorchanges = 1 THEN
                            PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                            PRINT b;
                        ELSEIF colorchanges = 0 THEN
                            PRINT #2, b;
                            PRINT b;
                        END IF
                    END IF
                END IF
            END IF

            IF flag$ = "Y2" THEN
                GOSUB colorchange
                COLOR t2, g2
                x = g2
                y = t2
                GOSUB displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                    PRINT b;
                ELSEIF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
            IF flag = "Y3" THEN
                GOSUB colorchange
                COLOR t, g
                x = g
                y = t
                GOSUB displ
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
            GOSUB specialcharacters3
            GOSUB specialcharacters2
            IF flag = "Y1" THEN
                IF b1 = 1 THEN
                    GOSUB colorchange
                    COLOR t1a, g1a
                    x = g1a
                    y = t1a
                    GOSUB displ
                    IF colorchanges = 1 THEN
                        PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                        PRINT b;
                    ELSEIF colorchanges = 0 THEN
                        PRINT #2, b;
                        PRINT b;
                    END IF

                ELSE IF b1 = 2 THEN
                        GOSUB colorchange
                        COLOR t1b, g1b
                        x = t1b
                        y = g1b
                        GOSUB displ
                        IF colorchanges = 1 THEN
                            PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                            PRINT b;
                        ELSEIF colorchanges = 0 THEN
                            PRINT #2, b;
                            PRINT b;
                        END IF

                    ELSEIF b1 = 3 THEN
                        GOSUB colorchange
                        COLOR t1c, g1c
                        x = g1c
                        y = t1c
                        GOSUB displ
                        IF colorchanges = 1 THEN
                            PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                            PRINT b;
                        ELSEIF colorchanges = 0 THEN
                            PRINT #2, b;
                            PRINT b;
                        END IF

                    ELSEIF b1 = 4 THEN
                        GOSUB colorchange
                        COLOR t1d, g1d
                        x = g1d
                        y = t1d
                        GOSUB displ
                        IF colorchanges = 1 THEN
                            PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                            PRINT b;
                        ELSEIF colorchanges = 0 THEN
                            PRINT #2, b;
                            PRINT b;
                        END IF
                    END IF
                END IF
            END IF

            IF flag = "Y2" THEN
                GOSUB colorchange
                x = g2
                y = t2
                COLOR t2, g2
                GOSUB displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcolorbbs + foregroundcolorbbs + b;
                    PRINT b;
                ELSEIF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
            IF flag = "Y3" THEN
                GOSUB colorchange
                x = g
                y = t
                COLOR t, g
                GOSUB displ
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
            GOSUB specialcharacters3
            GOSUB specialcharacters2
            IF flag = "Y1" THEN
                IF b1 = 1 THEN
                    GOSUB colorchange
                    COLOR t1a, g1a
                    x = g1a
                    y = t1a
                    GOSUB displ
                    IF colorchanges = 1 THEN
                        PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                        PRINT b;
                    ELSEIF colorchanges = 0 THEN
                        PRINT #2, b;
                        PRINT b;
                    END IF

                ELSE IF b1 = 2 THEN
                        GOSUB colorchange
                        COLOR t1b, g1b
                        x = t1b
                        y = g1b
                        GOSUB displ
                        IF colorchanges = 1 THEN
                            PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                            PRINT b;
                        ELSEIF colorchanges = 0 THEN
                            PRINT #2, b;
                            PRINT b;
                        END IF

                    ELSEIF b1 = 3 THEN
                        GOSUB colorchange
                        COLOR t1c, g1c
                        x = g1c
                        y = t1c
                        GOSUB displ
                        IF colorchanges = 1 THEN
                            PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                            PRINT b;
                        ELSEIF colorchanges = 0 THEN
                            PRINT #2, b;
                            PRINT b;
                        END IF

                    ELSEIF b1 = 4 THEN
                        GOSUB colorchange
                        COLOR t1d, g1d
                        x = g1d
                        y = t1d
                        GOSUB displ
                        IF colorchanges = 1 THEN
                            PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                            PRINT b;
                        ELSEIF colorchanges = 0 THEN
                            PRINT #2, b;
                            PRINT b;
                        END IF
                    END IF
                END IF
            END IF

            IF flag = "Y2" THEN
                GOSUB colorchange
                COLOR t2, g2
                x = g2
                y = t2
                GOSUB displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcolorpcb + foregroundcolorpcb + b;
                    PRINT b;
                ELSEIF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
            IF flag = "Y3" THEN
                GOSUB colorchange
                COLOR t, g
                x = g
                y = t
                GOSUB displ
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
            GOSUB specialcharacters3
            GOSUB specialcharacters2
            IF flag = "Y1" THEN
                IF b1 = 1 THEN
                    GOSUB colorchange
                    COLOR t1a, g1a
                    x = g1a
                    y = t1a
                    GOSUB displ
                    IF colorchanges = 1 THEN
                        PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                        PRINT b;
                    ELSEIF colorchanges = 0 THEN
                        PRINT #2, b;
                        PRINT b;
                    END IF

                ELSE IF b1 = 2 THEN
                        GOSUB colorchange
                        COLOR t1b, g1b
                        x = t1b
                        y = g1b
                        GOSUB displ
                        IF colorchanges = 1 THEN
                            PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                            PRINT b;
                        ELSEIF colorchanges = 0 THEN
                            PRINT #2, b;
                            PRINT b;
                        END IF

                    ELSEIF b1 = 3 THEN
                        GOSUB colorchange
                        COLOR t1c, g1c
                        x = g1c
                        y = t1c
                        GOSUB displ
                        IF colorchanges = 1 THEN
                            PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                            PRINT b;
                        ELSEIF colorchanges = 0 THEN
                            PRINT #2, b;
                            PRINT b;
                        END IF

                    ELSEIF b1 = 4 THEN
                        GOSUB colorchange
                        COLOR t1d, g1d
                        x = g1d
                        y = t1d
                        GOSUB displ
                        IF colorchanges = 1 THEN
                            PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                            PRINT b;
                        ELSEIF colorchanges = 0 THEN
                            PRINT #2, b;
                            PRINT b;
                        END IF
                    END IF
                END IF
            END IF

            IF flag = "Y2" THEN
                GOSUB colorchange
                COLOR t2, g2
                x = g2
                y = t2
                GOSUB displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcolorsynchro + foregroundcolorsynchro + b;
                    PRINT b;
                ELSEIF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
            IF flag = "Y3" THEN
                GOSUB colorchange
                COLOR t, g
                x = g
                y = t
                GOSUB displ
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
            GOSUB specialcharacters3
            GOSUB specialcharacters2
            IF flag = "Y1" THEN
                IF b1 = 1 THEN
                    GOSUB colorchange
                    COLOR t1a, g1a
                    x = g1a
                    y = t1a
                    GOSUB displ
                    IF colorchanges = 1 THEN
                        PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                        PRINT b;
                    ELSEIF colorchanges = 0 THEN
                        PRINT #2, b;
                        PRINT b;
                    END IF

                ELSE IF b1 = 2 THEN
                        GOSUB colorchange
                        COLOR t1b, g1b
                        x = t1b
                        y = g1b
                        GOSUB displ
                        IF colorchanges = 1 THEN
                            PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                            PRINT b;
                        ELSEIF colorchanges = 0 THEN
                            PRINT #2, b;
                            PRINT b;
                        END IF

                    ELSEIF b1 = 3 THEN
                        GOSUB colorchange
                        COLOR t1c, g1c
                        x = g1c
                        y = t1c
                        GOSUB displ
                        IF colorchanges = 1 THEN
                            PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                            PRINT b;
                        ELSEIF colorchanges = 0 THEN
                            PRINT #2, b;
                            PRINT b;
                        END IF

                    ELSEIF b1 = 4 THEN
                        GOSUB colorchange
                        COLOR t1d, g1d
                        x = g1d
                        y = t1d
                        GOSUB displ
                        IF colorchanges = 1 THEN
                            PRINT #2, backgroundcoloransi + foregroundcoloransi + b;
                            PRINT b;
                        ELSEIF colorchanges = 0 THEN
                            PRINT #2, b;
                            PRINT b;
                        END IF
                    END IF
                END IF
            END IF

            IF flag = "Y2" THEN
                GOSUB colorchange
                COLOR t2, g2
                x = g2
                y = t2
                GOSUB displ
                IF colorchanges = 1 THEN
                    PRINT #2, backgroundcolormystic + foregroundcolormystic + b;
                    PRINT b;
                ELSEIF colorchanges = 0 THEN
                    PRINT #2, b;
                    PRINT b;
                END IF
            END IF
            IF flag = "Y3" THEN
                GOSUB colorchange
                COLOR t, g
                x = g
                y = t
                GOSUB displ
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
    b8a = 0
    b9a = 0
    b10a = 0
    b11a = 0
    PRINT #2, ""
    PRINT
LOOP
fini:
b1a = 0
b2a = 0
b3a = 0
b4a = 0
b5a = 0
b6a = 0
b7a = 0
b8a = 0
b9a = 0
b10a = 0
b11a = 0
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
answer = UCASE$(INPUT$(1))
IF answer = "N" THEN
    GOTO finish
ELSE
    contlne = 0
    CLEAR
    GOTO rand

END IF

finish:
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
PRINT "Thank you for using BBS Menu Color Scheme Creator 18 RC8"
PRINT
END
pick0:
RANDOMIZE TIMER
pick = INT(RND(1) * 4) + 1
RETURN
displ:
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
    IF x = 0 THEN backgroundcolorpcb = "@X0"
    IF x = 1 THEN backgroundcolorpcb = "@X1"
    IF x = 2 THEN backgroundcolorpcb = "@X2"
    IF x = 3 THEN backgroundcolorpcb = "@X3"
    IF x = 4 THEN backgroundcolorpcb = "@X4"
    IF x = 5 THEN backgroundcolorpcb = "@X5"
    IF x = 6 THEN backgroundcolorpcb = "@X6"
    IF x = 7 THEN backgroundcolorpcb = "@X7"
    IF x = 8 THEN backgroundcolorpcb = "@X8"
    IF x = 9 THEN backgroundcolorpcb = "@X9"
    IF x = 10 THEN backgroundcolorpcb = "@XA"
    IF x = 11 THEN backgroundcolorpcb = "@XB"
    IF x = 12 THEN backgroundcolorpcb = "@XC"
    IF x = 13 THEN backgroundcolorpcb = "@XD"
    IF x = 14 THEN backgroundcolorpcb = "@XE"
    IF x = 15 THEN backgroundcolorpcb = "@XF"
    IF y = 0 THEN foregroundcolorpcb = "0@"
    IF y = 1 THEN foregroundcolorpcb = "1@"
    IF y = 2 THEN foregroundcolorpcb = "2@"
    IF y = 3 THEN foregroundcolorpcb = "3@"
    IF y = 4 THEN foregroundcolorpcb = "4@"
    IF y = 5 THEN foregroundcolorpcb = "5@"
    IF y = 6 THEN foregroundcolorpcb = "6@"
    IF y = 7 THEN foregroundcolorpcb = "7@"
    IF y = 8 THEN foregroundcolorpcb = "8@"
    IF y = 9 THEN foregroundcolorpcb = "9@"
    IF y = 10 THEN foregroundcolorpcb = "A@"
    IF y = 11 THEN foregroundcolorpcb = "B@"
    IF y = 12 THEN foregroundcolorpcb = "C@"
    IF y = 13 THEN foregroundcolorpcb = "D@"
    IF y = 14 THEN foregroundcolorpcb = "E@"
    IF y = 15 THEN foregroundcolorpcb = "F@"

END IF
IF display = "synchro" THEN
    IF x = 0 THEN backgroundcolorsynchro = CHR$(1) + "0"
    IF x = 1 THEN backgroundcolorsynchro = CHR$(1) + "1"
    IF x = 2 THEN backgroundcolorsynchro = CHR$(1) + "2"
    IF x = 3 THEN backgroundcolorsynchro = CHR$(1) + "3"
    IF x = 4 THEN backgroundcolorsynchro = CHR$(1) + "4"
    IF x = 5 THEN backgroundcolorsynchro = CHR$(1) + "5"
    IF x = 6 THEN backgroundcolorsynchro = CHR$(1) + "6"
    IF x = 7 THEN backgroundcolorsynchro = CHR$(1) + "7"
    IF y = 0 THEN foregroundcolorsynchro = "k"
    IF y = 1 THEN foregroundcolorsynchro = "b"
    IF y = 2 THEN foregroundcolorsynchro = "g"
    IF y = 3 THEN foregroundcolorsynchro = "c"
    IF y = 4 THEN foregroundcolorsynchro = "r"
    IF y = 5 THEN foregroundcolorsynchro = "m"
    IF y = 6 THEN foregroundcolorsynchro = "y"
    IF y = 7 THEN foregroundcolorsynchro = "w"


END IF
IF display = "mystic" THEN
    IF x = 0 THEN backgroundcolormystic = CHR$(254) + "16"
    IF x = 1 THEN backgroundcolormystic = CHR$(254) + "17"
    IF x = 2 THEN backgroundcolormystic = CHR$(254) + "18"
    IF x = 3 THEN backgroundcolormystic = CHR$(254) + "19"
    IF x = 4 THEN backgroundcolormystic = CHR$(254) + "20"
    IF x = 5 THEN backgroundcolormystic = CHR$(254) + "21"
    IF x = 6 THEN backgroundcolormystic = CHR$(254) + "22"
    IF x = 7 THEN backgroundcolormystic = CHR$(254) + "23"
    IF x = 8 THEN backgroundcolormystic = CHR$(254) + "24"
    IF x = 9 THEN backgroundcolormystic = CHR$(254) + "25"
    IF x = 10 THEN backgroundcolormystic = CHR$(254) + "26"
    IF x = 11 THEN backgroundcolormystic = CHR$(254) + "27"
    IF x = 12 THEN backgroundcolormystic = CHR$(254) + "28"
    IF x = 13 THEN backgroundcolormystic = CHR$(254) + "29"
    IF x = 14 THEN backgroundcolormystic = CHR$(254) + "30"
    IF x = 15 THEN backgroundcolormystic = CHR$(254) + "31"
    IF y = 0 THEN foregroundcolormystic = CHR$(254) + "00"
    IF y = 1 THEN foregroundcolormystic = CHR$(254) + "01"
    IF y = 2 THEN foregroundcolormystic = CHR$(254) + "02"
    IF y = 3 THEN foregroundcolormystic = CHR$(254) + "03"
    IF y = 4 THEN foregroundcolormystic = CHR$(254) + "04"
    IF y = 5 THEN foregroundcolormystic = CHR$(254) + "05"
    IF y = 6 THEN foregroundcolormystic = CHR$(254) + "06"
    IF y = 7 THEN foregroundcolormystic = CHR$(254) + "07"
    IF y = 8 THEN foregroundcolormystic = CHR$(254) + "08"
    IF y = 9 THEN foregroundcolormystic = CHR$(254) + "09"
    IF y = 10 THEN foregroundcolormystic = CHR$(254) + "10"
    IF y = 11 THEN foregroundcolormystic = CHR$(254) + "11"
    IF y = 12 THEN foregroundcolormystic = CHR$(254) + "12"
    IF y = 13 THEN foregroundcolormystic = CHR$(254) + "13"
    IF y = 14 THEN foregroundcolormystic = CHR$(254) + "14"
    IF y = 15 THEN foregroundcolormystic = CHR$(254) + "15"
END IF
RETURN
specialcharacters2:

IF b = CHR$(94) THEN
    b = corner1
    b1a = b1a + 1
    IF b1a = 1 THEN
        b1 = 1
    ELSEIF b1a = 1 AND b6a = 1 THEN
        b1 = 2
    ELSEIF b1a = 2 AND b6a = 2 THEN
        b1 = 3
    ELSEIF b1a = 3 AND b6a = 3 THEN
        b1 = 4

    END IF


END IF
IF b = CHR$(36) THEN
    b = corner2
    b2a = b2a + 1
    IF b2a = 1 THEN
        b1 = 1
    ELSEIF b2a = 1 AND b1a = 1 THEN
        b1 = 2
    ELSEIF b2a = 2 AND b1a = 2 THEN
        b1 = 3
    ELSEIF b2a = 3 AND b1a = 3 THEN
        b1 = 4

    END IF
END IF
IF b = CHR$(37) THEN
    b = corner3
    b3a = b3a + 1
    IF b3a = 1 THEN
        b1 = 1
    ELSEIF b3a = 1 AND b2a = 1 THEN
        b1 = 2
    ELSEIF b3a = 2 AND b2a = 2 THEN
        b1 = 3
    ELSEIF b3a = 3 AND b2a = 3 THEN
        b1 = 4
    END IF
END IF
IF b = CHR$(126) THEN
    b = corner4
    b4a = b4a + 1
    IF b4a = 1 THEN
        b1 = 1
    ELSEIF b4a = 1 AND b1a = 1 THEN
        b1 = 2
    ELSEIF b4a = 2 AND b1a = 2 THEN
        b1 = 3
    ELSEIF b4a = 3 AND b1a = 3 THEN
        b1 = 4
    END IF
END IF

IF b = CHR$(125) THEN
    b = middlelineconnector
    b1 = 1

END IF

IF b = CHR$(62) THEN
    b = bottomlineconnector
    b5a = b5a + 1
    IF b5a = 1 THEN
        b1 = 1
    ELSEIF b5a = 1 AND b3a = 1 THEN
        b1 = 2
    ELSEIF b5a = 2 AND b3a = 2 THEN
        b1 = 3
    ELSEIF b5a = 3 AND b3a = 3 THEN
        b1 = 4

    END IF
END IF

IF b = CHR$(123) THEN
    b = toplineconnector
    b6a = b6a + 1
    IF b6a = 1 THEN
        b1 = 1
    ELSEIF b6a = 1 AND b1a = 1 THEN
        b1 = 2
    ELSEIF b6a = 2 AND b1a = 2 THEN
        b1 = 3
    ELSEIF b6a = 3 AND b1a = 3 THEN
        b1 = 4
    END IF



END IF

IF b = CHR$(38) THEN
    b = connector1
    b8a = b8a + 1
    IF b8a = 1 THEN
        b1 = 1
    ELSEIF b8a = 2 AND b1a = 1 THEN
        b1 = 1
    ELSEIF b8a = 2 AND b1a = 2 THEN
        b1 = 2
    ELSEIF b8a = 3 AND b1a = 2 THEN
        b1 = 3
    ELSEIF b8a = 4 AND b1a = 3 THEN
        b1 = 4
    END IF



END IF
IF b = CHR$(61) THEN
    b = connector2
    b9a = b9a + 1
    IF b9a = 1 THEN
        b1 = 1
    ELSEIF b9a = 2 AND b4a = 1 THEN
        b1 = 2
    ELSEIF b9a = 3 AND b4a = 2 THEN
        b1 = 3
    ELSEIF b9a = 4 AND b4a = 3 THEN
        b1 = 4
    END IF

END IF
IF b = CHR$(35) THEN
    b = connector3
    b10a = b10a + 1
    IF b10a = 1 THEN
        b1 = 1
    ELSEIF b10a = 2 AND b1a = 1 THEN
        b10a = 1
    ELSEIF b10a = 2 AND b1a = 2 THEN
        b10a = 2
    ELSEIF b10a = 3 AND b1a = 2 THEN
        b1 = 3
    ELSEIF b10a = 4 AND b1a = 3 THEN
        b1 = 4
    END IF

END IF
IF b = CHR$(42) THEN
    b = connector4
    b11a = b11a + 1
    IF b11a = 1 THEN
        b1 = 1
    ELSEIF b11a = 2 AND b3a = 1 THEN
        b1 = 2
    ELSEIF b11a = 3 AND b3a = 2 THEN
        b1 = 3
    ELSEIF b11a = 4 AND b3a = 3 THEN
        b1 = 4
    END IF

END IF
IF b = CHR$(33) THEN
    b = leftverticalline
    b6a = b6a + 1
    IF b6a = 1 THEN
        b1 = 1
    ELSEIF b6a = 1 AND b1a = 1 THEN
        b1 = 2
    ELSEIF b6a = 2 AND b1a = 2 THEN
        b1 = 3
    ELSEIF b6a = 3 AND b1a = 3 THEN
        b1 = 4
    END IF
END IF
IF b = CHR$(60) THEN
    b = rightverticalline
    b7a = b7a + 1
    IF b7a = 1 THEN
        b1 = 1
    ELSEIF b7a = 1 AND b4a = 1 THEN
        b1 = 2
    ELSEIF b7a = 2 AND b4a = 2 THEN
        b1 = 3
    ELSEIF b7a = 3 AND b4a = 3 THEN
        b1 = 4
    END IF
END IF

RETURN
specialcharacters3:
IF b = CHR$(94) OR b = CHR$(60) OR b = CHR$(62) OR b = CHR$(123) OR b = CHR$(36) OR b = CHR$(43) OR b = CHR$(37) OR b = CHR$(126) OR b = CHR$(125) OR b = CHR$(38) OR b = CHR$(35) OR b = CHR$(42) OR b = CHR$(33) OR b = CHR$(61) THEN
    flag = "Y1"
ELSEIF b = CHR$(91) OR b = CHR$(93) THEN
    flag = "Y2"
ELSE
    flag = "Y3"
END IF
RETURN


' This is where I detect the amount of times that there are corners
' For each corner counted elsewhere in the program
' I do a different colorchange
' Thus opening up the possibility of differnt colors for each
' Box where the lines are
' Many boxes can be colored a different color
' Using this method
' Changes must also be made in the main program as well as in the
' Selection of colors for each set of corners, lines and connectors                                                                                                                               '

colorchange:
IF flag = "Y1" THEN
    g1a = g1a ' Color change for First Outer Box
    t1a = t1a
    g1b = g1b ' Color change for Second Outer Box
    t1b = t1b
    g1c = g1c ' Color change for Third Outer Box
    t1c = t1c
    g1d = g1d ' Color change for Fourth Outer Box
    t1d = t1d

    colorchanges = 1
ELSEIF flag = "Y2" THEN
    g2 = g2
    t2 = t2
    colorchanges = 1
ELSEIF flag = "Y3" THEN
    g = g
    t = t

    colorchanges = 1
ELSE
    g = g
    t = t
    colorchanges = 0
END IF
RETURN


errorhandle:
CLS
CLOSE #1
PRINT
PRINT "Oops, you did something wrong there (or I did) ... "
PRINT
PRINT "Most likely there was a filename that you entered"
PRINT "That did not exist or you have not created the"
PRINT "prompt file for the Menu item desired. Or you picked"
PRINT "a Menu filename and selected a display output instead "
PRINT "a Menu display, the difference is that Menu output "
PRINT "files have a prompt file, display files do not. "
PRINT
PRINT "Your File "; file1; " Has Not Been Found .... Please Try Again."
PRINT
PRINT "If the file is there and it still gives an error, then please"
PRINT "report this as a program error and contact me at rcamp48@rogers.com "
PRINT
PRINT "Press any key to try again......"
DO WHILE INKEY$ = ""
LOOP
GOTO inputscreen
RETURN

background:
CLS
PRINT
PRINT "Please enter the color of your Background "
PRINT
PRINT "Black        is color code 0            Black is color code    8"
PRINT "Blue         is color code 1            Blue is  color code    9"
PRINT "Green        is color code 2            Green is color code   10"
PRINT "Cyan         is color code 3            Cyan is  color code   11"
PRINT "Red          is color code 4            Red  is  color code   12"
PRINT "Magenta      is color code 5            Magenta is color code 13"
PRINT "Brown        is color code 6            Yellow  is color code 14"
PRINT "White        is color code 7            White   is colr code  15"
PRINT
PRINT "Don't worry, the program will translate from decimal to hex for this."
PRINT "Make your Background selection from [0] to [7] for Syncronet Ansi."
PRINT "And [0] to [15] for everything else"
PRINT
RETURN
fixedcolors:
g1a = 2
t1a = 14
g1b = 1
t1b = 12
g1c = 5
t1c = 7
g1d = 6
t1d = 11
g2 = 7
t2 = 1
g = 6
t = 14
RETURN
create:
CLS
PRINT "Save File: [Y]es or [N]o : "
answer = UCASE$(INPUT$(1))
IF answer$ = "N" THEN GOTO inputscreen
PRINT
PRINT
INPUT "Enter Filename [No extender] [Press enter for default] : ", filess
PRINT
IF filess = "" THEN filess = "default"

filess = filess + ".dat"
fnames = "c:\display\data\" + filess
PRINT "Saving data............"
PRINT
ON ERROR GOTO errorhandle
OPEN fnames FOR OUTPUT AS #2
PRINT #2, g1a
PRINT #2, t1a
PRINT #2, g1b
PRINT #2, t1b
PRINT #2, g1c
PRINT #2, t1c
PRINT #2, g1d
PRINT #2, t1d
PRINT #2, g2
PRINT #2, t2
PRINT #2, g
PRINT #2, t
CLOSE #2
RETURN
load:
CLS
PRINT "Load Data File..........."
INPUT "Enter Filename [No extender] [Press enter for default] :  ", filess
PRINT
IF filess = "" THEN filess = "default"
filess = filess + ".dat"
fnames = "c:\display\data\" + filess
PRINT
PRINT "Loading data..........."
PRINT
ON ERROR GOTO errorhandle
OPEN fnames FOR INPUT AS #1
INPUT #1, g1a
INPUT #1, t1a
INPUT #1, g1b
INPUT #1, t1b
INPUT #1, g1c
INPUT #1, t1c
INPUT #1, g1d
INPUT #1, t1d
INPUT #1, g2
INPUT #1, t2
INPUT #1, g
INPUT #1, t
CLOSE #1
RETURN
randomchoices:
RANDOMIZE TIMER
trulyrandom:
g1a = INT(RND(1) * 7) + 1
t1a = INT(RND(1) * 15) + 1
g1b = INT(RND(1) * 7) + 1
t1b = INT(RND(1) * 15) + 1
g1c = INT(RND(1) * 7) + 1
t1c = INT(RND(1) * 15) + 1
g1d = INT(RND(1) * 7) + 1
t1d = INT(RND(1) * 15) + 1
g2 = INT(RND(1) * 7) + 1
t2 = INT(RND(1) * 15) + 1
g = INT(RND(1) * 7) + 1
t = INT(RND(1) * 15) + 1

IF g1a = t1a THEN GOTO trulyrandom
IF g1b = t1b THEN GOTO trulyrandom
IF g1c = t1c THEN GOTO trulyrandom
IF g1d = t1d THEN GOTO trulyrandom
IF g1a = g1b THEN GOTO trulyrandom
IF g1a = g1c THEN GOTO trulyrandom
IF g1a = g1d THEN GOTO trulyrandom
IF g1b = g1a THEN GOTO trulyrandom
IF g1b = g1c THEN GOTO trulyrandom
IF g1b = g1d THEN GOTO trulyrandom
IF g1c = g1a THEN GOTO trulyrandom
IF g1c = g1b THEN GOTO trulyrandom
IF g1c = g1d THEN GOTO trulyrandom
IF g1d = g1a THEN GOTO trulyrandom
IF g1d = g1b THEN GOTO trulyrandom
IF g1d = g1c THEN GOTO trulyrandom
IF g1a = g THEN GOTO trulyrandom
IF g1b = g THEN GOTO trulyrandom
IF g1c = g THEN GOTO trulyrandom
IF g1d = g THEN GOTO trulyrandom
IF g = ga1 OR g = g2 THEN GOTO trulyrandom
IF g = g1b OR g = g2 THEN GOTO trulyrandom
IF g = g1c OR g = g2 THEN GOTO trulyrandom
IF g = g1d OR g = g2 THEN GOTO trulyrandom
IF g = t THEN GOTO trulyrandom
RETURN
