DECLARE SUB GFX.waitFullFrame ()
'$INCLUDE: 'GRAPHICS.BI'
'$INCLUDE: 'VANE.BI'

DIM SHARED chapterNumber AS INTEGER
DIM SHARED specialVal AS INTEGER
DIM SHARED command AS INTEGER
DIM SHARED nextAddress AS INTEGER
DIM SHARED languageMod AS INTEGER
DIM SHARED topOfScreen AS INTEGER
DIM SHARED titleBar AS INTEGER
DIM SHARED titleBarText AS STRING
DIM SHARED windowHeight AS INTEGER
DIM SHARED menuBottom AS INTEGER
DIM SHARED menuLeft AS INTEGER
DIM SHARED rememberMenuOpt AS INTEGER
DIM SHARED fancy AS INTEGER

DIM SHARED flags(127)  AS INTEGER
DIM SHARED scriptPool(4095)  AS INTEGER ' 8Kb is more than enough
DIM SHARED sptP AS INTEGER
DIM SHARED menuBuffer(10 * 10 \ 2) AS INTEGER
DIM SHARED commPool(31)  AS INTEGER

DIM SHARED chapterFileNames$(255)

SUB VANE.assignChapterFilename (chapter%, fileName$)
	chapterFileNames$(chapter%) = fileName$
END SUB

SUB VANE.Init (sv%, cmd%, na%, tos%, tb$, wh%, mb%, ml%)
	specialVal = sv%
	command = cmd%
	nextAddress = na%
	topOfScreen = tos%
	titleBar = tb$ <> ""
	titleBarText = tb$
	windowHeight = wh%
	menuBottom = mb%
	menuLeft = ml%
END SUB

SUB VANE.loadBinary
	DIM d AS STRING * 1
	f% = FREEFILE
	IF chapterFileNames$(chapterNumber) = "" THEN
		PRINT "No file mapped to chapter"; chapterNumber: END
	END IF
	OPEN chapterFileNames$(chapterNumber) FOR BINARY AS #f%
	DEF SEG = VARSEG(scriptPool(0)): address% = VARPTR(scriptPool(0))
	WHILE NOT EOF(f%)
		GET #f%, , d
		POKE address%, ASC(d)
		address% = address% + 1
	WEND
	DEF SEG
	CLOSE #f%
END SUB

FUNCTION VANE.ReadAddress%
	VANE.ReadAddress% = VANE.readByte% * 256 + VANE.readByte%
END FUNCTION

FUNCTION VANE.ReadByte%
	DEF SEG = VARSEG(scriptPool(0))
	res% = PEEK(sptP)
	sptP = sptP + 1
	DEF SEG
	VANE.ReadByte = res%
END FUNCTION

FUNCTION VANE.ReadVByte%
	res% = 0: b% = VANE.ReadByte
	IF b% > 127 THEN res% = flags%(b - 128) ELSE res% = b%
	VANE.ReadVByte = res%
END FUNCTION

FUNCTION VANE.runChapter% (chapter AS INTEGER)
	chapterNumber = chapter
	VANE.loadBinary
	
	menuOpts% = 0
	nextChapter% = 0
	doNothing% = 0
	
	' If command = 1 we must run from nextAddress
	IF command = 1 THEN
		sptP = VARPTR(scriptPool(0)) + nextAddress
		command = 0
	ELSE
		sptP = VARPTR(scriptPool(0))
	END IF
	
	allDone% = 0
	
	DO WHILE allDone% = 0
		IF doNothing% > 0 THEN
			GFX.waitFullFrame
			doNothing = doNothing - 1
		ELSE
			b% = VANE.ReadByte%
			SELECT CASE b%
				CASE &H1:
					' IMAGE
					b% = VANE.ReadVByte%
					''TODO''
				CASE &H2:
					' CUT_LOAD
					b% = VANE.ReadVByte%
					c% = VANE.ReadVByte%
					''TODO''
				CASE &H3:
					' CLEAR
					''TODO''
				CASE &H4:
					' CUT_SHOW
					b% = VANE.ReadVByte%
					x% = VANE.ReadVByte%
					y% = VANE.ReadVByte%
					''TODO''
				CASE &H5:
					' FANCY ON
					fancy = -1
				CASE &H6:
					' FANCY OFF
					fancy = 0
				CASE &H7:
					' Scroll_BG
					b% = VANE.ReadVByte%
					''WON'T IMPLEMENT''
				CASE &H8:
					' Restore_BG
					oldFancy% = fancy
					fancy = 0
					''TODO''
					fancy = oldFancy%
				CASE &H9:
					' CUT_OVL
					b% = VANE.ReadVByte%
					x% = VANE.ReadVByte%
					y% = VANE.ReadVByte%
					''TODO''
				CASE &H10, &H11:
					' TEXT
					c% = VANE.ReadVByte%
					VANE.doTextCycle c%
					IF b% = &H11 THEN doWt
				CASE &H1D:
					' Set sleep counter
					doNothing% = VANE.ReadByte% * 256 + VANE.ReadByte%
				CASE &H1E:                  
					' Clear text
					VANE.clrTextArea
				CASE &H1F:
					' WT
					doWt
				CASE &H20:
					' Clear menu
					menuOpts% = 0
				CASE &H21, &H22, &H23:
					' Menu fiddling
					IF b% = &H22 THEN
						writeTo% = VANE.ReadVByte% - 1
					ELSE
						writeTo% = menuOpts%
						IF b% = &H23 THEN flags%(VANE.ReadVByte%) = writeTo% + 1
					END IF
					' Add item
					' Copy bytes to menu buffer
					DEF SEG = VARSEG(menuBuffer(0))
					address% = VARPTR(menuBuffer(0)) + (writeTo% * 10)
					FOR i% = 0 TO 9
						POKE address%, VANE.ReadByte%
						address% = address% + 1
					NEXT i%
					IF writeTo% >= menuOpts% THEN menuOpts% = writeTo% + 1
					DEF SEG
				CASE &H30, &H31, &H32, &H33:
					' Conditional branching
					c% = VANE.ReadVByte%: d% = VANE.ReadVByte%
					address% = VANE.ReadAddress%
					VANE.doCondBranch b%, c%, d%, address%
				CASE &H3F:
					' GOTO
					VANE.jumpToAddress VANE.ReadAddress%
				CASE &H40:
					' LET
					flags%(VANE.ReadVByte%) = VANE.ReadVByte%
				CASE &H50:
					' DO MENU
					flags%(0) = VANE.doMenu%
				CASE &H60:
					' TALK
					'' TODO prep
					FOR i% = 1 TO 10
						b% = VANE.ReadByte%
						'' TODO draw
					NEXT i
					'' TODO draw more (check what does this do and somehow.. replicate!
				CASE &H61:
					' TALK OFF
					'' TODO. I need to design a gui which is easy to maintain
				CASE &H70:
					' CD PLAY
					b% = VANE.ReadVByte%
					'' Won't implement - or make it FM!
				CASE &H71:
					' CD STOP
					'' Won't implement - or make it FM!
				CASE &H72:
					' CD PAUSE
				CASE &H73:
					' CD UNPAUSE
				CASE &H80:
					' CLR
					b% = VANE.ReadVByte%
					VANE.niceCls b%
				CASE &H88:
					' SCROLL_SET TILE_POS
					b% = readByte%
					'' Won't implement
				CASE &H89:
					' SCROLL_TO LSB STEP
					b% = readByte%
					'' Won't implement
				CASE &H90:
					' INC
					b% = VANE.ReadVByte%
					flags%(b%) = flags%(b%) + 1
				CASE &H91:
					' DEC
					b% = VANE.ReadVByte%
					flags%(b%) = flags%(b%) - 1
				CASE &HA0:
					' CLEAR SAMPLES
					''TODO''
				CASE &HA1:
					' ADD_SAMPLE
					b% = VANE.ReadVByte%
				CASE &HA2:
					' PLAY_SAMPLE
					b% = VANE.ReadVByte%
				CASE &HE0:
					' SET N = A
					b% = VANE.ReadByte%
					commPool(b%) = VANE.ReadByte%
				CASE &HE1:
					' RUN_SLOT N
					b% = VANE.ReadByte%
					' Save current PC
					' commPool (&H25)=VANE.getIntPart (---)
					''TODO''                                                                               
				CASE &HFB:
					nextChapter% = VANE.ReadVByte%
					allDone% = -1
				CASE &HFC:
					nextChapter% = chapter%
					allDone% = -1
				CASE &HFD:
					nextChapter% = chapter% - 1
					IF nextChapter% < 0 THEN nextChapter% = 0
					allDone% = -1
				CASE &HFE:
					nextChapter% = chapter% + 1
					allDone% = -1
				CASE &HFF:
					nextChapter% = &HFF
					allDone% = -1
			END SELECT
		END IF
	LOOP
	
	VANE.runChapter = nextChapter%
END FUNCTION

SUB VANE.runGameFrom (ch%)
	DIM nextChapter, chapter AS INTEGER
	
''' Main Loop
	
	flags(127) = specialVal
	rememberMenuOpt = 1
	DO
		chapter = ch%
		fancy = 1
		DO
			nextChapter = VANE.runChapter(chapter)
			chapter = nextChapter
		LOOP WHILE chapter < &HFF
	LOOP
	
END SUB

SUB VANE.titleBar
	GFX.Box 0, 0, 39, 0, 2
	GFX.gPrint 1, 0, titleBarText
END SUB

