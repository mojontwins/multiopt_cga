'$INCLUDE 'GRAPHICS.BI'
'$INCLUDE 'VANE.BI'

DIM SHARED chapterNumber AS INTEGER
DIM SHARED specialVal As INTEGER
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

DIM SHARED flags (127) AS INTEGER
DIM SHARED scriptPool (4095) AS INTEGER	' 8Kb is more than enough
DIM SHARED sptP AS INTEGER

DIM SHARED chapterFilenames$ (255)

SUB VANE.Init (sv%, cmd%, na%, tos%, tb$, wh%, mb%, ml%)
	specialVal = sv%
	command = cmd%
	nextAddress = na%
	topOfScreen = tos%
	titleBar =  tb$<>""
	titleBarText = tb$
	windowHeight = wh%
	menuBottom = mb%
	menuLeft = ml%
END SUB

SUB VANE.assignChapterFilename (chapter%, fileName$)
	chapterFileNames$ (chapter%) = fileName$
END SUB

SUB VANE.loadBinary ()
	DIM d AS STRING * 1
	f%=FREEFILE
	IF chapterFileNames$(chapterNumber)="" THEN 
		PRINT "No file mapped to chapter " + chapterNumber: END
	END IF
	OPEN chapterFileNames$(chapterNumber) FOR BINARY AS #f%
	DEF SEG=VARSEG(scriptPool (0)):address%=VARPTR(scriptPool(0))
	WHILE NOT EOF(f%)
		GET #f%, , d
		POKE address%, ASC(d)
		address%=address%+1
	WEND
	DEF SEG
	CLOSE #f%
END SUB

SUB VANE.titleBar ()
	GFX.Box 0,0,39,0,2
	GFX.gPrint 1,0,titleBarText
END SUB

SUB VANE.runGameFrom (ch%)
	DIM AS INTEGER nextChapter, chapter
	
''' Main Loop
	
	flags (127) = specialVal
	rememberMenuOpt = 1
	Do
		chapter = ch%			
		fancy = 1
		Do
			nextChapter = VANE.runChapter (chapter)
			chapter = nextChapter
		Loop While chapter < &HFF
	Loop
	
END SUB

FUNCTION VANE.runChapter (chapter AS INTEGER) AS INTEGER
	chapterNumber = chapter
	VANE.loadBinary ()
	
	menuOpts%=0
	nextChapter%=0
	doNothing%=0
	
	' If command = 1 we must run from nextAddress
	IF command = 1 THEN
		sptP=VARPTR (scriptPool(0))+nextAddress
		command = 0
	Else
		sptP=VARPTR (scriptPool(0))
	End If
	
	allDone%=0
	
	DO WHILE allDone%=0
		IF doNothing% > 0 Then
			GFX.WaitFullFrame
			doNothing = doNothing - 1
		ELSE
			b = VANE.readByte ()
			SELECT CASE b
			END SELECT
		END IF
	LOOP
	
	VANE.runChapter = nextChapter%
END SUB

FUNCTION VANE.ReadByte () AS INTEGER
	DEF SEG=VARSEG (scriptPool(0))
	res%=PEEK(sptP)
	sptP = sptP+1
	DEF SEG
	VANE.ReadByte = res%
END FUNCTION

FUNCTION VANE.ReadVByte () AS INTEGER
	res%=0:b%=VANE.ReadByte ()
	IF b%>127 THEN res%=flags%(b-128) ELSE res%=b%
	VANE.ReadVByte= res%
END SUB

FUNCTION VANE.ReadAddress () AS INTEGER
	VANE.ReadAddress =VANE.ReadByte()*256+VANE.ReadByte()
END FUNCTION
