DEFINT A-Z
'$DYNAMIC
'$INCLUDE: 'GIF.BI'

SUB ShowGIF (a$)
	ShowGIFAt 0, 0, a$
END SUB

SUB ShowGIFAt (XOrg, YOrg, a$)

	a$ = a$ + CHR$(0)

	ScrType = 0
	ScrOffset = 0
	ScrWidth = 80
	XRes = 639
	YRes = 199
	AdapterType = 0

	X0 = 0
	Y0 = 0
	X1 = XRes
	Y1 = YRes

	PalIgnore = 1

	REDIM GIFMem(LoadGIFMem * 8) AS INTEGER '8 integers = 16 bytes = 1 page.
	MemPointer = VARSEG(GIFMem(0))

	a = LoadGIF(MemPointer, VARSEG(a$), SADD(a$), ScrType, ScrOffset, ScrWidth, XRes, YRes, X0, Y0, X1, Y1, XOrg, YOrg, AdapterType, PalIgnore, 0, 0, 0)

	REDIM GIFMem(0) AS INTEGER

	IF a THEN 'check error variable
		SCREEN 0: WIDTH 80
		SELECT CASE ErrorReport
		CASE 0 TO 99
				PRINT "Critical Error #:"; ErrorReport
		CASE IS >= 100
				PRINT "DOS Error #:"; ErrorReport
		CASE -1
				PRINT "End of file before GIF terminator."
		CASE -2
				PRINT "LZW data stream is corrupted."
		CASE -3
				PRINT "Not a GIF file."
		CASE -4
				PRINT "Too many colors in GIF file for screen mode."
		CASE -5
				PRINT "Bad image descriptor or GIF too big to be processed."
		CASE -6
				PRINT "ScrType parameter is bad. Must be 0, 1 or 2."
		CASE -7
				PRINT "AdapterType parameter is bad or EGA palette specified for"
				PRINT "a 256 color mode."
		END SELECT
		END
	END IF
END SUB

REM $STATIC
SUB ShowGIFAt320 (XOrg, YOrg, a$)

	a$ = a$ + CHR$(0)

	ScrType = 0
	ScrOffset = 0
	ScrWidth = 40
	XRes = 319
	YRes = 199
	AdapterType = 0

	X0 = 0
	Y0 = 0
	X1 = XRes
	Y1 = YRes

	PalIgnore = 1

	REDIM GIFMem(LoadGIFMem * 8) AS INTEGER '8 integers = 16 bytes = 1 page.
	MemPointer = VARSEG(GIFMem(0))

	a = LoadGIF(MemPointer, VARSEG(a$), SADD(a$), ScrType, ScrOffset, ScrWidth, XRes, YRes, X0, Y0, X1, Y1, XOrg, YOrg, AdapterType, PalIgnore, 0, 0, 0)

	REDIM GIFMem(0) AS INTEGER

	IF a THEN 'check error variable
		SCREEN 0: WIDTH 80
		SELECT CASE ErrorReport
		CASE 0 TO 99
				PRINT "Critical Error #:"; ErrorReport
		CASE IS >= 100
				PRINT "DOS Error #:"; ErrorReport
		CASE -1
				PRINT "End of file before GIF terminator."
		CASE -2
				PRINT "LZW data stream is corrupted."
		CASE -3
				PRINT "Not a GIF file."
		CASE -4
				PRINT "Too many colors in GIF file for screen mode."
		CASE -5
				PRINT "Bad image descriptor or GIF too big to be processed."
		CASE -6
				PRINT "ScrType parameter is bad. Must be 0, 1 or 2."
		CASE -7
				PRINT "AdapterType parameter is bad or EGA palette specified for"
				PRINT "a 256 color mode."
		END SELECT
		END
	END IF

END SUB

