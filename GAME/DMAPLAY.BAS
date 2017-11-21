'$INCLUDE: 'DMAPLAY.BI'
DIM SHARED BasePort%, LenPort%, IRQ%, DMA%

REM $STATIC
FUNCTION DMA.DMADone%
	Count% = INP(LenPort%)
	Count2% = INP(LenPort%)
	Count& = CLNG(Count% + 1) * CLNG(Count2% + 1)
	IF (Count& - 1) >= &HFFFF& THEN junk% = INP(DSPDataAvail%): DMA.DMADone% = -1
END FUNCTION

SUB DMA.DMAPlay (Segment&, Offset&, Length&, freq&)
	' Transfers and plays the contents of the buffer.
	Length& = Length& - 1
	Page% = 0
	MemLoc& = Segment& * 16 + Offset&
	SELECT CASE DMA%
		CASE 0
		   PgPort% = &H87
		   AddPort% = &H0
		   LenPort% = &H1
		   ModeReg% = &H48
		CASE 1
		   PgPort% = &H83
		   AddPort% = &H2
		   LenPort% = &H3
		   ModeReg% = &H49
		CASE 2
		   PgPort% = &H81
		   AddPort% = &H4
		   LenPort% = &H5
		   ModeReg% = &H4A
		CASE 3
		   PgPort% = &H82
		   AddPort% = &H6
		   LenPort% = &H7
		   ModeReg% = &H4B
		CASE ELSE
		   PRINT "DMA channels 0-3 only are supported."
		   EXIT SUB
	END SELECT
	
	OUT &HA, &H4 + DMA%
	OUT &HC, &H0
	OUT &HB, ModeReg%
	OUT AddPort%, MemLoc& AND &HFF
	OUT AddPort%, (MemLoc& AND &HFFFF&) \ &H100
	IF (MemLoc& AND 65536) THEN Page% = Page% + 1
	IF (MemLoc& AND 131072) THEN Page% = Page% + 2
	IF (MemLoc& AND 262144) THEN Page% = Page% + 4
	IF (MemLoc& AND 524288) THEN Page% = Page% + 8
	OUT PgPort%, Page%
	OUT LenPort%, Length& AND &HFF
	OUT LenPort%, (Length& AND &HFFFF&) \ &H100
	OUT &HA, DMA%

	IF freq& < 23000 THEN
	   TimeConst% = 256 - 1000000 \ freq&
	   DMA.WriteDSP &H40
	   DMA.WriteDSP TimeConst%
	   DMA.WriteDSP &H14
	   DMA.WriteDSP (Length& AND &HFF)
	   DMA.WriteDSP ((Length& AND &HFFFF&) \ &H100)
	ELSE
	   IF DMA.DSPVersion! >= 3 THEN
		  TimeConst% = ((65536 - 256000000 \ freq&) AND &HFFFF&) \ &H100
		  DMA.WriteDSP &H40
		  DMA.WriteDSP TimeConst%
		  DMA.WriteDSP (Length& AND &HFF)
		  DMA.WriteDSP ((Length& AND &HFFFF&) \ &H100)
		  DMA.WriteDSP &H91
	   ELSE
		  PRINT "You need a Sound Blaster with a DSP v3.x+ to play at high speed."
		  EXIT SUB
	   END IF
	END IF
END SUB

SUB DMA.DMAState (StopGo%)
	' Stops or continues DMA play.
	IF StopGo% THEN DMA.WriteDSP &HD4 ELSE DMA.WriteDSP &HD0
END SUB

FUNCTION DMA.DSPVersion!
	' Gets the DSP version.
	DMA.WriteDSP &HE1
	Temp% = DMA.ReadDSP%
	Temp2% = DMA.ReadDSP%
	DMA.DSPVersion! = VAL(STR$(Temp%) + "." + STR$(Temp2%))
END FUNCTION

SUB DMA.GetBlaster
	' This subroutine parses the BLASTER environment string and returns settings.
	IF LEN(ENVIRON$("BLASTER")) = 0 THEN PRINT "BLASTER environment variable not set.": EXIT SUB
	FOR Length% = 1 TO LEN(ENVIRON$("BLASTER"))
	   SELECT CASE MID$(ENVIRON$("BLASTER"), Length%, 1)
		  CASE "A"
			BasePort% = VAL("&H" + MID$(ENVIRON$("BLASTER"), Length% + 1, 3))
		  CASE "I"
			IRQ% = VAL(MID$(ENVIRON$("BLASTER"), Length% + 1, 1))
		  CASE "D"
			DMA% = VAL(MID$(ENVIRON$("BLASTER"), Length% + 1, 1))
	   END SELECT
	NEXT
END SUB

SUB DMA.playVOC (file$, freq&)
   DMA.SpeakerState 1
   'MasterVolume 15, 15, 0
   DIM WavBuffer(1 TO 1) AS STRING * 5000
   FileName$ = file$
   f% = FREEFILE
   OPEN FileName$ FOR BINARY AS f%
	  GET #f%, 44, WavBuffer(1)
	  Length& = LOF(f%) - 44
   CLOSE f%
   IF Length& > 8192 THEN Length& = 5000
   DMA.DMAPlay VARSEG(WavBuffer(1)), VARPTR(WavBuffer(1)), Length&, freq&
   DO: LOOP UNTIL DMA.DMADone%
   ERASE WavBuffer
END SUB

SUB DMA.PlayVOCBG (file$, freq&)
   'MasterVolume 15, 15, 0
   DIM WavBuffer(1 TO 1) AS STRING * 5000
   FileName$ = file$
   f% = FREEFILE
   OPEN FileName$ FOR BINARY AS #f%
	  GET #f%, 44, WavBuffer(1)
	  Length& = LOF(f%) - 44
   CLOSE f%
   IF Length& > 8192 THEN Length& = 5000
   IF NOT DMA.DMADone% THEN EXIT SUB
   DMA.DMAPlay VARSEG(WavBuffer(1)), VARPTR(WavBuffer(1)), Length&, freq&
	
   ERASE WavBuffer
END SUB

FUNCTION DMA.ReadDSP%
	' Reads a byte from the DSP
	DO
	LOOP UNTIL INP(BasePort% + 14) AND &H80
	DMA.ReadDSP% = INP(BasePort% + 10)
END FUNCTION

FUNCTION DMA.ResetDSP%
	' Resets the DSP
	OUT BasePort% + 6, 1
	FOR Count% = 1 TO 4
	   junk% = INP(BasePort% + 6)
	NEXT
	OUT BasePort% + 6, 0
	IF INP(BasePort% + 14) AND &H80 = &H80 AND INP(BasePort% + 10) = &HAA THEN
	   DMA.ResetDSP% = -1
	ELSE
	   DMA.ResetDSP% = 0
	END IF
END FUNCTION

SUB DMA.SetStereo (OnOff%)
	OUT BasePort% + 4, &HE
	IF OnOff% THEN OUT BasePort% + 5, 2 ELSE OUT BasePort% + 5, 0
END SUB

SUB DMA.SpeakerState (OnOff%)
	' Turns speaker on or off.
	IF OnOff% THEN DMA.WriteDSP &HD1 ELSE DMA.WriteDSP &HD3
END SUB

FUNCTION DMA.SpeakerStatus%
	OUT BasePort% + 4, &HD8
	IF INP(BasePort% + 5) = &HFF THEN DMA.SpeakerStatus% = -1 ELSE DMA.SpeakerStatus% = 0
END FUNCTION

SUB DMA.WriteDAC (byte%)
	' Writes a byte to the DAC.
	DMA.WriteDSP &H10
	DMA.WriteDSP byte%
END SUB

SUB DMA.WriteDSP (byte%)
	' Writes a byte to the DSP
	DO
	LOOP WHILE INP(BasePort% + 12) AND &H80
	OUT BasePort% + 12, byte%
END SUB

