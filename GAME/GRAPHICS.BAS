'$INCLUDE: 'GRAPHICS.BI'

DIM SHARED wideNibbles%(15)
DIM SHARED colouredSquares%(10, 15)

SUB GFX.Box (x1%, y1%, x2%, y2%, c%)
    x0% = x1% * 16: y% = y1% * 8
    FOR yc% = y1% TO y2%
        x% = x0%
        FOR xc% = x1% TO x2%
            PUT (x%, y%), colouredSquares%(0, c%), PSET
            x% = x% + 16
        NEXT xc%
        y% = y% + 8
    NEXT yc%
END SUB

SUB GFX.gPrint (x0%, y%, t$)
    x% = x0%
    DEF SEG = &HFFA6
    FOR i% = 1 TO LEN(t$)
        addr% = &HE + ASC(MID$(t$, i%, 1)) * 8
        FOR j% = 0 TO 7
            o% = PEEK(addr% + j%)
            LINE (x%, y% + j%)-(x% + 15, y% + j%), 1, , CVI(CHR$(wideNibbles%(o% AND &HF)) + CHR$(wideNibbles%(o% \ 16)))
        NEXT j%
        x% = x% + 16
    NEXT i%
    DEF SEG
END SUB

SUB GFX.Init
    ' Populate wideNibbles. Can't be arsed to do it programatically
    wideNibbles%(0) = 0
    wideNibbles%(1) = &H3
    wideNibbles%(2) = &HC
    wideNibbles%(3) = &HF
    wideNibbles%(4) = &H30
    wideNibbles%(5) = &H33
    wideNibbles%(6) = &H3C
    wideNibbles%(7) = &H3F
    wideNibbles%(8) = &HC0
    wideNibbles%(9) = &HC3
    wideNibbles%(10) = &HCC
    wideNibbles%(11) = &HCF
    wideNibbles%(12) = &HF0
    wideNibbles%(13) = &HF3
    wideNibbles%(14) = &HFC
    wideNibbles%(15) = &HFF

    ' Init 640x200, color burst bit on (composite graphics)
    SCREEN 2
    OUT &H3D8, &H1A

    ' Display pattern and capture colour squares for later.
    GFX.gPrint 13 * 16 + 8, 90, "CGA COMPOSITE"
    x% = 320 - 15 * 8
    FOR i% = 1 TO 15
        a% = i% * 16 + i%
        FOR y% = 0 TO 7
            LINE (x%, 102 + y%)-(15 + x%, 102 + y%), 1, , CVI(CHR$(a%) + CHR$(a%))
        NEXT y%
        GET (x%, 102)-(15 + x%, 109), colouredSquares%(0, i%)
        x% = x% + 16
    NEXT i%
    SLEEP 1
    CLS
END SUB

SUB GFX.waitFullFrame
    WAIT &H3DA, 8
    WAIT &H3DA, 8, 8
END SUB

