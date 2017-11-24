' MOUSE MASTERING By Harding Brothers (C)

DECLARE FUNCTION mouseinit% ()
DECLARE SUB mousecall ()
DECLARE SUB MouseGetPressInfo (LBtn%, RBtn%, MBtn%, Count%, HPosn%, VPosn%)
DECLARE SUB MouseGetReleaseInfo (LBtn%, RBtn%, MBtn%, Count%, HPosn%, VPosn%)
DECLARE SUB MouseGetStatus (LBtn%, RBtn%, MBtn%, HPosn%, VPosn%)
DECLARE SUB mousehide ()
DECLARE SUB MouseReadCounters (HCount%, VCount%)
DECLARE SUB MouseSetHorizRange (HMin%, HMax%)
DECLARE SUB MouseSetPosn (HPosn%, VPosn%)
DECLARE SUB MouseSetup ()
DECLARE SUB MouseSetVertRange (VMin%, VMax%)
DECLARE SUB mouseshow ()

DECLARE SUB starodds ()
DEFINT A-Z
DECLARE SUB ldpers (n$)
DECLARE SUB ptpers ()
DECLARE SUB starfield ()
DECLARE FUNCTION qbut ()
DECLARE FUNCTION qresp ()
DECLARE SUB invcl ()
DECLARE SUB invpr ()
DECLARE FUNCTION invsl (n$)
DECLARE SUB putobjs (n)
DECLARE SUB pita (l, h, p)
DECLARE SUB ppreg (p1$, p2$, p3$, p4$)
DECLARE SUB spgp (x%, y%, c%, t$)
DECLARE SUB gprint (x%, y%, Culler%, text$)
DECLARE SUB but (n, t$)
DECLARE SUB spclr ()
DECLARE SUB cln ()
DECLARE SUB ldg (n, t$)
DECLARE SUB centra (y, t$)
DECLARE SUB gtlt ()
DECLARE SUB ptlt ()
DECLARE SUB rslt ()
DECLARE SUB ldnop (n$)
DECLARE SUB gets ()
DECLARE SUB puts ()
DECLARE SUB putf ()
DECLARE SUB thyob ()
DECLARE SUB wt ()
DECLARE FUNCTION move ()
DECLARE SUB ptx (x, y, c, t$)
DECLARE SUB paintingprog ()
OPTION BASE 0

COMMON SHARED mx, my, fin
COMMON SHARED t, xmn, ymn, psmn, mira, t3, clet, tester, NCursor, s0, st
COMMON SHARED psi, psix, psiy, stopat
COMMON SHARED indice

DIM SHARED dato AS STRING * 1

CONST FALSE = 0
CONST TRUE = NOT FALSE
MouseInt:
' Machine language routine 40 bytes long
DATA 55
DATA 89, E5
DATA 56
DATA 8B, 76, 06
DATA 8B, 04
DATA 8B, 5C, 02
DATA 8B, 4C, 04
DATA 8B, 54, 06
DATA 8E, 44, 08
DATA CD, 33
DATA 8C, 44, 08
DATA 89, 54, 06
DATA 89, 4C, 04
DATA 89, 5C, 02
DATA 89, 04
DATA 5E
DATA 5D
DATA CB

TYPE MouseRegs
    AX AS INTEGER
    BX AS INTEGER
    CX AS INTEGER
    DX AS INTEGER
    ES AS INTEGER
END TYPE

DIM SHARED MReg AS MouseRegs
REDIM SHARED MouseRoutine%(0 TO 19)
DIM SHARED MouseReady%
MouseReady% = 0
MouseSetup
MouseReady% = mouseinit

DIM SHARED tmr AS SINGLE
DIM SHARED tmr2 AS SINGLE
psi = 0
st = 0
s0 = 0
NCursor = 4
clet = 3
psmn = 1
mira = 4 '(abajo)
't = 697
t = 386
t2 = 1954
t3 = 404
t4 = 104
DIM SHARED in(4)
DIM SHARED i$(4)
DIM SHARED lt(t2)
DIM SHARED sp(t, 12)
DIM SHARED ms(t, 12)
DIM SHARED sp2(576, 5)
DIM SHARED ms2(576, 5)
DIM SHARED spp(t)
DIM SHARED mpp(t)
DIM SHARED f(t)
DIM SHARED nop(39, 24)
DIM SHARED pl(10, 4)
DIM SHARED objs(t3, 5)
DIM SHARED objm(t3, 5)
DIM SHARED objac(5)
DIM SHARED objwhere(5, 2)
DIM SHARED mouse(t4, 4)
DIM SHARED mmask(t4, 4)
DIM SHARED mbkgd(t4)

'RESTORE sprites
'FOR i = 1 TO 10
'    READ template$
'    OPEN "DATA\" + template$ + ".spr" FOR BINARY AS #1
'    OPEN "DATA\" + template$ + ".msk" FOR BINARY AS #2
'    FOR j = 0 TO t
'        GET #1, , sp(j, i)
'        GET #2, , ms(j, i)
'    NEXT j
'    CLOSE
'NEXT i

SCREEN 1
PALETTE 0, 0
PALETTE 1, 0
PALETTE 2, 0
PALETTE 3, 0
xx% = 0: yy% = 1
BLOAD "DATA\SPRITES.CGA"
FOR i% = 1 TO 12
    GET (xx%, yy%)-(31 + xx%, 47 + yy%), sp(0, i%)
    GET (xx%, 96 + yy%)-(31 + xx%, 96 + 47 + yy%), ms(0, i%)
    xx% = xx% + 32: IF xx% = 320 THEN xx% = 0: yy% = yy% + 48
NEXT i%
xx% = 0: yy% = 1
BLOAD "DATA\BUCHE.CGA"
FOR i% = 0 TO 4
    GET (xx%, yy%)-(47 + xx%, 47 + yy%), sp2(0, i%)
    GET (xx%, 48 + yy%)-(47 + xx%, 95 + yy%), ms2(0, i%)
    xx% = xx% + 48
NEXT i%
CLS

RESTORE mousecursors
FOR i = 1 TO 4
    READ template$
    OPEN "DATA\" + template$ + ".cur" FOR BINARY AS #1
    OPEN "DATA\" + template$ + ".cmk" FOR BINARY AS #2
    FOR j = 0 TO t4
        GET #1, , mouse(j, i)
        GET #2, , mmask(j, i)
    NEXT j
    CLOSE
NEXT i
PRINT
starodds
END

sprites:
DATA lf1,lf2,lf3,rt1,rt2,rt3,up1,up2,dw2,dw1
mousecursors:
DATA m1,m2,m3,m4
places:
casa:
DATA 3
'DATA 131,50,154,97
'DATA 163,0,319,130
'DATA 308,0,319,199
DATA 74,70,120,116
DATA 118,24,319,104
DATA 101,136,143,176
campo1:
DATA 3
'DATA 172,129,225,176
'DATA 0,0,117,133
'DATA 117,0,319,94
DATA 121,128,184,169
DATA 0,31,66,199
DATA 67,31,319,96
rio:
DATA 1
'DATA 0,0,319,97
DATA 0,0,319,129
tuhabitacion:
DATA 3
DATA 40,148,80,188
DATA 179,62,209,102
DATA 16,16,76,132
crater:
DATA 3
DATA 0,0,319,16
'DATA 0,0,0,0
DATA 50,150,90,180
'DATA 133,0,319,124
DATA 130,33,268,142
desfiladero:
DATA 3
DATA 0,0,319,16
'DATA 0,0,0,0
DATA 180,40,220,80
DATA 61,135,101,175
castillo:
DATA 2
DATA 0,0,1,1
DATA 118,116,208,179
Salon:
DATA 1
'DATA 123,19,193,44
DATA 79,54,132,127


SUB but (n, t$)
    SELECT CASE n
        CASE 1
            x1 = 5
            x2 = 75
            x = 40 - LEN(t$) * 4
        CASE 2
            x1 = 85
            x2 = 155
            x = 120 - LEN(t$) * 4
        CASE 3
            x1 = 165
            x2 = 235
            x = 200 - LEN(t$) * 4
        CASE 4
            x1 = 245
            x2 = 315
            x = 280 - LEN(t$) * 4
        CASE ELSE
            EXIT SUB
    END SELECT
    FOR y = 23 * 8 - 4 TO 24 * 8 + 4 STEP 2
        LINE (x1, y)-(x2, y), , , 85 + 256 * 85
        LINE (x1, y + 1)-(x2, y + 1), , , -(85 + 256 * 85) - 1
    NEXT y
    FOR i = -1 TO 1
        FOR j = -1 TO 1
            IF i <> 0 OR j <> 0 THEN gprint x + i, 23 * 8 + j, 0, t$
    NEXT j, i
    gprint x, 23 * 8, clet, t$
    LINE (x1, 23 * 8 - 5)-(x2, 23 * 8 - 5), clet
    LINE (x1, 24 * 8 + 5)-(x2, 24 * 8 + 5), clet
    LINE (x1 - 1, 23 * 8 - 4)-(x1 - 1, 24 * 8 + 4), clet
    LINE (x2 + 1, 23 * 8 - 4)-(x2 + 1, 24 * 8 + 4), clet
END SUB

SUB centra (y, t$)
    LOCATE y, 21 - LEN(t$) \ 2
    PRINT t$
END SUB

SUB cln
    FOR i = 0 TO 5
        objac(i) = 0
    NEXT i
END SUB

SUB gets
    'PRINT xmn * 8 + 41
    'PRINT ymn * 8 + 62
    IF ymn < 0 THEN ymn = 0
    IF ymn > 19 THEN ymn = 19
    IF xmn < 0 THEN xmn = 0
    IF xmn > 34 THEN xmn = 34
    GET (xmn * 8, ymn * 8)-(xmn * 8 + 31, ymn * 8 + 47), f(1)
END SUB

SUB gprint (x%, y%, Culler%, text$)
    'this routine allows printing text at any pixel location
    '      in the graphics modes without disturbing the background
    'by Douglas H. Lusher
    DEF SEG = &HFFA6 ' ROM segment for character shape table
    xx% = x%
    FOR char% = 1 TO LEN(text$)
        Addr% = 8 * ASC(MID$(text$, char%, 1)) + 14
        FOR Ln% = 0 TO 7
            LineFormat% = CVI(CHR$(0) + CHR$(PEEK(Addr% + Ln%)))
            yy% = y% + Ln%
            LINE (xx%, yy%)-(xx% + 7, yy%), Culler%, , LineFormat%
        NEXT
        xx% = xx% + 8
    NEXT
    DEF SEG
END SUB

SUB gtlt
    GET (4, 4)-(315, 28), lt(1)
END SUB

SUB invcl
    FOR i = 1 TO 4
        in(i) = 0
        i$(i) = ""
    NEXT i
END SUB

SUB invpr
    FOR i = 1 TO 4
        y = 140 + (i - 1) * 10
        spgp 318 - (8 * LEN(i$(i))), y, 3, i$(i)
    NEXT i
END SUB

FUNCTION invsl (n$)
    ptlt
    centra 2, "USAR " + n$ + " EN..."
    pi = NCursor
    NCursor = 4
    wt
    NCursor = pi
    MouseGetStatus l%, r%, m%, x%, y%
    x% = x% \ 2
    aa = 999
    FOR i = 0 TO 10 ' las 11 PLACES que hay
        IF x% + 5 > pl(i, 1) AND x% + 5 < pl(i, 3) AND y% + 5 > pl(i, 2) AND y% + 5 < pl(i, 4) THEN
            aa = i
        END IF
    NEXT i
    invsl = aa
END FUNCTION

SUB ldg (n, t$)
    OPEN "DATA\" + t$ + ".scn" FOR BINARY AS #1
    OPEN "DATA\" + t$ + ".msk" FOR BINARY AS #2
    FOR i = 0 TO t3
        GET #1, , objs(i, n)
        GET #2, , objm(i, n)
    NEXT i
    objac(n) = -1
    CLOSE
END SUB

SUB ldnop (n$)
    OPEN "DATA\" + n$ FOR BINARY AS #1
    FOR i = 0 TO 39
        FOR j = 0 TO 24
            GET #1, , nop(i, j)
    NEXT j, i
    CLOSE
END SUB

SUB ldpers (n$)
    OPEN "DATA\" + n$ + ".spr" FOR BINARY AS #1
    OPEN "DATA\" + n$ + ".msk" FOR BINARY AS #2
    FOR i = 0 TO t
        GET #1, , spp(i)
        GET #2, , mpp(i)
    NEXT i
    CLOSE 1, 2
END SUB

DEFSNG A-Z
SUB mousecall
    DEF SEG = VARSEG(MouseRoutine%(0))
    Addr% = VARPTR(MouseRoutine%(0))
    CALL Absolute(MReg, Addr%)
    DEF SEG
END SUB

SUB MouseGetPressInfo (LBtn%, RBtn%, MBtn%, Count%, HPosn%, VPosn%)
    IF MouseReady% THEN
        IF (LBtn% OR RBtn% OR MBtn%) <> 0 THEN
            MReg.AX = 5
            IF LBtn% THEN MReg.BX = 0
            IF RBtn% THEN MReg.BX = 1
            IF MBtn% THEN MReg.BX = 2
            mousecall
            LBtn% = MReg.AX AND 1
            RBtn% = (MReg.AX AND 2) \ 2
            MBtn% = (MReg.AX AND 4) \ 4
            Count% = MReg.BX
            HPosn% = MReg.CX
            VPosn% = MReg.DX
        ELSE
            CALL MouseGetStatus(LBtn%, RBtn%, MBtn%, HPosn%, VPosn%)
            Count% = 0
        END IF
    ELSE
        LBtn% = 0
        RBtn% = 0
        MBtn% = 0
        Count% = 0
        HPosn% = -1
        VPosn% = -1
    END IF
END SUB

SUB MouseGetReleaseInfo (LBtn%, RBtn%, MBtn%, Count%, HPosn%, VPosn%)
    IF MouseReady% THEN
        IF (LBtn% OR RBtn% OR MBtn%) <> 0 THEN
            MReg.AX = 6
            IF LBtn% THEN MReg.BX = 0
            IF RBtn% THEN MReg.BX = 1
            IF MBtn% THEN MReg.BX = 2
            mousecall
            LBtn% = MReg.AX AND 1
            RBtn% = (MReg.AX AND 2) \ 2
            MBtn% = (MReg.AX AND 4) \ 4
            Count% = MReg.BX
            HPosn% = MReg.CX
            VPosn% = MReg.DX
        ELSE
            CALL MouseGetStatus(LBtn%, RBtn%, MBtn%, HPosn%, VPosn%)
            Count% = 0
        END IF
    ELSE
        LBtn% = 0
        RBtn% = 0
        MBtn% = 0
        Count% = 0
        HPosn% = -1
        VPosn% = -1
    END IF
END SUB

SUB MouseGetStatus (LBtn%, RBtn%, MBtn%, HPosn%, VPosn%)
    IF MouseReady% THEN
        MReg.AX = 3
        mousecall
        LBtn% = MReg.BX AND 1
        RBtn% = (MReg.BX AND 2) \ 2
        MBtn% = (MReg.BX AND 4) \ 4
        HPosn% = MReg.CX
        VPosn% = MReg.DX
    ELSE
        LBtn% = 0
        RBtn% = 0
        MBtn% = 0
        HPosn% = -1
        VPosn% = -1
    END IF
END SUB

SUB mousehide
    IF MouseReady% THEN
        MReg.AX = 2
        mousecall
    END IF
END SUB

FUNCTION mouseinit%
    DEF SEG = 0
    Sum% = 0
    FOR i% = &H33 * 4 TO &H33 * 4 + 3
        Sum% = Sum% + PEEK(i%)
    NEXT i%
    IF Sum% = 0 THEN
        mouseinit% = 0
        EXIT FUNCTION
    END IF
    MReg.AX = 0
    mousecall
    IF MReg.AX = 0 THEN
        mouseinit% = 0
    ELSE
        mouseinit% = MReg.BX
    END IF
END FUNCTION

SUB MouseReadCounters (HCount%, VCount%)
    IF MouseReady% THEN
        MReg.AX = 11
        mousecall
        HCount% = MReg.CX
        VCount% = MReg.DX
    END IF
END SUB

SUB MouseSetHorizRange (HMin%, HMax%)
    IF MouseReady% THEN
        MReg.AX = 7
        MReg.CX = HMin%
        MReg.DX = HMax%
        mousecall
    END IF
END SUB

SUB MouseSetPosn (HPosn%, VPosn%)
    IF MouseReady% THEN
        MReg.AX = 4
        MReg.CX = HPosn%
        MReg.DX = VPosn%
        mousecall
    END IF
END SUB

SUB MouseSetup
    RESTORE MouseInt
    DEF SEG = VARSEG(MouseRoutine%(0))
    Addr% = VARPTR(MouseRoutine%(0))
    FOR i = 0 TO 39
        READ a$
        POKE Addr% + i, VAL("&H" + a$)
    NEXT i
    IF a$ <> "CB" THEN ERROR 255
    DEF SEG
END SUB

SUB MouseSetVertRange (VMin%, VMax%)
    IF MouseReady% THEN
        MReg.AX = 8
        MReg.CX = VMin%
        MReg.DX = VMax%
        mousecall
    END IF
END SUB

SUB mouseshow
    IF MouseReady% THEN
        MReg.AX = 1
        mousecall
    END IF
END SUB

DEFINT A-Z
FUNCTION move
    IF ymn = 20 THEN
        tmr = TIMER
        DO
        LOOP WHILE TIMER < tmr + .02
        putf
        ymn = ymn - 1
        gets
        puts
    END IF
    IF TIMER >= tmr2 + 2 THEN putf: rslt: tmr2 = TIMER: gets: puts
    NCursor = 1 ' Andar
    ' ®Estamos en algun sitio?
    MouseGetStatus l%, r%, m%, x%, y%
    x% = x% \ 2
    NCursor = 1
    FOR i = 0 TO 10 ' las 11 PLACES que hay
        ' pl (n,x1,y1,x2,y2)
        IF x% + 5 > pl(i, 1) AND x% + 5 < pl(i, 3) AND y% + 5 > pl(i, 2) AND y% + 5 < pl(i, 4) THEN
            IF st = 0 THEN NCursor = 3 ELSE NCursor = 2
        END IF
    NEXT i
    IF x% > 230 THEN
        IF y% >= 140 AND y% <= 150 THEN IF in(1) = -1 THEN NCursor = 4
        IF y% >= 150 AND y% <= 160 THEN IF in(2) = -1 THEN NCursor = 4
        IF y% >= 160 AND y% <= 170 THEN IF in(3) = -1 THEN NCursor = 4
        IF y% >= 170 AND y% <= 180 THEN IF in(4) = -1 THEN NCursor = 4
    END IF
    xxx% = x%
    yyy% = y%
    IF yyy% > 179 THEN yyy% = 179
    IF xxx% > 299 THEN xxx% = 299
    GET (xxx%, yyy%)-(xxx% + 19, yyy% + 19), mbkgd(1)
    PUT (xxx%, yyy%), mmask(1, NCursor), AND
    PUT (xxx%, yyy%), mouse(1, NCursor), XOR
    DO
        MouseGetStatus l%, r%, m%, x%, y%
        x% = x% \ 2
        IF y% > 179 THEN y% = 179
        IF x% > 299 THEN x% = 299
        IF xxx% <> x% OR yyy% <> y% THEN
            PUT (xxx%, yyy%), mbkgd(1), PSET
            xxx% = x%
            yyy% = y%
            GET (xxx%, yyy%)-(xxx% + 19, yyy% + 19), mbkgd(1)
            PUT (xxx%, yyy%), mmask(1, NCursor), AND
            PUT (xxx%, yyy%), mouse(1, NCursor), XOR
        END IF
        xx% = x% \ 8
        yy% = y% \ 8
        ' ®Estamos en algun sitio?
        NCursor = 1
        np = 0
        FOR i = 0 TO 10 ' las 11 PLACES que hay
            ' pl (n,x1,y1,x2,y2)
            IF x% + 5 > pl(i, 1) AND x% + 5 < pl(i, 3) AND y% + 5 > pl(i, 2) AND y% + 5 < pl(i, 4) THEN
                IF st = 0 THEN NCursor = 3 ELSE NCursor = 2
                np = i
            END IF
        NEXT i
        ob = 0
        IF x% > 230 THEN
            IF y% >= 140 AND y% <= 150 THEN IF in(1) = -1 THEN NCursor = 4: ob = 1
            IF y% >= 150 AND y% <= 160 THEN IF in(2) = -1 THEN NCursor = 4: ob = 2
            IF y% >= 160 AND y% <= 170 THEN IF in(3) = -1 THEN NCursor = 4: ob = 3
            IF y% >= 170 AND y% <= 180 THEN IF in(4) = -1 THEN NCursor = 4: ob = 4
        END IF
        IF tester = 1 THEN LOCATE 1, 1: PRINT x%; y%: LOCATE 2, 1: PRINT xx%; yy%
        IF r% THEN
            IF st = 0 THEN st = 1 ELSE st = 0
            DO
                MouseGetStatus l%, r%, m%, x%, y%
                x% = x% \ 2
            LOOP WHILE r%
            IF NCursor <> 1 THEN
                IF NCursor = 3 THEN NCursor = 2 ELSE NCursor = 3
                PUT (xxx%, yyy%), mbkgd(1), PSET
                xxx% = x%
                yyy% = y%
                GET (xxx%, yyy%)-(xxx% + 19, yyy% + 19), mbkgd(1)
                IF yyy% > 179 THEN yyy% = 179
                IF xxx% > 299 THEN xxx% = 299
                PUT (xxx%, yyy%), mmask(1, NCursor), AND
                PUT (xxx%, yyy%), mouse(1, NCursor), XOR
            END IF
        END IF
        IF l% THEN
            ' ®Algun objeto de la 'zona-inventario'?
            IF ob <> 0 THEN
                move = 300 + ob
                PUT (xxx%, yyy%), mbkgd(1), PSET
                EXIT FUNCTION
            END IF
            p = 0
            IF st = 1 THEN p = 100
            ' ®NOS MOVEMOS?
            IF xx% <> xmn OR yy% <> ymn THEN GOSUB mover
            ' 'CLIC' en alguna 'PLACE'?
            IF np <> 0 THEN
                move = np + 100 + p
                PUT (xxx%, yyy%), mbkgd(1), PSET ' mousehide
                EXIT FUNCTION
            END IF
            ' ®Lleg¢ el mu§eco a alguna esquina de la pantalla?
            IF xmn <= 1 AND xx% <= 1 THEN move = 1: PUT (xxx%, yyy%), mbkgd(1), PSET: EXIT FUNCTION ' POR LA IZQ=1
            IF xmn >= 34 AND xx% >= 34 THEN move = 2: PUT (xxx%, yyy%), mbkgd(1), PSET: EXIT FUNCTION ' POR LA DER=2
            IF ymn <= 1 AND yy% <= 1 THEN move = 3: PUT (xxx%, yyy%), mbkgd(1), PSET: EXIT FUNCTION ' POR ARRIBA=3
            IF ymn >= 18 AND yy% >= 18 THEN move = 4: PUT (xxx%, yyy%), mbkgd(1), PSET: EXIT FUNCTION ' POR DEBAJO=4
        END IF
        ' ®Teclas especiales? (SAVE/LOAD, QUIT...)
        k$ = INKEY$
        k$ = UCASE$(k$)
        IF k$ = " " THEN move = 20: PUT (xxx%, yyy%), mbkgd(1), PSET: EXIT FUNCTION ' INVENTORY=20
        IF k$ = "S" THEN move = 10: PUT (xxx%, yyy%), mbkgd(1), PSET: EXIT FUNCTION ' SAVE=10
        IF k$ = "L" THEN move = 11: PUT (xxx%, yyy%), mbkgd(1), PSET: EXIT FUNCTION ' LOAD=11
        IF k$ = CHR$(27) THEN move = 12: PUT (xxx%, yyy%), mbkgd(1), PSET: EXIT FUNCTION ' ESC=12
    LOOP
mover:
    PUT (xxx%, yyy%), mbkgd(1), PSET ' mousehide
    yy% = yy% - 4
    DO
        IF tester = 1 THEN LOCATE 3, 1: PRINT xmn; ymn
        putf
        kxmn = xmn
        kymn = ymn
        IF xmn > xx% THEN
            IF nop(xmn - 1, ymn + 5) = 0 THEN xmn = xmn - 1: mira = 1
        END IF
        IF xmn < xx% THEN
            IF xmn > 39 - 5 THEN xmn = 39 - 5
            IF nop(xmn + 4, ymn + 5) = 0 THEN xmn = xmn + 1: mira = 2
        END IF
        IF ymn > yy% THEN
            IF nop(xmn, ymn + 5) = 0 AND nop(xmn + 1, ymn + 5) = 0 AND nop(xmn + 2, ymn + 5) = 0 AND nop(xmn + 3, ymn + 5) = 0 THEN ymn = ymn - 1: mira = 3
        END IF
        IF ymn < yy% THEN
            IF ymn > 19 THEN ymn = 19
            IF nop(xmn, ymn + 6) = 0 AND nop(xmn + 1, ymn + 6) = 0 AND nop(xmn + 2, ymn + 6) = 0 AND nop(xmn + 3, ymn + 6) = 0 THEN ymn = ymn + 1: mira = 4
        END IF
        gets
        psmn = psmn + 1
        IF psmn = 5 THEN psmn = 1
        puts
        FOR ig = 0 TO 5
            IF objac(ig) THEN
                IF (objwhere(ig, 2) \ 8) > ymn + 2 THEN
                    PUT (objwhere(ig, 1), objwhere(ig, 2)), objm(1, ig), AND
                    PUT (objwhere(ig, 1), objwhere(ig, 2)), objs(1, ig), XOR
                END IF
            END IF
        NEXT ig
        tmr = TIMER
        DO
        LOOP WHILE TIMER < tmr + .01
        WAIT &H3DA, 8: WAIT &H3DA, 8, 8
        ' ®Se ha movido?
        IF kxmn = xmn AND kymn = ymn THEN EXIT DO
    LOOP UNTIL (xmn = xx% OR xmn = stopat) AND ymn = yy%
    psmn = 2
    putf
    puts
    FOR ig = 0 TO 5
        IF objac(ig) THEN
            IF (objwhere(ig, 2) \ 8) > ymn + 2 THEN
                PUT (objwhere(ig, 1), objwhere(ig, 2)), objm(1, ig), AND
                PUT (objwhere(ig, 1), objwhere(ig, 2)), objs(1, ig), XOR
            END IF
        END IF
    NEXT ig
    MouseGetStatus l%, r%, m%, x%, y%
    xxx% = x% \ 2
    yyy% = y%
    IF yyy% > 179 THEN yyy% = 179
    IF xxx% > 299 THEN xxx% = 299
    GET (xxx%, yyy%)-(xxx% + 19, yyy% + 19), mbkgd(1)
    PUT (xxx%, yyy%), mmask(1, NCursor), AND
    PUT (xxx%, yyy%), mouse(1, NCursor), XOR
    RETURN
END FUNCTION

SUB pita (l, h, p)
    FOR i = l TO h STEP p
        SOUND i, .1
    NEXT i
END SUB

SUB ppreg (p1$, p2$, p3$, p4$)
    spgp 2, 16 * 8, 3, p1$
    spgp 2, 18 * 8, 3, p2$
    spgp 2, 20 * 8, 3, p3$
    spgp 2, 22 * 8, 3, p4$
END SUB

SUB ptlt
    tmr2 = TIMER
    LINE (4, 4)-(315, 28), 0, BF
    LINE (4, 4)-(315, 28), 2, B
END SUB

SUB ptpers
    PUT (psix, psiy), mpp(1), AND
    PUT (psix, psiy), spp(1), XOR
END SUB

SUB ptx (x, y, c, t$)
    xx = x
    FOR n = 1 TO LEN(t$)
        LOCATE 1, 1
        COLOR 8
        PRINT MID$(t$, n, 1)
        FOR x1 = 0 TO 7
            FOR y1 = 0 TO 16
                IF POINT(x1, y1) = 8 THEN PSET (xx + x1, y + y1), c
        NEXT y1, x1
        xx = xx + 8
        LOCATE 1, 1
        PRINT " "
    NEXT n
END SUB

SUB putf
    IF ymn < 0 THEN ymn = 0
    IF ymn > 19 THEN ymn = 19
    IF xmn < 0 THEN xmn = 0
    IF xmn > 34 THEN xmn = 34
    PUT (xmn * 8, ymn * 8), f(1), PSET
END SUB

SUB putobjs (n)
    FOR i = 1 TO n
        PUT (objwhere(i, 1), objwhere(i, 2)), objm(1, i), AND
        PUT (objwhere(i, 1), objwhere(i, 2)), objs(1, i), XOR
    NEXT i
END SUB

SUB puts
    'psmn,mira
    IF ymn < 0 THEN ymn = 0
    IF ymn > 19 THEN ymn = 19
    IF xmn < 0 THEN xmn = 0
    IF xmn > 34 THEN xmn = 34
    xmnn = xmn * 8
    ymnn = ymn * 8
    SELECT CASE mira
        CASE 2 ' Izquierda
            SELECT CASE psmn
                CASE 1
                    PUT (xmnn, ymnn), ms(0, 2), AND
                    PUT (xmnn, ymnn), sp(0, 2), XOR
                CASE 2, 4
                    PUT (xmnn, ymnn), ms(0, 1), AND
                    PUT (xmnn, ymnn), sp(0, 1), XOR
                CASE 3
                    PUT (xmnn, ymnn), ms(0, 3), AND
                    PUT (xmnn, ymnn), sp(0, 3), XOR
            END SELECT
        CASE 1 ' Derecha
            SELECT CASE psmn
                CASE 1
                    PUT (xmnn, ymnn), ms(0, 5), AND
                    PUT (xmnn, ymnn), sp(0, 5), XOR
                CASE 2, 4
                    PUT (xmnn, ymnn), ms(0, 4), AND
                    PUT (xmnn, ymnn), sp(0, 4), XOR
                CASE 3
                    PUT (xmnn, ymnn), ms(0, 6), AND
                    PUT (xmnn, ymnn), sp(0, 6), XOR
            END SELECT
        CASE 4 ' Arriba
            SELECT CASE psmn
                CASE 1, 3
                    PUT (xmnn, ymnn), ms(0, 7), AND
                    PUT (xmnn, ymnn), sp(0, 7), XOR
                CASE 2
                    PUT (xmnn, ymnn), ms(0, 8), AND
                    PUT (xmnn, ymnn), sp(0, 8), XOR
                CASE 4
                    PUT (xmnn, ymnn), ms(0, 9), AND
                    PUT (xmnn, ymnn), sp(0, 9), XOR
            END SELECT
        CASE 3 ' Abajo
            SELECT CASE psmn
                CASE 1, 3
                    PUT (xmnn, ymnn), ms(0, 10), AND
                    PUT (xmnn, ymnn), sp(0, 10), XOR
                CASE 2
                    PUT (xmnn, ymnn), ms(0, 11), AND
                    PUT (xmnn, ymnn), sp(0, 11), XOR
                CASE 2
                    PUT (xmnn, ymnn), ms(0, 12), AND
                    PUT (xmnn, ymnn), sp(0, 12), XOR
            END SELECT
    END SELECT
END SUB

FUNCTION qbut
    NCursor = 4
    i = 0
    MouseGetStatus l%, r%, m%, x%, y%
    xx% = x% \ 2
    yy% = y%
    IF yy% > 179 THEN yy% = 179
    IF xx% > 299 THEN xx% = 299
    GET (xx%, yy%)-(xx% + 19, yy% + 19), mbkgd(1)
    PUT (xx%, yy%), mmask(1, NCursor), AND
    PUT (xx%, yy%), mouse(1, NCursor), XOR
    DO
        MouseGetStatus l%, r%, m%, x%, y%
        LOCATE 1
        'PRINT x%, y%, l%, r%, m%
        x% = x% \ 2
        IF y% > 179 THEN y% = 179
        IF x% > 299 THEN x% = 299
        IF xx% <> x% OR yy% <> y% THEN
            PUT (xx%, yy%), mbkgd(1), PSET
            xx% = x%
            yy% = y%
            GET (xx%, yy%)-(xx% + 19, yy% + 19), mbkgd(1)
            PUT (xx%, yy%), mmask(1, NCursor), AND
            PUT (xx%, yy%), mouse(1, NCursor), XOR
        END IF
    LOOP WHILE l% = 0 AND r% = 0
    IF y% >= 179 THEN
        IF x% > 5 AND x% < 75 THEN i = 1
        IF x% > 85 AND x% < 155 THEN i = 2
        IF x% > 165 AND x% < 235 THEN i = 3
        IF x% > 235 AND x% < 315 THEN i = 4
    END IF
    PUT (xx%, yy%), mbkgd(1), PSET
    qbut = i
    DO
        MouseGetStatus l%, r%, m%, x%, y%
    LOOP WHILE l% OR r%
END FUNCTION

FUNCTION qresp
    NCursor = 4
    i = 0
    MouseGetStatus l%, r%, m%, x%, y%
    xx% = x% \ 2
    yy% = y%
    IF yy% > 179 THEN yy% = 179
    IF xx% > 299 THEN xx% = 299
    GET (xx%, yy%)-(xx% + 19, yy% + 19), mbkgd(1)
    PUT (xx%, yy%), mmask(1, NCursor), AND
    PUT (xx%, yy%), mouse(1, NCursor), XOR
    DO
        MouseGetStatus l%, r%, m%, x%, y%
        x% = x% \ 2
        IF y% > 179 THEN y% = 179
        IF x% > 299 THEN x% = 299
        IF xx% <> x% OR yy% <> y% THEN
            PUT (xx%, yy%), mbkgd(1), PSET
            xx% = x%
            yy% = y%
            GET (xx%, yy%)-(xx% + 19, yy% + 19), mbkgd(1)
            PUT (xx%, yy%), mmask(1, NCursor), AND
            PUT (xx%, yy%), mouse(1, NCursor), XOR
        END IF
    LOOP WHILE l% = 0 AND r% = 0
    IF y% > 22 * 8 AND y < 23 * 8 THEN i = 4
    IF y% > 20 * 8 AND y% < 21 * 8 THEN i = 3
    IF y% > 18 * 8 AND y% < 19 * 8 THEN i = 2
    IF y% > 16 * 8 AND y% < 17 * 8 THEN i = 1
    PUT (xx%, yy%), mbkgd(1), PSET
    qresp = i
    DO
        MouseGetStatus l%, r%, m%, x%, y%
    LOOP WHILE l% = 1 OR r% = 1
    
END FUNCTION

SUB rslt
    PUT (4, 4), lt(1), PSET
END SUB

SUB spclr
    FOR i = 0 TO 100
        LINE (0, i)-(320, i), 0
        LINE (0, 200 - i)-(320, 200 - i), 0
    NEXT
    CLS
END SUB

SUB spgp (x%, y%, c%, t$)
    FOR i = -1 TO 1
        FOR j = -1 TO 1
            IF i <> 0 OR j <> 0 THEN
                gprint x% + i, y% + j, 0, t$
            END IF
    NEXT j, i
    gprint x%, y%, c%, t$
END SUB

SUB starfield
    PALETTE 1, 9
    PALETTE 2, 14
    PALETTE 3, 7
    
    DIM x AS SINGLE
    DIM y AS SINGLE
    DIM CX AS SINGLE
    DIM cy AS SINGLE
    DIM contador AS SINGLE
    
    ldg 1, "cohete"
    ldg 2, "planeta"
    CLS
    StarNum% = 200
    LNum% = 3
    DIM StarX(StarNum%), StarY(StarNum%), layer(LNum%)
    layer(1) = -1
    layer(2) = -2
    layer(3) = -4
    FOR i% = 1 TO StarNum%
        StarX(i%) = (RND * 319) + 1
        StarY(i%) = (RND * 199) + 1
    NEXT i%
    contador = 20
    xx = -1
    x = -1
    y = -1
    CX = x
    cy = y
    DO
        c = INT(contador)
        IF c = 40 THEN x = 0: y = 150
        IF c > 40 AND c < 90 THEN y = y - 1.4: x = x + 1.4
        IF c > 89 AND c < 110 THEN y = y + 1: x = x + 2
        IF c > 110 AND c < 130 THEN x = x - .1: y = y - .01
        IF c > 130 AND c < 150 THEN x = x + .1: y = y + .1
        
        tmr = TIMER
        DO
        LOOP WHILE TIMER < tmr + .001
        WAIT &H3DA, 8: WAIT &H3DA, 8, 8
        
        IF c = 80 AND c < 110 THEN centra 2, "SURCANDO EL ESPACIO"
        IF c = 110 OR c = 150 THEN LOCATE 2, 1: PRINT SPACE$(40)
        IF c > 120 AND c < 150 THEN centra 2, "TE DIRIGES VELOZ HACIA TU DESTINO"
        IF c > 160 AND c < 190 THEN centra 2, "≠≠CUIDADO!!"
        IF xx >= 0 THEN LINE (xx, 98)-(xx + 40, 138), 0, BF
        IF c = 180 THEN xx = 279 ' 16 en 16
        IF c > 180 THEN xx = xx - 16
        IF xx >= 0 THEN PUT (xx, 98), objm(1, 2), AND: PUT (xx, 98), objs(1, 2), XOR
        IF xx > 0 AND xx <= 120 THEN
            FOR i = 1 TO 200
                SOUND RND * 100 + 100, .1
            NEXT i
            centra 2, "ME PARECE QUE HAS TENIDO UN"
            centra 3, "'ATERRIZAJE' FORZOSO..."
            SLEEP 2
            EXIT DO
        END IF
        contador = contador + .9
        CL = 1
        FOR i% = 1 TO StarNum%
            PSET (StarX(i%), StarY(i%)), 0
            IF i% > (StarNum% / LNum%) * CL THEN CL = CL + 1
            StarX(i%) = StarX(i%) + layer(CL)
            IF StarX(i%) < 1 THEN StarX(i%) = 319: StarY(i%) = RND * 199
            IF StarX(i%) > 319 THEN StarX(i%) = 1: StarY(i%) = RND * 199
            c% = CL
            PSET (StarX(i%), StarY(i%)), c%
        NEXT i%
        IF CX <> x OR cy <> y THEN
            LINE (CX, cy)-(CX + 39, cy + 39), 0, BF
            cy = y
            CX = x
        END IF
        IF x >= 0 AND y >= 0 THEN
            PUT (x, y), objm(1, 1), AND
            PUT (x, y), objs(1, 1), XOR
        END IF
    LOOP UNTIL LEN(INKEY$)
    
    
END SUB

SUB starodds

    ' Poner a 1 para debug,
    ' Poner a 0 para release.
    tester = 0
    ' Ya.

    SCREEN 1
    NCursor = 4
    PALETTE 2, 1
    PALETTE 1, 12
    PALETTE 3, 15
    BLOAD "DATA\lsadv.cga"
    wt
menu:
    '*** VARIABLES
    stopat = 999
    PiedraEstelar = 0
    ConocePiedra = 0
    LlavesSabe = 0
    Llaves = 0
    LlavesDonde = 0
    ToolsOpen = 0
    Aceite = 0
    Tool = 0
    Advertido = 0
    AdvertidoC = 0
    CaracolHabla = 0
    lodo = 0
    PozoAbierto = 0
    Martillo = 0
    pistolalaser = 0
    ' *** DEBUG
    ' GOTO DEBUG
    '***
    BLOAD "DATA\starodds.cga"
    'paleta
    PALETTE 0, 0
    PALETTE 2, 1
    PALETTE 1, 9
    PALETTE 3, 7
    but 1, "Intro"
    but 2, "Jugar"
    but 3, "Codigo"
    but 4, "Salir"
    DO
        i = qbut
        SELECT CASE (i)
            CASE 1
                GOTO intro
            CASE 2
                GOTO jugar
            CASE 3
                GOTO codigo
            CASE 4
                SCREEN 0
                WIDTH 80
                PRINT "(C) LS Adventures 1997/1998/2000"
                END
        END SELECT
    LOOP
intro:
    PALETTE 3, 11
    PALETTE 2, 3
    PALETTE 1, 1
    BLOAD "DATA\p00.cga"
    ldnop "p00.nop"
    xmn = 20
    ymn = 14
    gets
    puts
    gtlt
    ptlt
    objwhere(1, 1) = 45
    objwhere(1, 2) = 130
    cln
    ldg 1, "roca"
    PUT (45, 130), objm(1, 1), AND
    PUT (45, 130), objs(1, 1), XOR
    centra 2, "VOY A DAR UNA VUELTA"
    centra 3, "≠QUE ABURRIMIENTO!"
    pl(1, 1) = 45
    pl(1, 2) = 130
    pl(1, 3) = 84
    pl(1, 4) = 169
    DO
        i = move
        SELECT CASE i
            CASE 101 AND NOT PiedraEstelar
                rslt
                ptlt
                centra 2, "VAYA... PARECE UNA PIEDRA "
                centra 3, "ESTELAR. TODAVIA QUEMA UN POCO"
            CASE 201 AND NOT PiedraEstelar
                BLOAD "DATA\p00.cga"
                gets
                puts
                ptlt
                centra 2, "≠PUES PA'ENTRO!"
                centra 3, "®QUIEN SABE? ≠PODRIA SER UTIL!"
                PiedraEstelar = -1
            CASE 12
                GOTO menu
            CASE 11
                GOTO codigo
        END SELECT
        IF PiedraEstelar THEN
            NCursor = 4
            wt
            ptlt
            centra 2, "≠VAYA POR DONDE! PARECE QUE LA PIEDRA"
            centra 3, "TIENE UN MENSAJE ESCRITO."
            wt
            EXIT DO
        END IF
    LOOP
    ' El Mensaje
    PALETTE 1, 8
    PALETTE 2, 7
    PALETTE 3, 15
    BLOAD "DATA\texto.cga"
    wt
    PALETTE 3, 11
    PALETTE 2, 3
    PALETTE 1, 1
    BLOAD "DATA\p00.cga"
    gets
    puts
    gtlt
    ptlt
    PUT (45, 130), objm(1, 1), AND
    PUT (45, 130), objs(1, 1), XOR
    centra 2, "≠HOSTIA! LA LUNA ESA ESTA AQUI AL LADO"
    centra 3, "®QUE HAGO? ®VOY?"
    wt
    ptlt
    centra 2, "BUENO, VENGA, VOY. ESTOY TO ABURRIO."
    centra 3, "ADEMA HASE FARTA UNA GASHI QUE NO VEA."
    wt
    spclr
jugar:
salir:
    xmn = 30
    pv = 1
    ymn = 15
p1:
    PALETTE 1, 12
    PALETTE 2, 2
    PALETTE 3, 10
    RESTORE casa
    thyob
    BLOAD "DATA\casa.cga"
    ldnop "casa.nop"
    gets
    puts
    cln
    gtlt
    IF pv = 1 THEN
        ptlt
        centra 2, "VAYA. ®DONDE HABRE PUESTO LAS LLAVES"
        centra 3, "DE CASA? ≠HE DE COGER MI COHETE!"
        pv = 0
    END IF
    DO
        i = move
        SELECT CASE i
            CASE 1
                xmn = 33
                GOTO p2
            CASE 2, 4
                xmn = 33
                ymn = 15
                mira = 1
                GOTO p3
            CASE 101, 201
                IF NOT Llaves THEN
                    ptlt
                    centra 2, "LA PUERTA ESTA CERRADA CON LLAVES"
                    centra 3, "DEBEN HABERSEME CAIDO POR AHI"
                ELSE
                    ptlt
                    centra 2, "≠PAENTRO!"
                    centra 3, "(POR FIN)"
                    wt
                    ptlt
                    centra 2, "CODIGO:"
                    centra 3, "PEROLA"
                    SLEEP 2
                    GOTO fase2
                END IF
            CASE 102
                ptlt
                centra 2, "≠QUE ORGULLOSO ESTOY DE MI PIEDRAS"
                centra 3, "CULTIVADAS! ≠LAS MEJORES DEL BARRIO!"
            CASE 103
                ptlt
                centra 2, "NO SE PARA QUE TENGO UN BUZON MOLON"
                centra 3, "SI NADIE SABE DONDE VIVO..."
            CASE 12
                GOTO menu
            CASE 11
                GOTO codigo
            CASE 10
                ptlt
                centra 2, "ESTAS EN LA PRIMERA FASE."
                centra 3, "AQUI NO HAY PASSWORD."
        END SELECT
    LOOP
p2:
    RESTORE campo1
    PALETTE 1, 1
    PALETTE 2, 8
    PALETTE 3, 9
    BLOAD "DATA\campo1.cga"
    ldnop "campo1.nop"
    gets
    puts
    thyob
    gtlt
    DO
        i = move
        SELECT CASE i
            CASE 4
                ymn = 15
                xmn = 2
                mira = 2
                GOTO p3
            CASE 2
                IF ymn < 11 THEN ymn = 11
                xmn = 2
                GOTO p1
            CASE 101
                ptlt
                centra 2, "ESTA PIEDRA FEA SE CARGA TODO MI BO-"
                centra 3, "NITO JARDIN. TENGO QUE ARRANCARLA."
            CASE 201
                IF NOT ConocePiedra THEN
                    ConocePiedra = -1
                    ptlt
                    centra 2, "≠VAYA! ≠NO PUEDO CON ELLA!... ®EH?"
                    centra 3, "≠ESTA PIEDRA SE MUEVE!"
                    NCursor = 4
                    wt
                    ptlt
                    centra 2, "CODIGO:"
                    centra 3, "PIEDRA"
                    SLEEP 1
                END IF
                GOTO piedratalking
            CASE 102, 103
                ptlt
                centra 2, "MIS PRECIOSAS"
                centra 3, "PIEDRAS CULTIVADAS"
            CASE 12
                GOTO menu
            CASE 11
                GOTO codigo
            CASE 10
                ptlt
                centra 2, "ESTAS EN LA PRIMERA FASE."
                centra 3, "AQUI NO HAY PASSWORD."
        END SELECT
    LOOP
piedratalking:
    IF LlavesDonde THEN GOTO yatapiedra
    ConocePiedra = -1
    BLOAD "DATA\piera.cga"
    PALETTE 1, 8
    PALETTE 2, 7
    PALETTE 3, 15
    pita 2000, 4000, 10
    centra 2, "≠EH, COLEGA! ®QUE INTENTAS HACERME?"
    centra 3, "≠DEJAME EN PAZ!"
    NCursor = 4
    ppreg "ER... YO YA ME IBA", "NADA, NADA. SOLO ESTABA MIRANDO", "ARRANCARTE DE MI JARDIN, PIEDRA FEA", ""
    xmn = 29
    ymn = 15
    DO
        i = qresp
        IF i = 1 THEN GOTO p2
        IF i = 2 THEN GOTO piedra2
        IF i = 3 THEN GOTO piedramal
    LOOP
piedramal:
    BLOAD "DATA\campo1.cga"
    PALETTE 1, 1
    PALETTE 2, 8
    PALETTE 3, 9
    xmn = 29
    ymn = 15
    gets
    puts
    gtlt
    ptlt
    centra 2, "CON QUE SI, ®EH?"
    centra 3, "≠≠TOMA!!"
    SLEEP 2
    rslt
    PALETTE 1, 10
    PALETTE 2, 14
    PALETTE 3, 7
    p = 1
    FOR j = 1 TO 10
        IF p = 1 THEN PALETTE 3, 7 ELSE PALETTE 3, 15
        IF p = 1 THEN p = 2 ELSE p = 1
        FOR i = 105 TO 166
            LINE (204, 140)-(319, i), INT(RND * 3) + 1
            'LINE (204, 140)-(319, 166), 1
            'LINE (319, 105)-(319, 166), 1
        NEXT i
        pita 3000, 4000, 100
    NEXT j
    BLOAD "DATA\campo1.cga"
    FOR j = 1 TO 10
        IF p = 1 THEN PALETTE 3, 7 ELSE PALETTE 3, 15
        IF p = 1 THEN p = 2 ELSE p = 1
        FOR i = 105 TO 166
            LINE (204, 140)-(319, i), INT(RND * 3) + 1
            'LINE (204, 140)-(319, 166), 1
            'LINE (319, 105)-(319, 166), 1
        NEXT i
        pita 3000, 4000, 100
        WAIT &H3DA, 8: WAIT &H3DA, 8, 8
    NEXT j
    BLOAD "DATA\campo1.cga"
    PALETTE 1, 1
    PALETTE 2, 8
    PALETTE 3, 9
    ptlt
    centra 2, "UN IMBECIL MENOS"
    centra 3, "≠JA!"
    NCursor = 4
    wt
    GOTO menu
piedra2:
    BLOAD "DATA\piera.cga"
    centra 2, "®Y QUE MIRAS?"
    ppreg "TU CARA BONITA.", "ES QUE HE PERDIDO LAS LLAVES DE MI CASA.", "!!UNA PIEDRA QUE HABLA!!", ""
    DO
        i = qresp
        IF i = 1 OR i = 3 THEN GOTO dspiedra
        IF i = 2 THEN LlavesSabe = -1: GOTO piedra3
    LOOP
dspiedra:
    BLOAD "DATA\piera.cga"
    centra 1, "BUENO, ME PRESENTO: SOY LA PIEDRA"
    centra 2, "ECONEJOR. SEGUN VEO, PUEDO PENSAR. CREO"
    centra 3, "QUE TIENE QUE VER CON LAS CAGADAS RADIO-"
    centra 4, "ACTIVAS DE LAS CABRAS COSMICAS..."
    ppreg "PUES YO CREO QUE ERES UNA CAGADA COSMICA", "!NO ME LO PUEDO CREER!", "ES MUY POSIBLE", "ME DA IGUAL. TE VOY A ARRANCAR"
    DO
        i = qresp
        IF i = 1 OR i = 4 THEN GOTO piedramal
        IF i = 2 THEN GOTO piedracasimal
        IF i = 3 THEN GOTO piedra4
    LOOP
piedracasimal:
    BLOAD "DATA\piera.cga"
    pita 8000, 1000, -100
    centra 2, "®ME ESTAS LLAMANDO MENTIROSO?"
    ppreg "SOLO ESTABA IMPRESIONADO", "!VADE RETRO! !ERES UNA ALUCINACION!", "!AQUI NO HAY CABRAS COSMICAS! !FANTASMA!", "TU MISMO"
    DO
        i = qresp
        IF i = 1 THEN GOTO piedra4
        IF i = 2 OR i = 3 OR i = 4 THEN GOTO piedramal
    LOOP
piedra3:
    BLOAD "DATA\piera.cga"
    pita 8000, 1000, -100
    centra 2, "ERES UN MALEDUCADO"
    centra 3, "AL MENOS PODRIAS PRESENTARTE"
    ppreg "SOY ULULUGA, REPRESENTANTE DE ENERYANSE", "!NO PUEDE SER! !UNA PIEDRA QUE HABLA!", "®A UNA MIERDA DE PIEDRA?... !NO!", ""
    DO
        i = qresp
        IF i = 1 THEN GOTO piedra4
        IF i = 2 THEN GOTO piedracasimal
        IF i = 3 THEN GOTO piedramal
    LOOP
piedra4b:
    centra 2, "®QUE BUSCAS?"
    ppreg "LAS LLAVES DE MI CASA", "MONTANAZ DE CANUTOZ", "", ""
    DO
        i = qresp
        IF i = 2 THEN centra 2, "≠TU NO ERES QWIDI! ®QUE BUSCAS?"
        IF i = 1 THEN LlavesSabe = -1: GOTO piedra4
    LOOP
    DO
    LOOP
piedra4:
    BLOAD "DATA\piera.cga"
    IF LlavesSabe = 0 THEN GOTO piedra4b
    centra 2, "RESPECTO A TUS LLAVES... CREO"
    centra 3, "HABER VISTO UNAS EN EL RIACHUELO"
    centra 4, "QUE HAY POR AQUI CERCA"
    LlavesDonde = -1
    ppreg "GRACIAS, ECONEJOR.", "", "", ""
    wt
    BLOAD "DATA\piera.cga"
    centra 2, "CODIGO:"
    centra 3, "POTOPO"
    GOTO p2
yatapiedra:
    BLOAD "DATA\piera.cga"
    PALETTE 1, 8
    PALETTE 2, 7
    PALETTE 3, 15
    centra 3, "HOLA, 'OMPARE"
    ppreg "HOLA, ECONEJOR.", "", "", ""
    wt
    GOTO p2
p3:
    BLOAD "DATA\rio.cga"
    PALETTE 1, 5
    PALETTE 2, 9
    PALETTE 3, 3
    ldnop "rio.nop"
    gets
    puts
    cln
    gtlt
    RESTORE rio
    thyob
    IF LlavesDonde AND NOT Llaves THEN
        pl(2, 1) = 224
        pl(2, 2) = 147
        pl(2, 3) = 264
        pl(2, 4) = 187
        objwhere(1, 1) = 224
        objwhere(1, 2) = 147
        ldg 1, "llavecin"
        PUT (224, 147), objm(1, 1), AND
        PUT (224, 147), objs(1, 1), XOR
    END IF
    DO
        i = move
        SELECT CASE i
            CASE 1
                ymn = 18
                xmn = 16
                mira = 3
                GOTO p2
            CASE 2
                ymn = 17
                xmn = 16
                mira = 3
                GOTO p1
            CASE 101
                ptlt
                centra 2, "AQUI CRECEN SALVAJES TODAS ESTAS"
                centra 3, "PIEDRAS. LAS MIAS SON MEJORES"
            CASE 102
                ptlt
                centra 2, "≠SON MIS LLAVES!"
                centra 3, "≠LAS ENCONTRE!"
            CASE 202
                BLOAD "DATA\rio.cga"
                gets
                puts
                ptlt
                cln
                centra 2, "UF... POR FIN PODRE ENTRAR EN CASA"
                centra 3, "GRACIAS A ECONEJOR... MENOS MAL."
                Llaves = -1
            CASE 12
                GOTO menu
            CASE 11
                GOTO codigo
        END SELECT
    LOOP
fase2:
    invcl
    xmn = 16
    ymn = 16
    RESTORE tuhabitacion
    thyob
Fase2a:
    PALETTE 1, 1
    PALETTE 2, 2
    PALETTE 3, 12
    cln
    o = 1
    objwhere(1, 1) = 40
    objwhere(1, 2) = 148
    objwhere(2, 1) = 179
    objwhere(2, 2) = 54
    IF ToolsOpen = -1 THEN
        ldg 1, "tools2"
    ELSE
        ldg 1, "tools1"
    END IF
    IF Aceite = 0 THEN
        ldg 2, "oil"
        o = 2
    ELSE
        pl(2, 1) = 0
        pl(2, 2) = 0
        pl(2, 3) = 0
        pl(2, 4) = 0
    END IF
    ldg 3, "llavein"
    BLOAD "DATA\tucasa.cga"
    ldnop "tucasa.nop"
    invpr
    putobjs o
    gtlt
    gets
    puts
    DO
        i = move
        SELECT CASE i
            CASE 101
                ptlt
                centra 2, "ES MI CAJA DE HERRAMIENTAS"
                IF ToolsOpen = -1 THEN
                    centra 3, "AHORA ESTA ABIERTA"
                ELSE
                    centra 3, "PARECE QUE ESTA UN POCO ATASCADA"
                END IF
            CASE 201
                ptlt
                IF ToolsOpen = -1 THEN
                    IF Tool = -1 THEN
                        centra 2, "YA COGI ANTES LA LLAVE"
                        centra 3, "NO NECESITO NADA MAS"
                    ELSE
                        centra 2, "CREO QUE LO UNICO QUE TIENE VALOR"
                        centra 3, "ES ESTA LLAVE INGLESA."
                        Tool = -1
                        in(3) = -1
                        i$(3) = "LLAVE"
                        invpr
                    END IF
                ELSE
                    centra 2, "ESTA CERRADA Y NO SE PUEDE ABRIR:"
                    centra 3, "LAS BISAGRAS DEBEN ESTAR OXIDADAS."
                END IF
            CASE 102
                ptlt
                centra 2, "UN FRASCO CON ACEITE LUBRICANTE"
                centra 3, "Æ87 EN 1Ø HECHO CON MOCO DE ABEJA."
            CASE 202
                Aceite = -1
                in(4) = -1
                i$(4) = "ACEITE"
                GOTO Fase2a
            CASE 103
                ptlt
                centra 2, "ES MI COHETE SUPERSONICO"
                centra 3, "AUN NECESITA ALGUNOS AJUSTES"
            CASE 203
                ptlt
                centra 2, "NO FUNCIONA."
                centra 3, "TENGO QUE AJUSTARLO UN POCO."
            CASE 304
                IF in(4) = -1 THEN
                    i = invsl("ACEITE")
                    IF i = 1 THEN
                        ptlt
                        centra 2, "CON EL ACEITE SE AFLOJAN LAS BISAGRAS"
                        centra 3, "Y ABRES LA CAJA DE HERRAMIENTAS"
                        SLEEP 2
                        ToolsOpen = -1
                        GOTO Fase2a
                    ELSE
                        ptlt
                        centra 2, "EL ACEITE"
                        centra 3, "NO HACE NADA AHI"
                    END IF
                END IF
            CASE 303
                IF in(3) = -1 THEN
                    i = invsl("LLAVE")
                    IF i = 3 THEN
                        ptlt
                        centra 2, "≠PERFECTO!"
                        centra 3, "EL COHETE ESTA LISTO PARA PARTIR"
                        NCursor = 4
                        wt
                        ptlt
                        centra 2, "CODIGO:"
                        centra 3, "BOCATA"
                        SLEEP 1
                        GOTO fase3
                    ELSE
                        ptlt
                        centra 2, "®QUE HACES CON LA LLAVE"
                        centra 3, "EN ESE LUGAR?"
                    END IF
                END IF
            CASE 12
                GOTO menu
            CASE 11
                GOTO codigo
        END SELECT
    LOOP
codigo:
    BLOAD "DATA\starodds.cga"
    PALETTE 2, 8
    PALETTE 1, 1
    PALETTE 3, 3
    LINE (50, 50)-(270, 150), 0, BF
    LINE (50, 50)-(270, 150), 3, B
    centra 7, "INTRODUCE CODIGO"
    LOCATE 11, 17
    PRINT "˙˙˙˙˙˙"
    x = 17
    c$ = ""
    DO
        LOCATE 11, x
        PRINT CHR$(3)
        DO
            k$ = INKEY$
            IF k$ = CHR$(8) THEN EXIT DO
            k$ = UCASE$(k$)
        LOOP UNTIL (k$ >= "A" AND k$ <= "Z")
        IF k$ = CHR$(8) THEN
            IF LEN(c$) <> 0 THEN
                LOCATE 11, x
                PRINT "˙"
                x = x - 1
                c$ = LEFT$(c$, LEN(c$) - 1)
            END IF
        ELSE
            LOCATE 11, x
            PRINT k$
            c$ = c$ + k$
            x = x + 1
        END IF
        DO
        LOOP WHILE INKEY$ <> ""
    LOOP WHILE x < 23
    IF c$ = "PIEDRA" THEN GOTO piedratalking
    IF c$ = "POTOPO" THEN ConocePiedra = -1: LlavesDonde = -1: xmn = 29: ymn = 15: GOTO p2
    IF c$ = "PEROLA" THEN GOTO fase2
    IF c$ = "BOCATA" THEN GOTO fase3
    IF c$ = "MOJONO" THEN pistolalaser = -1: GOTO ElCastillo
    IF c$ = "PEJONO" THEN GOTO ElCastillo
    centra 14, "CODIGO ERRONEO"
    SLEEP 1
    GOTO menu
fase3:
    CLS
    starfield
    cln
    invcl
    ymn = 10
    xmn = 7
    CLS
    centra 11, "TRAS UNOS MINUTOS"
    centra 12, "LOGRAS PONERTE EN PIE"
    SLEEP 2
    i$(1) = ""
    i$(2) = ""
    i$(3) = ""
    i$(4) = ""
    in(1) = 0
    in(2) = 0
    in(3) = 0
    in(4) = 0
f3a:
    RESTORE crater
    PALETTE 0, 0
    PALETTE 1, 9
    PALETTE 2, 12
    PALETTE 3, 10
    BLOAD "DATA\planet1.cga"
    ldnop "planet1.nop"
    thyob
    invpr
    cln
    gtlt
    objwhere(1, 1) = 50
    objwhere(1, 2) = 150
    ldg 1, "caraco"
    putobjs 1
    gets
    puts
    DO
        i = move
        SELECT CASE i
            CASE 101, 201
                IF ymn < 1 THEN
                    IF xmn < 7 THEN xmn = 7
                    mira = 4
                    GOTO f3b
                ELSE
                    ptlt
                    centra 2, "POR ALLI HAY UN DESFILADERO"
                    centra 3, "NO HAY QUE ANDAR MUCHO."
                END IF
            CASE 102
                IF Advertido THEN
                    ptlt
                    centra 2, "TU AMIGO EL CARACOL AMARGADO"
                    centra 3, "TE SALUDA CON LA CABEZA."
                ELSE
                    ptlt
                    centra 2, "PARECE UN CARACOL BASTANTE"
                    centra 3, "AMARGADO DE LA VIDA."
                END IF
            CASE 202
                IF AdvertidoC THEN
                    ptlt
                    centra 2, "TU AMIGO EL CARACOL AMARGADO"
                    centra 3, "PARECE NO TENER GANAS DE HABLAR"
                ELSE
                    ptlt
                    centra 2, "EL CARACOL TE SALUDA DANDOTE LOS"
                    centra 3, "BUENOS DIAS..."
                    wt
                    GOTO Caracol
                END IF
            CASE 103
                ptlt
                centra 2, "'ESO' QUE HAY AHI ES LO QUE QUEDA"
                centra 3, "DE TU POBRE NAVE..."
                wt
                ptlt
                IF Martillo THEN
                    centra 2, "Y TODAVIA NO HABIAS TERMINADO"
                    centra 3, "DE PAGARLA... BUFFFF"
                ELSE
                    centra 2, "QUIZA SE HAYA SALVADO ALGUN"
                    centra 3, "DISPOSITIVO QUE ME SIRVA..."
                END IF
                wt
                rslt
                putf
                gets
                puts
                invpr
            CASE 203
                IF Martillo THEN
                    ptlt
                    centra 2, "NO HAY NADA MAS..."
                ELSE
                    Martillo = -1
                    ptlt
                    centra 2, "≠VAYA! ≠SE HA SALVADO UN DISPOSITIVO"
                    centra 3, "DE LA NAVE! ≠UN MARTILLO!"
                    wt
                    rslt
                    putf
                    gets
                    puts
                    i$(3) = "MARTILLO"
                    in(3) = -1
                    invpr
                END IF
            CASE 303
                i = invsl("MARTILLO")
                IF i = 2 THEN
                    ptlt
                    centra 2, "INTENTAS CARGARTE AL CARACOL DE UN"
                    centra 3, "MARTILLAZO..."
                    wt
                    ptlt
                    centra 2, "PERO EL MARTILLO REBOTA CONTRA LA"
                    centra 3, "CASCARA Y TE ROMPE LA CABEZA..."
                    wt
                    GOTO menu
                END IF
                IF i = 3 THEN
                    ptlt
                    centra 2, "LA NAVE ESTA YA BASTANTE ROTA"
                    centra 3, "NO CREO QUE LA PUEDAS ROMPER MAS"
                END IF
                IF i = 999 OR i = 1 THEN
                    ptlt
                    centra 2, "ESO NO ES MUY UTIL AQUI"
                    centra 3, "PRUEBA OTRA COSA"
                END IF
                wt
                rslt
                putf
                gets
                puts
            CASE 304
                i = invsl("LASER")
                IF i = 2 THEN
                    ptlt
                    centra 2, "INTENTAS CARGARTE AL CARACOL DE UN"
                    centra 3, "DISPARO CON EL LASER..."
                    wt
                    ptlt
                    centra 2, "PERO EL LASER REBOTA CONTRA LA CASCARA"
                    centra 3, "Y TE HACE UN BONITO AGUJERO EN LA CARA"
                    wt
                    GOTO menu
                END IF
                IF i = 3 THEN
                    ptlt
                    centra 2, "LA NAVE ESTA YA BASTANTE ROTA"
                    centra 3, "NO CREO QUE LA PUEDAS ROMPER MAS"
                END IF
                IF i = 999 OR i = 1 THEN
                    ptlt
                    centra 2, "ESO NO ES MUY UTIL AQUI"
                    centra 3, "PRUEBA OTRA COSA"
                END IF
                wt
                rslt
                putf
                gets
                puts
            CASE 4
                ymn = 14
                xmn = 2
                mira = 2
                GOTO f3c
            CASE 12
                GOTO menu
            CASE 11
                GOTO codigo
        END SELECT
    LOOP
f3b:
    RESTORE desfiladero
    PALETTE 0, 0
    PALETTE 1, 10
    PALETTE 2, 4
    PALETTE 3, 12
    BLOAD "DATA\desfilad.cga"
    ldnop "desfilad.nop"
    thyob
    invpr
    cln
    IF NOT pistolalaser THEN
        ldg 1, "laser"
    ELSE
        ldg 1, "blank"
        pl(2, 1) = 999
        pl(2, 2) = 999
        pl(2, 3) = 999
        pl(2, 4) = 999
    END IF
    IF PozoAbierto THEN
        ldg 2, "pozo2"
    ELSE
        ldg 2, "pozo1"
    END IF
    objwhere(1, 1) = 180
    objwhere(1, 2) = 40
    objwhere(2, 1) = 61
    objwhere(2, 2) = 135
    putobjs 2
    gtlt
    gets
    puts
    DO
        i = move
        SELECT CASE i
            CASE 101, 201
                IF ymn < 1 THEN
                    xmn = 8
                    mira = 4
                    GOTO f3a
                ELSE
                    ptlt
                    centra 2, "POR ALLI ESTA EL CRATER DONDE ME"
                    centra 3, "HE ESTRELLADO..."
                END IF
            CASE 102
                ptlt
                centra 2, "≠VAYA! UNA ESTUPENDA PISTOLA LASER"
                centra 3, "ULTIMO MODELO, Y NUEVA"
                wt
                rslt
                putf
                gets
                puts
            CASE 202
                ptlt
                centra 2, "COGES LA PISTOLA PA TI PA SIEMPRE"
                centra 3, "ESTA REPLETA DE ENERGIA."
                wt
                pistolalaser = -1
                i$(4) = "LASER"
                in(4) = -1
                GOTO f3b
            CASE 103, 203
                IF PozoAbierto = 0 THEN
                    ptlt
                    centra 2, "ES UN POZO DE LODO CUBIERTO. DESPIDE"
                    centra 3, "UN HEDOR ASQUEROSO."
                    wt
                    rslt
                    putf
                    gets
                    puts
                ELSE
                    IF Advertido = 0 THEN
                        ptlt
                        centra 2, "≠TE VAS A CAER!"
                        wt
                        rslt
                        putf
                        gets
                        puts
                        Advertido = -1
                    ELSE
                        ptlt
                        centra 2, "TE CAES EN EL LODO Y TE PRINGAS ENTERO"
                        centra 3, "≠APESTAS!"
                        lodo = -1
                        wt
                        rslt
                        putf
                        gets
                        puts
                    END IF
                END IF
            CASE 303
                i = invsl("MARTILLO")
                IF i = 3 THEN
                    IF PozoAbierto = 0 THEN
                        ptlt
                        centra 2, "DE UN MARTILLAZO TE CARGAS LA CU-"
                        centra 3, "BIERTA DEL POZO."
                        PozoAbierto = -1
                        wt
                        GOTO f3b
                    ELSE
                        ptlt
                        centra 2, "LA CUBIERTA DEL POZO YA ESTA ROTA"
                        centra 3, "TE SALPICAS DE LODO CON EL MARTILLO"
                        wt
                        rslt
                        putf
                        gets
                        puts
                        lodo = -1
                    END IF
                END IF
                IF i = 999 OR i = 1 OR i = 2 THEN
                    ptlt
                    centra 2, "ESO NO ES MUY UTIL AQUI"
                    centra 3, "PRUEBA OTRA COSA"
                END IF
                wt
                rslt
                putf
                gets
                puts
            CASE 304
                i = invsl("LASER")
                IF i = 3 THEN
                    ptlt
                    centra 2, "UN DISPARO CONTRA EL INDEFENSO"
                    centra 3, "LODO NO CAUSA NINGÈN EFECTO."
                END IF
                IF i = 999 OR i = 1 OR i = 2 THEN
                    ptlt
                    centra 2, "ESO NO ES MUY UTIL AQUI"
                    centra 3, "PRUEBA OTRA COSA"
                END IF
                wt
                rslt
                putf
                gets
                puts
            CASE 4
                ymn = 14
                xmn = 2
                mira = 2
                GOTO f3c
            CASE 12
                GOTO menu
            CASE 11
                GOTO codigo
        END SELECT
    LOOP
f3c:
    RESTORE castillo
    BLOAD "DATA\castillo.cga"
    ldnop "castillo.nop"
    thyob
    invpr
    cln
    PALETTE 0, 0
    PALETTE 1, 1
    PALETTE 2, 12
    PALETTE 3, 7
    ''
    ' Cambiar esto.
    'psix = 115
    'psiy = 114
    'ldpers "soldado"
    'ptpers
    'psix = 137
    'psiy = 130
    'ptpers
    ''
    GET (158, 138)-(206, 186), sp2(0, 5)
    PUT (115, 122), ms2(0, 0), AND
    PUT (115, 122), sp2(0, 0), XOR
    PUT (158, 138), ms2(0, 1), AND
    PUT (158, 138), sp2(0, 1), XOR

    gtlt
    ptlt
    centra 2, "≠ALTO! NO TE ACERQUES MAS"
    centra 3, "O TE HAREMOS MUCHA PUPA"
    gets
    puts
    IF lodo THEN

        FOR i% = 1 TO 4
            PUT (158, 138), sp2(0, 5), PSET
            PUT (158, 138), ms2(0, i%), AND
            PUT (158, 138), sp2(0, i%), XOR
            SLEEP 1
        NEXT i%

        ptlt
        centra 2, "≠≠QUE PESTE TRAE ESE TIO!!"
        centra 3, "VAMONOS, QUE ME ASFIXIO"
        SLEEP 4
        BLOAD "DATA\castillo.cga"
        invpr
        gets
        puts
        ptlt
        centra 2, "CODIGO:"
        IF pistolalaser THEN
            centra 3, "MOJONO"
        ELSE
            centra 3, "PEJONO"
        END IF
        SLEEP 3
        GOTO ElCastillo
    END IF
    DO
        stopat = 8
        i = move
        SELECT CASE i
            CASE 1
                stopat = 999
                ymn = 16
                xmn = 16
                mira = 3
                GOTO f3a
            CASE 102
                ptlt
                centra 2, "SON UNOS SOLDADOS CON CARA DE MALA"
                centra 3, "HOSTIA, MUY AGRESIVOS"
            CASE 202
                ptlt
                centra 2, "MEJOR QUE NO TE METAS CON ELLOS"
            CASE 303
                i = invsl("MARTILLO")
                IF i = 2 THEN
                    ptlt
                    centra 2, "INTENTAS AGREDIR A LOS GUARDIAS"
                    centra 3, "CON TU PRECIOSO MARTILLO"
                    wt
                    GOTO TeDisparan
                END IF
                IF i = 999 OR i = 1 OR i = 3 THEN
                    ptlt
                    centra 2, "ESO NO ES MUY UTIL AQUI"
                    centra 3, "PRUEBA OTRA COSA"
                END IF
                wt
                rslt
                putf
                gets
                puts
            CASE 303
                i = invsl("MARTILLO")
                IF i = 2 THEN
                    ptlt
                    centra 2, "SACAS TU SUPERPISTOLA PARA CAR-"
                    centra 3, "GARTE A LOS GUARDIAS..."
                    wt
                    GOTO TeDisparan
                END IF
                IF i = 999 OR i = 1 OR i = 3 THEN
                    ptlt
                    centra 2, "ESO NO ES MUY UTIL AQUI"
                    centra 3, "PRUEBA OTRA COSA"
                END IF
                wt
                rslt
                putf
                gets
                puts
            CASE 12
                GOTO menu
            CASE 11
                GOTO codigo
        END SELECT
        IF xmn > 7 THEN ' Disparos
            GOTO TeDisparan
        END IF
    LOOP
TeDisparan:
    xxx1 = xmn * 8 + 16
    yyy1 = ymn * 8 + 16
    ptlt
    centra 2, "≠TE LO ADVERTIMOS!"
    centra 3, "≠≠FUEGO!!"
    SLEEP 2
    FOR i = 0 TO 5
        FOR j = 0 TO 3
            LINE (xxx1, yyy1)-(123, 150), j
            LINE (xxx1, yyy1)-(145, 165), j
            pita 1000, 10000, 500
            pita 10000, 1000, -500
        NEXT j
    NEXT i
    GOTO menu
Caracol:
    NCursor = 4
    CLS
    PALETTE 0, 0
    PALETTE 1, 4
    PALETTE 2, 13
    PALETTE 3, 15
    BLOAD "DATA\caracol.cga"
    pita 1000, 1500, 10
    pita 1500, 1000, -10
    pita 1000, 10000, 500
    pita 10000, 1000, -500
    ppreg "QUE TE OCURRE, CARACOL?", "TE VEO MALA CARA, CARACOL", "TE PARECES AL SNAIL DEL SEGA", "NOS VEMOS OTRO SIGLO, COLEGA"
    DO
        i = qresp
        IF i = 4 THEN CLS : GOTO f3a
        IF i = 1 OR i = 2 THEN GOTO HablarCaracol
        IF i = 3 THEN EXIT DO
    LOOP
    BLOAD "DATA\caracol.cga"
    ptlt
    centra 2, "SI, ES QUE ANTES TRABAJABA EN"
    centra 3, "ESE JUEGO... AHORA VIVO AQUI."
    ppreg "QUE TE OCURRE, CARACOL?", "TE VEO MALA CARA, CARACOL", "", "NOS VEMOS OTRO SIGLO, COLEGA"
    DO
        i = qresp
        IF i = 4 THEN CLS : GOTO f3a
        IF i = 1 OR i = 2 THEN GOTO HablarCaracol
    LOOP
HablarCaracol:
    BLOAD "DATA\caracol.cga"
    spgp 10, 20, 2, "ES POR CULPA DE ESE STOJOKORIEN. VIVE"
    spgp 10, 30, 2, "AQUI DESDE HACE MUCHO, PERO ULTIMA-"
    spgp 10, 40, 2, "MENTE NO NOS DEJAN EN PAZ LOS GRITOS"
    spgp 10, 50, 2, "DE UNA CHICA QUE SE HA TRAIDO A SU"
    spgp 10, 60, 2, "CASTILLO."
    ppreg "PUEDES INFORMARME SOBRE ESO?", "", "", ""
    DO
        i = qresp
        IF i = 1 OR i = 2 OR i = 3 OR i = 4 THEN EXIT DO
    LOOP
QuieroSaber:
    BLOAD "DATA\caracol.cga"
    spgp 10, 20, 2, "Y QUE QUIERES SABER, PRINGUE?"
    ppreg "DIME ALGO SOBRE LA CHICA", "DIME ALGO SOBRE EL CASTILLO", "DIME ALGO SOBRE STOJOKORIEN", "BUENO, NO TENGO MAS TIEMPO... ADIOS"
    DO
        i = qresp
        IF i = 1 THEN GOTO AlgoChica
        IF i = 2 THEN GOTO AlgoCastillo
        IF i = 3 THEN GOTO AlgoStojokorien
        IF i = 4 THEN GOTO f3a
    LOOP
AlgoChica:
    BLOAD "DATA\caracol.cga"
    spgp 10, 20, 2, "LA CHICA... UUUUF! ESTA BUENISIMA"
    spgp 10, 30, 2, "CREO QUE VIENE DE UN LEJANO PLANETA DE"
    spgp 10, 40, 2, "LA GALAXIA DE MOCO FINO, Y QUE ES UNA"
    spgp 10, 50, 2, "PRINCESA O ALGO ASI. SE TIRA TODAS LAS"
    spgp 10, 60, 2, "LAS NOCHES LLORANDO Y NO DEJA DORMIR A"
    spgp 10, 70, 2, "NADIE. ULTIMAMENTE HA ESTADO TIRANDO"
    spgp 10, 80, 2, "PELUOS ESTELARES CON MENSAJES A VER SI"
    spgp 10, 90, 2, "ALGUIEN VIENE A RESCATARLA."
    spgp 10, 105, 2, "TU TIENES PINTA DE SER DE LOS TIPICOS"
    spgp 10, 115, 2, "IMBECILES QUE ESTAN TAN ABURRIDOS "
    spgp 10, 125, 2, "COMO PARA IR DE SALVADOR DE DAMISELAS."
    spgp 10, 135, 2, "SUERTE, RAMBITO."
    wt
    GOTO QuieroSaber
AlgoCastillo:
    BLOAD "DATA\caracol.cga"
    spgp 10, 20, 2, "EL CASTILLO ES UNA ESPECIE DE FORTA-"
    spgp 10, 30, 2, "LEZA QUE STOJOKORIEN SE MONTO AQUI"
    spgp 10, 40, 2, "EN LA LUNA TIRITH. EL UNICO ACCESO"
    spgp 10, 50, 2, "ES POR LA PUERTA PRINCIPAL PERO ESTA"
    spgp 10, 60, 2, "PROTEGIDA POR DOS GUARDIANES QUE "
    spgp 10, 70, 2, "TIENEN MUY MALA HOSTIA."
    spgp 10, 85, 2, "TIENE A LA PRINCESA EN LO ALTO DE LA"
    spgp 10, 95, 2, "TORRE MAS ALTA, PORQUE ANTES LA TENIA"
    spgp 10, 105, 2, "EN UNA TORRE MAS BAJA Y LA TIA SE"
    spgp 10, 115, 2, "TIRABA POR LA VENTANA PARA ESCAPARSE"
    spgp 10, 125, 2, "LA PEDAZO DE ANIMAL."
    wt
    GOTO QuieroSaber
AlgoStojokorien:
    BLOAD "DATA\caracol.cga"
    spgp 10, 20, 2, "STOJOKORIEN ES UN SUPERVILLANO CON"
    spgp 10, 30, 2, "AFANES DE CONQUISTA DEL UNIVERSO, "
    spgp 10, 40, 2, "PERO COMO TIENE MUY POCO DINERO HA"
    spgp 10, 50, 2, "OPTADO POR RAPTAR A LA CHICA ESTA"
    spgp 10, 60, 2, "A VER SI LE DAN UNA BUENA RECOM-"
    spgp 10, 70, 2, "PENSA. HA ENVIADO UN CORREO AL PLA-"
    spgp 10, 80, 2, "NETA DEL QUE PROCEDE LA GACHI PARA"
    spgp 10, 90, 2, "COMUNICARSELO A SU PARE EL REY, PE-"
    spgp 10, 100, 2, "RO EL TRAFICO ESTA TAN MALO QUE "
    spgp 10, 110, 2, "TODAVIA NO HA RECIBIDO RESPUESTA."
    wt
    GOTO QuieroSaber
ElCastillo:
    xmn = 15
    ymn = 19
    
    CLS
    RESTORE Salon
    BLOAD "DATA\salon.cga"
    ldnop "salon.nop"
    thyob
    cln
    PALETTE 0, 0
    PALETTE 1, 5
    PALETTE 2, 12
    PALETTE 3, 7
    invcl
    gtlt
    gets
    puts
    DO
        i = move
        SELECT CASE i
            CASE 101
                ptlt
                centra 2, "PARECE UNA ESPECIE DE MECANISMO"
                centra 3, "QUE NO SERIA DIFICIL ACCIONAR..."
            CASE 201
                ptlt
                centra 2, "A VER SI... UN MOMENTO..."
                centra 3, "UFFF... CREO QUE YA... VALE."
                wt
                GOTO TheGreatStrojokorien
            CASE 12
                GOTO menu
            CASE 11
                GOTO codigo
        END SELECT
    LOOP
TheGreatStrojokorien:
    ' Esto es el final del jueguecito.
    CLS
    PALETTE 1, 3
    PALETTE 2, 11
    PALETTE 3, 15
    BLOAD "DATA\fin1.CGA"
    pita 1000, 8000, 100
    pita 8000, 1000, 25
    spgp 10, 20, 2, "QUE HACES AQUI, INTRUSO? COMO"
    spgp 10, 30, 2, "OSAS ENTRAR SIN INVITACION EN"
    spgp 10, 40, 2, "LOS APOSENTOS DE STROJOKORIEN?"
    wt
    BLOAD "DATA\fin1.CGA"
    spgp 10, 20, 2, "!!PREPARATE A MORIR!!"
    IF NOT pistolalaser THEN
        t# = TIMER
        c1 = 0
        c2 = 0
        c3 = 0
        c4 = 0
        DO
            PALETTE 0, c1
            PALETTE 1, c2
            PALETTE 2, c3
            PALETTE 3, c4
            c1 = c1 + 1
            IF c1 = 16 THEN c1 = 0: c2 = c2 + 1: IF c2 = 16 THEN c2 = 0: c3 = c3 + 1: IF c3 = 16 THEN c3 = 0: c3 = c3 + 1: IF c3 = 16 THEN c3 = 0
            WAIT &H3DA, 8
        LOOP WHILE TIMER < t# + 2
        CLS
        PALETTE 0, 0
        PALETTE 1, 8
        PALETTE 2, 7
        PALETTE 3, 15
        centra 10, "ESTAS MUERTO"
        centra 11, "®PERO A QUIEN SE LE OCURRE"
        centra 12, "IR DESARMADO?"
        wt
        GOTO menu
    END IF
    BLOAD "DATA\fin1.CGA"
    ptlt
    centra 2, "EN UN MOMENTO TE SACAS TU"
    centra 3, "PEASO PISTOLA LASER..."
    wt
    BLOAD "DATA\fin1.cga"
    spgp 10, 20, 2, "!HOSTIA! ESTE LA TIENE MAS GRANDE"
    spgp 10, 30, 2, "QUE YO!!"
    spgp 10, 45, 2, "ESTA BIEN, NO TE MATARE DE MOMENTO,"
    spgp 10, 55, 2, "PERO DIME A QUE VIENES."
    ppreg "A HACERTE UN TRATO", "A JUGAR CON TU PLAYSTATION, MOLON", "A RESCATAR A LA PRINCESA", "A MATARTE, CAPULLO"
    DO
        qi = qresp
        SELECT CASE qi
            CASE 1, 2, 4
                BLOAD "DATA\fin1.cga"
                spgp 10, 20, 2, "OK. Y YO ESTABA HACIENDO TIEMPO"
                spgp 10, 30, 2, "PARA PREPARAR MI DISPOSITIVO"
                spgp 10, 40, 2, "ASESINO. !ADIOS!"
                wt
                BLOAD "DATA\fin1.cga"
                FOR i = 0 TO 100 STEP 10
                    LINE (100 - i, 100 - i)-(219 + i, 100 + i), INT(RND * 3) + 1, BF
                    t# = TIMER
                    WHILE t# + .05 > TIMER
                WEND
            NEXT i
            CLS
            centra 11, "MUERES DE UNA FORMA TAN HORRIBLE"
            centra 12, "QUE ME CENSURARIAN EL JUEGO SI"
            centra 13, "TE LA CUENTO."
            wt
            GOTO menu
    END SELECT
LOOP WHILE qi <> 3
BLOAD "DATA\fin1.cga"
spgp 10, 20, 2, "!ANDA! !PUES HABERLO DICHO ANTES!"
spgp 10, 30, 2, "YO QUE ESTABA DESEANDO DE LIBRARME"
spgp 10, 40, 2, "DE ESA CAFRE..."
wt
BLOAD "DATA\fin1.cga"
spgp 10, 20, 2, "TE CUENTO: YO QUERIA CASARME CON LA"
spgp 10, 30, 2, "GACHI, PERO LA TIA SE PUSO TAN PE-"
spgp 10, 40, 2, "SADA CON ESO DE QUE SE QUERIA ES-"
spgp 10, 50, 2, "CAPAR QUE ME HARTE DE ELLA."
wt
BLOAD "DATA\fin1.cga"
spgp 10, 20, 2, "ENTONCES DECIDI MANDARLA A PASEO,"
spgp 10, 30, 2, "PERO ENTONCES EL MOCO COSMICO QUE"
spgp 10, 40, 2, "HABITA EN ALGUN LUGAR DEL CASTI-"
spgp 10, 50, 2, "LLO SE ENAMORO DE ELLA Y NO LA"
spgp 10, 60, 2, "SUELTA."
wt
BLOAD "DATA\fin1.cga"
spgp 10, 20, 2, "SI TU ERES CAPAZ DE QUITARSELA, YA"
spgp 10, 30, 2, "SABES... YO ME VOY A JUGAR AL TOMB"
spgp 10, 40, 2, "RAIDER. !ADIOS!"
wt
DEBUG:
CLS
ptlt
centra 2, "CAMINAS POR LOS PASILLOS HASTA"
centra 3, "QUE OYES UNA VOZ FEMENINA."
wt
CLS
PALETTE 1, 14
PALETTE 2, 12
PALETTE 3, 15
BLOAD "DATA\fin3.CGA"
spgp 10, 20, 1, "!AH! POR FAVOR, RESCATAME DEL MOCO"
wt
BLOAD "DATA\fin3.CGA"
spgp 10, 20, 3, "TRANQUI. AQUI TIENES A ULULUGA QUE"
spgp 10, 30, 3, "TE SALVARA IPSO-FACTO"
wt
BLOAD "DATA\fin3.cga"
spgp 10, 20, 1, "ESO ESPERO, PORQUE ALLI LLEGA EL"
spgp 10, 30, 1, "CAQUILLON..."
wt
BLOAD "DATA\fin3.cga"
CLS
PALETTE 1, 10
PALETTE 2, 12
BLOAD "DATA\fin2.cga"
pita 400, 8000, 50
spgp 10, 20, 1, "WWWEUUISAHKKKEKRKASKK D KSLADKOWOI"
spgp 10, 30, 1, "SDPAOIW SDKLKSLAK AKLSKDLLKS SAKLD"
spgp 10, 45, 3, "(QUE, TRADUCIDO, QUIERE DECIR:"
spgp 10, 55, 3, "HEY, CAPULLO, QUE HACES CON MI "
spgp 10, 65, 3, "PRINCESA NTAGWENIZIMA?)"
wt
BLOAD "DATA\fin2.cga"
ptlt
centra 2, "≠AHORA! TIENES QUE SELECCIONAR"
centra 3, "UN ARMA DE ENTRE LO QUE LLEVAS"
LINE (10, 60)-(309, 189), 0, BF
LINE (10, 60)-(309, 189), 2, B
centra 12, "PISTOLA LASER MASTER"
centra 13, "COMIDA DEL BUCHE"
centra 14, "QUITAGRAPAS OXIDADO"
centra 15, "ROLLO DE CELOFAN"
centra 16, "BOCADILLO A MEDIAS"
centra 17, "CLEENEX SIN USAR"
centra 18, "BUJIA GASTADA"
centra 19, "DISKETTE CON VIRUS"
centra 20, "EL Nß 3 DE SPIDERMAN"
centra 21, "UNA TIRITA USADA"
centra 22, "RESTOS DE PUS"
centra 9, "INVENTARIO"
centra 10, "=========="
DO
    wt
    MouseGetStatus l%, r%, m%, x%, y%
    IF y% > 88 THEN
        opcion% = (y% - 88) \ 8
        SELECT CASE opcion%
            CASE 0
                BLOAD "DATA\fin2.CGA"
                ptlt
                centra 2, "TU RAYO ATRAVIESA AL MOCO PERO NO"
                centra 3, "LE HACE NADA..."
                wt
                GOTO GameOverMocoso
            CASE 1
                BLOAD "DATA\fin2.cga"
                ptlt
                centra 2, "EL MOCO SE COME LA COMIDA Y ESTA"
                centra 3, "AUMENTA SU TAMA•O Y PODER!"
                wt
                GOTO GameOverMocoso
            CASE 2
                BLOAD "DATA\fin2.cga"
                ptlt
                centra 2, "AL INTENTAR SACAR EL QUITAGRAPAS"
                centra 3, "TE PINCHAS Y TE QUEDAS GROGUI"
                wt
                GOTO GameOverMocoso
            CASE 3
                BLOAD "DATA\fin2.cga"
                ptlt
                centra 2, "AL SACARLO SE TE QUEDA PEGADO EL"
                centra 3, "TIPICO RESTILLO AL DEDO..."
                wt
                GOTO GameOverMocoso
            CASE 4
                BLOAD "DATA\fin2.cga"
                ptlt
                centra 2, "EL MOCO SE COME LA COMIDA Y êSTA"
                centra 3, "AUMENTA SU TAMA•O Y PODER!"
                wt
                GOTO GameOverMocoso
            CASE 6
                BLOAD "DATA\fin2.cga"
                ptlt
                centra 2, "®PERO QUE MIERDA VAS A HACER CON "
                centra 3, "UNA BUJIA GASTADA?"
                wt
                GOTO GameOverMocoso
            CASE 7
                BLOAD "DATA\fin2.cga"
                ptlt
                centra 2, "EL MOCO SE SACA UN CD DEL PANDA"
                centra 3, "ANTIVIRUS Y TE MACHACA."
                wt
                GOTO GameOverMocoso
            CASE 8
                BLOAD "DATA\fin2.cga"
                ptlt
                centra 2, "≠≠NO ES MOMENTO DE PONERSE A LEER!!"
                wt
                GOTO GameOverMocoso
            CASE 9
                BLOAD "DATA\fin2.cga"
                ptlt
                centra 2, "≠MIERDA, QUE ASCO! EL MOCO CRECE"
                centra 3, "CON ESAS COSAS."
                wt
                GOTO GameOverMocoso
            CASE 10
                BLOAD "DATA\fin2.cga"
                ptlt
                centra 2, "LOS RESTOS DE PUS PASAN A FORMAR"
                centra 3, "PARTE DEL MOCO GORDO."
                wt
                GOTO GameOverMocoso
        END SELECT
    END IF
LOOP WHILE opcion% <> 5
CLS
PALETTE 1, 1
PALETTE 2, 9
BLOAD "DATA\fin4.CGA"
spgp 10, 20, 2, "!BUENA IDEA, ULULUGA! LO HAS DE-"
spgp 10, 30, 2, "JADO FUERA DE COMBATE."
wt
BLOAD "DATA\fin4.cga"
spgp 10, 20, 3, "BUENO, AHORA EN AGRADECIMIENTO"
spgp 10, 30, 3, "TU SERAS MI NOVIA, NO?"
wt
BLOAD "DATA\fin4.cga"
spgp 10, 20, 2, "BUENO... ESO TE LO DIGO CUANDO"
spgp 10, 30, 2, "ME HAYAS LLEVADO A MI PLANETA."
wt
BLOAD "DATA\fin4.cga"
spgp 10, 20, 3, "ESTA BIEN. !VAMONOS!"
wt
CLS
centra 12, "FIN"
SLEEP 1
centra 12, "(POR AHORA)"
SLEEP 1
CLS
GOTO menu
GameOverMocoso:
CLS
centra 12, "EL MOCO TE DESTROZA"
wt
CLS
PALETTE 1, 14
PALETTE 2, 12
BLOAD "DATA\fin3.CGA"
spgp 10, 20, 1, "NO SE POR QUE PERO ME LO ESPE-"
spgp 10, 30, 1, "RABA... PERO BUENO, A VER SI VIENE"
spgp 10, 40, 1, "UNO MAS GUAPO..."
wt
GOTO menu
END SUB

SUB thyob
    FOR i = 1 TO 10
        pl(i, 1) = 0
        pl(i, 2) = 0
        pl(i, 3) = 0
        pl(i, 4) = 0
    NEXT i
    READ numero
    FOR i = 1 TO numero
        READ pl(i, 1), pl(i, 2), pl(i, 3), pl(i, 4)
        IF tester = 1 THEN
            LINE (pl(i, 1), pl(i, 2))-(pl(i, 3), pl(i, 4)), 1, B
            LOCATE 1 + pl(i, 2) \ 8, 1 + pl(i, 1) \ 8: PRINT LTRIM$(STR$(i))
        END IF
    NEXT i
    IF tester = 1 THEN wt
END SUB

SUB wt
    DO
        MouseGetStatus l%, r%, m%, x%, y%
        GET (x% \ 2, y%)-(x% \ 2 + 19, y% + 19), mbkgd(1)
    LOOP WHILE l% OR r% OR INKEY$ <> ""
    xx% = x% \ 2
    yy% = y%
    IF yy% > 179 THEN yy% = 179
    IF xx% > 299 THEN xx% = 299
    PUT (xx%, yy%), mmask(1, NCursor), AND
    PUT (xx%, yy%), mouse(1, NCursor), XOR
    DO
        MouseGetStatus l%, r%, m%, x%, y%
        x% = x% \ 2
        IF tester = 1 THEN LOCATE 1, 1: PRINT x%; y%: LOCATE 2, 1: PRINT xx%; yy%
        IF y% > 179 THEN y% = 179
        IF x% > 299 THEN x% = 299
        IF xx% <> x% OR yy% <> y% THEN
            PUT (xx%, yy%), mbkgd(1), PSET
            xx% = x%
            yy% = y%
            GET (xx%, yy%)-(xx% + 19, yy% + 19), mbkgd(1)
            PUT (xx%, yy%), mmask(1, NCursor), AND
            PUT (xx%, yy%), mouse(1, NCursor), XOR
        END IF
    LOOP UNTIL l% OR r% OR INKEY$ <> ""
    PUT (xx%, yy%), mbkgd(1), PSET
    DO
        MouseGetStatus l%, r%, m%, x%, y%
    LOOP WHILE l% OR r% OR INKEY$ <> ""
END SUB

