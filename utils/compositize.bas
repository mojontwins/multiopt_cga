' Compositize v0.1 20171121
' Copyright 2017 by The Mojon Twins

#include "file.bi"
#include "fbpng.bi"
#include "fbgfx.bi"
#include once "crt.bi"

#define RGBA_R( c ) ( CUInt( c ) Shr 16 And 255 )
#define RGBA_G( c ) ( CUInt( c ) Shr  8 And 255 )
#define RGBA_B( c ) ( CUInt( c )        And 255 )
#define RGBA_A( c ) ( CUInt( c ) Shr 24         )

Sub usage
	Print "$ compositize.exe in.png pal.png out.bsv"
End Sub

Sub OutputLine (outF As Integer, imgOut As Any Ptr, y As Integer)
	Dim As uByte d
	Dim As Integer x, i
	For x = 0 To 639 Step 8
		d = 0
		For i = 0 To 7
			If Point (x+i, y, imgOut) Then
				d = d Or (1 Shl (7-i))
			End If
		Next i
		Put #outF, , d
	Next x
End Sub

Dim As Any Ptr imgIn, imgOut
Dim As Integer pal (15), i, j, x, y, w, h, c, cpal, outF
Dim As uByte d
Dim As Integer white

If Command (3) = "" Then usage: End

screenres 640, 480, 32, , -1

' Read palette:
imgIn = png_load (Command (2))
For i = 0 To 15: pal (i) = Point (i, 0, imgIn): Next i
ImageDestroy imgIn

' Open in
imgIn = png_load (Command (1))
If ImageInfo (imgIn, w, h, , , , ) Then
	Puts "Error loading image " & Command (1)
	System
End If
	
' Create out
imgOut = ImageCreate (w*4, h, 0)

white = RGBA (255, 255, 255, 255)

' Convert
For y = 0 To h - 1
	For x = 0 To w - 1
		cpal = 0
		c = Point (x, y, imgIn)
		For i = 0 To 15
			If pal (i) = c Then cpal = i
		Next i
		If (cpal And 8) = 8 Then Pset imgOut, (x*4, y), white
		If (cpal And 4) = 4 Then Pset imgOut, (x*4+1, y), white
		If (cpal And 2) = 2 Then Pset imgOut, (x*4+2, y), white
		If (cpal And 1) = 1 Then Pset imgOut, (x*4+3, y), white
	Next x
Next y

' Save as BSV (memory copy)

png_save Command (3) & ".png", imgOut

outF = FreeFile
Open Command (3) For Binary As #outF

' QB Header
d = &HFD: Put #outF, , d

' Segment = B800, Offset = 0000, Length = 4000. LSB then MSB.
d = &H00: Put #outF, , d
d = &HB8: Put #outF, , d
d = &H00: Put #outF, , d
d = &H00: Put #outF, , d
d = &H00: Put #outF, , d
d = &H40: Put #outF, , d

' Raw data (binary)

' Even
For y = 0 To h - 2 Step 2
	OutputLine outF, imgOut, y
Next y

' Fill 192 bytes
d = 0: For i = 0 To 191: Put #outF, , d: Next i

' Odd
For y = 1 To h - 1 Step 2
	OutputLine outF, imgOut, y
Next y

' Fill 192 bytes
d = 0: For i = 0 To 191: Put #outF, , d: Next i

close outF
