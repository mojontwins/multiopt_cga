DECLARE FUNCTION LoadGIF% (BYVAL MemPointer%, BYVAL FileSeg%, BYVAL FileOfs%, BYVAL PScrType%, BYVAL PScrOffset%, BYVAL PScrWidth%, BYVAL XRes%, BYVAL YRes%, BYVAL X0%, BYVAL Y0%, BYVAL X1%, BYVAL Y1%, BYVAL XOrg%, BYVAL YOrg%, BYVAL AdaptorType%,  _
BYVAL PalIgnore%, BYVAL PalSeg%, BYVAL PalOfs%, PalColors%)
DECLARE FUNCTION ErrorReport% ()
DECLARE FUNCTION LoadGIFMem% ()
DECLARE SUB ShowGIFAt (XOrg%, YOrg%, a$)
DECLARE SUB ShowGIF (a$)                        
