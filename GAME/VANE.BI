DECLARE SUB VANE.Init (sv%, cmd%, na%, tos%, tb$, wh%, mb%, ml%)
DECLARE SUB VANE.assignChapterFilename (chapter%, fileName$)
DECLARE SUB VANE.loadBinary ()
DECLARE SUB VANE.titleBar ()
DECLARE SUB VANE.runGameFrom (ch%)
DECLARE FUNCTION VANE.runChapter (chapter AS INTEGER) AS INTEGER
DECLARE FUNCTION VANE.ReadByte () AS INTEGER
DECLARE FUNCTION VANE.ReadVByte () AS INTEGER
DECLARE FUNCTION VANE.ReadAddress () AS INTEGER
