'$Dynamic
Screen _NewImage(640, 480, 32)
Type Vec2
    As Long X, Y
End Type
Dim As Long VerticalScrollOffset, HorizontalScrollOffset, VerticalCharsVisible, HorizontalCharsVisible
Dim As Double ScrollOffset, ScrollOffsetVelocity
Dim As Long I
Dim As Vec2 TextDrawOffset, Cursor(0), SelectionStart, SelectionEnd
TextDrawOffset.X = 0
TextDrawOffset.Y = 1

Dim Shared FontWidth, FontHeight: FontWidth = 9: FontHeight = 18
_Font _LoadFont("arialbd.ttf", 16)

Dim Shared FILE_STATUS(0)
Dim Shared FILE$(0), FILE_CURSOR(0)
Dim Shared FILE_PARSED$(0)

Dim Shared TEDI_CFG
TEDI_CFG = FreeFile
ParseConfig

For I = 1 To _CommandCount
    INFILE$ = Command$(I)
    If _FileExists(INFILE$) = 0 Then INFILE$ = _StartDir$ + "\" + INFILE$
    If _FileExists(INFILE$) = 0 Then _Continue
    ParseFile INFILE$
Next I
If UBound(FILE_STATUS) = 0 Then ParseFile "Untitled.txt"

ScrollOffset = 0
iFile = 1
Color -1, 0

Do
    Cls , 0
    _Limit 60
    VerticalCharsVisible = _Height \ FontHeight - TextDrawOffset.Y
    HorizontalCharsVisible = _Width \ FontWidth - TextDrawOffset.X + 1
    OMW = MW: LMW = 0: MW = 0
    While _MouseInput: LMW = _MouseWheel: MW = IIF(LMW, IIF(Sgn(MW) = Sgn(LMW), MW + LMW, LMW), MW): Wend
    If MW Then ScrollOffsetVelocity = 4 * MW Else ScrollOffsetVelocity = ScrollOffsetVelocity - Sgn(ScrollOffsetVelocity) / 4
    ScrollOffset = ScrollOffset + ScrollOffsetVelocity
    If ScrollOffset < 0 Then ScrollOffset = 0 And ScrollOffsetVelocity = 0
    For I = ScrollOffset \ FontHeight To ScrollOffset \ FontHeight + VerticalCharsVisible + 1
        P1 = ListLongGet(FILE_PARSED$(iFile), I)
        P2 = ListLongGet(FILE_PARSED$(iFile), I + 1) - 3
        L$ = Mid$(FILE$(iFile), P1, P2 - P1 + 1)
        _PrintString (TextDrawOffset.X * FontWidth, Int(I) * FontHeight - ScrollOffset), Mid$(L$, HorizontalScrollOffset, HorizontalCharsVisible)
    Next I
    _Display
Loop Until Inp(&H60) = 1
System

Sub ParseConfig
    Open "tedi.cfg" For Binary As #TEDI_CFG
    Close #TEDI_CFG
End Sub
Sub SaveConfig
    Open "tedi.cfg" For Binary As #TEDI_CFG
    Close #TEDI_CFG
End Sub
Sub ParseFile (F$)
    __F = FreeFile
    iFile = UBound(FILE$) + 1
    ReDim _Preserve FILE$(1 To iFile)
    If _FileExists(F$) Then
        Open F$ For Binary As #__F
        FILE$(iFile) = String$(LOF(__F), 0)
        Get #__F, , FILE$(iFile)
        Close #__F
    End If
    ReDim _Preserve FILE_STATUS(1 To iFile)
    FILE_STATUS(iFile) = 1
    'Parsing File
    ReDim _Preserve FILE_PARSED$(1 To iFile)
    FILE_PARSED$(iFile) = ListLongNew$
    P = 0
    Do
        ListLongAdd FILE_PARSED$(iFile), P + 1 + Sgn(P)
        P = InStr(P + 1, FILE$(iFile), Chr$(13) + Chr$(10))
    Loop While P
End Sub
Function IIF (C, E1, E2)
    IIF = -(C <> 0) * E1 - (C = 0) * E2
End Function
Function Max (A, B)
    Max = -A * (A > B) - B * (A <= B)
End Function
Function Min (A, B)
    Min = -A * (A < B) - B * (A >= B)
End Function
Function inRange (A, B, C)
    inRange = (A <= B) And (B <= C)
End Function
Function inBox (X1, Y1, X, Y, X2, Y2)
    inBox = (X1 <= X) And (X <= X2) And (Y1 <= Y) And (Y <= Y2)
End Function
'$Include:'ListLong.bas'
