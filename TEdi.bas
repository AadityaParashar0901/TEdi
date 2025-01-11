'$Dynamic
Screen _NewImage(640, 480, 32)
Type Vec2
    As Long X, Y
End Type
Dim Shared As Long VerticalScrollOffset, HorizontalScrollOffset, VerticalCharsVisible, HorizontalCharsVisible
Dim Shared As Double ScrollOffset, ScrollOffsetVelocity
Dim As Long I
Dim Shared As Vec2 TextDrawOffset, Cursor(0), SelectionStart(0), SelectionEnd(0)
TextDrawOffset.X = 5
TextDrawOffset.Y = 2

Dim Shared FontWidth, FontHeight: FontWidth = 9: FontHeight = 18
Const TABSIZE = 4

Dim Shared FILE_STATUS(0), FILE_NAMES$(0)
Dim Shared FILE$(0), CURSORPOSITION(0) As _Unsigned Long
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
Dim Shared iFile As Long
iFile = 1
Color -1, 0

Do
    Cls , 0
    _Limit 60
    VerticalCharsVisible = _Height \ FontHeight - TextDrawOffset.Y - 1
    HorizontalCharsVisible = _Width \ FontWidth - TextDrawOffset.X - 3
    CheckKeyBinds
    CheckMouse
    '---Draw Menu & File Tabs---
    Line (0, 0)-(_Width, 15), _RGB32(31), BF
    _PrintString (0, 0), " File  Edit <- These buttons are broken ;)"
    Line (0, 15)-(_Width, 31), _RGB32(31), BF
    Line (0, 15)-(_Width, 15), _RGB32(127), BF
    For I = iFile - 3 To iFile + 3
        If inRange(LBound(FILE_NAMES$), I, UBound(FILE_NAMES$)) = 0 Then _Continue
        Line (_Width / 2 + (I - iFile) * 56 - 28, 16)-(_Width / 2 + (I - iFile) * 56 + 28, 31), _RGB32(63), BF
        _PrintString (_Width / 2 + (I - iFile) * 56 - Len(FILE_NAMES$(I)) / 2 * FontWidth, 15), FILE_NAMES$(I)
    Next I
    '---------------------------
    '-----------Draw------------
    Line (TextDrawOffset.X * FontWidth, 32)-(TextDrawOffset.X * FontWidth, _Height), _RGB32(127)
    For I = ScrollOffset To Min(ListLongLength(FILE_PARSED$(iFile)), ScrollOffset + VerticalCharsVisible)
        P1 = ListLongGet(FILE_PARSED$(iFile), I)
        P2 = ListLongGet(FILE_PARSED$(iFile), I + 1) - 3
        LINENUMBER$ = _Trim$(Str$(I + 1))
        L$ = Mid$(FILE$(iFile), P1, P2 - P1 + 1)
        _PrintString ((TextDrawOffset.X - Len(LINENUMBER$)) * FontWidth, Int(I - ScrollOffset + TextDrawOffset.Y) * FontHeight), LINENUMBER$
        If I = Cursor(iFile).Y Then DrawCursorX = (TABSIZE - 1) * CountChars(L$, 9) + Cursor(iFile).X
        Replace L$, Chr$(9), Space$(TABSIZE)
        K = 0: For J = HorizontalScrollOffset To Min(Len(L$), HorizontalScrollOffset + HorizontalCharsVisible)
            _PrintString ((TextDrawOffset.X + K) * FontWidth, Int(I - ScrollOffset + TextDrawOffset.Y) * FontHeight), Mid$(L$, J, 1)
            K = K + 1
        Next J
    Next I
    '---------------------------
    '----------Cursor-----------
    If Timer(0.1) - Int(Timer) > 0.5 Then _PrintString ((DrawCursorX + TextDrawOffset.X) * FontWidth, Int(Cursor(iFile).Y - ScrollOffset + TextDrawOffset.Y) * FontHeight), "_"
    '---------------------------
    '-------Status Bar----------
    Line (0, _Height - 10)-(_Width, _Height), _RGB32(63), BF
    '---------------------------
    _Display
Loop Until Inp(&H60) = 1
System

Sub CheckKeyBinds
    Key$ = InKey$
    If Len(Key$) = 1 Then
        Select Case Asc(Key$)
            Case 9, 32 To 127: FILE$(iFile) = Left$(FILE$(iFile), CURSORPOSITION(iFile) - 1) + Key$ + Mid$(FILE$(iFile), CURSORPOSITION(iFile))
                CURSORPOSITION(iFile) = CURSORPOSITION(iFile) + 1
                Cursor(iFile).X = Cursor(iFile).X + 1
            Case 13: FILE$(iFile) = Left$(FILE$(iFile), CURSORPOSITION(iFile) - 1) + Chr$(13) + Chr$(10) + Mid$(FILE$(iFile), CURSORPOSITION(iFile))
                CURSORPOSITION(iFile) = CURSORPOSITION(iFile) + 2
                Cursor(iFile).X = 1
                Cursor(iFile).Y = Cursor(iFile).Y + 1
            Case 8: If Mid$(FILE$(iFile), CURSORPOSITION(iFile) - 2, 2) = Chr$(13) + Chr$(10) Then
                    FILE$(iFile) = Left$(FILE$(iFile), CURSORPOSITION(iFile) - 3) + Mid$(FILE$(iFile), CURSORPOSITION(iFile))
                    CURSORPOSITION(iFile) = CURSORPOSITION(iFile) - 2
                    Cursor(iFile).X = ListLongGet(FILE_PARSED$(iFile), Cursor(iFile).Y)
                    Cursor(iFile).Y = Cursor(iFile).Y - 1
                    Cursor(iFile).X = Cursor(iFile).X - ListLongGet(FILE_PARSED$(iFile), Cursor(iFile).Y) - 2
                Else
                    FILE$(iFile) = Left$(FILE$(iFile), CURSORPOSITION(iFile) - 2) + Mid$(FILE$(iFile), CURSORPOSITION(iFile))
                    CURSORPOSITION(iFile) = CURSORPOSITION(iFile) - 1
                    Cursor(iFile).X = Cursor(iFile).X - 1
                End If
        End Select
        ReParseFile iFile
    ElseIf Len(Key$) = 2 Then
        Select Case Asc(Key$, 2)
            Case 83: FILE$(iFile) = Left$(FILE$(iFile), CURSORPOSITION(iFile) - 1) + Mid$(FILE$(iFile), CURSORPOSITION(iFile) + 1)
            Case 75: CURSORPOSITION(iFile) = CURSORPOSITION(iFile) - 1
            Case 77: CURSORPOSITION(iFile) = CURSORPOSITION(iFile) + 1
        End Select
        ReParseFile iFile
    End If
    CURSORPOSITION(iFile) = Max(Min(CURSORPOSITION(iFile), Len(FILE$(iFile))), 1)
End Sub

Sub CheckMouse
    OMW = MW: LMW = 0: MW = 0
    While _MouseInput: LMW = _MouseWheel: MW = IIF(LMW, IIF(Sgn(MW) = Sgn(LMW), MW + LMW, LMW), MW): Wend
    ScrollOffset = ScrollOffset + MW
    If ScrollOffset < 0 Then ScrollOffset = 0
End Sub

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
    ReDim _Preserve FILE_NAMES$(1 To iFile)
    FILE_NAMES$(iFile) = FILENAME$(F$)
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
    ReDim _Preserve CURSORPOSITION(1 To iFile)
    CURSORPOSITION(iFile) = 1
    ReDim _Preserve Cursor(1 To iFile) As Vec2
    Cursor(iFile).X = 1
    Cursor(iFile).Y = 0
End Sub
Sub ReParseFile (iFile)
    FILE_PARSED$(iFile) = ListLongNew$
    ListLongAdd FILE_PARSED$(iFile), 1
    For I = 1 To Len(FILE$(iFile))
        If Asc(FILE$(iFile), I) = 13 Then If Asc(FILE$(iFile), I + 1) = 10 Then ListLongAdd FILE_PARSED$(iFile), I + 2
    Next I
    'P = 0
    'Do
    '    ListLongAdd FILE_PARSED$(iFile), P + 1 + Sgn(P)
    '    P = InStr(P + 1, FILE$(iFile), Chr$(13) + Chr$(10))
    'Loop While P
End Sub
Function IIF (C, E1, E2)
    IIF = -(C <> 0) * E1 - (C = 0) * E2
End Function
Function CountChars~& (I$, C~%%)
    C~& = 0
    For I~& = 1 To Len(I$)
        If Asc(I$, I~&) = C~%% Then C~& = C~& + 1
    Next I~&
    CountChars = C~&
End Function
Sub Replace (A$, B$, C$)
    P = InStr(FILE$(iFile), B$)
    LB~& = Len(B$)
    LC~& = Len(C$)
    If P = 0 Then Exit Sub
    Do
        A$ = Left$(A$, P - 1) + C$ + Mid$(A$, P + LB~& + 1)
        P = InStr(P + LC~& + 1, FILE$(iFile), B$)
    Loop While P
End Sub
Function FILENAME$ (I$)
    If InStr(I$, "\") Then FILENAME$ = Mid$(I$, _InStrRev(I$, "\") + 1) Else FILENAME$ = I$
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
