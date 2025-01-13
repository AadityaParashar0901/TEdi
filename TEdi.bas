'$Dynamic
$Resize:On
Type Vec2
    As _Unsigned Long X, Y
End Type
Screen _NewImage(960, 540, 32)
Const FONTWIDTH = 8, FONTHEIGHT = 16
Const TABSIZE = 8

Dim Shared As Long VerticalScrollOffset(0), HorizontalScrollOffset(0)
Dim Shared As Vec2 TextDrawOffset: SetVec2 TextDrawOffset, 5, 1
Dim Shared As _Unsigned Long TOTALFILESOPENED, FILE_LINES(0)
Dim Shared As String FILE_NAME(0), FILE_CONTENT(0), FILE_COLOR_START(0), FILE_LINES_START(0), FILE_LINES_END(0), FILE_COLOURS(0)
For I = 1 To _CommandCount
    INFILE$ = Command$(I)
    If _FileExists(INFILE$) = 0 Then INFILE$ = _StartDir$ + "/" + INFILE$
    If _FileExists(INFILE$) = 0 Then _Continue
    ReadFile INFILE$
Next I
If TOTALFILESOPENED = 0 Then System
currentFile = 1
_Title FILE_NAME(currentFile)
Do
    Cls , 0
    _Limit 60
    If _Resize Then
        W = _ResizeWidth: H = _ResizeHeight
        If Sgn(W) And Sgn(H) Then Screen _NewImage(W, H, 32)
    End If
    VerticalCharsVisible = _Height \ FONTHEIGHT - TextDrawOffset.Y - 1: HorizontalCharsVisible = _Width \ FONTWIDTH - TextDrawOffset.X

    'Mouse
    While _MouseInput: VerticalScrollOffset(currentFile) = VerticalScrollOffset(currentFile) + _MouseWheel: Wend
    VerticalScrollOffset(currentFile) = Max(1, Min(FILE_LINES(currentFile) - _SHR(VerticalCharsVisible, 1), VerticalScrollOffset(currentFile)))

    'Keyboard
    If (_KeyDown(67) Or _KeyDown(99)) Then currentFile = (currentFile Mod TOTALFILESOPENED) + 1: _Title FILE_NAME(currentFile): While _KeyDown(67) Or _KeyDown(99): Wend
    Key$ = InKey$
    Select Case Len(Key$)
        Case 2: Select Case Asc(Key$, 2)
                Case 72: VerticalScrollOffset(currentFile) = Max(VerticalScrollOffset(currentFile) - 1, 1)
                Case 80: VerticalScrollOffset(currentFile) = Min(VerticalScrollOffset(currentFile) + 1, FILE_LINES(currentFile))
                Case 75: HorizontalScrollOffset(currentFile) = Max(HorizontalScrollOffset(currentFile) - 1, 1)
                Case 77: HorizontalScrollOffset(currentFile) = HorizontalScrollOffset(currentFile) + 1
            End Select
    End Select

    'Display
    Line ((TextDrawOffset.X + 0.5) * FONTWIDTH, TextDrawOffset.Y * FONTHEIGHT)-((TextDrawOffset.X + 0.5) * FONTWIDTH, (TextDrawOffset.Y + VerticalCharsVisible + 1) * FONTHEIGHT), -1 'Seperator
    DI~& = TextDrawOffset.Y * FONTHEIGHT: For I~& = VerticalScrollOffset(currentFile) To Min(VerticalScrollOffset(currentFile) + VerticalCharsVisible, FILE_LINES(currentFile))
        'Line Number
        Color -1, 0: LINENUMBER$ = LTrim$(Str$(I~&)): _PrintString ((TextDrawOffset.X - Len(LINENUMBER$)) * FONTWIDTH, DI~&), LINENUMBER$

        'Line Print
        START~& = HorizontalScrollOffset(currentFile) + ListLongGet(FILE_LINES_START(currentFile), I~&)
        COLOUROFFSET~& = ListLongGet(FILE_COLOR_START(currentFile), I~&)
        K = TextDrawOffset.X * FONTWIDTH: For J~& = ListLongGet(FILE_LINES_START(currentFile), I~&) To ListLongGet(FILE_LINES_END(currentFile), I~&)
            BYTE~%% = Asc(FILE_CONTENT(currentFile), J~&)
            COLOUROFFSET~& = COLOUROFFSET~& - inRange(33, BYTE~%%, 126)
            If J~& >= START~& Then
                K = K + FONTWIDTH
                Select Case BYTE~%%
                    Case 9: K = TABSIZE * FONTWIDTH * ((K \ FONTWIDTH - TextDrawOffset.X) \ TABSIZE + 1) + (TextDrawOffset.X) * FONTWIDTH
                    Case 33 To 126: Color ListLongGet(FILE_COLOURS(currentFile), COLOUROFFSET~&), 0: _PrintString (K, DI~&), Chr$(BYTE~%%)
                    Case Else: Color _RGB32(255, 0, 0), 0: _PrintString (K, DI~&), Chr$(BYTE~%%)
                End Select
                If K > _Width Then Exit For
            End If
        Next J~&
    DI~& = DI~& + FONTHEIGHT: Next I~&

    _Display
Loop Until Inp(&H60) = 1
System
Sub ReadFile (FILE$)
    __F& = FreeFile
    iFile = UBound(FILE_NAME) + 1
    ReDim _Preserve FILE_NAME(1 To iFile), FILE_CONTENT(1 To iFile), FILE_COLOURS(1 To iFile), FILE_COLOR_START(1 To iFile), FILE_LINES(1 To iFile): FILE_NAME(iFile) = FILENAME$(FILE$)
    ReDim _Preserve FILE_LINES_START(1 To iFile), FILE_LINES_END(1 To iFile)
    ReDim _Preserve VerticalScrollOffset(1 To iFile), HorizontalScrollOffset(1 To iFile)
    Open FILE$ For Binary As #__F&
    FILE_CONTENT(iFile) = String$(LOF(__F&), 0)
    Get #__F&, , FILE_CONTENT(iFile)
    Close #__F&
    ParseFile iFile
    TOTALFILESOPENED = TOTALFILESOPENED + 1
End Sub
Sub ParseFile (iFile)
    FILE_COLOR_START(iFile) = ListLongNew$
    FILE_COLOURS(iFile) = ListLongNew$
    FILE_LINES_START(iFile) = ListLongNew$
    FILE_LINES_END(iFile) = ListLongNew$
    FILE_LINES(iFile) = 1
    ListLongAdd FILE_LINES_START(iFile), 1
    ListLongAdd FILE_COLOR_START(iFile), 0
    Dim I As _Unsigned Long
    For I = 1 To Len(FILE_CONTENT(iFile))
        If (I Mod 1000) = 0 Then Locate 1, 1: Print "Parsing File: "; I; "/"; Len(FILE_CONTENT(iFile))
        BYTE~%% = Asc(FILE_CONTENT(iFile), I)
        Select Case BYTE~%%
            Case 13: If Asc(FILE_CONTENT(iFile), I + 1) = 10 Then
                    ListLongAdd FILE_LINES_END(iFile), I - 1
                    ListLongAdd FILE_LINES_START(iFile), I + 2
                    ListLongAdd FILE_COLOR_START(iFile), ListLongLength(FILE_COLOURS(iFile))
                    FILE_LINES(iFile) = FILE_LINES(iFile) + 1
                    I = I + 1
                End If
            Case 33, 35 To 45, 47, 58 To 64, 91 To 94, 96, 123 To 125: ListLongAdd FILE_COLOURS(iFile), _RGB32(0, 191, 0)
            Case 34: STRINGMODE = 1 - STRINGMODE: ListLongAdd FILE_COLOURS(iFile), _RGB32(255, 127, 0)
            Case 46: ListLongAdd FILE_COLOURS(iFile), -1
            Case 48 To 57: ListLongAdd FILE_COLOURS(iFile), IIF(inRange(48, LB~%%, 57) Or inRange(65, LB~%%, 90) Or inRange(97, LB~%%, 122), ListLongGet(FILE_COLOURS(iFile), ListLongLength(FILE_COLOURS(iFile))), _RGB32(255, 127, 255))
            Case 65 To 90, 97 To 122: ListLongAdd FILE_COLOURS(iFile), -1
            Case 95: ListLongAdd FILE_COLOURS(iFile), -1
        End Select
        If STRINGMODE And BYTE~%% <> 34 Then Mid$(FILE_COLOURS(iFile), Len(FILE_COLOURS(iFile)) - 3, 4) = MKL$(_RGB32(255, 191, 0))
        LB~%% = IIF(inRange(33, BYTE~%%, 126), BYTE~%%, LB~%%)
    Next I
    ListLongAdd FILE_LINES_END(iFile), Len(FILE_CONTENT(iFile))
End Sub
Function FILENAME$ (I$)
    If InStr(I$, "\") Then FILENAME$ = Mid$(I$, _InStrRev(I$, "\") + 1) Else FILENAME$ = I$
End Function
Function Replace$ (I$, A$, B$)
    LA~& = Len(A$)
    LB~& = Len(B$)
    O$ = I$
    I~& = InStr(O$, A$)
    Replace$ = O$
    If I~& = 0 Then Exit Function
    Do
        O$ = Left$(O$, I~& - 1) + B$ + Mid$(O$, I~& + LA~&)
        I~& = InStr(I~& + LB~&, O$, A$)
    Loop While I~&
    Replace$ = O$
End Function
Sub SetVec2 (A As Vec2, B&, C&): A.X = B&: A.Y = C&: End Sub
Function Max (A, B): Max = -A * (A > B) - B * (A <= B): End Function
Function Min (A, B): Min = -A * (A < B) - B * (A >= B): End Function
Function inRange (A, B, C): inRange = (A <= B) And (B <= C): End Function
Function inBox (X1, Y1, X, Y, X2, Y2): inBox = (X1 <= X) And (X <= X2) And (Y1 <= Y) And (Y <= Y2): End Function
Function IIF~& (A As Single, B As _Unsigned Long, C As _Unsigned Long): IIF = -C * (A = 0) - B * (A <> 0): End Function
'$Include:'HashTable.bas'
'$Include:'List.bas'
'$Include:'ListLong.bas'
'$Include:'Stack.bas'
Sub Dump (S$): For I = 1 To Len(S$): Print " "; FILTERCHAR$(Asc(S$, I)); " ";: T$ = T$ + H2$(Asc(S$, I)) + " ": Next I: Print: Print T$: End Sub
Function FILTERCHAR$ (A~%%): Select Case A~%%: Case 32 To 127: FILTERCHAR$ = Chr$(A~%%): Case Else: FILTERCHAR$ = ".": End Select: End Function
Function H2$ (A~%%): H$ = Hex$(A~%%): H2$ = String$(2 - Len(H$), 48) + H$: End Function
