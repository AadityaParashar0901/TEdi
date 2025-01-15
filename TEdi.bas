'$Dynamic
$Resize:On
Type Vec2
    As Long X, Y
End Type
Type CHAR_TYPES
    As _Unsigned Long KeyWords, Numbers, Strings, Symbols, Normal
End Type
Screen _NewImage(960, 540, 32)
Const FONTWIDTH = 8, FONTHEIGHT = 16
Const TABSIZE = 8

Dim Shared As Long VerticalScrollOffset, HorizontalScrollOffset
Dim Shared As _Unsigned Long VerticalCharsVisible, HorizontalCharsVisible
Dim Shared As Vec2 TextDrawOffset: SetVec2 TextDrawOffset, 5, 2

Dim Shared As String Lines(0), Colour(0)
Dim Shared As Vec2 Cursor, DisplayCursor, OldCursor: SetVec2 Cursor, 1, 1
Dim Shared As Vec2 SelectionStart, SelectionEnd
Dim Shared As _Unsigned Long CursorColour
Dim Shared As CHAR_TYPES THEME_COLOURS
THEME_COLOURS.Normal = _RGB32(255, 255, 255)
THEME_COLOURS.Symbols = _RGB32(0, 191, 0)
THEME_COLOURS.Numbers = _RGB32(255, 127, 255)
THEME_COLOURS.Strings = _RGB32(255, 191, 0)
THEME_COLOURS.KeyWords = _RGB32(0, 127, 191)
If _FileExists(Command$(1)) = 0 Then InFile$ = _StartDir$ + "/" + Command$(1) Else InFile$ = Command$(1)
If _FileExists(InFile$) = 0 Then System
ReadFile InFile$

Do
    Cls , 0: _Limit 60
    If _Resize Then
        W = _ResizeWidth: H = _ResizeHeight
        If Sgn(W) And Sgn(H) Then Screen _NewImage(W, H, 32)
    End If
    VerticalCharsVisible = _Height \ FONTHEIGHT - TextDrawOffset.Y - 1: HorizontalCharsVisible = _Width \ FONTWIDTH - TextDrawOffset.X - 5

    If Timer - Int(Timer) > 0.5 Then ShowCursor = -1 Else ShowCursor = 0

    'Mouse
    OLDMOUSEBUTTON = _MouseButton(1)
    While _MouseInput
        VerticalScrollOffset = VerticalScrollOffset + _MouseWheel
    Wend
    If _MouseButton(1) Then
        If inRange(TextDrawOffset.X, _MouseX \ FONTWIDTH, TextDrawOffset.X + HorizontalCharsVisible) And inRange(TextDrawOffset.Y, _MouseY \ FONTHEIGHT, TextDrawOffset.Y + VerticalCharsVisible) Then
            Cursor.X = HorizontalScrollOffset + _MouseX \ FONTWIDTH - TextDrawOffset.X + 1
            Cursor.Y = VerticalScrollOffset + _MouseY \ FONTHEIGHT - TextDrawOffset.Y
            If OLDMOUSEBUTTON = 0 Then SelectionStart = Cursor
            SelectionEnd = Cursor
        End If
    End If
    VerticalScrollOffset = Max(1, Min(VerticalScrollOffset, UBound(Lines) - _SHR(VerticalCharsVisible, 1)))

    'Keyboard
    Key$ = InKey$
    OldKeyShift = KeyShift
    KeyShift = _KeyDown(100303) Or _KeyDown(100304)
    KeyCtrl = _KeyDown(100305) Or _KeyDown(100306)
    KeyAlt = _KeyDown(100307) Or _KeyDown(100308)

    If Len(Key$) = 1 Then
        If KeyCtrl Then
            Select Case Asc(Key$)
                Case 3: 'Ctrl + C
                    _Clipboard$ = SelectedText$
                Case 11: 'Ctrl + K
                    HorizontalScrollOffset = Max(0, HorizontalScrollOffset - 1)
                Case 12: 'Ctrl + L
                    HorizontalScrollOffset = HorizontalScrollOffset + 1
                Case 21: 'Ctrl + U
                Case 22: 'Ctrl + V
                    InsertText _Clipboard$
                Case 26: 'Ctrl + Z
            End Select
        Else
            Select Case Asc(Key$)
                Case 8: If Cursor.X > 1 Then
                        DeleteText 1
                    Else
                        If Cursor.Y > 1 Then
                            Cursor.Y = Cursor.Y - 1
                            Cursor.X = Len(Lines(Cursor.Y)) + 1
                            Lines(Cursor.Y) = Lines(Cursor.Y) + Lines(Cursor.Y + 1)
                            ReParseLine Cursor.Y
                            For I~& = Cursor.Y + 1 To UBound(Lines) - 1
                                Swap Lines(I~&), Lines(I~& + 1)
                                Swap Colour(I~&), Colour(I~& + 1)
                            Next I~&
                            ReDim _Preserve As String Lines(1 To UBound(Lines) - 1), Colour(1 To UBound(Colour) + 1)
                        End If
                    End If

                Case 13: ReDim _Preserve As String Lines(1 To UBound(Lines) + 1), Colour(1 To UBound(Colour) + 1)
                    For I~& = UBound(Lines) - 1 To Cursor.Y + 1 Step -1
                        Swap Lines(I~&), Lines(I~& + 1)
                        Swap Colour(I~&), Colour(I~& + 1)
                    Next I~&
                    If Cursor.X < Len(Lines(Cursor.Y)) Then
                        Lines(Cursor.Y + 1) = Mid$(Lines(Cursor.Y), Cursor.X)
                        Lines(Cursor.Y) = Left$(Lines(Cursor.Y), Cursor.X - 1)
                    End If
                    ReParseLine Cursor.Y
                    ReParseLine Cursor.Y + 1
                    Cursor.Y = Cursor.Y + 1
                    Cursor.X = 1

                Case 32 To 126: InsertText Key$

            End Select
        End If
    ElseIf Len(Key$) = 2 Then
        Select Case Asc(Key$, 2)
            Case 73: Cursor.Y = Max(1, Cursor.Y - VerticalCharsVisible)
                VerticalScrollOffset = Max(1, VerticalScrollOffset - VerticalCharsVisible)

            Case 81: Cursor.Y = Min(UBound(Lines), Cursor.Y + VerticalCharsVisible)
                VerticalScrollOffset = Min(UBound(lines) - _SHR(VerticalCharsVisible, 1), VerticalScrollOffset + VerticalCharsVisible)

            Case 83: If Cursor.X <= Len(Lines(Cursor.Y)) Then
                    DeleteText -1
                Else
                    If UBound(Lines) > Cursor.Y Then
                        Lines(Cursor.Y) = Lines(Cursor.Y) + Lines(Cursor.Y + 1)
                        For I~& = Cursor.Y + 1 To UBound(Lines) - 1
                            Swap Lines(I~&), Lines(I~& + 1)
                            Swap Colour(I~&), Colour(I~& + 1)
                        Next I~&
                        ReDim _Preserve As String Lines(1 To UBound(Lines) - 1), Colour(1 To UBound(Colour) - 1)
                        ReParseLine Cursor.Y
                    End If
                End If

            Case 72 'Up
                Cursor.Y = Max(1, Cursor.Y - 1)

            Case 141: 'Ctrl + Up
                VerticalScrollOffset = Max(1, VerticalScrollOffset - 1)

            Case 80 'Down
                Cursor.Y = Min(Cursor.Y + 1, UBound(Lines))

            Case 145: 'Ctrl + Down
                VerticalScrollOffset = VerticalScrollOffset + 1

            Case 75 'Left
                Cursor.X = Cursor.X - 1

            Case 77 'Right
                Cursor.X = Cursor.X + 1

            Case 71 'Home
                Cursor.X = 1

            Case 79 'End
                Cursor.X = Len(Lines(Cursor.Y)) + 1

        End Select
    End If
    If Len(Key$) Then
        If inRange(HorizontalScrollOffset, Cursor.X, HorizontalScrollOffset + HorizontalCharsVisible) = 0 Then
            If HorizontalScrollOffset > Cursor.X Then HorizontalScrollOffset = Cursor.X Else HorizontalScrollOffset = Cursor.X - HorizontalCharsVisible
        End If
        If inRange(VerticalScrollOffset, Cursor.Y, VerticalScrollOffset + VerticalCharsVisible) = 0 Then
            If VerticalScrollOffset > Cursor.Y Then VerticalScrollOffset = Cursor.Y Else VerticalScrollOffset = Cursor.Y - VerticalCharsVisible
        End If
    End If
    VerticalScrollOffset = Max(1, Min(VerticalScrollOffset, UBound(Lines) - _SHR(VerticalCharsVisible, 1)))
    Cursor.Y = Max(1, Min(Cursor.Y, UBound(Lines)))
    Cursor.X = Max(1, Min(Cursor.X, Len(Lines(Cursor.Y)) + 1))
    If KeyShift And OldKeyShift = 0 Then SelectionStart = Cursor
    If KeyShift Then SelectionEnd = Cursor
    If SelectionStart.Y <> SelectionEnd.Y Then
        If SelectionEnd.Y < SelectionStart.Y Then Swap SelectionStart, SelectionEnd
    Else
        If SelectionEnd.X < SelectionStart.X Then Swap SelectionStart, SelectionEnd
    End If

    'Display
    Line ((TextDrawOffset.X + 0.5) * FONTWIDTH, TextDrawOffset.Y * FONTHEIGHT)-((TextDrawOffset.X + 0.5) * FONTWIDTH, (TextDrawOffset.Y + VerticalCharsVisible + 1) * FONTHEIGHT), -1 'Seperator
    Line ((TextDrawOffset.X + HorizontalCharsVisible + 0.5) * FONTWIDTH, TextDrawOffset.Y * FONTHEIGHT)-((TextDrawOffset.X + HorizontalCharsVisible + 0.5) * FONTWIDTH, (TextDrawOffset.Y + VerticalCharsVisible + 1) * FONTHEIGHT), -1 'Seperator
    Line ((TextDrawOffset.X + 1) * FONTWIDTH, (TextDrawOffset.Y + Cursor.Y - VerticalScrollOffset) * FONTHEIGHT)-((TextDrawOffset.X + HorizontalCharsVisible) * FONTWIDTH, (TextDrawOffset.Y + Cursor.Y + 1 - VerticalScrollOffset) * FONTHEIGHT), _RGB32(31), BF 'Line Highlighter

    'Cursor
    SetVec2 DisplayCursor, FONTWIDTH * (Cursor.X + TextDrawOffset.X - HorizontalScrollOffset), FONTHEIGHT * (Cursor.Y + TextDrawOffset.Y - VerticalScrollOffset + 0.125)

    'Lines
    DI~& = TextDrawOffset.Y * FONTHEIGHT
    For I~& = VerticalScrollOffset To Min(VerticalScrollOffset + VerticalCharsVisible, UBound(Lines))
        'Line Number
        Color -1, 0
        LINENUMBER$ = LTrim$(Str$(I~&))
        _PrintString ((TextDrawOffset.X - Len(LINENUMBER$)) * FONTWIDTH, DI~&), LINENUMBER$

        'Line Print
        COLOUROFFSET~& = 0
        K = (TextDrawOffset.X - HorizontalScrollOffset) * FONTWIDTH
        For J~& = 1 To Len(Lines(I~&))
            BYTE~%% = Asc(Lines(I~&), J~&)
            COLOUROFFSET~& = COLOUROFFSET~& - inRange(33, BYTE~%%, 126)
            Color , 0
            If Vec2Equal(SelectionStart, SelectionEnd) = 0 Then
                If inRange(SelectionStart.Y + 1, I~&, SelectionEnd.Y - 1) Then Color , _RGB32(63)
                If SelectionStart.Y = I~& And I~& < SelectionEnd.Y And SelectionStart.X <= J~& Then Color , _RGB32(63)
                If SelectionStart.Y < I~& And I~& = SelectionEnd.Y And SelectionEnd.X >= J~& Then Color , _RGB32(63)
                If SelectionStart.Y = SelectionEnd.Y And inRange(SelectionStart.X, J~&, SelectionEnd.X) Then Color , _RGB32(63)
                If inRange(SelectionStart.Y, I~&, SelectionEnd.Y) = 0 Then Color , 0
            End If
            Select Case BYTE~%%
                Case 9, 32: Color -1
                Case 33 To 126: Color ListLongGet(Colour(I~&), COLOUROFFSET~&)
                Case Else: Color _RGB32(255, 0, 0)
            End Select
            If I~& = Cursor.Y And J~& = Cursor.X Then
                DisplayCursor.X = K - (HorizontalScrollOffset - 1) * FONTWIDTH
                CursorColour = _DefaultColor
            End If
            If I~& = Cursor.Y And Cursor.X > Len(Lines(Cursor.Y)) Then
                DisplayCursor.X = K - (HorizontalScrollOffset - 2) * FONTWIDTH
                CursorColour = -1
            End If
            If BYTE~%% = 9 Then
                OldK = K
                K = (((K \ TABSIZE - TextDrawOffset.X + HorizontalScrollOffset) \ FONTWIDTH + 1) * TABSIZE + TextDrawOffset.X - HorizontalScrollOffset) * FONTWIDTH
            Else
                K = K + FONTWIDTH
            End If
            If K >= (TextDrawOffset.X + 1) * FONTWIDTH Then
                Select Case BYTE~%%
                    Case 33 To 126: _PrintString (K, DI~&), Chr$(BYTE~%%)
                    Case 9, 32
                    Case Else: _PrintString (K, DI~&), Chr$(BYTE~%%)
                End Select
            End If
            If K \ FONTWIDTH >= TextDrawOffset.X + HorizontalCharsVisible - 1 Then Exit For
        Next J~&
        DI~& = DI~& + FONTHEIGHT
    Next I~&

    'Draw Cursor
    If Vec2Equal(Cursor, OldCursor) = 0 Then ShowCursor = -1
    Color CursorColour, 0
    If inRange((TextDrawOffset.X + 1) * FONTWIDTH, DisplayCursor.X, (TextDrawOffset.X + HorizontalCharsVisible) * FONTWIDTH) And inRange(VerticalScrollOffset, Cursor.Y, VerticalScrollOffset + VerticalCharsVisible) Then If ShowCursor Then _PrintString (DisplayCursor.X, DisplayCursor.Y), Chr$(22)
    OldCursor = Cursor

    _Display
Loop
System

Function SelectedText$
    If SelectionStart.Y <> SelectionEnd.Y Then
        For I~& = SelectionStart.Y To SelectionEnd.Y
            If I~& = SelectionStart.Y Then
                ST$ = Mid$(Lines(I~&), SelectionStart.X)
            ElseIf I~& = SelectionEnd.Y Then
                ST$ = ST$ + Left$(Lines(I~&), SelectionEnd.X + 1)
            End If
            If I~& < SelectionEnd.Y Then
                ST$ = ST$ + NEWLINE$
            End If
        Next I~&
        SelectedText$ = ST$
    Else
        SelectedText$ = Mid$(Lines(SelectionStart.Y), SelectionStart.X, SelectionEnd.X - SelectionStart.X + 1)
    End If
End Function
Sub InsertText (K$)
    If InStr(K$, Chr$(13)) Or InStr(K$, Chr$(10)) Then
        SY~& = Cursor.Y
        For I~& = 1 To Len(K$)
            BYTE~%% = Asc(K$, I~&)
            Select Case BYTE~%%
                Case 13: If Asc(K$, I~& + 1) = 10 Then
                        ReDim _Preserve As String Lines(1 To UBound(Lines) + 1), Colour(1 To UBound(Colour) + 1)
                        For J~& = UBound(Lines) - 1 To Cursor.Y + 1 Step -1
                            Swap Lines(J~&), Lines(J~& + 1)
                            Swap Colour(J~&), Colour(J~& + 1)
                        Next J~&
                        Lines(Cursor.Y + 1) = Mid$(Lines(Cursor.Y), Cursor.X)
                        Lines(Cursor.Y) = Left$(Lines(Cursor.Y), Cursor.X - 1)
                        Cursor.Y = Cursor.Y + 1
                        Cursor.X = 1
                        I~& = I~& + 1
                    End If
                Case 10
                    ReDim _Preserve As String Lines(1 To UBound(Lines) + 1), Colour(1 To UBound(Colour) + 1)
                    For J~& = UBound(Lines) - 1 To Cursor.Y + 1 Step -1
                        Swap Lines(J~&), Lines(J~& + 1)
                        Swap Colour(J~&), Colour(J~& + 1)
                    Next J~&
                    Cursor.Y = Cursor.Y + 1
                    Cursor.X = 1
                Case Else: Lines(Cursor.Y) = Left$(Lines(Cursor.Y), Cursor.X - 1) + Chr$(BYTE~%%) + Mid$(Lines(Cursor.Y), Cursor.X)
                    Cursor.X = Cursor.X + 1
            End Select
        Next I~&
        For I~& = SY~& To Cursor.Y
            ReParseLine I~&
        Next I~&
    Else
        Lines(Cursor.Y) = Left$(Lines(Cursor.Y), Cursor.X - 1) + K$ + Mid$(Lines(Cursor.Y), Cursor.X)
        Cursor.X = Cursor.X + Len(K$)
        ReParseLine Cursor.Y
    End If
End Sub
Sub DeleteText (SIZE&)
    If SIZE& > 0 Then
        Lines(Cursor.Y) = Left$(Lines(Cursor.Y), Cursor.X - SIZE& - 1) + Mid$(Lines(Cursor.Y), Cursor.X)
        Cursor.X = Cursor.X - 1
    ElseIf SIZE& < 0 Then
        Lines(Cursor.Y) = Left$(Lines(Cursor.Y), Cursor.X - 1) + Mid$(Lines(Cursor.Y), Cursor.X - SIZE&)
    End If
    ReParseLine Cursor.Y
End Sub

Sub ReadFile (FILE$)
    Print "Reading File"
    __F = FreeFile
    Open FILE$ For Binary As #__F
    __FC$ = String$(LOF(__F), 0)
    Get #__F, , __FC$
    Close #__F
    CURRENTLINE~& = 1
    __OldI~& = 1
    ReDim Colour(1 To 1) As String
    Colour(1) = ListLongNew$
    Y = CsrLin
    __L~& = Len(__FC$)
    For __I~& = 1 To __L~&
        BYTE~%% = Asc(__FC$, __I~&)
        Select Case BYTE~%%
            Case 13: If Asc(__FC$, __I~& + 1) = 10 Then
                    ReDim _Preserve Lines(1 To CURRENTLINE~&) As String
                    Lines(CURRENTLINE~&) = Mid$(__FC$, __OldI~&, __I~& - __OldI~&)
                    CURRENTLINE~& = CURRENTLINE~& + 1
                    ReDim _Preserve Colour(1 To CURRENTLINE~&) As String
                    Colour(CURRENTLINE~&) = ListLongNew$
                    __I~& = __I~& + 1
                    __OldI~& = __I~& + 1
                End If
            Case 10: ReDim _Preserve Lines(1 To CURRENTLINE~&) As String
                Lines(CURRENTLINE~&) = Mid$(__FC$, __OldI~&, __I~& - __OldI~&)
                CURRENTLINE~& = CURRENTLINE~& + 1
                ReDim _Preserve Colour(1 To CURRENTLINE~&) As String
                Colour(CURRENTLINE~&) = ListLongNew$
                __OldI~& = __I~& + 1
        End Select
        If Timer - ST! > 0.1 Then Locate Y, 1: Print "Reading Byte"; __I~&; "/"; __L~&: ST! = Timer
    Next __I~&
    ReDim _Preserve As String Lines(1 To CURRENTLINE~&)
    Lines(CURRENTLINE~&) = Mid$(__FC$, __OldI~&)
    ParseFile
End Sub
Sub ParseFile
    Print "Parsing File"
    Y = CsrLin
    For __I~& = 1 To UBound(Lines)
        If Timer - ST! > 0.1 Then Locate Y, 1: Print "Parsing Line"; __I~&; "/"; UBound(Lines): ST! = Timer
        ReParseLine __I~&
    Next __I~&
End Sub
Sub ReParseLine (__LINE~&)
    Colour(__LINE~&) = ListLongNew$
    For __I~& = 1 To Len(Lines(__LINE~&))
        BYTE~%% = Asc(Lines(__LINE~&), __I~&)
        If STRINGMODE And BYTE~%% <> 34 Then
            ListLongAdd Colour(__LINE~&), THEME_COLOURS.Strings
        Else
            Select Case BYTE~%%
                Case 33, 35 To 45, 47, 58 To 64, 91 To 94, 96, 123 To 126: ListLongAdd Colour(__LINE~&), THEME_COLOURS.Symbols
                Case 34: STRINGMODE = 1 - STRINGMODE: ListLongAdd Colour(__LINE~&), THEME_COLOURS.Symbols
                Case 46: ListLongAdd Colour(__LINE~&), IIF(inRange(48, LB~%%, 57) Or inRange(65, LB~%%, 90) Or inRange(97, LB~%%, 122), ListLongGet(Colour(__LINE~&), ListLongLength(Colour(__LINE~&))), THEME_COLOURS.Normal)
                Case 48 To 57: ListLongAdd Colour(__LINE~&), IIF(inRange(48, LB~%%, 57) Or inRange(65, LB~%%, 90) Or inRange(97, LB~%%, 122), ListLongGet(Colour(__LINE~&), ListLongLength(Colour(__LINE~&))), THEME_COLOURS.Numbers)
                Case 65 To 90, 97 To 122: ListLongAdd Colour(__LINE~&), THEME_COLOURS.Normal
                Case 95: ListLongAdd Colour(__LINE~&), THEME_COLOURS.Normal
            End Select
        End If
        LB~%% = BYTE~%%
    Next __I~&
End Sub

Sub SaveFile (FILE$)
    __FS~& = 0
    Dim I As _Unsigned Long
    __UL~& = UBound(Lines)
    For I = 1 To __UL~&
        __FS~& = __FS~& + Len(Lines(I))
    Next I
    __FC$ = String$(__FS~&, 0)
    __FS~& = 1
    For I = 1 To __UL~&
        Mid$(__FC$, __FS~&, Len(Lines(I))) = Lines(I)
        __FS~& = __FS~& + Len(Lines(I))
    Next I
    __F = FreeFile
    Open FILE$ For Binary As #__F
    Get #__F, , __FC$
    Close #__F
    __FC$ = ""
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
Function Vec2Equal (A As Vec2, B As Vec2): Vec2Equal = (A.X = B.X) And (A.Y = B.Y): End Function
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
