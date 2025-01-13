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

Dim Shared As String Lines(0), Colour(0)


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
