'$Dynamic
Screen _NewImage(960, 540, 32)
Dim shared as _unsigned long VerticalScrollOffset, HorizontalScrollOffset
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
