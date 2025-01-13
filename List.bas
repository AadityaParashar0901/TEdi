Function ListNew$
    ListNew$ = Chr$(1) + MKL$(0)
End Function
Function ListLength~& (LIST$)
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 1 Then Exit Function
    ListLength~& = CVL(Mid$(LIST$, 2, 4))
End Function
Sub ListAdd (LIST$, ITEM$)
    If Len(LIST$) < 5 Then Exit Sub
    If Asc(LIST$) <> 1 Then Exit Sub
    LIST$ = Chr$(1) + MKL$(CVL(Mid$(LIST$, 2, 4)) + 1) + Mid$(LIST$, 6) + MKI$(Len(ITEM$)) + ITEM$
End Sub
Function ListGet$ (LIST$, POSITION As _Unsigned Long)
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 1 Then Exit Function
    If CVL(Mid$(LIST$, 2, 4)) < POSITION - 1 Then Exit Function
    O = 6
    For I = 1 To POSITION - 1
        L = CVI(Mid$(LIST$, O, 2))
        O = O + L + 2
    Next I
    ListGet$ = Mid$(LIST$, O + 2, CVI(Mid$(LIST$, O, 2)))
End Function
Sub ListInsert (LIST$, ITEM$, POSITION As _Unsigned Long)
    If Len(LIST$) < 5 Then Exit Sub
    If Asc(LIST$) <> 1 Then Exit Sub
    If CVL(Mid$(LIST$, 2, 4)) < POSITION - 1 Then Exit Sub
    O = 6
    For I = 1 To POSITION - 1
        L = CVI(Mid$(LIST$, O, 2))
        O = O + L + 2
    Next I
    LIST$ = Chr$(1) + MKL$(CVL(Mid$(LIST$, 2, 4)) + 1) + Mid$(LIST$, 6, O - 6) + MKI$(Len(ITEM$)) + ITEM$ + Mid$(LIST$, O)
End Sub
Sub ListDelete (LIST$, POSITION As _Unsigned Long)
    If Len(LIST$) < 5 Then Exit Sub
    If Asc(LIST$) <> 1 Then Exit Sub
    If CVL(Mid$(LIST$, 2, 4)) < POSITION - 1 Then Exit Sub
    O = 6
    For I = 1 To POSITION - 1
        L = CVI(Mid$(LIST$, O, 2))
        O = O + L + 2
    Next I
    LIST$ = Chr$(1) + MKL$(CVL(Mid$(LIST$, 2, 4)) - 1) + Mid$(LIST$, 6, O - 6) + Mid$(LIST$, O + CVI(Mid$(LIST$, O, 2)) + 2)
End Sub
Function ListSearch~& (LIST$, ITEM$)
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 1 Then Exit Function
    O = 6
    For I = 1 To CVL(Mid$(LIST$, 2, 4))
        L = CVI(Mid$(LIST$, O, 2))
        If ITEM$ = Mid$(LIST$, O + 2, L) Then ListSearch~& = I: Exit Function
        O = O + L + 2
    Next I
    ListSearch~& = 0
End Function
Function ListSearchI~& (LIST$, ITEM$)
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 1 Then Exit Function
    O = 6
    For I = 1 To CVL(Mid$(LIST$, 2, 4))
        L = CVI(Mid$(LIST$, O, 2))
        If _StriCmp(ITEM$, Mid$(LIST$, O + 2, L)) = 0 Then ListSearchI~& = I: Exit Function
        O = O + L + 2
    Next I
    ListSearchI~& = 0
End Function
Sub ListEdit (LIST$, ITEM$, POSITION As _Unsigned Long)
    If Len(LIST$) < 5 Then Exit Sub
    If Asc(LIST$) <> 1 Then Exit Sub
    If CVL(Mid$(LIST$, 2, 4)) < POSITION - 1 Then Exit Sub
    O = 6
    For I = 1 To POSITION - 1
        L = CVI(Mid$(LIST$, O, 2))
        O = O + L + 2
    Next I
    LIST$ = Left$(LIST$, O - 1) + MKI$(Len(ITEM$)) + ITEM$ + Mid$(LIST$, O + CVI(Mid$(LIST$, O, 2)) + 2)
End Sub
