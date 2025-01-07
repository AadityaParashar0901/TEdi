Function ListLongNew$
    ListLongNew$ = Chr$(3) + MKL$(0)
End Function
Function ListLongPrint$ (LIST$)
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 3 Then Exit Function
    O = 6: T_OFFSET = 2
    T$ = String$(Len(LIST$) - 4, 0)
    Asc(T$) = 91 '[
    For I = 1 To CVL(Mid$(LIST$, 2, 4)) - 1
        N$ = _Trim$(Str$(CVL(Mid$(LIST$, 6 + I * 4, 4))))
        L = Len(N$)
        Mid$(T$, T_OFFSET, L + 1) = N$ + ","
        T_OFFSET = T_OFFSET + L + 1
    Next I
    N$ = _Trim$(Str$(CVL(Mid$(LIST$, 6 + I * 4, 4))))
    L = Len(N$)
    Mid$(T$, T_OFFSET, L + 1) = N$ + "]"
    ListLongPrint$ = Left$(T$, T_OFFSET + L + 1)
End Function
Function ListLongLength~& (LIST$)
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 3 Then Exit Function
    ListLongLength~& = CVL(Mid$(LIST$, 2, 4))
End Function
Sub ListLongAdd (LIST$, ITEM&)
    If Len(LIST$) < 5 Then Exit Sub
    If Asc(LIST$) <> 3 Then Exit Sub
    LIST$ = Chr$(3) + MKL$(CVL(Mid$(LIST$, 2, 4)) + 1) + Mid$(LIST$, 6) + MKL$(ITEM&)
End Sub
Function ListLongGet& (LIST$, POSITION As _Unsigned Long)
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 3 Then Exit Function
    If CVL(Mid$(LIST$, 2, 4)) < POSITION - 1 Then Exit Function
    If Len(LIST$) < 6 + 4 * POSITION Then Exit Function
    ListLongGet& = CVL(Mid$(LIST$, 6 + 4 * POSITION, 4))
End Function
Sub ListLongInsert (LIST$, ITEM&, POSITION As _Unsigned Long)
    If Len(LIST$) < 5 Then Exit Sub
    If Asc(LIST$) <> 3 Then Exit Sub
    If CVL(Mid$(LIST$, 2, 4)) < POSITION - 1 Then Exit Sub
    LIST$ = Chr$(3) + MKL$(CVL(Mid$(LIST$, 2, 4)) + 1) + Mid$(LIST$, 6, POSITION * 4) + MKL$(ITEM&) + Mid$(LIST$, 6 + POSITION * 4)
End Sub
Sub ListLongDelete (LIST$, POSITION As _Unsigned Long)
    If Len(LIST$) < 5 Then Exit Sub
    If Asc(LIST$) <> 3 Then Exit Sub
    If CVL(Mid$(LIST$, 2, 4)) < POSITION - 1 Then Exit Sub
    LIST$ = Chr$(3) + MKL$(CVL(Mid$(LIST$, 2, 4)) - 1) + Mid$(LIST$, 6, POSITION * 4) + Mid$(LIST$, 6 + POSITION * 4)
End Sub
Function ListLongSearch~& (LIST$, ITEM&)
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 3 Then Exit Function
    For I = 1 To CVL(Mid$(LIST$, 2, 4))
        If ITEM& = CVL(Mid$(LIST$, 6 + I * 4, 4)) Then ListLongSearch~& = I: Exit Function
    Next I
    ListLongSearch~& = 0
End Function
Sub ListLongEdit (LIST$, ITEM&, POSITION As _Unsigned Long)
    If Len(LIST$) < 5 Then Exit Sub
    If Asc(LIST$) <> 3 Then Exit Sub
    If CVL(Mid$(LIST$, 2, 4)) < POSITION - 1 Then Exit Sub
    LIST$ = Left$(LIST$, 5 + POSITION * 4) + MKL$(ITEM&) + Mid$(LIST$, 6 + POSITION * 4)
End Sub
