Function StackNew$
    StackNew$ = Chr$(2) + MKL$(0)
End Function
Sub StackPush (STACK$, ITEM$)
    If Len(STACK$) < 5 Then Exit Sub
    If Asc(STACK$) <> 2 Then Exit Sub
    STACK$ = Chr$(2) + MKL$(CVL(Mid$(STACK$, 2, 4)) + 1) + MKI$(Len(ITEM$)) + ITEM$ + Mid$(STACK$, 6)
End Sub
Function StackPeek$ (STACK$)
    If Len(STACK$) < 5 Then Exit Function
    If Asc(STACK$) <> 2 Then Exit Function
    If CVL(Mid$(STACK$, 2, 4)) < 1 Then Exit Function
    StackPeek$ = Mid$(STACK$, 8, CVI(Mid$(STACK$, 6, 2)))
End Function
Function StackPop$ (STACK$)
    If Len(STACK$) < 5 Then Exit Function
    If Asc(STACK$) <> 2 Then Exit Function
    StackPop$ = Mid$(STACK$, 8, CVI(Mid$(STACK$, 6, 2)))
    STACK$ = Chr$(2) + MKL$(CVL(Mid$(STACK$, 2, 4)) - 1) + Mid$(STACK$, CVI(Mid$(STACK$, 6, 2)) + 8)
End Function
