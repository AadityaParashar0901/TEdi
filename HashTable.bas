Function HashTableNew$
    HASH_TABLE_SIZE = 256
    HTN$ = Chr$(7) + MKL$(HASH_TABLE_SIZE)
    HTNL$ = HashTable_ListNew$
    For I~& = 1 To HASH_TABLE_SIZE: HashTable_ListAdd HTNL$, HashTable_ListNew$: Next I~&
    HashTableNew$ = HTN$ + HTNL$
End Function
Sub HashTableInsert (HASHTABLE$, ITEM$)
    H~% = HashTable_CalculateHash~%(ITEM$)
    H$ = Mid$(HASHTABLE$, 6)
    L$ = HashTable_ListGet$(H$, H~%)
    HashTable_ListAdd L$, ITEM$
    HashTable_ListEdit H$, L$, H~%
    HASHTABLE$ = Left$(HASHTABLE$, 5) + H$
End Sub
Function HashTableSearch~& (HASHTABLE$, ITEM$)
    H~% = HashTable_CalculateHash~%(ITEM$)
    HashTableSearch~& = HashTable_ListSearch~&(HashTable_ListGet$(Mid$(HASHTABLE$, 6), H~%), ITEM$)
End Function
Sub HashTableDelete (HASHTABLE$, ITEM$)
    H~% = HashTable_CalculateHash~%(ITEM$)
    H$ = Mid$(HASHTABLE$, 6)
    L$ = HashTable_ListGet$(H$, H~%)
    S~& = HashTable_ListSearch~&(L$, ITEM$)
    If S~& = 0 Then Exit Sub
    HashTable_ListDelete L$, S~&
    HashTable_ListEdit H$, L$, H~%
    HASHTABLE$ = Left$(HASHTABLE$, 5) + H$
End Sub
Function HashTable_CalculateHash~% (ITEM$)
    T~% = 0
    For I~& = 1 To Len(ITEM$)
        T~% = T~% + Asc(ITEM$, I~&)
    Next I~&
    HashTable_CalculateHash~% = 1 + T~% Mod 256
End Function
Function HashTable_ListNew$
    HashTable_ListNew$ = Chr$(1) + MKL$(0)
End Function
Sub HashTable_ListAdd (HashTable_List$, ITEM$)
    If Len(HashTable_List$) < 5 Then Exit Sub
    If Asc(HashTable_List$) <> 1 Then Exit Sub
    HashTable_List$ = Chr$(1) + MKL$(CVL(Mid$(HashTable_List$, 2, 4)) + 1) + Mid$(HashTable_List$, 6) + MKI$(Len(ITEM$)) + ITEM$
End Sub
Function HashTable_ListGet$ (LIST$, POSITION As _Unsigned Long)
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 1 Then Exit Function
    If CVL(Mid$(LIST$, 2, 4)) < POSITION - 1 Then Exit Function
    O = 6
    For I = 1 To POSITION - 1
        L = CVI(Mid$(LIST$, O, 2))
        O = O + L + 2
    Next I
    HashTable_ListGet$ = Mid$(LIST$, O + 2, CVI(Mid$(LIST$, O, 2)))
End Function
Sub HashTable_ListDelete (HashTable_List$, POSITION As _Unsigned Long)
    If Len(HashTable_List$) < 5 Then Exit Sub
    If Asc(HashTable_List$) <> 1 Then Exit Sub
    If CVL(Mid$(HashTable_List$, 2, 4)) < POSITION - 1 Then Exit Sub
    O = 6
    For I = 1 To POSITION - 1
        L = CVI(Mid$(HashTable_List$, O, 2))
        O = O + L + 2
    Next I
    HashTable_List$ = Chr$(1) + MKL$(CVL(Mid$(HashTable_List$, 2, 4)) - 1) + Mid$(HashTable_List$, 6, O - 6) + Mid$(HashTable_List$, O + CVI(Mid$(HashTable_List$, O, 2)) + 2)
End Sub
Function HashTable_ListSearch~& (HashTable_List$, ITEM$)
    If Len(HashTable_List$) < 5 Then Exit Function
    If Asc(HashTable_List$) <> 1 Then Exit Function
    O = 6
    For I = 1 To CVL(Mid$(HashTable_List$, 2, 4))
        L = CVI(Mid$(HashTable_List$, O, 2))
        If ITEM$ = Mid$(HashTable_List$, O + 2, L) Then HashTable_ListSearch~& = I: Exit Function
        O = O + L + 2
    Next I
    HashTable_ListSearch~& = 0
End Function
Function HashTable_ListSearchI~& (HashTable_List$, ITEM$)
    If Len(HashTable_List$) < 5 Then Exit Function
    If Asc(HashTable_List$) <> 1 Then Exit Function
    O = 6
    For I = 1 To CVL(Mid$(HashTable_List$, 2, 4))
        L = CVI(Mid$(HashTable_List$, O, 2))
        If _StriCmp(ITEM$, Mid$(HashTable_List$, O + 2, L)) = 0 Then HashTable_ListSearchI~& = I: Exit Function
        O = O + L + 2
    Next I
    HashTable_ListSearchI~& = 0
End Function
Sub HashTable_ListEdit (HashTable_List$, ITEM$, POSITION As _Unsigned Long)
    If Len(HashTable_List$) < 5 Then Exit Sub
    If Asc(HashTable_List$) <> 1 Then Exit Sub
    If CVL(Mid$(HashTable_List$, 2, 4)) < POSITION - 1 Then Exit Sub
    O = 6
    For I = 1 To POSITION - 1
        L = CVI(Mid$(HashTable_List$, O, 2))
        O = O + L + 2
    Next I
    HashTable_List$ = Left$(HashTable_List$, O - 1) + MKI$(Len(ITEM$)) + ITEM$ + Mid$(HashTable_List$, O + CVI(Mid$(HashTable_List$, O, 2)) + 2)
End Sub
