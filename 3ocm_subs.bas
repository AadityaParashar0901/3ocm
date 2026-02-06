$Console:Only
$Let DEBUG = 0
FILE_EXT$ = ".3ocm"
'$Include:'include\compression_template.bi'
Function Compress$ (I$)
    Dim As _Unsigned Long I, B_OFFSET, O_OFFSET
    Dim As _Unsigned _Byte B, B1, B2, B3
    Dim As _Unsigned _Byte C(0 To 16777215)
    Dim As _Unsigned Long C_I
    Dim As _Unsigned _Bit * 3 J, K: J = 0
    B$ = String$(Remain(Len(I$), 8), 0)
    O$ = String$(Len(I$), 0)
    B_OFFSET = 1
    O_OFFSET = 1
    For I = 1 To Len(I$)
        B = Asc(I$, I)
        C_I = _SHL(B3, 16) Or _SHL(B2, 8) Or B1
        Select Case C(C_I) = B
            Case 0:
                Asc(B$, B_OFFSET) = _SetBit(Asc(B$, B_OFFSET), J)
                Asc(O$, O_OFFSET) = B: O_OFFSET = O_OFFSET + 1
                C(C_I) = B
        End Select
        J = J + 1
        B_OFFSET = B_OFFSET - (J = 0)
        B3 = B2
        B2 = B1
        B1 = B
    Next I
    O_OFFSET = O_OFFSET - 1
    O$ = Left$(O$, O_OFFSET)
    B$ = Left$(B$, B_OFFSET)
    If RAW_MODE = 0 Then
        O$ = _Deflate$(Left$(O$, O_OFFSET))
        B$ = _Deflate$(Left$(B$, B_OFFSET))
    End If
    Compress$ = MKL$(Len(O$)) + Chr$(J) + O$ + B$
    O$ = ""
    B$ = ""
End Function
Function Decompress$ (I$)
    Dim As _Unsigned Long I, B_OFFSET, O_OFFSET
    Dim As _Unsigned _Byte B, B1, B2, B3, L
    Dim As _Unsigned _Byte C(0 To 16777215)
    Dim As _Unsigned Long C_I
    Dim As _Unsigned _Bit * 3 J, K: J = 0
    O$ = Mid$(I$, 6, CVL(Left$(I$, 4)))
    B$ = Mid$(I$, 6 + Len(O$))
    K = Asc(I$, 5)
    If RAW_MODE = 0 Then
        O$ = _Inflate$(O$)
        B$ = _Inflate$(B$)
    End If
    O_OFFSET = 1
    B_OFFSET = 1
    T$ = String$((Len(B$) + (K > 0)) * 8 + K, 0)
    For I = 1 To Len(T$)
        C_I = _SHL(B3, 16) Or _SHL(B2, 8) Or B1
        Select Case _ReadBit(Asc(B$, B_OFFSET), J)
            Case 0: Asc(T$, I) = C(C_I)
                B = C(C_I)
            Case Else:
                B = Asc(O$, O_OFFSET): O_OFFSET = O_OFFSET + 1
                Asc(T$, I) = B
                C(C_I) = B
        End Select
        J = J + 1
        B_OFFSET = B_OFFSET - (J = 0)
        B3 = B2
        B2 = B1
        B1 = B
    Next I
    O$ = ""
    B$ = ""
    Decompress$ = T$
End Function
'$Include:'include\bits.bm'
