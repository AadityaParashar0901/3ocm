$Console:Only
If _CommandCount = 0 Then System
Dim As _Unsigned Long BS
Dim ST!, LT!, I$, O$, Y%, INFILE$, OUTFILE$, PATHPREFIX$, MODE%%, L&
If Command$(1) = "-c" Or Command$(1) = "--compress" Then MODE%% = 1
If Command$(1) = "-d" Or Command$(1) = "--decompress" Then MODE%% = 2
INFILE$ = Command$(2)
If _FileExists(INFILE$) = 0 Then PATHPREFIX$ = _StartDir$ + "\"
INFILE$ = PATHPREFIX$ + INFILE$
If _FileExists(INFILE$) = 0 Then Print "File "; Command$(2); " does not exists!": System
Y% = CsrLin + 1
Dim Shared RAW_MODE As _Byte
If _StriCmp(Command$(3), "r") = 0 Or _StriCmp(Command$(4), "r") = 0 Then RAW_MODE = -1 Else RAW_MODE = 0

Dim As _Unsigned Long I, B_OFFSET, O_OFFSET
Dim As _Unsigned _Byte B, B1, B2, B3, L
Dim As _Unsigned _Byte C(0 To 16777215)
Dim As _Unsigned Long C_I
Dim As _Unsigned _Bit * 3 J, K: J = 0

Select Case MODE%%
    Case 1: Print "Compressing": Open INFILE$ For Binary As #1
        OUTFILE$ = INFILE$ + ".3ocm"
        If RAW_MODE Then OUTFILE$ = OUTFILE$ + "_raw"
        Open OUTFILE$ For Output As #2
        Close #2
        Open OUTFILE$ For Binary As #2
        ST! = Timer(0.001)
        BS = 2 ^ Val(Command$(3))
        If BS = 1 Then BS = 2 ^ 20
        Do
            LT! = Timer(0.001)
            If LOF(1) - Seek(1) + 1 >= BS Then I$ = Space$(BS) Else I$ = Space$(LOF(1) - Seek(1) + 1)
            Get #1, , I$
            B$ = String$(Remain(Len(I$), 8), 0)
            O$ = String$(Len(I$), 0)
            B_OFFSET = 1: O_OFFSET = 1
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
            I$ = MKL$(Len(O$)) + MKL$(Len(B$)) + Chr$(J)
            Put #2, , I$: I$ = ""
            Put #2, , O$: O$ = ""
            Put #2, , B$: B$ = ""
            If LOF(1) <= Seek(1) - 1 Then Exit Do
            Locate Y%, 1: Print Round(100 * (Seek(1) - 1) / LOF(1)); "%", Round(100 * LOF(2) / (Seek(1) - 1)); "%", Round(Timer(0.001) - LT!); "s", Round((Timer(0.001) - LT!) / BS * (LOF(1) - Seek(1) + 1)); "s", Round(Timer(0.001) - ST!); "s"
        Loop
        Print "Ratio:"; Round(100 * LOF(2) / LOF(1)); "%"
        Print "Time: "; Timer(0.001) - ST!; "s"
        Close
    Case 2: Print "Decompressing": Open INFILE$ For Binary As #1
        OUTFILE$ = INFILE$ + ".out"
        Open OUTFILE$ For Output As #2
        Close #2
        Open OUTFILE$ For Binary As #2
        ST! = Timer(0.001)
        Do
            LT! = Timer(0.001)
            Get #1, , L~&
            If EOF(1) = -1 Then Exit Do
            O$ = String$(L~&, 0)
            Get #1, , L~&
            B$ = String$(L~&, 0)
            Get #1, , K$1
            K = Asc(K$1)
            Get #1, , O$
            Get #1, , B$
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
            Put #2, , T$
            Locate Y%, 1: Print Round(100 * (Seek(1) - 1) / LOF(1)); "%", Round(Timer(0.001) - LT!); "s", Round(Timer(0.001) - ST!); "s"
        Loop
        Print "Time: "; Timer(0.001) - ST!; "s"
        Close
End Select
System
Function Round (__N As Double)
    Round = Int(100 * __N) / 100
End Function
Function Remain~& (A~&, B~&)
    Remain~& = A~& \ B~& + Sgn(A~& Mod B~&)
End Function
Function NewHex$ (I&, T)
    H$ = Hex$(I&)
    NewHex$ = String$(T - Len(H$), 48) + H$
End Function
Function ByteToBits$ (__BYTE As _Unsigned _Byte, __MAX_LEN As _Unsigned _Byte)
    Dim __I As _Unsigned _Byte
    Dim __O$
    __O$ = String$(__MAX_LEN, 48)
    For __I = 0 To __MAX_LEN - 1
        If __BYTE And 2 ^ __I Then Asc(__O$, __MAX_LEN - __I) = 49
    Next __I
    ByteToBits$ = __O$
End Function
