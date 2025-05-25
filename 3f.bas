$Console:Only
Type Entry
    As _Unsigned _Byte Type, Dictionary
    As _Unsigned Long Size, CSize
    As _Unsigned Long Hash
End Type
Dim Shared As String DICTIONARY
Dim Shared As _Unsigned _Byte C(0 To 16777215)
FILE_EXT$ = ".3f"
Dim Shared As _Unsigned Long BLOCK_SIZE: BLOCK_SIZE = 1048576
Dim Shared As _Unsigned _Byte DEFLATE_MODE: DEFLATE_MODE = 1
Dim As _Unsigned Long I, J, Seed
Select Case Command$(1)
    Case "-a": COMMAND = 1
    Case "-x": COMMAND = 2
    Case "-l": COMMAND = 3
    Case "-t": COMMAND = 4
    Case "-e": COMMAND = 5
    Case "-d": COMMAND = 6
    Case Else: Print "3rd Order Context Model Compress Utility"
        Print "Usage: "; Command$(0); " {COMMAND} [OPTIONS] [FILEs]"
        Print "Commands:"
        Print "    -a    Add"
        Print "    -x [] Extract"
        Print "    -l    List"
        Print "    -t    Test"
        Print "    -e [] Encrypt"
        Print "    -d [] Decrypt"
        Print "Options:"
        Print "    -b []     Block Size"
        Print "    -d []     Dictionary"
        Print "    -od []    Output Dictionary"
        Print "    -rd []    Random Dictionary"
        Print "    --deflate"
        Print "    --no-deflate"
End Select
For I = 2 To _CommandCount
    Select Case Command$(I)
        Case "-b": BLOCK_SIZE = Val(Command$(I + 1)): I = I + 1
            BLOCK_SIZE = _SHL(1, BLOCK_SIZE)
            Print "Block Size = "; PrintSize$(BLOCK_SIZE)
        Case "-d": INFILE$ = Command$(I + 1): I = I + 1
            If _FileExists(INFILE$) = 0 Then INFILE$ = _StartDir$ + "\" + INFILE$
            If _FileExists(INFILE$) = 0 Then Print Command$(I); " does not exists.": _Continue
            Print "Reading Dictionary from File "; INFILE$
            Open INFILE$ For Binary As #1
            D$ = String$(LOF(1), 0)
            Get #1, , D$
            Close #1
            DICTIONARY = OneByteDecode(D$)
            D$ = ""
            For J = 0 To 16777215
                C(J) = Asc(DICTIONARY, J + 1)
            Next J
        Case "-od": INFILE$ = Command$(I + 1): I = I + 1
            Print "Writing Dictionary to File "; INFILE$
            DICTIONARY = String$(16777216, 0)
            For J = 0 To 16777215
                Asc(DICTIONARY, J + 1) = C(J)
            Next J
            D$ = OneByteEncode(DICTIONARY)
            If _FileExists(INFILE$) Then Kill INFILE$
            Open INFILE$ For Binary As #1
            Put #1, , D$
            Close #1
        Case "-rd": Seed = Val(Command$(I + 1)): I = I + 1
            If Seed = 0 Then Seed = Timer
            Print "Creating Random Dictionary"
            For J = 0 To 16777215
                Randomize Seed
                C(J) = Int(Rnd * 256)
            Next J
        Case "--deflate": DEFLATE_MODE = 1
            Print "Using Deflate"
        Case "--no-deflate": DEFLATE_MODE = 0
            Print "Not Using Deflate"
        Case Else: INFILE$ = Command$(I)
            If _FileExists(INFILE$) = 0 Then INFILE$ = _StartDir$ + "\" + INFILE$
            If _FileExists(INFILE$) = 0 Then Print Command$(I); " does not exists.": _Continue
            Open INFILE$ For Binary As #1
            Y% = CsrLin + 1
            Select Case COMMAND
                Case 1
                    Print "Compressing "; INFILE$; " -> "; INFILE$ + FILE_EXT$
                    Open INFILE$ + FILE_EXT$ For Output As #2: Close #2
                    Open INFILE$ + FILE_EXT$ For Binary As #2
                    ST! = Timer(0.001): Do: LT! = Timer(0.001)
                        I$ = String$(Min(LOF(1) - Seek(1) + 1, BLOCK_SIZE), 0)
                        Get #1, , I$: O$ = Compress$(I$)
                        I$ = MKL$(Len(O$)) + O$: O$ = ""
                        Put #2, , I$
                        P = Round(100 * (Seek(1) - 1) / LOF(1))
                        Locate Y%, 1: Print "[" + String$(P, 45) + Space$(100 - P) + "]"; P; "% "
                        Print " Ratio | Speed | Remaining Time | Elapsed Time | Processed | Compressed Size"
                        T$ = _Trim$(Str$(Round(100 * LOF(2) / (Seek(1) - 1)))) + "%"
                        T$ = T$ + Space$(8 - Len(T$)) + PrintSize$(Round((Seek(1) - 1) / (Timer(0.001) - ST!))) + "/s"
                        T$ = T$ + Space$(22 - Len(T$)) + PrintTime$(Round((Timer(0.001) - ST!) * (LOF(1) / (Seek(1) - 1) - 1)))
                        T$ = T$ + Space$(38 - Len(T$)) + PrintTime$(Round(Timer(0.001) - ST!))
                        T$ = T$ + Space$(50 - Len(T$)) + PrintSize$(Seek(1) - 1)
                        T$ = T$ + Space$(65 - Len(T$)) + PrintSize$(LOF(2))
                        Print T$ + Space$(78 - Len(T$))
                        If LOF(1) < Seek(1) Then Exit Do
                    Loop
                    Locate Y%, 1: Print "Ratio: "; Round(100 * LOF(2) / LOF(1)); "% => "; PrintSize$(LOF(1)); " -> "; PrintSize$(LOF(2)); Space$(80)
                    Print "Time: "; PrintTime$(Timer(0.001) - ST!); Space$(80)
                    Print Space$(100)
                    Locate Y% + 2, 1
                    Close
                Case 2
                    Print "Decompressing "; INFILE$; " -> "; INFILE$ + ".out"
                    Open INFILE$ + ".out" For Output As #2: Close #2
                    Open INFILE$ + ".out" For Binary As #2
                    ST! = Timer(0.001): Do: LT! = Timer(0.001)
                        I$ = String$(Min(LOF(1) - Seek(1) + 1, BLOCK_SIZE), 0)
                        Get #1, , L&
                        If EOF(1) Then Exit Do
                        I$ = String$(L&, 0)
                        Get #1, , I$
                        O$ = Decompress$(I$)
                        Put #2, , O$
                        P = Round(100 * (Seek(1) - 1) / LOF(1))
                        Locate Y%, 1: Print "[" + String$(P, 45) + Space$(100 - P) + "]"
                        Print " Speed | Elapsed Time"
                        T$ = Space$(3) + PrintTime$(Round(Timer(0.001) - LT!))
                        T$ = T$ + Space$(13 - Len(T$)) + PrintTime$(Round(Timer(0.001) - ST!))
                        T$ = T$ + Space$(22 - Len(T$))
                        Print T$
                    Loop
                    Locate Y%, 1: Print "Time: "; Timer(0.001) - ST!: Print
                    Locate Y% + 1, 1
                    Close
                Case 3

            End Select
    End Select
Next I
System
Function Compress$ (I$)
    Dim As _Unsigned Long I, B_OFFSET, O_OFFSET
    Dim As _Unsigned _Byte B, B1, B2, B3
    Dim As _Unsigned Long C_I
    Dim As _Unsigned _Bit * 3 J: J = 0
    B$ = String$(Remain(Len(I$), 8), 0)
    O$ = String$(Len(I$), 0)
    B_OFFSET = 1
    O_OFFSET = 1
    $Checking:Off
    For I = 1 To Len(I$)
        B = Asc(I$, I)
        C_I = _SHL(B3, 16) Or _SHL(B2, 8) Or B1
        Select Case C(C_I) = B
            Case 0:
                Asc(B$, B_OFFSET) = _SetBit(Asc(B$, B_OFFSET), J)
                Asc(O$, O_OFFSET) = B - C(C_I): O_OFFSET = O_OFFSET + 1
                C(C_I) = B
        End Select
        J = J + 1
        B_OFFSET = B_OFFSET - (J = 0)
        B3 = B2
        B2 = B1
        B1 = B
    Next I
    $Checking:On
    O_OFFSET = O_OFFSET - 1
    O$ = Left$(O$, O_OFFSET)
    B$ = Left$(B$, B_OFFSET)
    If DEFLATE_MODE Then
        O$ = OneByteEncode$(O$) 'O$ = _Deflate$(O$)
        B$ = _Deflate$(B$)
    End If
    Compress$ = MKL$(Len(O$)) + Chr$(J) + O$ + B$
    O$ = ""
    B$ = ""
End Function
Function Decompress$ (I$)
    Dim As _Unsigned Long I, B_OFFSET, O_OFFSET
    Dim As _Unsigned _Byte B, B1, B2, B3
    Dim As _Unsigned Long C_I
    Dim As _Unsigned _Bit * 3 J, K: J = 0
    O$ = Mid$(I$, 6, CVL(Left$(I$, 4)))
    B$ = Mid$(I$, 6 + Len(O$))
    K = Asc(I$, 5)
    If DEFLATE_MODE Then
        O$ = OneByteDecode$(O$)
        B$ = _Inflate$(B$)
    End If
    O_OFFSET = 1
    B_OFFSET = 1
    T$ = String$((Len(B$) + (K > 0)) * 8 + K, 0)
    $Checking:Off
    For I = 1 To Len(T$)
        C_I = _SHL(B3, 16) Or _SHL(B2, 8) Or B1
        Select Case _ReadBit(Asc(B$, B_OFFSET), J)
            Case 0: Asc(T$, I) = C(C_I)
                B = C(C_I)
            Case Else:
                B = Asc(O$, O_OFFSET) + C(C_I): O_OFFSET = O_OFFSET + 1
                Asc(T$, I) = B
                C(C_I) = B
        End Select
        J = J + 1
        B_OFFSET = B_OFFSET - (J = 0)
        B3 = B2
        B2 = B1
        B1 = B
    Next I
    $Checking:On
    O$ = ""
    B$ = ""
    Decompress$ = T$
End Function
'$Include:'include\bytetobits.bm'
Function Round (__N As Double)
    Round = Int(100 * __N) / 100
End Function
Function Remain~& (A~&, B~&)
    Remain~& = A~& \ B~& + Sgn(A~& Mod B~&)
End Function
Function PrintTime$ (__T As Single)
    If __T = 0 Then PrintTime$ = "0": Exit Function
    __H = __T \ 3600
    __M = (__T Mod 3600) \ 60
    __S = __T Mod 60
    If __H Then T$ = _Trim$(Str$(__H)) + "h "
    If __H Or __M Then T$ = T$ + _Trim$(Str$(__M)) + "m "
    If __T - Int(__T) > 0 Then D$ = _Trim$(Str$(Round(__T - Int(__T)))) Else D$ = ""
    PrintTime$ = T$ + _Trim$(Str$(__S)) + D$ + "s"
End Function
Function PrintSize$ (__T As _Unsigned Long)
    If __T = 0 Then
        PrintSize$ = "0 B"
        Exit Function
    End If
    Select Case Int(Log(__T) / Log(2) / 10)
        Case 0: PrintSize$ = _Trim$(Str$(__T)) + " B"
        Case 1: PrintSize$ = _Trim$(Str$(Round(__T / _SHL(1, 10)))) + " KB"
        Case 2: PrintSize$ = _Trim$(Str$(Round(__T / _SHL(1, 20)))) + " MB"
        Case 3: PrintSize$ = _Trim$(Str$(Round(__T / _SHL(1, 30)))) + " GB"
    End Select
End Function
Function OneByteEncode$ (__I$)
    Dim As _Unsigned _Byte __ONEBYTE, __C
    Dim As _Unsigned Long __BYTE_BUFFER_OFFSET, __POSITION_BUFFER_OFFSET, __I, __LENA, __Frequency_Table(0 To 255)
    Dim __J As _Unsigned _Bit * 3
    Dim As String __BYTE_BUFFER, __POSITION_BUFFER
    __LENA = Len(__I$)
    For __I = 1 To __LENA
        __BYTE~%% = Asc(__I$, __I)
        __Frequency_Table(__BYTE~%%) = __Frequency_Table(__BYTE~%%) + 1
    Next __I
    For __BI~%% = 0 To 255
        If __Frequency_Table(__BI~%%) > __Frequency_Table(__ONEBYTE) Then __ONEBYTE = __BI~%%
    Next __BI~%%
    __BYTE_BUFFER = String$(Len(__I$), 0): __POSITION_BUFFER = String$(Remain(Len(__I$), 8) + 1, 0)
    For __I = 1 To Len(__I$)
        __C = Asc(__I$, __I): If __J = 0 Then __POSITION_BUFFER_OFFSET = __POSITION_BUFFER_OFFSET + 1
        If __C <> __ONEBYTE Then
            Asc(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET) = _SetBit(Asc(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET), __J)
            __BYTE_BUFFER_OFFSET = __BYTE_BUFFER_OFFSET + 1: Asc(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET) = __C
        End If
        __J = __J + 1
    Next __I
    __POSITION_BUFFER = _Deflate$(Left$(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET))
    __BYTE_BUFFER = _Deflate$(Left$(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET))
    OneByteEncode$ = MKL$(Len(__I$)) + MKL$(Len(__POSITION_BUFFER)) + MKL$(Len(__BYTE_BUFFER)) + Chr$(__ONEBYTE) + __POSITION_BUFFER + __BYTE_BUFFER
    __POSITION_BUFFER = ""
    __BYTE_BUFFER = ""
End Function
Function OneByteDecode$ (__I$)
    Dim As _Unsigned Long __I, __BYTE_BUFFER_OFFSET, __POSITION_BUFFER_OFFSET
    Dim As _Unsigned _Bit * 3 __J
    Dim As String __BYTE_BUFFER, __POSITION_BUFFER, __OUT_BUFFER
    __OUT_LENGTH~& = CVL(Left$(__I$, 4))
    __POSITION_BUFFER_DEFLATE_LENGTH~& = CVL(Mid$(__I$, 5, 4))
    __BYTE_BUFFER_DEFLATE_LENGTH~& = CVL(Mid$(__I$, 9, 4))
    __ONEBYTE~%% = Asc(__I$, 13)
    __POSITION_BUFFER = _Inflate$(Mid$(__I$, 14, __POSITION_BUFFER_DEFLATE_LENGTH~&))
    __BYTE_BUFFER = _Inflate$(Mid$(__I$, 14 + __POSITION_BUFFER_DEFLATE_LENGTH~&, __BYTE_BUFFER_DEFLATE_LENGTH~&))
    __OUT_BUFFER = String$(__OUT_LENGTH~&, 0)
    __POSITION_BUFFER_OFFSET = 0
    __BYTE_BUFFER_OFFSET = 0
    For __I = 1 To __OUT_LENGTH~&
        If __J = 0 Then __POSITION_BUFFER_OFFSET = __POSITION_BUFFER_OFFSET + 1
        If _ReadBit(Asc(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET), __J) Then
            __BYTE_BUFFER_OFFSET = __BYTE_BUFFER_OFFSET + 1
            Asc(__OUT_BUFFER, __I) = Asc(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET)
        Else
            Asc(__OUT_BUFFER, __I) = __ONEBYTE~%%
        End If
        __J = __J + 1
    Next __I
    __POSITION_BUFFER = ""
    __BYTE_BUFFER = ""
    OneByteDecode = __OUT_BUFFER
End Function
'$Include:'include\min.bm'
'$Include:'include\crc32.bm'
