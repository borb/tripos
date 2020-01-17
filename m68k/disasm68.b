/*****************************************************************************\
*                           Systems Research Group                            *
*******************************************************************************


           ##    ##   ######    ######   ##    ##  ######      ####   
           ###  ###  ########  ########  ##   ##   #######    ######  
           ########  ##        ##    ##  ##  ##    ##    ##  ##    ## 
           ## ## ##  #######    ######   ####      ##    ##  ######## 
           ##    ##  ##    ##  ##    ##  ## ##     ##    ##  ##    ## 
           ##    ##  ##    ##  ##    ##  ##  ##    ##    ##  ##    ## 
           ##    ##   ######   ########  ##   ##   #######   ##    ## 
           ##    ##    ####     ######   ##    ##  ######    ##    ## 


*******************************************************************************
*   I. D. Wilson           Last Modified   -   IDW   -   19/03/84             *
\*****************************************************************************/



SECTION "M68KDA"


GET "LIBHDR"


GLOBAL
$(
    word              :  ug + 0
    size              :  ug + 1
    reg               :  ug + 2
    opm               :  ug + 3
    bit8              :  ug + 4
    bits54            :  ug + 5
    progaddress       :  ug + 6
    progsize          :  ug + 7
    lbuff             :  ug + 8
    llen              :  ug + 9
    lpos              :  ug + 10
    reg2              :  ug + 11
    hunktop           :  ug + 12
    swrch             :  ug + 13
    reg1              :  ug + 14
    disasmlevel       :  ug + 15
    disasmlabel       :  ug + 16
    currentloc        :  ug + 17
    wordbuff          :  ug + 18
    infile            :  ug + 19
    outfile           :  ug + 20
    sysout            :  ug + 21
    memorybuff        :  ug + 22
    memorybuffp       :  ug + 23
    progbase          :  ug + 24
    progtop           :  ug + 25
$)


MANIFEST
$(
    //  Instruction Opcodes
    //  ===================

    opcodemask    =  #B1111000000000000  // Opcode is in the top 4 bits.

    f.b0000       =  #B0000000000000000  // Bit Manipulation/MOVEP/Immediate
    f.b0001       =  #B0001000000000000  // Move Byte
    f.b0010       =  #B0010000000000000  // Move Long
    f.b0011       =  #B0011000000000000  // Move Word
    f.b0100       =  #B0100000000000000  // Miscellaneous (?)
    f.b0101       =  #B0101000000000000  // ADDQ/SUBQ/Scc/DBcc
    f.b0110       =  #B0110000000000000  // Bcc
    f.b0111       =  #B0111000000000000  // MOVEQ
    f.b1000       =  #B1000000000000000  // OR/DIV/SBCD
    f.b1001       =  #B1001000000000000  // SUB/SUBX
    f.b1010       =  #B1010000000000000  // *** Unassigned ***
    f.b1011       =  #B1011000000000000  // CMP/EOR
    f.b1100       =  #B1100000000000000  // AND/MUL/ABCD/EXG
    f.b1101       =  #B1101000000000000  // ADD/ADDX
    f.b1110       =  #B1110000000000000  // Shift/Rotate
    f.b1111       =  #B1111000000000000  // *** Unassigned ***


    //  Bit Type Codes
    //  ==============

    b.tst         =  #B00          // TEST bit
    b.chg         =  #B01          // CHANGE bit
    b.clr         =  #B10          // CLEAR bit
    b.set         =  #B11          // SET bit


    //  Operand Size Codes
    //  ==================

    sizemask      =  #B11          // Two bit size field
    s.byte        =  #B00          // BYTE operation
    s.word        =  #B01          // WORD operation
    s.long        =  #B10          // LONG operation


    //  Operand MODE Codes
    //  ==================

    registermask  =  #B111         // 3 bit register field

    am.Dr         =  #B000         // DATA register direct
    am.Ar         =  #B001         // ADDRESS register direct
    am.Ar.ind     =  #B010         // ADDRESS register indirect
    am.Ar.pi      =  #B011         // ADDRESS register indirect
    am.Ar.pd      =  #B100         // ADDRESS register indirect pre decrement
    am.Ar.disp    =  #B101         // ADDRESS register indirect + displacement
    am.Ar.index   =  #B110         // ADDRESS register indirect + index


    //  All the rest assume a register field of #B111
    //  =============================================

    am.abs16      =  #B000         // Absolute SHORT
    am.abs32      =  #B001         // Absolute Long
    am.PC.disp    =  #B010         // PC + displacement
    am.PC.index   =  #B011         // PC + index
    am.imm        =  #B100         // Immediate or SR
$)




MANIFEST
$(
    //  Internal representation for decoded instructions
    //  ================================================

    i.move.byte       =  1
    i.move.long       =  2
    i.move.word       =  3
    i.DBcc            =  4
    i.Scc             =  5
    i.addq            =  6
    i.subq            =  7
    i.Bcc             =  8
    i.moveq           =  9
    i.divu            =  10
    i.mulu            =  11
    i.divs            =  12
    i.muls            =  13
    i.sbcd            =  14
    i.or              =  15
    i.subx            =  16
    i.sub             =  17
    i.cmpm            =  18
    i.eor             =  19
    i.cmp             =  20
    i.abcd            =  21
    i.exg             =  22
    i.and             =  23
    i.addx            =  24
    i.add             =  25
    i.chk             =  26
    i.lea             =  27
    i.move.from.sr    =  28
    i.negx            =  29
    i.move.to.sr      =  30
    i.not             =  31
    i.clr             =  32
    i.move.to.ccr     =  33
    i.neg             =  34
    i.nbcd            =  35
    i.swap            =  36
    i.pea             =  37
    i.extw            =  38
    i.extl            =  39
    i.movem.from.reg  =  40
    i.movem.to.reg    =  41
    i.jmp             =  42
    i.jsr             =  43
    i.trap            =  44
    i.link            =  45
    i.unlk            =  46
    i.move.to.usp     =  47
    i.move.from.usp   =  48
    i.reset           =  49
    i.nop             =  50
    i.stop            =  51
    i.rte             =  52
    i.rts             =  53
    i.trapv           =  54
    i.rtr             =  55
    i.movep           =  56
    i.bit.dynamic     =  57
    i.ori             =  58
    i.andi            =  59
    i.subi            =  60
    i.addi            =  61
    i.eori            =  62
    i.cmpi            =  63
    i.bit.static      =  64
    i.tas             =  65
    i.tst             =  66
$)



MANIFEST
$(
    t.hunk            =  1000
    t.reloc16         =  1010
    t.reloc32         =  1001
    t.end             =  1002
    t.abshunk         =  1003
    t.absrel16        =  1011
    t.absrel32        =  1004
    t.ext             =  1005

    ext.defrel        =  1
    ext.defabs        =  2
    ext.ref           =  129

    size.b            =  1
    size.w            =  2
    size.l            =  4
$)



LET start()  BE
$(
    LET args      =  "FILE/A,TO/K"
    LET argv      =  VEC 50
    LET v         =  VEC 256/bytesperword
    LET fromfile  =  0
    LET tofile    =  0

    swrch        :=  wrch
    sysout       :=  output()
    memorybuff   :=  0
    infile       :=  0
    outfile      :=  0
    lbuff        :=  v

    //  First, get the name of the file to be disassembled, and try to
    //  open it.

    UNLESS  rdargs( args, argv, 50 )  THEN
        error( "Bad arguments for string *"%S*"", args )

    infile  :=  findinput( argv!0 )

    IF  infile = 0  THEN
        error( "Cannot open input file *"%S*"", argv!0 )

    TEST  argv!1 \= 0  THEN
    $(
        outfile  :=  findoutput( argv!1 )

        IF  outfile = 0  THEN
        $(
            selectinput( infile )
            endread()
            error( "Cannot open output file *"%S*"", argv!1 )
        $)
    $)
    ELSE  outfile  :=  0

    writef( "M68KDA  Version 3.13  Dis-assembling *"%S*"*N", argv!0 )

    selectinput( infile )
    selectoutput( outfile = 0  ->  sysout, outfile )

    $(  //  Main decoding loop of the disassembler

        LET l     =  readwords( @wordbuff, 1 )
        LET name  =  VEC 1

        IF  l = 0                THEN  BREAK
        IF  testflags( #B0001 )  THEN  err( "BREAK" )

        SWITCHON  wordbuff  INTO
        $(
            CASE t.abshunk      :  readwords( @progaddress, 1 )
                                   readwords( @progsize, 1 )

                                   progaddress  :=  progaddress * bytesperword
                                   progsize     :=  progsize    * bytesperword

                                   writef( "ABS Hunk.  ORG = $%X6  Size = %N*N*N",
                                            progaddress, progsize )

                                   readhunk()

                                   newline()
                                   LOOP

            CASE t.hunk         :  progaddress  :=  0
                                   readwords( @progsize, 1 )

                                   progsize  :=  progsize * bytesperword

                                   writef( "Hunk.  Size = %N*N*N", progsize )

                                   readhunk()
                                   
                                   newline()
                                   LOOP


            CASE t.reloc16      :
            CASE t.reloc32      :
            CASE t.absrel16     :
            CASE t.absrel32     :  readwords( @progsize, 1 )
            
                                   writef( "%S.  Size = %N",
                                            ( wordbuff = t.reloc16  ->  "Reloc16",
                                              wordbuff = t.reloc32  ->  "Reloc32",
                                              wordbuff = t.absrel16 ->  "Absrel16",
                                              wordbuff = t.absrel32 ->  "Absrel32",
                                              "<Error>" ), progsize )

                                   FOR  i = 0  TO  progsize-1  DO
                                   $(
                                       readwords( @wordbuff, 1 )
                                       
                                       IF  i REM 6  =  0        THEN  newline()
                                       IF  testflags( #B0001 )  THEN  err( "BREAK" )
                                       
                                       writef( "%X8  ", wordbuff )
                                   $)

                                   writes( "*N*N" )
                                   LOOP


            CASE t.ext          :  writes( "Ext.*N" )

                                   $(  //  Repeat loop to get the list of
                                       //  external symbols.
                                       
                                       LET type  =  0
                                       
                                       IF  testflags( #B0001 )  THEN  err( "BREAK" )

                                       readwords( name+0, 1 )
                                       
                                       IF  name!0 = 0  THEN  BREAK
                                       
                                       readwords( name+1, 1 )
                                       
                                       type  :=  name % 0
                                       
                                       wrch( '"' )
                                       FOR  i = 1  TO  7  DO  wrch( name % i )
                                       wrch( '"' )
                                       
                                       writes( "    " )
                                       
                                       TEST  type = ext.defrel  |  type = ext.defabs  THEN
                                       $(
                                           readwords( @wordbuff, 1 )

                                           writes( type = ext.defrel  ->  "Defrel", 
                                                                          "Defabs" )
                                                                          
                                           writef( "  %X8*N", wordbuff )
                                       $)
                                       ELSE
                                       
                                       TEST  type = ext.ref  THEN
                                       $(
                                           readwords( @progsize, 1 )
                                           
                                           writef( "Ref     Size = %N", progsize )
                                           
                                           FOR  i = 0  TO  progsize-1  DO
                                           $(
                                               readwords( @wordbuff, 1 )

                                               IF  i REM 6  =  0  THEN  newline()
                                               
                                               writef( "%X8  ", wordbuff )
                                           $)
                                           
                                           newline()
                                       $)
                                       ELSE  err( "Illegal type %N after T.EXT", type )
                                   $)
                                   REPEAT

                                   newline()                                   
                                   LOOP


            CASE t.end          :  LOOP

            DEFAULT             :  err( "Illegal type %N in module", wordbuff )
        $)
    $)
    REPEAT

    endread()
    endoutput()
    writes( "*NM68KDA Finished.*N" )
$)



AND readhunk()  BE
$(
    progbase  :=  progaddress
    progtop   :=  progaddress + progsize

    memorybuff   :=  getvec( progsize / bytesperword )

    IF  memorybuff = 0  THEN
        err( "Cannot get hunk buffer" )

    memorybuffp  :=  0

    readwords( memorybuff, progsize / bytesperword )

    WHILE  progaddress < progtop  DO
    $(
        IF  testflags( #B0001 )  THEN  err( "BREAK" )

        listloc()
    $)

    freevec( memorybuff )
    memorybuff  :=  0
$)



AND endoutput()  BE  UNLESS  outfile = 0  DO
$(
    endstream( outfile )

    selectoutput( sysout )
$)



AND listloc()  BE
$(
    LET lbase  =  0

    lpos  :=  0
    llen  :=  0

    FOR  i = 0  TO  255  DO  lbuff % i  :=  '*S'

    currentloc   :=  progaddress
    disasmlevel  :=  level()
    word         :=  readword()

    wrch  :=  writechar
    writef( "$%X6:  ", currentloc )

    disasm()

disasmlabel:

    lbase  :=  llen < 40  ->  40, llen + 1

    lpos   :=  lbase + 0

    FOR  i = currentloc  TO  progaddress-1  BY  size.b  DO
         wrch( c( location( i, size.b ) ) )

    lpos   :=  lbase + 12

    FOR  i = currentloc  TO  progaddress-1  BY  size.w  DO
         writef( "%X4 ", location( i, size.w ) )

    wrch  :=  swrch

    FOR  i = 0  TO  llen - 1  DO  wrch( lbuff % i )
    newline()
$)



AND location( address, size )  =  VALOF
$(
    LET v  =  0

    FOR  i = 0  TO  size - 1  DO
         v  :=  (v << 8)  +  memorybuff % (address - progbase + i)

    RESULTIS  v
$)



AND c( char )  =
    ( "................................ !*"#$%&'()**+,-./0123456789:;<=>?*
      *@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]\^_`abcdefghijklmnopqrstuvwxyz{}|~."
    ) % ((char & #X7F) + 1)



AND readword()  =  VALOF
$(
    LET word  =  get2bytes( memorybuff, memorybuffp )

    memorybuffp  :=  memorybuffp + 1
    progaddress  :=  progaddress + 2

    RESULTIS  word
$)



AND writechar( ch )  BE
$(
    lbuff % lpos  :=  ch
    lpos          :=  lpos + 1

    IF  lpos > llen  THEN  llen  :=  lpos
$)



AND disasm()  BE
$(
//  Dis-assemble the instruction whose first 16 bits are given in the global
//  "word".  The instruction is split up into certain essential parts, and
//  is then decoded depending on the Opcode field (Most significant 4 bits).

    size    :=  sizefield()
    reg     :=  registerfield()
    opm     :=  opmodefield()
    bits54  :=  bits54field()
    reg1    :=  reg1field()
    reg2    :=  reg2field()
    bit8    :=  bit( 8 )

    SWITCHON  word & opcodemask  INTO
    $(
        CASE f.b0000 :  decode0000()             ;  ENDCASE

        CASE f.b0001 :  size  :=  s.byte
                        gen( i.move.byte )       ;  ENDCASE

        CASE f.b0010 :  size  :=  s.long
                        gen( i.move.long )       ;  ENDCASE

        CASE f.b0011 :  size  :=  s.word
                        gen( i.move.word )       ;  ENDCASE

        CASE f.b0100 :  misc1()                  ;  ENDCASE

        CASE f.b0101 :  TEST  size = #B11  THEN
                              TEST reg = #B001
                                   THEN  gen( i.DBcc )
                                   ELSE  gen( i.Scc  )
                        ELSE  TEST bit8 = 0
                                   THEN  gen( i.addq )
                                   ELSE  gen( i.subq )
                        ENDCASE

        CASE f.b0110 :  gen( i.Bcc )             ;  ENDCASE
        CASE f.b0111 :  gen( i.moveq )           ;  ENDCASE

        CASE f.b1000 :  TEST  opm    = #B011  THEN  gen( i.divu )  ELSE
                        TEST  opm    = #B111  THEN  gen( i.divs )  ELSE
                        TEST  opm    = #B100  &
                              reg    = #B000  THEN  gen( i.sbcd )  ELSE
                              gen( i.or )
                        ENDCASE

        CASE f.b1001 :  TEST  size = #B11               THEN  gen( i.sub  )   ELSE
                        TEST  reg = am.Dr  &  bit8 = 1  THEN  gen( i.subx )   ELSE
                        TEST  reg = am.Ar  &  bit8 = 1  THEN  gen( i.subx )   ELSE
                              gen( i.sub )
                        ENDCASE

        CASE f.b1010 :  dataword()               ;  ENDCASE

        CASE f.b1011 :  TEST  size = #B11  THEN  gen( i.cmp )  ELSE
                        TEST  bit8 = 1  THEN
                              TEST  reg = #B001
                                    THEN  gen( i.cmpm )
                                    ELSE  gen( i.eor  )
                        ELSE  gen( i.cmp )
                        ENDCASE

        CASE f.b1100 :  TEST  opm    = #B011  THEN  gen( i.mulu )  ELSE
                        TEST  opm    = #B111  THEN  gen( i.muls )  ELSE
                        TEST  bits54 = #B00   THEN
                              TEST  opm  = #B100 THEN gen( i.abcd ) ELSE
                              TEST  opm  = #B110 THEN gen( i.exg )  ELSE
                              TEST  opm  = #B101 THEN gen( i.exg )
                              ELSE  gen( i.and )
                        ELSE  gen( i.and )
                        ENDCASE

        CASE f.b1101 :  TEST  size = #B11               THEN  gen( i.add  )   ELSE
                        TEST  reg = am.Dr  &  bit8 = 1  THEN  gen( i.addx )   ELSE
                        TEST  reg = am.Ar  &  bit8 = 1  THEN  gen( i.addx )   ELSE
                              gen( i.add )
                        ENDCASE

        CASE f.b1110 :  TEST  size = #B11
                              THEN  memshift()
                              ELSE  registershift()
                        ENDCASE

        CASE f.b1111 :  dataword()               ;  ENDCASE


        DEFAULT      :  // Should Never Happen!
                        // ====================

                        err( "Bad opcode in *"disasm*" - %X4", word )
    $)
$)



AND misc1() BE
$(
// Deals with all the miscellaneous instructions which have
// opcode #B0100.

    TEST  bit8 = 1  THEN
    $(
        TEST size = #B10  THEN gen( i.chk )    ELSE
        TEST size = #B11  THEN gen( i.lea )    ELSE
             dataword()
    $)
    ELSE
    $(
        SWITCHON  (word >> 9) & #B111  INTO
        $(
            CASE #B000 :  TEST  size = #B11
                                THEN gen( i.move.from.sr )
                                ELSE gen( i.negx )
                          ENDCASE


            CASE #B001 :  gen( i.clr )
                          ENDCASE


            CASE #B010 :  TEST  size = #B11
                                THEN  gen( i.move.to.ccr )
                                ELSE  gen( i.neg )
                          ENDCASE

            CASE #B011 :  TEST  size = #B11
                                THEN  gen( i.move.to.sr )
                                ELSE  gen( i.not )
                          ENDCASE


            CASE #B100 :  TEST  size = #B00  THEN  gen( i.nbcd )  ELSE
                          TEST  size = #B01  THEN
                                TEST  reg = am.Dr
                                      THEN  gen( i.swap )
                                      ELSE  gen( i.pea )
                          ELSE  TEST  reg = am.Dr  THEN
                                      gen( bit( 6 ) = 0  ->  i.extw, i.extl )
                                ELSE  gen( i.movem.from.reg )
                          ENDCASE


            CASE #B101 :  TEST  size = #B11
                                THEN  gen( i.tas )
                                ELSE  gen( i.tst )
                          ENDCASE


            CASE #B110 :  gen( i.movem.to.reg )
                          ENDCASE


            CASE #B111 :  TEST  size    = #B11  THEN  gen( i.jmp )  ELSE
                          TEST  size    = #B10  THEN  gen( i.jsr )  ELSE
                          TEST  bits54  = #B00  THEN  gen( i.trap)  ELSE
                                misc2()
                          ENDCASE
        $)
    $)
$)



AND misc2() BE
$(
    SWITCHON  reg  INTO
    $(
        DEFAULT    :  dataword()                   ;  RETURN
        CASE #B010 :  gen( i.link )                ;  RETURN
        CASE #B011 :  gen( i.unlk )                ;  RETURN
        CASE #B100 :  gen( i.move.to.usp )         ;  RETURN
        CASE #B101 :  gen( i.move.from.usp )       ;  RETURN

        CASE #B110 :  // Go on to the next part of the decoding
                      ENDCASE
    $)


    SWITCHON  word & #B111  INTO
    $(
        CASE #B000 :  gen( i.reset )               ;  ENDCASE
        CASE #B001 :  gen( i.nop )                 ;  ENDCASE
        CASE #B010 :  gen( i.stop )                ;  ENDCASE
        CASE #B011 :  gen( i.rte )                 ;  ENDCASE
        CASE #B101 :  gen( i.rts )                 ;  ENDCASE
        CASE #B110 :  gen( i.trapv )               ;  ENDCASE
        CASE #B111 :  gen( i.rtr )                 ;  ENDCASE

        DEFAULT    :  dataword()                   ;  ENDCASE
    $)
$)



AND decode0000()  BE
$(
    TEST  bit8 = 1  THEN
        //  If bit 8 is a one, then this is a dynamic
        //  BIT or a MOVEP instruction.  If the register
        //  field of the effective address is ADDRESS
        //  then it must be a MOVEP - assumed BIT otherwise

        TEST  reg = am.Ar
              THEN  gen( i.movep )
              ELSE  gen( i.bit.dynamic )

    ELSE
    $(
        SWITCHON  (word >> 9) & #B111  INTO
        $(
            CASE  #B000 :  gen( i.ori )                ;  ENDCASE
            CASE  #B001 :  gen( i.andi )               ;  ENDCASE
            CASE  #B010 :  gen( i.subi )               ;  ENDCASE
            CASE  #B011 :  gen( i.addi )               ;  ENDCASE
            CASE  #B101 :  gen( i.eori )               ;  ENDCASE
            CASE  #B110 :  gen( i.cmpi )               ;  ENDCASE
            CASE  #B100 :  gen( i.bit.static )         ;  ENDCASE

            DEFAULT     :  dataword()                  ;  ENDCASE
        $)
    $)
$)



AND dataword() BE
$(
// The decoding of the instruction has failed abismally, and so, we
// can only assume that this is a word of data.

    writes( "DC.W" )
    setoploc()
    writef( "$%X4", word )
$)



AND registershift() BE
$(
// More decoding is necessary, before we can print out this instruction
// we know that it is in the general class of data-register-shift
// instructions.

    LET ir    =  bit( 5 )
    LET type  =  reg & #B11

    writes( rcode( type ) )
    wrch( bit8 = 0  ->  'R', 'L' )
    writesize()
    setoploc()

    TEST  ir = 0  THEN
          writef( "#%N,", reg2 = 0  ->  8, reg2 )
    ELSE  writef( "D%N,", reg2 )

    writef( "D%N", reg1 )
$)



AND memshift() BE
$(
// Like "registershift" - this needs more decoding, but we
// assume that it is a memory shift instruction

    LET type  =  reg2 & #B11

    writes( rcode( type ) )
    wrch( bit8 = 0  ->  'R', 'L' )
    setoploc()

    write.ea( word )
$)



AND gen( instruction ) BE
$(
//  We now know what sort of instruction we are dealing with, so decoding
//  is now easy.  First write out the Opcode of the instruction.

    LET opmode   =  ?
    LET regmask  =  ?

    writes( opcode( instruction ) )

    SWITCHON  instruction  INTO
    $(
        CASE i.move.byte        :
        CASE i.move.word        :
        CASE i.move.long        :  setoploc()
                                   write.ea( word )
                                   wrch( ',' )
                                   write.ea( swapea( word >> 6 ) )
                                   ENDCASE

        CASE i.DBcc             :
        CASE i.Scc              :
        CASE i.Bcc              :  writes( ccode( (word >> 8) & #B1111, instruction ) )

                                   TEST  instruction = i.DBcc  THEN
                                   $(
                                       setoploc()
                                       writef( "D%N,", reg1 )
                                       writeoffset( extend( readword() ) )
                                   $)
                                   ELSE

                                   TEST  instruction = i.Bcc  THEN
                                   $(
                                       LET short   =  bextend( word & #XFF )
                                       LET offset  =  ?

                                       TEST  (word & #XFF) = 0  THEN
                                             offset  :=  extend( readword() )
                                       ELSE
                                       $(
                                           writes( ".S" )
                                           offset  :=  short
                                       $)

                                       setoploc()
                                       writeoffset( offset )
                                   $)

                                   ELSE  //  instruction = i.Scc
                                   $(
                                       setoploc()
                                       write.ea( word )
                                   $)
                                   ENDCASE


        CASE i.addq             :
        CASE i.subq             :  writesize()
                                   setoploc()
                                   writef( "#%N,", reg2 = 0  ->  8, reg2 )
                                   write.ea( word )
                                   ENDCASE


        CASE i.moveq           :  setoploc()
                                  writef( "#%N,D%N", bextend( word & #XFF), reg2 )
                                  ENDCASE


        CASE i.chk             :
        CASE i.divu            :
        CASE i.divs            :
        CASE i.mulu            :
        CASE i.muls            :  size  :=  s.word
                                  setoploc()
                                  write.ea( word )
                                  writef( ",D%N", reg2 )
                                  ENDCASE


        CASE i.addx            :
        CASE i.subx            :  writesize()

        CASE i.abcd            :
        CASE i.sbcd            :  setoploc()
                                  writef( bit( 3 ) = 0  ->  "D%N,D%N",
                                                            "-(A%N),-(A%N)",
                                          reg1, reg2 )
                                  ENDCASE


        CASE i.or              :
        CASE i.and             :
        CASE i.eor             :  writesize()
                                  setoploc()

                                  TEST  bit8 = 0  THEN
                                  $(
                                      write.ea( word )
                                      writef( ",D%N", reg2 )
                                  $)
                                  ELSE
                                  $(
                                      writef( "D%N,", reg2 )
                                      write.ea( word )
                                  $)
                                  ENDCASE


        CASE i.add             :
        CASE i.sub             :
        CASE i.cmp             :  TEST  size = #B11  THEN
                                  $(
                                      wrch( 'A' )
                                      size  :=  (bit8 = 0)  ->  s.word, s.long
                                      writes( size = s.word  ->  ".W", ".L" )
                                      setoploc()
                                      write.ea( word )
                                      writef( ",A%N", reg2 )
                                  $)
                                  ELSE
                                  $(
                                      writesize()
                                      setoploc()

                                      TEST  bit8 = 0  THEN
                                      $(
                                          write.ea( word )
                                          writef( ",D%N", reg2 )
                                      $)
                                      ELSE
                                      $(
                                          writef( "D%N,", reg2 )
                                          write.ea( word )
                                      $)
                                  $)
                                  ENDCASE


        CASE i.cmpm            :  writesize()
                                  setoploc()
                                  writef( "(A%N)+,(A%N)+", reg1, reg2 )
                                  ENDCASE


        CASE i.exg            :  setoploc()
                                 opmode  :=  (word >> 3) & #B11111

                                 TEST  opmode = #B01000  THEN
                                       writef( "D%N,D%N", reg2, reg1 )
                                 ELSE
                                 TEST  opmode = #B01001  THEN
                                       writef( "A%N,A%N", reg2, reg1 )

                                 ELSE
                                 TEST  opmode = #B10001  THEN
                                       writef( "D%N,A%N", reg2, reg1 )

                                 ELSE  duffinstruction()
                                 ENDCASE


        CASE i.lea            :  setoploc()
                                 write.ea( word )
                                 writef( ",A%N", reg2 )
                                 ENDCASE


        CASE i.neg            :
        CASE i.negx           :
        CASE i.clr            :
        CASE i.tst            :
        CASE i.not            :  writesize()
                                 setoploc()
                                 write.ea( word )
                                 ENDCASE



        CASE i.swap           :
        CASE i.extl           :
        CASE i.extw           :
        CASE i.nbcd           :
        CASE i.jmp            :
        CASE i.jsr            :
        CASE i.pea            :
        CASE i.tas            :  setoploc()
                                 write.ea( word )
                                 ENDCASE


        CASE i.reset          :
        CASE i.nop            :
        CASE i.rte            :
        CASE i.rts            :
        CASE i.rtr            :
        CASE i.trapv          :  ENDCASE


        CASE i.ori            :
        CASE i.andi           :
        CASE i.subi           :
        CASE i.addi           :
        CASE i.eori           :
        CASE i.cmpi           :  writesize()
                                 setoploc()
                                 writes( "#$" )
                                 TEST  size = s.byte  THEN  writehex( readword(), 2 )  ELSE
                                 TEST  size = s.word  THEN  writehex( readword(), 4 )  ELSE
                                 TEST  size = s.long  THEN
                                 $(
                                     writehex( readword(), 4 )
                                     writehex( readword(), 4 )
                                 $)
                                 ELSE  duffinstruction()
                                 wrch( ',' )
                                 write.ea( word )
                                 ENDCASE


        CASE i.move.to.sr     :
        CASE i.move.from.sr   :
        CASE i.move.to.ccr    :  size  :=  instruction = i.move.to.ccr  ->  s.byte, s.word
                                 setoploc()

                                 TEST  instruction = i.move.to.sr  THEN
                                 $(
                                     write.ea( word )
                                     writes( ",SR" )
                                 $)
                                 ELSE

                                 TEST  instruction = i.move.from.sr  THEN
                                 $(
                                     writes( "SR," )
                                     write.ea( word )
                                 $)

                                 ELSE  // instruction = i.move.to.ccr
                                 $(
                                     write.ea( word )
                                     writes( ",CCR" )
                                 $)
                                 ENDCASE


        CASE i.move.to.usp    :
        CASE i.move.from.usp  :  setoploc()
                                 writef( instruction = i.move.to.usp  ->
                                         "A%N,USP", "USP,A%N",  reg1 )
                                 ENDCASE


        CASE i.movep          :  IF  bit8 = 0  THEN  duffinstruction()
                                 writes( (opm & #B001) = 0  ->  ".W", ".L" )
                                 setoploc()

                                 TEST  (opm & #B010) = 0  THEN
                                       writef( "%N(A%N),D%N", extend( readword() ), reg1, reg2 )
                                 ELSE  writef( "D%N,%N(A%N)", reg2, extend( readword() ), reg1 )
                                 ENDCASE


        CASE i.stop           :  setoploc()
                                 writef( "#$%X4", readword() )
                                 ENDCASE


        CASE i.trap           :  setoploc()
                                 writef( "#%N", word & #B1111 )
                                 ENDCASE


        CASE i.link           :
        CASE i.unlk           :  setoploc()
                                 writef( "A%N", reg1 )
                                 IF  instruction = i.link  THEN
                                     writef( ",#%N", extend( readword() ) )
                                 ENDCASE


        CASE i.movem.to.reg   :
        CASE i.movem.from.reg :  writes( bit( 6 ) = 0  ->  ".W", ".L" )
                                 setoploc()
                                 regmask  :=  readword()

                                 TEST  instruction = i.movem.to.reg  THEN
                                 $(
                                     write.ea( word )
                                     wrch( ',' )
                                     writeregs( regmask )
                                 $)
                                 ELSE
                                 $(
                                     writeregs( regmask )
                                     wrch( ',' )
                                     write.ea( word )
                                 $)
                                 ENDCASE


        CASE i.bit.static     :
        CASE i.bit.dynamic    :  writes( bcode( size ) )
                                 setoploc()

                                 TEST  instruction = i.bit.static  THEN
                                       writef( "#%N,", readword() & #B11111 )
                                 ELSE  writef( "D%N,", reg2 )

                                 write.ea( word )
                                 ENDCASE


        DEFAULT               :  err( "Bad code %N in *"gen*"", instruction )
    $)
$)



AND opcode( instruction )  =  VALOF
$(
//  Return the String representation of the Opcode given by the argument
//  "instruction".  The string is in minimum width so that size specifiers
//  can be added to it later.

    SWITCHON  instruction  INTO
    $(
        CASE i.move.byte       : RESULTIS "MOVE.B"
        CASE i.move.long       : RESULTIS "MOVE.L"
        CASE i.move.word       : RESULTIS "MOVE.W"
        CASE i.DBcc            : RESULTIS "DB"
        CASE i.Scc             : RESULTIS "S"
        CASE i.addq            : RESULTIS "ADDQ"
        CASE i.subq            : RESULTIS "SUBQ"
        CASE i.Bcc             : RESULTIS "B"
        CASE i.moveq           : RESULTIS "MOVEQ"
        CASE i.divu            : RESULTIS "DIVU"
        CASE i.mulu            : RESULTIS "MULU"
        CASE i.divs            : RESULTIS "DIVS"
        CASE i.muls            : RESULTIS "MULS"
        CASE i.sbcd            : RESULTIS "SBCD"
        CASE i.or              : RESULTIS "OR"
        CASE i.subx            : RESULTIS "SUBX"
        CASE i.sub             : RESULTIS "SUB"
        CASE i.cmpm            : RESULTIS "CMPM"
        CASE i.eor             : RESULTIS "EOR"
        CASE i.cmp             : RESULTIS "CMP"
        CASE i.abcd            : RESULTIS "ABCD"
        CASE i.exg             : RESULTIS "EXG"
        CASE i.and             : RESULTIS "AND"
        CASE i.addx            : RESULTIS "ADDX"
        CASE i.add             : RESULTIS "ADD"
        CASE i.chk             : RESULTIS "CHK"
        CASE i.lea             : RESULTIS "LEA"
        CASE i.move.from.sr    : RESULTIS "MOVE"
        CASE i.negx            : RESULTIS "NEGX"
        CASE i.move.to.sr      : RESULTIS "MOVE"
        CASE i.not             : RESULTIS "NOT"
        CASE i.clr             : RESULTIS "CLR"
        CASE i.move.to.ccr     : RESULTIS "MOVE"
        CASE i.neg             : RESULTIS "NEG"
        CASE i.nbcd            : RESULTIS "NBCD"
        CASE i.swap            : RESULTIS "SWAP"
        CASE i.pea             : RESULTIS "PEA"
        CASE i.extw            : RESULTIS "EXT.W"
        CASE i.extl            : RESULTIS "EXT.L"
        CASE i.movem.from.reg  : RESULTIS "MOVEM"
        CASE i.movem.to.reg    : RESULTIS "MOVEM"
        CASE i.jmp             : RESULTIS "JMP"
        CASE i.jsr             : RESULTIS "JSR"
        CASE i.trap            : RESULTIS "TRAP"
        CASE i.link            : RESULTIS "LINK"
        CASE i.unlk            : RESULTIS "UNLK"
        CASE i.move.to.usp     : RESULTIS "MOVE"
        CASE i.move.from.usp   : RESULTIS "MOVE"
        CASE i.reset           : RESULTIS "RESET"
        CASE i.nop             : RESULTIS "NOP"
        CASE i.stop            : RESULTIS "STOP"
        CASE i.rte             : RESULTIS "RTE"
        CASE i.rts             : RESULTIS "RTS"
        CASE i.trapv           : RESULTIS "TRAPV"
        CASE i.rtr             : RESULTIS "RTR"
        CASE i.movep           : RESULTIS "MOVEP"
        CASE i.bit.dynamic     : RESULTIS "B"
        CASE i.ori             : RESULTIS "ORI"
        CASE i.andi            : RESULTIS "ANDI"
        CASE i.subi            : RESULTIS "SUBI"
        CASE i.addi            : RESULTIS "ADDI"
        CASE i.eori            : RESULTIS "EORI"
        CASE i.cmpi            : RESULTIS "CMPI"
        CASE i.bit.static      : RESULTIS "B"
        CASE i.tas             : RESULTIS "TAS"
        CASE i.tst             : RESULTIS "TST"

        DEFAULT                :  RESULTIS "????"
    $)
$)



AND sizestring( code )  =  code = s.byte  ->  ".B",
                           code = s.word  ->  ".W",
                           code = s.long  ->  ".L",  ".?"



AND ccode( code, instruction )  =  VALOF
$(
    SWITCHON  code  INTO
    $(
        CASE #B0000 :  RESULTIS  instruction = i.Bcc   ->  "RA", "T"
        CASE #B0001 :  RESULTIS  instruction = i.DBcc  ->  "RA",
                                 instruction = i.Bcc   ->  "SR", "F"
        CASE #B0010 :  RESULTIS  "HI"
        CASE #B0011 :  RESULTIS  "LS"
        CASE #B0100 :  RESULTIS  "CC"
        CASE #B0101 :  RESULTIS  "CS"
        CASE #B0110 :  RESULTIS  "NE"
        CASE #B0111 :  RESULTIS  "EQ"
        CASE #B1000 :  RESULTIS  "VC"
        CASE #B1001 :  RESULTIS  "VS"
        CASE #B1010 :  RESULTIS  "PL"
        CASE #B1011 :  RESULTIS  "MI"
        CASE #B1100 :  RESULTIS  "GE"
        CASE #B1101 :  RESULTIS  "LT"
        CASE #B1110 :  RESULTIS  "GT"
        CASE #B1111 :  RESULTIS  "LE"

        DEFAULT     :  err( "Bad code %N in *"ccode*"", code )
    $)
$)



AND rcode( code )  =  VALOF
$(
    SWITCHON  code  INTO
    $(
        CASE #B00  :  RESULTIS "AS"
        CASE #B01  :  RESULTIS "LS"
        CASE #B10  :  RESULTIS "ROX"
        CASE #B11  :  RESULTIS "RO"

        DEFAULT    :  RESULTIS "????"
    $)
$)



AND bcode( code )  =  VALOF
$(
    SWITCHON  code  INTO
    $(
        CASE b.tst  :  RESULTIS  "TST"
        CASE b.set  :  RESULTIS  "SET"
        CASE b.clr  :  RESULTIS  "CLR"
        CASE b.chg  :  RESULTIS  "CHG"

        DEFAULT     :  RESULTIS  "???"
   $)
$)



AND extend( value )  =  (value & #X7FFF) - (value & #X8000)



AND bextend( value ) =  (value & #X7F)   - (value & #X80)



AND swapea( ea )     =  ((ea >> 3) & #B111) | ((ea & #B111) << 3)



AND writesize()  BE  writes( sizestring( size ) )



AND setoploc()   BE  lpos  :=  18



AND writeoffset( offset )  BE
$(
    LET increment  =  offset + 2
    LET lp         =  ?

    wrch( '**' )
    UNLESS  increment < 0  DO  wrch( '+' )
    writen( increment )

    lp  :=  lpos

    WHILE  lpos < 30  DO  wrch( '*S' )
    writef( "[ $%X4 ]", currentloc + increment )

    lpos  :=  lp
$)



AND writeregs( regmask )  BE
$(
    LET sp   = FALSE

    IF  ((word >> 3) & #B111) = am.Ar.pd  THEN
        regmask  :=  reversebits( regmask )

    FOR  r = 1  TO  2  DO
    $(
        LET regtype  =  r = 1  ->  'D', 'A'

        FOR  i = 0  TO  7  DO
        $(
            IF  (regmask & 1) = 1  THEN
            $(
                IF  sp  THEN  wrch( '/' )
                writef( "%C%N", regtype, i )
                sp  :=  TRUE
            $)

            regmask  :=  regmask >> 1
        $)
    $)
$)



AND reversebits( value )  =  VALOF
$(
    LET newvalue = 0

    FOR  i = 1  TO  16  DO
    $(
        newvalue  :=  (newvalue << 1) | (value & 1)
        value     :=  value >> 1
    $)

    RESULTIS  newvalue
$)



AND write.ea( ea )  BE
$(
    LET offset  =  ?
    LET extword =  ?

    TEST  ((ea >> 3) & #B111) = #B111  THEN
    $(
        SWITCHON  ea & #B111  INTO
        $(
            CASE am.abs16     :  writef( "$%X4", readword() )
                                 ENDCASE

            CASE am.abs32     :  writef( "$%X4", readword() )
                                 writehex( readword(), 4 )
                                 ENDCASE

            CASE am.PC.disp   :  wrch( '**' )
                                 offset  :=  2 + extend( readword() )
                                 UNLESS  offset < 0  DO  wrch( '+' )
                                 writen( offset )
                                 ENDCASE

            CASE am.PC.index  :  wrch( '**' )
                                 extword  :=  readword()
                                 offset   :=  2 + bextend( extword & #XFF )
                                 UNLESS  offset < 0  DO  wrch( '+' )
                                 writef( "%N(%C%N", offset,
                                          (extword & #X8000) = 0  ->  'D', 'A',
                                          (extword >> 12) & #B111 )
                                 writes( (extword & #X800) = 0  ->  ".W)", ".L)" )
                                 ENDCASE

            CASE am.imm       : writes( "#$" )
                                TEST  size = s.byte  THEN  writehex( readword(), 2 )  ELSE
                                TEST  size = s.word  THEN  writehex( readword(), 4 )  ELSE
                                TEST  size = s.long  THEN
                                $(
                                    writehex( readword(), 4 )
                                    writehex( readword(), 4 )
                                $)
                                ELSE  duffinstruction()
                                ENDCASE

            DEFAULT          :  duffinstruction()
        $)
    $)
    ELSE
    $(
        LET r  =  ea & #B111

        SWITCHON  (ea >> 3) & #B111  INTO
        $(
            CASE am.Dr       :  writef( "D%N", r )
                                ENDCASE

            CASE am.Ar       :  writef( "A%N", r )
                                ENDCASE

            CASE am.Ar.ind   :  writef( "(A%N)", r )
                                ENDCASE

            CASE am.Ar.pi    :  writef( "(A%N)+", r )
                                ENDCASE

            CASE am.Ar.pd    :  writef( "-(A%N)", r )
                                ENDCASE

            CASE am.Ar.disp  :  writef( "%N(A%N)", extend( readword() ), r )
                                ENDCASE

            CASE am.Ar.index :  extword  :=  readword()
                                writef( "%N(A%N,", bextend( extword & #XFF), r )
                                writef( "%C%N%S", (extword & #X8000) = 0  ->  'D', 'A',
                                                  (extword >> 12) & #B111,
                                                  (extword & #X800) = 0   ->  ".W)", ".L)" )
                                ENDCASE

            DEFAULT          :  err( "Bad EA %X4 in *"write.ea*"", ea )
        $)
   $)
$)



AND sizefield()          =  (word >> 6) & sizemask


AND registerfield()      =  (word >> 3) & registermask


AND reg1field()          =  word        & registermask


AND reg2field()          =  (word >> 9) & registermask


AND opmodefield()        =  (word >> 6) & #B111


AND bit( n )             =  (word >> n) & 1


AND bits54field()        =  (word >> 4) & #B11



AND duffinstruction()  BE
$(
    progaddress  :=  currentloc
    lpos         :=  0
    llen         :=  0
    word         :=  readword()

    FOR  i = 0  TO  255  DO  lbuff % i  :=  '*S'

    writef( "$%X6:  ", currentloc )

    dataword()

    longjump( disasmlevel, disasmlabel )
$)



AND error( format, argument )  BE
$(
    wrch  :=  swrch

    writes( "*N****** Error:  " )
    writef( format, argument )
    writes( "*N****** Abandoned*N" )
    stop( 20 )
$)



AND err( format, argument ) BE
$(
// Some sort of internal error

    endread()
    endoutput()
    UNLESS  memorybuff = 0  DO  freevec( memorybuff )
    error( format, argument )
$)
     

