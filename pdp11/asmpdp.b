// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

GET "libhdr"

GLOBAL
$(
absloc           : ug +  0 || Absolute section location counter
addrmode         : ug +  1 || Operand address mode
addrval          : ug +  2 || Operand address word value
ch               : ug +  3 || Last character read
exptype          : ug +  4 || Expression type
expval           : ug +  5 || Expression value
location         : ug +  6 || Current location counter
radix            : ug +  7 || Default radix for reading numbers
ws.size          : ug +  8 || Total work space allocated
relloc           : ug +  9 || Relocatable section location counter
warncount        : ug + 10 || Count of warnings issued
declsyswords     : ug + 11 || Sets up initial symbol table
symb             : ug + 12 || Pointer to symbol table entry
symbtype         : ug + 13 || Type of symbol
symbval          : ug + 14 || Value of symbol
tagv             : ug + 15 || Vector used for reading tags

absmin           : ug + 16
absmax           : ug + 17
absvec           : ug + 18
absrp            : ug + 19
absrvec          : ug + 20

relmin           : ug + 21
relmax           : ug + 22
relvec           : ug + 23
relrp            : ug + 24
relrvec          : ug + 25

minloc           : ug + 26
maxloc           : ug + 27
codevec          : ug + 28
relp             : ug + 29
relocvec         : ug + 30
locmode          : ug + 31

tagtable         : ug + 32

outbuf           : ug + 33
outbufp          : ug + 34
nerrs            : ug + 35
printres         : ug + 36
charpos          : ug + 37

ended            : ug + 38
pass1            : ug + 39
pass2            : ug + 40
ttused           : ug + 41
ilength          : ug + 42

valpntr          : ug + 43
typepntr         : ug + 44
listing          : ug + 45
listlev          : ug + 46
errcount         : ug + 47
locsave          : ug + 48
addrtype         : ug + 49

buildword        : ug + 50
dodir            : ug + 51
doinstr          : ug + 52
readnumber       : ug + 53
readtag          : ug + 54
skiplayout       : ug + 55
changemode       : ug + 56
readsymb         : ug + 57
readabsexp       : ug + 58
setloc           : ug + 59
complain         : ug + 60
readexp          : ug + 61
error            : ug + 62
rch              : ug + 63
unrch            : ug + 64
lookup           : ug + 65
putn             : ug + 66
printloc         : ug + 67
skipcomma        : ug + 68
putb             : ug + 69
skiprest         : ug + 70

outcode          : ug + 72
morewords        : ug + 73
morebytes        : ug + 74
gbyte            : ug + 75
tidy.up.and.stop : ug + 76
stvec.chain      : ug + 77 // Chain of symbol table blocks
current.stvec    : ug + 78 // Latest block allocated
stvec.offset     : ug + 79 // Offset in latest block

codestream       : ug + 81

sourcestream     : ug + 84
liststream       : ug + 85
progname         : ug + 86
putloc           : ug + 87
reportundefs     : ug + 88
gvec             : ug + 89
clearbits        : ug + 90
$)


MANIFEST
$(
|| Symbol types
s.abs   =  1     || Tag with absolute value
s.dir   =  2     || Assembler directive
s.dot   =  3     || Location counter symbol
s.instr =  4     || Instruction mnemonic
s.new   =  5     || Newly created symbol table entry
s.none  =  6     || No symbol found before end of line
s.reg   =  7     || Register tag
s.rel   =  8     || Tag with relocatable value
s.labr  =  9     || Left angle bracket
s.number= 10     || Number (e.g. 123 or 'e )
s.monop = 11     || Monadic operator
s.pcent = 12     || Percent

|| Operators

op.plus   = 1
op.minus  = 2
op.times  = 3
op.over   = 4
op.and    = 5
op.or     = 6

|| Symbol table type field bits

stb.muldef = #400
stb.setnow = #200
stb.setever= #100
stb.temp   = #1000
stb.nrc    = #2000  || For non range-compatible
                    || instructions

|| Instruction types

i.a     =  1     || Single address operand
i.aa    =  2     || Two address operands
i.bch   =  3     || Branch instruction
i.r     =  4     || Single register operand
i.rd    =  5     || Register, destination operands
i.ro    =  6     || Register, offset operands
i.sr    =  7     || Source, register operands
i.zop   =  8     || Zero operands
i.3n    =  9     || Operand is a 3 bit number
i.6n    = 10     || Operand is a 6 bit number
i.8n    = 11     || Operand is an 8 bit number

|| Symbol table types for instructions.
|| These are the type fields for entries for the
|| instruction tags in the symbol table.

sti.a   =  (i.a   << 11) + s.instr
sti.aa  =  (i.aa  << 11) + s.instr
sti.bch =  (i.bch << 11) + s.instr
sti.r   =  (i.r   << 11) + s.instr
sti.rd  =  (i.rd  << 11) + s.instr
sti.ro  =  (i.ro  << 11) + s.instr
sti.sr  =  (i.sr  << 11) + s.instr
sti.zop =  (i.zop << 11) + s.instr
sti.3n  =  (i.3n  << 11) + s.instr
sti.6n  =  (i.6n  << 11) + s.instr
sti.8n  =  (i.8n  << 11) + s.instr

|| Instruction types for instructions which are
|| not available on all PDP-11s.

sti.az  = sti.a   | stb.nrc
sti.rz  = sti.r   | stb.nrc
sti.rdz = sti.rd  | stb.nrc
sti.roz = sti.ro  | stb.nrc
sti.srz = sti.sr  | stb.nrc
sti.zopz= sti.zop | stb.nrc
sti.3nz = sti.3n  | stb.nrc
sti.6nz = sti.6n  | stb.nrc

|| Directive types

d.ascii =  1     || ASCII string
d.asciz =  2     || ASCII string ending with zero byte
d.asect =  3     || Start/resume absolute section
d.blkb  =  4     || Reserve n bytes
d.blkw  =  5     || Reserve n words
d.byte  =  6     || Assemble byte value(s)
d.csect =  7     || Start/resume relocatable section
d.end   =  8     || End of source
d.even  =  9     || Align to word boundary
d.limit = 10     || Assemble 2 words for prog limits
d.list  = 11     || Increment list count
d.nlist = 12     || Decrement list level
d.odd   = 13     || Align to odd byte boundary
d.psect = 14     || Start/resume relocatable section
d.radix = 15     || Set default radix for reading numbers
d.unimp = 17     || Unimplemented DOS MACRO directive
d.word  = 18     || Assemble word value(s)

|| Expression types

e.abs   =  1     || Absolute expression
e.reg   =  2     || Register expression
e.rel   =  3     || Relocatable expression

|| Object module section identifiers

t.hunk    = 1000
t.reloc   = 1001
t.end     = 1002
t.abshunk = 1003
t.absrel  = 1004

|| Miscellaneous values

tagchars=  6     || Max. number of chars in a tag
tagsize = (tagchars + bytesperword - 1)/bytesperword
                 || Number of words needed for tag
tagbyteupb = (tagsize * bytesperword) - 1
                 || Last byte offset in TAGSIZE words
maxwol  =  3     || Max words printed on a line
maxbol  =  6     || Max bytes printed on a line

|| Symbol table entry offsets

st.type =  tagsize + 1 || Symbol type, use bits, intruction type
st.value=  tagsize + 2 || Symbol value

|| Basic constants

avsize       =    120/bytesperword
tagtablesize =     50
maxint       =  32767
initobp      =     38  || Initial value of OUTBUFP
outbuflim    =    120
stvecupb     =    200 // Unit in which symbol table blocks are allocated
$)

.
SECTION "PDPASM1"
GET ""

LET start() BE
    $(
    LET ob   = VEC 60
    LET tt   = VEC tagtablesize - 1
    LET tgv  = VEC 2
    LET argv = VEC avsize


    liststream, sourcestream := 0, 0
    codestream               := 0
    absvec, absrvec          := 0, 0
    errcount, warncount      := 0, 0
    relvec, relrvec          := 0, 0
    ws.size                  := 0

    // Initialise symbol table block chain
    stvec.chain   := 0
    current.stvec := @stvec.chain
    stvec.offset  := stvecupb+1 // To force initial getvec


    IF rdargs("prog=from/a,code=to,ver/k,list/s", argv, avsize) = 0
    THEN $( writes("Invalid arguments*N"); stop(20) $)

    progname := argv!0
    TEST argv!3 \= 0 // LIST
    THEN listlev, listing :=     1, TRUE
    ELSE listlev, listing := -1000, FALSE // NOLIST

    sourcestream := findinput(progname)
    checkopen(sourcestream, "FROM", argv!0)

    UNLESS argv!1 = 0 // CODE stream
    THEN
      $(
      codestream := findoutput(argv!1)
      checkopen(codestream, "TO", argv!1)
      $)

    UNLESS argv!2 = 0
    THEN
      $(
      liststream := findoutput(argv!2)
      checkopen(liststream, "VER", argv!2)
      selectoutput(liststream)
      $)

    tagv     := tgv
    tagtable := tt
    FOR j = 0 TO tagtablesize - 1 DO tt ! j := 0


    ttused := 0
    outbuf := ob

    writes("TRIPOS PDP-11 Assembler version 1.4*N")

    /******** Only works under TRIPOS ********/
    freevec(declsyswords()) // Set up initial symbol table,
                            // and free code which does it.

    firstpass()
    secondpass()
    UNLESS codestream=0 THEN outcode()
    reportundefs()

    TEST errcount = 0
    THEN writes("*NNo")
    ELSE writef("*N%N", errcount)

    writes(" errors detected")
    UNLESS warncount=0
    THEN writef("; %N warnings", warncount)
    writes("*N*NAssembly statistics (words):*N*N*
           *           Absolute   Relocatable*N")
    writef("Code        %I5       %I5*N",
           (absmax=0 -> 0,(absmax-absmin+1)/2), (relmax+1)/2)
    writef("Reloc. info %I5       %I5*N*N", absrp, relrp)
    writef("Space used for symbol table: %N words*N", ttused)
    writef("Total workspace used: %N words*N", ws.size)
    writes("Assembly complete*N")
    tidy.up.and.stop(errcount=0 -> 0, 10)
    $)





AND checkopen(stream, key, name) BE
    IF stream = 0
    THEN error("Can't open %S stream *"%S*"*N", key, name)



AND firstpass() BE
    $(
    absmin, relmin := maxint, 0
    absmax, relmax := 0,0
    absrp,  relrp  := 0,0
    absloc, relloc := 0,0

    locmode                  := s.rel
    relp                     := relrp
    minloc, maxloc, location := 0, 0, 0

    radix   := 8
    charpos := 1
    ended := FALSE; pass1 := TRUE; pass2 := FALSE

    selectinput(sourcestream)

    UNTIL ended doline()

    || Force dumping of location variables
    changemode(locmode = s.rel -> s.abs, s.rel)

    newline(); newline()
    $)



AND secondpass() BE
    $(
    || Get the vectors for the code and relocation info

    UNLESS absmax < absmin
    THEN
      $( LET absvecupb = (absmax-absmin)/2
         absvec := gvec(absvecupb)

         // Make sure all unused words are set
         // to zero (SYSLINK assumes this)
         FOR z=0 TO absvecupb DO absvec!z := 0
         absvec := absvec - absmin/2
      $)

    UNLESS absrp=0 THEN absrvec := gvec(absrp - 1)
    relvec  := gvec(relmax/2)
    UNLESS relrp=0 THEN relrvec := gvec(relrp - 1)

    absrp, relrp   := 0, 0
    absloc, relloc := 0, 0


    codevec, relocvec, relp := relvec, relrvec, relrp

    locmode := s.rel

    radix                    := 8
    charpos                  := 1
    minloc, maxloc, location := relmin, relmax, 0
    pass1, pass2             := FALSE, TRUE
    ended                    := FALSE
    outbufp                  := initobp

    clearbits()

    endread()
    sourcestream := findinput(progname); selectinput(sourcestream)

    UNTIL ended
    DO $(
       LET listsave = listing || So we reflect .NLIST
       nerrs := 0
       morewords, morebytes := 0, 0
       clearobuf()

       doline()

       IF listsave | (nerrs > 0)
       THEN TEST outbufp = (initobp + 1)
            THEN wrch('*N')  || Empty source line
            ELSE
              $(
              printbuf()

              TEST morewords>0
              THEN
                $(
                || This source line generated too many words of code
                || for one output line.  Read the remaining values
                || from CODEVEC and print them on subsequent lines.
                LET wloc = location >> 1
                LET wol = 0
                clearobuf()
                putloc(location - morewords*2)

                FOR j = wloc-morewords TO wloc-1
                DO $(
                   putn(14+wol*8,codevec!j,s.abs)
                   wol := wol + 1

                   IF wol = maxwol
                   THEN
                     $(
                     printbuf()
                     clearobuf()
                     putloc(j*2 + 2)
                     wol := 0
                     $)
                   $)

                || Last line may not be full
                UNLESS wol = 0 THEN printbuf()
                $)
              ELSE
                $(
                IF morebytes > 0
                THEN
                  $(
                  LET bol = 0
                  clearobuf()
                  putloc(location - morebytes)

                  FOR j = location-morebytes TO location - 1
                  DO $(
                     putb(14+bol*4,gbyte(codevec,j))
                     bol := bol + 1

                     IF bol = maxbol
                     THEN
                       $(
                       printbuf()
                       clearobuf()
                       putloc(j + 1)
                       bol := 0
                       $)
                     $)

                  || Last line may not be full
                  UNLESS bol=0 THEN printbuf()
                  $)
                $)
              $)
        $)

    || Force dumping of location variables
    changemode(locmode = s.rel -> s.abs, s.rel)
    $)


AND clearobuf() BE
    $(
    || Clears the output buffer
    outbufp := initobp
    FOR j = 0 TO outbuflim DO outbuf%j := ' '
    $)


AND printbuf() BE
    $(
    || Prints the output buffer
    FOR j = 0 TO outbufp - 2 DO wrch(outbuf%j)
    wrch('*N')
    $)


AND gvec(u) = VALOF
    $(
    || Does the necessary checking after GETVEC.
    LET a = getvec(u)

    IF a = 0
    THEN error("Insufficient store for workspace*
               * (%N words used so far)*N", ws.size)

    ws.size := ws.size + u + 1
    RESULTIS a
    $)


AND doline() BE
    $( || This routine handles a complete line of source.
      || First the labels are set, and then READREST is
      || called, either for an equivalence line, or for
      || an ordinary line. (Directive, instruction or
      || data word).
    IF testflags(1)
    THEN
      $( writes("****** BREAK - no code produced*N")
         tidy.up.and.stop(20)
      $)

    locsave := location

    printres, printloc := FALSE, FALSE
    setlabels()
    TEST ch = '='
    THEN
        $(  || Equivalence line
        LET s, st = symb, symbtype
        rch(); readsymb(); readrest(FALSE)
        setsymb(s, st, expval, exptype)
        putn(14, expval, exptype)
        $)
    ELSE
        $(
        readrest(TRUE)

        || Deal with an implicit .WORD directive

        IF (exptype = s.abs) | (exptype = s.rel) |
           (exptype = s.reg)
        THEN
            $(
            putloc()
            buildword(expval,exptype,0)
            IF exptype = s.reg THEN complain('Q')
            $)
        $)

    IF printloc THEN putloc(locsave)
    || Read to end of line
    skiprest()
    $)


AND setlabels() BE || Sets labels on line
    $(
    rch(); readsymb(); skiplayout()

    UNLESS ch = ':' THEN RETURN

    printloc := TRUE
    setsymb(symb, symbtype, location, locmode)
    $) REPEAT


AND skiplayout() BE
    $( || Skips over spaces, tabs and comments
    WHILE (ch = '*S') | (ch = '*T') DO rch()

    IF ch = ';' THEN skiprest() || Skip comment
    $)


AND skipcomma() BE
    $(
    || Skips to the char after the next comma.
    || Complains if it has to read other than layout
    || characters.

    skiplayout()
    IF ch=',' THEN $( rch(); RETURN $)
    IF ch = '*N' THEN RETURN

    complain('Q')

    UNTIL (ch = ',') | (ch = '*N')
    DO rch()

    IF ch = ',' THEN rch()
    $)


AND skiprest() BE UNTIL ch = '*N' DO rch()



AND setsymb(s,st,v,t) BE
    $(
    UNLESS (st = s.new) | (st = s.abs) |
           (st = s.rel) | (st = s.reg) |
           (st = s.dot)
    THEN
        $( || Invalid symbol for assignment
        complain('Q')
        RETURN
        $)

    IF st = s.dot
    THEN
        $(  || Assignment to location counter
        TEST t = locmode
        THEN setloc(v)
        ELSE complain('A')

        RETURN
        $)

    UNLESS (t = s.abs) | (t = s.rel) | (t = s.reg)
    THEN
        $(
        complain('Q')
        t := s.abs; v := 0
        $)

    TEST (st.type ! s & stb.muldef) > 0
    THEN complain('M')
    ELSE
        TEST (st.type ! s & stb.setnow) > 0
        THEN st.type ! s := st.type ! s | stb.muldef
        ELSE
            $(
            st.type ! s := (st.type ! s & #177700) | t
            st.value ! s := v
            st.type ! s := st.type ! s | stb.setnow |
              stb.setever
            $)
    $)


AND readrest(frefs) BE
    $(
    || Read rest of line, which may be:
    ||       Instruction
    ||       Data word
    ||       Directive
    ||       Empty
    || The routine is entered with the first symbol
    || described by SYMB and SYMBTYPE.
    LET s = symb

    SWITCHON symbtype INTO
        $(
        DEFAULT: exptype := s.abs

                IF pass1 & frefs THEN RETURN

                readexp(frefs)
                readsymb()
                ENDCASE

        CASE s.none:  exptype := s.none
                      RETURN

        CASE s.dir:   dodir()
                      exptype := s.dir
                      ENDCASE

        CASE s.instr: doinstr()
         printloc := TRUE
                      exptype := s.instr
                      ENDCASE
        $)

    readsymb()
    UNLESS symbtype = s.none THEN complain('Q')
    $)






.
SECTION "PDPASM2"

GET ""

LET readabsexp(frefs) = VALOF
    $(
    readexp(frefs)

    UNLESS exptype = s.abs
    THEN
        $(
        complain('A')
        exptype, expval := s.abs, 0
        $)

    RESULTIS expval
    $)


AND readexp(frefs)  BE
    $(
    LET et,st,eval,sval,op = 0,0,0,0,op.plus
    LET abssymb = FALSE
    LET relsymb = FALSE
    LET regsymb = FALSE

    IF symbtype = s.monop
    THEN $( op := symb;  readsymb() $)

        $(  || Main repeat loop
        sval,st := 0,0

        SWITCHON symbtype INTO
            $(
            CASE s.labr:   readsymb()
                           readexp(frefs)
                           sval := expval
                           st := (exptype = s.rel -> 1,0)

                           IF ch \= '>'
                           THEN $( complain('Q'); BREAK $)

                           rch()
                           GOTO elab

            CASE s.dot:    sval := location

                           TEST locmode = s.rel
                           THEN relsymb, st := TRUE, 1
                           ELSE abssymb, st := TRUE, 1

                           GOTO elab

            CASE s.number: sval := symb
                           GOTO elab

            CASE s.new:    || In the first pass, this
                           || must be assumed relocatable
                           || so that the relocation vector
                           || obtained is at least big
                           || enough. In the second pass,
                           || these are undefined symbols,
                           || taken as absolute.
                           IF pass2 | (NOT frefs)
                           THEN
                               $(
                               abssymb := TRUE
                               GOTO tagterm
                               $)
            CASE s.rel:    st := 1
                           relsymb := TRUE
                           GOTO tagterm

            CASE s.pcent:  regsymb := TRUE
                           readsymb()
                           sval := readabsexp(FALSE)
                           GOTO elab

            CASE s.reg:    regsymb := TRUE
                           GOTO tagterm

            CASE s.instr:
            CASE s.abs:    abssymb := TRUE

        tagterm: sval := st.value ! symb
                 UNLESS (st.type ! symb & stb.setnow) > 0 |
                        frefs
                 THEN complain('F')

        elab:    SWITCHON op INTO
                     $(
                     CASE op.plus:  eval := eval + sval
                                    et := et + st
                                    ENDCASE

                    CASE op.minus: eval := eval - sval
                                   et := et - st
                                   ENDCASE

                    CASE op.times: IF (st > 0) & (et > 0)
                                   THEN
                                       $(
                                       complain('X')
                                       eval, et, st := 0, 0, 0
                                       $)
                                   eval := eval * sval
                                   et := st*eval + et*sval
                                   ENDCASE

                    CASE op.over:  IF (st > 0) | (et REM sval \= 0)
                                   THEN
                                       $(
                                       complain('X')
                                       eval, et, st := 0, 0, 0
                                       $)
                                   eval := eval/sval
                                   et := et/sval
                                   ENDCASE

                    CASE op.or:
                    CASE op.and:   IF (et > 0) | (st > 0)
                                   THEN
                                       $(
                                       complain('X')
                                       eval := 0
                                       $)
                                   eval := (op = op.or ->
                                                 eval | sval,
                                                 eval & sval)
                                   ENDCASE

                 $)
            ENDCASE

            DEFAULT: complain('Q')
           $)

        skiplayout()
        op := chkop()

        TEST op > 0
        THEN
            $(  || Another operator
            rch();  skiplayout()

            IF chkop() > 0
            THEN
                $(
                complain('X')
                symb,symbtype := 0,s.number
                LOOP
                $)
            readsymb()
            $)
         ELSE BREAK || Not an operator: expression finished

        $) REPEAT

    IF (et \= 0) & (et \= 1)
    THEN $( complain('X');  et := 0 $)

    TEST regsymb
    THEN
        $(
        exptype := s.reg
        IF abssymb | relsymb
        THEN complain('X')
        $)
    ELSE exptype := (et = 1 -> s.rel,s.abs)
    expval := eval

    $)


AND chkop() = VALOF
  SWITCHON ch INTO
    $( CASE '+': RESULTIS op.plus
       CASE '-': RESULTIS op.minus
       CASE '**':RESULTIS op.times
       CASE '/': RESULTIS op.over
       CASE '!': RESULTIS op.or
       CASE '&': RESULTIS op.and

       DEFAULT:  RESULTIS 0

    $)


AND readsymb() BE
  $( || Reads symbol, setting SYMB and SYMBTYPE
     $( skiplayout()
        SWITCHON ch INTO

          $( CASE '*N': symbtype := s.none; RETURN

             CASE '0':CASE '1':CASE'2':CASE'3':CASE'4':
             CASE '5':CASE '6':CASE '7':CASE '8': CASE '9':
                symb := readnumber()
                symbtype := s.number
                RETURN

              CASE ',':
                symbtype := s.none
                BREAK

              CASE '%':
                symbtype := s.pcent
                BREAK

              CASE '<':
                symbtype := s.labr
                BREAK

              CASE '*'':
                rch()
                symb := ch
                symbtype := s.number
                TEST ch = '*N'
                THEN complain('Q')
                ELSE rch()

                RETURN

              CASE '"':
                symbtype := s.number
                rch()
                symb := ch

                IF ch = '*N'
                THEN $( complain('Q'); RETURN $)
                rch()
                symb := symb + (ch << 8)

                TEST ch = '*N'
                THEN complain('Q')
                ELSE rch()

                RETURN

              CASE '+': CASE '-':
                symbtype := s.monop
                symb := (ch = '+' -> op.plus,op.minus)
                BREAK

              CASE '.':
                rch()
                UNLESS ('0' <= ch <= '9') | ('A' <= ch <= 'Z') THEN
                  $( symbtype := s.dot
                     RETURN
                  $)
                unrch(); ch := '.'

              CASE 'A':CASE 'B':CASE 'C':CASE'D':CASE 'E':
              CASE 'F':CASE 'G':CASE 'H':CASE 'I':CASE 'J':
              CASE 'K':CASE 'L':CASE 'M':CASE 'N':CASE 'O':
              CASE 'P':CASE 'Q':CASE 'R':CASE 'S':CASE 'T':
              CASE 'U':CASE 'V':CASE 'W':CASE 'X':CASE 'Y':
              CASE 'Z':
                readtag()
                RETURN

              DEFAULT:
                complain('I')
                outbufp := outbufp - 1
                pch('['); pch(ch); pch(']')
                rch()

           $)

        $) REPEAT

     rch()
  $)


AND changemode(mode) BE
    IF mode \= locmode
    THEN
        $(
        TEST locmode = s.abs
        THEN
            $(
            absmin := minloc
            absmax := maxloc
            absloc := location
            absrp := relp
            minloc := relmin
            maxloc := relmax
            location := relloc
            codevec := relvec
            relocvec := relrvec
            relp := relrp
            $)
        ELSE
            $(
            relmin := minloc
            relmax := maxloc
            relloc := location
            relrp := relp
            minloc := absmin
            maxloc := absmax
            location := absloc
            codevec := absvec
            relocvec := absrvec
            relp := absrp
            $)

        locmode := mode
        $)




AND setloc(newloc) BE
  $(
  IF newloc > maxloc THEN maxloc := newloc
  IF newloc < minloc THEN minloc := newloc
  location := newloc
  $)



AND rch() BE
    $(
    TEST charpos >= 73
    THEN $( charpos := 0; skiprest() $)
    ELSE
        $(
        ch := rdch()
        IF ch = endstreamch
        THEN $( ended := TRUE;  ch := '*N' $)

        pch(ch)
        charpos := charpos + 1
        $)

    IF ch = '*N' THEN charpos := 1
    $)


AND unrch() BE
  $( unrdch()
     outbufp := outbufp - 1
     charpos := charpos - 1
  $)


AND putloc(locval) BE putn(6, locval, locmode)


AND putb(p, n) BE
    $(  || Write value of one byte to buffer
    FOR i = p + 2 TO p BY -1
    DO
        $(
        outbuf%i := (n & 7) + '0'
        n := n >> 3
        $)
    $)


AND putn(p, n, t) BE
    $(
    || Writes the value of N starting at column P.
    || If T is S.REL then the value is flagged with
    || a quote.
    || If it is S.REG then the value is flagged with '%'.
    n := n & #177777  || In case BYTESPERWORD > 2

    FOR j = p + 5 TO p BY -1
    DO
        $(
        outbuf%j := (n & 7) + '0'
        n := n >> 3
        $)

    IF t = s.rel THEN outbuf%(p+6) := '*''
    IF t = s.reg THEN outbuf%(p+6) := '%'
    $)


AND pch(ch) BE
  TEST outbufp > outbuflim THEN
    complain('L')
   ELSE
    $( outbuf%outbufp := ch
       outbufp := outbufp + 1
    $)

AND clearbits() BE
  FOR j = 0 TO tagtablesize - 1 DO
    $( LET p = tagtable ! j
       UNTIL p = 0 DO
         $( IF (st.type ! p & stb.temp) > 0 THEN
              st.type ! p := st.type ! p &
                             NOT stb.setnow
            p := !p
         $)
    $)


AND error(format, a, b, c) BE
    $(
    writef(format, a, b, c)
    tidy.up.and.stop(16)
    $)



AND lookup() BE
    $(
    || Looks up the tag in TAGV in the symbol table.
    || If it is not already there, a new entry is
    || created, with type S.NEW and value zero.
    || Returns with SYMB pointing to the entry, and
    || sets SYMBTYPE

    LET hashval = ((tagv!0 + tagv!(tagsize-1)) >> 1) REM
                  tagtablesize
    LET i = 0

    symb := tagtable!hashval

    UNTIL (symb = 0) | (i >= tagsize)
    DO
        $(
        TEST symb!(i + 1) = tagv!i
        THEN i := i + 1
        ELSE symb, i := !symb, 0
        $)

    TEST symb = 0
    THEN
        $(  || Need to create a new entry
        symb := newvec(tagsize + 2)
        !symb := tagtable!hashval   || Link it in
        tagtable!hashval := symb

        || And copy in the tag
        FOR k = 0 TO tagsize - 1
        DO symb!(k + 1) := tagv!k

        || Set initial value and type
        symb!st.type := s.new | stb.temp
        symb!st.value := 0
        $)
    ELSE
        || Check for undefined symbols
        IF pass2 & ((symb ! st.type & stb.setever) = 0)
        THEN complain('U')

    symbtype := (symb ! st.type) & #77
    $)




AND newvec(upb) = VALOF
    $(
    // Allocates a vector with upperbound upb from
    // current.stvec if possible.  Otherwise, it
    // gets a new block of store, appends it to
    // stvec.chain and allocates the vector from
    // there.
    LET p = stvec.offset + current.stvec
    stvec.offset := stvec.offset + upb + 1

    IF stvec.offset > stvecupb
    THEN
      $( || Current vec is full: get another
      LET last.vec = current.stvec
      current.stvec := gvec(stvecupb) // OK if returns
      last.vec!0 := current.stvec // Append to chain
      current.stvec!0 := 0        // End of chain
      stvec.offset    := 1
      RESULTIS newvec(upb) // Goes terribly wrong if
                           // upb > stvecupb !!
                           // (It never is)
      $)

    // OK: room in current vector
    ttused := ttused + upb + 1
    RESULTIS p
    $)



AND reportundefs() BE
    $(  || Print list of undefined symbols
    LET messnotout, numonline = TRUE, 0

    FOR j = 0 TO tagtablesize - 1
    DO
        $(
        LET p = tagtable ! j

        UNTIL p = 0
        DO
            $(
            IF ((st.type ! p) & stb.setever) = 0
            THEN
                $(
                IF messnotout
                THEN
                    $(
                    writes("*N*NThe following symbols*
                           * are undefined:*N*N")
                    messnotout := FALSE
                    $)

                writes("   ")

                FOR j = 0 TO tagchars - 1
                DO wrch((p+1)%j)

                numonline := numonline + 1

                IF numonline >= 8
                THEN $( newline(); numonline := 0 $)
                $)

            p := !p
            $)
        $)
    IF numonline \= 0 THEN newline()
    $)


.

SECTION "PDPASM3"
GET ""


LET dodir() BE
    $(
    // Deals with the directive currently in SYMB

    SWITCHON symb ! st.value INTO
        $(
        CASE d.ascii: // ASCII string
                      buildstring(FALSE)
                      ENDCASE

        CASE d.asciz: // ASCII string with zero byte
                      // on the end
                      buildstring(TRUE)
                      ENDCASE

        CASE d.asect: // Start/resume absolute section
                      changemode(s.abs)
                      putn(14, location, locmode)
                      ENDCASE

        CASE d.blkb:  // Leave block of n bytes
                      readsymb()
                      readabsexp(FALSE)
                      putn(14, expval, exptype)
                      setloc(location + expval)
                      ENDCASE

        CASE d.blkw:  // Leave block of n words
                      evenloc()  // Align to word boundary
                      readsymb()
                      readabsexp(FALSE)
                      putn(14, expval, exptype)
                      setloc(location + 2*expval)
                      ENDCASE

        CASE d.byte:  // Assemble byte values

                      $(
                      LET b = 0

                      readsymb()

                      UNTIL symbtype = s.none
                      DO
                          $(
                          readabsexp(TRUE)
                          buildbyte(expval, b)
                          skipcomma()
                          readsymb()
                          b := b + 1
                          $)
                      $)
                      ENDCASE

        CASE d.csect: // Start/resume relocatable
        CASE d.psect: // section
                      changemode(s.rel)
                      putn(14, location, locmode)
                      skiprest()  // Ignore section name
                      ENDCASE

        CASE d.end:   ended := TRUE
                      skiprest()  // Ignore entry point
                      ENDCASE

        CASE d.even:  // Align to word boundary
                      IF (location & 1) = 1
                      THEN location := location + 1
                      ENDCASE

        CASE d.limit: // Assemble two words containing
                      // program limits
                      buildword(minloc, locmode, 0)
                      buildword(maxloc, locmode, 1)
                      ENDCASE

        CASE d.list:  // Increment listing level
                      readsymb()  // Check for parms

                      IF symbtype = s.none
                      THEN
                          $(  // No parms
                          listlev := listlev + 1
                          listing := (listlev > 0)
                          $)
                      ENDCASE

        CASE d.nlist: // Decrement listing level
                      readsymb()

                      IF symbtype = s.none
                      THEN
                          $(
                          listlev := listlev - 1
                          listing := (listlev > 0)
                          $)
                      ENDCASE

        CASE d.odd:   // Make location counter odd
                      location := location | 1
                      ENDCASE

        CASE d.radix: // Set default radix to n.
                      // Value must be between 2 and
                      // 10 (inclusive).
                      // The value is read in base 10.
                      // If it is not acceptable, the
                      // radix will become 10.
                      radix := 10
                      readsymb()
                      readabsexp(FALSE)

                      TEST 2 <= radix <= 10
                      THEN radix := expval
                      ELSE complain('N')

                      putn(14, expval, exptype)
                      ENDCASE

        CASE d.word:  // Assemble word values
                      $(
                      LET w = 0

                      evenloc() // Align to word boundary
                      readsymb()

                      UNTIL symbtype = s.none
                      DO
                          $(
                          readexp()
                          buildword(expval, exptype, w)
                          skipcomma()
                          readsymb()
                          w := w + 1
                          $)

                      $)
                      ENDCASE

        CASE d.unimp: // Used for DOS MACRO directives
                      // which are not implemented here
                      warn('Y')
                      skiprest()  // Ignore any parms
                      ENDCASE

        DEFAULT:      // Should never occur!
                      error("Invalid directive *
                           *code %N*N", symb!st.value)
        $)
    $)


AND buildword(v, t, wordonline) BE
    $(
    // Lays down one word of code on the next word
    // boundary.  Puts an entry in the appropriate
    // relocation vector if the word is relocatable.
    // Puts the word in the output line buffer

    v := v & #177777  // Needed when BYTESPERWORD > 2
    setloc(location)  // To get MINLOC right
    printloc := TRUE
    evenloc()

    IF pass2
    THEN
        $(
        codevec ! (location/2) := v

        TEST wordonline < maxwol
        THEN putn(14 + wordonline*8, v, t)
        ELSE morewords := morewords + 1
        $)

    IF t = s.rel
    THEN
        $(
        IF pass2 THEN relocvec!relp := location/2
        relp := relp + 1
        $)
    setloc(location + 2)
    $)


AND buildbyte(v, byteonline) BE
    $(
    // Puts down one byte of code and complains
    // if the value was too big for one byte.
    // Writes the value of the byte to the output
    // line buffer

    setloc(location)  // To get MINLOC right
    printloc := TRUE

    UNLESS (v & #177400) = 0
    THEN complain('T')

    IF pass2
    THEN
        $(
        setbyte(codevec, location, v)

        TEST byteonline < maxbol
        THEN putb(14 + byteonline*4, v)
        ELSE morebytes := morebytes + 1
        $)

    setloc(location + 1)
    $)


AND evenloc() BE
    $(
    // Makes location counter even and complains
    // if it wasn't before.
    IF (location & 1) = 1
    THEN
        $(
        locsave := locsave + 1
        location := location + 1
        complain('B')
        $)
    $)


AND readaddress() = VALOF
    $(
    // Reads the general operand whose first character
    // is in or after CH.
    // The value returned is the number of bytes that
    // this operand adds to the instruction
    // length (0 or 2).
    // The l.s. 6 bits of ADDRMODE are set to the
    // code for the address mode and register.
    // ADDRVAL is set to the value, and ADDRTYPE to
    // the type of the extra operand word (if any).

    skiplayout()

    IF 'A' <= ch <= 'Z'
    THEN
        $(  // R, X(R), or A
        readsymb()
        readexp(TRUE)

        IF exptype = s.reg
        THEN
            $(  // Simple register operand R
            addrmode := #00 + expval
            RESULTIS 0
            $)

        IF (exptype = s.rel) | (exptype = s.abs)
        THEN
            $(
            IF ch = '('
            THEN
                $(  // Indexed register X(R)
                addrval := expval
                addrtype := exptype
                readbrarket()
                addrmode := #60 + expval
                RESULTIS 2
                $)

            TEST exptype = locmode
            THEN
                $(  // Relative address
                addrval := expval - locsave - ilength - 2
                addrtype := s.abs
                $)
            ELSE
                $(  // Real address
                addrval := expval
                addrtype := exptype
                $)

            addrmode := #67
            RESULTIS 2
            $)

        complain('Q')
        RESULTIS 0
        $)

    // The first character of the operand was not
    // a letter - try the other possibilities.

    SWITCHON ch INTO
        $(
        CASE '(': // (R) or (R)+
                  readbrarket()

                  TEST ch = '+'
                  THEN
                      $(
                      rch()
                      addrmode := #20 // (R)+
                      $)
                  ELSE addrmode := #10  // (R)

                  addrmode := addrmode + expval
                  RESULTIS 0

        CASE '#': // Immediate operand #n
                  rch()
                  readsymb()
                  readexp()
                  addrval := expval
                  addrtype := exptype
                  addrmode := #27
                  RESULTIS 2

        CASE '%': // Register expression operand
                  readsymb()
                  readregexp()
                  addrmode := expval
                  RESULTIS 0

        CASE '-': // -(R) or -X(R)
                  rch()

                  IF ch = '('
                  THEN
                      $(  // -(R)
                      readbrarket()
                      addrmode := #40 + expval
                      RESULTIS 0
                      $)

                  // Expect -X(R), where X is an absolute
                  // expression

                  unrch()
                  ch := '-'
                  readsymb()
                  readabsexp(TRUE)
                  addrval := expval
                  addrtype := exptype
                  readbrarket()
                  addrmode := #60 + expval
                  RESULTIS 2

        CASE '@': // Deferred mode: @R, @(R)+, @-(R),
                  // @X(R), @#A or @A
                  // Deferred modes have the same codes
                  // as those without the '@', with
                  // bit #10 set in ADDRMODE.

                  rch()
                  $(
                  LET a = readaddress()
                  addrmode := addrmode | #10
                  RESULTIS a
                  $)

        CASE '0': CASE '1': CASE '2':
        CASE '3': CASE '4': CASE '5':
        CASE '6': CASE '7': CASE '8':
        CASE '9':
                  // X(R) or A
                  readsymb()
                  readexp()

                  IF ch = '('
                  THEN
                      $(  // Indexed register X(R)
                      addrval := expval
                      addrtype := s.abs
                      readbrarket()
                      addrmode := #60 + expval
                      RESULTIS 2
                      $)

                  // Relative address A
                  addrmode := #67
                  addrval := expval - location - 4
                  addrtype := s.abs
                  RESULTIS 2

        DEFAULT:  // What was the operand?
                  complain('Q')
                  RESULTIS 0
        $)
    $)


AND readbrarket() BE
    $(
    // Reads '(', a register expression, and ')'.
    // Called with '(' in CH.
    // Puts the value of the expression into the
    // global EXPVAL.
    // If the expression is not a register expression,
    // then gives error 'R' and sets EXPVAL to zero.
    // If the ')' is missing, gives soft error 'K'.

    rch()  // Skip the '('
    readsymb()
    readregexp()  // Sets EXPVAL

    TEST ch = ')'
    THEN rch()
    ELSE complain('K')
    $)


AND readnumber() = VALOF
    $(
    // Reads the number which starts with CH.
    // If it ends with '.' then it is treated as decimal,
    // otherwise it is read in the current RADIX.
    // If the number contains the digit(s) 8 or 9, does
    // not end in '.', and RADIX is not 10, then it
    // is read as decimal, and error 'N' is reported.
    // In any other case where a digit greater than
    // or equal to RADIX is encountered, reading stops,
    // and the function returns zero.
    // RADIX must not be greater than 10.

    LET digitv = VEC 36
    LET i = 0
    LET flag89 = FALSE
    LET val = 0
    LET locradix = radix

    WHILE '0' <= ch <= '9'
    DO
        $(
        digitv!i := ch
        i := i + 1
        IF (ch = '8') | (ch = '9') THEN flag89 := TRUE
        rch()
        $)

    TEST ch = '.'
    THEN $( locradix := 10; rch() $)
    ELSE IF flag89 & (radix \= 10)
         THEN
             $(
             complain('N')
             locradix := 10
             $)

    // Now work out the value
    FOR k = 0 TO i-1
    DO
        $(
        LET d = digitv!k - '0'

        IF d >= locradix
        THEN $( complain('N'); RESULTIS 0 $)

        val := val*locradix + d
        $)

    RESULTIS val
    $)


AND readtag() BE
    $(
    // Reads the tag whose first character is in CH,
    // looks it up in the symbol table, and creates
    // an entry for it if necessary. SYMB is set
    // to point to the entry; SYMBTYPE is set from
    // the type field if the entry existed, otherwise
    // to S.NEW

    LET len = 0

    WHILE ('A' <= ch <= 'Z') | ('0' <= ch <= '9') |
          (ch = '.')
    DO
        $(
        UNLESS len = tagchars
        THEN
            $( // Only keep first TAGCHARS chars
            tagv%len := ch
            len := len + 1
            $)

        rch()
        $)

    // Pad out with spaces
    FOR k = len TO tagbyteupb DO tagv%k := ' '

    lookup()
    $)



AND doinstr() BE
    $(
    // Called with an opcode in SYMB.
    // Reads and produces code for the instruction
    // and its operands.

    LET w = VEC 2
    LET wt = VEC 2  // Word type
    LET it = (symb ! st.type) >> 11
    LET l = 0
    ilength := 2  // Instruction length

    IF ((symb ! st.type) & stb.nrc) \= 0
    THEN warn('Z')

    w!0 := symb ! st.value
    wt!0 := s.abs

    SWITCHON it INTO
        $(
        CASE i.a:   // Single address operand
                    l := readaddress()

                    IF l>0
                    THEN
                        $(
                        w!1 := addrval
                        wt!1 := addrtype
                        ilength := ilength + l
                        $)

                    w!0 := w!0 + addrmode


                    // Check for the non range-compatible
                    // instructions   JMP  (R)+
                    //                JMP  R

                    IF (w!0 & #177750) = #000100
                    THEN warn('Z')

                    ENDCASE

        CASE i.aa:  // Two address operands
                    l := readaddress()
                    w!0 := w!0 + (addrmode << 6)

                    IF l>0
                    THEN
                        $(
                        w!1 := addrval
                        wt!1 := addrtype
                        ilength := ilength + l
                        $)

                    skipcomma()
                    l := readaddress()
                    w!0 := w!0 + addrmode

                    IF l>0
                    THEN
                        $(
                        w!(ilength/2) := addrval
                        wt!(ilength/2) := addrtype
                        ilength := ilength + l
                        $)

                    // Check for non range-compatible
                    // operand combinations:
                    //   op  R,(R)+       op  R,@(R)+
                    //   op  R,-(R)       op  R,@-(R)
                    //   MOV PC,X(R)      MOV PC,@X(R)

                    l := w!0

                    IF (l & 7) = ((l >> 6) & 7)
                    THEN
                        // Same register used twice
                        // Test the modes
                        IF (2 <= ((l >> 3) & #707) <= 5)
                        THEN warn('Z')

                    // Test for MOV PC,X(R)
                    IF (l & #177760) = #010760
                    THEN warn('Z')

                    ENDCASE

        CASE i.bch: // Branch instruction
                    readsymb()

                    readexp(TRUE)

                    TEST exptype = locmode
                    THEN l := expval - locsave - 2
                    ELSE l := 1000  // To provoke 'A'

                    IF (l < -256) | (l > 254) |
                       ((l REM 2) = 1)
                    THEN
                        $(  // Invalid offset
                        complain('A')
                        l := 0
                        $)

                    w!0 := w!0 + ((l >> 1) & #377)
                    ENDCASE

        CASE i.r:   // Single register operand
                    readsymb()
                    w!0 := w!0 + readregexp()
                    ENDCASE

        CASE i.rd:  // Register, destination operands
                    readsymb()
                    w!0 := w!0 + (readregexp() << 6)
                    skipcomma()
                    l := readaddress()
                    w!0 := w!0 + addrmode

                    IF l>0
                    THEN
                        $(
                        w!1 := addrval
                        wt!1 := addrtype
                        ilength := ilength + l
                        $)

                    // Check for the non range-compatible
                    // instructions   JSR  anyreg,(R)+
                    //                JSR  R,R

                    l := w!0

                    IF (l & #177000) = #004000
                    THEN
                        // JSR instruction
                        IF ((l & #70) = #20) |
                           (((l >> 6) & 7) = (l & #77))
                        THEN warn('Z')
                    ENDCASE

        CASE i.ro:  // Register, offset operands
                    readsymb()
                    w!0 := w!0 + (readregexp() << 6)
                    skipcomma()
                    readsymb()

                    readexp(FALSE)

                    TEST exptype = locmode
                    THEN l := locsave + 2 - expval
                    ELSE l := -1  // To provoke error 'A'

                    // Can only branch backwards
                    IF (l < 0) | (l > 126) |
                       ((l REM 2) = 1)
                    THEN
                        $(  // Invalid offset
                        complain('A')
                        l := 0
                        $)

                    w!0 := w!0 + ((l >> 1) & #77)
                    ENDCASE

        CASE i.sr:  // Source, register operands
                    l := readaddress()
                    skipcomma()
                    w!0 := w!0 + addrmode

                    IF l>0
                    THEN
                        $(
                        w!1 := addrval
                        wt!1 := addrtype
                        ilength := ilength + l
                        $)

                    readsymb()
                    w!0 := w!0 + (readregexp() << 6)
                    ENDCASE

        CASE i.zop: // Zero operands
                    ENDCASE

        CASE i.3n:  // 3 bit number
                    readsymb()
                    l := readabsexp(FALSE)

                    TEST 0 <= l <= 7
                    THEN w!0 := w!0 + l
                    ELSE complain('T')
                    ENDCASE

        CASE i.6n:  // 6 bit number
                    readsymb()
                    l := readabsexp(FALSE)

                    TEST 0 <= l <= 63
                    THEN w!0 := w!0 + l
                    ELSE complain('T')
                    ENDCASE

        CASE i.8n:  // 8 bit number
                    readsymb()
                    l := readabsexp(FALSE)

                    TEST 0 <= l <= 255
                    THEN w!0 := w!0 + l
                    ELSE complain('T')
                    ENDCASE
        $)

    // Now output the assembled words

    FOR i = 0 TO ilength/2 - 1
    DO buildword(w!i, wt!i, i)

    $)


AND buildstring(endinzero) BE
    $(
    // Reads the parameters to .ASCII and .ASCIZ,
    // and outputs the bytes of the string.
    // A zero byte is output last if ENDINZERO is TRUE.

    LET b = 0  // Number of byte on line
    LET term, val = ?, ?

    skiplayout()

    UNTIL ch = '*N'
    DO
        $(
        TEST ch = '<'
        THEN
            $(  // <single byte value>
            rch()  // Skip the '<'
            readsymb()
            val := readabsexp(FALSE)
            buildbyte(val, b)
            b := b + 1

            TEST ch = '>'
            THEN rch()
            ELSE complain('Q')
            $)
        ELSE
            $(  // Delimited string
            term := ch
            rch()

            UNTIL (ch = term) | (ch = '*N')
            DO
                $(
                buildbyte(ch, b)
                b := b + 1
                rch()
                $)

            IF ch = term THEN rch()
            $)

        skiplayout()
        $)

    IF endinzero THEN buildbyte(0, b)
    $)


AND readregexp() = VALOF
    $(  // Reads a register expression
    readexp(FALSE)

    UNLESS (exptype = s.reg) & (0 <= expval <= 7)
    THEN
        $(
        complain('R')
        exptype, expval := s.reg, 0
        $)

    RESULTIS expval
    $)



AND readrelexp(frefs) = VALOF
    $(
    readexp(frefs)

    UNLESS exptype = s.rel
    THEN
        $(
        complain('A')
        exptype, expval := s.rel, 0
        $)

    RESULTIS expval
    $)



AND outcode() BE
    $(
    // Output the assembled code
    LET printstr = output()

    selectoutput(codestream)

    // First the relocatable HUNK
    outword(t.hunk)
    outword((relmax + 1)/2)
    writewords(relvec, (relmax + 1)/2)

    // Second the relocation info for it
    IF relrp > 0
    THEN
        $(
        outword(t.reloc)
        outword(relrp)
        writewords(relrvec, relrp)
        $)

    // Third the absolute HUNK
    IF absmax > 0
    THEN
        $(
        outword(t.abshunk)
        outword(absmin/2)
        outword((absmax - absmin + 1)/2)

        writewords(absvec + absmin/2, (absmax - absmin + 1)/2)

        // Fourth the ABSHUNK reloc info
        IF absrp > 0
        THEN
            $(
            outword(t.absrel)
            outword(absrp)

            writewords(absrvec, absrp)
            $)
        $)

    outword(t.end)
    selectoutput(printstr)
    $)


AND outword(x) BE writewords(@x, 1)


AND setbyte(v, o, b) BE
    $(
    // Like PUTBYTE, except that it always packs
    // two bytes per word, starting with the
    // least significant.
    LET w = o/2

    TEST (o & 1) = 0
    THEN v!w := (v!w & #177400) | (b & #377)
    ELSE v!w := (v!w & #377) | ((b << 8) & #177400)
    $)



AND gbyte(v, o) = VALOF
    // Inverse of SETBYTE
    TEST (o & 1) = 0
    THEN RESULTIS v!(o >> 1) & #377
    ELSE RESULTIS (v!(o >> 1) >> 8) & #377



AND warn(code) BE
    $( // Record a warning
    IF pass1 THEN RETURN
    mark.error(code)
    warncount := warncount+1
    $)


AND complain(code) BE
    $( // Record an assembly error
    IF pass1 THEN RETURN
    mark.error(code)
    errcount := errcount + 1
    $)


AND mark.error(code) BE
    $(  || Puts out the error letter CODE
    UNLESS (nerrs >= 5) | pass1
    THEN
        $(
        FOR i = 0 TO 4
        DO  IF code = outbuf%i
            THEN RETURN  || CODE letter already there

        outbuf%nerrs := code
        nerrs := nerrs + 1
        $)
    $)



AND tidy.up.and.stop(n) BE
    $(
    // Frees vectors, closes streams, and stops
    IF pass2
    THEN
      $( // These have not been got in pass 1
      UNLESS absvec=0
      THEN freevec(absvec + absmin/2)
      freevec(absrvec)
      freevec(relvec)
      freevec(relrvec)
      $)

    // Free symbol table blocks
    UNTIL stvec.chain = 0
    DO $(
       LET v = stvec.chain
       stvec.chain := v!0
       freevec(v)
       $)

    UNLESS sourcestream=0
    THEN $( selectinput(sourcestream); endread() $)
    UNLESS liststream=0
    THEN $( selectoutput(liststream); endwrite() $)
    UNLESS codestream=0
    THEN $( selectoutput(codestream); endwrite() $)
    stop(n)
    $)


.
SECTION "DeclSysWords"
GET ""
GET "clihdr" // Just for cli.module !!

LET declsyswords() = VALOF
    $(
    || Sets up the initial symbol table with the opcodes,
    || register symbols, and assembler directives.
    || Returns the address of this BCPL section
    || so it can be freed when it has been used

    || Values of the opcodes:

    valpntr := TABLE #000000, #000001, #000002, #000003,
                     #000004, #000005, #000006, #000100,
                     #000200, #000230, #000240, #000241,
                     #000242, #000244, #000250, #000257,
                     #000261, #000262, #000264, #000270,
                     #000277, #000300, #000400, #001000,
                     #001400, #002000, #002400, #003000,
                     #003400, #004000, #005000, #005100,
                     #005200, #005300, #005400, #005500,
                     #005600, #005700, #006000, #006100,
                     #006200, #006300, #006400, #006500,
                     #006600, #006700, #010000, #020000,
                     #030000, #040000, #050000, #060000,
                     #070000, #071000, #072000, #073000,
                     #074000, #075000, #075010, #075020,
                     #075030, #077000, #100000, #100400,
                     #101000, #101400, #102000, #102400,
                     #103000, #103000, #103400, #103400,
                     #104000, #104400, #105000, #105100,
                     #105200, #105300, #105400, #105500,
                     #105600, #105700, #106000, #106100,
                     #106200, #106300, #106400, #106500,
                     #106600, #106700, #110000, #120000,
                     #130000, #140000, #150000, #160000,

                     || Register values

                     0, 1, 2, 3, 4, 5, 6, 7,

                     || Directives

                     d.ascii, d.asciz, d.asect, d.blkb,
                     d.blkw,  d.byte,  d.csect, d.unimp,
                     d.unimp, d.end,   d.unimp, d.unimp,
                     d.unimp, d.unimp, d.even,  d.unimp,
                     d.unimp, d.unimp, d.unimp, d.unimp,
                     d.unimp, d.unimp, d.unimp, d.unimp,
                     d.unimp, d.unimp, d.limit, d.list,
                     d.unimp, d.unimp, d.unimp, d.unimp,
                     d.nlist, d.unimp, d.odd,   d.unimp,
                     d.psect, d.unimp, d.radix, d.unimp,
                     d.unimp, d.unimp, d.unimp, d.word

    || The types of the initial symbols.
    || The types of the opcodes are S.INSTR with
    || the instruction type in the m.s. 6 bits
    || of the word.

    typepntr := TABLE sti.zop, sti.zop, sti.zop, sti.zop,
                      sti.zop, sti.zop, sti.zopz,sti.a,
                      sti.r,   sti.3nz,  sti.zop, sti.zop,
                      sti.zop, sti.zop, sti.zop, sti.zop,
                      sti.zop, sti.zop, sti.zop, sti.zop,
                      sti.zop, sti.a,   sti.bch, sti.bch,
                      sti.bch, sti.bch, sti.bch, sti.bch,
                      sti.bch, sti.rd,  sti.a,   sti.a,
                      sti.a,   sti.a,   sti.a,   sti.a,
                      sti.a,   sti.a,   sti.a,   sti.a,
                      sti.a,   sti.a,   sti.6nz, sti.az,
                      sti.az,  sti.az,  sti.aa,  sti.aa,
                      sti.aa,  sti.aa,  sti.aa,  sti.aa,
                      sti.srz, sti.srz, sti.srz, sti.srz,
                      sti.rdz, sti.rz,  sti.rz,  sti.rz,
                      sti.rz,  sti.roz, sti.bch, sti.bch,
                      sti.bch, sti.bch, sti.bch, sti.bch,
                      sti.bch, sti.bch, sti.bch, sti.bch,
                      sti.8n,  sti.8n,  sti.a,   sti.a,
                      sti.a,   sti.a,   sti.a,   sti.a,
                      sti.a,   sti.a,   sti.a,   sti.a,
                      sti.a,   sti.a,   sti.az,  sti.az,
                      sti.az,  sti.az,  sti.aa,  sti.aa,
                      sti.aa,  sti.aa,  sti.aa,  sti.aa,

                      || Register symbols

                      s.reg, s.reg, s.reg, s.reg,
                      s.reg, s.reg, s.reg, s.reg,

                      || Directives

                      s.dir, s.dir, s.dir, s.dir,
                      s.dir, s.dir, s.dir, s.dir,
                      s.dir, s.dir, s.dir, s.dir,
                      s.dir, s.dir, s.dir, s.dir,
                      s.dir, s.dir, s.dir, s.dir,
                      s.dir, s.dir, s.dir, s.dir,
                      s.dir, s.dir, s.dir, s.dir,
                      s.dir, s.dir, s.dir, s.dir,
                      s.dir, s.dir, s.dir, s.dir,
                      s.dir, s.dir, s.dir, s.dir,
                      s.dir, s.dir, s.dir, s.dir

    || Set up symbol table entries for all the symbols

    declare("HALT/WAIT/RTI/BPT/*
            *IOT/RESET/RTT/JMP/*
            *RTS/SPL/NOP/CLC/*
            *CLV/CLZ/CLN/CCC/*
            *SEC/SEV/SEZ/SEN/*
            *SCC/SWAB/BR/BNE/*
            *BEQ/BGE/BLT/BGT/*
            *BLE/JSR/CLR/COM/*
            *INC/DEC/NEG/ADC/*
            *SBC/TST/ROR/ROL/*
            *ASR/ASL/MARK/MFPI/*
            *MTPI/SXT/MOV/CMP/*
            *BIT/BIC/BIS/ADD//")

    declare("MUL/DIV/ASH/ASHC/*
            *XOR/FADD/FSUB/FMUL/*
            *FDIV/SOB/BPL/BMI/*
            *BHI/BLOS/BVC/BVS/*
            *BCC/BHIS/BCS/BLO/*
            *EMT/TRAP/CLRB/COMB/*
            *INCB/DECB/NEGB/ADCB/*
            *SBCB/TSTB/RORB/ROLB/*
            *ASRB/ASLB/MTPS/MFPD/*
            *MTPD/MFPS/MOVB/CMPB/*
            *BITB/BICB/BISB/SUB//")

    || Standard register symbols

    declare("R0/R1/R2/R3/R4/R5/SP/PC//")

    || Assembler directives

    declare(".ASCII/.ASCIZ/.ASECT/.BLKB/*
            *.BLKW/.BYTE/.CSECT/.DSABL/*
            *.ENABL/.END/.ENDC/.ENDM/*
            *.EOT/.ERROR/.EVEN/.FLT2/*
            *.FLT4/.GLOBL/.IDENT/.IF/*
            *.IFF/.IFT/.IFTF/.IIF/*
            *.IRP/.IRPC/.LIMIT/.LIST//")

    declare(".MACRO/.MEXIT/.NARG/.NCHR/*
            *.NLIST/.NTYPE/.ODD/.PAGE/*
            *.PSECT/.PRINT/.RADIX/.RAD50/*
            *.REPT/.SBTTL/.TITLE/.WORD//")

    // This BCPL section is no longer needed.
    // Unchain it, and pass back the section
    // address, so the caller can free it.
    /****** Only works under TRIPOS *******/
    $(
       LET dsw.module = cli.module
       LET r = ?
       UNTIL !!dsw.module = 0
       DO dsw.module := !dsw.module

       // Unlink the segment
       r := !dsw.module
       !dsw.module := 0
       RESULTIS r
    $)
    $)


AND declare(words) BE
    $(
    || Takes the words separated by '/' from the
    || string WORDS, and creates symbol table entries
    || for them.
    || A null word marks the end of WORDS.
    || The values and types are taken from VALPNTR
    || and TYPEPNTR.

    LET i, length = 1, 0

        $(  || Main loop
        LET ch = words%i

        TEST ch = '/'
        THEN
            $(  || Have read a complete word
            IF length = 0 THEN RETURN

            || Fill rest of tag area with spaces
            FOR k = length TO tagbyteupb
            DO tagv%k := ' '

            lookup()
            symb!st.value := !valpntr
            valpntr := valpntr + 1
            symb!st.type := !typepntr | stb.setnow |
                            stb.setever
            typepntr := typepntr + 1
            length := 0
            $)
        ELSE
            $(  || Read next character, trusting that no
                || word is longer than TAGCHARS chars
            tagv%length := ch
            length := length + 1
            $)

        i := i + 1
        $) REPEAT
    $)



