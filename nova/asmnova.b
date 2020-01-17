// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

GET "LIBHDR"
GET "CLIHDR"

GLOBAL $(

absloc         :       150
absmin         :       151
absmax         :       152
absvec         :       153
absrp          :       154
absrvec        :       155
avec           :       156

relloc         :       160
relmin         :       161
relmax         :       162
relvec         :       163
relrp          :       164
relrvec        :       165

location       :       170
minloc         :       171
maxloc         :       172
codevec        :       173
relp           :       174
relocvec       :       175
locmode        :       176

indirect       :       177
noload         :       178

external.symbol:       179

symb           :       180
symbtype       :       181
expval         :       182
exptype        :       183
tagv           :       184
ch             :       185
tagtable       :       186
stvec          :       187
stvecp         :       188
dotexp         :       189

outbuf         :       190
outbufp        :       191
nerrs          :       192
printres       :       193
charpos        :       194

ended          :       195
pass1          :       196
pass2          :       197

prstart        :       198
prend          :       199

nonull         :       200
packlr         :       201
radix          :       202

title.not.read :       203

valpntr        :       205
typepntr       :       206

numerrs        :       207
errors.found   :       208

argv           :       210
sourcestream   :       211
liststream     :       212
codestream     :       213
listing        :       214
failed         :       215
sysout         :       217

complain       :       220
freevectors    :       221
skiprest       :       222
skipoperand    :       223
readsymb       :       224
outcode        :       225
changemode     :       226
setloc         :       227
build          :       228
unrch          :       229
putloc         :       230
putval         :       231
putn           :       232
pch            :       233
clearbits      :       234
error          :       235
declsyswords   :       236
reportundefs   :       237
rch            :       238
skiplayout     :       240
stvecupb       :       241
inhdr          :       242
hdrstream      :       243
tidy.up.on.fail:       244
warn           :       245

       $)


MANIFEST
$(

// Basic constants
stvdefsize   =   3000
tagtablesize =    200
argvupb      =     50
maxint       =  32767
locationlimit= maxint
outbuflim    =    120

yes          =   TRUE
no           =  FALSE

// Symbol types
s.abs   =  1     // Tag with absolute value
s.dir   =  2     // Assembler directive
s.dot   =  3     // Location counter symbol
s.instr =  4     // Instruction mnemonic
s.new   =  5
s.none  =  6     // No symbol found before end of line
s.skpmn =  7     // Skip mnemonic for ALC insts.
s.rel   =  8     // Tag with relocatable value
s.lbr   =  9     // Left parenthesis
s.number= 10     // Number (123. or "e, say)
s.monop = 11     // Monadic operator
s.ext   = 12     // External reference


// Operators

op.plus   = 1
op.minus  = 2
op.times  = 3
op.over   = 4
op.and    = 5
op.or     = 6

// Instruction types

i.mra   =  1     // Memory reference with AC
i.mr    =  2     // MR without AC
i.alc   =  3     // Arithmetic and logical
i.ioa   =  4     // I/O with AC and device
i.io    =  5     // I/O with device
i.ion   =  6     // I/O with AC
i.zop   =  7     // No operands (HALT)

// Symbol table types for instructions.
// These are the type fields for entries for the
// instruction tags in the symbol table.

sti.mra   = (i.mra   << 10) + s.instr
sti.mr    = (i.mr    << 10) + s.instr
sti.alc   = (i.alc   << 10) + s.instr
sti.ioa   = (i.ioa   << 10) + s.instr
sti.io    = (i.io    << 10) + s.instr
sti.ion   = (i.ion   << 10) + s.instr
sti.zop   = (i.zop   << 10) + s.instr

// Directives

d.blk   =  1
d.end   =  2
d.loc   =  3
d.nrel  =  4
d.rdx   =  5
d.txt   =  6
d.txtm  =  7
d.txtn  =  8
d.zrel  =  9
d.ent   = 10
d.extn  = 11
d.titl  = 12
d.list  = 13

d.unimp = 20

tagchars=  5     // Max. number of chars in a tag
tagsize = (tagchars + bytesperword - 1)/bytesperword
                 // Number of words needed for tag
tagbyteupb = (tagsize * bytesperword) - 1
                 // Last byte offset in TAGSIZE words
fatal   =  yes  // For ERROR
continue = no // For ERROR


// Symbol table entry offsets

st.type  = tagsize + 1
st.value = tagsize + 2

// Symbol table type bits

stb.ent      =   #40
stb.setever  =  #100
stb.setnow   =  #200
stb.muldef   =  #400
stb.temp     = #1000

st.type.mask =   #X1F

// Object module identifiers.

t.hunk    = 1000
t.reloc   = 1001
t.end     = 1002
t.abshunk = 1003
t.absrel  = 1004
t.ext     = 1005

// External symbol type bytes

ext.entrel =   1
ext.entabs =   2
ext.ext    = 129


$)

.
SECTION "NOVASM"

GET ""

LET start() BE
  $( LET ob = VEC 60
     LET av = VEC argvupb
     LET tgv = VEC 2



     sysout   := output()
     argv     := av
     tagv     := tgv

     outbuf := ob


     // Read the parameters and open files.

     hdrstream,sourcestream,liststream,codestream:=0,0,0,0

     failed := no

     IF rdargs("PROG=FROM/A,CODE=TO,VER/K,LIST/S,*
               *SPACE/K,HDR/K",argv,argvupb) = 0 THEN
       $( writes("Invalid parameters*N")
          stop(return.hard)
       $)

     TEST argv!4 = 0 THEN
       stvecupb := stvdefsize
      ELSE
       stvecupb := stringtonum(argv!4,stvdefsize)

     stvec := getvec(stvecupb + tagtablesize)
     stvecp := stvec + stvecupb + 1

     IF stvec = 0 THEN
       $( writes("Insufficient heap for symbol table*N")
          stop(return.hard)
       $)

     tagtable := stvecp
     FOR j = 0 TO tagtablesize - 1 DO
       tagtable ! j := 0

     sourcestream := findinput(argv!0)

     checkopen(sourcestream,argv!0,"PROG")

     IF argv!5 \= 0 THEN
       $( hdrstream := findinput(argv!5)
          checkopen(hdrstream,argv!5,"HDR")
       $)


     listing := argv!3 \= 0

     IF argv!2 \= 0 THEN
       $( liststream := findoutput(argv!2)
          checkopen(liststream,argv!2,"VER")
       $)

     IF argv!1 \= 0 THEN
       $( codestream := findoutput(argv!1)
          checkopen(codestream,argv!1,"CODE")
       $)

     IF failed THEN // One or more open fails
       tidy.up.on.fail()


     declsyswords()

     IF liststream \= 0 THEN
       selectoutput(liststream)

     writes("TRIPOS NOVA Assembler Version 2*E")

     firstpass()

     secondpass()

     stop(numerrs > 0 -> return.soft,0)

  $)

AND checkopen(stream,file,key) BE
  IF stream = 0 THEN
    $( writef("Can't open %S file *"%S*"*N",key,file)
       failed := yes
    $)


AND tidy.up.on.fail() BE
  $( freevec(stvec)
     IF hdrstream \= 0 THEN
       $( selectinput(hdrstream)
          endread()
       $)
     IF sourcestream \= 0 THEN
       $( selectinput(sourcestream)
          endread()
       $)
     IF liststream \= 0 THEN
       $( selectoutput(liststream)
          endwrite()
       $)
     IF codestream \= 0 THEN
       $( selectoutput(codestream)
          endwrite()
       $)
     stop(return.hard)
  $)







AND firstpass() BE
  $( absmin, relmin := maxint,maxint
     absmax, relmax := 0,0
     absrp,  relrp  := 0,0
     absloc, relloc := 0,0

     locmode, relp := s.abs, absrp
     minloc, maxloc, location := absmin,absmax,absloc

     charpos := 1
     ended := no; pass1 := yes; pass2 := no
     radix, packlr, nonull := 8, no, no

     inhdr := no

     title.not.read := yes

     TEST argv!5 \= 0 THEN
       $( inhdr := yes
          selectinput(hdrstream)
       $)
      ELSE
       selectinput(sourcestream)

     UNTIL ended DO
       $( outbufp := 22
          errors.found := no
          doline()
          IF testflags(1) THEN
            $( selectoutput(sysout)
               writes("*N****BREAK - no code produced*N")
               freevectors()
               tidy.up.on.fail()
            $)
       $)

     // Force saving of final location values

     changemode((locmode = s.abs -> s.rel, s.abs))

     IF absmax = 0 THEN absmin := 0

     writes("*N*N")

     endread()

     IF inhdr THEN
       $( selectinput(sourcestream)
          endread()
       $)

     sourcestream, hdrstream := 0, 0

  $)



AND secondpass() BE
  $( // Get the code vectors

     avec, relvec, absrvec, relrvec := 0,0,0,0
     avec    := gvec(absmax - absmin - 1)
     absvec  := avec - absmin
     absrvec := gvec(absrp - 1)
     relvec  := gvec(relmax - 1)
     relrvec := gvec(relrp - 1)

     // Clear the absolute vector.
     // This is for SYSLINK.

     FOR j = absmin TO absmax - 1 DO
       absvec ! j := 0

     absmin, relmin := maxint,maxint
     absmax, relmax := 0,0
     absrp,  relrp  := 0,0
     absloc, relloc := 0,0

     locmode := s.abs

     charpos := 1
     codevec, relocvec, relp := absvec, absrvec, absrp
     minloc, maxloc, location := absmin,absmax,absloc
     ended := no; pass1 := no
     radix, packlr, nonull, pass2 := 8,no,no,yes

     clearbits()

     sourcestream := findinput(argv!0)

     TEST argv!5 \= 0 THEN
       $( inhdr := yes
          hdrstream := findinput(argv!5)
          selectinput(hdrstream)
       $)
      ELSE
       $( inhdr := no
          hdrstream := 0
          selectinput(sourcestream)
       $)

     numerrs := 0

     title.not.read := yes

     UNTIL ended DO
       $( outbufp := 22; nerrs := 0
          prstart, prend := 0, -1
          errors.found := no

          IF testflags(1) THEN
            $( selectoutput(sysout)
               writes("*N****BREAK - no code produced*N")
               freevectors()
               tidy.up.on.fail()
            $)

          FOR j = 0 TO outbuflim DO
            putbyte(outbuf,j,' ')

          doline()

          IF errors.found THEN
            numerrs := numerrs + 1

          UNLESS listing | nerrs > 0 THEN
            LOOP

          FOR j = 0 TO outbufp - 1 DO
            wrch(getbyte(outbuf,j))

          IF listing THEN
            FOR p = prstart TO prend DO
              writef("              %O6*N",codevec!p)
       $)

     endread()

     IF inhdr THEN
       $( selectinput(sourcestream)
          endread()
       $)

     // Output final messages.
     changemode(locmode = s.abs -> s.rel,s.abs)

     TEST numerrs > 0 THEN
       writef("*N*N%N line(s) with errors*N",numerrs)
      ELSE
       writes("*N*NNo errors detected*N")

     reportundefs()

     IF absmax = 0 THEN
       absmin := 0

     writef("*N*NAssembly statistics (words): *N*N*
             *           Absolute   Relocatable*N*N*
             *Code        %I5       %I5*N*
             *Reloc. info %I5       %I5*N",
         absmax - absmin, relmax,absrp,relrp)

     writef("*N%N words of symbol table were used*N",
         stvec+stvecupb+1-stvecp)


       writes("*N*NAssembly complete*N")

     // Close listing stream:

     IF liststream \= 0 THEN
       endwrite()

     IF argv!1 \= 0 THEN
       outcode()

     // Free the code vectors

     freevectors()

     freevec(stvec)

  $)


AND gvec(u) = VALOF
  $( IF u < 0 THEN
       RESULTIS 0
     $( LET v = getvec(u)
        IF v = 0 THEN
          error("Insufficient store for vectors*N")
        RESULTIS v
     $)
  $)



AND freevectors() BE
  $( IF avec \= 0 THEN
       freevec(avec)
     IF absrvec \= 0 THEN
       freevec(absrvec)
     IF relvec \= 0 THEN
       freevec(relvec)
     IF relrvec \= 0 THEN
       freevec(relrvec)
  $)



AND complain(code) BE
  $( errors.found := yes
     warn(code)
  $)



AND warn(code) BE
  $( UNLESS nerrs >= 5 | pass1 THEN
       $( FOR i = 0 TO 4 DO
            IF code = getbyte(outbuf,i) THEN
              RETURN
          putbyte(outbuf,nerrs,code)
          nerrs := nerrs + 1
       $)
  $)



AND doline() BE
  $( // This routine handles a complete line of source.
     // First the labels are set, and then READREST is
     // called, either for an equivalence line, or for
     // an ordinary line. (Directive, instruction or
     // data word).
     indirect, noload := no, no
     setlabels()
     TEST ch = '=' THEN // Equivalence line
       $( LET s,st = symb,symbtype
          rch(); readsymb(); readrest(no)
          setsymb(s,st,expval,exptype)
       $)
      ELSE
       $( readrest(yes)
          IF (exptype = s.abs) | (exptype = s.rel) THEN
            $( putloc()
               build(expval,exptype)
            $)
       $)
     IF printres THEN  // Value needed in listing
       putval(expval,exptype)

     // Read to end of line
     skiprest()
  $)


AND setlabels() BE // Sets labels on line
  WHILE VALOF $(
    rch(); readsymb(); skiplayout()
    RESULTIS ch = ':' $) DO
    setsymb(symb,symbtype,location,locmode)


AND skiplayout() BE
  $( // Skips over spaces, tabs, commas and comments
     //  Sets INDIRECT or NOLOAD if @ or # are found.
     WHILE (ch = '*S') | (ch = '*T') | (ch = ',') |
       (ch = '@') | (ch = '#') DO
       $( IF ch = '@' THEN indirect := yes
          IF ch = '#' THEN noload   := yes
          rch()
       $)
     IF ch = ';' THEN // Skip comment
       skiprest()
  $)


AND skip.operand() BE
  UNTIL ch = ' ' | ch = '*N' | ch = ',' |
        ch = ';' | ch = '*T' | ch = '#' | ch = '@' DO
    rch()


AND skiprest() BE
  UNTIL (ch = '*N') DO rch()



AND setsymb(s,st,v,t) BE
  $( IF (st \= s.abs) & (st \= s.rel) &
        (st \= s.new)  & (st \= s.ext) THEN
       $( complain('Q')
          RETURN
       $)
     IF (t \= s.abs) & (t \= s.rel) & (t \= s.ext) THEN
       $( complain('Q')
          t := s.abs; v := 0
       $)
     TEST (st.type ! s & stb.muldef) > 0 THEN
       complain('M')
      ELSE
       TEST (st.type ! s & stb.setnow) > 0 THEN
         st.type ! s := st.type ! s | stb.muldef
        ELSE
         TEST t = s.ext & (st.type!s & stb.ent) \= 0 THEN
           complain('I')
          ELSE
           $( st.type ! s := (st.type ! s & #177740) | t
              st.value ! s := v
              st.type ! s := st.type ! s | stb.setnow |
                stb.setever
           $)
  $)


AND readrest(frefs) BE
  $( // Read rest of line, which may be:
     //       Instruction
     //       Data word
     //       Directive
     //       Empty
     // The routine is entered with the first symbol
     // described by SYMB and SYMBTYPE.
     LET s = symb



     printres := yes

     SWITCHON symbtype INTO

       $(
          CASE s.none:
            printres := no
            exptype := s.none
           RETURN

          CASE s.dir:
            dodir()
            ENDCASE

          CASE s.instr:
            doinstr(frefs)
            ENDCASE


          DEFAULT:
            readexp(frefs)
            IF errors.found THEN
              exptype := s.rel
            expval := expval + (indirect -> #100000,0)
            indirect := no
            readsymb()

       $)

     IF symbtype \= s.none THEN
       complain('Q')

     IF noload | indirect THEN
       complain('Q')

     RETURN


  $)





AND dodir() BE

  // Handle an assembler directive.
  //  The only directives recognised currently are:
  //     .LOC
  //     .ZREL
  //     .BLK
  //     .NREL
  //     .TXTN
  //     .TXTM
  //     .RDX
  //     .END
  //     .TXT
  //     .ENT
  //     .EXTN
  //     .TITL
  //
  // Others are flagged with Y as warnings.
  //
  $( LET t = st.value ! symb

     SWITCHON t INTO

       $( CASE d.loc:
            readsymb()
            readexp(no)
            changemode(exptype)
            setloc(expval)
            ENDCASE

          CASE d.zrel: // Fudged to be .LOC 50
            printres := no
            changemode(s.abs)
            setloc(40)
            expval := 40
            ENDCASE

          CASE d.blk:
            readsymb()
            readabsexp(no)
            setloc(location + expval)
            ENDCASE

          CASE d.nrel:
            printres := no
            changemode(s.rel)
            ENDCASE

          CASE d.txtn:
            readsymb()
            nonull := readabsexp(no) \= 0
            ENDCASE

          CASE d.txtm:
            readsymb()
            packlr := readabsexp(no) \= 0
            ENDCASE

          CASE d.rdx:
            radix := 10; readsymb()
            readabsexp(no)
            TEST 2 <= expval <= 10 THEN
              radix := expval
             ELSE
              complain('R')
            ENDCASE

          CASE d.end:
            ended := yes
            printres := no
            ENDCASE

          CASE d.txt:
            $( LET word, term, len = 0,0,0
               putloc()
               prstart := location + 1
               skiplayout()
               term := ch; rch()
               UNTIL (ch = term) | (ch = '*N') DO
                 $( LET c = readstrch()
                    len := len + 1
                    TEST (len REM 2) = 0 THEN
                      $( word := word + c
                         UNLESS packlr THEN
                           word := (word >> 8) +
                              ((word & 255) << 8)
                         build(word,s.abs)
                         word := 0
                      $)
                     ELSE
                      word := c << 8
                 $)
               UNLESS packlr THEN // Final word
                 word := (word>>8)+((word & 255)<<8)
               UNLESS nonull & (len REM 2 = 0) THEN
                 build(word,s.abs)
               prend := location - 1
               IF (len \= 0) & pass2 THEN
                 expval := codevec ! (prstart - 1)
            $)
            skiprest()
            ENDCASE

          CASE d.ent:
          CASE d.extn:
            // Read a list of names.
            $( readsymb()
               skiplayout()
               TEST t = d.extn THEN
                 setsymb(symb, symbtype, 0, s.ext)
                ELSE
                 TEST symbtype = s.ext THEN
                   complain('I')
                  ELSE
                   // Must check that the symbol is a tag
                   TEST symbtype=s.abs | symbtype=s.rel |
                        symbtype=s.new THEN
                     st.type!symb := st.type!symb | stb.ent
                    ELSE
                     complain('Q')
            $) REPEATUNTIL ch = '*N'

            ENDCASE

          CASE d.titl:
            TEST title.not.read THEN
              $( title.not.read := no
                 skiplayout()
                 TEST pass1 THEN
                   $( writes(" assembling ")
                      UNTIL ch = ' ' | ch = '*N' |
                            ch = ';' | ch = '*T' DO
                        $( wrch(ch)
                           rch()
                        $)
                      wrch('*E')
                   $)
                  ELSE
                   skiprest()
              $)
             ELSE
              $( skiprest()
                 warn('T')
              $)
            symbtype := s.none
            ENDCASE

          CASE d.list:
            readsymb()
            listing := readabsexp(no) = 0 & argv!3 \= 0
            ENDCASE

          CASE d.unimp:
            skiprest()
            printres := no
            warn('Y')
            symbtype := s.none
            ENDCASE

       $)

     readsymb()
     exptype := s.dir
  $)



AND doinstr(frefs) BE
  $( LET i = (st.type ! symb) >> 10
     LET v = st.value ! symb

     exptype := s.abs

     IF pass1 & frefs THEN
       // Only look at rest in PASS2
       RETURN

     readsymb()

     SWITCHON i INTO

       $( CASE i.mra:
            v := v + (readac(frefs) << 11)
            readsymb()

          CASE i.mr:
            v := v + readaddress(frefs)
            v := v + (indirect -> #2000,0)
            indirect := no
            ENDCASE

          CASE i.alc:
            v := v+(readac(frefs)<<13); readsymb()
            v := v+(readac(frefs)<<11); readsymb()
            IF symbtype = s.skpmn THEN
              $( v := v + st.value ! symb
                 readsymb()
              $)
            v := v + (noload -> #10,0)
            noload := no
            ENDCASE

          CASE i.ioa:
            v := v+(readac(frefs)<<11); readsymb()


          CASE i.io:
            v := v + readdev(frefs); readsymb()
            ENDCASE

          CASE i.ion:
            v := v+(readac(frefs)<<11); readsymb()
            ENDCASE

          CASE i.zop:
            ENDCASE

       $)

     exptype := s.abs
     expval := v
  $)



AND readaddress(frefs) = VALOF
  $( LET e = 0; readexp(frefs); e:=expval

     readsymb()

     IF exptype = s.ext THEN
       $( complain('A')
          skiprest()
          RESULTIS 0
       $)

     TEST symbtype = s.none THEN
       $( LET d = e - location
          TEST (exptype = s.abs) &
               ((NOT dotexp) |
                (d < -128) | (d > 127)) THEN
            $( IF 0 <= e <= 255 THEN
                 RESULTIS expval
               complain('A')
               RESULTIS 0
            $)
           ELSE
            $( IF locmode = s.abs & exptype = s.rel THEN
                 $( complain('A')
                    RESULTIS 0
                 $)
               IF -128 <= d < 128 THEN
                 RESULTIS ((1 << 8) + (d & 255))
               complain('A')
               RESULTIS 0
            $)
       $)
      ELSE
       $( LET ac = readac(frefs); readsymb()
          IF ((ac = 0) & (0 <= e <= 255)) |
             ((ac > 0) & (-128 <= e < 127)) THEN
             RESULTIS ((ac << 8) + (e & 255))
          complain('A')
          RESULTIS 0
       $)
  $)



AND readac(frefs) = VALOF
  $( LET ac = readabsexp(frefs)
     IF 0 <= ac <= 3 THEN
       RESULTIS ac
     complain('O')
     RESULTIS 0
  $)


AND readdev(frefs) = VALOF
  $( LET d = readabsexp(frefs)
     IF 0 <= d <= 63 THEN
       RESULTIS d
     complain('O')
     RESULTIS 0
  $)


AND readabsexp(frefs) = VALOF
  $( LET e = 0; readexp(frefs); e := expval
     IF exptype = s.abs THEN
       RESULTIS e
     complain('R')
     exptype := s.abs
     RESULTIS 0
  $)


AND readexp(frefs) BE
  $( LET et, edt, eval, op = 0, 0, 0, op.plus
     LET dotpos, unknown   = yes, no

     IF symbtype = s.monop THEN
       $( op := symb
          readsymb()
       $)

     $( LET sval,sdt,st = 0,0,0

        SWITCHON symbtype INTO

          $( CASE s.lbr:
               readsymb()
               readexp(frefs); sval := expval
               st := (exptype = s.rel -> 1,0)
               IF ch \= ')' THEN
                 $( complain('E')
                    GOTO experr
                 $)
               rch()
               GOTO elab

             CASE s.dot:
               sval := location
               st := (locmode = s.rel -> 1,0)
               sdt := 1
               GOTO elab

             CASE s.number:
               sval := symb
               GOTO elab

             CASE s.ext:
               st := 10000
               IF ~ frefs THEN
                 complain('F')
               external.symbol := symb
               GOTO elab

             CASE s.rel:
               st := 1
               GOTO elb2

             CASE s.new:
               unknown := yes

             CASE s.instr:
             CASE s.abs:
         elb2: sval := st.value ! symb
               UNLESS (st.type ! symb & stb.setnow) > 0 |
                 frefs THEN
                 complain('F')

         elab: SWITCHON op INTO
                 $( CASE op.plus:
                      eval := eval + sval
                      et   := et   + st
                      edt  := edt  + sdt
                      ENDCASE

                    CASE op.minus:
                      IF ABS st > 1 THEN
                        GOTO relerr
                      eval := eval - sval
                      et   := et   - st
                      edt  := edt  - sdt
                      ENDCASE

                    CASE op.times:
                      IF (st > 0) & (et > 0) THEN
                        GOTO relerr
                      eval := eval * sval
                      et   := st*eval + et*sval
                      edt  := sdt*eval + edt*sval
                      ENDCASE

                    CASE op.over:
                      IF (st>0) | (et REM sval\=0) THEN
                        GOTO relerr
                      eval := eval/sval
                      et := et/sval
                      dotpos := (edt REM sval=0) & dotpos
                      edt := edt/sval
                      ENDCASE

                    CASE op.or: CASE op.and:
                      IF (et > 0) | (st > 0) THEN
                        GOTO relerr
                      dotpos := dotpos & (edt = sdt = 0)
                      eval := (op = op.or -> eval | sval,
                                             eval & sval)
                      ENDCASE

                 $)
               ENDCASE

             DEFAULT:
               complain('E')
               GOTO experr

          $)

        op := chkop()

        TEST op > 0 THEN // Another operator
          $( rch()
             IF chkop() > 0 THEN // Two operators together
               $( warn('O')
                  symb,symbtype := 0,s.number
                  LOOP
               $)
             readsymb()
          $)
         ELSE // Not an operator: expression finished
          BREAK

     $) REPEAT

     IF (et \= 0) & (et \= 1) & (et \= 10000) THEN
       GOTO relerr

     dotexp  := (edt = 1) & dotpos
     exptype := (et = 1 | unknown -> s.rel,
                 et = 10000       -> s.ext, s.abs)
     expval := eval

     RETURN

relerr:
     complain('R')

experr:
     expval, exptype := 0, s.abs

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


AND readstrch() = VALOF
  $( LET r,c = radix,ch
     IF c = '<' THEN
       $( radix := 8
          rch(); readsymb()
          IF (symbtype \= s.number) |
             (ch       \= '>') THEN
            $( complain('S')
               radix := r
               skiprest()
               RESULTIS 0
            $)
          c := symb
          radix := r
       $)
     rch()
     RESULTIS c
  $)


AND stringtonum(string, def) = VALOF
$( LET n = 0
   FOR i = 1 TO string%0 DO
   $( LET ch = string%i
      UNLESS '0'<=ch<='9' RESULTIS def
      n := 10*n + ch-'0'
   $)
   RESULTIS n
$)
.


SECTION "NOVASM2"

GET ""

LET readsymb() BE
  $( // Reads symbol, setting SYMB and SYMBTYPE
     $( skiplayout()
        SWITCHON ch INTO

          $( CASE '*N': symbtype := s.none; RETURN


             CASE '0':CASE '1':CASE'2':CASE'3':CASE'4':
             CASE'5':CASE'6':CASE'7':CASE'8': CASE'9':
                symb := readnum()
                symbtype := s.number
                RETURN

              CASE '(':
                symbtype := s.lbr
                BREAK

              CASE '"':
                rch()
                symbtype := s.number
                TEST ch = '*N' THEN
                  symb := #15
                 ELSE
                  $( symb := ch
                     rch()
                  $)
                RETURN

              CASE '+': CASE '-':
                symbtype := s.monop
                symb := (ch = '+' -> op.plus,op.minus)
                BREAK

              CASE '.':
                rch()
                UNLESS ('A' <= ch <= 'Z') |
                       ('0' <= ch <= '9') |
                       (ch     =     '.') THEN
                  $( symbtype := s.dot
                     RETURN
                  $)
                unrch(); ch := '.'

              CASE'A':CASE'B':CASE'C':CASE'D':CASE'E':
              CASE'F':CASE'G':CASE'H':CASE'I':CASE'J':
              CASE'K':CASE'L':CASE'M':CASE'N':CASE'O':
              CASE'P':CASE'Q':CASE'R':CASE'S':CASE'T':
              CASE'U':CASE'V':CASE'W':CASE'X':CASE'Y':
              CASE 'Z':
                readtag()
                RETURN

              DEFAULT:
                complain('B')
                unrch(); pch('[')
                rch(); pch(']')
                rch()

           $)

        $) REPEAT

     rch()
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
            putbyte(tagv, len, ch)
            len := len + 1
            $)

        rch()
        $)

    // Pad out with spaces
    FOR k = len TO tagbyteupb DO putbyte(tagv, k, ' ')

    lookup()
    $)


AND readnum() = VALOF
  $( LET v = VEC 40
     LET n,r,p = 0,radix,-1

     $( p := p + 1
        putbyte(v,p,ch)
        rch()
     $) REPEATWHILE '0' <= ch <= '9'

     IF ch = '.' THEN
       $( r := 10; rch() $)

     FOR j = 0 TO p DO
       $( LET d = getbyte(v,j) - '0'
          TEST 0 <= d < r THEN
            n := n * r + d
           ELSE
            complain('N')
       $)

     RESULTIS n
  $)



AND changemode(mode) BE
  IF mode \= locmode THEN
    $( TEST locmode = s.abs THEN
         $( absmin := minloc
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
         $( relmin := minloc
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
  $( IF (newloc > locationlimit) | (newloc < 0) THEN
       error("Location out of range*N")
     IF newloc > maxloc THEN
       maxloc := newloc
     location := newloc
  $)



AND build(v,t) BE
  $( IF pass2 THEN
       $( codevec ! location := v
          IF t = s.rel THEN
            relocvec ! relp := location
          IF t = s.ext THEN
            // Add reference to external symbol
            $( LET v = newvec(2)
               v!0 := st.value ! external.symbol
               v!1 := location
               v!2 := locmode
               st.value ! external.symbol := v
            $)
       $)
     IF t = s.rel THEN
       relp := relp + 1
     IF location < minloc THEN
       minloc := location
     setloc(location + 1)
  $)



AND outcode() BE
  $( // Output the code.

     selectoutput(codestream)

     // First the relocatable HUNK.
     outword(t.hunk)
     outword(relmax)
     writewords(relvec,relmax)

     // Second the relocation info.
     IF relrp > 0 THEN
       $( outword(t.reloc)
          outword(relrp)
          writewords(relrvec,relrp)
       $)

     // Next the external sybmol information
     //  for the relocatable hunk.
     outext(s.rel)

     // Next the absolute HUNK.
     IF absmax \= 0 THEN
       $( outword(t.abshunk)
          outword(absmin)
          outword(absmax - absmin)

     writewords(absvec + absmin,absmax - absmin)
       $)

     // Next the ABSHUNK reloc. info
     IF absrp > 0 THEN
       $( outword(t.absrel)
          outword(absrp)
          writewords(absrvec,absrp)
       $)

     // Finally the EXTernal symbol information
     //  for the absolute hunk.
     outext(s.abs)


     outword(t.end)

     endwrite()

  $)


AND outword(n) BE writewords(@ n,1)


AND outext(reltype) BE
  // Output the external symbol information.
  // This information is of two types:
  //  1. Definition of external symbols
  //  2. References to external symbols
  // The first type is produced when reltype
  //  is s.rel, for symbols with both
  //  absolute and relocatable values.
  // The second type is produced when the
  //  reference type matches reltype.
  $( LET extout = no
     FOR j = 0 TO tagtablesize - 1 DO
       $( LET s = tagtable ! j
          UNTIL s = 0 DO
            $( LET t1 = st.type ! s
               LET t2 = t1 & st.type.mask
               TEST (t1 & stb.ent) \= 0 &
                    reltype = s.rel THEN
                 $( UNLESS extout THEN
                      $( outword(t.ext)
                         extout := yes
                      $)
                    outname(s, t2, yes, st.value ! s)
                 $)
                ELSE
                 IF t2 = s.ext THEN
                   $( LET ec = 0
                      FOR j = 0 TO 1 DO
                        $( LET l = st.value ! s
                           UNTIL l = 0 DO
                             $( IF l!2 = reltype THEN
                                  TEST j = 0 THEN
                                    ec := ec + 1
                                   ELSE
                                    outword(l!1)
                                l := !l
                             $)
                           IF (j = 0) & (ec \= 0) THEN
                             $( UNLESS extout THEN
                                  $( outword(t.ext)
                                     extout := yes
                                  $)
                                outname(s, 0, no, ec)
                             $)
                        $)
                   $)
               s := !s
            $)
       $)
     IF extout THEN
       outword(0)
  $)


AND outname(s, type, name, value) BE
  $( LET v = name -> (type = s.rel -> ext.entrel,
                                      ext.entabs
                     ), ext.ext
     FOR j = 0 TO 6 DO
       $( LET c = (j >= tagchars -> ' ', (s + 1) % j)
          v := (v << 8) + c
          IF (j & 1) = 0 THEN
            $( outword(v)
               v := 0
            $)
       $)
     outword(value)
  $)





AND rch() BE
  $( TEST charpos >= 73 THEN
       $( charpos := 0; skiprest() $)
      ELSE
       $( ch := rdch()
          IF ch = endstreamch THEN
            TEST inhdr THEN
              $( inhdr := no
                 endread()
                 hdrstream := 0
                 selectinput(sourcestream)
                 ch := rdch()
              $)
             ELSE
              $( ended := yes
                 ch := '*N'
              $)
          pch(ch)
          charpos := charpos + 1
       $)
     IF ch = '*N' THEN
       charpos := 1
  $)


AND unrch() BE
  $( unrdch()
     outbufp := outbufp - 1
  $)


AND putloc() BE
  $( putn(6,location)
     IF locmode = s.abs THEN
       putbyte(outbuf,12,'#')
  $)


AND putval(v,t) BE
  $( putn(14,v)
     IF t = s.rel THEN
       putbyte(outbuf,20,'*'')
     IF t = s.ext THEN
       putbyte(outbuf,20,'X')
  $)

AND putn(p,n) BE
  FOR j = p + 5 TO p BY -1 DO
    $( putbyte(outbuf,j,(n & 7) + '0')
       n := n >> 3
    $)


AND pch(ch) BE
  TEST outbufp > outbuflim THEN
    warn('L')
   ELSE
    $( putbyte(outbuf,outbufp,ch)
       outbufp := outbufp + 1
    $)

AND declare(words) BE
    $(
    // Takes the words separated by '/' from the
    // string WORDS, and creates symbol table entries
    // for them.
    // A null word marks the end of WORDS.
    // The values and types are taken from VALPNTR
    // and TYPEPNTR.

    LET i, length = 1, 0

        $(  // Main loop
        LET ch = getbyte(words, i)

        TEST ch = '/'
        THEN
            $(  // Have read a complete word
            IF length = 0 THEN RETURN

            // Fill rest of tag area with spaces
            FOR k = length TO tagbyteupb
            DO putbyte(tagv, k, ' ')

            lookup()
            symb!st.value := !valpntr
            valpntr := valpntr + 1
            symb!st.type := !typepntr | stb.setever |
                                        stb.setnow
            typepntr := typepntr + 1
            length := 0
            $)
        ELSE
            $(  // Read next character, trusting that no
                // word is longer than TAGCHARS chars
            putbyte(tagv, length, ch)
            length := length + 1
            $)

        i := i + 1
        $) REPEAT
    $)

AND lookup() BE
    $(
    // Looks up the tag in TAGV in the symbol table.
    // If it is not already there, a new entry is
    // created, with type S.NEW and value zero.
    // Returns with SYMB pointing to the entry.

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
            $(  // Need to create a new entry
            IF pass2 THEN complain('U')
            symb := newvec(tagsize + 2)
            !symb := tagtable!hashval   // Link it in
            tagtable!hashval := symb

            // And copy in the tag
            FOR k = 0 TO tagsize - 1
            DO symb!(k + 1) := tagv!k

            // Set initial value and type
            symb!st.type := s.new |  stb.temp
            symb!st.value := 0
            $)
        ELSE
            IF (st.type ! symb & stb.setever) = 0 THEN
              IF pass2 THEN complain('U')
    symbtype := (symb ! st.type) & st.type.mask
    $)




AND newvec(n) = VALOF
    $(
    // Allocates a vector with upperbound N from STVEC.
    stvecp := stvecp - n - 1
    IF (stvecp - stvec) <= 0
    THEN error("Not enough room for symbol table*N")

    RESULTIS stvecp
    $)




AND reportundefs() BE
  $( // Print list of undefined symbols
     LET messnotout, numonline = yes,0

     FOR j = 0 TO tagtablesize - 1 DO
       $( LET p = tagtable ! j
          UNTIL p = 0 DO
            $( IF (st.type!p & st.type.mask) = s.new THEN
                 $( IF messnotout THEN
                      $( messnotout := no
                         writes("*N*NThe following symbol*
                            *(s) are undefined:*N*N")
                      $)
                    writes("   ")
                    FOR j = 0 TO tagchars - 1 DO
                      wrch(getbyte(@ p ! 1,j))
                    numonline :=numonline + 1
                    IF numonline >= 8 THEN
                      $( newline()
                         numonline := 0
                      $)
                 $)
               p := !p
            $)
       $)
     IF numonline \= 0 THEN newline()
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


AND error(b) BE
  $( writes("*N*N")
     writes(b)
     freevectors()
     tidy.up.on.fail()
  $)



AND declsyswords() BE
  $(  // Ditto.........



   valpntr := TABLE #020000, #040000,
                    #010000, #014000, #000000, #004000,

                    #102000, #102100, #102200, #102300,
                    #102060, #102160, #102260, #102360,
                    #102040, #102140, #102240, #102340,
                    #102020, #102120, #102220, #102320,

                    #103000, #103100, #103200, #103300,
                    #103060, #103160, #103260, #103360,
                    #103040, #103140, #103240, #103340,
                    #103020, #103120, #103220, #103320,

                    #103400, #103500, #103600, #103700,
                    #103460, #103560, #103660, #103760,
                    #103440, #103540, #103640, #103740,
                    #103420, #103520, #103620, #103720,

                    #100000, #100100, #100200, #100300,
                    #100060, #100160, #100260, #100360,
                    #100040, #100140, #100240, #100340,
                    #100020, #100120, #100220, #100320,

                    #101400, #101500, #101600, #101700,
                    #101460, #101560, #101660, #101760,
                    #101440, #101540, #101640, #101740,
                    #101420, #101520, #101620, #101720,

                    #101000, #101100, #101200, #101300,
                    #101060, #101160, #101260, #101360,
                    #101040, #101140, #101240, #101340,
                    #101020, #101120, #101220, #101320,

                    #100400, #100500, #100600, #100700,
                    #100460, #100560, #100660, #100760,
                    #100440, #100540, #100640, #100740,
                    #100420, #100520, #100620, #100720,

                    #102400, #102500, #102600, #102700,
                    #102460, #102560, #102660, #102760,
                    #102440, #102540, #102640, #102740,
                    #102420, #102520, #102620, #102720,

                    #060400, #060600, #060700, #060500,
                    #061000, #061200, #061300, #061100,
                    #061400, #061600, #061700, #061500,
                    #062000, #062200, #062300, #062100,
                    #062400, #062600, #062700, #062500,
                    #063000, #063200, #063300, #063100,

                    #060000, #060200, #060300, #060100,
                    #063500, #063400, #063700, #063600,
                    #060477, #061477, #062077,
                    #062677, #060177, #060277, #063077,
                    #073301, #073101,

                    // Directive values.

                    d.blk,   d.unimp, d.unimp, d.unimp,
                    d.unimp, d.unimp, d.unimp, d.unimp,
                    d.unimp, d.unimp, d.end,   d.unimp,
                    d.ent,   d.unimp, d.unimp, d.unimp,
                    d.extn,  d.unimp, d.unimp, d.unimp,
                    d.unimp, d.unimp, d.unimp, d.unimp,
                    d.loc,   d.nrel,  d.rdx,   d.titl,
                    d.txt,   d.unimp, d.unimp, d.unimp,
                    d.txtm,  d.txtn,  d.unimp, d.unimp,
                    d.zrel,  d.list,

                    // Device codes

                    #01, #10, #11, #12, #13, #14, #15,
                    #16, #17, #20, #21, #22, #23, #24,
                    #30, #31, #32, #33, #50, #51, #52,
                    #53, #57, #60, #62, #70, #73, #77,

                    // Skip mnemonics

                    1, 2, 3, 4, 5, 6, 7

   typepntr := TABLE sti.mra, sti.mra,
                     sti.mr,  sti.mr,  sti.mr,  sti.mr,

                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,

                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,

                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,

                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,

                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,

                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,

                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,

                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,
                     sti.alc, sti.alc, sti.alc, sti.alc,

                     sti.ioa, sti.ioa, sti.ioa, sti.ioa,
                     sti.ioa, sti.ioa, sti.ioa, sti.ioa,
                     sti.ioa, sti.ioa, sti.ioa, sti.ioa,
                     sti.ioa, sti.ioa, sti.ioa, sti.ioa,
                     sti.ioa, sti.ioa, sti.ioa, sti.ioa,
                     sti.ioa, sti.ioa, sti.ioa, sti.ioa,

                     sti.io,  sti.io,  sti.io,  sti.io,
                     sti.io,  sti.io,  sti.io,  sti.io,
                     sti.ion, sti.ion, sti.ion,
                     sti.zop, sti.zop, sti.zop, sti.zop,
                     sti.zop, sti.zop,

                     // Directives (38 - I hope)

                     s.dir,   s.dir,   s.dir,   s.dir,
                     s.dir,   s.dir,   s.dir,   s.dir,
                     s.dir,   s.dir,   s.dir,   s.dir,
                     s.dir,   s.dir,   s.dir,   s.dir,
                     s.dir,   s.dir,   s.dir,   s.dir,
                     s.dir,   s.dir,   s.dir,   s.dir,
                     s.dir,   s.dir,   s.dir,   s.dir,
                     s.dir,   s.dir,   s.dir,   s.dir,
                     s.dir,   s.dir,   s.dir,   s.dir,
                     s.dir,   s.dir,

                     // Devices (absolute symbols)

                     s.abs,   s.abs,   s.abs,   s.abs,
                     s.abs,   s.abs,   s.abs,   s.abs,
                     s.abs,   s.abs,   s.abs,   s.abs,
                     s.abs,   s.abs,   s.abs,   s.abs,
                     s.abs,   s.abs,   s.abs,   s.abs,
                     s.abs,   s.abs,   s.abs,   s.abs,
                     s.abs,   s.abs,   s.abs,   s.abs,

                     // Skip mnemonics for ALC's

                     s.skpmn, s.skpmn, s.skpmn, s.skpmn,
                     s.skpmn, s.skpmn, s.skpmn, s.skpmn


   declare("LDA/STA/ISZ/DSZ/JMP/JSR//")

   declare("ADC/ADCL/ADCR/ADCS/*
           *ADCC/ADCCL/ADCCR/ADCCS/*
           *ADCO/ADCOL/ADCOR/ADCOS/*
           *ADCZ/ADCZL/ADCZR/ADCZS/*
           *ADD/ADDL/ADDR/ADDS/*
           *ADDC/ADDCL/ADDCR/ADDCS/*
           *ADDO/ADDOL/ADDOR/ADDOS/*
           *ADDZ/ADDZL/ADDZR/ADDZS//")

   declare("AND/ANDL/ANDR/ANDS/*
           *ANDC/ANDCL/ANDCR/ANDCS/*
           *ANDO/ANDOL/ANDOR/ANDOS/*
           *ANDZ/ANDZL/ANDZR/ANDZS/*
           *COM/COML/COMR/COMS/*
           *COMC/COMCL/COMCR/COMCS/*
           *COMO/COMOL/COMOR/COMOS/*
           *COMZ/COMZL/COMZR/COMZS//")

   declare("INC/INCL/INCR/INCS/*
           *INCC/INCCL/INCCR/INCCS/*
           *INCO/INCOL/INCOR/INCOS/*
           *INCZ/INCZL/INCZR/INCZS/*
           *MOV/MOVL/MOVR/MOVS/*
           *MOVC/MOVCL/MOVCR/MOVCS/*
           *MOVO/MOVOL/MOVOR/MOVOS/*
           *MOVZ/MOVZL/MOVZR/MOVZS//")

   declare("NEG/NEGL/NEGR/NEGS/*
           *NEGC/NEGCL/NEGCR/NEGCS/*
           *NEGO/NEGOL/NEGOR/NEGOS/*
           *NEGZ/NEGZL/NEGZR/NEGZS/*
           *SUB/SUBL/SUBR/SUBS/*
           *SUBC/SUBCL/SUBCR/SUBCS/*
           *SUBO/SUBOL/SUBOR/SUBOS/*
           *SUBZ/SUBZL/SUBZR/SUBZS//")


   declare("DIA/DIAC/DIAP/DIAS/*
           *DOA/DOAC/DOAP/DOAS/*
           *DIB/DIBC/DIBP/DIBS/*
           *DOB/DOBC/DOBP/DOBS/*
           *DIC/DICC/DICP/DICS/*
           *DOC/DOCC/DOCP/DOCS//")

   declare("NIO/NIOC/NIOP/NIOS/*
           *SKPBZ/SKPBN/SKPDZ/SKPDN/*
           *READS/INTA/MSKO/*
           *IORST/INTEN/INTDS/HALT/*
           *MUL/DIV//")


   // Assembler directives: most are ignored.

   declare(".BLK/.COMM/.CSIZ/.DALC/.DIAC/*
           *.DIO/.DIOA/.DMR/.DMRA/.DUSR/*
           *.END/.ENDC/.ENT/.ENTO/.EOT/*
           *.EXTD/.EXTN/.EXTU/.GADD/.GLOC/*
           *.IFE/.IFG/.IFL/.IFN/.LOC/.NREL//")

   declare(".RDX/.TITL/.TXT/.TXTE/.TXTF/*
           *.TXTF/.TXTM/.TXTN/.TXTO/.XPNG/*
           *.ZREL/.LIST//")


   // Device codes: initially only a few.

   declare("MDV/TTI/TTO/PTR/PTP/RTC/PLT/CDR/*
           *LPT/DSK/ADCV/MTA/DACV/DCM/QTY/*
           *IBM1/IBM2/DKP/TTI1/TTO1/PTR1/PTP1/*
           *LPT1/DSK1/MTA1/QTY1/DKP1/CPU//")


   // Skip mnemonics for ALC instructions

   declare("SKP/SZC/SNC/SZR/SNR/SEZ/SBN//")
  $)


