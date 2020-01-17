// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

GET "LIBHDR"
GET "CLIHDR"

GLOBAL $(

absloc               :     150
absmin               :     151
absmax               :     152
absvec               :     153
absrp                :     154
absrvec              :     155
avec                 :     156

relloc               :     160
relmin               :     161
relmax               :     162
relvec               :     163
relrp                :     164
relrvec              :     165

location             :     170
minloc               :     171
maxloc               :     172
codevec              :     173
relp                 :     174
relocvec             :     175
locmode              :     176

label                :     177
labeltype            :     178

symb                 :     180
symbtype             :     181
expval               :     182
exptype              :     183
tagv                 :     184
ch                   :     185
tagtable             :     186
stvec                :     187
stvecp               :     188

external.symbol      :     189

outbuf               :     190
outbufp              :     191
nerrs                :     192
flagbuf              :     193
charpos              :     194

ended                :     195
pass1                :     196
pass2                :     197

prstart              :     198
prend                :     199

in.section           :     202
psect.read           :     203

valpntr              :     205
typepntr             :     206

errors.found         :     207
numerrs              :     208

argv                 :     210
sourcestream         :     211
liststream           :     212
codestream           :     213
listing              :     214
failed               :     215
line.number          :     216
sysout               :     217

operand.found        :     220
indirect             :     222

title.not.read.yet   :     239
stvecupb             :     241
inhdr                :     242
hdrstream            :     243

checkopen            :     250
firstpass            :     251
secondpass           :     252
tidy.up.on.fail      :     253
gvec                 :     254
freevectors          :     255
complain             :     256
warn                 :     257
doline               :     258
setlabel             :     259
skiplayout           :     260
skipfield            :     261
skip.operand         :     262
skiprest             :     263
setsymb              :     264
readrest             :     265
dodir                :     266
doinstr              :     267
generate.mr          :     268
generate.skip        :     269
generate.mrx         :     270
read.operand         :     271
read.abs.operand     :     272
read.register        :     273
read.address         :     274
operands.left        :     275
check.section        :     276
readabsexp           :     277
readexp              :     278
chkop                :     279
readsymb             :     280
readtag              :     281
readnum              :     282
digval               :     283
changemode           :     284
setloc               :     285
build                :     286
outcode              :     287
outword              :     288
rch                  :     289
unrch                :     290
putloc               :     291
putval               :     292
putdec               :     293
putn                 :     294
digit                :     295
pch                  :     296
declare              :     297
lookup               :     298
newvec               :     299
reportundefs         :     300
clearbits            :     301
error                :     302
declsyswords         :     303
stringtonum          :     304
readstring           :     305

       $)


MANIFEST
$(

// Basic constants

stvdefsize   =   3000
tagtablesize =    200
argvupb      =     50
outbuflim    =    120
flagsvecupb  =     50
flagsmax     =    (flagsvecupb + 1) * bytesperword

// Symbol types

s.abs   =  1     // Tag with absolute value
s.dir   =  2     // Assembler directive
s.dollar=  3     // Location counter symbol
s.dbase =  4     // $$ symbol.
s.instr =  5     // Instruction mnemonic
s.new   =  6     // Unset tag
s.none  =  7     // No symbol found before end of line
s.rel   =  8     // Tag with relocatable value
s.lbr   =  9     // Left parenthesis
s.number= 10     // Number (123 or X'345', say)
s.monop = 11     // Monadic operator
s.drel  = 12     // Value relative to $$
s.ext   = 13     // External reference


// Operators

op.plus   = 1
op.minus  = 2
op.times  = 3
op.over   = 4
op.and    = 5
op.or     = 6
op.xor    = 7
op.shift  = 8

// Instruction types

i.zop      =  1   // No operands
i.n        =  2   // 4-bit argument
i.mpydiv   =  3   // Optional 4-bit argument
i.sh       =  4   // Right shift (R,n)
i.s        =  5   // Stack
i.sn       =  6   // Stack and integer
i.sorsn    =  7   // Stack and optional integer
i.rr       =  8   // Register-register
i.r        =  9   // Single register
i.rlit     = 10   // Register-constant
i.io       = 11   // IO (R,devadrs)
i.rtniv    = 12   // RTNIV
i.skip     = 13   // Conditional jump
i.mr       = 14   // Memory reference
i.rmrx     = 15   // MRX with register
i.mrx      = 16   // MRX without register
i.cdea     = 17   // CDEA
i.cea      = 18   // CEA
i.arg      = 19   // ARG

// Symbol table types for instructions

sti.zop    = [i.zop    << 10] + s.instr
sti.n      = [i.n      << 10] + s.instr
sti.mpydiv = [i.mpydiv << 10] + s.instr
sti.sh     = [i.sh     << 10] + s.instr
sti.s      = [i.s      << 10] + s.instr
sti.sn     = [i.sn     << 10] + s.instr
sti.sorsn  = [i.sorsn  << 10] + s.instr
sti.rr     = [i.rr     << 10] + s.instr
sti.r      = [i.r      << 10] + s.instr
sti.rlit   = [i.rlit   << 10] + s.instr
sti.io     = [i.io     << 10] + s.instr
sti.rtniv  = [i.rtniv  << 10] + s.instr
sti.skip   = [i.skip   << 10] + s.instr
sti.mr     = [i.mr     << 10] + s.instr
sti.rmrx   = [i.rmrx   << 10] + s.instr
sti.mrx    = [i.mrx    << 10] + s.instr
sti.cdea   = [i.cdea   << 10] + s.instr
sti.cea    = [i.cea    << 10] + s.instr
sti.arg    = [i.arg    << 10] + s.instr

// Directives
// The values contain a #X100 bit if the value of a
//  label should NOT take on the value of the location.

d.asect   =   1 + #X100
d.dc      =   2
d.def     =   3
d.ds      =   4
d.end     =   5
d.equ     =   6 + #X100
d.org     =   7 + #X100
d.psect   =   8 + #X100
d.ref     =   9
d.text    =  10
d.title   =  11

d.null    =  19
d.unimp   =  20 + #X100

tagchars=  6     // Max. number of chars in a tag
tagsize = (tagchars + bytesperword - 1)/bytesperword
                 // Number of words needed for tag
tagbyteupb = (tagsize * bytesperword) - 1
                 // Last byte offset in TAGSIZE words
maxint  = 32767

yes     = TRUE
no      = FALSE


// Symbol table entry offsets

st.type  = tagsize + 1
st.value = tagsize + 2

// Symbol table type bits

stb.muldef = #400
stb.setnow = #200
stb.setever= #100
stb.def    =  #40
stb.temp   = #1000

st.type.mask = #37



// Object module identifiers.

t.hunk    = 1000
t.reloc   = 1001
t.end     = 1002
t.abshunk = 1003
t.absrel  = 1004
t.ext     = 1005

// External symbol type bytes

ext.namrel =    1
ext.namabs =    2
ext.ref    =  129


$)

.
SECTION "GASM"

GET "BCPL.GASM"

LET start() BE
  $( LET ob  = VEC 60
     LET av  = VEC argvupb
     LET tgv = VEC tagsize
     LET fgv = VEC flagsvecupb


     sysout   := output()

     argv     := av
     tagv     := tgv

     outbuf   := ob
     flagbuf  := fgv


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

     writes("TRIPOS GA/16 Assembler Version 1*E")

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

     title.not.read.yet := yes
     in.section         :=  no
     psect.read         :=  no
     inhdr              :=  no

     TEST argv!5 \= 0 THEN
       $( inhdr := yes
          selectinput(hdrstream)
       $)
      ELSE
       selectinput(sourcestream)

     UNTIL ended DO
       $( outbufp := 22; errors.found := no
          doline()
          IF testflags(1) THEN
            $( selectoutput(sysout)
               writes("*N****BREAK - no code produced*N")
               freevectors()
               tidy.up.on.fail()
            $)
       $)

     // Force the saving of the final location values

     changemode((locmode = s.abs -> s.rel,s.abs))

     IF absmax = 0 THEN
       absmin := 0

     wrch('*N')
     wrch('*N')

     endread()

     IF inhdr THEN
       $( selectinput(sourcestream)
          endread()
       $)

     sourcestream, hdrstream := 0, 0

  $)



AND secondpass() BE
  $( LET relsize, abssize = relmax, absmax - absmin

     numerrs := 0  // No. of lines with errors

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

     charpos     := 1
     line.number := 1

     codevec, relocvec, relp := absvec, absrvec, absrp
     minloc, maxloc, location := absmin,absmax,absloc
     ended := no; pass1 := no; pass2 := yes

     clearbits()

     title.not.read.yet := yes
     in.section         :=  no
     psect.read         :=  no

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

     UNTIL ended DO
       $( outbufp := 22; nerrs := 0
          prstart, prend := 0, -1
          errors.found := no

          FOR j = 0 TO outbuflim DO
            outbuf % j := ' '

          doline()

          putdec(17, line.number)
          line.number := line.number + 1

          IF ended THEN
            // Check for phase errors!
            // First save the final values again
            $( changemode(locmode = s.abs -> s.rel,s.abs)
               IF absmax = 0 THEN
                 absmin := 0
               IF relsize \= relmax |
                  abssize \= (absmax - absmin) THEN
                 complain('P')
            $)

          IF errors.found THEN
            numerrs := numerrs + 1

          IF listing | nerrs > 0 THEN
            FOR j = 0 TO outbufp - 1 DO
              wrch(getbyte(outbuf,j))

          IF listing THEN
            $( FOR p = prstart TO prend DO
                 $( LET fi = p - prstart
                    LET fl = fi<flagsmax->flagbuf%fi, ' '
                    writef("   %X4  %X4 %C*N",
                      p, codevec!p, fl)
                 $)

            $)

          IF testflags(1) THEN
            $( selectoutput(sysout)
               writes("*N****BREAK - no code produced*N")
               freevectors()
               tidy.up.on.fail()
            $)
       $)

     endread()

     IF inhdr THEN
       $( selectinput(sourcestream)
          endread()
       $)

     // Output final messages.

     TEST numerrs > 0 THEN
       writef("*N*N%N line(s) with errors*N",numerrs)
      ELSE
       writes("*N*NNo errors detected*N")

     reportundefs()

     writef("*N*NAssembly statistics (words): *N*N*
             *           Absolute   Relocatable*N*N*
             *Code        %I5       %I5*N*
             *Reloc. info %I5       %I5*N",
         absmax - absmin, relmax,absrp,relrp)

     writef("*N%N words of symbol table were used*N",
         stvec+stvecupb+1-stvecp)


     writes("*N*NAssembly complete*N")

     // Close listing stream:

     IF liststream THEN
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
  $( UNLESS nerrs >= 3 | pass1 THEN
       $( FOR i = 0 TO 2 DO
            IF code = getbyte(outbuf,i) THEN
              RETURN
          putbyte(outbuf,nerrs,code)
          nerrs := nerrs + 1
       $)
  $)



AND doline() BE
  //
  // Handles a complete line of code, of any sort.
  // First the label (if any) is read, and then
  // READREST is called.

  $( label    := 0     // Non-zero if there was a label

     operand.found := no

     rch()             // Get first character

     IF ch \= '**' & ch \= '*N' THEN

       $( TEST 'A' <= ch <= 'Z' THEN // Label
            $( readtag()
               label, labeltype := symb, symbtype
            $)
           ELSE
            IF ch \= ' ' THEN
              $( complain('L')
                 skipfield()
              $)
          readrest()   // Read rest of line

       $)

     skiprest()  // Skip over comment

  $)


AND setlabel() BE
  IF label \= 0 THEN
    setsymb(label,labeltype,location,locmode)


AND skipwithspace() BE
  // Does a 'skiplayout', but insists on at least one
  //  seperator. (space, *N or *T)
  TEST ch = ' ' | ch = '*N' | ch = '*T' THEN
    skiplayout()
   ELSE
    complain('C')


AND skipwithspacetoop() BE
  // As 'skipwithspace', but only allows 7
  //  or fewer spaces before comment is assumed.
  $( LET nsp = 0
     WHILE ch = ' ' | ch = '*T' DO
       $( nsp := nsp + 1
          rch()
       $)
     IF nsp > 7 THEN
       skiprest()
     IF nsp = 0 & ch \= '*N' THEN
       complain('C')
  $)


AND skiplayout() BE
  // Skips over space and tab.
  // Note that commas are significant (cf. Nova)
  WHILE (ch = ' ') | (ch = '*T') DO
    rch()


AND skipfield() BE
  // Skips to end of field (Label, operation, etc.)
  UNTIL (ch = ' ') | (ch = '*T') | (ch = '*N') DO
    rch()


AND skip.operand() BE
  // Skips to then end of an operand.
  UNTIL (ch = ' ') | (ch = ',') |
        (ch = '*N') | (ch = '*T') DO
    rch()


AND skiprest() BE
  // Skips to end of line, ignoring comments.
  UNTIL (ch = '*N') DO rch()



AND setsymb(s,st,v,t) BE
  $( IF (st \= s.abs) & (st \= s.rel) &
        (st \= s.new) & (st \= s.ext) THEN
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
         TEST t = s.ext & (st.type!s & stb.def) \= 0 THEN
           complain('I')
          ELSE
           $( st.type ! s := (st.type ! s & #177740) | t
              IF pass2 & v \= st.value ! s THEN
                complain('P')
              st.value ! s := v
              st.type ! s := st.type ! s | stb.setnow |
                stb.setever
           $)
  $)


AND readrest() BE
  $( // Reads rest of line, which may be:
     //       1. Instruction
     //       2. Directive
     //       3. Empty
     //
     skiplayout()
     readsymb()

     SWITCHON symbtype INTO

       $( CASE s.none:
            setlabel()
            ENDCASE

          CASE s.dir:
            dodir()
            ENDCASE

          DEFAULT:
            complain('C')
            build(#X0000,s.abs)
            ENDCASE

          CASE s.instr:
            doinstr()
            ENDCASE

       $)
  $)




AND dodir() BE
  //
  // Reads a directive. The symbol has already been read.
  // The directive type contains a bit #x100 if a label
  //  on the directive should NOT be set to the current
  //  location.
  //
  // The only directives handled are:
  //
  //     ASECT
  //     DC
  //     DEF
  //     DS
  //     END
  //     EQU
  //     ORG
  //     PSECT
  //     REF
  //     TEXT
  //     TITLE
  //
  // Others are flagged with a warning (Y), and ignored.

  $( LET t = st.value ! symb

     IF (t & #X100) = 0 THEN
       setlabel()

     skipwithspacetoop()

     SWITCHON t INTO

       $( CASE d.asect:
            //        ASECT  [abs. expn]
            in.section := yes
            expval    := 0
            IF operands.left() THEN
              readabsexp(no)
            UNLESS errors.found THEN
              $( changemode(s.abs)
                 setloc(expval)
              $)
            putloc()
            setlabel()
            ENDCASE

          CASE d.dc:
            //        DC  exp[,exp]*
            unrch()
            $( rch()
               readsymb()
               readexp(yes)
               IF errors.found THEN
                 exptype := s.rel
               build(expval,exptype)
            $) REPEATWHILE ch = ','
            ENDCASE

          CASE d.ds:
            //        DS  absexpn
            readabsexp(no)
            putloc()
            UNLESS errors.found THEN
              setloc(location + expval)
            ENDCASE

          CASE d.end:
            //        END
            ended := yes
            putloc()
            RETURN

          CASE d.equ:
            // LABEL  EQU  expn
            readsymb()
            readexp(no)
            TEST label = 0 THEN
              complain('A')
             ELSE
              setsymb(label, labeltype, expval, exptype)
            putval(expval, exptype)
            ENDCASE

          CASE d.org:
            //        ORG  expn
            readsymb()
            readexp(no)
            UNLESS errors.found THEN
              TEST locmode = exptype THEN
                setloc(expval)
               ELSE
                complain('R')
            setlabel()
            putloc()
            ENDCASE

          CASE d.psect:
            //        PSECT  [abs. expn]
            IF psect.read THEN
              $( complain('M')
                 RETURN
              $)
            in.section, psect.read := yes, yes
            expval,     exptype    :=   0, s.rel
            IF operands.left() THEN
              $( readabsexp(no)
                 IF errors.found THEN
                   expval, exptype := 0, s.rel
              $)
            changemode(exptype)
            setloc(expval)
            putloc()
            setlabel()
            ENDCASE

          CASE d.def:
          CASE d.ref:
             //        REF  name[,name']*
             //        DEF  name[,name']*
            unrch()
            $( rch(); readsymb()
               TEST t = d.ref THEN
                 setsymb(symb, symbtype, 0, s.ext)
                ELSE
                 TEST symbtype=s.new | symbtype=s.abs |
                      symbtype = s.rel THEN
                   st.type!symb := st.type!symb | stb.def
                  ELSE
                   TEST symbtype = s.ext THEN
                     complain('I')
                    ELSE
                     complain('Q')
            $) REPEATWHILE ch = ','
            ENDCASE

          CASE d.text:
            //        TEXT  'string'
            IF ch \= '*'' THEN
              $( complain('E')
                 RETURN
              $)
            readstring(build, yes)
            ENDCASE

          CASE d.title:
             //        TITLE  'string'
            IF ch \= '*'' THEN
              $( complain('E')
                 RETURN
              $)
            IF title.not.read.yet THEN
              $( title.not.read.yet := no
                 IF pass1 THEN
                   writes(" assembling *"")

                 rch()
                 UNTIL ch = '*'' | ch = '*N' DO
                   $( IF pass1 THEN wrch(ch)
                      rch()
                   $)
                 IF pass1 THEN writes("*"*E")
                 TEST ch = '*N' THEN
                   complain('E')
                  ELSE
                   rch()
              $)
            ENDCASE

          CASE d.unimp:
            warn('Y')
          CASE d.null:
            RETURN

       $)

     skipwithspace()

  $)


AND doinstr() BE
  //
  // Handles an instruction.
  // The mnemonic has already been read.
  //

  $( LET t = st.type  ! symb >> 10
     LET e = st.value ! symb

     skipwithspacetoop()

     setlabel()

     SWITCHON t INTO

       $( CASE i.zop:
            //        INH
            build(e, s.abs)
            RETURN

          CASE i.n:
          CASE i.mpydiv:
            //        WAIT  12
            $( LET n = 15
               IF operands.left() | t = i.n THEN
                 n := read.abs.operand(0, 15)
               build(e + n, s.abs)
               ENDCASE
            $)

          CASE i.sh:
            //        SRA   Z,3
            e := e + [read.register() << 5]
            e := e + read.abs.operand(1, 16) - 1
            build(e, s.abs)
            ENDCASE

          CASE i.s:
          CASE i.sn:
          CASE i.sorsn:
            //        RTN   1,345
            e := e + read.abs.operand(0, 1)
            TEST operands.left() & t = i.sorsn |
                 t = i.sn THEN
              $( read.operand()
                 build(e | #B100, s.abs)
                 build(expval, exptype)
              $)
             ELSE
              build(e, s.abs)
            ENDCASE

          CASE i.rr:
            //        ADDC  A,C
            e := e + [read.register() << 5]
            e := e + [read.register() << 8]
            build(e, s.abs)
            ENDCASE

          CASE i.r:
            //        ZERO  Z
            e := e + [read.register() << 5]
            build(e, s.abs)
            ENDCASE

          CASE i.rlit:
            //        ADDV  D,3456
            e := e + [read.register() << 5]
            read.operand()
            build(e, s.abs)
            build(expval, exptype)
            ENDCASE

          CASE i.io:
            //        DTOM  Z,X'34'
            e := e + [read.register() << 8]
            e := e + read.abs.operand(0, 63)
            build(e, s.abs)
            ENDCASE

          CASE i.rtniv:
            //        RTNIV addr
            read.operand()
            build(e,s.abs)
            build(expval, exptype)
            ENDCASE

          CASE i.skip:
            //        SKN   L23
            read.operand()
            generate.skip(e, expval, exptype)
            ENDCASE

          CASE i.mr:
            //        JSR   *$$+235
            $( LET mode, a, at = no, ?, ?
               read.address()
               a, at := expval, exptype
               IF operands.left() THEN
                 $( read.abs.operand(0, 1)
                    mode := yes
                 $)
               generate.mr(e, a, at, expval, mode)
               ENDCASE
            $)

          CASE i.cdea:
            //        CDEA  $$+2456
            read.operand()
            IF exptype \= s.drel THEN
              $( complain('R')
                 expval := 0
              $)
            IF (expval & #X8000) \= 0 THEN
              $( complain('A')
                 expval := 0
              $)
            expval := expval | #X8000
            build(e, s.abs)
            build(expval, s.abs)
            ENDCASE

          CASE i.cea:
            //        CEA   D,*34,X
            build(e, s.abs)
            e := #X4000

          CASE i.rmrx:
            //        LDR   C,30,Z
            e := e + [read.register() << 5]

          CASE i.mrx:
          CASE i.arg:
            //        LARS  *$$+23
            $( LET mode, a, at = no, ?, ?
               read.address()
               a, at := expval, exptype
               IF t = i.arg THEN
                 UNLESS operands.left() | indirect THEN
                   IF exptype = s.drel &
                      (expval & #X8000) = 0 THEN
                     // As for CDEA
                     $( build(expval | #X8000, s.abs)
                        ENDCASE
                     $)
               IF operands.left() THEN
                 e := e + [read.abs.operand(0, 3) << 8]
               IF operands.left() THEN
                 $( read.abs.operand(0, 3)
                    mode := yes
                 $)
               generate.mrx(e, a, at, expval, mode)
               ENDCASE
            $)

       $)

     skipwithspace()

  $)




AND read.register() = read.abs.operand(0, 7)


AND read.abs.operand(l, h) = VALOF
  //
  // Reads operand, which must be absolute and
  //  in the range l - h.
  //
  $( read.operand()
     IF exptype \= s.abs THEN
       $( complain('R')
          expval, exptype := l, s.abs
       $)
     UNLESS l <= expval <= h THEN
       $( complain('E')
          expval := l
       $)
     RESULTIS expval
  $)


AND read.address() BE
  //
  // Reads operand which may be address.
  // It checks for the indirect (*) symbol.
  //
  $( indirect := no
     IF ch = '**' THEN
       $( indirect := yes
          rch()
       $)
     read.operand()
  $)


AND read.operand() BE
  //
  // Reads directive or instruction operand.
  // Omitted operands are returned as 0.
  //
  $( expval, exptype := 0, s.abs
     IF ch = ' ' | ch = '*N' | ch = '*T' THEN
       UNLESS operand.found THEN
         $( complain('Q')
            RETURN
         $)
     operand.found := yes
     IF ch \= ',' THEN
       $( readsymb()
          readexp(yes)
       $)

     TEST ch = ',' THEN
       $( rch()
          operand.found := no
       $)
      ELSE
       IF ch \= ' ' & ch \= '*N' & ch \= '*T' THEN
         $( complain('Q')
            skip.operand()
            expval, exptype := 0, s.abs
         $)
  $)

AND operands.left() = ch \= ' ' & ch \= '*N' & ch \= '*T'


AND check.section() BE
  UNLESS in.section THEN
    $( complain('R')
       in.section := yes
       changemode(s.rel)
       setloc(0)
    $)




AND generate.skip(e, a, at) BE
  //
  // Generates conditional skip instruction.
  // The address referenced is (a, at).
  //
  $( TEST locmode \= at THEN
       complain('A')
      ELSE
       $( LET d = a - location
          TEST -255 <= d <= 256 THEN
            e := e + [(d - 1) & #X1FF]
           ELSE
            complain('A')
       $)
     build(e, s.abs)
  $)



AND generate.mr(e, a, at, m, m.given) BE
  //
  // Generate an MR instruction.
  // The address is (a, at).
  // If m.given is true, then the user supplied
  //  an addressing mode.
  //

  $( TEST m.given THEN

       // User supplied mode - address must be absolute
       TEST at \= s.abs THEN
         complain('R')
        ELSE
         TEST 0 <= a <= 1023 THEN
           $( e := e + a
              IF m = 1 THEN e := e + #X800
           $)
          ELSE
           complain('A')

      ELSE

       TEST at = locmode | at = s.drel THEN
         $( TEST at = locmode THEN
              a := (a - location + 511) NEQV 512
             ELSE
              e := e | #X800
            TEST 0 <= a <= 1023 THEN
              e := e + a
             ELSE
              complain('A')
         $)
        ELSE
         complain('R')

     IF indirect THEN e := e | #X400

     build(e, s.abs)
  $)


AND generate.mrx(e, a, at, m, m.given) BE
  //
  // Generates an MRX instruction.
  // The parameters are as for the preceding routine.
  //

  $( LET double = no

     TEST m.given THEN

       // User supplied mode.
       // The address can be relocatable (or external) for
       //  the two-word modes (2 and 3)

       $( double := m > 1
          TEST double THEN
            $( UNLESS at=s.rel | at=s.abs | at=s.ext THEN
                 $( complain('R')
                    a, at := 0, s.abs
                 $)
               e := e + 31
            $)
           ELSE
            TEST at = s.abs THEN
              TEST 0 <= a <= 31 THEN
                e := e + a
               ELSE
                complain('A')
             ELSE
              complain('R')
          IF m = 1 | m = 3 THEN e := e | #X800
       $)

      ELSE

       $( TEST at=s.rel | at=s.ext | a > 30 | a < 0 THEN
            $( double := yes
               e := e + 31
            $)
           ELSE
            e := e + a
          IF at = s.drel THEN
            $( at := s.abs
               e := e | #X800
            $)
       $)

     IF indirect THEN e := e | #X400

     build(e, s.abs)

     IF double THEN
       build(a, at)

  $)


.

SECTION "GASM2"

GET "BCPL.GASM"

LET readabsexp(frefs) BE
  // Called from 'dodir'.
  // Must read a symbol.
  $( readsymb(); readexp(frefs)
     IF exptype \= s.abs THEN
       $( complain('R')
          expval, exptype := 0, s.abs
       $)
  $)


AND readexp(frefs) BE
  $( LET et,eval,mop,op,unknown = 0,0,0,op.plus,no
     LET lastpri = 1841    // Antonin Dvorak !!
     LET drel    = no

     $( LET sval,st = 0,0

        SWITCHON symbtype INTO

          $( CASE s.lbr:
               readsymb()
               readexp(frefs)
               sval := expval
               st := (exptype = s.rel -> 1,0)
               IF ch \= ')' THEN
                 $( complain('E')
                    GOTO experr
                 $)
               rch()
               GOTO elab

             CASE s.monop:
               IF mop \= 0 THEN
                 $( complain('E')
                    GOTO experr
                 $)
               mop := symb
               readsymb()
               LOOP

             CASE s.dollar:
               check.section()
               sval := location
               st := (locmode = s.rel -> 1,0)
               GOTO elab

             CASE s.number:
               sval := symb
               GOTO elab

             CASE s.ext:
               st := 10000
               UNLESS frefs THEN
                 complain('F')
               external.symbol := symb
               GOTO elab

             CASE s.dbase:
               drel := yes
               st := 10000
               GOTO elab

             CASE s.rel:
               st := 1
               GOTO elb2

             CASE s.new:
               unknown := yes
             CASE s.abs:

         elb2: sval := st.value ! symb
               UNLESS (st.type ! symb & stb.setnow) > 0 |
                 frefs THEN
                 complain('F')

         elab: // Apply monadic operator
               SWITCHON mop INTO
                 $( CASE op.minus:
                      sval := - sval
                      st := - st
                      IF st \= 0 THEN GOTO relerr
                 $)
               mop := 0 // Reset

               SWITCHON op INTO
                 $( CASE op.plus:
                      eval := eval + sval
                      et   := et   + st
                      ENDCASE

                    CASE op.minus:
                      IF ABS st > 1 THEN
                        GOTO relerr
                      eval := eval - sval
                      et   := et   - st
                      ENDCASE

                    CASE op.times:
                      IF (st > 0) | (et > 0) THEN
                        GOTO relerr

                      eval := eval * sval
                      ENDCASE

                    CASE op.over:
                      IF (st > 0) | (et > 0) THEN
                        GOTO relerr
                      eval := eval/sval
                      ENDCASE

                    CASE op.or: CASE op.and: CASE op.xor:
                      IF (et > 0) | (st > 0) THEN
                        GOTO relerr
                      eval := (op=op.or->eval | sval,
                               op=op.xor->eval NEQV sval,
                                          eval & sval)
                      ENDCASE

                    CASE op.shift:
                      IF (st > 0) | (et > 0) THEN
                        GOTO relerr
                      eval := eval << sval
                      ENDCASE

                 $)
               ENDCASE

             DEFAULT:
               complain('E')
               GOTO experr

          $)

        op := chkop()

        TEST op > 0 THEN // Another operator
          $( LET pri = priorities(op)
             IF pri > lastpri THEN
               $( complain('E')
                  GOTO experr
               $)
             lastpri := pri
             rch()
             readsymb()
          $)
         ELSE // Not an operator: expression finished
          BREAK

     $) REPEAT

     IF (et \= 0) & (et \= 1) & (et \= 10000) THEN
       GOTO relerr

     exptype := (et = 1 | unknown -> s.rel,
                 et = 0           -> s.abs,
                 drel             -> s.drel, s.ext)
     expval := eval

     RETURN

relerr:

     complain('R')

experr:

     exptype, expval := s.abs, 0  // :0000
     skip.operand()

  $)


AND chkop() = VALOF
  SWITCHON ch INTO
    $( CASE '+': RESULTIS op.plus
       CASE '-': RESULTIS op.minus
       CASE '**':RESULTIS op.times
       CASE '/': RESULTIS op.over
       CASE '!': RESULTIS op.or
       CASE '&': RESULTIS op.and
       CASE '@': RESULTIS op.shift
       CASE '%': RESULTIS op.xor

       DEFAULT:  RESULTIS 0

    $)

AND priorities(op) = VALOF
  SWITCHON op INTO
    $( CASE op.or:    CASE op.xor:   RESULTIS 1
       CASE op.and:                  RESULTIS 2
       CASE op.plus:  CASE op.minus: RESULTIS 4
       CASE op.times: CASE op.over:  RESULTIS 5
       CASE op.shift:                RESULTIS 6
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



AND symbfromquote(word) BE symb := word

AND readsymb() BE
  $( // Reads symbol, setting SYMB and SYMBTYPE
     LET radix = 16
     LET savch = ch

     $( SWITCHON ch INTO

          $( CASE ' ':CASE '*N': symbtype:=s.none; RETURN

             CASE'0':CASE'1':CASE'2':CASE'3':CASE'4':
             CASE'5':CASE'6':CASE'7':CASE'8':CASE'9':
                symb := readnum(10)
                symbtype := s.number
                RETURN

              CASE '(':
                symbtype := s.lbr
                BREAK

              CASE '*'':
                readstring(symbfromquote,no)
                symbtype := s.number
                RETURN

              CASE '+':
                symbtype := s.monop
                symb := op.plus
                BREAK

              CASE '-':
                symbtype := s.monop
                symb := op.minus
                BREAK

              CASE '$':
                rch()
                IF ch = '$' THEN
                  $( symbtype := s.dbase
                     BREAK
                  $)
                symbtype := s.dollar
                RETURN

              CASE 'O': radix := 8
              CASE 'X':
                rch()
                IF ch = '*'' THEN
                  $( rch()
                     symbtype := s.number
                     symb := readnum(radix)
                     IF ch \= '*'' THEN
                       $( complain('N')
                          skip.operand()
                          symb := 0
                          RETURN
                       $)
                     BREAK
                  $)
                unrch()
                ch := savch

              CASE'A':CASE'B':CASE'C':CASE'D':CASE'E':
              CASE'F':CASE'G':CASE'H':CASE'I':CASE'J':
              CASE'K':CASE'L':CASE'M':CASE'N':
              CASE'P':CASE'Q':CASE'R':CASE'S':CASE'T':
              CASE'U':CASE'V':CASE'W':        CASE'Y':
              CASE'Z':
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
          (ch = '$')
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


AND readstring(routine,shift) = VALOF
  $( LET word, nc = 0, 0
     $( rch()
        IF ch = '*'' THEN
          $( rch()
             BREAK
          $)
        IF ch = '*N' THEN
          $( complain('E')
             BREAK
          $)
        IF ch = '$' THEN
          // Two hex digits expected
          $( LET d = 0
             FOR j = 1 TO 2 DO
               $( rch()
                  ch := digval(ch)
                  UNLESS 0 <= ch <= 15 THEN
                    $( unrch()
                       complain('E')
                       ch := 0
                       BREAK
                    $)
                  d := d * 16 + ch
               $)
             ch := d
          $)
        IF ((nc & 1) = 0) & (nc > 0) THEN
          routine(word,s.abs)
        nc := nc + 1
        word := [(word << 8) + ch] & #XFFFF
     $) REPEAT

     IF (nc & 1) \= 0 THEN
       IF shift THEN
         word := [(word << 8) + ' '] & #XFFFF
     routine(word,s.abs)

     RESULTIS nc
  $)


AND readnum(radix) = VALOF
  $( LET v = VEC 40
     LET n,p = 0,-1

     LET d = digval(ch)

     IF d < 0 THEN
       $( complain('N')
          RESULTIS 0
       $)

     $( p := p + 1
        v % p := d
        rch()
        d := digval(ch)
     $) REPEATWHILE d >= 0

     FOR j = 0 TO p DO
       $( LET d = v % j
          TEST 0 <= d < radix THEN
            n := n * radix + d
           ELSE
            complain('N')
       $)

     RESULTIS n
  $)


AND digval(c) = '0' <= c <= '9' -> c - '0',
                'A' <= c <= 'F' -> c - 'A' + 10, -1


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
  $( check.section()
     IF newloc > maxloc THEN
       maxloc := newloc
     location := newloc
  $)



AND build(v,t) BE
  $( check.section()
     IF t \= s.abs & t \= s.rel & t \= s.ext THEN
       $( complain('R')
          t := s.abs
       $)
     IF pass2 THEN
       $( TEST prstart = 0 THEN
            $( putloc()
               putval(v, t)
               prstart := location + 1
            $)
           ELSE
            $( LET fi = location - prstart
               IF fi < flagsmax THEN
                 flagbuf % fi := typeflag(t)
            $)
          prend := location
          codevec ! location := v
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
               TEST (t1 & stb.def) \= 0 &
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
  $( LET v = name -> (type = s.rel -> ext.namrel,
                                      ext.namabs
                     ), ext.ref
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
                 line.number := 1
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


AND putdec(p, n) BE
  $( LET v = VEC 3
     FOR j = 3 TO 0 BY -1 DO
       $( v!j := n REM 10
          n   := n  /  10
       $)
     FOR j = 0 TO 3 DO
       outbuf % [j+p] := digit(v!j)
  $)


AND putloc() BE
  putn(3,location)


AND putval(v, t) BE
  $( putn(9,v)
     outbuf % 14 := typeflag(t)
  $)



AND putn(p,n) BE
  FOR j = p + 3 TO p BY -1 DO
    $( outbuf % j := digit(n & #XF)
       n := n >> 4
    $)


AND typeflag(t) = t = s.rel -> 'P',
                  t = s.ext -> 'X', ' '


AND digit(d) = d <= 9 -> d + '0', d + 'A' - 10


AND pch(ch) BE
  IF outbufp <= outbuflim THEN
    $( outbuf % outbufp := ch
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
                LET t = ?
            IF length = 0 THEN RETURN

            // Fill rest of tag area with spaces
            FOR k = length TO tagbyteupb
            DO putbyte(tagv, k, ' ')

            lookup()
            symb!st.value := !valpntr
            t := !typepntr
            symb!st.type := t | stb.setever | stb.setnow
            valpntr := valpntr + 1
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
    IF (stvecp - stvec) <= 0 THEN
      error("Not enough room for symbol table*N")

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
  $( writes(b)
     freevectors()
     tidy.up.on.fail()
  $)



AND declsyswords() BE
  $(  // Ditto.........



   valpntr := TABLE

       /* MR     */ #X7000, #X6000, #X4000, #X5000,

       /* MRX    */ #XE000, #XF040, #XF000, #XF080,
                    #X8000, #XC000, #X3000, #XF0C0,
                    #XB000, #X9000, #XD000, #XA000,

       /* SKIP   */ #X2600, #X2400, #X2000, #X2800,
                    #X2E00, #X2200, #X2A00, #X2C00,

       /* RTNIV  */ #X0112,

       /* R-R    */ #X0819, #X0809, #X0817, #X0807,
                    #X081D, #X080D, #X0815, #X0816,
                    #X0806, #X0818, #X0808,

       /* R-LIT  */ #X0119, #X0109, #X0117, #X0107,
                    #X0115, #X011D, #X010D, #X0116,
                    #X0106, #X0118, #X0108,

       /* R      */ #X070B, #X0700, #X0702, #X0504,
                    #X0604, #X0502, #X070E, #X0610,
                    #X0501, #X0701, #X0503, #X0508,
                    #X0608, #X0510, #X0600, #X0602,
                    #X0601, #X0603, #X0703, #X0705,
                    #X0704, #X10BE, #X18BE,

       /* R SH   */ #X0210, #X0300, #X0310, #X0200,

       /* ZOP    */ #X0408, #X040C, #X0403, #X0402,
                    #X0420, #X0430, #X0440, #X0480,
                    #X0090, #X0091,

       /* n      */ #X0000, #X0010,

       /* IO     */ #X1000, #X1080, #X1880, #X1040,
                    #X1840, #X10C0,

       /* MPYDIV */ #X0080, #X00A0,

       /* Stack  */ #X0040, #X004E, #X0042, #X0054,
                    #X0022, #X0020, #X0028, #X0026,
                    #X002E, #X002A, #X0030, #X0032,
                    #X0034, #X0038, #X003C, #X003E,
                    #X005C, #X005E, #X0050, #X0052,
                    #X0058, #X005A,

       /* CEA..  */ #X0092, #X0092, #X4000,

       // Directive values

       d.null,  d.null,  d.null,  d.null,  d.title,
       d.null,  d.def,   d.ref,   d.unimp, d.unimp,
       d.psect, d.org,   d.asect, d.unimp, d.unimp,
       d.unimp, d.equ,   d.unimp, d.ds,    d.unimp,
       d.unimp, d.dc,    d.text,  d.unimp, d.unimp,
       d.end,   d.unimp,

       // Register mnemonics

       0, 1, 2, 3, 4, 5, 6, 7


   typepntr := TABLE

        sti.mr,     sti.mr,     sti.mr,     sti.mr,

        sti.rmrx,   sti.mrx,    sti.mrx,    sti.mrx,
        sti.rmrx,   sti.rmrx,   sti.rmrx,   sti.mrx,
        sti.rmrx,   sti.rmrx,   sti.rmrx,   sti.rmrx,

        sti.skip,   sti.skip,   sti.skip,   sti.skip,
        sti.skip,   sti.skip,   sti.skip,   sti.skip,

        sti.rtniv,

        sti.rr,     sti.rr,     sti.rr,     sti.rr,
        sti.rr,     sti.rr,     sti.rr,     sti.rr,
        sti.rr,     sti.rr,     sti.rr,

        sti.rlit,   sti.rlit,   sti.rlit,   sti.rlit,
        sti.rlit,   sti.rlit,   sti.rlit,   sti.rlit,
        sti.rlit,   sti.rlit,   sti.rlit,

        sti.r,      sti.r,      sti.r,      sti.r,
        sti.r,      sti.r,      sti.r,      sti.r,
        sti.r,      sti.r,      sti.r,      sti.r,
        sti.r,      sti.r,      sti.r,      sti.r,
        sti.r,      sti.r,      sti.r,      sti.r,
        sti.r,      sti.r,      sti.r,

        sti.sh,     sti.sh,     sti.sh,     sti.sh,

        sti.zop,    sti.zop,    sti.zop,    sti.zop,
        sti.zop,    sti.zop,    sti.zop,    sti.zop,
        sti.zop,    sti.zop,

        sti.n,      sti.n,

        sti.io,     sti.io,     sti.io,     sti.io,
        sti.io,     sti.io,

        sti.mpydiv, sti.mpydiv,

        sti.sorsn,  sti.sn,     sti.sorsn,  sti.s,
        sti.sorsn,  sti.s,      sti.s,      sti.s,
        sti.s,      sti.sorsn,  sti.s,      sti.s,
        sti.s,      sti.s,      sti.s,      sti.s,
        sti.s,      sti.s,      sti.s,      sti.s,
        sti.s,      sti.s,

        sti.cdea,   sti.cea,    sti.arg,

        // Directive types (27?)

        s.dir, s.dir, s.dir, s.dir, s.dir,
        s.dir, s.dir, s.dir, s.dir, s.dir,
        s.dir, s.dir, s.dir, s.dir, s.dir,
        s.dir, s.dir, s.dir, s.dir, s.dir,
        s.dir, s.dir, s.dir, s.dir, s.dir,
        s.dir, s.dir,

        s.abs, s.abs, s.abs, s.abs, s.abs,
        s.abs, s.abs, s.abs




   declare("JMP/JSR/LDA/STA//")

   declare("CMR/DECM/INCM/LARS/LDBY/LDR/*
           *RBIT/SARS/SBIT/STBY/STR/TBIT//")

   declare("SKM/SKN/SKOF/SKOT/SKP/SKR/SKS/SKZ//")

   declare("RTNIV//")

   declare("ADD/ADDC/AND/ANDC/OR/ORC/*
           *RTR/SUB/SUBC/XOR/XORC//")

   declare("ADDV/ADDVC/ANDV/ANDVC/LDV/*
           *ORV/ORVC/SUBV/SUBVC/XORV/XORVC//")

   declare("ADDS/CMPL/DECR/DSPL/EXBY/EXIT/INCR/*
           *RCSW/RISE/RLK/RTRN/TRS/TSR/XEC/ZERO/*
           *ZLBY/ZRBY/SLC/SLCL/SLIO/SLIZ/RCSM/RCSR//")

   declare("SRA/SRC/SRCL/SRLC//")

   declare("BMS/FMS/INE/INH/LKR/LKS/*
           *PMA/SYNC/CARG/CARGM//")

   declare("WAIT/TRAP//")

   declare("CTRL/DTIM/DTIR/DTOM/DTOR/TEST//")

   declare("MPY/DIV//")

   declare("NTR/NTRC/NTRD/NTRE/RTN/RTND/RTNDI/RTNE/*
           *RTNEI/RTNI/ALC/ALCD/DLC/DLCD/RESA/*
           *RESAB/RESC/RESCY/SAVA/SAVAB/SAVC/SAVCY//")

   declare("CDEA/CEA/ARG//")

   // Directives

   declare("ASM/PRINT/NLP/DATE/TITLE/EJECT/*
           *DEF/REF/SREF/DSECT/PSECT/ORG/*
           *ASECT/BASE/COM/GLOB/EQU/SET/DS/*
           *LPOOL/ADRL/DC/TEXT/DO/GOTO/END/EOT//")

   // Register mnemonics

   declare("A/X/Y/Z/B/C/D/E//")

  $)







