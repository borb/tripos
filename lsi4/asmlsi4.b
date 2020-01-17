// (C) Copyright 1978 Tripos Research Group
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
absmin.set     :       157
absmax.set     :       158

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

label          :       177
labeltype      :       178

symb           :       180
symbtype       :       181
expval         :       182
exptype        :       183
tagv           :       184
ch             :       185
tagtable       :       186
stvec          :       187
stvecp         :       188

external.symbol:       189

outbuf         :       190
outbufp        :       191
nerrs          :       192
charpos        :       194

ended          :       195
pass1          :       196
pass2          :       197

prstart        :       198
prend          :       199

end.of.operands:       201

valpntr        :       205
typepntr       :       206

errors.found   :       207
numerrs        :       208

argv           :       210
sourcestream   :       211
liststream     :       212
codestream     :       213
listing        :       214
failed         :       215
list.required  :       216
sysout         :       217

operand        :       220
operand.type   :       221
indirect.operand:      222
literal.operand:       223
pre.indexed    :       224
post.indexed   :       225
relocatable.address:   226
byte.factor    :       227

first.word     :       235
space.list     :       236
print.this.line:       237
blank.lines    :       238
title.not.read.yet:    239
stvecupb       :       241
inhdr          :       242
hdrstream      :       243

checkopen:250
firstpass:251
secondpass:252
tidy.up.on.fail:253
gvec:254
freevectors:255
complain:256
warn:257
doline:258
setlabel:259
skiplayout:260
skipfield:261
skip.operand:262
skiprest:263
setsymb:264
readrest:265
dodir:266
doinstr:267
get.operand:268
operands.match:269
types.match:270
read.operand:271
read.general.operand:272
read.indexer:273
generate.instruction:274
branch.addr:275
gen.addr:276
readabsexp:277
readexp:278
chkop:279
readsymb:280
readtag:281
readnum:282
digval:283
changemode:284
setloc:285
build:286
outcode:287
outword:288
rch:289
unrch:290
putloc:291
putval:292
putaddress:293
putn:294
digit:295
pch:296
declare:297
lookup:298
newvec:299
reportundefs:300
clearbits:301
error:302
declsyswords:303
stringtonum:304
readstring:305

       $)


MANIFEST
$(

// Basic constants
stvdefsize   =   3000
tagtablesize =    200
argvupb      =     50
titlecharsmax=     40
x.register   =      2
num.operands =      3
outbuflim    =    120

// Symbol types
s.abs   =  1     // Tag with absolute value
s.dir   =  2     // Assembler directive
s.dollar=  3     // Location counter symbol
s.instr =  4     // Instruction mnemonic
s.new   =  5
s.none  =  6     // No symbol found before end of line
s.rel   =  7     // Tag with relocatable value
s.lbr   =  8     // Left parenthesis
s.number=  9     // Number (123. or "e, say)
s.monop = 10     // Monadic operator
s.ext   = 22     // External reference

s.2reg  = 11     // Register A or Q
s.4reg  = 12     // Register X or Y
s.6reg  = 13     // Register A,Q,X,Y,K or L
s.s     = 14     // Register S
s.8reg  = 15     // Register A,Q,X,Y,K,L,S or P
s.dreg  = 16     // Register pair AQ,AX or AY

s.shift = 17     // LO,R,RO or RA
s.jumpcon=18     // OV, SS or CY
s.search =19     // LT,LE,EQ,NE,GT,GE,TL or HD
s.insert= 20     // SUC,UNC,SUCNE or NEQ
s.delete= 21     // SUCEQ

// Operand types

o.none    = 0    // No operand
o.any     = 31   // Any operand

o.2reg    = s.2reg
o.4reg    = s.4reg
o.6reg    = s.6reg
o.s       = s.s
o.8reg    = s.8reg
o.dreg    = s.dreg

o.shift   = s.shift
o.jumpcon = s.jumpcon
o.search  = s.search
o.insert  = s.insert
o.delete  = s.delete

o.mem     =  1   // Address for cond. jump (No * or index)
o.gen     =  2   // General memory address
o.8bit    =  3   // Absolute value in range 0-255
o.bit     =  4   // Absolute value in range 0-15
o.sinval  =  5   // Absolute value in range 1-16
o.sublit  =  6   // Literal in range 1-256 (For SUB immed)
o.poslit  =  7   // Literal in range 0-255
o.neglit  =  8   // Literal in range -256 to -1
o.locate  =  9   // Absolute value 0 or 1.


// Operators

op.plus   = 1
op.minus  = 2
op.times  = 3
op.over   = 4
op.and    = 5
op.or     = 6
op.not    = 7
op.shift  = 8

// Instruction types

i.zop   =  1     // No operands (HALT)
i.push  =  2     // PUSH instruction

// Bits in the type word for 'general' format

general.bit = #X20
byte.bit    = #X10

// Symbol table types for instructions.
// These are the type fields for entries for the
// instruction tags in the symbol table.

sti.zop   = (i.zop   << 10) + s.instr
sti.push  = (i.push  << 10) + s.instr

sti.g1    = ((general.bit +            1) << 10) + s.instr
sti.g2    = ((general.bit +            2) << 10) + s.instr
sti.g3    = ((general.bit +            3) << 10) + s.instr
sti.g4    = ((general.bit +            4) << 10) + s.instr
sti.g5    = ((general.bit +            5) << 10) + s.instr
sti.g6    = ((general.bit +            6) << 10) + s.instr
sti.g7    = ((general.bit +            7) << 10) + s.instr
sti.g8    = ((general.bit +            8) << 10) + s.instr

sti.gb1   = ((general.bit + byte.bit + 1) << 10) + s.instr
sti.gb2   = ((general.bit + byte.bit + 2) << 10) + s.instr
sti.gb3   = ((general.bit + byte.bit + 3) << 10) + s.instr
sti.gb4   = ((general.bit + byte.bit + 4) << 10) + s.instr

// Operand patterns for symbol table.
//   g.x  Single word
//   e.x  Extended addressing
//   2,4,6,8,s,d  Register
//   gen, mem     Addresses
//   bit          0-15
//   pl,sl,nl     Literals
//   sin          1-16
//   j            OV, SS or CY
//   b8           8 bit number (DAFC, etc.)
//   shfs, shfd   Single, double shifts
//   srch         SEARCH
//   lcte         LOCATE
//   inst         INSERT
//   dlte         DELETE

extended.bit   = #X8000

g.gen4   = (o.gen     << 10) + (o.4reg    << 5)
g.4gen   = (o.4reg    << 10) + (o.gen     << 5)
g.gen2   = (o.gen     << 10) + (o.2reg    << 5)
g.2gen   = (o.2reg    << 10) + (o.gen     << 5)
g.gend   = (o.gen     << 10) + (o.dreg    << 5)
g.dgen   = (o.dreg    << 10) + (o.gen     << 5)
g.gen    = (o.gen     << 10)
g.4mem   = (o.4reg    << 10) + (o.mem     << 5)
g.jmem   = (o.jumpcon << 10) + (o.mem     << 5)
g.88     = (o.8reg    << 10) + (o.8reg    << 5)
g.86     = (o.8reg    << 10) + (o.6reg    << 5)
g.8s     = (o.8reg    << 10) + (o.s       << 5)
g.66     = (o.6reg    << 10) + (o.6reg    << 5)
g.6s     = (o.6reg    << 10) + (o.s       << 5)
g.8      = (o.8reg    << 10)
g.s      = (o.s       << 10)
g.6      = (o.6reg    << 10)
g.4      = (o.4reg    << 10)
g.pl4    = (o.poslit  << 10) + (o.4reg    << 5)
g.4pl    = (o.4reg    << 10) + (o.poslit  << 5)
g.nl4    = (o.neglit  << 10) + (o.4reg    << 5)
g.sl4    = (o.sublit  << 10) + (o.4reg    << 5)
g.pl2    = (o.poslit  << 10) + (o.2reg    << 5)
g.sin    = (o.sinval  << 10)
g.4b8    = (o.4reg    << 10) + (o.8bit    << 5)
g.b84    = (o.8bit    << 10) + (o.4reg    << 5)
g.b8     = (o.8bit    << 10)
g.bit4   = (o.bit     << 10) + (o.4reg    << 5)
g.bits   = (o.bit     << 10) + (o.s       << 5)
g.shfs   = (o.4reg    << 10) + (o.shift   << 5) + o.sinval
g.shfd   = (o.dreg    << 10) + (o.shift   << 5) + o.sinval
g.srch   = (o.search  << 10) + (o.gen     << 5)
g.lcte   = (o.locate  << 10)
g.inst   = (o.insert  << 10)
g.dlte   = (o.delete  << 10)

e.gen4   = g.gen4 + extended.bit
e.4gen   = g.4gen + extended.bit
e.gen2   = g.gen2 + extended.bit
e.2gen   = g.2gen + extended.bit
e.gend   = g.gend + extended.bit
e.dgen   = g.dgen + extended.bit
e.gen    = g.gen  + extended.bit
e.srch   = g.srch + extended.bit
e.inst   = g.inst + (o.gen << 5) + extended.bit
e.dlte   = g.dlte + (o.gen << 5) + extended.bit

// Directives

d.abs   = 1
d.rel   = 2
d.org   = 3
d.res   = 4 + #X100
d.equ   = 5 + #X100
d.list  = 6
d.space = 7
d.data  = 8 + #X100
d.text  = 9 + #X100
d.titl  =10
d.end   =11 + #X100
d.extr  =12
d.nam   =13

d.unimp =20 + #X100

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
stb.nam    =  #40
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
ext.ext    =  129


$)

.
SECTION "L4ASM"

GET "BCPL.ASM"

LET start() BE
  $( LET ob = VEC 60
     LET av = VEC argvupb
     LET tgv = VEC 2
     LET opv = VEC num.operands * 6 - 1

     operand          := opv - 1
     operand.type     := opv + num.operands - 1
     indirect.operand := opv + num.operands * 2 - 1
     literal.operand  := opv + num.operands * 3 - 1
     pre.indexed      := opv + num.operands * 4 - 1
     post.indexed     := opv + num.operands * 5 - 1



     sysout   := output()

     argv     := av
     tagv     := tgv

     outbuf := ob

     title.not.read.yet  := yes


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
     list.required := listing

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

     writes("TRIPOS LSI 4/30 Assembler Version 2*E")

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
  $( relmin         := maxint
     relmax         := 0

     absmin.set     := no
     absmax.set     := no
     absrp,  relrp  := 0,0
     absloc, relloc := 0,0

     locmode, relp := s.abs, absrp
     minloc, maxloc, location := absmin,absmax,absloc

     charpos := 1
     ended := no; pass1 := yes; pass2 := no

     inhdr := no

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

     // Force saving of final location values

     changemode((locmode = s.abs -> s.rel, s.abs))

     IF NOT absmin.set THEN
       absmin, absmax := 0, 0

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
  $( // Get the code vectors

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

     relmin         := maxint
     relmax         := 0

     absmin.set     := no
     absmax.set     := no

     absrp,  relrp  := 0,0
     absloc, relloc := 0,0

     locmode := s.abs

     charpos := 1
     codevec, relocvec, relp := absvec, absrvec, absrp
     minloc, maxloc, location := absmin,absmax,absloc
     ended := no; pass1 := no; pass2 := yes
     first.word,space.list,listing := no,no,list.required

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

     UNTIL ended DO
       $( outbufp := 22; nerrs := 0
          blank.lines, print.this.line := 0, yes
          prstart, prend := 0, -1
          errors.found := no

          FOR j = 0 TO outbuflim DO
            putbyte(outbuf,j,' ')

          doline()

          IF errors.found THEN
            numerrs := numerrs + 1

          IF (listing & print.this.line) |
            nerrs > 0 THEN
            FOR j = 0 TO outbufp - 1 DO
              wrch(getbyte(outbuf,j))

          IF listing THEN
            $( FOR p = prstart TO prend DO
                 writef("       %X4 %X4*N",p,codevec!p)

               FOR j = 1 TO blank.lines DO wrch('*N')
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
     changemode(locmode = s.abs -> s.rel,s.abs)

     TEST numerrs > 0 THEN
       writef("*N*N%N line(s) with errors*N",numerrs)
      ELSE
       writes("*N*NNo errors detected*N")

     reportundefs()

     IF NOT absmin.set THEN
       absmin, absmax := 0, 0

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
  $( // Handles a complete line of code, of any sort.
     // First the label (if any) is read, and then
     // READREST is called.

     label    := 0     // Non-zero if there was a label

     external.symbol := 0

     byte.factor := 1  // = 2 for byte instructions.

     rch()             // Get first character

     end.of.operands := no

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
  // Does a 'skiplayout', but insists that there is
  //  at least one layout character. (' ','*N' or '*T')
  TEST ch = ' ' | ch = '*N' | ch = '*T' THEN
    skiplayout()
   ELSE
    complain('Q')


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
  // Only called for instructions.
  $( UNTIL (ch = ' ') | (ch = ',') | (ch = '*N') DO
       rch()
     end.of.operands := ch = ' '
  $)


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
         TEST t = s.ext & (st.type!s & stb.nam) \= 0 THEN
           complain('I')
          ELSE
           $( st.type ! s := (st.type ! s & #177740) | t
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
            complain('O')
            build(#X0E0D,s.abs)
            ENDCASE

          CASE s.instr:
            doinstr()
            ENDCASE

       $)
  $)




AND dodir() BE
  // Handles a directive.
  // The directive symbol itself has already
  //  been read.
  // The type field contains a bit :100 if a label
  //  is allowed. (Eg. DATA, END, TEXT, etc.)

  $( LET t = st.value ! symb  // Directive type
     TEST (t & #X100) = 0 THEN
       IF label \= 0 THEN
         complain('L')
      ELSE
       IF t \= d.equ THEN
         setlabel()

     // Skip layout characters (at least one)

     skipwithspace()

     SWITCHON t INTO

       $( CASE d.abs:
            readabsexp(no)
            UNLESS errors.found THEN
              $( changemode(s.abs)
                 setloc(expval)
              $)
            putloc()
            ENDCASE

          CASE d.rel: readsymb()
            readexp(no)
            UNLESS errors.found THEN
              $( changemode(s.rel)
                 setloc(expval)
              $)
            putloc()
            ENDCASE

          CASE d.org: readsymb()
            readexp(no)
            TEST exptype = locmode THEN
              setloc(expval)
             ELSE
              UNLESS errors.found THEN
                complain('R')
            putloc()
            ENDCASE

          CASE d.res: readabsexp(no)
            UNLESS errors.found THEN
              $( LET w = expval
                 TEST ch = ',' THEN
                   $( rch(); readabsexp(yes)
                      putval(expval)
                      FOR j = 1 TO w DO
                        build(expval,exptype)
                      prend := prstart - 1
                   $)
                  ELSE
                   $( putloc()
                      setloc(location + w)
                   $)
              $)
            ENDCASE

          CASE d.equ: readsymb()
            readexp(no)
            TEST label = 0 THEN
              complain('L')
             ELSE
              setsymb(label,labeltype,expval,exptype)
            putval(expval)
            ENDCASE

          CASE d.list: readabsexp(no)
            UNLESS errors.found THEN
              $( space.list := (expval & #X40) > 0
                 first.word := (expval & #X08) > 0
                 listing :=   ((expval & #X01) = 0) &
                                list.required
              $)
            putval(expval)
            ENDCASE

          CASE d.space: readabsexp(no)
            blank.lines := expval
            print.this.line := space.list
            putval(expval)
            ENDCASE

          CASE d.end:
            ended := yes
            putloc()
            RETURN

          CASE d.data: unrch()
            $( rch(); readsymb(); readexp(yes)
               IF errors.found THEN
                 exptype := s.rel
               build(expval, exptype)
            $) REPEATWHILE ch = ','
            IF ch \= ' ' & ch \= '*N' THEN
              complain('D')
            IF first.word THEN
              prend := prstart - 1
            ENDCASE

          CASE d.text:
            IF ch \= '*'' THEN
              $( complain('T')
                 ENDCASE
              $)
            readstring(build,yes)
            IF first.word THEN
              prend := prstart - 1
            ENDCASE

          CASE d.titl:
            IF title.not.read.yet THEN
              $( LET l, lastnb = 1, 0
                 LET title=VEC titlecharsmax/bytesperword

                 FOR j = 1 TO titlecharsmax DO
                   title % j := ' '

                 UNTIL ch = '*N' DO
                   $( IF ch \= ' ' THEN
                        $( IF l <= titlecharsmax THEN
                             $( lastnb := l
                                title % l := ch
                             $)
                        $)
                      l := l + 1
                      rch()
                   $)

                 IF lastnb \= 0 THEN
                   $( title % 0 := lastnb
                      writef(" assembling *"%S*"*E",title)
                   $)

                 title.not.read.yet := no

              $)
            RETURN

          CASE d.nam:
          CASE d.extr:
            unrch()
            $( rch()
               UNLESS 'A' <= ch <= 'Z' THEN
                 $( complain('Q')
                    skip.operand()
                    LOOP
                 $)
               readsymb()
               TEST t = d.extr THEN
                 setsymb(symb, symbtype, 0, s.ext)
                ELSE
                 TEST symbtype = s.ext THEN
                   complain('I')
                  ELSE
                   st.type!symb := st.type!symb | stb.nam
            $) REPEATWHILE ch = ','
            IF ch \= ' ' & ch \= '*N' THEN
              complain('Q')
            ENDCASE


          CASE d.unimp:
            warn('Y')
            RETURN

       $)

     skipwithspace()

  $)


AND doinstr() BE
  // Reads an instruction.
  // The mnemonic has already been read.

  $( LET itype = (st.type ! symb) >> 10
     LET s     = symb

     relocatable.address := no

     // Skip layout characters (at least one)

     skipwithspace()

     setlabel()

     SWITCHON itype INTO

       $( CASE i.zop:  // No operands
            build(st.value ! s,s.abs)
            ENDCASE

          CASE i.push: // One absolute operand
            build(st.value ! s,s.abs)
            expval := 0
            IF pass2 THEN
              readabsexp(yes)
            build(expval,s.abs)
            ENDCASE

          DEFAULT: unrch()  // General.
            byte.factor := (itype & byte.bit) = 0 -> 1,2

            FOR j = 1 TO num.operands DO
              get.operand(j)

            UNLESS end.of.operands THEN
              complain('S')

            $( LET npat = itype & 15
               LET pattern = s ! st.value

               FOR j = 1 TO npat DO
                 TEST operands.match(!pattern) THEN
                   $( generate.instruction(pattern!1,
                        (!pattern & #X8000) \= 0)
                      ENDCASE
                   $)
                  ELSE
                   pattern := pattern + 2
               complain('S')
               build(#X0E0D,s.abs)
            $)
       $)
  $)


AND get.operand(n) = VALOF
  $( indirect.operand ! n := no
     literal.operand  ! n := no
     post.indexed     ! n := no
     pre.indexed      ! n := no
     operand.type     ! n := o.none
     RESULTIS read.operand(n)
  $)


AND operands.match(pattern) = VALOF
  $( LET v = VEC num.operands
     FOR j = 1 TO num.operands DO
       $( LET ptype = (pattern >> 10) & #X1F
          v!j := ptype
          pattern := pattern << 5
          UNLESS types.match(ptype,j) THEN
            RESULTIS FALSE
       $)
     FOR j = 1 TO num.operands DO
       $( IF operand.type ! j = s.rel THEN
            relocatable.address := yes
          operand.type ! j := v ! j
       $)
     RESULTIS TRUE
  $)


AND types.match(type,n) = VALOF
  $( LET val = operand ! n
     LET ot  = operand.type ! n
     LET absrel = [(ot=s.abs) | (ot=s.rel) | (ot=s.ext)] &
                  NOT literal.operand ! n
     LET noxyind = NOT (indirect.operand ! n |
                        post.indexed     ! n |
                        pre.indexed      ! n )
     LET abslit = (ot = s.abs) & noxyind
     LET absolute = abslit & NOT literal.operand ! n
     LET literal  = abslit &     literal.operand ! n

     IF ot = o.any THEN RESULTIS TRUE

     SWITCHON type INTO

       $( CASE o.none:
          CASE o.dreg: CASE o.s:
          CASE o.jumpcon: CASE o.search:
          CASE o.insert:
            RESULTIS type = ot

          CASE o.shift:  // L is a register!
            IF (ot = s.6reg) & (val = #B101) THEN
              $( operand ! n := 0
                 RESULTIS yes
              $)
            RESULTIS ot = o.shift

          CASE o.delete: // SUC or SUCEQ
            RESULTIS (type = ot) |
                     (ot = o.insert) & (val = 0)

          CASE o.mem:
            RESULTIS absrel & noxyind

          CASE o.gen:
            RESULTIS absrel

          CASE o.2reg: CASE o.4reg:
          CASE o.6reg: CASE o.8reg:
            RESULTIS o.2reg <= ot <= type

          CASE o.sublit:
            RESULTIS literal & (1 <= val <= 256)

          CASE o.poslit:
            RESULTIS literal & (0 <= val <= 255)

          CASE o.neglit:
            RESULTIS literal & (-256 <= val < 0)

          CASE o.sinval:
            RESULTIS absolute & (1 <= val <= 16)

          CASE o.bit:
            RESULTIS absolute & (0 <= val <= 15)

          CASE o.8bit:
            RESULTIS absolute & (0 <= val <= 255)

          CASE o.locate:
            RESULTIS absolute & (0 <= val <= 1)
       $)
     RESULTIS FALSE
  $)




AND read.operand(n) = VALOF
  // Reads instruction operand, delivering
  //  yes if there was one.
  TEST end.of.operands THEN

    RESULTIS no

   ELSE

    $( rch()   // Read first character

       TEST (ch = '*N') | (ch = ' ') | (ch = ',') THEN
         operand.type ! n := o.none // Null
        ELSE
         TEST (ch = '=') | (ch = '**') THEN
           read.general.operand(n,ch = '=', ch = '**')
          ELSE
           $( // Must be a symbol
              readsymb()

              SWITCHON symbtype INTO

                $( CASE s.abs: CASE s.new: CASE s.rel:
                   CASE s.ext: CASE s.lbr: CASE s.monop:
                   CASE s.dollar: CASE s.number:

                     read.general.operand(n,no,no)
                     ENDCASE

                   CASE s.2reg: CASE s.4reg: CASE s.6reg:
                   CASE s.8reg: CASE s.dreg: CASE s.s:
                   CASE s.shift: CASE s.jumpcon:
                   CASE s.search: CASE s.insert:
                   CASE s.delete:

                     operand.type ! n := symbtype
                     operand      ! n := st.value ! symb
                     ENDCASE

                   DEFAULT:

                     complain('S')
                     operand.type ! n := o.any
                     skip.operand()

                $)
           $)

       IF ch \= ',' THEN
         TEST (ch = '*N') | (ch = ' ') THEN
           end.of.operands := yes
          ELSE
           $( complain('S')
              operand.type  ! n := o.any
              skip.operand()
           $)

       RESULTIS yes

    $)


AND read.general.operand(n, literal, indirect) BE
  // Read general expression.
  // If either of the parameters is yes, then
  //  the first symbol has not yet been read.
  // If in the first pass, ignore the expression

//  TEST pass1 THEN
//    $( operand.type ! n := o.any
//       skip.operand()
//    $)
//   ELSE
    $( indirect.operand ! n := indirect
       literal.operand  ! n := literal

       IF literal | indirect THEN
         $( rch(); readsymb()
         $)
       readexp(yes)
       operand ! n := expval
       operand.type ! n := exptype
       FOR j = 1 TO 2 DO
         TEST ch = '(' THEN
           read.indexer(n)
          ELSE
           BREAK
    $)


AND read.indexer(n) BE
  // Reads any index registers [ (X), (Y) or (X,Y)].
  // Called with '(' already read.
  $( LET regfound = no; rch()
     $( LET in = ?
        IF ch = ')' THEN
          $( UNLESS regfound THEN
               warn('I')
             rch()
             RETURN

          $)
        readsymb()
        IF symbtype \= s.4reg THEN
          $( complain('S')
             operand.type ! n := o.any
             RETURN
          $)
        in := st.value ! symb = x.register ->
                   @ post.indexed ! n,
                   @ pre.indexed  ! n
        IF ! in THEN
          warn('I')
        !in, regfound := yes, yes

        IF ch = ',' THEN
          $( rch()
             regfound := no
          $)
     $) REPEAT

  $)


AND generate.instruction(base,extended) BE
  $( LET v1,v2,v2type,regop = base,0,s.rel,0

     IF pass2 THEN

       FOR j = 1 TO num.operands DO
         $( LET o = operand ! j
            SWITCHON operand.type ! j INTO

              $( CASE o.shift: // No ROTATE arithmetic.
                   IF (base = #X0E02) & (o = #X000C) THEN
                     $( complain('S')
                        v1 := #X0E0D
                        BREAK
                     $)

                 CASE o.dreg: CASE o.jumpcon:
                 CASE o.locate: CASE o.search:
                 CASE o.insert: CASE o.delete:
                 CASE o.8bit:  CASE o.poslit:
                   v1 := v1 + o
                   ENDCASE

                 CASE o.sinval: o := o - 1
                 CASE o.bit:    v1 := v1 + (o << 4)
                   ENDCASE

                 CASE o.2reg: CASE o.4reg:
                   v1 := v1 + (o << 12)
                   ENDCASE

                 CASE o.s:
                   IF (base & #XFFF0) = #X0E00 THEN
                     // SBIT, etc. on S: no action
                     ENDCASE

                 CASE o.6reg: CASE o.8reg:
                   $( LET sh = (8 * regop + 4)
                      // NB: operands other way round
                      //  for comparisons (CSK and CSM)!
                      IF base=#X000B | base=#X0006 THEN
                        sh := 16 - sh
                      v1 := v1 + (o << sh)
                   $)
                   regop := regop + 1
                   ENDCASE

                 CASE o.sublit: o := -o
                 CASE o.neglit: o := o & #XFF
                   v1 := v1 + o
                   ENDCASE

                 CASE o.mem:
                   v1 := v1 + branch.addr(o)
                   putaddress(o)
                   ENDCASE

                 CASE o.gen:
                   v1 := v1 + gen.addr(o,extended,
                            @ v2,@ v2type,j)
                   putaddress(o)
                   ENDCASE
              $)
         $)

     build(v1,s.abs)
     IF extended THEN
       build(v2,v2type)
  $)


AND branch.addr(loc) = VALOF
  $( LET d = ?
     IF relocatable.address \= (locmode = s.rel) |
       external.symbol \= 0 THEN
       $( complain('A')
          RESULTIS 0
       $)
     d := loc - location
     UNLESS -63 <= d <= 64 THEN
       $( complain('A')
          RESULTIS 0
       $)
     RESULTIS (d + 63)
  $)


AND gen.addr(loc,extended,at.v2,at.v2t,j) = VALOF
  TEST extended THEN
    $( LET v = 0
       LET relative =
         relocatable.address & (locmode = s.rel)
       TEST relative THEN
         $( v := v + #X80
            !at.v2 := loc - location - 2
            !at.v2t:= s.abs
         $)
        ELSE
         $( !at.v2 := loc
            !at.v2t:= relocatable.address -> s.rel,
                      external.symbol = 0 -> s.abs, s.ext
         $)
       IF pre.indexed ! j THEN
         v := v + #X10
       IF post.indexed ! j THEN
         v := v + #X20
       IF indirect.operand ! j THEN
         v := v + #X40
       RESULTIS v
    $)
   ELSE
    $( LET d, relative = ?, no
       LET v = loc
       IF external.symbol \= 0 THEN
         $( complain('A')
            RESULTIS 0
         $)
       TEST relocatable.address THEN
         // Must be relative
         $( IF locmode = s.abs THEN
              $( complain('A')
                 RESULTIS 0
              $)
            relative := yes
         $)
        ELSE
         UNLESS 0 <= loc <= 63 THEN
           $( IF locmode = s.rel THEN
                $( complain('A')
                   RESULTIS 0
                $)
              relative := yes
           $)
       IF relative THEN
         $( LET d = loc - (location + 1) * byte.factor
            UNLESS -128 <= d <= 127 THEN
              $( complain('A')
                 RESULTIS 0
              $)
            IF pre.indexed ! j | post.indexed ! j THEN
              $( complain('S')
                 RESULTIS 0
              $)
            v := #X200 + d + 128
         $)
       IF post.indexed ! j THEN
         v := v + #X80
       IF pre.indexed ! j THEN
         v := v + #X40
       IF indirect.operand ! j THEN
         v := v + #X100
       RESULTIS v
    $)


.

SECTION "L4ASM2"

GET "BCPL.ASM"

LET readabsexp(frefs) BE
  // Called from 'dodir'.
  // Must read a symbol.
  $( readsymb(); readexp(frefs)
     IF exptype \= s.abs THEN
       $( complain('A')
          expval, exptype := 0, s.abs
       $)
  $)


AND readexp(frefs) BE
  $( LET et,eval,mop,op,unknown = 0,0,0,op.plus,no

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
               sval := location
               st := (locmode = s.rel -> 1,0)
               GOTO elb2

             CASE s.number:
               sval := symb
               GOTO elab

             CASE s.ext:
               st := 10000
               UNLESS frefs THEN
                 complain('F')
               external.symbol := symb
               GOTO elab

             CASE s.rel:
               st := 1
               GOTO elb3

             CASE s.new:
               unknown := yes
             CASE s.abs:

         elb3: sval := st.value ! symb
               UNLESS (st.type ! symb & stb.setnow) > 0 |
                 frefs THEN
                 complain('F')

         elb2: // Symbolic terms are doubled for
               //  byte addressing
               sval := sval * byte.factor

         elab: // Apply monadic operator
               SWITCHON mop INTO
                 $( CASE op.not:
                      sval := sval + 1
                    CASE op.minus:
                      sval := - sval
                      st := - st
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
                      IF (st > 0) & (et > 0) THEN
                        GOTO relerr

                      eval := eval * sval
                      et   := st*eval + et*sval
                      ENDCASE

                    CASE op.over:
                      IF (st > 0) | (et > 0) THEN
                        GOTO relerr
                      eval := eval/sval
                      ENDCASE

                    CASE op.or: CASE op.and:
                      IF (et > 0) | (st > 0) THEN
                        GOTO relerr
                      eval := (op = op.or -> eval | sval,
                                             eval & sval)
                      ENDCASE

                    CASE op.shift:
                      IF (st > 0) |
                         (sval < 0) &
                         (et & ((1 << -sval)-1)) \= 0 THEN
                        GOTO relerr
                      TEST sval >= 0 THEN
                        $( eval := eval << sval
                           et := et << sval
                        $)
                       ELSE
                        $( eval := eval >> (- sval)
                           et := et >> (- sval)
                        $)
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
             readsymb()
          $)
         ELSE // Not an operator: expression finished
          BREAK

     $) REPEAT

     IF (et \= 0) & (et \= 1) & (et \= 10000) THEN
       GOTO relerr

     exptype := (et = 1 | unknown -> s.rel,
                 et = 10000       -> s.ext, s.abs)
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
       CASE ';': RESULTIS op.or
       CASE '&': RESULTIS op.and
       CASE '%': RESULTIS op.shift

       DEFAULT:  RESULTIS 0

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
     $( SWITCHON ch INTO

          $( CASE ' ':CASE '*N': symbtype:=s.none; RETURN

             CASE '0': // Octal value with a leading zero
               symb := readnum(8)
               symbtype := s.number
               RETURN

             CASE ':': // Hex value, e.g. :ABCD
               rch()
               symb := readnum(16)
               symbtype := s.number
               RETURN

             CASE'1':CASE'2':CASE'3':CASE'4':
             CASE'5':CASE'6':CASE'7':CASE'8':CASE'9':
                symb := readnum(10)
                symbtype := s.number
                RETURN

              CASE '(':
                symbtype := s.lbr
                BREAK

              CASE '*'':
                $( LET nc = readstring(symbfromquote,no)
                   UNLESS 1 <= nc <= 2 THEN
                     $( complain('T')
                        symb := 0
                     $)
                   symbtype := s.number
                   RETURN
                $)

              CASE '+':
                symbtype := s.monop
                symb := op.plus
                BREAK

              CASE '-':
                symbtype := s.monop
                symb := op.minus
                BREAK

              CASE '\': CASE '~':
                symbtype := s.monop
                symb := op.not
                BREAK

              CASE '$':
                symbtype := s.dollar
                BREAK

              CASE'A':CASE'B':CASE'C':CASE'D':CASE'E':
              CASE'F':CASE'G':CASE'H':CASE'I':CASE'J':
              CASE'K':CASE'L':CASE'M':CASE'N':CASE'O':
              CASE'P':CASE'Q':CASE'R':CASE'S':CASE'T':
              CASE'U':CASE'V':CASE'W':CASE'X':CASE'Y':
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
          (ch = ':')
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
  $( LET word, quote, nc = 0, no, 0
     $( LET insert = no; rch()
        TEST ch = '*'' THEN
          $( IF quote THEN
               insert := yes
             quote := ~ quote
          $)
         ELSE
          $( IF quote | (ch = '*N') THEN
               BREAK
             insert := yes

          $)
        IF insert THEN // New char for string
          $( IF ((nc & 1) = 0) & (nc > 0) THEN
               $( routine(word,s.abs)
                  word := 0
               $)
             nc := nc + 1
             word := (word << 8) + ch
          $)
     $) REPEAT

     IF (nc & 1) \= 0 THEN
       IF shift THEN
         word := (word << 8) + ' '
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
  $( IF locmode = s.abs & NOT absmax.set THEN
       $( absmax.set := yes
          maxloc     := newloc
       $)
     IF (newloc - maxloc) > 0 THEN
       maxloc := newloc
     location := newloc
  $)



AND build(v,t) BE
  $( IF pass2 THEN
       $( IF prstart = 0 THEN
            $( putloc()
               putval(v)
               prstart := location + 1
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
     IF locmode = s.abs & NOT absmin.set THEN
       $( absmin.set := yes
          minloc     := location
       $)
     IF (location - minloc) < 0 THEN
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
     IF absmin.set THEN
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
               TEST (t1 & stb.nam) \= 0 &
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
  putn(7,location)


AND putval(v) BE
  putn(12,v)


AND putaddress(v) BE
  putn(17,v)


AND putn(p,n) BE
  FOR j = p + 3 TO p BY -1 DO
    $( outbuf % j := digit(n & #XF)
       n := n >> 4
    $)


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
            TEST ((t & st.type.mask) = s.instr) &
                (((t >> 10) & general.bit) \= 0) THEN
              $( symb!st.value := valpntr
                 valpntr := valpntr + 2*((t>>10) & 15)
              $)
             ELSE
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

       /* COPY   */ g.gen4, #X8000, g.4gen, #X8400,
       /* COPY   */ g.86,   #X0000, g.8s,   #X0000,
       /* COPY   */ g.pl4,  #X0900, g.nl4,  #X0800,
       /* COPYB  */ g.gen4, #X8000, g.4gen, #X8400,
       /* COPYE  */ e.gen4, #X0708, e.4gen, #X0709,
       /* COPYE  */ e.gend, #X0706, e.dgen, #X0707,
       /* EXCH   */ g.gen4, #X9000, g.4gen, #X9000,
       /* EXCH   */ g.66,   #X0008, g.6s,   #X0008,
       /* EXCHB  */ g.gen4, #X9000, g.4gen, #X9000,
       /* EXCHE  */ e.gen4, #X070C, e.4gen, #X070C,

       /* ADD    */ g.gen4, #X8800, g.86,   #X0002,
       /* ADD    */ g.pl4,  #X0B00,
       /* ADDB   */ g.gen4, #X8800,
       /* ADDE   */ e.gen4, #X070A,
       /* ADDC   */ e.gen2, #X0702,
       /* AND    */ g.gen2, #X9400, g.86,   #X0004,
       /* AND    */ g.8s,   #X0004, g.pl4,  #X1800,
       /* ANDB   */ g.gen2, #X9400,
       /* ANDE   */ e.gen2, #X070E,
       /* COMP   */ g.86,   #X0009, g.8s,   #X0009,
       /* DIV    */ e.gend, #X0701,
       /* MUL    */ e.gend, #X0700,
       /* NEG    */ g.86,   #X0001,
       /* OR     */ g.gen2, #XB400, g.86,   #X0007,
       /* OR     */ g.8s,   #X0007, g.pl4,  #X1900,
       /* ORB    */ g.gen2, #XB400,
       /* ORE    */ e.gen2, #X270E,
       /* SUB    */ g.gen4, #X8C00, g.86,   #X0003,
       /* SUB    */ g.sl4,  #X0A00,
       /* SUBB   */ g.gen4, #X8C00,
       /* SUBE   */ e.gen4, #X070B,
       /* SUBC   */ e.gen2, #X2702,
       /* XOR    */ g.gen2, #X9800, g.86,   #X0005,
       /* XOR    */ g.8s,   #X0005, g.pl4,  #X1700,
       /* XORB   */ g.gen2, #X9800,
       /* XORE   */ e.gen2, #X070F,

       /* CSK    */ g.2gen, #XB800, g.88,   #X000B,
       /* CSK    */ g.4pl,  #X0D00,
       /* CSKB   */ g.2gen, #XB800,
       /* CSKE   */ e.2gen, #X270F,
       /* CLSN   */ g.4pl,  #X0C00, g.pl4,  #X0C00,
       /* CSM    */ g.88,   #X0006,
       /* CSME   */ e.4gen, #X0705,
       /* IMS    */ g.gen,  #XDC00,
       /* IMSE   */ e.gen,  #X470D,
       /* DMSE   */ e.gen,  #X670D,
       /* CSN    */ g.88,   #X000A,

       /* ADDF   */ e.gen,  #X5C00,
       /* CSKF   */ e.gen,  #X5C04,
       /* DIVF   */ e.gen,  #X5C03,
       /* MULF   */ e.gen,  #X5C02,
       /* SUBF   */ e.gen,  #X5C01,

       /* SHIFT  */ g.shfs, #X0E00, g.shfd, #X0F00,
       /* ROTATE */ g.shfs, #X0E02, g.shfd, #X0F02,

       /* JEQ    */ g.4mem, #X1100,
       /* JNE    */ g.4mem, #X1180,
       /* JGT    */ g.4mem, #X1200,
       /* JLE    */ g.4mem, #X1280,
       /* JGE    */ g.4mem, #X1300,
       /* JLT    */ g.4mem, #X1380,
       /* JT     */ g.jmem, #X1600,
       /* JF     */ g.jmem, #X1680,
       /* IJNE   */ g.4mem, #X1480,
       /* IJEQ   */ g.4mem, #X1400,
       /* JEQD   */ g.4mem, #X1500,
       /* JNED   */ g.4mem, #X1580,

       /* JMP    */ g.gen,  #X9C00,
       /* JMPE   */ e.gen,  #X070D,
       /* JST    */ g.gen,  #XBC00,
       /* JSTE   */ e.gen,  #X270D,
       /* JSK    */ g.gen,  #XFC00,
       /* JSKE   */ e.gen,  #X0703,
       /* FMT    */ g.4,    #X0308,
       /* XNX    */ g.4,    #X030A,
       /* SIN    */ g.sin,  #X0E0F,
       /* CEA    */ e.gen4, #X0704,
       /* STRAP  */ g.4b8,  #X1A00,
       /* UTRAP  */ g.4b8,  #X1B00,
       /* AIB    */ g.b8,   #X4500,
       /* AIN    */ g.b8,   #X0500,
       /* AOB    */ g.b8,   #X6500,
       /* AOT    */ g.b8,   #X2500,
       /* SELP   */ g.4b8,  #X0400,
       /* SST    */ g.b8,   #X0600,
       /* IN     */ g.b84,  #X0100,
       /* OUT    */ g.4b8,  #X0200,
       /* ICD    */ g.6,    #X0300, g.s,    #X0300,
       /* OCD    */ g.8,    #X0301,

       /* SBIT   */ g.bit4, #X000C, g.bits, #X0E05,
       /* RBIT   */ g.bit4, #X000D, g.bits, #X0E07,
       /* CBIT   */ g.bit4, #X000E, g.bits, #X0E06,
       /* TBIT   */ g.bit4, #X000F, g.bits, #X0E04,

       /* SEARCH */ e.srch, #X1D00,
       /* LOCATE */ g.lcte, #X3D00,
       /* INSERT */ g.inst, #X5D00, e.inst, #X5D01,
       /* DELETE */ g.dlte, #X7D00, e.dlte, #X7D01,

       /* Zops   */ #X030C, #X030D, #X0309, #X2309,
                    #X0E47, #X0E45, #X0E87, #X0E85,
                    #X0000, #X0E25, #X0E27, #X0E0D,
                    #X6309, #X0302, #X0303,
                    #X4309,

                    // Directive values.

                    d.abs,   d.unimp, d.unimp, d.data,
                    d.end,   d.unimp, d.unimp, d.equ,
                    d.unimp, d.unimp, d.unimp, d.unimp,
                    d.unimp, d.list,  d.unimp, d.unimp,
                    d.unimp, d.unimp, d.unimp, d.org,
                    d.rel,   d.unimp, d.res,   d.unimp,
                    d.unimp, d.unimp, d.unimp, d.space,
                    d.unimp, d.unimp, d.text,  d.titl,
                    d.nam,   d.extr,

                    // Registers

                    #B000, #B100, #B010, #B110,
                    #B111, #B101, #B011, #B001,
                    #X4000, #X2000, #X6000,

                    // Shift descriptions

                    #B0001, #B1000, #B1001, #B1100,

                    // Jumps

                    #X2000, #X0000, #X4000,

                    // SEARCH conditions

                    4, 6, 2, 5, 1, 3, 0, 7,

                    // INSERT

                    0, 2, 4, 6,

                    // DELETE

                    2,

                    // LOCATE

                    0,1

   typepntr := TABLE sti.g6,  sti.gb2, sti.g4,  sti.g4,
                     sti.gb2, sti.g2,

                     sti.g3,  sti.gb1, sti.g1,  sti.g1,
                     sti.g4,  sti.gb1, sti.g1,  sti.g2,
                     sti.g1,  sti.g1,  sti.g1,  sti.g4,
                     sti.gb1, sti.g1,  sti.g3,  sti.gb1,
                     sti.g1,  sti.g1,  sti.g4,  sti.gb1,
                     sti.g1,

                     sti.g3,  sti.gb1, sti.g1,  sti.g2,
                     sti.g1,  sti.g1,  sti.g1,  sti.g1,
                     sti.g1,  sti.g1,

                     sti.g1,  sti.g1,  sti.g1,  sti.g1,
                     sti.g1,

                     sti.g2,  sti.g2,

                     sti.g1,  sti.g1,  sti.g1,  sti.g1,
                     sti.g1,  sti.g1,  sti.g1,  sti.g1,
                     sti.g1,  sti.g1,  sti.g1,  sti.g1,

                     sti.g1,  sti.g1,  sti.g1,  sti.g1,
                     sti.g1,  sti.g1,  sti.g1,  sti.g1,
                     sti.g1,  sti.g1,  sti.g1,  sti.g1,
                     sti.g1,  sti.g1,  sti.g1,  sti.g1,
                     sti.g1,  sti.g1,  sti.g1,  sti.g1,
                     sti.g2,  sti.g1,

                     sti.g2,  sti.g2,  sti.g2,  sti.g2,

                     sti.g1,  sti.g1,  sti.g2,  sti.g2,

                     sti.zop, sti.zop, sti.zop, sti.zop,
                     sti.zop, sti.zop, sti.zop, sti.zop,
                     sti.zop, sti.zop, sti.zop, sti.zop,
                     sti.zop, sti.zop, sti.zop,
                     sti.push,

                     // Directives (34 - I hope)

                     s.dir,   s.dir,   s.dir,   s.dir,
                     s.dir,   s.dir,   s.dir,   s.dir,
                     s.dir,   s.dir,   s.dir,   s.dir,
                     s.dir,   s.dir,   s.dir,   s.dir,
                     s.dir,   s.dir,   s.dir,   s.dir,
                     s.dir,   s.dir,   s.dir,   s.dir,
                     s.dir,   s.dir,   s.dir,   s.dir,
                     s.dir,   s.dir,   s.dir,   s.dir,
                     s.dir,   s.dir,

                     // Registers

                     s.2reg, s.2reg, s.4reg, s.4reg,
                     s.6reg, s.6reg, s.s,    s.8reg,
                     s.dreg, s.dreg, s.dreg,

                     // Shift descriptions

                     s.shift, s.shift, s.shift,
                     s.shift,

                     // Jumps

                     s.jumpcon, s.jumpcon, s.jumpcon,

                     // SEARCH

                     s.search,s.search,s.search,s.search,
                     s.search,s.search,s.search,s.search,

                     // INSERT

                     s.insert,s.insert,s.insert,s.insert,

                     // DELETE

                     s.delete,

                     // LOCATE

                     s.abs, s.abs



   declare("COPY/COPYB/COPYE/EXCH/EXCHB/EXCHE//")

   declare("ADD/ADDB/ADDE/ADDC/AND/ANDB/ANDE/*
           *COMP/DIV/MUL/NEG/OR/ORB/ORE/*
           *SUB/SUBB/SUBE/SUBC/XOR/XORB/XORE//")

   declare("CSK/CSKB/CSKE/CLSN/CSM/CSME/*
           *IMS/IMSE/DMSE/CSN//")

   declare("ADDF/CSKF/DIVF/MULF/SUBF//")

   declare("SHIFT/ROTATE//")

   declare("JEQ/JNE/JGT/JLE/JGE/JLT/JT/JF/*
           *IJNE/IJEQ/JEQD/JNED//")

   declare("JMP/JMPE/JST/JSTE/JSK/JSKE/FMT/*
           *XNX/SIN/CEA/STRAP/UTRAP/*
           *AIB/AIN/AOB/AOT/SELP/SST/IN/OUT/*
           *ICD/OCD//")

   declare("SBIT/RBIT/CBIT/TBIT//")

   declare("SEARCH/LOCATE/INSERT/DELETE//")

   declare("FIX/FLOAT/UIS/RSK/CID/CIE/DIN/*
           *EIN/NOP/SBM/SWM/HLT/POP/*
           *OMCD/IMCD/PUSH//")


   // Assembler directives: most are ignored.

   declare("ABS/BAC/CHAN/DATA/END/ENDC/ENDM/*
           *EQU/EXTR/FORM/IFF/IFT/INST/LIST/*
           *LOAD/LPOOL/MACRO/NAM/NOTE/ORG/REL/*
           *REPT/RES/SAVE/SET/SEXT/SNAM/SPACE/*
           *SPAD/SREL/TEXT/TITL/NAM/EXTR//")


   // Registers and register pairs

   declare("A/Q/X/Y/K/L/S/P/AQ/AX/AY//")


   // Shift descriptions

   declare("LO/R/RO/RA//")


   // Conditional jumps

   declare("OV/SS/CY//")


   // SEARCH instruction conditions

   declare("LT/LE/EQ/NE/GT/GE/TL/HD//")


   // INSERT conditions

   declare("SUC/UNC/SUCNE/NEQ//")


   // DELETE condition

   declare("SUCEQ//")

   // LOCATE symbols (absolute values 0 and 1).

   declare("NODE/PREV//")

  $)







