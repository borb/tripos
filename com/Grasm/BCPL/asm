/*

















*************************************************************************
*  (C) Copyright 1980  Systems Research Group, University of Cambridge  *
*************************************************************************
*                                                                       *
*               G E N E R A L     A S S E M B L E R                     *
*                                                                       *
*                        S E C T I O N S                                *
*                                                                       *
*************************************************************************
**  C  Gray  Girling      COMPUTER LAB,  CAMBRIDGE           26.02.80  **
*************************************************************************





















*/




//  LOG OF CHANGES:
//  ===============
//
//  Log entry is <Version number> <date> <initials> <change>
//
//  3.000  26.02.80  CGG   Relocation and externals added
//  3.006  21.03.80  CGG   Bug in external refs fixed
//  3.008  24.03.80  CGG   Machine type CHEX record produced
//  3.009  03.04.80  CGG   Obscure pagination bug fixed
//  3.011  11.04.80  CGG   Default for S option increased to 3000
//                         Forward references for EQU vars allowed
//  3.012  12.04.80  CGG   Fault in REF chains fixed
//                         Fewer zero length CHEX data records produced
//  3.013  15.04.80  CGG   NEEDS directive added
//  3.016  29.04.80  CGG   Outputs CHEX record type 12
//  3.017  01.05.80  CGG   Random store reads fixed
//  3.020  09.05.80  CGG   Stack & global vector requirements reduced
//  3.021  13.05.80  CGG   Listing bug fixed
//  3.022  16.05.80  CGG   Strings in dictionary tree lose pointer 8% S saved
//  3.023  21.05.80  CGG   CHEX generalized for 8X300
//  3.024  28.05.80  CGG   TITLE directive uses label for CHEX module name
//  3.026  05.06.80  CGG   Memory requirements reduced for PDP11
//  3.027  06.06.80  CGG   ~ or \ (not) allowed in expressions
//  3.028  11.07.80  CGG   NEEDS extended to pass option into CHEX output
//                         WREF added - generates new CHEX record
//  3.029  17.07.80  CGG   END <entrypoint> implemented
//  3.031  04.08.80  CGG   Gets text after last END right (and missing ENDs)
//  3.034  22.08.80  CGG   DEF <expression> feature added
//                         PRAG directive installed and CHEX generated
//  3.037  10.10.80  CGG   DAT-A pragmat generated
//                         Procedure for finding time installed in m/c
//                         dependent part of code
//                         File handling and looking up rationalized
//  3.043  11.11.80  CGG   Restriction on number of GET files removed
//  3.045  14.01.81  CGG   Bug in TEXT variables fixed
//  3.046  14.02.81  CGG   Dynamic DD names + GETLIB library installed for IBM
//  3.054  16.10.82  NJO   Dictionary tree section (ASM2) completely
//                         replaced by HASH TABLE mechanism with 60%
//                         increase in speed
//  3.056  12.01.82  CGG   New CHEX machine type record generated







SECTION "asm1"




/*<RSX
NEEDS "TITLE"
GET "libhdr"
GET "asmhdr"
/*RSX>*/


/*<CAP
GET ".**.l.bcpl.libhdr"
GET ".**.cgg.asmhdr"
/*CAP>*/

/*<IBM
GET "LIBHDR"
GET "ASMHDR"
/*IBM>*/

//*<TRIPOS:
GET "libhdr"
GET ":Gray.bcpl.asmhdr"
/*TRIPOS>*/





/*  These globals kept for their comments - in asmhdr now
GLOBAL $( // dontknow              // set after looking for a label
             memsize               // size of label memory
             binfile               // binary file stream pointer (output)
          // out                   // standard output file
          // error                 // error routine
          // pass                  // set to 'first' or 'second'
             lineno                // number of current line
             outpos                // page and line number of output
             errorlabel            // label to go to in the event of an error
             errorlevel            // dynamic level at which error occurs
          // name                  // name of this program
             throws                // TRUE if page throws are included in o/p
          // list                  // level at which listing takes place
             realbinary            // TRUE when binary & not intel hex produced
             fatal                 // TRUE when error stops second pass starting
             warning               // TRUE when a warning message is outstanding
             warn1                 // 1st error argument in a warning
             warn2                 // 2nd    "            "
             warn3                 // 3rd    "            "
             warn4                 // 4th    "            "
             warnpos               // character position at warning
             short                 // TRUE if listing to be compact
             allsyms               // TRUE if all symbols are to be printed in
                                   // the dictionary printout
             errcount              // number of errors in this parse
             sectno                // current sectno number
             envfile               // file for dumping environment into
             restartpage           // TRUE when page throw needed before output
             iden.valid            // TRUE when type has been checked correct
$)
*/





//
//                       ERROR  MANAGER
//






LET geterror(rc) = VALOF
SWITCHON rc INTO
$(  CASE e.libfull:     RESULTIS "label table is full"
    CASE e.nocodes:     RESULTIS "mnemonic tables not initialised"
    CASE e.nospace:     RESULTIS "not enough space for execution"
    CASE e.fullstring:  RESULTIS "string is longer than 255 characters"
    CASE e.badsym:      RESULTIS "unrecognisable item"
    CASE e.nolab:       RESULTIS "label expected"
    CASE e.badcode:     RESULTIS "illegal mnemonic"
    CASE e.badnum:      RESULTIS "number expected"
    CASE e.badbyte:     RESULTIS "value too large for machine's word length"
    CASE e.doublelab:   RESULTIS "label is doubly defined"
    CASE e.interror:    RESULTIS "internal error number %N"
    CASE e.badfile:     RESULTIS "can*'t open file %S"
    CASE e.badbase:     RESULTIS "bad numeric base char - '%C'"
    CASE e.badlab:      RESULTIS "label is undefined"
    CASE e.titledone:   RESULTIS "title already given"
    CASE e.mtbuf:       RESULTIS "internal error - bad access to binary buffer"
    CASE e.expected:    RESULTIS "'%C' expected"
    CASE e.ftoosmall:   RESULTIS "number (%N) too large for field"
    CASE e.divzero:     RESULTIS "divide by zero"
    CASE e.notlab:      RESULTIS "symbol is not a label"
    CASE e.badform:     RESULTIS "mnemonic illegal in this format"
    CASE e.nostr:       RESULTIS "string expected"
    CASE e.filexs:      RESULTIS "too many files"
    CASE e.posnum:      RESULTIS "positive number expected"
    CASE e.noname  :    RESULTIS "name expected"
    CASE e.badtype :    RESULTIS "wrong sort of name"
    CASE e.nobkt   :    RESULTIS "missing bracket"
    CASE e.nocomnt :    RESULTIS "comment expected"
    CASE e.badtext:     RESULTIS "recursive text expansion illegal"
    CASE e.forward:     RESULTIS "illegal forwards reference"
    CASE e.binfull:     RESULTIS "too much binary generated on this line"
    CASE e.badrel:      RESULTIS "relocatable or external illegal here"
    CASE e.badext:      RESULTIS "external variable must appear on its own"
    CASE e.relsection:  RESULTIS "only one relocatable section allowed"
    CASE e.baddef:      RESULTIS "symbol never defined"
    CASE e.modeset:     RESULTIS "mode is already set"
    CASE e.newentry:    RESULTIS "entry point already given"
    CASE e.badloaddir:  RESULTIS "facility not available in loader format"
    CASE e.overflow:    RESULTIS "run out of address space"
    DEFAULT:            RESULTIS errormess(rc)
$)


AND errormess(rc) = "bad error code given!!"








//
//                      Undefined Library Functions
//






/*      The following are generally useful procedures some of which may be
    available on particular machines
*/




LET simplegetvec(n) = VALOF
TEST memory!0+n+1>memsize
THEN error(e.libfull)
ELSE $(  LET ans=@memory!(memory!0)
         memory!0:=memory!0+n+1
         RESULTIS ans
     $)



LET capitalch(ch) = ('a'<=ch<='z'->ch+'A'-'a',ch)



LET compstring(s1, s2) = VALOF
$(  /*    If you are interested in the speed of this assembler you might like
       to know that it spends up to 25% of its time in this routine.
          This is due to its use within the dictionary to look up symbols -
       a hash table dictionary might yield a significant improvement.
          26.02.80
    */
    LET d=(s1%0-s2%0)
    LET n=(d>0 -> s2%0, s1%0)
    LET i=1
    WHILE i<=n & s1%i=s2%i DO i:=i+1
    RESULTIS (i>n-> d, (s1%i - s2%i))
$)



LET getstr(string) = VALOF
$(  LET len=string%0/bytesperword
    LET s=simplegetvec(len)
    TEST s=null
    THEN error(e.nospace)
    ELSE FOR i=0 TO len DO s!i:=string!i
    RESULTIS s
$)






.
SECTION "asm2"




/*<RSX
NEEDS "TITLE"
GET "libhdr"
GET "asmhdr"
/*RSX>*/


/*<CAP
GET ".**.l.bcpl.libhdr"
GET ".**.cgg.asmhdr"
/*CAP>*/

/*<IBM
GET "LIBHDR"
GET "ASMHDR"
/*IBM>*/

//*<TRIPOS:
GET "libhdr"
GET ":Gray.bcpl.asmhdr"
/*TRIPOS>*/





//
//                       DICTIONARY
//




/*     This module is responsible for all accesses to the dictionary -- which
    is a hash table.   To modules using these procedures the dictionary seems to
    hold a set of triples associating the name of a symbol with its type
    and a value (which must fit into one word).
       'findentry' is the only procedure to lookup a symbol name,  but other
    routines scan the whole hash table to:
             write out the symbol table - 'printlabs'
             delete all the symbols - 'deletelabs'
             generate a loader symbol table - 'gen.defs'
       Users of the general assembler are allowed to invent their own types
    and must redefine the relevent procedures in order that the above procedures
    take account of their existence.
       All symbols are inserted using 'putlab', and there are several procedures
    for retrieving and investigating the symbols subsequently.  These procedures
    set various descriptive flags:
              'dontknow'  -- TRUE if the value sought is a forward reference
              'iden.valid'-- TRUE if the symbol exists here on this pass
       26.02.80

       The symbol table is in fact a hash table.  Words in the table are
    anchors of chains of elements of a given hash value, linked through
    the field 't.link'.  The hash table is obtained on the first call of
    the 'findentry' routine.  Space is got by a call of 'simplegetvec'.
       NOTE that the hashing routines rely on strings being padded up to
    a full word size with 0 bytes, i.e. 'standard' BCPL strings.
       (Hashing code added by Nick Ody).
       16.07.81
*/




STATIC
$( hash.table      = 0
   hash.table.size = 100
$)




MANIFEST
$(  t.type   = 0
    t.link   = 1
    t.val    = 2
    t.info   = 3
    tentrysize = t.info+1
    t.str    = tentrysize
/*  // types:  (for reference)
    type.none   = 0
    type.const  = 1
    type.var    = 2
    type.lab    = 3
    type.ref    = 4
    type.text   = 5
    type.       = type.text+1
    type.mask   = byte1
    flag.double = bit8
    flag.rel    = bit9
    flag.def    = bit10
    flag.fwd    = bit11
    flag.needs  = bit12
    type.wref   = type.ref | flag.def
    type.def    = type.none | flag.def
    type.rellab = type.lab | flag.rel
    type.relvar = type.var | flag.rel
    type.relconst = type.const | flag.rel
*/
$)






LET findentry (string) = VALOF
$(  /*    This procedure is the only one responsible for returning a pointer
       to the place in the symbol table from which the symbol's record should be
       referred. (i.e. a pointer to a pointer).  Apart from the procedures
       which traverse the symbol table it is the only one to access it.
          Since the interface encourages multiple calls on this procedure
       with the same arguments (comparing its type with several others in
       different modules for example) the last reference to the symbol table
       is cached in 'dicposn'.
          The reference to 'lex.space' is because, in a higher module, all
       strings are held in this piece of store - I deny responsibility for it!
       'dicposn' is initialised to 0 before a new string is read into
       'lex.space'
          01.05.80
          The 'string' passed in should be padded to a full word with 0s;
       this is relied on by several routines.
          16.07.81
    */
    IF dicposn\=0 & string=lex.space THEN
RESULTIS dicposn

    IF hash.table = 0 THEN    // First call - obtain space for table
    $(  hash.table.size := (memsize - memory ! 0) / 12
       // Remaining memory DIV 12 makes table about half full
       // Round size up to a prime
try.next.size:
       FOR i = 2 TO hash.table.size - 1 DO
       $(  IF hash.table.size REM i = 0 THEN
           $(  hash.table.size := hash.table.size + 1
               // Wasn't prime - try again
               GOTO try.next.size
           $)
       $)
       hash.table := simplegetvec(hash.table.size)

       FOR i = 0 TO hash.table.size DO hash.table ! i := null
    $)

    dicposn := hash.chain.anchor(string) - t.link
    WHILE t.link ! dicposn \= null DO    // Now search the hash list for a match
    $(  LET dic.string = t.str + t.link ! dicposn

        FOR i = 0 TO string % 0 / bytesperword DO
        $(  IF string ! i \= dic.string ! i THEN
                GOTO next.string         // Match failed - go round again
        $)

    BREAK                                // OK - exit from loop

next.string:

        dicposn := t.link ! dicposn
    $)
    dicposn := dicposn + t.link
    RESULTIS dicposn
$)





AND hash.chain.anchor(string) = VALOF
//   Returns the address (in the hash table) of the anchor of
// the hash chain corresponding to the given string.
//
//   The hashing algorithm takes the exclusive-or of all the
// words of the string, and uses this value REM the hash table size,
// which is arranged to be prime.  This method relies on the trailing
// bytes of the string being padded with zeroes.
$(  LET hashval =  string ! 0
    FOR i = 0 TO string % 0 / bytesperword DO hashval := hashval NEQV string ! i
    RESULTIS hash.table + (ABS hashval) REM hash.table.size
$)




AND getsymb(s) = VALOF
$(  LET t = !findentry(s)
    LET symty = (t=null->type.none, t!t.type&type.mask)
    LET rel.pos=(t=null-> 1, t!t.info-lineno)
    IF t=null | rel.pos>=0 |
       symty=type.ref  | symty=type.none | (t!t.type&flag.fwd)\=0 THEN
       dontknow:=TRUE
    IF symty=type.ref & rel.pos>=0 THEN warn(e.forward)
    iden.valid:= \(t=null-> pass=second,
                  (t!t.type&flag.fwd)\=0 & rel.pos>=0)
    RESULTIS t
$)




AND getlab(s, ansvec) = VALOF
$(  /* if symbol 's' is found 'ansvec' is filled with its value in
       offset 0, its type in offset 1 and the address where its value
       field is kept in offset 2.  It is also returned as the result
       of the call.. 'null' is returned if the symbol was not found
    */
    LET ans=getsymb(s)
    UNLESS ans=null THEN
    $(  ansvec!0 := ((ans!t.type&type.mask) >= type. ->
                     valtype((ans!t.type&type.mask), ans!t.val), ans!t.val)
        ansvec!1 := ans!t.type & \flag.double
        ansvec!2 := @ans!t.val
        ans:=ansvec
    $)
    RESULTIS ans
$)



AND putlab(string,v,ty) = VALOF
$(  /*   Inserts 'string' with value 'v' and type 'ty' into the
       dictionary.  Returns TRUE if 's' was a forewards reference
       and FALSE otherwise.  Allows sensible type conversions and
       value redefinitions.
         A problem, in practice, is that, for efficiency's sake, many symbols
       are only set during the first pass and not checked again on the second
       -- since error messages cannot be generated on the first pass double
       definition is not explicitly reported.
         A second factor to consider with respect to this code, and with the
       surrounding code, is that it is important to give similar results
       on both first and second passes in order to avoid differences in the
       flow of control between the two passes.
         27.02.80
         If a new symbol is inserted it is placed at the end of the
       hash chain, this being pointed to by the result of a call to 'findentry'.
         16.07.81
    */
    LET t       = findentry(string)
    LET ans     = ?
    LET rel.posn= ?
    LET symty   = ?

    TEST !t = null THEN
    $(  // pointer to dictionary element is null
        // element does not exist for this string so create it
        LET length = (string % 0) / bytesperword
        LET new.elem = simplegetvec(tentrysize + length)

        new.elem ! t.val  := v      // Fill in fields of new element
        new.elem ! t.info := lineno
        new.elem ! t.type := ty
        FOR i = 0 TO length DO (new.elem + t.str) ! i := string ! i
        new.elem ! t.link := null   // Link it into end of chain
        !t                := new.elem
        IF pass = second THEN error(e.interror, 6)
RESULTIS TRUE
    $) ELSE t := !t

    IF t=null THEN              // entry was not defined before
    TEST pass=second THEN error (e.interror, 6) ELSE
RESULTIS TRUE

    symty:=t!t.type&type.mask   // extract type number
    rel.posn := t!t.info-lineno
    ans:= rel.posn>=0 | symty=type.ref  // forwards reference ?
//  writef("** putlab(%S,%X4,%N) forward=%C t!ty=%N t!val=%N*N",
//         string,v,ty,(ans->'T','F'),t!t.type,t!t.val)
    TEST pass=second & (t!t.type&flag.double)\=0 THEN
      // flag.double is SET if this symbol was doubly defined
      // during the first pass (when the error could not be reported)
      error(e.doublelab, string) ELSE
    $(  // if symbol was DEF'd it can now be redefined to its new type
        IF (ty & flag.def)\=0 THEN
        $(  // it is legal to redefine a REF as a DEF but not vice versa
            TEST symty\=type.ref & rel.posn<0 THEN
            $(  t!t.type:=t!t.type | flag.double
                // use 'warn' so that first pass will do the same as the
                // second. (otherwise binary output might get out of phase)
                warn(e.doublelab, t+t.str)
            $) ELSE
            t!t.type:=(symty=type.ref -> type.def, t!t.type | flag.def)
            IF symty=type.ref THEN
            $(  // if symbol was a ref change variables completely to make
                // it look as if it were defined as a DEF here
                t!t.info:=lineno
                symty := t!t.type & type.mask
                rel.posn := 0
            $)
            // error if a DEF variable was never defined during the first pass
            IF pass=second & symty=type.none THEN error(e.baddef)
        $)
        // sort out whether any type coersion being done is legal or not
        // redefining a variable to 'type.none' has no effect
        // (type.none is used as part of the primitive type for DEF)
        UNLESS (ty & type.mask) = type.none THEN
        TEST (ty&type.mask)=type.ref & (pass=second | (ty&flag.needs)\=0) THEN
            // a NEEDS variable (type.ref with flag.needs) can be redefined
            // an unlimited number of times without changing their value
            IF symty=type.ref THEN
            TEST (ty&flag.needs)=0 THEN t!t.val:=v
            ELSE t!t.type:=t!t.type | flag.needs  // make it a NEEDS variable
        ELSE
        TEST VALOF SWITCHON symty INTO
        // can this type be redefined ?
        $(  CASE type.none:    RESULTIS TRUE
            CASE type.const:
            CASE type.lab:     RESULTIS rel.posn=0
            DEFAULT:           RESULTIS (ty&type.mask)=symty
        $) THEN
        $(  // yes! coersion is legal: delete old value
            deletetype(t!t.type, t!t.val)
            // redefine dictionary record
            t!t.val:=v
            t!t.info:=lineno
            t!t.type:=ty  | (t!t.type & (flag.double | flag.def))
        $) ELSE
        $(  // no! coersion is illegal: delete redefining value
            deletetype(t!t.type, t!t.val)
            // mark as a double definition and try to give error message
            t!t.type:=t!t.type | flag.double
            warn(e.doublelab, string)
        $)
    $)
    RESULTIS ans                // TRUE if 's' was a forewards reference
$)




//      Types can be invented by the user of the general assembler.
// new type number's (to go in the 'type.mask' field of the type part of
// a dictionary entry) may start at 'type.'.
//      If new types are used the procedures 'freetype', 'valtype', 'savetype'
// (no longer used), and 'printtype' must be defined to delete an object,
// return a value to be used in an arithmetic expression for that object,
// print out an object's creating line of text, and print out the object's
// value in an 11 character field, respectively.  Each of these procedures
// takes the type and the value of such an object as parameter.
//      Predeclared types are already dealt with.




AND looktype(ty, s) = VALOF
$(  LET t=!findentry(s)
    // iden.valid is TRUE if 's' is of type 'ty' and already declared
    iden.valid:=(t=null | t!t.info >= lineno -> FALSE,(t!t.type&type.mask)=ty)
    RESULTIS (iden.valid->t!t.val, 0)
$)



AND checktype (ty, s) = VALOF
$(  LET sym=getsymb(s)
    LET ans=FALSE
    TEST sym=null
    THEN IF pass=first DO
            $(  dontknow:=TRUE
                ans:=TRUE
            $)
    ELSE ans:=((sym!t.type&type.mask) = ty)
//  writef("Checktype : ans = %s*n",(ans->"TRUE","FALSE"))
    RESULTIS ans
$)



AND gettype(ty,s) = VALOF
$(  // produces an error is 's' is not type 'ty'
    // returns the value of the symbol
    LET ans=getsymb(s)
    IF ans=null & pass=second |
       ans\=null & (ans!t.type&type.mask)\=ty
    DO warn(e.badtype)
//  writef("gettype : ans=%X4, its type = %N, name='%S'*N",
//         ans, ans!t.type, ans+t.str)
    RESULTIS (ans=null -> -1, ans!t.val)
$)


AND valtype(ty, v) = VALOF            // for redefinition
$(  warn(e.notlab)
    RESULTIS -1
$)



AND deletetype(ty, val) BE
SWITCHON ty & type.mask INTO
$(  CASE type.none:
    CASE type.const:
    CASE type.var:
    CASE type.lab:
    CASE type.ref:
    CASE type.text:
      ENDCASE
    DEFAULT:
      freetype(ty, val)
$)



AND freetype(ty, val) BE RETURN         // for redefinition


AND writetype(ty, val) BE
$(  LET string = VALOF
    SWITCHON ty INTO
    $(  CASE type.none:    RESULTIS "undefined  "
        CASE type.const:   RESULTIS "const  %X4"
        CASE type.var:     RESULTIS "var    %X4"
        CASE type.lab:     RESULTIS "lab    %X4"
        CASE type.ref:
             TEST val=#XFFFF THEN
                           RESULTIS "ref  UNUSED"
             ELSE
                           RESULTIS "ref   @%X4"
        CASE type.text:
             val := val % 0
                           RESULTIS "text %I3 ch"
        DEFAULT:
             printtype(ty, val)                    // User defined type
             RETURN
    $)
    writef(string, val)
$)



AND printtype(ty,val) BE writef("%I5 %I5",ty, val)     // for redefinition





AND printlabs() BE
$(  // This procedure completely rewritten to print symbols out in
    // alphabetic order from the hash table (previous data structure
    // was a tree).
    //    16.07.81
    LET number.of.symbols = 0
    LET chpos             = 0

    //Count entries in hash table
    FOR anchor = hash.table TO hash.table + hash.table.size DO
    $(  LET entry = ! anchor

        WHILE entry \= null DO
        $(  LET line.no = t.info ! entry
            IF allsyms | file.number(@line.no) = 0 |
               (t.type ! entry & flag.double) \= 0 THEN
               number.of.symbols := number.of.symbols + 1
            entry := t.link ! entry
        $)

    $)

    UNLESS number.of.symbols = 0 THEN
    $(  LET list     = concatenate.hash.list()   // Get all symbols
        LET rels     = FALSE     // flags for particular key items
        LET defs     = FALSE
        LET weakrefs = FALSE

        // Now sort the symbol list by calling sort.list
        $(  LET compare(elem1, elem2) = compstring(elem1 + t.str, elem2 + t.str)
            sort.list(@list, null, null, t.link, compare)
        $)

        // reserve number of lines necessary for cross reference and for
        // key to symbols that are to be used
        neads(number.of.symbols/((pw+5)/(32+3))+
                          (mode=relative->1,0)+1+3+(badlabs>0->1,0)+1+1)

        // neads will have set 'restartpage' if there are not enough lines to
        // the current output page.
        // the 'wrch' will only write a page throw if 'throws' is TRUE
        IF restartpage THEN wrch('*P')

        writef("*N*NNames available in section %N:*N*N",sectno)
        WHILE list \= null DO
        $(  LET line     = t.info ! list
            LET file.no  = file.number(@line)
            LET typecode = t.type ! list
            LET string   = t.str + list
            MANIFEST  $(  outfield = 32  $)
            IF chpos+outfield>pw THEN
            $(  wrch('*N')
                IF restartpage THEN wrch('*P')
                chpos:=0
            $)
            IF allsyms | file.no=0 | (typecode&flag.double)\=0 THEN
            $(  chpos:=chpos+outfield
                writef((file.no =  0 -> "   ",
                        file.no < 10 -> "+%I1 ",
                                        "+%I2"), file.no)
                writef("%I4 ",line)
                FOR i=1 TO 9 DO wrch(string%0 >= i -> string%i, ' ')
                wrch('*S')
                writetype(typecode&type.mask, t.val ! list)
                wrch((typecode&flag.double) = 0 -> '*S','**')
                wrch((typecode&flag.rel)    = 0 -> '*S','R')
                wrch((typecode&flag.def)   \= 0 -> 'D',
                     (typecode&flag.weak)  \= 0 -> 'W','*S')
                rels:=rels | (typecode & flag.rel)\=0
                TEST (typecode & flag.def)=0 THEN
                    IF (typecode & flag.weak)\=0 THEN weakrefs:=TRUE
                ELSE defs:=TRUE
                UNLESS chpos+2>pw THEN
                $(  writes("  ")
                    chpos:=chpos+2
                $)
            $)
            list := t.link ! list
        $)

        writes("*N*N*N")

        IF badlabs>0 THEN writes("** - multiply defined label*N")
        IF rels      THEN writes("R - relocatable value*N")
        IF defs      THEN writes("D - internally defined value*N")
        IF weakrefs  THEN writes("W - weak reference*N")
        wrch('*N')
    $)
$)



AND deletelabs() BE
$(  LET entry = concatenate.hash.list()   // Get all symbols
    WHILE entry \= null DO                // and run down list deleting each
    $(  deletetype(t.type ! entry & type.mask, t.val ! entry)
        entry := t.link ! entry
    $)
    hash.table := 0                       // Table will be freed, so mark it
$)                                        // so that a new one will be obtained





AND concatenate.hash.list() = VALOF
//   Concatenate all the hash lists together and hang them from
// the normally unused last hash table slot.
//   The routine may be called more than once on the same table
//   The value returned is the address of the first list element.
$(  // The <HASH conditional compilation comments are used to collect
    // statistics about the hashing algorithm.
/*<HASH statistics
   LET size.incidence.vec = VEC 5
   LET max.chain.size     = 0
/*HASH>*/

   IF hash.table ! hash.table.size \= null THEN
   // List concatenated already, just return it
RESULTIS hash.table ! hash.table.size

/*<HASH
   FOR i = 0 TO 5 DO size.incidence.vec ! i := 0
/*HASH>*/

   FOR anchor = hash.table TO hash.table + hash.table.size - 1 DO
   $(
/*<HASH
      LET count = 0
/*HASH>*/

      IF ! anchor \= null THEN
      $(  LET entry = ! anchor
/*<HASH
          count := 1
/*HASH>*/
          WHILE t.link ! entry \= null DO
          $(  // Find the last element in the chain
              entry := t.link ! entry
/*<HASH
              count := count + 1
/*HASH>*/
          $)

          // Now link the old chain onto the end
          // and put the new chain in the complete chain head
          t.link ! entry               := hash.table ! hash.table.size
          hash.table ! hash.table.size := ! anchor
          ! anchor := null
      $)

/*<HASH
        IF count > max.chain.size THEN max.chain.size := count
        IF count >= 5 THEN count := 5
        size.incidence.vec ! count := size.incidence.vec ! count + 1
/*HASH>*/
    $)

/*<HASH
    writef("Hash table size: %N,  chain length incidences:*N", hash.table.size)
    FOR i = 0 TO 4 DO writef("  %N:  %N*N", i, size.incidence.vec ! i)
    writef(">=5:  %N*N*
           *longest: %N*N", size.incidence.vec ! 5, max.chain.size)
/*HASH>*/
    RESULTIS hash.table ! hash.table.size
$)



AND gen.defs() BE
// goes through the dictionary generating external/internal reference
// records in the output format for the relevent variables (REF & DEF)
$(  code.gen(cd.clear)
    FOR anchor = hash.table TO hash.table + hash.table.size DO
    $(  LET t = ! anchor
        WHILE t\=null DO
        $(  LET typecode = t.type ! t
            LET value    = t.val ! t
            LET string   = t.str + t

            IF (typecode&flag.double)\=0 THEN badlabs:=badlabs+1
            TEST (typecode & type.mask)=type.ref THEN
                IF (typecode&flag.needs)\=0 | value\=#XFFFF THEN
                    // i.e. never used!
                    code.gen( ((typecode&flag.weak)=0->cd.ext,cd.wext),
                              string, value /* last referance */)
            ELSE
               IF (typecode & flag.def)=flag.def &
                   (typecode&type.mask)\=type.none THEN
                   TEST (typecode & flag.rel)=0 & mode=relative
                   THEN code.gen(cd.absint, string, value /* address */)
                   ELSE code.gen(cd.int, string, value /* address */)
            t := t.link ! t
        $)
    $)
$)



AND sort.list(first.elem.ptr, last.elem.link, null.elem, link.field, compare) BE
//
//   Sort the list a pointer to the first element of which is given.
//
//   The list is single linked through the given link field and
// the null element reference is given in 'null.elem'.
//
//   The list is sorted using a user supplied comparison routine.
//
//   The comparison routine should accept 2 list elements as arguments.
// Sorting is such that for any two adjacent elements En and En+1 in
// the final list:
//                  compare(En, En+1) <= 0
//
//   For any two elements where compare(Em, En) = 0, their order in the
// final list will be the same as in the original list.
//
//   WARNING.    This routine uses the quicksort algorithm so
//               will be poor for an already-sorted list.
//
//   The first element of the sorted list will be inserted into
// the first element pointer, and 'last.elem.link' assigned to
// the link field of the last element of the sorted list.

WHILE TRUE DO
$(  LET first.elem  = ! first.elem.ptr
    LET sublist1    = ?
    LET sublist2    = ?
    LET length.diff = ?

    IF first.elem = null.elem THEN                 // Empty list
    $(  !first.elem.ptr := last.elem.link
        RETURN
    $)

    sublist1 := link.field ! first.elem
    sublist2 := null.elem
    IF sublist1 = null.elem THEN
    $(  link.field ! first.elem := last.elem.link
        RETURN
    $)

    $(  LET entry         = sublist1
        LET prev.entry    = @sublist1 - link.field
        LET sublist2.tail = @sublist2 - link.field

        length.diff := 0

        // Now pass through sublist1 removing all elements larger than
        // first.elem and placing them on sublist2.
        WHILE entry \= null.elem DO
        $(  TEST compare(entry, first.elem) > 0 THEN
            $(  LET next.entry = link.field ! entry

                // Remove element from sublist1
                link.field ! prev.entry := next.entry

                link.field ! sublist2.tail := entry     // Append to sublist2
                link.field ! entry         := null.elem
                sublist2.tail              := entry

                entry := next.entry
                length.diff := length.diff - 1
            $) ELSE
            $(  prev.entry := entry
                entry      := link.field ! entry
                length.diff := length.diff + 1
            $)
        $)
    $)

    // Now sort the shorter of the two sublists and
    // loop round to deal with the longer.
    TEST length.diff > 0 THEN
    $(  link.field ! first.elem := sublist2

        sort.list(first.elem + link.field, last.elem.link,
                               null.elem, link.field, compare)

        last.elem.link   := first.elem
        ! first.elem.ptr := sublist1
    $) ELSE
    $(  ! first.elem.ptr := sublist1

        sort.list(first.elem.ptr, first.elem,
                               null.elem, link.field, compare)

        first.elem.ptr   := first.elem + link.field
        ! first.elem.ptr := sublist2
    $)
$)
.





SECTION "asm3"




/*<RSX
GET "libhdr"
GET "asmhdr"
/*RSX>*/


/*<CAP
GET ".**.l.bcpl.libhdr"
GET ".**.cgg.asmhdr"
/*CAP>*/

/*<IBM
GET "LIBHDR"
GET "ASMHDR"
/*IBM>*/

//*<TRIPOS:
GET "libhdr"
GET ":Gray.bcpl.asmhdr"
/*TRIPOS>*/



















//
//                    Loader   Output   Module
//






/*       This module is responsible for providing facilities for the
   generation of a load file -- The default format of such a file is
   an extension of Mostec style Intel hex  (see the Mostec Z80 assembler
   reference manual appendix C for a complete definition) called Cambridge Hex.
   Cambridge Hex is defined elsewhere.
         The chosen format is generally applicable and will be generalised
   for use on 16 bit machines.  It can be changed by assigning 'code.gen'
   to some other procedure which supports the same entry reasons (see
   'intel.hex'.
         'putword' and 'putaddress' can be used for the direct generation
   of absolute code.  'putlabelspec' has a more complicated argument and will
   generate external, relocatable or absolute data.  The object taken as an
   argument is generated in a vector of size 'spec.size' in the complimentary
   procedure 'labelexpression' declared in another module.
      26.02.80
*/





/*     The following are kept for reference & documentation
GLOBAL
$(     binbuf   : ioman+3       //  buffer for binary produced by that line
       pc       : ioman+6       //  contains expected value of program counter
$)
*/






LET fitsmask(n,m) = (n>=0 -> (n&\m)=0, (\(n&\m|m))=0 )


LET putword(n) BE
$(  // bytesperasmword is the number of bytes in the object machine's word
    // (i.e. the length of information corresponding to an increment in
    // address).  'bytesperword' is the number of bytes in a word on the
    // machine that we are executing on.
    LET m=(1<<(bytesperasmword*8))-1
    IF binbuf!b.nextmod=binbuf!b.top DO
       $(  IF binbuf!b.top >= binbufsize+b.nextmod DO error(e.binfull)
           binbuf!b.top:=binbuf!b.top+1
       $)
    binbuf!b.nextmod:=binbuf!b.nextmod+1
    binbuf!(binbuf!b.nextmod):=n & m
    UNLESS fitsmask(n,m) DO warn(e.badbyte)
$)



AND putaddress(n) BE
$(  LET m=(1<<(8*bytesperasmword))-1
    LET sign=n&\((1<< (8*wordsperaddress*bytesperasmword) )-1)
    FOR i=1 TO wordsperaddress DO
    TEST ~msbytefirst THEN
    $(  putword(m&n)
        m:=m << (8*wordsperaddress)
    $) ELSE
    putword((n >> (8*bytesperasmword*(wordsperaddress-i))) & m)
    UNLESS sign=0 | sign=(-1&(1 << (8*wordsperaddress*bytesperasmword) )-1) THEN
    error(e.badbyte)
$)



AND movebinword(places) BE
/*     This procedure will be rarely used:  it repositions the pointer to the
   binary buffer of bytes generated so far on this line 'binbuf' so that
   reference can be made to bytes formerly generated.
*/
TEST b.nextmod<binbuf!b.nextmod+places<=binbuf!b.top
THEN binbuf!b.nextmod:=binbuf!b.nextmod+places
ELSE error(e.mtbuf)



AND outhex(n) BE FOR i=bytesperasmword-1 TO 0 BY -1 DO
   writehex((n>>(i*8)) & byte1, 2)





AND code.gen(type, a1, a2) =

//*<CHEX
    intelhex(type, a1, a2)
/*CHEX>*/

/*<TIMBIN
    timbin(type, a1, a2)
/*TIMBIN>*/

// this routine can be redefined in the user's part of the assembler
// to generate a different output format, the 'type's are as follows:
//    cd.undef   sets undefined byte filler
//    cd.clear   clears internal buffers
//    cd.newpc   changes loader's pc to 'arg1'
//    cd.data    outputs data byte 'arg1'
//    cd.eof     tidies up loader file (binout)
//    cd.int     declares internal symbol 'arg1'
//               at address 'arg2'
//    cd.ext     declares external symbol 'arg1'
//               last used at address 'arg2'
//    cd.rel     declares word at address 'arg1'
//               as relocatable
//    cd.module  declares module name 'arg1'
//    cd.code    declares code type 'arg1' with
//               'arg2' bytes per word
//    cd.absint  declares absolute internal symbol
//               'arg1' at address 'arg2'
//    cd.wext    declares weak external symbol
//               'arg1' last used at address 'arg2'
//    cd.opt     linker option string 'arg1'
//               interpreted at level 'arg2'
//    cd.prag    name of pragmat is 'arg1' text is
//               'arg2'



//*<CHEX
AND intelhex(type, arg, arg1) BE
// this procedure generates "Cambridge Hex", see definition.
// It is compatable with "Intel Hex" if records begining '$' are
// taken as comments, as recomended.  Hence the code output
// should be able to be loaded by any standard Intel Hex loader.
// (this only applies to 8 bit machines).
UNLESS binfile=0 | pass=first THEN
$(
    LET hex=TABLE 0,  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
                      0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0
    LET rel=TABLE 0,  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
                      0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0
    STATIC
    $(  beginblock = 0     // begining of next block for map of store
        oldpc = -1         // calculated value of current pc
        pc.valid = FALSE   // FALSE until first pc is set
        first.valid = FALSE// last pc was first valid pc set
        overflowing = FALSE// TRUE when about to run out of address space
    $)
    LET alpharec(str, type, info) BE
    $(  // generates a checksummed record with string 'str' in the field
        // from columns 2 to 5 with record type 'type'.
        // 'info' represents the information necessary for the body of the
        // record.
        LET sum = type+(info&byte1)+((info&byte2) >> 8)    // checksum
        selectoutput(binfile)
        IF str=0 THEN str:="NoName"
        wrch('$')
        FOR i=1 TO 6 DO
        $(  LET ch=(i>str%0 -> '*S', str%i)
            wrch(ch)
            sum:=sum+cvtchar(ch)
        $)
        TEST type=cd.code   THEN writef("%X2%X4", type, info) ELSE
        TEST type=cd.module THEN writef("%X2%X2", type, info) ELSE
        $(  LET b=8*bytesperasmword
            LET m=(1<<b)-1
            writehex(type, 2)
            FOR i=wordsperaddress-1 TO 0 BY -1 DO
            outhex((info>>(i*b))&m)
        $)
        outhex(-sum)
        wrch('*N')
    $)
    LET clearbuf(hex, type) BE
    $(  // clears one of the buffers kept in this routine ('hex' or 'rel').
        // this buffer is pointed to by 'hex'.
        // generates a data or relocating information record depending upon
        // 'type'.
        LET blocklen=(type=cd.data -> hex!0, hex!0/2)
        selectoutput(binfile)
        IF pc.valid | blocklen>0 THEN
        $(  IF blocklen\=0 | type=cd.data THEN
            $(  LET blockaddr=(type=cd.data -> oldpc-blocklen, 0)
                LET sum=blocklen+(blockaddr >> 8)+(blockaddr & #XFF)+type
                wrch(type=cd.data -> ':','$')
                writehex(blocklen,2)
                writehex(blockaddr, 4)
                writehex(type, 2)
                FOR i=1 TO hex!0 DO
                $(  sum:=sum+hex!i
                    outhex(hex!i)
                $)
                outhex(-sum)
                wrch('*N')
            $)
            hex!0:=0
        $)
    $)
    LET saveout=output()
    LET savewrch=wrch
    wrch:=syswrch
    SWITCHON type INTO
    $(  CASE cd.undef:
             // 'arg' is the value for the byte used for undefined output
             ENDCASE
        CASE cd.clear:
            // clear internal buffers
            UNLESS hex!0=0 THEN clearbuf(hex, cd.data)
            clearbuf(rel, cd.rel)
            selectoutput(saveout)
            ENDCASE
        CASE cd.newpc:
            // program counter has changed
            UNLESS oldpc=arg THEN
            $(  TEST pc.valid THEN
                $(  LET s1=(arg>=0 -> +1,-1)
                    LET s2=(oldpc>=0 -> +1,-1)
                    IF hex!0>0 | (s1=s2->s1*(arg-oldpc),s2)<0 | first.valid THEN
                    clearbuf(hex, cd.data)
                    first.valid:=FALSE
                $) ELSE first.valid:=TRUE
        //      UNLESS mapfile=0 THEN
        //        $(  selectoutput(mapfile)
        //            writef("%X4 - %X4  (%N)*N",beginblock,
        //                   oldpc,oldpc-beginblock)
        //            beginblock:=arg
        //        $)
                oldpc:=arg
                selectoutput(saveout)
            $)
            pc.valid := TRUE
            ENDCASE
        CASE cd.data:
            $(  // output 'arg' to the intel hex file
                IF overflowing THEN error(e.overflow)
                hex!0:=hex!0+1
                hex!(hex!0):=arg
                overflowing := (oldpc = #XFFFF)
                // #XFFFF should be the following really:
                // (1 << (bytesperasmword * wordsperaddress)) - 1
                oldpc := oldpc + 1
                IF hex!0>=hexoutwidth THEN
                $(  clearbuf(hex, type)
                    selectoutput(saveout)
                $)
            $)
            ENDCASE
        CASE cd.eof:
            // arg is the programs start address
            IF overflowing & hex!0=0 THEN oldpc := #XFFFF
            clearbuf(hex, cd.data)
            clearbuf(rel, cd.rel)
            writef(":00%X401", arg)
            outhex( -(1+(arg&byte1)+((arg&byte2)>>8)) )
            wrch('*N')
            selectoutput(saveout)
            ENDCASE
        CASE cd.int:
        CASE cd.absint:
        CASE cd.wext:
        CASE cd.ext:
            // 'arg' holds the name of the symbol and:
            // 'arg1' is the address of the internal symbol, or
            // 'arg1' is the last symbol that used the external symbol
            alpharec(arg, type, arg1)
            selectoutput(saveout)
            ENDCASE
        CASE cd.rel:
            // store 'arg' as address in 'rel' buffer for relocation info
            rel!0 := rel!0+2
            rel!(rel!0-1) := (arg&byte2) >> 8
            rel!(rel!0)   := (arg&byte1)
            IF rel!0>=hexoutwidth THEN
            $(  clearbuf(rel, cd.rel)
                selectoutput(saveout)
            $)
            ENDCASE
        CASE cd.module:
            // 'arg' is TRUE if module is absolute FALSE otherwise,
            // 'arg1' is the name of the new module
            clearbuf(hex, cd.data)
            clearbuf(rel, cd.rel)
            alpharec(arg1, cd.module, (arg->0,1))
            pc.valid := FALSE
            selectoutput(saveout)
            ENDCASE
        CASE cd.code:
            // 'arg' is the name of the object machine
            // 'arg1' is packed as follows:
            //          bits 15 - 12:  bytes per addressable unit
            //          bits 11 -  8:  bytes per address
            //          bits 7  -  0:  flags (bit 0 = MSB first)
            UNLESS hex!0=0 THEN clearbuf(hex)
            clearbuf(rel, cd.rel)
            alpharec(arg, cd.code, arg1)
            selectoutput(saveout)
            ENDCASE
        CASE cd.opt:
            // 'arg' is the option string to be given
            // 'arg1' is the level of the option
            $(  LET sum= -type-(arg1 & #XFF) - arg%0
                selectoutput(binfile)
                FOR i=1 TO arg%0 DO sum:=sum-cvtchar(arg%i)
                writef("$%X2%X20011%S%X2*N",arg%0,arg1,arg,sum)
                selectoutput(saveout)
            $)
            ENDCASE
        CASE cd.prag:
            // 'arg' is the name of the pragmat begin given
            // 'arg1' is the pragmat string being generated
            $(  LET sum= -type-arg%0
                selectoutput(binfile)
                FOR i=1 TO arg%0 DO sum:=sum-cvtchar(arg%i)
                wrch('$')
                FOR i=1 TO 6 DO
                $(  LET ch=(i>arg1%0->'*S',arg1%i)
                    sum:=sum-cvtchar(ch)
                    wrch(ch)
                $)
                writef("%X2%X2%S%X2*N",type,arg%0,arg,sum)
                selectoutput(saveout)
            $)
            ENDCASE
        DEFAULT:
            error(e.interror, 8)
    $)
    wrch:=savewrch
$)
/*CHEX>*/




/*<TIMBIN
AND timbin(type, arg, arg1) BE
// this procedure generates "TIMBIN".
// It is roughly compatable with TRIPOS binary, see definition.
UNLESS binfile=0 THEN
$(  MANIFEST $(  relbuf.size = 32  $)
    LET rel=TABLE 0,  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
                      0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0
    STATIC
    $(  oldpc = -1            // calculated value of current pc
        pc.valid = FALSE      // FALSE until first pc is set
        first.valid = FALSE   // last pc was first valid pc set
        absolute.module=TRUE  // TRUE when the current module is absolute
        block.len = 0         // length of current data block
        block.list = null     // list of data block lengths
        block.list.top = null // top entry in the block list
        undef = 0             // value of undefined bytes
        last.end.out = 0      // last address in last block output
        pending.byte = -1     // -1 if no byte pending otherwise the byte
    $)
    LET abs.cmp(v1, v2) = VALOF
    $(  LET s1 = (v1>=0 -> +1, -1)
        LET s2 = (v2>=0 -> +1, -1)
        RESULTIS (s1=s2 -> s1*(v1-v2), s2)
    $)
    LET writeword(word) BE
    $(  wrch(word>>8)
        wrch(word & #XFF)
    $)
    LET clearbuf(rel) BE
    $(  // clears the buffer 'rel'
        // generates a relocating information record
        LET blocklen = rel!0
        IF pc.valid | blocklen>0 THEN
        $(  LET saveout = output()
            selectoutput(binfile)
            IF blocklen\=0 THEN
            $(  LET base = (block.list.top=null -> 0, block.list.top!2)
                writeword(1010)
                writeword(blocklen)
                FOR i=1 TO rel!0 DO writeword(rel!i-base)
            $)
            rel!0:=0
            selectoutput(saveout)
        $)
    $)
    LET new.buf() BE
    $(  // record new block
        TEST block.list.top=null THEN   // list it empty
        $(  block.list.top := simplegetvec(2)
            block.list := block.list.top
        $) ELSE
        $(  block.list.top!0 := simplegetvec(2)
            block.list.top := block.list.top!0
        $)
        block.list.top!0 := null
        block.list.top!1 := block.len
        block.list.top!2 := oldpc-block.len
        block.len := 0
    $)
    LET end.block() BE
    UNLESS block.list.top=null THEN
    $(  IF pending.byte>=0 THEN
        $(  LET saveout = output()
            selectoutput(binfile)
            writeword(absolute.module -> 1003, 1000)
            IF absolute.module THEN
            writeword(block.list.top!1 + block.list.top!2 - 1)
            writeword(1)
            writeword((pending.byte<<8) | (undef & #XFF))
            selectoutput(saveout)
            pending.byte := -1
        $)
        block.list.top := block.list.top!0
    $)
    LET begin.block() BE
    UNLESS block.list.top=null THEN
    IF pc.valid | block.list.top!1>0 THEN
    $(  LET blockaddr = block.list.top!2
        LET fore.block = (\absolute.module &
                          abs.cmp(blockaddr, last.end.out) > 0 ->
                          blockaddr - last.end.out, 0) +
                        (pending.byte>=0 -> 1,0)
        LET blocklen = block.list.top!1 + fore.block
        blockaddr := blockaddr - fore.block
//      writef("TIMBIN: next block %X4 - %X4 (fore block size %X4)*N",
//            blockaddr, blockaddr+blocklen, fore.block)
        UNLESS blocklen=0 & \absolute.module THEN
        $(  // allow zero length records for absolute records
            // (since they provide the information that the assembler
            // has visited that location) but not for relocatable
            // ones (since there is not location field).
            LET saveout = output()
            selectoutput(binfile)
            writeword(absolute.module -> 1003, 1000)
            IF absolute.module THEN writeword(blockaddr)
            writeword(blocklen/2)
            UNLESS absolute.module THEN
            $(  block.list.top!1 := blocklen
                block.list.top!2 := blockaddr  // to account for FORE.BLOCK
                IF pending.byte>=0 THEN wrch(pending.byte)
                FOR i=1 TO (fore.block & (\1)) - (pending.byte>=0 -> 1, 0) DO
                wrch(undef)
                pending.byte := ((fore.block&1)=1 -> undef, -1)
            $)
            last.end.out := blockaddr+blocklen
            selectoutput(saveout)
        $)
    $)
    LET savewrch=wrch
    wrch:=syswrch
    TEST pass=first THEN
    SWITCHON type INTO
    $(  CASE cd.undef:
            // 'arg' holds value for byte used for undefined code
            undef := arg
            ENDCASE
        CASE cd.clear:
            // clear internal buffers
//          writef("TIMBIN: line %i4 CLEAR*N", line.of.file)
            new.buf()
            rel!0 := 0
            ENDCASE
        CASE cd.newpc:
            // program counter has changed
//          writef("TIMBIN: line %I4 NEWPC %X4 (old PC = %X4)*N",
//                 line.of.file, arg, oldpc)
            UNLESS oldpc=arg THEN
            $(  TEST pc.valid THEN
                $(  LET s1=(arg>=0 -> +1,-1)
                    LET s2=(oldpc>=0 -> +1,-1)
                    IF block.len>0 | (s1=s2->s1*(arg-oldpc),s2)<0 |
                       first.valid THEN new.buf()
                    first.valid:=FALSE
                $) ELSE first.valid:=TRUE
                oldpc:=arg
                pc.valid:=TRUE
            $)
            ENDCASE
        CASE cd.data:
//          writef("TIMBIN: line %I4 DATA %X2 (buffer size = %N)*N",
//                 line.of.file, arg, block.len)
            block.len := block.len+1
            oldpc:=oldpc+1
            ENDCASE
        CASE cd.rel:
            // store 'arg' as address in 'rel' buffer for relocation info
//          writef("TIMBIN: line %I2 RELDATA %X4 (pc = %X4)*N",
//                 line.of.file, arg, oldpc)
            rel!0 := rel!0+1
            IF rel!0>=relbuf.size THEN
            $(  new.buf()
                rel!0 := 0
            $)
            ENDCASE
        CASE cd.module:
            // 'arg' is TRUE if module is absolute FALSE otherwise,
            // 'arg1' is the name of the new module
//          writef("TIMBIN: line %I4 MODULE*N", line.of.file)
            block.list := null
            block.list.top := null
            block.len := 0
            rel!0 := 0
            pc.valid := FALSE
            ENDCASE
        DEFAULT: ENDCASE
    $) ELSE   //  pass two
    SWITCHON type INTO
    $(  CASE cd.clear:
            // clear internal buffers
            end.block()
            clearbuf(rel)
            begin.block()
            ENDCASE
        CASE cd.newpc:
            // program counter has changed
            UNLESS oldpc=arg THEN
            $(  TEST pc.valid THEN
                $(  IF block.list.top!1>0 | abs.cmp(arg,oldpc) |
                    first.valid THEN
                    $(  TEST absolute.module THEN end.block() ELSE
                        block.list.top := block.list.top!0
                        clearbuf(rel)
                        begin.block()
                    $)
                    first.valid:=FALSE
                $) ELSE first.valid:=TRUE
                oldpc:=arg
                pc.valid:=TRUE
            $)
            ENDCASE
        CASE cd.data:
            $(  // output 'arg' to the intel hex file
                LET saveout = output()
                selectoutput(binfile)
                TEST pending.byte>=0 THEN
                $(  writeword((pending.byte<<8) | (arg&#XFF))
                    pending.byte := -1
                $) ELSE pending.byte := arg
                selectoutput(saveout)
                oldpc:=oldpc+1
            $)
            ENDCASE
        CASE cd.eof:
            $(  // arg is the programs start address
                LET saveout = output()
                selectoutput(binfile)
                writeword(1002)
                selectoutput(saveout)
            $)
            ENDCASE
        CASE cd.int:
        CASE cd.absint:
        CASE cd.wext:
        CASE cd.ext:
            // 'arg' holds the name of the symbol and:
            // 'arg1' is the address of the internal symbol, or
            // 'arg1' is the last symbol that used the external symbol
            error(e.badloaddir)
            ENDCASE
        CASE cd.rel:
            // store 'arg' as address in 'rel' buffer for relocation info
            rel!0 := rel!0+1
            rel!(rel!0)   := arg
            IF rel!0>=relbuf.size THEN
            $(  block.list.top := block.list.top!0
                clearbuf(rel, cd.rel)
                begin.block()
            $)
            ENDCASE
        CASE cd.module:
            // 'arg' is TRUE if module is absolute FALSE otherwise,
            // 'arg1' is the name of the new module
            absolute.module := arg
            pc.valid := FALSE
            last.end.out := 0             // address of end of last block
            block.list.top := block.list  // rewind to begining of the list
            begin.block()
            ENDCASE
        CASE cd.code:
            // 'arg' is the name of the object machine
            // 'arg1' is the number of bytes per word used (1)
            ENDCASE
        CASE cd.opt:
            // 'arg' is the option string to be given
            // 'arg1' is the level of the option
            error(e.badloaddir)
            ENDCASE
        CASE cd.prag:
            // 'arg' is the name of the pragmat begin given
            // 'arg1' is the pragmat string being generated
            ENDCASE
        DEFAULT:
            error(e.interror, 8)
    $)
    wrch:=savewrch
$)
/*TIMBIN>*/




AND putlabelspec(spec) BE
$(  /*    'spec' is a 3 element vector which is the result of parsing
       some label expression (see 'label.expression').  Offset 0 contains
       the value obtained.  Offset 1 can be 0 or 1 (and other values if
       the parsed expression was illegal or an external), 0 implies that
       the value is absolute.  Offset 2 is zero unless the expression was
       an external in which case it will contain the address of the external
       symbol's value field.
    */
    LET local.pc = pc+binbuf!b.top-b.nextmod
    TEST spec!2=0 THEN putaddress(spec!0) ELSE putaddress(!(spec!2))
    IF spec!1\=0 /* i.e. relocatable or external */ THEN
    TEST spec!2=0 /* not external */ THEN code.gen(cd.rel, local.pc)
    ELSE !(spec!2) := local.pc
$)









.
SECTION "asm4"




/*<RSX
NEEDS "TITLE"
GET "libhdr"
GET "asmhdr"
/*RSX>*/


/*<CAP
GET ".**.l.bcpl.libhdr"
GET ".**.cgg.asmhdr"
/*CAP>*/

/*<IBM
GET "LIBHDR"
GET "ASMHDR"
/*IBM>*/

//*<TRIPOS:
GET "libhdr"
GET ":Gray.bcpl.asmhdr"
/*TRIPOS>*/





//
//                  Character  Input  and  Output
//





/*  The following are kept for reference:
GLOBAL
$(     linbuf   : ioman+1       //  buffer for text of assembly line
       linbuf.out:ioman+2       //  next char no to be taken from linbuf
       ch       : ioman+5       //  last character read from the input file
       pagetitle: ioman+7       //  title string for the top of the page
       linbuf.top:ioman+8       //  top of line buffer pointer
$)
*/




STATIC $( saved.rch = 0 $)





LET deleteline() BE
$(  linbuf.top := 0
    linbuf.out := 0
$)


AND putch(ch) BE
UNLESS linbuf.top >= linbufsize DO
   $(  linbuf.top := linbuf.top + 1
       linbuf.out:=linbuf.out+1
       linbuf%(linbuf.top) := ch
   $)



AND other.rch() BE
$(  // this routine is used for 'rch' when tabs are being expanded.
    // having a separate routine saves a test on a global for each
    // call of 'rch': this results in a significant difference in speed.
    // the number of times that '*S' must be returned is kept in
    // 'linbuf.out' and is decremented on each call.  The original 'rch'
    // is reenstated when the tab has been fully expanded. (the old
    // 'rch' is saved in the pointer which originally held this routine
    // 'other.rch'!
    ch:='*S'
    linbuf.out:=linbuf.out-1
    IF linbuf.out<=0 THEN
    $(  rch := saved.rch
        saved.rch := other.rch
    $)
$)



AND rch() BE
$(  // the assembler spends a significant amount of time in this routine so
    // nothing must be done to slow it down.  It uses 'rdch' to read characters
    // and it accumulates them in the buffer 'linbuf', a pointer to the current
    // character of which is held in 'linbuf.top'.
    // Tabs are tested for and expanded on input: this being the proper place
    // to do them!
    // 'linbuf' is not used for any other purpose other than printing out in
    // the listing.  Hence its truncation when 'linbuf.top' reaches the set
    // value of 'linbufsize' represents the truncation of the listing set by
    // the P option.  (see initialisation in which 'linbufsize' is calculated
    // in terms of this value).
    ch:=rdch()
    UNLESS linbuf.top>=linbufsize DO
    $(  linbuf.top := linbuf.top + 1
        linbuf%(linbuf.top):=ch
    $)
    IF ch='*T' DO
    $(  LET endchno=?
        LET newlen=?
        linbuf.out:= 7 - ((linbuf.top-1) REM 8)
        newlen:=linbuf.top+linbuf.out
        endchno:=(newlen>linbufsize -> linbufsize, newlen)
        FOR i=linbuf.top TO endchno DO linbuf%i:='*S'
        linbuf.top:=endchno
        ch:='*S'
        UNLESS linbuf.out=0 THEN
        $(  saved.rch:=rch
            rch:=other.rch
        $)
    $)
$)


AND error(arg1,arg2,arg3,arg4) BE
$(

    /*  Note:  THIS  PROCEDURE  PRODUCES  A  DYNAMIC  GOTO
               SO  THAT  EXECUTION  CONTINUES
               AT  "ERRORLABEL"  ON  EXIT
    */

    // This routine is called either because there has been a 'warning'
    // or because there was a genuine error.  The difference being that
    // in a warning, the arguments are kept in 'warnvec' until a line is
    // assembled and then used in 'error' so that the normal processing
    // of the line is not affected.
    // If the routine is called as an 'error' when a warning is already
    // outstanding it is the warning that is printed: only one message
    // ever gets printed for any particular line.
    // If 'arg1' (the error code) is negative the error is judged to be
    // fatal and the flag 'finishpass' is set TRUE to terminate the
    // processing of the assembly.  It also writes a message to the initial
    // output stream of the program.
    // Error messages for the error codes are given by 'geterror'
    // This routine and 'outline' are the main two routines which write
    // to the output listing file.
    LET pos=item.start          // forward reference
    LET crosses = "**************************  "
    errcount:=errcount+1
//  writef("jumping to level %N, to label %N*N",errorlevel,errorlabel)
    TEST arg1<0
    THEN $(  finishpass:=TRUE
             fatal:=TRUE
         $)
    ELSE IF warning DO
            $(  arg1:=warnvec!1
                arg2:=warnvec!2
                arg3:=warnvec!3
                arg4:=warnvec!4
                warning:=FALSE
                pos:=warnvec!0
//              writef("Error is warning , pos=%N*N",pos)
            $)
    TEST pass=first & \finishpass
    THEN outline('*S')
    ELSE $(  LET savelist=list
             LET charsperasmword = 2*bytesperasmword + 1
             LET error.message = geterror(arg1)
             neads(short->2,3)
             IF restartpage THEN wrch('*P')
             UNLESS fatal DO
                $(  list:=1
                    outline('**')
                    list:=savelist
                $)
             TEST pass=first
             THEN writes("*N*N*N")
             ELSE UNLESS short DO
                     $(  writes(crosses)
                         FOR i=1 TO binbufwidth*charsperasmword+2+pos
                         DO wrch('*S')
                         writes("|*N")
                     $)
             writes(crosses)
             FOR i=1 TO binbufwidth*charsperasmword+2 DO wrch('*S')
             writef("%C Error: ",comntch)
             TEST error.message=0 THEN
             writef("number %N") ELSE
             writef(error.message,arg2,arg3,arg4)
             wrch('*N')
             IF finishpass DO
                $(  LET saveout=output()
                    selectoutput(out)
                    writef("*N*NFatal error (file %N line %N): ",
                           file.id, line.of.file)
                    TEST error.message = 0 THEN
                    writef("number %N", -arg1) ELSE
                    writef(error.message,arg2,arg3,arg4)
                    wrch('*N')
                    selectoutput(saveout)
                $)
         $)

    longjump(errorlevel,errorlabel)

$)


AND warn(arg1, arg2, arg3, arg4) BE UNLESS warning DO
   $(  warning:=TRUE
       warnvec!1:=arg1
       warnvec!2:=arg2
       warnvec!3:=arg3
       warnvec!4:=arg4
       warnvec!0:=item.start
//     writef("Warn called: warnvec!0=%N*N",warnvec!0)
   $)


//  AND executeanywarning() BE IF warning DO error(0)





AND outline(firstchar) BE
$(  // This procedure is produces all the lines of output for a line of
    // assembler input.  It also deals with '*P' and '*N' characters
    // generated by appropriate directives.
    // First the rest of the line is read so that it will all be accumulated
    // in 'linbuf' for output:
    WHILE ('*N'\=ch\=endstreamch) DO rch()
    // don't output anything on the first pass!
    TEST pass=second THEN
    $(  // don't generate any output unless 'list' is positive either
        TEST list > 0 THEN
        $(  LET bufpos=b.nextmod+1
            // this points into 'binbuf', a vector of hex produced by this
            // assembly line.  It is initialised to point at the begining and
            // is later used to advance in increments correspondin to the
            // maximum number of bytes that can be displayed on one line
            // (successive lots being output on successive lines on their own).
            LET firstflag = TRUE
            // This is true only for the first line written out.  The line read
            // into 'linbuf' is only output on the first line, successive lines
            // are marked by their program counter and the hex only.
            LET control.line=?
            // This will be true if the only character on the line is '*P' or
            // '*C' or is empty (except of spaces): in this case the line is not
            // output but either the appropriate number of '*N's output or a new
            // page summoned (by seting 'restartpage')
            LET no.output=(rch=textrch)  // i.e. still in text expansion
            // If the end of the line happens to have fallen in the middle of
            // the expansion of a TEXT variable (see later) the generation of
            // output on this line is with  held until the TEXT expansion is
            // complete: care is taken to ensure that a deletion of the line
            // in one of the "text expansion lines" also results in the
            // deletion of the actual line.
            LET i=1
            // work out if this is a 'control.line':
            WHILE i<=linbuf.top & (linbuf%i='*S' | linbuf%i='*N') DO i:=i+1
            control.line:= i>linbuf.top | linbuf.top=0
            $(rpt
                // The code in this repeat loop generates a line of output
                // Lines continue to be printed until 'bufpos' reaches the
                // end of the hex buffer 'binbuf' (or stops if no output is to
                // be generated anyway).
                IF restartpage & (\control.line | \firstflag) THEN wrch('*P')
                // i.e. there is something to output and 'restart page' is
                // pending so do a page throw!
                UNLESS no.output | restartpage & control.line THEN
                UNLESS firstchar='*S' THEN wrch(firstchar)
                UNLESS no.output | binbuf!b.top<=b.nextmod & control.line THEN
                $(  IF firstchar='*S' THEN
                    // if the first character of this line is "free" and we
                    // are in a file (the number of which we are shortly to
                    // print) print a '+' for a warning.
                    wrch(file.id=0 -> '*S', '+')
                    UNLESS firstflag THEN
                    // keep 'startpc' up to date: it is a global which should
                    // always hold the program counter value at the begining
                    // of this line of output.  (i.e. the address of the first
                    // byte to be generated on or after this line).
                    startpc:=pc+(bufpos-(b.nextmod+1))
                    TEST file.id=0
                    THEN writef("  %I5 ",line.of.file) ELSE
                    // in a GET directive: print number of file
                    TEST file.id<10 THEN
                    writef("%I1  %I4 ",file.id,line.of.file)
                    ELSE writef("%I2 %I4 ",file.id,line.of.file)
                    // don't bother to print program counter on comment lines
                    TEST linbuf%1=comntch |
                         pc=startpc+binbuf!b.top-b.nextmod
                    THEN writes("      ")
                    ELSE writef("%X4  ",startpc)
//                  writef("bufpos = %N, binbufwidth= %N*N",bufpos,binbufwidth)
                    // print out hex generated on this line
                    FOR i=bufpos TO bufpos+binbufwidth-1 DO
                       TEST i<=binbuf!b.top
                       THEN $(  outhex(binbuf!i)
                                wrch('*S')
                            $)
                       ELSE FOR j=1 TO bytesperasmword*2+1
                            DO wrch('*S')
                    bufpos:=bufpos+binbufwidth
                $)
                // echo input line if this is the first line:
                TEST firstflag THEN
                $(  IF (control.line->\restartpage,\no.output) THEN
                    writes("  ")
                    FOR i=1 TO linbuf.top-1 DO
                    IF (control.line->\restartpage,\no.output) THEN
                    wrch(linbuf%i)
                    firstflag := FALSE
                    // If 'control.line' is TRUE then there is nothing worth
                    // printing on the line so dont write out the contents of
                    // the line if waiting for output before we start a new
                    // page (i.e. 'restartpage' is TRUE).  Otherwise don't
                    // bother if no output is to be generated anyway
                    // (i.e. 'no.output' is TRUE)
                    IF (control.line->\restartpage,\no.output) THEN wrch('*N')
                $) ELSE wrch('*N')
            $)rpt REPEATUNTIL bufpos>binbuf!b.top | no.output
            IF control.line | \no.output THEN deleteline()
        $) ELSE deleteline()
        // tell loader format generator what our current program counter is:
        IF binbuf!b.top>b.nextmod THEN code.gen(cd.newpc, pc)
        // output data:
        FOR i=1+b.nextmod TO binbuf!b.top DO code.gen(cd.data, binbuf!i)
    $) ELSE
    $(  deleteline()
/*<TIMBIN
        // tell loader format generator what our current program counter is:
        IF binbuf!b.top>b.nextmod THEN code.gen(cd.newpc, pc)
        // tell loader how much output data:
        FOR i=1+b.nextmod TO binbuf!b.top DO code.gen(cd.data, binbuf!i)
/*TIMBIN>*/
    $)
    pc:=pc+binbuf!b.top-b.nextmod
    // set 'binbuf' to empty
    binbuf!b.top := b.nextmod
    binbuf!b.nextmod := binbuf!b.top
    ch:='*N'
$)


AND mywrch(c) BE
$(  // This procedure is used to replace the normal BCPL 'wrch': it manages
    // the output listing.  In particular it knows the lenght of a page 'pw'
    // and generates a heading at the top of each page when a '*P' is written.
    // When a '*P' is necessary as the next character (if there is any more
    // output) the flag 'restartpage' is SET.
    // It is up to other parts of the program to test this flag and write
    // '*P' before generating any non-blank output.  This is necessary in
    // order to prevent a final headed blank page.
    LET savewrch=wrch
    wrch:=syswrch
    SWITCHON c INTO
    $(  DEFAULT: wrch (c)
                 ENDCASE
        CASE '*N':
             outpos:=outpos+1
             wrch(c)
             IF (outpos&byte1)>pl THEN restartpage:=throws
             ENDCASE
        CASE '*P':
             restartpage:=FALSE
             UNLESS throws ENDCASE
             $(  LET len=?
                LET dat = VEC 10
                 dat := get.time(dat, 10)
                 wrch('*P')
                 writef("%S assembler %S",name,dat)
                 len:=10+name%0+1+dat%0
                 UNLESS pagetitle=0 DO
                    $(  FOR i=len TO (pw-pagetitle%0)/2
                        DO $(  wrch('*S'); len := len+1
                           $)
                        writes(pagetitle)
                        len:=len+pagetitle%0
                    $)
                 FOR i=len+1 TO pw-8 DO wrch('*S')
                 writef("Page %I3*N*N*N",(outpos&byte2)>>8)
                 outpos:= (outpos&byte2) + (1<<8)
             $)
    $)
    wrch:=savewrch
$)




AND neads(lines) BE
  IF lines+(outpos&byte1)>pl DO  restartpage:=TRUE






.







SECTION "asm5"




/*<RSX
GET "libhdr"
GET "asmhdr"
/*RSX>*/


/*<CAP
GET ".**.l.bcpl.libhdr"
GET ".**.cgg.asmhdr"
/*CAP>*/

/*<IBM
GET "LIBHDR"
GET "ASMHDR"
/*IBM>*/

//*<TRIPOS:
GET "libhdr"
GET ":Gray.bcpl.asmhdr"
/*TRIPOS>*/








//
//                      T E X T   M A C R O S
//


STATIC
$(  text       = null
    text.out   = 0
    text.savch = '*S'
    text.savrch= 0
$)





LET textvar(str) BE
TEST rch=textrch
THEN error (e.badtext)
ELSE UNLESS str%0=0 DO
        $(  text.savch := ch
            text.savrch:= rch
            text.out   := 1
            text       := str
            ch         := text%text.out
            rch        := textrch
        $)



AND textrch() = VALOF
$(  text.out:=text.out+1
    TEST text.out>text%0
    THEN $(  ch   := text.savch
             rch  := text.savrch
         $)
    ELSE ch:=text%text.out
    RESULTIS ch
$)



AND endline() BE outline('*S')


AND newlyne() BE
UNLESS ch=endstreamch DO
   $(  ch:='*S'
       UNLESS rch=textrch THEN
       $(  lineno:=lineno+1
           line.of.file := line.of.file + 1
       $)
       rch()
   $)









//
//                      F I L E    T A B L E
//



/*
GLOBAL $(
//  files: fileman
//  line.of.file
//  fileno
//  max.files
$)
*/


MANIFEST
$(  no.of.files = 0        // offset in 'files' of number of files GOT
    first.line  = 1        // offset in 'files' of number of first line
    first.file  = 2        // offset of first file entry in 'files'
$)




// Line number information has to be kept about each of the files opened
// using the GET directive.  A vector of pointers to information blocks is
// kept.  The information contained in them is the number of the line counter
// 'lineno' at the begining of the file, and when the file has been read, and
// the name of the file.
// GET directives can be nested so calculation of the actual number of the line
// within the current file is a little complex ('file.number')!




LET newfile(s) = VALOF
$(  TEST 0<=files!no.of.files<=max.files THEN
    IF pass=second THEN
    $(  LET newfile=simplegetvec(2+s%0/2)
        files!(files!no.of.files+1) := newfile
        TEST newfile=null THEN error(e.nospace) ELSE
        $(  FOR i=0 TO s%0/2 DO newfile!(2+i):=s!i
            newfile!0 := lineno     // first line
            newfile!1 := lineno     // last line
        $)
    $) ELSE error(e.filexs)
    files!no.of.files := files!no.of.files + 1
    RESULTIS files!no.of.files - first.file + 1
$)


AND endfile(fno) BE
    IF pass=second THEN files!(fno+first.file-1)!1 := lineno    // last line


AND printfiles() BE
IF allsyms DO
   FOR i=first.file TO files!no.of.files DO
   IF files!i \= 0 THEN
   $(  IF restartpage THEN wrch('*P')
       writef("file +%N is %S (%N lines)*N",
              i-first.file+1, files!i+2, files!i!1-files!i!0)
   $)


AND resetfiles(to.line) BE
$(  files!no.of.files:=first.file-1
    files!first.line:=to.line
    file.id:=0
$)



AND file.number(lv.line) = VALOF
$(  LET i=first.file
    LET j=?             // maximum file number included in found file
    LET n=?             // iterates through immediately included files
    LET fmax=files!0
    WHILE i<=fmax & !lv.line>files!i!0 DO i:=i+1
    i:=i-1
    j:=i                // highest number for an included file
    WHILE i>=first.file & !lv.line>files!i!1 DO i:=i-1
    n:=i+1              // first included file
    WHILE n<=j DO
    $(  LET top=files!n!1
        !lv.line := !lv.line - (top-files!n!0)  // subtract size of files
        n:=n+1
        WHILE n<=j & files!n!0<=top DO n:=n+1   // skip files within this one
    $)
    TEST i=first.file-1 THEN !lv.line := !lv.line + files!first.line ELSE
    !lv.line := !lv.line - files!i!0
    RESULTIS i-first.file+1
$)






//
//                         F I E L D S
//



MANIFEST
$(  f.shift = byte1
    f.len   = byte2
    partsize= 8
$)



LET getf(no, field) = VALOF
$(  LET s=(field & f.shift)
    LET l=(field & f.len)>>partsize
    RESULTIS (no>>s) & ((1<<l)-1)
$)


LET putf(no, field, too) = VALOF
$(  LET s=(field & f.shift)
    LET l=(field & f.len) >> partsize
    LET m=(1<<l)-1
    LET ans = (too & \(m<<s)) | ((no & m)<<s)
    UNLESS fitsmask(no, m) DO warn(e.ftoosmall,no)
    RESULTIS ans
$)


AND trim(no, field) = VALOF
$(  LET l=(field & f.len)>>partsize
    LET m=(1<<l)-1
    RESULTIS no&m
$)


AND newf(pos, len) = (pos&byte1) | ((len&byte1)<<partsize)


AND putwordf(no,field) BE
IF pass=second DO
   TEST binbuf!b.top<1
   THEN error(e.mtbuf)
   ELSE binbuf!(binbuf!b.top):=putf(no, field, binbuf!(binbuf!b.top))












//
//                          T R E E S
//



MANIFEST
$(  t.left  = 0
    t.right = 1
    t.str   = 2
    t.val   = 3
    t.size  = 4
$)



LET tree.put(lvtree, name, item) BE
$(rpt
    IF !lvtree=null
    DO $(  LET newitem=simplegetvec(t.size-1)
           newitem!t.left, newitem!t.right:= null, null
           newitem!t.str:=name
           newitem!t.val:=item
           !lvtree:=newitem
           RETURN
       $)
    $(  LET c=compstring((!lvtree)!t.str, name)
        IF c=0 error (e.interror, 7)
        lvtree := !lvtree+(c<0 -> t.left, t.right)
    $)
$)rpt REPEAT



AND tree.get(tree, name) = VALOF
$(rpt
    IF tree=null RESULTIS tree
    $(  LET c=compstring(tree!t.str, name)
        IF c=0 RESULTIS tree
        tree := tree!(c<0 -> t.left, t.right)
    $)
$)rpt REPEAT






//
//                         C O D E    T A B L E
//





MANIFEST
$(  c.size = 0
    c.str  = 0
    c.fn   = 1
    c.opstr= 2
$)


LET code.put(name, proc, len, wd1, wd2, wd3, wd4, wd5) BE
$(  LET l=3+(len>0->len,-len)
    LET c=simplegetvec(l-1)
    c!c.str:=name
    c!c.fn :=proc
    c!(c.opstr+0) := len
    FOR i=c.opstr+1 TO l-1 DO c!i:=(@wd1)!(i-(c.opstr+1))
    tree.put(@codes, name, c)
$)


AND getcode(s) = VALOF
$(  LET t=tree.get(codes, s)
    RESULTIS (t=null->null, t!t.val)
$)


AND compcode(s, c) = (0=compstring(s, c!c.str))


AND initcodes() BE error(e.nocodes)       // for redefinition







//
//                 R E G I S T E R    S Y M B O L S
//



STATIC  $(  syms=null  $)


MANIFEST
$(  s.size  = 0
    s.str   = 0
    s.info  = 1
    r.bad   = 0
$)


LET reg.put(reg, val) BE tree.put(@syms, reg, val)


AND getreg(s) = VALOF
$(  LET t=tree.get(syms,s)
    RESULTIS (t=null->r.bad,t!t.val)
$)

AND initsyms() BE RETURN          //  for redefinition






.








SECTION "asm6"




/*<RSX
GET "libhdr"
GET "asmhdr"
/*RSX>*/


/*<CAP
GET ".**.l.bcpl.libhdr"
GET ".**.cgg.asmhdr"
/*CAP>*/

/*<IBM
GET "LIBHDR"
GET "ASMHDR"
/*IBM>*/

//*<TRIPOS:
GET "libhdr"
GET ":Gray.bcpl.asmhdr"
/*TRIPOS>*/









//
//                    L E X I C A L    A N A L Y S E R
//






/*     These Globals kept for documentation purposes - copy in ASMHDR
GLOBAL $(
// item.type : lexanal+0        //  contains an 'i' number (see below)
// item.info : lexanal+1        //  additional information
// item.start: lexanal+2        //  character position of lexical symb
// getitem   :       ?+3        //  getitem() sets up item.info & item.type
// scan      :       ?+4        //  scan(item) scans item if item=item.type
// expression:       ?+5        //  no:=expression() sets dontknow parses exprn
// bracketed : lexanal+3        //  TRUE if last expression parsed was bracketed
// lex.space : lexanal+4        //  memory for lexical analyser's strings
$)
*/







// The following procedure is used whenever the value of a character is placed
// in the output hex.  It can be redefined by the user of the general assembler
// to generate something other than ASCII which is the default translation.



//*<ASCII// if on ascii machine no need to translate characters:
LET cvtchar (ch) = ch
/*/      // now code for translation if not on ascii machine:
LET cvtchar(ch) = VALOF
SWITCHON ch INTO
$(  CASE 0   : RESULTIS 0          // '*0'
    CASE '*T': RESULTIS #X09
    CASE '*N':
    CASE 10  : RESULTIS #X0A       // '*L'
    CASE 12  : RESULTIS #X0C       // '*P'
    CASE 13  : RESULTIS #X0D       // '*C'
    CASE 27  : RESULTIS #X1B       // '*E'
    CASE '*S': RESULTIS #X20
    CASE '!': RESULTIS #X21
    CASE '"': RESULTIS #X22
    CASE '#': RESULTIS #X23
    CASE '$': RESULTIS #X24
    CASE '%': RESULTIS #X25
    CASE '&': RESULTIS #X26
    CASE '*'': RESULTIS #X27
    CASE '(': RESULTIS #X28
    CASE ')': RESULTIS #X29
    CASE '**': RESULTIS #X2A
    CASE '+': RESULTIS #X2B
    CASE ',': RESULTIS #X2C
    CASE '-': RESULTIS #X2D
    CASE '.': RESULTIS #X2E
    CASE '/': RESULTIS #X2F
    CASE '0': RESULTIS #X30
    CASE '1': RESULTIS #X31
    CASE '2': RESULTIS #X32
    CASE '3': RESULTIS #X33
    CASE '4': RESULTIS #X34
    CASE '5': RESULTIS #X35
    CASE '6': RESULTIS #X36
    CASE '7': RESULTIS #X37
    CASE '8': RESULTIS #X38
    CASE '9': RESULTIS #X39
    CASE ':': RESULTIS #X3A
    CASE ';': RESULTIS #X3B
    CASE '<': RESULTIS #X3C
    CASE '=': RESULTIS #X3D
    CASE '>': RESULTIS #X3E
    CASE '?': RESULTIS #X3F
    CASE '@': RESULTIS #X40
    CASE 'A': RESULTIS #X41
    CASE 'B': RESULTIS #X42
    CASE 'C': RESULTIS #X43
    CASE 'D': RESULTIS #X44
    CASE 'E': RESULTIS #X45
    CASE 'F': RESULTIS #X46
    CASE 'G': RESULTIS #X47
    CASE 'H': RESULTIS #X48
    CASE 'I': RESULTIS #X49
    CASE 'J': RESULTIS #X4A
    CASE 'K': RESULTIS #X4B
    CASE 'L': RESULTIS #X4C
    CASE 'M': RESULTIS #X4D
    CASE 'N': RESULTIS #X4E
    CASE 'O': RESULTIS #X4F
    CASE 'P': RESULTIS #X50
    CASE 'Q': RESULTIS #X51
    CASE 'R': RESULTIS #X52
    CASE 'S': RESULTIS #X53
    CASE 'T': RESULTIS #X54
    CASE 'U': RESULTIS #X55
    CASE 'V': RESULTIS #X56
    CASE 'W': RESULTIS #X57
    CASE 'X': RESULTIS #X58
    CASE 'Y': RESULTIS #X59
    CASE 'Z': RESULTIS #X5A
    CASE '[': RESULTIS #X5B
    CASE '\': RESULTIS #X5C
    CASE ']': RESULTIS #X5D
    CASE '^': RESULTIS #X5E
    CASE '_': RESULTIS #X5F
    CASE '`': RESULTIS #X60
    CASE 'a': RESULTIS #X61
    CASE 'b': RESULTIS #X62
    CASE 'c': RESULTIS #X63
    CASE 'd': RESULTIS #X64
    CASE 'e': RESULTIS #X65
    CASE 'f': RESULTIS #X66
    CASE 'g': RESULTIS #X67
    CASE 'h': RESULTIS #X68
    CASE 'i': RESULTIS #X69
    CASE 'j': RESULTIS #X6A
    CASE 'k': RESULTIS #X6B
    CASE 'l': RESULTIS #X6C
    CASE 'm': RESULTIS #X6D
    CASE 'n': RESULTIS #X6E
    CASE 'o': RESULTIS #X6F
    CASE 'p': RESULTIS #X70
    CASE 'q': RESULTIS #X71
    CASE 'r': RESULTIS #X72
    CASE 's': RESULTIS #X73
    CASE 't': RESULTIS #X74
    CASE 'u': RESULTIS #X75
    CASE 'v': RESULTIS #X76
    CASE 'w': RESULTIS #X77
    CASE 'x': RESULTIS #X78
    CASE 'y': RESULTIS #X79
    CASE 'z': RESULTIS #X7A
    CASE '{': RESULTIS #X7B
    CASE '|': RESULTIS #X7C
    CASE '}': RESULTIS #X7D
    CASE '~': RESULTIS #X7E
    DEFAULT: RESULTIS ch
$)
/**/


MANIFEST
$(  uletter = 10-'A'
    lletter = 10-'a'
$)



LET getitem() = VALOF
$(rpt
    // This procedure is the lexical analyser through which all input is taken.
    // Information about the lexical item just read is kept in:
    //      item.type         -  lexical item identifcation ('i.' constants)
    //      item.start        -  column at which item started
    //      item.info         -  extra information about item (e.g. value)
    // A work area called 'lex.space' is used to hold identifiers and strings
    // and a pointer to it is returned in 'item.info' when the item is either
    // of these types.   20.01.81

    // If an identifier is returned, the string is padded with zeroes up to
    // a whole word boundary.  This is required by some symbol table
    // manipulation routines.  16.07.81

    // This is a repeat loop so that the analysis can be reapplied in the case
    // of a TEXT variable expansion  --  I deny responsibility for it!

    LET p = 10
    item.type:=i.bad
    item.info:=null
    item.start:=linbuf.top-1
    ch := capitalch (ch)
    TEST 'A'<=ch<='Z'
    THEN $(  LET macro=?
             dicposn := 0
             p := 0
             $(rpt1
                 IF p<255 DO p := p+1
                 lex.space%p := ch
                 rch()
                 ch := capitalch (ch)
             $)rpt1 REPEATWHILE 'A'<=ch<='Z' | ch=sepch | '0'<=ch<='9'
             lex.space%0:=p
             p := p + 1
             UNTIL p REM bytesperword = 0 DO   //Now pad the string with 0s
             $( lex.space % p := 0             //up to word boundary as
                p := p + 1                     //required by symb tab routines
             $)
             item.type:=i.iden
             item.info:=lex.space
             macro:=looktype(type.text, item.info)
             IF iden.valid THEN
             $(  textvar(macro)
                 // don't bother to expand it if it's empty!
                 UNLESS item.start<=0 THEN WHILE ch='*S' DO rch()
                 LOOP     // eugh
             $)
         $)
    ELSE TEST ch=comntch THEN
    $(  // 'comntch' is a variable set by the user of the general assembler:
        // so it is checked first to override other definitions of that
        // character.
        item.type:=i.comnt
        item.info:=ch
    $) ELSE
    $(  LET no.rch=FALSE
        // this is TRUE unless a final 'rch' is needed to get the next char
        SWITCHON ch INTO
         // single character items first
         $(  CASE '*S':   item.type:=i.space            ; ENDCASE
             CASE ',' :   item.type:=i.comma            ; ENDCASE
             CASE '[' :   item.type:=i.lsqb             ; ENDCASE
             CASE ']' :   item.type:=i.rsqb             ; ENDCASE
             CASE '(' :   item.type:=i.lbkt             ; ENDCASE
             CASE ')' :   item.type:=i.rbkt             ; ENDCASE
             CASE '+' :   item.type:=i.plus             ; ENDCASE
             CASE '-' :   item.type:=i.minus            ; ENDCASE
             CASE '**':   item.type:=i.mult             ; ENDCASE
             CASE '/' :   item.type:=i.div              ; ENDCASE
             CASE '@' :   item.type:=i.immed            ; ENDCASE
             CASE '=' :   item.type:=i.equals           ; ENDCASE
             CASE ';' :   item.type:=i.semi             ; ENDCASE
             CASE ':' :   item.type:=i.colon            ; ENDCASE
             CASE '&' :   item.type:=i.and              ; ENDCASE
             CASE '|' :   item.type:=i.or               ; ENDCASE
             CASE '$' :   item.type:=i.dollar           ; ENDCASE
             CASE '%':    item.type:=i.percent          ; ENDCASE
             CASE '\':
             CASE '~':    item.type:=i.not              ; ENDCASE
             CASE '*N':   item.type:=i.stop;no.rch:=TRUE; ENDCASE
             CASE endstreamch: item.type:=i.end
                               no.rch:=TRUE
                               ENDCASE
             CASE '>' :
             CASE '<' :
               item.type := (ch='>') -> i.gt, i.lt
               item.info := ch
               rch()
               IF ch=item.info THEN
               $(  item.type:=(ch='>'->i.shr,i.shl)
                   rch()
               $)
               no.rch:=TRUE
               ENDCASE
             CASE '"' :
             CASE '*'':
               $(  LET p=0
                   LET del=ch
                   item.type:=(ch='"'->i.string, i.sstring)
                   item.info:=lex.space
                   rch()
                   UNTIL ch=del DO
                   $(  LET c = ?
                       IF (ch='*N')|(ch=endstreamch)
                       DO error (e.expected, del)
                       c:=ch
                       IF c='**' DO
                       $(  rch()
                           c:=VALOF
                           SWITCHON capitalch(ch) INTO
                           $(  CASE '0': RESULTIS 0
                               CASE 'T': RESULTIS '*T'
                               CASE 'L': RESULTIS 10
                               CASE 'P': RESULTIS '*P'
                               CASE 'N': RESULTIS '*N'
                               CASE 'C': RESULTIS 13
                               CASE 'E': RESULTIS 27
                               CASE 'S': RESULTIS '*S'
                               CASE '*N':
                                 endline()
                                 newlyne()
                                 rch() REPEATUNTIL ch\='*S'
                                 UNLESS ch=c DO error(e.expected,c)
                                 rch()
                                 LOOP
                               DEFAULT: RESULTIS ch
                           $)
                       $)
                       IF p=255 DO error (e.fullstring)
                       p:=p+1
                       lex.space%p := c
                       rch()
                   $)
                   lex.space%0:=p
               $)
               ENDCASE
             CASE '#':
               rch ()
               TEST '0'<=ch<='7' THEN p:=8 ELSE
               $(  SWITCHON capitalch(ch) INTO
                    $(  CASE 'B':  p:=2 ; ENDCASE
                        CASE 'O':  p:=8 ; ENDCASE
                        CASE 'D':  p:=10; ENDCASE
                        CASE 'X':  p:=16; ENDCASE
                        DEFAULT :  error(e.badbase,ch)
                    $)
                    rch()
               $)
              CASE '0': CASE '1': CASE '2': CASE '3': CASE '4':
              CASE '5': CASE '6': CASE '7': CASE '8': CASE '9':
               $(  LET valid = false
                   no.rch:=TRUE
                   item.type := i.number
                   item.info := 0
                   $(rpt1
                       LET d = ('0'<=ch<='9') -> ch-'0',
                       ('A'<=ch<='F') -> ch+uletter,
                       ('a'<=ch<='f') -> ch+lletter, 999
                       IF d=999
                       DO TEST valid
                          THEN ENDCASE
                          ELSE BREAK
                       UNLESS d<p BREAK
                       item.info:=item.info*p + d
                       rch()
                       valid := true
                   $)rpt1 REPEAT
                   error(e.badbase,ch)
               $)
             DEFAULT: error(e.badsym)
         $)
         UNLESS no.rch THEN rch()
    $)
    WHILE ch='*S' DO rch()
    RESULTIS item.type
$)rpt REPEAT        // for macro expansion part of code (at the top)


AND scan(item) = VALOF
TEST item=item.type
THEN $(  getitem();  RESULTIS TRUE  $)
ELSE RESULTIS FALSE


LET checkandskip (item, errparm) BE
TEST item.type=item
THEN getitem ()
ELSE error (e.expected, errparm)



.







SECTION "asm7"




/*<RSX
GET "libhdr"
GET "asmhdr"
/*RSX>*/


/*<CAP
GET ".**.l.bcpl.libhdr"
GET ".**.cgg.asmhdr"
/*CAP>*/

/*<IBM
GET "LIBHDR"
GET "ASMHDR"
/*IBM>*/

//*<TRIPOS:
GET "libhdr"
GET ":Gray.bcpl.asmhdr"
/*TRIPOS>*/









//
//                       Parsing  Procedure
//



LET parse(file) BE
$(  /* this routine operates in conjunction with 'nextpass'
       to set up the two instances of the input file 'in.first'
       and 'in.second' and to alternate between them in order
       to parse each section of the text twice.
           The state of the parser at the beginning of each 'pass'
       is saved in the global workspace 'state' and is used to
       restore that state at the end of the first pass.
    */
    LET savewrch=wrch
    LET pass.first=?
    LET pass.second=?
    LET max.mem=0
    LET endmissing=FALSE
    LET end.was.missing=FALSE
    syswrch := wrch
    warnvec := TABLE 0,0,0,0,0
    errcount := 0
    item.start := linbuf.top-1
    lex.space := simplegetvec(256/bytesperword-1)
    lab.space := simplegetvec(256/bytesperword-1)
    binbuf    := simplegetvec(binbufsize+b.nextmod)
    binbuf!b.top := b.nextmod
    binbuf!b.nextmod := binbuf!b.top
    entrypoint := #XFFFF
    sectno:=1
    deleteline()
    lineno:=0
    line.of.file:=0
    file.id:=0
    files := simplegetvec(max.files)
    files!0 := 0
    resetfiles(line.of.file)
    pc:=0
    mode:=null
    badlabs:=0
    outpos:=1<<8
    wrch:=mywrch
    def.count:=0
    pagetitle:=null
    modulename:=null
    finishpass:=FALSE
    fatal:=FALSE
    ch:='*S'
    pass:=first
    pass.first:=findasmin(file)
    pass.second:=(pass.first=0->0,findasmin(file))
    IF pass.first=0 | pass.second=0 THEN error(e.badfile, file)
    selectinput(pass.first)
    startparse()
//*<SECT:
    state:=TABLE 0,0,0,0,0,0,0,0
    state!in.lineno:=line.of.file
    state!in.errcount:=errcount
    state!in.first:=pass.first
    state!in.second:=pass.second
    state!in.list:=list
    state!in.memlevel := memory!0
/*SECT>*/
    code.gen(cd.module, TRUE, "")
    $(rpt
        LET had.text=FALSE
        had.text:=had.text | parseline() REPEATUNTIL
                item.type=i.end | finishpass | fatal
        IF memory!0>max.mem THEN max.mem:=memory!0
        UNLESS fatal THEN
        $(  end.was.missing:=endmissing
            endmissing:=(pass=first & item.type=i.end & had.text)
            IF item.type\=i.end | endmissing THEN nextpass()
        $)
    $)rpt REPEATUNTIL fatal | item.type=i.end & \endmissing
    wrch:=savewrch
    pass:=second      // nextpass might have left it as 'first'
    code.gen(cd.eof,entrypoint)
    endparse()
    $(  LET save.out=output()
        selectoutput(out)
        IF end.was.missing THEN
        $(  writes("END directive missing*N")
            errcount:=errcount+1
        $)
        writef("Maximum workspace used is %N words*N",max.mem)
        selectoutput(save.out)
    $)
    selectinput(pass.first)
    endread()
    selectinput(pass.second)
    endread()
$)


AND startparse() BE RETURN      // for redefinition

AND endparse() BE RETURN        // for redefinition


AND nextpass() BE
$(  endparse()
    TEST pass=first THEN
    $(  code.gen(cd.clear)    // clear CODE.GEN's buffers
        pass:=second
        IF mode=null THEN mode:=absolute
        code.gen(cd.module, mode=absolute, modulename)
        code.gen(cd.code,  name,  ((bytesperasmword & #XF) << 12) |
                 ((bytesperasmword * wordsperaddress & #XF) << 8) |
                 (msbytefirst -> #X01, #X00))
        $(  LET stamp=VEC 15
            code.gen(cd.prag, get.time(stamp, 15), "DAT-A")
        $)
        $(  LET saveout=output()
            selectoutput(out)
            wrch('*N')
            selectoutput(saveout)
        $)
//*<SECT:
        errcount:=state!in.errcount
        list:=state!in.list
        line.of.file := state!in.lineno
/*/
        errcount:=0
/*SECT>*/
        file.id := 0
        restartpage:=throws
        state!in.first:=input()
        selectinput(state!in.second)
    $) ELSE
    $(  gen.defs()  // clears internal buffers in CODE
//*<SECT:
        pass:=first
        state!in.errcount:=errcount
        code.gen(cd.module, TRUE, "")     // to inform CODE.GEN of new module
        state!in.second:=input()
        state!in.list:=list
        state!in.lineno := line.of.file
/*SECT>*/
        IF list>0 | allsyms | badlabs>0 THEN
        $(  printlabs()
            printfiles()
        $)
        deletelabs()
//*<SECT:
        pagetitle:=null
        modulename:=null
        sectno:=sectno+1
        selectinput(state!in.first)
        memory!0 := state!in.memlevel   // reset memory to previous limit
/*SECT>*/
    $)
    resetfiles(line.of.file)
    mode:=null
    pc:=0
    ch:='*n'
    lineno := 0
    finishpass:=FALSE
    def.count:=0
//*<SECT:
    startparse()
/*SECT>*/
$)



AND parseline() = VALOF
$(  LET savlev=errorlevel
    LET savlab=errorlabel
    LET mt.line=FALSE
    startpc:=pc
    warning:=FALSE
    dontknow:=FALSE
    errorlevel:=level()
    errorlabel:=xit
    newlyne()
/*<DEBUG:
    writef("Line %n PC= %n*n",lineno,pc)
/*DEBUG>*/
    getitem()
    mt.line:= (item.type=i.stop | item.type=i.end | item.type=i.comnt)
    UNLESS mt.line DO
       $(  LET mnem=?
           LET lab=lab.space
           TEST scan(i.space)
           THEN lab:=null
           ELSE TEST item.type=i.iden
                THEN $(  //Copy the identifier string into label buffer.
                         //A word copy is done in order to preserve the
                         //0s padding the string to word size, as these are
                         //required by symbol table routines.  A word copy
                         //should also be faster!  16.07.81
                         FOR i = 0 TO item.info % 0 / bytesperword DO
                            lab ! i := item.info ! i
                         getitem()
                         scan(i.endlab)
                     $)
                ELSE error(e.nolab)
           mt.line:=(lab=null &
                    (item.type=i.stop | item.type=i.end | item.type=i.comnt))
           TEST item.type=i.comnt | item.type=i.stop | item.type=i.end THEN
               get.and.declare(lab)
           ELSE TEST item.type\=i.iden THEN
           $(  UNLESS lab=null THEN get.and.declare(lab)
               error(e.badcode)
           $)
           ELSE $(  mnem:=getcode(item.info)
                    TEST mnem=null
                    THEN error(e.badcode)
                    ELSE $(  FOR i=1 TO (mnem+c.opstr)!0 DO
                             putword((mnem+c.opstr)!i)
/*<DEBUG:
                             writef("About to execute '%S'*n",mnem)
/*DEBUG>*/
                             dontknow:=FALSE
                             (mnem!c.fn) (lab, (mnem+c.opstr)!1)
                             IF comntcheck DO
                                UNLESS item.type=i.comnt |
                                       item.type=i.stop |
                                       item.type=i.end
                                DO error(e.nocomnt)
                         $)
                   $)
       $)
    IF warning DO error(0)
    endline()
xit:
//*<TRIPOS:
    fatal:=fatal | testflags(1)
    finishpass:=finishpass | fatal
/*TRIPOS>*/
    errorlevel:=savlev
    errorlabel:=savlab
    RESULTIS \mt.line
$)





AND get.and.declare(lab) BE
$(  // This is the standard procedure called by Mnemonic Procedures which want
    // any label on their line to be treated in the default way (i.e. have the
    // current program counter assigned to it).
    UNLESS lab=null THEN
    $(  IF mode=null THEN mode:=absolute
        IF pass=first THEN
            // DEF label if 'def.count' hasn't run out yet
            putlab(lab, pc, (mode=absolute->type.lab, type.rellab) |
                            (def.count>0->flag.def,0))
        UNLESS def.count=0 THEN def.count:=def.count-1
    $)
    getitem()
$)



AND newsection() BE RETURN            // for redefinition






//
//                     Expression   Analysis
//





LET is.expression() = VALOF
TEST item.type=i.here
THEn RESULTIS TRUE
ELSE SWITCHON item.type INTO
     $(  CASE i.iden:
         CASE i.number:
         CASE i.sstring:
         CASE i.minus:
         CASE i.plus:
         CASE i.not:
           RESULTIS TRUE
         DEFAULT:
           RESULTIS item.type=i.elbkt
     $)



AND expression() = VALOF
$(  LET spec=VEC spec.size
    label.expression(spec)
    UNLESS spec!1=0 THEN warn(e.badrel)
    RESULTIS spec!0
$)



AND pcrel.expression() = VALOF
$(  LET spec=VEC spec.size
    label.expression(spec)
    IF mode=null THEN mode:=absolute
    UNLESS mode=absolute THEN spec!1:=spec!1-1
    UNLESS spec!1=0 THEN warn(e.badrel)
    RESULTIS spec!0-pc
$)



AND label.expression(ansvec) = VALOF
$(  /*     This procedure parses an expression consisting of the operators
       |, &, <<,>>, +,-, *,/  with that relative precedence.  The components
       of the expression are parsed by 'term' (q.v.) and may consist of
       relative, absolute or external items.
           An expression including an external item may not contain any
       other item, nor a monadic -.  Expressions including relative items
       may not contain relative sub expressions in any operators other
       than + and - and must be formatted in such a way that the resulting
       expression has either an equal number of +ve and -ve relative sub
       expressions, in which case the resulting expression is absolute, or
       one more +ve relative sub expression than -ve, in which case the
       resulting expression is itself relative.
           A vector 'spec' is returned with the following values in the
       following offsets:

                offset 0:  the absolute or relative value of the expression
                           or, if the expression was an external, the address
                           from which this symbol was last referenced.
                offset 1:  the number of times that a relative offset must be
                           added to offset 0 at load time to get an absolute
                           value.  Only 0 (absolute) and 1 (relative) are
                           valid except if the expression was external in which
                           case this offset is large and non zero.
                offset 2:  zero unless the expression was external, in which
                           case it will be the address of the external symbols
                           value field, in which is maintained the assembly time
                           address of the last reference.
    */
    ande(ansvec)
    WHILE scan(i.or) DO
    $(  LET a=VEC 2
        ansvec!0 := ansvec!0 | ande(a)
        UNLESS ansvec!1=0 & a!1=0 THEN warn(e.badrel)
        bracketed:=FALSE
    $)
    RESULTIS ansvec
$)


AND ande(ansvec) = VALOF
$(  shifte(ansvec)
    WHILE scan(i.and) DO
    $(  LET s=VEC 2
        ansvec!0 := ansvec!0 & shifte(s)
        UNLESS ansvec!1=0 & s!1=0 THEN warn(e.badrel)
        bracketed:=FALSE
    $)
    RESULTIS ansvec!0
$)


AND shifte(ansvec) = VALOF
$(  LET not.val = scan(i.not)
    adde(ansvec)
    WHILE item.type=i.shr | item.type=i.shl DO
       $(  LET s=item.type
           LET a=VEC 2
           getitem()
           TEST s=i.shr
           THEN ansvec!0:=ansvec!0 >> adde(a)
           ELSE ansvec!0:=ansvec!0 << adde(a)
           UNLESS ansvec!1=0 & a!1=0 THEN warn(e.badrel)
           bracketed:=FALSE
       $)
    IF not.val THEN ansvec!0:=\ansvec!0
    RESULTIS ansvec!0
$)


AND adde(ansvec) = VALOF
$(  product(ansvec)
    $(rpt
        LET s = +1
        SWITCHON item.type INTO
        $(  DEFAULT:      RESULTIS ansvec!0
            CASE i.minus: s := -1
            CASE i.plus :
              $(  LET p=VEC 2
                  getitem ()
                  ansvec!0 := ansvec!0 + s * product (p)
                  ansvec!1 := ansvec!1 + s * p!1
                  IF ansvec!2 /* last external reference */ \= 0 | p!2 \= 0 THEN
                  warn(e.badext)
                  bracketed:=FALSE
              $)
              LOOP
        $)
    $)rpt REPEAT
$)


AND product(ansvec) = VALOF
$(  term(ansvec)
    WHILE item.type=i.div | item.type=i.mult DO
       $(  LET s=item.type
           LET t=VEC 2
           getitem()
           TEST s=i.div
           THEN TEST term(t)=0
                THEN warn(e.divzero)
                ELSE ansvec!0:=ansvec!0/t!0
           ELSE ansvec!0:=ansvec!0*term(t)
           UNLESS ansvec!1=0 & t!1=0 THEN warn(e.badrel)
           bracketed:=FALSE
       $)
    RESULTIS ansvec!0
$)



AND term(ansvec) = VALOF
$(  /*      This procedure parses the elements of an expression.  They
        may be preceeded by an arbitrary number of monadic sign symbols.
        They may be any of the following:

                a number: as defined by the lexical analyser 'getitem'
                a single quoted string: returns the value of the ascii
                        characters packed into a word.
                the 'here' symbol i.here: returns the value of the
                        program counter - a relative value in a relative
                        section and an absolute one in an absolute one.
                an external symbol: returns the address where it was last
                        used.  Offset 1 of 'ansvec' is set high to guarantee
                        that it will not indicate an absolute expression.
                        Offset 2 is set to a pointer to the information field
                        in the symbol's descriptor where the address where it
                        was last used is kept.
                a relative symbol: returns its relative address and sets
                        offset 1 of 'ansvec' to +/- 1 (depending upon the
                        number of monadic minus signs parsed).
                an absolute symbol: returns the value of the symbol and sets
                        offset 1 of 'ansvec' to 0.
                a bracketed expression: see 'label.expression'
    */
    LET sign = +1
    ansvec!0 := -1     // default value
    ansvec!1 := 0      // relocation count
    ansvec!2 := 0      // last reference of external symbol
    bracketed:=FALSE
    $(rpt
        SWITCHON item.type INTO
        $(  CASE i.minus:
                 sign := -sign
            CASE i.plus:
                 getitem ()
                 LOOP
            CASE i.iden:
                 $(  LET d=dontknow
                     UNLESS getlab(item.info, ansvec)=null THEN
                     $(  IF pass=second & (ansvec!1=type.none | \iden.valid)
                         THEN warn(e.badlab)
                         TEST (ansvec!1 /* type */ & type.mask) = type.def THEN
                             TEST mode=relative THEN
                             $(  ansvec!1:=1    // relocation count
                                 ansvec!2:=0
                             $) ELSE
                             $(  mode:=absolute
                                 ansvec!1:=0
                                 ansvec!2:=0
                             $)
                         ELSE
                         TEST (ansvec!1 /* type */ & flag.rel)\=0 THEN
                         $(  ansvec!1:=1   // relocation count
                             ansvec!2:=0
                         $) ELSE
                         TEST (type.mask&ansvec!1) /* type */ = type.ref THEN
                         $(  ansvec!1 := 1000   // a big number
                             IF sign=-1 THEN error(e.badext)
                         $) ELSE
                         $(  ansvec!1:=0
                             ansvec!2:=0
                         $)
                     $)
                     UNLESS iden.valid DO warn(e.badlab)
                     getitem()
                     dontknow:= dontknow | d
                 $)
                 ENDCASE
            CASE i.number:
                 ansvec!0:=item.info
                 getitem()
                 ENDCASE
            CASE i.sstring:
                 ansvec!0 := 0
                 FOR i=1 TO item.info%0 DO
                    ansvec!0:= (ansvec!0<<8) + cvtchar(item.info%i)
                 getitem()
                 ENDCASE
            DEFAULT:
                 TEST item.type=i.here
                 THEN $(  ansvec!0 := pc
                          TEST mode=relative THEN ansvec!1:=1 // relocatable
                          ELSE mode:=absolute
                          getitem ()
                      $)
                 ELSE TEST scan(i.elbkt)
                      THEN $(  label.expression(ansvec)
                               checkandskip(i.erbkt,')')
                               bracketed:=(sign=+1)
                               // in case '-' sign in front of bracket
                           $)
                      ELSE error(e.badnum)
        $)
        ansvec!0 := ansvec!0*sign
        ansvec!1 := ansvec!1*sign
        RESULTIS ansvec!0
    $)rpt REPEAT
$)



AND is.type(ty) = VALOF
$(  LET ans=FALSE
    IF item.type=i.iden DO ans:=checktype(ty, item.info)
    RESULTIS ans
$)









.








SECTION "asm8"




/*<RSX
GET "libhdr"
GET "asmhdr"
/*RSX>*/

/*<CAP
GET ".**.l.bcpl.libhdr"
GET ".**.cgg.asmhdr"
/*CAP>*/

/*<IBM
GET "LIBHDR"
GET "ASMHDR"
/*IBM>*/

//*<TRIPOS:
GET "libhdr"
GET ":Gray.bcpl.asmhdr"
/*TRIPOS>*/






//
//                   MNEMONIC  IMPLEMENTATION
//






//      This is a list of predeclared Mnemonic Procedures.  They must be
// inserted into the tree of mnemonic definitions using 'code.put'.
// They interpret their command lines in standard ways that are likely to
// 'fit' (perhaps a little losely) the prefered directives given in a
// target assembler.





LET dataproc(lab) BE
$(  get.and.declare(lab)
    $(rpt
        TEST item.type=i.string | item.type=i.sstring
        THEN $(  LET offset=(item.type=i.string->0,1)
                 FOR i=offset TO offset+(item.info%0-offset)/bytesperasmword DO
                    $(  LET pack=0
                        FOR j=0 TO bytesperasmword-1 DO
                           $(  LET char=(i*bytesperasmword+j>item.info%0 ->
                               '*S',cvtchar(item.info%(i*bytesperasmword+j)) )
                               TEST mscharfirst
                               THEN pack:=(pack<<8) | char
                               ELSE pack:=(pack>>8) |
                                    (char<<(8*(bytesperword-1)))
                           $)
                        putword(pack)
                    $)
                 getitem()
             $)
        ELSE TEST scan(i.equals)
             THEN TEST \is.expression()
                  THEN error(e.badnum)
                  ELSE
                  $(  LET spec=VEC spec.size
                      putlabelspec(label.expression(spec))
                  $)
             ELSE TEST \is.expression()
                  THEN error(e.badnum)
                  ELSE $(  LET no=expression()
                           putword(no)
                       $)
    $)rpt REPEATWHILE scan(i.comma)
$)



AND titlproc(lab) BE
$(  getitem()
    TEST pass=first
    THEN $(  LET savout=output()
             STATIC
             $(  done=-1
             $)
             IF done<sectno THEN
             $(  UNLESS item.type=i.string |
                        item.type=i.iden   |
                        item.type=i.sstring
                 DO error(e.nostr)
                 pagetitle:=getstr(item.info)
                 modulename:=(lab=null->pagetitle,getstr(lab))
                 selectoutput(out)
                 writef("Assembling *"%S*" ",item.info)
                 selectoutput(savout)
                 done:=sectno
             $)
         $)
    ELSE $(  STATIC
             $(  done=-1
             $)
             UNLESS done<sectno THEN error(e.titledone)
             UNLESS item.type=i.string | item.type=i.iden | item.type=i.sstring
             DO error(e.nostr)
             code.gen(cd.prag, item.info, "TITLE")
             done:=sectno
         $)
    getitem()
$)



AND equproc(lab) BE
TEST lab=null
THEN error(e.nolab)
ELSE $(  getitem()
         TEST \is.expression()
         THEN warn(e.badnum)
         ELSE $(  LET spec=VEC spec.size
                  label.expression(spec)
                  TEST spec!2 /* external */\=0 THEN error(e.badrel) ELSE
                  putlab(lab, spec!0, (dontknow & flag.fwd) |
                              (spec!1=0->type.const, type.relconst))
              $)
     $)



AND setproc(lab) BE
TEST lab=null
THEN error(e.nolab)
ELSE $(  getitem()
         TEST \is.expression()
         THEN error(e.badnum) ELSE
         $(  LET spec=VEC spec.size
             label.expression(spec)
             TEST spec!2 /* external */\=0 THEN error(e.badrel) ELSE
             putlab(lab, spec!0, (spec!1=0->type.var, type.relvar))
         $)
         getitem()
     $)



AND textproc(lab) BE
$(  getitem()
    IF lab=null DO error(e.nolab)
    UNLESS item.type=i.string | item.type=i.sstring DO error(e.nostr)
    IF pass=first DO putlab(lab, getstr(item.info), type.text)
    getitem()
$)



AND endproc(lab) BE
$(  get.and.declare(lab)
    IF is.expression() THEN
    $(  LET spec=VEC spec.size
        LET e= !label.expression(spec)
        TEST spec!1\=0 & spec!1\=1 THEN  error(e.badrel) ELSE
        IF pass=second THEN
        TEST entrypoint=#XFFFF THEN
        entrypoint:=e
        ELSE error(e.newentry)
    $)
    finishpass:=TRUE
$)



AND orgproc(lab) BE
$(  LET spec=VEC spec.size
    get.and.declare(lab)
    label.expression(spec)
    TEST spec!1\=0 & spec!1\=1 THEN error(e.badrel) ELSE
    TEST dontknow THEN error(e.forward) ELSE
    $(  pc:=spec!0
        startpc:=pc
    $)
$)



AND storeproc(lab) BE
$(  LET i=?
    get.and.declare(lab)
    i:=expression()
    TEST dontknow THEN error(e.forward) ELSE
    TEST i<0 THEN error(e.posnum) ELSE
    $(  code.gen(cd.newpc, pc)
        pc:=pc+i
        code.gen(cd.newpc, pc)
    $)
$)



AND absproc(lab) BE
$(  pc:=0
    startpc:=0
    TEST mode=null THEN mode:=absolute ELSE error(e.modeset)
    UNLESS lab=null | pass=second THEN putlab(lab,pc,type.lab)
    getitem()
$)



AND relproc(lab) BE
$(  pc:=0
    startpc:=0
    TEST mode=null THEN mode:=relative ELSE error(e.modeset)
    UNLESS lab=null | pass=second THEN putlab(lab,pc,type.rellab)
    getitem()
$)



AND declaration.list(lab, default.value, type) BE
$(  get.and.declare(lab)
    $(rpt
        UNLESS item.type=i.iden THEN error(e.nolab)
        putlab(item.info, default.value, type)
        getitem()
    $)rpt REPEATUNTIL \scan(i.comma)
$)



AND refproc(lab) BE declaration.list(lab, #XFFFF, type.ref)



AND wrefproc(lab) BE declaration.list(lab, #XFFFF, type.wref)



AND needsproc(lab) BE
$(  // The #XFFFF in this procedures definition represents a null pointer in
    // the generated code.  This, like REF variables, is because all references
    // to this symbol will be assembled as a pointer to the last reference to
    // that symbol in the output code. (and so the first must be the null
    // pointer to terminate the list).
    // This feature is rather Intel/Mostec/Cambridge Hex ortientated and may
    // be improved at length.   20.01.81
    get.and.declare(lab)
    $(rpt
        TEST item.type=i.iden THEN
        putlab(item.info, #XFFFF, type.ref | flag.needs) ELSE
        $(  LET level=0
            UNLESS item.type=i.string | item.type=i.sstring THEN
            $(  level:=expression()
                scan(i.colon)
            $)
            TEST item.type=i.string | item.type=i.sstring THEN
            code.gen(cd.opt, item.info, level) ELSE error(e.nostr)
        $)
        getitem()
    $)rpt REPEATUNTIL \scan(i.comma)
$)



AND defproc(lab) BE
$(  //  DEF variables start of life with undefined types 'type.none' and are
    //  only given a type when they are defined and given a proper value.
    get.and.declare(lab)
    $(rpt
        TEST item.type=i.iden THEN
        $(  putlab(item.info, -1, type.def)
            getitem()
        $) ELSE
        $(  LET i=expression()
            TEST dontknow THEN error(e.forward) ELSE def.count:=i
        $)
    $)rpt REPEATUNTIL \scan(i.comma)
$)



AND pragproc(lab) BE
TEST lab=0 THEN error(e.nolab) ELSE
$(  getitem()
    TEST item.type=i.string | item.type=i.sstring THEN
    $(  IF pass=second THEN code.gen(cd.prag, item.info, lab)
        getitem()
    $)
    ELSE error(e.nostr)
$)



AND spaceproc(lab) BE
$(  LET spaces=?
    get.and.declare(lab)
    spaces:=expression()
    IF pass=second THEN
    TEST dontknow THEN error(e.forward) ELSE
    $(  deleteline()
        FOR i=1 TO spaces-1 DO putch('*N')
    $)
$)


AND ejectproc(lab) BE
$(  LET lines=?
    get.and.declare(lab)
    lines := is.expression() -> expression(), pl
    IF pass=second THEN
    TEST dontknow THEN error(e.forward) ELSE
    $(  deleteline()
        neads(lines)
    $)
$)


AND listproc(lab) BE
$(  get.and.declare(lab)
    UNLESS list>0 THEN deleteline()
    TEST is.expression() THEN
    list:=list+expression() ELSE list:=list+1
$)


AND getproc(lab) BE
$(  get.and.declare(lab)
    TEST (i.string\=item.type\=i.sstring)
    THEN error(e.nostr) ELSE
    $(  LET s=findgetin(item.info%0=0->fromfile, item.info)
        TEST s=0
        THEN error(e.badfile,item.info) ELSE
        $(  LET savein=input()
            LET saveln=line.of.file
            LET savech=?
            LET savefno=file.id
            LET fno=newfile((item.info%0=0->fromfile,item.info))
            endline()
            file.id:=fno
            savech:=ch
            selectinput(s)
            list:=list-1
            line.of.file:=0
            parseline() REPEATUNTIL
                item.type=i.end | finishpass | fatal
            endread()
            selectinput(savein)
//*<SECT:
            finishpass:=fatal
/*SECT>*/
            endfile(fno)
            ch:=savech
            file.id:=savefno
            item.type:=i.stop    // i.e. not i.end or we'll stop!
            line.of.file:=saveln
            list:=list+1
        $)
    $)
$)




/*  This procedure unnecessary since introduction of relocation 13.03.80
AND envproc(lab) BE
$(  get.and.declare(lab)
    TEST item.type\=i.string
    THEN error(e.nostr)
    ELSE IF pass=first DO savedic(findoutput(item.info))
    getitem()
$)
*/





.







SECTION "asm9"




/*<RSX
GET "libhdr"
GET "asmhdr"
/*RSX>*/

/*<CAP
GET ".**.l.bcpl.libhdr"
GET ".**.cgg.asmhdr"
/*CAP>*/

/*<IBM
GET "LIBHDR"
GET "ASMHDR"
/*IBM>*/

//*<TRIPOS:
GET "libhdr"
GET ":Gray.bcpl.asmhdr"
/*TRIPOS>*/







//
//                Command  Line  &  Machine  Dependent  Parts
//







LET startasm(version) = "Undefined assembler section"
//  This procedure is to be redefined in the user's part of the assembler.
//  The 'version' given is a number in the thousands.  The number of thousands
//  must match the number of thousands of the version of the general assembler
//  that a specific assembler was written under since changing this number
//  indicates a non compatible change in the interface.
//  Compatible changes iterate through the lower orders of the number.
//  If the version is found to be incompatible a string should be returned
//  giveing the version of the assembler that was expected -- otherwise zero
//  should be returned.





LET endasm() BE RETURN



LET useropt(ch) = FALSE            // for redefinition


LET decodeopt(s) = VALOF
$(  LET error=FALSE
    LET temp = ?
    // defaults for options have already been set up
    UNLESS s=null DO FOR p=1 TO s%0 DO
    $(  TEST useropt(s%p) THEN LOOP ELSE
        SWITCHON capitalch(s%p) INTO
        $(  CASE 'P':
              /* include page throws in output */
              throws:=TRUE
              LOOP
            CASE 'C':
              /* compress output listing */
              short:=TRUE
              LOOP
            CASE 'W':
              /* page width (default is 76 chars) */
              temp := @pw
              ENDCASE
            CASE 'H':
              /* width of relocatable binary output (default 16 words) */
              temp := @hexoutwidth
              ENDCASE
            CASE 'S':
              /* amount of store available for label dictionary */
              temp := @memsize
              ENDCASE
            CASE 'L':
              /* depth of 'list 1' directives or getfiles to ignore */
              temp := @list
              ENDCASE
            CASE 'F':
              /* Set the maximum number of header files allowed */
              temp := @max.files
              ENDCASE
            CASE 'A':
              /* For All symbols to be printed in the dictionary */
              allsyms:=TRUE
            CASE ',': CASE '/': CASE '*S':
              LOOP
            DEFAULT:
              writef("Unknown option - *'%C*'*N",s%p)
              error:=TRUE
              LOOP
          $)
          // read in a number and put it in the location
          // indicated by temp:
          $(  LET flag = TRUE
              !temp := 0
              $(rpt
                  p := p+1
                  UNLESS p>s%0 DO SWITCHON s%p INTO
                  $(  CASE '1': CASE '2': CASE '3': CASE '4': CASE '5':
                      CASE '6': CASE '7': CASE '8': CASE '9': CASE '0':
                        !temp := 10*!temp + s%p - '0'
                        flag  := FALSE
                        LOOP
                      CASE '*S': IF flag LOOP
                  $)
                  IF FLAG DO !temp := -1
                  BREAK
              $)rpt REPEAT
              p := p-1
          $)
      $)
    IF list<0 DO list := 1
    hexoutwidth:=hexoutwidth/bytesperasmword
    error:=error | memsize<0 | pw<50 | \1<=hexoutwidth<=32 | max.files<=1
    RESULTIS error
$)





//*<TRIPOS
MANIFEST
$(  bad.rc = 20
    startargsize = 100/bytesperword
$)
LET getargs(startarg) = VALOF
$(  LET vect = getvec(startargsize)
    IF vect\=0 THEN
    TEST 0=rdargs("Prog=From/a,To=Hex,List,Opt/k",vect, startargsize) THEN
    $(  freevec(vect)
        vect := 0
    $) ELSE
    IF vect!2~=0 THEN
    $(  // presume that options should be set up as for a listing:
        pw := 136         // page width for printer
        list := 1         // list one level by default
    $)
    RESULTIS vect
$)
LET findasmin(file.name) = VALOF
$(  LET rc=findinput(file.name)
    LET r2=result2
    IF rc=0 THEN
    $(  writef("Can't open '%S': ",file.name)
        fault(r2)
    $)
    RESULTIS rc
$)
LET findgetin(file.name) = VALOF
$(  LET ans=findinput(file.name)
    LET prefix="SYS:Ring."
    IF ans=0 & prefix%0+name%0<40 THEN
    $(  LET dir=VEC 20
        LET savedir=currentdir
        LET newdir=?
        FOR i=1 TO prefix%0 DO dir%i:=prefix%i
        FOR i=1 TO name%0 DO dir%(prefix%0+i):=name%i
        dir%0 := prefix%0+name%0
        newdir:=locatedir(dir)
        UNLESS newdir=0 THEN
        $(  currentdir:=newdir
            ans:=findinput(file.name)
            UNLESS newdir=savedir THEN freeobj(newdir)
            currentdir:=savedir
        $)
    $)
    RESULTIS ans
$)
LET get.time(vect, n) = VALOF
$(  LET v = TABLE  0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
    LET tim=v+5
    LET dat=v
    LET ans=vect
    STATIC $( got.dat = FALSE $)
    UNLESS got.dat THEN datstring(v)
    TEST n*2<=1+tim%0+dat%0 THEN ans:="" ELSE
    $(  FOR i=1 TO tim%0 DO vect%i:=tim%i
        vect%(tim%0+1):='*S'
        FOR i=1 TO dat%0 DO vect%(tim%0+i+1):=dat%i
        vect%0:=dat%0+tim%0+1
        got.dat := TRUE
    $)
    RESULTIS ans
$)
/*TRIPOS>*/


/*<RSX
MANIFEST
$(  bad.rc = 100
    startargsize = 250
$)
LET getargs(startarg) = VALOF
$(  STATIC
    $(  rsxgetvec=0
    $)
    LET standardisedgetvec(size) = VALOF
    $(  LET rc=rsxgetvec(size)
        IF rc<0 DO RESULTIS 0
        RESULTIS rc
    $)
    LET ftstr=".HEX/.LST/.???"
    LET cmlstr="???"
    LET rc=0
    LET vect = ?
    rsxgetvec := getvec   // This code will only be executed once
    getvec := standardisedgetvec
    vect := getvec(startargsize)
    UNLESS vect=0 THEN
    $(  FOR i=1 TO 3 DO ftstr%(11+i),cmlstr%i:=name%i,name%i
        selectinput(findcml(cmlstr))
        rc:=findtitles(2, vect, ftstr)
        endread()
        selectinput(sysin)   // we need all the space we can get!!
        endread()
        TEST rc<0 THEN
        $(  freevec(vect)
            vect := 0
        $) ELSE
        $(  // findtitles returns two strings per file title
            // (1 for the file, 1 for the switches)
            // This routine must return a listing file,
            // a binary file, a source file and an option string
            // Therefore move pointers as necessary
            vect!1 := vect!0      // HEX output file
        //  vect!2 := vect!2     // LIST output file
            vect!0 := vect!4     // SOURCE input file
            vect!3 := vect!5     // OPTIONS
            // Now force non-explicitly given output files to null:
            IF rc<2 THEN vect!2:= 0
            IF vect!2~=0 THEN
            $(  // presume that listing is needed:
                pw := 136        // width of line printer
                list := 1        // list one level of files
            $)
        $)
    $)
    RESULTIS vect
$)
LET findasmin(file.name) = VALOF
$(  LET rc=findinput(file.name)
    IF rc<=0 THEN writef("Open on %S failed: rc = %N*N", file.name ,rc)
    RESULTIS (rc<0->0,rc)
$)
LET findgetin(file.name) = VALOF
$(  LET rc=findinput(file.name)
    LET prefix="DP3:[75,2]"
    IF rc<=0 & file.name%0+prefix%0+2+name%0<40 THEN
    $(  LET s=VEC 20
        FOR i=1 TO prefix%0 DO s%i := prefix%i
        FOR i=1 TO file.name%0 DO s%(prefix%0+i):=file.name%i
        s%(prefix%0+file.name%0+1):='.'
        FOR i=1 TO (name%0<3->name%0,3) DO s%(prefix%0+file.name%0+1+i):=name%i
        s%0:=prefix%0+file.name%0+1+(name%0<3->name%0,3)
        rc:=findinput(s)
    $)
    IF rc<=0 THEN
    writef("Open on %S failed: rc = %N*N", file.name, rc)
    RESULTIS (rc<0->0, rc)
$)
LET get.time(vect, n)=VALOF
$(  LET tim = TABLE 0,0,0, 0,0,0
    LET dat = TABLE 0,0,0, 0,0,0
    LET ans=vect
    STATIC $( got.dat = FALSE $)
    UNLESS got.dat THEN
    $(  timeofday(tim)
        date(dat)
    $)
    TEST n*2<=1+tim%0+dat%0 THEN ans:="" ELSE
    $(  FOR i=1 TO tim%0 DO vect%i:=tim%i
        vect%(tim%0+1):='*S'
        FOR i=1 TO dat%0 DO vect%(tim%0+i+1):=dat%i
        vect%0:=dat%0+tim%0+1
        got.dat := TRUE
    $)
    RESULTIS ans
$)
/*RSX>*/


/*<CAP
MANIFEST
$(  bad.rc = 20
    startargsize = 40
$)
LET getargs(startarg) = VALOF
$(  STATIC $(
        parmno=3
        error=FALSE
    $)
    LET vect = getvec(startargsize)
    LET putstr(vect, p, n) BE
    TEST error THEN
    $(  vect!parmno:=0
        parmno:=parmno+1
    $) ELSE
    $(  LET size=K.N0%0 / bytesperword + 1
        vect!parmno := vect+!p
        parmno := parmno+1
        TEST !p+size>n THEN error:=TRUE
        ELSE
        $(  FOR i=!p TO !p+size-1 DO
            vect!i:= K.N0!(i-!p)
            !p:=!p+ size
        $)
    $)
    LET p=4        // start of free store in 'vect'
    UNLESS vect=0 THEN
    $(  vect!0:="From|Prog|1"
        vect!1:="Hex|To|2|/a"
        vect!2:="List|3|/m"
        TEST 0=keyarg("OPT",parms.string) THEN putstr(vect, @p, startargsize)
        ELSE
        $(  vect!3:=0
            parmno:=parmno+1
        $)
        IF error THEN
        $(  freevec(vect)
            vect := 0
        $)
    $)
    RESULTIS vect
$)
LET get.time(vect, n) = VALOF
$(  STATIC
    $(  tim = 0
        dat = 0
    $)
    LET ans=vect
    IF tim=0 THEN tim:=time()
    IF dat=0 THEN dat:=date()
    TEST n*2<=1+tim%0+dat%0 THEN ans:="" ELSE
    $(  dat%3:='-'
        dat%7:='-'
        FOR i=1 TO tim%0 DO vect%i:=tim%i
        vect%(tim%0+1):='*S'
        FOR i=1 TO dat%0 DO vect%(tim%0+i+1):=dat%i
        vect%0:=dat%0+tim%0+1
    $)
    RESULTIS ans
$)
LET findasmin(find.name) = VALOF
$(  LET rc=findinput(find.name)
    IF rc=0 THEN
    $(  LET r2=errorcode
        writes("Can't open input file: ")
        writes(fault(r2))
        wrch('*N')
    $)
    RESULTIS rc
$)
LET findgetin(file.name) = VALOF
$(  LET rc=?
    TEST file.name%1='.' THEN rc:=findinput(file.name) ELSE
    $(  LET savedir=current.dir
        LET s=VEC 20
        MANIFEST $( p3=#X40030000 $)
        current.dir:=p3   // directory for file header lookup on CAP
        TEST file.name%0>=40 THEN rc:=0 ELSE   // name too long!
        $(  FOR i=1 TO file.name%0 DO s%(1+i):=file.name%i
            s%1:='.'
            s%0:=file.name%0+1
            rc:=findinput(s)
        $)
        current.dir:=savedir
    $)
    IF rc=0 THEN
    $(  LET r2=errorcode
        writef("Can't open '%S': ",file.name)
        writes(fault(r2))
        wrch('*N')
    $)
    RESULTIS rc
$)
/*CAP>*/



/*<IBM
MANIFEST
$(  bad.rc = 12
    startargsize = 50
$)
STATIC
$(  sys.abend = ?
$)
LET getargs(startarg) = VALOF
$(  // format of startarg is:
    //          "<file name>;<options>"   or
    //          "<file name> <options>"
    LET i=1                     // counts character position in 'startarg'
    LET len=startarg%0          // length of startarg
    LET ch=?
    LET vect=getvec(startargsize)
    UNLESS vect=0 THEN
    $(  vect!0 := vect+4
        WHILE i<len & i<bytesperword*(startargsize-4) & VALOF
        $(  ch:=startarg%i
            RESULTIS ch\=';' & ch\='*S'
        $) DO
        $(  vect!0%i := ch
            i:=i+1
        $)
        vect!0%0 := i-1             // length of file string
        i:=i+1                      // skip the ';' or '*S' terminator
        vect!1:="HEX"
        vect!2:="LIST"
        vect!3:=vect+4+(i-2+1+bytesperword-1)/bytesperword
        FOR n=i TO len DO vect!3%(n-i+1) := startarg%n
        vect!3%0 := len-i+1         // size of options string
        selectoutput(findoutput("SYSPRINT"))
        //  This is a good place to patch ABORT so that it does not
        // dump huge ammounts of data when the user specifies a bad
        // input file!
        sys.abend := abort
        abort := my.abort
    $)
    RESULTIS vect
$)
AND my.abort(code, addr, oldstack, data) BE
$(  LET scc, ucc = (code>>12) & #XFFF, code & #XFFF
    LET sysprint = findoutput("SYSPRINT")
    LET go.abort = TRUE
    IF sysprint=0 THEN
    $(  writetolog("MICROLIB Assembler requires SYSPRINT")
        STOP(16)
    $)
    selectoutput(sysprint)
    writef("%S Assembler ABEND, ", name)
    TEST ucc=0 THEN
    $(  SWITCHON scc INTO
        $(  CASE #X0D1: writes("run out of comp time*N")
                        go.abort := FALSE
                        ENDCASE
            CASE #X0D2: writes("fatal I/O error*N");      ENDCASE
            CASE #X0D3: writes("stack overflow*N");       ENDCASE
            DEFAULT:    writef("system completion code %X3*N", scc)
        $)
    $) ELSE writef("user completion code %N - ASSEMBLER ERROR*N",ucc)
    IF scc = #X001 THEN
    $(  writes("Possibly missing membername for PDS input*N")
        go.abort := FALSE
    $)
    IF scc = #X013 THEN
    $(  writes("Member of PDS not found*N")
        go.abort := FALSE
    $)
    IF (scc & #XFF) = #X37 THEN
    $(  writes("Some limit exceeded on output file size*N")
        go.abort := FALSE
    $)
    IF (scc >> 4) = #XC | scc = #X0D3 THEN
    $(  writes("Try increasing memory available (with %S) otherwise*N*
               * decrease the S option to the assembler (default s4000)*N")
        go.abort := FALSE
    $)
    IF (scc >> 4) = #X80 THEN
    $(  writes("Possibly insufficient I/O space. Use K run-time option*N")
        go.abort := FALSE
    $)
    IF (scc >> 4) = #X08 THEN   // Local to CAMBRIDGE 370 only
                         writes("Some resource limit has been exceeded*N")
    endwrite()
    TEST go.abort THEN
    sys.abend(code, addr, oldstack, data) ELSE stop(16)
$)
LET myfindinput(file) = VALOF
$( STATIC $( ddcount = 0 $)
   GLOBAL $( createdd : 120 $)
   // this global is provided by a linked assembly routine
   // which allows the dynamic creation of DDnames
   LET ddname = "DDxxxx"
   LET v      = VEC 3
   LET lnz    = 3
   LET ddc    = ddcount
   ddcount := ddcount + 1
   FOR j = 1 TO file % 0 DO file % j := capitalch(file % j)
   FOR j = 3 TO 0 BY -1 DO
     $( v ! j := ddc REM 10 + '0'
        ddc   := ddc  /  10
        IF v ! j \= '0' THEN lnz := j
     $)
   ddname % 0 := 6 - lnz
   FOR j = 3 TO lnz BY -1 DO ddname % (3 + j - lnz) := v ! j
// writef("file = '%S' DDname = '%S'*N*N",file,ddname)
   RESULTIS (createdd(ddname, file, FALSE) = 0 -> findinput(ddname), 0)
$)
LET findasmin(file.name)=VALOF
$(  LET rc=myfindinput(file.name)
    IF rc=0 THEN writef("Can't open '%S' for input*N",file.name)
    RESULTIS rc
$)
LET findgetin(file.name) = VALOF
$(  LET rc = myfindinput(file.name)
    IF rc=0 THEN rc:=findinput(file.name)
    IF rc=0 THEN
    $(  LET getlib = "GETLIB(xxxxxxxxx"
        MANIFEST
        $(  firstch = 8
            maxlen  = 8
        $)
        UNLESS file.name%0 > maxlen THEN
        $(  FOR i=1 TO file.name%0 DO getlib%(i-1+firstch) := file.name%i
            getlib%(file.name%0+firstch) := ')'
            getlib%0 := firstch+file.name%0
            rc := findinput(getlib)
        $)
    $)
    RESULTIS rc
$)
LET get.time(vect, n) = VALOF
$(  STATIC
    $(  tim = 0
        dat = 0
    $)
    LET ans=vect
    IF tim=0 THEN tim:=timeofday()
    IF dat=0 THEN dat:=date()
    TEST n*2<=1+(tim%0-1)+(dat%0-2) THEN ans:="" ELSE
    $(  dat%(2+3):='-'
        FOR i=2+5 TO 2+6 DO IF 'A'<=dat%i<='Z' THEN dat%i:=dat%i+'a'-'A'
        dat%(2+7):='-'
        tim%(1+3):=':'
        tim%(1+6):=':'
        FOR i=2 TO tim%0 DO vect%(i-1):=tim%i
        vect%(tim%0-1+1):='*S'
        FOR i=3 TO dat%0 DO vect%(tim%0-1+i+1-2):=dat%i
        vect%0:=(dat%0-2)+(tim%0-1)+1
    $)
    RESULTIS ans
$)
/*IBM>*/





LET start(startarg) BE
$(  LET error=TRUE
    LET args = 0   // argument vector
    LET initialised=FALSE
    LET listfile=?
    $(  LET versionno=3056
        LET versionid="Version 3.056 06-Jan-82"
        LET asmver=?
        msbytefirst := FALSE     // order of bytes in an address
        mscharfirst := TRUE      // order of characters in a word of a string
        binbufwidth := 4
        i.here := i.mult
        comntch:=';'
        i.elbkt := i.lbkt
        i.erbkt := i.rbkt
        i.endlab:=i.colon
        comntcheck := FALSE
        sepch := '.'
        bytesperasmword := 1
        wordsperaddress := 2
        name := "unnamed"
        asmver := startasm(versionno)
        TEST asmver\=0
        THEN writef("Incompatible assembler section version numbers*N*
             *General assembler id: %S*N*
             *Specific assembler id: %S*N",versionid,asmver)
        ELSE
        $(  // set up default values of options so that GETARGS can
            // change them should it so desire:
            memsize:=3000
            throws:=FALSE
            list:=0
            short:=FALSE
            allsyms:=FALSE
            pw:=80
            hexoutwidth:=32
            max.files:=25
            // now call machine specific routine to
            // return file names + option string
            args := getargs(startarg)
            TEST args=0
            THEN writef("Bad arguments for %S assembler*N",name)
            ELSE TEST decodeopt(args!3)
                 THEN writef("Bad OPT string for %S assembler*N",name)
                 ELSE $(  LET outfile=args!2
                          fromfile:=args!0
                          writef("%S assembler.  %S*N",name,versionid)
                          binfile := findoutput(args!1)
                          listfile:= (outfile=0->output(), findoutput(outfile))
                          out := output()
                          memory := getvec(memsize)
                          linbufsize := pw+1-
                                        (15+(1+2*bytesperasmword)*binbufwidth+2)
                          linbuf := getvec(linbufsize/bytesperword+1)
                          TEST memsize<10 | memory=0 | linbuf=0
                          THEN writes("Insufficient store for execution*N")
                          ELSE TEST listfile=0
                               THEN writef("Can't open %S for output*N",
                                            outfile=0->"OUTPUT stream",outfile)
                               ELSE error:=FALSE
                          initialised:=TRUE
                      $)
        $)
    $)
    UNLESS error THEN
    $(  UNLESS out=listfile DO selectoutput(listfile)
        memory!0 := 1    // initialise memory vector
        errorlevel := level()
        errorlabel := emergencyexit
        codes := null
        initcodes()
        initsyms()
        parse(fromfile)
emergencyexit:
        selectoutput(out)
        IF pass=second THEN
        $(  TEST  badlabs=0 & errcount=0 THEN
            $(  writes("No errors ")
                error:=FALSE
            $) ELSE
            $(  UNLESS errcount=0 THEN
                writef("%N error%S ", errcount, (errcount=1->"","s"))
                UNLESS errcount=0 | badlabs=0 THEN writes("and ")
                UNLESS badlabs=0 THEN
                writef("%N bad label%S ", badlabs, (badlabs=1->"","s"))
            $)
            writes("in this assembly*N")
        $)
    $)
    IF initialised THEN
    $(  UNLESS listfile=0 | listfile=out DO
        $(  selectoutput(listfile)
            endwrite()
        $)
        UNLESS binfile=0 DO
        $(  selectoutput(binfile)
            endwrite()
        $)
        selectoutput(out)
        UNLESS linbuf=null DO freevec(linbuf)
        UNLESS memory=0 DO freevec(memory)
    $)
    endasm()
    UNLESS args=0 THEN freevec(args)
    IF error | fatal THEN stop(bad.rc)
$)


