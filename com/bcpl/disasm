SECTION "LSI4DISASM"

GET "LIBHDR"
GET "CLIHDR"


//******************************************************************************
//                                                                             *
// TRIPOS LSI4 disassembler mark two.                                          *
//                                                                             *
//        This version accepts TRIPOS binary input, and includes local and     *
//            external relocation information in the output.                   *
//                                                                             *
// Richard Evans.                                                 March 1980   *
//                                                                             *
//                                                                             *
// Command format:   DISASM  [FROM] file  [TO file]  [OPT opts]                *
//                                                                             *
//******************************************************************************




GLOBAL $( work.vector.chain           :  ug
          instream                    :  ug +  1
          outstream                   :  ug +  2
          standout                    :  ug +  3
          exhausted                   :  ug +  4

          full.map                    :  ug + 10
          print.symdefs               :  ug + 11
          disassemble.all.words       :  ug + 12
          disassemble.next.word       :  ug + 13
          narrow.output               :  ug + 14
          print.hunks                 :  ug + 15

          bintype                     :  ug + 20

          relpointer                  :  ug + 25
          symchain                    :  ug + 26

          heapptr                     :  ug + 30
          heap.block                  :  ug + 31

          chars.printed               :  ug + 35
          standard.wrch               :  ug + 36
       $)


MANIFEST $( yes                      =  TRUE
            no                       = FALSE

            heap.block.size          =   200

            st.deftype               =     0
            st.value                 =     1
            st.link                  =     2
            st.name                  =     3
            size.st                  =  st.name + 7/bytesperword + 1

            rel.loc                  =     0
            rel.rt                   =     1
            rel.val                  =     2
            rel.link                 =     3
            size.rel                 =     4

            rt.local                 =     0
            rt.ext                   =     1
            rt.hunk                  =     2

            t.hunk                   =  1000
            t.reloc                  =  1001
            t.end                    =  1002
            t.abshunk                =  1003
            t.absreloc               =  1004
            t.ext                    =  1005
            t.block                  =  1006
            t.table                  =  1008
            t.lkedext                =  1009
            t.overlay                =  1010
            t.break                  =  1011

            ext.defrel               =     1
            ext.defabs               =     2
            ext.common               =   130
         $)





LET start() BE  // DISASM  [FROM] file  [TO file]  [OPT opts]
  //
  // Options are:       U       unformatted binary
  //                    M       map all TRIPOS binary types
  //                    E       print symbol settings
  //                    A       disassemble ALL words
  //                    N       narrower output
  //                    Q       quick - don't disassemble hunks
  //
  $( LET unformatted = no
     LET argv        = VEC 100

     work.vector.chain     := 0
     instream              := 0
     outstream             := 0
     standout              := output()
     exhausted             := no
     standard.wrch         := wrch

     full.map              := no
     print.symdefs         := no
     disassemble.all.words := no
     narrow.output         := no
     print.hunks           := yes

     disassemble.next.word := yes

     // Initialise the data structures, etc.

     clear.up.tables()

     // Read the parameters.

     IF rdargs("FROM/A,TO/K,OPT/K", argv, 100) = 0 THEN
       collapse("Invalid parameters")

     // Set up a new 'wrch', to count characters written.

     wrch                 := my.wrch
     chars.printed        := 0

     // And open the files.

     instream := findinput(argv!0)

     IF instream = 0 THEN
       collapse("Can't open *"%S*"", argv!0)

     selectinput(instream)

     IF argv!1 ~= 0 THEN
       //
       $( outstream := findoutput(argv!1)

          IF outstream = 0 THEN
            collapse("Can't open *"%S*"", argv!1)

          selectoutput(outstream)
       $)

     // Decode the options, if any.

     IF argv!2 ~= 0 THEN
       //
       $( LET opts = argv!2

          FOR j = 1 TO opts % 0 DO
            //
            SWITCHON capitalch(opts % j) INTO

              $( CASE 'U': unformatted           := yes; ENDCASE
                 CASE 'M': full.map              := yes; ENDCASE
                 CASE 'E': print.symdefs         := yes; ENDCASE
                 CASE 'A': disassemble.all.words := yes; ENDCASE
                 CASE 'N': narrow.output         := yes; ENDCASE
                 CASE 'Q': print.hunks           := no;  ENDCASE
              $)
       $)

     // Disassemble the file.

     TEST unformatted THEN
       //
       $( LET word1 = ?

          IF get.first.word(@ word1) THEN
            disassemble.file(word1)
       $)
      ELSE
       read.tripos.binary()

     // Finish off.

     collapse(0)
  $)




AND collapse(string, a, b, c) BE
  //
  // Stops the program with the error message given by
  //  'string'.  If 'string' is zero, no message is printed
  //  and a return code of zero is set.  Otherwise the code
  //  is non-zero.
  //
  $( // Free work vectors.

     clear.up.tables()

     // Reset standard version of 'wrch'

     wrch := standard.wrch

     // Close streams

     IF instream ~= 0 THEN
       endread()

     IF outstream ~= 0 THEN
       $( selectoutput(outstream)
          endwrite()
       $)

     selectoutput(standout)

     IF string = 0 THEN stop(0)

     writef(string, a, b, c)
     newline()

     stop(return.severe)
  $)




AND read.tripos.binary() BE
  //
  // Reads and disassembles the TRIPOS binary file that is
  //  the current input stream.
  //
  $( IF NOT get.first.word(@ bintype) THEN
       RETURN

     // Loop through the file.

     WHILE NOT exhausted DO

       $( SWITCHON bintype INTO

            $( CASE t.hunk:
                 //
                 $( LET size = getword()

                    map(yes, "t.hunk     %N", size)

                    // Note that 'read.hunk' returns with the next
                    //  binary type already read.

                    read.hunk(0, size)

                    LOOP
                 $)

               CASE t.abshunk:
                 //
                 $( LET base = getword()
                    LET size = getword()

                    map(yes, "t.abshunk  %U  %N", base, size)

                    read.hunk(base, size)

                    LOOP
                 $)

               CASE t.end:
                 //
                 map(no, "t.end")
                 ENDCASE

               CASE t.block:
                 //
                 $( LET size = getword()

                    map(yes, "t.block    %U", size)

                    // Note that 't.block's are not followed
                    //  by any data, but may contain symbol
                    //  setting information.

                    read.hunk(0, 0)

                    LOOP
                 $)

               CASE t.table:
                 //
                 $( LET u = getword()
                    LET i = getword()
                    LET m = getword()

                    map(no, "t.table    %N  %N  %N", u, i, m)

                    FOR j = i TO m DO getword()

                    ENDCASE
                 $)

               CASE t.break:
                 //
                 map(no, "t.break")
                 ENDCASE

               CASE t.overlay:
                 //
                 $( LET u = getword()
                    LET m = getword()

                    map(no, "t.overlay  %N  %N", u, m)

                    FOR j = 1 TO u DO getword()

                    ENDCASE
                 $)

               DEFAULT:
                 //
                 writef("Un(known/expected) binary type %N*
                        * - starting simple scan*N*N", bintype)

                 disassemble.file(bintype)
                 RETURN

            $)

          exhausted := readwords(@ bintype, 1) = 0

       $)

     writes("End of file*N")
  $)




AND read.hunk(base, size) BE
  //
  // Reads and disassembles a hunk, with the given load base and size.
  //
  $( LET v    = getvector(size)
     LET hunk = v - base

     // Read the binary words of the file.

     size := getwords(v, size)

     // Check for relocation information.

     read.relocation()

     // Print the hunk's contents.
     //  (or only symbols if Q option was used).

     disassemble.hunk(hunk, base, base + size - 1)

     // Clear up relocation and symbol information.

     clear.up.tables()
  $)




AND read.relocation() BE
  //
  // Reads 't.reloc', 't.absreloc', 't.ext' and 't.lkedext'
  //  relocation blocks.
  //
  $( bintype := getword()

     SWITCHON bintype INTO

       $( CASE t.reloc:
          CASE t.absreloc:
            //
            $( LET n = getword()

               map(no, (bintype=t.reloc -> "t.reloc    %N", "t.absreloc %N"), n)

               read.rel(n, rt.local, ?)

               ENDCASE
            $)


          CASE t.ext:
            //
            map(no, "t.ext")

            $( LET s      = getword()
               LET symbol = ?

               IF s = 0 THEN BREAK

               symbol := read.symbol(s)

               IF st.deftype ! symbol = 0 THEN
                 read.rel(st.value ! symbol, rt.ext, symbol)

            $) REPEAT

            ENDCASE


          CASE t.lkedext:
            //
            map(no, "t.lkedext")

            $( LET n = getword()

               IF n = 0 THEN BREAK

               read.rel(n, rt.hunk, getword())

            $) REPEAT

            ENDCASE


          DEFAULT:
            //
            RETURN

       $)

  $) REPEAT




AND read.symbol(w1) BE
  //
  // Reads the symbol whose first word is 'w1'.
  //
  $( LET sym  = getblk(size.st)
     LET p    = 0
     LET word = w1
     LET type = w1 >> 8
     LET len  = 0

     FOR j = 1 TO 7 DO
       //
       $( LET ch = ?

          p := p + 1

          TEST (p & 1) = 0 THEN
            //
            $( word := getword()
               ch   := ascii(word >> 8)
            $)
           ELSE
            //
            ch := ascii(word & #XFF)

          (st.name + sym) % p := ch

          IF ch ~= ' ' THEN
            len := p
       $)

     (st.name + sym) % 0 := len

     IF type = ext.common THEN
       getword()

     st.value   ! sym := getword()
     st.deftype ! sym := (type >= 128 -> 0, (type = ext.defrel -> 1,2))
     st.link    ! sym := symchain
     symchain         := sym
  $)




AND read.rel(n, rt, val) BE
  //
  // Reads 'n' relocation locations of bintype 'rt' and value 'val'.
  //
  FOR j = 1 TO n DO
    //
    $( LET loc    = getword()
       LET lv.rel = @ relpointer
       LET relb   = ?

       IF exhausted THEN LOOP

       relb           := getblk(size.rel)

       rel.loc ! relb := loc
       rel.rt  ! relb := rt
       rel.val ! relb := val

       // Insert location into correct place in relocation chain.

       WHILE ! lv.rel ~= 0 & (rel.loc ! (! lv.rel) - loc) < 0 DO
         lv.rel := ! lv.rel + rel.link

       rel.link ! relb := ! lv.rel
       ! lv.rel        := relb
    $)




AND map(always, string, a, b, c, d, e) BE
  //
  IF always | full.map THEN
    //
    $( writef(string, a, b, c, d, e)
       writes("*N*N")
    $)




AND disassemble.hunk(hunk, first, last) BE
  //
  // Disassembles the words in the vector 'hunk', with
  //  subscripts 'first' to 'last'.
  //
  // First print symbol definitions, if required.
  //
  $( IF print.symdefs THEN
       //
       $( LET s = symchain
          LET p = no

          WHILE s ~= 0 DO
            //
            $( IF st.deftype ! s ~= 0 THEN
                 //
                 $( writes(st.name + s)

                    set.char.number(9)
                    writes(" EQU  ")
                    IF st.deftype ! s = 1 THEN wrch('L')
                    writef("%U0*N", st.value ! s)

                    p := yes
                 $)

               s := st.link ! s
            $)

          IF p THEN
            newline()
       $)

     disassemble.next.word := yes

     // There is nothing more to do if the Q option was
     //  requested.

     IF NOT print.hunks THEN
       RETURN

     FOR loc = first TO last DO
       //
       $( LET single = loc = last
          LET word1  = hunk ! loc
          LET word2  = single -> 0, hunk ! (loc+1)

          disassemble.words(word1, word2, loc, single)
       $)

     newline()
  $)




AND disassemble.file(word1) BE
  //
  // Disassembles the rest of the file without checking for
  //  TRIPOS binary format.
  //
  // The first word to print is in 'word1'.
  //
  $( LET location = 0
     LET pos      = 0
     LET count    = 1
     LET buffer   = VEC 99

     WHILE count ~= 0 DO
       //
       $( LET word2 = ?

          pos := pos + 1

          IF pos = count THEN
            $( count := ABS readwords(buffer, 100)
               pos   := 0
            $)

          word2 := buffer ! pos

          disassemble.words(word1, word2, location, count = 0)

          location := location + 1
          word1    := word2
       $)

     writes("*NEnd of file*N")
  $)




AND disassemble.words(word1, word2, loc, single) BE
  //
  // Disassembles the word 'word1' (and 'word2' for two-word)
  //  instructions), which occurs at location 'loc'.
  //
  // If 'single' is true, the instruction is known to be
  //  single length.
  //
  $( LET dis    = disassemble.next.word

     check.break()

     print.location(loc)

     disassemble.next.word := yes

     // If necessary, print in assembler format.
     // This is only done is the word is not relocatable.

     // First move past relocations for previous words.

     WHILE relpointer ~= 0 & (rel.loc ! relpointer - loc) < 0 DO
       relpointer := rel.link ! relpointer

     IF dis & (relpointer = 0 | rel.loc ! relpointer ~= loc) THEN
       disassemble.instruction(word1, word2, loc, single)

          // Print in other formats.

     print.word(word1, loc)
  $)




AND print.location(loc) BE
  //
  writef("%U5: ", loc)




AND disassemble.instruction(w1, w2, loc, single) BE
  //
  // Prints the instruction, with words 'w1' and 'w2', at
  //  location 'loc' in assembler form.
  //
  // If 'single' is true, the instruction must be single
  //  length, whatever the opcode.
  //
  // Instructions are divided into several groups:
  //
  //   1. Short memory reference
  //   2. Extended memory reference (two-word)
  //   3. Immediate
  //   4. Register-register
  //   5. Bit change/test
  //   6. Shifts and rotates
  //   7. Conditional jumps
  //   8. Input/output
  //   9. Floating point
  //  10. Miscellaneous
  //
  // If the instruction cannot be decoded, then nothing
  //  is printed.
  //
  $( // Short memory reference instructions can be recognised
     //  easily by a set top bit.

     IF (w1 & #X8000) ~= 0 THEN
       $( short.memory.reference(loc, w1)
          RETURN
       $)

     // Other groups can be distinguished by considering bits
     //  12-8 of the instruction.
     //
     //    xxxbbbbbxxxxxxxx          group
     //
     //       00111             extended memory reference
     //   01000 - 01101         immediate
     //   10111 - 11001         immediate
     //       x0000             register-register or bit operation
     //       0111x             shift or rotate, and some misc.
     //   10001 - 10110         conditional jump
     //   00001 - 00100         input/output and some misc.
     //       1101x             traps
     //       11100             floating point

     SWITCHON (w1 >> 8) & #B11111 INTO

       $( CASE #B00111:  // Extended memory reference.
            //
            IF NOT single THEN
              extended.memory.reference(w1, w2, loc)
            ENDCASE


          CASE #B01000:  // Immediate operations
          CASE #B01001:
          CASE #B01010:
          CASE #B01011:
          CASE #B01100:
          CASE #B01101:
          CASE #B10111:
          CASE #B11000:
          CASE #B11001:
            //
            immediate(w1)
            ENDCASE


          CASE #B00000:  // Register-register or bit operation
          CASE #B10000:
            //
            register.register.or.bit(w1)
            ENDCASE


          CASE #B01110:  // Shift, rotate and misc.
          CASE #B01111:
            //
            shift.rotate.or.misc(w1)
            ENDCASE


          CASE #B10001:  // Conditional jump.
          CASE #B10010:
          CASE #B10011:
          CASE #B10100:
          CASE #B10101:
          CASE #B10110:
            //
            conditional.jump(w1, loc)
            ENDCASE


          CASE #B00001:  // Input/output and misc.
          CASE #B00010:
          CASE #B00011:
          CASE #B00100:
          CASE #B00101:
          CASE #B00110:
            //
            // Test for PUSH (:4309) specially, since it is
            //  the only one which is double length.
            //
            TEST w1 = #X4309 THEN
              //
              IF NOT single THEN push(w2, loc)
             ELSE
              input.output.misc(w1)
            ENDCASE


          CASE #B11010:  // Trap instructions
          CASE #B11011:
            //
            traps(w1)
            ENDCASE


          CASE #B11100:  // Floating point.
            //
            // Floating point instructions have the form:
            //
            //   01011100xxxxxxxx
            //
            // Other opcodes are in fact character/numeric instructions,
            //  but this disassembler isn't brave enough to try to cope
            //  with them.
            //
            IF single THEN ENDCASE

            IF (w1 >> 8) = #B01011100 THEN
              floating.point(w1, w2, loc)

            ENDCASE

       $)
  $)





AND short.memory.reference(loc, word) BE
  //
  // Prints the short instruction 'word', which is at
  //  location 'loc'.
  //
  // The three possible formats of short instructions are:
  //
  //   1rroooRIdddddddd
  //   1rooooRIdddddddd
  //   1oooooRIdddddddd
  //
  //  r indicates a register field.
  //  o is an opcode field.
  //  R is the relative bit.
  //  I is the indirect bit.
  //  d indicates the displacement, which, for relative addressing,
  //     is eight bits, and for absolute addressing, contains X and
  //     Y indexing bits, and a six-bit offest.
  //
  // The instructions in detail are:
  //
  //   1rr000RIdddddddd       COPY  addr,r
  //   1rr001RIdddddddd       COPY  r,addr
  //   1rr010RIdddddddd       ADD   addr,r
  //   1rr011RIdddddddd       SUB   addr,r
  //   1rr100RIdddddddd       EXCH  addr,r
  //
  //   1r0101RIdddddddd       AND   addr,r
  //   1r0110RIdddddddd       XOR   addr,r
  //   1r1101RIdddddddd       OR    addr,r
  //   1r1110RIdddddddd       CSK   r,addr
  //
  //   100111RIdddddddd       JMP   addr
  //   101111RIdddddddd       JST   addr
  //   110111RIdddddddd       IMS   addr
  //   111111RIdddddddd       JSK   addr
  //
  $( LET uses.register = yes
     LET tostore       = no
     LET disp          = word & #B11111111
     LET mode          = (word >> 6) & #B1111
     LET register      = (word >> 12) & #B110
     LET regchar       = ?
     LET op.bits3      = (word >> 10) & #B111
     LET bit13         = (word >> 13) & #B1
     LET bit14         = (word >> 14) & #B1
     LET operation     = ?

     // Consider the bottom three bits of the opcode field.

     SWITCHON op.bits3 INTO

       $( CASE #B000:
          CASE #B001:
            //
            operation := "COPY"
            tostore   := op.bits3 = #B001
            ENDCASE


          CASE #B010:
            //
            operation := "ADD "
            ENDCASE


          CASE #B011:
            //
            operation := "SUB "
            ENDCASE


          CASE #B100:
            //
            operation := "EXCH"
            ENDCASE


          CASE #B101:
            //
            // One bit register field, four bit opcode field.
            //
            register  := register & #B100
            operation := bit13 = 0 -> "AND ", "OR  "
            ENDCASE


          CASE #B110:
            //
            // As above.
            //
            register := register & #B100

            TEST bit13 = 0 THEN
              //
              operation := "XOR "
             ELSE
              //
              $( operation := "CSK "
                 tostore   := yes
              $)

            ENDCASE


          CASE #B111:
            //
            // No register bits, and a five bit opcode field.
            //
            uses.register := no

            operation     := bit13 = 0 -> (bit14 = 0 -> "JMP ", "IMS "),
                                          (bit14 = 0 -> "JST ", "JSK ")
            ENDCASE

       $)

     // Now print the operation.

     writef("%S   ", operation)

     // If 'tostore' is true, the register must be printed first.

     IF uses.register THEN
       regchar := register.char(register)

     IF tostore & uses.register THEN
       writef("%C,", regchar)

     // Now check for a relative address.

     TEST (mode & #B1000) ~= 0 THEN
       //
       $( disp := disp - 128
          mode := mode & #B1100
       $)
      ELSE
       disp := disp & #B111111

     // Now print the address.

     print.address(mode, disp, loc, yes)

     // Finally, print the register if necessary.

     IF NOT tostore & uses.register THEN
       writef(",%C", regchar)
  $)




AND extended.memory.reference(word1, word2, loc) BE
  //
  // Prints the extended instruction whose words are 'word1'
  //  and 'word2', and whose address is 'loc'.
  //
  // The possible formats of extended instructions are:
  //
  //   0rr00111RIXYoooo
  //   0ro00111RIXYoooo
  //   0oo00111RIXYoooo
  //
  // r indicates a register field.
  // o indicates the opcode field(s).
  // RIXY is the mode (relative, indirect, X index, Y index)
  //
  // The second word is a 16 bit displacement, relative or absolute.
  //
  // The instructions in detail are:
  //
  //   0rr00111RIXY0000       MUL   addr,Ar
  //   0rr00111RIXY0001       DIV   addr,Ar
  //   0rr00111RIXY0100       CEA   addr,r
  //   0rr00111RIXY0101       CMSE  r,addr
  //   0rr00111RIXY0110       COPYE addr,Ar
  //   0rr00111RIXY0111       COPYE Ar,addr
  //   0rr00111RIXY1000       COPYE addr,r
  //   0rr00111RIXY1001       COPYE r,addr
  //   0rr00111RIXY1010       ADDE  addr,r
  //   0rr00111RIXY1011       SUBE  addr,r
  //   0rr00111RIXY1100       EXCHE addr,r
  //
  //   0r000111RIXY0010       ADDC  addr,r
  //   0r100111RIXY0010       SUBC  addr,r
  //   0r000111RIXY1110       ANDE  addr,r
  //   0r100111RIXY1110       ORE   addr,r
  //   0r000111RIXY1111       XORE  addr,r
  //   0r100111RIXY1111       CSKE  r,addr
  //
  //   00000111RIXY0011       JSKE  addr
  //   00000111RIXY1101       JMPE  addr
  //   00100111RIXY1101       JSTE  addr
  //   01000111RIXY1101       IMSE  addr
  //   01100111RIXY1101       DMSE  addr
  //
  // In the above, Ar indicates a register pair (AQ, AX or AY).
  //
  $( LET uses.register = yes
     LET register      = (word1 >> 12) & #B110
     LET regchar       = ?
     LET register.pair = no
     LET tostore       = no
     LET operation     = ?
     LET op.bits4      = word1 & #B1111
     LET bit13         = (word1 >> 13) & #B1
     LET bit14         = (word1 >> 14) & #B1

     // Find the operation and details of register usage.

     SWITCHON op.bits4 INTO

       $( CASE #B0000:
            //
            register.pair := yes
            operation     := "MUL  "
            ENDCASE


          CASE #B0001:
            //
            register.pair := yes
            operation     := "DIV  "
            ENDCASE


          CASE #B0010:
            //
            register  := register & #B100
            operation := bit13 = 0 -> "ADDC ", "SUBC "
            ENDCASE


          CASE #B0011:
            //
            // Only :0703 is allocated, as JSKE.
            //
            IF bit13 ~= 0 | bit14 ~= 0 THEN
              RETURN

            uses.register := no
            operation     := "JSKE "
            ENDCASE


          CASE #B0100:
            //
            operation := "CEA  "
            ENDCASE


          CASE #B0101:
            //
            tostore   := yes
            operation := "CSME "
            ENDCASE


          CASE #B0110:
          CASE #B0111:
          CASE #B1000:
          CASE #B1001:
            //
            register.pair := op.bits4 <= #B0111
            tostore       := (op.bits4 & #B1) ~= 0
            operation     := "COPYE"
            ENDCASE


          CASE #B1010:
            //
            operation := "ADDE "
            ENDCASE


          CASE #B1011:
            //
            operation := "SUBE "
            ENDCASE


          CASE #B1100:
            //
            operation := "EXCHE"
            ENDCASE


          CASE #B1101:
            //
            uses.register := no
            operation     := bit13 = 0 -> (bit14 = 0 -> "JMPE ", "IMSE "),
                                          (bit14 = 0 -> "JSTE ", "DMSE ")
            ENDCASE


          CASE #B1110:
            //
            register  := register & #B100
            operation := bit13 = 0 -> "ANDE ", "ORE  "
            ENDCASE


          CASE #B1111:
            //
            register  := register & #B100

            TEST bit13 = 0 THEN
              //
              operation := "XORE "
             ELSE
              //
              $( tostore   := yes
                 operation := "CSKE "
              $)

            ENDCASE

       $)


     // Now print the operation.

     writef("%S  ", operation)

     // Now, if required, the register first.

     regchar := register.char(register)

     IF tostore & uses.register THEN
       //
       $( IF register.pair THEN
            wrch('A')

          writef("%C,", regchar)
       $)

     // Now the address.

     print.address((word1 >> 4) &#B1111, word2, loc + 1, no)

     // The register, if not a 'to store' operation.

     IF NOT tostore & uses.register THEN
       //
       $( wrch(',')

          IF register.pair THEN
            wrch('A')

          wrch(regchar)
       $)

     disassemble.next.word := disassemble.all.words
  $)




AND immediate(word) BE
  //
  // Prints the immediate instruction given be 'word'.
  //
  // The instructions are:
  //
  //   0rr01000iiiiiiii       COPY  =i-256,r
  //   0rr01001iiiiiiii       COPY  =i,r
  //   0rr01010iiiiiiii       SUB   =256-i,r
  //   0rr01011iiiiiiii       ADD   =i,r
  //   0rr01100iiiiiiii       CLSN  r,=i
  //   0rr01101iiiiiiii       CSK   r,=i
  //   0rr10111iiiiiiii       XOR   =i,r
  //   0rr11000iiiiiiii       AND   =i,r
  //   0rr11001iiiiiiii       OR    =i,r
  //
  // r indicates a register field, and i is an 8-bit immediate
  //  operand in the range 0-255.  (Note the special load negative
  //  and subtract cases).
  //
  $( LET immediate = word & #B11111111
     LET reversed  = no
     LET regchar   = register.char((word >> 12) & #B110)
     LET operation = ?

     SWITCHON (word >> 8) & #B11111 INTO

       $( CASE #B01000:
            //
            immediate := immediate - 256


          CASE #B01001:
            //
            operation := "COPY"
            ENDCASE


          CASE #B01010:
            //
            immediate := 256 - immediate
            operation := "SUB "
            ENDCASE


          CASE #B01011:
            //
            operation := "ADD "
            ENDCASE


          CASE #B01100:
            //
            reversed  := yes
            operation := "CLSN"
            ENDCASE


          CASE #B01101:
            //
            reversed  := yes
            operation := "CSK "
            ENDCASE

          CASE #B10111:
            //
            operation := "XOR "
            ENDCASE


          CASE #B11000:
            //
            operation := "AND "
            ENDCASE


          CASE #B11001:
            //
            operation := "OR  "
            ENDCASE


          DEFAULT:
            //
            // Not an immediate operation.
            //
            RETURN

       $)

     writef("%S   ", operation)

     // Print the immediate operand and the register.

     IF reversed THEN
       writef("%C,", regchar)

     writef("=%N", immediate)

     IF NOT reversed THEN
       writef(",%C", regchar)

  $)




AND register.register.or.bit(word) BE
  //
  // Prints the reg-reg (or bit change) instruction 'word'.
  //
  // The possible formats are:
  //
  //   0ddd00000sssoooo
  //   0rr00000kkkkoooo
  //
  //  d and s indicate register fields.
  //  o is the opcode.
  //  r is a register field.
  //  k is a bit number (0-15).
  //
  // The instructions in detail are:
  //
  //   0ddd00000sss0000       COPY  s,d
  //   0ddd00000sss0001       NEG   s,d
  //   0ddd00000sss0010       ADD   s,d
  //   0ddd00000sss0011       SUB   s,d
  //   0ddd00000sss0100       AND   s,d
  //   0ddd00000sss0101       XOR   s,d
  //   0ddd00000sss0110       CSM   d,s
  //   0ddd00000sss0111       OR    s,d
  //   0ddd00000sss1000       EXCH  s,d
  //   0ddd00000sss1001       COMP  s,d
  //   0ddd00000sss1010       CSN   s,d
  //   0ddd00000sss1011       CSK   d,s
  //
  //   0rr00000kkkk1100       SBIT  k,r
  //   0rr00000kkkk1101       RBIT  k,r
  //   0rr00000kkkk1110       CBIT  k,r
  //   0rr00000kkkk1111       TBIT  k,r
  //
  $( LET op.bits4 = word & #B1111

     TEST op.bits4 >= #B1100 THEN
       //
       // Bit operation.
       //
       $( LET regchar = register.char((word >> 12) & #B110)
          LET bitnum  = (word >> 4) & #B1111

          writef("%CBIT   %N,%C", "SRCT" % (op.bits4 - 11), bitnum, regchar)
       $)
      ELSE
       //
       $( LET schar = register.char((word >>  4) & #B111)
          LET dchar = register.char((word >> 12) & #B111)
          LET rev   = no
          LET op    = ?

          SWITCHON op.bits4 INTO

            $( CASE #B0000: op := "COPY";             ENDCASE

               CASE #B0001: op := "NEG ";             ENDCASE

               CASE #B0010: op := "ADD ";             ENDCASE

               CASE #B0011: op := "SUB ";             ENDCASE

               CASE #B0100: op := "AND ";             ENDCASE

               CASE #B0101: op := "XOR ";             ENDCASE

               CASE #B0110: op := "CSM "; rev := yes; ENDCASE

               CASE #B0111: op := "OR  ";             ENDCASE

               CASE #B1000: op := "EXCH";             ENDCASE

               CASE #B1001: op := "COMP";             ENDCASE

               CASE #B1010: op := "CSN ";             ENDCASE

               CASE #B1011: op := "CSK "; rev := yes; ENDCASE

            $)

          // For CSM and CSK, reverse the register fields.

          IF rev THEN
            $( LET sc = schar
               schar := dchar
               dchar := sc
            $)

          writef("%S   %C,%C", op, schar, dchar)
       $)
  $)




AND shift.rotate.or.misc(word) BE
  //
  // Prints the shift (or rotate) instruction 'word'.
  //
  // Bit operations on S and some miscellaneous instructions are
  //  also handled by this routine.
  //
  // The format of shift operations is:
  //
  //   0rr0111pnnnnoooo
  //
  //  r indicates a register field.
  //  p is one for a register pair operation.
  //  n is the number of bits to shift, minus one.
  //  o is the opcode.
  //
  // The instructions in detail are:
  //
  //   0rr01110nnnn0000       SHIFT  r,L,b
  //   0rr01110nnnn0001       SHIFT  r,LO,b
  //   0rr01110nnnn0010       ROTATE r,L,b
  //   0rr01110nnnn0011       ROTATE r,LO,b
  //   0rr01110nnnn1000       SHIFT  r,R,b
  //   0rr01110nnnn1001       SHIFT  r,RO,b
  //   0rr01110nnnn1010       ROTATE r,R,b
  //   0rr01110nnnn1011       ROTATE r,RO,b
  //   0rr01110nnnn1100       SHIFT  r,RA,b
  //   0rr01111nnnn0000       SHIFT  Ar,L,b
  //   0rr01111nnnn0001       SHIFT  Ar,LO,b
  //   0rr01111nnnn0010       ROTATE Ar,L,b
  //   0rr01111nnnn0011       ROTATE Ar,LO,b
  //   0rr01111nnnn1000       SHIFT  Ar,R,b
  //   0rr01111nnnn1001       SHIFT  Ar,RO,b
  //   0rr01111nnnn1010       ROTATE Ar,R,b
  //   0rr01111nnnn1011       ROTATE Ar,RO,b
  //   0rr01111nnnn1100       SHIFT  Ar,RA,b
  //
  //   00001110kkkk0100       TBIT   k,S
  //   00001110kkkk0101       SBIT   k,S
  //   00001110kkkk0110       CBIT   k,S
  //   00001110kkkk0111       RBIT   k,S
  //
  //   0000111000001101       HLT
  //   00001110kkkk1111       SIN    k+1
  //
  // All other bit patters are assumed to be undefined.
  //
  // In the above, b is n+1, and Ar indicates a register pair.
  //
  // Thus, for SHIFT and ROTATE, individual bits are useful:
  //
  //    bit            meaning
  //
  //     0                O
  //     1              ROTATE
  //     2                A     (only used with SHIFT RA)
  //     3                R
  //
  $( LET op.bits4 = word & #B1111
     LET regchar  = register.char((word >> 12) & #B110)
     LET count    = (word >> 4) & #B1111

     // Check for a bit operation on S first.

     IF #B0100 <= op.bits4 <= #B0111 THEN
       //
       // Bits 14-12 and 8 must all be zero.
       //
       $( IF (word & #B0110000100000000) ~= #B0 THEN
            RETURN

          writef("%CBIT   %N,S", "TSCR" % (op.bits4 - 3), count)

          RETURN
       $)

     // Now check for the miscellaneous instructions, which have
     //  opcodes 1101 and 1111.

     IF op.bits4 >= #B1101 THEN
       //
       // The only instructions bit-patterns assigned are for HLT and SIN.
       //
       $( IF word = #X0E0D THEN
            //
            $( writes("HLT")
               RETURN
            $)

          IF (word & #XFF0F) = #X0E0F THEN
            //
            writef("SIN    %N", count + 1)

          RETURN
       $)

     // Now the instruction is known to be a shift or rotate.
     // Write out the operation.

     writes(((word & #B10) = 0 -> "SHIFT  ", "ROTATE "))

     // The register comes next - if bit 8 is set, it is a pair.

     IF (word & #B100000000) ~= 0 THEN
       wrch('A')

     // Write out the register and direction (L or R).

     writef("%C,%C", regchar, ((word & #B1000) = 0 -> 'L', 'R'))

     // Write out O (for shifts through the OV bit), or A (for
     //  arithmetic shifts). Note that these two bits cannot be
     //  set together.

     IF (word & #B1) ~= 0 THEN
       wrch('O')

     IF (word & #B100) ~= 0 THEN
       wrch('A')

     // Now write out the number of places to shift.

     writef(",%N", count+1)
  $)




AND conditional.jump(word, loc) BE
  //
  // Prints the conditional jump 'word' at location 'loc'.
  //
  // The possible formats are:
  //
  //   0rr10ooooddddddd
  //   0oo10110fddddddd
  //
  // r is a register field.
  // o is the opcode.
  // d is the seven-bit displacement. (:3F = $)
  // f is one for a JF, zero for JT.
  //
  // The instructions in detail are:
  //
  //   0rr100010ddddddd       JEQ   r,addr
  //   0rr100011ddddddd       JNE   r,addr
  //   0rr100100ddddddd       JGT   r,addr
  //   0rr100101ddddddd       JLE   r,addr
  //   0rr100110ddddddd       JGE   r,addr
  //   0rr100111ddddddd       JLT   r,addr
  //   0rr101000ddddddd       IJEQ  r,addr
  //   0rr101001ddddddd       IJNE  r,addr
  //   0rr101010ddddddd       JEQD  r,addr
  //   0rr101011ddddddd       JNED  r,addr
  //
  //   000101100ddddddd       JT    SS,addr
  //   000101101ddddddd       JF    SS,addr
  //   001101100ddddddd       JT    OV,addr
  //   001101101ddddddd       JF    OV,addr
  //   010101100ddddddd       JT    CY,addr
  //   010101101ddddddd       JF    CY,addr
  //
  $( LET op.bits4 = (word >> 7) & #B1111
     LET disp     = (word & #B1111111) - 64
     LET op.bits2 = (word >> 13) & #B11

     // Distinguish between JT/JF jumps and the others.

     TEST op.bits4 >= #B1100 THEN
       //
       // There are some unassigned opcodes.
       //
       $( IF op.bits4 >= #B1110 | op.bits2 = #B11 THEN
            RETURN

          writef("J%C     ", ((op.bits4 & #B1) = 0 -> 'T', 'F'))

          // Now the SS, OV or CY.

          wrch("SOC" % (op.bits2 + 1))
          wrch("SVY" % (op.bits2 + 1))
       $)
      ELSE
       //
       $( LET regchar = register.char(op.bits2 << 1)
          LET op      = ?

          SWITCHON op.bits4 INTO

            $( CASE #B0010: op := "JEQ "; ENDCASE

               CASE #B0011: op := "JNE "; ENDCASE

               CASE #B0100: op := "JGT "; ENDCASE

               CASE #B0101: op := "JLE "; ENDCASE

               CASE #B0110: op := "JGE "; ENDCASE

               CASE #B0111: op := "JLT "; ENDCASE

               CASE #B1000: op := "IJEQ"; ENDCASE

               CASE #B1001: op := "IJNE"; ENDCASE

               CASE #B1010: op := "JEQD"; ENDCASE

               CASE #B1011: op := "JNED"; ENDCASE

            $)

          // Write out the operation and the register.

          writef("%S   %C", op, regchar)
       $)

     // Now, in both cases, output the destination address.

     wrch(',')

     print.address(#B1000, disp, loc, yes)
  $)




AND push(word2, loc) BE
  //
  // Prints a two-word PUSH instruction.
  // For this instruction, the second word is not marked
  //  for "don't disassemble", since PUSH instructions are
  //  probably rare, and may be data.
  //
  $( writes("PUSH   ")
     print.address(0, word2, loc + 1, no)
  $)




AND input.output.misc(word) BE
  //
  // Prints a variety of instructions, including input/output
  //  and miscellaneous instructions such as FMT and UIS.
  //
  // The instructions in detail are:
  //
  //   0rr00001dddddddd       IN    dafc,r
  //   0rr00010dddddddd       OUT   r,dafc
  //
  //   0rr0001100001000       FMT   r
  //   0000001100001001       UIS
  //   0rr0001100001010       XNX   r
  //   0010001100001001       RSK
  //   0110001100001001       POP
  //
  //   0rr00100dddddddd       SELP  r,dafc
  //   00000101dddddddd       AIN   dafc
  //   00100101dddddddd       AOT   dafc
  //   01000101dddddddd       AIB   dafc
  //   01100101dddddddd       AOB   dafc
  //   00000110dddddddd       SST   dafc
  //
  // r is a register field.
  // d indicates the eight bit 'device and function code' (dafc) field.
  //
  $( LET op.bits3      = (word >>  8) & #B111
     LET op.bits2      = (word >> 13) & #B11
     LET uses.register = yes
     LET regchar       = register.char(op.bits2 << 1)
     LET op            = ?

     // Check for the more bizarre instructions first.

     IF op.bits3 = #B011 THEN
       //
       $( LET op.bits4 = word & #B1111

          SWITCHON op.bits4 INTO

            $( CASE #B1000: writef("FMT    %C", regchar);         ENDCASE

               CASE #B1001:
                 //
                 SWITCHON op.bits2 INTO

                   $( CASE #B00: op := "UIS"; ENDCASE
                      CASE #B01: op := "RSK"; ENDCASE
                      CASE #B10: RETURN
                      CASE #B11: op := "POP"; ENDCASE
                   $)

                 writes(op)

                 ENDCASE

               CASE #B1010: writef("XNX    %C", regchar);         ENDCASE

               CASE #B1100: IF op.bits2 = 0 THEN writef("FIX");   ENDCASE

               CASE #B1101: IF op.bits2 = 0 THEN writef("FLOAT"); ENDCASE

            $)

          RETURN
       $)

     // Now that those nasties have been disposed of, what's left
     //  must be input/output of some sort.

     SWITCHON op.bits3 INTO

       $( CASE #B001: op := "IN  "; ENDCASE

          CASE #B010: op := "OUT "; ENDCASE

          CASE #B100: op := "SELP"; ENDCASE

          CASE #B101:
            //
            uses.register := no

            SWITCHON op.bits2 INTO

              $( CASE #B00: op := "AIN "; ENDCASE
                 CASE #B01: op := "AOT "; ENDCASE
                 CASE #B10: op := "AIB "; ENDCASE
                 CASE #B11: op := "AOB "; ENDCASE
              $)

            ENDCASE

          CASE #B110:
            //
            // SST, but the register field must be zero.
            //
            IF op.bits2 ~= 0 THEN RETURN

            uses.register := no
            op            := "SST "

            ENDCASE

       $)

     // Write out the operation.

     writef("%S   ", op)

     // And the register, first for OUT and SELP

     IF uses.register & op.bits3 ~= #B001 THEN
       writef("%C,", regchar)

     // Now the dafc.

     writef(":%X2", word & #B11111111)

     // If IN, output the register second.

     IF op.bits3 = #B001 THEN
       writef(",%C", regchar)
  $)




AND traps(w) BE
  //
  // 0rr11010kkkkkkkk    STRAP  r,k
  // 0rr11011kkkkkkkk    UTRAP  r,k
  //
  $( LET r = register.char((w >> 12) & #B110)
     LET k = w & #XFF
     LET c = ((w >> 8) & 1) = 0 -> 'S', 'U'

     writef("%CTRAP  %C,%N", c, r, k)
  $)




AND floating.point(word1, word2, loc) BE
  //
  // Prints the floating point operation at 'loc', given by
  //  'word1' and 'word2'.
  //
  // The instructions in detail are:
  //
  //   01011100RIXY0000       ADDF  addr
  //   01011100RIXY0001       SUBF  addr
  //   01011100RIXY0010       MULF  addr
  //   01011100RIXY0011       DIVF  addr
  //   01011100RIXY0100       CSKF  addr
  //
  //  RIXY are the mode bits.
  //
  $( LET op.bits4 = word1 & #B1111
     LET op       = ?

     SWITCHON op.bits4 INTO

       $( CASE #B0000: op := "ADDF"; ENDCASE

          CASE #B0001: op := "SUBF"; ENDCASE

          CASE #B0010: op := "MULF"; ENDCASE

          CASE #B0011: op := "DIVF"; ENDCASE

          CASE #B0100: op := "CSKF"; ENDCASE

          DEFAULT:     RETURN
       $)

     writef("%S   ", op)

     print.address((word1 >> 4) & #B1111, word2, loc + 1, no)

     disassemble.next.word := disassemble.all.words
  $)




AND register.char(r) =
  //
  [TABLE 'A', 'P', 'X', 'S', 'Q', 'L', 'Y', 'K'] ! r




AND print.address(mode, word2, loc, use.dollar) BE
  //
  // Prints the address, referenced from 'loc', that is
  //  described by 'word2' and 'mode'.
  //
  // The mode contains the four mode bits from short or extended
  //  memory reference instructions, as follows:
  //
  //    RIXY              R = 1 for relative addressing, 0 for absolute.
  //                      I = 1 for indirection.
  //                      X = 1 for X indexing.
  //                      Y = 1 for Y indexing.
  //
  // 'word2' is a full-word value, which may be negative.
  //
  // For relative addressing, if 'use.dollar' is true, and the
  //  displacement is small (not more than 2), the address is
  //  printed as $+n or $-n.
  //
  // Rn indicates a relative reference to location n.
  // Ln indicates a relocatable reference to location n.
  //
  $( LET printed  = no

     // 'se.word2' is a sign-extended version of 'word2', for use when
     //  the program is running on a 32 (or less) bit machine.

     LET se.word2 = (word2 & #X8000) ~= 0 -> word2 | #XFFFF0000, word2

     // First check for indirection.

     IF (mode & #B100) ~= 0 THEN
       wrch('**')

     // Now deal with relative addressing.

     IF (mode & #B1000) ~= 0 THEN
       //
       $( LET destination = loc + se.word2 + 1
          LET disp        = destination - loc

          TEST use.dollar & ABS disp <= 2 THEN
            //
            $( wrch('$')

               IF disp > 0 THEN
                 wrch('+')

               IF disp ~= 0 THEN
                 writen(disp)
            $)
           ELSE
            writef("R%U0", destination)

          // Now the 'value' of 'word2' has been printed.

          printed := yes
          word2   := 0
       $)

     // Now deal with the relocation information.
     // 'relpointer' is left pointing at the relocation
     //  information for the current word, if any, since
     //  some words are printed as addresses twice.

     $( LET rp = relpointer

        WHILE rp ~= 0 & rel.loc ! rp = loc DO
          //
          $( IF printed THEN
               wrch('+')

             print.rel(word2, rel.rt ! rp, rel.val ! rp)

             // For local relocation, the value of the word will have
             //  been printed in the routine call above.

             IF rel.rt ! rp = rt.local THEN
               word2 := 0

             printed := yes
             rp      := rel.link ! rp
          $)
     $)

     // Now the final displacement and indexing information.

     IF NOT printed | word2 ~= 0 THEN
       //
       $( IF printed & se.word2 > 0 THEN
            wrch('+')

          writen(se.word2)
       $)

     SWITCHON mode & #B11 INTO

       $( CASE #B01: writes("(Y)");   ENDCASE
          CASE #B10: writes("(X)");   ENDCASE
          CASE #B11: writes("(X,Y)"); ENDCASE
       $)
  $)




AND print.rel(loc, rt, val) BE
  //
  SWITCHON rt INTO

    $( CASE rt.local:
         //
         writef("L%U0", loc)
         ENDCASE

       CASE rt.ext:
         //
         writes(st.name + val)
         ENDCASE

       CASE rt.hunk:
         //
         writef("H#%N", val)
         ENDCASE
    $)




AND print.word(word, loc) BE
  //
  // Prints the word 'word' (at location 'loc') in hexadecimal,
  //  decimal and ASCII character format.
  //
  // If the word is relocatable, only an address constant is
  //  printed.
  //
  $( LET se.word = (word & #X8000) = 0 -> word, word | #XFFFF0000

     set.char.number(32)
     writes("  ")

     // Check for relocation.

     IF relpointer ~= 0 & rel.loc ! relpointer = loc THEN
       //
       $( print.address(0, word, loc, no)
          newline()
          RETURN
       $)

     // Otherwise, print in hex., decimal and ASCII.
     // (Unless the 'narrow' option is set, in which case
     //  only the hex. is printed).

     writef(":%X4", word)

     IF NOT narrow.output THEN
       writef("   %I6   *"%C%C*"", se.word,ascii(word >> 8),ascii(word & #XFF))

     newline()
  $)




AND ascii(n) =
  //
  // Returns the character with ASCII code 'n', unless the
  //  character is not printable, in which case '?' is
  //  the result.
  //
  n < #X20 | n > #X7F -> '?',
                   " !*"#$%&'()**+,-./0123456789:;<=>?@ABCDEFGHIJKLMNO*
                   *PQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~?" % (n-#X1F)




AND getword() = VALOF
  //
  // Reads a word from the input file.
  //
  // If the file has ended, a message is output, and zero is returned.
  //
  TEST exhausted THEN
    RESULTIS 0
   ELSE
    $( LET w = ?

       IF readwords(@ w, 1) = 0 THEN
         //
         $( writes("File ended prematurely*N*N")
            exhausted := yes
            RESULTIS 0
         $)

       RESULTIS w
    $)




AND getwords(v, n) = VALOF
  //
  // Attempts to read 'n' words into vector 'v', returning the
  //  number of words actually read.
  //
  TEST exhausted THEN
    RESULTIS 0
   ELSE
    //
    $( LET read = ABS readwords(v, n)

       IF read ~= n THEN
         //
         $( writef("File ended while reading %N words*N*N", n)
            exhausted := yes
         $)

       RESULTIS read
    $)




AND get.first.word(lv.w) = VALOF
  //
  // Reads the first word of input into '!lv.w', returning
  //  true if successful.
  //
  $( IF readwords(lv.w, 1) = 0 THEN
       //
       $( writes("File is empty*N")
          RESULTIS no
       $)

     RESULTIS yes
  $)




AND clear.up.tables() BE
  //
  // Clears up all the data structures, by simply freeing
  //  all the allocated vectors.
  //
  $( WHILE work.vector.chain ~= 0 DO
       $( LET v = work.vector.chain
          work.vector.chain := ! v
          freevec(v)
       $)

     relpointer := 0
     symchain   := 0

     heapptr    := heap.block.size
  $)




AND getblk(size) = VALOF
  //
  $( LET p = ?

     IF heapptr + size > heap.block.size THEN
       //
       $( heap.block := getvector(heap.block.size)
          heapptr    := 0
       $)

     p       := heapptr
     heapptr := heapptr + size

     RESULTIS p + heap.block
  $)




AND getvector(size) = VALOF
  //
  $( LET v = getvec(size)

     IF v = 0 THEN
       //
       collapse("Insufficient free store")

     ! v               := work.vector.chain
     work.vector.chain := v

     RESULTIS v+1
  $)




AND check.break() BE
  //
  IF testflags(1) THEN collapse("****BREAK")




AND set.char.number(n) BE
  //
  FOR j = chars.printed TO n-1 DO wrch(' ')




AND my.wrch(ch) BE
  //
  $( TEST ch = '*N' THEN
       chars.printed := 0
      ELSE
       chars.printed := chars.printed + 1

     standard.wrch(ch)
  $)



