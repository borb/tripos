#  Make file for the MC68020 assembler
#
#  IDW  -  27/02/87


B    =  bcpl
O    =  obj

HDR  =  m68khdr
NEW  =  $[B]new-$(HDR)
OLD  =  $[B]old-$(HDR)

BIN  =  t:asm


.PREFIXES~ $[B] $[O]


all~ $(BIN)


sorthdr~
        sorthdr $(HDR) to $(NEW) ug ug
        copy    $(HDR) to $(OLD)
        copy    $(NEW) to $(HDR)


$(BIN)~
        joinlib !here:+ as $(BIN)
          [$[O]m1]
          [$[O]m2]
          [$[O]m3]
          [$[O]m4]
          [$[O]m5]
          [$[O]m6]
          [$[O]m7]
          [$[O]m8]
          [$[O]m9]
          [$[O]m10]
          [$[O]m11]
          [$[O]m12]
        +


$[B]$[O]~ $(NEW)
        bcp $< to $@ opt ws30000/sw500


