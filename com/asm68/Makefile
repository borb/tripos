#  Make file for the MC68000 assembler
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
          [$[O]m68kasm1]
          [$[O]m68kasm2]
          [$[O]m68kasm3]
          [$[O]m68kasm4]
          [$[O]m68kasm5]
          [$[O]m68kasm6]
        +


$[B]$[O]~ $(NEW)
        bcp $< to $@ opt ws30000/cc:68K/sw500


