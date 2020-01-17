#  Make file for the IDW "make" program.
#
#  IDW  -  12/03/87


B         =  bcpl
O         =  obj

MAKE      =  o:make

HDR       =  makehdr
OLD       =  $[B]old-$(HDR)
NEW       =  $[B]new-$(HDR)

CWD       =  ""
BWD       =  bd:make


.PREFIXES~  $[B] $[O]


all~ $(MAKE)


sorthdr~
        sorthdr $(HDR) to $(NEW) ug ug
        copy    $(HDR) to $(OLD)
        copy    $(NEW) to $(HDR)


backup~
        copydir $(CWD) to $(BWD) update


print~
        list !here:+ opt dense,pm,u,nn,pn
          [$(HDR)]
          [$[B]make]
          [$[B]syntax]
          [$[B]execute]
        +


$(MAKE)~
        joinlib !here:+ as $(MAKE)
          [$[O]make]
          [$[O]syntax]
          [$[O]execute]
        +


$[B]$[O]~ $(NEW)
        bcp $< to $@ opt ws25000


