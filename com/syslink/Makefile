#  Make file for the "syslink" program.
#
#  IDW  -  12/03/87


B         =  bcpl
O         =  68000-obj

SYSLINK   =  o:syslink

HDR       =  syslinkhdr
OLD       =  $[B]old-$(HDR)
NEW       =  $[B]new-$(HDR)

CWD       =  ""
BWD       =  bd:syslink


.PREFIXES~ $[B] $[O]


all~ $(SYSLINK)


sorthdr~
        sorthdr $(HDR) to $(NEW) ug ug
        copy    $(HDR) to $(OLD)
        copy    $(NEW) to $(HDR)


backup~
        copydir $(CWD) to $(BWD) update


print~
        list !here:+ opt dense,pm,u,nn,pn
          [$(HDR)]
          [$[B]syslink]
          [$[B]syntax]
          [$[B]execute]
        +


$(SYSLINK)~
        joinlib !here:+ as $(SYSLINK)
          [$[O]syslink]
          [$[O]syntax]
          [$[O]execute]
        +


$[B]$[O]~ $(NEW)
        bcp $< to $@ opt ws25000


