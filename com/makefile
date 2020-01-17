# Makefile for com
B       = BCPL
b       = BCP
O       = :c
OBJ     = $O.balfour-printer $O.monitor $O.print $O.mount $O.ownname
SRC     = $B.balfour-printer $b.monitor $B.print $B.mount $B.ownname

BCPLFLAGS=OPT=FNWS30000

.PREFIXES ~ $b. $B. $O.

all~    $(OBJ)
$(SRC)~

$B.$O.~
        $(BCPL) $(BCPLFLAGS) $< $@

$b.$O.~
        $(BCPL) $(BCPLFLAGS) $< $@


