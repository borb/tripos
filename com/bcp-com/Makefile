O       = 68000-obj
ODEST   = :l
S       = bcp
V       = k
ERRS    = $S.err $S.fat $S.sml $S.fep $S.nsw $S.sye $S.tre
ERRO    = $O.err $O.fat $O.sml $O.fep $O.nsw $O.sye $O.tre
SYNS    = $S.syn $S.syn1 $S.syn2 $S.syn3 $S.syn4 $S.syn5 $S.syn6 $S.syn7 $S.get $S.rch
SYNO    = $O.syn $O.syn1 $O.syn2 $O.syn3 $O.syn4 $O.syn5 $O.syn6 $O.syn7 $O.get $O.rch
TRNS    = $S.trn $S.trn1 $S.trn2 $S.trn3 $S.trn4 $S.trn5 $S.trn6 $S.trn7
TRNO    = $O.trn $O.trn1 $O.trn2 $O.trn3 $O.trn4 $O.trn5 $O.trn6 $O.trn7
MCGS    = $S.mcg $S.mcg1 $S.mcg2 $S.mcg3 $S.mcg4 $S.mcg5 $S.mcg6 $S.mcg7 $S.mcg8
MCGO    = $O.mcg $O.mcg1 $O.mcg2 $O.mcg3 $O.mcg4 $O.mcg5 $O.mcg6 $O.mcg7 $O.mcg8
MSTS    = $S.bcp $S.nbcp $S.mst
MSTO    = $O.bcp $O.nbcp $O.mst $O.nmst
OVLS    = $S.ovl
OVLO    = $O.ovl-$V $O.ovl-spare-$V $O.novl-$V $O.novl-spare-$V
BCPO    = $O.bcp-$V $O.bcp-spare-$V $O.nbcp-$V $O.nbcp-spare-$V

UTILS   = $S.bcp-zap-pointtable $S.com
UTILO   = $O.bcp-zap-pointtable $O.com

.PREFIXES~      $O. $S.

BCPLFLAGS = SN
BCPL    = bcp

$(ERRS)~
$(SYNS)~
$(TRNS)~
$(MCGS)~
$(MSTS)~
$(OVLS)~
$(UTILS)~

all~    $(ODEST).bcp-$V-ov $(BCPO) $(UTILO)

$S.$O.~
        $(BCPL) FROM $< TO $@ OPT :$(BCPLFLAGS)

$O.nmst~ $S.mst
        $(BCPL) FROM $S.mst TO $@ OPT :$(BCPLFLAGS):cc:NOVL:

$O.ovl-$V~ $S.ovl
        $(BCPL) FROM $S.ovl TO $@ OPT :$(BCPLFLAGS):cc:SPARE:cc:$V:
$O.ovl-spare-$V~ $S.ovl
        $(BCPL) FROM $S.ovl TO $@ OPT :$(BCPLFLAGS):cc:SPARE:cc:$V:
$O.novl-$V~ $S.novl
        $(BCPL) FROM $S.novl TO $@ OPT :$(BCPLFLAGS):cc:NOVL:cc:$V:
$O.novl-spare-$V~ $S.novl
        $(BCPL) FROM $S.novl TO $@ OPT :$(BCPLFLAGS):cc:SPARE:cc:NOVL:cc:$V:

$O.bcp-$V~      $O.bcp $O.ovl-$V $O.mst
        link $O bcp ovl-$V TEMP $@ ver
$O.bcp-spare-$V~        $O.bcp $O.ovl-spare-$V $O.mst
        link $O bcp ovl-spare-$V TEMP $@ ver
$O.nbcp-$V~     $O.nbcp $O.novl-$V $O.nmst
        link $O nbcp novl-$V TEMP $@ ver
$O.nbcp-spare-$V~       $O.nbcp $O.novl-spare-$V $O.nmst
        link $O nbcp novl-spare-$V TEMP $@ ver

$(ODEST).bcp-$V-err~    $(ERRO)
        link $O ERR TEMP $@ ver

$(ODEST).bcp-$V-syn~    $(SYNO)
        link $O SYN TEMP $@ ver

$(ODEST).bcp-$V-trn~    $(TRNO)
        link $O trn TEMP $@ ver

$(ODEST).bcp-$V-cg~     $(MCGO)
        link $O MCG TEMP $@ ver

$(ODEST).bcp-$V-ov~     $(ODEST).bcp-$V-err $(ODEST).bcp-$V-syn $(ODEST).bcp-$V-trn $(ODEST).bcp-$V-cg
        pb:com $(ODEST).bcp-$V-err $(ODEST).bcp-$V-syn $(ODEST).bcp-$V-trn $(ODEST).bcp-$V-cg to $(ODEST).bcp-$V-ov


