# This makefile is used to create .tgz .zip versions
# of the distribution directorie 
#     archive

PUB = /homes/mr/public_html

# Public HTML directory if not mountable on this machine
# and the shared drive is called E: (/dose on Linux).
# Remember to call ssh-add before calling make sshpub.
SSHPUB = sandy.cl.cam.ac.uk:public_html

help:
	@echo
	@echo "make all      Construct the files: archive.tgz and archive.zip"
	@echo "make dosd     Put them in my D drive"
	@echo "make dose     Put them in my E drive"
	@echo "make pub      Put them also in my home page"
	@echo "make sshpubd  Put them in /dosd and my home page using scp"
	@echo "make sshpube  Put them in /dose and my home page using scp"
	@echo

all:	
	rm -f *~ */*~
	echo >TGZDATE
	echo -n "Distributed from machine: " >>TGZDATE
	hostname >>TGZDATE
	date >>TGZDATE
	rm -f FILES
	cp doc/README .
	(cd ..; tar cvzf archive.tgz Archive)
	rm -f ../archive.zip
	(cd ..; zip -rv9 archive.zip Archive)
	cp TGZDATE FILES
	ls -l ../archive.tgz ../archive.zip>>FILES

pub:	dosd
	cp README FILES ../archive.tgz ../archive.zip $(PUB)/Archive
	cat FILES

sshpubd:	dosd
	scp README FILES ../archive.tgz ../archive.zip $(SSHPUB)/Archive
	cat FILES

sshpube:	dose
	scp README FILES ../archive.tgz ../archive.zip $(SSHPUB)/Archive
	cat FILES

dosd:	all
	cp ../archive.tgz ../archive.zip /dosd

dose:	all
	cp ../archive.tgz ../archive.zip /dose


