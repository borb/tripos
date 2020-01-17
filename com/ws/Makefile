#  Make file for William Stoye's "WS" editor.
#
#  IDW   -   27/03/87


B    =  bcpl
O    =  obj
BIN  =  o:ws


.PREFIXES~  $[B] $[O]


all~  $(BIN)


$(BIN)~
        joinlib !here:+ as $@
          [$[O]sysdep]
          [$[O]wspl]
          [$[O]meta]
          [$[O]escape]
          [$[O]help]
          [$[O]status]
          [$[O]obey]
          [$[O]main]
          [$[O]text]
          [$[O]pbuf]
          [$[O]wsoscall]
          [$[O]split]
          [$[O]find]
          [$[O]obey2]
          [$[O]block]
          [$[O]screen]
          [sys:l.note-point]
          [idw:ws-process.obj.ws-twids]
        +


$[B]$[O]~  wrshdr
        bcp $< to $@ opt ws25000/qt


