      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-output-file "group2/EXIT_PERFORM__until__CYCLE.out" }
        identification division.
        program-id. "foo".
        data division.
        working-storage section.
        77 a pic 99 value zero.
        procedure division.
            perform until a >= 10
            add 1 to a
            display a space with no advancing
            if a < 4
                display "cycling"
                exit perform cycle
                end-if
            if a equals 7
                display "exiting"
                exit perform
                end-if
            display "normal"
            end-perform.
            goback.

