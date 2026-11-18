      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-output-file "group2/ROUNDED_to_PIC_999v9.out" }
        identification              division.
        program-id.                 prog.
        data                        division.
        working-storage             section.
        01 aaa              pic s999v99.
        01 bbb              pic s999v9.
        01 should_be        pic s999v9.
        01 answers.
        02 answer           pic s999v9 occurs 7 times.
        01 methods.
        02 meth             pic x(24) occurs 7 times.
        01 methi            pic 9.
        procedure                   division.
            move "away-from-zero"           to meth(1)
            move "nearest-away-from-zero"   to meth(2)
            move "nearest-even"             to meth(3)
            move "nearest-toward-zero"      to meth(4)
            move "toward-greater"           to meth(5)
            move "toward-lesser"            to meth(6)
            move "truncation"               to meth(7)
            move 0.11 to aaa
            move 0.2 to answer(1)  *> away-from-zero
            move 0.1 to answer(2)  *> nearest-away-from-zero
            move 0.1 to answer(3)  *> nearest-even
            move 0.1 to answer(4)  *> nearest-toward-zero
            move 0.2 to answer(5)  *> toward-greater
            move 0.1 to answer(6)  *> toward-lesser
            move 0.1 to answer(7)  *> truncation
            move 1 to methi perform reportt.
            move 2 to methi perform reportt.
            move 3 to methi perform reportt.
            move 4 to methi perform reportt.
            move 5 to methi perform reportt.
            move 6 to methi perform reportt.
            move 7 to methi perform reportt.

            move 0.15 to aaa
            move 0.2 to answer(1)  *> away-from-zero
            move 0.2 to answer(2)  *> nearest-away-from-zero
            move 0.2 to answer(3)  *> nearest-even
            move 0.1 to answer(4)  *> nearest-toward-zero
            move 0.2 to answer(5)  *> toward-greater
            move 0.1 to answer(6)  *> toward-lesser
            move 0.1 to answer(7)  *> truncation
            move 1 to methi perform reportt.
            move 2 to methi perform reportt.
            move 3 to methi perform reportt.
            move 4 to methi perform reportt.
            move 5 to methi perform reportt.
            move 6 to methi perform reportt.
            move 7 to methi perform reportt.

            move -0.11 to aaa
            move -0.2 to answer(1)  *> away-from-zero
            move -0.1 to answer(2)  *> nearest-away-from-zero
            move -0.1 to answer(3)  *> nearest-even
            move -0.1 to answer(4)  *> nearest-toward-zero
            move -0.1 to answer(5)  *> toward-greater
            move -0.2 to answer(6)  *> toward-lesser
            move -0.1 to answer(7)  *> truncation
            move 1 to methi perform reportt.
            move 2 to methi perform reportt.
            move 3 to methi perform reportt.
            move 4 to methi perform reportt.
            move 5 to methi perform reportt.
            move 6 to methi perform reportt.
            move 7 to methi perform reportt.

            move -0.15 to aaa
            move -0.2 to answer(1)  *> away-from-zero
            move -0.2 to answer(2)  *> nearest-away-from-zero
            move -0.2 to answer(3)  *> nearest-even
            move -0.1 to answer(4)  *> nearest-toward-zero
            move -0.1 to answer(5)  *> toward-greater
            move -0.2 to answer(6)  *> toward-lesser
            move -0.1 to answer(7)  *> truncation
            move 1 to methi perform reportt.
            move 2 to methi perform reportt.
            move 3 to methi perform reportt.
            move 4 to methi perform reportt.
            move 5 to methi perform reportt.
            move 6 to methi perform reportt.
            move 7 to methi perform reportt.

            goback.
        reportt.
            evaluate true
                when methi = 1 add zero to aaa giving bbb rounded mode away-from-zero
                when methi = 2 add zero to aaa giving bbb rounded mode nearest-away-from-zero
                when methi = 3 add zero to aaa giving bbb rounded mode nearest-even
                when methi = 4 add zero to aaa giving bbb rounded mode nearest-toward-zero
                when methi = 5 add zero to aaa giving bbb rounded mode toward-greater
                when methi = 6 add zero to aaa giving bbb rounded mode toward-lesser
                when methi = 7 add zero to aaa giving bbb rounded mode truncation
                end-evaluate
            display "moving " aaa " should be " answer(methi) space function trim(meth(methi))
            if bbb is not equal to answer(methi)
                display "       ERROR! We got     " bbb " which is not correct"
                end-if
            continue.
        end program                 prog.

