      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/Checking_IS_NUMERIC__2_.out" }
        identification       division.
        program-id.          prog.
        data                 division.
        working-storage      section.
        01 varN                pic s9999.
        01 varP redefines varN pic  9999.
        procedure            division.
        move 1234 to varN
        if varP is numeric 
            display "varP correctly tests numeric" 
        else 
            display "varP tests not numeric, which is wrong" 
            end-if
        move -1234 to varN.
        if varP is numeric 
            display "varP tests numeric, which is wrong" 
        else 
            display "varP correctly tests not numeric" 
            end-if
        goback.
        end program          prog.

