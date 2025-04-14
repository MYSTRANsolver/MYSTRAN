## 15.3.0-dev (2024/x/x)


Added:
 - Additional Output Flags: https://github.com/MYSTRANsolver/MYSTRAN/issues/72

Fixed:
 - 


Highlighted Changes
-------------------
Reworked output file management to make it easier to output everything:
 - PARAM,PRTALL/FILES,YES/NO (default=NO)
 - PARAM,PRTOP2/OP2,YES/NO (default=NO)
 - PARAM,PRTANS/ANS,YES/NO (default=NO)
   - alias for DEBUG,200,1
 - PARAM,PRTNEU/NEU,YES/NO (default=NO)
   - previously done with PARAM,POST,-1
     - PARAM,POST,-1 is no longer used (and will be used by the OP2 for MSC/NX compatibility)

 - Case 1:
    - PARAM,PRTALL,YES
    - PARAM,PRTOP2,YES
    -> OP2 will be written with all results (as well as F06, ANS, NEU)
 - Case 2:
    - PARAM,PRTALL,NO
    - PARAM,PRTOP2,YES
    -> OP2 will be written with all op2 results
 - Case 3:
    - PARAM,PRTALL,NO
    - PARAM,PRTOP2,NO
    -> Case Control will dictate what the OP2 writes


## 15.2.1 (2024/x/x)

See the release notes.

Added:
- asdf

Fixed:
- [fixed segfault caused by closed ANS file](https://github.com/MYSTRANsolver/MYSTRAN/pull/64)
- [Cards may now have comments in them](https://github.com/MYSTRANsolver/MYSTRAN/pull/68)
- [continuation & stress note bug fix](https://github.com/MYSTRANsolver/MYSTRAN/pull/58)
- [added common READ_BDF_LINE subroutine](https://github.com/MYSTRANsolver/MYSTRAN/pull/71)
- [Fix broken UTF-8 in some output files](https://github.com/MYSTRANsolver/MYSTRAN/pull/79)

## 15.2.0 (2024/4/7)

This update implements a significant improvement to the RBE3 element 
(which would hardly be possible without the invaluable assistance we 
got from Victor from the MecWay project), a TUBE2 option for PBARL 
props, and adds grid point forces in OP2 output.

Added:
 - [Adding TUBE2 option to PBARL](https://github.com/MYSTRANsolver/MYSTRAN/pull/40)
 - [Adding GPFORCE op2 writing to Mystran](https://github.com/MYSTRANsolver/MYSTRAN/pull/55)

Fixed:
 - [The RBE3 Fix](https://github.com/MYSTRANsolver/MYSTRAN/pull/59)
