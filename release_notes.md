## 17.0.0 (2025/08/24)


Added:
 - [Added better superLU version, better build script, and counters that don't slow mystran down](https://github.com/MYSTRANsolver/MYSTRAN/pull/131)
 -  [Implemented initial composite buckling for QUAD4 laminates](https://github.com/MYSTRANsolver/MYSTRAN/pull/139)
 -  [Improvments to shell element issues and fix for #125 (including initial work for shell buckling)](https://github.com/MYSTRANsolver/MYSTRAN/pull/127)
 -  [Added CQUAD8 element which uses MITC8](https://github.com/MYSTRANsolver/MYSTRAN/pull/144)
 -  [Added stress strain and force output for MITC8](https://github.com/MYSTRANsolver/MYSTRAN/pull/147)
 -  [Added MITC4+ element basic element matrices](https://github.com/MYSTRANsolver/MYSTRAN/pull/149)
 -  [Added many features for the MITC4+ element](https://github.com/MYSTRANsolver/MYSTRAN/pull/153)
 -  [Added -1/t^2 factor for MID4 output for PSHELLS/PCOMPS](https://github.com/MYSTRANsolver/MYSTRAN/pull/154)
 -  [Added mass matrix for MITC4+ element](https://github.com/MYSTRANsolver/MYSTRAN/pull/155)

Fixed:
 - [Made AUTOSPC NSET apply to 3 instead of 1, fixes problems for shells with small thicknesses and fixes issue #129](https://github.com/MYSTRANsolver/MYSTRAN/pull/132)
 - [Fixed coordinate system rotation bug which fixes issue #128](https://github.com/MYSTRANsolver/MYSTRAN/pull/133)
 - [Implemented temporary fix to K6ROT](https://github.com/MYSTRANsolver/MYSTRAN/pull/134)
 - [Modifies shear factor for solids and membrane composites; fixes issue #136](https://github.com/MYSTRANsolver/MYSTRAN/pull/134)
 - [Made BAILOUT perform more like NASTRAN](https://github.com/MYSTRANsolver/MYSTRAN/pull/140)



## 16.0.0 (2025/08/09)


This update contains several fixes, including breaking changes to the output.

## Changes
* Runtime performance improvement for large problems by @Copper280z in https://github.com/MYSTRANsolver/MYSTRAN/pull/87
* Bandit fix by @ceanwang in https://github.com/MYSTRANsolver/MYSTRAN/pull/91
* fix issues #95 #96 #98 by @victorkemp in https://github.com/MYSTRANsolver/MYSTRAN/pull/100
* Fix issue #99 shell orthotropic thermal strain by @victorkemp in https://github.com/MYSTRANsolver/MYSTRAN/pull/101
* fix #102 PCOMP THETA by @victorkemp in https://github.com/MYSTRANsolver/MYSTRAN/pull/105
* fix TRIA3K nonsymmetric composite error message by @victorkemp in https://github.com/MYSTRANsolver/MYSTRAN/pull/106
* PARAM QUADTYP MIN4 default by @victorkemp in https://github.com/MYSTRANsolver/MYSTRAN/pull/107
* fix PARAM STR_CID for solids by @victorkemp in https://github.com/MYSTRANsolver/MYSTRAN/pull/111
* Fix issue #109 CBUSH long format by @victorkemp in https://github.com/MYSTRANsolver/MYSTRAN/pull/110
* Solid corner stress by @victorkemp in https://github.com/MYSTRANsolver/MYSTRAN/pull/113
* Fixed MCID option for CQUAD4 and CTRIA3 by @victorkemp in https://github.com/MYSTRANsolver/MYSTRAN/pull/117
* Q4SURFIT default value by @victorkemp in https://github.com/MYSTRANsolver/MYSTRAN/pull/116
* Corrected CHAN's I2,J; and I1's A,I1,I2,J by @victorkemp in https://github.com/MYSTRANsolver/MYSTRAN/pull/119
* fixed and enabled solid element differential stiffness by @victorkemp in https://github.com/MYSTRANsolver/MYSTRAN/pull/123
* fix two memory bugs by @Bruno02468 in https://github.com/MYSTRANsolver/MYSTRAN/pull/124


**All commits since 15.2.1**: https://github.com/MYSTRANsolver/MYSTRAN/compare/15.2.1...16.0.0

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
