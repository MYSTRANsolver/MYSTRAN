## 16.0.0 (2025/08/09)


Added:
 - [Runtime performance improvement for large problems](https://github.com/MYSTRANsolver/MYSTRAN/pull/87)
 - [Implemented THETA term for the PCOMP card](https://github.com/MYSTRANsolver/MYSTRAN/pull/105)
 - [Implemented previously nonfunctioning STR_CID for solids](https://github.com/MYSTRANsolver/MYSTRAN/pull/111)
 - [Implemented computation and f06 output for corner stresses on solid elements](https://github.com/MYSTRANsolver/MYSTRAN/pull/113)
 - [Added/fixed existing implementation of MCID for CQUAD4](https://github.com/MYSTRANsolver/MYSTRAN/pull/117)
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
 - [Additional Output Flags](https://github.com/MYSTRANsolver/MYSTRAN/issues/72)
 - [fixed minor linking issue with Bandit for compilation simplicity](https://github.com/MYSTRANsolver/MYSTRAN/pull/91)
 - [Fixed issues #95, #96 and #98 which were all related to coordinate system transformations for materials and elements](https://github.com/MYSTRANsolver/MYSTRAN/pull/100)
 - [Fixed incorrect thermal strains on orthotropic materials](https://github.com/MYSTRANsolver/MYSTRAN/pull/101)
 - [Fixed TRIA3K composites solving when they should have errored](https://github.com/MYSTRANsolver/MYSTRAN/pull/106)
 - [Changed default shell element formulation from MIN4T to MIN4](https://github.com/MYSTRANsolver/MYSTRAN/pull/107)
 - [Fixed way that long format CBUSH cards are read](https://github.com/MYSTRANsolver/MYSTRAN/pull/110)
 - [Improved accuracy of MIN4T elements by changing default stress integration order to bi-linear rather than bi-quadratic](https://github.com/MYSTRANsolver/MYSTRAN/pull/116)
 - [Fixed issue #118 which addresses errors in I1 section and Channel section (more work still needs to be done)](https://github.com/MYSTRANsolver/MYSTRAN/pull/119)
 - [Fixed errors in solid element differential stiffness matrix](https://github.com/MYSTRANsolver/MYSTRAN/pull/123)
 - [Fixed memory errors which caused issue #122](https://github.com/MYSTRANsolver/MYSTRAN/pull/124)
 - [Made AUTOSPC NSET apply to 3 instead of 1, fixes problems for shells with small thicknesses and fixes issue #129](https://github.com/MYSTRANsolver/MYSTRAN/pull/132)
 - [Fixed coordinate system rotation bug which fixes issue #128](https://github.com/MYSTRANsolver/MYSTRAN/pull/133)
 - [Implemented temporary fix to K6ROT](https://github.com/MYSTRANsolver/MYSTRAN/pull/134)
 - [Modifies shear factor for solids and membrane composites; fixes issue #136](https://github.com/MYSTRANsolver/MYSTRAN/pull/134)
 - [Made BAILOUT perform more like NASTRAN](https://github.com/MYSTRANsolver/MYSTRAN/pull/140)

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
