## Dev (2024/x/x)

lorem ipsum

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
