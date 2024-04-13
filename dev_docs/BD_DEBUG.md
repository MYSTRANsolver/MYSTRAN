# DEBUG
Define debug parameters

## Format:

|   1   | 2  |   3   |  4 | 5  | 6  | 7  | 8  | 9  | 10 | 
| ----- | -- | ----- | -- | -- | -- | -- | -- | -- | -- |
| DEBUG | i  | VALUE |    |    |    |    |    |    |    | 
| DEBUG | 31 | 1     |    |    |    |    |    |    |    | 

## Data Description:

| Field | Contents | Type | Default |
| ----- | -------- | ---- | ------- |
| i | Debug number (index in DEBUG array) | 0 < Integer < 100 | None |
| VALUE | The value for DEBUG(i) Integer | 0 |

## Remarks:

1. No other element in the model may have the same element ID

2. See table below for actions taken based on the various debug values. Unless otherwise stated,
DEBUG(i) = 0 is the default and, for the “print” parameters, no printing is done.

|  I  | DEBUG(I) | PARAM | Action (NOTE: default values are zero) |
| --- | -------- | ----- | -------------------------------------- |
| 1 | 1 | | Print KIND parameters defined in module PENTIUM_II_KIND to F06 file |
| 2 | 1 | | Print constants (parameters) defined in module CONSTANTS_1 |
| 3 | 1 | | Print machine parameters as determined by LAPACK function DLAMCH |
| 4 | 1 | | Do not use BMEAN when calculating membrane quad element stiffness for warped elements |
| 5 | 1 | | Print Gauss quadrature abscissas and weight s for plate elements |
| 6 | 1 | | Print some quad elem data to BUG file (over and above what is printed with Case Control ELDATA) |
|   | 2 | | Print some hexa elem data to BUG file (over and above what is printed with Case Control ELDATA) |
| 7 | 1 |  | Print arrays ESORT1, ESORT2, EPNT, ETYPE in subr ELESORT before/after sorting elems |
| 8 | 1 |  | Print grid temperature data in subr TEMPERATURE_DATA_PROC |
|   | 2 |  | Print elem temperature data in subr TEMPERATURE_DATA_PROC |
|   | 3 |  | Print both grid and elem temperature data in subr TEMPERATURE_DATA_PROC |
| 9 | > 0 | | Prints debug info on BAR pin flag processing |
| 10 | 11 or 33 | | Print data on algorithm to create STF stiffness arrays in subr ESP |
|    | 12 or 32 | | Print detailed data on algorithm to create STF arrays in subr SPARSE |
|    | 13 or 33 | | Print template of nonzero terms in KGG if PARAM SETLKTK = 1 or 2 |
|    | 21 or 33 | | Print data on algorithm to create EMS mass arrays in subr ESP |
|    | 22 or 32 | | Print detailed data on algorithm to create EMS mass arrays in subr SPARSE |
| 11 | 1 |  | Print individual 6x6 rigid body. displacement matrices in basic and global coordinates for each grid |
|    | 2 |  | Print NGRID by 6 rigid body displacement matrix in global coordinates for the model |
|    | 3 |  | Print both |
| 12 | 1 | | Use area shear factors in computing BAR stiffness matrix regardless of I12 value |
| 13 | 1 | | Print grid sequence tables in subr SEQ |
| 14 | 1 | | Print matrices generated in the rigid element generation subr's |
| 15 | 1 | | Print concentrated mass data in subr CONM2_PROC_1 |
| 16 | 1 | | Use static equivalent instead of work equivalent pressure loads for the QUAD4, TRIA3 |
| 17 | > 0 | | Print some info in subr KGG_SINGULARITY_PROC for grids that have AUTOSPC'd components |
|    | > 1 | | Do above for all grids (not just ones that have AUTOSPC's) |
| 18 | > 0 | | Print diagnostics in subr QMEM1 regarding checks on the BMEAN matrix satisfying R.B. motion |
| 19 | 1 | | Print debug output from subr STOKEN |
| 20 | 0 | | Use simple solution for GMN if RMM is diagonal. |
|    | 1 | | Bypass the simple solution for GMN if RMM is diagonal and use subr SOLVE_GMN instead |
| 21 | 0 | | Use MATMULT_SFF to multiply stiffness matrix times rigid body displs in STIFF_MAT_EQUIL_CHK |
|    | 1 | | Use LAPACK subroutine DSBMV |
| 22 | 1 | | Print RBMAT in subr STIFF_MAT_EQUIL_CHK |
| 23 | > 0 | | Do equilibrium checks on stiffness matrix even though model has SPOINT's |
| 24 | 1 or 3 | | Print KFSe matrix in subr REDUCE_KNN_TO_KFF |
|    | 2 or 3 | | Print KSSe matrix in subr REDUCE_KNN_TO_KFF |
| 25 | 1 or 3 | | Print PFYS matrix in subr REDUCE_N_FS |
|    | 2 or 3 | | Print QSYS matrix in subr REDUCE_N_FS |
| 26 | 1 | | Print YS matrix (S-set enforcorced displs) in LINK2 (LAPACK) |
| 32 | 1 | | Print PL load matrix in LINK3-LAPACK |
| 33 | 1 | | Print UL displacement matrix before refining sulotion in LINK3_LAPACK |
| 34 | 1 or 3 | |  Print ABAND matrix (KLL in band form) before equilibrating it in LINK3 (LAPACK |
| | 2 or 3 | |Print ABAND matrix after equilibrating it in LINK3 (LAPACK) |
| 35 | 1 | | Print ABAND’s decomp matrix (KLL triangular factor) in LINK3 (LAPACK) |
| 36 | 1 | | Print grid 6x6 mass for every grid in LINK2 |
| 40  | 1 or 3 | |  Print banded stiffness matrix ABAND in subr EIG_GIV_MGIV |
|     | 2 or 3 | | Print banded mass matrix ABAND in subr EIG_GIV_MGIV |
|     | 1      | | print RFAC = KLL - sigma*MLL in subr EIG_INV |
|     | 1      | | print RFAC = KLL - sigma*MLL in subr EIG_LANCZOS |
| 41  | 1 | | Print KLL stiffness matrix in LINK4 |
| 42  | 1 | | Print MLL stiffness matrix in LINK4 |
| 43  | 1 | | Print eigenvectors in LINK4 (normally not printed until LINK9) |
| 46  | 1 | | Print debug info for Inverse Power eigenvalue extraction |
| 47  | 1 | | Print eigenvalue estimates at each iteration in Lanczos |
| 48  | 1 | | Do not calculate off-diag terms in generalized mass matrix |
| 49  | 1 | | Print diagnostics in ARPACK subroutine DSBAND | 
| 55  | 1  | | Print PHIXG in full format in EXPAND_PHIXA_TO_PHIXG |
|     | 2 | |  Print PHIZG in full format in LINK5 |
|     | 3 | | Do both  |
| 100 | > 0 |  | Check allocation status of allocatable arrays. |
| | > 1 |  | Also write memory allocated to all arrays to F06 file. |
| 101 | > 0| | Write sparse I_MATOUT array in subroutine READ_MATRIX_1. |
|     | > 1 | | Call subroutine to check I_MATOUT array to make sure that terms are nondecreasing |
| 102 | > 0 | | Print debug info in subroutine MERGE_MAT_COLS_SSS |
| 103 | > 0 | | Do not use MRL (or MLR) in calc of modal participation factors and effective mass |
| 104 | > 0 | | Check if KRRcb is singular |
| 105 | > 0 | | write KLLs matrix to unformatted file |
| 106 | > 0 | | write info on all files in subr WRITE_ALLOC_MEM_TABLE (if 0 only write for those arrays that have memory allocated to them |
| 107 | > 0 | | Write allocated memory in F04 file with 6 decimal points (3 if DEBUG(107) = 0) |
| 108 | > 0 | | Write EDAT table |
| 109 | > 0 | | Write debug info in subr ELMDIS |
| 110 | > 0 | | Write debug info for BUSH elem in subrs ELMDAT1, ELMGM1 |
| 111 | > 0 | | Write some debug info on RSPLINE |
| 112 | > 0 | | Write THETAM (plate element material angle) and the location in subr EMG where it was calculated |
| 113 | > 0 | | Write PBARL entries in a special format that has 1 line per PBAR entry |
| 114 | > 0 | | Write debug info in subr OU4_PARTVEC_PROC |
| 115 | > 0 | | Write debug info in subr READ_INCLUDE_FILNAM |
| 116 | = 1 | | Write debug info in Yale subr SFAC |
| | = 2 | | Write debug info in Yale subr NFAC |
| | = 3 | | Do both  |
| 172 | > 0 |    | Calc PHI_SQ for the MIN4T based on area weighting of the TRIA3's. Otherwise, use simple average |
| 173 | = 1 |    |  Write some debug info in subr PARSE_CSV_STRING |
| 173 | = 2 |    |  Write some more detailed data |
| 174 | > 0 |    |  Print MPFACTOR, MEFFMASS values with 2 decimal places of accuracy rather than 6 |
| 175 | > 0 |    |  Write debug output from subroutine SURFACE_FIT regarding the polynomial fit to obtain element corner stresses from Gauss point stresses |
| 176 | > 0 |    |  Calculate stresses using element SEi, STEi matrices and displacements rather than from BEi matrices and strains |
| 177 | > 0 |    |  Print BAR, ROD margins of safety whether or not they would otherwise be |
| 178 | = 1 |    |  Print info on user key if PROTECTED = 'N' |
| 179 | = 1 |    |  Print blank space at beg of lines of output for CUSERIN entries in the F06 file |
| 180 | > 0 |    |  Write debug info to F06 for USERIN elements |
| 181 | = 1 |    |  Include USERIN RB mass in subr GPWG even though user did not input 3rd matrix (RBM0) on IN4FIL |
| 182 | = 1 |    |  Print debug data in subr MGGS_MASS_MATRIX for scalar mass matrix |
| 183 | = 1 |    |  Write some debug data for generating TDOF array |
| 184 | > 0 |    |  Write L1M data to F06 |
| 185 | > 0 |    |  Let eigen routines find and process all eigenval, vecs found even if NVEC > NDOFL - NUM_MLL_DIAG_ZEROS |
| 186 | > 0 |    |  Print debug info for pressure loads on faces of solid elements |
| 187 | > 0 |    |  Write list ao the number of various elastic elements in the DAT file to the F06 file |
| 188 | > 0 |    |  Do not abort in QPLT3 if KOO is reported to be singular |
| 189 |=1   |    |   Print messages in subroutine ESP for KE in local coords if element diagonal stiffness < 0 |
| 189 |=2   |    |  Print these messages in subroutine ESP after transformation to global |
| 189 |=3   |    |  Do both |
| 190 | > 0 |    |  Do not round off FAILURE_INDEX to 0 in subr POLY_FAILURE_INDEX | 
| 191 | = 0 |    |  Use temperatures at Gauss points for thermal loads in solid elements | 
| 192 | > 0 |    |  Print some summary info for max abs value of GP force balance for each solution vector | 
| 193 | = 1 |    |   call FILE_INQUIRE at end of LINK1 | 
| | = 2 |    |   call FILE_INQUIRE at end of LINK2  | 
| | = 3 |    |  call FILE_INQUIRE at end of LINK3 | 
| | = 4 |    |  call FILE_INQUIRE at end of LINK4 | 
| | = 5 |    |  call FILE_INQUIRE at end of LINK5 | 
| | = 6 |    |  call FILE_INQUIRE at end of LINK6 | 
| | = 9 |    |  call FILE_INQUIRE at end of LINK9 | 
| | = 100 |    |  call FILE_INQUIRE at end of MAIN | 
| | = 999 |    |  do all of the above | 
| 194 | 1 or 3 |    |  skip check on CW/CCW numbering of QUAD's | 
| 194 | 2 or 3 |    |  2 or 3 skip check on QUAD interior angles < 180 deg | 
| 194 | 3      |    |   skip both | 
| 195 | > 0 |    |  Print CB OTM matrices to F06 at end of LINK9 | 
| 196 | =0  |    |   Matrix output filter SMALL = EPSIL(1) | 
| 196 | > 0 |    |   Matrix output filter SMALL = TINY (param defined by user with default = 0.D0) | 
| 197 | > 0 |    |  Print debug info in subr EC_ENTRY_OUTPUT4 which reads Exec Control OUTPUT4 entries | 
| 198 | > 0 |    |  Write debug info in subroutine QPLT3 (for QUAD4 element) | 
| 199 | > 0 |    |  Check matrix times its inverse = identity matrix in several subroutines | 
| 200 | > 0 | $${\color{red}\text{PRTANS}}$$ |  Write problem answers (displs, etc) to filename.ANS as well as to filename.F06 (where filename is the name of the DAT data file submitted to MYSTRAN. This feature is generally only useful to the author when performing checkout of test problem answers | 
| 201 | > 0 |    |  Allow SOL = BUCKLING or DIFFEREN to run even if some elements are not coded for these soln's | 
| 202 | > 0 |    |  Calculate rigid body and constant strain sanity checks on strain-displacement matrices | 
| 203 | > 0 |    |  Print debug info in subroutine BAR1 (for the BAR element) | 
| 248 | > 0 |    |  Override fatal error and continue with orthotropic material properties for MIN4T QUAD4 | 
| 249 | > 0 |    |  In subroutine BREL1 call code for Timoshenko (BART) instead of Euler (BAR1) BAR element | 
