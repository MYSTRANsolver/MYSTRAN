! ###############################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2019 Dr William R Case, Jr (dbcase29@gmail.com)                                              
                                                                                                         
! Permission is hereby granted, free of charge, to any person obtaining a copy of this software and      
! associated documentation files (the "Software"), to deal in the Software without restriction, including
! without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to   
! the following conditions:                                                                              
                                                                                                         
! The above copyright notice and this permission notice shall be included in all copies or substantial   
! portions of the Software and documentation.                                                                              
                                                                                                         
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS                                
! OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,                            
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE                            
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER                                 
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,                          
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN                              
! THE SOFTWARE.                                                                                          
! _______________________________________________________________________________________________________
                                                                                                        
! End MIT license text.                                                                                      

   MODULE ALLOCATE_MODEL_STUF_Interface

   INTERFACE

      SUBROUTINE ALLOCATE_MODEL_STUF ( NAME_IN, CALLING_SUBR )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, TWO, THREE, SIX, ONEPP6
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, TOT_MB_MEM_ALLOC 
      USE SCONTR, ONLY                :  LBAROFF, LBUSHOFF, LCMASS, LCONM2, LCORD, LEDAT, LELE, LFORCE, LGRAV, LGRID,              &
                                         LIND_GRDS_MPCS, LLOADC, LLOADR, LMATANGLE, LMATL, LMPC, LMPCADDC, LMPCADDR, LPBAR, LPBEAM,&
                                         LPBUSH, LPCOMP, LPCOMP_PLIES, LPDAT, LPELAS, LPLATEOFF, LPLATETHICK, LPLOAD, LPMASS,      &
                                         LPROD, LPSHEAR, LPSHEL, LPSOLID, LPUSER1, LPUSERIN, LRFORCE, LRIGEL, LSEQ, LSETLN, LSETS, &
                                         LSLOAD, LSPC, LSPC1, LSPCADDC, LSPCADDR, LSUB, LTDAT, LVVEC
      USE SCONTR, ONLY                :  MAX_ELEM_DEGREE, MAX_GAUSS_POINTS, MAX_STRESS_POINTS, MCMASS, MCONM2, MCORD, MDT, MELDOF, &
                                         MELGP, MGRID, MMATL, MOFFSET, MPBAR, MPBEAM, MPBUSH, MPCOMP_PLIES, MPCOMP0, MPELAS,       &
                                         MPMASS, MPRESS, MPLOAD4_3D_DATA, MPROD, MPSHEAR, MPSHEL, MPSOLID, MPUSER1, MPUSERIN,      &
                                         MRCONM2, MRCORD,                                                                          &
                                         MRGRID, MRMATLC, MRPBAR, MRPBEAM, MRPBUSH, MRPCOMP_PLIES, MRPCOMP0, MRPELAS, MRPMASS,     &
                                         MRPROD, MRPSHEAR, MRPSHEL, MRPUSER1, MUSERIN_MAT_NAMES
      USE SCONTR, ONLY                :  NDOFG, NGRID, NMPC, NPCOMP, NPLOAD4_3D, NRBAR, NRBE1, NRBE2, NSPC, NTSUB, NUM_MPCSIDS,    &
                                         NUM_SPCSIDS
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ALLOCATE_MODEL_STUF_BEGEND

      USE MODEL_STUF, ONLY            :  AGRID, BE1, BE2, BE3, BGRID, DOFPIN, DT, ME, OFFDIS, OFFDIS_B, OFFSET, KE, KED, KEM,      &
                                         PEB, PEG, PEL, PPE, PRESS, PTE, SE1, SE2, SE3, STE1, STE2, STE3, UEB, UEG, UEL, UGG,      &
                                         USERIN_NUM_ACT_GRDS, USERIN_ACT_COMPS, USERIN_ACT_GRIDS, USERIN_MAT_NAMES, XEB, XEL, XGL
      USE MODEL_STUF, ONLY            :  CMASS, CONM2, PMASS, RPMASS, RCONM2
      USE MODEL_STUF, ONLY            :  CORD, RCORD, TN
      USE MODEL_STUF, ONLY            :  SEQ1, SEQ2
      USE MODEL_STUF, ONLY            :  BAROFF, BUSHOFF, EDAT, EOFF, EPNT, ESORT1, ESORT2, ETYPE, PLATEOFF, PLATETHICK, VVEC
      USE MODEL_STUF, ONLY            :  PRESS_SIDS, PDATA, PPNT, PTYPE
      USE MODEL_STUF, ONLY            :  PLOAD4_3D_DATA
      USE MODEL_STUF, ONLY            :  FORMOM_SIDS
      USE MODEL_STUF, ONLY            :  GRAV_SIDS, RFORCE_SIDS, SLOAD_SIDS
      USE MODEL_STUF, ONLY            :  GRID, RGRID
      USE MODEL_STUF, ONLY            :  GRID_ID, GRID_SEQ, INV_GRID_SEQ, MPC_IND_GRIDS
      USE MODEL_STUF, ONLY            :  MATL, RMATL, PBAR, RPBAR, PBEAM, RPBEAM, PBUSH, RPBUSH, PCOMP, RPCOMP, PELAS, RPELAS,     &
                                         PROD, RPROD, PSHEAR, RPSHEAR, PSHEL, RPSHEL, PSOLID, PUSER1, RPUSER1, PUSERIN,            &
                                         USERIN_ACT_COMPS, USERIN_ACT_GRIDS, USERIN_MAT_NAMES 
      USE MODEL_STUF, ONLY            :  MPC_SIDS, MPCSIDS, MPCADD_SIDS
      USE MODEL_STUF, ONLY            :  SPC_SIDS, SPC1_SIDS, SPCSIDS, SPCADD_SIDS
      USE MODEL_STUF, ONLY            :  ALL_SETS_ARRAY, ONE_SET_ARRAY, SETS_IDS, SC_ACCE, SC_DISP, SC_ELFN, SC_ELFE, SC_GPFO,     &
                                         SC_MPCF, SC_OLOA, SC_SPCF, SC_STRE, SC_STRN, LOAD_SIDS, LOAD_FACS        
      USE MODEL_STUF, ONLY            :  ELDT, ELOUT, GROUT, OELOUT, OGROUT, LABEL, SCNUM, STITLE, SUBLOD, TITLE
      USE MODEL_STUF, ONLY            :  SYS_LOAD
      USE MODEL_STUF, ONLY            :  CETEMP, CETEMP_ERR, CGTEMP, CGTEMP_ERR, ETEMP, ETEMP_INIT, GTEMP, GTEMP_INIT, TDATA, TPNT
      USE MODEL_STUF, ONLY            :  RIGID_ELEM_IDS
      USE MODEL_STUF, ONLY            :  MATANGLE
      USE MODEL_STUF, ONLY            :  SPCSETS, MPCSETS
      USE MODEL_STUF, ONLY            :  GRID_ELEM_CONN_ARRAY

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: NAME_IN           ! Name of group of arrays to allocate
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Array name of the matrix to be allocated in sparse format
      CHARACTER(31*BYTE)              :: NAME              ! Specific array name used for output error message
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ALLOCATE_MODEL_STUF_BEGEND
 
      END SUBROUTINE ALLOCATE_MODEL_STUF

   END INTERFACE

   END MODULE ALLOCATE_MODEL_STUF_Interface

