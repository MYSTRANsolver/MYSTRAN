! ###############################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2019 Dr William R Case, Jr (dbcase29@gmail,com)                                              
                                                                                                         
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

   MODULE ELMDAT1_Interface

   INTERFACE

      SUBROUTINE ELMDAT1 ( INT_ELEM_ID, WRITE_WARN )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  FATAL_ERR, MEDAT0_CUSERIN, MELGP, MEMATC, MEMATR, MEPROP, METYPE, MOFFSET, MRMATLC,       &
                                         MRPBAR, MRPBEAM, MRPBUSH, MRPELAS, MRPROD, MRPSHEAR, MRPUSER1, MPSOLID, BLNK_SUB_NAM,     &
                                         NCORD, NGRID
      USE SCONTR, ONLY                :  DEDAT_Q4_MATANG_KEY, DEDAT_Q4_THICK_KEY, DEDAT_Q4_POFFS_KEY,                              &
                                         DEDAT_T3_MATANG_KEY, DEDAT_T3_THICK_KEY, DEDAT_T3_POFFS_KEY 
      USE PARAMS, ONLY                :  EPSIL, TSTM_DEF
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELMDAT_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONEPM4, TWO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE MODEL_STUF, ONLY            :  AGRID, BAROFF, BUSH_CID, BUSH_OCID, BUSHOFF, BGRID, CAN_ELEM_TYPE_OFFSET, CORD, DOFPIN,   &
                                         EDAT, EID, ELAS_COMP, ELDOF, ELEM_LEN_12, ELGP, ELMTYP, EMAT, EOFF, NUM_EMG_FATAL_ERRS,   &
                                         EPROP, EPNT, ETYPE, GRID, RGRID, GRID_ID, INTL_MID, INTL_PID, ISOLID, MATANGLE, MATL,     &
                                         MTRL_TYPE, NUM_SEi, OFFDIS, OFFSET, PBAR, PBEAM, PCOMP, PCOMP_PROPS, PLATEOFF, PLATETHICK,&
                                         PROD, PSHEAR, PSHEL, PSOLID, PUSER1, PUSERIN, RMATL, RPBAR, RPBEAM, RPBUSH,               &
                                         RPELAS, RPROD, RPSHEAR, RPSHEL, RPUSER1, TYPE, VVEC, XEB, ZOFFS

      USE MODEL_STUF, ONLY            :  USERIN_ACT_GRIDS, USERIN_ACT_COMPS, USERIN_CID0, USERIN_IN4_INDEX,                        &
                                         USERIN_MAT_NAMES, USERIN_NUM_BDY_DOF, USERIN_NUM_ACT_GRDS, USERIN_NUM_SPOINTS,            &
                                         USERIN_MASS_MAT_NAME, USERIN_LOAD_MAT_NAME, USERIN_RBM0_MAT_NAME, USERIN_STIF_MAT_NAME

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN        ! If 'Y' write warning messages, otherwise do not

      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID       ! Internal element ID

      INTEGER(LONG)                   :: USERIN_INDEX_COMP ! Index in EDAT where displ comp list begins
      INTEGER(LONG)                   :: USERIN_INDEX_GRID ! Index in EDAT where grid list begins
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELMDAT_BEGEND

      END SUBROUTINE ELMDAT1

   END INTERFACE

   END MODULE ELMDAT1_Interface

