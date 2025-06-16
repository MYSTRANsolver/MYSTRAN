! ###############################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
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
                                         DEDAT_T3_MATANG_KEY, DEDAT_T3_THICK_KEY, DEDAT_T3_POFFS_KEY,                              &
                                                              DEDAT_Q8_THICK_KEY, DEDAT_Q8_POFFS_KEY
      USE PARAMS, ONLY                :  EPSIL, TSTM_DEF
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELMDAT_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONEPM4, ONE, TWO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE MODEL_STUF, ONLY            :  AGRID, BAROFF, BUSH_CID, BUSH_OCID, BUSH_VVEC, BUSH_VVEC_OR_CID, BUSHOFF, BGRID,          &
                                         CAN_ELEM_TYPE_OFFSET, CORD, DOFPIN, EDAT, EID, ELAS_COMP, ELDOF, ELEM_LEN_12, ELGP,       &
                                         ELMTYP, EMAT, EOFF, NUM_EMG_FATAL_ERRS, EPROP, EPNT, ETYPE, GRID, RGRID, GRID_ID,         &
                                         INTL_MID, INTL_PID, ISOLID, MATANGLE, MATL, MTRL_TYPE, NUM_SEi, OFFDIS, OFFDIS_O, OFFSET, &
                                         PBAR, PBEAM, PCOMP, PCOMP_PROPS, PLATEOFF, PLATETHICK, PROD, PSHEAR, PSHEL, PSOLID,       &
                                         PUSER1, PUSERIN, RMATL, RPBAR, RPBEAM, RPBUSH, RPELAS, RPROD, RPSHEAR, RPSHEL, RPUSER1,   &
                                         TYPE, VVEC, XEB, ZOFFS

      USE MODEL_STUF, ONLY            :  USERIN_ACT_GRIDS, USERIN_ACT_COMPS, USERIN_CID0, USERIN_IN4_INDEX,                        &
                                         USERIN_MAT_NAMES, USERIN_NUM_BDY_DOF, USERIN_NUM_ACT_GRDS, USERIN_NUM_SPOINTS,            &
                                         USERIN_MASS_MAT_NAME, USERIN_LOAD_MAT_NAME, USERIN_RBM0_MAT_NAME, USERIN_STIF_MAT_NAME

      USE ELMDAT1_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ELMDAT1'
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN         ! If 'Y' write warning messages, otherwise do not
      CHARACTER( 1*BYTE)              :: GET_VVEC           ! If 'Y' run code to get VVEC 
      CHARACTER( 1*BYTE)              :: VVEC_DEFINED       ! If 'Y' then a VVEC was found for elements that require one

      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID        ! Internal element ID
      INTEGER(LONG)                   :: ACID               ! Actual coord sys ID

      INTEGER(LONG)                   :: EPNTK              ! Value from array EPNT at the row for this internal elem ID. It is the
!                                                             row number in array EDAT where data begins for this element. 

      INTEGER(LONG)                   :: IPNTR              ! Pointer into an array
      INTEGER(LONG)                   :: VVEC_FLAG          ! Either actual grid ID for V vector or -IVVEC

      INTEGER(LONG)                   :: I,J                ! DO loop indices
      INTEGER(LONG)                   :: IERROR             ! Local error count
      INTEGER(LONG)                   :: ICID               ! Internal coord sys ID
      INTEGER(LONG)                   :: IGID_V             ! Internal grid ID for V vector
      INTEGER(LONG)                   :: USERIN_INDEX_COMP  ! Index in EDAT where displ comp list begins
      INTEGER(LONG)                   :: USERIN_INDEX_GRID  ! Index in EDAT where grid list begins
      INTEGER(LONG)                   :: IPIN(2)            ! Pinflag fields from EDAT for a BAR or BEAM elem 
      INTEGER(LONG)                   :: IREM               ! Indicator that a pinflag has digits different than 1,2,3,4,5 or 6
      INTEGER(LONG)                   :: IROW               ! Row no. in a real array where data is found for this element
      INTEGER(LONG)                   :: IVVEC              ! Row number in VVEC where V vector for this elem is found
      INTEGER(LONG)                   :: DELTA              ! Delta EDAT row count to get to plate thickness key for QUAD, TRIA
      INTEGER(LONG)                   :: NFLAG              ! Row number in array DOFPIN
      INTEGER(LONG)                   :: NUM_COMPS          ! No. displ components (1 for SPOINT, 6 for actual grid)
      INTEGER(LONG)                   :: NUMMAT             ! No. matl properties for an element type
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELMDAT_BEGEND

      REAL(DOUBLE)                    :: DXI                ! An offset distance in direction 1
      REAL(DOUBLE)                    :: DYI                ! An offset distance in direction 2
      REAL(DOUBLE)                    :: DZI                ! An offset distance in direction 3
      REAL(DOUBLE)                    :: EPS1               ! A small number to compare for real zero
      REAL(DOUBLE)                    :: K_BUSH(6),B_BUSH(6)! Values read from PBUSH for stiffness, damping
      REAL(DOUBLE)                    :: PHID, THETAD       ! Outputs from subr GEN_T0L
      REAL(DOUBLE)                    :: THICK_AVG          ! Average of all THICK(i)
      REAL(DOUBLE)                    :: T0G(3,3)           ! Matrix to transform V vector from global to basic coords 
      REAL(DOUBLE)                    :: VV(3)              ! V vector in basic coords for this elem
 
      INTRINSIC                       :: MOD, FLOOR

      END SUBROUTINE ELMDAT1

   END INTERFACE

   END MODULE ELMDAT1_Interface

