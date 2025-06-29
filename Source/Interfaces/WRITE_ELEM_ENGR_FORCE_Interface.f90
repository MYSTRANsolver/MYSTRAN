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

   MODULE WRITE_ELEM_ENGR_FORCE_Interface

   INTERFACE

      SUBROUTINE WRITE_ELEM_ENGR_FORCE ( JSUB, NUM, IHDR, NUM_PTS, ITABLE )

  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ANS, ERR, F04, F06, OP2
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, INT_SC_NUM, NDOFR, NUM_CB_DOFS, NVEC, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  PRTANS
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE LINK9_STUFF, ONLY           :  EID_OUT_ARRAY, OGEL
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_ELEM_ENGR_FORCE_BEGEND
      USE MODEL_STUF, ONLY            :  ELEM_ONAME, LABEL, SCNUM, STITLE, TITLE, TYPE
      USE CC_OUTPUT_DESCRIBERS, ONLY  :  FORC_OUT
      USE WRITE_ELEM_ENGR_FORCE_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_ELEM_ENGR_FORCE'
      CHARACTER(LEN=*), INTENT(IN)    :: IHDR              ! Indicator of whether to write an output header
      CHARACTER(128*BYTE)             :: FILL              ! Padding for output format
      CHARACTER(LEN=LEN(ELEM_ONAME))  :: ONAME             ! Element name to write out in F06 file

      INTEGER(LONG), INTENT(IN)       :: JSUB              ! Solution vector number
      INTEGER(LONG), INTENT(IN)       :: NUM               ! The number of rows of OGEL to write out
      INTEGER(LONG), INTENT(IN)       :: NUM_PTS           ! Num diff stress points for one element
      INTEGER(LONG), INTENT(INOUT)    :: ITABLE            ! the current op2 subtable, should be -3, -5, ...
      INTEGER(LONG)                   :: BDY_COMP          ! Component (1-6) for a boundary DOF in CB analyses
      INTEGER(LONG)                   :: BDY_GRID          ! Grid for a boundary DOF in CB analyses
      INTEGER(LONG)                   :: BDY_DOF_NUM       ! DOF number for BDY_GRID/BDY_COMP
      INTEGER(LONG)                   :: I,J,J1,K,L        ! DO loop indices or counters
      INTEGER(LONG)                   :: NUM_TERMS         ! Number of terms to write out for shell elems
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_ELEM_ENGR_FORCE_BEGEND
      LOGICAL                         :: WRITE_F06, WRITE_OP2, WRITE_ANS   ! flag

      REAL(DOUBLE)                    :: ABS_ANS(8)       ! Max ABS for all element output
      REAL(DOUBLE)                    :: MAX_ANS(8)       ! Max for all element output
      REAL(DOUBLE)                    :: MIN_ANS(8)       ! Min for all element output
      
      ! op2 info
      CHARACTER( 8*BYTE)              :: TABLE_NAME             ! the name of the op2 table

      ! table -3 info
      INTEGER(LONG)                   :: ISUBCASE_INDEX         ! the index into SCNUM
      INTEGER(LONG)                   :: ANALYSIS_CODE          ! static/modal/time/etc. flag
      INTEGER(LONG)                   :: ELEMENT_TYPE           ! the OP2 flag for the element
      CHARACTER(LEN=128)              :: TITLEI                 ! the model TITLE
      CHARACTER(LEN=128)              :: STITLEI                ! the subcase SUBTITLE
      CHARACTER(LEN=128)              :: LABELI                 ! the subcase LABEL
      INTEGER(LONG)                   :: FIELD5_INT_MODE
      REAL(DOUBLE)                    :: FIELD6_EIGENVALUE

!     op2 specific flags
      INTEGER(LONG)                   :: DEVICE_CODE  ! PLOT, PRINT, PUNCH flag
      INTEGER(LONG)                   :: NUM_WIDE     ! the number of "words" for an element
      INTEGER(LONG)                   :: NVALUES      ! the number of "words" for all the elments
      INTEGER(LONG)                   :: NTOTAL       ! the number of bytes for all NVALUES
      INTEGER(LONG)                   :: ISUBCASE     ! the subcase ID
      INTEGER(LONG)                   :: NELEMENTS
  
      END SUBROUTINE WRITE_ELEM_ENGR_FORCE

   END INTERFACE

   END MODULE WRITE_ELEM_ENGR_FORCE_Interface

