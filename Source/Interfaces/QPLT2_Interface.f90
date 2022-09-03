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

   MODULE QPLT2_Interface

   INTERFACE

      SUBROUTINE QPLT2 ( OPT, AREA, XSD, YSD, BIG_BB )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MAX_ORDER_GAUSS, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  QPLT2_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, HALF, ONE, FOUR
      USE PARAMS, ONLY                :  EPSIL, IORQ2B, IORQ2T
      USE MODEL_STUF, ONLY            :  ALPVEC, BE2, BE3, BENSUM, DT, EID, ELDOF, FCONV_SHEAR_THICK, EB, ET,                      &
                                         ERR_SUB_NAM, FCONV, KE, INTL_MID, PCOMP_LAM, PCOMP_PROPS, PHI_SQ, PPE,                    &
                                         PRESS, PTE, SE2, SE3, SHELL_D, SHELL_DALP, SHELL_T, SHRSUM, STE2, TYPE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      IMPLICIT NONE 
  
      CHARACTER(46*BYTE)              :: IORD_MSG          ! Character name of the integration order (used for debug output)
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices

      INTEGER(LONG)                   :: GAUSS_PT          ! Gauss point number (used for DEBUG output in subr SHP2DQ
      INTEGER(LONG), PARAMETER        :: IDB( 8) = (/ 4, & ! IDB(1) =  4 means bending 8X8  elem DOF  1 is MYSTRAN 24X24 elem DOF  4
                                                      5, & ! IDB(2) =  5 means bending 8X8  elem DOF  2 is MYSTRAN 24X24 elem DOF  5
                                                     10, & ! IDB(3) = 10 means bending 8X8  elem DOF  3 is MYSTRAN 24X24 elem DOF 10
                                                     11, & ! IDB(4) = 11 means bending 8X8  elem DOF  4 is MYSTRAN 24X24 elem DOF 11
                                                     16, & ! IDB(5) = 16 means bending 8X8  elem DOF  5 is MYSTRAN 24X24 elem DOF 16
                                                     17, & ! IDB(6) = 17 means bending 8X8  elem DOF  6 is MYSTRAN 24X24 elem DOF 17
                                                     22, & ! IDB(7) = 22 means bending 8X8  elem DOF  7 is MYSTRAN 24X24 elem DOF 22
                                                     23 /) ! IDB(8) = 23 means bending 8x8  elem DOF  8 is MYSTRAN 24x24 elem DOF 23

      INTEGER(LONG), PARAMETER        :: IDS(12) = (/ 3, & ! IDS(1) =  3 means shear  12x12 elem DOF  1 is MYSTRAN 24X24 elem DOF  3
                                                      4, & ! IDS(2) =  4 means shear  12x12 elem DOF  2 is MYSTRAN 24X24 elem DOF  4
                                                      5, & ! IDS(3) =  5 means shear  12x12 elem DOF  3 is MYSTRAN 24X24 elem DOF  5
                                                      9, & ! IDS(4) =  9 means shear  12x12 elem DOF  4 is MYSTRAN 24X24 elem DOF  9
                                                     10, & ! IDS(4) = 10 means shear  12x12 elem DOF  5 is MYSTRAN 24X24 elem DOF 10
                                                     11, & ! IDS(4) = 11 means shear  12x12 elem DOF  6 is MYSTRAN 24X24 elem DOF 11
                                                     15, & ! IDS(5) = 15 means shear  12x12 elem DOF  7 is MYSTRAN 24X24 elem DOF 15
                                                     16, & ! IDS(6) = 16 means shear  12x12 elem DOF  8 is MYSTRAN 24X24 elem DOF 16
                                                     17, & ! IDS(6) = 17 means shear  12x12 elem DOF  9 is MYSTRAN 24X24 elem DOF 17
                                                     21, & ! IDS(7) = 21 means shear  12x12 elem DOF 10 is MYSTRAN 24X24 elem DOF 21
                                                     22, & ! IDS(8) = 22 means shear  12x12 elem DOF 11 is MYSTRAN 24X24 elem DOF 22
                                                     23 /) ! IDS(8) = 23 means shear  12x12 elem DOF 12 is MYSTRAN 24X24 elem DOF 23

      INTEGER(LONG), PARAMETER        :: IORD_STRESS_Q4 = 2! Gauss integration order for stress/strain recovery matrices
      INTEGER(LONG)                   :: IORDXX            ! Gaussian integration order to use when subr ORDER is called
      INTEGER(LONG), PARAMETER        :: NUM_NODES = 4     ! Quad has 4 nodes
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = QPLT2_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: AREA              ! Element area
      REAL(DOUBLE) , INTENT(IN)       :: XSD(4)            ! Diffs in x coords of quad sides in local coords
      REAL(DOUBLE) , INTENT(IN)       :: YSD(4)            ! Diffs in y coords of quad sides in local coords
      REAL(DOUBLE) , INTENT(OUT)      :: BIG_BB(3,ELDOF,IORQ2B*IORQ2B)

      REAL(DOUBLE)                    :: EALP(3)           ! Intermed var used in calc STEi therm stress coeffs
 
      END SUBROUTINE QPLT2

   END INTERFACE

   END MODULE QPLT2_Interface

