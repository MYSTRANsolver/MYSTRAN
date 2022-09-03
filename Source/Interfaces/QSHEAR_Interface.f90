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

   MODULE QSHEAR_Interface

   INTERFACE

      SUBROUTINE QSHEAR ( OPT, IORD, RED_INT_SHEAR, XSD, YSD )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_ORDER_GAUSS, MAX_STRESS_POINTS, MEFE, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  QSHEAR_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, FOUR
      USE MODEL_STUF, ONLY            :  ALPVEC, BE1, BMEANT, DT, EID, ELDOF, ELGP, EM, ERR_SUB_NAM, HBAR, KE, MXWARP,             &
                                         NUM_EMG_FATAL_ERRS, PCOMP_LAM, PCOMP_PROPS, PPE, PRESS, PTE,                              &
                                         SE1, STE1, SHELL_AALP, SHELL_A, TREF, TYPE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      IMPLICIT NONE 
  
      CHARACTER(1*BYTE) , INTENT(IN)  :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices
      CHARACTER( 1*BYTE), INTENT(IN)  :: RED_INT_SHEAR     ! If 'Y', use Gaussian weighted average of B matrices for shear terms
      CHARACTER(46*BYTE)              :: IORD_MSG          ! Character name of the integration order (used for debug output)

      INTEGER(LONG), INTENT(IN)       :: IORD              ! Gaussian integration order for element
      INTEGER(LONG)                   :: GAUSS_PT          ! Gauss point number (used for output in subr SHP2DQ
      INTEGER(LONG), PARAMETER        :: ID1( 8) = (/ 1, & ! ID1(1) =  1 means virgin  8X8  elem DOF  1 is MYSTRAN 24X24 elem DOF  1
                                                      2, & ! ID1(2) =  2 means virgin  8X8  elem DOF  2 is MYSTRAN 24X24 elem DOF  2
                                                      7, & ! ID1(3) =  7 means virgin  8X8  elem DOF  3 is MYSTRAN 24X24 elem DOF  7
                                                      8, & ! ID1(4) =  8 means virgin  8X8  elem DOF  4 is MYSTRAN 24X24 elem DOF  8
                                                     13, & ! ID1(5) = 13 means virgin  8X8  elem DOF  5 is MYSTRAN 24X24 elem DOF 13
                                                     14, & ! ID1(6) = 14 means virgin  8X8  elem DOF  6 is MYSTRAN 24X24 elem DOF 14
                                                     19, & ! ID1(7) = 19 means virgin  8X8  elem DOF  7 is MYSTRAN 24X24 elem DOF 19
                                                     20 /) ! ID1(8) = 20 means virgin  8x8  elem DOF  8 is MYSTRAN 24x24 elem DOF 20

      INTEGER(LONG), PARAMETER        :: ID2(12) = (/ 1, & ! ID2( 1)=  1 means expand 12x12 elem DOF  1 is MYSTRAN 24X24 elem DOF  1
                                                      2, & ! ID2( 2)=  2 means expand 12x12 elem DOF  2 is MYSTRAN 24X24 elem DOF  2
                                                      3, & ! ID2( 3)=  3 means expand 12x12 elem DOF  3 is MYSTRAN 24X24 elem DOF  3
                                                      7, & ! ID2( 4)=  7 means expand 12x12 elem DOF  4 is MYSTRAN 24X24 elem DOF  7
                                                      8, & ! ID2( 5)=  8 means expand 12x12 elem DOF  5 is MYSTRAN 24X24 elem DOF  8
                                                      9, & ! ID2( 6)=  9 means expand 12x12 elem DOF  6 is MYSTRAN 24X24 elem DOF  9
                                                     13, & ! ID2( 7)= 13 means expand 12x12 elem DOF  7 is MYSTRAN 24X24 elem DOF 13
                                                     14, & ! ID2( 8)= 14 means expand 12x12 elem DOF  8 is MYSTRAN 24X24 elem DOF 14
                                                     15, & ! ID2( 9)= 15 means expand 12x12 elem DOF  9 is MYSTRAN 24X24 elem DOF 15
                                                     19, & ! ID2(10)= 19 means expand 12x12 elem DOF 10 is MYSTRAN 24X24 elem DOF 19
                                                     20, & ! ID2(11)= 20 means expand 12x12 elem DOF 11 is MYSTRAN 24X24 elem DOF 20
                                                     21 /) ! ID2(12)= 21 means expand 12x12 elem DOF 12 is MYSTRAN 24X24 elem DOF 21
 
      INTEGER(LONG), PARAMETER        :: NUM_NODES = 4     ! Quad has 4 nodes
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = QSHEAR_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: XSD(4)            ! Diffs in x coords of quad sides in local coords
      REAL(DOUBLE) , INTENT(IN)       :: YSD(4)            ! Diffs in y coords of quad sides in local coords

      REAL(DOUBLE)                    :: DUM1(3,8)         ! Intermediate matrix used in solving for KE stiffness matrix       
      REAL(DOUBLE)                    :: DUM2(8,8)         ! Intermediate matrix used in solving for KE stiffness matrix     
      REAL(DOUBLE)                    :: DUM5(8,8)         ! Intermediate matrix used in solving for KE stiffness matrix
      REAL(DOUBLE)                    :: DUM6(12,8)        ! Intermediate matrix used in solving for KE stiffness matrix     
      REAL(DOUBLE)                    :: DUM7(12,12)       ! Intermediate matrix used in solving for KE stiffness matrix     
      REAL(DOUBLE)                    :: DUM9(3,8)         ! Intermediate matrix used in solving for SEi stress recovery matrices

      REAL(DOUBLE)                    :: SUMB              ! An intermediate variable used in calc B matrix for reduced integration
      REAL(DOUBLE)                    :: SUMD              ! An intermediate variable used in calc B matrix for reduced integration
 
      END SUBROUTINE QSHEAR

   END INTERFACE

   END MODULE QSHEAR_Interface

