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

   MODULE QPLT1_Interface

   INTERFACE

      SUBROUTINE QPLT1 ( OPT, AREA, XSD, YSD )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MAX_ORDER_GAUSS, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  QPLT1_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, FOUR
      USE PARAMS, ONLY                :  IORQ2B
      USE MODEL_STUF, ONLY            :  ALPVEC, BE2, DT, EB, EID, KE, PRESS, PPE, PTE, SE2, STE2, SHELL_D, SHELL_DALP
 
      IMPLICIT NONE 
  
      CHARACTER(46*BYTE)              :: IORD_MSG          ! Character name of the integration order (used for debug output)
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices
      INTEGER(LONG)                   :: GAUSS_PT          ! Gauss point number (used for DEBUG output in subr SHP2DQ
      INTEGER(LONG), PARAMETER        :: ID(12) =  (/ 3, & ! ID(1) =  3 means virgin 12x12 elem DOF  1 is MYSTRAN 24X24 elem DOF  3
                                                      4, & ! ID(2) =  4 means virgin 12x12 elem DOF  2 is MYSTRAN 24X24 elem DOF  4
                                                      5, & ! ID(2) =  5 means virgin 12x12 elem DOF  3 is MYSTRAN 24X24 elem DOF  5
                                                      9, & ! ID(3) =  9 means virgin 12x12 elem DOF  4 is MYSTRAN 24X24 elem DOF  9
                                                     10, & ! ID(4) = 10 means virgin 12x12 elem DOF  5 is MYSTRAN 24X24 elem DOF 10
                                                     11, & ! ID(4) = 11 means virgin 12x12 elem DOF  6 is MYSTRAN 24X24 elem DOF 11
                                                     15, & ! ID(5) = 15 means virgin 12x12 elem DOF  7 is MYSTRAN 24X24 elem DOF 15
                                                     16, & ! ID(6) = 16 means virgin 12x12 elem DOF  8 is MYSTRAN 24X24 elem DOF 16
                                                     17, & ! ID(6) = 17 means virgin 12x12 elem DOF  9 is MYSTRAN 24X24 elem DOF 17
                                                     21, & ! ID(7) = 21 means virgin 12x12 elem DOF 10 is MYSTRAN 24X24 elem DOF 21
                                                     22, & ! ID(8) = 22 means virgin 12x12 elem DOF 11 is MYSTRAN 24X24 elem DOF 22
                                                     23 /) ! ID(8) = 23 means virgin 12x12 elem DOF 12 is MYSTRAN 24X24 elem DOF 23

      INTEGER(LONG), PARAMETER        :: IORD_STRESS_Q4 = 2! Gauss integration order for stress/strain recovery matrices
      INTEGER(LONG), PARAMETER        :: NUM_NODES = 8     ! DKQ element has 8 nodes (4 are internal)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = QPLT1_BEGEND
 
      REAL(DOUBLE) , INTENT(IN)       :: AREA              ! Element area
      REAL(DOUBLE) , INTENT(IN)       :: XSD(4)            ! Diffs in x coords of quad sides in local coords
      REAL(DOUBLE) , INTENT(IN)       :: YSD(4)            ! Diffs in y coords of quad sides in local coords
      REAL(DOUBLE)                    :: DUM1(3,12)        ! Intermediate matrix used in solving for PTE therm lds & KE stiffness
      REAL(DOUBLE)                    :: DUM2(12,12)       ! Intermediate matrix used in solving for KE element stiffness
      REAL(DOUBLE)                    :: DUM3(12)          ! Intermediate matrix used in solving for PTE thermal loads
      REAL(DOUBLE)                    :: DUM4(12)          ! Intermediate matrix used in solving for PTE thermal loads
      REAL(DOUBLE)                    :: EALP(3)           ! Intermed var used in calc STEi therm stress coeffs
 
      END SUBROUTINE QPLT1

   END INTERFACE

   END MODULE QPLT1_Interface

