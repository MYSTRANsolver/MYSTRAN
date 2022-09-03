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

   MODULE TPLT1_Interface

   INTERFACE

      SUBROUTINE TPLT1 ( OPT, AREA, X2E, X3E, Y3E )

  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, WRT_LOG, f06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  TPLT1_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, THREE, FOUR, SIX, TWELVE
      USE MODEL_STUF, ONLY            :  ALPVEC, BE2, DT, EB, KE, PRESS, PPE, PTE, SHELL_DALP, SHELL_D, SHELL_PROP_ALP, SE2, STE2
 
      IMPLICIT NONE 
  
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = TPLT1_BEGEND
 
      REAL(DOUBLE) , INTENT(IN)       :: AREA              ! Element area
      REAL(DOUBLE) , INTENT(IN)       :: X2E               ! x coord of elem node 2
      REAL(DOUBLE) , INTENT(IN)       :: X3E               ! x coord of elem node 3
      REAL(DOUBLE) , INTENT(IN)       :: Y3E               ! y coord of elem node 3
      REAL(DOUBLE)                    :: C11               ! Intermediate variable used in calc SEi stress recovery matrices
      REAL(DOUBLE)                    :: C12               ! Intermediate variable used in calc SEi stress recovery matrices
      REAL(DOUBLE)                    :: C21               ! Intermediate variable used in calc SEi stress recovery matrices
      REAL(DOUBLE)                    :: C22               ! Intermediate variable used in calc SEi stress recovery matrices
      REAL(DOUBLE)                    :: C33               ! Intermediate variable used in calc SEi stress recovery matrices
      REAL(DOUBLE)                    :: CT0               ! Intermediate variable used in calc PTE stress recovery matrices
      REAL(DOUBLE)                    :: D1(3,3)           ! Output from subr ATRA, called herein (used in calc KE)
      REAL(DOUBLE)                    :: D2(3,3)           ! Output from subr ATRA, called herein (used in calc KE)
      REAL(DOUBLE)                    :: D3(3,3)           ! Output from subr ATRA, called herein (used in calc KE)
      REAL(DOUBLE)                    :: D4(3,3)           ! Output from subr ATRA, called herein (used in calc KE)
      REAL(DOUBLE)                    :: D5(3,3)           ! Output from subr ATRA, called herein (used in calc KE)
      REAL(DOUBLE)                    :: E1                ! Intermediate variable used in calc KE elem stiffness
      REAL(DOUBLE)                    :: E2                ! Intermediate variable used in calc KE elem stiffness
      REAL(DOUBLE)                    :: E4                ! Intermediate variable used in calc KE elem stiffness
      REAL(DOUBLE)                    :: EALP(3)           ! Intermed var used in calc STEi therm stress coeffs
      REAL(DOUBLE)                    :: L12S              ! Intermediate variable used in calc Pi, Ti, Qi, Ri variables
      REAL(DOUBLE)                    :: L23S              ! Intermediate variable used in calc Pi, Ti, Qi, Ri variables
      REAL(DOUBLE)                    :: L31S              ! Intermediate variable used in calc Pi, Ti, Qi, Ri variables
      REAL(DOUBLE)                    :: P4                ! Intermediate variable used in calc array S
      REAL(DOUBLE)                    :: P5                ! Intermediate variable used in calc array S
      REAL(DOUBLE)                    :: P6                ! Intermediate variable used in calc array S
      REAL(DOUBLE)                    :: Q4                ! Intermediate variable used in calc array S
      REAL(DOUBLE)                    :: Q5                ! Intermediate variable used in calc array S
      REAL(DOUBLE)                    :: R4                ! Intermediate variable used in calc array S
      REAL(DOUBLE)                    :: R5                ! Intermediate variable used in calc array S
      REAL(DOUBLE)                    :: T4                ! Intermediate variable used in calc array S
      REAL(DOUBLE)                    :: T5                ! Intermediate variable used in calc array S
 
      END SUBROUTINE TPLT1

   END INTERFACE

   END MODULE TPLT1_Interface

