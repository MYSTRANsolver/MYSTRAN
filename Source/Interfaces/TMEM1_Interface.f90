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

   MODULE TMEM1_Interface

   INTERFACE

      SUBROUTINE TMEM1 ( OPT, AREA, X2E, X3E, Y3E, WRT_BUG_THIS_TIME, BIG_BM )

  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUG, F04, WRT_BUG, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_BCHK_BIT, ELDT_BUG_BMAT_BIT, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  TMEM1_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, THREE
      USE MODEL_STUF, ONLY            :  ALPVEC, BE1, EID, DT, EM, ELDOF, KE, PCOMP_LAM, PCOMP_PROPS, PRESS, PPE, PTE, SE1, STE1,  &
                                         SHELL_AALP, SHELL_A, SHELL_PROP_ALP, TREF, TYPE, XEB, XEL
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      IMPLICIT NONE 
 
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRT_BUG_THIS_TIME ! If 'Y' then write to BUG file if WRT_BUG array says to

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = TMEM1_BEGEND
 
      REAL(DOUBLE) , INTENT(IN)       :: AREA              ! Element area
      REAL(DOUBLE) , INTENT(IN)       :: X2E               ! x coord of elem node 2
      REAL(DOUBLE) , INTENT(IN)       :: X3E               ! x coord of elem node 3
      REAL(DOUBLE) , INTENT(IN)       :: Y3E               ! y coord of elem node 3

      REAL(DOUBLE) , INTENT(OUT)      :: BIG_BM(3,ELDOF,1) ! Strain-displ matrix for this elem for all Gauss points (for all DOF's)

      REAL(DOUBLE)                    :: DUM1(ELDOF,1)     ! Intermediate matrix used in determining PTE thermal loads
      REAL(DOUBLE)                    :: EALP(3)           ! Intermed var used in calc STEi therm stress coeffs
      REAL(DOUBLE)                    :: C01               ! Intermediate variable used in calc PTE, SEi, STEi, KE
      REAL(DOUBLE)                    :: C02               ! Intermediate variable used in calc PTE, SEi, STEi, KE
      REAL(DOUBLE)                    :: C03               ! Intermediate variable used in calc PTE, SEi, STEi, KE
      REAL(DOUBLE)                    :: C04               ! Intermediate variable used in calc PTE, SEi, STEi, KE
      REAL(DOUBLE)                    :: CT0               ! Intermediate variable used in calc PTE thermal loads
 
      END SUBROUTINE TMEM1

   END INTERFACE

   END MODULE TMEM1_Interface

