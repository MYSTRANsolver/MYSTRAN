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

   MODULE BSMIN3_Interface

   INTERFACE

      SUBROUTINE BSMIN3 ( XI, A, B, AREA, MESSAG, WRT_BUG_THIS_TIME, BS )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUG, F04, WRT_BUG, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_BMAT_BIT, ELDT_BUG_BCHK_BIT, MIN4T_QUAD4_TRIA_NO
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BSMIN3_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, TWO
      USE MODEL_STUF, ONLY            :  EID, TYPE, XTB, XTL
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG

      IMPLICIT NONE
  
      CHARACTER(LEN=*), INTENT(IN)    :: MESSAG            ! Message to print out if BCHECK is run
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRT_BUG_THIS_TIME ! If 'Y' then write to BUG file if WRT_BUG array says to

      INTEGER(LONG), PARAMETER        :: NR        = 2     ! An input to subr BCHECK, called herein
      INTEGER(LONG), PARAMETER        :: NC        = 9     ! An input to subr BCHECK, called herein
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BSMIN3_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: A(3)              ! Vector of x coord differences
      REAL(DOUBLE) , INTENT(IN)       :: AREA              ! Elem area
      REAL(DOUBLE) , INTENT(IN)       :: B(3)              ! Vector of y coord differences
      REAL(DOUBLE) , INTENT(IN)       :: XI(3)             ! Vector of area coordinates
      REAL(DOUBLE) , INTENT(OUT)      :: BS(2,9)           ! Output strain-displ matrix for this elem
      END SUBROUTINE BSMIN3

   END INTERFACE

   END MODULE BSMIN3_Interface

