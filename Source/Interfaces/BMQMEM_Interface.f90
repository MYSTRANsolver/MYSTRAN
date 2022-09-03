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

   MODULE BMQMEM_Interface

   INTERFACE

      SUBROUTINE BMQMEM ( DPSHX, IGAUS, JGAUS, MESSAG, WRT_BUG_THIS_TIME, BM )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUG, F04, WRT_BUG, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_BMAT_BIT, ELDT_BUG_BCHK_BIT
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BMQMEM_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE MODEL_STUF, ONLY            :  BMEANT, EID, HBAR, MXWARP, TYPE, XEB, XEL
 
      IMPLICIT NONE
  
      CHARACTER(LEN=*), INTENT(IN)    :: MESSAG            ! Messag to print out if BCHECK is run
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRT_BUG_THIS_TIME ! If 'Y' then write to BUG file if WRT_BUG array says to

      INTEGER(LONG), INTENT(IN)       :: IGAUS             ! I index of Gauss point (needed for some optional output)
      INTEGER(LONG), INTENT(IN)       :: JGAUS             ! J index of Gauss point (needed for some optional output)
      INTEGER(LONG), PARAMETER        :: ID1( 8) = (/ 1, & ! ID1(1) =  1
                                                      2, & ! ID1(2) =  2
                                                      7, & ! ID1(3) =  7
                                                      8, & ! ID1(4) =  8
                                                     13, & ! ID1(5) = 13
                                                     14, & ! ID1(6) = 14
                                                     19, & ! ID1(7) = 19
                                                     20 /) ! ID1(8) = 20

      INTEGER(LONG), PARAMETER        :: ID2(12) = (/ 1, & ! ID2( 1)=  1
                                                      2, & ! ID2( 2)=  2
                                                      3, & ! ID2( 3)=  3
                                                      7, & ! ID2( 4)=  7
                                                      8, & ! ID2( 5)=  8
                                                      9, & ! ID2( 6)=  9
                                                     13, & ! ID2( 7)= 13
                                                     14, & ! ID2( 8)= 14
                                                     15, & ! ID2( 9)= 15
                                                     19, & ! ID2(10)= 19
                                                     20, & ! ID2(11)= 20
                                                     21 /) ! ID2(12)= 21
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BMQMEM_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: DPSHX(2,4)        ! Derivatives of the 4 node bilinear isopar interps wrt elem x and y
      REAL(DOUBLE) , INTENT(OUT)      :: BM(3,8)           ! Output strain-displ matrix for this elem
      END SUBROUTINE BMQMEM

   END INTERFACE

   END MODULE BMQMEM_Interface

