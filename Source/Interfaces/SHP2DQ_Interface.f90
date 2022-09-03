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

   MODULE SHP2DQ_Interface

   INTERFACE

      SUBROUTINE SHP2DQ ( IGAUS, JGAUS, NUM_NODES, CALLING_SUBR, IORD_MSG, IORZZZ, SSI, SSJ, WRT_BUG_THIS_TIME, PSH, DPSHG )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUG, ERR, F04, F06, WRT_BUG, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_SHPJ_BIT, MEFE, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  SHP_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, FOUR
      USE MODEL_STUF, ONLY            :  EID, EMG_IFE, ERR_SUB_NAM, NUM_EMG_FATAL_ERRS, TYPE
 
      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Subr that called this subr (used for debug output)
      CHARACTER(LEN=*), INTENT(IN)    :: IORD_MSG          ! Character name of the integration order (used for debug output)
      CHARACTER(17*BYTE)              :: NAME(5)           ! Used for output annotation
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRT_BUG_THIS_TIME ! If 'Y' then write to BUG file if WRT_BUG array says to

      INTEGER(LONG), INTENT(IN)       :: IGAUS             ! I index of Gauss point (needed for some optional output)
      INTEGER(LONG), INTENT(IN)       :: JGAUS             ! J index of Gauss point (needed for some optional output)
      INTEGER(LONG), INTENT(IN)       :: IORZZZ            ! Integration order (used for debug output)
      INTEGER(LONG), INTENT(IN)       :: NUM_NODES         ! Number of element nodes
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SHP_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: SSI               ! Gauss point location component
      REAL(DOUBLE) , INTENT(IN)       :: SSJ               ! Gauss point location component
      REAL(DOUBLE) , INTENT(OUT)      :: PSH(NUM_NODES)    ! Shape functions for all grid points for this Gauss point
      REAL(DOUBLE) , INTENT(OUT)      :: DPSHG(2,NUM_NODES)! Derivatives of PSH with respect to xi and eta.
      REAL(DOUBLE)                    :: A1,A2,A3          ! Intermediate variables used in calculating outputs
      REAL(DOUBLE)                    :: B1,B2,B3          ! Intermediate variables used in calculating outputs
 
      END SUBROUTINE SHP2DQ

   END INTERFACE

   END MODULE SHP2DQ_Interface

