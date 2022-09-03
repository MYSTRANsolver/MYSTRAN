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

   MODULE JAC2D_Interface

   INTERFACE

      SUBROUTINE JAC2D ( SSI, SSJ, XSD, YSD, WRT_BUG_THIS_TIME, JAC, JACI, DETJ )

  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  BUG, ERR, F04, F06, WRT_BUG, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_SHPJ_BIT, MEFE
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  JACOBIAN_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, FOUR
      USE PARAMS, ONLY                :  EPSIL
      USE MODEL_STUF, ONLY            :  EID, EMG_IFE, EMG_RFE, ERR_SUB_NAM, NUM_EMG_FATAL_ERRS, TYPE
  
      IMPLICIT NONE
 
      CHARACTER( 1*BYTE), INTENT(IN)  :: WRT_BUG_THIS_TIME ! If 'Y' then write to BUG file if WRT_BUG array says to

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = JACOBIAN_BEGEND
 
      REAL(DOUBLE) , INTENT(IN)       :: SSI               ! A Gauss point coord.
      REAL(DOUBLE) , INTENT(IN)       :: SSJ               ! A Gauss point coord.
      REAL(DOUBLE) , INTENT(IN)       :: XSD(4)            ! 1-D arrays of differences in x side dimensions (local)
      REAL(DOUBLE) , INTENT(IN)       :: YSD(4)            ! 1-D arrays of differences in y side dimensions (local)
      REAL(DOUBLE) , INTENT(OUT)      :: DETJ              ! Determinant of JAC
      REAL(DOUBLE) , INTENT(OUT)      :: JAC(2,2)          ! 2 x 2 Jacobian matrix
      REAL(DOUBLE) , INTENT(OUT)      :: JACI(2,2)         ! 2 x 2 inverse of JAC
  
      END SUBROUTINE JAC2D

   END INTERFACE

   END MODULE JAC2D_Interface

