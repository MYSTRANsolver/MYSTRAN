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

   MODULE ORDER_TRIA_Interface

   INTERFACE

      SUBROUTINE ORDER_TRIA ( KORDER, SS_I, SS_J, HH_IJ )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_ORDER_TRIA
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ORDER_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, SIXTH, THIRD, HALF, TWO
  
      IMPLICIT NONE
   
      INTEGER(LONG), INTENT(IN)       :: KORDER                ! Triangular integration order to use
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ORDER_BEGEND
  
      REAL(DOUBLE) ,INTENT(OUT)       :: SS_I(MAX_ORDER_TRIA)  ! Triangular integration abscissa's
      REAL(DOUBLE) ,INTENT(OUT)       :: SS_J(MAX_ORDER_TRIA)  ! Triangular integration abscissa's
      REAL(DOUBLE) ,INTENT(OUT)       :: HH_IJ(MAX_ORDER_TRIA) ! Triangular integration weight coeffs
      REAL(DOUBLE) , PARAMETER        :: A1 = .0597158717D0    ! Intermediate constant
      REAL(DOUBLE) , PARAMETER        :: A2 = .7974269853D0    ! Intermediate constant
      REAL(DOUBLE) , PARAMETER        :: B1 = .4701420641D0    ! Intermediate constant
      REAL(DOUBLE) , PARAMETER        :: B2 = .1012865073D0    ! Intermediate constant
      REAL(DOUBLE) , PARAMETER        :: W1 = .1125D0          ! Intermediate constant
      REAL(DOUBLE) , PARAMETER        :: W2 = .0661970763D0    ! Intermediate constant
      REAL(DOUBLE) , PARAMETER        :: W3 = .0629695902D0    ! Intermediate constant
  
      END SUBROUTINE ORDER_TRIA

   END INTERFACE

   END MODULE ORDER_TRIA_Interface

