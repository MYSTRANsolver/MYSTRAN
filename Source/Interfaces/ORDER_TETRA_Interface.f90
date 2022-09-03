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

   MODULE ORDER_TETRA_Interface

   INTERFACE

      SUBROUTINE ORDER_TETRA ( KORDER, SSS_I, SSS_J, SSS_K, HHH_IJK )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_ORDER_TETRA
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ORDER_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, SIXTH, QUARTER, HALF, ONE, TWO, TWELVE
  
      IMPLICIT NONE
   
      INTEGER(LONG), INTENT(IN)       :: KORDER                   ! Triangular integration order to use
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ORDER_BEGEND
  
      REAL(DOUBLE) , INTENT(OUT)      :: SSS_I (MAX_ORDER_TETRA)  ! Gauss abscissa's
      REAL(DOUBLE) , INTENT(OUT)      :: SSS_J (MAX_ORDER_TETRA)  ! Gauss abscissa's
      REAL(DOUBLE) , INTENT(OUT)      :: SSS_K (MAX_ORDER_TETRA)  ! Gauss abscissa's
      REAL(DOUBLE) , INTENT(OUT)      :: HHH_IJK(MAX_ORDER_TETRA) ! Gauss weight coeffs
      REAL(DOUBLE) , PARAMETER        :: ALPHA = .58541020D0      ! Intermediate constant
      REAL(DOUBLE) , PARAMETER        :: BETA  = .13819660D0      ! Intermediate constant
  
      END SUBROUTINE ORDER_TETRA

   END INTERFACE

   END MODULE ORDER_TETRA_Interface

