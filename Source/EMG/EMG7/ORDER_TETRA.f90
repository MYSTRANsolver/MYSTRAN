! ##################################################################################################################################
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
  
      SUBROUTINE ORDER_TETRA ( KORDER, SSS_I, SSS_J, SSS_K, HHH_IJK )
 
! Calculates abscissa and weight coefficients for triangular integration for the TETRA element
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_ORDER_TETRA
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ORDER_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, SIXTH, QUARTER, HALF, ONE, TWO, TWELVE
  
      USE ORDER_TETRA_USE_IFs
      IMPLICIT NONE
   
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ORDER_TETRA'

      INTEGER(LONG), INTENT(IN)       :: KORDER                   ! Triangular integration order to use
      INTEGER(LONG)                   :: I                        ! DO loop index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ORDER_BEGEND
  
      REAL(DOUBLE) , INTENT(OUT)      :: SSS_I (MAX_ORDER_TETRA)  ! Gauss abscissa's
      REAL(DOUBLE) , INTENT(OUT)      :: SSS_J (MAX_ORDER_TETRA)  ! Gauss abscissa's
      REAL(DOUBLE) , INTENT(OUT)      :: SSS_K (MAX_ORDER_TETRA)  ! Gauss abscissa's
      REAL(DOUBLE) , INTENT(OUT)      :: HHH_IJK(MAX_ORDER_TETRA) ! Gauss weight coeffs
      REAL(DOUBLE) , PARAMETER        :: ALPHA = .58541020D0      ! Intermediate constant
      REAL(DOUBLE) , PARAMETER        :: BETA  = .13819660D0      ! Intermediate constant
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      DO I=1,MAX_ORDER_TETRA
         SSS_I(I)   = ZERO
         SSS_J(I)   = ZERO
         SSS_K(I)   = ZERO
         HHH_IJK(I) = ZERO
      ENDDO

      IF      (KORDER == 1) THEN

         SSS_I(1) = QUARTER  ;     SSS_J(1) = QUARTER  ;     SSS_K(1) = QUARTER  ;     HHH_IJK(1) = SIXTH

      ELSE IF (KORDER == 4) THEN

         SSS_I(1) = ALPHA    ;     SSS_J(1) = BETA     ;     SSS_K(1) = BETA     ;     HHH_IJK(1) = ONE/(TWO*TWELVE)
         SSS_I(2) = BETA     ;     SSS_J(2) = ALPHA    ;     SSS_K(2) = BETA     ;     HHH_IJK(2) = ONE/(TWO*TWELVE)
         SSS_I(3) = BETA     ;     SSS_J(3) = BETA     ;     SSS_K(3) = ALPHA    ;     HHH_IJK(3) = ONE/(TWO*TWELVE)
         SSS_I(4) = BETA     ;     SSS_J(4) = BETA     ;     SSS_K(4) = BETA     ;     HHH_IJK(4) = ONE/(TWO*TWELVE)

      ELSE

         WRITE(ERR,1931) SUBR_NAME, KORDER
         WRITE(F06,1931) SUBR_NAME, KORDER
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit

      ENDIF
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1931 FORMAT(' *ERROR  1931: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TETRAHEDRAL INTEGRATION ORDER MUST BE EITHER 1 OR 4 BUT VALUE IS ',I8)

! **********************************************************************************************************************************
  
      END SUBROUTINE ORDER_TETRA
