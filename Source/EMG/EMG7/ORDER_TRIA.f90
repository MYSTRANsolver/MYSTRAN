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
  
      SUBROUTINE ORDER_TRIA ( KORDER, SS_I, SS_J, HH_IJ )
 
! Calculates abscissa and weight coefficients for triangular integration for the PENTA element
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_ORDER_TRIA
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ORDER_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, SIXTH, THIRD, HALF, TWO
  
      USE ORDER_TRIA_USE_IFs
      IMPLICIT NONE
   
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ORDER_TRIA'

      INTEGER(LONG), INTENT(IN)       :: KORDER                ! Triangular integration order to use
      INTEGER(LONG)                   :: I                     ! DO loop index
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
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      DO I=1,MAX_ORDER_TRIA
         SS_I(I)  = ZERO
         SS_J(I)  = ZERO
         HH_IJ(I) = ZERO
      ENDDO

      IF      (KORDER == 1) THEN

         SS_I(1) = THIRD    ;     SS_J(1) = THIRD    ;     HH_IJ(1) = HALF

      ELSE IF (KORDER == 3) THEN

         SS_I(1) = SIXTH    ;     SS_J(1) = TWO*THIRD;     HH_IJ(1) = SIXTH
         SS_I(2) = SIXTH    ;     SS_J(2) = SIXTH    ;     HH_IJ(2) = SIXTH
         SS_I(3) = TWO*THIRD;     SS_J(3) = SIXTH    ;     HH_IJ(3) = SIXTH

      ELSE IF (KORDER == 7) THEN

         SS_I(1) = THIRD;     SS_J(1) = THIRD;     HH_IJ(1) = W1
         SS_I(2) = A1   ;     SS_J(2) = B1   ;     HH_IJ(2) = W2
         SS_I(3) = B1   ;     SS_J(3) = A1   ;     HH_IJ(3) = W2
         SS_I(4) = B1   ;     SS_J(4) = B1   ;     HH_IJ(4) = W2
         SS_I(5) = A2   ;     SS_J(5) = B2   ;     HH_IJ(5) = W3
         SS_I(6) = B2   ;     SS_J(6) = A2   ;     HH_IJ(6) = W3
         SS_I(7) = B2   ;     SS_J(7) = B2   ;     HH_IJ(7) = W3
 
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
                    ,/,14X,' TRIANGULAR INTEGRATION ORDER MUST BE EITHER 1, 3 OR 7 BUT VALUE IS ',I8)

! **********************************************************************************************************************************
  
      END SUBROUTINE ORDER_TRIA
