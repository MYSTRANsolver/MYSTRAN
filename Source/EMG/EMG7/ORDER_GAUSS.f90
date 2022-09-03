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
  
      SUBROUTINE ORDER_GAUSS ( KORDER, SSS, HHH )
 
! Calculates abscissa and weight coefficients for Gaussian integration of order KORDER = 1 to 10.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_ORDER_GAUSS, MEFE
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ORDER_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, TWO
      USE CONSTANTS_GAUSS, ONLY       :  HHV, SSV
      USE MODEL_STUF, ONLY            :  EMG_IFE, ERR_SUB_NAM, NUM_EMG_FATAL_ERRS
  
      USE ORDER_GAUSS_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ORDER_GAUSS'

      INTEGER(LONG), INTENT(IN)       :: KORDER               ! Gaussian integration order to use
      INTEGER(LONG)                   :: I                    ! DO loop index
      INTEGER(LONG)                   :: II                   ! A term in a computed index into SSS, HHH arrays
      INTEGER(LONG)                   :: JJ                   ! A computed index into SSS, HHH arrays
      INTEGER(LONG)                   :: KK                   ! A computed index into SSS, HHH arrays
      INTEGER(LONG)                   :: LL                   ! A computed index into SSS, HHH arrays
      INTEGER(LONG)                   :: MM                   ! A computed index into SSS, HHH arrays
      INTEGER(LONG)                   :: NN                   ! A term in a computed index into SSS, HHH arrays
      INTEGER(LONG)                   :: IBEGIN(11) = (/0, 1, 2, 4, 6, 9,12,16,20,25,30/)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ORDER_BEGEND
  
      REAL(DOUBLE) ,INTENT(OUT)       :: SSS(MAX_ORDER_GAUSS) ! Gauss abscissa's
      REAL(DOUBLE) ,INTENT(OUT)       :: HHH(MAX_ORDER_GAUSS) ! Gauss weight coeffs
  
      INTRINSIC MOD
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      DO I=1,MAX_ORDER_GAUSS
         SSS(I) = ZERO
         HHH(I) = ZERO
      ENDDO

! Check KORDER to make sure it no less than 1 nor greater than MAX_ORDER.  Error if not.

      IF ((KORDER >= 1) .AND. (KORDER <= MAX_ORDER_GAUSS)) THEN

! Abscissa and weight coefficients for Gaussian integ. of order KORDER
  
         IF (KORDER == 1) THEN
            SSS(1) = ZERO
            HHH(1) = TWO
         ELSE
            II = IBEGIN(KORDER)
            LL = IBEGIN(KORDER+1)
            JJ = LL - II
            NN = 2*JJ
            IF (MOD(KORDER,2) /= 0) THEN
               SSS(JJ) = SSV(LL-1)
               HHH(JJ) = HHV(LL-1)
               NN = NN - 1
               JJ = JJ - 1
            ENDIF
            DO KK=1,JJ
               LL = II + KK - 1
               MM = NN - KK + 1
               SSS(MM) =  SSV(LL)
               HHH(MM) =  HHV(LL)
               SSS(KK) = -SSV(LL)
               HHH(KK) =  HHV(LL)
             ENDDO
         ENDIF
 
      ELSE
  
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         IF (WRT_ERR > 0) THEN
            WRITE(ERR,1931) SUBR_NAME, MAX_ORDER_GAUSS, KORDER
            WRITE(F06,1931) SUBR_NAME, MAX_ORDER_GAUSS, KORDER
         ELSE
            IF (NUM_EMG_FATAL_ERRS <= MEFE) THEN
               ERR_SUB_NAM(NUM_EMG_FATAL_ERRS) = SUBR_NAME
               EMG_IFE(NUM_EMG_FATAL_ERRS,1)   = 1931
               EMG_IFE(NUM_EMG_FATAL_ERRS,2)   = KORDER
            ENDIF
         ENDIF
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
                    ,/,14X,' GAUSSIAN INTEGRATION ORDER CANNOT BE < 1 OR >',I3,' BUT VALUE IS ',I8)


! **********************************************************************************************************************************
  
      END SUBROUTINE ORDER_GAUSS
