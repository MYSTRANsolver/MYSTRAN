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
 
      SUBROUTINE BAR_MARGIN ( ICOL, S1, S2, S3, S4, S5, MS1, MS2, MS3, MSP1, MSP2, MSP3 )
 
! Calculates margins of safety for BAR element
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BAR_MARGIN_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, ONEPM6, ONEPM15, ONEPP10
      USE PARAMS, ONLY                :  EPSIL 
      USE MODEL_STUF, ONLY            :  ULT_STRE

      USE BAR_MARGIN_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BAR_MARGIN'
      CHARACTER(LEN=*), INTENT(OUT)   :: MSP1              ! If '1',  print margins in F06 file. If '0', do not print.
      CHARACTER(LEN=*), INTENT(OUT)   :: MSP2              ! If '1',  print margins in F06 file. If '0', do not print.
      CHARACTER(LEN=*), INTENT(OUT)   :: MSP3              ! If '1',  print margins in F06 file. If '0', do not print.
 
      INTEGER(LONG), INTENT(IN)       :: ICOL              ! Column no. from ULT_STRE to get max allow. stresses
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BAR_MARGIN_BEGEND
 
      REAL(DOUBLE), INTENT(OUT)       :: MS1               ! Calculated margin of safety
      REAL(DOUBLE), INTENT(OUT)       :: MS2               ! Calculated margin of safety
      REAL(DOUBLE), INTENT(OUT)       :: MS3               ! Calculated margin of safety
      REAL(DOUBLE), INTENT(IN)        :: S1                ! An input stress for which margins are calculated
      REAL(DOUBLE), INTENT(IN)        :: S2                ! An input stress for which margins are calculated
      REAL(DOUBLE), INTENT(IN)        :: S3                ! An input stress for which margins are calculated
      REAL(DOUBLE), INTENT(IN)        :: S4                ! An input stress for which margins are calculated
      REAL(DOUBLE), INTENT(IN)        :: S5                ! An input stress for which margins are calculated
      REAL(DOUBLE)                    :: EPS5              ! Small value
      REAL(DOUBLE)                    :: EPS6              ! Small value
      REAL(DOUBLE)                    :: S1U               ! = S1 unless S1 is zero and then it is (+/-)EPS6
      REAL(DOUBLE)                    :: S2U               ! = S2 unless S2 is zero and then it is (+/-)EPS6
      REAL(DOUBLE)                    :: S3U               ! = S3 unless S3 is zero and then it is (+/-)EPS6
      REAL(DOUBLE)                    :: S4U               ! = S4 unless S4 is zero and then it is (+/-)EPS6
      REAL(DOUBLE)                    :: S1M               ! DABS(S1)
      REAL(DOUBLE)                    :: S2M               ! DABS(S2)
      REAL(DOUBLE)                    :: S3M               ! DABS(S3)
      REAL(DOUBLE)                    :: S4M               ! DABS(S4)
      REAL(DOUBLE)                    :: S5M               ! DABS(S5)
      REAL(DOUBLE)                    :: SCP               !-DABS(SC) 
      REAL(DOUBLE)                    :: ST,SC,SS          ! Material allowables in tension, compr., shear
      REAL(DOUBLE)                    :: STM,SCM,SSM       ! DABS(ST), etc
 
      INTRINSIC                       :: DABS, DMIN1
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS5 = EPSIL(5)
      EPS6 = EPSIL(6)

! Initialize outputs

      MSP1 = ' '
      MSP2 = ' '
      MSP3 = ' '
      MS1  = ZERO
      MS2  = ZERO
      MS3  = ZERO

! Calc outputs

      ST   =  ULT_STRE(1,ICOL)
      SC   =  ULT_STRE(2,ICOL)
      SS   =  ULT_STRE(3,ICOL)
      STM  =  DABS(ST)
      SCM  =  DABS(SC)
      SSM  =  DABS(SS)
      SCP  = -SCM

! Calculate margins for BAR1 element. S1 is max stress at end A, S2 is max stress at end B. S3 is min stress at
! end A, S4 is min stress at end B.

      S1M = DABS(S1)
      S2M = DABS(S2)
      S3M = DABS(S3)
      S4M = DABS(S4)

      S1U = S1
      IF (S1M <= EPS5) THEN
         IF (S1 >= ZERO) THEN
            S1U =  EPS6
         ELSE
            S1U = -EPS6
         ENDIF
      ENDIF

      S2U = S2
      IF (S2M <= EPS5) THEN
         IF (S2 >= ZERO) THEN
            S2U =  EPS6
         ELSE
            S2U = -EPS6
         ENDIF
      ENDIF

      S3U = S3
      IF (S3M <= EPS5) THEN
         IF (S3 >= ZERO) THEN
            S3U =  EPS6
         ELSE
            S3U = -EPS6
         ENDIF
      ENDIF

      S4U = S4
      IF (S4M <= EPS5) THEN
         IF (S4 >= ZERO) THEN
            S4U =  EPS6
         ELSE
            S4U = -EPS6
         ENDIF
      ENDIF

      IF ((S1M < EPS5) .AND. (S2M < EPS5)) THEN            ! Margin for tension
         MS1  = ONEPP10   !(i.e. 1.E+10)
         MSP1 = '0'
      ELSE IF ((S1 > ZERO) .AND. (S2 <= ZERO)) THEN
         MS1  = STM/S1U - ONE
         MSP1 = '1'
      ELSE IF ((S1 <= ZERO) .AND. (S2 > ZERO)) THEN
         MS1  = STM/S2U - ONE
         MSP1 = '1'
      ELSE
         MS1  = DMIN1(STM/S1U,STM/S2U) - ONE
         IF ((S1 > ZERO) .AND. (S2 > ZERO)) THEN
            MSP1 = '1'
         ELSE
            MSP1 = '0'
         ENDIF
      ENDIF
 
      IF (STM < EPS5) THEN
         MSP1 = '0'
      ENDIF

      IF ((S3M < EPS5) .AND. (S4M < EPS5)) THEN            ! Margin for compression
         MS2  = ONEPP10   !(i.e. 1.E+10)
         MSP2 = '0'
      ELSE IF ((S3 >= ZERO) .AND. (S4 < ZERO)) THEN
         MS2  = SCP/S4U - ONE
         MSP2 = '1'
      ELSE IF ((S3 < ZERO) .AND. (S4 >= ZERO)) THEN
         MS2 = SCP/S3U - ONE
         MSP2 = '1'
      ELSE
         MS2 = DMIN1(SCP/S3U,SCP/S4U) - ONE
         IF ((S3 < ZERO) .AND. (S4 < ZERO)) THEN
            MSP2 = '1'
         ELSE
            MSP2 = '0'
         ENDIF
      ENDIF

      IF (SCM < EPS5) THEN
         MSP2 = '0'
      ENDIF

      S5M = DABS(S5)                                       ! ROD1 torsional margin (positive or negative torsional stress)

      IF (S5M < EPS5) THEN
         MS3  = ONEPP10   !(i.e. 1.E+10)
         MSP3 = '0'
      ELSE
         MS3  = (SSM/S5M) - ONE
         MSP3 = '1'
      ENDIF
      IF (SSM < EPS5) THEN
         MSP3 = '0'
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

      END SUBROUTINE BAR_MARGIN
