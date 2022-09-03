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
 
      SUBROUTINE ROD_MARGIN (ICOL, S1, S2, MS1, MS2, MSP1, MSP2 )
 
! Calculates margins of safety for ROD element
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ROD_MARGIN_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, ONEPM6, ONEPP10
      USE PARAMS, ONLY                :  EPSIL 
      USE MODEL_STUF, ONLY            :  ULT_STRE

      USE ROD_MARGIN_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ROD_MARGIN'
      CHARACTER(LEN=*), INTENT(OUT)   :: MSP1,MSP2         ! If '1',  print margins in F06 file. If '0', do not print. 
 
      INTEGER(LONG), INTENT(IN)       :: ICOL              ! Column no. from ULT_STRE to get max allow. stresses
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ROD_MARGIN_BEGEND
 
      REAL(DOUBLE), INTENT(OUT)       :: MS1               ! Calculated margin of safety
      REAL(DOUBLE), INTENT(OUT)       :: MS2               ! Calculated margin of safety
      REAL(DOUBLE), INTENT(IN)        :: S1                ! An input stress for which margins are calculated
      REAL(DOUBLE), INTENT(IN)        :: S2                ! An input stress for which margins are calculated
      REAL(DOUBLE)                    :: EPS5              ! Small value
      REAL(DOUBLE)                    :: S1M,S2M           ! DABS(S1), etc
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

! Initialize outputs

      MSP1 = ' '
      MSP2 = ' '
      MS1  = ZERO
      MS2  = ZERO

! Calc outputs

      ST   =  ULT_STRE(1,ICOL)
      SC   =  ULT_STRE(2,ICOL)
      SS   =  ULT_STRE(3,ICOL)
      STM  =  DABS(ST)
      SCM  =  DABS(SC)
      SSM  =  DABS(SS)
      SCP  = -SCM
 
! Calculate margins for ROD1 element. S1 is axial stress, S2 is torsional stress (S3, S4 not used) 
! MS1 is the margin for axial stress and MS2 is the margin for torsion.

      S1M = DABS(S1)
 
      IF (S1 >= ZERO) THEN                                 ! ROD margin for positive axial stress
 
         IF (S1M < EPS5) THEN
            MS1  = ONEPP10
            MSP1 = '0'
         ELSE
            MS1  = (STM/S1) - ONE
            MSP1 = '1'
         ENDIF
         IF (STM < EPS5) THEN
            MSP1 = '0'
         ENDIF
 
      ELSE                                                 ! ROD margin for negative axial stress
 
         IF (S1M < EPS5) THEN
            MS1  = ONEPP10
            MSP1 = '0'
         ELSE
            MS1  = (SCP/S1) - ONE
            MSP1 = '1'
         ENDIF
         IF (SCM < EPS5) THEN
            MSP1 = '0'
         ENDIF
 
      ENDIF

      S2M = DABS(S2)                                       ! ROD torsional margin (positive or negative torsional stress)
 
      IF (S2M < EPS5) THEN
         MS2  = ONEPP10
         MSP2 = '0'
      ELSE
         MS2  = (SSM/S2M) - ONE
         MSP2 = '1'
      ENDIF
      IF (SSM < EPS5) THEN
         MSP2 = '0'
      ENDIF
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

      END SUBROUTINE ROD_MARGIN
