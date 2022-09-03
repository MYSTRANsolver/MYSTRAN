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

      SUBROUTINE INDEP_FAILURE_INDEX ( STREi, STRNi, STRE_ALLOWABLES, STRN_ALLOWABLES, FAILURE_INDEX )

! Calculates ply failure index based on failure criteria defined by the user on the PCOMP Bulk Data entry (e.g. max strain)

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MACHINE_PARAMS, ONLY        :  MACH_LARGE_NUM
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  EPSIL
      USE MODEL_STUF, ONLY            :  FAILURE_THEORY
      USE SUBR_BEGEND_LEVELS, ONLY    :  INDEP_FAILURE_INDEX_BEGEND

      USE INDEP_FAILURE_INDEX_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'INDEP_FAILURE_INDEX'

      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = INDEP_FAILURE_INDEX_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: STRE_ALLOWABLES(9)! Allowable stresses (incl tension and compr for normal stresses)
      REAL(DOUBLE), INTENT(IN)        :: STRN_ALLOWABLES(9)! Allowable strains (incl tension and compr for normal stresses)
      REAL(DOUBLE), INTENT(IN)        :: STREi(6)          ! 6 components of strain
      REAL(DOUBLE), INTENT(IN)        :: STRNi(6)          ! 6 components of stress
      REAL(DOUBLE), INTENT(OUT)       :: FAILURE_INDEX     ! Failure index (scalar value)
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare for real zero
      REAL(DOUBLE)                    :: XT,XC             ! Tension/compr allowable normal stresses in direction 1
      REAL(DOUBLE)                    :: YT,YC             ! Tension/compr allowable normal stresses in direction 2
      REAL(DOUBLE)                    :: ZT,ZC             ! Tension/compr allowable normal stresses in direction 3
      REAL(DOUBLE)                    :: R,S,T             ! Allowable shear stresses in planes 23, 13, 12
      REAL(DOUBLE)                    :: RATIO(6)          ! Ratios of stress or strain to an allowable

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      EPS1 = EPSIL(1)

      FAILURE_INDEX = ZERO

      CALL GET_MACHINE_PARAMS

! Reset STRE_ALLOWABLES such that, if one is zero, then its inverse it is reset to DSQRT(MACH_LARGE_NUM).
! We use DSQRT() since we will be calculating terms that haxe the product of XT*XC, etc, in the denominator.

      IF      (FAILURE_THEORY == 'STRE') THEN

         XT = STRE_ALLOWABLES(1)    ;    XC = STRE_ALLOWABLES(2)      
         YT = STRE_ALLOWABLES(3)    ;    YC = STRE_ALLOWABLES(4)      
         ZT = STRE_ALLOWABLES(5)    ;    ZC = STRE_ALLOWABLES(6)      

         R  = STRE_ALLOWABLES(7)
         S  = STRE_ALLOWABLES(8)
         T  = STRE_ALLOWABLES(9)
                                                           ! Reset STRE_ALLOWABLES such that, if one is zero, then its inverse is 
                                                           ! reset to DSQRT(MACH_LARGE_NUM).
         IF (DABS(XT) < EPS1) THEN    ;    XT = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
         IF (DABS(XC) < EPS1) THEN    ;    XC = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
         IF (DABS(YT) < EPS1) THEN    ;    YT = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
         IF (DABS(YC) < EPS1) THEN    ;    YC = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
         IF (DABS(ZT) < EPS1) THEN    ;    ZT = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
         IF (DABS(ZC) < EPS1) THEN    ;    ZC = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
         IF (DABS(R ) < EPS1) THEN    ;    R  = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
         IF (DABS(S ) < EPS1) THEN    ;    S  = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
         IF (DABS(T ) < EPS1) THEN    ;    T  = DSQRT(MACH_LARGE_NUM)    ;    ENDIF

         IF (STREi(1) > ZERO) THEN
            RATIO(1) = DABS(STREi(1)/XT)
         ELSE
            RATIO(1) = DABS(STREi(1)/XC)
         ENDIF   

         IF (STREi(2) > ZERO) THEN
            RATIO(2) = DABS(STREi(2)/YT)
         ELSE
            RATIO(2) = DABS(STREi(2)/YC)
         ENDIF   

         IF (STREi(3) > ZERO) THEN
            RATIO(3) = DABS(STREi(3)/ZT)
         ELSE
            RATIO(3) = DABS(STREi(3)/ZC)
         ENDIF   

         IF (STREi(4) > ZERO) THEN
            RATIO(4) = DABS(STREi(4)/R)
         ELSE
            RATIO(4) = DABS(STREi(4)/R)
         ENDIF   

         IF (STREi(5) > ZERO) THEN
            RATIO(5) = DABS(STREi(5)/S)
         ELSE
            RATIO(5) = DABS(STREi(5)/S)
         ENDIF   

         IF (STREi(6) > ZERO) THEN
            RATIO(6) = DABS(STREi(6)/T)
         ELSE
            RATIO(6) = DABS(STREi(6)/T)
         ENDIF   

         FAILURE_INDEX = RATIO(1)
         DO I=2,5
            IF (RATIO(I) > FAILURE_INDEX) THEN
               FAILURE_INDEX = RATIO(I)
            ENDIF
         ENDDO         

      ELSE IF (FAILURE_THEORY == 'STRN') THEN

         XT = STRN_ALLOWABLES(1)    ;    XC = STRN_ALLOWABLES(2)      
         YT = STRN_ALLOWABLES(3)    ;    YC = STRN_ALLOWABLES(4)      
         ZT = STRN_ALLOWABLES(5)    ;    ZC = STRN_ALLOWABLES(6)      

         R  = STRN_ALLOWABLES(7)
         S  = STRN_ALLOWABLES(8)
         T  = STRN_ALLOWABLES(9)
                                                           ! Reset STRN_ALLOWABLES such that, if one is zero, then its inverse is 
                                                           ! reset to DSQRT(MACH_LARGE_NUM).
         IF (DABS(XT) < EPS1) THEN    ;    XT = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
         IF (DABS(XC) < EPS1) THEN    ;    XC = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
         IF (DABS(YT) < EPS1) THEN    ;    YT = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
         IF (DABS(YC) < EPS1) THEN    ;    YC = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
         IF (DABS(ZT) < EPS1) THEN    ;    ZT = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
         IF (DABS(ZC) < EPS1) THEN    ;    ZC = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
         IF (DABS(R ) < EPS1) THEN    ;    R  = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
         IF (DABS(S ) < EPS1) THEN    ;    S  = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
         IF (DABS(T ) < EPS1) THEN    ;    T  = DSQRT(MACH_LARGE_NUM)    ;    ENDIF

         IF (STRNi(1) > ZERO) THEN
            RATIO(1) = DABS(STRNi(1)/XT)
         ELSE
            RATIO(1) = DABS(STRNi(1)/XC)
         ENDIF   

         IF (STRNi(2) > ZERO) THEN
            RATIO(2) = DABS(STRNi(2)/YT)
         ELSE
            RATIO(2) = DABS(STRNi(2)/YC)
         ENDIF   

         IF (STRNi(3) > ZERO) THEN
            RATIO(3) = DABS(STRNi(3)/ZT)
         ELSE
            RATIO(3) = DABS(STRNi(3)/ZC)
         ENDIF   

         IF (STRNi(4) > ZERO) THEN
            RATIO(4) = DABS(STRNi(4)/R)
         ELSE
            RATIO(4) = DABS(STRNi(4)/R)
         ENDIF   

         IF (STRNi(5) > ZERO) THEN
            RATIO(5) = DABS(STRNi(5)/S)
         ELSE
            RATIO(5) = DABS(STRNi(5)/S)
         ENDIF   

         IF (STRNi(6) > ZERO) THEN
            RATIO(6) = DABS(STRNi(6)/T)
         ELSE
            RATIO(6) = DABS(STRNi(6)/T)
         ENDIF   

         FAILURE_INDEX = RATIO(1)
         DO I=2,5
            IF (RATIO(I) > FAILURE_INDEX) THEN
               FAILURE_INDEX = RATIO(I)
            ENDIF
         ENDDO

      ELSE

         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,9205) FAILURE_THEORY
         WRITE(F06,9205) FAILURE_THEORY
         CALL OUTA_HERE ( 'Y' )

      ENDIF

      IF (DEBUG(190) == 0) THEN                            ! If user wants actual small value, use DEBUG(190)
         IF (FAILURE_INDEX < EPS1) THEN
            FAILURE_INDEX = ZERO
         ENDIF
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 9205 FORMAT(' *ERROR  9205: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INVALID FAILURE THEORY FOR = ',A)

! **********************************************************************************************************************************

      END SUBROUTINE INDEP_FAILURE_INDEX

