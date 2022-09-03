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

      SUBROUTINE POLY_FAILURE_INDEX ( STREi, STRE_ALLOWABLES, FAILURE_INDEX )

! Calculates failure index based on polynomial failure criteria (HILL, HOFF, TSAI), STRESS array and ultimate stresses

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, HALF, ONE, TWO
      USE MACHINE_PARAMS, ONLY        :  MACH_LARGE_NUM
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  EPSIL
      USE MODEL_STUF, ONLY            :  FAILURE_THEORY
      USE SUBR_BEGEND_LEVELS, ONLY    :  POLY_FAILURE_INDEX_BEGEND

      USE POLY_FAILURE_INDEX_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'POLY_FAILURE_INDEX'

      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = POLY_FAILURE_INDEX_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: STRE_ALLOWABLES(9)! Allowable stresses (incl tension and compr for normal stresses)
      REAL(DOUBLE), INTENT(IN)        :: STREi(6)          ! 6 components of stress
      REAL(DOUBLE), INTENT(OUT)       :: FAILURE_INDEX     ! Failure index (scalar value)
      REAL(DOUBLE)                    :: DUM61(6,1)        ! Intermediate matrix
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare for real zero
      REAL(DOUBLE)                    :: F1(6)             ! 1st order Interaction coeff col matrix
      REAL(DOUBLE)                    :: F2(6,6)           ! 2nd order Interaction coeff matrix
      REAL(DOUBLE)                    :: F1I_MAT(1)        ! The part of failure index due to F1 terms
      REAL(DOUBLE)                    :: F2I_MAT(1)        ! The part of failure index due to F2 terms
      REAL(DOUBLE)                    :: XT,XC             ! Tension/compr allowable normal stresses in direction 1
      REAL(DOUBLE)                    :: YT,YC             ! Tension/compr allowable normal stresses in direction 2
      REAL(DOUBLE)                    :: ZT,ZC             ! Tension/compr allowable normal stresses in direction 3
      REAL(DOUBLE)                    :: X,Y,Z             ! X = XT if sig1 > 0 or X = XC if sig1 < 0, etc
      REAL(DOUBLE)                    :: R,S,T             ! Allowable shear stresses in planes 23, 13, 12

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

      DO I=1,6
         F1(I) = ZERO
         DO J=1,6
            F2(I,J) = ZERO
         ENDDO
      ENDDO

      XT = STRE_ALLOWABLES(1)    ;    XC = STRE_ALLOWABLES(2)      
      YT = STRE_ALLOWABLES(3)    ;    YC = STRE_ALLOWABLES(4)      
      ZT = STRE_ALLOWABLES(5)    ;    ZC = STRE_ALLOWABLES(6)      

      R  = STRE_ALLOWABLES(7)
      S  = STRE_ALLOWABLES(8)
      T  = STRE_ALLOWABLES(9)

      CALL GET_MACHINE_PARAMS

! Reset STRE_ALLOWABLES such that, if one is zero, then its inverse it is reset to DSQRT(MACH_LARGE_NUM).
! We use DSQRT() since we will be calculating terms that haxe the product of XT*XC, etc, in the denominator.

      IF (DABS(XT) < EPS1) THEN    ;    XT = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
      IF (DABS(XC) < EPS1) THEN    ;    XC = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
      IF (DABS(YT) < EPS1) THEN    ;    YT = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
      IF (DABS(YC) < EPS1) THEN    ;    YC = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
      IF (DABS(ZT) < EPS1) THEN    ;    ZT = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
      IF (DABS(ZC) < EPS1) THEN    ;    ZC = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
      IF (DABS(R ) < EPS1) THEN    ;    R  = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
      IF (DABS(S ) < EPS1) THEN    ;    S  = DSQRT(MACH_LARGE_NUM)    ;    ENDIF
      IF (DABS(T ) < EPS1) THEN    ;    T  = DSQRT(MACH_LARGE_NUM)    ;    ENDIF

      IF      (FAILURE_THEORY == 'HILL') THEN

         IF (STREi(1) > ZERO) THEN
            X = XT
         ELSE
            X = XC
         ENDIF

         IF (STREi(2) > ZERO) THEN
            Y = YT
         ELSE
            Y = YC
         ENDIF

         IF (STREi(3) > ZERO) THEN
            Z = ZT
         ELSE
            Z = ZC
         ENDIF

         F2(1,1) = ONE/(X*X)
         F2(2,2) = ONE/(Y*Y)
         F2(3,3) = ONE/(Z*Z)

         F2(4,4) = ONE/(R*R)
         F2(5,5) = ONE/(S*S)
         F2(6,6) = ONE/(T*T)

         F2(1,2) = -HALF*(ONE/(X*X) + ONE/(Y*Y) - ONE/(Z*Z))
         F2(1,3) = -HALF*(ONE/(Z*Z) + ONE/(X*X) - ONE/(Y*Y))
         F2(2,3) = -HALF*(ONE/(Y*Y) + ONE/(Z*Z) - ONE/(X*X))

         DO I=1,6
            DO J=1,I-1
               F2(I,J) = F2(J,I)
            ENDDO
         ENDDO

         CALL MATMULT_FFF   ( F2  , STREi , 6, 6, 1, DUM61    )
         CALL MATMULT_FFF_T ( STREi, DUM61, 6, 1, 1, F2I_MAT )

         FAILURE_INDEX = F2I_MAT(1)

      ELSE IF (FAILURE_THEORY == 'HOFF') THEN

         F1(1)   = ONE/XT - ONE/XC
         F1(2)   = ONE/YT - ONE/YC
         F1(3)   = ONE/ZT - ONE/ZC

         F2(1,1) = ONE/(XT*XC)
         F2(2,2) = ONE/(YT*YC)
         F2(3,3) = ONE/(ZT*ZC)

         F2(4,4) = ONE/(R*R)
         F2(5,5) = ONE/(S*S)
         F2(6,6) = ONE/(T*T)

         F2(1,2) = -HALF*(F2(1,1) + F2(2,2) - F2(3,3))
         F2(1,3) = -HALF*(F2(1,1) + F2(3,3) - F2(2,2))
         F2(2,3) = -HALF*(F2(3,3) + F2(2,2) - F2(1,1))

         CALL MATMULT_FFF   ( F2  , STREi , 6, 6, 1, DUM61   )
         CALL MATMULT_FFF_T ( STREi, DUM61, 6, 1, 1, F2I_MAT )

         CALL MATMULT_FFF_T ( F1  , STREi , 6, 1, 1, F1I_MAT )

         FAILURE_INDEX = F1I_MAT(1) + F2I_MAT(1)

      ELSE IF (FAILURE_THEORY == 'TSAI') THEN

         F1(1)   = ONE/XT - ONE/XC
         F1(2)   = ONE/YT - ONE/YC
         F1(3)   = ONE/ZT - ONE/ZC

         F2(1,1) = ONE/(XT*XC)
         F2(2,2) = ONE/(YT*YC)
         F2(3,3) = ONE/(ZT*ZC)

         F2(4,4) = ONE/(R*R)
         F2(5,5) = ONE/(S*S)
         F2(6,6) = ONE/(T*T)         

         F2(1,2) = -HALF*ONE/((DSQRT(XT*XC))*(DSQRT(YT*YC)))
         F2(1,3) = -HALF*ONE/((DSQRT(XT*XC))*(DSQRT(ZT*ZC)))
         F2(2,3) = -HALF*ONE/((DSQRT(YT*YC))*(DSQRT(ZT*ZC)))

         CALL MATMULT_FFF   ( F2  , STREi , 6, 6, 1, DUM61    )
         CALL MATMULT_FFF_T ( STREi, DUM61, 6, 1, 1, F2I_MAT )

         CALL MATMULT_FFF_T ( F1  , STREi , 6, 1, 1, F1I_MAT )

         FAILURE_INDEX = F1I_MAT(1) + F2I_MAT(1)

      ELSE

         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,9205) 'POLYNOMIAL', FAILURE_THEORY, 'HILL, HOFF or TSAI'
         WRITE(F06,9205) 'POLYNOMIAL', FAILURE_THEORY, 'HILL, HOFF or TSAI'
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
                    ,/,14X,' INVALID ',A,' FAILURE THEORY = ',A,'. VALID ONES ARE: ',A)

! **********************************************************************************************************************************

      END SUBROUTINE POLY_FAILURE_INDEX

