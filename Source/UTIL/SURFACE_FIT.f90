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

      SUBROUTINE SURFACE_FIT ( NUM_FITS, NUM_COEFFS, XI, YI, WI, XO, YO, WO, DEB, MESSAGE, OUNT, POLY_PCT_ERR, PCT_ERR_MAX, IERR )

! Fits a 2D surface polynomial to a set of input values. The subroutine is coded to fit a surface of up to 2nd order:

!              WF(X,Y) = B(0) + B(1)*X + B(2)*Y + B(3)*XY + B(4)*X^2 + B(5)*Y^2

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE SUBR_BEGEND_LEVELS, ONLY    :  SURFACE_FIT_BEGEND
      USE LSQ_MYSTRAN

      USE SURFACE_FIT_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SURFACE_FIT'
      CHARACTER(LEN=*), INTENT(IN)    :: MESSAGE               ! Message printed if debug output is printed

      INTEGER(LONG), INTENT(IN)       :: DEB                   ! If > 0 then DEB_SURFACE_FIT will be run
      INTEGER(LONG), INTENT(IN)       :: NUM_FITS              ! Number of data points to fit
      INTEGER(LONG), INTENT(IN)       :: NUM_COEFFS            ! Number of coefficients in the fitting polynomial
      INTEGER(LONG), INTENT(IN)       :: OUNT(2)               ! Output units for SURFACE_FIT
      INTEGER(LONG), INTENT(OUT)      :: IERR                  ! Error indicator
      INTEGER(LONG)                   :: I,J                   ! DO loop indices
      INTEGER(LONG)                   :: IFAULT                ! Return code from subr REGCF
      INTEGER(LONG), PARAMETER        :: MAX_COEFFS = 6        ! Maximum number of coefficients coded for ther polynomial fit
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SURFACE_FIT_BEGEND

      LOGICAL                         :: LINDEP(MAX_COEFFS)

      REAL(DOUBLE), INTENT(IN)        :: WI(NUM_FITS)          ! Values of the function to fit at the input data points
      REAL(DOUBLE), INTENT(IN)        :: XI(NUM_FITS)          ! X coords of the input  data points
      REAL(DOUBLE), INTENT(IN)        :: YI(NUM_FITS)          ! Y coords of the input  data points
      REAL(DOUBLE), INTENT(IN)        :: XO(NUM_FITS)          ! X coords of the output data points
      REAL(DOUBLE), INTENT(IN)        :: YO(NUM_FITS)          ! Y coords of the output data points
      REAL(DOUBLE), INTENT(OUT)       :: POLY_PCT_ERR(NUM_FITS)! % difference between fitted & actual data (norm'd to max act data)
      REAL(DOUBLE), INTENT(OUT)       :: PCT_ERR_MAX           ! Max value from array POLY_PCT_ERR (max error at any input data pt)
      REAL(DOUBLE), INTENT(OUT)       :: WO(NUM_FITS)          ! Values of the function to fit at the output data points
      REAL(DOUBLE)                    :: DEN                   ! Intermediate value in a calculation
      REAL(DOUBLE)                    :: B(0:MAX_COEFFS-1)     ! Coefficients in the polynomial fit
      REAL(DOUBLE)                    :: RES(NUM_FITS)         ! Actual minus fitted data
      REAL(DOUBLE)                    :: RES_ABS               ! Max abs of the residual
      REAL(DOUBLE)                    :: WF(NUM_FITS)          ! Values at the input XI, YI coords of the fitted polynomial
      REAL(DOUBLE)                    :: WI_MAX                ! Max abs value of WI


      REAL(DOUBLE)                    :: XROW(0:MAX_COEFFS-1)! Array of XYI values for one data point

                                                             ! Values of XI, YI and their products to go into the poly fit eqn
      REAL(DOUBLE)                    :: XYI(NUM_FITS,0:MAX_COEFFS-1)

                                                             ! Values of XO, YO and their products to go into the poly fit eqn
      REAL(DOUBLE)                    :: XYO(NUM_FITS,0:MAX_COEFFS-1)

      REAL(DOUBLE), PARAMETER         :: WT = ONE            ! Parameter

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      IERR = 0

! Make sure that we have not requested an impossible situation with NUM_COEFFS

      IF ((NUM_COEFFS < 1) .OR. (NUM_COEFFS > MAX_COEFFS)) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(OUNT(1),958) SUBR_NAME, NUM_COEFFS, MAX_COEFFS
         IF (OUNT(2) /= OUNT(1)) THEN
            WRITE(OUNT(2),958) SUBR_NAME, NUM_COEFFS, MAX_COEFFS
         ENDIF
         IERR = 1
         RETURN
      ENDIF

! Initialize

      DO I=1,NUM_FITS
         DO J=1,MAX_COEFFS
            XYI(I,J-1) = ZERO
            XROW(J-1)  = ZERO
            LINDEP(J) = .FALSE.
         ENDDO
      ENDDO

! Calc polynomial fit to the input WI at its coords, XI, YI

      CALL STARTUP (NUM_COEFFS-1, .TRUE.)

      WI_MAX = ZERO
      DO I=1,NUM_FITS
         IF (DABS(WI(I)) > WI_MAX) WI_MAX = WI(I)
         IF (NUM_COEFFS >= 1) THEN
            XYI(I,0) = ONE
         ENDIF
         IF (NUM_COEFFS >= 2) THEN
            XYI(I,1) = XI(I)
         ENDIF
         IF (NUM_COEFFS >= 3) THEN
            XYI(I,2) = YI(I)
         ENDIF
         IF (NUM_COEFFS >= 4) THEN
            XYI(I,3) = XI(I)*XI(I)
         ENDIF
         IF (NUM_COEFFS >= 5) THEN
            XYI(I,4) = XI(I)*YI(I)
         ENDIF
         IF (NUM_COEFFS >= 6) THEN
            XYI(I,5) = YI(I)*YI(I)
         ENDIF
         XROW(1:NUM_COEFFS-1) = XYI(I,1:NUM_COEFFS-1)
         XROW(0) = ONE
         CALL INCLUD ( WT, XROW, WI(I) )
      ENDDO 

      CALL TOLSET ()

      CALL SING ( LINDEP, IFAULT )

      CALL REGCF ( B, NUM_COEFFS, IFAULT )

! Calc percent errors in polynomial fit to input data

      RES_ABS = ZERO
      DO I=1,NUM_FITS
         WF(I) = ZERO
         DO J=0,NUM_COEFFS-1
            WF(I) = WF(I) + B(J)*XYI(I,J)
         ENDDO
         RES(I) = WI(I) - WF(I)
         IF (DABS(RES(I)) > RES_ABS) THEN
            RES_ABS = DABS(RES(I))
         ENDIF
      ENDDO

      PCT_ERR_MAX = ZERO
      DO I=1,NUM_FITS
         IF (DABS(WI_MAX) > 0.0) THEN
            DEN = DABS(WI_MAX)
         ELSE
            DEN = ONE
         ENDIF
         POLY_PCT_ERR(I) = 100*RES(I)/DEN
         IF (DABS(PCT_ERR_MAX) < DABS(POLY_PCT_ERR(I))) THEN
            PCT_ERR_MAX = POLY_PCT_ERR(I)
         ENDIF
      ENDDO

! Calc fit to output coords, XO, YO 

      DO I=1,NUM_FITS
         IF (NUM_COEFFS >= 1) THEN
            XYO(I,0)   = ONE
         ENDIF
         IF (NUM_COEFFS >= 2) THEN
            XYO(I,1)   = XO(I)
         ENDIF
         IF (NUM_COEFFS >= 3) THEN
            XYO(I,2)   = YO(I)
         ENDIF
         IF (NUM_COEFFS >= 4) THEN
            XYO(I,3)   = XO(I)*XO(I)
         ENDIF
         IF (NUM_COEFFS >= 5) THEN
            XYO(I,4)   = XO(I)*YO(I)
         ENDIF
         IF (NUM_COEFFS >= 6) THEN
            XYO(I,5)   = YO(I)*YO(I)
         ENDIF
         WO(I) = ZERO
         DO J=0,NUM_COEFFS-1
            WO(I) = WO(I) + B(J)*XYO(I,J)
         ENDDO
      ENDDO

      IF (DEB > 0) CALL DEB_SURFACE_FIT

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  958 FORMAT(' *ERROR   958: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' THE VX VECTOR FOR ',A,' ELEMENT ',I8,' WAS LEFT UNSORTED. IT MUST BE SORTED TO DETERMINE VY, VZ') 

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE DEB_SURFACE_FIT

      IMPLICIT NONE

      INTEGER(LONG)                   :: II                ! DO loop index

! **********************************************************************************************************************************
      WRITE(OUNT(2),2000)
      WRITE(OUNT(2),'(A)') ' DEBUG 175 results from subr SURFACE_FIT'//MESSAGE//':'
      WRITE(OUNT(2),*)     ' --------------------------------------'
      WRITE(OUNT(2),2001) IFAULT
      WRITE(OUNT(2),'(A,1ES10.2,A)') ' PCT_ERR_MAX = ',PCT_ERR_MAX,'%'
      WRITE(OUNT(2),*)

      IF      (NUM_COEFFS == 1) THEN
         WRITE(OUNT(2),2011) NUM_FITS, NUM_COEFFS
         WRITE(OUNT(2),2021) (B(I), I=0,NUM_COEFFS-1)
      ELSE IF (NUM_COEFFS == 2) THEN
         WRITE(OUNT(2),2012) NUM_FITS, NUM_COEFFS
         WRITE(OUNT(2),2022) (B(I), I=0,NUM_COEFFS-1)
      ELSE IF (NUM_COEFFS == 3) THEN
         WRITE(OUNT(2),2013) NUM_FITS, NUM_COEFFS
         WRITE(OUNT(2),2023) (B(I), I=0,NUM_COEFFS-1)
      ELSE IF (NUM_COEFFS == 4) THEN
         WRITE(OUNT(2),2014) NUM_FITS, NUM_COEFFS
         WRITE(OUNT(2),2024) (B(I), I=0,NUM_COEFFS-1)
      ELSE IF (NUM_COEFFS == 5) THEN
         WRITE(OUNT(2),2014) NUM_FITS, NUM_COEFFS
         WRITE(OUNT(2),2025) (B(I), I=0,NUM_COEFFS-1)
      ELSE IF (NUM_COEFFS == 6) THEN
         WRITE(OUNT(2),2014) NUM_FITS, NUM_COEFFS
         WRITE(OUNT(2),2026) (B(I), I=0,NUM_COEFFS-1)
      ENDIF
      WRITE(OUNT(2),*)

      IF (RES_ABS > 0) THEN
         WRITE(OUNT(2),2031)
      ELSE
         WRITE(OUNT(2),2032)
      ENDIF

      DO II=1,NUM_FITS
         IF (DABS(WI_MAX) > 0.0) THEN
            WRITE(OUNT(2),2051) II, XI(II), YI(II), WI(II), WF(II), RES(II), POLY_PCT_ERR(II)
         ELSE
            WRITE(OUNT(2),2052) II, XI(II), YI(II), WI(II), WF(II), RES(II)
         ENDIF
      ENDDO

      WRITE(OUNT(2),*)
      WRITE(OUNT(2),2061)
      DO II=1,NUM_FITS
         WRITE(OUNT(2),2062) II, XO(II), YO(II), WO(II)
      ENDDO

! **********************************************************************************************************************************
 2000 FORMAT(' ******************************************************************************************************************',&
             '******************')

 2001 FORMAT(' Return code from polynomial fit: IFAULT = ',I3,/)

 2011 FORMAT(' Fitted quadratic surface for ',I2,' data points using a ',I2,'st order polynomial:',/,                              &
             ' -------------------------------------------------------------------------')

 2012 FORMAT(' Fitted quadratic surface for ',I2,' data points using a ',I2,'nd order polynomial:',/,                              &
             ' -------------------------------------------------------------------------')

 2013 FORMAT(' Fitted quadratic surface for ',I2,' data points using a ',I2,'rd order polynomial:',/,                              &
             ' -------------------------------------------------------------------------')

 2014 FORMAT(' Fitted quadratic surface for ',I2,' data points using a ',I2,'th order polynomial:',/,                              &
             ' -------------------------------------------------------------------------')

 2021 FORMAT('  Y = B0                                              ',//,                                                          &
             '      where: B0 = ',1ES14.6,/)

 2022 FORMAT('  Y = B0 + B1*X1                                      ',//,                                                          &
             '      where: B0 = ',1ES14.6,/,                                                                                       &
             '             B1 = ',1ES14.6,/)

 2023 FORMAT('  Y = B0 + B1*X1 + B2*X2                              ',//,                                                          &
             '      where: B0 = ',1ES14.6,/,                                                                                       &
             '             B1 = ',1ES14.6,/,                                                                                       &
             '             B2 = ',1ES14.6,/)

 2024 FORMAT('  Y = B0 + B1*X1 + B2*X2 + B3*X1^2                    ',//,                                                          &
             '      where: B0 = ',1ES14.6,/,                                                                                       &
             '             B1 = ',1ES14.6,/,                                                                                       &
             '             B2 = ',1ES14.6,/,                                                                                       &
             '             B3 = ',1ES14.6,/)

 2025 FORMAT('  Y = B0 + B1*X1 + B2*X2 + B3*X1^2 +B4*X1*Y1          ',//,                                                          &
             '      where: B0 = ',1ES14.6,/,                                                                                       &
             '             B1 = ',1ES14.6,/,                                                                                       &
             '             B2 = ',1ES14.6,/,                                                                                       &
             '             B3 = ',1ES14.6,/,                                                                                       &
             '             B4 = ',1ES14.6,/)

 2026 FORMAT('  Y = B0 + B1*X1 + B2*X2 +B3*X1^2 + B4*X1*Y1 + B5*Y2^2',//,                                                          &
             '      where: B0 = ',1ES14.6,/,                                                                                       &
             '             B1 = ',1ES14.6,/,                                                                                       &
             '             B2 = ',1ES14.6,/,                                                                                       &
             '             B3 = ',1ES14.6,/,                                                                                       &
             '             B4 = ',1ES14.6,/,                                                                                       &
             '             B5 = ',1ES14.6,/)


 2031 FORMAT(' Comparison of actual data to fitted curve:',/,' -----------------------------------------',/,                       &
             '     I      XI(I)         YI(I)         WI(I)         WF(I)       WI - WF       Err')

 2032 FORMAT(' Comparison of actual data to fitted curve:',/,' -----------------------------------------',/,                       &
             '     I      XI(I)         YI(I)         WI(I)         WF(I)       WI - WF')

 2051 FORMAT(I6,5(1ES14.6),F8.3,'%')

 2052 FORMAT(I6,5(1ES14.6))

 2061 FORMAT(' Polynomial values at output data points:',/,' -----------------------------------------',/,                       &
             '     I      XO(I)         YO(I)         WO(I)')

 2062 FORMAT(I6,3(1ES14.6))

! **********************************************************************************************************************************

      END SUBROUTINE DEB_SURFACE_FIT

      END SUBROUTINE SURFACE_FIT

