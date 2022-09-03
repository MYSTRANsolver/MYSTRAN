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

      SUBROUTINE POLYNOM_FIT_STRE_STRN ( STR_IN, NROW, NCOL, STR_OUT, STR_OUT_PCT_ERR, STR_OUT_ERR_INDEX, PCT_ERR_MAX )

! Extrapolates stress/strain values at the points at which the stress/strain matrices were calculated (i.e. Gauss points or MIN4T
! sub-tria centroids) to element corner nodes. Uses polynomial surface fit algorithm in module LSQ_MYSTRAN with the polynomial
! fit returned from subr SURFACE_FIT, called herein.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_ORDER_GAUSS, MAX_STRESS_POINTS
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, TWO, THREE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE MODEL_STUF, ONLY            :  EID, ELGP, TYPE, XEL
      USE PARAMS, ONLY                :  Q4SURFIT, QUAD4TYP
      USE SUBR_BEGEND_LEVELS, ONLY    :  POLYNOM_FIT_STRE_STRN_BEGEND

      USE POLYNOM_FIT_STRE_STRN_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'POLYNOM_FIT_STRE_STRN'
      CHARACTER(100*BYTE)             :: MESSAGE             ! Message to send to subr SURFACE_FIT for debug output purposes

      INTEGER(LONG), INTENT(IN)       :: NCOL                ! Number of cols in arrays STR_IN, STR_OUT (1 more than the num of elem
!                                                              nodes since 1st col in arrays is due to elem center stress/strain
!                                                              which is not processed by this subr)
      INTEGER(LONG), INTENT(IN)       :: NROW                ! Number of rows in arrays STR_IN, STR_OUT

                                                             ! Stress/strain index number (1-9) where % error in fit is max
      INTEGER(LONG), INTENT(OUT)      :: STR_OUT_ERR_INDEX(MAX_STRESS_POINTS)
      INTEGER(LONG), PARAMETER        :: IORD = 2            ! Gaussian integration order
      INTEGER(LONG)                   :: I,J                 ! DO loop indices
      INTEGER(LONG)                   :: OUNT(2)             ! Output units for SURFACE_FIT
      INTEGER(LONG)                   :: SF_IERR             ! Output error indicator from subr SURFACE_FIT
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = POLYNOM_FIT_STRE_STRN_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: STR_IN(NROW,NCOL)   ! Input stress/strain vals. NROW are num of diff stress/strain vals and
!                                                              NCOL are number of points to use in the poly fit for one value

      REAL(DOUBLE), INTENT(OUT)       :: STR_OUT(NROW,NCOL)  ! Output stress/strain vals. NROW are num of diff stress/strain vals
!                                                              and NCOL are number of points to use in the poly fit for one value

      REAL(DOUBLE), INTENT(OUT)       :: PCT_ERR_MAX         ! Max value from array PCT_ERR (max poly fit err at any input data pt)

                                                             ! 2D array of POLY_PCT_ERR for all stress points
      REAL(DOUBLE)                    :: PCT_ERR(NROW,MAX_STRESS_POINTS)

                                                             ! % diff bet. fitted and actual input data for 1 stress or strain point
!                                                              Only NROW x NCOL-1 vals determined (or needed)
      REAL(DOUBLE)                    :: POLY_PCT_ERR(MAX_STRESS_POINTS)

                                                             ! 1st val is zero for center stress (not fitted) and remainder are for
!                                                              the points fitted (from row 2 to row NCOL; or NCOL-1 values) 
      REAL(DOUBLE), INTENT(OUT)       :: STR_OUT_PCT_ERR(MAX_STRESS_POINTS)

      REAL(DOUBLE)                    :: PCT_ERR_1STR        ! % error from 1 of the NROW stress/strain values
      REAL(DOUBLE)                    :: HHH(MAX_ORDER_GAUSS)! Gauss weights
      REAL(DOUBLE)                    :: SSS(MAX_ORDER_GAUSS)! Gauss point coords
      REAL(DOUBLE)                    :: XI(NCOL-1)          ! X coords of the input  data points
      REAL(DOUBLE)                    :: YI(NCOL-1)          ! Y coords of the input  data points
      REAL(DOUBLE)                    :: WI(NCOL-1)          ! Values of the function to fit at the input data points
      REAL(DOUBLE)                    :: XO(NCOL-1)          ! X coords of the output data points
      REAL(DOUBLE)                    :: YO(NCOL-1)          ! Y coords of the output data points
      REAL(DOUBLE)                    :: XEA(NCOL-1,3)       ! Actual local element coords corresponding to XEP
      REAL(DOUBLE)                    :: XEP(NCOL-1,3)       ! Parametric coords of NCOL points
      REAL(DOUBLE)                    :: WO(NCOL-1)          ! Values of the function to fit at the output data points

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      MESSAGE(1:) = ' '

      OUNT(1) = ERR
      OUNT(2) = F06

      DO J=1,MAX_STRESS_POINTS
         POLY_PCT_ERR(J) = ZERO
         STR_OUT_PCT_ERR(J) = ZERO
         DO I=1,NROW
            PCT_ERR(I,J) = ZERO
         ENDDO
      ENDDO

      DO J=1,NCOL
         STR_OUT_ERR_INDEX(J) = 0
         DO I=1,NROW
            STR_OUT(I,J) = STR_IN(I,J)
         ENDDO
      ENDDO 

! Calc actual coords of the points for which the BEi, SEi matrices were calculated

      IF (TYPE(1:5) == 'QUAD4') THEN

         IF (NCOL /= ELGP+1) THEN                          ! We assume that the number of stress/strain points = ELGP+1, so check it
            WRITE(ERR,9202) SUBR_NAME, TYPE, NCOL, ELGP+1
            WRITE(F06,9202) SUBR_NAME, TYPE, NCOL, ELGP+1
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )
         ENDIF
                                                           ! For the MIN4  QUAD4, XEP parametric coords are the Gauss point coords
         IF ((QUAD4TYP == 'MIN4 ') .OR. (TYPE(1:6) == 'QUAD4K')) THEN
            CALL ORDER_GAUSS ( IORD, SSS, HHH )
            XEP(1,1) =  SSS(1)      ;   XEP(1,2) =  SSS(1)      ;   XEP(1,3) = ZERO
            XEP(2,1) =  SSS(1)      ;   XEP(2,2) =  SSS(2)      ;   XEP(2,3) = ZERO
            XEP(3,1) =  SSS(2)      ;   XEP(3,2) =  SSS(1)      ;   XEP(3,3) = ZERO
            XEP(4,1) =  SSS(2)      ;   XEP(4,2) =  SSS(2)      ;   XEP(4,3) = ZERO
         ELSE                                              ! For the MIN4T QUAD4, XEP parametric coords are centroids of the 4 trias
            XEP(1,1) =  ZERO        ;   XEP(1,2) = -TWO/THREE   ;   XEP(1,3) = ZERO
            XEP(2,1) =  TWO/THREE   ;   XEP(2,2) =  ZERO        ;   XEP(2,3) = ZERO
            XEP(3,1) =  ZERO        ;   XEP(3,2) =  TWO/THREE   ;   XEP(3,3) = ZERO
            XEP(4,1) = -TWO/THREE   ;   XEP(4,2) =  ZERO        ;   XEP(4,3) = ZERO
         ENDIF

         CALL PARAM_CORDS_ACT_CORDS ( 4, IORD, XEP, XEA )  ! Get actual local elem coords corresponding to the parametric coords
         DO I=1,ELGP
            XI(I) = XEA(I,1)                               ! XEA are act coords of the XEP.        XI is input to subr SURFACE_FIT
            YI(I) = XEA(I,2)
            XO(I) = XEL(I,1)                               ! XEL are act coords of the elem nodes. XO is input to subr SURFACE_FIT
            YO(I) = XEL(I,2)

         ENDDO

! Now do surface fit on the data (STR_IN) to get stress/strain at the elem corners (STR_OUT)

         PCT_ERR_MAX = ZERO
         DO I=1,NROW
            DO J=2,NCOL
               WI(J-1) = STR_IN(I,J)                       ! The input stress/strain are the values at the Gauss or tria centroids
               STR_OUT_PCT_ERR(J) = ZERO
            ENDDO
            WRITE(MESSAGE( 1:16),'(A )') ' for stress row '
            WRITE(MESSAGE(17:18),'(I2)') I
            WRITE(MESSAGE(19:22),'(A )') ' of '
            WRITE(MESSAGE(23:24),'(I2)') NROW
            WRITE(MESSAGE(25:41),'(A )') ' for MIN4T QUAD4 '
            WRITE(MESSAGE(42:49),'(I8)') EID
            CALL SURFACE_FIT ( NCOL-1, Q4SURFIT, XI, YI, WI, XO, YO, WO, DEBUG(175), MESSAGE, OUNT, POLY_PCT_ERR, PCT_ERR_1STR,    &
                               SF_IERR )
            IF (DABS(PCT_ERR_1STR) > DABS(PCT_ERR_MAX)) THEN
               PCT_ERR_MAX = PCT_ERR_1STR
            ENDIF
            DO J=2,NCOL
               STR_OUT(I,J) = WO(J-1)                      ! These are the stress/strain values extrapolated to the element corners
               PCT_ERR(I,J) = POLY_PCT_ERR(J-1)
            ENDDO
         ENDDO
                                                           ! Sort over rows of PCT_ERR to get largest abs val for all stress/strain
         DO J=2,NCOL                                       ! values for each stress point
            DO I=1,NROW
               IF (DABS(PCT_ERR(I,J)) > DABS(STR_OUT_PCT_ERR(J))) THEN
                  STR_OUT_PCT_ERR(J)   = PCT_ERR(I,J)
                  STR_OUT_ERR_INDEX(J) = I
               ENDIF
            ENDDO
         ENDDO

      ELSE

         RETURN

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
            WRITE(F06,9202) SUBR_NAME, TYPE, NCOL, ELGP

 9202 FORMAT(' *ERROR  9202: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' FOR ELEMENT TYPE = "',A,'" THE INPUT VALUE OF ARGUMENT NCOL WAS = ',I8,' BUT MUST BE ELGP+1 = ',I8)


! **********************************************************************************************************************************

      END SUBROUTINE POLYNOM_FIT_STRE_STRN

