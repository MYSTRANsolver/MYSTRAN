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
 
      SUBROUTINE WRITE_MEFFMASS
 
! Writes output for modal effective mass
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ANS, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NVEC
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_MEFFMASS_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, ONE_HUNDRED, PI
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE EIGEN_MATRICES_1, ONLY      :  EIGEN_VAL, MEFFMASS
      USE MODEL_STUF, ONLY            :  MEFM_RB_MASS, LABEL, STITLE, TITLE
      USE PARAMS, ONLY                :  EPSIL, GRDPNT, MEFMCORD, MEFMGRID, MEFMLOC, SUPINFO, WTMASS
  
      USE WRITE_MEFFMASS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_MEFFMASS'
      CHARACTER(14*BYTE)              :: CHAR_PCT(6)       ! Character representation of MEFFMASS sum percents of total model mass
      CHARACTER(1*BYTE)               :: IHDR   = 'Y'      ! Indicator of whether to write an output header

      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_MEFFMASS_BEGEND

      REAL(DOUBLE)                    :: CYCLES            ! Circular frequency of a mode
      REAL(DOUBLE)                    :: EPS1              ! Small number to compare against zero
      REAL(DOUBLE)                    :: MEFM_TOTALS(6)    ! Totals for the 6 modal effective masses over all modes
      REAL(DOUBLE)                    :: MODES_PCT(6)      ! Modal mass as % of total mass

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

! Write output headers.

      IF (IHDR == 'Y') THEN

         WRITE(F06,900)

         WRITE(F06,909) TITLE(1)                           ! There is always a TITLE(1), etc (even if they are blank)
         WRITE(F06,909) STITLE(1)
         WRITE(F06,909) LABEL(1)

         WRITE(F06,*)
 
      ENDIF

! Write modal effective masses
  
      WRITE(F06,9102) MEFMCORD
      
      IF      (MEFMLOC == 'GRDPNT') THEN
         IF (MEFMGRID == 0) THEN
            WRITE(F06,9103)
         ELSE
            WRITE(F06,9104) GRDPNT
         ENDIF
      ELSE IF (MEFMLOC == 'CG    ') THEN
         WRITE(F06,9105)
      ELSE IF (MEFMLOC == 'GRID  ') THEN
         WRITE(F06,9106) MEFMGRID
      ENDIF
      
      IF (DEBUG(174) == 0) THEN
         WRITE(F06,9107)
      ELSE
         WRITE(F06,9108)
      ENDIF

      IF (DEBUG(200) > 0) THEN
         WRITE(ANS,*)
         WRITE(ANS,9202) MEFMCORD
         WRITE(ANS,9207)
      ENDIF

      DO J=1,6
         MEFM_TOTALS(J) = ZERO
      ENDDO   
 
      DO I=1,NVEC

         CYCLES = DSQRT(DABS(EIGEN_VAL(I)))/(TWO*PI)

         IF (DEBUG(174) == 0) THEN
            WRITE(F06,9110) I, CYCLES, (MEFFMASS(I,J)/WTMASS,J=1,6)
         ELSE
            WRITE(F06,9111) I, CYCLES, (MEFFMASS(I,J)/WTMASS,J=1,6)
         ENDIF

         IF (DEBUG(200) > 0) THEN
            WRITE(ANS,9210) I, CYCLES, (MEFFMASS(I,J)/WTMASS,J=1,6)
         ENDIF

         DO J=1,6
            MEFM_TOTALS(J) = MEFM_TOTALS(J) + MEFFMASS(I,J)/WTMASS
         ENDDO   

      ENDDO

      IF (DEBUG(174) == 0) THEN
         WRITE(F06,9112) (MEFM_TOTALS(J),J=1,6)
      ELSE
         WRITE(F06,9113) (MEFM_TOTALS(J),J=1,6)
      ENDIF
                                                           ! MEFM_RB_MASS is in the same units as in the DAT file
      IF (DEBUG(174) == 0) THEN
         WRITE(F06,9116) (MEFM_RB_MASS(I,I),I=1,6)
      ELSE
         WRITE(F06,9117) (MEFM_RB_MASS(I,I),I=1,6)
      ENDIF

      IF (DEBUG(200) > 0) THEN
         WRITE(ANS,9212) (MEFM_TOTALS(J),J=1,6)
         WRITE(ANS,9216) (MEFM_RB_MASS(I,I),I=1,6)
      ENDIF
   
! For each of the 6 modal masses, calc % of total mass. A character variable is used to store the % so that blank percentages can
! be printed if zero modal mass exists for a component (T1 - R3) or if a denominator in the % expression is zero

      IF (DABS(MEFM_RB_MASS(1,1)) > EPS1) THEN
         MODES_PCT(1) = ONE_HUNDRED*MEFM_TOTALS(1)/MEFM_RB_MASS(1,1)
         WRITE(CHAR_PCT(1),1001) MODES_PCT(1), '%'
      ELSE                                                 ! Denominator is zero so leave % blank
         CHAR_PCT(1)(1:) = ' '
         IF (MEFM_TOTALS(1) > EPS1) THEN                   ! Error: denominator is zero but numerator is not, so write message
            WRITE(ERR,2001)
            IF (SUPINFO == 'N') THEN
               WRITE(F06,2001)
            ENDIF
         ENDIF
      ENDIF

      IF (DABS(MEFM_RB_MASS(2,2)) > EPS1) THEN
         MODES_PCT(2) = ONE_HUNDRED*MEFM_TOTALS(2)/MEFM_RB_MASS(2,2)
         WRITE(CHAR_PCT(2),1001) MODES_PCT(2), '%'
      ELSE                                                 ! Denominator is zero so leave % blank
         CHAR_PCT(2)(1:) = ' '
         IF (MEFM_TOTALS(2) > EPS1) THEN                   ! Error: denominator is zero but numerator is not, so write message
            WRITE(ERR,2002)
            IF (SUPINFO == 'N') THEN
               WRITE(F06,2002)
            ENDIF
         ENDIF
      ENDIF

      IF (DABS(MEFM_RB_MASS(3,3)) > EPS1) THEN
         MODES_PCT(3) = ONE_HUNDRED*MEFM_TOTALS(3)/MEFM_RB_MASS(3,3)
         WRITE(CHAR_PCT(3),1001) MODES_PCT(3), '%'
      ELSE                                                 ! Denominator is zero so leave % blank
         CHAR_PCT(3)(1:) = ' '
         IF (MEFM_TOTALS(3) > EPS1) THEN                   ! Error: denominator is zero but numerator is not, so write message
            WRITE(ERR,2003)
            IF (SUPINFO == 'N') THEN
               WRITE(F06,2003)
            ENDIF
         ENDIF
      ENDIF

      IF (DABS(MEFM_RB_MASS(4,4)) > EPS1) THEN
         MODES_PCT(4) = ONE_HUNDRED*MEFM_TOTALS(4)/MEFM_RB_MASS(4,4)
         WRITE(CHAR_PCT(4),1001) MODES_PCT(4), '%'
      ELSE                                                 ! Denominator is zero so leave % blank
         CHAR_PCT(4)(1:) = ' '
         IF (MEFM_TOTALS(4) > EPS1) THEN                   ! Error: denominator is zero but numerator is not, so write message
            WRITE(ERR,2004)
            IF (SUPINFO == 'N') THEN
               WRITE(F06,2004)
            ENDIF
         ENDIF
      ENDIF

      IF (DABS(MEFM_RB_MASS(5,5)) > EPS1) THEN
         MODES_PCT(5) = ONE_HUNDRED*MEFM_TOTALS(5)/MEFM_RB_MASS(5,5)
         WRITE(CHAR_PCT(5),1001) MODES_PCT(5), '%'
      ELSE                                                 ! Denominator is zero so leave % blank
         CHAR_PCT(5)(1:) = ' '
         IF (MEFM_TOTALS(5) > EPS1) THEN                   ! Error: denominator is zero but numerator is not, so write message
            WRITE(ERR,2005)
            IF (SUPINFO == 'N') THEN
               WRITE(F06,2005)
            ENDIF
         ENDIF
      ENDIF

      IF (DABS(MEFM_RB_MASS(6,6)) > EPS1) THEN
         MODES_PCT(6) = ONE_HUNDRED*MEFM_TOTALS(6)/MEFM_RB_MASS(6,6)
         WRITE(CHAR_PCT(6),1001) MODES_PCT(6), '%'
      ELSE                                                 ! Denominator is zero so leave % blank
         CHAR_PCT(6)(1:) = ' '
         IF (MEFM_TOTALS(6) > EPS1) THEN                   ! Error: denominator is zero but numerator is not, so write message
            WRITE(ERR,2006)
            IF (SUPINFO == 'N') THEN
               WRITE(F06,2006)
            ENDIF
         ENDIF
      ENDIF

      WRITE(F06,9118) (CHAR_PCT(I),I=1,6)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  900 FORMAT('--------------------------------------------------------------------------------------------------------------------'&
            ,'----------------')

  909 FORMAT(1X,A)

 1001 FORMAT(F13.2,A)

 2001 FORMAT(' *INFORMATION: CANNOT CALCULATE T1 MODAL EFFECTIVE MASS PERCENT OF MODEL MASS SO IT IS LEFT BLANK')

 2002 FORMAT(' *INFORMATION: CANNOT CALCULATE T2 MODAL EFFECTIVE MASS PERCENT OF MODEL MASS SO IT IS LEFT BLANK')

 2003 FORMAT(' *INFORMATION: CANNOT CALCULATE T3 MODAL EFFECTIVE MASS PERCENT OF MODEL MASS SO IT IS LEFT BLANK')

 2004 FORMAT(' *INFORMATION: CANNOT CALCULATE R1 MODAL EFFECTIVE MASS PERCENT OF MODEL IXX  SO IT IS LEFT BLANK')

 2005 FORMAT(' *INFORMATION: CANNOT CALCULATE R2 MODAL EFFECTIVE MASS PERCENT OF MODEL IYY  SO IT IS LEFT BLANK')

 2006 FORMAT(' *INFORMATION: CANNOT CALCULATE R3 MODAL EFFECTIVE MASS PERCENT OF MODEL IZZ  SO IT IS LEFT BLANK')

 9101 FORMAT(' *ERROR  9101: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' PARAM WTMASS SHOULD NOT BE ZERO BUT IS = ',1ES14.6)

 9102 FORMAT(14X,'                    E F F E C T I V E   M O D A L   M A S S E S   O R   W E I G H T S',/,                        &
             14X,'                                     (in coordinate system ',I8,')',/,                                           &
             14X,'                       Units are same as units for mass input in the Bulk Data Deck')

 9103 FORMAT(14X,'                          Reference point is the basic coordinate system origin',/)

 9104 FORMAT(14X,'                            Reference point is the PARAM GRDPNT grid: ',I8,/)

 9105 FORMAT(14X,'                              Reference point is the model center of gravity',/)

 9106 FORMAT(14X,'                                    Reference point is grid ',I8,/)

 9107 FORMAT(13X,'MODE     CYCLES          T1            T2            T3            R1            R2            R3',/,            &
             13X,' NUM')

 9108 FORMAT(13X,'MODE       CYCLES          T1            T2            T3            R1            R2            R3',/,          &
             13X,' NUM')

 9110 FORMAT(9X,I8,7(1ES14.6))

 9111 FORMAT(9X,I8,7(1ES14.2))

 9112 FORMAT(32X,' ------------  ------------  ------------  ------------  ------------  ------------',/,                          &
             17X,'Sum all modes:',6(1ES14.6))

 9113 FORMAT(32X,'     --------      --------      --------      --------      --------      --------',/,                          &
             17X,'Sum all modes:',6(1ES14.2))

 9116 FORMAT(14X,'Total model mass:',6(1ES14.6))

 9117 FORMAT(14X,'Total model mass:',6(1ES14.2))

 9118 FORMAT(8X,'Modes % of total mass*:',6(A14),//,' *If all modes are calculated the % of total mass should be 100% of the '     &
               ,'free mass (i.e. not counting mass at constrained DOF''s).',/,                                                     &
               '  Percentages are only printed for components that have finite model mass.',/,                                     &
               '                                                               -----')
 9202 FORMAT(21X,'                    E F F E C T I V E   M O D A L   M A S S E S   O R   W E I G H T S',/,                        &
             21X,'                                     (in coordinate system ',I8,')',/,                                           &
             21X,'                       Units are same as units for mass input in the Bulk Data Deck')

 9207 FORMAT(20X,'MODE     CYCLES          T1            T2            T3            R1            R2            R3',/,            &
             20X,' NUM')

 9210 FORMAT(16X,I8,7(1ES14.6))

 9212 FORMAT(39X,' ------------  ------------  ------------  ------------  ------------  ------------',/,                          &
             10X,'Sum all modes:',14X,6(1ES14.6))

 9216 FORMAT(7X,'Total model mass:',14X,6(1ES14.6))

! **********************************************************************************************************************************
 
      END SUBROUTINE WRITE_MEFFMASS
