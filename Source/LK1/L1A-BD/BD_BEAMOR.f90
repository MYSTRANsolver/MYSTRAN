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
 
      SUBROUTINE BD_BEAMOR ( CARD )
 
! Processes BEAMOR Bulk Data Cards. Reads and checks the property ID, if present, and the V vector,
! if present. The BEAMOR V vector type (BEAMOR_VVEC_TYPE) was determined in subr BEAMOR0
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LVVEC, NBEAMOR
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  EPSIL
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_BEAMOR_BEGEND
      USE MODEL_STUF, ONLY            :  BEAMOR_VVEC_TYPE, BEAMOR_G0, BEAMOR_VV, BEAMOR_PID
 
      USE BD_BEAMOR_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_BEAMOR'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: I4INP             ! A value read from input file that should be an integer value
      INTEGER(LONG)                   :: PGM_ERR   = 0     ! A  count of the number of coding errors
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_BEAMOR_BEGEND
 
      REAL(DOUBLE)                    :: EPS1              ! A small value to compare zero to
      REAL(DOUBLE)                    :: R8INP             ! A value read from input file that should be a real value

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! BEAMOR Bulk Data Card routine
  
!   FIELD   ITEM         
!   -----   ------------ 
!    3      Property ID  
!    6-8    V-Vector     
! 
      EPS1 = EPSIL(1)

! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Read and check data

      IF (JCARD(3)(1:) /= ' ') THEN                        ! Read prop ID
         CALL I4FLD ( JCARD(3), JF(3), I4INP )
         IF (IERRFL(3) == 'N') THEN
            IF (I4INP /= BEAMOR_PID) THEN
               PGM_ERR = PGM_ERR + 1                       ! Coding error: This value doesn't agree with that read in LOADB0
               WRITE(ERR,11851) SUBR_NAME, JF(3), JCARD(1), BEAMOR_PID, I4INP  
               WRITE(F06,11851) SUBR_NAME, JF(3), JCARD(1), BEAMOR_PID, I4INP 
            ENDIF
            IF (I4INP <= 0) THEN
               FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1192) JF(3), JCARD(1), JCARD(2), ' > 0 ', I4INP
            WRITE(F06,1192) JF(3), JCARD(1), JCARD(2), ' > 0 ', I4INP
            ENDIF
         ENDIF
      ENDIF

      IF (BEAMOR_VVEC_TYPE == 'VECTOR   ') THEN             ! Get V vec or grid no. Subr BEAMOR0 determined BEAMOR_VVEC_TYPE
         LVVEC = LVVEC + 1
         DO J=1,3
            IF (JCARD(J+5)(1:) /= ' ') THEN
               CALL R8FLD ( JCARD(J+5), JF(J+5), R8INP )
               IF (IERRFL(J+5) == 'N') THEN
                  IF (DABS(R8INP - BEAMOR_VV(J)) > EPS1) THEN
                     PGM_ERR = PGM_ERR + 1                 ! Coding error: This value doesn't agree with that read in LOADB0
                     WRITE(ERR,11852) SUBR_NAME, JF(J+5), JCARD(1), BEAMOR_VV(J), R8INP  
                     WRITE(F06,11852) SUBR_NAME, JF(J+5), JCARD(1), BEAMOR_VV(J), R8INP 
                  ENDIF
               ENDIF
            ENDIF 
         ENDDO
      ELSE IF (BEAMOR_VVEC_TYPE == 'GRID     ') THEN
         CALL I4FLD ( JCARD(6), JF(6), I4INP )
         IF (IERRFL(6) == 'N') THEN
            IF (I4INP /= BEAMOR_G0) THEN
               PGM_ERR = PGM_ERR + 1                       ! Coding error: This value doesn't agree with that read in LOADB0
               WRITE(ERR,11851) SUBR_NAME, JF(6), JCARD(1), BEAMOR_G0, I4INP  
               WRITE(F06,11851) SUBR_NAME, JF(6), JCARD(1), BEAMOR_G0, I4INP 
            ENDIF
            IF (I4INP <= 0) THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1192) JF(3), JCARD(1), JCARD(2), ' > 0 ', I4INP
               WRITE(F06,1192) JF(3), JCARD(1), JCARD(2), ' > 0 ', I4INP
            ENDIF
         ENDIF
      ELSE IF (BEAMOR_VVEC_TYPE == 'ERROR    ') THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1102)
         WRITE(F06,1102)
      ENDIF
 
      CALL CARD_FLDS_NOT_BLANK ( JCARD,2,0,4,5,0,0,0,9 )   ! Issue warning if fields 2, 4, 5, 9 are not blank
      CALL BD_IMBEDDED_BLANK ( JCARD,0,3,0,0,6,7,8,0 )     ! Make sure that there are no imbedded blanks in fields 3, 6-8
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

      IF (PGM_ERR > 0) THEN                                ! PGM_ERR /= 0 is a coding error, so quit
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1102 FORMAT(' *ERROR  1102: ERROR IN SPECIFYING V VECTOR ON BEAMOR CARD. EITHER FIELD 6 MUST BE A POSITIVE INTEGER GRID POINT'    &
                    ,/,14X,' OR FIELDS 6, 7, 8 MUST CONTAIN REAL VECTOR COMPONENTS')

11851 FORMAT(' *ERROR  1185: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' VALUE READ FROM FIELD ',I3,' ON ',A,' CARD IN THIS SUBROUTINE'                                        &
                    ,/,14X,' DOES NOT AGREE WITH THAT READ FROM ORIGINAL BULK DATA DECK SCAN.'                                     &
                    ,/,14X,' VALUES ARE: ',I8,' (ORIGINAL BULK DATA SCAN),'                                                        &
                    ,/,14X,'        AND: ',I8,' (HERE)')

11852 FORMAT(' *ERROR  1185: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' VALUE READ FROM FIELD ',I3,' ON ',A,' CARD IN THIS SUBROUTINE'                                        &
                    ,/,14X,' DOES NOT AGREE WITH THAT READ FROM ORIGINAL BULK DATA DECK SCAN.'                                     &
                    ,/,14X,' VALUES ARE: ',1ES13.6,' (ORIGINAL BULK DATA SCAN),'                                                   &
                    ,/,14X,'        AND: ',1ES13.6,' (HERE)')

 1192 FORMAT(' *ERROR  1192: ID IN FIELD ',I3,' OF ',A,A,' MUST BE ',A,' BUT IS = ',I8)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_BEAMOR
