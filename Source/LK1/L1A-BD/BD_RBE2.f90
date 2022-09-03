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
 
      SUBROUTINE BD_RBE2 ( CARD, LARGE_FLD_INP )
 
! Processes RBE2 Bulk Data Cards. Writes RBE2 element records to file L1F for later processing.
! Two records are written for each dependent Grid/DOF pair:

!       1) Record 1 has the rigid element type: 'RBAR    '
!       2) Record 2 has:
!            REID: Rigid element ID
!            DGID: Dependent   Grid ID
!            DDOF: Dependent   DOF's
!            IGID: Independent Grid ID
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1F
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LRIGEL, NRBE2, NRIGEL, NRECARD, NTERM_RMG
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_RBE2_BEGEND
      USE MODEL_STUF, ONLY            :  RIGID_ELEM_IDS
 
      USE BD_RBE2_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_RBE2'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER( 8*BYTE)              :: IP6TYP            ! An output from subr IP6CHK called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARDO            ! An output from subr IP6CHK called herein
      CHARACTER( 8*BYTE), PARAMETER   :: RTYPE = 'RBE2    '! Rigid element type
 
      INTEGER(LONG)                   :: DDOF      = 0     ! Dependent DOF's at DGID's
      INTEGER(LONG)                   :: DGID      = 0     ! Dependent grid ID's
      INTEGER(LONG)                   :: J                 ! DO loop indiex
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IDUM              ! Dummy arg in subr IP^CHK not used herein
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: IGID      = 0     ! Independent grid ID
      INTEGER(LONG)                   :: JERR      = 0     ! A local error count
      INTEGER(LONG)                   :: NUM_COMP  = 0     ! Total number of components specified in DDOF
      INTEGER(LONG)                   :: RELID      = 0     ! This elements' ID
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_RBE2_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! RBE2 Bulk Data Card routine
 
!   FIELD   ITEM           
!   -----   ------------   
!    2      RELID, Rigid Elem ID
!    3      IGID, Independent Grid ID
!    4      DDOF, Dependent DOF's for the Grids following
!   5-9     DGID, Dependent Grid ID's
   
! on optional continuation cards:
!   2-9     DGID, Dependent grid ID's
 
! Data is written to file L1F for later processing after checks on format of data.
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Check for overflow

      NRBE2  = NRBE2+1
      NRIGEL = NRIGEL + 1
!xx   IF (NRIGEL > LRIGEL) THEN
!xx      FATAL_ERR = FATAL_ERR + 1
!xx      WRITE(ERR,1163) SUBR_NAME,JCARD(1),LRIGEL
!xx      WRITE(F06,1163) SUBR_NAME,JCARD(1),LRIGEL
!xx      CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
!xx   ENDIF

! Read and check data
 
      CALL I4FLD ( JCARD(2), JF(2), RELID )                 ! Read Elem ID
      IF (IERRFL(2) /= 'N') THEN
         JERR = JERR + 1
      ELSE
         RIGID_ELEM_IDS(NRIGEL) = RELID
      ENDIF

      CALL I4FLD ( JCARD(3), JF(3), IGID )                 ! Read independent Grid ID
      IF (IERRFL(3) /= 'N') THEN
         JERR = JERR + 1
      ENDIF
      
      NUM_COMP = 0                                         ! Get dependent DOF's in field 4 and count their number (NUM_COMP)
      CALL I4FLD ( JCARD(4), JF(4), DDOF )
      IF (IERRFL(4) == 'N') THEN
         CALL IP6CHK ( JCARD(4), JCARDO, IP6TYP, IDUM )
         IF (IP6TYP == 'COMP NOS') THEN
            DO J=1,JCARD_LEN
               IF(JCARD(4)(J:J) /= ' ') THEN
                 NUM_COMP = NUM_COMP + 1
               ENDIF
            ENDDO
         ELSE
            JERR      = JERR + 1
            FATAL_ERR = FATAL_ERR + 1 
            WRITE(ERR,1124) JF(4),JCARD(1),JCARD(2),JF(4),JCARD(4)
            WRITE(F06,1124) JF(4),JCARD(1),JCARD(2),JF(4),JCARD(4)
         ENDIF
      ENDIF
 
      DO J=5,9
         IF (JCARD(J)(1:) == ' ') THEN
            CYCLE
         ELSE            
            CALL I4FLD ( JCARD(J), JF(J), DGID )           ! Get dep. grid ID's in fields 5 - 9
            IF ((IERRFL(J) == 'N') .AND. (JERR == 0)) THEN
               WRITE(L1F) RTYPE                            ! Write element type to LINK1F
               WRITE(L1F) RELID,DGID,DDOF,IGID              ! Write data to LINK1F, one record per dependent Grid
               NRECARD = NRECARD + 1
               NTERM_RMG = NTERM_RMG + 7*NUM_COMP
            ENDIF
         ENDIF
      ENDDO 
 
      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,0,5,6,7,8,9 )     ! Make sure that there are no imbedded blanks in fields 2-9,except 4(DOF)
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields         
 
! Read and check data on optional continuation cards with additional dep. grids
 
      DO 
         IF (LARGE_FLD_INP == 'N') THEN
            CALL NEXTC  ( CARD, ICONT, IERR )
         ELSE
            CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
            CARD = CHILD
         ENDIF
         CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
         IF (ICONT == 1) THEN
            DO J=2,9
               IF (JCARD(J)(1:) == ' ') THEN
                  CYCLE
               ELSE            
                  CALL I4FLD ( JCARD(J), JF(J), DGID )     ! Get dep. grid ID's in fields 5 - 9
                  IF ((IERRFL(J) == 'N') .AND. (JERR == 0)) THEN
                     WRITE(L1F) RTYPE                      ! Write element type to LINK1F
                     WRITE(L1F) RELID,DGID,DDOF,IGID        ! Write data to LINK1F, one record per dependent Grid
                     NRECARD = NRECARD + 1
                     NTERM_RMG = NTERM_RMG + 7*NUM_COMP
                  ENDIF
               ENDIF
            ENDDO
    
            CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 ) ! Make sure that there are no imbedded blanks in fields 2-9
            CALL CRDERR ( CARD )                           ! CRDERR prints errors found when reading fields

            CYCLE
         ELSE
            EXIT
         ENDIF
      ENDDO 
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1124 FORMAT(' *ERROR  1124: INVALID DOF NUMBER IN FIELD ',I3,' ON ',A,' ENTRY WITH ID = ',A                                       &
                    ,/,14X,' MUST BE A COMBINATION OF DIGITS 1-6. HOWEVER, FIELD ',I3, ' HAS: "',A,'"')


 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_RBE2
