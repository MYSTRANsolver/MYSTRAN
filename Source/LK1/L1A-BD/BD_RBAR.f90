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
 
      SUBROUTINE BD_RBAR ( CARD )
 
! Processes RBAR Bulk Data Cards. Writes RBAR element records to file L1F for later processing.
! Two records are written for each dependent Grid/DOF pair:

!       1) Record 1 has the rigid element type: 'RBAR    '
!       2) Record 2 has:
!            RELID: Rigid element ID
!            GID1 : 1st Grid ID
!            IDOF1: Independent DOF's at GID1
!            DDOF1: Dependent   DOF's at GID1
!            GID2 : 2nd Grid ID
!            IDOF2: Independent DOF's at GID2
!            DDOF2: Dependent   DOF's at GID2
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1F
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LRIGEL, NRBAR, NRIGEL, NRECARD
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_RBAR_BEGEND
      USE MODEL_STUF, ONLY            :  RIGID_ELEM_IDS
 
      USE BD_RBAR_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_RBAR'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER( 8*BYTE)              :: IP6TYP(5:8)       ! An output from subr IP6CHK called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARDO            ! An output from subr IP6CHK called herein
      CHARACTER( 8*BYTE), PARAMETER   :: RTYPE = 'RBAR    '! Rigid element type
 
      INTEGER(LONG)                   :: DOF_NOS(6)= 0     ! Fields 5 & 6 should fill this in to be: 1 2 3 4 5 6
      INTEGER(LONG)                   :: GID1      = 0     ! Grid ID for 1st RBAR grid
      INTEGER(LONG)                   :: GID2      = 0     ! Grid ID for 2nd RBAR grid
      INTEGER(LONG)                   :: INT1      = 0     ! An integer 1-6 read from internal file
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: JERR      = 0     ! A local error count
      INTEGER(LONG)                   :: IDOF_ERR  = 0     ! Count of the no. of DOF component errors in fields 5,6
      INTEGER(LONG)                   :: IDUM              ! Dummy arg in subr IP^CHK not used herein
      INTEGER(LONG)                   :: RBDOF(4)          ! The DOF's in fields 5,6,7,8
      INTEGER(LONG)                   :: RELID     = 0     ! Rigid element ID
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_RBAR_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! RBAR Bulk Data Card routine
 
!   FIELD   ITEM           
!   -----   ------------   
!    2      RELID, Rigid Elem ID
!    3      GID1 , Grid ID for 1st RBAR grid
!    4      GID2 , Grid ID for 2nd RBAR grid
!    5      IDOF1, Indep. DOF's at GID1 
!    6      IDOF2, Indep. DOF's at GID2 
!    7      DDOF1, Depen. DOF's at GID1 
!    8      DDOF2, Depen. DOF's at GID2 
   
 
! Data is written to file LINK1F for later processing after checks on format of data.
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Check for overflow

      NRBAR  = NRBAR+1
      NRIGEL = NRIGEL + 1
!xx   IF (NRIGEL > LRIGEL) THEN
!xx      FATAL_ERR = FATAL_ERR + 1
!xx      WRITE(ERR,1163) SUBR_NAME,JCARD(1),LRIGEL
!xx      WRITE(F06,1163) SUBR_NAME,JCARD(1),LRIGEL
!xx      CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
!xx   ENDIF

! Read and check data
 
      CALL I4FLD ( JCARD(2), JF(2), RELID )                 ! Read rigid element ID in field 2 
      IF (IERRFL(2) /= 'N') THEN
         JERR = JERR + 1
      ELSE
         RIGID_ELEM_IDS(NRIGEL) = RELID
      ENDIF
 
      CALL I4FLD ( JCARD(3), JF(3), GID1 )                 ! Read 1st grid in field 3
      IF (IERRFL(3) /= 'N') THEN
         JERR = JERR + 1
      ENDIF

      CALL I4FLD ( JCARD(4), JF(4), GID2 )                 ! Read 2nd grid in field 4
      IF (IERRFL(4) /= 'N') THEN
         JERR = JERR + 1
      ENDIF
      
      DO J=5,6                                             ! Read independent DOF's in fields 5, 6
         CALL I4FLD ( JCARD(J), JF(J), RBDOF(J-4) )
         IF (IERRFL(J) == 'N') THEN
            CALL IP6CHK ( JCARD(J), JCARDO, IP6TYP(J), IDUM )
            IF ((IP6TYP(J) == 'COMP NOS') .OR. (IP6TYP(J) == 'BLANK   ')) THEN
               CONTINUE
            ELSE
               FATAL_ERR  = FATAL_ERR + 1 
               IDOF_ERR   = IDOF_ERR + 1
               WRITE(ERR,1124) J,JCARD(1),JCARD(2),J,JCARD(J)
               WRITE(F06,1124) J,JCARD(1),JCARD(2),J,JCARD(J)
            ENDIF
         ELSE
            IDOF_ERR = IDOF_ERR + 1
         ENDIF
      ENDDO
 
      DO J=7,8                                             ! Read dependent DOF's in fields 7, 8
         CALL I4FLD ( JCARD(J), JF(J), RBDOF(J-4) )
         IF (IERRFL(J) == 'N') THEN
            CALL IP6CHK ( JCARD(J), JCARDO, IP6TYP(J), IDUM )
            IF ((IP6TYP(J) == 'COMP NOS') .OR. (IP6TYP(J) == 'BLANK   ')) THEN
               CONTINUE
            ELSE
               FATAL_ERR  = FATAL_ERR + 1 
               JERR       = JERR + 1
               WRITE(ERR,1124) J,JCARD(1),JCARD(2),J,JCARD(J)
               WRITE(F06,1124) J,JCARD(1),JCARD(2),J,JCARD(J)
            ENDIF
         ELSE
            JERR = JERR + 1
         ENDIF
      ENDDO
 
      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,0,0,0,0,0 )     ! Make sure that there are no imb blanks in fields 2-4. Flds 5-8 DOF's
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,0,9 )   ! Issue warning if field 9 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
 
! If IDOF fields 5,6 have valid DOF no's (or blank), check that the DOF's in those fields specify all 6 DOF no's

      IF (IDOF_ERR == 0) THEN
         DO J=1,JCARD_LEN
            IF (JCARD(5)(J:J) /= ' ') THEN 
               READ (JCARD(5)(J:J),'(I8)') INT1
               IF (DOF_NOS(INT1) == 0) THEN
                  DOF_NOS(INT1) = INT1
               ELSE
                  FATAL_ERR = FATAL_ERR + 1
                  IDOF_ERR  = IDOF_ERR + 1
               ENDIF
            ENDIF
         ENDDO
         DO J=1,JCARD_LEN
            IF (JCARD(6)(J:J) /= ' ') THEN 
               READ (JCARD(6)(J:J),'(I8)') INT1
               IF (DOF_NOS(INT1) == 0) THEN
                  DOF_NOS(INT1) = INT1
               ELSE
                  FATAL_ERR = FATAL_ERR + 1
                  IDOF_ERR  = IDOF_ERR + 1
               ENDIF
            ENDIF
         ENDDO 
         DO J=1,6
            IF (DOF_NOS(J) == 0) THEN
               FATAL_ERR = FATAL_ERR + 1
               IDOF_ERR  = IDOF_ERR + 1
            ENDIF
         ENDDO
         IF (IDOF_ERR > 0) THEN
            WRITE(ERR,1142) RELID,JCARD(5),JCARD(6)
            WRITE(F06,1142) RELID,JCARD(5),JCARD(6)
         ENDIF
      ENDIF

! Write data to file L1F if JERR and IDOF_ERR = 0 

      IF ((JERR == 0) .AND. (IDOF_ERR == 0)) THEN
         WRITE(L1F) RTYPE
         WRITE(L1F) RELID,GID1,RBDOF(1),RBDOF(3),GID2,RBDOF(2),RBDOF(4)
         NRECARD = NRECARD + 1
      ENDIF

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

 1142 FORMAT(' *ERROR  1142: RBAR ELEM NUMBER ',I8,' HAS INCORRECT INDEP. DOFs IN FIELDS 5 AND/OR 6.'                              &
                    ,/,14X,' THESE 2 FIELDS MUST COMBINE TO SPECIFY DOFs 1,2,3,4,5,6 (EACH DOF ONCE, AND ONLY ONCE).'              &
                    ,/,14X,' HOWEVER, FIELD 5 WAS "',A,'", AND FIELD 6 WAS "',A,'"') 

 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_RBAR
