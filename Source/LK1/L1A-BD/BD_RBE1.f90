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
 
      SUBROUTINE BD_RBE1 ( CARD, LARGE_FLD_INP )
 
! Processes RBE1 Bulk Data Cards. Writes RBE1 element records to file L1F for later processing.
! Two records are written for each dependent Grid/DOF pair:

!       1) Record 1 has the rigid element type: 'RBE1    '
!       2) Record 2 has: 
!             RELID           : Rigid element ID
!             IGID(i),IDOF(i) : Pairs of indep Grid/DOF (up to 6)
!             DGID, DDOF      : One pair of dependent Grid/DOF              
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1F
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LRIGEL, NRBE1, NRIGEL, NRECARD
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_RBE1_BEGEND
      USE MODEL_STUF, ONLY            :  RIGID_ELEM_IDS
 
      USE BD_RBE1_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_RBE1'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: CHR_IDOF(6)
      CHARACTER( 8*BYTE)              :: IP6TYP            ! An output from subr IP6CHK called herein
      CHARACTER(LEN(JCARD))           :: JCARD1_PARENT     ! Field 1 of parent card
      CHARACTER(LEN(JCARD))           :: JCARD2_PARENT     ! Field 2 of parent card
      CHARACTER(LEN(JCARD))           :: JCARDO            ! An output from subr IP6CHK called herein
      CHARACTER( 1*BYTE)              :: MORE_IDOF = 'N'   ! = 'Y' if a cont card with indep DOF's is found
      CHARACTER( 1*BYTE)              :: UMFND             ! = 'Y' when we find "UM" in field 2 of cont card no. 1 or 2
      CHARACTER( 8*BYTE), PARAMETER   :: RTYPE = 'RBE1    '! Rigid element type
 
      INTEGER(LONG)                   :: CONT_NO   = 0     ! Count of the continuation cards for this parent
      INTEGER(LONG)                   :: DGID      = 0     ! A dependent grid
      INTEGER(LONG)                   :: DDOF      = 0     ! Dependent DOF's at DGID
      INTEGER(LONG)                   :: DOF_NOS(6)= 0     ! Fields 5 & 6 should fill this in to be: 1 2 3 4 5 6
      INTEGER(LONG)                   :: IDOF_ERR  = 0     ! Count of the no. of DOF component errors in fields 5,6
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IDOF(6)   = 0     ! Independent DOF's at IGID(i)
      INTEGER(LONG)                   :: IDUM              ! Dummy arg in subr IP^CHK not used herein
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: IGID(6)   = 0     ! Independent grid ID's
      INTEGER(LONG)                   :: INT1      = 0     ! An integer 1-6 read from internal file
      INTEGER(LONG)                   :: JERR      = 0     ! A local error count
      INTEGER(LONG)                   :: JFLD1             ! A computed field number on the card
      INTEGER(LONG)                   :: JFLD2             ! A computed field number on the card
      INTEGER(LONG)                   :: NUM_IDOF_FLDS = 0 ! Number of fields that have independent DOF's specified
      INTEGER(LONG)                   :: RELID     = 0     ! This rigid elements' ID
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_RBE1_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN 
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! RBE1 Bulk Data Card routine
 
!   FIELD   ITEM           
!   -----   ------------   
!    2      RELID  , Rigid Elem ID
!    3      IGID(1), Grid ID for 1st independent grid
!    4      IDOF(1), DOF's at 1st independent grid 
!    5      IGID(2), Grid ID for 2nd independent grid, if it exists
!    6      IDOF(2), DOF's at 2nd independent grid, if IGID(2) exists 
!    7      IGID(3), Grid ID for 3rd independent grid, if it exists
!    8      IDOF(3), DOF's at 3rd independent grid, if IGID(3) exists

! Possible continuation card (if there are 4, 5 or 6 independent grids) 
!   FIELD   ITEM           
!   -----   ------------      
!    3      IGID(4), Grid ID for 4th independent grid, if it exists 
!    4      IDOF(4), DOF's at 4th independent grid, if IGID(4) exists 
!    5      IGID(5), Grid ID for 5th independent grid, if it exists
!    6      IDOF(5), DOF's at 5th independent grid, if IGID(5) exists 
!    7      IGID(6), Grid ID for 6th independent grid, if it exists
!    8      IDOF(6), DOF's at 6th independent grid, if IGID(6) exists
!    The collection of IDOF(i) must yield 6 DOF's that completely  describe a general rigid body motion of the
!    rigid element

! Mandatory continuation card
!   FIELD   ITEM           
!   -----   ------------      
!    2      "UM"
!    3      DGID, Grid ID for 1st dependent grid
!    4      DDOF, DOF's at 1st dependent grid 
!    5-8    Up to 2 more pairs of DGID/DDOF, if they exist, followed by more continuation cards with up to 3 pairs
!           of DGID/DDOF in fields 3-8, if needed
 
! Data is written to file L1F for later processing after checks on 
! format of data.
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      JCARD1_PARENT = JCARD(1)
      JCARD2_PARENT = JCARD(2)
 
! Check for overflow

      NRBE1  = NRBE1+1
      NRIGEL = NRIGEL+1
!xx   IF (NRIGEL > LRIGEL) THEN
!xx      FATAL_ERR = FATAL_ERR + 1
!xx      WRITE(ERR,1163) SUBR_NAME,JCARD(1),LRIGEL
!xx      WRITE(F06,1163) SUBR_NAME,JCARD(1),LRIGEL
!xx      CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
!xx   ENDIF

! Initialize CHR_IDOF
 
      DO I=1,6
         CHR_IDOF(I)(1:) = ' '
      ENDDO 
 
! Read Elem ID
 
      CALL I4FLD ( JCARD(2), JF(2), RELID )
      IF (IERRFL(2) /= 'N') THEN
         JERR = JERR + 1
      ELSE
         RIGID_ELEM_IDS(NRIGEL) = RELID
      ENDIF

! Read up to 3 pairs of independent grids and DOF's in fields 3-8 of the parent card.
! For any of the 3 pairs, if either of the 2 fields is blank, the other one must also be blank. 
 
      NUM_IDOF_FLDS = 0
      DO J=1,3
         JFLD1 = 2*J+1
         JFLD2 = 2*J+2
         IF      ((JCARD(JFLD1)(1:) == ' ') .AND. (JCARD(JFLD2)(1:) == ' ')) THEN
            CYCLE
         ELSE IF ((JCARD(JFLD1)(1:) /= ' ') .AND. (JCARD(JFLD2)(1:) == ' ')) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1181) JCARD2_PARENT,JFLD1,JFLD2 
            WRITE(F06,1181) JCARD2_PARENT,JFLD1,JFLD2 
         ELSE IF ((JCARD(JFLD1)(1:) == ' ') .AND. (JCARD(JFLD2)(1:) /= ' ')) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1182) JCARD2_PARENT,JFLD1,JFLD2 
            WRITE(F06,1182) JCARD2_PARENT,JFLD1,JFLD2 
         ELSE IF ((JCARD(JFLD1)(1:) /= ' ') .AND. (JCARD(JFLD2)(1:) /= ' ')) THEN 
            NUM_IDOF_FLDS = NUM_IDOF_FLDS + 1
            CHR_IDOF(NUM_IDOF_FLDS) = JCARD(JFLD2)
            CALL I4FLD ( JCARD(JFLD1), JF(JFLD1), IGID(NUM_IDOF_FLDS) )
            IF (IERRFL(JFLD1) /= 'N') THEN
               JERR = JERR + 1
            ENDIF
            CALL I4FLD ( JCARD(JFLD2), JF(JFLD2), IDOF(NUM_IDOF_FLDS) )
            IF (IERRFL(JFLD2) == 'N') THEN
               CALL IP6CHK ( JCARD(JFLD2), JCARDO, IP6TYP, IDUM )
               IF ((IP6TYP == 'COMP NOS') .OR. (IP6TYP == 'BLANK   '))  THEN
                  CONTINUE
               ELSE
                  FATAL_ERR  = FATAL_ERR + 1
                  IDOF_ERR = IDOF_ERR + 1 
                  WRITE(ERR,1124) JFLD2,JCARD1_PARENT,JCARD2_PARENT,JFLD2,JCARD(JFLD2)
                  WRITE(F06,1124) JFLD2,JCARD1_PARENT,JCARD2_PARENT,JFLD2,JCARD(JFLD2)
               ENDIF
            ELSE
               JERR = JERR + 1
            ENDIF
         ENDIF
      ENDDO
 
      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,0,5,0,7,0,0 )     ! Make sure no imbedded blanks in fields 2,3,5,7. Flds 4,6,8 ae DOF's
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,0,9 )     ! Issue warning if field 9 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

! Now look for a continuation card. It may have up to 3 additional pairs of independent grid/DOF or it may begin the
! dependent grid/DOF list. If it is the later, then field 2 must have "UM"

      CONT_NO = 0
      UMFND   = 'N'
      DO
         IF (LARGE_FLD_INP == 'N') THEN
            CALL NEXTC  ( CARD, ICONT, IERR )
         ELSE
            CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
            CARD = CHILD
         ENDIF
         CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
         IF (ICONT == 1) THEN
            CONT_NO = CONT_NO + 1
            IF (JCARD(2)(1:2) == 'UM') THEN
               UMFND = 'Y'
            ENDIF
            IF (UMFND =='N') THEN                          ! Read up to 3 additional pairs of indep grid/DOF's
               IF (CONT_NO == 1) THEN
                  MORE_IDOF = 'Y'
                  DO J=1,3
                     JFLD1 = 2*J+1
                     JFLD2 = 2*J+2
                     IF      ((JCARD(JFLD1)(1:) == ' ') .AND. (JCARD(JFLD2)(1:) == ' ')) THEN
                        CYCLE
                     ELSE IF ((JCARD(JFLD1)(1:) /= ' ') .AND. (JCARD(JFLD2)(1:) == ' ')) THEN
                        FATAL_ERR = FATAL_ERR + 1
                        WRITE(ERR,1181) JCARD2_PARENT,JFLD1,JFLD2 
                        WRITE(F06,1181) JCARD2_PARENT,JFLD1,JFLD2 
                     ELSE IF ((JCARD(JFLD1)(1:) == ' ') .AND. (JCARD(JFLD2)(1:) /= ' ')) THEN
                        FATAL_ERR = FATAL_ERR + 1
                        WRITE(ERR,1182) JCARD2_PARENT,JFLD1,JFLD2       
                        WRITE(F06,1182) JCARD2_PARENT,JFLD1,JFLD2 
                     ELSE IF ((JCARD(JFLD1)(1:) /= ' ') .AND. (JCARD(JFLD2)(1:) /= ' ')) THEN 
                        NUM_IDOF_FLDS = NUM_IDOF_FLDS + 1
                        CHR_IDOF(NUM_IDOF_FLDS) = JCARD(JFLD2)
                        CALL I4FLD ( JCARD(JFLD1), JF(JFLD1), IGID(NUM_IDOF_FLDS) )
                        IF (IERRFL(JFLD1) /= 'N') THEN
                           JERR = JERR + 1
                        ENDIF
                        CALL I4FLD ( JCARD(JFLD2), JF(JFLD2), IDOF(NUM_IDOF_FLDS) )
                        IF (IERRFL(JFLD2) == 'N') THEN
                           CALL IP6CHK ( JCARD(JFLD2), JCARDO, IP6TYP, IDUM )
                           IF ((IP6TYP == 'COMP NOS') .OR. (IP6TYP == 'BLANK   '))  THEN
                              CONTINUE
                           ELSE
                              FATAL_ERR  = FATAL_ERR + 1
                              IDOF_ERR = IDOF_ERR + 1 
                              WRITE(ERR,1124) JFLD2,JCARD1_PARENT,JCARD2_PARENT,JFLD2,JCARD(JFLD2)
                              WRITE(F06,1124) JFLD2,JCARD1_PARENT,JCARD2_PARENT,JFLD2,JCARD(JFLD2)
                           ENDIF
                        ELSE
                           JERR = JERR + 1
                        ENDIF
                     ENDIF
                  ENDDO
               ELSE
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1135) JCARD1_PARENT, JCARD2_PARENT, CONT_NO
                  WRITE(F06,1135) JCARD1_PARENT, JCARD2_PARENT, CONT_NO
                  RETURN                                   ! Cannot continue, NUM_IDOF_FLDS cannot exceed 6 (for array CHR_IDOF)
               ENDIF

               CALL BD_IMBEDDED_BLANK ( JCARD,0,3,0,5,0,7,0,0 ) ! Make sure no imbedded blanks in fields 3,5,7 (4,6,8 can be IDOF's)
               CALL CARD_FLDS_NOT_BLANK ( JCARD,2,0,0,0,0,0,0,9 )
               CALL CRDERR ( CARD )                        ! CRDERR prints errors found when reading fields

            ELSE                                           ! Found "UM", so read up to 3 pairs of dep. grid/DOF on this con't card
               DO                                          ! There are any number of continuation cards w/dep. grid/DOF 
                  DO J=1,3
                     JFLD1 = 2*J+1
                     JFLD2 = 2*J+2
                     IF      ((JCARD(JFLD1)(1:) == ' ') .AND. (JCARD(JFLD2)(1:) == ' ')) THEN
                        CYCLE
                     ELSE IF ((JCARD(JFLD1)(1:) /= ' ') .AND. (JCARD(JFLD2)(1:) == ' ')) THEN
                        FATAL_ERR = FATAL_ERR + 1
                        WRITE(ERR,1181) JCARD2_PARENT,JFLD1,JFLD2 
                        WRITE(F06,1181) JCARD2_PARENT,JFLD1,JFLD2 
                     ELSE IF ((JCARD(JFLD1)(1:) == ' ') .AND. (JCARD(JFLD2)(1:) /= ' ')) THEN
                        FATAL_ERR = FATAL_ERR + 1
                        WRITE(ERR,1182) JCARD2_PARENT,JFLD1,JFLD2 
                        WRITE(F06,1182) JCARD2_PARENT,JFLD1,JFLD2 
                     ELSE IF ((JCARD(JFLD1)(1:) /= ' ') .AND. (JCARD(JFLD2)(1:) /= ' ')) THEN 
                        CALL I4FLD ( JCARD(JFLD1), JF(JFLD1), DGID )
                        IF (IERRFL(JFLD1) /= 'N') THEN
                           JERR = JERR + 1
                        ENDIF
                        CALL I4FLD ( JCARD(JFLD2), JF(JFLD2), DDOF )
                        IF (IERRFL(JFLD2) == 'N') THEN
                           CALL IP6CHK ( JCARD(JFLD2), JCARDO, IP6TYP, IDUM )
                           IF ((IP6TYP == 'COMP NOS') .OR. (IP6TYP == 'BLANK   '))  THEN
                              CONTINUE
                           ELSE
                              FATAL_ERR  = FATAL_ERR + 1
                              IDOF_ERR = IDOF_ERR + 1 
                              WRITE(ERR,1124) JFLD2,JCARD1_PARENT,JCARD2_PARENT,JFLD2,JCARD(JFLD2)
                              WRITE(F06,1124) JFLD2,JCARD1_PARENT,JCARD2_PARENT,JFLD2,JCARD(JFLD2)
                           ENDIF
                        ELSE
                           JERR = JERR + 1   
                        ENDIF
                        IF (JERR == 0) THEN
                           WRITE(L1F) RTYPE
                           WRITE(L1F) RELID,DGID,DDOF,NUM_IDOF_FLDS,(IGID(I),IDOF(I),I=1,NUM_IDOF_FLDS)
                           NRECARD = NRECARD + 1
                        ENDIF
                     ENDIF
                  ENDDO

                  CALL BD_IMBEDDED_BLANK ( JCARD,0,3,0,5,0,5,0,0 ) ! Make sure no imbed blanks in fields 3,5,7 (2,4,6 can be IDOF's)
                  CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,0,9 )
                  CALL CRDERR ( CARD )

                  IF (LARGE_FLD_INP == 'N') THEN
                     CALL NEXTC  ( CARD, ICONT, IERR )
                  ELSE
                     CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
                     CARD = CHILD
                  ENDIF
                  CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
                  IF (ICONT == 1) THEN
                     CYCLE
                  ELSE
                     EXIT
                  ENDIF
               ENDDO
            ENDIF         
         ELSE
            EXIT
         ENDIF
      ENDDO

      IF (UMFND == 'N') THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1136) JCARD1_PARENT,JCARD2_PARENT
         WRITE(F06,1136) JCARD1_PARENT,JCARD2_PARENT
      ENDIF

! If IDOF fields all have valid DOF no's (or blank), check that the DOF's in those fields specify all 6 DOF no's

      IF (IDOF_ERR == 0) THEN
         DO I=1,NUM_IDOF_FLDS
            DO J=1,JCARD_LEN
               IF (CHR_IDOF(I)(J:J) /= ' ') THEN 
                  READ (CHR_IDOF(I)(J:J),'(I8)') INT1
                  IF (DOF_NOS(INT1) == 0) THEN
                     DOF_NOS(INT1) = INT1
                  ELSE
                     FATAL_ERR = FATAL_ERR + 1
                     IDOF_ERR  = IDOF_ERR + 1
                  ENDIF
               ENDIF
            ENDDO
         ENDDO 
         DO J=1,6
            IF (DOF_NOS(J) == 0) THEN
               FATAL_ERR = FATAL_ERR + 1
               IDOF_ERR  = IDOF_ERR + 1
            ENDIF
         ENDDO
         IF (IDOF_ERR > 0) THEN
            WRITE(ERR,1143) RELID,(CHR_IDOF(I),I=1,3)
            WRITE(F06,1143) RELID,(CHR_IDOF(I),I=1,3)
            IF (MORE_IDOF == 'N') THEN
               WRITE(ERR,11431)
               WRITE(F06,11431)
            ELSE
               WRITE(ERR,11432) (CHR_IDOF(I),I=4,6)
               WRITE(F06,11432) (CHR_IDOF(I),I=4,6)
            ENDIF
         ENDIF
      ENDIF      

      CALL CRDERR ( CARD )         
 
! **********************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1124 FORMAT(' *ERROR  1124: INVALID DOF NUMBER IN FIELD ',I3,' ON ',A,' ENTRY WITH ID = ',A                                       &
                    ,/,14X,' MUST BE A COMBINATION OF DIGITS 1-6. HOWEVER, FIELD ',I3, ' HAS: "',A,'"')

 1135 FORMAT(' *ERROR  1135: ',A,' ENTRY WITH ID = ',A,' SHOULD HAVE A MAX OF 1 CONTINUATION ENTRY WITHOUT "UM" IN FIELD 2.'       &
                    ,/,14X,' HOWEVER, CONTINUATION ENTRY NUMBER ',I8,' HAS BEEN FOUND')

 1136 FORMAT(' *ERROR  1136: REQUIRED CONTINUATION FOR ',A,' ID = ',A,' MISSING')
 
 1143 FORMAT(' *ERROR  1143: RBE1 ELEM NUMBER ',I8,' HAS INCORRECT INDEPENDENT DOFs IDENTIFIED.'                                   &
                    ,/,14X,' THESE (UP TO) 6 FIELDS MUST COMBINE TO SPECIFY DOFs 1,2,3,4,5,6 (EACH DOF ONCE, AND ONLY ONCE).'      &
                    ,/,14X,' HOWEVER, FIELDS 4,6,8 OF THE PARENT     ENTRY HAD: "',A,'", "',A,'", "',A,'"')

11431 FORMAT(          14X,' AND THERE WAS NO CONTINUATION ENTRY WITH ADDITIONAL INDEPENDENT DOFs IDENTIFIED')
11432 FORMAT(          14X,' AND      FIELDS 4,6,8 OF A CONTINUATION ENTRY HAD: "',A,'", "',A,'", "',A,'"')

 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

 1181 FORMAT(' *ERROR  1181: RBE1 ELEMENT NUMBER ',A,' HAS NON-BLANK GRID FIELD ',I2,' AND BLANK DOF FIELD ',I2                    &
                    ,/,14X,' THIS PAIR OF GRID/DOF MUST BE EITHER BOTH BLANK OR BOTH NON-BLANK')

 1182 FORMAT(' *ERROR  1182: RBE1 ELEMENT NUMBER ',A,' HAS BLANK GRID FIELD ',I2,' AND NON-BLANK DOF FIELD ',I2                    &
                    ,/,14X,' THIS PAIR OF GRID/DOF MUST BE EITHER BOTH BLANK OR BOTH NON-BLANK')

 1183 FORMAT(' *ERROR  1183: RBE1 ELEMENT NUMBER ',A,' HAS NO CONTINUATION ENTRY. AT LEAST 1 IS REQUIRED')

 1184 FORMAT(' *ERROR  1184: RBE1 ELEMENT NUMBER ',A,' HAS ',I8,' INDEPENDENT DOFs DEFINED. REQUIREMENT IS 6')

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_RBE1
