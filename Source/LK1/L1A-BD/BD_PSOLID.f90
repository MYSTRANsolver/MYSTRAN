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
  
      SUBROUTINE BD_PSOLID ( CARD, IOR3D )
  
! Processes PSOLID Bulk Data Cards. Reads and checks:

!  1) Property ID and material ID and enter them into array PSOLID
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ECHO, FATAL_ERR, IERRFL, JCARD_LEN, JF, LPSOLID, NPSOLID, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PSOLID_BEGEND
      USE MODEL_STUF, ONLY            :  PSOLID

      USE BD_PSOLID_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_PSOLID'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: CHR_FLD           ! Character data from 1 card field that has been left adjusted
 
      INTEGER(LONG), INTENT(OUT)      :: IOR3D             ! Integration order for this PSOLID entry
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: ID        = 0     ! An integer ID read from a field of this card
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PSOLID_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! PSOLID Bulk Data Card routine
 
!   FIELD   ITEM            ARRAY ELEMENT
!   -----   ------------    -------------
!    2      Prop ID          PSOLID(npsolid,1)
!    3      Mat ID           PSOLID(npsolid,2)
!    4      Matl coord sys   PSOLID(npsolid,3)
!    5      Int order        PSOLID(npsolid,4)
!    6      Stress locations PSOLID(npsolid,5) ***** Not currently used *****
!    7      Int scheme       PSOLID(npsolid,6)
 
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Check for overflow

      NPSOLID = NPSOLID+1
!xx   IF (NPSOLID > LPSOLID) THEN
!xx      FATAL_ERR = FATAL_ERR + 1
!xx      WRITE(ERR,1163) SUBR_NAME,JCARD(1),LPSOLID
!xx      WRITE(F06,1163) SUBR_NAME,JCARD(1),LPSOLID
!xx      CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
!xx   ENDIF
 
! Read and check data on parent card

      PSOLID(NPSOLID,1) = 0
      CALL I4FLD ( JCARD(2), JF(2), ID )                   ! Read property ID and enter into array PSOLID
      IF (IERRFL(2) == 'N') THEN
         DO J=1,NPSOLID-1
            IF (ID == PSOLID(J,1)) THEN                    ! Make sure that this is a unique PSOLID entry
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1145) JCARD(1),ID
               WRITE(F06,1145) JCARD(1),ID
               EXIT
            ENDIF
         ENDDO   
         PSOLID(NPSOLID,1) = ID
      ENDIF
 
      PSOLID(NPSOLID,2) = 0
      CALL I4FLD ( JCARD(3), JF(3), ID )                   ! Read material ID and enter into array PSOLID
      IF (IERRFL(3) == 'N') THEN
         IF (ID <= 0) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1192) JF(3), JCARD(1), JCARD(2), ' > 0 ', ID
            WRITE(F06,1192) JF(3), JCARD(1), JCARD(2), ' > 0 ', ID
         ELSE
            PSOLID(NPSOLID,2) = ID
         ENDIF
      ENDIF

      PSOLID(NPSOLID,3) = -1                               ! Mat'l coord sys ID default value = -1 to distinguish from CID > or = 0
      IF (JCARD(4)(1:) /= ' ') THEN                        ! Read material coord system ID and enter into array PSOLID
         CALL I4FLD ( JCARD(4), JF(4), ID )
         IF (IERRFL(4) == 'N') THEN
            IF (ID < -1) THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1192) JF(4), JCARD(1), JCARD(2), ' >= -1 ', ID
               WRITE(F06,1192) JF(4), JCARD(1), JCARD(2), ' >= -1 ', ID
            ELSE
               PSOLID(NPSOLID,3) = ID
            ENDIF
         ENDIF
      ENDIF

      PSOLID(NPSOLID,4) = 0                                ! Read integration order and enter into array PSOLID
      CHR_FLD = JCARD(5)
      CALL LEFT_ADJ_BDFLD ( CHR_FLD )
      IF       (CHR_FLD(1:) == ' ') THEN                   ! Don't allow actual 0
         PSOLID(NPSOLID,4) = 0                             ! Temporarily set  int order to 0. Subr ELMDAT1 will change this
      ELSE IF ((CHR_FLD(1:8) == 'TWO     ') .OR. (CHR_FLD(1:8) == '2       ')) THEN
         PSOLID(NPSOLID,4) = 2
      ELSE IF ((CHR_FLD(1:8) == 'THREE   ') .OR. (CHR_FLD(1:8) == '3       ')) THEN
         PSOLID(NPSOLID,4) = 3
      ELSE
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1104) JCARD(5),JF(5),JCARD(1),JCARD(2)
         WRITE(F06,1104) JCARD(5),JF(5),JCARD(1),JCARD(2)
      ENDIF
      IOR3D = PSOLID(NPSOLID,4)                            ! Return this    integration order to calling subr

      PSOLID(NPSOLID,5) = 0                                ! This feature - stress location definition, not currently used)

      PSOLID(NPSOLID,6) = 0                                ! Read integration scheme and enter into array PSOLID
      CHR_FLD = JCARD(7)
      CALL LEFT_ADJ_BDFLD ( CHR_FLD )
      IF      ((CHR_FLD(1:8) == 'REDUCED ') .OR. (CHR_FLD(1:8) == '        ') .OR. (CHR_FLD(1:8) == '0       ')) THEN
         ID = 0
      ELSE IF ((CHR_FLD(1:8) == 'FULL    ') .OR. (CHR_FLD(1:8) == '1       ')) THEN
         ID = 1
      ELSE
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1104) JCARD(7),JF(7),JCARD(1),JCARD(2)
         WRITE(F06,1104) JCARD(7),JF(7),JCARD(1),JCARD(2)
      ENDIF
      PSOLID(NPSOLID,6) = ID

! Make sure that the entries for integrtion order and scheme make sense

      IF ((PSOLID(NPSOLID,4) == 0) .AND. (PSOLID(NPSOLID,6) /= 0)) THEN
         PSOLID(NPSOLID,6) = 0
         WARN_ERR = WARN_ERR + 1
         WRITE(ERR,101) CARD
         WRITE(ERR,1107) JCARD(2)
         IF (SUPWARN == 'N') THEN
            IF (ECHO == 'NONE  ') THEN
               WRITE(F06,101) CARD
            ENDIF
            WRITE(F06,1107) JCARD(2)
         ENDIF
      ENDIF

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,0,7,0,0 )     ! Make sure that there are no imbedded blanks in fields 2-5,7
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,6,0,8,9 )   ! Issue warning if fields 6, 8 and 9 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(A)

 1104 FORMAT(' *ERROR  1104: INVALID ENTRY = "',A,'" IN FIELD ',I3,' OF ',A,' ENTRY WITH ID = ',A)

 1107 FORMAT(' *WARNING    : DATA IN FIELDS 5 AND 7 OF PSOLID ENTRY WITH ID = ',A,' ARE INCONSISTENT. DEFAULTS WILL BE USED')

 1145 FORMAT(' *ERROR  1145: DUPLICATE ',A,' ENTRY WITH ID = ',I8)
 
 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

 1192 FORMAT(' *ERROR  1192: ID IN FIELD ',I3,' OF ',A,A,' MUST BE ',A,' BUT IS = ',I8)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_PSOLID
