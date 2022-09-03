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
  
      SUBROUTINE BD_RFORCE ( CARD, LARGE_FLD_INP, CC_LOAD_FND )
  
! Processes RFORCE Bulk Data Cards.  Also, the set ID is written to array RFORCE_SIDS which is checked in subroutine
! LOADB to make sure that all set ID's requested in Case Control were found in the Bulk Data.
! A record is written to file LINK1U for each RFORCE Bulk Data card with the following data:

!   SETID, CID, ACCEL(1-6)
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1U
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LRFORCE, LSUB, NRFORCE, NSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_RFORCE_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  RFORCE_SIDS, SUBLOD
 
      USE BD_RFORCE_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_RFORCE'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD               ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)          ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: NAME          ! The character name of this Bulk Data entry
      CHARACTER( 1*BYTE),INTENT(INOUT):: CC_LOAD_FND(LSUB,2)! 'Y' if B.D load/temp card w/ same set ID (SID) as C.C. LOAD = SID
 
      INTEGER(LONG)                   :: CID        = 0     ! Coord ID on the RFORCE card
      INTEGER(LONG)                   :: CONT_COUNT = 0     ! Count of number of continuation entries
      INTEGER(LONG)                   :: I                  ! DO loop index
      INTEGER(LONG)                   :: ICONT      = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR       = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: GID        = 0     ! Grid ID (or 0) of the grid that the rotational grav accels refer to
      INTEGER(LONG)                   :: JERR       = 0     ! A local error count
      INTEGER(LONG)                   :: SETID      = 0     ! Set ID on the RFORCE card
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_RFORCE_BEGEND
  
      REAL(DOUBLE)                    :: R8INP              ! A real value read from RFORCE entry
      REAL(DOUBLE)                    :: SCALEF_AA  = ZERO  ! Scale factor for angular accel    on the RFORCE card
      REAL(DOUBLE)                    :: SCALEF_AV  = ZERO  ! Scale factor for angular velocity on the RFORCE card
      REAL(DOUBLE)                    :: VEC(3)             ! Vector components of the angular vel and/or accel on the RFORCE entry
 
      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! RFORCE Bulk Data Card routine
 
!   FIELD   ITEM                                        ARRAY ELEMENT
!   -----   ------------                                -------------
!    2      Load set ID                                 RFORCE_SIDS(nrforce)
!    3      Grid pt about which the rotation occurs     GID
!    4      Cord. ID that rotation vector is defined in CID
!    5      Scale factor for angular velocity           SCALEF_AV
!    6-8    Vector comps                                VEC(1-3)

! on optional second card:
!    2      Scale factor for angular acceleration       SCALEF_AA
 
! Initialize variables

      DO I=1,3
         VEC(I)   = ZERO
      ENDDO

! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      NAME = JCARD(1)
 
! Check for overflow

      NRFORCE = NRFORCE+1
!xx   IF (NRFORCE > LRFORCE) THEN
!xx      FATAL_ERR = FATAL_ERR + 1
!xx      WRITE(ERR,1163) SUBR_NAME,JCARD(1),LRFORCE
!xx      WRITE(F06,1163) SUBR_NAME,JCARD(1),LRFORCE
!xx      CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
!xx   ENDIF

! Read and check data
 
      CALL I4FLD ( JCARD(2), JF(2), SETID )
      IF (IERRFL(2) == 'N') THEN
         DO I=1,NSUB
            IF (SETID == SUBLOD(I,1)) THEN
               CC_LOAD_FND(I,1) = 'Y'
            ENDIF
         ENDDO   
         RFORCE_SIDS(NRFORCE) = SETID
      ENDIF
 
      CALL I4FLD ( JCARD(3), JF(3), GID )
      CALL I4FLD ( JCARD(4), JF(4), CID )
      CALL R8FLD ( JCARD(5), JF(5), SCALEF_AV )
      CALL R8FLD ( JCARD(6), JF(6), VEC(1) )
      CALL R8FLD ( JCARD(7), JF(7), VEC(2) )
      CALL R8FLD ( JCARD(8), JF(8), VEC(3) )
 
      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,0 )     ! Make sure that there are no imbedded blanks in fields 2-8
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,0,9 )   ! Issue warning if field 9 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
 
      DO I=2,8
         IF (IERRFL(I) == 'Y') THEN
            JERR = JERR + 1
         ENDIF
      ENDDO
       
! Optional Second Card:

      IF (LARGE_FLD_INP == 'N') THEN
         CALL NEXTC  ( CARD, ICONT, IERR )
      ELSE
         CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
         CARD = CHILD
      ENDIF
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      IF (ICONT == 1) THEN

         CONT_COUNT = 1
         CALL R8FLD ( JCARD(2), JF(2), R8INP )             ! Read grid ID
         IF (IERRFL(2) == 'N') THEN
            IF (R8INP >= 0) THEN
               SCALEF_AA = R8INP
            ELSE
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1194) NAME,JF(2),CONT_COUNT,JCARD(2)
               WRITE(F06,1194) NAME,JF(2),CONT_COUNT,JCARD(2)
            ENDIF
         ENDIF
         
         CALL BD_IMBEDDED_BLANK ( JCARD,2,0,0,0,0,0,0,0 )  ! Make sure that there are no imbedded blanks in fields 2-5
         CALL CARD_FLDS_NOT_BLANK ( JCARD,0,3,4,5,6,7,8,9 )! Issue warning if fields 6-9 not blank
         CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields

         IF (IERRFL(2) == 'Y') THEN
            JERR = JERR + 1
         ENDIF

      ENDIF

! Write data to file LINK1U if there were no errors

      IF (JERR == 0) THEN
         WRITE(L1U) SETID, CID, GID, SCALEF_AV, SCALEF_AA, (VEC(I),I=1,3)
      ENDIF
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

 1194 FORMAT(' *ERROR  1194: NEGATIVE GRID ID NOT ALLOWED ON ',A,' ENTRY. VALUE IN FIELD ',I3,' OF CONTINUATION ENTRY NUMBER '     &
                            ,I3,' IS = ',A)

 ! *********************************************************************************************************************************
 
      END SUBROUTINE BD_RFORCE
