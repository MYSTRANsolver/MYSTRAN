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
  
      SUBROUTINE BD_PBUSH ( CARD, LARGE_FLD_INP )
  
! Processes PBUSH Bulk Data Cards. Reads and checks:

!  1) Prop ID and Material ID and enter into array PBUSH
!  2) Area, moments of inertia, torsional constant ans nonstructural mass and enter into array RPBUSH
!  3) From 1st continuation card (if present): coords of 4 points for stress recovery and enter into array RPBUSH
!  4) From 2nd continuation card (if present): area factors for transverse shear and I12 and enter into array RPBUSH
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE PARAMS, ONLY                :  EPSIL
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LPBUSH, NPBUSH, WARN_ERR
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PBUSH_BEGEND
      USE MODEL_STUF, ONLY            :  PBUSH, RPBUSH
 
      USE BD_PBUSH_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME   =   'BD_PBUSH'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(1*BYTE)               :: FOUND_RCV = 'N'   ! 'Y' if RCV continuation entry is present
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG)                   :: I4INP             ! An integer value read from a field on this BD entry
      INTEGER(LONG)                   :: ICONT       = 0   ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR        = 0   ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: NUM_ENTRIES       ! Num of quantities to read depending on field 3 of parent or cont entry
      INTEGER(LONG)                   :: OFFSET            ! Array index offset
      INTEGER(LONG)                   :: PROPERTY_ID = 0   ! Property ID (field 2 of this property card)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PBUSH_BEGEND
 
      REAL(DOUBLE)                    :: R8INP             ! A real value read from a field on this BD entry

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! PBUSH Bulk Data Card routine
 
!   FIELD   ITEM                                            ARRAY ELEMENT
!   -----   ----                                            -------------
!    2      Property ID                                     PBUSH(npbush,1)
!    3      "K", "B", 'GE" or "RCV"
!   If field 3 = "K"  :
!    4-9    Ki                                             RPBUSH(npbush,1- 6)  
!   If field 3 = "B"  :
!    4-9    Bi                                             RPBUSH(npbush,7-12)  
!   If field 3 = "GE" :
!    4-9    GEi                                            RPBUSH(npbush,13)  
!   If field 3 = "RCV":
!    4-9    RCVi                                           RPBUSH(npbush,14-17)

! Cont entries have the vals for "K", "B", "GE", "RCV" that are not on the 1st entry  
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
      NPBUSH = NPBUSH+1
 
! Read and check data on parent card

      CALL I4FLD ( JCARD(2), JF(2), I4INP )                ! Read property ID and enter into array PBUSH
      IF (IERRFL(2) == 'N') THEN
         PROPERTY_ID = I4INP
         DO J=1,NPBUSH-1
            IF (PROPERTY_ID == PBUSH(J,1)) THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1145) JCARD(1),PROPERTY_ID
               WRITE(F06,1145) JCARD(1),PROPERTY_ID
               EXIT
             ENDIF
         ENDDO   
         PBUSH(NPBUSH,1) = PROPERTY_ID
      ENDIF
 
      CALL LEFT_ADJ_BDFLD ( JCARD(3) )                     ! Determine which of the inputs are on the parent entry
      OFFSET      = 0
      NUM_ENTRIES = 6
      IF      (JCARD(3)(1:3) == 'K  ') THEN
         OFFSET      = 0
         NUM_ENTRIES = 6
         CALL BD_IMBEDDED_BLANK   ( JCARD,2,0,4,5,6,7,8,9 )
      ELSE IF (JCARD(3)(1:3) == 'B  ') THEN
         OFFSET      = 6
         NUM_ENTRIES = 6
         CALL BD_IMBEDDED_BLANK   ( JCARD,2,0,4,5,6,7,8,9 )
      ELSE IF (JCARD(3)(1:3) == 'GE ') THEN
         OFFSET      = 12
         NUM_ENTRIES = 6
         CALL BD_IMBEDDED_BLANK   ( JCARD,2,0,4,0,0,0,0,0 )
         CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,5,6,7,8,9 )
      ELSE IF (JCARD(3)(1:3) == 'RCV') THEN
         OFFSET      = 18
         NUM_ENTRIES = 4
         CALL BD_IMBEDDED_BLANK   ( JCARD,2,0,4,5,6,7,0,0 )
         CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,8,9 )
      ELSE
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1178) JCARD(1), JCARD(2), JCARD(3)
         WRITE(F06,1178) JCARD(1), JCARD(2), JCARD(3)
      ENDIF

      DO J=1,NUM_ENTRIES                                   ! Read real property values in fields 4-9
         CALL R8FLD ( JCARD(J+3), JF(J+3), R8INP )
         IF (IERRFL(J+3) == 'N') THEN
            RPBUSH(NPBUSH,J+OFFSET) = R8INP
         ENDIF
      ENDDO
 
      CALL CRDERR ( CARD )

! Read and check data on 3 optional con't entries

      FOUND_RCV = 'N'
pcont:DO I=1,4

         IF (LARGE_FLD_INP == 'N') THEN
            CALL NEXTC  ( CARD, ICONT, IERR )
         ELSE
            CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
            CARD = CHILD
         ENDIF
         IF (ICONT == 1) THEN

            CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
            CALL LEFT_ADJ_BDFLD ( JCARD(3) )               ! Determine whaich of the inputs are on the parent entry

            OFFSET      = 0
            NUM_ENTRIES = 0
            IF      (JCARD(3)(1:3) == 'K  ') THEN
               OFFSET      = 0
               NUM_ENTRIES = 6
               CALL BD_IMBEDDED_BLANK   ( JCARD,2,0,4,5,6,7,8,9 )
            ELSE IF (JCARD(3)(1:3) == 'B  ') THEN
               OFFSET      = 6
               NUM_ENTRIES = 6
               CALL BD_IMBEDDED_BLANK   ( JCARD,2,0,4,5,6,7,8,9 )
            ELSE IF (JCARD(3)(1:3) == 'GE ') THEN
               OFFSET      = 12
               NUM_ENTRIES = 6
               CALL BD_IMBEDDED_BLANK   ( JCARD,2,0,4,0,0,0,0,0 )
               CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,5,6,7,8,9 )
            ELSE IF (JCARD(3)(1:3) == 'RCV') THEN
               FOUND_RCV = 'Y'
               OFFSET      = 18
               NUM_ENTRIES = 4
               CALL BD_IMBEDDED_BLANK   ( JCARD,2,0,4,5,6,7,0,0 )
               CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,8,9 )
            ELSE IF (JCARD(3)(1:3) == 'M  ') THEN
               OFFSET      = 22
               NUM_ENTRIES = 1
               CALL BD_IMBEDDED_BLANK   ( JCARD,2,0,4,5,6,7,0,0 )
               CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,8,9 )
            ELSE
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1148) PROPERTY_ID, 'K, B, GE, or RCV', JCARD(3)
               WRITE(F06,1148) PROPERTY_ID, 'K, B, GE, or RCV', JCARD(3)
               EXIT
            ENDIF
            DO J=1,NUM_ENTRIES                             ! Read real property values in fields 4-9
               CALL R8FLD ( JCARD(J+3), JF(J+3), R8INP )
               IF (IERRFL(J) == 'N') THEN
                  RPBUSH(NPBUSH,J+OFFSET) = R8INP
               ENDIF
            ENDDO
 
            CALL CRDERR ( CARD )

         ELSE

            EXIT pcont

         ENDIF

      ENDDO pcont

      IF (FOUND_RCV == 'N') THEN
         DO J=19,22                                     ! If no RCV con't entry, default stress/strain RCV's to 1.0
            RPBUSH(NPBUSH,J) = ONE
         ENDDO
      ENDIF


! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1145 FORMAT(' *ERROR  1145: DUPLICATE ',A,' ENTRY WITH ID = ',I8)
 
 1148 FORMAT(' *ERROR  1148: FIELD 3 OF PBUSH ', I8, ' CONTINUATION ENTRY SHOULD BE ', A, ' BUT IS: ', A)

 1178 FORMAT(' *ERROR  1178: ',A,A,' HAS INCORRECT VALUE "',A,'"  IN FIELD 3')

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_PBUSH
