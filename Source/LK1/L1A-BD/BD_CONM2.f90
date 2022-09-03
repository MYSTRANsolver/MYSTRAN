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
 
      SUBROUTINE BD_CONM2 ( CARD, LARGE_FLD_INP )
 
! Processes CONM2 Bulk Data Cards
!  1) Reads CONM2 ID, grid ID and coord system ID and puts them into array CONM2
!  2) Reads mass, offsets, and moments of inertia and puts them into array RCONM2 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ECHO, FATAL_ERR, IERRFL, JCARD_LEN, JF, LCONM2, NCONM2, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_CONM2_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  SUPWARN
      USE MODEL_STUF, ONLY            :  CONM2, RCONM2
 
      USE BD_CONM2_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_CONM2'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN=LEN(CARD))        :: CARDP             ! Parent card
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARD_ID          ! The COMN2 ID 
      CHARACTER(16*BYTE)              :: NAME              ! Name for output error purposes
 
      INTEGER(LONG)                   :: CONM2_ID  = 0     ! The ID for this CONM2 (field 2)
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_CONM2_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! CONM2 Bulk Data Card routine
 
!   FIELD   ITEM           ARRAY ELEMENT
!   -----   ------------   -------------
!    2      Element ID     CONM2(nconm2,1)
!    3      Grid ID        CONM2(nconm2,2)
!    4      Cord. ID       CONM2(nconm2,3)
!    5      Mass           RCONM2(nconm2,1)
!    6      D1 offset      RCONM2(nconm2,2)
!    7      D2 offset      RCONM2(nconm2,3)
!    8      D3 offset      RCONM2(nconm2,4)
! on optional second card:
!    2      I11 moi-11     RCONM2(nconm2,5)
!    3      I21 moi-21     RCONM2(nconm2,6)
!    4      I22 moi-22     RCONM2(nconm2,7)
!    5      I31 moi-31     RCONM2(nconm2,8)
!    6      I32 moi-32     RCONM2(nconm2,9)
!    7      I33 moi-33     RCONM2(nconm2,10)
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      CARDP = CARD
      JCARD_ID = JCARD(2)
 
! Check for overflow

      NCONM2 = NCONM2+1

! Read data

      CALL I4FLD ( JCARD(2), JF(2), CONM2_ID )             ! Check for duplicate ID number
      IF (IERRFL(2) == 'N') THEN
         DO I=1,NCONM2-1
            IF (CONM2_ID == CONM2(I,1)) THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1145) JCARD(1),CONM2_ID
               WRITE(F06,1145) JCARD(1),CONM2_ID
               EXIT
            ENDIF
         ENDDO 
         CONM2(NCONM2,1) = CONM2_ID
      ENDIF

      CALL I4FLD ( JCARD(3), JF(3),  CONM2(NCONM2,2) )
      CALL I4FLD ( JCARD(4), JF(4),  CONM2(NCONM2,3) )
      CALL R8FLD ( JCARD(5), JF(5), RCONM2(NCONM2,1) )
      CALL R8FLD ( JCARD(6), JF(6), RCONM2(NCONM2,2) )
      CALL R8FLD ( JCARD(7), JF(7), RCONM2(NCONM2,3) )
      CALL R8FLD ( JCARD(8), JF(8), RCONM2(NCONM2,4) )

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,0 )     ! Make sure that there are no imbedded blanks in fields 2-8
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,0,9 )   ! Issue warning if field 9 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

! Optional Second Card:
 
      IF (LARGE_FLD_INP == 'N') THEN
         CALL NEXTC  ( CARD, ICONT, IERR )
      ELSE
         CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
         CARD = CHILD
      ENDIF
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )

      RCONM2(NCONM2, 5) = 0.0
      RCONM2(NCONM2, 6) = 0.0
      RCONM2(NCONM2, 7) = 0.0
      RCONM2(NCONM2, 8) = 0.0
      RCONM2(NCONM2, 9) = 0.0
      RCONM2(NCONM2,10) = 0.0

      IF (ICONT == 1) THEN

         IF (CARD(1:) /= ' ') THEN                        ! Only process continuation entries if 1st one is not totally blank

            CALL R8FLD ( JCARD(2), JF(2), RCONM2(NCONM2, 5) )
            CALL R8FLD ( JCARD(3), JF(3), RCONM2(NCONM2, 6) )
            CALL R8FLD ( JCARD(4), JF(4), RCONM2(NCONM2, 7) )
            CALL R8FLD ( JCARD(5), JF(5), RCONM2(NCONM2, 8) )
            CALL R8FLD ( JCARD(6), JF(6), RCONM2(NCONM2, 9) )
            CALL R8FLD ( JCARD(7), JF(7), RCONM2(NCONM2,10) )

            CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,0,0 )
            CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,8,9)
            CALL CRDERR ( CARD )

         ENDIF

      ENDIF
 
! Check for negative mass/MOI's
 
      IF((RCONM2(NCONM2, 1) < ZERO) .OR. (RCONM2(NCONM2, 1) < ZERO) .OR.                                                           &
         (RCONM2(NCONM2, 1) < ZERO) .OR. (RCONM2(NCONM2, 1) < ZERO)) THEN 
         WRITE(ERR,101) CARDP
         IF (ECHO == 'NONE  ') THEN
            IF (SUPWARN == 'N') THEN
               WRITE(F06,101) CARDP
               WRITE(F06,101) CARD
            ENDIF
         ENDIF
      ENDIF

      IF (RCONM2(NCONM2, 1) < ZERO) THEN
         WARN_ERR = WARN_ERR + 1
         NAME  = 'MASS            '
         WRITE(ERR,1134) JCARD_ID,NAME,RCONM2(NCONM2, 1) 
         IF (SUPWARN == 'N') THEN
            WRITE(F06,1134) JCARD_ID,NAME,RCONM2(NCONM2, 1)
         ENDIF
      ENDIF 

      IF (RCONM2(NCONM2, 5) < ZERO) THEN
         WARN_ERR = WARN_ERR + 1
         NAME = 'PRINCIPAL MOI-11'
         WRITE(ERR,1134) JCARD_ID,NAME,RCONM2(NCONM2, 5) 
         IF (SUPWARN == 'N') THEN
            WRITE(F06,1134) JCARD_ID,NAME,RCONM2(NCONM2, 5)
         ENDIF
      ENDIF 

      IF (RCONM2(NCONM2, 7) < ZERO) THEN
         WARN_ERR = WARN_ERR + 1
         NAME = 'PRINCIPAL MOI-22'
         WRITE(ERR,1134) JCARD_ID,NAME,RCONM2(NCONM2, 7) 
         IF (SUPWARN == 'N') THEN
            WRITE(F06,1134) JCARD_ID,NAME,RCONM2(NCONM2, 7)
         ENDIF
      ENDIF 

      IF (RCONM2(NCONM2,10) < ZERO) THEN
         WARN_ERR = WARN_ERR + 1
         NAME = 'PRINCIPAL MOI-33'
         WRITE(ERR,1134) JCARD_ID, NAME, RCONM2(NCONM2,10) 
         IF (SUPWARN == 'N') THEN
            WRITE(F06,1134) JCARD_ID, NAME, RCONM2(NCONM2,10)
         ENDIF
      ENDIF 

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(A)

 1134 FORMAT(' *WARNING    : NEGATIVE MASS OR PRINCIPAL MOI VALUE ON CONM2 ',A,': ',A16,' = ',ES14.6) 
 
 1145 FORMAT(' *ERROR  1145: DUPLICATE ',A,' ENTRY WITH ID = ',I8)

 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_CONM2
