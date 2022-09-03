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
 
      SUBROUTINE FFIELD ( CARD, IERR )
 
! Routine to handle only small field input CARD. The 8 col fields of CARD will be expanded to 16 col fields and the returned CARD
! will have 10 fields of 16 cols each:

!  1) Convert fields 2-9 of a free field Bulk Data card to left justified fixed field card. It is assumed that if input CARD has a
!     comma then it is a free-field card

!  2) Left justify fields 2 - 9 of cards that are fixed field
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, BD_ENTRY_LEN, FATAL_ERR, IMB_BLANK, JCARD_LEN
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  FFIELD_BEGEND

      USE FFIELD_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'FFIELD'
      CHARACTER(LEN=*),  INTENT(INOUT):: CARD              ! 
      CHARACTER( 1*BYTE)              :: FOUND_DATA        ! 
      CHARACTER( 3*BYTE)              :: FREEFLD           ! = 'Y' if CARD is free field form
      CHARACTER(LEN=JCARD_LEN)        :: LJCARD(10)        ! 10 fields of LCARD
      CHARACTER(LEN=BD_ENTRY_LEN)     :: LCARD             ! 
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! Fields of CARD
      CHARACTER(LEN=JCARD_LEN)        :: TJCARD(10)        ! Fields of TCARD
      CHARACTER(LEN=LEN(CARD))        :: TCARD             ! Temporary CARD
 
      INTEGER(LONG)                   :: CARD_LEN          ! Length of CARD
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG), INTENT(OUT)      :: IERR              ! = 1 if a field  is longer than 8 chars on a free field card
      INTEGER(LONG)                   :: IFD               ! Counter for the 10 fields of a Bulk Data CARD
      INTEGER(LONG)                   :: JCT               ! Column counter in free-field CARD
      INTEGER(LONG)                   :: K1S,K2S,K1L       ! Indices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = FFIELD_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      CARD_LEN = LEN(CARD)

! Initialize
 
      DO I = 1,CARD_LEN
         TCARD(I:I) = ' '
      ENDDO
 
      DO I=1,10
         TJCARD(I)(1:) = ' '
      ENDDO 
 
      IERR = 0
 
! Look for ',' on input string 'CARD'; if found, we assume card is free-field
 
      IF ((INDEX(CARD,',') > 0) .OR. (INDEX(CARD,ACHAR(9)) > 0)) THEN
         FREEFLD = 'YES'
      ELSE
         FREEFLD = 'NO '
      ENDIF

! Process CARD

      IF (FREEFLD == 'NO ') THEN                           ! Expand small field card to 16 cols/field and left justify
 
         LCARD(1:) = ' '
         DO I=1,10
            K1S =  8*(I-1) + 1   ;   K2S = K1S + 7
            K1L = 16*(I-1) + 1
            LCARD(K1L:K1L+7) = CARD(K1S:K2S)
         ENDDO

         CALL MKJCARD ( SUBR_NAME, LCARD, LJCARD )         ! LJCARD are fields from LCARD (16 col fields)
 
         DO I = 2,9                                        ! Left justify fields
            IF (LJCARD(I)(1:) == ' ' .OR. LJCARD(I)(1:1) /= ' ') THEN
               TJCARD(I) = LJCARD(I)  
            ELSE
               DO J=1,JCARD_LEN
                  IF (LJCARD(I)(J:J) /= ' ') THEN
                     TJCARD(I)(1:) = LJCARD(I)(J:)
                     EXIT  
                  ENDIF
               ENDDO
            ENDIF   
         ENDDO   
         DO I=2,9
            LJCARD(I) = TJCARD(I)
         ENDDO          

         CALL MKCARD ( LJCARD, CARD ) 
 
      ELSE                                                 ! Convert free-field 'CARD' to fixed field 'TCARD'

         DO I=1,10
            TJCARD(I)(1:) = ' '
         ENDDO

         I    = 0
         IFD  = 1
         JCT  = 0
loop1:   DO

            I = I + 1
            IF (I > BD_ENTRY_LEN) EXIT loop1

            IF (CARD(I:I)  == ' ') CYCLE loop1

            IF ((CARD(I:I) /= ',') .AND. (CARD(I:I) /= ACHAR(9))) THEN

               JCT = JCT+1
 
               IF (JCT > 16) THEN                          ! NOTE: free field can only have <= 16 cols
                  WRITE(ERR,1002)
                  WRITE(F06,1002)
                  WRITE(ERR,129) CARD
                  WRITE(F06,129) CARD
                  IERR = 1
                  FATAL_ERR = FATAL_ERR + 1
                  EXIT loop1
               ELSE
                  TJCARD(IFD)(JCT:JCT) = CARD(I:I)
               ENDIF

            ELSE

               IFD = IFD+1
               JCT = 0
               IF (IFD > 10) EXIT loop1

            ENDIF

         ENDDO loop1

         CALL MKCARD ( TJCARD, CARD )

      ENDIF
 
! Check fields for any imbedded blanks and set error if any are found
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      DO I=2,9
         FOUND_DATA   = 'N'
         IMB_BLANK(I) = 'N'
         DO J=JCARD_LEN,1,-1
            IF (JCARD(I)(J:J) /= ' ') THEN
               FOUND_DATA = 'Y'
            ELSE 
               IF(FOUND_DATA == 'Y') THEN
                  IMB_BLANK(I) = 'Y'
               ELSE
                  CYCLE
               ENDIF
            ENDIF
         ENDDO
      ENDDO
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1002 FORMAT(' *ERROR  1002: TOO LONG AN ENTRY (MORE THAN 16 CHARS) ON THE FOLLOWING ENTRY (MAYBE A COMMA WAS FOUND WHERE ONE',    &
                           ' SHOULD NOT BE):')

  129 FORMAT(A)

! **********************************************************************************************************************************

      END SUBROUTINE FFIELD
