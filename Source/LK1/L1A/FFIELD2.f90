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
 
      SUBROUTINE FFIELD2 ( CARD1, CARD2, CARD, IERR )
 
! Routine to process large field format BD entries (must be fixed field - free field not allowed for large field format:

! 1) Input 2 physical 80 col cards (read in LOADB) that form one logical entry
 
!    a) Card 1 has 
!         i) BD entry name (CARD1_FLD1))
!        ii) 4 fields of data (CARD1_FLD2 - CARD1_FLD5))
!       iii) cont entry (CARD1_FLD6)

!    b) Card 2 has
!         i) Cont entry (CARD2_FLD1)
!        ii) 4 fields of data (CARD2_FLD2 - CARD2_FLD5)
!       iii) cont entry (CARD2_FLD6)

! 2) Make sure CARD1_FLD6 is same as CARD2_FLD1 (cont entries must match)

! 3) Expand fields in each entry from 8 to 16 cols

! 4) Create new entry with 10 fields of 16 cols each that has:

!    a) field  1: CARD1_FLD1 (BD card name)
!    b) field  2: CARD1_FLD2
!    c) field  3: CARD1_FLD3
!    d) field  4: CARD1_FLD4
!    e) field  5: CARD1_FLD5
!    f) field  6: CARD2_FLD2
!    g) field  7: CARD2_FLD3
!    h) field  8: CARD2_FLD4
!    i) field  9: CARD2_FLD5
!    j) field 10: CARD2_FLD6 (cont from field 6 of 2nd half of entry)

! 5) Left justify fields
  
! N O TE : each of the 2 physical entries making up 1 logical large field entry has 80 cols in 6 fields (1st and fields are 8 cols
!          and 2nd - 6th fields are large field format with 16 cols each)


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, BD_ENTRY_LEN, ECHO, FATAL_ERR, IMB_BLANK, JCARD_LEN
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  FFIELD2_BEGEND

      USE FFIELD2_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'FFIELD2'
      CHARACTER(LEN=*),  INTENT(IN)   :: CARD1             ! 1st physical entry of the large field entry
      CHARACTER(LEN=*),  INTENT(IN)   :: CARD2             ! 2nd physical entry of the large field entry
      CHARACTER(LEN=*),  INTENT(OUT)  :: CARD              ! Card with 10 fields of 16 cols each with the data from CARD1, CARD2
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! 10 fields of CARD . All 10 fields are 16 cols wide
      CHARACTER(LEN=JCARD_LEN)        :: JCARD1(6)         !  6 fields of CARD1. Fields 1,6 are 8 cols. Fields 2,3,4,5 are 16 cols
      CHARACTER(LEN=JCARD_LEN)        :: JCARD2(6)         !  6 fields of CARD2. Fields 1,6 are 8 cols. Fields 2,3,4,5 are 16 cols
      CHARACTER(LEN=JCARD_LEN)        :: TJCARD(10)        ! Temporary JCARD's
 
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG), INTENT(OUT)      :: IERR              ! = 1 if a field  is longer than 8 chars on a free field card
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = FFIELD2_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      IERR = 0

      CARD(1:) = ' '

      DO I=1,6
         JCARD1(I)(1:) = ' '
         JCARD2(I)(1:) = ' '
      ENDDO

! Make sure the entries are not free field

      IF ((INDEX(CARD1,',') > 0) .OR. (INDEX(CARD1,ACHAR(9)) > 0)) THEN
         FATAL_ERR = FATAL_ERR + 1
         IF (ECHO == 'NONE  ') THEN
            IF (SUPWARN == 'N') THEN
               WRITE(F06,129) CARD1
            ENDIF
         ENDIF
         WRITE(ERR,1014)
         WRITE(F06,1014)
         RETURN
      ENDIF

! Put fields of CARD1 and CARD2 into 16 col fields of JCARD1(i=1,6), JCARD2(i=1,6)

      JCARD1(1)(1: 8) = CARD1( 1: 8)   ;   JCARD1(1)(9:16) = ' '
      JCARD1(2)(1:16) = CARD1( 9:24) 
      JCARD1(3)(1:16) = CARD1(25:40) 
      JCARD1(4)(1:16) = CARD1(41:56) 
      JCARD1(5)(1:16) = CARD1(57:72) 
      JCARD1(6)(1: 8) = CARD1(73:80)

      JCARD2(1)(1: 8) = CARD2( 1: 8)
      JCARD2(2)(1:16) = CARD2( 9:24) 
      JCARD2(3)(1:16) = CARD2(25:40) 
      JCARD2(4)(1:16) = CARD2(41:56) 
      JCARD2(5)(1:16) = CARD2(57:72) 
      JCARD2(6)(1: 8) = CARD2(73:80)   ;   JCARD2(6)(9:16) = ' '

! Make sure that CARD2 is the 2nd half of CARD1 (continuation entry from field 6 of CARD1 must match field 1 of CARD2)

      IERR = 0
      IF      (JCARD2(1)(1:) == JCARD1(6)(1:)) THEN
         CONTINUE
      ELSE IF ((JCARD2(1)(1:1) == '*') .AND. (JCARD1(6)(1:1) == ' ') .AND. (JCARD2(1)(2:8) == JCARD1(6)(2:8))) THEN
         CONTINUE
      ELSE IF ((JCARD2(1)(1:1) == ' ') .AND. (JCARD1(6)(1:1) == '*') .AND. (JCARD2(1)(2:8) == JCARD1(6)(2:8))) THEN
         CONTINUE
      ELSE
         IERR = IERR + 1
      ENDIF

      IF (IERR /= 0) THEN
         FATAL_ERR = FATAL_ERR + 1
         IF (ECHO == 'NONE  ') THEN
            IF (SUPWARN == 'N') THEN
               WRITE(F06,129) CARD1
               WRITE(F06,129) CARD2
            ENDIF
         ENDIF
         WRITE(ERR,1020) JCARD1(6), JCARD2(1)
         WRITE(F06,1020) JCARD1(6), JCARD2(1)
         RETURN
      ENDIF

! Assemble output CARD from fields of CARD1,2

      JCARD( 1) = JCARD1(1)
      JCARD( 2) = JCARD1(2)
      JCARD( 3) = JCARD1(3)
      JCARD( 4) = JCARD1(4)
      JCARD( 5) = JCARD1(5)

      JCARD( 6) = JCARD2(2)
      JCARD( 7) = JCARD2(3)
      JCARD( 8) = JCARD2(4)
      JCARD( 9) = JCARD2(5)
      JCARD(10) = JCARD2(6)

! Left justify fields of CARD

      DO I = 2,9                                        ! Left justify fields
         IF (JCARD(I)(1:) == ' ' .OR. JCARD(I)(1:1) /= ' ') THEN
            TJCARD(I) = JCARD(I)  
         ELSE
            DO J=1,JCARD_LEN
               IF (JCARD(I)(J:J) /= ' ') THEN
                  TJCARD(I)(1:) = JCARD(I)(J:)
                  EXIT  
               ENDIF
            ENDDO
         ENDIF   
      ENDDO   
      DO I=2,9
         JCARD(I) = TJCARD(I)
      ENDDO          

! Put left justified fields into CARD

      CALL MKCARD ( JCARD, CARD ) 

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  129 FORMAT(A)

 1014 FORMAT(' *ERROR  1014: FREE FIELD FORMAT NOT ALLOWED FOR LARGE FIELD FORMAT BULK DATA ENTRIES')

 1020 FORMAT(' *ERROR  1020: 2ND PHYSICAL ENTRY OF A LARGE FIELD ENTRY IS NOT A CONTINUATION TO THE 1ST ENTRY.'                    &
                    ,/,14X,' THE LAST FIELD OF THE IST ENTRY = "',A,'" AND THE FIRST FIELD OF THE 2ND ENTRY = "',A,'"')

! ##################################################################################################################################
 
      END SUBROUTINE FFIELD2
