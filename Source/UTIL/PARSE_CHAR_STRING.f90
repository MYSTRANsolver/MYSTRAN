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
 
      SUBROUTINE PARSE_CHAR_STRING ( CHAR_STRING, STRING_LEN, MAX_WORDS, MWLEN, NUM_WORDS, WORDS, IERR )
 
! Parses a character string whose words are separated by blanks and/or commas into a 1-D array, WORDS, of the words in the string.
! For example, if CHAR_STRING is the string (without quotes): "  SORT1 ,   REAL,PRINT  ,,  ,  QAZ  VONMISES", then this subr
! parses CHAR_STRING into 5 words in the array WORDS:

!                            WORDS(1) = SORT1
!                            WORDS(2) = REAL
!                            WORDS(3) = PRINT
!                            WORDS(4) = QAZ
!                            WORDS(5) = VONMISES

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS 
      USE SUBR_BEGEND_LEVELS, ONLY    :  PARSE_CHAR_STRING_BEGEND

      USE PARSE_CHAR_STRING_USE_IFs

      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER         :: MAX_LEN_BAD_WRD=150!  

      INTEGER(LONG), INTENT(IN)        :: MAX_WORDS          ! Dim of WORDS (number of words in CHAR_STRING cannot exceed this)
      INTEGER(LONG), INTENT(IN)        :: MWLEN              ! Maximum length, in characters, of the entries in array WORDS

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)) :: SUBR_NAME = 'PARSE_CHAR_STRING'
      CHARACTER(LEN=*)    , INTENT(IN) :: CHAR_STRING        ! Character string to be parsed
      CHARACTER(LEN=MWLEN), INTENT(OUT):: WORDS(MAX_WORDS)   ! Array of the words parsed from CHAR_STRING.
      CHARACTER(LEN=MWLEN)             :: WORD               ! One word that will go into array WORDS

      INTEGER(LONG), INTENT(IN)        :: STRING_LEN         ! Length, in characters, of CHAR_STRING
      INTEGER(LONG), INTENT(OUT)       :: IERR               ! Error designator
      INTEGER(LONG), INTENT(OUT)       :: NUM_WORDS          ! Number of distinct words in CHAR_STRING
      INTEGER(LONG)                    :: NUM         = 0    ! Lesser of NUM_WORDS and MAX_WORDS
      INTEGER(LONG)                    :: CHAR_COUNT         ! Index into CHAR_STRING to a character in that string (not ' ' or ',')
      INTEGER(LONG)                    :: I,J                ! DO loop indices
      INTEGER(LONG)                    :: WORD_LEN           ! Length of one of the words in CHAR_STRING (must be <= MWLEN)
      INTEGER(LONG), PARAMETER         :: SUBR_BEGEND = PARSE_CHAR_STRING_BEGEND
 
! *********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGIN',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      DO I=1,MAX_WORDS
         WORDS(I)(1:) = ' '
      ENDDO

      IERR = 0

! Call debug code if requested

      IF (DEBUG(173) >= 1) CALL DEBUG_PARSE_CHR_STRNG ( 1, '173' )

! Parse words from CHAR_STRING

      CHAR_COUNT = 0
      NUM_WORDS  = 0
nwrds:DO

         CHAR_COUNT = CHAR_COUNT + 1

         IF (CHAR_COUNT > STRING_LEN) THEN

            EXIT nwrds

         ELSE

            IF ((CHAR_STRING(CHAR_COUNT:CHAR_COUNT) == ',') .OR. (CHAR_STRING(CHAR_COUNT:CHAR_COUNT) == ' ')) THEN

               CYCLE nwrds

            ELSE

               WORD_LEN  = 0
               NUM_WORDS = NUM_WORDS + 1

               WORD(1:) = ' '
one_wrd:       DO
                  WORD_LEN = WORD_LEN + 1
                  IF (DEBUG(173) > 1) CALL DEBUG_PARSE_CHR_STRNG ( 2, '173' )
                  IF (WORD_LEN > MWLEN) THEN
                     IERR = 1
                     CALL PARSE_CHAR_STRING_MSG ( 1, WORD )
                     CYCLE one_wrd
                  ENDIF
                  WORD(WORD_LEN:WORD_LEN) = CHAR_STRING(CHAR_COUNT:CHAR_COUNT)
                  CHAR_COUNT = CHAR_COUNT + 1
                  IF (CHAR_COUNT > STRING_LEN+1) THEN      ! Need +1 in order to get last WORD put into array WORDS
                     EXIT nwrds
                  ENDIF
                  IF ((CHAR_STRING(CHAR_COUNT:CHAR_COUNT) == ',') .OR. (CHAR_STRING(CHAR_COUNT:CHAR_COUNT) == ' ')) THEN
                     EXIT one_wrd
                  ELSE
                     IF (WORD_LEN < MWLEN) THEN
                        CYCLE one_wrd
                     ELSE IF (WORD_LEN == MWLEN) then      ! The word is already as long as is allowed. See if it continues
j_do:                   DO J=1,MAX_LEN_BAD_WRD             ! Loop trying to get to end of this long word to see if there are others

                           CHAR_COUNT = CHAR_COUNT + 1
                           IF ((CHAR_STRING(CHAR_COUNT:CHAR_COUNT) == ',') .OR. (CHAR_STRING(CHAR_COUNT:CHAR_COUNT) == ' ')) THEN
                              EXIT one_wrd
                           ENDIF

                           IF (J < MAX_LEN_BAD_WRD) THEN
                              IERR = 1
                              CALL PARSE_CHAR_STRING_MSG ( 1, WORD )
                              CYCLE j_do
                           ENDIF

                           IF (J == MAX_LEN_BAD_WRD) THEN  ! Give up and get out of this loop. This word is absurdly too long
                              EXIT one_wrd
                           ENDIF

                        ENDDO j_do
                     ELSE
                        EXIT one_wrd
                     ENDIF   
                  ENDIF
               ENDDO one_wrd

               IF (NUM_WORDS > MAX_WORDS) THEN
                  IERR = 2
                  CALL PARSE_CHAR_STRING_MSG ( 2, WORD )
                  EXIT nwrds
               ENDIF

               WORDS(NUM_WORDS) = WORD

               IF (DEBUG(173) > 1) CALL DEBUG_PARSE_CHR_STRNG ( 3, '173' )

            ENDIF

         ENDIF

      ENDDO nwrds 

      IF (DEBUG(173) >= 1) CALL DEBUG_PARSE_CHR_STRNG ( 4, '173' )

      IF (IERR > 0) THEN
         WRITE(F06,9998)
         NUM = MAX_WORDS
         IF (NUM_WORDS <= MAX_WORDS) NUM = NUM_WORDS 
         DO I=1,NUM
            WRITE(F06,9999) I, WORDS(I)
         ENDDO
         WRITE(F06,*)
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

! **********************************************************************************************************************************
 9998 FORMAT('               THE WORDS FROM THE STRING ARE PRINTED BELOW:',/)

 9999 FORMAT(I16,2X,'"',A,'"')

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE PARSE_CHAR_STRING_MSG ( OPT, WORD )

      IMPLICIT NONE

      CHARACTER(LEN=MWLEN)            :: WORD              ! One word that will go into array WORDS

      INTEGER(LONG)                   :: OPT               ! Tells which option to use here

      WARN_ERR = WARN_ERR + 1

      IF (OPT == 1) THEN
         WRITE(ERR,961) MWLEN, WORD
         WRITE(F06,961) MWLEN, WORD
      ELSE IF (OPT == 2) THEN 
         WRITE(ERR,962) MAX_WORDS
         WRITE(F06,962) MAX_WORDS
      ENDIF

! **********************************************************************************************************************************
  961 FORMAT(' *WARNING    : THE LENGTH OF A WORD IN ABOVE STRING EXCEEDED ',I5,' CHARACTERS. THAT WORD, ','"',A,'...", IS IGNORED')

  962 FORMAT(' *WARNING    : THE NUMBER OF WORDS IN ABOVE CHARACTER STRING EXCEEDED',I5,'. REMAINING WORDS IGNORED')

! **********************************************************************************************************************************

      END SUBROUTINE PARSE_CHAR_STRING_MSG

! ##################################################################################################################################

      SUBROUTINE DEBUG_PARSE_CHR_STRNG ( WHAT, DEB_NUM )

      CHARACTER( 3*BYTE)              :: DEB_NUM           ! 

      INTEGER(LONG)                   :: WHAT

! **********************************************************************************************************************************
      IF (WHAT == 1) THEN
         WRITE(F06,10)
         WRITE(F06,11) DEB_NUM
         WRITE(F06,12) CHAR_STRING(1:STRING_LEN)
         WRITE(F06,*)
      ENDIF

      IF (WHAT == 2) THEN
         WRITE(F06,21) NUM_WORDS, CHAR_COUNT, CHAR_STRING(CHAR_COUNT:CHAR_COUNT), word_len
      ENDIF

      IF (WHAT == 3) THEN
         WRITE(F06,*)
      ENDIF

      IF (WHAT == 4) THEN
         WRITE(F06,41) NUM_WORDS, STRING_LEN
         NUM = NUM_WORDS
         IF (NUM_WORDS > MAX_WORDS) NUM = MAX_WORDS
         DO I=1,NUM
            WRITE(F06,42) I, WORDS(I)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,10)
      ENDIF

! **********************************************************************************************************************************
   10 FORMAT('******************************************************************************')

   11 FORMAT('In subr PARSE_CHAR_STRING for DEBUG(', A,')' /)

   12 FORMAT('   CHAR_STRING = "',A,'"')

   21 FORMAT('   Word num, I, CHAR_STRING(I:I) = ',I3,I4,2X,A, i8)

   41 FORMAT('   There are ',I2,' word(s) in the ',I3,' character variable CHAR_STRING:',/,                                        &
             '   --------------------------------------------------------------')

   42 FORMAT('     Word ',I2,' = "',A,'"')

! **********************************************************************************************************************************

      END SUBROUTINE DEBUG_PARSE_CHR_STRNG

      END SUBROUTINE PARSE_CHAR_STRING

