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
  
      SUBROUTINE CARD_FLDS_NOT_BLANK ( JCARD, FLD2, FLD3, FLD4, FLD5, FLD6, FLD7, FLD8, FLD9 )
  
! Prepares message when some fields of a Bulk data card that should be blank, aren't
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, JCARD_LEN, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  CARD_FLDS_NOT_BLANK_BEGEND
 
      USE CARD_FLDS_NOT_BLANK_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM))    :: SUBR_NAME = 'CARD_FLDS_NOT_BLANK'
      CHARACTER(LEN=JCARD_LEN), INTENT(IN):: JCARD(10)         ! The 10 fields of 8 characters making up CARD
      CHARACTER( 1*BYTE)                  :: COMMENT           ! 'Y' or 'N' depending on whether non-blank fields are a comment 
      CHARACTER( 8*BYTE)                  :: MSSG8             ! Message with all fields that are not blank that should be blank
      CHARACTER( 1*BYTE)                  :: MSSG1             ! Message that has the field number in it
 
      INTEGER(LONG), INTENT(IN)           :: FLD2              ! Refers to field 2 of a B.D. card. If /= 0, then check this field 
      INTEGER(LONG), INTENT(IN)           :: FLD3              ! Refers to field 3 of a B.D. card. If /= 0, then check this field
      INTEGER(LONG), INTENT(IN)           :: FLD4              ! Refers to field 4 of a B.D. card. If /= 0, then check this field
      INTEGER(LONG), INTENT(IN)           :: FLD5              ! Refers to field 5 of a B.D. card. If /= 0, then check this field
      INTEGER(LONG), INTENT(IN)           :: FLD6              ! Refers to field 6 of a B.D. card. If /= 0, then check this field
      INTEGER(LONG), INTENT(IN)           :: FLD7              ! Refers to field 7 of a B.D. card. If /= 0, then check this field
      INTEGER(LONG), INTENT(IN)           :: FLD8              ! Refers to field 8 of a B.D. card. If /= 0, then check this field
      INTEGER(LONG), INTENT(IN)           :: FLD9              ! Refers to field 9 of a B.D. card. If /= 0, then check this field
      INTEGER(LONG)                       :: ALL_FLDS(2:9)     ! Array of the FLDi (2 through 9)
      INTEGER(LONG)                       :: I,J               ! Do loop indices
      INTEGER(LONG), PARAMETER            :: SUBR_BEGEND = CARD_FLDS_NOT_BLANK_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Set ALL_FLDS

      ALL_FLDS(2) = FLD2
      ALL_FLDS(3) = FLD3
      ALL_FLDS(4) = FLD4
      ALL_FLDS(5) = FLD5
      ALL_FLDS(6) = FLD6
      ALL_FLDS(7) = FLD7
      ALL_FLDS(8) = FLD8
      ALL_FLDS(9) = FLD9

! Scan for 1st non-blank field that should not have data and see if the 1st non-blank character is '$'.
! If it is, then the non-blank fields have a comment, so return

      COMMENT = 'N'
i_do: DO I=2,9
         IF (ALL_FLDS(I) /= I) THEN                        ! CYCLE until we find 1st field to check
            CYCLE
         ELSE                                              ! Found a field that should be blank (unless it begins a comment)
j_do:       DO J=1,JCARD_LEN                               ! CYCLE through chars of this field to see if 1st non-blank char is '$'
               IF (JCARD(I)(J:J) == ' ') THEN
                  CYCLE j_do
               ELSE
                  IF (JCARD(I)(J:J) == '$') THEN           ! Found a '$' (with blanks preceeding it) so we found a comment
                     COMMENT = 'Y'
                     EXIT i_do                             ! Set COMMENT = 'Y' and EXIT outer loop
                  ENDIF
               ENDIF
            ENDDO j_do
         ENDIF
      ENDDO i_do

! Issue warning if fields not blank that should be.

      IF (COMMENT == 'N') THEN

         MSSG8 = '        '
         IF ((JCARD(2)(1:) /= ' ') .AND. (FLD2 == 2)) THEN
            MSSG1 = '2'
            MSSG8 =                  MSSG1(1:1) // MSSG8(2:8)
         ENDIF
         IF ((JCARD(3)(1:) /= ' ') .AND. (FLD3 == 3)) THEN
            MSSG1 = '3'
            MSSG8 = MSSG8(1:1) // MSSG1(1:1) // MSSG8(3:8)
         ENDIF
         IF ((JCARD(4)(1:) /= ' ') .AND. (FLD4 == 4)) THEN
            MSSG1 = '4'
            MSSG8 = MSSG8(1:2) // MSSG1(1:1) // MSSG8(4:8)
         ENDIF
         IF ((JCARD(5)(1:) /= ' ') .AND. (FLD5 == 5)) THEN
            MSSG1 = '5'
            MSSG8 = MSSG8(1:3) // MSSG1(1:1) // MSSG8(5:8)
         ENDIF
         IF ((JCARD(6)(1:) /= ' ') .AND. (FLD6 == 6)) THEN
            MSSG1 = '6'
            MSSG8 = MSSG8(1:4) // MSSG1(1:1) // MSSG8(6:8)
         ENDIF
         IF ((JCARD(7)(1:) /= ' ') .AND. (FLD7 == 7)) THEN
            MSSG1 = '7'
            MSSG8 = MSSG8(1:5) // MSSG1(1:1) // MSSG8(7:8)
         ENDIF
         IF ((JCARD(8)(1:) /= ' ') .AND. (FLD8 == 8)) THEN
            MSSG1 = '8'
            MSSG8 = MSSG8(1:6) // MSSG1(1:1) // MSSG8(8:8)
         ENDIF  
         IF ((JCARD(9)(1:) /= ' ') .AND. (FLD9 == 9)) THEN
            MSSG1 = '9'
            MSSG8 = MSSG8(1:7) // MSSG1(1:1)
         ENDIF
         IF (MSSG8 /= '        ') THEN      
            WARN_ERR = WARN_ERR+1
            WRITE(ERR,1726) MSSG8 
            IF (SUPWARN == 'N') THEN
               WRITE(F06,1726) MSSG8
            ENDIF 
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
 1726 FORMAT(' *WARNING    : FIELD(s) ',A8,' ON PREVIOUS ENTRY SHOULD BE BLANK AND ARE IGNORED')
 
! **********************************************************************************************************************************
 
      END SUBROUTINE CARD_FLDS_NOT_BLANK
