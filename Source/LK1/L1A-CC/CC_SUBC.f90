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
 
      SUBROUTINE CC_SUBC ( CARD )
 
! Processes Case Control SUBCASE cards
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  CC_ENTRY_LEN, FATAL_ERR, LSUB, NSUB, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  CC_SUBC_BEGEND
      USE MODEL_STUF, ONLY            :  SCNUM
 
      USE CC_SUBC_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CC_SUBC'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=LEN(CARD))        :: CARD1             ! CARD shifted to begin in col after "=" sign
      CHARACTER(1*BYTE)               :: EQUAL_SIGN        ! 'Y' if CARD has an = sign between SUBCASE and the subcase number
 
      INTEGER(LONG)                   :: ECOL              ! Col, on CARD, where "E" is located (last letter of SUBCASE)
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IERR              ! Output from subr CSHIFT indicating an error
      INTEGER(LONG)                   :: JERR              ! Error indicator if this subcase number is the same as a previous one
      INTEGER(LONG)                   :: SUBCASE_NUM       ! Subcase number from the SUBCASE card being read
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CC_SUBC_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Process SUBCASE cards 
 
      NSUB = NSUB + 1
      IF (NSUB > LSUB) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1279) SUBR_NAME,LSUB
         WRITE(F06,1279) SUBR_NAME,LSUB
         CALL OUTA_HERE ( 'Y' )                              ! Coding error, so quit
         RETURN
      ENDIF
 
! Is there an = sign?

      EQUAL_SIGN = 'N'
      DO I=1,CC_ENTRY_LEN
         IF (CARD(I:I) == '=') THEN
            EQUAL_SIGN = 'Y'
            EXIT
         ENDIF
      ENDDO
 
      JERR = 0
      
! There should be no problem finding the 'E' in SUBCASE, subr LOADC wouldn't have called this subr otherwise
 
      IF (EQUAL_SIGN == 'Y') THEN
         CALL CSHIFT ( CARD, '=', CARD1, ECOL, IERR )
      ELSE
         CALL CSHIFT ( CARD, 'E', CARD1, ECOL, IERR )
      ENDIF
      IF (IERR == 0) THEN
         READ(CARD1,'(I8)') SUBCASE_NUM
         DO I=1,NSUB-1
            IF (SUBCASE_NUM == SCNUM(I)) THEN
               FATAL_ERR = FATAL_ERR + 1
               JERR = JERR + 1
               WRITE(ERR,1277) SUBCASE_NUM
               WRITE(F06,1277) SUBCASE_NUM
               EXIT
            ENDIF
         ENDDO
         IF (JERR == 0) THEN
            SCNUM(NSUB) = SUBCASE_NUM
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
 1277 FORMAT(' *ERROR  1277: SUBCASE NUMBER ',I8,' IS A DUPLICATE SUBCASE NUMBER')

 1279 FORMAT(' *ERROR  1279: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY SUBCASES; LIMIT = ',I8)

! **********************************************************************************************************************************
 
      END SUBROUTINE CC_SUBC 
