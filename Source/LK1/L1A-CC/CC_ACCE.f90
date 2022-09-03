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
 
      SUBROUTINE CC_ACCE ( CARD )
 
! Processes Case Control ACCE cards
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, F04, PCHSTAT
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, CC_CMD_DESCRIBERS, LSUB, NSUB, NCCCD 
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  CC_ACCE_BEGEND
      USE CC_OUTPUT_DESCRIBERS, ONLY  :  ACCE_OUT
      USE MODEL_STUF, ONLY            :  SC_ACCE
 
      USE CC_ACCE_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CC_ACCE'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER( 1*BYTE)              :: FOUND_PRINT       ! CC_CMD_DESCRIBERS has request for "PRINT"
      CHARACTER( 1*BYTE)              :: FOUND_PUNCH       ! CC_CMD_DESCRIBERS has request for "PUNCH"
 
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: SETID             ! Set ID on this Case Control card
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CC_ACCE_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! CC_OUTPUTS processes all output type Case Control entries (they all have some common code so it is put there)

      CALL CC_OUTPUTS ( CARD, 'ACCE', SETID )

! Check to see if BOTH, ENGR or NODE were in the ELFO request

      FOUND_PRINT = 'N'
      FOUND_PUNCH = 'N'
      DO I=1,NCCCD
         IF (CC_CMD_DESCRIBERS(I)(1:5) == 'PRINT') FOUND_PRINT = 'Y'
         IF (CC_CMD_DESCRIBERS(I)(1:5) == 'PUNCH') FOUND_PUNCH = 'Y'
      ENDDO

      ACCE_OUT(1:) = ' '
      IF ((FOUND_PRINT == 'Y') .AND. (FOUND_PUNCH == 'N')) ACCE_OUT(1:5) = 'PRINT'
      IF ((FOUND_PRINT == 'N') .AND. (FOUND_PUNCH == 'Y')) ACCE_OUT(1:5) = 'PUNCH'  ;  PCHSTAT = 'KEEP    '
      IF ((FOUND_PRINT == 'Y') .AND. (FOUND_PUNCH == 'Y')) ACCE_OUT(1:4) = 'BOTH'   ;  PCHSTAT = 'KEEP    '
      IF ( ACCE_OUT(1:) == ' ') ACCE_OUT(1:5) = 'PRINT'    ! Neither PRINT or PUNCH found so default to PRINT

! Set CASE CONTROL output request variable to SETID
 
      IF (NSUB == 0) THEN
         DO I = 1,LSUB
            SC_ACCE(I) = SETID
         ENDDO   
      ELSE
         SC_ACCE(NSUB) = SETID
      ENDIF
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 
      END SUBROUTINE CC_ACCE 
