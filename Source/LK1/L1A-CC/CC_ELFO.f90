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
 
      SUBROUTINE CC_ELFO ( CARD )
 
! Processes Case Control ELFO (elforce) entrias
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, F04, err
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, CC_CMD_DESCRIBERS, LSUB, NCCCD, NSUB 
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  CC_ELFO_BEGEND
      USE MODEL_STUF, ONLY            :  SC_ELFE, SC_ELFN
 
      USE CC_ELFO_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CC_ELFO'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER( 1*BYTE)              :: FOUND_BOTH        ! CC_CMD_DESCRIBERS has request for "BOTH"
      CHARACTER( 1*BYTE)              :: FOUND_ENGR        ! CC_CMD_DESCRIBERS has request for "ENGR"
      CHARACTER( 1*BYTE)              :: FOUND_NODE        ! CC_CMD_DESCRIBERS has request for "NODE"
 
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: SETID             ! Set ID on this Case Control card
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CC_ELFO_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! CC_OUTPUTS processes all output type Case Control entries (they all have some common code so it is put there)

      CALL CC_OUTPUTS ( CARD, 'ELFO', SETID )

! Check to see if BOTH, ENGR or NODE were in the ELFO request

      FOUND_BOTH = 'N'
      FOUND_ENGR = 'N'
      FOUND_NODE = 'N'
      DO I=1,NCCCD
         IF (CC_CMD_DESCRIBERS(I)(1:4) == 'BOTH') FOUND_BOTH = 'Y'
         IF (CC_CMD_DESCRIBERS(I)(1:4) == 'ENGR') FOUND_ENGR = 'Y'
         IF (CC_CMD_DESCRIBERS(I)(1:4) == 'NODE') FOUND_NODE = 'Y'
      ENDDO


! Set CASE CONTROL output request variable to SETID
 
      IF      (FOUND_BOTH == 'Y') THEN

         IF (NSUB == 0) THEN
            DO I = 1,LSUB
               SC_ELFE(I) = SETID
               SC_ELFN(I) = SETID
            ENDDO   
         ELSE
            SC_ELFE(NSUB) = SETID
            SC_ELFN(NSUB) = SETID
         ENDIF
 
      ELSE IF (FOUND_NODE == 'Y') THEN

         IF (NSUB == 0) THEN
            DO I = 1,LSUB
               SC_ELFN(I) = SETID
            ENDDO   
         ELSE
            SC_ELFN(NSUB) = SETID
         ENDIF
 
      ELSE                                                 ! Default is ENGR

         IF (NSUB == 0) THEN
            DO I = 1,LSUB
               SC_ELFE(I) = SETID
            ENDDO   
         ELSE
            SC_ELFE(NSUB) = SETID
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
 
      END SUBROUTINE CC_ELFO 
