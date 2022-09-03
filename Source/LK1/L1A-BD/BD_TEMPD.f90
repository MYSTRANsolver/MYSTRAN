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
  
      SUBROUTINE BD_TEMPD ( CARD, CC_LOAD_FND )
  
! Processes TEMPD Bulk Data Cards and writes CARD to file LINK1K for later processing
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1K
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LSUB, NSUB, NTCARD
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_TEMPD_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  SUBLOD
 
      USE BD_TEMPD_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_TEMPD'
      CHARACTER(LEN=*),INTENT(IN)     :: CARD                ! A Bulk Data card
      CHARACTER( 1*BYTE),INTENT(INOUT):: CC_LOAD_FND(LSUB,2) ! 'Y' if B.D load/temp card w/ same set ID (SID) as C.C. LOAD = SID
      CHARACTER(LEN=LEN(CARD))        :: CARDIJ(4)           ! A card made up of the name TEMPD with the 2 fields of data (set ID
!                                                              and temperature) from one, of the possibly 4, TEMPD sets
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)           ! The 10 fields of characters making up CARD
      CHARACTER( 1*BYTE)              :: KEEP_IT(4)          ! ='Y' if the SID on TEMPD is one we need to write to LINK1K
 
      INTEGER(LONG)                   :: I4INP     = 0       ! Value read by subr I4FLD when reading a CARD field
      INTEGER(LONG)                   :: JERR      = 0       ! Error count
      INTEGER(LONG)                   :: I,J                 ! DO loop indices
      INTEGER(LONG)                   :: SID       = 0       ! Set ID read from CARD
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_TEMPD_BEGEND
  
      REAL(DOUBLE)                    :: RTEMP     = ZERO    ! Real value of a temperature
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!  TEMPD Bulk Data card check (for format checking only)
 
!    FIELD   ITEM          
!    -----   ------
!     2      SID                           
!     3      Temp
!     4      SID
!     5      Temp
!     6      SID
!     7      Temp          
!     8      SID
!     9      Temp          
 
    
!  Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Initialize CARDIJ(1-4)

      DO I=1,4
         CARDIJ(I)(1:LEN(CARD)) = ' '
      ENDDO 

! Read and check data

      DO I=1,4                                             ! Check that pairs of fields either both have data or neither has data
         IF      ((JCARD(2*I)(1:) == ' ') .AND. (JCARD(2*I+1)(1:) == ' ')) THEN
            CYCLE
         ELSE IF ((JCARD(2*I)(1:) == ' ') .AND. (JCARD(2*I+1)(1:) /= ' ')) THEN
            JERR = JERR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1191) 2*I, 2*I+1, JCARD(1)
            WRITE(F06,1191) 2*I, 2*I+1, JCARD(1)
         ELSE IF ((JCARD(2*I)(1:) /= ' ') .AND. (JCARD(2*I+1)(1:) == ' ')) THEN
            JERR = JERR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1191) 2*I, 2*I+1, JCARD(1)
            WRITE(F06,1191) 2*I, 2*I+1, JCARD(1)
         ENDIF
      ENDDO 

      IF (JERR == 0) THEN                                  ! Overall format is OK

         DO I=1,4
            KEEP_IT(I) = 'N'
            IF (JCARD(2*I)(1:) /= ' ') THEN
               CALL I4FLD ( JCARD(2*I), JF(2*I), I4INP )
               IF (IERRFL(2*I) == 'N') THEN
                  DO J=1,NSUB
                     SID = I4INP
                     IF (SID == SUBLOD(J,2)) THEN
                        CC_LOAD_FND(J,2) = 'Y'
                        KEEP_IT(I)  = 'Y'
                     ENDIF
                  ENDDO
               ENDIF
               CALL R8FLD ( JCARD(2*I+1), JF(2*I+1), RTEMP )
               IF (KEEP_IT(I) == 'Y') THEN
                  CARDIJ(I)(            1:  JCARD_LEN) = JCARD(1)
                  CARDIJ(I)(  JCARD_LEN+1:2*JCARD_LEN) = JCARD(2*I)
                  CARDIJ(I)(2*JCARD_LEN+1:3*JCARD_LEN) = JCARD(2*I+1)
               ENDIF
            ENDIF
         ENDDO   
 
         DO I=2,8                                          ! Set JERR if there were any errors reading fields
            IF (IERRFL(I) == 'Y') THEN
               JERR = JERR + 1
               EXIT
            ENDIF
         ENDDO

      ENDIF 

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )     ! Make sure that there are no imbedded blanks in fields 2-9
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

      IF (JERR == 0) THEN                                  ! Write CARDIJ(1-4) to file LINK1K for later processing
         DO I=1,4
            IF (KEEP_IT(I) == 'Y') THEN
               WRITE(L1K) CARDIJ(I)
               NTCARD = NTCARD+1
            ENDIF
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
 1191 FORMAT(' *ERROR  1191: INVALID DATA IN FIELDS ',I3,' AND ',I3,' ON ',A,' EITHER BOTH FIELDS SHOULD BE NONBLANK OR THEY',     &
                           ' SHOULD BOTH BE BLANK')

! **********************************************************************************************************************************
  
      END SUBROUTINE BD_TEMPD
