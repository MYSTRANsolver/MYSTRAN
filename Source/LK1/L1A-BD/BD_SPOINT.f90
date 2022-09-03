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
 
      SUBROUTINE BD_SPOINT ( CARD )
 
! Read Bulk Data SPOINT entries. Enter the SPOINT number into array GRID (in col 1) and set GRID(ngrid,6) to 1 to indicate SPOINT

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, NGRID
      USE TIMDAT, ONLY                :  TSEC
      USE MODEL_STUF, ONLY            :  GRID
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_SPOINT_BEGEND
 
      USE BD_SPOINT_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_SPOINT'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER( 8*BYTE)              :: TOKEN             ! The 1st 8 characters from a JCARD
      CHARACTER( 8*BYTE)              :: TOKTYP            ! An output from subr TOKCHK called herein
 
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: JERR      = 0     ! Error indicator for several types of error in format #2 of input
      INTEGER(LONG)                   :: SPOINT1   = 0     ! An SPOINT number
      INTEGER(LONG)                   :: SPOINT2   = 0     ! An SPOINT number
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_SPOINT_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! SPOINT Bulk Data Card routine
 
!   FIELD   ITEM           
!   -----   ------------   
! Format #1:
!   2-9     SPOINT ID's
! on optional continuation cards:
!   2-9     SPOINT ID's
 
! Format #2:
!    2      SPOINT ID 1
!    3      "THRU"
!    4      SPOINT ID 2
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Field 3 of SPOINT must have "THRU" or a SPOINT number or blank.

      TOKEN = JCARD(3)(1:8)                                ! Only send the 1st 8 chars of this JCARD. It has been left justified
      CALL TOKCHK ( TOKEN, TOKTYP )                        ! TOKTYP must be THRU', 'INTEGR', or 'BLANK'
      
! **********************************************************************************************************************************
! Format # 2
 
      IF (TOKTYP == 'THRU    ') THEN                      
 
         JERR = 0

         IF (JCARD(2)(1:) /= ' ') THEN                     ! Get 1st SPOINT ID
            CALL I4FLD ( JCARD(2), JF(2), SPOINT1 )
         ELSE            
            JERR      = JERR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1125) 'SCALAR POINT', JF(2), JCARD(1)
            WRITE(F06,1125) 'SCALAR POINT', JF(2), JCARD(1)
         ENDIF
 
         IF (JCARD(4)(1:) /= ' ') THEN                     ! Get 2nd SPOINT ID
            CALL I4FLD ( JCARD(4), JF(4), SPOINT2 )
         ELSE            
            JERR      = JERR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1125) 'SCALAR POINT', JF(4), JCARD(1)
            WRITE(F06,1125) 'SCALAR POINT', JF(4), JCARD(1)
         ENDIF
 
         IF ((IERRFL(2)=='N') .AND. (IERRFL(4)=='N')) THEN ! Check SPOINT2 > SPOINT1 if there were no errors reading them
            IF (SPOINT2 <= SPOINT1) THEN
               JERR      = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1128) JCARD(1)
               WRITE(F06,1128) JCARD(1)
            ENDIF
         ENDIF            
 
         CALL BD_IMBEDDED_BLANK ( JCARD,2,0,4,0,0,0,0,0 )  ! Make sure that there are no imbedded blanks in fields 2, 4
         CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,5,6,7,8,9 )! Issue warning if fields 5, 6, 7, 8, 9 not blank
         CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields

         IF ((JERR == 0) .AND. (IERRFL(2) == 'N') .AND. (IERRFL(4) == 'N')) THEN
            DO J=1,SPOINT2-SPOINT1+1
               NGRID = NGRID + 1
               GRID(NGRID,1) = SPOINT1 + J - 1
               GRID(NGRID,6) = 1
            ENDDO
         ENDIF
 
! **********************************************************************************************************************************
! Format #1 
 
      ELSE IF ((TOKTYP == 'INTEGER ') .OR. (TOKTYP == 'BLANK   ')) THEN 
 
         JERR = 0

         DO J=2,9                                          ! Get SPOINT ID's in fields 2 - 9
            IF (JCARD(J)(1:) == ' ') THEN
               CYCLE
            ELSE            
               CALL I4FLD ( JCARD(J), JF(J), SPOINT1 )
               IF ((JERR == 0) .AND. (IERRFL(J) == 'N')) THEN
                  NGRID = NGRID + 1
                  GRID(NGRID,1) = SPOINT1
                  GRID(NGRID,6) = 1
               ENDIF
            ENDIF
         ENDDO
  
         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )  ! Make sure that there are no imbedded blanks in fields 2-9
         CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields         
 
      ELSE                                                 ! Error - Field 4 did not have "THRU", or an integer, or was blank 
 
         FATAL_ERR = FATAL_ERR+1
         WRITE(ERR,1127) JF(4), JCARD(1)
         WRITE(F06,1127) JF(4), JCARD(1)
         CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1125 FORMAT(' *ERROR  1125: NO ',A,' SPECIFIED IN FIELD',I4,' ON ',A,' CARD')

 1127 FORMAT(' *ERROR  1127: INVALID DATA IN FIELD ',I2,' OF ',A,' CARD. FIELD MUST HAVE THRU OR A SPOINT NUMBER OR BE BLANK')

 1128 FORMAT(' *ERROR  1128: ON ',A,' THE IDs MUST BE IN INCREASING ORDER FOR THRU OPTION')
 
! **********************************************************************************************************************************
 
      END SUBROUTINE BD_SPOINT
