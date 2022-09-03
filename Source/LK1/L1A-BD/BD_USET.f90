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
  
      SUBROUTINE BD_USET ( CARD )
  
! Processes USET Bulk Data Cards. Reads and checks data and then write a record to file LINK1X for later processing.
! Each record in file LINK1X has:

!          USET_NAME, COMPJ, GRIDJ, DOFSET 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1X
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ECHO, FATAL_ERR, IERRFL, JCARD_LEN, JF, NUM_USET_RECORDS, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_USET_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  SUPWARN
      USE DOF_TABLES, ONLY            :  TSET_CHR_LEN

      USE BD_USET_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_USET'
      CHARACTER(LEN=*),INTENT(IN)     :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: CHRFLD            ! A character field from the entry
      CHARACTER( 8*BYTE)              :: IP6TYP            ! An output from subr IP6CHK called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARDO            ! An output from subr IP6CHK called herein
      CHARACTER( 2*BYTE)              :: USET_NAME         ! Name in field 2 of the USET entry
 
      INTEGER(LONG)                   :: COMPJ     = 0     ! Displ components constrained at GRIDJ
      INTEGER(LONG)                   :: GRIDJ     = 0     ! Grid ID on USET card
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IDUM              ! Dummy arg in subr IP6CHK not used herein
      INTEGER(LONG)                   :: JERR      = 0     ! A local error count
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_USET_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!  USET Bulk Data Card routine
 
!    FIELD   ITEM           ARRAY ELEMENT
!    -----   ------------   -------------
!     2      USET name (i.e. "U1", "U2")         
!     3      Grid ID        
!     4      Comp. numbers  
!     5      Grid ID        
!     6      Comp.numbers   
!     7      Grid ID        
!     8      Comp.numbers   
 
 
! Initialize

      JERR = 0

!  Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Read USET name

      CALL CHAR_FLD ( JCARD(2), JF(2), CHRFLD )            ! Read field 2: USET name
      IF (IERRFL(2) == 'N') THEN
         CALL LEFT_ADJ_BDFLD ( CHRFLD )
         USET_NAME = CHRFLD(1:2)
         IF ((USET_NAME /= 'U1      ') .AND. (USET_NAME /= 'U2      ')) THEN
            JERR = JERR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1199) SUBR_NAME, USET_NAME
            WRITE(F06,1199) SUBR_NAME, USET_NAME
         ENDIF
      ENDIF

! Process data in fields 3-8

      DO I=3,7,2                                           ! There can be 3 sets of (Grid, Comp) on each card

         IF (JCARD(I)(1:) /= ' ') THEN

            CALL I4FLD ( JCARD(I), JF(I), GRIDJ )          ! Read Grid ID 
                                                           ! Read displ components
            CALL IP6CHK ( JCARD(I+1), JCARDO, IP6TYP, IDUM )
            IF ((IP6TYP == 'COMP NOS') .OR. (IP6TYP == 'ZERO    ') .OR. (IP6TYP == 'BLANK   ')) THEN
               CALL I4FLD ( JCARDO, JF(I+1), COMPJ )          
            ELSE
               JERR      = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1124) JF(I+1), JCARD(1), JCARD(2), JF(I+1), JCARD(I+1)
               WRITE(F06,1124) JF(I+1), JCARD(1), JCARD(2), JF(I+1), JCARD(I+1)
            ENDIF

            IF ((JERR == 0 ) .AND. (IERRFL(I) == 'N') .AND. (IERRFL(I+1) == 'N')) THEN
               NUM_USET_RECORDS = NUM_USET_RECORDS + 1     ! Incr count of number of entries written to file LINK1X

               WRITE(L1X) USET_NAME(1:2), COMPJ, GRIDJ, GRIDJ
            ENDIF

         ELSE                                              ! Field 3, 5 or 7 is blank
 
            IF (JCARD(I+1)(1:) /= ' ') THEN
               WARN_ERR = WARN_ERR + 1
               WRITE(ERR,101) CARD
               WRITE(ERR,1144) JCARD(1), JCARD(2), JF(I+1), JF(I)
               IF (SUPWARN == 'N') THEN
                  IF (ECHO == 'NONE  ') THEN
                     WRITE(F06,101) CARD
                  ENDIF
                  WRITE(F06,1144) JCARD(1), JCARD(2), JF(I+1), JF(I)
               ENDIF
            ENDIF

         ENDIF

      ENDDO
  
      CALL BD_IMBEDDED_BLANK   ( JCARD,0,3,0,5,0,7,0,0 )   ! Make sure that there are no imbedded blanks in fields 2,3,5,6,8,9
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,8,9 )
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(A)

 1124 FORMAT(' *ERROR  1124: INVALID DOF NUMBER IN FIELD ',I3,' ON ',A,' ENTRY WITH ID = ',A                                       &
                    ,/,14X,' MUST BE A COMBINATION OF DIGITS 1-6. HOWEVER, FIELD ',I3, ' HAS: "',A,'"')

 1144 FORMAT(' *WARNING    : ON ',A,' SNAME = ',A,' FIELD ',I3,' IS IGNORED SINCE GRID FIELD ',I3,' IS BLANK.')

 1199 FORMAT(' *ERROR  1199: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' SNAME ON THE ABOVE USET ENTRY MUST BE "U1" OR "U2" BUT IS "',A,'"')

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_USET
