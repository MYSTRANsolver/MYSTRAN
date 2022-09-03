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
  
      SUBROUTINE BD_SPC ( CARD, CC_SPC_FND )
  
! Processes SPC Bulk Data Cards. Reads and checks data and then write a record to file LINK1O for later processing.
! Each record in file LINK1O has:

!          SETID, COMPJ, GRIDJ, RSPCJ, DOFSET 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1O
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ECHO, FATAL_ERR, IERRFL, JCARD_LEN, JF, LSPC, NSPC, NUM_SPC_RECORDS, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_SPC_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  EPSIL, SUPWARN
      USE DOF_TABLES, ONLY            :  TSET_CHR_LEN
      USE MODEL_STUF, ONLY            :  SPC_SIDS, SPCSET

      USE BD_SPC_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_SPC'
      CHARACTER(LEN=*),INTENT(IN)     :: CARD              ! A Bulk Data card
      CHARACTER( 1*BYTE),INTENT(INOUT):: CC_SPC_FND        ! ='Y' if this SPC is a set requested in Case Control
      CHARACTER(LEN=LEN(TSET_CHR_LEN)):: DOFSET            ! 'SB' or 'SE' to denote the set that an SPC'd DOF belongs to
      CHARACTER( 8*BYTE)              :: IP6TYP            ! An output from subr IP6CHK called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARDO            ! An output from subr IP6CHK called herein
 
      INTEGER(LONG)                   :: COMPJ     = 0     ! Displ components constrained at GRIDJ
      INTEGER(LONG)                   :: GRIDJ     = 0     ! Grid ID on SPC card
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IDUM              ! Dummy arg in subr IP^CHK not used herein
      INTEGER(LONG)                   :: JERR      = 0     ! A local error count
      INTEGER(LONG)                   :: SETID     = 0     ! SPC set ID
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_SPC_BEGEND
 
      REAL(DOUBLE)                    :: DEPS1             ! A small positive number to compare real zero
      REAL(DOUBLE)                    :: RSPCJ     = ZERO  ! Enforced displ value 

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!  SPC Bulk Data Card routine
 
!    FIELD   ITEM           ARRAY ELEMENT
!    -----   ------------   -------------
!     2      Set ID         
!     3      Grid ID        
!     4      Comp. numbers  
!     5      Displacement   
!     6      Grid ID        
!     7      Comp.numbers   
!     8      Displacement   
 
 
!xx   CC_SPC_FND = 'N'                                    ! ERROR. When this is in, then it is reset each time an SPC card is read

      DEPS1 = DABS(EPSIL(1))

!  Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Check for overflow

      NSPC = NSPC + 1
!xx   IF (NSPC > LSPC) THEN
!xx      FATAL_ERR = FATAL_ERR + 1
!xx      WRITE(ERR,1163) SUBR_NAME,JCARD(1),LSPC
!xx      WRITE(F06,1163) SUBR_NAME,JCARD(1),LSPC
!xx      CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit
!xx   ENDIF

! Read set ID

      CALL I4FLD ( JCARD(2), JF(2), SETID )                ! Read field 2: Set ID
      IF (IERRFL(2) == 'N') THEN
         IF (SETID == SPCSET) THEN
            CC_SPC_FND = 'Y'
         ENDIF
         SPC_SIDS(NSPC) = SETID
      ELSE
         JERR = JERR + 1
      ENDIF
 
      DO I=1,2                                             ! There can be 2 sets of (Grid, Comp, Value) on each card

         IF (JCARD(3*I)(1:) /= ' ') THEN

            JERR = 0
            CALL I4FLD ( JCARD(3*I), JF(3*I), GRIDJ )      ! Read Grid ID 
                                                           ! Read displ components
            CALL IP6CHK ( JCARD(3*I+1), JCARDO, IP6TYP, IDUM )
            IF ((IP6TYP == 'COMP NOS') .OR. (IP6TYP == 'ZERO    ') .OR. (IP6TYP == 'BLANK   ')) THEN
               CALL I4FLD ( JCARDO, JF(3*I+1), COMPJ )          
            ELSE
               JERR      = JERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1124) JF(3*I+1),JCARD(1),JCARD(2),JF(3*I+1),JCARD(3*I+1)
               WRITE(F06,1124) JF(3*I+1),JCARD(1),JCARD(2),JF(3*I+1),JCARD(3*I+1)
            ENDIF

            CALL R8FLD ( JCARD(3*I+2), JF(3*I+2), RSPCJ )  ! Read permanent SPC
 
            IF (DABS(RSPCJ) > DEPS1) THEN                  ! SPC are SE set if enforced displ > 0, SB set if = 0 
               DOFSET = 'SE'
            ELSE
               DOFSET = 'SB'
            ENDIF
 
            IF ((JERR == 0 ) .AND. (IERRFL(3*I) == 'N') .AND. (IERRFL(3*I+1) == 'N') .AND. (IERRFL(3*I+2) == 'N')) THEN
               NUM_SPC_RECORDS = NUM_SPC_RECORDS + 1       ! Incr count of number of entries written to file LINK1O
               WRITE(L1O) SETID,COMPJ,GRIDJ,GRIDJ,RSPCJ,DOFSET
            ENDIF

         ELSE                                              ! Field 3 or 6 is blank
 
            IF ((JCARD(3*I+1)(1:) /= ' ') .OR. (JCARD(3*I+2)(1:) /= ' ')) THEN
               WARN_ERR = WARN_ERR + 1
               WRITE(ERR,101) CARD
               WRITE(ERR,1144) JCARD(1),JCARD(2),JF(3*I+1),JF(3*I+2),JF(3*I)
               IF (SUPWARN == 'N') THEN
                  IF (ECHO == 'NONE  ') THEN
                     WRITE(F06,101) CARD
                  ENDIF
                  WRITE(F06,1144) JCARD(1),JCARD(2),JF(3*I+1),JF(3*I+2),JF(3*I)
               ENDIF
            ENDIF

         ENDIF

      ENDDO
  
      CALL BD_IMBEDDED_BLANK   ( JCARD,2,3,0,5,6,0,8,0 )   ! Make sure that there are no imbedded blanks in fields 2,3,5,6,8,9
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,0,9 )
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

 1144 FORMAT(' *WARNING    : ON ',A,' SET ID = ',A,' FIELDS ',I3,' AND ',I3,' ARE IGNORED SINCE GRID FIELD ',I3,' IS BLANK.')

 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_SPC
