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
  
      SUBROUTINE BD_CUSERIN ( CARD, LARGE_FLD_INP, NG, NS )
  
! Processes CUSERIN Bulk Data Cards
!  1) Sets ETYPE for this element type
!  2) Calls subr ELEPRO to read element ID, property ID and connection data into array EDAT
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LGUSERIN, LSUSERIN, MEDAT0_CUSERIN,       &
                                         NCUSERIN, NEDAT, NELE, WARN_ERR 
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_CUSERIN_BEGEND
      USE MODEL_STUF, ONLY            :  EDAT, ETYPE
 
      USE BD_CUSERIN_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_CUSERIN'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER( 1*BYTE)              :: CID0_WARN         ! 'Y' if CID0 field is blank or 0
      CHARACTER(LEN=JCARD_LEN)        :: ID                ! Element ID (field 2 of parent card)
      CHARACTER( 8*BYTE)              :: IP6TYP            ! An output from subr IP6CHK called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN=JCARD_LEN)        :: JCARDO            ! An output from subr IP6CHK called herein
      CHARACTER(LEN=JCARD_LEN)        :: NAME              ! JCARD(1) from parent entry
      CHARACTER( 8*BYTE)              :: TOKEN             ! The 1st 8 characters from a JCARD
      CHARACTER( 8*BYTE)              :: TOKTYP            ! An output from subr TOKCHK called herein
 
      INTEGER(LONG), INTENT(OUT)      :: NG                ! Number of GRID's for the elem as defined on parent card field 5
      INTEGER(LONG), INTENT(OUT)      :: NS                ! Number of SPOINT's for the elem as defined on parent card field 5
      INTEGER(LONG)                   :: FIELDS_NOT_BLANK  ! Indicator of problem with fields not being blank on CARD
      INTEGER(LONG)                   :: I4INP             ! Integer value read from card
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IDOF              ! Displ component (1,2,3,4,5 or 6) that one end of CELSA conn. to
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: JERR              ! Local error count
      INTEGER(LONG)                   :: NG_FOUND          ! Total number of grids found on cont cards
      INTEGER(LONG)                   :: NS_FOUND          ! Total number of SPOINT's found on cont cards
      INTEGER(LONG)                   :: NUM_COMPS         ! Number of displ comps for a grid for this elem
      INTEGER(LONG)                   :: NUM_BDY_DOF       ! Total num of DOF for this elem (sum of all NUM_COMPS; NOT elem ELDOF)
      INTEGER(LONG)                   :: SPOINT1   = 0     ! An SPOINT number
      INTEGER(LONG)                   :: SPOINT2   = 0     ! An SPOINT number

                                                           ! Array of grids on the CUSERIN entry (not incl SPOINT's)
      INTEGER(LONG)                   :: USERIN_GRIDS(LGUSERIN)

                                                           ! Array of SPOINT's on the CUSERIN entry
      INTEGER(LONG)                   :: USERIN_SPOINTS(LSUSERIN)

                                                           ! Array of displ components on the CUSERIN entry (for USERIN_GRIDS)
      INTEGER(LONG)                   :: USERIN_COMPS(LGUSERIN)

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_CUSERIN_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! CUSERIN element Bulk Data Card routine
 
!   FIELD         ITEM                  ARRAY ELEMENT
!   -----   ----------------         -----------------
!    1      Elem type                 ETYPE(nele) = 'USERIN  '
!    2      ELID, Element ID          EDAT(nedat+1)
!    3      PID, "Prop" ID            EDAT(nedat+2)
!    4      NG , Num GRID's           EDAT(nedat+3)
!    5      NS , Num SPOINT's         EDAT(nedat+4)
!    6      CID0, basic coord sys ID  EDAT(nedat+5)
 
! Cont cards defining NG GRIDCOMP pairs
!    2-9    NG GRID/COMP pairs        EDAT(nedat+?)

! Cont cards defining NS SPOINT's
! Format #1:
!   2-9     SPOINT ID's
! on optional continuation cards:
!   2-9     Grid ID's
 
! Format #2:
!    2      SPOINT ID 1
!    3      "THRU"
!    4      SPOINT ID 2
 

! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )

      NAME = JCARD(1)
      ID   = JCARD(2)
 
! Read fields 4 and 5 since we need the values of NG, NS below

      NG = 0
      CALL I4FLD ( JCARD(4), JF(4), I4INP )
      IF (IERRFL(4) == 'N') THEN
         NG = I4INP
      ENDIF

      NS = 0
      CALL I4FLD ( JCARD(5), JF(5), I4INP )
      IF (IERRFL(4) == 'N') THEN
         NS = I4INP
      ENDIF

! Read and check data on parent entry. 1st arg in call to ELEPRO says to increment NELE

      CALL ELEPRO ( 'Y', JCARD, MEDAT0_CUSERIN, MEDAT0_CUSERIN, 'Y', 'Y', 'N', 'N', 'N', 'Y', 'Y', 'Y' )
      NCUSERIN = NCUSERIN + 1
      ETYPE(NELE) = 'USERIN  '

      CALL BD_IMBEDDED_BLANK   ( JCARD,2,3,4,5,6,0,0,0 )   ! Make sure that there are no imbedded blanks in fields 2-4
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,7,8,9 )   ! Issue warning if fields 7-9 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

      CID0_WARN = 'N'
      CALL I4FLD ( JCARD(6), JF(6), I4INP )
      IF (IERRFL(6) == 'N') THEN
         IF (I4INP == 0) THEN
            CID0_WARN = 'Y'
         ENDIF
      ENDIF

      NUM_BDY_DOF = 0

! **********************************************************************************************************************************
! Continuation cards for grids/components:

      NG_FOUND = 0
do_i1:DO WHILE (NG_FOUND < NG)

         IF (LARGE_FLD_INP == 'N') THEN
            CALL NEXTC  ( CARD, ICONT, IERR )
         ELSE
            CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
            CARD = CHILD
         ENDIF
         CALL MKJCARD ( SUBR_NAME, CARD, JCARD )

         IF (ICONT == 1) THEN

do_j1:      DO J=1,4

               IF (JCARD(2*J)(1:) /= ' ') THEN             ! There is a grid number in field 2*J (fields 2, 4, 6 or 8)

                  NG_FOUND = NG_FOUND + 1

                  USERIN_GRIDS(NG_FOUND) = 0
                  CALL I4FLD ( JCARD(2*J), JF(2*J), I4INP )
                  IF (IERRFL(2*J) == 'N') THEN
                     USERIN_GRIDS(NG_FOUND) = I4INP
                  ENDIF

                  USERIN_COMPS(NG_FOUND) = 0
                  CALL IP6CHK ( JCARD(2*J+1), JCARDO, IP6TYP, NUM_COMPS )
                  NUM_BDY_DOF = NUM_BDY_DOF + NUM_COMPS
                  CALL I4FLD ( JCARDO, JF(2*J+1), IDOF )
                  IF (IP6TYP /= 'COMP NOS') THEN
                     FATAL_ERR = FATAL_ERR + 1
                     WRITE(ERR,1124) JF(2*J+1), NAME, ID, JF(2*J+1), JCARD(2*J+1)
                     WRITE(F06,1124) JF(2*J+1), NAME, ID, JF(2*J+1), JCARD(2*J+1)
                  ELSE
                     USERIN_COMPS(NG_FOUND) = IDOF
                  ENDIF

               ELSE                                        ! Field 2*J is blank

                  IF (NG_FOUND < NG) THEN                  ! --- If we haven't found all of the grids (specified in field 4
                     FATAL_ERR = FATAL_ERR+1               ! --- of parent) then error
                     WRITE(ERR,1149) NAME, ID, NG, ' GRIDs ', NG_FOUND
                     WRITE(F06,1149) NAME, ID, NG, ' GRIDs ', NG_FOUND
                  ENDIF

                  FIELDS_NOT_BLANK = 0                     ! Make sure there are no blank fields between fields of data
                  IF      (J == 1) THEN
                     IF (CARD(  JCARD_LEN+1:9*JCARD_LEN) /= ' ') THEN
                        FIELDS_NOT_BLANK = 2
                     ENDIF
                  ELSE IF (J == 2) THEN
                     IF (CARD(3*JCARD_LEN+1:9*JCARD_LEN) /= ' ') THEN
                        FIELDS_NOT_BLANK = 4
                     ENDIF
                  ELSE IF (J == 3) THEN
                     IF (CARD(5*JCARD_LEN+1:9*JCARD_LEN) /= ' ') THEN
                        FIELDS_NOT_BLANK = 6
                     ENDIF
                  ELSE IF (J == 4) THEN
                     IF (CARD(7*JCARD_LEN+1:9*JCARD_LEN) /= ' ') THEN
                        FIELDS_NOT_BLANK = 8
                     ENDIF
                  ENDIF
                  IF (FIELDS_NOT_BLANK /= 0) THEN
                     FATAL_ERR = FATAL_ERR + 1
                     WRITE(ERR,1150) NAME, ID, FIELDS_NOT_BLANK
                     WRITE(F06,1150) NAME, ID, FIELDS_NOT_BLANK
                  ENDIF

                  EXIT do_i1

               ENDIF

            ENDDO do_j1

            CALL CRDERR ( CARD )

            IF (NG_FOUND > NG) THEN
               FATAL_ERR = FATAL_ERR+1
               WRITE(ERR,1149) NAME, ID, NG, ' GRIDs ', NG_FOUND
               WRITE(F06,1149) NAME, ID, NG, ' GRIDs ', NG_FOUND
            ENDIF

         ELSE

            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1149) NAME, ID, NG, ' GRIDs ', NG_FOUND
            WRITE(F06,1149) NAME, ID, NG, ' GRIDs ', NG_FOUND
            EXIT do_i1

         ENDIF

      ENDDO do_i1

      IF (NG_FOUND /= NG) THEN
         FATAL_ERR = FATAL_ERR+1
         WRITE(ERR,1149) NAME, ID, NG, ' GRIDs ', NG_FOUND
         WRITE(F06,1149) NAME, ID, NG, ' GRIDs ', NG_FOUND
      ENDIF

! **********************************************************************************************************************************
! Continuation cards for SPOINT's:

      NS_FOUND = 0
spnts:IF (NS > 0) THEN

         IF (LARGE_FLD_INP == 'N') THEN
            CALL NEXTC  ( CARD, ICONT, IERR )
         ELSE
            CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
            CARD = CHILD
         ENDIF
         CALL MKJCARD ( SUBR_NAME, CARD, JCARD )

cont:    IF (ICONT == 1) THEN

            TOKEN = JCARD(3)(1:8)                          ! Only send the 1st 8 chars of this JCARD. It has been left justified
            CALL TOKCHK ( TOKEN, TOKTYP )                  ! TOKTYP must be THRU', 'INTEGR', or 'BLANK'

tok:        IF (TOKTYP == 'THRU    ') THEN                      

               JERR = 0

               IF (JCARD(2)(1:) /= ' ') THEN               ! Get 1st SPOINT ID
                  CALL I4FLD ( JCARD(2), JF(2), SPOINT1 )
               ELSE            
                  JERR      = JERR + 1
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1125) 'SCALAR POINT', JF(2), JCARD(1)
                  WRITE(F06,1125) 'SCALAR POINT', JF(2), JCARD(1)
               ENDIF
 
               IF (JCARD(4)(1:) /= ' ') THEN            ! Get 2nd SPOINT ID
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
                     NS_FOUND = NS_FOUND + 1
                     USERIN_SPOINTS(NS_FOUND) = SPOINT1 + J - 1
                  ENDDO
               ENDIF
 
               IF (NS_FOUND /= NS) THEN                    ! make sure we found the correct amount of SPOINT's
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1149) NAME, ID, NS, ' SPOINTs ', NS_FOUND
                  WRITE(F06,1149) NAME, ID, NS, ' SPOINTs ', NS_FOUND
               ENDIF

               CALL CRDERR ( CARD )

            ELSE                                           ! TOKTYP was not "THRU"

               NS_FOUND = 0
do_i2:         DO WHILE (NS_FOUND < NS)

                  IF (ICONT == 1) THEN

                     DO J=2,9
                        IF (JCARD(J)(1:) /= ' ') THEN
                           NS_FOUND = NS_FOUND + 1
                           USERIN_SPOINTS(NS_FOUND) = 0
                           CALL I4FLD ( JCARD(J), JF(J), I4INP )
                           IF (IERRFL(J) == 'N') THEN
                              USERIN_SPOINTS(NS_FOUND) = I4INP
                           ENDIF
                        ENDIF
                     ENDDO

                     IF (NS_FOUND > NS) THEN
                        FATAL_ERR = FATAL_ERR+1
                        WRITE(ERR,1149) NAME, ID, NS, ' SPOINTs ', NS_FOUND
                        WRITE(F06,1149) NAME, ID, NS, ' SPOINTs ', NS_FOUND
                     ENDIF

                  ELSE

                     FATAL_ERR = FATAL_ERR + 1
                     WRITE(ERR,1149) NAME, ID, NS, ' SPOINTs ', NS_FOUND
                     WRITE(F06,1149) NAME, ID, NS, ' SPOINTs ', NS_FOUND
                     EXIT do_i2

                  ENDIF

                  IF (LARGE_FLD_INP == 'N') THEN
                     CALL NEXTC  ( CARD, ICONT, IERR )
                  ELSE
                     CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
                     CARD = CHILD
                  ENDIF
                  CALL MKJCARD ( SUBR_NAME, CARD, JCARD )

               ENDDO do_i2

            ENDIF tok

         ELSE

            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1136) NAME, ID
            WRITE(F06,1136) NAME, ID

         ENDIF cont

      ENDIF spnts

! Check CID0 and give warning if 0 (CID0 is in field 6 of parent entry and was read above)

      IF (CID0_WARN == 'Y') THEN
         WARN_ERR = WARN_ERR + 1
         WRITE(ERR,8999) NAME, ID, JF(6), NAME(2:)
         IF (SUPWARN == 'N') THEN
            WRITE(F06,8999) NAME, ID, JF(6), NAME(2:)
         ENDIF
      ENDIF

! **********************************************************************************************************************************



      DO J=1,NG_FOUND                                      ! Load USERIN_GRIDS   into array EDAT
         NEDAT = NEDAT + 1
         EDAT(NEDAT) = USERIN_GRIDS(J)
      ENDDO

      DO J=1,NS_FOUND                                      ! Load USERIN_SPOINTS into array EDAT
         NEDAT = NEDAT + 1
         EDAT(NEDAT) = USERIN_SPOINTS(J)
      ENDDO

      DO J=1,NG_FOUND                                      ! Load USERIN_COMPS   into array EDAT
         NEDAT = NEDAT + 1
         EDAT(NEDAT) = USERIN_COMPS(J)
      ENDDO

      NEDAT = NEDAT + 1                                    ! Load elem number of boundary DOF's into last word in EDAT
      EDAT(NEDAT) = NUM_BDY_DOF


! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1124 FORMAT(' *ERROR  1124: INVALID DOF NUMBER IN FIELD ',I3,' ON ',A,' ENTRY WITH ID = ',A                                       &
                    ,/,14X,' MUST BE A COMBINATION OF DIGITS 1-6. HOWEVER, FIELD ',I3, ' HAS: "',A,'"')

 1125 FORMAT(' *ERROR  1125: NO ',A,' SPECIFIED IN FIELD',I4,' ON ',A,' CARD')

 1128 FORMAT(' *ERROR  1128: ON ',A,' THE IDs MUST BE IN INCREASING ORDER FOR THRU OPTION')
 
 1136 FORMAT(' *ERROR  1136: REQUIRED CONTINUATION FOR ',A,' ID = ',A,' MISSING')
 
 1149 FORMAT(' *ERROR  1149: ',A,A,' HAS ',I8,A,' SPECIFIED ON THE PARENT ENTRY BUT ',I8,' HAVE BEEN FOUND ON CONTINUATION ENTRIES')

 1150 FORMAT(' *ERROR  1150: ',A,A,' HAS BLANK FIELDS BETWEEN FIELDS OF DATA FROM FIELD ',I2,' TO FIELD 9. NOT ALLOWED')

 8999 FORMAT(' *WARNING    : ',A,A,' HAS COORD SYSTEM ID (FIELD ',I2,' ON PARENT ENTRY) FOR THE BASIC SYSTEM OF THIS ELEMENT = 0.' &
                      ,/,14X,' THIS MEANS THAT THE BASIC COORD SYSTEM WHEN THAT ',A,' ELEM WAS GENERATED IS THE SAME AS, AND',     &
                             ' ALIGNED WITH, THE'                                                                                  &
                      ,/,14X,' BASIC COORD SYSTEM FOR THIS OVERALL MODEL USING IT')





! **********************************************************************************************************************************
 
      END SUBROUTINE BD_CUSERIN
