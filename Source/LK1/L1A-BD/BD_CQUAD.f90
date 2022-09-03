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
  
      SUBROUTINE BD_CQUAD ( CARD, LARGE_FLD_INP, NUM_GRD )
  
! Processes CQUADi, CQDPLTi, CQDMEMi Bulk Data Cards
!  1) Sets ETYPE for this element type
!  2) Calls subr ELEPRO to read element ID, property ID and connection data into array EDAT

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, IERRFL, FATAL_ERR, JCARD_LEN, JF, LMATANGLE, LPLATEOFF, LPLATETHICK,        &
                                         MEDAT_CQUAD, NCQUAD4K, NCQUAD4, NEDAT, NELE, NMATANGLE, NPLATEOFF, NPLATETHICK
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_CQUAD_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  EDAT, ETYPE, MATANGLE, PLATEOFF, PLATETHICK
 
      USE BD_CQUAD_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_CQUAD'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: JCARD_EDAT(10)    ! JCARD values sent to subr ELEPRO
      CHARACTER( 8*BYTE)              :: TOKEN             ! The 1st 8 characters from a JCARD
      CHARACTER( 8*BYTE)              :: TOKTYP            ! Indicator of the type of val found in a B.D. field (e.g. int, real,...)

      INTEGER(LONG), INTENT(OUT)      :: NUM_GRD           ! Number of GRID's + SPOINT's for the elem
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: I4INP             ! A value read from input file that should be an integer
      INTEGER(LONG)                   :: INT41,INT42        ! An integer used in getting MATANGLE
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_CQUAD_BEGEND
 
      REAL(DOUBLE)                    :: R8INP     = ZERO  ! A value read from input file that should be a real value

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! CQUADi element Bulk Data Card routine
 
!   FIELD   ITEM           ARRAY ELEMENT
!   -----   ------------   -------------
!    1      Element type   ETYPE(nele) = Q1, Q2, Q3, QA, QB
!    2      Element ID     EDAT(nedat+1)
!    3      Property ID    EDAT(nedat+2)
!    4      Grid A         EDAT(nedat+3)
!    5      Grid B         EDAT(nedat+4)
!    6      Grid C         EDAT(nedat+5)
!    7      Grid D         EDAT(nedat+6)
!    8      Matl angle     NMATANGLE (MATANGLE key) goes in EDAT(nedat+7)
!    9      Offset         NPLATEOFF (PLATEOFF key) goes in EDAT(nedat+8)
!                          EDAT(nedat+9)
! on optional second card:
!   4-7     Ti             Membrane thicknes at grids 1-4. These will go into array PLATETHICK
!                          EDAT(nedat+10) will hold NPLATETHICK the row in PLATETHICK where Ti is located
 
! Make JCARD from CARD

      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Set JCARD_EDAT to JCARD

      DO I=1,10
         JCARD_EDAT(I) = JCARD(I)
      ENDDO 

! Check property ID field. Set to element ID if blank
  
      IF (JCARD(3)(1:) == ' ') THEN
         JCARD_EDAT(3) = JCARD(2)
      ENDIF

! Read and check data
                                                           ! Load 6 items into EDAT
      CALL ELEPRO ( 'Y', JCARD_EDAT, 6, MEDAT_CQUAD, 'Y', 'Y', 'Y', 'Y', 'Y', 'Y', 'N', 'N' )
 
      NUM_GRD = 4
      IF       (JCARD(1)(1:7) == 'CQUAD4K') THEN
         NCQUAD4K = NCQUAD4K + 1
         ETYPE(NELE) = 'QUAD4K  '
      ELSE IF ((JCARD(1)(1:7) == 'CQUAD4 ') .OR. (JCARD(1)(1:7) == 'CQUAD4*')) THEN
         NCQUAD4 = NCQUAD4 + 1
         ETYPE(NELE) = 'QUAD4   '
      ENDIF
 
! Read material property orientation angle. It takes 2 values put into EDAT to cover all of the possibilities of field 8:
!  (a) If field 8 is a real value it is the angle of the material axis relative to the element x axis. 
!         (1) the 2 values to put into EDAT are: the value in field 8 (this will be the row in MATANGLE to get the angle), and a 0
!  (b) If field 8 is an integer it means that the angle is identified by a coord system.
!         (2) if field 8 is a positive number the 2 values to put into EDAT are: the neg of field 8 integer and a 2
!         (3) if field 8 is an actual 0 (basic coodr sys) the 2 values to put into EDAT are: the neg of field 8 integer and a 1
!  (c) If field 8 is blank:
!         (4) put into EDAT the 2 values: 0 and 0

      IF (JCARD(8)(1:) /= ' ') THEN                        ! Matl angle field has something in it so there is a angle defined

         TOKEN = JCARD(8)(1:8)                             ! Only send the 1st 8 chars of this JCARD. It has been left justified
         CALL TOKCHK ( TOKEN, TOKTYP )
         IF      ((TOKTYP /= 'INTEGER ') .AND. (TOKTYP /= 'FL PT   ')) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE (ERR,1196) JCARD(1), JCARD(2), JCARD(8)
            WRITE (F06,1196) JCARD(1), JCARD(2), JCARD(8)

         ELSE IF (TOKTYP == 'FL PT   ') THEN               ! -- Set keys based on "FL PT" number in material angle field
            NMATANGLE = NMATANGLE + 1
            IF (NMATANGLE > LMATANGLE) THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1141) SUBR_NAME,LMATANGLE
               WRITE(F06,1141) SUBR_NAME,LMATANGLE
               CALL OUTA_HERE ( 'Y' )
            ENDIF
            INT41 = NMATANGLE                              ! ---- Mtrl angle key is row in MATANGLE
            INT42 = 0                                      ! ---- Basic or not key (0 is undefined)
            CALL R8FLD ( JCARD(8), JF(8), R8INP )
            IF (IERRFL(8) == 'N') THEN
               MATANGLE(NMATANGLE) = R8INP
            ENDIF

         ELSE IF (TOKTYP == 'INTEGER ') THEN               ! -- Set keys for "INTEGER" number in material angle field
            CALL I4FLD ( JCARD(8), JF(8), I4INP )
            INT41 = -I4INP                                 ! ---- Mtrl angle key is set to neg of the coord sys ID
            IF      (I4INP > 0) THEN
               INT42 = 2                                   ! -------- If field was > 0 set "Basic or not" key to 2
            ELSE IF (I4INP == 0) THEN
               INT42 = 1                                   ! -------- If field was = 0 set "Basic or not" key to 1
            ELSE IF (I4INP < 0) THEN
               INT42 = 0                                   ! -------- If field was < 0 then error
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1197) JCARD(1), JCARD(2), JF(8), JCARD(8)
               WRITE(F06,1197) JCARD(1), JCARD(2), JF(8), JCARD(8)
            ENDIF

         ENDIF

      ELSE                                                 ! Matl angle field is blank so no angle specified

         INT41 = 0
         INT42 = 0

      ENDIF

      NEDAT = NEDAT + 1
      EDAT(NEDAT) = INT41
      NEDAT = NEDAT + 1
      EDAT(NEDAT) = INT42

! Read plate offset

      IF (JCARD(9)(1:) /= ' ') THEN                        ! 8: Load plate offset key (or 0 if none) into EDAT
         NPLATEOFF = NPLATEOFF + 1
         IF (NPLATEOFF > LPLATEOFF) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1144) SUBR_NAME,' TOO MANY PLATE OFFSETS. LIMIT IS NPLATEOFF =  ',LPLATEOFF
            WRITE(F06,1144) SUBR_NAME,' TOO MANY PLATE OFFSETS. LIMIT IS NPLATEOFF =  ',LPLATEOFF
            CALL OUTA_HERE ( 'Y' )
         ENDIF
         NEDAT = NEDAT + 1
         EDAT(NEDAT) = NPLATEOFF
         CALL R8FLD ( JCARD(9), JF(9), R8INP )
         IF (IERRFL(9) == 'N') THEN
            PLATEOFF(NPLATEOFF) = R8INP
         ENDIF
      ELSE

         NEDAT = NEDAT + 1 
         EDAT(NEDAT) = 0

      ENDIF

! Load a 0 into EDAT as a flag for whether this element references a PSHELL or a PCOMP.
! This will be decided in subr ELEM_PROP_MATL_IIDS when the actual PID in EDAT is converted to an internal PID.

      NEDAT = NEDAT + 1                                    ! 8: PSHELL/PCOMP flag (to be set in subr ELEM_PROP_MATL_IIDS)
      EDAT(NEDAT) = 0

! Load a 0 into EDAT as a flag for whether this element has thicknesses defined on a continuation entry

      NEDAT = NEDAT + 1                                    ! 8: PSHELL/PCOMP flag (to be set in subr ELEM_PROP_MATL_IIDS)
      EDAT(NEDAT) = 0

      CALL BD_IMBEDDED_BLANK   ( JCARD,2,3,4,5,6,7,8,9 )   ! Make sure that there are no imbedded blanks in fields 2-9
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
  
! Optional second card (to define membrane thicknesses as grid values):

      IF (LARGE_FLD_INP == 'N') THEN
         CALL NEXTC  ( CARD, ICONT, IERR )
      ELSE
         CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
         CARD = CHILD
      ENDIF
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      IF (ICONT == 1) THEN                                 ! Since there is a cont entry we assume Ti are here

         IF (CARD(1:) /= ' ') THEN                         ! Only process continuation entries if 1st one is not totally blank

            EDAT(NEDAT) = NPLATETHICK + 1

            DO J=4,7                                       ! Read 4 thicknesses
               NPLATETHICK = NPLATETHICK + 1
               IF (NPLATETHICK > LPLATETHICK) THEN
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1144) SUBR_NAME,' TOO MANY PLATE THICKNESSES. LIMIT IS NPLATETHICK = ',LPLATETHICK
                  WRITE(F06,1144) SUBR_NAME,' TOO MANY PLATE THICKNESSES. LIMIT IS NPLATETHICK = ',LPLATETHICK
                  CALL OUTA_HERE ( 'Y' )
               ENDIF
               CALL R8FLD ( JCARD(J), JF(J), R8INP )
               IF (IERRFL(J) == 'N') THEN
                  PLATETHICK(NPLATETHICK) = R8INP
               ENDIF
            ENDDO

            CALL BD_IMBEDDED_BLANK ( JCARD,0,0,4,5,6,7,0,0 )
            CALL CARD_FLDS_NOT_BLANK ( JCARD,2,3,0,0,0,0,8,9 )
            CALL CRDERR ( CARD )

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
 1141 FORMAT(' *ERROR  1141: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY PLATE ELEMENT MATERIAL PROPERTY ANGLES. LIMIT IS NMATANGLE =  ',I8) 

 1144 FORMAT(' *ERROR  1144: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,A,I8) 

 1196 FORMAT(' *ERROR  1196: VALUE FOR MATERIAL ANGLE ON ',A,A,' MUST BE AN INTEGER OR REAL NUMBER BUT VALUE READ WAS ',A)

 1197 FORMAT(' *ERROR  1197: FOR ',A,A,' THE COORD SYS ID IN FIELD ',I2,' MUST BE >= 0. HOWEVER, THE VALUE INPUT WAS ',A)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_CQUAD
