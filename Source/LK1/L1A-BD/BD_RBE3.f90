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
  
      SUBROUTINE BD_RBE3 ( CARD, LARGE_FLD_INP )
  
! Processes RBE3 Bulk Data Cards. Writes RBE3 card data to file L1F
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1F
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LRIGEL, MRBE3, NRECARD, NRIGEL
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_RBE3_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  RIGID_ELEM_IDS
 
      USE BD_RBE3_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_RBE3'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER( 1*BYTE)              :: CDOF(6)           ! An output from subr RDOF
      CHARACTER(LEN(JCARD))           :: CHRINP            ! An 8 character field returned from subr IP6CHK
      CHARACTER(LEN(JCARD))           :: CHAR_ELID         ! Char value for element ID in field 2 of parent entry
      CHARACTER( 1*BYTE)              :: FND_NEW_WGT       ! 'Y' if a weight entry has been found
      CHARACTER( 8*BYTE)              :: IP6TYP            ! Descriptor of what is in the 8 char field sent to subr IP6CHK
      CHARACTER( 1*BYTE)              :: NEXT_MUST_BE_GRID ! 'Y' if next field must be grid (i.e. need grid after wgt and comp)
      CHARACTER( 8*BYTE), PARAMETER   :: RTYPE = 'RBE3    '! Rigid element type
      CHARACTER( 8*BYTE)              :: TOKEN             ! The 1st 8 characters from a JCARD
      CHARACTER( 8*BYTE)              :: TOKTYP            ! Type of TOKEN returned from subr TOKCHK
 
      INTEGER(LONG)                   :: CHK(2:9)          ! Array to tell which fields to check for imbedded blankS
      INTEGER(LONG)                   :: COMP(MRBE3)       ! Array of indep displ comp values (1-6) found on this logical RBE3 card
      INTEGER(LONG)                   :: GRID(MRBE3)       ! Array of indep GRID ID's found on this logical RBE3 card
      INTEGER(LONG)                   :: GRID1     = 0     ! One grid value
      INTEGER(LONG)                   :: I4INP     = 0     ! A value read from input file that should be an integer value
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: INTDOF    = 0     ! Displ component numbers from a Ci field
      INTEGER(LONG)                   :: IRBE3             ! Count of triplets of indep grid/comp/weights read
      INTEGER(LONG)                   :: JERR      = 0     ! A local error count
      INTEGER(LONG)                   :: NUM_Ci            ! Number of displ components in a Ci field
      INTEGER(LONG)                   :: REFC_NUM_Ci       ! Number of displ components in REFC field
      INTEGER(LONG)                   :: REFC      = 0     ! REFC value in field 5 of parent entry
      INTEGER(LONG)                   :: REFGRID   = 0     ! REFGRID value in field 4 of parent entry
      INTEGER(LONG)                   :: RELID     = 0     ! This elements' ID
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_RBE3_BEGEND
 
      REAL(DOUBLE)                    :: R8INP             ! A real value read from a field on this RBE3 entry
      REAL(DOUBLE)                    :: WGT               ! A weight read from a field on this RBE3 entry
      REAL(DOUBLE)                    :: WTi(MRBE3)        ! Array of RBE3 weight values
      REAL(DOUBLE)                    :: WT_TOT            ! Total of all WTi(i)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! RBE3 Bulk Data Card:
 
!   FIELD   ITEM            EXPLANATION 
!   -----   ------------    -------------
!    2      ELID            RBE3 elem ID
!    3      blank
!    4      REFGRID         Ref grid point ID (this grid will go into M-set)
!    5      REFC            Ref component number (1-6) (comp of REFGRID) 
!    6      WT1             Weighting factor for 1st component of displ
!    7      C1              Comp number for associated with WT1 
!    8      G1,1            Grid associated with WT1
!    9      G1,2 or next WTi or blank

! on optional continuation entries:
!    2-9    contain more of the same: weights, components, grida


! Subsequent entries have the same format where a WTi is specified followed by a displ component Ci followed by a list of grids that
! have the WTi for that component
 
      JERR = 0

! Initialize arrays

      DO J=1,MRBE3
         GRID(J) = 0
         COMP(J) = 0
         WTi(J)  = ZERO
      ENDDO
      WT_TOT = ZERO 

      TOKTYP(1:) = ' '

      IRBE3 = 0

! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Up the elem count

      NRIGEL = NRIGEL+1
 
! Read and check data on parent entry
 
      CHAR_ELID = JCARD(2)
      CALL I4FLD ( JCARD(2), JF(2), I4INP )                ! Field 2: Elem ID
      IF (IERRFL(2) == 'N') THEN
         RELID                   = I4INP
         RIGID_ELEM_IDS(NRIGEL) = RELID
      ELSE
         JERR = JERR + 1
      ENDIF

      CALL I4FLD ( JCARD(4), JF(4), I4INP )                ! Field 4: REFGRID
      IF (IERRFL(4) == 'N') THEN
         REFGRID = I4INP
      ELSE
         JERR = JERR + 1
      ENDIF

      CALL I4FLD ( JCARD(5), JF(5), I4INP )                ! Field 5: REFC
      IF (IERRFL(5) == 'N') THEN
         CALL IP6CHK ( JCARD(5), CHRINP, IP6TYP, REFC_NUM_Ci )
         IF (IP6TYP(1:8) == 'COMP NOS') THEN
            REFC = I4INP
         ELSE
            JERR = JERR + 1
            WRITE(ERR,1124) JF(5), 'RBE3', CHAR_ELID, JF(5), JCARD(5)
            WRITE(F06,1124) JF(5), 'RBE3', CHAR_ELID, JF(5), JCARD(5)
         ENDIF
      ELSE
         JERR = JERR + 1
      ENDIF

      WGT = ZERO
      CALL R8FLD ( JCARD(6), JF(6), R8INP )                ! Field 6: 1st weight
      IF (IERRFL(6) == 'N') THEN
         WGT = R8INP
      ELSE
         JERR = JERR + 1
      ENDIF

      NUM_Ci = 0                                           ! Field 7: 1st field with Ci components
      INTDOF = 0
      CALL I4FLD ( JCARD(7), JF(7), I4INP )
      IF (IERRFL(7) == 'N') THEN
         CALL IP6CHK ( JCARD(7), CHRINP, IP6TYP, NUM_Ci )
         IF (IP6TYP(1:8) == 'COMP NOS') THEN
            INTDOF = I4INP
         ELSE
            JERR = JERR + 1
            WRITE(ERR,1124) JF(7), 'RBE3', CHAR_ELID, JF(7), JCARD(7)
            WRITE(F06,1124) JF(7), 'RBE3', CHAR_ELID, JF(7), JCARD(7)
         ENDIF
      ELSE
         JERR = JERR + 1
      ENDIF

      NEXT_MUST_BE_GRID = 'Y'
      GRID1 = 0                                            ! Field 8: 1st grid                
      CALL I4FLD ( JCARD(8), JF(8), I4INP )
      IF (IERRFL(8) == 'N') THEN
         GRID1 = I4INP
      ELSE
         JERR = JERR + 1
      ENDIF
      NEXT_MUST_BE_GRID = 'N'

      IRBE3 = IRBE3 + 1                                    ! Write 1st set of wgt/comp/indep grid to arrays
      IF (IRBE3 > MRBE3) CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, IRBE3, 'WTi' )
      WTi(IRBE3)  = WGT
      WT_TOT = WT_TOT + WTi(IRBE3)
      COMP(IRBE3) = INTDOF
      GRID(IRBE3) = GRID1
                                                           ! Field 9: if not blank then it must be a new weight or another grid
      FND_NEW_WGT = 'N'                                    ! that uses the previously read WT1 and C1
      TOKEN = JCARD(9)(1:8)                                ! Only send the 1st 8 chars of this JCARD. It has been left justified
      CALL TOKCHK ( TOKEN, TOKTYP )
      IF (TOKTYP /= 'BLANK   ') THEN
         IF      (TOKTYP == 'FL PT   ') THEN
            CALL R8FLD ( JCARD(9), JF(9), R8INP )
            IF (IERRFL(9) == 'N') THEN
               WGT         = R8INP
               FND_NEW_WGT = 'Y'
            ELSE
               JERR = JERR + 1
            ENDIF
         ELSE IF (TOKTYP == 'INTEGER ') THEN               ! Since it is integer, assume a grid and incr IRBE3 and write to arrays
            CALL I4FLD ( JCARD(9), JF(9), I4INP )
            IRBE3 = IRBE3 + 1
            GRID(IRBE3) = 0
            IF (IRBE3 > MRBE3) CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, IRBE3, 'WTi' )
            WTi(IRBE3)  = WGT
            WT_TOT = WT_TOT + WTi(IRBE3)
            COMP(IRBE3) = INTDOF
            IF (IERRFL(8) == 'N') THEN
               GRID(IRBE3) = I4INP
            ELSE
               JERR = JERR + 1
            ENDIF
         ELSE
            JERR = JERR + 1
            WRITE(ERR,1151) '9', 'RBE3', CHAR_ELID, JCARD(9)
            WRITE(F06,1151) '9', 'RBE3', CHAR_ELID, JCARD(9)
         ENDIF
      ENDIF

      CALL BD_IMBEDDED_BLANK ( JCARD,2,0,4,0,6,0,8,9 )     ! Make sure there are no imbedded blanks (except 3,5,7)
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,3,0,0,0,0,0,0 )   ! Issue warning if field 3 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

! Read and check data on optional continuation entries

      I = 0
      DO

         IF (LARGE_FLD_INP == 'N') THEN
            CALL NEXTC  ( CARD, ICONT, IERR )
         ELSE
            CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
            CARD = CHILD
         ENDIF
         CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
         IF (ICONT == 1) THEN

            I = I + 1

            DO J=2,9
               CHK(J) = 0
               CALL CHAR_FLD ( JCARD(J), JF(J), CHRINP )
               IF ((CHRINP(1:2) == 'UM') .OR. (CHRINP(1:4) == '"UM"')) THEN
                  JERR = JERR + 1
                  WRITE(ERR,1155) JF(J), 'RBE3', CHAR_ELID, I
                  WRITE(F06,1155) JF(J), 'RBE3', CHAR_ELID, I
               ENDIF
            ENDDO

do_j:       DO J=2,9

               IF (FND_NEW_WGT == 'Y') THEN                ! Last field read was FL PT so next field must be a Ci (displ components)
                  TOKEN = JCARD(J)(1:8)                    ! Only send the 1st 8 chars of this JCARD. It has been left justified
                  CALL TOKCHK ( TOKEN, TOKTYP )
                  INTDOF = 0
                  CALL I4FLD ( JCARD(J), JF(J), I4INP )
                  IF (IERRFL(J) == 'N') THEN
                     CALL IP6CHK ( JCARD(J), CHRINP, IP6TYP, NUM_Ci )
                     IF (IP6TYP(1:8) == 'COMP NOS') THEN
                        INTDOF = I4INP
                     ELSE
                        JERR = JERR + 1
                        WRITE(ERR,1129) JF(J), 'RBE3', CHAR_ELID, I, JF(J), JCARD(J)
                        WRITE(F06,1129) JF(J), 'RBE3', CHAR_ELID, I, JF(J), JCARD(J)
                     ENDIF
                  ELSE
                     JERR = JERR + 1
                  ENDIF
                  CHK(J) = J
                  FND_NEW_WGT = 'N'
                  NEXT_MUST_BE_GRID = 'Y'
                  CYCLE do_j
               ENDIF

               TOKEN = JCARD(J)(1:8)                       ! Only send the 1st 8 chars of this JCARD. It has been left justified
               CALL TOKCHK ( TOKEN, TOKTYP )               ! Last field read was not a weight so see what it is
               IF      (TOKTYP == 'BLANK   ') THEN
                  CYCLE do_j
               ELSE IF (TOKTYP == 'FL PT   ') THEN         ! Found a new weight
                  IF (NEXT_MUST_BE_GRID == 'N') THEN
                     WGT = ZERO
                     FND_NEW_WGT = 'Y'
                     CALL R8FLD ( JCARD(J), JF(J), R8INP )
                     IF (IERRFL(6) == 'N') THEN
                        WGT = R8INP
                     ELSE
                        JERR = JERR + 1
                     ENDIF
                  ELSE
                     JERR = JERR + 1
                     WRITE(ERR,1154) J, 'RBE3', CHAR_ELID, I
                     WRITE(F06,1154) J, 'RBE3', CHAR_ELID, I
                     NEXT_MUST_BE_GRID = 'N'
                  ENDIF
               ELSE                                        ! Not a weight or Ci so this must be a grid
                  GRID1 = 0
                  NEXT_MUST_BE_GRID = 'N'
                  CALL I4FLD ( JCARD(J), JF(J), I4INP )
                  IF (IERRFL(J) == 'N') THEN
                     GRID1 = I4INP
                     IRBE3 = IRBE3 + 1
                     IF (IRBE3 > MRBE3) CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, IRBE3, 'WTi' )
                     WTi(IRBE3)  = WGT
                     WT_TOT = WT_TOT + WTi(IRBE3)
                     COMP(IRBE3) = INTDOF
                     GRID(IRBE3) = GRID1
                  ELSE
                     JERR = JERR + 1
                  ENDIF
               ENDIF
            ENDDO do_j  
            CALL BD_IMBEDDED_BLANK ( JCARD, CHK(2), CHK(3), CHK(4), CHK(5), CHK(6), CHK(7), CHK(8), CHK(9)  )
            CALL CRDERR ( CARD )

         ELSE

            EXIT

         ENDIF

      ENDDO

! Write data to file L1F

      IF (JERR == 0) THEN
         WRITE(L1F) RTYPE
         NRECARD = NRECARD + 1
         WRITE(L1F) RELID, REFGRID, REFC, IRBE3, WT_TOT
         DO I=1,IRBE3
            WRITE(L1F) GRID(I), COMP(I), WTi(I)
            CALL RDOF ( COMP(I), CDOF )
            DO J=1,6
               IF (CDOF(J) /= '0') THEN
               ENDIF
            ENDDO
         ENDDO
      ELSE
         FATAL_ERR = FATAL_ERR + 1
      ENDIF

!xx   NTERM_RMG = REFC_NUM_Ci*(NTERM_RMG + 1)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1120 FORMAT(' *ERROR  1120: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY TRIPLETS OF GRID/COMPONENT/RBE3 COEFF ON BULK DATA RBE3 CARD WITH SET ID = ',I8              &
                    ,/,14X,' LIMIT IS = ',I12)

 1124 FORMAT(' *ERROR  1124: INVALID DOF NUMBER IN FIELD ',I3,' ON ',A,' ENTRY WITH ID = ',A                                       &
                    ,/,14X,' MUST BE A COMBINATION OF DIGITS 1-6. HOWEVER, FIELD ',I3, ' HAS: "',A,'"')


 1129 FORMAT(' *ERROR  1129: INVALID DOF NUMBER IN FIELD ',I3,' ON ',A,' ID = ',A,' CONTINUATION ENTRY',I4                         &
                    ,/,14X,' MUST BE A COMBINATION OF DIGITS 1-6. HOWEVER, FIELD ',I3, ' HAS: "',A,'"')


 1151 FORMAT(' *ERROR  1151: FIELD ',A,' ON ',A,' ID = ',A                                                                         &
                    ,/,14X,' MUST BE EITHER BLANK, A WEIGHT, OR A GRID NUMBER BUT IS: "',A,'"')

 1154 FORMAT(' *ERROR  1154: FIELD ',I3,' ON ',A,' ID = ',A,' CONT ENTRY',I4,' MUST BE A GRID PT SINCE THE',                       &
                           ' PREVIOUS 2 ENTRIES WERE A WEIGHT AND COMPS')

 1155 FORMAT(' *ERROR  1155: FIELD ',I3,' ON ',A,' ID = ',A,' CONT ENTRY',I4,' HAS "UM". THIS OPTION NOT PROGRAMMED IN MYSTRAN'    &
                    ,/,14X,' NOTIFY AUTHOR IF YOU WOULD LIKE THIS OPTION ADDED')





! **********************************************************************************************************************************

      END SUBROUTINE BD_RBE3
