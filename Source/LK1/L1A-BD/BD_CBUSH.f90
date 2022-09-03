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

      SUBROUTINE BD_CBUSH ( CARD, LARGE_FLD_INP )

! Processes CBUSH Bulk Data card:

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LBUSHOFF, LVVEC, MEDAT_CBUSH,&
                                         NBUSHOFF, NCBUSH, NEDAT, NELE, NVVEC, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_CBUSH_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, HALF
      USE PARAMS, ONLY                :  EPSIL, SUPWARN
      USE MODEL_STUF, ONLY            :  BUSHOFF, EDAT, ETYPE, VVEC

      USE BD_CBUSH_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_CBUSH'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: EID               ! Field 2 of CBUSH card
      CHARACTER( 1*BYTE)              :: FOUND     = 'N'   ! 'Y' if the V vec is one that is already stored in array VVEC
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of 8 characters making up CARD
      CHARACTER(LEN=JCARD_LEN)        :: JCARD_EDAT(10)    ! JCARD values sent to subr ELEPRO
      CHARACTER(LEN=JCARD_LEN)        :: NAME              ! Name of this entry (field 1)
      CHARACTER( 9*BYTE)              :: VVEC_TYPE         ! Type of V vector on this CBUSH 

      INTEGER(LONG)                   :: CID               ! Coord sys ID for v vector (required under some circumstances)
      INTEGER(LONG)                   :: G0   = 0          ! Grid specifying V vector for this CBUSH, if input
      INTEGER(LONG)                   :: I4INP     = 0     ! A value read from input file that should be an integer value
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: JERR      = 0     ! A local error count
      INTEGER(LONG)                   :: NEDAT_START       ! Value of NEDAT at start of this subr
      INTEGER(LONG)                   :: OCID              ! Coord sys ID for offsets
      INTEGER(LONG)                   :: VVEC_NUM  = 0     ! V vector number
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_CBUSH_BEGEND

      REAL(DOUBLE)                    :: VV(3)             ! The 3 components of the V vector for this CBUSH elem
      REAL(DOUBLE)                    :: EPS1              ! A small number to compare real zero
      REAL(DOUBLE)                    :: R8INP     = ZERO  ! A value read from input file that should be a real value

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! CBUSH element Bulk Data Card routine

!   FIELD   ITEM           ARRAY ELEMENT
!   -----   ------------   -------------
!    1      Element type   ETYPE(nele)
!    2      Element ID     EDAT(nedat+1)
!    3      Property ID    EDAT(nedat+2)
!    -      Num of grids   EDAT)nedat+3
!    4      Grid A         EDAT(nedat+4)
!    5      Grid B         EDAT(nedat+5)
!    6-8    V-Vector       EDAT(nedat+6) (see VVEC explanation below)
!    9      CID            EDAT(nedat+7) Elem coord sys identification. 0 is basic. blank means to use G0 or X1,2,3 

! on optional second card:
!    2      S              Location of spring/damper (def = 0.5). This is a relative distance = offset/BUSH length
!    3      OCID           EDAT(nedat+8) Offset coord sys ID
!   none    NBUSHOFF       EDAT(nedat+9)
!    4-6    S1,2,3         Offset vector. The Si are actual offsets

! NOTES:

! If V-vector is specified via a grid point then EDAT(nedat+5) is set to that grid number.
! If V-vector is specified via an actual vector, the vector is loaded into array VVEC(NVVEC,J) (J=1,2,3) unless
! a vector equal to it has been put in VVEC. EDAT(nedat+5) is set equal to -NVVEC, where NVVEC is the row number in array VVEC.

! Offsets are in fields 4 - 9 of the first continuation card. If there are no offsets for this element, a zero is entered
! in array EDAT(nedat7).

      NEDAT_START = NEDAT
      EPS1 = EPSIL(1)

! Make JCARD from CARD

      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      NAME = JCARD(1)
      EID  = JCARD(2)

! Set JCARD_EDAT to JCARD

      DO I=1,10
         JCARD_EDAT(I) = JCARD(I)
      ENDDO 

! Check property ID field. Set to EID if blank

      IF (JCARD(3)(1:) == ' ') THEN
         JCARD_EDAT(3) = JCARD(2)
      ENDIF

! Make sure that grids in fields 4 and 5 are different

      CALL LEFT_ADJ_BDFLD ( JCARD(4) )
      CALL LEFT_ADJ_BDFLD ( JCARD(5) )
      IF (JCARD(4) == JCARD(5)) THEN
         WRITE(F06,*) ' * ERROR : GRIDS ON CBUSH CANNOT BE SAME'
      ENDIF

! Call ELEPRO to increment NELE and load some of the connection data into array EDAT

      CALL ELEPRO ( 'Y', JCARD_EDAT, 4, MEDAT_CBUSH , 'Y', 'Y', 'Y', 'Y', 'N', 'N', 'N', 'N' )
      NCBUSH = NCBUSH+1
      ETYPE(NELE)(1:4) = 'BUSH'

! Get the V vector for this CBUSH. If field 9 is not blank, ignore fields 6-8 since v vector will be defined using CID in field 9
! The following sets slots 5 and 6 in EDAT since VVEC can be defined by:
!   (1) A vector defined in field 6,7,8 (field 6 can be a grid or 6-8 can be a vector), or
!   (2) CID in field 9. In this case the VVEC will be along the directions defined by CID and will be determined in a later subr
! If CID is blank then V must be defined by G0 in field 6 or Xi in fields 6-8

      VVEC_TYPE = 'UNDEFINED'
vec:  IF (JCARD(9)(1:) == ' ') THEN                        ! CID field is blank so VVEC should be defined in fields 6,7,8

         DO J=1,3
            VV(J) = ZERO
         ENDDO

         DO J=1,JCARD_LEN                                  ! See if there is an actual V vector.
            IF ((JCARD(6)(J:J) == '.') .OR. (JCARD(7)(J:J) == '.') .OR. (JCARD(8)(J:J) == '.')) THEN
               VVEC_TYPE = 'VECTOR   '
               EXIT
            ENDIF
         ENDDO
         IF (VVEC_TYPE == 'VECTOR   ') THEN                ! If there is an actual V vector, get components
            LVVEC = LVVEC + 1
            JERR = 0
            DO J=1,3
               CALL R8FLD ( JCARD(J+5), JF(J+5), R8INP )
               IF (IERRFL(J+5) == 'N') THEN
                  VV(J) = R8INP
               ELSE
                  JERR = JERR + 1
               ENDIF
            ENDDO
            IF (JERR /= 0) THEN
               VVEC_TYPE = 'ERROR    '                     ! Found error in V vector components, so reset VVEC_TYPE 
            ENDIF
         ELSE                                              ! Check to see if there is a grid no. for specifying VVEC
            IF ((JCARD(6)(1:) /= ' ') .AND. (JCARD(7)(1:) == ' ') .AND. (JCARD(8)(1:) == ' ')) THEN
               VVEC_TYPE = 'GRID     '    
               CALL I4FLD ( JCARD(6), JF(6), I4INP )
               IF (IERRFL(6) == 'N') THEN
                  G0 = I4INP
                  IF (G0 < 0) THEN
                     FATAL_ERR = FATAL_ERR + 1
                     WRITE(ERR,1187) NAME, EID, G0
                     WRITE(F06,1187) NAME, EID, G0
                  ENDIF
               ELSE
                  VVEC_TYPE = 'ERROR    '                  ! Found error in field 6, so reset VVEC_TYPE
               ENDIF
!xx         ELSE
!xx            IF ((JCARD(6)(1:) == ' ') .AND. (JCARD(7)(1:) == ' ') .AND. (JCARD(8)(1:) == ' ')) THEN
!xx               VVEC_TYPE = 'UNDEFINED'
!xx               FATAL_ERR = FATAL_ERR + 1
!xx               WRITE(ERR,1188) NAME, EID
!xx               WRITE(F06,1188) NAME, EID
!xx            ELSE
!xx               VVEC_TYPE = 'ERROR    '
!xx               FATAL_ERR = FATAL_ERR + 1
!xx               WRITE(ERR,1186) NAME, EID
!xx               WRITE(F06,1186) NAME, EID
!xx            ENDIF
            ENDIF
         ENDIF
                                                           ! Load V vector data into EDAT and into VVEC, if not already there
         IF (VVEC_TYPE == 'GRID     ') THEN

            EDAT(NEDAT_START+6) = G0                       ! --- Slot 6 in EDAT is VVEC defined by grid G0 

         ELSE IF (VVEC_TYPE == 'VECTOR   ') THEN

            FOUND = 'N'                                    ! --- See if there is already a VVEC with these components
            DO J=1,NVVEC
               IF ((DABS(VV(1) - VVEC(J,1)) < EPS1) .AND. (DABS(VV(2) - VVEC(J,2)) < EPS1) .AND.                                   &
                   (DABS(VV(3) - VVEC(J,3)) < EPS1)) THEN
                  VVEC_NUM = J
                  FOUND = 'Y'
                  EXIT
               ENDIF
            ENDDO

            IF (FOUND == 'N') THEN   
               NVVEC = NVVEC + 1
               IF (NVVEC > LVVEC) THEN
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1132) SUBR_NAME,LVVEC
                  WRITE(F06,1132) SUBR_NAME,LVVEC
                  CALL OUTA_HERE ( 'Y' )
               ENDIF
               VVEC_NUM = NVVEC
               DO J=1,3
                  VVEC(NVVEC,J) = VV(J)
               ENDDO
            ENDIF

            EDAT(NEDAT_START+6) = -VVEC_NUM                ! --- Slot 6 in EDAT is VVEC defined by neg of a vector number

         ELSE

            EDAT(NEDAT_START+6) = 0                        ! --- Slot 6 in EDAT is undefined VVEC

         ENDIF

         EDAT(NEDAT_START+7) = -99                         ! --- Slot 7 in EDAT is for CID. Use CID = -99 when CID field is blank

      ELSE                                                 ! --- CID field is not blank so VVEC is defined by CID

         EDAT(NEDAT_START+6) = 0                           ! --- Load 0 into EDAT slot 5 for VVEC since field 9 not blank

         EDAT(NEDAT_START+7) = 0                           ! --- Initialize. 0 (does not mean basic)
         CALL I4FLD ( JCARD(9), JF(9), CID )
         IF (IERRFL(9) == 'N') THEN
            IF (CID >= 0) THEN
               EDAT(NEDAT_START+7) = CID                   ! --- Slot 6 in EDAT is for CID. Use actual value if no read error
            ELSE
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1189) NAME, EID, JF(9), CID
               WRITE(F06,1189) NAME, EID, JF(9), CID
            ENDIF
         ENDIF
                                                           ! Issue warning since both 6-8 and 9 are non blank
         IF ((JCARD(6)(1:) /= ' ') .OR. (JCARD(7)(1:) /= ' ') .OR. (JCARD(8)(1:) /= ' ')) THEN
            WARN_ERR = WARN_ERR + 1
            WRITE(ERR,1002) NAME, EID
            IF (SUPWARN == 'N') THEN
               WRITE(F06,1002) NAME, EID
            ENDIF
         ENDIF

      ENDIF vec

! Write warnings and errors for parent entry, if any

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )     ! Make sure that there are no imbedded blanks in fields 2-9
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields         

! Optional Second Card: NOTE: The offset on this entry is relative to grid A and has only 3 components.
! The offset from grid B will be to the same location in space as where the offset from grid A is (since the CBUSH is a zero
! length element). Thus, we really do not need terms in array OFFDIS for grid B. However, put the grid A offset values in the
! grid B offset position anyway.

      IF (LARGE_FLD_INP == 'N') THEN
         CALL NEXTC  ( CARD, ICONT, IERR )
      ELSE
         CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
         CARD = CHILD
      ENDIF
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )

      OCID = -99                                           ! Initially set OCID = -99 since we will test for -1 and > 0 below
      IF (ICONT == 1) THEN

         IF (CARD(1:) /= ' ') THEN                         ! Only process continuation entries if 1st one is not totally blank

            CALL LEFT_ADJ_BDFLD ( JCARD(3) )               ! Get OCID. OCID=-1 is default and means use elem sys and S, not S1,2,3
            IF ((JCARD(3)(1:) == ' ') .OR. (JCARD(3)(1:2) == '-1')) THEN
               OCID = -1                                   ! OCID = -1 is default and means use elem system and S, not S1,2,3
            ELSE                                           ! Field 3 not blank so get value to use for OCID
               CALL I4FLD ( JCARD(3), JF(3), I4INP )
               IF (IERRFL(2) == 'N') THEN
                  OCID = I4INP
               ELSE
                  OCID = -99                               ! Use OCID = -99 to indicate an error
               ENDIF
               IF (OCID < -1) THEN
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1180) JCARD(3), NAME, EID
                  WRITE(F06,1180) JCARD(3), NAME, EID
               ENDIF
            ENDIF
            EDAT(NEDAT_START+8) = OCID                     ! Slot 8 in EDAT is the OCID value. Slot 7 will be NBUSHOFF

            IF      (OCID == -1) THEN                      ! Get components of BUSHOFF
               CALL R8FLD ( JCARD(2), JF(2), R8INP )
               IF (IERRFL(1) == 'N') THEN
                  NBUSHOFF = NBUSHOFF + 1
                  EDAT(NEDAT_START+9) = NBUSHOFF           ! Slot 9 in EDAT is for the offset key
                  BUSHOFF(NBUSHOFF,1) = R8INP
                  BUSHOFF(NBUSHOFF,2) = ZERO
                  BUSHOFF(NBUSHOFF,3) = ZERO
               ENDIF
            ELSE IF (OCID >= 0) THEN
               NBUSHOFF = NBUSHOFF + 1
               EDAT(NEDAT_START+9) = NBUSHOFF              ! Slot 9 in EDAT is for the offset key
               DO J=1,3
                  CALL R8FLD ( JCARD(J+3), JF(J+3), R8INP )
                  IF (IERRFL(J+3) == 'N') THEN
                     BUSHOFF(NBUSHOFF,J  ) = R8INP
                  ENDIF
               ENDDO
            ENDIF
            CALL BD_IMBEDDED_BLANK   ( JCARD,2,3,4,5,6,0,0,0 )
            CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,7,8,9 )
            CALL CRDERR ( CARD )

         ELSE

            EDAT(NEDAT_START+8) = -1                       ! Con't entry was blank so default OCID and null offset flag
            EDAT(NEDAT_START+7) =  0

            NBUSHOFF = NBUSHOFF + 1
            EDAT(NEDAT_START+9) = NBUSHOFF                 ! Slot 9 in EDAT is for the offset key
            BUSHOFF(NBUSHOFF,1) = HALF
            BUSHOFF(NBUSHOFF,2) = ZERO
            BUSHOFF(NBUSHOFF,3) = ZERO

         ENDIF

      ELSE

         EDAT(NEDAT_START+8) = -1                          ! There was no con't entry so default OCID
         NBUSHOFF = NBUSHOFF + 1
         EDAT(NEDAT_START+9) = NBUSHOFF                    ! Slot 9 in EDAT is for the offset key
         BUSHOFF(NBUSHOFF,1) = HALF
         BUSHOFF(NBUSHOFF,2) = ZERO
         BUSHOFF(NBUSHOFF,3) = ZERO

      ENDIF

! Write warnings and errors if any

! Update NEDAT

      NEDAT = NEDAT_START + MEDAT_CBUSH

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1002 FORMAT(' *WARNING    : ',A,A,' HAS V VEC DEFINED TWO WAYS: FIELDS 6,7,AND/OR 8 AND ALSO FIELD 9. CID IN FIELD 9 WILL BE USED')

 1132 FORMAT(' *ERROR  1132: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY V VECTORS. LIMIT IS ',I8)

 1186 FORMAT(' *ERROR  1186: ERROR IN SPECIFYING V VECTOR ON ',A,A,'. EITHER FIELD 6 MUST BE A POSITIVE INTEGER GRID POINT'        &
                    ,/,14X,' OR FIELDS 6, 7, 8 MUST CONTAIN REAL VECTOR COMPONENTS (WITH DECIMAL POINTS)')

 1180 FORMAT(' *ERROR  1180: INVALID OCID = ',A,' ON ',A,A)

 1187 FORMAT(' *ERROR  1187: GRID SPECIFYING V VECTOR ON ',A,A,' MUST BE > 0. VALUE IS = ',I8)

 1188 FORMAT(' *ERROR  1188: NO V VECTOR SPECIFIED FOR ',A,' ELEMENT ID = ',A)

 1189 FORMAT(' *ERROR  1189: ',A,A,' MUST HAVE NON-NEGATIVE VALUE FOR CID IF FIELD ',I2,' IS NOT BLANK. VALUE READ WAS ',I8)
 



! **********************************************************************************************************************************

      END SUBROUTINE BD_CBUSH
