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
  
      SUBROUTINE BD_RSPLINE ( CARD, LARGE_FLD_INP )
  
! Processes RSPLINE Bulk Data Cards. Writes RSPLINE card data to file L1F. Note that this code only recognizes 2 indep grids on a
! given RSPLINE unlike NASTRAN (thus user must enter a different RSPLINE for each pair of indep grids between which the cubic
! spline will be fitted to all of the dep grid/comps
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1F
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, MRSPLINE, NRSPLINE, NRECARD, NRIGEL
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_RSPLINE_BEGEND
      USE MODEL_STUF, ONLY            :  RIGID_ELEM_IDS
 
      USE BD_RSPLINE_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_RSPLINE'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: CHRINP            ! A character field returned from subr IP6CHK
      CHARACTER( 1*BYTE)              :: ERR_1158_WRITTEN  ! Indicator of whether fatal error #1158 has been written

      CHARACTER(LEN(JCARD))           :: FLDS_C(MRSPLINE+1)! Char array of all fields on this entry that have grid or comp vals.
!                                                            It will have IGRID1, then pairs of DGRID(i)/COMP(i), then IGRID2.

      CHARACTER(LEN(JCARD))           :: ID                ! Char value for element ID in field 2 of parent entry
      CHARACTER( 8*BYTE)              :: IP6TYP            ! Descriptor of what is in the 8 char field sent to subr IP6CHK
      CHARACTER(LEN(JCARD))           :: NAME              ! JCARD(1) from parent entry
      CHARACTER( 8*BYTE), PARAMETER   :: RTYPE = 'RSPLINE '! Rigid element type
 
      INTEGER(LONG)                   :: DCOMP(MRSPLINE+1) ! Array of dep displ comp values (1-6) found on this logical RSPLINE card
      INTEGER(LONG)                   :: DGRID(MRSPLINE+1) ! Array of dep GRID ID's found on this logical RSPLINE card

      INTEGER(LONG)                   :: FLDS_I(MRSPLINE+1)! Integer array of all fields on this entry that have grid or comp vals.
!                                                            It will have IGRID1, then pairs of DGRID(i)/COMP(i), then IGRID2.

      INTEGER(LONG)                   :: FLDS_F(MRSPLINE+1)! Field number on the RSPLINE entry where a value exists (parent or cont)
      INTEGER(LONG)                   :: I4INP     = 0     ! A value read from input file that should be an integer value
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: IGRID1,IGRID2     ! The 2 independent grids on the RSPLINE entry
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: JERR      = 0     ! A local error count
      INTEGER(LONG)                   :: JE                ! An intermediale variable  
      INTEGER(LONG)                   :: NUM_DEPENDENTS    ! Count of number of pairs of dependent grids/components
      INTEGER(LONG)                   :: NUM_ENTRIES       ! Count of number of entries placed into array GC_FLDS
      INTEGER(LONG)                   :: NUM_Ci            ! Number of displ components in a DCOMP field
      INTEGER(LONG)                   :: ELID      = 0     ! This elements' ID
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_RSPLINE_BEGEND
 
      REAL(DOUBLE)                    :: DL_RAT            ! Value in field 3 for D/L ratio
      REAL(DOUBLE)                    :: R8INP             ! A real value read from a field on this RSPLINE entry
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! RSPLINE Bulk Data Card:
 
!   FIELD   ITEM            EXPLANATION 
!   -----   ------------    -------------
!    2      ELID            RSPLINE elem ID
!    3      D/L             Diam/length ratio of the rod used in determining the beam deflection relationship
!    4      G1              Indep grid at one end of the RSPLINE line
!    5-9    Gi,Ci           Dep grid/comps (dep grid/comps)

! on optional continuation entries:
!    2-9    contain either a grid or comp number. To be conservative each cont entry will be assumed to have an 8 dep entries

! Subsequent entries have the same format.
 
      JERR = 0

! Initialize arrays

      IGRID1 = 0
      IGRID2 = 0
      DO J=1,MRSPLINE+1
         DGRID(J)      = 0
         DCOMP(J)      = 0
         FLDS_I(J)     = 0
         FLDS_F(J)     = 0
         FLDS_C(J)(1:) = ' '
      ENDDO
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      NAME = JCARD(1)
      ID   = JCARD(2)
 
! Up the elem count

      NRSPLINE = NRSPLINE + 1
      NRIGEL   = NRIGEL+1
 
! Read and check data on parent entry
 
      CALL I4FLD ( JCARD(2), JF(2), I4INP )                ! Field 2: Elem ID
      IF (IERRFL(2) == 'N') THEN
         ELID                   = I4INP
         RIGID_ELEM_IDS(NRIGEL) = ELID
      ELSE
         JERR = JERR + 1
      ENDIF

      CALL R8FLD ( JCARD(3), JF(3), R8INP )                ! Field 3: DL_RAT
      IF (IERRFL(3) == 'N') THEN
         DL_RAT = R8INP
      ELSE
         JERR = JERR + 1
      ENDIF

      NUM_ENTRIES = 0                                      ! Fields 4-9
      DO J=4,9
         CALL I4FLD ( JCARD(J), JF(J), I4INP )
         IF (IERRFL(J) == 'N') THEN
            NUM_ENTRIES = NUM_ENTRIES + 1
            IF (NUM_ENTRIES > MRSPLINE + 1) CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, NUM_ENTRIES, 'FLDS ARRAYS' )
            FLDS_I(NUM_ENTRIES) = I4INP
            FLDS_C(NUM_ENTRIES) = JCARD(J)
            FLDS_F(NUM_ENTRIES) = JF(J)
         ELSE
            JERR = JERR + 1
         ENDIF
      ENDDO
            
      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )     ! Make sure there are no imbedded blanks
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields

! Read and check data on optional continuation entries

do_1: DO
         IF (LARGE_FLD_INP == 'N') THEN
            CALL NEXTC  ( CARD, ICONT, IERR )
         ELSE
            CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
            CARD = CHILD
         ENDIF
         IF (ICONT == 1) THEN
            CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
            DO J=2,9
               CALL I4FLD ( JCARD(J), JF(J), I4INP )
               IF (IERRFL(J) == 'N') THEN
                  NUM_ENTRIES = NUM_ENTRIES + 1
                  IF (NUM_ENTRIES > MRSPLINE + 1) CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, NUM_ENTRIES, 'FLDS ARRAYS' )
                  FLDS_I(NUM_ENTRIES) = I4INP
                  FLDS_C(NUM_ENTRIES) = JCARD(J)
                  FLDS_F(NUM_ENTRIES) = JF(J)
               ELSE
                  JERR = JERR + 1
               ENDIF
            ENDDO
            CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )
            CALL CRDERR ( CARD )
         ELSE
            EXIT do_1
         ENDIF
      ENDDO do_1
                  
! FLDS_I may have some number of 0 entries at the end which should not be counted in NUM_ENTRIES.
! Scan FLDS_I (from end to begining) to find where last nonzero entry exists. This value is the one we want for NUM_ENTRIES

      JE = NUM_ENTRIES
      DO J=JE,1,-1
         IF (FLDS_I(J) == 0) THEN
            NUM_ENTRIES = NUM_ENTRIES - 1
            CYCLE
         ELSE
            EXIT
         ENDIF
      ENDDO

! Now that we have an accurate count for NUM_ENTRIES we can make some sanity checks on FLDS_I. NUM_ENTRIES should be an even
! number (IGRID1, IGRID2 and pairs of DGRID(i), DCOMP(i)). Also, FLDS_I should not have any zero values

      ERR_1158_WRITTEN = 'N'

      JE = MODULO ( NUM_ENTRIES, 2 )
      IF (JE /= 0) THEN                                    ! NUM_ENTRIES is not an even number unless JE = 0
         JERR = JERR + 1
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1158) NAME, ID, 'THIS RSPLINE ENTRY DOES NOT HAVE AN EVEN NUMBER OF: DEP GRIDS + PAIRS OF DEP GRID/COMP ENTRIES'
         WRITE(F06,1158) NAME, ID, 'THIS RSPLINE ENTRY DOES NOT HAVE AN EVEN NUMBER OF: DEP GRIDS + PAIRS OF DEP GRID/COMP ENTRIES'
         ERR_1158_WRITTEN = 'Y'
      ENDIF

      DO J=1,NUM_ENTRIES
         IF (FLDS_I(J) == 0) THEN                         ! Zero vals in FLDS_I
            JERR = JERR + 1
            FATAL_ERR = FATAL_ERR + 1
            IF (ERR_1158_WRITTEN == 'N') THEN
               WRITE(ERR,1158) NAME, ID, 'THIS RSPLINE ENTRY HAS SOME FIELDS THAT ARE BLANK OR THAT CONTAIN 0 VALUES'
               WRITE(F06,1158) NAME, ID, 'THIS RSPLINE ENTRY HAS SOME FIELDS THAT ARE BLANK OR THAT CONTAIN 0 VALUES'
            ELSE
               WRITE(ERR,11582)
               WRITE(F06,11582)
            ENDIF
         ENDIF
      ENDDO

! Make sure all DCOMP(i) are proper component numbers

      DO J=3,NUM_ENTRIES,2
         CALL IP6CHK ( FLDS_C(J), CHRINP, IP6TYP, NUM_Ci )
         IF (IP6TYP(1:8) /= 'COMP NOS') THEN
            JERR = JERR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1124) FLDS_F(J), NAME, ID, FLDS_F(J), FLDS_C(J)
            WRITE(F06,1124) FLDS_F(J), NAME, ID, FLDS_F(J), FLDS_C(J)
         ENDIF
      ENDDO

! Determine the fields that will be written to L1F

      IGRID1 = FLDS_I(1)
      IF (NUM_ENTRIES > 0) THEN
         IGRID2 = FLDS_I(NUM_ENTRIES)
      ENDIF
      NUM_DEPENDENTS = 0
      DO J=2,NUM_ENTRIES-1,2
         NUM_DEPENDENTS = NUM_DEPENDENTS + 1
         DGRID(NUM_DEPENDENTS) = FLDS_I(J)
         DCOMP(NUM_DEPENDENTS) = FLDS_I(J+1)
      ENDDO


! If JERR /= 0 we can write data to L1F

      IF (JERR == 0) THEN
         DO J=1,NUM_DEPENDENTS
            WRITE(L1F) RTYPE
            WRITE(L1F) ELID, J, NUM_DEPENDENTS, IGRID1, IGRID2, DGRID(J), DCOMP(J), DL_RAT
            NRECARD = NRECARD + 1
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
 1124 FORMAT(' *ERROR  1124: INVALID DOF NUMBER IN FIELD ',I3,' ON ',A,' ENTRY WITH ID = ',A                                       &
                    ,/,14X,' MUST BE A COMBINATION OF DIGITS 1-6. HOWEVER, FIELD ',I3, ' HAS: "',A,'"')


 1129 FORMAT(' *ERROR  1129: INVALID DOF NUMBER IN FIELD ',I3,' ON ',A,' ID = ',A,' CONTINUATION ENTRY',I4                         &
                    ,/,14X,' MUST BE A COMBINATION OF DIGITS 1-6. HOWEVER, FIELD ',I3, ' HAS: "',A,'"')


 1158 FORMAT(' *ERROR  1158: FORMAT ERROR ON ',A,A,'. FORMAT MUST HAVE:'                                                           &
                    ,/,14X,' (a) TWO AND ONLY TWO INDEP GRIDS (IN FIELD 4 OF THE PARENT ENTRY AND IN THE LAST FIELD ON THE',       &
                           ' LOGICAL RSPLINE ENTRY)'                                                                               &
                    ,/,14X,' (b) PAIRS OF DEP GRIDS/COMPS FOLLOWING INDEP GRID 1 AND BEFORE INDEP GRID 2 WITH NO BLANK FIELDS'     &
                    ,/,15x,A)


11582 FORMAT('               THIS RSPLINE ENTRY ALSO HAS SOME FIELDS THAT ARE BLANK OR THAT CONTAIN 0 VALUES')

! **********************************************************************************************************************************

      END SUBROUTINE BD_RSPLINE
