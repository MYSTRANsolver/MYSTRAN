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
  
      SUBROUTINE BD_PBARL ( CARD, LARGE_FLD_INP, PBARL_TYPE )
  
! Processes PBARL Bulk Data Cards. Reads and checks:

!  1) Prop ID and Material ID and enter into array PBAR
!  2) Read data on type of crossection and dimensions and calculate section props
!  3) Create an equivalent PBAR entry (data goes into arrays PBAR, RPBAR)
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE DERIVED_DATA_TYPES, ONLY    :  CHAR1_INT1
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ECHO, IERRFL, FATAL_ERR, JCARD_LEN, JF, LPBAR, NPBAR, NPBARL
      USE PARAMS, ONLY                :  EPSIL, PBARLSHR, SUPINFO
      USE CONSTANTS_1, ONLY           :  PI, ZERO, QUARTER, THIRD, HALF, ONE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE,     &
                                         TEN, TWELVE

      USE TIMDAT, ONLY                :  TSEC
      USE MODEL_STUF, ONLY            :  PBAR, RPBAR
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PBARL_BEGEND
 
      USE BD_PBARL_USE_IFs

      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER        :: NS        = 25    ! Dimension of array BAR_SHAPE
      TYPE(CHAR1_INT1)                :: BAR_SHAPE(NS)     ! Array with the BAR crossection type and the num of D(i) needed for it

      CHARACTER(LEN(BLNK_SUB_NAM))    :: SUBR_NAME   =   'BD_PBARL'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(OUT)   :: PBARL_TYPE        ! Name of the cross-section (e.g. I, BAR, etc)
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: CS_TYPE           ! Name of the cross-section (e.g. I, BAR, etc)
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER( 1*BYTE)              :: FOUND             ! 'Y' if a correct name for a cross-section was found in field 5
      CHARACTER(LEN(JCARD))           :: ID                ! Character value of element ID (field 2 of parent card)
      CHARACTER(99*BYTE)              :: MSG               ! Error message written out
      CHARACTER(LEN(JCARD))           :: NAME              ! JCARD(1) from parent entry
 
      INTEGER(LONG)                   :: ICONT     = 0     ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR      = 0     ! Error indicator
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: KD                ! Counter
      INTEGER(LONG)                   :: NUM_D             ! Number of D(i) values to read from the continuation entries
      INTEGER(LONG)                   :: MATL_ID   = 0     ! Material ID (field 3 of this property card)
      INTEGER(LONG)                   :: PROP_ID   = 0     ! Property ID (field 2 of this property card)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PBARL_BEGEND
 
      REAL(DOUBLE)                    :: AREA      = ZERO  ! Cross-sectional area
      REAL(DOUBLE)                    :: D(NS)             ! Dimensions of cross-secion of the bar
      REAL(DOUBLE)                    :: I1,I2     = ZERO  ! Moments of inertia
      REAL(DOUBLE)                    :: K1,K2     = ZERO  ! Shear flex factors
      REAL(DOUBLE)                    :: I12       = ZERO  ! Product of inertia
      REAL(DOUBLE)                    :: JTOR      = ZERO  ! Torsional constant
      REAL(DOUBLE)                    :: NSM       = ZERO  ! Non structural mass
      REAL(DOUBLE)                    :: Y(4)      = ZERO  ! Y coords in cross-section for 4 points of data recovery
      REAL(DOUBLE)                    :: Z(4)      = ZERO  ! Z coords in cross-section for 4 points of data recovery
      REAL(DOUBLE)                    :: R8INP             ! A real value read from a field on this PBARL entry

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! PBAR Bulk Data Card routine
 
!   FIELD   ITEM                                            ARRAY ELEMENT
!   -----   ----                                            -------------
!    2      Property ID                                     PBAR(npbar, 1)
!    3      Material ID                                     PBAR(npbar, 2)
!    4      GROUP   ***(NOT USED)***
!    5      TYPE (type of cross-section)

! on mandatory second card: (the number of D(i) are set according to TYPE. See BAR_SHAPE array)
!    2      D(1)
!    3      D(2)
!    4      D(3)
!    5      D(4)
!    6      D(5)
!    7      D(6)
!    8      D(7)
!    9      D(8)
! on optional cards:
!    2      D(9)
!    .       .
!    .       .
!    .      NSM (last entry)
 
 
! Initialize

      AREA = ZERO
      I1   = ZERO
      I2   = ZERO
      I12  = ZERO
      JTOR = ZERO
      Y(1) = ZERO   ;   Z(1) = ZERO
      Y(2) = ZERO   ;   Z(2) = ZERO
      Y(3) = ZERO   ;   Z(3) = ZERO
      Y(4) = ZERO   ;   Z(4) = ZERO
      K1   = ZERO
      K2   = ZERO

      NSM  = ZERO

      DO I=1,NS
         BAR_SHAPE(I)%Col_1(1:) = ' '
         BAR_SHAPE(I)%Col_2     = 0
         D(I)                   = ZERO
      ENDDO

      BAR_SHAPE( 1)%Col_1(1:8) = 'BAR     '   ;  BAR_SHAPE( 1)%Col_2 = 2  
      BAR_SHAPE( 2)%Col_1(1:8) = 'BOX     '   ;  BAR_SHAPE( 2)%Col_2 = 4  
      BAR_SHAPE( 3)%Col_1(1:8) = 'BOX1    '   ;  BAR_SHAPE( 3)%Col_2 = 6  
      BAR_SHAPE( 4)%Col_1(1:8) = 'CHAN    '   ;  BAR_SHAPE( 4)%Col_2 = 4  
      BAR_SHAPE( 5)%Col_1(1:8) = 'CHAN1   '   ;  BAR_SHAPE( 5)%Col_2 = 4  
      BAR_SHAPE( 6)%Col_1(1:8) = 'CHAN2   '   ;  BAR_SHAPE( 6)%Col_2 = 4  
      BAR_SHAPE( 7)%Col_1(1:8) = 'CROSS   '   ;  BAR_SHAPE( 7)%Col_2 = 4  
      BAR_SHAPE( 8)%Col_1(1:8) = 'H       '   ;  BAR_SHAPE( 8)%Col_2 = 4  
      BAR_SHAPE( 9)%Col_1(1:8) = 'HAT     '   ;  BAR_SHAPE( 9)%Col_2 = 4  
      BAR_SHAPE(10)%Col_1(1:8) = 'HEXA    '   ;  BAR_SHAPE(10)%Col_2 = 3  
      BAR_SHAPE(11)%Col_1(1:8) = 'I       '   ;  BAR_SHAPE(11)%Col_2 = 6  
      BAR_SHAPE(12)%Col_1(1:8) = 'I1      '   ;  BAR_SHAPE(12)%Col_2 = 4  
      BAR_SHAPE(13)%Col_1(1:8) = 'ROD     '   ;  BAR_SHAPE(13)%Col_2 = 1  
      BAR_SHAPE(14)%Col_1(1:8) = 'T       '   ;  BAR_SHAPE(14)%Col_2 = 4  
      BAR_SHAPE(15)%Col_1(1:8) = 'T1      '   ;  BAR_SHAPE(15)%Col_2 = 4  
      BAR_SHAPE(16)%Col_1(1:8) = 'T2      '   ;  BAR_SHAPE(16)%Col_2 = 4  
      BAR_SHAPE(17)%Col_1(1:8) = 'TUBE    '   ;  BAR_SHAPE(17)%Col_2 = 2  
      BAR_SHAPE(18)%Col_1(1:8) = 'Z       '   ;  BAR_SHAPE(18)%Col_2 = 4  

      NPBAR         = NPBAR  + 1
      PBAR(NPBAR,3) = NPBARL

! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
      NAME = JCARD(1)
      ID   = JCARD(2)
 
! Code not written for cross-section dimension data spilling over to a 2nd cont entry so give error if BAR_SHAPE(i)%Col_2 > 7

      DO I=1,NS
         IF (BAR_SHAPE(I)%Col_2 > 7) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1172) SUBR_NAME, NAME
            WRITE(F06,1172) SUBR_NAME, NAME
            CALL OUTA_HERE ( 'Y' )
         ENDIF
      ENDDO

! Read and check data on parent card

      CALL I4FLD ( JCARD(2), JF(2), PROP_ID )              ! Read property ID and enter into array PBAR
      IF (IERRFL(2) == 'N') THEN
         DO J=1,NPBAR-1
            IF (PROP_ID == PBAR(J,1)) THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1145) JCARD(1),PROP_ID
               WRITE(F06,1145) JCARD(1),PROP_ID
               EXIT
             ENDIF
         ENDDO   
         PBAR(NPBAR,1) = PROP_ID
      ENDIF
 
      CALL I4FLD ( JCARD(3), JF(3), MATL_ID )              ! Read material ID and enter into array PBAR
      IF (IERRFL(3) == 'N') THEN
         IF (MATL_ID <= 0) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1192) JF(3), JCARD(1), JCARD(2), ' > 0 ', MATL_ID 
            WRITE(F06,1192) JF(3), JCARD(1), JCARD(2), ' > 0 ', MATL_ID 
         ELSE
            PBAR(NPBAR,2) = MATL_ID
         ENDIF
      ENDIF

      NUM_D = 0
      CALL LEFT_ADJ_BDFLD ( JCARD(5) )                     ! Read cross-section type in field 5
      CALL CHAR_FLD ( JCARD(5), JF(5), CS_TYPE )
      FOUND = 'N'
      DO I=1,NS
         IF (CS_TYPE(1:8) == BAR_SHAPE(I)%Col_1) THEN
            NUM_D = BAR_SHAPE(I)%Col_2
            FOUND = 'Y'
         ENDIF
      ENDDO
      IF (FOUND == 'N') THEN                               ! If a valid CS_TYPE not found give error and return
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1162) CS_TYPE(1:8), JF(5), NAME, JCARD(5)
         WRITE(F06,1162) CS_TYPE(1:8), JF(5), NAME, JCARD(5)
         RETURN
      ENDIF

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,0,5,0,0,0,0 )     ! Make sure that there are no imbedded blanks in fields 2,3,5
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,4,0,6,7,8,9 )   ! Issue warning if fields 4 and 6-9 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
 
      PBARL_TYPE(1:) = ' '
      PBARL_TYPE(1:) = CS_TYPE(1:)

! Read D(i) dimension values on continuation entries

      IERR = 0                                             ! Mandatory 1st continuation entry
      IF (LARGE_FLD_INP == 'N') THEN
         CALL NEXTC  ( CARD, ICONT, IERR )
      ELSE
         CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
         CARD = CHILD
      ENDIF
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      IF (ICONT == 1) THEN

         KD = 0
D_do1:   DO J=2,9                                          ! --- Read cross-section dimension data
            IF (KD < NUM_D) THEN
               IF (JCARD(J)(1:) /= ' ') THEN
                  CALL R8FLD ( JCARD(J), JF(J), R8INP )
                  IF (IERRFL(J) == 'N') THEN
                     KD = KD + 1
                     D(KD) = R8INP
                     IF (D(KD) <= ZERO) THEN
                        FATAL_ERR = FATAL_ERR + 1
                        WRITE(ERR,1174) NAME, ID, JF(J), D(KD)
                        WRITE(F06,1174) NAME, ID, JF(J), D(KD)
                        CYCLE D_do1
                     ENDIF
                     CYCLE D_do1
                  ENDIF
               ELSE
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1171) NAME, ID, CS_TYPE(1:8), NUM_D
                  WRITE(F06,1171) NAME, ID, CS_TYPE(1:8), NUM_D
                  EXIT D_do1
               ENDIF
            ELSE
               EXIT D_do1
            ENDIF
         ENDDO D_do1
         IF (KD <= 7) THEN
            CALL R8FLD( JCARD(KD+2), JF(KD+2), R8INP )        ! ---  Read NSM
            IF (IERRFL(KD+1) == 'N') THEN
               NSM = R8INP
            ENDIF
         ENDIF

      ELSE

         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1136) NAME, ID
         WRITE(F06,1136) NAME, ID

      ENDIF
  

! Call routines to calc bar cross-section props, depending on CS_TYPE

      IERR = 0
      IF      (CS_TYPE(1:8) == BAR_SHAPE( 1)%Col_1) THEN   ;   NUM_D = BAR_SHAPE( 1)%Col_2   ;   CALL SECTION_PROPS_BAR   ( IERR )
      ELSE IF (CS_TYPE(1:8) == BAR_SHAPE( 2)%Col_1) THEN   ;   NUM_D = BAR_SHAPE( 2)%Col_2   ;   CALL SECTION_PROPS_BOX   ( IERR )
      ELSE IF (CS_TYPE(1:8) == BAR_SHAPE( 3)%Col_1) THEN   ;   NUM_D = BAR_SHAPE( 3)%Col_2   ;   CALL SECTION_PROPS_BOX1  ( IERR )
      ELSE IF (CS_TYPE(1:8) == BAR_SHAPE( 4)%Col_1) THEN   ;   NUM_D = BAR_SHAPE( 4)%Col_2   ;   CALL SECTION_PROPS_CHAN  ( IERR )
      ELSE IF (CS_TYPE(1:8) == BAR_SHAPE( 5)%Col_1) THEN   ;   NUM_D = BAR_SHAPE( 5)%Col_2   ;   CALL SECTION_PROPS_CHAN1 ( IERR )
      ELSE IF (CS_TYPE(1:8) == BAR_SHAPE( 6)%Col_1) THEN   ;   NUM_D = BAR_SHAPE( 6)%Col_2   ;   CALL SECTION_PROPS_CHAN2 ( IERR )
      ELSE IF (CS_TYPE(1:8) == BAR_SHAPE( 7)%Col_1) THEN   ;   NUM_D = BAR_SHAPE( 7)%Col_2   ;   CALL SECTION_PROPS_CROSS ( IERR )
      ELSE IF (CS_TYPE(1:8) == BAR_SHAPE( 8)%Col_1) THEN   ;   NUM_D = BAR_SHAPE( 8)%Col_2   ;   CALL SECTION_PROPS_H     ( IERR )
      ELSE IF (CS_TYPE(1:8) == BAR_SHAPE( 9)%Col_1) THEN   ;   NUM_D = BAR_SHAPE( 9)%Col_2   ;   CALL SECTION_PROPS_HAT   ( IERR )
      ELSE IF (CS_TYPE(1:8) == BAR_SHAPE(10)%Col_1) THEN   ;   NUM_D = BAR_SHAPE(10)%Col_2   ;   CALL SECTION_PROPS_HEXA  ( IERR )
      ELSE IF (CS_TYPE(1:8) == BAR_SHAPE(11)%Col_1) THEN   ;   NUM_D = BAR_SHAPE(11)%Col_2   ;   CALL SECTION_PROPS_I     ( IERR )
      ELSE IF (CS_TYPE(1:8) == BAR_SHAPE(12)%Col_1) THEN   ;   NUM_D = BAR_SHAPE(12)%Col_2   ;   CALL SECTION_PROPS_I1    ( IERR )
      ELSE IF (CS_TYPE(1:8) == BAR_SHAPE(13)%Col_1) THEN   ;   NUM_D = BAR_SHAPE(13)%Col_2   ;   CALL SECTION_PROPS_ROD   ( IERR )
      ELSE IF (CS_TYPE(1:8) == BAR_SHAPE(14)%Col_1) THEN   ;   NUM_D = BAR_SHAPE(14)%Col_2   ;   CALL SECTION_PROPS_T     ( IERR )
      ELSE IF (CS_TYPE(1:8) == BAR_SHAPE(15)%Col_1) THEN   ;   NUM_D = BAR_SHAPE(15)%Col_2   ;   CALL SECTION_PROPS_T1    ( IERR )
      ELSE IF (CS_TYPE(1:8) == BAR_SHAPE(16)%Col_1) THEN   ;   NUM_D = BAR_SHAPE(16)%Col_2   ;   CALL SECTION_PROPS_T2    ( IERR )
      ELSE IF (CS_TYPE(1:8) == BAR_SHAPE(17)%Col_1) THEN   ;   NUM_D = BAR_SHAPE(17)%Col_2   ;   CALL SECTION_PROPS_TUBE  ( IERR )
      ELSE IF (CS_TYPE(1:8) == BAR_SHAPE(18)%Col_1) THEN   ;   NUM_D = BAR_SHAPE(18)%Col_2   ;   CALL SECTION_PROPS_Z     ( IERR )
      ELSE
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1162) JCARD(5), JF(5), NAME, ID
         WRITE(F06,1162) JCARD(5), JF(5), NAME, ID
         RETURN
      ENDIF

      IF (IERR == 0) THEN

         RPBAR(npbar, 1) = AREA
         RPBAR(npbar, 2) = I1
         RPBAR(npbar, 3) = I2
         RPBAR(npbar, 4) = JTOR
         RPBAR(npbar, 5) = NSM
         RPBAR(npbar, 6) = Y(1)
         RPBAR(npbar, 7) = Z(1)
         RPBAR(npbar, 8) = Y(2)
         RPBAR(npbar, 9) = Z(2)
         RPBAR(npbar,10) = Y(3)
         RPBAR(npbar,11) = Z(3)
         RPBAR(npbar,12) = Y(4)
         RPBAR(npbar,13) = Z(4)
         IF (PBARLSHR == 'Y') THEN
            RPBAR(npbar,14) = K1
            RPBAR(npbar,15) = K2
         ELSE
            RPBAR(npbar,14) = ZERO
            RPBAR(npbar,15) = ZERO
         ENDIF
         RPBAR(npbar,16) = I12

      CALL CHECK_BAR_MOIs ( 'PBARL', ID, I1, I2, I12, IERR )     ! Call subr to check sensibility of I1, I2, I12 combinations
      RPBAR(NPBAR, 2) = I1
      RPBAR(NPBAR, 3) = I2
      RPBAR(NPBAR,16) = I12
      IF (IERR /= 0) THEN
         FATAL_ERR = FATAL_ERR + 1
      ENDIF

      ELSE

         MSG(1:) = ' '
         IF (IERR ==  1) MSG = 'INVALID DIMENSIONS FOR CROSS-SECTION'
         IF (IERR == 99) MSG = 'CODE NOT WRITTEN FOR THIS CROSS-SECTION YET'
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,1177) NAME, ID, CS_TYPE(1:8), MSG
         WRITE(F06,1177) NAME, ID, CS_TYPE(1:8), MSG

      ENDIF
 
! Write PBAR equivalent Bulk Data entries:
!
!     IF (ECHO(1:4) /= 'NONE') THEN
!        CALL WRITE_PBAR_EQUIV
!     ENDIF
!
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1136 FORMAT(' *ERROR  1136: REQUIRED CONTINUATION FOR ',A,' ID = ',A,' MISSING')
 
 1145 FORMAT(' *ERROR  1145: DUPLICATE ',A,' ENTRY WITH ID = ',I8)
 
 1162 FORMAT(' *ERROR  1162: INVALID NAME = "',A,'" FOR CROSS-SECTION IN FIELD ',I2,' ON ',A,' ENTRY = ',A,                        &
                          '. CHECK USERS MANUAL FOR VALID ENTRIES')

 1171 FORMAT(' *ERROR  1171: CONTINUATION ENTRY  FOR ',A,A,' DOES NOT HAVE ENOUGH DIMENSION DATA FOR CROSS-SECTION "',A,'".',      &
                           ' ENTRY SHOULD HAVE ',I2,' VALUES')

 1172 FORMAT(' *ERROR  1172: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' CODE NOT WRITTEN TO READ MORE THAN 7 CROSS-SECTION DIMENSIONS FOR ',A)

 1174 FORMAT(' *ERROR  1174: CROSS SECTION DIMENSIONS ON ',A,A,' MUST BE > 0 BUT FIELD ',I2,' HAS VALUE ',1ES10.2)

 1177 FORMAT(' *ERROR  1177: ',A,A,' FOR "',A,'" CROSS-SECTION: ',A)

 1192 FORMAT(' *ERROR  1192: ID IN FIELD ',I3,' OF ',A,A,' MUST BE ',A,' BUT IS = ',I8)


! **********************************************************************************************************************************
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE WRITE_PBAR_EQUIV

      USE PARAMS, ONLY                :  PBARLDEC, PBARLSHR
      USE SCONTR, ONLY                :  JCARD_LEN

      IMPLICIT NONE

      CHARACTER( 6*BYTE)              :: FMT0              ! Format used in writing data to char array ICARD
      CHARACTER( 8*BYTE)              :: FMT1              ! Format used in writing data to char array ICARD
      CHARACTER(LEN=JCARD_LEN)        :: ICARD(2:9)        ! Char array for fields 2-9 of equiv PBAR B.D. entries

! **********************************************************************************************************************************
      WRITE(F06,101)
      WRITE(F06,102) ID
      WRITE(F06,104)

      FMT0 = '(8X)'

      IF      (PBARLDEC == 0) THEN
         FMT1 = '(F8.0)'
      ELSE IF (PBARLDEC == 1) THEN
         FMT1 = '(F8.1)'
      ELSE IF (PBARLDEC == 2) THEN
         FMT1 = '(F8.2)'
      ELSE IF (PBARLDEC == 3) THEN
         FMT1 = '(F8.3)'
      ELSE IF (PBARLDEC == 4) THEN
         FMT1 = '(F8.4)'
      ELSE IF (PBARLDEC == 5) THEN
         FMT1 = '(F8.5)'
      ELSE IF (PBARLDEC == 6) THEN
         FMT1 = '(F8.6)'
      ELSE
         FMT1 = FMT0                                       ! Write blank fields if PBARLDEC is not caught being out of range 0-6
      ENDIF

      WRITE(ICARD(2),201)  PROP_ID   ;   CALL LEFT_ADJ_BDFLD ( ICARD(2) )
      WRITE(ICARD(3),201)  MATL_ID   ;   CALL LEFT_ADJ_BDFLD ( ICARD(3) )
      WRITE(ICARD(4),FMT1) AREA      ;   CALL LEFT_ADJ_BDFLD ( ICARD(4) )
      WRITE(ICARD(5),FMT1) I1;       ;   CALL LEFT_ADJ_BDFLD ( ICARD(5) )
      WRITE(ICARD(6),FMT1) I2;       ;   CALL LEFT_ADJ_BDFLD ( ICARD(6) )
      WRITE(ICARD(7),FMT1) JTOR;     ;   CALL LEFT_ADJ_BDFLD ( ICARD(7) )
      WRITE(ICARD(8),FMT1) NSM;      ;   CALL LEFT_ADJ_BDFLD ( ICARD(8) )
      WRITE(ICARD(9),FMT0)
      WRITE(F06,301) (ICARD(I),I=2,8)                      ! Write parent entry

      WRITE(ICARD(2),FMT1) Y(1)      ;   CALL LEFT_ADJ_BDFLD ( ICARD(2) )
      WRITE(ICARD(3),FMT1) Z(1)      ;   CALL LEFT_ADJ_BDFLD ( ICARD(3) )
      WRITE(ICARD(4),FMT1) Y(2)      ;   CALL LEFT_ADJ_BDFLD ( ICARD(4) )
      WRITE(ICARD(5),FMT1) Z(2)      ;   CALL LEFT_ADJ_BDFLD ( ICARD(5) )
      WRITE(ICARD(6),FMT1) Y(3)      ;   CALL LEFT_ADJ_BDFLD ( ICARD(6) )
      WRITE(ICARD(7),FMT1) Z(3)      ;   CALL LEFT_ADJ_BDFLD ( ICARD(7) )
      WRITE(ICARD(8),FMT1) Y(4)      ;   CALL LEFT_ADJ_BDFLD ( ICARD(8) )
      WRITE(ICARD(9),FMT1) Z(4)      ;   CALL LEFT_ADJ_BDFLD ( ICARD(9) )
      WRITE(F06,302) (ICARD(I),I=2,9)                      ! Write 1st cont entry

      IF (PBARLSHR == 'Y') THEN                            ! Don't write K1, K2 if param PBARLSHR = 'N'
         WRITE(ICARD(2),FMT1) K1     ;   CALL LEFT_ADJ_BDFLD ( ICARD(2) )
         WRITE(ICARD(3),FMT1) K2     ;   CALL LEFT_ADJ_BDFLD ( ICARD(3) )
      ELSE
         WRITE(ICARD(2),FMT0)        ;   CALL LEFT_ADJ_BDFLD ( ICARD(2) )
         WRITE(ICARD(3),FMT0)        ;   CALL LEFT_ADJ_BDFLD ( ICARD(3) )
      ENDIF
      WRITE(ICARD(4),FMT1) I12       ;   CALL LEFT_ADJ_BDFLD ( ICARD(4) )
      WRITE(ICARD(5),FMT0)      
      WRITE(ICARD(6),FMT0)      
      WRITE(ICARD(7),FMT0)      
      WRITE(ICARD(8),FMT0)      
      WRITE(ICARD(9),FMT0)
      IF ((PBARLSHR == 'Y') .OR. (DABS(I12) > 0)) THEN     ! Only write 2nd cont entry if we want K1, K2 included or I12 is /= 0.
         WRITE(F06,302) (ICARD(I),I=2,9)
      ENDIF

      WRITE(F06,101)

! **********************************************************************************************************************************
  101 FORMAT('$*******************************************************************************')

  102 FORMAT('$ PBAR equivalent for PBARL ',A)

  104 FORMAT('$--1---|-------2-------|-------3-------|-------4-------|-------5-------|-------6-------|-------7-------|-------8---',&
                 '----|-------9-------|--10---|')

  201 FORMAT(I8)

  210 FORMAT(F8.0)

  211 FORMAT(F8.1)

  212 FORMAT(F8.2)

  213 FORMAT(F8.3)

  214 FORMAT(F8.4)

  215 FORMAT(F8.5)

  216 FORMAT(F8.6)

  301 FORMAT('PBAR    ',7A8)

  302 FORMAT('        ',8A8)

      END SUBROUTINE WRITE_PBAR_EQUIV

! ##################################################################################################################################

      SUBROUTINE SECTION_PROPS_BAR ( JERR )

      IMPLICIT NONE

      INTEGER(LONG), INTENT(OUT)      :: JERR              ! Error indicator

      REAL(DOUBLE)                    :: A                 ! Short dimension
      REAL(DOUBLE)                    :: B                 ! Longer dimension
      REAL(DOUBLE)                    :: BETA              ! Ratio of dimensions

! **********************************************************************************************************************************
      JERR = 0

      A    = D(1)                                          ! D(1), D(2) /= 0 was checked when D(i) were input
      B    = D(2)

      AREA =  A*B
      I1   =  A*B*B*B/TWELVE
      I2   =  B*A*A*A/TWELVE
      I12  =  ZERO
      IF (A > B) THEN
         BETA = B/A
         JTOR =  A*B*B*B*THIRD*(ONE - 0.630D0*BETA*(ONE - BETA*BETA*BETA*BETA/TWELVE))
      ELSE
         BETA = A/B
         JTOR =  B*A*A*A*THIRD*(ONE - 0.630D0*BETA*(ONE - BETA*BETA*BETA*BETA/TWELVE))
      ENDIF

      K1   =  FIVE/SIX
      K2   =  FIVE/SIX

      Y(1) =  HALF*B   ;   Z(1) =  HALF*A
      Y(2) = -Y(1)     ;   Z(2) =  Z(1)
      Y(3) = -Y(1)     ;   Z(3) = -Z(1)
      Y(4) =  Y(1)     ;   Z(4) = -Z(1)

      END SUBROUTINE SECTION_PROPS_BAR

! ##################################################################################################################################

      SUBROUTINE SECTION_PROPS_BOX ( JERR )

      IMPLICIT NONE

      INTEGER(LONG), INTENT(OUT)      :: JERR              ! Error indicator

      REAL(DOUBLE)                    :: B1,H1             ! Outside dimensions of BOX
      REAL(DOUBLE)                    :: B2,H2             ! Inside  dimensions of BOX
      REAL(DOUBLE)                    :: ASTAR             ! Area enclosed by middle line of wall
      REAL(DOUBLE)                    :: S                 ! Length of middle wall
      REAL(DOUBLE)                    :: T                 ! Thickness of wall

! **********************************************************************************************************************************
      JERR = 0

      B1    = D(1)
      H1    = D(2)
      B2    = D(1) - TWO*D(4)
      H2    = D(2) - TWO*D(3)
      S     = B1 + B2 + H1 + H2
      T     = HALF*(D(3) + D(4))
      ASTAR = QUARTER*(B1 + B2)*(H1 + H2)

      IF (B2 <= ZERO) JERR = 1
      IF (H2 <= ZERO) JERR = 1
      IF (JERR > 0) RETURN

      AREA =  B1*H1 - B2*H2
      IF (AREA <= ZERO) THEN
         JERR = 1
         RETURN
      ENDIF

      I1   =  (B1*H1*H1*H1 - B2*H2*H2*H2)/TWELVE
      I2   =  (H1*B1*B1*B1 - H2*B2*B2*B2)/TWELVE
      I12  =  ZERO
      JTOR =  FOUR*ASTAR*ASTAR*T/S
      K1   =  H2/D(2)
      K2   =  B2/D(1)
      Y(1) =  HALF*D(2)   ;   Z(1) =  HALF*D(1)
      Y(2) = -HALF*D(2)   ;   Z(1) =  HALF*D(1)
      Y(3) = -HALF*D(2)   ;   Z(3) = -HALF*D(1)
      Y(4) =  HALF*D(2)   ;   Z(4) = -HALF*D(1)

      END SUBROUTINE SECTION_PROPS_BOX

! ##################################################################################################################################

      SUBROUTINE SECTION_PROPS_BOX1 ( JERR )

      IMPLICIT NONE

      INTEGER(LONG), INTENT(OUT)      :: JERR              ! Error indicator

      REAL(DOUBLE)                    :: ASTAR             ! Area enclosed by middle line of wall
      REAL(DOUBLE)                    :: DEN               ! Intermediate variable
      REAL(DOUBLE)                    :: H                 ! Height of box
      REAL(DOUBLE)                    :: W                 ! Width  of box
      REAL(DOUBLE)                    :: YCG               ! Y location of C.G. relative to lower left corner
      REAL(DOUBLE)                    :: ZCG               ! Z location of C.G. relative to lower left corner
      REAL(DOUBLE)                    :: TL                ! Thickness of left   web
      REAL(DOUBLE)                    :: TR                ! Thickness of right  web
      REAL(DOUBLE)                    :: TB                ! Thickness of bottom web
      REAL(DOUBLE)                    :: TT                ! Thickness of top    web
   
! **********************************************************************************************************************************
      JERR = 0

      H  = D(2)
      W  = D(1)
      TL = D(6)
      TR = D(5)
      TB = D(4)
      TT = D(3)

      IF (W - TL - TR <= ZERO) JERR = 1
      IF (H - TT - TB <= ZERO) JERR = 1

      ASTAR = (W - HALF*(TL + TR))*(H - HALF*(TT + TB))
      DEN   = (H - HALF*(TT + TB))*(ONE/TL + ONE/TR) + (W - HALF*(TL + TR))*(ONE/TT + ONE/TB)
      IF (DEN > 0) THEN
         JTOR  =  FOUR*ASTAR*ASTAR/DEN
      ELSE
         JERR = 1
      ENDIF

      AREA = H*(TL + TR) + (W - TL - TR)*(TT + TB)
      IF (AREA <= ZERO) THEN
         JERR = 1
      ENDIF

      IF (JERR > 0) RETURN

      YCG  =  (H*(TL + TR)*HALF*H + (W - TL - TR)*(HALF*TB + (H - HALF*TT)))/AREA
      ZCG  =  (H*TL*HALF*TL + H*TR*(W - HALF*TR) + (W - TL -TR)*(TT + TB)*(TL + HALF*(W - TL - TR)))/AREA
                                                           ! Calc I1,I2 as I for each of parts about own C.G + part area times D^2
      I1   =  (TL + TR)*H*H*H/TWELVE + H*TL*(YCG - HALF*H)*(YCG - HALF*H)                                                          &
            + (W - TL - TR)*(TT*TT*TT + TB*TB*TB)/TWELVE + (W - TL -TR)*TB*(YCG - HALF*TB)*(YCG - HALF*TB)                         &
            +                                              (W - TL -TR)*TT*(H - HALF*TT - YCG)*(H - HALF*TT - YCG)
      I2   =  H*(TL*TL*TL + TR*TR*TR)/TWELVE + H*TL*(ZCG - HALF*TL)*(ZCG - HALF*TL) + H*TR*(W - HALF*TR -ZCG)*(W - HALF*TR -ZCG)   &
            + (TT + TB)*W*W*W/TWELVE + W*(TT + TB)*(HALF*W - ZCG)*(HALF*W - ZCG)
      I12  =  ZERO
      Y(1) =  H - YCG   ;   Z(1) =  W - ZCG
      Y(2) = -YCG       ;   Z(2) =  Z(1)
      Y(3) =  Y(2)      ;   Z(3) = -ZCG
      Y(4) =  Y(1)      ;   Z(4) =  Z(3)
      K1   =  (H - TT - TB)/H
      K2   =  (W - TL - TR)/W

      END SUBROUTINE SECTION_PROPS_BOX1

! ##################################################################################################################################

      SUBROUTINE SECTION_PROPS_CHAN ( JERR )

      IMPLICIT NONE

      INTEGER(LONG), INTENT(OUT)      :: JERR              ! Error indicator

      REAL(DOUBLE)                    :: ALPHA = 1.12D0    ! Constant in calc of JTOR for open cross-sections
      REAL(DOUBLE)                    :: B                 ! Overall width
      REAL(DOUBLE)                    :: DEN               ! Intermediate variable
      REAL(DOUBLE)                    :: H                 ! Overall height
      REAL(DOUBLE)                    :: TW                ! Web thickness
      REAL(DOUBLE)                    :: TF                ! Flange thickness
      REAL(DOUBLE)                    :: ZSHR              ! Z location of shear center relative to center line of vertical web

! **********************************************************************************************************************************
      JERR = 0

      B  = D(1)
      H  = D(2)
      TW = D(3)
      TF = D(4)

      DEN  = SIX*(B - HALF*TW)*TF + (H - TF)*TW
      IF (DEN > ZERO) THEN
         ZSHR = -THREE*TF*(B - HALF*TW)*(B - HALF*TW)/DEN
      ELSE
         JERR = 1
      ENDIF

      IF (H - TWO*TF < ZERO) JERR = 1
      IF (JERR > 0) RETURN

      AREA =  (H - TWO*TF)*TW + TWO*B*TF
      I1   =  TW*H*H*H/TWELVE + TWO*((B - TW)*TF*TF*TF/TWELVE + (B - TW)*TF*QUARTER*(H - TF)*(H - TF))

      I2   =  H*TW*TW*TW/TWELVE + H*TW*ZSHR*ZSHR                                                                                   &
            + TF*(B - TW)*(B - TW)*(B - TW) + TWO*TF*(B - TW)*(-ZSHR + HALF*(B + TW))*(-ZSHR + HALF*(B + TW))

      I12  =  ZERO
      JTOR =  ALPHA*THIRD*(H*TW*TW*TW + (B - TF)*TF*TF*TF)
      Y(1) =  HALF*H   ;   Z(1) = -ZSHR - HALF*TW + B 
      Y(2) = -Y(1)     ;   Z(2) =  Z(1) 
      Y(3) = -Y(1)     ;   Z(3) = -ZSHR - HALF*TW 
      Y(4) =  Y(1)     ;   Z(4) =  Z(3)
      K1   =  (H - TWO*TF)/H
      K2   =  (B - TW)/B

      END SUBROUTINE SECTION_PROPS_CHAN

! ##################################################################################################################################

      SUBROUTINE SECTION_PROPS_CHAN1 ( JERR )

      IMPLICIT NONE

      INTEGER(LONG), INTENT(OUT)      :: JERR              ! Error indicator

      REAL(DOUBLE)                    :: ALPHA = 1.12D0    ! Constant in calc of JTOR for open cross-sections
      REAL(DOUBLE)                    :: B                 ! Overall width
      REAL(DOUBLE)                    :: DEN               ! Intermediate variable
      REAL(DOUBLE)                    :: H                 ! Overall height
      REAL(DOUBLE)                    :: TW                ! Web thickness
      REAL(DOUBLE)                    :: TF                ! Flange thickness
      REAL(DOUBLE)                    :: ZSHR              ! Z location of shear center relative to lower left corner

! **********************************************************************************************************************************
      JERR = 0

      B  = D(1) + D(2)
      H  = D(4)
      TW = D(2)
      TF = HALF*(D(4) - D(3))

      DEN  = SIX*(B - HALF*TW)*TF + (H - TF)*TW
      IF (DEN > ZERO) THEN
         ZSHR = -THREE*TF*(B - HALF*TW)*(B - HALF*TW)/DEN
      ELSE
         JERR = 1
      ENDIF

      IF (TF < ZERO) JERR = 1
      IF (JERR > 0) RETURN

      AREA =  (H - TWO*TF)*TW + TWO*B*TF
      I1   =  TW*H*H*H/TWELVE + TWO*((B - TW)*TF*TF*TF/TWELVE + (B - TW)*TF*QUARTER*(H - TF)*(H - TF))

      I2   =  H*TW*TW*TW/TWELVE + H*TW*ZSHR*ZSHR                                                                                   &
            + TF*(B - TW)*(B - TW)*(B - TW) + TWO*TF*(B - TW)*(-ZSHR + HALF*(B + TW))*(-ZSHR + HALF*(B + TW))

      I12  =  ZERO
      JTOR =  ALPHA*THIRD*(H*TW*TW*TW + (B - TF)*TF*TF*TF)
      Y(1) =  HALF*H   ;   Z(1) = -ZSHR - HALF*TW + B 
      Y(2) = -Y(1)     ;   Z(2) =  Z(1) 
      Y(3) = -Y(1)     ;   Z(3) = -ZSHR - HALF*TW 
      Y(4) =  Y(1)     ;   Z(4) =  Z(3)
      K1   =  (H - TWO*TF)/H
      K2   =  (B - TW)/B

      END SUBROUTINE SECTION_PROPS_CHAN1

! ##################################################################################################################################

      SUBROUTINE SECTION_PROPS_CHAN2 ( JERR )

      IMPLICIT NONE

      INTEGER(LONG), INTENT(OUT)      :: JERR              ! Error indicator

      REAL(DOUBLE)                    :: ALPHA = 1.12D0    ! Constant in calc of JTOR for open cross-sections
      REAL(DOUBLE)                    :: B                 ! Overall width
      REAL(DOUBLE)                    :: DEN               ! Intermediate variable
      REAL(DOUBLE)                    :: H                 ! Overall height
      REAL(DOUBLE)                    :: TW                ! Web thickness
      REAL(DOUBLE)                    :: TF                ! Flange thickness
      REAL(DOUBLE)                    :: ZSHR              ! Z location of shear center relative to lower left corner

! **********************************************************************************************************************************
      JERR = 0

      B  = D(3)
      H  = D(4)
      TW = D(2)
      TF = D(1)

      DEN  = SIX*(B - HALF*TW)*TF + (H - TF)*TW
      IF (DEN > ZERO) THEN
         ZSHR = -THREE*TF*(B - HALF*TW)*(B - HALF*TW)/DEN
      ELSE
         JERR = 1
      ENDIF

      IF (TF < ZERO) JERR = 1
      IF (JERR > 0) RETURN

      AREA =  (H - TWO*TF)*TW + TWO*B*TF
      I2   =  H*TW*TW*TW/TWELVE + H*TW*ZSHR*ZSHR                                                                                   &
            + TF*(B - TW)*(B - TW)*(B - TW) + TWO*TF*(B - TW)*(-ZSHR + HALF*(B + TW))*(-ZSHR + HALF*(B + TW))

      I1   =  TW*H*H*H/TWELVE + TWO*((B - TW)*TF*TF*TF/TWELVE + (B - TW)*TF*QUARTER*(H - TF)*(H - TF))

      I12  =  ZERO
      JTOR =  ALPHA*THIRD*(H*TW*TW*TW + (B - TF)*TF*TF*TF)
      Y(1) =  HALF*H   ;   Z(1) = -ZSHR - HALF*TW + B 
      Y(2) = -Y(1)     ;   Z(2) =  Z(1) 
      Y(3) = -Y(1)     ;   Z(3) = -ZSHR - HALF*TW 
      Y(4) =  Y(1)     ;   Z(4) =  Z(3)
      K1   =  (H - TWO*TF)/H
      K2   =  (B - TW)/B

      END SUBROUTINE SECTION_PROPS_CHAN2

! ##################################################################################################################################

      SUBROUTINE SECTION_PROPS_CROSS ( JERR )

      IMPLICIT NONE

      INTEGER(LONG), INTENT(OUT)      :: JERR              ! Error indicator

      REAL(DOUBLE)                    :: ALPHA = 1.0D0     ! Constant in calc of JTOR for open cross-sections
!                                                            Pilkey has 1.17 on #10 in table 2.5 but MSC uses 1.0

      REAL(DOUBLE)                    :: AVERT             ! Area of vertical member
      REAL(DOUBLE)                    :: AHORZ             ! Area of horizontal member
      REAL(DOUBLE)                    :: HV                ! Height of vert member
      REAL(DOUBLE)                    :: WH                ! Width of horiz member
      REAL(DOUBLE)                    :: TV                ! Thickness of vertical member
      REAL(DOUBLE)                    :: TH                ! Thickness of horizontal member
      REAL(DOUBLE)                    :: W0                ! Width of left and righ

! **********************************************************************************************************************************
      JERR = 0

      HV = D(3)
      W0 = HALF*D(1)
      WH = D(1) + D(2)
      TV = D(2)
      TH = D(4)

      AVERT = HV*TV
      AHORZ = WH*TH
      AREA = AVERT + AHORZ - TV*TH
      IF (AREA <= ZERO) THEN
         JERR = 1
         RETURN
      ENDIF

      I1   =  TV*HV*HV*HV/TWELVE + TWO*W0*TH*TH*TH/TWELVE
      I2   =  HV*TV*TV*TV/TWELVE + TWO*(TH*W0*W0*W0/TWELVE + TH*W0*QUARTER*(TV + W0)*(TV+W0))
      I12  =  ZERO
      JTOR =  ALPHA*THIRD*(HV*TV*TV*TV + TWO*W0*TH*TH*TH)
      Y(1) =  HALF*HV  ;   Z(1) =  ZERO
      Y(2) =  ZERO     ;   Z(2) =  HALF*TV + W0
      Y(3) = -Y(1)     ;   Z(3) =  ZERO
      Y(4) =  ZERO     ;   Z(4) = -Z(2) 

      K1   =  (FIVE/SIX)*AVERT/AREA
      K2   =  (FIVE/SIX)*AHORZ/AREA

      END SUBROUTINE SECTION_PROPS_CROSS

! ##################################################################################################################################

      SUBROUTINE SECTION_PROPS_H ( JERR )

      IMPLICIT NONE

      INTEGER(LONG), INTENT(OUT)      :: JERR              ! Error indicator

      REAL(DOUBLE)                    :: ALPHA = 1.0D0     ! Constant in calc of JTOR for open cross-sections
!                                                            Pilkey has 1.31, MSC uses 1.0

      REAL(DOUBLE)                    :: AVERT             ! Area of vertical member
      REAL(DOUBLE)                    :: AHORZ             ! Area of horizontal member
      REAL(DOUBLE)                    :: HF                ! Height
      REAL(DOUBLE)                    :: WW                ! Width of center member
      REAL(DOUBLE)                    :: TF                ! Thickness of vertical member
      REAL(DOUBLE)                    :: TW                ! Thickness of center member

! **********************************************************************************************************************************
      JERR = 0

      HF = D(3)
      WW = D(1)
      TF = HALF*D(2)
      TW = D(4)

      AVERT = HF*TF
      AHORZ = WW*TW
      AREA =  TWO*AVERT + AHORZ

      I1   =  TWO*TF*HF*HF*HF/TWELVE + WW*TW*TW*TW/TWELVE
      I2   =  TWO*(HF*TF*TF*TF/TWELVE + HF*TF*QUARTER*(WW + TF)*(WW + TF)) + TW*WW*WW*WW/TWELVE
      I12  =  ZERO
      JTOR =  ALPHA*THIRD*(TWO*HF*TF*TF*TF + WW*TW*TW*TW)
      Y(1) =  HALF*HF  ;   Z(1) =  HALF*WW + TF
      Y(2) = -Y(1)     ;   Z(2) =  Z(1)
      Y(3) =  Y(2)     ;   Z(3) = -Z(1)
      Y(4) =  Y(1)     ;   Z(4) =  Z(3)

      K1 = (FIVE/SIX)*TWO*AVERT/AREA
      K2 = AHORZ/AREA

      END SUBROUTINE SECTION_PROPS_H

! ##################################################################################################################################

      SUBROUTINE SECTION_PROPS_HAT ( JERR )

      IMPLICIT NONE

      INTEGER(LONG), INTENT(OUT)      :: JERR              ! Error indicator

      REAL(DOUBLE)                    :: ALPHA = 1.15D0    ! Constant in calc of JTOR for open cross-sections
!                                                            Avg: Pilkey has 1.12 for channel section and 1.17 for I section
      REAL(DOUBLE)                    :: DEN               ! Intermediate variable
      REAL(DOUBLE)                    :: NUM               ! Intermediate variable
      REAL(DOUBLE)                    :: H                 ! Height
      REAL(DOUBLE)                    :: T                 ! Thickness of hat walls
      REAL(DOUBLE)                    :: W1                ! Overall width of top of hat
      REAL(DOUBLE)                    :: W2                ! Width of base legs of hat
      REAL(DOUBLE)                    :: W3                ! Inside width of top of hat
      REAL(DOUBLE)                    :: YSHR              ! Location of shear center relative to center of top of hat

! **********************************************************************************************************************************
      JERR = 0

      H  = D(1)
      T  = D(2)
      W1 = D(3)
      W2 = D(4)
      W3 = W1 - TWO*T

      DEN  = W3*W3*W3 + SIX*W3*W3*(H - T) + SIX*(W2 + T)*W3*W3 + EIGHT*(W2 + T)*(W2 + T)*(W2 + T) + TWELVE*(W2 + T)*(W2 + T)*W3
      NUM  = (H - T)*(THREE*(H - T)*W3*W3 - EIGHT*W3*W3*W3)
      IF (DEN > ZERO) THEN
         YSHR = NUM/DEN
      ELSE
         JERR = 1
      ENDIF

      IF (H - T      < ZERO) JERR = 1
      IF (W1 - TWO*T < ZERO) JERR = 1

      AREA =  T*(TWO*W2 + TWO*H + (W1 - TWO*T))
      IF (AREA <= ZERO) THEN
         JERR = 1
      ENDIF

      IF (JERR > 0) RETURN

      I1   =  TWO*W2*T*T*T/TWELVE + TWO*W2*T*(H - T + YSHR)*T*(H - T + YSHR)                                                       &
            + TWO*T*H*H*H/TWELVE + TWO*H*T*(HALF*H + YSHR - HALF*T)*(HALF*H + YSHR - HALF*T)                                       &
            + W3*T*T*T/TWELVE + W3*T*(HALF*T + YSHR)*(HALF*T + YSHR)

      I2   =  TWO*T*W2*W2*W2/TWELVE + W2*T*QUARTER*(W1 + W2)*(W1 + W2)                                                             &
            + TWO*H*T*T*T/TWELVE + TWO*H*T*(W3 + HALF*T)*(W3 + HALF*T)                                                             &
            + T*(W3*W3*W3)/TWELVE

      I12  =  ZERO
      JTOR =  ALPHA*THIRD*( TWO*W2 + TWO*H + (W1 - TWO*T) )*T*T*T
      Y(1) =  HALF*T + YSHR       ; Z(1) =  HALF*W1
      Y(2) = -YSHR + HALF*T - H   ; Z(2) =  HALF*W1 + W2
      Y(3) =  Y(2)                ; Z(3) = -Z(2)
      Y(4) =  Y(1)                ; Z(4) = -Z(1)
      K1   =  (H - TWO*T)/H
      K2   =  (W1 + W3)/(W1 + W2)

      END SUBROUTINE SECTION_PROPS_HAT

! ##################################################################################################################################

      SUBROUTINE SECTION_PROPS_HEXA ( JERR )

      IMPLICIT NONE

      INTEGER(LONG), INTENT(OUT)      :: JERR              ! Error indicator

      REAL(DOUBLE)                    :: BETA              ! Ratio of dimensions
      REAL(DOUBLE)                    :: H0                ! Overall height
      REAL(DOUBLE)                    :: HT                ! Height of triangular portion
      REAL(DOUBLE)                    :: W0                ! Overall width
      REAL(DOUBLE)                    :: W1                ! Width of base
      REAL(DOUBLE)                    :: WAVG              ! Average width
      REAL(DOUBLE)                    :: WT                ! Width of triangular portion

! **********************************************************************************************************************************
      JERR = 0

      H0   = D(3)
      W0   = D(2)
      WT   = D(1)
      W1   = W0 -TWO*WT
      WAVG = HALF*(W0 + W1)
      HT   = HALF*H0

      AREA =  W1*H0 + TWO*WT*HT 
      I1   =  W1*H0*H0*H0/TWELVE + FOUR*( WT*HT*HT*HT/(THREE*TWELVE) + HALF*WT*HT*HT*HT/NINE )
      I2   =  H0*W1*W1*W1/TWELVE + FOUR*( HT*WT*WT*WT/(THREE*TWELVE) + HALF*WT*HT*WT*WT/NINE )
      I12  =  ZERO
      IF (WAVG > H0) THEN
         BETA = H0/WAVG
         JTOR =  WAVG*H0*H0*H0*THIRD*(ONE - 0.630D0*BETA*(ONE - BETA*BETA*BETA*BETA/TWELVE))
      ELSE
         BETA = WAVG/H0
         JTOR =  H0*WAVG*WAVG*WAVG*THIRD*(ONE - 0.630D0*BETA*(ONE - BETA*BETA*BETA*BETA/TWELVE))
      ENDIF
      Y(1) =  HT     ;   Z(1) =  ZERO
      Y(2) = -HT     ;   Z(2) =  ZERO
      Y(3) =  ZERO   ;   Z(3) =  HALF*W0
      Y(4) =  ZERO   ;   Z(4) = -Z(3)
      K1   =  FIVE/SIX
      K2   =  FIVE/SIX

      END SUBROUTINE SECTION_PROPS_HEXA

! ##################################################################################################################################

      SUBROUTINE SECTION_PROPS_I ( JERR )

      IMPLICIT NONE

      INTEGER(LONG), INTENT(OUT)      :: JERR              ! Error indicator

      REAL(DOUBLE)                    :: ALPHA = 1.0D0     ! Constant in calc of JTOR for open cross-sections
!                                                            Pilkey has 1.31 in #10 and has 1.0 in #12 on Table 2.5. MSC uses 1.0 

      REAL(DOUBLE)                    :: ATOP              ! Area of top flange
      REAL(DOUBLE)                    :: ABOT              ! Area of bottom flange
      REAL(DOUBLE)                    :: AWEB              ! Area of web
      REAL(DOUBLE)                    :: H0                ! Height of I beam
      REAL(DOUBLE)                    :: HW                ! Height of web
      REAL(DOUBLE)                    :: HM                ! Height between C/L of top and bottom
      REAL(DOUBLE)                    :: TB                ! Thickness of top flange
      REAL(DOUBLE)                    :: TT                ! Thickness of bottom flange
      REAL(DOUBLE)                    :: TW                ! Thickness of web
      REAL(DOUBLE)                    :: WB                ! Width of top flange
      REAL(DOUBLE)                    :: WT                ! Width of bottom flange
      REAL(DOUBLE)                    :: YCG               ! Dist from bottom of I section to section C.G.

! **********************************************************************************************************************************
      JERR = 0

      H0 = D(1)
      HW = H0 - D(5) - D(6)
      TB = D(5)
      TT = D(6)
      TW = D(4)
      WB = D(2)
      WT = D(3)
      HM = H0 - HALF*(TT + TB)

      IF (HW < ZERO) JERR = 1
      IF ((WB - TW  < ZERO) .OR. (WT - TW < ZERO)) JERR = 1
      IF (JERR > 0) RETURN

      ATOP = WT*TT
      ABOT = WB*TB
      AWEB = HW*TW
      AREA =  ATOP + ABOT + AWEB

      YCG  =  (WB*TB*(HALF*TB) + HW*TW*(TB + HALF*HW) + WT*TT*(H0 - HALF*TT))/AREA
                                                           ! Calc I1 as I for each of parts about own C.G + part area times D^2
      I1   =  WT*TT*TT*TT/TWELVE + WT*TT*(H0 - HALF*TT - YCG)*(H0 - HALF*TT - YCG)                                                 &
            + WB*TB*TB*TB/TWELVE + WB*TB*(YCG  - HALF*TB)*(YCG  - HALF*TB)                                                         &
            + TW*HW*HW*HW/TWELVE + TW*HW*(TB + HALF*HW - YCG)*(TB + HALF*HW - YCG)
                                                           ! Due to sym about Y only need B*H^3/12 for 2 flanges + web
      I2   =  (TT*WT*WT*WT + HW*TW*TW*TW + TB*WB*WB*WB)/TWELVE

      I12  =  ZERO
      JTOR =  ALPHA*THIRD*(WT*TT*TT*TT + WB*TB*TB*TB + HM*TW*TW*TW)
      Y(1) =  H0 - YCG     ;   Z(1) =  HALF*WT
      Y(2) = -YCG          ;   Z(2) =  HALF*WB
      Y(3) =  Y(2)         ;   Z(3) = -Z(2)
      Y(4) =  Y(1)         ;   Z(4) = -Z(1)

      K1   =  AWEB/AREA                                    ! Assume web carries shear uniformly in plane 1 shear
      K2   =  (FIVE/SIX)*(ATOP + ABOT)/AREA                ! Assume top & bot flanges carry all shear parabolically in plane 2

      END SUBROUTINE SECTION_PROPS_I

! ##################################################################################################################################

      SUBROUTINE SECTION_PROPS_I1 ( JERR )

      IMPLICIT NONE

      INTEGER(LONG), INTENT(OUT)      :: JERR              ! Error indicator

      REAL(DOUBLE)                    :: ALPHA = 1.0D0     ! Constant in calc of JTOR for open cross-sections
!                                                            Pilkey has 1.30 in #10 and 1.0 in #12 in table 2.5

      REAL(DOUBLE)                    :: ATOP              ! Area of top flange
      REAL(DOUBLE)                    :: ABOT              ! Area of bottom flange
      REAL(DOUBLE)                    :: AWEB              ! Area of web
      REAL(DOUBLE)                    :: H0                ! Height of I beam
      REAL(DOUBLE)                    :: HW                ! Height of web
      REAL(DOUBLE)                    :: TB                ! Thickness of top flange
      REAL(DOUBLE)                    :: TT                ! Thickness of bottom flange
      REAL(DOUBLE)                    :: TW                ! Thickness of web
      REAL(DOUBLE)                    :: WB                ! Width of top flange
      REAL(DOUBLE)                    :: WT                ! Width of bottom flange
      REAL(DOUBLE)                    :: YCG               ! Dist from bottom of I section to section C.G.

! **********************************************************************************************************************************
      JERR = 0

      H0 = D(4)
      HW = D(3)
      TB = HALF*(D(4) - D(3))
      TT = TB
      TW = D(2)
      WB = TWO*D(1) + D(2)
      WT = WB

      IF (HW < ZERO) JERR = 1
      IF ((WB - TW  < ZERO) .OR. (WT - TW < ZERO)) JERR = 1
      IF (JERR > 0) RETURN

      ATOP = WT*TT
      ABOT = WB*TB
      AWEB = HW*TW
      AREA =  ATOP + ABOT + AWEB

      YCG  =  ZERO
                                                           ! Calc I1 as I for each of parts about own C.G + part area times D^2
      I1   =  WT*TT*TT*TT/TWELVE + WT*TT*(H0 - HALF*TT - YCG)*(H0 - HALF*TT - YCG)                                                 &
            + WB*TB*TB*TB/TWELVE + WB*TB*(YCG  - HALF*TB)*(YCG  - HALF*TB)                                                         &
            + HW*TW*TW*TW/TWELVE + HW*TW*(TB + HALF*HW - YCG)*(TB + HALF*HW - YCG)
                                                           ! Due to sym about Y only need B*H^3/12 for 2 flanges + web
      I2   =  (TT*WT*WT*WT + HW*TW*TW*TW + TB*WB*WB*WB)/TWELVE

      I12  =  ZERO
      JTOR =  ALPHA*THIRD*(WB*TB*TB*TB + HW*TW*TW*TW + WT*TT*TT*TT)
      Y(1) =  H0 - YCG     ;   Z(1) =  HALF*WT
      Y(2) = -YCG          ;   Z(2) =  HALF*WB
      Y(3) =  Y(2)         ;   Z(3) = -Z(2)
      Y(4) =  Y(1)         ;   Z(4) = -Z(1)

      K1   =  AWEB/AREA                                    ! Assume web carries shear uniformly in plane 1 shear
      K2   =  (FIVE/SIX)*(ATOP + ABOT)/AREA                ! Assume top & bot flanges carry all shear parabolically in plane 2

      END SUBROUTINE SECTION_PROPS_I1

! ##################################################################################################################################

      SUBROUTINE SECTION_PROPS_ROD ( JERR )

      IMPLICIT NONE

      INTEGER(LONG), INTENT(OUT)      :: JERR              ! Error indicator

      REAL(DOUBLE)                    :: RAD

! **********************************************************************************************************************************
      JERR = 0

      RAD = D(1)

      AREA =  PI*RAD*RAD
      I1   =  QUARTER*PI*RAD*RAD*RAD*RAD
      I2   =  I1
      I12  =  ZERO
      JTOR =  I1 + I2
      Y(1) =  RAD    ;   Z(1) =  ZERO 
      Y(2) =  ZERO   ;   Z(2) =  RAD 
      Y(3) = -RAD    ;   Z(3) =  ZERO
      Y(4) =  ZERO   ;   Z(4) = -RAD 

      K1   =  SIX/SEVEN
      K2   =  SIX/SEVEN

      END SUBROUTINE SECTION_PROPS_ROD

! ##################################################################################################################################

      SUBROUTINE SECTION_PROPS_T ( JERR )

      IMPLICIT NONE

      INTEGER(LONG), INTENT(OUT)      :: JERR              ! Error indicator

      REAL(DOUBLE)                    :: ALPHA = 1.0D0     ! Constant in calc of JTOR for open cross-sections
!                                                            Pilkey has 1.12 in #10 on table 2.5. MSC uses 1.0

      REAL(DOUBLE)                    :: AVERT             ! Area of vertical member
      REAL(DOUBLE)                    :: AHORZ             ! Area of horizontal member
      REAL(DOUBLE)                    :: JCONS             ! Intermediate variable
      REAL(DOUBLE)                    :: KCONS             ! Intermediate variable
      REAL(DOUBLE)                    :: DEN               ! Intermediate variable
      REAL(DOUBLE)                    :: NUM               ! Intermediate variable
      REAL(DOUBLE)                    :: WT                ! Width of top flange
      REAL(DOUBLE)                    :: TT                ! Thickness of top flange
      REAL(DOUBLE)                    :: HW                ! Height of web
      REAL(DOUBLE)                    :: TW                ! Thickness of web
      REAL(DOUBLE)                    :: YCG               ! Dist from bottom of web to C.G.
      

! **********************************************************************************************************************************
      JERR = 0

      WT = D(1)
      TT = D(3)
      HW = D(2) - D(3)
      TW = D(4)

      IF (HW < ZERO) JERR = 1
      IF (ABS(WT - TW) < ZERO) JERR = 1
      IF (JERR > 0) RETURN

      AVERT = HW*TW
      AHORZ = WT*TT
      AREA = AVERT + AHORZ
      IF (AREA <= ZERO) THEN
         JERR = 1
         RETURN
      ENDIF

      YCG  =  (WT*TT*(HW + HALF*TT) + HW*TW*(HALF*HW))/AREA
                                                           ! Calc I1 as I for each of parts about own C.G + part area times D^2
      I1   =  WT*TT*TT*TT/TWELVE + WT*TT*(HW + HALF*TT - YCG)*(HW + HALF*TT - YCG)                                                &
            + TW*HW*HW*HW/TWELVE + HW*TW*(YCG - HALF*HW)*(YCG - HALF*HW)

      I2   =  (TT*WT*WT*WT + HW*TW*TW*TW)/TWELVE
      I12  =  ZERO
      JTOR =  ALPHA*THIRD*(WT*TT*TT*TT + HW*TW*TW*TW)
      Y(1) =  HW + TT - YCG   ;   Z(1) =  ZERO
      Y(2) =  Y(1)            ;   Z(2) =  HALF*WT
      Y(3) = -YCG             ;   Z(3) =  ZERO
      Y(4) =  Y(1)            ;   Z(4) = -Z(2)

      JCONS = WT*TT/((HW + HALF*TT)*TW)
      KCONS = WT/(HW + HALF*TT)
      NUM   = TEN*(ONE + FOUR*JCONS)*(ONE + FOUR*JCONS)
      DEN   = (12.D0 + 96.D0*JCONS + 276.D0*JCONS*JCONS + 192.D0*JCONS*JCONS*JCONS) + 30.D0*KCONS*KCONS*JCONS*(ONE + JCONS)
      IF (DABS(DEN) > ZERO) THEN
         K1 = NUM/DEN
      ELSE
         K1 = ZERO
      ENDIF
      K2    = (FIVE/SIX)*AHORZ/AREA

      END SUBROUTINE SECTION_PROPS_T

! ##################################################################################################################################

      SUBROUTINE SECTION_PROPS_T1 ( JERR )

      IMPLICIT NONE

      INTEGER(LONG), INTENT(OUT)      :: JERR              ! Error indicator

      REAL(DOUBLE)                    :: ALPHA = 1.0D0     ! Constant in calc of JTOR for open cross-sections
!                                                            Pilkey has 1.12 in #10 on table 2.5. MSC uses 1.0

      REAL(DOUBLE)                    :: AVERT             ! Area of vertical member
      REAL(DOUBLE)                    :: AHORZ             ! Area of horizontal member
      REAL(DOUBLE)                    :: JCONS             ! Intermediate variable
      REAL(DOUBLE)                    :: KCONS             ! Intermediate variable
      REAL(DOUBLE)                    :: DEN               ! Intermediate variable
      REAL(DOUBLE)                    :: NUM               ! Intermediate variable
      REAL(DOUBLE)                    :: HV                ! Height of vert flange
      REAL(DOUBLE)                    :: TV                ! Thickness of vert flange
      REAL(DOUBLE)                    :: WH                ! Width of horiz flange
      REAL(DOUBLE)                    :: TH                ! Thickness of horiz flange
      REAL(DOUBLE)                    :: ZCG               ! Dist from bottom of web to C.G.

! **********************************************************************************************************************************
      JERR = 0

      HV = D(1)
      TV = D(3)
      WH = D(2)
      TH = D(4)

      AVERT = HV*TV
      AHORZ = WH*TH
      AREA = AHORZ + AVERT
      IF (AREA <= ZERO) THEN
         JERR = 1
         RETURN
      ENDIF

      ZCG =  (HV*TV*(WH + HALF*TV) + WH*TH*HALF*WH)/AREA

      I1   =  WH*TH*TH*TH/TWELVE + TV*HV*HV*HV/TWELVE
                                                           ! Calc I2 as I for each of parts about own C.G + part area times D^2
      I2   =  TH*WH*WH*WH/TWELVE + WH*TH*(ZCG - HALF*WH)*(ZCG - HALF*WH)                                                           &
            + HV*TV*TV*TV/TWELVE + HV*TV*(ZCG - WH - HALF*TV)*(ZCG - WH - HALF*TV)
      I12  =  ZERO
      JTOR =  ALPHA*THIRD*(WH*TH*TH*TH + HV*TV*TV*TV)
      Y(1) =  ZERO      ;   Z(1) =  (WH + TV - ZCG)
      Y(2) = -HALF*HV   ;   Z(2) =  Z(1)
      Y(3) =  ZERO      ;   Z(3) = -ZCG
      Y(4) = -Y(2)      ;   Z(4) =  Z(1)

      JCONS = HV*TV/((WH + HALF*TV)*TH)
      KCONS = HV/(WH + HALF*TV)
      NUM   = TEN*(ONE + FOUR*JCONS)*(ONE + FOUR*JCONS)
      DEN   = (12.D0 + 96.D0*JCONS + 276.D0*JCONS*JCONS + 192.D0*JCONS*JCONS*JCONS) + 30.D0*KCONS*KCONS*JCONS*(ONE + JCONS)
      K1    = (FIVE/SIX)*AVERT/AREA
      IF (DABS(DEN) > ZERO) THEN
         K2 = NUM/DEN
      ELSE
         K2 = ZERO
      ENDIF

      END SUBROUTINE SECTION_PROPS_T1

! ##################################################################################################################################

      SUBROUTINE SECTION_PROPS_T2 ( JERR )

      IMPLICIT NONE

      INTEGER(LONG), INTENT(OUT)      :: JERR              ! Error indicator

      REAL(DOUBLE)                    :: ALPHA = 1.0D0     ! Constant in calc of JTOR for open cross-sections
!                                                            Pilkey has 1.12 in #10 on table 2.5. MSC uses 1.0

      REAL(DOUBLE)                    :: AVERT             ! Area of vertical member
      REAL(DOUBLE)                    :: AHORZ             ! Area of horizontal member
      REAL(DOUBLE)                    :: JCONS             ! Intermediate variable
      REAL(DOUBLE)                    :: KCONS             ! Intermediate variable
      REAL(DOUBLE)                    :: DEN               ! Intermediate variable
      REAL(DOUBLE)                    :: NUM               ! Intermediate variable
      REAL(DOUBLE)                    :: WB                ! Width of bottom flange
      REAL(DOUBLE)                    :: TB                ! Thickness of bottom flange
      REAL(DOUBLE)                    :: HW                ! Height of web
      REAL(DOUBLE)                    :: TW                ! Thickness of web
      REAL(DOUBLE)                    :: YCG               ! Dist from bottom of I section to section C.G.

! **********************************************************************************************************************************
      JERR = 0

      WB = D(1)
      TB = D(3)
      HW = D(2) - D(3)
      TW = D(4)

      IF (HW <= ZERO) JERR = 1
      IF (ABS(WB - TW) < ZERO) JERR = 1
      IF (JERR > 0) RETURN

      AVERT = HW*TW
      AHORZ = WB*TB
      AREA = AVERT + AHORZ
      IF (AREA <= ZERO) THEN
         JERR = 1
         RETURN
      ENDIF

      YCG  =  (TB*WB*HALF*TB + HW*TW*(TB + HALF*HW))/AREA
                                                           ! Calc I1 as I for each of parts about own C.G + part area times D^2
      I1   =  WB*TB*TB*TB/TWELVE + WB*TB*(YCG - HALF*TB)*(YCG - HALF*TB)                                                           &
            + TW*HW*HW*HW/TWELVE + TW*HW*(TB + HALF*HW - YCG)*(TB + HALF*HW - YCG)
      I2   =  (TB*WB*WB*WB + HW*TW*TW*TW)/TWELVE
      I12  =  ZERO
      JTOR =  ALPHA*THIRD*(WB*TB*TB*TB + HW*TW*TW*TW)
      JTOR =  ALPHA*THIRD*((D(1)-D(4))*D(3)*D(3)*D(3) + D(2)*D(4)*D(4)*D(4))
      Y(1) =  TB + HW - YCG   ;   Z(1) =  HALF*TW
      Y(2) =  -YCG            ;   Z(2) =  HALF*WB
      Y(3) =  Y(2)            ;   Z(3) = -Z(2)
      Y(4) =  Y(1)            ;   Z(4) = -Z(1)

      JCONS = WB*TB/((HW + HALF*TB)*TW)
      KCONS = WB/(HW + HALF*TB)
      NUM   = TEN*(ONE + FOUR*JCONS)*(ONE + FOUR*JCONS)
      DEN   = (12.D0 + 96.D0*JCONS + 276.D0*JCONS*JCONS + 192.D0*JCONS*JCONS*JCONS) + 30.D0*KCONS*KCONS*JCONS*(ONE + JCONS)
      IF (DABS(DEN) > ZERO) THEN
         K1 = NUM/DEN
      ELSE
         K1 = ZERO
      ENDIF
      K2    = (FIVE/SIX)*AHORZ/AREA

      END SUBROUTINE SECTION_PROPS_T2

! ##################################################################################################################################

      SUBROUTINE SECTION_PROPS_TUBE ( JERR )

      IMPLICIT NONE

      INTEGER(LONG), INTENT(OUT)      :: JERR              ! Error indicator

      REAL(DOUBLE)                    :: JCONS             ! Intermediate variable
      REAL(DOUBLE)                    :: NU                ! Poisson ratio
      REAL(DOUBLE)                    :: RAD1,RAD2         ! Radii
      REAL(DOUBLE)                    :: RAD21             ! RAD2/RAD1

! **********************************************************************************************************************************
      JERR = 0

      RAD1 = D(1)
      RAD2 = D(2)

      IF (RAD1 - RAD2 < ZERO) JERR = 1
      IF (JERR > 0) RETURN

      AREA =  PI*(RAD1*RAD1 - RAD2*RAD2)
      IF (AREA <= ZERO) THEN
         JERR = 1
         RETURN
      ENDIF

      I1   =  QUARTER*PI*(RAD1*RAD1*RAD1*RAD1 - RAD2*RAD2*RAD2*RAD2)
      I2   =  I1
      I12  =  ZERO
      JTOR =  TWO*I1
      Y(1) =  RAD1   ;   Z(1) =  ZERO 
      Y(2) =  ZERO   ;   Z(2) =  RAD1
      Y(3) = -Y(1)   ;   Z(3) =  ZERO
      Y(4) =  ZERO   ;   Z(4) = -Z(2)

      NU   =  ZERO
      RAD21=  RAD2/RAD1
      JCONS=  (1 + RAD21*RAD21)
      K1   =  SIX*(1 + NU)*JCONS*JCONS/((SEVEN + SIX*NU)*JCONS*JCONS + TWO*(TEN + SIX*NU)*RAD21*RAD21)
      K2   =  K1

      END SUBROUTINE SECTION_PROPS_TUBE

! ##################################################################################################################################

      SUBROUTINE SECTION_PROPS_Z ( JERR )

      IMPLICIT NONE

      INTEGER(LONG), INTENT(OUT)      :: JERR              ! Error indicator

      REAL(DOUBLE)                    :: ALPHA = 1.0D0     ! Constant in calc of JTOR for open cross-sections
!                                                            Pilkey has 1.12 in #10 on table 2.5. MSC uses 1.0

      REAL(DOUBLE)                    :: ATOP              ! Area of top flange
      REAL(DOUBLE)                    :: ABOT              ! Area of bottom flange
      REAL(DOUBLE)                    :: AWEB              ! Area of web
      REAL(DOUBLE)                    :: WF                ! Width of bottom flange
      REAL(DOUBLE)                    :: TF                ! Thickness of bottom flange
      REAL(DOUBLE)                    :: HW                ! Height of web
      REAL(DOUBLE)                    :: TW                ! Thickness of web

! **********************************************************************************************************************************
      JERR = 0

      WF = D(1) + D(2)
      TF = HALF*(D(4) - D(3))
      HW = D(3)
      TW = D(2)

      IF (HW <= 0) JERR = 1
      IF (JERR > 0) RETURN

      ATOP = WF*TF
      ABOT = ATOP
      AWEB = HW*TW

      AREA =  ATOP + ABOT + AWEB
      IF (AREA <= ZERO) THEN
         JERR = 1
         RETURN
      ENDIF

      I1   =  TW*HW*HW*HW/TWELVE + TWO*( WF*TF*TF*TF/TWELVE + WF*TF*QUARTER*(HW + TF)*(HW + TF) )
      I2   =  HW*TW*TW*TW/TWELVE + TWO*( TF*WF*WF*WF/TWELVE + TF*WF*QUARTER*(WF - TW)*(WF - TW) )
      I12  =  -TF*D(1)*(D(1) + D(2))*(D(3) + TF)/TWO
      JTOR =  ALPHA*THIRD*(TWO*WF*TF*TF*TF + HW*TW*TW*TW)
      Y(1) =  HALF*HW + TF     ;   Z(1) =  HALF*TW
      Y(2) = -Y(1)             ;   Z(2) =  (WF - HALF*TW)
      Y(3) = -Y(1)             ;   Z(3) = -Z(1)
      Y(4) =  Y(1)             ;   Z(4) = -Z(2)
      K1   =  AWEB/AREA
      K2   =  (FIVE/SIX)*(ATOP + ABOT)/AREA
      

      END SUBROUTINE SECTION_PROPS_Z

      END SUBROUTINE BD_PBARL
