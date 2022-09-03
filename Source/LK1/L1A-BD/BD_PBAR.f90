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
  
      SUBROUTINE BD_PBAR ( CARD, LARGE_FLD_INP )
  
! Processes PBAR Bulk Data Cards. Reads and checks:

!  1) Prop ID and Material ID and enter into array PBAR
!  2) Area, moments of inertia, torsional constant ans nonstructural mass and enter into array RPBAR
!  3) From 1st continuation card (if present): coords of 4 points for stress recovery and enter into array RPBAR
!  4) From 2nd continuation card (if present): area factors for transverse shear and I12 and enter into array RPBAR
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE PARAMS, ONLY                :  EPSIL, SUPINFO
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, BARTOR, IERRFL, FATAL_ERR, JCARD_LEN, JF, LPBAR, NPBAR
      USE CONSTANTS_1, ONLY           :  ZERO
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PBAR_BEGEND
      USE MODEL_STUF, ONLY            :  PBAR, RPBAR
 
      USE BD_PBAR_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME   =   'BD_PBAR'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
      CHARACTER(LEN(JCARD))           :: ID                ! Character value of element ID (field 2 of parent card)
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
 
      INTEGER(LONG)                   :: ICONT       = 0   ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR        = 0   ! Error indicator
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: MATERIAL_ID = 0   ! Material ID (field 3 of this property card)
      INTEGER(LONG)                   :: PROPERTY_ID = 0   ! Property ID (field 2 of this property card)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PBAR_BEGEND
 
      REAL(DOUBLE)                    :: I1          = ZERO! Moment of inertia
      REAL(DOUBLE)                    :: I2          = ZERO! Moment of inertia
      REAL(DOUBLE)                    :: I12         = ZERO! Product of inertia
      REAL(DOUBLE)                    :: EPS1              ! A small number

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
!    4      Area                                           RPBAR(npbar, 1)
!    5      Inertia 1, I1                                  RPBAR(npbar, 2)
!    6      Inertia 2, I2                                  RPBAR(npbar, 3)
!    7      Torsional constant, J                          RPBAR(npbar, 4)
!    8      Non-structural mass                            RPBAR(npbar, 5)

! on optional second card:
!    2      Y1 y coord of 1st point for stress recovery)   RPBAR(npbar, 6)
!    3      Z1 z coord of 1st point for stress recovery)   RPBAR(npbar, 7)
!    4      Y2 y coord of 2nd point for stress recovery)   RPBAR(npbar, 8)
!    5      Z2 z coord of 2nd point for stress recovery)   RPBAR(npbar, 9)
!    6      Y3 y coord of 3rd point for stress recovery)   RPBAR(npbar,10)
!    7      Z3 z coord of 3rd point for stress recovery)   RPBAR(npbar,11)
!    8      Y4 y coord of 4th point for stress recovery)   RPBAR(npbar,12)
!    9      Z4 z coord of 4th point for stress recovery)   RPBAR(npbar,13)
! on optional third card:
!    2      K1, Plane 1 shear factor                       RPBAR(npbar,14)
!    3      K2, Plane 2 shear factor                       RPBAR(npbar,15)
!    4      I12, Product of inertia                        RPBAR(npbar,16)
!    5      C Torsional stress recovery coefficient        RPBAR(npbar,17)
 
 
      EPS1 = EPSIL(1)

      CHILD(1:) = ' '

! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Check for overflow

      NPBAR = NPBAR+1
 
! Read and check data on parent card

      ID   = JCARD(2)
      CALL I4FLD ( JCARD(2), JF(2), PROPERTY_ID )          ! Read property ID and enter into array PBAR
      IF (IERRFL(2) == 'N') THEN
         DO J=1,NPBAR-1
            IF (PROPERTY_ID == PBAR(J,1)) THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1145) JCARD(1),PROPERTY_ID
               WRITE(F06,1145) JCARD(1),PROPERTY_ID
               EXIT
             ENDIF
         ENDDO   
         PBAR(NPBAR,1) = PROPERTY_ID
      ENDIF
 
      CALL I4FLD ( JCARD(3), JF(3), MATERIAL_ID )          ! Read material ID and enter into array PBAR
      IF (IERRFL(3) == 'N') THEN
         IF (MATERIAL_ID <= 0) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1192) JF(3), JCARD(1), JCARD(2), ' > 0 ', MATERIAL_ID 
            WRITE(F06,1192) JF(3), JCARD(1), JCARD(2), ' > 0 ', MATERIAL_ID 
         ELSE
            PBAR(NPBAR,2) = MATERIAL_ID
         ENDIF
      ENDIF

      DO J = 1,5                                           ! Read real property values in fields 4-8
         CALL R8FLD ( JCARD(J+3), JF(J+3), RPBAR(NPBAR,J) )
      ENDDO
      IF (IERRFL(5)  == 'N') THEN
         I1 = RPBAR(NPBAR,2)   
      ENDIF   
      IF (IERRFL(6)  == 'N') THEN
         I2 = RPBAR(NPBAR,3)
      ENDIF   
 
      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,0 )     ! Make sure that there are no imbedded blanks in fields 2-8
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,0,0,0,9 )   ! Issue warning if field 9 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
 
! Read and check data on optional 2nd, 3rd cards:
 
      I12 = ZERO                                           ! Init I12 since the 2nd cont entry, which would reat it, may not exist
 
      IF (LARGE_FLD_INP == 'N') THEN
         CALL NEXTC  ( CARD, ICONT, IERR )                 ! Read 2nd card
      ELSE
         CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
         CARD = CHILD
      ENDIF
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
      IF (ICONT == 1) THEN
         DO J = 6,13                                       ! Read real property values in fields 2-9 of 2nd card
            CALL R8FLD ( JCARD(J-4), JF(J-4), RPBAR(NPBAR,J) )
         ENDDO   
         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )  ! Make sure that there are no imbedded blanks in fields 2-9
         CALL CRDERR ( CARD )                              ! CRDERR prints errors found when reading fields

         IF (LARGE_FLD_INP == 'N') THEN
            CALL NEXTC  ( CARD, ICONT, IERR )              ! Read 3rd card
         ELSE
            CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
            CARD = CHILD
         ENDIF
         CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
         IF (ICONT == 1) THEN
            DO J = 14,16                                   ! Read real property values in fields 2-4 of 3rd card
               CALL R8FLD ( JCARD(J-12), JF(J-12), RPBAR(NPBAR,J) )
            ENDDO
            IF (IERRFL(4) == 'N') THEN
               I12 = RPBAR(NPBAR,16)
            ENDIF
                                                           ! Read torsional stress coefficient
            CALL R8FLD ( JCARD(5), JF(5), RPBAR(NPBAR,17) )
            IF (DABS(RPBAR(NPBAR,17)) > EPS1) THEN         ! Set BARTOR to 'Y' if any BAR elem has a nonzero torsional stress coeff
               BARTOR = 'Y'
            ENDIF   

            CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,0,0,0,0)! Make sure that there are no imbedded blanks in fields 2-5
            CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,6,7,8,9 )
            CALL CRDERR ( CARD )                           ! CRDERR prints errors found when reading fields

         ENDIF
      ENDIF

! Call subr to check sensibility of I1, I2, I12 combinations

      CALL CHECK_BAR_MOIs ( 'PBAR', ID, I1, I2, I12, IERR )
      RPBAR(NPBAR, 2) = I1
      RPBAR(NPBAR, 3) = I2
      RPBAR(NPBAR,16) = I12
      IF (IERR /= 0) THEN
         FATAL_ERR = FATAL_ERR + 1
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1145 FORMAT(' *ERROR  1145: DUPLICATE ',A,' ENTRY WITH ID = ',I8)
 
 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

 1192 FORMAT(' *ERROR  1192: ID IN FIELD ',I3,' OF ',A,A,' MUST BE ',A,' BUT IS = ',I8)


! **********************************************************************************************************************************
 
      END SUBROUTINE BD_PBAR
