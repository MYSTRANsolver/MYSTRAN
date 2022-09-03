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
  
      SUBROUTINE BD_PSHEL ( CARD, LARGE_FLD_INP )
  
!  Processes PSHELL Bulk Data Cards. Reads and checks data:

!  1) Property ID and 3 material ID's (membrane, bending, transverse shear) and enter into array PSHEL
!  2) Membrane thick., bending moment of inertia ratio, shear thick. ratio, nonstr mass and enter into array RPSHEL
!  3) From 1st cont card (if present): material ID for bending/membrane coupling
!  4) From 1st cont card (if present): locations for stress recovery and offset and enter into array RPSHEL
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  IERRFL, FATAL_ERR, JCARD_LEN, JF, LPSHEL, NPSHEL, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PSHEL_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO
      USE PARAMS, ONLY                :  EPSIL
      USE MODEL_STUF, ONLY            :  PSHEL, RPSHEL
 
      USE BD_PSHEL_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME     = 'BD_PSHEL'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
      CHARACTER(LEN(CARD))            :: CHILD             ! "Child" card read in subr NEXTC, called herein
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG)                   :: ICONT         = 0 ! Indicator of whether a cont card exists. Output from subr NEXTC
      INTEGER(LONG)                   :: IERR          = 0 ! Error indicator returned from subr NEXTC called herein
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: MATERIAL_ID   = 0 ! Material ID
      INTEGER(LONG)                   :: N             = 1 ! Counter
      INTEGER(LONG)                   :: PROPERTY_ID   = 0 ! Property ID (field 2 of this parent property card)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PSHEL_BEGEND
 
      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!  PSHELL Bulk Data Card routine
 
!    FIELD   ITEM           ARRAY ELEMENT
!    -----   ------------   -------------
!  on first card:
!     2      Prop ID         PSHEL(npshel,1)
!     3      MID1            PSHEL(npshel,2)
!     4      Thickness - TM RPSHEL(npshel,1)
!     5      MID2            PSHEL(npshel,3)
!     6      Bend. Inertia  RPSHEL(npshel,2)
!     7      MID3            PSHEL(npshel,4)
!     8      Shear - TS/TM  RPSHEL(npshel,3)
!     9      NSM            RPSHEL(npshel,4)
!  on optional second card:
!     2      Stress - Z1    RPSHEL(npshel,5)
!     3      Stress - Z2    RPSHEL(npshel,6)
!     4      MID4            PSHEL(npshel,5)
!            TS/TM indicator PSHEL(npshel,6) (set to 1 to use default value if field 8 of parent card is blank, otherwise 0 to
!                                             indicate that there is a TS/TM value in field 8)

! Blank material ID fields will be interpreted as:
!     MID1 : no membrane stiffness, no membrane/coupling stiffness
!     MID2 : no bending stiffness, no transverse shear stiffness, no membrane/bending coupling stiffness
!     MID3 : no transverse shear flexibility
!     MID4 : no membrane/coupling  
 
!  Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Check for overflow

      NPSHEL = NPSHEL+1
 
! Read and check data on parent card

      CALL I4FLD ( JCARD(2), JF(2), PROPERTY_ID )          ! Read PSHEL ID
      IF (IERRFL(2) == 'N') THEN
         DO J=1,NPSHEL-1
            IF (PROPERTY_ID == PSHEL(J,1)) THEN
               FATAL_ERR = FATAL_ERR + 1 
               WRITE(ERR,1145) JCARD(1),PROPERTY_ID
               WRITE(F06,1145) JCARD(1),PROPERTY_ID
               EXIT
            ENDIF
         ENDDO   
         PSHEL(NPSHEL,1) = PROPERTY_ID
      ENDIF
 
! Read data from fields 3,5,7 (material ID's) and 2,4,6 (TM, 12I/(TM^3), TS/TM)

      N = 1                                                ! Read 3 material ID's into array PSHEL
      DO J = 3,7,2
         N = N + 1
         CALL I4FLD ( JCARD(J), JF(J), MATERIAL_ID )
         IF (IERRFL(J) == 'N') THEN
            IF (JCARD(J)(1:) == ' ') THEN                  ! Set MID1-3 to zero if field is blank (see below for reset of MID3),
               PSHEL(NPSHEL,N) = 0
            ELSE                                           ! otherwise, read numerical value of MID1-3 and make sure it is > 0
               IF (MATERIAL_ID <= 0) THEN
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1193) JF(J),JCARD(1),MATERIAL_ID
                  WRITE(F06,1193) JF(J),JCARD(1),MATERIAL_ID
               ELSE
                  PSHEL(NPSHEL,N) = MATERIAL_ID
               ENDIF
            ENDIF
         ENDIF
         CALL R8FLD (JCARD(J+1),JF(J+1),RPSHEL(NPSHEL,N-1))! Read TM, 12I/(TM^3), TS/TM
      ENDDO

!xx   IF (JCARD(7)(1:) == ' ') THEN                        ! Reset MID3 to MID2 if MID3 field is blank
!xx      PSHEL(NPSHEL,4) = PSHEL(NPSHEL,3)
!xx   ENDIF
 
      IF ((IERRFL(5)=='N') .AND. (IERRFL(7)=='N')) THEN    ! Check that MID3 is not > 0 when MID2 is blank (error)
         IF ((PSHEL(NPSHEL,4) > 0) .AND. (JCARD(5)(1:) == ' ')) THEN
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1160) PSHEL(NPSHEL,1),PSHEL(NPSHEL,3),PSHEL(NPSHEL,4)
            WRITE(F06,1160) PSHEL(NPSHEL,1),PSHEL(NPSHEL,3),PSHEL(NPSHEL,4)
         ENDIF
      ENDIF  
 
      IF (JCARD(6)(1:) == ' ') THEN                        ! Set default for bending inertia when field 6 is blank
         RPSHEL(NPSHEL,2) = ONE
      ENDIF
 
      IF (JCARD(8)(1:) == ' ') THEN                        ! Set PSHEL(npshel,6) if default value for TS/TM is to be used
         PSHEL(NPSHEL,6) = 1                               ! Use default value
      ELSE
         PSHEL(NPSHEL,6) = 0                               ! Do not use default value, a value was on the PSHEL card for TS/TM
      ENDIF
 
      CALL R8FLD ( JCARD(9), JF(9), RPSHEL(NPSHEL,4) )     ! Read NSM, nonstructural mass

      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,6,7,8,9 )     ! Make sure that there are no imbedded blanks in fields 2-9
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
 
! Read and check data on optional continuation card:
 
      IF (LARGE_FLD_INP == 'N') THEN
         CALL NEXTC  ( CARD, ICONT, IERR )
      ELSE
         CALL NEXTC2 ( CARD, ICONT, IERR, CHILD )
         CARD = CHILD
      ENDIF
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )

      PSHEL (NPSHEL,5) =  0                                ! Default mat'l ID for bending/membrane coupling = 0
      RPSHEL(NPSHEL,5) = -RPSHEL(NPSHEL,1)/TWO             ! Default Z1 = -TM/2
      RPSHEL(NPSHEL,6) =  RPSHEL(NPSHEL,1)/TWO             ! Default Z2 =  TM/2
      RPSHEL(NPSHEL,7) =  ZERO                             ! Default offset = 0

      IF (ICONT == 1) THEN                                 ! Read values from cont card:
         IF (JCARD(2)(1:) /= ' ') THEN                     ! Read Z1
            CALL R8FLD ( JCARD(2), JF(2), RPSHEL(NPSHEL,5) )
         ENDIF
         IF (JCARD(3)(1:) /= ' ') THEN                     ! Read Z2
            CALL R8FLD ( JCARD(3), JF(3), RPSHEL(NPSHEL,6) )
         ENDIF
         IF (JCARD(4)(1:) /= ' ') THEN                     ! Read mat'l ID for bending/membrane coupling
            CALL I4FLD ( JCARD(4), JF(4),  PSHEL(NPSHEL,5) )
         ENDIF
         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,0,0,0,0,0 )  ! Make sure that there are no imbedded blanks in fields 2-4    
         CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,5,6,7,8,9 )! Issue warning if fields 5, 6, 7, 8, 9 not blank
         CALL CRDERR ( CARD )
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

 1160 FORMAT(' *ERROR  1160: PSHEL ID ',I8,' HAS BENDING MATERIAL ID = ',I8,' AND TRANSVERSE SHEAR MATERIAL ID = ',I8              &
                    ,/,14X,' WHEN BENDING MATERIAL ID IS 0, THE TRANSVERSE SHEAR MATERIAL ID CANNOT BE > 0') 
 
 1163 FORMAT(' *ERROR  1163: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ',A,' ENTRIES; LIMIT = ',I12)

 1193 FORMAT(' *ERROR  1193: MATERIAL ID IN FIELD ',I3,' OF ',A,' MUST BE > 0 OR BLANK BUT IS = ',I8)


! **********************************************************************************************************************************
 
      END SUBROUTINE BD_PSHEL
