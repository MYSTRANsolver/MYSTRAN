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
 
      SUBROUTINE BD_PSHEAR ( CARD )
 
! Processes PSHEAR Bulk Data Cards

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, MPSHEAR, MRPSHEAR, NPSHEAR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PSHEAR_BEGEND
      USE MODEL_STUF, ONLY            :  PSHEAR, RPSHEAR
 
      USE BD_PSHEAR_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_PSHEAR'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD

      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: MATERIAL_ID   = 0 ! Material ID
      INTEGER(LONG)                   :: PROPERTY_ID   = 0 ! Property ID (field 2 of this parent property card)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PSHEAR_BEGEND

      REAL(DOUBLE)                    :: R8INP             ! Real value read from a field on the PSHEAR entry

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! PSHEAR element Bulk Data Card routine

!    FIELD   ITEM           ARRAY ELEMENT
!    -----   ------------   -------------
!     2      Prop ID         PSHEAR(npshear,1)
!     3      MID1            PSHEAR(npshear,2)
!     4      Thickness      RPSHEAR(npshear,1)
!     5      NSM            RPSHEAR(npshear,2)
!     6      F1             RPSHEAR(npshear,3)
!     7      F2             RPSHEAR(npshear,4)
 
!  Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Check for overflow

      NPSHEAR = NPSHEAR+1
 
! Read and check data on parent card

      CALL I4FLD ( JCARD(2), JF(2), PROPERTY_ID )          ! Read PSHEAR ID
      IF (IERRFL(2) == 'N') THEN
         DO J=1,NPSHEAR-1
            IF (PROPERTY_ID == PSHEAR(J,1)) THEN
               FATAL_ERR = FATAL_ERR + 1 
               WRITE(ERR,1145) JCARD(1),PROPERTY_ID
               WRITE(F06,1145) JCARD(1),PROPERTY_ID
               EXIT
            ENDIF
         ENDDO   
         PSHEAR(NPSHEAR,1) = PROPERTY_ID
      ENDIF
 
! Read material ID from field 3

      CALL I4FLD ( JCARD(3), JF(3), MATERIAL_ID )
      IF (IERRFL(3) == 'N') THEN
         IF (MATERIAL_ID > 0) THEN
            PSHEAR(NPSHEAR,2) = MATERIAL_ID
         ELSE
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1193) JF(J),JCARD(1),MATERIAL_ID
            WRITE(F06,1193) JF(J),JCARD(1),MATERIAL_ID
         ENDIF
      ENDIF

! Read real values in fields 4-7

      DO J=4,7
         CALL R8FLD ( JCARD(J), JF(J), R8INP )
         IF (IERRFL(J) == 'N') THEN
            RPSHEAR(NPSHEAR,J-3) = R8INP
         ENDIF
      ENDDO

!cc NOTE: CHANGE THIS SO THAT FIELDS 6,7 CAN BE READ ONCE I ADD F1, F2 CAPABILITY IN SUBR QSHEAR
      CALL BD_IMBEDDED_BLANK   ( JCARD,2,3,4,5,0,0,0,0 )
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,6,7,8,9 )
      CALL CRDERR ( CARD )

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1145 FORMAT(' *ERROR  1145: DUPLICATE ',A,' ENTRY WITH ID = ',I8)

 1193 FORMAT(' *ERROR  1193: MATERIAL ID IN FIELD ',I3,' OF ',A,' MUST BE > 0 OR BLANK BUT IS = ',I8)

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_PSHEAR
