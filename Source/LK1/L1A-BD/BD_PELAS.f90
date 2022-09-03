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
  
      SUBROUTINE BD_PELAS ( CARD )
  
! Processes PELAS Bulk Data Cards. Read and check

!  1) Property ID and enter into array PELAS
!  2) Stiffness, damping, stress recovery coeff. and enter into array RPELAS
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LPELAS, NPELAS
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PELAS_BEGEND
      USE MODEL_STUF, ONLY            :  PELAS, RPELAS
 
      USE BD_PELAS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_PELAS'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: PROP_ID   = 0     ! Property ID (field 2 of this property card)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PELAS_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! PELAS Bulk Data Card routine
 
!   FIELD   ITEM           ARRAY ELEMENT
!   -----   ------------   -------------
!    2      Prop ID         PELAS(npelas,1)
!    3      Sp. Rate - K   RPELAS(npelas,1)
!    4      Damping - GE   RPELAS(npelas,2)
!    5      Stress recov.  RPELAS(npelas,3)
 
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Check for overflow

      NPELAS = NPELAS+1
 
! Read and check data

      CALL I4FLD ( JCARD(2), JF(2), PROP_ID )              ! Read property ID and enter into array PELAS
      IF (IERRFL(2) == 'N') THEN
         DO J=1,NPELAS-1
            IF (PROP_ID == PELAS(J,1)) THEN
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1145) JCARD(1), PROP_ID
               WRITE(F06,1145) JCARD(1), PROP_ID
               EXIT
            ENDIF
         ENDDO   
         PELAS(NPELAS,1) = PROP_ID
      ENDIF
 
      DO J = 1,3                                           ! Read real property values in fields 3-5
         CALL R8FLD ( JCARD(J+2), JF(J+2), RPELAS(NPELAS,J) )
      ENDDO   
 
      CALL BD_IMBEDDED_BLANK ( JCARD,2,3,4,5,0,0,0,0 )     ! Make sure that there are no imbedded blanks in fields 2-9
      CALL CARD_FLDS_NOT_BLANK ( JCARD,0,0,0,0,6,7,8,9 )   ! Issue warning if fields 6, 7, 8, 9 not blank
      CALL CRDERR ( CARD )                                 ! CRDERR prints errors found when reading fields
  
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

! **********************************************************************************************************************************
 
      END SUBROUTINE BD_PELAS
