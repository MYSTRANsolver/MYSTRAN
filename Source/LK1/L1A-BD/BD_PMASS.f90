! ##################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
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
  
      SUBROUTINE BD_PMASS ( CARD )
  
! Processes PMASS Bulk Data Cards. Read and check

!  1) Property ID and enter into array PMASS
!  2) Mass value and enter into array RPMASS
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, NPMASS
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_PMASS_BEGEND
      USE MODEL_STUF, ONLY            :  PMASS, RPMASS
 
      USE BD_PMASS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_PMASS'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: PMASS_PID         ! Prop number from field 2,4,6,8
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_PMASS_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! PMASS Bulk Data Card routine
 
!   FIELD   ITEM           ARRAY ELEMENT
!   -----   ------------   -------------
!    2      Prop ID         PMASS(npelas,1)
!    3      Mass, MI       RPMASS(npelas,1)
!    4      Prop ID         PMASS(npelas,2)
!    5      Mass, MI       RPMASS(npelas,2)
!    6      Prop ID         PMASS(npelas,3)
!    7      Mass, MI       RPMASS(npelas,3)
!    8      Prop ID         PMASS(npelas,4)
!    9      Mass, MI       RPMASS(npelas,4)
 
 
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Read and check data

      DO I=1,4

         IF (JCARD(2*I)(1:) /= ' ') THEN
            NPMASS = NPMASS + 1

            CALL I4FLD ( JCARD(2*I), JF(2*I), PMASS_PID )
            IF (IERRFL(2*I) == 'N') THEN
               DO J=1,NPMASS-1
                  IF (PMASS_PID == PMASS(J,1)) THEN
                     FATAL_ERR = FATAL_ERR + 1
                     WRITE(ERR,1145) JCARD(1),PMASS_PID
                     WRITE(F06,1145) JCARD(1),PMASS_PID
                     EXIT
                  ENDIF
               ENDDO 
               PMASS(NPMASS,1) = PMASS_PID
            ENDIF
            CALL R8FLD ( JCARD(2*I+1), JF(2*I+1), RPMASS(NPMASS,1) )
         ENDIF

      ENDDO

      IF (JCARD(2)(1:) /= ' ') THEN
         CALL BD_IMBEDDED_BLANK ( JCARD,2,3,0,0,0,0,0,0 )
      ENDIF

      IF (JCARD(4)(1:) /= ' ') THEN
         CALL BD_IMBEDDED_BLANK ( JCARD,0,0,4,5,0,0,0,0 )
      ENDIF

      IF (JCARD(6)(1:) /= ' ') THEN
         CALL BD_IMBEDDED_BLANK ( JCARD,0,0,0,0,6,7,0,0 )
      ENDIF

      IF (JCARD(8)(1:) /= ' ') THEN
         CALL BD_IMBEDDED_BLANK ( JCARD,0,0,0,0,0,0,8,9 )
      ENDIF

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
 
! **********************************************************************************************************************************

      END SUBROUTINE BD_PMASS
