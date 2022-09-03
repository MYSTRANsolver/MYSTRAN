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
 
      SUBROUTINE BD_BAROR0 ( CARD )
 
! Processes BAROR Bulk Data Card to increment LVVEC if the BAROR card
! has a V vector. Also, determine type of V vector is on this BAROR card.
! When this subr finishes, BAROR_VVEC_TYPE will be either:
!         a) 'VECTOR   ' means this BAROR card had V vector in fields 6-8
!         b) 'GRID     ' means this BAROR card had an integer in field 6
!                        and blank fields 7 and 8 (indicating a grid for V vector).           
!         c) 'UNDEFINED' means this BAROR card had blank fields 6, 7 and 8
!         d) 'ERROR    ' means anything but (a), (b), or (c). Subr BD_BAROR will print error
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, IERRFL, JCARD_LEN, JF, LVVEC, NBAROR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_BAROR0_BEGEND
      USE MODEL_STUF, ONLY            :  BAROR_PID, BAROR_G0, BAROR_VV, BAROR_VVEC_TYPE, JBAROR
 
      USE BD_BAROR0_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_BAROR0'
      CHARACTER(LEN=*), INTENT(IN)    :: CARD              ! A Bulk Data card
      CHARACTER(LEN=JCARD_LEN)        :: JCARD(10)         ! The 10 fields of characters making up CARD
 
      INTEGER(LONG)                   :: I4INP     = 0     ! A value read from input file that should be an integer value
      INTEGER(LONG)                   :: J                 ! DO loop index
      INTEGER(LONG)                   :: JERR      = 0     ! A local error count
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_BAROR0_BEGEND
 
      REAL(DOUBLE)                    :: R8INP             ! A value read from input file that should be a real value

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Make JCARD from CARD
 
      CALL MKJCARD ( SUBR_NAME, CARD, JCARD )
 
! Count the number of BAROR cards. The count will be checked to make sure that there is no more than 1 in subr LOADB

      NBAROR = NBAROR + 1

! Only set BAROR values if this is the 1st BAROR card. There should be only 1 BAROR card.
! Reset IERRFL's to 'N', after testing, since CDRERR is not being called until BD_BAROR is called from LOADB

      IF (NBAROR == 1) THEN

         DO J=1,10                                         ! Set JBAROR to JCARD. We need JBAROR(3) (the prop ID) in subr ELEPRO
            JBAROR(J) = JCARD(J)
         ENDDO 

         IF (JCARD(3)(1:) /= ' ') THEN
            CALL I4FLD ( JCARD(3), JF(3), I4INP )
            IF (IERRFL(3) == 'N') THEN
               BAROR_PID = I4INP
            ELSE
               JERR = JERR + 1
               IERRFL(3) = 'N'
            ENDIF
         ENDIF

         DO J=1,JCARD_LEN                                  ! See if there is an actual V vector.
            IF ((JCARD(6)(J:J) == '.') .OR. (JCARD(7)(J:J) == '.') .OR. (JCARD(8)(J:J) == '.')) THEN
               BAROR_VVEC_TYPE = 'VECTOR   '
               EXIT
            ENDIF
         ENDDO

         IF (BAROR_VVEC_TYPE == 'VECTOR   ') THEN          ! If there was an actual V vector, get components.

            LVVEC = LVVEC + 1
            JERR = 0
            DO J=1,3
               CALL R8FLD ( JCARD(J+5), JF(J+5), R8INP )
               IF (IERRFL(J+5) == 'N') THEN
                  BAROR_VV(J) = R8INP
               ELSE
                  JERR = JERR + 1
                  IERRFL(J+5) = 'N'                        ! Reset IERRFL - we don't want card field errors written from this subr
               ENDIF
            ENDDO
            IF (JERR /= 0) THEN
               BAROR_VVEC_TYPE = 'ERROR    '               ! Found err in V vector components, so reset BAROR_VVEC_TYPE to 'ERROR' 
            ENDIF

         ELSE                                              ! Check to see if there is a grid no. for specifying VVEC

            IF ((JCARD(6)(1:) /= ' ') .AND. (JCARD(7)(1:) == ' ') .AND. (JCARD(8)(1:) == ' ')) THEN

               BAROR_VVEC_TYPE = 'GRID     '               ! We will check in subr BD_BAROR for read error
               CALL I4FLD ( JCARD(6), JF(6), I4INP )
               IF (IERRFL(6) == 'N') THEN
                  IF (I4INP > 0) THEN
                     BAROR_G0 = I4INP
                  ELSE
                     BAROR_VVEC_TYPE = 'ERROR    '         ! Found error in field 6, so reset VVEC_TYPE
                     IERRFL(6) = 'N'                       ! Reset IERRFL - we don't want card field errors written from this subr
                  ENDIF
               ENDIF

            ELSE

               IF ((JCARD(6)(1:) == ' ') .AND. (JCARD(7)(1:) == ' ') .AND. (JCARD(8)(1:) == ' ')) THEN
                  BAROR_VVEC_TYPE = 'UNDEFINED'
               ELSE
                  BAROR_VVEC_TYPE = 'ERROR    '
               ENDIF

            ENDIF

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
 
      END SUBROUTINE BD_BAROR0
