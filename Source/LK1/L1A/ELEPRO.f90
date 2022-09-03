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
 
      SUBROUTINE ELEPRO ( INCR_NELE, JCARD, NFIELD, NMORE,                                                                         &
                          CHK_FLD2, CHK_FLD3, CHK_FLD4, CHK_FLD5, CHK_FLD6, CHK_FLD7, CHK_FLD8, CHK_FLD9 )
 
! Element connection data processor.

!  1) Increments count on number of elements and checks that the number does not exceed with the total count made by subr LOADB0
!  2) Checks to make sure that, when all integer data is put into EDAT for this element, that NEDAT won't exceed the max value that
!     was counted in LOADB0. NMORE
!  3) Verifies that all integer ID's are > 0 (since it is generally reading only elem ID, prop ID and grid connections)
!  4) Loads NFIELD fields of data for this element into integer array EDAT (the calling routine may add more data)

! If there are no errors:

!  3) Reads element connection data into EDAT
!  4) Resets pointer array, EPNT.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  IERRFL, FATAL_ERR, JF, LEDAT, LELE, NEDAT, NELE, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELEPRO_BEGEND
      USE MODEL_STUF, ONLY            :  EDAT, EPNT
 
      USE ELEPRO_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ELEPRO'
      CHARACTER(LEN=*), INTENT(IN)    :: CHK_FLD2          ! If 'N', then if field 2 is blank it will not be checked for > 0
      CHARACTER(LEN=*), INTENT(IN)    :: CHK_FLD3          ! If 'N', then if field 3 is blank it will not be checked for > 0
      CHARACTER(LEN=*), INTENT(IN)    :: CHK_FLD4          ! If 'N', then if field 4 is blank it will not be checked for > 0
      CHARACTER(LEN=*), INTENT(IN)    :: CHK_FLD5          ! If 'N', then if field 5 is blank it will not be checked for > 0
      CHARACTER(LEN=*), INTENT(IN)    :: CHK_FLD6          ! If 'N', then if field 6 is blank it will not be checked for > 0
      CHARACTER(LEN=*), INTENT(IN)    :: CHK_FLD7          ! If 'N', then if field 7 is blank it will not be checked for > 0
      CHARACTER(LEN=*), INTENT(IN)    :: CHK_FLD8          ! If 'N', then if field 8 is blank it will not be checked for > 0
      CHARACTER(LEN=*), INTENT(IN)    :: CHK_FLD9          ! If 'N', then if field 9 is blank it will not be checked for > 0
      CHARACTER(LEN=*), INTENT(IN)    :: INCR_NELE         ! If 'Y', increment NELE. Otherwise do not increment NELE
      CHARACTER(LEN=*), INTENT(IN)    :: JCARD(10)         ! The 10 fields of a Bulk Data card
      CHARACTER( 9*BYTE)              :: NAME = '         '! Name for output error purposes
      CHARACTER(LEN=LEN(CHK_FLD2))    :: CHK_FLD_ARRAY(2:9)! Array containing CHK_FLDi's
 
      INTEGER(LONG), INTENT(IN)       :: NFIELD            ! Number of card fields to read from JCARD (start w/ field 2) 
      INTEGER(LONG), INTENT(IN)       :: NMORE             ! Number of terms that have to be written to EDAT for this element
!                                                            in total (not just here). There may be multiple calls to this subr for
!                                                            one element in which case NMORE is the additional amount of data to be
!                                                            written in this call. In addition, some data fields for this element
!                                                            may be written to EDAT in the subr that called this subr in which case
!                                                            NMORE may be greater than NFIELD so that a check on whether that
!                                                            additional data will fit into EDAT can be made here.
      INTEGER(LONG)                   :: I4INP             ! A value read from input file that should be an integer value
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELEPRO_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      CHK_FLD_ARRAY(2) = CHK_FLD2
      CHK_FLD_ARRAY(3) = CHK_FLD3
      CHK_FLD_ARRAY(4) = CHK_FLD4
      CHK_FLD_ARRAY(5) = CHK_FLD5
      CHK_FLD_ARRAY(6) = CHK_FLD6
      CHK_FLD_ARRAY(7) = CHK_FLD7
      CHK_FLD_ARRAY(8) = CHK_FLD8
      CHK_FLD_ARRAY(9) = CHK_FLD9

      IF      (INCR_NELE == 'Y') THEN
         NELE = NELE + 1                                   ! Increment element counter, 'NELE'
      ELSE IF (INCR_NELE /= 'N') THEN
         NAME = 'INCR_NELE'
         WRITE(ERR,1013) NAME, INCR_NELE
         WRITE(F06,1013) NAME, INCR_NELE
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )
      ENDIF

      DO I=2,9
         IF ((CHK_FLD_ARRAY(I) /= 'Y') .AND. (CHK_FLD_ARRAY(I) /= 'N')) THEN
            NAME = 'CHK_FLD'
            WRITE(ERR,1013) NAME, CHK_FLD_ARRAY(I)
            WRITE(F06,1013) NAME, CHK_FLD_ARRAY(I)
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )
         ENDIF
      ENDDO

      IF (NELE > LELE) THEN
         WRITE(ERR,1000) SUBR_NAME, LELE
         WRITE(F06,1000) SUBR_NAME, LELE
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )                            ! Coding error (too many elems), so quit
      ENDIF 
 
      IF ((NEDAT + NMORE) > LEDAT) THEN
         WRITE(ERR,1001) SUBR_NAME, LEDAT
         WRITE(F06,1001) SUBR_NAME, LEDAT
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )                            ! Coding error (too much EDAT data), so quit
      ENDIF
 
      IF (INCR_NELE == 'Y') THEN
         EPNT(NELE) = NEDAT+1                              ! Set element pointer EPNT to start for new element
      ENDIF

jdo:  DO J=1,NFIELD                                        ! Load element data into array EDAT
 
         IF ((J == 3) .AND. (JCARD(1)(1:6) == 'CBUSH ')) THEN
            NEDAT = NEDAT + 1
            IF (JCARD(5)(1:) == ' ') THEN                  ! CBUSH has G2 blank so BUSH elem has only G1 grid. Other end is grounded
               EDAT(NEDAT) = 1
            ELSE                                           ! CBUSH G2 not blank so BUSH elem has 2 grids
               EDAT(NEDAT) = 2
            ENDIF
         ENDIF

         CALL I4FLD ( JCARD(J+1), JF(J+1), I4INP )
         IF (IERRFL(J+1) == 'N') THEN
            IF (CHK_FLD_ARRAY(J+1) == 'Y') THEN
               IF (I4INP <= 0) THEN                        ! No error if BUSH and G2 field is blank (BUSH grounded at end G2)
                  IF ((JCARD(1)(1:8) == 'CBUSH   ') .AND. (JCARD(5)(1:) == ' ')) THEN
                     EXIT jdo
                  ENDIF
                  WRITE(ERR,1021) JCARD(1), JCARD(2), I4INP, JF(J+1)
                  WRITE(F06,1021) JCARD(1), JCARD(2), I4INP, JF(J+1)
                  FATAL_ERR = FATAL_ERR + 1
               ENDIF
            ENDIF
            NEDAT = NEDAT + 1
            EDAT(NEDAT) = I4INP
         ENDIF

      ENDDO jdo

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1000 FORMAT(' *ERROR  1000: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MANY ELEMENTS. LIMIT = ',I8)

 1001 FORMAT(' *ERROR  1001: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' TOO MUCH ELEMENT DATA IN ARRAY EDAT. LIMIT = ',I8)

 1013 FORMAT(' *ERROR  1013: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' SUBROUTINE CALLED WITH ILLEGAL VALUE FOR ',A,' = ',A,'. MUST BE EITHER Y OR N')

 1021 FORMAT(' *ERROR  1021: ',A,A,' HAS INTEGER = ',I8,' IN FIELD ',I3,'. MUST BE > 0')

! **********************************************************************************************************************************
 
      END SUBROUTINE ELEPRO
