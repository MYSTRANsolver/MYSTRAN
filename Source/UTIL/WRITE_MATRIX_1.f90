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

      SUBROUTINE WRITE_MATRIX_1 ( FILNAM, UNT, CLOSE_IT, CLOSE_STAT, MESSAG, NAME, NTERM, NROWS, I_MATIN, J_MATIN, MATIN )

! Writes sparse (compressed row storage format) matrix data to an unformatted file in the format:

!    Record 1  : NTERM       = No. nonzero terms in matrix (also no. records following this one)
!    Record 1+i: i, j, value = row no., col no., nonzero value for matrix MATOUT


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_MATRIX_1_BEGEND

      USE WRITE_MATRIX_1_USE_IFs

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_MATRIX_1'
      CHARACTER(LEN=*), INTENT(IN)    :: CLOSE_IT          ! ='Y'/'N' whether to close UNT or not
      CHARACTER(LEN=*), INTENT(IN)    :: CLOSE_STAT        ! What to do with file when it is closed
      CHARACTER(LEN=*), INTENT(IN)    :: FILNAM            ! File name
      CHARACTER(LEN=*), INTENT(IN)    :: NAME              ! Matrix name
      CHARACTER(LEN=*), INTENT(IN)    :: MESSAG            ! File description. Input to subr UNFORMATTED_OPEN 

      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in MATIN
      INTEGER(LONG), INTENT(IN)       :: NTERM             ! Number of matrix terms that should be in FILNAM
      INTEGER(LONG), INTENT(IN)       :: UNT               ! Unit number of FILNAM
      INTEGER(LONG), INTENT(IN)       :: I_MATIN(NROWS+1)  ! Row numbers for terms in matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: J_MATIN(NTERM)    ! Col numbers for terms in matrix MATIN
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices or counters
      INTEGER(LONG)                   :: NTERM_ROW_I       ! Number of terms in row I of MATIN
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_MATRIX_1_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: MATIN(NTERM)      ! Real values for matrix MATIN

      INTRINSIC DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      OUNT(1) = ERR
      OUNT(2) = F06

      CALL FILE_OPEN ( UNT, FILNAM, OUNT, 'REPLACE', MESSAG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )

! Write sparse (compressed row storage) matrix to file in i, j, val format:

      WRITE(UNT) NTERM
      K = 0
!xx   WRITE(SC1, * )
      DO I=1,NROWS
         NTERM_ROW_I = I_MATIN(I+1) - I_MATIN(I)
         WRITE(SC1,12345,ADVANCE='NO') NAME, I, NROWS, CR13
         DO J=1,NTERM_ROW_I
            K = K + 1
            IF (K > NTERM) CALL ARRAY_SIZE_ERROR_1( SUBR_NAME, NTERM, NAME) 
            WRITE(UNT) I,J_MATIN(K),MATIN(K)

         ENDDO 
      ENDDO
      WRITE(SC1,*) CR13

      IF (CLOSE_IT == 'Y') THEN
         CALL FILE_CLOSE ( UNT, FILNAM, CLOSE_STAT, 'Y' )
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
12345 FORMAT(7X,A,': writing row  ',i8,' of ',i8,A)

! **********************************************************************************************************************************

      END SUBROUTINE WRITE_MATRIX_1
