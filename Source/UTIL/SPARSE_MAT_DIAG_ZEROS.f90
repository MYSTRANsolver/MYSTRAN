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

       SUBROUTINE SPARSE_MAT_DIAG_ZEROS ( NAME, NROWS_A, NTERM_A, I_A, J_A, NUM_A_DIAG_ZEROS )

! Determines the number of zero diagonal terms in an input matrix that is stored in compressed row storage format (CRS format)

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  SPARSE_MAT_DIAG_ZEROS_BEGEND

      USE SPARSE_MAT_DIAG_ZEROS_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SPARSE_MAT_DIAG_ZEROS'
      CHARACTER(LEN=*), INTENT(IN)    :: NAME               ! Name of input matrix
      CHARACTER( 1*BYTE)              :: FND_DIAG_TERM='x'  ! If 'Y' we found a diag term in a row of matrix NAME

      INTEGER(LONG), INTENT(IN)       :: NROWS_A            ! Number of rows in input matrix A
      INTEGER(LONG), INTENT(IN)       :: NTERM_A            ! Number of nonzero terms in input matrix A
      INTEGER(LONG), INTENT(IN)       :: I_A(NROWS_A+1)     ! Array of row no's for terms in input matrix A
      INTEGER(LONG), INTENT(IN)       :: J_A(NTERM_A)       ! Array of col no's for terms in input matrix A
      INTEGER(LONG)                   :: A_NTERM_ROW_I      ! Number of terms in row I of matrix A
      INTEGER(LONG)                   :: A_ROW_BEG          ! Index into array I_A where a row of matrix A begins
      INTEGER(LONG)                   :: A_ROW_END          ! Index into array I_A where a row of matrix A ends
      INTEGER(LONG), INTENT(OUT)      :: NUM_A_DIAG_ZEROS   ! Number of zero diagonal terms in input matrix A
      INTEGER(LONG)                   :: I,K                ! DO loop indices 
      INTEGER(LONG)                   :: ZERO_DIAGS(NROWS_A)! Row numbers where there are zero diag terms
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SPARSE_MAT_DIAG_ZEROS_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      NUM_A_DIAG_ZEROS = 0

! Calc outputs

      DO I=1,NROWS_A
         ZERO_DIAGS(I) = 0
      ENDDO

      A_ROW_BEG        = 1
i_do: DO I=1,NROWS_A
         A_NTERM_ROW_I = I_A(I+1) - I_A(I)
         A_ROW_END = A_ROW_BEG + A_NTERM_ROW_I - 1         ! A_ROW_BEG to A_ROW_END is range of indices of terms in A for row I of A
         IF (A_NTERM_ROW_I == 0) THEN                      ! If there are no terms in row I then there is a zero diag term
            NUM_A_DIAG_ZEROS = NUM_A_DIAG_ZEROS + 1
            ZERO_DIAGS(NUM_A_DIAG_ZEROS) = I
            FND_DIAG_TERM = 'Y'
         ELSE
k_do:       DO K=A_ROW_BEG,A_ROW_END
               FND_DIAG_TERM = 'Y'
               IF (J_A(K) == I) THEN                       ! There is a diag term in this row, so cycle on rows
                  A_ROW_BEG = A_ROW_END + 1
                  CYCLE i_do
               ELSE
                  FND_DIAG_TERM = 'N'
               ENDIF
            ENDDO k_do
            IF (FND_DIAG_TERM == 'N') THEN
               NUM_A_DIAG_ZEROS = NUM_A_DIAG_ZEROS + 1     ! In a row with some nonzero terms we have found no diag term
               ZERO_DIAGS(NUM_A_DIAG_ZEROS) = I
            ENDIF
         ENDIF
         A_ROW_BEG = A_ROW_END + 1
      ENDDO i_do

      IF (DEBUG(89) > 0) THEN
         WRITE(F06,100) NAME, NUM_A_DIAG_ZEROS 
         WRITE(F06,101) (ZERO_DIAGS(I),I=1,NUM_A_DIAG_ZEROS)
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  100 FORMAT(' *INFORMATION: MATRIX ',A,' HAS ',I8,' ZERO DIAGONAL TERMS IN ROWS:')

  101 FORMAT(16I8)

! **********************************************************************************************************************************

      END SUBROUTINE SPARSE_MAT_DIAG_ZEROS