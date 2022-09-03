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
 
      SUBROUTINE CRS_NONSYM_TO_CRS_SYM ( NAME_A, NROW_A, NTERM_A, I_A, J_A, A, NAME_B, NTERM_B, I_B, J_B, B )

! Transforms a square symmetric input matrix, A, that is stored in non-symmetric sparse CRS form (i.e. all nonzero terms stored)
! into output matrix B that is stored as symmetric sparse CRS form (i.e. only terms on and above the diagonal stored)

! Input matrix A is stored as:

!      I_A is an array of NROW_A+1 integers that is used to specify the number of nonzero terms in rows of matrix A. That is:
!          I_A(I+1) - I_A(I) are the number of nonzero terms in row I of matrix A

!      J_A is an integer array giving the col numbers of the NTERM_A nonzero terms in matrix A

!        A is a real array of all of the nonzero terms in matrix A.

! Output matrix B is stored as

!      I_B is an array of NROW_A+1 integers that is used to specify the number of nonzero terms in rows of matrix B. That is:
!          I_B(I+1) - I_B(I) are the number of nonzero terms in row I of matrix B

!      J_B is an integer array giving the col numbers of the nonzero terms in matrix B

!        B is a real array of the nonzero terms on and above the diagonal of input matrix A.

! The number of terms in B (i.e. NTERM_B) is related to NTERM_A and NROW_A as: NTERM_B = (NTERM_A - NROW_A)/2 + NROW_A. This assumes
! a square input matrix. The relationship is not checked herein. In addition, symmetry of the input matrix is assumed and only the
! terms on, and above, the diagonal of A are stored in B.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  CRS_NONSYM_TO_CRS_SYM_BEGEND
 
      USE CRS_NONSYM_TO_CRS_SYM_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CRS_NONSYM_TO_CRS_SYM'
      CHARACTER(LEN=*), INTENT(IN)    :: NAME_A            ! Name of input matrix
      CHARACTER(LEN=*), INTENT(IN)    :: NAME_B            ! Name of output matrix
 
      INTEGER(LONG), INTENT(IN)       :: NROW_A            ! Number of rows in input matrix, A
      INTEGER(LONG), INTENT(IN)       :: NTERM_A           ! Number of nonzero terms in input  matrix, A
      INTEGER(LONG), INTENT(IN)       :: NTERM_B           ! Number of nonzero terms in output matrix, B
      INTEGER(LONG), INTENT(IN)       :: I_A(NROW_A+1)     ! I_A(I+1) - I_A(I) are the number of nonzeros in A row I
      INTEGER(LONG), INTENT(IN)       :: J_A(NTERM_A)      ! Col numbers for nonzero terms in A
      INTEGER(LONG), INTENT(OUT)      :: I_B(NROW_A+1)     ! I_B(I+1) - I_B(I) are the num of nonzeros in B row I
      INTEGER(LONG), INTENT(OUT)      :: J_B(NTERM_B)      ! Col numbers for nonzero terms in B
      INTEGER(LONG)                   :: I,K               ! DO loop indices or counters
      INTEGER(LONG)                   :: KBEG_A            ! Index into array I_A where a row of matrix A begins
      INTEGER(LONG)                   :: KEND_A            ! Index into array I_A where a row of matrix A ends
      INTEGER(LONG)                   :: KTERM_B           ! Count of number of nonzero terms put into output matrix B
      INTEGER(LONG)                   :: A_NTERM_ROW_I     ! Number of terms in a row of input matrix A
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CRS_NONSYM_TO_CRS_SYM_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: A(NTERM_A)        ! Real nonzero values in input  matrix A
      REAL(DOUBLE) , INTENT(OUT)      :: B(NTERM_B)        ! Real nonzero values in output matrix B

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      DO I=1,NROW_A+1
         I_B(I) = 0
      ENDDO

      DO I=1,NTERM_B
         J_B(I) = 0
           B(I) = ZERO
      ENDDO

      I_B(1)  = 1
      KTERM_B = 0
      KBEG_A  = 1
      DO I=1,NROW_A
         I_B(I+1) = I_B(I)
         A_NTERM_ROW_I = I_A(I+1) - I_A(I)
         KEND_A = KBEG_A + A_NTERM_ROW_I - 1               ! KBEG_A to KEND_A is the range of indices of terms in A for row I of A
         DO K=KBEG_A,KEND_A
            IF (J_A(K) >= I) THEN                          ! This is a term from A that is on, or above, the diagonal
               KTERM_B = KTERM_B + 1                       ! Increment the counter for the total number of terms in output matrix B
               IF (KTERM_B > NTERM_B) CALL ARRAY_SIZE_ERROR_1( SUBR_NAME, NTERM_B, NAME_B ) 
               I_B(I+1) = I_B(I+1) + 1                       ! Increment I_B(I+1) for the number of terms in row I of B
               J_B(KTERM_B) = J_A(K)                       ! Set column number for the term to go into B
                 B(KTERM_B) =   A(K)                       ! Set value for the term to go into B
            ENDIF
         ENDDO
         KBEG_A = KEND_A + 1
      ENDDO 
         
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 
      END SUBROUTINE CRS_NONSYM_TO_CRS_SYM