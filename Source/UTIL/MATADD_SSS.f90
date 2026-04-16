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
 
      SUBROUTINE MATADD_SSS ( NROWS, MAT_A_NAME, NTERM_A, I_A, J_A, A, ALPHA,                          &
                                     MAT_B_NAME, NTERM_B, I_B, J_B, B, BETA,                           &
                                     MAT_C_NAME, NTERM_C, I_C, J_C, C )
 
!///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
! Subroutine MATADD_SSS_NTERM must be run before this subroutine to calculate NTERM_C, an input to this subroutine, that is the
! number of nonzero terms in C. Then memory can be allocated to C before this subroutine is called
!///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

! Performs the matrix operation C = A + B. Input matrices A and B are in sparse (compressed row storage) format.
! Output matrix C is in sparse format.

! Matrices A and B must have the same number of rows and cols. Ensuring this is the responsibility of the user.

! NOTE: Both of the 2 input matrices, as well as the matrix resulting from the addition, must be stored the same as far as
! symmetry is concerned. That is, if A (or B) is a symmetric matrix and is stored with only the terms on and above the diagonal,
! then both input matrices must be stored with only the terms on and above the diagonal. Matrix C will, by implication, be
! symmetric and have only terms on and above its diagonal in array C. Thus, this subr cannot add 2 matrices where one is stored
! symmetric and the other is not. The user is required to ensure that this is the case.

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  MATADD_SSS_BEGEND
 
      USE MATADD_SSS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MATADD_SSS'
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_A_NAME        ! Name of matrix A
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_B_NAME        ! Name of matrix B
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_C_NAME        ! Name of matrix C

      INTEGER(LONG), INTENT(IN )      :: NROWS             ! Number of rows in input matrices A and B
      INTEGER(LONG), INTENT(IN )      :: NTERM_A           ! Number of nonzero terms in input matrix A
      INTEGER(LONG), INTENT(IN )      :: NTERM_B           ! Number of nonzero terms in input matrix B
      INTEGER(LONG), INTENT(IN )      :: NTERM_C           ! Number of nonzero terms in output matrix C
      INTEGER(LONG), INTENT(IN )      :: I_A(NROWS+1)      ! I_A(I+1) - I_A(I) = no. terms in row I of matrix A
      INTEGER(LONG), INTENT(IN )      :: I_B(NROWS+1)      ! I_B(I+1) - I_B(I) = no. terms in row I of matrix B
      INTEGER(LONG), INTENT(IN )      :: J_A(NTERM_A)      ! Col no's for nonzero terms in matrix A
      INTEGER(LONG), INTENT(IN )      :: J_B(NTERM_B)      ! Col no's for nonzero terms in matrix B
      INTEGER(LONG), INTENT(OUT)      :: I_C(NROWS+1)      ! I_C(I+1) - I_C(I) = no. terms in row I of matrix C
      INTEGER(LONG), INTENT(OUT)      :: J_C(NTERM_C)      ! Col no's for nonzero terms in matrix C
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MATADD_SSS_BEGEND
       
      REAL(DOUBLE) , INTENT(IN )      :: A(NTERM_A)        ! Nonzero terms in matrix A
      REAL(DOUBLE) , INTENT(IN )      :: B(NTERM_B)        ! Nonzero terms in matrix B
      REAL(DOUBLE) , INTENT(IN )      :: ALPHA             ! Scalar multiplier for matrix A
      REAL(DOUBLE) , INTENT(IN )      :: BETA              ! Scalar multiplier for matrix B
      REAL(DOUBLE) , INTENT(OUT)      :: C(NTERM_C)        ! Nonzero terms in matrix C

      INTEGER(LONG)                   :: ROW
      INTEGER(LONG)                   :: P_A
      INTEGER(LONG)                   :: P_B
      INTEGER(LONG)                   :: COL_A
      INTEGER(LONG)                   :: COL_B
      INTEGER(LONG)                   :: CNT
      REAL(DOUBLE)                    :: V
      
      
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************

      CNT = 0
      I_C(1) = 1

      DO ROW=1,NROWS
         P_A = I_A(ROW)
         P_B = I_B(ROW)
      
         DO WHILE(P_A < I_A(ROW+1) .OR. P_B < I_B(ROW+1))
         
                                                           ! Sentinel when A's row is exhausted
            IF (P_A < I_A(ROW+1)) then
               COL_A = J_A(P_A)
            ELSE
               COL_A = HUGE(0)
            ENDIF
                                                           ! Sentinel when B's row is exhausted
            IF (P_B < I_B(ROW+1)) then
               COL_B = J_B(P_B)
            ELSE
               COL_B = HUGE(0)
            ENDIF

 
            IF (COL_A < COL_B) THEN                        ! Only A has an entry in this column
               CNT = CNT + 1
               C(CNT) = ALPHA * A(P_A)
               J_C(CNT) = COL_A
               P_A = P_A + 1
            ELSE IF (COL_B < COL_A) THEN                   ! Only B has an entry in this column
               CNT = CNT + 1
               C(CNT) = BETA * B(P_B)
               J_C(CNT) = COL_B
               P_B = P_B + 1
            ELSE                                           ! Both have an entry — add
               V = ALPHA * A(P_A) + BETA * B(P_B)
               CNT = CNT + 1
               C(CNT) = V
               J_C(CNT) = COL_A
               P_A = P_A + 1
               P_B = P_B + 1
            ENDIF
         
         ENDDO
      
         I_C(ROW+1) = CNT + 1
      
      ENDDO


! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF
 
      RETURN

! **********************************************************************************************************************************


      END SUBROUTINE MATADD_SSS
