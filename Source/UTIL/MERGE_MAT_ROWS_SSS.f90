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
 
      SUBROUTINE MERGE_MAT_ROWS_SSS ( MAT_A_NAME, NROW_A, NTERM_A, I_A, J_A, A, MERGE_VEC_VALS_A,                                  &
                                      MAT_B_NAME, NROW_B, NTERM_B, I_B, J_B, B, MERGE_VEC_VALS_B, MERGE_VEC,                       &
                                      MAT_C_NAME,                  I_C, J_C, C )
 
! Merges rows of 2 sparse CRS matrices, which have the same number of cols, into a new sparse CRS matrix 

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SPARSE_ALG_ARRAYS, ONLY     :  LOGICAL_VEC, REAL_VEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  MERGE_MAT_ROWS_SSS_BEGEND
 
      USE MERGE_MAT_ROWS_SSS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MERGE_MAT_ROWS_SSS'
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_A_NAME              ! Name of matrix A
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_B_NAME              ! Name of matrix B
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_C_NAME              ! Name of matrix C

      INTEGER(LONG), INTENT(IN )      :: NROW_A                  ! Number of rows in matrix A (needed to dimension arrays below)
      INTEGER(LONG), INTENT(IN )      :: NROW_B                  ! Number of rows in matrix B (needed to dimension arrays below)
      INTEGER(LONG), INTENT(IN )      :: NTERM_A                 ! Number of nonzero terms in input matrix A
      INTEGER(LONG), INTENT(IN )      :: NTERM_B                 ! Number of nonzero terms in input matrix B
      INTEGER(LONG), INTENT(IN )      :: I_A(NROW_A+1)           ! I_A(I+1) - I_A(I) = no. terms in row I of matrix A
      INTEGER(LONG), INTENT(IN )      :: I_B(NROW_B+1)           ! I_B(I+1) - I_B(I) = no. terms in row I of matrix B
      INTEGER(LONG), INTENT(IN )      :: J_A(NTERM_A)            ! Col no's for nonzero terms in matrix A
      INTEGER(LONG), INTENT(IN )      :: J_B(NTERM_B)            ! Col no's for nonzero terms in matrix B
      INTEGER(LONG), INTENT(IN )      :: MERGE_VEC(NROW_A+NROW_B)! Vector used in merging rows of A and B into C
      INTEGER(LONG), INTENT(IN )      :: MERGE_VEC_VALS_A        ! Values in MERGE_VEC corresponding to rows in matrix A
      INTEGER(LONG), INTENT(IN )      :: MERGE_VEC_VALS_B        ! Values in MERGE_VEC corresponding to rows in matrix B
      INTEGER(LONG), INTENT(OUT)      :: I_C(NROW_A+NROW_B+1)    ! I_C(I+1) - I_C(I) = no. terms in row I of matrix C
      INTEGER(LONG), INTENT(OUT)      :: J_C(NTERM_A+NTERM_B)    ! Col no's for nonzero terms in matrix C
      INTEGER(LONG)                   :: I,J                     ! DO loop indices or counters
      INTEGER(LONG)                   :: KTERM_A                 ! Count of number terms in arrays J_A and A
      INTEGER(LONG)                   :: KTERM_B                 ! Count of number terms in arrays J_B and B
      INTEGER(LONG)                   :: KTERM_C                 ! Count of number terms in arrays J_C and C
      INTEGER(LONG)                   :: NROW_C                  ! Num of rows in matrix C
      INTEGER(LONG)                   :: NUM_IN_ROW_OF_A         ! Num terms in a row of A matrix
      INTEGER(LONG)                   :: NUM_IN_ROW_OF_B         ! Num terms in a row of B matrix
      INTEGER(LONG)                   :: NUM_IN_ROW_OF_C         ! Num terms in a row of C matrix
      INTEGER(LONG)                   :: ROW_NUM_A               ! Row number in matrix A
      INTEGER(LONG)                   :: ROW_NUM_B               ! Row number in matrix B
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MERGE_MAT_ROWS_SSS_BEGEND
       
      REAL(DOUBLE) , INTENT(IN )      :: A(NTERM_A)              ! Nonzero terms in matrix A
      REAL(DOUBLE) , INTENT(IN )      :: B(NTERM_B)              ! Nonzero terms in matrix B
      REAL(DOUBLE) , INTENT(OUT)      :: C(NTERM_A+NTERM_B)      ! Nonzero terms in matrix C

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      NROW_C    = NROW_A  + NROW_B
      ROW_NUM_A = 0
      ROW_NUM_B = 0
      KTERM_A   = 0
      KTERM_B   = 0
      KTERM_C   = 0
      I_C(1) = 1
      DO I=1,NROW_C
         I_C(I+1) = I_C(I)
         IF      (MERGE_VEC(I) == MERGE_VEC_VALS_A) THEN   ! Get a row of matrix A and put it into C
            ROW_NUM_A       = ROW_NUM_A + 1
            NUM_IN_ROW_OF_A = I_A(ROW_NUM_A+1) - I_A(ROW_NUM_A)
            NUM_IN_ROW_OF_C = NUM_IN_ROW_OF_A
            DO J=1,NUM_IN_ROW_OF_A
               KTERM_A      = KTERM_A + 1
               I_C(I+1)     = I_C(I+1) + 1
               KTERM_C      = KTERM_C + 1
               J_C(KTERM_C) = J_A(KTERM_A)
                 C(KTERM_C) =   A(KTERM_A)
            ENDDO
         ELSE IF (MERGE_VEC(I) == MERGE_VEC_VALS_B) THEN   ! Get a row of matrix B and put it into C
            ROW_NUM_B       = ROW_NUM_B + 1
            NUM_IN_ROW_OF_B = I_B(ROW_NUM_B+1) - I_B(ROW_NUM_B)
            NUM_IN_ROW_OF_C = NUM_IN_ROW_OF_B
            DO J=1,NUM_IN_ROW_OF_B
               KTERM_B      = KTERM_B + 1
               I_C(I+1)     = I_C(I+1) + 1
               KTERM_C      = KTERM_C + 1
               J_C(KTERM_C) = J_B(KTERM_B)
                 C(KTERM_C) =   B(KTERM_B)
            ENDDO
         ENDIF
      ENDDO

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF
 
      RETURN

! **********************************************************************************************************************************


! **********************************************************************************************************************************

      END SUBROUTINE MERGE_MAT_ROWS_SSS
