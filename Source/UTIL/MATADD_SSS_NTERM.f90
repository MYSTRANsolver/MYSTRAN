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
 
      SUBROUTINE MATADD_SSS_NTERM ( NROWS, MAT_A_NAME, NTERM_A, I_A, J_A, SYM_A, MAT_B_NAME, NTERM_B, I_B, J_B, SYM_B,             &
                                           MAT_C_NAME, NTERM_C )
 
! Setup routine for performing the sparse matrix add operation C = A + B where A, B and C are in stored in sparse CRS format.
! This subr must be run prior to the subr that actually does the add (MATADD_SSS) in order to calc NTERM_C, the number of terms that
! will be in C (so that memory could be allocated, prior to this MATADD_SSS, for arrays J_C and C)

! Matrices A and B must have the same number of rows and cols. Ensuring this is the responsibility of the user.

! NOTE: Both of the 2 input matrices, as well as the matrix resulting from the addition, must be stored the same as far as
! symmetry is concerned. That is, if A (or B) is a symmetric matrix and is stored with only the terms on and above the diagonal,
! then both input matrices must be stored with only the terms on and above the diagonal. Matrix C will, by implication, be
! symmetric and have only terms on and above its diagonal in array C. Thus, this subr cannot add 2 matrices where one is stored
! symmetric and the other is not. The user is required to ensure that this is the case.

      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  MATADD_SSS_NTERM_BEGEND
 
      USE MATADD_SSS_NTERM_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MATADD_SSS_NTERM'
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_A_NAME        ! Name of matrix A
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_B_NAME        ! Name of matrix B
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_C_NAME        ! Name of matrix C
      CHARACTER(LEN=*), INTENT(IN)    :: SYM_A             ! Flag for whether matrix A is stored sym (terms on and above diag)
!                                                            or nonsym (all terms)
      CHARACTER(LEN=*), INTENT(IN)    :: SYM_B             ! Flag for whether matrix B is stored sym (terms on and above diag)
!                                                            or nonsym (all terms)

      INTEGER(LONG), INTENT(IN )      :: NROWS             ! Number of rows in input matrices A and B
      INTEGER(LONG), INTENT(IN )      :: NTERM_A           ! Number of nonzero terms in input matrix A
      INTEGER(LONG), INTENT(IN )      :: NTERM_B           ! Number of nonzero terms in input matrix B
      INTEGER(LONG), INTENT(IN )      :: I_A(NROWS+1)      ! I_A(I+1) - I_A(I) = no. terms in row I of matrix A
      INTEGER(LONG), INTENT(IN )      :: I_B(NROWS+1)      ! I_B(I+1) - I_B(I) = no. terms in row I of matrix B
      INTEGER(LONG), INTENT(IN )      :: J_A(NTERM_A)      ! Col no's for nonzero terms in matrix A
      INTEGER(LONG), INTENT(IN )      :: J_B(NTERM_B)      ! Col no's for nonzero terms in matrix B
      INTEGER(LONG), INTENT(OUT)      :: NTERM_C           ! Number of nonzero terms in output matrix C
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MATADD_SSS_NTERM_BEGEND


      INTEGER(LONG)                   :: ROW
      INTEGER(LONG)                   :: P_A
      INTEGER(LONG)                   :: P_B
      INTEGER(LONG)                   :: COL_A
      INTEGER(LONG)                   :: COL_B
      INTEGER(LONG)                   :: CNT

       
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Make sure that input matrices A and B are stored in same format

      IF (SYM_A /= SYM_B) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,941) SUBR_NAME, MAT_A_NAME, MAT_B_NAME, MAT_A_NAME, SYM_A, MAT_B_NAME, SYM_B
         WRITE(F06,941) SUBR_NAME, MAT_A_NAME, MAT_B_NAME, MAT_A_NAME, SYM_A, MAT_B_NAME, SYM_B
         CALL OUTA_HERE ( 'Y' )
      ENDIF



      CNT = 0

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
               P_A = P_A + 1
            ELSE IF (COL_B < COL_A) THEN                   ! Only B has an entry in this column
               CNT = CNT + 1
               P_B = P_B + 1
            ELSE                                           ! Both have an entry
               CNT = CNT + 1
               P_A = P_A + 1
               P_B = P_B + 1
            ENDIF
         
         ENDDO
            
      ENDDO

      NTERM_C = CNT


! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF
 
      RETURN

! **********************************************************************************************************************************
  941 FORMAT(' *ERROR   941: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INPUT MATRICES ',A,' AND ',A,' MUST BOTH BE STORED IN THE SAME FORMAT (SYM MUST BE BOTH "Y" OR "N").' &
                    ,/,14X,' HOWEVER, MATRIX ',A,' HAS SYM = ',A,' AND MATRIX ',A,' HAS SYM = ',A)


! **********************************************************************************************************************************

      END SUBROUTINE MATADD_SSS_NTERM
