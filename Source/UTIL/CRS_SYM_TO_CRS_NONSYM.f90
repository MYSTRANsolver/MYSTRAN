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
 
      SUBROUTINE CRS_SYM_TO_CRS_NONSYM ( NAME_A, NROW_A, NTERM_A, I_A, J_A, A, NAME_B, NTERM_B, I_B, J_B, B, WRT_SCREEN )

! Transforms a square symmetric input matrix, A, that is stored in symmetric sparse CRS form (i.e. only nonzero terms on and above
! diagonal stored) into output matrix B that is stored as nonsymmetric sparse CRS form (i.e. all nonzero terms stored)

! Input matrix A is stored as:

!      I_A is an array of NROW_A+1 integers that is used to specify the number of nonzero terms in rows of matrix A. That is:
!          I_A(I+1) - I_A(I) are the number of nonzero terms in row I of matrix A

!      J_A is an integer array giving the col numbers of the NTERM_A nonzero terms in matrix A

!        A is a real array of the nonzero terms on and above the diagonal in matrix A.

! Output matrix B is stored as

!      I_B is an array of NROW_A+1 integers that is used to specify the number of nonzero terms in rows of matrix B. That is:
!          I_B(I+1) - I_B(I) are the number of nonzero terms in row I of matrix B

!      J_B is an integer array giving the col numbers of the nonzero terms in matrix B

!        B is a real array of all of the nonzero terms that would be in input matrix A.

! The number of terms in B (i.e. NTERM_B) is related to NTERM_A and NROW_A as: NTERM_B = 2*NTERM_A - NDIAG_A_NZ where
! NDIAG_A_NZ are the number of nonzero diagonal terms in input matriz A. This assumes
! a square input matrix. The relationship is not checked herein. In addition, symmetry of the input matrix is assumed.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  CRS_SYM_TO_CRS_NONSYM_BEGEND
 
      USE CRS_SYM_TO_CRS_NONSYM_USE_IFs

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CRS_SYM_TO_CRS_NONSYM'
      CHARACTER(LEN=*), INTENT(IN)    :: NAME_A            ! Name of input matrix
      CHARACTER(LEN=*), INTENT(IN)    :: NAME_B            ! Name of output matrix
      CHARACTER(LEN=*), INTENT(IN)    :: WRT_SCREEN        ! If 'Y' then write msgs to screen
 
      INTEGER(LONG), INTENT(IN)       :: NROW_A            ! Number of rows in input matrix, A
      INTEGER(LONG), INTENT(IN)       :: NTERM_A           ! Number of nonzero terms in input  matrix, A
      INTEGER(LONG), INTENT(IN)       :: NTERM_B           ! Number of nonzero terms in output matrix, B
      INTEGER(LONG), INTENT(IN)       :: I_A(NROW_A+1)     ! I_A(I+1) - I_A(I) are the number of nonzeros in A row I
      INTEGER(LONG), INTENT(IN)       :: J_A(NTERM_A)      ! Col numbers for nonzero terms in A
      INTEGER(LONG), INTENT(OUT)      :: I_B(NROW_A+1)     ! I_B(I+1) - I_B(I) are the num of nonzeros in B row I
      INTEGER(LONG), INTENT(OUT)      :: J_B(NTERM_B)      ! Col numbers for nonzero terms in B
      INTEGER(LONG)                   :: A_NTERM_ROW_I     ! Number of terms in a row of input matrix A
      INTEGER(LONG)                   :: A_ROW_BEG         ! Index into array I_A where a row of matrix A begins
      INTEGER(LONG)                   :: A_ROW_END         ! Index into array I_A where a row of matrix A ends
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: I2_A(NTERM_A)     ! Row numbers of the terms in A
      INTEGER(LONG)                   :: K                 ! Counter
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CRS_SYM_TO_CRS_NONSYM_BEGEND

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

      IF (WRT_SCREEN == 'Y') THEN
   !xx   WRITE(SC1, * )                                    ! Write blank line to screen
      ENDIF

! Generate I2

      K = 0
      DO I=1,NROW_A
         A_NTERM_ROW_I = I_A(I+1) - I_A(I)
         DO J = 1,A_NTERM_ROW_I
            K = K + 1
            I2_A(K) = I
         ENDDO
      ENDDO

      I_B(1) = 1
      K      = 0
      A_ROW_BEG = 1
i_do: DO I=1,NROW_A                                        ! Matrix multiply loop. Range over the rows in A

         IF (WRT_SCREEN == 'Y') THEN
            WRITE(SC1,12345,ADVANCE='NO') I, NROW_A, NAME_B, CR13
         ENDIF 

         I_B(I+1) = I_B(I)

         A_NTERM_ROW_I = I_A(I+1) - I_A(I)                 ! Number of terms in matrix A in row I 
         A_ROW_END = A_ROW_BEG + A_NTERM_ROW_I - 1         ! A_ROW_BEG to A_ROW_END is range of indices of terms in A for row I of A

         DO J=1,A_ROW_BEG-1                                ! 1st, look for terms that would be in this row, but are not, due to sym
            IF (J_A(J) == I) THEN
               I_B(I+1) = I_B(I+1) + 1
               K        = K + 1
               J_B(K)   = I2_A(J)
                 B(K)   =    A(J)
            ENDIF
         ENDDO

         DO J=A_ROW_BEG,A_ROW_END                          ! 2nd, get terms from this row of A from the diagonal out
            I_B(I+1) = I_B(I+1) + 1
            K        = K + 1
            J_B(K)   = J_A(J)
              B(K)   =   A(J)
         ENDDO

         A_ROW_BEG = A_ROW_END + 1

      ENDDO i_do
      WRITE(SC1,*) CR13
         
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
12345 FORMAT(7X,'Calculating        : row  ',I8,' of ',I8,' matrix ',A,A)

! **********************************************************************************************************************************

      END SUBROUTINE CRS_SYM_TO_CRS_NONSYM