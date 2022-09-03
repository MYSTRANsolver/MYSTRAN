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

      SUBROUTINE MERGE_MAT_COLS_SSS ( MAT_A_NAME, NTERM_A, I_A, J_A, A, SYM_A, NCOL_A,                                             &
                                      MAT_B_NAME, NTERM_B, I_B, J_B, B, SYM_B, NROWS,                                              &
                                      MAT_C_NAME,          I_C, J_C, C, SYM_C )

! Merges 2 sparse CRS matrices (A and B), which have the same number of rows, into a new sparse CRS matrix C.
! The columns of A are the 1 through NCOL_A columns of matrix C. The cols of B begin at column NCOL_A+1 in matrix C.
! The number of columns in C is NCOL_A + the number of columns in B (not explicitly specified - i.e. the number of columns
! of B can be inferred from the array J_B)

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  MERGE_MAT_COLS_SSS_BEGEND

      USE MERGE_MAT_COLS_SSS_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MERGE_MAT_COLS_SSS'
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_A_NAME              ! Name of matrix A
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_B_NAME              ! Name of matrix B
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_C_NAME              ! Name of matrix C
      CHARACTER(LEN=*), INTENT(IN)    :: SYM_A                   ! Flag for whether matrix A is stored sym (terms on and above diag)
!                                                                  or nonsym (all terms)
      CHARACTER(LEN=*), INTENT(IN)    :: SYM_B                   ! Flag for whether matrix B is stored sym (terms on and above diag)
!                                                                  or nonsym (all terms)
      CHARACTER(LEN=*), INTENT(IN)    :: SYM_C                   ! Flag for whether matrix C is stored sym (terms on and above diag)
!                                                                  or nonsym (all terms)

      INTEGER(LONG)   , INTENT(IN)    :: NCOL_A                  ! Number of cols in matrix A
      INTEGER(LONG)   , INTENT(IN)    :: NROWS                   ! Number of rows in matrices A, B, C (needed to dim arrays below)
      INTEGER(LONG)   , INTENT(IN)    :: NTERM_A                 ! Number of nonzero terms in input matrix A
      INTEGER(LONG)   , INTENT(IN)    :: NTERM_B                 ! Number of nonzero terms in input matrix B
      INTEGER(LONG)   , INTENT(IN)    :: I_A(NROWS+1)            ! I_A(I+1) - I_A(I) = no. terms in row I of matrix A
      INTEGER(LONG)   , INTENT(IN)    :: I_B(NROWS+1)            ! I_B(I+1) - I_B(I) = no. terms in row I of matrix B
      INTEGER(LONG)   , INTENT(IN)    :: J_A(NTERM_A)            ! Col no's for nonzero terms in matrix A
      INTEGER(LONG)   , INTENT(IN)    :: J_B(NTERM_B)            ! Col no's for nonzero terms in matrix B
      INTEGER(LONG)   , INTENT(OUT)   :: I_C(NROWS+1)            ! I_C(I+1) - I_C(I) = no. terms in row I of matrix C
      INTEGER(LONG)   , INTENT(OUT)   :: J_C(NTERM_A+NTERM_B)    ! Col no's for nonzero terms in matrix C
      INTEGER(LONG)                   :: I,J                     ! DO loop indices
      INTEGER(LONG)                   :: K_A,K_B,K_C,L           ! Counters
      INTEGER(LONG)                   :: K_C_BEG_ROW             ! Beginning count for terms in C starting in a row
      INTEGER(LONG)                   :: J_C_ROW(NTERM_A+NTERM_B)! Col no's from one row of C (can't be any more in 1 row than this)
      INTEGER(LONG)                   :: NUM_IN_ROW_OF_A         ! Num terms in a row of A matrix
      INTEGER(LONG)                   :: NUM_IN_ROW_OF_B         ! Num terms in a row of B matrix
      INTEGER(LONG)                   :: NUM_IN_ROW_OF_C         ! Num terms in a row of C matrix
      INTEGER(LONG)   , PARAMETER     :: SUBR_BEGEND = MERGE_MAT_COLS_SSS_BEGEND

      REAL(DOUBLE)    , INTENT(IN)    :: A(NTERM_A)              ! Nonzero terms in matrix A
      REAL(DOUBLE)    , INTENT(IN)    :: B(NTERM_B)              ! Nonzero terms in matrix B
      REAL(DOUBLE)    , INTENT(OUT)   :: C(NTERM_A+NTERM_B)      ! Nonzero terms in matrix C
      REAL(DOUBLE)                    :: C_ROW(NTERM_A+NTERM_B)  ! Real values of C in 1 row (can't be any more in 1 row than this)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Make sure that input matrices A and B, and output matrix C, are stored in same format

      IF ((SYM_A /= SYM_B) .OR. (SYM_A /= SYM_C) .OR. (SYM_B /= SYM_C)) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,942) SUBR_NAME, MAT_A_NAME, MAT_B_NAME, MAT_C_NAME, SYM_A, SYM_B, SYM_C
         WRITE(F06,942) SUBR_NAME, MAT_A_NAME, MAT_B_NAME, MAT_C_NAME, SYM_A, SYM_B, SYM_C
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! Initialize

      IF (DEBUG(102) > 0) CALL MERGE_MAT_COLS_SSS_DEB ( '0' )
      DO I=1,NROWS+1
         I_C(I) = 0
      ENDDO
      DO I= 1,NTERM_A+NTERM_B
         J_C(I) = 0
           C(I) = ZERO
      ENDDO

      I_C(1) = 1
      K_A    = 0
      K_B    = 0
      K_C    = 0

      DO I=1,NROWS

         L = 0

         NUM_IN_ROW_OF_A = I_A(I+1) - I_A(I)
         NUM_IN_ROW_OF_B = I_B(I+1) - I_B(I)
         NUM_IN_ROW_OF_C = NUM_IN_ROW_OF_A + NUM_IN_ROW_OF_B

         I_C(I+1) = I_C(I) + NUM_IN_ROW_OF_C

         IF (K_A+NUM_IN_ROW_OF_A > NTERM_A)         CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, NTERM_A, MAT_A_NAME )
         IF (L  +NUM_IN_ROW_OF_A > NTERM_A+NTERM_B) CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, NTERM_A+NTERM_B, MAT_C_NAME )
         DO J=1,NUM_IN_ROW_OF_A
            K_A = K_A + 1
            K_C = K_C + 1
            L   = L + 1
            J_C_ROW(L) = J_A(K_A)
              C_ROW(L) =   A(K_A)
         ENDDO

         IF (K_B+NUM_IN_ROW_OF_B > NTERM_B)         CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, NTERM_B, MAT_B_NAME )
         IF (L  +NUM_IN_ROW_OF_B > NTERM_A+NTERM_B) CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, NTERM_A+NTERM_B, MAT_C_NAME )
         DO J=1,NUM_IN_ROW_OF_B
            K_B = K_B + 1
            K_C = K_C + 1
            L   = L + 1
            J_C_ROW(L) = NCOL_A + J_B(K_B)
              C_ROW(L) =   B(K_B)
         ENDDO

         K_C_BEG_ROW = K_C - NUM_IN_ROW_OF_C + 1

         IF (DEBUG(102) > 0) CALL MERGE_MAT_COLS_SSS_DEB ( '2' )

         CALL SORT_INT1_REAL1 ( SUBR_NAME, MAT_C_NAME, L, J_C_ROW, C_ROW )

         IF (K_C_BEG_ROW+L-1 > NTERM_A+NTERM_B)     CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, NTERM_A+NTERM_B, MAT_C_NAME )
         DO J=1,L
            J_C(K_C_BEG_ROW+J-1) = J_C_ROW(J)
              C(K_C_BEG_ROW+J-1) =   C_ROW(J)
            IF (DEBUG(102) > 0) CALL MERGE_MAT_COLS_SSS_DEB ( '3' )
         ENDDO

      ENDDO

      IF (DEBUG(102) > 0) CALL MERGE_MAT_COLS_SSS_DEB ( '9' )

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  942 FORMAT(' *ERROR   942: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INPUT MATRICES ',A,' AND ',A,' AND OUTPUT MATRIX ',A,' MUST ALL BE STORED IN THE SAME FORMAT'         &
                    ,/,14X,' (SYM MUST ALL BE "Y" OR "N") HOWEVER, THE 3 MATRICES ARE TAGGED AS "',A,'", AND "',A,'" AND "',A,'"')

! ##################################################################################################################################

      CONTAINS

! ##################################################################################################################################

      SUBROUTINE MERGE_MAT_COLS_SSS_DEB ( WHICH )

      USE PENTIUM_II_KIND, ONLY       :  BYTE

      CHARACTER( 1*BYTE), INTENT(IN)  :: WHICH             ! Decides what to print out for this call to this subr

! **********************************************************************************************************************************
      IF (WHICH == '0') THEN
         WRITE(F06,99870) MAT_A_NAME, MAT_B_NAME, MAT_C_NAME
      ENDIF

      IF (WHICH == '2') THEN
         WRITE(F06,99880) I
         WRITE(F06,99881) I, MAT_A_NAME, (J_A(J),A(J),J=I_A(I),I_A(I+1)-1)
         WRITE(F06,99882) I, MAT_B_NAME, (J_B(J),B(J),J=I_B(I),I_B(I+1)-1)
         WRITE(F06,99883) I, MAT_C_NAME, (J_C_ROW(J),C_ROW(J),J=1,L)
         WRITE(F06,*)
         WRITE(F06,*) ' K_C_BEG_ROW = ', K_C_BEG_ROW,'(INDEX IN C WHERE J_C_ROW BEGINS)'
         WRITE(F06,*)
         WRITE(F06,99884)
      ENDIF

      IF (WHICH == '3') THEN
         WRITE(F06,99885) J, L, K_C_BEG_ROW+J-1, J_C(K_C_BEG_ROW+J-1), C(K_C_BEG_ROW+J-1)
      ENDIF

      IF (WHICH == '9') THEN 
         WRITE(F06,99890)
      ENDIF

! **********************************************************************************************************************************
99870 format('  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////',&
             '/////////////////',/,'  Data from subr MERGE_MAT_COLS: merging matrices A =',a,' and B = ',a,' into matrix C = ',a,/)

99880 format('  *****************************************************************************************************************',&
             '*****************',/,'  Data for row ',i4,/,'  -----------------')

99881 format('  J_A    , A     for row ',i8,' of sparse input  matrix ',a10,': ',32767(i8,1es14.6))

99882 format('  J_B    , B     for row ',i8,' of sparse input  matrix ',a10,': ',32767(i8,1es14.6))

99883 format('  J_C_ROW, C_ROW for row ',i8,' of sparse output matrix ',a10,': ',32767(i8,1es14.6))

99890 format('  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////',&
             '/////////////////')

99884 format('        Data for row     1 of C with P = K_C_BEG_ROW+J-1  ',/,                                                       &
             '       ---------------------------------------------------',/,                                                       &
             '       J       L          P            J_C(P)     C(P)')

99885 format(i8,i8,i11,i17,1es14.6)

! **********************************************************************************************************************************

      END SUBROUTINE MERGE_MAT_COLS_SSS_DEB
!
      END SUBROUTINE MERGE_MAT_COLS_SSS
