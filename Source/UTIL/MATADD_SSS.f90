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
 
      SUBROUTINE MATADD_SSS ( NROWS, MAT_A_NAME, NTERM_A, I_A, J_A, A, ALPHA, MAT_B_NAME, NTERM_B, I_B, J_B, B, BETA,  &
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

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SPARSE_ALG_ARRAYS, ONLY     :  LOGICAL_VEC, REAL_VEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  MATADD_SSS_BEGEND
 
      USE MATADD_SSS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MATADD_SSS'
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_A_NAME        ! Name of matrix A
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_B_NAME        ! Name of matrix B
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_C_NAME        ! Name of matrix C
      CHARACTER( 2*BYTE)              :: ALG               ! Which algorithm is used in solving for the terms in a row of C

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
      INTEGER(LONG)                   :: I,J               ! DO loop indices or counters
      INTEGER(LONG)                   :: KTERM_C           ! Count of number of terms as they are entered into arrays J_C and C
      INTEGER(LONG)                   :: MAXIMAX_COL_NUM_A ! Highest col number in matrix A for any row
      INTEGER(LONG)                   :: MAXIMAX_COL_NUM_B ! Highest col number in matrix B for any row
      INTEGER(LONG)                   :: MAXIMAX_COL_NUM_C ! Highest col number in matrix C for any row
      INTEGER(LONG)                   :: MAX_COL_NUM_A     ! Highest col number in matrix A for one row
      INTEGER(LONG)                   :: MAX_COL_NUM_B     ! Highest col number in matrix B for one row
      INTEGER(LONG)                   :: MAX_COL_NUM_C     ! Highest col number in matrix C for one row
      INTEGER(LONG)                   :: MIN_COL_NUM_A     ! Lowest  col number in matrix A for one row
      INTEGER(LONG)                   :: MIN_COL_NUM_B     ! Lowest  col number in matrix B for one row
      INTEGER(LONG)                   :: MIN_COL_NUM_C     ! Lowest  col number in matrix C for one row
      INTEGER(LONG)                   :: NUM_A_ROW_I       ! Num terms in row I of A matrix
      INTEGER(LONG)                   :: NUM_B_ROW_I       ! Num terms in row I of B matrix
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MATADD_SSS_BEGEND
       
      REAL(DOUBLE) , INTENT(IN )      :: A(NTERM_A)        ! Nonzero terms in matrix A
      REAL(DOUBLE) , INTENT(IN )      :: B(NTERM_B)        ! Nonzero terms in matrix B
      REAL(DOUBLE) , INTENT(IN )      :: ALPHA             ! Scalar multiplier for matrix A
      REAL(DOUBLE) , INTENT(IN )      :: BETA              ! Scalar multiplier for matrix B
      REAL(DOUBLE) , INTENT(OUT)      :: C(NTERM_C)        ! Nonzero terms in matrix C

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      DO I=1,NROWS+1
         I_C(I) = 0
      ENDDO

      DO I=1,NTERM_C
         J_C(I) = 0
           C(I) = ZERO
      ENDDO

      IF ((DEBUG(81) == 2) .OR. (DEBUG(81) == 3)) CALL MATADD_SSS_DEB ( '1', '   ' )

! Determine the highest col number in arrays J_A and J_B

      MAXIMAX_COL_NUM_A = 0
      DO I=1,NTERM_A
         IF (J_A(I) > MAXIMAX_COL_NUM_A) THEN
            MAXIMAX_COL_NUM_A = J_A(I)
         ENDIF
      ENDDO 

      MAXIMAX_COL_NUM_B = 0
      DO I=1,NTERM_B
         IF (J_B(I) > MAXIMAX_COL_NUM_B) THEN
            MAXIMAX_COL_NUM_B = J_B(I)
         ENDIF
      ENDDO

      MAXIMAX_COL_NUM_C = MAX ( MAXIMAX_COL_NUM_A, MAXIMAX_COL_NUM_B )

! Allocate memory to array LOGICAL_VEC, REAL_VEC (will have as many terms as MAXIMAX_COL_NUM_C and will be initialized to .FALSE.)
! In the code below, terms in range MIN_COL_NUM_C to MAX_COL_NUM_C will get reset to .TRUE. if there will be a nonzero term
! in a column of C. The variables MIN_COL_NUM_C and MAX_COL_NUM_C get calculated in the DO loop below for each row of C.)

      CALL ALLOCATE_SPARSE_ALG ( 'LOGICAL_VEC', 1, MAXIMAX_COL_NUM_C, SUBR_NAME )
      CALL ALLOCATE_SPARSE_ALG (    'REAL_VEC', 1, MAXIMAX_COL_NUM_C, SUBR_NAME )

! Add matrices A and B to get matrix C (in sparse, compressed row storage, format)  

      I_C(1) = 1

      KTERM_C = 0
      DO I=1,NROWS                                                   ! Cycle over rows of A and B to find terms in matrix C

         NUM_A_ROW_I = I_A(I+1) - I_A(I)                             ! Number of nonzero terms in row I of matrix A
         NUM_B_ROW_I = I_B(I+1) - I_B(I)                             ! Number of nonzero terms in row I of matrix B

a_nor_b: IF ((NUM_A_ROW_I == 0) .AND. (NUM_B_ROW_I == 0)) THEN       ! This row of A and also of B is null, so C will have no terms
            ALG = 'NN'
            MIN_COL_NUM_A = 0
            MAX_COL_NUM_A = 0
            MIN_COL_NUM_B = 0
            MAX_COL_NUM_B = 0
            MIN_COL_NUM_C = 0
            MAX_COL_NUM_C = 0
            I_C(I+1) = I_C(I)
         ENDIF a_nor_b
  
a_no_b:  IF ((NUM_A_ROW_I /= 0) .AND. (NUM_B_ROW_I == 0)) THEN       ! This row of A is not null but row of B is null
            ALG = 'YN'
            MIN_COL_NUM_B = 0
            MAX_COL_NUM_B = 0
            MIN_COL_NUM_A = J_A(I_A(I))                       
            MAX_COL_NUM_A = J_A(I_A(I+1)-1)           
            MIN_COL_NUM_C = MIN_COL_NUM_A
            MAX_COL_NUM_C = MAX_COL_NUM_A
            IF (KTERM_C > NTERM_C) CALL ARRAY_SIZE_ERROR_1  ( SUBR_NAME, NTERM_C, MAT_C_NAME ) 
            I_C(I+1) = I_C(I) + NUM_A_ROW_I
            DO J=I_A(I),I_A(I+1)-1
               KTERM_C = KTERM_C + 1
               J_C(KTERM_C) = J_A(J)
                 C(KTERM_C) = ALPHA*A(J)
            ENDDO
         ENDIF a_no_b

b_no_a:  IF ((NUM_A_ROW_I == 0) .AND. (NUM_B_ROW_I /= 0)) THEN       ! This row of A is null but row of B is not null
            ALG = 'NY'
            MIN_COL_NUM_A = 0
            MAX_COL_NUM_A = 0
            MIN_COL_NUM_B = J_B(I_B(I))                       
            MAX_COL_NUM_B = J_B(I_B(I+1)-1)           
            MIN_COL_NUM_C = MIN_COL_NUM_B
            MAX_COL_NUM_C = MAX_COL_NUM_B
            IF (KTERM_C > NTERM_C) CALL ARRAY_SIZE_ERROR_1  ( SUBR_NAME, NTERM_C, MAT_C_NAME ) 
            I_C(I+1) = I_C(I) + NUM_B_ROW_I
            DO J=I_B(I),I_B(I+1)-1
               KTERM_C = KTERM_C + 1
               J_C(KTERM_C) = J_B(J)
                 C(KTERM_C) = BETA*B(J)
            ENDDO
         ENDIF b_no_a

a_and_b: IF ((NUM_A_ROW_I /= 0) .AND. (NUM_B_ROW_I /= 0)) THEN       ! This row of A and of B is not null.


            ALG = 'YY'
            MIN_COL_NUM_C = MAX (MAXIMAX_COL_NUM_A,MAXIMAX_COL_NUM_B)! For each row of the matrices, the following code finds the
            MAX_COL_NUM_C = 0                                        ! range of cols (MIN_COL_NUM_C to MAX_COL_NUM_C) over which 

            MIN_COL_NUM_A = J_A(I_A(I))
            MAX_COL_NUM_A = J_A(I_A(I+1)-1)

            MIN_COL_NUM_B = J_B(I_B(I))
            MAX_COL_NUM_B = J_B(I_B(I+1)-1)

            MIN_COL_NUM_C = MIN ( MIN_COL_NUM_A, MIN_COL_NUM_B )
            MAX_COL_NUM_C = MAX ( MAX_COL_NUM_A, MAX_COL_NUM_B )

            DO J=1,MAXIMAX_COL_NUM_C                                 ! Initialize LOGICAL_VEC, REAL_VEC before calc'ing them below
               LOGICAL_VEC(J) = .FALSE.
                  REAL_VEC(J) =  ZERO
            ENDDO

            DO J=I_A(I),I_A(I+1)-1
               LOGICAL_VEC(J_A(J)) = .TRUE.
               REAL_VEC(J_A(J)) = REAL_VEC(J_A(J)) + ALPHA*A(J)
            ENDDO 

            DO J=I_B(I),I_B(I+1)-1
               LOGICAL_VEC(J_B(J)) = .TRUE.
               REAL_VEC(J_B(J)) = REAL_VEC(J_B(J)) + BETA*B(J)
            ENDDO

            I_C(I+1) = I_C(I)                              ! Update I_C
            DO J=MIN_COL_NUM_C,MAX_COL_NUM_C
               IF (LOGICAL_VEC(J)) THEN
                  I_C(I+1) = I_C(I+1) + 1
               ENDIF
            ENDDO

            DO J=MIN_COL_NUM_C,MAX_COL_NUM_C
               IF (LOGICAL_VEC(J)) THEN
                  KTERM_C = KTERM_C + 1
                  IF (KTERM_C > NTERM_C) CALL ARRAY_SIZE_ERROR_1  ( SUBR_NAME, NTERM_C, MAT_C_NAME ) 
                  J_C(KTERM_C) = J
                    C(KTERM_C) = REAL_VEC(J)
               ENDIF
            ENDDO

         ENDIF a_and_b

         IF (ALG /= 'NN') THEN
            IF ((DEBUG(81) == 2) .OR. (DEBUG(81) == 3)) CALL MATADD_SSS_DEB ( '3', ALG )
         ENDIF

      ENDDO

      CALL DEALLOCATE_SPARSE_ALG ( 'LOGICAL_VEC' )
      CALL DEALLOCATE_SPARSE_ALG (    'REAL_VEC' )

      IF ((DEBUG(81) == 2) .OR. (DEBUG(81) == 3)) CALL MATADD_SSS_DEB ( '9', '   ' )

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF
 
      RETURN

! **********************************************************************************************************************************

! ##################################################################################################################################

      CONTAINS

! ##################################################################################################################################

      SUBROUTINE MATADD_SSS_DEB ( WHICH, ALG )

      CHARACTER(LEN=*), INTENT(IN)    :: ALG               ! Which algorithm is used
      CHARACTER( 1*BYTE)              :: WHICH             ! Decides what to print out for this call to this subr

      INTEGER(LONG)                   :: DELTA_NTERM_C     ! Number of terms in a row of matrix C
      INTEGER(LONG)                   :: JJ                ! Local DO loop index
      INTEGER(LONG)                   :: KK                ! Local counter

! **********************************************************************************************************************************
      IF      (WHICH == '1') THEN

         WRITE(F06,*)
         WRITE(F06,1011)
         WRITE(F06,1012)
         WRITE(F06,1013)
         WRITE(F06,1014) MAT_A_NAME, MAT_B_NAME, MAT_C_NAME
         WRITE(F06,1015) NROWS, NTERM_A, NROWS, NTERM_B, NTERM_C
         WRITE(F06,1016) ALPHA, BETA
         WRITE(F06,1017)
         WRITE(F06,*)

      ELSE IF (WHICH == '2') THEN

      ELSE IF (WHICH == '3') THEN

         WRITE(F06,1021)
         WRITE(F06,1022) I
         WRITE(F06,1024)
         WRITE(F06,*)

         DELTA_NTERM_C = I_C(I+1) - I_C(I)
         DO JJ=1,DELTA_NTERM_C
            KK = KTERM_C - DELTA_NTERM_C + JJ
            IF (JJ == 1) THEN
               WRITE(F06,1031) I, ALG, MIN_COL_NUM_A, MAX_COL_NUM_A, MIN_COL_NUM_B, MAX_COL_NUM_B, MIN_COL_NUM_C, MAX_COL_NUM_C,&
                               I_C(I), KK, J_C(KK), C(KK)
            ELSE
               WRITE(F06,1032) KK, J_C(KK), C(KK)
            ENDIF
         ENDDO
         WRITE(F06,*)

      ELSE IF (WHICH == '4') THEN

      ELSE IF (WHICH == '5') THEN

      ELSE IF (WHICH == '6') THEN

      ELSE IF (WHICH == '7') THEN

      ELSE IF (WHICH == '8') THEN

      ELSE IF (WHICH == '9') THEN

         WRITE(F06,*)
         WRITE(F06,1091) MAT_C_NAME
         DO I=1,NROWS+1                                    ! The number of rows in C is the same as that in A
            WRITE(F06,9192) I,I_C(I)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,1093)
         DO KK=1,NTERM_C
            WRITE(F06,1094) KK, J_C(KK), C(KK)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,1095)

      ENDIF

! **********************************************************************************************************************************
 1011 FORMAT(' __________________________________________________________________________________________________________________',&
             '_________________'                                                                                               ,//,&
             ' :::::::::::::::::::::::::::::::::::::::START DEBUG(81) OUTPUT FROM SUBROUTINE MATADD_SSS::::::::::::::::::::::::::',&
              ':::::::::::::::::',/)

 1012 FORMAT(' SSS SPARSE MATRIX ADD ROUTINE: Add sparse matrices A and B in Compressed Row Storage (CRS) format to obtain CRS',   &
' sparse matrix C.',/,' ------------------------------')

 1013 FORMAT(' A and B must be stored in the same format with regard to symmetry (if one is stored symmetric, with only terms on' ,&
' and above the',/,' diagonal, the other must be also stored as symmetric.',/)

 1014 FORMAT(42X,' The name of CRS formatted matrix A is: ',A                                                                   ,/,&
             42x,' The name of CRS formatted matrix B is: ',A                                                                   ,/,&
             42x,' The name of CRS formatted matrix C is: ',A,/)

 1015 FORMAT(30X,' Matrix A has ',I8,' rows and             '  ,I12,' nonzero terms'                                            ,/,&
             30X,' Matrix B has ',I8,' rows and             '  ,I12,' nonzero terms'                                            ,/,&
             30X,' Matrix C will have same number of rows and ',I12,' nonzero terms*'                                           ,/,&
             22X,'*(as detrmined by subr MATADD_SSS_NTERM which had to have been run prior to this subr)'/)

 1016 FORMAT(' Add ',1ES14.6,' times matrix A to ',1ES14.6,' times matrix B to obtain matrix C',/)

 1017 FORMAT(                                                                                                                      &
' Alg YN (below) is for the case where A has terms in the row being processed but B does not'                                   ,/,&
' Alg NY (below) is for the case where B has terms in the row being processed but A does not'                                   ,/,&
' Alg YY (below) is for the case where both A and B have nonzero terms in the row being processed'                             ,//,&
' Output is only given for non null rows of matrix C')

 1021 FORMAT(' ******************************************************************************************************************',&
              '*****************')

 1022 FORMAT(27X,' W O R K I N G   O N   R O W ',I8,'   O F   O U T P U T   M A T R I X   C',//)

 1024 FORMAT(20X,'Col Num Range For     Col Num Range For     Col Num Range For    Data For Nonzeros For This Row For Matrix C:',/,&
             20X,'Nonzero Terms In A    Nonzero Terms In B    Nonzero Terms In C   Row Start    Index  Col Nums        Values'  ,/,&
6X,'Row   Alg     Min Col   Max Col     Min Col   Max Col     Min Col   Max Col      I_C(I)         K    J_C(K)         C(K)'   ,/,&
            19X,'------------------    ------------------    ------------------    ---------------------------------------------')


 1031 FORMAT(1X,I8,4X,A2,  4X,I8,2X,I8,  4X,I8,2X,I8,  4X,I8,2X,I8,2X,  I10,I10,I10,1ES17.6)

 1032 FORMAT(93X,2I10,1ES17.6)

 1091 FORMAT(' ******************************************************************************************************************',&
              '*****************'                                                                                               ,/,&
             ' SUMMARY: Compressed Row Storage (CRS) format of matrix C = ',A,':',/,' -------'                                 ,//,&
             ' 1) Index, L, and array I_C(L) for matrix C, where I_C(L+1) - I_C(L) is the number of nonzero terms in row L of'    ,&
             ' matrix C.',/,'    (also, I_C(L) is the index, K, in array C(K) where row L begins - up to, but not including, the' ,&
             ' last entry in I_C(L)).',/)

 9192 FORMAT('    L, I_C(L)       = ',2I12)

 1093 FORMAT(' 2) Index, K, and arrays J_C(K) and C(K). C(K) are the nonzeros in matrix C and J_C(K) is the col number in matrix', &
             ' C for term C(K).',/)

 1094 FORMAT('    K, J_C(K), C(K) = ',2I12,1ES15.6)

 1095 FORMAT(' ::::::::::::::::::::::::::::::::::::::END DEBUG(81) OUTPUT FROM SUBROUTINE MATADD_SSS:::::::::::::::::::::::::::::',&
              ':::::::::::::::::'                                                                                               ,/,&
             ' __________________________________________________________________________________________________________________',&
             '_________________',/)

! **********************************************************************************************************************************

      END SUBROUTINE MATADD_SSS_DEB

      END SUBROUTINE MATADD_SSS
