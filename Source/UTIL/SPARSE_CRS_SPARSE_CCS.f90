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
 
      SUBROUTINE SPARSE_CRS_SPARSE_CCS ( NROWS_A, NCOLS_A, NTERMS_A, MAT_A_NAME, I_A, J_A, A, MAT_B_NAME, J_B, I_B, B, WRT_SCREEN )

! Converts matrices in sparse compressed row storage (CRS) format to sparse compressed column storage (CCS) format
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  SPARSE_CRS_SPARSE_CCS_BEGEND
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      USE SPARSE_CRS_SPARSE_CCS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SPARSE_CRS_SPARSE_CCS'
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_A_NAME        ! Name of input  matrix in CRS format
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_B_NAME        ! Name of output matrix in CCS format
      CHARACTER(LEN=*), INTENT(IN)    :: WRT_SCREEN        ! If 'Y' then write msgs to screen
 
      INTEGER(LONG), INTENT(IN)       :: NCOLS_A           ! Number of cols in input matrix, A (and output matrix B)
      INTEGER(LONG), INTENT(IN)       :: NROWS_A           ! Number of rows in input matrix, A (and output matrix B)
      INTEGER(LONG), INTENT(IN)       :: NTERMS_A          ! Number of nonzero terms in input matrix, A (and output matrix B)
      INTEGER(LONG), INTENT(IN)       :: I_A(NROWS_A+1)    ! I_A(I+1) - I_A(I) are the number of nonzeros in A row I
      INTEGER(LONG), INTENT(IN)       :: J_A(NTERMS_A)     ! Col numbers for nonzero terms in A
      INTEGER(LONG), INTENT(OUT)      :: I_B(NTERMS_A)     ! Row numbers for nonzero terms in B
      INTEGER(LONG), INTENT(OUT)      :: J_B(NCOLS_A+1)    ! J_B(I+1) - J_B(I) are the number of nonzeros in B col I
      INTEGER(LONG)                   :: I,J,K,L           ! DO loop indices or counters
      INTEGER(LONG)                   :: I2_A(NTERMS_A)    ! Array of row numbers for each term in A
      INTEGER(LONG)                   :: COL_J_NUM_TERMS   ! Number of terms in col J of output matrix B
      INTEGER(LONG)                   :: ROW_I_NUM_TERMS   ! Number of terms in row I of input  matrix A
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SPARSE_CRS_SPARSE_CCS_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: A(NTERMS_A)       ! Real nonzero values in input  matrix A
      REAL(DOUBLE) , INTENT(OUT)      :: B(NTERMS_A)       ! Real nonzero values in output matrix B

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      DO I=1,NTERMS_A
         I_B(I) = 0
           B(I) = ZERO
      ENDDO

      DO I=1,NCOLS_A+1
         J_B(I) = 0
      ENDDO

      IF (WRT_SCREEN == 'Y') THEN
   !xx   WRITE(SC1, * )                                    ! Write blank line to screen
      ENDIF

      IF ((DEBUG(87) == 1) .OR. (DEBUG(87) == 3)) CALL CRS_CCS_DEB ( '1' )

! Create I2_A array of row numbers for terms in A. These can be deduced from I_A but we can eliminate an internal
! DO loop if we create I2_A from I_A

      K = 0
      DO I=1,NROWS_A
         ROW_I_NUM_TERMS = I_A(I+1) - I_A(I)
         DO J = 1,ROW_I_NUM_TERMS
            K = K + 1
            I2_A(K) = I
         ENDDO
      ENDDO

      L = 0                                                ! Counter for terms going into B
      J_B(1) = 1
      DO J=1,NCOLS_A
         IF (WRT_SCREEN == 'Y') THEN
            WRITE(SC1,12345,ADVANCE='NO') J, NCOLS_A, MAT_A_NAME, MAT_B_NAME, CR13
         ENDIF 
         COL_J_NUM_TERMS = 0 
         DO K=1,NTERMS_A
            IF (J_A(K) == J) THEN                          ! We found a term that belongs in col J
               COL_J_NUM_TERMS = COL_J_NUM_TERMS + 1       ! Update the number of terms counted that belong to this column
               L = L + 1
               IF (L > NTERMS_A) CALL ARRAY_SIZE_ERROR_1( SUBR_NAME, NTERMS_A, MAT_B_NAME )
               I_B(L) = I2_A(K)                            ! Array I_B has row numbers of the NTERMS_A terms going into B
                 B(L) = A(K)
            ENDIF
         ENDDO
         J_B(J+1) = J_B(J) + COL_J_NUM_TERMS               ! J_B used to tell how many terms there are in each col of B
      ENDDO
      WRITE(SC1,*) CR13

      IF ((DEBUG(87) == 1) .OR. (DEBUG(87) == 3)) CALL CRS_CCS_DEB ( '2' )

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
12345 FORMAT(7X,'Extracting col  ',I8,' of ',I8,' from matrix ',A,' for matrix ',A,A)

! **********************************************************************************************************************************
 
! ##################################################################################################################################

      CONTAINS

! ##################################################################################################################################

      SUBROUTINE CRS_CCS_DEB ( WHICH )

      CHARACTER( 1*BYTE)              :: WHICH                  ! Decides what to print out for this call to this subr

      INTEGER(LONG)                   :: II                     ! Local DO loop index
      INTEGER(LONG)                   :: NCOLS_B                ! Number of cols in matrix B
      INTEGER(LONG)                   :: NTERMS_B               ! Number of nonzero terms in matrix B

! **********************************************************************************************************************************
      IF      (WHICH == '1') THEN

         WRITE(F06,*)
         WRITE(F06,1011)
         WRITE(F06,1012)
         WRITE(F06,1014) MAT_A_NAME, MAT_B_NAME
         WRITE(F06,1016) NROWS_A, NCOLS_A, NTERMS_A
         WRITE(F06,*)

         WRITE(F06,2800) 
         WRITE(F06,*)
         WRITE(F06,3001)
         DO II=1,NROWS_A+1
            WRITE(F06,3002) II,I_A(II)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,3003)
         DO II=1,NTERMS_A
            WRITE(F06,3004) II, J_A(II), A(II)
         ENDDO
         WRITE(F06,*)

      ELSE IF (WHICH == '2') THEN

         NCOLS_B  = NCOLS_A
         NTERMS_B = NTERMS_A

         WRITE(F06,2800) 
         WRITE(F06,*)
         WRITE(F06,3021)
         DO II=1,NCOLS_B+1
            WRITE(F06,3022) II,J_B(II)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,3023)
         DO II=1,NTERMS_B
            WRITE(F06,3024) II, I_B(II), B(II)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,1095)

      ENDIF

! **********************************************************************************************************************************
 1011 FORMAT(' __________________________________________________________________________________________________________________',&
             '_________________'                                                                                               ,//,&
             ' :::::::::::::::::::::::::::::::::::::::::START DEBUG(87) OUTPUT FROM SUBROUTINE CRS_CCS:::::::::::::::::::::::'    ,&
              ':::::::::::::::::::::',/)

 1012 FORMAT(' SPARSE MATRIX CRS TO CCS CONVERSION ROUTINE: Convert input matrix A, stored in sparse Compressed Row Storage'      ,&
' (CRS) format, to',/,' -------------------------------------------',/,&
' output matrix B, stored in sparse Compressed Column (CCS) format.',/)

 1014 FORMAT(40X,' The name of CRS formatted matrix A is: ',A                                                                   ,/,&
             40x,' The name of CCS formatted matrix B is: ',A,/)

 1016 FORMAT(26X,' Matrix A  has ',I8,' rows and ',I8,' cols and '  ,I12,' nonzero terms',/)

 2800 FORMAT(1X,'****************************************************************************************************************',&
                '*******************')

 3001 FORMAT(' Compressed Row Storage (CRS) format of input matrix A:'                                                          ,/,&
             ' ------------------------------------------------------'                                                         ,//,&
             ' 1) Index, L, and array I_A(L) for matrix A, where I_A(L+1) - I_A(L) is the number of nonzero terms in row L of'    ,&
             ' matrix A.',/,'    (also, I_A(L) is the index, K, in array A(K) where row L begins - up to, but not including, the' ,&
             ' last entry in I_A(L)).',/)

 3002 FORMAT('    L, I_A(L)       = ',2I12)

 3003 FORMAT(' 2) Index, K, and arrays J_A(K) and A(K). A(K) are the nonzeros in matrix A and J_A(K) is the col number in matrix', &
             ' A for term A(K).',/)

 3004 FORMAT('    K, J_A(K), A(K) = ',2I12,1ES15.6)

 3021 FORMAT(' Compressed Col Storage (CCS) format of output matrix B:'                                                         ,/,&
             ' -------------------------------------------------------'                                                        ,//,&
             ' 1) Index, L, and array J_B(L) for matrix B, where J_B(L+1) - J_B(L) is the number of nonzero terms in row L of'    ,&
             ' matrix B.',/,'    (also, J_B(L) is the index, K in array B(K), where row L begins - up to, but not including, the' ,&
             ' last entry in J_B(L)).',/)

 3022 FORMAT('    L, J_B(L)       = ',2I12)

 3023 FORMAT(' 2) Index, K, and arrays I_B(K) and B(K). B(K) are the nonzeros in matrix B and I_B(K) is the col number in matrix', &
             ' B for term B(K).',/)

 3024 FORMAT('    K, I_B(K), B(K) = ',2I12,1ES15.6)

 1095 FORMAT(' ::::::::::::::::::::::::::::::::::::::::END DEBUG(87) OUTPUT FROM SUBROUTINE CRS_CCS::::::::::::::::::::::::::',    &
              ':::::::::::::::::::::'                                                                                           ,/,&
             ' __________________________________________________________________________________________________________________',&
             '_________________',/)

! **********************************************************************************************************************************

      END SUBROUTINE CRS_CCS_DEB

      END SUBROUTINE SPARSE_CRS_SPARSE_CCS
