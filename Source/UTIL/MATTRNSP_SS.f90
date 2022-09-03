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
 
      SUBROUTINE MATTRNSP_SS ( NROWA, NCOLA, NTERM, MAT_A_NAME, I_A, J_A, A, MAT_AT_NAME, I_AT, J_AT, AT )

! Transposes input matrix defined by arrays I_A, J_A, A (CRS sparse format) and puts result into output arrays
! I_AT, J_AT, AT (also in CRS sparse format) 
 
! Input matrix A is stored in compressed row storage (CRS) format using arrays I_A(NROWA+1), J_A(NTERM_A) A(NTERM_A) where NROWA
! is the number of rows in matrix A and NTERM_A are the number of nonzero terms in matrix A:

!      I_A is an array of NROWA+1 integers that is used to specify the number of nonzero terms in rows of matrix A. That is:
!          I_A(I+1) - I_A(I) are the number of nonzero terms in row I of matrix A

!      J_A is an integer array giving the col numbers of the NTERM_A nonzero terms in matrix A

!        A is a real array of the nonzero terms in matrix A.

! Output matrix AT is stored in compressed row storage (CRS) format using arrays I_AT(NCOLA+1), J_AT(NTERM_A) AT(NTERM_A).
! The number of nonzero terms in matrix AT is the same as in matrix A:

!      I_AT is an array of NCOLA+1 integers that is used to specify the number of nonzero terms in rows of matrix AT. That is:
!          I_AT(I+1) - I_AT(I) are the number of nonzero terms in row I of matrix AT

!      J_AT is an integer array giving the col numbers of the nonzero terms in matrix AT

!        AT is a real array of the nonzero terms in matrix AT (same values as in A but arranged differently).

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  MATTRNSP_SS_BEGEND
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      USE MATTRNSP_SS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MATTRNSP_SS'
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_A_NAME        ! Name of matrix to be transposed
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_AT_NAME       ! Name of matrix that is transposed
 
      INTEGER(LONG), INTENT(IN)       :: NCOLA             ! Number of cols in input matrix, A
      INTEGER(LONG), INTENT(IN)       :: NROWA             ! Number of rows in input matrix, A
      INTEGER(LONG), INTENT(IN)       :: NTERM             ! Number of nonzero terms in input matrix, A
      INTEGER(LONG), INTENT(IN)       :: I_A(NROWA+1)      ! I_A(I+1) - I_A(I) are the number of nonzeros in A row I
      INTEGER(LONG), INTENT(IN)       :: J_A(NTERM)        ! Col numbers for nonzero terms in A
      INTEGER(LONG), INTENT(OUT)      :: I_AT(NCOLA+1)     ! I_AT(I+1) - I_AT(I) are the num of nonzeros in AT row I
      INTEGER(LONG), INTENT(OUT)      :: J_AT(NTERM)       ! Col numbers for nonzero terms in AT
      INTEGER(LONG)                   :: I,J,K,L           ! DO loop indices or counters
      INTEGER(LONG)                   :: ISTART            ! Starting value of I when looking for row number of a term in MATIN
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MATTRNSP_SS_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: A(NTERM)          ! Real nonzero values in input  matrix A
      REAL(DOUBLE) , INTENT(OUT)      :: AT(NTERM)         ! Real nonzero values in output matrix AT

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      DO I=1,NCOLA+1
         I_AT(I) = 0
      ENDDO

      DO I=1,NTERM
         J_AT(I) = 0
           AT(I) = ZERO
      ENDDO

      IF ((DEBUG(85) == 1) .OR. (DEBUG(85) == 3)) CALL MATTRNSP_SS_DEB ( '1', '   ' )

      L = 0                                                ! Counter for terms going into AT
      I_AT(1) = 1
      DO J=1,NCOLA
         I_AT(J+1) = I_AT(J)
         ISTART  = 1
         DO K=1,NTERM
            IF (J_A(K) == J) THEN                          ! We found a term that belongs in col J
               I_AT(J+1) = I_AT(J+1) + 1
i_do:          DO I=ISTART,NROWA                           ! Find out what the row number is for this term
                  IF ((K >= I_A(I)) .AND. (K < I_A(I+1))) THEN
                     L       = L + 1
                     J_AT(L) = I                           ! J_AT has row numbers of the NTERM terms going into AT
                       AT(L) = A(K)
                     ISTART = I                            ! Reset I loop start point. Next term in this col will be higher row num.
                     EXIT i_do                             ! We found row number so continue with K loop
                  ENDIF
               ENDDO i_do
            ENDIF
         ENDDO
      ENDDO

      IF ((DEBUG(85) == 1) .OR. (DEBUG(85) == 3)) CALL MATTRNSP_SS_DEB ( '2', '   ' )

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! ##################################################################################################################################

      CONTAINS

! ##################################################################################################################################

      SUBROUTINE MATTRNSP_SS_DEB ( WHICH, ALG )

      CHARACTER(LEN=*), INTENT(IN)    :: ALG                    ! Which algorithm is used (#1 for terms above diag when SYM_A='Y'
!                                                                 or #2 for terms in row from diag out)
      CHARACTER( 1*BYTE)              :: WHICH                  ! Decides what to print out for this call to this subr

      INTEGER(LONG)                   :: II                     ! Local DO loop index

! **********************************************************************************************************************************
      IF      (WHICH == '1') THEN

         WRITE(F06,*)
         WRITE(F06,1011)
         WRITE(F06,1012)
         WRITE(F06,1014) MAT_A_NAME, MAT_AT_NAME
         WRITE(F06,1016) NROWA, NCOLA, NTERM
         WRITE(F06,*)

         WRITE(F06,2800) 
         WRITE(F06,*)
         WRITE(F06,3001)
         DO II=1,NROWA+1
            WRITE(F06,3002) II,I_A(II)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,3003)
         DO II=1,NTERM
            WRITE(F06,3004) II, J_A(II), A(II)
         ENDDO
         WRITE(F06,*)

      ELSE IF (WHICH == '2') THEN

         WRITE(F06,2800) 
         WRITE(F06,3020) MAT_AT_NAME, NCOLA, NROWA, NTERM
         WRITE(F06,*)
         WRITE(F06,3021)
         DO II=1,NCOLA+1
            WRITE(F06,3022) II,I_AT(II)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,3023)
         DO II=1,NTERM
            WRITE(F06,3024) II, J_AT(II), AT(II)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,1095)

      ENDIF

! **********************************************************************************************************************************
 1011 FORMAT(' __________________________________________________________________________________________________________________',&
             '_________________'                                                                                               ,//,&
             ' :::::::::::::::::::::::::::::::::::::::START DEBUG(85) OUTPUT FROM SUBROUTINE MATTRNSP_SS:::::::::::::::::::::::::' &
             ,':::::::::::::::::',/)

 1012 FORMAT(' SSS SPARSE MATRIX TRANSPOSITION ROUTINE: Transpose matrix A, stored in sparse Compressed Row Storage (CRS) format,',&
' to matrix AT,',/,' ----------------------------------------',/,' also stored in sparse CRS format.',/)

 1014 FORMAT(40X,' The name of CRS formatted matrix A  is: ',A                                                                  ,/,&
             40x,' The name of CRS formatted matrix AT is: ',A,/)

 1016 FORMAT(26X,' Matrix A  has ',I8,' rows and ',I8,' cols and '  ,I12,' nonzero terms',/)

 2800 FORMAT(1X,'****************************************************************************************************************',&
                '*******************')

 3001 FORMAT(' Compressed Row Storage (CRS) format of input matrix A:'                                                          ,/,&
             ' ------------------------------------------------------'                                                         ,//,&
             ' 1) Index, L, and array I_A(L) for matrix A, where I_A(L+1) - I_A(L) is the number of nonzero terms in row L of',    &
             ' matrix A.',/,'    (also, I_A(L) is the index, K, in array A(K) where row L begins - up to, but not including, the', &
             ' last entry in I_A(L)).',/)

 3002 FORMAT('    L, I_A(L)       = ',2I12)

 3003 FORMAT(' 2) Index, K, and arrays J_A(K) and A(K). A(K) are the nonzeros in matrix A and J_A(K) is the col number in matrix A'&
            ,' for term A(K).',/)

 3004 FORMAT('    K, J_A(K), A(K) = ',2I12,1ES15.6)

 3020 FORMAT(' Output matrix AT = ',A,' has ',I8,' rows and ',I8,' cols with ',I8,' nonzero terms')

 3021 FORMAT(' Compressed Row Storage (CRS) format of output matrix AT:'                                                        ,/,&
             ' --------------------------------------------------------'                                                       ,//,&
             ' 1) Index, L, and array I_AT(L) for matrix AT, where I_AT(L+1) - I_AT(L) is the number of nonzero terms in row L of',&
             ' matrix AT.',/,'    (also, I_AT(L) is the index, K, in array AT(K) where row L begins - up to, but not including,',  &
             ' the last entry in I_AT(L)).',/)

 3022 FORMAT('    L, I_C(L)       = ',2I12)

 3023 FORMAT(' 2) Index, K, and arrays J_AT(K) and AT(K). AT(K) are the nonzeros in matrix AT and J_AT(K) is the col number in',   &
             '  matrix ATfor term AT(K).',/)

 3024 FORMAT('    K, J_AT(K), AT(K) = ',2I12,1ES15.6)

 1095 FORMAT(' ::::::::::::::::::::::::::::::::::::::END DEBUG(85) OUTPUT FROM SUBROUTINE MATTRNSP_SS::::::::::::::::::::::::::::',&
              ':::::::::::::::::'                                                                                               ,/,&
             ' ___________________________________________________________________________________________________________________'&
            ,'________________',/)

! **********************************************************************************************************************************

      END SUBROUTINE MATTRNSP_SS_DEB

      END SUBROUTINE MATTRNSP_SS
