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
 
      SUBROUTINE PARTITION_SS ( MAT_A_NAME, NTERM_A, NROW_A, NCOL_A, SYM_A, I_A, J_A, A                                            &
                              , ROW_PART_VEC, COL_PART_VEC, VAL_ROWS, VAL_COLS, AROW_MAX_TERMS                                     &
                              , MAT_B_NAME, NTERM_B, NROW_B, SYM_B, I_B, J_B, B )                
 
!///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
! Subroutine PARTITION_SSS_NTERM must be run before this subroutine to calculate NTERM_B and AROW_MAX_TERMS inputs to this
! subroutine. Then memory can be allocated to C before this subroutine is called
!///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

! Partitions matrices. An input matrix (A) in sparse CRS (compressed row storage) format represented by:
 
!             I_A(i) integer array of indices that give the starting locations, in A, where each row begins
!             J_A(i) integer array of col numbers of nonzero terms, and
!               A(i) real array of nonzero terms (i=1 to NTERM_A) in the input matrix

! whose size is NROW_A x NCOL_A is partitioned using partitioning vectors:

!                         ROW_PART_VEC(i) - a row partitioning vector (i=1 to NROW_A containing VAL_ROWS = 1's and 2's) and
!                         COL_PART_VEC(i) - a col partitioning vector (i=1 to NROW_A containing VAL_COLS = 1's and 2's)

! into one output matrix in sparse format:

! The rows for which ROW_PART_VEC(i) = VAL_ROWS and the cols for which COL_PART_VEC(i) = VAL_COLS are the ones to be partitioned.
! The outout from this subr are the arrays representing the partitioned matrix (B) in sparse CRS format:

!                         I_B(i) integer array of indices that give the starting locations, in B, where each row begins
!                         J_B(i) integer array of col numbers of nonzero terms, and
!                           B(i) real array of nonzero terms (i=1 to NTERM_B) in the partitioned matrix 

! This subr can handle sparse CRS input matrices which are stored as symmetric (i.e. only nonzero terms on and above the diagonal)
! as well as sparse CRS matrices in which all nonzero terms are stored. If the inpyt matrix is stored as symmetric then SYM_A = 'Y'
! In addition, the matrix to be partitioned can (when SYM_A = 'Y') be partitioned as a symmetric matrix or as a
! matrix with all nonzero terms in it. The input variable SYM_B controls this option. If SYM_B = 'Y' then the output matrix will
! be stored as symmetric (only terms on, and above, the diagonal are in B). If SYM_B /= 'Y', then all nonzero terms from
! A for the rows and columns to be partitioned will be stored in B


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  PARTITION_SS_BEGEND
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      USE PARTITION_SS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'PARTITION_SS'
      CHARACTER(LEN=*), INTENT(IN )   :: SYM_A                  ! 'Y' if input matrix is symmetric (and terms below diag
                                                                !     are not included in A)
      CHARACTER(LEN=*), INTENT(IN )   :: SYM_B                  ! 'Y' if all terms (incl below diag) are to be output for
                                                                !     output matrix B (only use when SYM_A = 'Y')
      CHARACTER(LEN=*), INTENT(IN )   :: MAT_B_NAME             ! Name of partitioned output matrix
      CHARACTER(LEN=*), INTENT(IN )   :: MAT_A_NAME             ! Name of input matrix to be partitioned
      CHARACTER( 1*BYTE)              :: J_AROW_NULL            ! = 'Y' if array J_AROW is null when a row of A is processed
      INTEGER(LONG), INTENT(IN)       :: AROW_MAX_TERMS         ! Max number of terms in any row of A
      CHARACTER( 3*BYTE)              :: ALG(AROW_MAX_TERMS)    ! Which algorithm is used (#2 for terms above diag when SYM_A='Y'
!                                                                 or #1 for terms in row from diag out)

      INTEGER(LONG), INTENT(IN )      :: NCOL_A                 ! No. cols in A
      INTEGER(LONG), INTENT(IN )      :: NROW_A                 ! No. rows in A
      INTEGER(LONG), INTENT(IN )      :: NROW_B                 ! No. rows in B
      INTEGER(LONG), INTENT(IN )      :: NTERM_A                ! No. terms in A
      INTEGER(LONG), INTENT(IN )      :: NTERM_B                ! No. terms that going into B (see subr PARTITION_SS_SIZE)
      INTEGER(LONG), INTENT(IN )      :: COL_PART_VEC(NCOL_A)   ! Col partitioning vector (1's and 2's)
      INTEGER(LONG), INTENT(IN )      :: I_A(NROW_A+1)          ! Starting locations in A for each row
      INTEGER(LONG), INTENT(IN )      :: J_A(NTERM_A)           ! Col number for each A input matrix term
      INTEGER(LONG), INTENT(IN )      :: ROW_PART_VEC(NROW_A)   ! Row partitioning vector (1's and 2's)
      INTEGER(LONG), INTENT(IN )      :: VAL_ROWS               ! Value in ROW_PART_VEC to look for for partitioning rows  
      INTEGER(LONG), INTENT(IN )      :: VAL_COLS               ! Value in COL_PART_VEC to look for for partitioning cols
      INTEGER(LONG), INTENT(OUT)      :: I_B(NROW_B+1)          ! Starting locations in B for each row
      INTEGER(LONG), INTENT(OUT)      :: J_B(NTERM_B)           ! Col number for each B output matrix term
      INTEGER(LONG)                   :: A_NTERM_ROW_I          ! Number of terms in A row I
      INTEGER(LONG)                   :: B_COL_NUM_ARRAY(NCOL_A)! Col number for terms where COL_PART_VEC = VAL_COLS
!                                                                 e.g., if COL_PART_VEC = 1 1 0 1 0 1 0 0, VAL_COLS = 1,
!                                                                 then:       B_COL_NUM_ARRAY = 1 2 0 3 0 4 0 0
!                                                                 It is a counter for the VAL_COLS terms in COL_PART_VEC 
      INTEGER(LONG)                   :: B_COL_NUM              ! A col number value from B_COL_NUM_ARRAY 
      INTEGER(LONG)                   :: B_ROW_NUM_ARRAY(NROW_A)! Row number for terms where ROW_PART_VEC = VAL_ROWS 
      INTEGER(LONG)                   :: B_ROW_NUM              ! A row number value from B_ROW_NUM_ARRAY 
      INTEGER(LONG)                   :: I,II,K,L               ! DO loop indices or counters 
      INTEGER(LONG)                   :: I1,I2                  ! DO loop range
      INTEGER(LONG)                   :: IERROR                 ! Error indicator
      INTEGER(LONG)                   :: J_AROW(AROW_MAX_TERMS) ! Col numbers of terms in real array AROW (see below)
      INTEGER(LONG)                   :: KBEG_A                 ! Index into array I_A where a row of matrix A ends
      INTEGER(LONG)                   :: KEND_A                 ! Index into array I_A where a row of matrix A ends
      INTEGER(LONG)                   :: KTERM_B                ! Counter for terms as they are put into B
      INTEGER(LONG)                   :: KTERM_AROW             ! Count of number of terms in a row of A (need for SYM='Y'
      INTEGER(LONG)                   :: NCOL_B                 ! Number of columns in the output matrix
      INTEGER(LONG)                   :: ROW_AT_COLJ_BEG(NCOL_A)! jth term is row number in A where col j nonzeros begin 
      INTEGER(LONG)                   :: ROW_AT_COLJ_END(NCOL_A)! jth term is row number in A where col j nonzeros end
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = PARTITION_SS_BEGEND
       
      REAL(DOUBLE) , INTENT(IN )      :: A(NTERM_A)             ! Input  matrix nonzero terms
      REAL(DOUBLE) , INTENT(OUT)      :: B(NTERM_B)             ! Output matrix nonzero terms
      REAL(DOUBLE)                    :: AROW(AROW_MAX_TERMS)   ! Array that will contain the nonzero terms from one row of A

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC,MAT_A_NAME,MAT_B_NAME
 9001    FORMAT(1X,A,' BEGN ',F10.3,' Input matrix is ',A,'. Partitioned output matrix is ',A)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      DO I=1,NROW_B+1
         I_B(I) = 0
      ENDDO

      DO I=1,NTERM_B
         J_B(I) = 0
           B(I) = ZERO
      ENDDO

! Check inputs for sensibility

      IF (SYM_A == 'Y') THEN
         IF (NROW_A /= NCOL_A) THEN                        ! SYM_A = 'Y' doesn't make sense unless input matrix is square
            WRITE(ERR, 918) SUBR_NAME, MAT_A_NAME, NROW_A, NCOL_A
            WRITE(F06, 918) SUBR_NAME, MAT_A_NAME, NROW_A, NCOL_A
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )
         ENDIF
      ENDIF

! Set B_ROW_NUM_ARRAY and B_COL_NUM_ARRAY arrays based on the partitioning vectors. Note we use NROW_A and NCOL_A for the size of
! arrays B_ROW_NUM_ARRAY and B_COL_NUM_ARRAY since we did not know thw row/col size of B when we had to dimension these arrays
! above. We do know that the row/col size for B is less than that for A so we are safe in using NROW_A and NCOL_A for the sizes.

      K = 0
      DO I=1,NROW_A
         B_ROW_NUM_ARRAY(I) = 0
         IF (ROW_PART_VEC(I) == VAL_ROWS) THEN
            K = K + 1
            B_ROW_NUM_ARRAY(I) = K
         ENDIF
      ENDDO

      K = 0
      DO I=1,NCOL_A
         B_COL_NUM_ARRAY(I) = 0
         IF (COL_PART_VEC(I) == VAL_COLS) THEN
            K = K + 1
            B_COL_NUM_ARRAY(I) = K
         ENDIF
      ENDDO
      NCOL_B = K

! Make sure output requested makes sense

      IF (SYM_B == 'Y') THEN

         IF (NROW_B /= NCOL_B) THEN                        ! SYM_B = 'Y' doesn't make sense unless outut matrix is square
            WRITE(ERR, 919) SUBR_NAME, MAT_B_NAME, NROW_B, NCOL_B
            WRITE(F06, 919) SUBR_NAME, MAT_B_NAME, NROW_B, NCOL_B
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )
         ENDIF

         IF (SYM_A == 'Y') THEN                            ! If input and output are SYM = 'Y'
            IF (NROW_B == NCOL_B) THEN                     ! and output is square then need B_ROW_NUM_ARRAY = B_COL_NUM_ARRAY
               IERROR = 0
               DO I=1,NROW_B
                  IF (B_ROW_NUM_ARRAY(I) == B_COL_NUM_ARRAY(I)) THEN
                     CYCLE
                  ELSE
                     IERROR = 1
                     EXIT
                  ENDIF
               ENDDO
               IF (IERROR /= 0) THEN
                  WRITE(ERR, 916) SUBR_NAME, MAT_B_NAME, MAT_A_NAME
                  WRITE(F06, 916) SUBR_NAME, MAT_B_NAME, MAT_A_NAME
                  FATAL_ERR = FATAL_ERR + 1
                  CALL OUTA_HERE ( 'Y' )
               ENDIF
            ENDIF
         ENDIF

      ENDIF

      IF ((DEBUG(86) == 1) .OR. (DEBUG(86) == 3)) CALL PARTITION_SS_DEB ( '1' )

! Create arrays that give the row numbers at which col j begins and ends. When SYM_A = 'Y' and SYM_B = 'N', we have to get
! terms for the B partition that are not explicitly in A. This is done by getting A terms in the column (above the
! diagonal of a row) as well as the explicit terms from A that are there from the diagonal out to the end of the row. The two
! arrays: ROW_AT_COLJ_BEG and ROW_AT_COLJ_END are used to aid in getting the terms in the column above the diagonal.
! ROW_AT_COLJ_BEG is an array that gives, for each col of A, the starting row number of nonzero terms in that column.  
! ROW_AT_COLJ_END is an array that gives, for each col of A, the ending   row number of nonzero terms in that column.
! The span: ROW_AT_COLJ_BEG to ROW_AT_COLJ_END is used when we search for terms in the columns.  
! We only need ROW_AT_COLJ_BEG and ROW_AT_COLJ_END when A is input symmetric and B is not to be output as symmetric  

      IF ((SYM_A == 'Y') .AND. (SYM_B == 'N')) THEN

         CALL ROW_AT_COLJ_BEGEND ( MAT_A_NAME, NROW_A, NROW_A, NTERM_A, I_A, J_A,                            &
                                   ROW_AT_COLJ_BEG, ROW_AT_COLJ_END )
      ENDIF

! Load AROW with all of the terms that would be in row I of A (including those that were not actually there due to SYM_A
! Do this only for the rows that will be partitioned, i.e. the rows for which ROW_PART_VEC = VAL_ROWS

      KTERM_B = 0
      L       = 0
      I_B(1)  = 1
      KBEG_A  = 1

i_do: DO I=1,NROW_A                                        ! Matrix partition loop. Range over the rows in A

         A_NTERM_ROW_I = I_A(I+1) - I_A(I)                 ! Number of terms in matrix A in row I 
         KEND_A = KBEG_A + A_NTERM_ROW_I - 1               ! KBEG_A to KEND_A is range of indices of terms in A row I

         IF (ROW_PART_VEC(I) == VAL_ROWS) THEN

            B_ROW_NUM = B_ROW_NUM_ARRAY(I)

            L = L + 1
            I_B(L+1) = I_B(L)                              ! Start out with next I_B equal to last.

                                                           ! Write message to screen
            CALL OURTIM
            WRITE(SC1,12345,ADVANCE='NO') MAT_B_NAME, MAT_A_NAME, L, NROW_B, SYM_A, SYM_B, HOUR, MINUTE, SEC, SFRAC, CR13
            DO K=1,AROW_MAX_TERMS                          ! Null J_AROW and AROW each time we begin a new row of A
               AROW(K)    = ZERO
               J_AROW(K)  = 0
               ALG(K)(1:) = ' '
            ENDDO 
            J_AROW_NULL = 'Y'                              ! Nothing in J_AROW at this point

            KTERM_AROW = 0

                                                           ! Build AROW for one row of matrix A
            IF ((SYM_A == 'Y').AND.(SYM_B == 'N')) THEN    ! 1st, if this SYM is true, get terms in col above the diag in row I

               DO K=1,KBEG_A-1 
                  IF (J_A(K) == I) THEN
                     KTERM_AROW         = KTERM_AROW + 1
                     AROW(KTERM_AROW)   = A(K)
                     I1 = ROW_AT_COLJ_BEG(I)
                     I2 = ROW_AT_COLJ_END(I)
                     DO II=I1,I2                           ! For terms whose row no. is I, det. their col no. and load into J_AROW
                        IF ((K >= I_A(II)) .AND. (K < I_A(II+1))) THEN
                           J_AROW(KTERM_AROW) = II
                           J_AROW_NULL = 'N'
                           ALG(KTERM_AROW) = ' #2'
                        ENDIF
                     ENDDO
                  ENDIF
               ENDDO

            ENDIF

            DO K=KBEG_A,KEND_A                             ! 2nd, always get terms from this row of A from the diagonal out
               KTERM_AROW = KTERM_AROW + 1
               AROW(KTERM_AROW)   = A(K)
               J_AROW(KTERM_AROW) = J_A(K)
               J_AROW_NULL = 'N'
               ALG(KTERM_AROW) = ' #1'
            ENDDO

            IF (J_AROW_NULL == 'N') THEN
               IF ((DEBUG(86) == 1) .OR. (DEBUG(86) == 3)) CALL PARTITION_SS_DEB ( '2' )
            ENDIF

            DO K=1,AROW_MAX_TERMS
               IF (J_AROW(K) == 0) THEN
                  EXIT
               ELSE
                  IF (COL_PART_VEC(J_AROW(K)) == VAL_COLS) THEN
                     B_COL_NUM    = B_COL_NUM_ARRAY(J_AROW(K))
                     KTERM_B      = KTERM_B  + 1
                     I_B(L+1)     = I_B(L+1) + 1
                     J_B(KTERM_B) = J_AROW(K)
                     J_B(KTERM_B) = B_COL_NUM_ARRAY(J_AROW(K))
                       B(KTERM_B) = AROW(K)
                     IF ((DEBUG(86) == 1) .OR. (DEBUG(86) == 3)) CALL PARTITION_SS_DEB ( '4' )
                  ELSE
                     IF ((DEBUG(86) == 1) .OR. (DEBUG(86) == 3)) CALL PARTITION_SS_DEB ( '5' )
                  ENDIF
               ENDIF
            ENDDO 

         ENDIF

         KBEG_A = KEND_A + 1

         IF ((DEBUG(86) == 1) .OR. (DEBUG(86) == 3)) CALL PARTITION_SS_DEB ( '6' )

      ENDDO i_do
      WRITE(SC1,*) CR13

      IF ((DEBUG(86) == 1) .OR. (DEBUG(86) == 3)) CALL PARTITION_SS_DEB ( '7' )

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF
 
      RETURN

! **********************************************************************************************************************************
  916 FORMAT(' *ERROR   916: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' OUTPUT MATRIX ',A,' WAS REQUESTED TO BE WRITTEN IN SYMMETRIC FORM'                                    &
                    ,/,14X,' HOWEVER, THE PARTITION IS NOT A SYMMETRIC ONE SINCE THE ROW NUMBERS PARTITIONED'                      &
                    ,/,14x,' ARE NOT THE SAME AS THE COLUMN NUMBERS PARTITIONED FROM INPUT MATRIX',A,' WHICH IS SYMMETRIC')

  918 FORMAT(' *ERROR   918: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INPUT MATRIX ',A,' WAS TAGGED AS SYMMETRIC BUT IT IS NOT SQUARE.'                                     &
                    ,/,14X,' IT HAS ',I8,' ROWS AND ',I8,' COLS')

  919 FORMAT(' *ERROR   919: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' OUTPUT MATRIX ',A,' WAS REQUESTED TO BE WRITTEN IN SYMMETRIC FORM BUT IT IS NOT SQUARE.'              &
                    ,/,14X,' IT HAS ',I8,' ROWS AND ',I8,' COLS')

12345 format(7X,'Part. ',A4,' from   ',A4,': row ',I8,' of ',I8,' SYM = ',A,A,4X,I2,':',I2,':',I2,'.',I3,A)







! **********************************************************************************************************************************

! ##################################################################################################################################

      CONTAINS

! ##################################################################################################################################

      SUBROUTINE PARTITION_SS_DEB ( WHICH )

      CHARACTER( 1*BYTE)              :: WHICH                  ! Decides what to print out for this call to this subr

! **********************************************************************************************************************************
      IF      (WHICH == '1') THEN

         WRITE(F06,*)
         WRITE(F06,1011)
         WRITE(F06,1012)
         WRITE(F06,1013)
         WRITE(F06,1014) MAT_A_NAME, MAT_B_NAME
         WRITE(F06,1015) NROW_A, NCOL_A, NTERM_A, NROW_B, NCOL_B, NTERM_B
         IF (SYM_A == 'Y') THEN
            WRITE(F06,1016)
         ELSE
            WRITE(F06,1017)
         ENDIF
         IF (SYM_B == 'Y') THEN
            WRITE(F06,10161)
         ELSE
            WRITE(F06,10171)
         ENDIF
         WRITE(F06,1018)
         WRITE(F06,*)


      ELSE IF (WHICH == '2') THEN

         WRITE(F06,1021)
         WRITE(F06,1022) I, B_ROW_NUM
         WRITE(F06,1023)

      ELSE IF (WHICH == '3') THEN

      ELSE IF (WHICH == '4') THEN

         WRITE(F06,1041) ALG(K), K, I, J_AROW(K), AROW(K), B_ROW_NUM, B_COL_NUM, B(KTERM_B)

      ELSE IF (WHICH == '5') THEN

         WRITE(F06,1051) ALG(K), K, I, J_AROW(K), AROW(K)

      ELSE IF (WHICH == '6') THEN

         WRITE(F06,*)

      ELSE IF (WHICH == '7') THEN

         WRITE(F06,*)
         WRITE(F06,1061) MAT_B_NAME
         DO I=1,NROW_B+1
            WRITE(F06,1062) I,I_B(I)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,1063)
         DO K=1,NTERM_B
            WRITE(F06,1064) K, J_B(K), B(K)
         ENDDO
         WRITE(F06,*)
         WRITE(F06,1065)

      ENDIF

! **********************************************************************************************************************************
 1011 FORMAT(' __________________________________________________________________________________________________________________',&
             '_________________'                                                                                               ,//,&
             ' :::::::::::::::::::::::::::::::::::::::START DEBUG(86) OUTPUT FROM SUBROUTINE PARTITION_SS::::::::::::::::::::::::',&
              ':::::::::::::::::',/)

 1012 FORMAT(' SPARSE MATRIX PARTITION ROUTINE: Partition matrix B, stored in Compressed Row Storage (CRS) format, from matrix A' ,&
' in CRS format',/,' --------------------------------',/)

 1013 FORMAT(' Matrices A and/or B can be stored as symmetric (only terms on and above the diagonal), or with all nonzero terms' ,&
' stored.',/)

 1014 FORMAT(40X,' The name of CRS formatted matrix A is: ',A                                                                   ,/,&
             40x,' The name of CRS formatted matrix B is: ',A,/)

 1015 FORMAT(22X,' Matrix A has ',I8,' rows and ',I8,' cols and has       ',I12,' nonzero terms'                                ,/,&
             22X,' Matrix B has ',I8,' rows and ',I8,' cols and will have ',I12,' nonzero terms*'                               ,/,&
             22X,'*(as detrmined by subr PARTITION_SS_NTERM which had to have been run prior to this subr)'/)

 1016 FORMAT(' Matrix A was input as a symmetric CRS array (only those terms on and above the diagonal are stored in array A)',/)

 1017 FORMAT(' Matrix A was input as a CRS array with all nonzero terms stored in array A',/)

10161 FORMAT(' Matrix B will be output as a symmetric CRS array (only those terms on and above the diag are stored in array B)',/)

10171 FORMAT(' Matrix B will be output as a CRS array with all nonzero terms stored in array A',/)

 1018 FORMAT(                                                                                                                      &
' In order to handle symmetric A matrices, which do not have all terms in a row (only those from the diagonal out), arrays AROW',/,&
' and J_AROW are used. AROW is a 1D array that contains all nonzero terms from one row of A (including those that are not'      ,/,&
' explicitly in array A due to symmetry). The partitioning of matrix B from matrix A is then accomplished (one row at a time)'  ,/,&
' by comparing the column numbers in J_AROW with the input partitioning vectors to extract terms from AROW and to insert them'  ,/,&
' into array B'                                                                                                                ,//,&
' Alg #1 (below) gets data for arrays J_AROW and AROW directly from the Compressed Row Storage (CRS) format of array A'         ,/,&
' Alg #2 (below) is only needed if matrix A is input as symmetric (only terms on and above the diagonal) and gets terms for'    ,/,&
'         J_AROW and AROW from column I of matrix A while working on row I of matrix A. These are the terms that would be below',/,&
'         the diag in matrix A but are not explicitly in the array due to symmetry storage'                                    ,//,&
' For each non null row of matrix A that is to be partitioned into matrix B, the following shows the development of arrays'     ,/,&
' J_AROW and AROW and the terms that are partitioned into output matrix B') 

 1021 FORMAT(' ******************************************************************************************************************',&
              '*****************')

 1022 FORMAT(' W O R K I N G   O N   R O W ',I8,'   O F   I N P U T   M A T R I X   A,   R O W   ',I8,'   O F   O U T P U T',      &
             '   M A T R I X   B',/)

 1023 FORMAT(15X,'Alg        Index               Data from matrix A                            Data for matrix B'               ,/,&
             15X,'---        -----       ----------------------------------            ----------------------------------'      ,/,&
             15X,'             K             Row       Col         Value                   Row       Col        Value'          ,/,&
             15X,'                               J_AROW(K)        AROW(K)',/)

 1041 FORMAT(15X,A,1X,I10,6X,2I10,1ES17.6,9X,2I10,1ES17.6)

 1051 FORMAT(15X,A,1X,I10,6X,2I10,1ES17.6)

 1061 FORMAT(' ******************************************************************************************************************',&
              '*****************'                                                                                               ,/,&
             ' SUMMARY: Compressed Row Storage (CRS) format of matrix B = ',A,':',/,' -------'                                 ,//,&
             ' 1) Index, L, and array I_B(L) for matrix B, where I_B(L+1) - I_B(L) is the number of nonzero terms in row L of'    ,&
             ' matrix B.',/,'    (also, I_B(L) is the index, K, in array B(K) where row L begins - up to, but not including, the' ,&
             ' last entry in I_B(L)).',/)

 1062 FORMAT('    L, I_B(L)       = ',2I12)

 1063 FORMAT(' 2) Index, K, and arrays J_B(K) and B(K). B(K) are the nonzeros in matrix B and J_B(K) is the col number in matrix', &
             ' B for term B(K).',/)

 1064 FORMAT('    K, J_B(K), B(K) = ',2I12,1ES15.6)

 1065 FORMAT(' ::::::::::::::::::::::::::::::::::::::::END DEBUG(86) OUTPUT FROM SUBROUTINE PARTITION_SS:::::::::::::::::::::::::',&
              ':::::::::::::::::'                                                                                               ,/,&
             ' __________________________________________________________________________________________________________________',&
             '_________________',/)







! **********************************************************************************************************************************

      END SUBROUTINE PARTITION_SS_DEB

      END SUBROUTINE PARTITION_SS
