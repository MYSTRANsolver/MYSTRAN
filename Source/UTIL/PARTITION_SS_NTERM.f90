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
 
      SUBROUTINE PARTITION_SS_NTERM ( MAT_A_NAME, NTERM_A, NROW_A, NCOL_A, SYM_A, I_A, J_A                                         &
                                    , ROW_PART_VEC, COL_PART_VEC, VAL_ROWS, VAL_COLS, AROW_MAX_TERMS                               &
                                    , MAT_B_NAME, NTERM_B, SYM_B )                
 
! Determines size of a partitioned matrix. An input matrix (MATIN) in sparse CRS (compressed row storage) format represented by:

!             I_A(i) integer array of indices that give the starting locations, in MATIN, where each row begins
!             J_A(i) integer array of col numbers of nonzero terms, and
!               MATIN(i) real array of nonzero terms (i=1 to NTERM_A) in the input matrix

! whose size is NROW_A xNCOL_A is to be partitioned (in subr PARTITION_SS after this subr) using partitioning vectors:

!                         ROW_PART_VEC(i) - a row partitioning vector (i=1 to NROW_A containing VAL_ROWS = 1's and 2's) and
!                         COL_PART_VEC(i) - a col partitioning vector (i=1 to NROW_A containing VAL_COLS = 1's and 2's)

! into one output matrix (MATOUT) in the same sparse format.

! The rows for which ROW_PART_VEC(i) = VAL_ROWS and the cols for which COL_PART_VEC(i) = VAL_COLS are the ones to be partitioned.

! This subroutine determines the number of nonzero terms that will be in that partition so memory can be allocated.

! This subr can handle sparse CRS input matrices which are stored as symmetric (i.e. only nonzero terms on and above the diagonal)
! as well as sparse CRS matrices in which all nonzero terms are stored. If the input matrix is stored as symmetric then SYM_A = 'Y'
! In addition, the matrix to be partitioned (in subr PARTITION_SS) can (when SYM_A = 'Y') be partitioned as a symmetric matrix or
! as amatrix with all nonzero terms in it. The input variable SYM_B controls this option. If SYM_B = 'Y' then the output matrix
! will be stored as symmetric (only terms on, and above, the diagonal are in MATOUT). If SYM_B /= 'Y', then all nonzero terms from
! MATIN for the rows and columns to be partitioned will be stored in MATOUT


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, TSEC
      USE SPARSE_ALG_ARRAYS, ONLY     :  ALG, J_AROW
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  PARTITION_SS_NTERM_BEGEND
 
      USE PARTITION_SS_NTERM_USE_IFs

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)      ! This causes a carriage return simulating the "+" action in a FORMAT
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'PARTITION_SS_NTERM'
      CHARACTER(LEN=*), INTENT(IN )   :: SYM_A                  ! 'Y' if input matrix is symmetric (and terms below diag
                                                                !     are not included in MATIN)
      CHARACTER(LEN=*), INTENT(IN )   :: SYM_B                  ! 'Y' if all terms (incl below diag) are to be output for
!                                                                  output matrix MATOUT (only use when SYM_A = 'Y')
      CHARACTER(LEN=*), INTENT(IN )   :: MAT_B_NAME             ! Name of partitioned output matrix
      CHARACTER(LEN=*), INTENT(IN )   :: MAT_A_NAME             ! Name of input matrix to be partitioned
      CHARACTER( 1*BYTE)              :: J_AROW_NULL            ! = 'Y' if array J_AROW is null when a row of A is processed

      INTEGER(LONG), INTENT(IN )      :: NTERM_A                ! No. terms in MATIN
      INTEGER(LONG), INTENT(IN )      :: NROW_A                 ! No. rows in MATIN
      INTEGER(LONG), INTENT(IN )      :: NCOL_A                 ! No. cols in MATIN
      INTEGER(LONG), INTENT(IN )      :: VAL_ROWS               ! Value in ROW_PART_VEC to look for for partitioning rows  
      INTEGER(LONG), INTENT(IN )      :: VAL_COLS               ! Value in COL_PART_VEC to look for for partitioning cols
      INTEGER(LONG), INTENT(IN )      :: I_A(NROW_A+1)          ! Starting locations in MATIN for each row
      INTEGER(LONG), INTENT(IN )      :: J_A(NTERM_A)           ! Col number for each MATIN input matrix term
      INTEGER(LONG), INTENT(IN )      :: ROW_PART_VEC(NROW_A)   ! Row partitioning vector (1's and 2's)
      INTEGER(LONG), INTENT(IN )      :: COL_PART_VEC(NCOL_A)   ! Col partitioning vector (1's and 2's)
      INTEGER(LONG), INTENT(OUT)      :: AROW_MAX_TERMS         ! Max number of terms in any row of A
      INTEGER(LONG), INTENT(OUT)      :: NTERM_B                ! No. terms that go into MATOUT (from subr PARTITION_SS_NTERM)
      INTEGER(LONG)                   :: B_COL_NUM_ARRAY(NCOL_A)! Col number for terms where COL_PART_VEC = VAL_COLS
!                                                                 e.g., if COL_PART_VEC = 1 1 0 1 0 1 0 0, VAL_COLS = 1,
!                                                                 then: B_COL_NUM_ARRAY = 1 2 0 3 0 4 0 0
!                                                                 It is a counter for the VAL_COLS terms in COL_PART_VEC 
      INTEGER(LONG)                   :: B_COL_NUM              ! A col number value from B_COL_NUM_ARRAY 
      INTEGER(LONG)                   :: B_ROW_NUM_ARRAY(NROW_A)! Row number for terms where ROW_PART_VEC = VAL_ROWS 
      INTEGER(LONG)                   :: B_ROW_NUM              ! A row number value from B_ROW_NUM_ARRAY 
      INTEGER(LONG)                   :: I,II,K,L               ! DO loop indices or counters 
      INTEGER(LONG)                   :: I1,I2                  ! DO loop range
      INTEGER(LONG)                   :: IERROR                 ! Error indicator
      INTEGER(LONG)                   :: KBEG_MATIN             ! Index into array I_A where a row of matrix A ends
      INTEGER(LONG)                   :: KEND_MATIN             ! Index into array I_A where a row of matrix A ends
      INTEGER(LONG)                   :: KTERM_AROW             ! Count of number of terms in a row of MATIN (need for SYM='Y'
      INTEGER(LONG)                   :: MATIN_NTERM_ROW_I      ! Number of terms in MATIN row I
      INTEGER(LONG)                   :: NCOL_B                 ! Number of columns in the output matrix
      INTEGER(LONG)                   :: NROW_B                 ! No. rows in B
      INTEGER(LONG)                   :: ROW_AT_COLJ_BEG(NCOL_A)! jth term is row number in MATIN where col j nonzeros begin 
      INTEGER(LONG)                   :: ROW_AT_COLJ_END(NCOL_A)! jth term is row number in MATIN where col j nonzeros end
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = PARTITION_SS_NTERM_BEGEND

      INTRINSIC                       :: DABS
       
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC,MAT_A_NAME,MAT_B_NAME
 9001    FORMAT(1X,A,' BEGN ',F10.3,' Input matrix is ',A,'. Determine memory to allocate to sparse arrays for partition ',A)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      AROW_MAX_TERMS = 0
      NTERM_B        = 0

!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages

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
      NROW_B = K 

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

      IF ((DEBUG(86) == 2) .OR. (DEBUG(86) == 3)) CALL PARTITION_SS_NTERM_DEB ( '1' )

! Create arrays that give the row numbers at which col j begins and ends. When SYM_A = 'Y' and SYM_B = 'N', we have to get
! terms for the MATOUT partition that are not explicitly in MATIN. This is done by getting MATIN terms in the column (above the
! diagonal of a row) as well as the explicit terms from MATIN that are there from the diagonal out to the end of the row. The two
! arrays: ROW_AT_COLJ_BEG and ROW_AT_COLJ_END are used to aid in getting the terms in the column above the diagonal.
! ROW_AT_COLJ_BEG is an array that gives, for each col of MATIN, the starting row number of nonzero terms in that column.  
! ROW_AT_COLJ_END is an array that gives, for each col of MATIN, the ending   row number of nonzero terms in that column.
! The span: ROW_AT_COLJ_BEG to ROW_AT_COLJ_END is used when we search for terms in the columns.
! We only need ROW_AT_COLJ_BEG and ROW_AT_COLJ_END when MATIN is input symmetric and MATOUT is not to be output as symmetric  

      IF ((SYM_A == 'Y') .AND. (SYM_B == 'N')) THEN

         CALL ROW_AT_COLJ_BEGEND ( MAT_A_NAME, NROW_A, NROW_A, NTERM_A, I_A, J_A,                                                  &
                                   ROW_AT_COLJ_BEG, ROW_AT_COLJ_END )
      ENDIF

! Set up array J_ROW so it can handle the row from MATIN that has the most terms in it. It will have col numbers for all of the
! terms in a row of MATIN, including those for terms that are not in MATIN due to symmetry (but only if SYM_B = 'Y' also)

      AROW_MAX_TERMS = 0                                   ! Find the max number of nonzero terms in any row of input matrix MATIN
      KBEG_MATIN     = 1
      DO I=1,NROW_A                                   ! Matrix partition loop. Range over the rows in MATIN
         KTERM_AROW = 0                                       
         MATIN_NTERM_ROW_I = I_A(I+1) - I_A(I)     ! Number of terms in matrix MATIN in row I 
         KEND_MATIN = KBEG_MATIN + MATIN_NTERM_ROW_I - 1   ! KBEG_MATIN to KEND_MATIN is range of indices of terms in MATIN row I
         IF (ROW_PART_VEC(I) == VAL_ROWS) THEN
            KTERM_AROW = 0
            IF ((SYM_A == 'Y') .AND. (SYM_B == 'N')) THEN
               KTERM_AROW = KTERM_AROW + ROW_AT_COLJ_END(I) - ROW_AT_COLJ_BEG(I) + 1
            ENDIF
            DO K=KBEG_MATIN,KEND_MATIN                     ! 2nd, get terms from this row of MATIN from the diagonal out
               KTERM_AROW = KTERM_AROW + 1
            ENDDO
         ENDIF
         KBEG_MATIN = KEND_MATIN + 1
         IF (KTERM_AROW > AROW_MAX_TERMS) THEN
            AROW_MAX_TERMS = KTERM_AROW
         ENDIF
      ENDDO
                                                           ! Allocate integer vector to hold the column no's of the terms in the row
      CALL ALLOCATE_SPARSE_ALG ( 'J_AROW', AROW_MAX_TERMS, 0, SUBR_NAME )
                                                           ! Allocate char array to hold the algorithm number used in the partition
      CALL ALLOCATE_SPARSE_ALG (    'ALG', AROW_MAX_TERMS, 0, SUBR_NAME )

! Load J_AROW with col numbers of all of the terms that would be in row I of MATIN (including those that were not actually there due
! to SYM_MATIN. Do this only for the rows that will be partitioned, i.e. the rows for which ROW_PART_VEC = VAL_ROWS.
! If SYM_A = 'Y' and SYM_B = 'Y', we only need to get terms from MATIN that are in row I (which are from diagonal out)
! If SYM_A = 'Y' and SYM_B = 'N', we need to also get all of the terms in the col above the diagonal in row I

      NTERM_B = 0
      KBEG_MATIN   = 1

      L = 0
i_do: DO I=1,NROW_A                                        ! Matrix partition loop. Range over the rows in MATIN

         MATIN_NTERM_ROW_I = I_A(I+1) - I_A(I)             ! Number of terms in matrix MATIN in row I 
         KEND_MATIN = KBEG_MATIN + MATIN_NTERM_ROW_I - 1   ! KBEG_MATIN to KEND_MATIN is range of indices of terms in MATIN row I

         IF (ROW_PART_VEC(I) == VAL_ROWS) THEN

            L = L + 1
                                                           ! Write message to screen
            CALL OURTIM
            WRITE(SC1,12345,ADVANCE='NO') MAT_B_NAME, L, NROW_B, SYM_A, SYM_B, HOUR, MINUTE, SEC, SFRAC, CR13

            B_ROW_NUM = B_ROW_NUM_ARRAY(I)

            DO K=1,AROW_MAX_TERMS                          ! Null J_AROW and AROW each time we begin a new row of A
               J_AROW(K) = 0
               ALG(K)(1:) = ' '
            ENDDO 
            J_AROW_NULL = 'Y'                              ! Nothing in J_AROW at this point

            KTERM_AROW = 0

            IF ((SYM_A == 'Y') .AND. (SYM_B == 'N')) THEN  ! 1st, if this SYM is true, count terms in col above the diag in row I
                        
               DO K=1,KBEG_MATIN-1                         ! Go through all terms in sparse MATIN from K=1 to where row I begins 
                  IF (J_A(K) == I) THEN                    ! looking for terms whose column number is the row number I
                     KTERM_AROW = KTERM_AROW + 1
                     I1 = ROW_AT_COLJ_BEG(I)
                     I2 = ROW_AT_COLJ_END(I)
                     DO II=I1,I2                           ! For terms whose col no. is I, determine row number and load into J_AROW
                        IF ((K >= I_A(II)) .AND. (K < I_A(II+1))) THEN
                           J_AROW(KTERM_AROW) = II
                           J_AROW_NULL = 'N'
                           ALG(KTERM_AROW) = ' #2'
                        ENDIF
                     ENDDO
                  ENDIF
               ENDDO

            ENDIF

            DO K=KBEG_MATIN,KEND_MATIN                     ! 2nd, always count terms from this row of MATIN from the diagonal out
               KTERM_AROW = KTERM_AROW + 1
               J_AROW(KTERM_AROW) = J_A(K)
               J_AROW_NULL = 'N'
               ALG(KTERM_AROW) = ' #1'
            ENDDO

            IF (J_AROW_NULL == 'N') THEN
               IF ((DEBUG(86) == 2) .OR. (DEBUG(86) == 3)) CALL PARTITION_SS_NTERM_DEB ( '2' )
            ENDIF

            DO K=1,AROW_MAX_TERMS
               IF (J_AROW(K) == 0) THEN
                  EXIT
               ELSE
                  IF (COL_PART_VEC(J_AROW(K)) == VAL_COLS) THEN
                     B_COL_NUM = B_COL_NUM_ARRAY(J_AROW(K))
                     NTERM_B = NTERM_B  + 1
                     IF ((DEBUG(86) == 2) .OR. (DEBUG(86) == 3)) CALL PARTITION_SS_NTERM_DEB ( '4' )
                  ELSE
                     IF ((DEBUG(86) == 2) .OR. (DEBUG(86) == 3)) CALL PARTITION_SS_NTERM_DEB ( '5' )
                  ENDIF
               ENDIF
            ENDDO 


         ENDIF

         KBEG_MATIN = KEND_MATIN + 1

      ENDDO i_do
!xx   WRITE(SC1,*) CR13

      IF ((DEBUG(86) == 2) .OR. (DEBUG(86) == 3)) CALL PARTITION_SS_NTERM_DEB ( '7' )

      CALL DEALLOCATE_SPARSE_ALG ( 'ALG' )
      CALL DEALLOCATE_SPARSE_ALG ( 'J_AROW' )

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

12345 format(7X,'Det part. size of ',A4,': row ',I8,' of ',I8,' SYM = ',A,A,4X,I2,':',I2,':',I2,'.',I3,A)

! ##################################################################################################################################

      CONTAINS

! ##################################################################################################################################

      SUBROUTINE PARTITION_SS_NTERM_DEB ( WHICH )

      CHARACTER( 1*BYTE)              :: WHICH                  ! Decides what to print out for this call to this subr

      INTEGER(LONG)                   :: KK                     ! Local count 
      INTEGER(LONG)                   :: II                     ! Local DO loop variable 

! **********************************************************************************************************************************
      IF      (WHICH == '1') THEN

         KK = 0
         DO II=1,NCOL_A
            B_COL_NUM_ARRAY(II) = 0
            IF (COL_PART_VEC(II) == VAL_COLS) THEN
               KK = KK + 1
               B_COL_NUM_ARRAY(II) = KK
            ENDIF
         ENDDO
         NCOL_B = KK

         WRITE(F06,*)
         WRITE(F06,2011)
         WRITE(F06,2012)
         WRITE(F06,2013)
         WRITE(F06,2014) MAT_A_NAME, MAT_B_NAME
         WRITE(F06,2015) NROW_A, NCOL_A, NTERM_A, NROW_B, NCOL_B
         IF (SYM_A == 'Y') THEN
            WRITE(F06,2017)
         ELSE
            WRITE(F06,2018)
         ENDIF
         IF (SYM_B == 'Y') THEN
            WRITE(F06,20171)
         ELSE
            WRITE(F06,20181)
         ENDIF
         WRITE(F06,2019)
         WRITE(F06,*)


      ELSE IF (WHICH == '2') THEN

         WRITE(F06,2021)
         WRITE(F06,2022) I, B_ROW_NUM
         WRITE(F06,2023)

      ELSE IF (WHICH == '3') THEN

      ELSE IF (WHICH == '4') THEN

         WRITE(F06,2041) ALG(K), K, I, J_AROW(K), B_ROW_NUM, B_COL_NUM

      ELSE IF (WHICH == '5') THEN

         WRITE(F06,2051) ALG(K), K, I, J_AROW(K)

      ELSE IF (WHICH == '6') THEN

         WRITE(F06,*)

      ELSE IF (WHICH == '7') THEN

         WRITE(F06,2021)
         WRITE(f06,2071) MAT_B_NAME, MAT_A_NAME, NTERM_B, NROW_B, NCOL_B
         WRITE(f06,*)
         WRITE(F06,2072)

      ENDIF

! **********************************************************************************************************************************
 2011 FORMAT(' __________________________________________________________________________________________________________________',&
             '_________________'                                                                                               ,//,&
             ' ::::::::::::::::::::::::::::::::::::START DEBUG(86) OUTPUT FROM SUBROUTINE PARTITION_NTERM_SS:::::::::::::::::::::',&
              ':::::::::::::::::',/)

 2012 FORMAT(' SETUP FOR SPARSE MATRIX PARTITION ROUTINE: Determine memory required for Compressed Row Storage (CRS) formatted'&
,' matrix B when it is',/,' -----------------------------------------',/,' partitioned from matrix A',/)

 2013 FORMAT(' Matrices A and/or B can be stored as symmetric (only terms on and above the diagonal), or with all nonzero terms' ,&
' stored.',/)

 2014 FORMAT(40X,' The name of CRS formatted matrix A is: ',A                                                                   ,/,&
             40x,' The name of CRS formatted matrix B is: ',A,/)

 2015 FORMAT(27X,' Matrix A has ',I8,' rows and ',I8,' cols and '  ,I12,' nonzero terms'                                        ,/,&
             27X,' Matrix B has ',I8,' rows and ',I8,' cols and a number of nonzero terms TBD here',/)

 2017 FORMAT(' Matrix A was flagged as a symmetric CRS array (only those terms on and above the diag are stored in array A)',/)

 2018 FORMAT(' Matrix A was flagged as a CRS array with all nonzero terms stored in array A',/)

20171 FORMAT(' Matrix B was flagged to be a symmetric CRS array (only those terms on and above the diag are stored in array B)',/)

20181 FORMAT(' Matrix B was flagged to be a CRS array with all nonzero terms stored in array A',/)

 2019 FORMAT(                                                                                                                      &
' In order to handle symmetric A matrices, which do not have all terms in a row (only those from the diag out), array J_AROW is',/,&
' used. J_AROW is a 1D array that contains the column numbers for all nonzero terms in one row of matrix A including those that',/,&
' may not actually be in array A due to symmetry storage. This array is used in a simulated partitioning of matrix B from'      ,/,&
' matrix A so that an estimate can be made of the nonzero terms that will go into the sparse arrays for B. The estimate is'     ,/,&
' needed so that memory can be allocated to sparse arrays for B prior to actually doing the partition in subr PARTITION_SS.'   ,//,&
' Alg #1 (below) gets data for array J_AROW directly from the Compressed Row Storage (CRS) format of array A.'                  ,/,&
' Alg #2 (below) is only needed if matrix A is input as symmetric (only terms on and above the diagonal) and gets terms for'    ,/,&
'         J_AROW from column I of matrix A while working on row I of matrix A. These are the terms that would be below the diag',/,&
'         in matrix A but are not explicitly in the array due to symmetry storage.'                                            ,//,&
' For each non null row of matrix A that is to be partitioned into matrix B, the following shows the development of array'      ,/,&
' J_AROW and the rows and columns where terms will be placed in the partitioned matrix (when subr PARTITION_SS is run)') 

 2021 FORMAT(1X,/' **************************************************************************************************************',&
              '*********************')

 2022 FORMAT(1X,' W O R K I N G   O N   R O W ',I8,'   O F   I N P U T   M A T R I X   A,   R O W   ',I8,'   O F   O U T P U T'   ,&
             '   M A T R I X   B',/)

 2023 FORMAT(29X,'Alg        Index       Data from matrix A              Data for matrix B'                                     ,/,&
             29X,'---        -----       ------------------             -------------------'                                    ,/,&
             29X,'              K             Row       Col                   Row       Col'                                    ,/,&
             29X,'                                J_AROW(K)',/)

 2041 FORMAT(29X,A,2X,I10,6X,I10,I10,12X,I10,I10)

 2051 FORMAT(29X,A,2X,I10,6X,I10,I10)

 2071 FORMAT( ' Matrix ',A,' partitioned from ',A,' will have ',I12,' nonzero terms in ',I8,' rows and ',I8,' columns',/)

 2072 FORMAT(' :::::::::::::::::::::::::::::::::::::END DEBUG(86) OUTPUT FROM SUBROUTINE PARTITION_NTERM_SS::::::::::::::::::::::',&
              ':::::::::::::::::'                                                                                               ,/,&
             ' __________________________________________________________________________________________________________________',&
             '_________________',/)








! **********************************************************************************************************************************

      END SUBROUTINE PARTITION_SS_NTERM_DEB

      END SUBROUTINE PARTITION_SS_NTERM
