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

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SPARSE_ALG_ARRAYS, ONLY     :  LOGICAL_VEC
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
      CHARACTER( 2*BYTE)              :: ALG               ! Which algorithm is used in solving for the terms in a row of C

      INTEGER(LONG), INTENT(IN )      :: NROWS             ! Number of rows in input matrices A and B
      INTEGER(LONG), INTENT(IN )      :: NTERM_A           ! Number of nonzero terms in input matrix A
      INTEGER(LONG), INTENT(IN )      :: NTERM_B           ! Number of nonzero terms in input matrix B
      INTEGER(LONG), INTENT(IN )      :: I_A(NROWS+1)      ! I_A(I+1) - I_A(I) = no. terms in row I of matrix A
      INTEGER(LONG), INTENT(IN )      :: I_B(NROWS+1)      ! I_B(I+1) - I_B(I) = no. terms in row I of matrix B
      INTEGER(LONG), INTENT(IN )      :: J_A(NTERM_A)      ! Col no's for nonzero terms in matrix A
      INTEGER(LONG), INTENT(IN )      :: J_B(NTERM_B)      ! Col no's for nonzero terms in matrix B
      INTEGER(LONG), INTENT(OUT)      :: NTERM_C           ! Number of nonzero terms in output matrix C
      INTEGER(LONG)                   :: DELTA_NTERM_C     ! Number of terms that will go into matrix C for one row
      INTEGER(LONG)                   :: I,J               ! DO loop indices or counters
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
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MATADD_SSS_NTERM_BEGEND
       
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

! Initialize outputs

      NTERM_C = 0

      IF ((DEBUG(81) == 1) .OR. (DEBUG(81) == 3)) CALL MATADD_SSS_NTERM_DEB ( '1', '   ' )

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

! Write warning if these MAXIMAX_COL_NUM_A /= MAXIMAX_COL_NUM_B
!  
!     IF (MAXIMAX_COL_NUM_A /= MAXIMAX_COL_NUM_B) THEN
!        WARN_ERR = WARN_ERR + 1
!        WRITE(ERR,1700) MAT_A_NAME, MAT_B_NAME, SUBR_NAME, MAT_A_NAME, MAXIMAX_COL_NUM_A, MAT_B_NAME, MAXIMAX_COL_NUM_B 
!        IF (SUPWARN == 'N') THEN
!           WRITE(F06,1700) MAT_A_NAME, MAT_B_NAME, SUBR_NAME, MAT_A_NAME, MAXIMAX_COL_NUM_A, MAT_B_NAME, MAXIMAX_COL_NUM_B 
!        ENDIF
!     ENDIF 
!  
      IF ((DEBUG(81) == 1) .OR. (DEBUG(81) == 3)) CALL MATADD_SSS_NTERM_DEB ( '2', '   ' )

! Allocate memory to array LOGICAL_VEC (it will have as many terms as MAXIMAX_COL_NUM_C and will be initialized to .FALSE.)
! In the code below, terms in range MIN_COL_NUM_C to MAX_COL_NUM_C will get reset to .TRUE. if there will be a nonzero term
! in a column of C. The variables MIN_COL_NUM_C and MAX_COL_NUM_C get calculated in the DO loop below for each row of C.)

      CALL ALLOCATE_SPARSE_ALG ( 'LOGICAL_VEC', 1, MAXIMAX_COL_NUM_C, SUBR_NAME )

! Count number of terms (NTERM_C) that will go into output matrix C

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
            DELTA_NTERM_C = 0
         ENDIF a_nor_b
  
a_no_b:  IF ((NUM_A_ROW_I /= 0) .AND. (NUM_B_ROW_I == 0)) THEN       ! This row of A is not null but row of B is null
            ALG = 'YN'
            MIN_COL_NUM_B = 0
            MAX_COL_NUM_B = 0
            MIN_COL_NUM_A = J_A(I_A(I))                       
            MAX_COL_NUM_A = J_A(I_A(I+1)-1)           
            MIN_COL_NUM_C = MIN_COL_NUM_A
            MAX_COL_NUM_C = MAX_COL_NUM_A
            DELTA_NTERM_C = NUM_A_ROW_I                              ! so DELTA_NTERM_C is only the num of terms in A for this row
            NTERM_C = NTERM_C + NUM_A_ROW_I
         ENDIF a_no_b

b_no_a:  IF ((NUM_A_ROW_I == 0) .AND. (NUM_B_ROW_I /= 0)) THEN       ! This row of A is null but row of B is not null
            ALG = 'NY'
            MIN_COL_NUM_A = 0
            MAX_COL_NUM_A = 0
            MIN_COL_NUM_B = J_B(I_B(I))                       
            MAX_COL_NUM_B = J_B(I_B(I+1)-1)           
            MIN_COL_NUM_C = MIN_COL_NUM_B
            MAX_COL_NUM_C = MAX_COL_NUM_B
            DELTA_NTERM_C = NUM_B_ROW_I                              ! so DELTA_NTERM_C is only the num of terms in A for this row
            NTERM_C = NTERM_C + NUM_B_ROW_I
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

            DO J=1,MAXIMAX_COL_NUM_C                                 ! Initialize LOGICAL_VEC before calc'ing it below
               LOGICAL_VEC(J) = .FALSE.
            ENDDO

            DO J=I_A(I),I_A(I+1)-1
               LOGICAL_VEC(J_A(J)) = .TRUE.
            ENDDO 

            DO J=I_B(I),I_B(I+1)-1
               LOGICAL_VEC(J_B(J)) = .TRUE.
            ENDDO

            DELTA_NTERM_C = 0                                        ! LOGICAL_VEC now has T for each col that will have a term in
            DO J=MIN_COL_NUM_C,MAX_COL_NUM_C                         ! matrix A+B. Count the T's in LOGICAL_VEC. This will be the
               IF (LOGICAL_VEC(J)) THEN                              ! number of nonzero terms in this row for matrix A+B
                  DELTA_NTERM_C = DELTA_NTERM_C + 1
               ENDIF
            ENDDO
            NTERM_C = NTERM_C + DELTA_NTERM_C                        ! Update NTERM_C, the total count of nonzero's in A+B  

         ENDIF a_and_b

         IF (ALG /= 'NN') THEN
            IF ((DEBUG(81) == 1) .OR. (DEBUG(81) == 3)) CALL MATADD_SSS_NTERM_DEB ( '3', ALG )
         ENDIF

      ENDDO

      CALL DEALLOCATE_SPARSE_ALG ( 'LOGICAL_VEC' )

      IF ((DEBUG(81) == 1) .OR. (DEBUG(81) == 3)) CALL MATADD_SSS_NTERM_DEB ( '9', '   ' )

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

 1700 FORMAT(' *WARNING    : POSSIBLE INCOMPATIBILITY IN ADDING MATRIX ',A,' AND MATRIX ',A,' FOUND IN SUBR ',A                    &
                    ,/,14X,' THE HIGHEST NUMBER OF A NONZERO COLUMN IN MATRIX ',A,' IS COL NUMBER ',I8,' AND'                      &
                    ,/,14X,' THE HIGHEST NUMBER OF A NONZERO COLUMN IN MATRIX ',A,' IS COL NUMBER ',I8,'.'                         &
                    ,/,14X,' THIS IS NOT NECESSARILY AN ERROR, ONLY A NOTE TO THE PROGRAMMER THAT ITS SENSIBILITY BE VERIFIED.'    &
                    ,/,14X,' THE MOST COMMON CAUSE FOR THIS MESSAGE IS THAT ONE OF THE SPARSE MATRICES HAS NULL COLS ON THE',      &
                           ' RIGHT SIDE OF THE MATRIX',/)

! **********************************************************************************************************************************

! ##################################################################################################################################

      CONTAINS

! ##################################################################################################################################

      SUBROUTINE MATADD_SSS_NTERM_DEB ( WHICH, ALG )

      CHARACTER(LEN=*)  , INTENT(IN)  :: ALG               ! Which algorithm is used
      CHARACTER( 1*BYTE), INTENT(IN)  :: WHICH             ! Decides what to print out for this call to this subr

! **********************************************************************************************************************************
      IF      (WHICH == '1') THEN

         WRITE(F06,*)
         WRITE(F06,1011)
         WRITE(F06,1012)
         WRITE(F06,1013)
         WRITE(F06,1014) MAT_A_NAME, MAT_B_NAME, MAT_C_NAME
         WRITE(F06,1015) NROWS, NTERM_A, NROWS, NTERM_B
         WRITE(F06,1019)
         WRITE(F06,*)

      ELSE IF (WHICH == '2') THEN

         WRITE(F06,1021)
         WRITE(F06,1022)
         WRITE(F06,1024)
         WRITE(F06,*)
      ELSE IF (WHICH == '3') THEN

         WRITE(F06,1031) I, ALG, MIN_COL_NUM_A, MAX_COL_NUM_A, MIN_COL_NUM_B, MAX_COL_NUM_B, MIN_COL_NUM_C, MAX_COL_NUM_C,&
                         DELTA_NTERM_C, NTERM_C
                               
      ELSE IF (WHICH == '4') THEN

      ELSE IF (WHICH == '5') THEN

      ELSE IF (WHICH == '6') THEN

      ELSE IF (WHICH == '7') THEN

      ELSE IF (WHICH == '8') THEN

      ELSE IF (WHICH == '9') THEN

         WRITE(F06,*)
         WRITE(F06,1091) MAT_C_NAME, NTERM_C

      ENDIF

! **********************************************************************************************************************************
 1011 FORMAT(' ___________________________________________________________________________________________________________________'&
            ,'________________'                                                                                                ,//,&
             ' ::::::::::::::::::::::::::::::::::::START DEBUG(81) OUTPUT FROM SUBROUTINE MATADD_SSS_NTERM:::::::::::::::::::::::',&
              ':::::::::::::::::',/)

 1012 FORMAT(' SSS SETUP FOR SPARSE MATRIX ADD ROUTINE: Determine memory required for Compressed Row Storage (CRS) formatted',     &
' matrix C resulting',/,' ---------------------------------------',/,' from the addition of two CRS matrices A and B',/)

 1013 FORMAT(' A and B must be stored in the same format with regard to symmetry (if one is stored symmetric, with only terms on' ,&
' and above the',/,' diagonal, the other must be also stored as symmetric.',/)

 1014 FORMAT(42X,' The name of CRS formatted matrix A is: ',A                                                                   ,/,&
             42x,' The name of CRS formatted matrix B is: ',A                                                                   ,/,&
             42x,' The name of CRS formatted matrix C is: ',A,/)

 1015 FORMAT(30X,' Matrix A has ',I8,' rows and             '  ,I12,' nonzero terms'                                            ,/,&
             30X,' Matrix B has ',I8,' rows and             '  ,I12,' nonzero terms',/)

 1019 FORMAT(                                                                                                                      &
' Alg YN (below) is for the case where A has terms in the row being processed but B does not'                                   ,/,&
' Alg NY (below) is for the case where B has terms in the row being processed but A does not'                                   ,/,&
' Alg YY (below) is for the case where both A and B have nonzero terms in the row being processed'                             ,//,&
' Output is only given for non null rows of matrix C')

 1021 FORMAT(' *******************************************************************************************************************'&
             ,'****************')

 1022 FORMAT(1X,'D E T E R M I N I N G   T H E   N U M B E R   N O N Z E R O S   T E R M S   N E E D E D   F O R   M A T R I X   C'&
                 ,//)

 1024 FORMAT(20X,'Col Num Range For     Col Num Range For     Col Num Range For    Data For Nonzeros For This Row Of Matrix C:' ,/,&
             20X,'Nonzero Terms In A    Nonzero Terms In B    Nonzero Terms In C          Number Terms     Accumulative'  ,/,&
6X,'Row   Alg     Min Col   Max Col     Min Col   Max Col     Min Col   Max Col           In This Row      Number Terms'   ,/,&
            19X,'------------------    ------------------    ------------------    ---------------------------------------------')


 1031 FORMAT(1X,I8,4X,A2,4X,I8,2X,I8,4X,I8,2X,I8,4X,I8,2X,I8,11X,I8,11X,I8,I10)

 1091 FORMAT(' *******************************************************************************************************************'&
             ,'****************'                                                                                                ,/,&
             ' SUMMARY: Matrix C = ',A,' will have ',I12,' nonzero terms',/)

 1095 FORMAT(' :::::::::::::::::::::::::::::::::::END DEBUG(81) OUTPUT FROM SUBROUTINE MATADD_SSS_NTERM::::::::::::::::::::::::::',&
              ':::::::::::::::::'                                                                                               ,/,&
             ' ___________________________________________________________________________________________________________________'&
            ,'________________',/)

! **********************************************************************************************************************************

      END SUBROUTINE MATADD_SSS_NTERM_DEB

      END SUBROUTINE MATADD_SSS_NTERM
