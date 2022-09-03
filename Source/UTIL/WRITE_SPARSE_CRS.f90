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
 
      SUBROUTINE WRITE_SPARSE_CRS ( MAT_NAME, ROW_SET, COL_SET, NTERM_A, NROWS_A, I_AXX, J_AXX, AXX )
 
! Writes a matrix that is in sparse CRS format to the F06 output file based on user request via Bulk Data PARAM PRTijk entries

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  SPARSTOR, TINY
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_SPARSE_CRS_BEGEND
 
      USE WRITE_SPARSE_CRS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_SPARSE_CRS'
      CHARACTER(LEN=*), INTENT(IN)    :: COL_SET           ! Set designator for cols of matrix
      CHARACTER(LEN=*), INTENT(IN)    :: ROW_SET           ! Set designator for rows of matrix
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_NAME          ! Input matrix descriptor
      CHARACTER(132*BYTE)             :: LINE_OUT          ! Line to print out (to describe matrix) that is centered

      INTEGER(LONG), INTENT(IN)       :: NTERM_A           ! No. of terms in sparse matrix    
      INTEGER(LONG), INTENT(IN)       :: NROWS_A           ! No. of rows  in sparse matrix    
      INTEGER(LONG), INTENT(IN)       :: I_AXX(NROWS_A+1)  ! Array of starting indices for the 1-st term in rows of AXX
      INTEGER(LONG), INTENT(IN)       :: J_AXX(NTERM_A)    ! Array of col no's for terms in matrix AXX
      INTEGER(LONG)                   :: COL_COMP    = 0   ! Component number returned from subr GET_GRID_AND_COMP
      INTEGER(LONG)                   :: COL_GRID    = 0   ! Grid number returned from subr GET_GRID_AND_COMP
      INTEGER(LONG)                   :: I,J               ! DO loop index
      INTEGER(LONG)                   :: INDEX             ! Index into character array LINE_OUT
      INTEGER(LONG)                   :: K                 ! Counter       
      INTEGER(LONG)                   :: MAT_NAME_LEN      ! Length of char array MAT_NAME. On input, it is the length as defined
!                                                            in the calling subr. In this subr, MAT_NAME is striped of trailing
!                                                            blanks to get only the actual message. On exit MAT_NAME_LEN is the
!                                                            length of the finite message in MAT_NAME (i.e. without trailing blanks)
      INTEGER(LONG)                   :: NTERM_ROW_I       ! Number of terms in row I of AXX
      INTEGER(LONG)                   :: NULL_ROWS_A       ! Number of null rows in input matrix
      INTEGER(LONG)                   :: ROW_COMP    = 0   ! Component number returned from subr GET_GRID_AND_COMP
      INTEGER(LONG)                   :: ROW_GRID    = 0   ! Grid number returned from subr GET_GRID_AND_COMP
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_SPARSE_CRS_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: AXX(NTERM_A)      ! Array of terms in matrix AXX
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Strip out trailing blanks from MAT_NAME and put remainder centered in array LINE_OUT

      MAT_NAME_LEN = LEN(MAT_NAME)                         ! This is the length of MAT_NAME as input (includes trailing blanks)

      DO  I=MAT_NAME_LEN,1,-1                              ! Calc length of description in MAT_NAME, excluding trailing blanks
         IF (MAT_NAME(I:I) == ' ') THEN
            CYCLE
         ELSE
            MAT_NAME_LEN = I
            EXIT
         ENDIF
      ENDDO

      LINE_OUT(1:) = ' '                                   ! Center MAT_NAME (w/0 trailing blanks) in LINE_OUT
      INDEX = (LEN(LINE_OUT) - MAT_NAME_LEN)/2 -12         ! Subt 12 to allow for SPARSTOR msg
      LINE_OUT(INDEX:) = MAT_NAME(1:MAT_NAME_LEN) // ' (with PARAM SPARSTOR = ' // SPARSTOR // ')'

! Determine how many null rows there are in the input matrix (used in output description of matrix)

      NULL_ROWS_A = 0
      DO I=1,NROWS_A
         NTERM_ROW_I = I_AXX(I+1) - I_AXX(I)
         IF (NTERM_ROW_I == 0) THEN
            NULL_ROWS_A = NULL_ROWS_A + 1
         ENDIF
      ENDDO

! Write the matrix out

      IF (COL_SET == 'SUBCASE') THEN                       ! Input matrix has as many cols as there are subcases

         IF (TINY == ZERO) THEN
            WRITE(F06,101) LINE_OUT, NROWS_A, NTERM_A, NULL_ROWS_A
         ELSE
            WRITE(F06,102) LINE_OUT, TINY, NROWS_A, NTERM_A, NULL_ROWS_A
         ENDIF
         K = 0
         DO I=1,NROWS_A

            NTERM_ROW_I = I_AXX(I+1) - I_AXX(I)
            IF (NTERM_ROW_I == 0) CYCLE
            ROW_GRID = 0
            ROW_COMP = 0
            IF (ROW_SET(1:) /= ' ') THEN
               CALL GET_GRID_AND_COMP ( ROW_SET, I, ROW_GRID, ROW_COMP )
            ENDIF

            IF ((ROW_GRID > 0) .AND. (ROW_COMP > 0)) THEN

               DO J=1,NTERM_ROW_I 
                  K = K+1
                  IF (DABS(AXX(K)) > TINY) THEN
                     WRITE(F06,11) K, ROW_GRID, ROW_COMP, J_AXX(K), AXX(K)
                  ENDIF
               ENDDO

            ELSE

               DO J=1,NTERM_ROW_I 
                  IF (DABS(AXX(K)) > TINY) THEN
                     WRITE(F06,12) K, I, J_AXX(K),AXX(K)
                  ENDIF
               ENDDO

            ENDIF

            WRITE(F06,*)

         ENDDO

         WRITE(F06,*)

      ELSE                                                 ! Input matrix has column numbers output as integers, not grid-comp

         IF (TINY == ZERO) THEN
            WRITE(F06,201) LINE_OUT, NROWS_A, NTERM_A, NULL_ROWS_A
         ELSE
            WRITE(F06,202) LINE_OUT, TINY, NROWS_A, NTERM_A, NULL_ROWS_A
         ENDIF
         K = 0
         DO I=1,NROWS_A 

            NTERM_ROW_I = I_AXX(I+1) - I_AXX(I)
            IF (NTERM_ROW_I == 0) CYCLE
            ROW_GRID = 0
            ROW_COMP = 0
            IF (ROW_SET(1:) /= ' ') THEN
               CALL GET_GRID_AND_COMP ( ROW_SET, I, ROW_GRID, ROW_COMP )
            ENDIF

            IF ((ROW_GRID > 0) .AND. (ROW_COMP > 0)) THEN

               DO J=1,NTERM_ROW_I 
                  K = K+1
                  COL_GRID = 0
                  COL_COMP = 0
                  IF (COL_SET(1:) /= ' ') THEN
                     CALL GET_GRID_AND_COMP ( COL_SET, J_AXX(K), COL_GRID, COL_COMP )
                  ENDIF
                  IF (( COL_GRID > 0) .AND. (COL_COMP > 0)) THEN
                     IF (DABS(AXX(K)) > TINY) THEN
                        WRITE(F06,21) K, ROW_GRID, ROW_COMP, COL_GRID, COL_COMP, AXX(K)
                     ENDIF
                  ELSE
                     IF (DABS(AXX(K)) > TINY) THEN
                        WRITE(F06,22) K, ROW_GRID, ROW_COMP, J_AXX(K), AXX(K)
                     ENDIF
                  ENDIF
               ENDDO

            ELSE

               DO J=1,NTERM_ROW_I 
                  K = K+1
                  COL_GRID = 0
                  COL_COMP = 0
                  IF (COL_SET(1:) /= ' ') THEN
                     CALL GET_GRID_AND_COMP ( COL_SET, J_AXX(K), COL_GRID, COL_COMP )
                  ENDIF
                  IF (( COL_GRID > 0) .AND. (COL_COMP > 0)) THEN
                     IF (DABS(AXX(K)) > TINY) THEN
                        WRITE(F06,23) K, I, COL_GRID, COL_COMP, AXX(K)
                     ENDIF
                  ELSE
                     IF (DABS(AXX(K)) > TINY) THEN
                        WRITE(F06,24) K, I, J_AXX(K), AXX(K)
                     ENDIF
                  ENDIF
               ENDDO

            ENDIF

            WRITE(F06,*)

         ENDDO

         WRITE(F06,*)

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(/,'                                    NONZERO TERMS OF SPARSE CRS (Compressed Row Storage) MATRIX:'                  &
            ,/,A                                                                                                                   &
            ,/,'                         (Matrix has ',I8,' rows and ',I8,' nonzeros. ',I8,' null rows not written)'               &
            ,/,'                            Term           Row                    Col                     Value'                   &
            ,/,'                                  (Grid-Comp or row no)  (Internal subcase no)',/)

  102 FORMAT(/,'                                    NONZERO TERMS OF SPARSE CRS (Compressed Row Storage) MATRIX:'                  &
            ,/,A                                                                                                                   &
            ,/,'                                    (Terms with abs value <= PARAM TINY =',1ES10.3,' not output)'                  &
            ,/,'                         (Matrix has ',I8,' rows and ',I8,' nonzeros. ',I8,' null rows not written)'               &
            ,/,'                            Term           Row                    Col                     Value'                   &
            ,/,'                                  (Grid-Comp or row no)  (Internal subcase no)',/)

  201 FORMAT(/,'                                    NONZERO TERMS OF SPARSE CRS (Compressed Row Storage) MATRIX:'                  &
            ,/,A                                                                                                                   &
            ,/,'                         (Matrix has ',I8,' rows and ',I8,' nonzeros. ',I8,' null rows not written)'               &
            ,/,'                            Term           Row                    Col                     Value'                   &
            ,/,'                                  (Grid-Comp or row no)  (Grid-Comp or col no)',/)

  202 FORMAT(/,'                                    NONZERO TERMS OF SPARSE CRS (Compressed Row Storage) MATRIX:'                  &
            ,/,A                                                                                                                   &
            ,/,'                                    (Terms with abs value <= PARAM TINY =',1ES10.3,' not output)'                  &
            ,/,'                         (Matrix has ',I8,' rows and ',I8,' nonzeros. ',I8,' null rows not written)'               &
            ,/,'                            Term           Row                    Col                     Value'                   &
            ,/,'                                  (Grid-Comp or row no)  (Grid-Comp or col no)',/)

   11 FORMAT(20X,I12,5X,I8,'-',I1,13X,I8,14X,1ES21.14,'     11')

   12 FORMAT(20X,I12,5X,I8,15X,I8,14X,1ES21.14,'     12')

   21 FORMAT(20X,I12,5X,I8,'-',I1,13X,I8,'-',I1,12X,1ES21.14,'     21')

   22 FORMAT(20X,I12,5X,I8,'-',I1,13X,I8,14X,1ES21.14,'     22')

   23 FORMAT(20X,I12,5X,I8,15X,I8,'-',I1,12X,1ES14.6,'     23')

   24 FORMAT(20X,I12,5X,I8,15X,I8,14X,1ES21.14,'     24')

! **********************************************************************************************************************************
 
      END SUBROUTINE WRITE_SPARSE_CRS
