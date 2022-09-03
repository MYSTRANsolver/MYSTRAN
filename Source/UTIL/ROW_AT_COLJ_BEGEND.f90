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
 
      SUBROUTINE ROW_AT_COLJ_BEGEND ( NAME, NROWS, NCOLS, NTERM, I_A, J_A, ROW_AT_COLJ_BEG, ROW_AT_COLJ_END )

! Creates arrays ROW_AT_COLJ_BEG and ROW_AT_COLJ_END which are:

! ROW_AT_COLJ_BEG is an array that gives, for each col of sparse CRS input matrix A, the start row no. of nonzero terms in that col.
! ROW_AT_COLJ_END is an array that gives, for each col of sparse CRS input matrix A, the last  row no. of nonzero terms in that col.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ROW_AT_COLJ_BEGEND_BEGEND

      USE ROW_AT_COLJ_BEGEND_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ROW_AT_COLJ_BEGEND'
      CHARACTER(LEN=*), INTENT(IN )   :: NAME                  ! Name of input matrix A

      INTEGER(LONG), INTENT(IN )      :: NTERM                 ! No. terms in MATIN
      INTEGER(LONG), INTENT(IN )      :: NROWS                 ! No. rows in MATIN
      INTEGER(LONG), INTENT(IN )      :: NCOLS                 ! No. cols in MATIN
      INTEGER(LONG), INTENT(IN )      :: I_A(NROWS+1)          ! I_A(i+1) - I_A(i) is no. terms in row i of matrix A
      INTEGER(LONG), INTENT(IN )      :: J_A(NTERM)            ! Array of column numbers for matrix A
      INTEGER(LONG), INTENT(OUT)      :: ROW_AT_COLJ_BEG(NCOLS)! jth term is row number in MATIN where col j nonzeros begin 
      INTEGER(LONG), INTENT(OUT)      :: ROW_AT_COLJ_END(NCOLS)! jth term is row number in MATIN where col j nonzeros end
      INTEGER(LONG)                   :: COL_NUM               ! A column number from J_MATIN
      INTEGER(LONG)                   :: I,J,K                 ! DO loop indices or counters 
      INTEGER(LONG)                   :: NTERM_ROW_I           ! Number of terms in matrix A row I
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ROW_AT_COLJ_BEGEND_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      DO I=1,NCOLS
         ROW_AT_COLJ_END(I) = 0
      ENDDO


      DO J=1,NCOLS                                         ! Initialize arrays
         ROW_AT_COLJ_BEG(J) = 0                            ! This is the array that has the row nos at which each col of NAME begins
         ROW_AT_COLJ_END(J) = 0                            ! This is the array that has the row nos at which each col of NAME ends
      ENDDO

      K = 0
      DO I=1,NROWS
         NTERM_ROW_I = I_A(I+1) - I_A(I)
         DO J=1,NTERM_ROW_I
            K = K + 1
            IF (K > NTERM) CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, NTERM, NAME)
            COL_NUM = J_A(K)
            IF (ROW_AT_COLJ_BEG(COL_NUM) == 0) THEN
               ROW_AT_COLJ_BEG(COL_NUM) = I
            ENDIF
         ENDDO
      ENDDO

      K = NTERM + 1
      DO I=NROWS,1,-1
         NTERM_ROW_I = I_A(I+1) - I_A(I)
         DO J=NTERM_ROW_I,1,-1
            K = K - 1
            IF (K < 1) CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, NTERM, NAME)
            COL_NUM = J_A(K)
            IF (ROW_AT_COLJ_END(COL_NUM) == 0) THEN
               ROW_AT_COLJ_END(COL_NUM) = I
            ENDIF
         ENDDO
      ENDDO

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF
 
      RETURN

! **********************************************************************************************************************************

      END SUBROUTINE ROW_AT_COLJ_BEGEND

