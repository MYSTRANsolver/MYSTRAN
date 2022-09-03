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
 
      SUBROUTINE PARTITION_FF ( MAT_A_NAME, NROW_A, NCOL_A, A, ROW_PART_VEC, COL_PART_VEC, VAL_ROWS, VAL_COLS,                     &
                                MAT_B_NAME, NROW_B, NCOL_B, B )                
 
! Partitions full matrix B from full matrix A. The input matrix, A, in full format represented by:
 
!                      A(i,j) is a real array of all terms (i=1 to NROW_A, j = 1 to NCOL_A)

! is partitioned using partitioning vectors:

!                         ROW_PART_VEC(i) - a row partitioning vector (i=1 to NROW_A containing VAL_ROWS = 1's and 2's) and
!                         COL_PART_VEC(i) - a col partitioning vector (i=1 to NCOL_A containing VAL_COLS = 1's and 2's)

! into one output matrix, B, in full format:

! The rows for which ROW_PART_VEC(i) = VAL_ROWS and the cols for which COL_PART_VEC(i) = VAL_COLS are the ones to be partitioned.
! The outout from this subr is array B in full CRS format:

!                      B(i,j) is a real array of all terms (i=1 to NROW_B, j = 1 to NCOL_B) in the partitioned matrix 


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  PARTITION_FF_BEGEND
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      USE PARTITION_FF_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'PARTITION_FF'
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_B_NAME             ! Name of partitioned output matrix
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_A_NAME             ! Name of input matrix to be partitioned
      INTEGER(LONG)   , INTENT(IN)    :: NCOL_A                 ! No. cols in A
      INTEGER(LONG)   , INTENT(IN)    :: NROW_A                 ! No. rows in A
      INTEGER(LONG)   , INTENT(IN)    :: COL_PART_VEC(NCOL_A)   ! Col partitioning vector (1's and 2's)
      INTEGER(LONG)   , INTENT(IN)    :: ROW_PART_VEC(NROW_A)   ! Row partitioning vector (1's and 2's)
      INTEGER(LONG)   , INTENT(IN)    :: VAL_ROWS               ! Value in ROW_PART_VEC to look for for partitioning rows  
      INTEGER(LONG)   , INTENT(IN)    :: VAL_COLS               ! Value in COL_PART_VEC to look for for partitioning cols
      INTEGER(LONG)   , INTENT(IN)    :: NCOL_B                 ! No. cols in B
      INTEGER(LONG)   , INTENT(IN)    :: NROW_B                 ! No. rows in B
      INTEGER(LONG)                   :: I,J                    ! DO loop indices 
      INTEGER(LONG)                   :: IB,JB                  ! Counters
      INTEGER(LONG)   , PARAMETER     :: SUBR_BEGEND = PARTITION_FF_BEGEND
       
      REAL(DOUBLE)    , INTENT(IN )   :: A(NROW_A,NCOL_A)       ! Input  matrix

      REAL(DOUBLE)    , INTENT(OUT)   :: B(NROW_B,NCOL_B)       ! Output matrix

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC,MAT_A_NAME,MAT_B_NAME
 9001    FORMAT(1X,A,' BEGIN',F10.3,' Input matrix is ',A,'. Partitioned output matrix is ',A)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      DO I=1,NROW_B
         DO J=1,NCOL_B
            B(I,J) = ZERO
         ENDDO
      ENDDO

      IB = 0
      DO I=1,NROW_A
         IF (ROW_PART_VEC(I) == VAL_ROWS) THEN
            IB = IB + 1
            JB = 0
            DO J=1,NCOL_A
               IF (COL_PART_VEC(J) == VAL_COLS) THEN
                  JB = JB + 1
                  B(IB,JB) = A(I,J)
               ENDIF
            ENDDO
         ENDIF
      ENDDO


! Make sure that IB and JB are the row/col size of B

      IF (IB /= NROW_B) THEN
         WRITE(ERR,957) SUBR_NAME, 'IB AND NROW_B', IB, NROW_B
         WRITE(F06,957) SUBR_NAME, 'IB AND NROW_B', IB, NROW_B
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )
      ENDIF

      IF (JB /= NCOL_B) THEN
         WRITE(ERR,957) SUBR_NAME, 'JB AND NCOL_B', JB, NCOL_B
         WRITE(F06,957) SUBR_NAME, 'JB AND NCOL_B', JB, NCOL_B
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  952 FORMAT(' *ERROR   952: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,15X,  A,' MUST BE THE SAME BUT ARE = ',I8,' AND ',I8,' RESPECTIVELY')

  957 FORMAT(' *ERROR   957: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,15X,  A,' MUST BE THE SAME BUT ARE = ',I8,' AND ',I8,' RESPECTIVELY')


! **********************************************************************************************************************************

      END SUBROUTINE PARTITION_FF
