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

      SUBROUTINE GET_I_MAT_FROM_I2_MAT ( MAT_NAME, NROWS, NTERMS, I2_MAT, I_MAT ) 

! I_MAT is the sparse compressed row format for sparse array MAT. I2_MAT has a row number for each of the terms in sparse MAT.
! I2_MAT is generally used when some sparse matrices are written to a file since it then has a row, col, value for all terms in the
! sparse MAT. I_MAT is usually the way the row numbers are stored in memory for sparse matrices.
! This subr creates I_MAT from a given I2_MAT

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_I_MAT_FROM_I2_MAT_BEGEND

      USE GET_I_MAT_FROM_I2_MAT_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GET_I_MAT_FROM_I2_MAT'
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_NAME          ! Matrix name

      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in MAT
      INTEGER(LONG), INTENT(IN)       :: NTERMS            ! Number of matrix terms that should be in MAT
      INTEGER(LONG), INTENT(IN)       :: I2_MAT(NTERMS)    ! Row numbers for terms in matrix MAT
      INTEGER(LONG), INTENT(OUT)      :: I_MAT(NROWS+1)    ! Row numbers for terms in matrix MAT
      INTEGER(LONG)                   :: I,K               ! DO loop indices or counters
      INTEGER(LONG)                   :: IROW              ! Integer row value read from FILNAM
      INTEGER(LONG)                   :: IROW_OLD          ! Previous value of IROW
      INTEGER(LONG)                   :: KTERM             ! Count of number of nonzero terms read from FILNAM
      INTEGER(LONG)                   :: MAT_ERR    = 0    ! Error count
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_I_MAT_FROM_I2_MAT_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      IF (NTERMS > 0) THEN

         KTERM    = 0
         IROW_OLD = 0
         I_MAT(1) = 1

k_do1:   DO K = 1,NTERMS
            IROW = I2_MAT(K)
            IF (IROW > IROW_OLD) THEN
               DO I=IROW_OLD+1,IROW
                  I_MAT(I+1) = I_MAT(I)
               ENDDO
               IROW_OLD = IROW
            ELSE IF (IROW < IROW_OLD) THEN
               MAT_ERR   = MAT_ERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,926) SUBR_NAME, MAT_NAME, IROW_OLD, IROW
               WRITE(F06,926) SUBR_NAME, MAT_NAME, IROW_OLD, IROW
               CYCLE k_do1
            ENDIF
            I_MAT(IROW+1) = I_MAT(IROW+1) + 1
            KTERM         = KTERM + 1
         ENDDO k_do1

         IF (MAT_ERR /= 0) THEN
            WRITE(ERR,9996) SUBR_NAME,MAT_ERR
            WRITE(F06,9996) SUBR_NAME,MAT_ERR
            CALL OUTA_HERE ( 'Y' )                         ! Quit due to above errors in k_do1 loop
         ENDIF

         IF (IROW < NROWS) THEN                            ! Fill out remainder of I_MAT, if needed
            DO I=IROW+1,NROWS
               I_MAT(I+1) = I_MAT(I)
            ENDDO
         ENDIF

      ELSE                                                 ! MAT is null so set I_MAT

         DO I=1,NROWS+1
            I_MAT(I) = 1
         ENDDO

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF
 
      RETURN

! **********************************************************************************************************************************
  926 FORMAT(' *ERROR   926: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' MATRIX ',A,' IS STORED INCORRECTLY. THE MATRIX MUST BE STORED BY ROWS IN NUMERICAL ORDER '            &
                    ,/,14X,' HOWEVER, ROW ',I12,' IS STORED BEFORE ROW ',I12)

 9996 FORMAT(/,' PROCESSING ABORTED IN SUBROUTINE ',A,' DUE TO ABOVE ',I8,' ERRORS')

! **********************************************************************************************************************************

      END SUBROUTINE GET_I_MAT_FROM_I2_MAT

