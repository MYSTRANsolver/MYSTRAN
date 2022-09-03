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
 
      SUBROUTINE GET_SPARSE_CRS_ROW ( MATIN_NAME, ROW_NUM, NTERM, NROWS, NCOLS, I_MATIN, J_MATIN, MATIN, BETA, OUT_VEC, NULL_ROW )

! Gets a row (ROW_NUM) from a matrix in sparse (compressed row storage) format described by I_MATIN, J_MATIN, MATIN
! arrays, multiplies it by BETA, and puts result into array OUT_VEC. Sets NULL_ROW to 'Y' if result is null.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_SPARSE_CRS_ROW_BEGEND
      
      USE GET_SPARSE_CRS_ROW_USE_IFs

      IMPLICIT NONE 
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GET_SPARSE_CRS_ROW'
      CHARACTER(LEN=*), INTENT(IN )   :: MATIN_NAME        ! Name of input matrix to be partitioned
      CHARACTER(LEN=*), INTENT(OUT)   :: NULL_ROW          ! = 'Y' if OUT_VEC is null

      INTEGER(LONG), INTENT(IN )      :: NCOLS             ! No. cols in MATIN
      INTEGER(LONG), INTENT(IN )      :: NROWS             ! No. rows in MATIN
      INTEGER(LONG), INTENT(IN )      :: NTERM             ! No. terms in MATIN
      INTEGER(LONG), INTENT(IN )      :: I_MATIN(NROWS+1)  ! Starting locations in MATIN for each row
      INTEGER(LONG), INTENT(IN )      :: J_MATIN(NTERM)    ! Col numbers for terms in MATIN
      INTEGER(LONG), INTENT(IN )      :: ROW_NUM           ! Row number for the row to get in MATIN
      INTEGER(LONG)                   :: J,K               ! DO loop indices or counters
      INTEGER(LONG)                   :: NUM_TERMS_IN_ROW  ! No. terms in row ROW_NUM of MATIN
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_SPARSE_CRS_ROW_BEGEND
                                 
      REAL(DOUBLE) , INTENT(IN)       :: MATIN(NTERM)      ! Nonzero terms in matrix MATIN
      REAL(DOUBLE) , INTENT(IN)       :: BETA              ! Scalar multiplier for row from MATIN
      REAL(DOUBLE) , INTENT(OUT)      :: OUT_VEC(NCOLS)    ! Output vector containing the terms from row ROW_NUM of MATIN

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      DO J=1,NCOLS
         OUT_VEC(J) = ZERO
      ENDDO 

! Make sure ROW NUM is a row of MATIN

      IF ((ROW_NUM < 1) .OR. (ROW_NUM > NROWS)) THEN
         WRITE(ERR,929) SUBR_NAME,ROW_NUM,MATIN_NAME,NROWS
         WRITE(F06,929) SUBR_NAME,ROW_NUM,MATIN_NAME,NROWS
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )                              ! Coding error (invalid ROW_NUM), so quit
      ENDIF

! Load nonzero terms from row ROW_NUM of MATIN into OUT_VEC

      NULL_ROW = 'N'
      NUM_TERMS_IN_ROW = I_MATIN(ROW_NUM+1) - I_MATIN(ROW_NUM)
      IF (NUM_TERMS_IN_ROW == 0) THEN
         NULL_ROW = 'Y'
      ELSE
         K = I_MATIN(ROW_NUM) 
         DO J=1,NUM_TERMS_IN_ROW
            OUT_VEC(J_MATIN(K)) = BETA*MATIN(K)
            K = K + 1
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
  929 FORMAT(' *ERROR   929: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ATTEMPT TO GET ROW = ',I12,' FROM MATRIX ',A,' WHEN IT ONLY HAS ROW NUMBERS 1 THRU ',I12)   

! **********************************************************************************************************************************
 
      END SUBROUTINE GET_SPARSE_CRS_ROW
