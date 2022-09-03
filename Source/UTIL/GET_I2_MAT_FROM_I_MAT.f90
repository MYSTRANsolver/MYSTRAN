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

      SUBROUTINE GET_I2_MAT_FROM_I_MAT ( MAT_NAME, NROWS, NTERMS, I_MAT, I2_MAT ) 

! This subr does the inverse of subr GET_I_MAT_FROM_I2_MAT

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_I2_MAT_FROM_I_MAT_BEGEND

      USE GET_I2_MAT_FROM_I_MAT_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GET_I2_MAT_FROM_I_MAT'
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_NAME          ! Matrix name

      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in MAT
      INTEGER(LONG), INTENT(IN)       :: NTERMS            ! Number of matrix terms that should be in MAT
      INTEGER(LONG), INTENT(IN)       :: I_MAT(NROWS+1)    ! Row indicators for terms in matrix MAT
      INTEGER(LONG), INTENT(OUT)      :: I2_MAT(NTERMS)    ! Row numbers for terms in matrix MAT
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices or counters
      INTEGER(LONG)                   :: NUM_IN_ROW_I      ! Number of nonzero terms in row I
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_I2_MAT_FROM_I_MAT_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize

      DO I=1,NTERMS
         I2_MAT(I) = 0
      ENDDO

! Get I2_MAT

      K = 0
      IF (NTERMS > 0) THEN
         DO I=1,NROWS
            NUM_IN_ROW_I = I_MAT(I+1) - I_MAT(I)
            DO J=1,NUM_IN_ROW_I
               K = K + 1
               IF (K > NTERMS)  CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, K, MAT_NAME )
               I2_MAT(K) = I
            ENDDO
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

      END SUBROUTINE GET_I2_MAT_FROM_I_MAT

