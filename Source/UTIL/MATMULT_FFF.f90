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
 
      SUBROUTINE MATMULT_FFF ( A, B, NROWA, NCOLA, NCOLB, C )
 
! Multiplies two matrices: A x B. Returns result, matrix C. All matrices are in full format
! NOTE: User is responsible for making sure that A and B are conformable
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  MATMULT_FFF_BEGEND
 
      USE MATMULT_FFF_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MATMULT_FFF'

      INTEGER(LONG), INTENT(IN)       :: NROWA             ! No. rows in input matrix A
      INTEGER(LONG), INTENT(IN)       :: NCOLA             ! No. cols in input matrix A 
      INTEGER(LONG), INTENT(IN)       :: NCOLB             ! No. cols in input matrix B 
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices or counters
      INTEGER(LONG)                   :: NROWB             ! 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MATMULT_FFF_BEGEND
 
      REAL(DOUBLE) , INTENT(IN)       :: A(NROWA,NCOLA)    ! Input  matrix A
      REAL(DOUBLE) , INTENT(IN)       :: B(NCOLA,NCOLB)    ! Input  matrix B
      REAL(DOUBLE) , INTENT(OUT)      :: C(NROWA,NCOLB)    ! Output matrix C
 
 ! *********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      DO I=1,NROWA
         DO J=1,NCOLB
            C(I,J) = ZERO
         ENDDO
      ENDDO

      NROWB = NCOLA

! Multiply A x B

      DO I =1,NROWA
         DO J = 1,NCOLB
            C(I,J) = ZERO
            DO K = 1,NROWB
               C(I,J) = C(I,J) + A(I,K)*B(K,J)
            ENDDO
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

      END SUBROUTINE MATMULT_FFF
