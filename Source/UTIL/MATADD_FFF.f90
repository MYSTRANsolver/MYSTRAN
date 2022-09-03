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
 
      SUBROUTINE MATADD_FFF ( A, B, NROW, NCOL, ALPHA, BETA, ITRNSPB, C)
 
! Adds two matrices: A + B (if ITRNSPB = 0), or A + B' (if ITRNSPB = 1 and A and B are square).
! Returns result, matrix C. All matrices are in full format
! User must make certain that matrices A and B have the same number of rows and cols
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  EPSIL
      USE SUBR_BEGEND_LEVELS, ONLY    :  MATADD_FFF_BEGEND
 
      USE MATADD_FFF_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MATADD_FFF'

      INTEGER(LONG), INTENT(IN)       :: NROW              ! Number of rows in matrces A, B, C
      INTEGER(LONG), INTENT(IN)       :: NCOL              ! Number of cols in matrces A, B, C
      INTEGER(LONG), INTENT(IN)       :: ITRNSPB           ! Transpose indicator for matrix B
      INTEGER(LONG)                   :: I,J               ! DO loop indices or counters
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MATADD_FFF_BEGEND
 
      REAL(DOUBLE) , INTENT(IN)       :: A(NROW,NCOL)      ! Input  matrix A
      REAL(DOUBLE) , INTENT(IN)       :: B(NROW,NCOL)      ! Input  matrix B
      REAL(DOUBLE) , INTENT(IN)       :: ALPHA             ! Scalar multiplier for matrix A
      REAL(DOUBLE) , INTENT(IN)       :: BETA              ! Scalar multiplier for matrix B

      REAL(DOUBLE) , INTENT(OUT)      :: C(NROW,NCOL)      ! Output matrix C
 
 ! *********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************


! Initialize outputs

      DO I=1,NROW
         DO J=1,NCOL
            C(I,J) = ZERO
         ENDDO
      ENDDO

! Check for coding error

      IF ((ITRNSPB /=0) .AND. (ITRNSPB /=1)) THEN
         WRITE(ERR,933) SUBR_NAME,ITRNSPB
         WRITE(F06,933) SUBR_NAME,ITRNSPB
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )
      ENDIF

      IF ((ITRNSPB == 1) .AND. (NROW /= NCOL)) THEN
         WRITE(ERR,934) SUBR_NAME,ITRNSPB, NROW, NCOL
         WRITE(F06,934) SUBR_NAME,ITRNSPB, NROW, NCOL
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! Add matrices A and B (or A and B')

      IF (ITRNSPB == 0) THEN                               ! Add A, B

         DO I =1,NROW
            DO J = 1,NCOL
               C(I,J) = ALPHA*A(I,J) + BETA*B(I,J)
            ENDDO   
         ENDDO

      ELSE                                                 ! Add A, B'

         DO I =1,NROW
            DO J = 1,NCOL
               C(I,J) = ALPHA*A(I,J) + BETA*B(J,I)
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
  933 FORMAT(' *ERROR   933: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INPUT PARAMETER ITRNSPB MUST BE 0 OR 1 BUT VALUE IS ',I8)

  934 FORMAT(' *ERROR   934: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' WHEN INPUT PARAMETER ITRNSPB =',I3,' THEN INPUT PARAMETERS NROW AND NCOL MUST BE EQUAL. '             &
                    ,/,14X,' HOWEVER, AS INPUT TO THIS SUBR, NROW = ',I8,' AND NCOL = ',I8)


! **********************************************************************************************************************************

      END SUBROUTINE MATADD_FFF
