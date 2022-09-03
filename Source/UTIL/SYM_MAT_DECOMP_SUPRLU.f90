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
 
      SUBROUTINE SYM_MAT_DECOMP_SUPRLU ( CALLING_SUBR, MATIN_NAME, NROWS, NTERMS, I_MATIN, J_MATIN, MATIN, INFO )

! Decomposes a symmetric band matrix into triangular factors. The input matrix, MATIN, is stored in CRS sparse format

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06, SC1
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC       
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  CRS_CCS, SPARSTOR
      USE SCRATCH_MATRICES, ONLY      :  I_CCS1, J_CCS1, CCS1
      USE SuperLU_STUF, ONLY          :  SLU_FACTORS
      USE SUBR_BEGEND_LEVELS, ONLY    :  SYM_MAT_DECOMP_SUPRLU_BEGEND

      USE SYM_MAT_DECOMP_SUPRLU_USE_IFs
                      
      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SYM_MAT_DECOMP_SUPRLU'

      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! The subr that called this subr (used for output error purposes)
      CHARACTER(LEN=*), INTENT(IN)    :: MATIN_NAME        ! Name of matrix to be decomposed

      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in sparse matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: NTERMS            ! Number of nonzeros in sparse matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: I_MATIN(NROWS+1)  ! Indicators of number of nonzero terms in rows of matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: J_MATIN(NTERMS)   ! Col numberts of nonzero terms in matrix MATIN

      INTEGER(LONG), INTENT(INOUT)    :: INFO              ! Output from SuperLU routine

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SYM_MAT_DECOMP_SUPRLU_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: MATIN(NTERMS)     ! A small number to compare real zero
      REAL(DOUBLE)                    :: DUM_COL(NROWS)    ! Temp variable for solving equations

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************

      DO I=1,NROWS                                         ! Need a null col of loads when SuperLU is called to factor KLL
         DUM_COL(I) = ZERO                                 ! (only because it appears in the calling list)
      ENDDO

      IF      (SPARSTOR == 'SYM   ') THEN

         write(f06,*) ' Code not written for sparse SuperLU decomp when SPARSTOR = SYM'
         stop

      ELSE IF (SPARSTOR == 'NONSYM') THEN

         IF      (CRS_CCS == 'CRS') THEN                ! Use MATIN stored in Compressed Row Storage (CRS) format

            CALL C_FORTRAN_DGSSV( 1, NROWS, NTERMS, 1, MATIN, J_MATIN, I_MATIN, DUM_COL, NROWS, SLU_FACTORS, INFO )

         ELSE IF (CRS_CCS == 'CCS') THEN                ! Use MATIN stored in Compressed Col Storage (CCS) format

            CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NROWS, NTERMS, SUBR_NAME )
            CALL SPARSE_CRS_SPARSE_CCS ( NROWS, NROWS, NTERMS, MATIN_NAME, I_MATIN, J_MATIN, MATIN, 'CCS1', J_CCS1, I_CCS1, CCS1,  &
                                        'Y' )
            CALL C_FORTRAN_DGSSV( 1, NROWS, NTERMS, 1, CCS1, I_CCS1, J_CCS1, DUM_COL, NROWS, SLU_FACTORS, INFO )

         ELSE

            WRITE(ERR,933) SUBR_NAME, 'CRS_CCS'
            WRITE(F06,933) SUBR_NAME, 'CRS_CCS'
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )

         ENDIF


      ELSE                                              ! Error - incorrect CRS_CCS 

         WRITE(ERR,932) SUBR_NAME, 'SPARSTOR'
         WRITE(F06,932) SUBR_NAME, 'SPARSTOR'
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )

      ENDIF

      IF (INFO .EQ. 0) THEN
         WRITE (SC1,9902) MATIN_NAME, SUBR_NAME 
         WRITE (F06,9902) MATIN_NAME, SUBR_NAME
      ELSE
         WRITE(SC1,9903) INFO, TRIM(SUBR_NAME), TRIM(CALLING_SUBR)
         WRITE(ERR,9903) INFO, TRIM(SUBR_NAME), TRIM(CALLING_SUBR)
         WRITE(F06,9903) INFO, TRIM(SUBR_NAME), TRIM(CALLING_SUBR)
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

!***********************************************************************************************************************************
  932 FORMAT(' *ERROR   932: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' PARAMETER ', A, ' MUST BE EITHER "SYM" OR "NONSYM" BUT VALUE IS ',A)

  933 FORMAT(' *ERROR   933: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' PARAMETER ', A, ' MUST BE EITHER "CRS" OR "CCS" BUT VALUE IS ',A)

 9902 FORMAT(' SUPERLU FACTORIZATION OF MATRIX ', A, ' SUCCEEDED IN SUBR ', A)

 9903 FORMAT(' *ERROR  9903: SUPERLU SPARSE SOLVER HAS FAILED WITH INFO = ', I12,' IN SUBR ', A, ' CALLED BY SUBR ', A)

!***********************************************************************************************************************************

      END SUBROUTINE SYM_MAT_DECOMP_SUPRLU



