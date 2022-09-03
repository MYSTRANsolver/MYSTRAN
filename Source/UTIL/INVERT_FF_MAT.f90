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

      SUBROUTINE INVERT_FF_MAT ( CALLING_SUBR, MAT_A_NAME, A, NROWS, INFO )

! Invert symmetric matrix A which is stored in full format. The return has the inverse of the matrix in array A

      USE PENTIUM_II_KIND, ONLY       :  DOUBLE, LONG
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  INVERT_FF_MAT_BEGEND
      USE LAPACK_SYM_MAT_INV

      USE INVERT_FF_MAT_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'INVERT_FF_MAT'
      CHARACTER(LEN=*), INTENT(IN )   :: CALLING_SUBR      ! Name of subr that called this subr
      CHARACTER(LEN=*), INTENT(IN )   :: MAT_A_NAME        ! Name of input matrix to be inverted

      INTEGER(LONG)   , INTENT(IN)    :: NROWS             ! Row/col size of input matrix A
      INTEGER(LONG)   , INTENT(OUT)   :: INFO              ! Output from LAPACK routines to do factorization of Lapack band matrix
                                                           !   0:  successful exit
                                                           ! < 0:  if INFO = -i, the i-th argument had an illegal value
                                                           ! > 0:  if INFO =  i, the leading minor of order i is not pos definite
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)   , PARAMETER     :: SUBR_BEGEND = INVERT_FF_MAT_BEGEND


      REAL(DOUBLE)    , INTENT(INOUT) :: A(NROWS,NROWS)    ! Matrix to invert. Inverted matrix returned in A

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! In DPOTRF A has the matrix to invert as input and the triangular factor of A coming out. In DPOTRI A has the tria factor of the
! matrix to invert going in and the inverse of the matrix coming out

      CALL DPOTRF ( 'U', NROWS, A, NROWS, INFO )           ! Factor A

      IF (INFO > 0) THEN                                ! Error factoring A

         WRITE(ERR,994) MAT_A_NAME, INFO, CALLING_SUBR
         WRITE(F06,994) MAT_A_NAME, INFO, CALLING_SUBR

      ELSE

         CALL DPOTRI ( 'U', NROWS, A, NROWS, INFO )     ! No error factoring A so invert

         IF (INFO > 0) THEN                             ! Error inverting A

            WRITE(ERR,995) MAT_A_NAME, INFO, CALLING_SUBR
            WRITE(F06,995) MAT_A_NAME, INFO, CALLING_SUBR

         ELSE                                           ! Matrix inverted OK so set lower triangle of A based on symmetry

            DO I=1,NROWS
               DO J=1,I-1
                  A(I,J) = A(J,I)
               ENDDO
            ENDDO

         ENDIF

      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  994 FORMAT(' *ERROR   994: THE FACTORIZATION OF MATRIX NAMED "',A,'" COULD NOT BE COMPLETED BY LAPACK SUBR DPOTRF '              &
                    ,/,14X,' A SINGULARITY WAS FOUND IN ROW ',I8,'. THIS SUBR WAS CALLED BY SUBR ',A)

  995 FORMAT(' *ERROR   995: INVERSION OF MATRIX ',A,' COULD NOT BE COMPLETED BY LAPACK SUBR DPOTRI '                              &
                    ,/,14X,' THE DIAG TERM IN ROW ',I8,' OF THE TRIANG FACTOR OF THE MATRIX IS 0. THIS SUBR WAS CALLED BY SUBR ',A)

98764 format(32767(1es14.6))

98765 format(32767a14)

! **********************************************************************************************************************************

      END SUBROUTINE INVERT_FF_MAT

