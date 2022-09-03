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
 
      SUBROUTINE FULL_TO_SPARSE_CRS ( MATIN_NAME, N, M, MATIN_FULL, NTERM_ALLOC, SMALL, CALLING_SUBR, SYM_OUT,                     &
                                      I_MATOUT, J_MATOUT, MATOUT )

! Converts matrices in full format to sparse (compressed row storage) format 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  FULL_TO_SPARSE_CRS_BEGEND
 
      USE FULL_TO_SPARSE_CRS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'FULL_TO_SPARSE_CRS'
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR         ! Name of subr that called this one
      CHARACTER(LEN=*), INTENT(IN)    :: MATIN_NAME           ! Name of matrix
      CHARACTER(LEN=*), INTENT(IN)    :: SYM_OUT              ! 'Y' or 'N' symmetry indicator for output matrix.
 
      INTEGER(LONG), INTENT(IN)       :: N                    ! Number of rows in input matrix, MATIN_FULL
      INTEGER(LONG), INTENT(IN)       :: M                    ! Number of cols in input matrix, MATIN_FULL
      INTEGER(LONG), INTENT(IN)       :: NTERM_ALLOC          ! Number of nonzero terms allocated to MATOUT in calling subr
      INTEGER(LONG), INTENT(OUT)      :: I_MATOUT(N+1)        ! I_MATOUT(I+1) - I_MATOUT(I) = number of nonzeros in MATOUT row I
      INTEGER(LONG), INTENT(OUT)      :: J_MATOUT(NTERM_ALLOC)! Col numbers for nonzero terms in MATOUT
      INTEGER(LONG)                   :: I,J                  ! DO loop indices
      INTEGER(LONG)                   :: JSTART               ! Starting value for a DO loop
      INTEGER(LONG)                   :: KTERM                ! Counter
      INTEGER(LONG)                   :: ROW_I_NTERMS         ! No. terms in row I of output matrix MATOUT
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = FULL_TO_SPARSE_CRS_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: MATIN_FULL(N,M)      ! Real nonzero values in input matrix MATIN
      REAL(DOUBLE) , INTENT(IN)       :: SMALL                ! Terms < SMALL are filtered out (both here and in calling subr)
      REAL(DOUBLE) , INTENT(OUT)      :: MATOUT(NTERM_ALLOC)  ! Real nonzero values in output matrix MATOUT

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
34568 FORMAT(' I, J, MATIN_FULL(I,J) = ', 2i8, 1es14.6)
! Initialize outputs

      DO I=1,N+1
         I_MATOUT(I) = 0
      ENDDO

      DO I=1,NTERM_ALLOC
         J_MATOUT(I) = 0
           MATOUT(I) = ZERO
      ENDDO

      KTERM = 0
      I_MATOUT(1) = 1
      DO I=1,N
         ROW_I_NTERMS = 0
         I_MATOUT(I+1) = I_MATOUT(I)
         IF      (SYM_OUT == 'Y') THEN
            JSTART = I
         ELSE IF (SYM_OUT == 'N') THEN
            JSTART = 1
         ENDIF
         DO J=JSTART,M
            IF (DABS(MATIN_FULL(I,J)) > SMALL) THEN
               KTERM = KTERM + 1
               ROW_I_NTERMS = ROW_I_NTERMS + 1
               IF (KTERM > NTERM_ALLOC) CALL ARRAY_SIZE_ERROR_1( SUBR_NAME, NTERM_ALLOC, MATIN_NAME )
               I_MATOUT(I+1)   = I_MATOUT(I+1) + 1
               J_MATOUT(KTERM) = J
                 MATOUT(KTERM) = MATIN_FULL(I,J)
            ENDIF
         ENDDO
      ENDDO

      IF (KTERM /= NTERM_ALLOC) THEN
         FATAL_ERR = FATAL_ERR + 1
         WRITE(ERR,963) SUBR_NAME, MATIN_NAME, KTERM, NTERM_ALLOC, CALLING_SUBR
         WRITE(F06,963) SUBR_NAME, MATIN_NAME, KTERM, NTERM_ALLOC, CALLING_SUBR
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
  963 FORMAT(' *ERROR   993: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' THE CALCD NUMBER OF TERMS PUT INTO MATRIX "',A,'" WAS ',I12,' BUT THERE WERE ',I12,' ALLOCATED TO IT' &
                    ,/,14X,' BY THE CALLING SUBR: ',A)

! **********************************************************************************************************************************
 
      END SUBROUTINE FULL_TO_SPARSE_CRS
