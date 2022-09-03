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
 
      SUBROUTINE SPARSE_CRS_TERM_COUNT ( NROWS, NTERM_IN, MATIN_NAME, I_MATIN, J_MATIN, NTERM_OUT )
                                                        
! Counts terms in sparse compressed row (CRS) storage format matrices that are on, or above, the diagonal. Used to get the number
! of terms from a matrix stored as sparse nonsym that will be in the same matrix stored as sparse sym.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  SPARSE_CRS_TERM_COUNT_BEGEND
 
      USE SPARSE_CRS_TERM_COUNT_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SPARSE_CRS_TERM_COUNT'
      CHARACTER(LEN=*), INTENT(IN)    :: MATIN_NAME          ! Name of input matrix
 
      INTEGER(LONG), INTENT(IN)       :: NROWS               ! Number of rows in input matrix, MATIN
      INTEGER(LONG), INTENT(IN)       :: NTERM_IN            ! Number of nonzero terms in input matrix, MATIN
      INTEGER(LONG), INTENT(IN)       :: I_MATIN(NROWS+1)    ! I_MATIN(I+1) - I_MATIN(I) are the number of nonzeros in MATIN row I
      INTEGER(LONG), INTENT(IN)       :: J_MATIN(NTERM_IN)   ! Col numbers for nonzero terms in MATIN
      INTEGER(LONG), INTENT(OUT)      :: NTERM_OUT           ! Number of nonzero terms in output matrix, MATOUT
      INTEGER(LONG)                   :: I,K                 ! DO loop indices or counters
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SPARSE_CRS_TERM_COUNT_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      NTERM_OUT = 0

! Calc outputs

      NTERM_OUT   = 0
      DO I=1,NROWS
         DO K=I_MATIN(I),I_MATIN(I+1)-1
            IF (J_MATIN(K) >= I) THEN                      ! This is a term on or above the diag that will go into the output matrix
               NTERM_OUT = NTERM_OUT + 1
            ELSE
               CYCLE
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
 
      END SUBROUTINE SPARSE_CRS_TERM_COUNT
