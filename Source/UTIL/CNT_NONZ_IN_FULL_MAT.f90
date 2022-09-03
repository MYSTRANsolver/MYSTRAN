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
 
      SUBROUTINE CNT_NONZ_IN_FULL_MAT ( MATIN_NAME, MATIN, NROWS, NCOLS, SYM, NTERM_NONZERO, SMALL )
 
! Counts the number of significant (abs val larger than variable SMALL) numbers that are in a portion of full matrix:
!  If SYM = 'N' then all  terms in MATIN are used in the count
!  If SYM = 'Y' then only terms in MATIN upper triangle are used in the count

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  EPSIL, SUPINFO, TINY
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG 
      USE SUBR_BEGEND_LEVELS, ONLY    :  CNT_NONZ_IN_FULL_MAT_BEGEND

      USE CNT_NONZ_IN_FULL_MAT_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CNT_NONZ_IN_FULL_MAT'
      CHARACTER(LEN=*), INTENT(IN)    :: MATIN_NAME        ! Name of the matrix
      CHARACTER(LEN=*), INTENT(IN)    :: SYM               ! See above ('ALL' or 'UTR')

      INTEGER(LONG), INTENT(IN)       :: NCOLS             ! Number of cols in the matrix
      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in the matrix
      INTEGER(LONG), INTENT(OUT)      :: NTERM_NONZERO     ! Number of nonzero (or significant) values in the matrix
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: JSTART            ! A computed DO loop index
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CNT_NONZ_IN_FULL_MAT_BEGEND
 
      REAL(DOUBLE) , INTENT(IN)       :: MATIN(NROWS,NCOLS)! Input full matrix
      REAL(DOUBLE) , INTENT(OUT)      :: SMALL             ! Filter for small terms

! *********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGIN',F10.3)
      ENDIF

! **********************************************************************************************************************************
      IF (DEBUG(196) == 0) THEN
         SMALL = EPSIL(1)
         WRITE(ERR,101) SMALL, MATIN_NAME, SUBR_NAME
         IF (SUPINFO == 'N') THEN
            WRITE(F06,101) SMALL, MATIN_NAME, SUBR_NAME
         ENDIF
      ELSE
         SMALL = TINY
         WRITE(ERR,102) SMALL, MATIN_NAME, SUBR_NAME
         IF (SUPINFO == 'N') THEN
            WRITE(F06,102) SMALL, MATIN_NAME, SUBR_NAME
         ENDIF
      ENDIF

      NTERM_NONZERO = 0

      DO I=1,NROWS
         IF      (SYM == 'N') THEN
            JSTART = 1
         ELSE IF (SYM == 'Y') THEN
            JSTART = I
         ENDIF
         DO J=JSTART,NCOLS
            IF (DABS(MATIN(I,J)) > SMALL) THEN
               NTERM_NONZERO = NTERM_NONZERO + 1
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
  101 FORMAT(' *INFORMATION: TERMS WHOSE ABS VALUE ARE < MACH_PREC =',1ES10.3,' ARE NOT INCLUDED IN MATRIX ',A,' IN SUBR ',A       &
                    ,/,14X,' AS THIS FULL MATRIX IS BEING CONVERTED TO A SPARSE MATRIX')

  102 FORMAT(' *INFORMATION: TERMS WHOSE ABS VALUE ARE < PARAM TINY =',1ES10.3,' ARE NOT INCLUDED IN MATRIX ',A,' IN SUBR ',A      &
                    ,/,14X,' AS THIS FULL MATRIX IS BEING CONVERTED TO A SPARSE MATRIX')

! **********************************************************************************************************************************

      END SUBROUTINE CNT_NONZ_IN_FULL_MAT
