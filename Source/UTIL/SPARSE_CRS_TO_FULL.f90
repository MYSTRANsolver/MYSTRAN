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
 
      SUBROUTINE SPARSE_CRS_TO_FULL ( MATIN_NAME, NTERM_IN, NROWS, NCOLS, SYM_IN, I_MATIN, J_MATIN, MATIN, MATOUT )

! Converts matrices in sparse compressed row storage format to full format 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS
      USE SUBR_BEGEND_LEVELS, ONLY    :  SPARSE_CRS_TO_FULL_BEGEND
 
      USE SPARSE_CRS_TO_FULL_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SPARSE_CRS_TO_FULL'
      CHARACTER(LEN=*), INTENT(IN)    :: SYM_IN              ! 'Y' or 'N' symmetry indicator for input matrix.
      CHARACTER(LEN=*), INTENT(IN)    :: MATIN_NAME          ! Name of matrix
 
      INTEGER(LONG), INTENT(IN)       :: NCOLS               ! Number of cols in input matrix, MATIN
      INTEGER(LONG), INTENT(IN)       :: NROWS               ! Number of rows in input matrix, MATIN
      INTEGER(LONG), INTENT(IN)       :: NTERM_IN            ! Number of nonzero terms in input matrix, MATIN
      INTEGER(LONG), INTENT(IN)       :: I_MATIN(NROWS+1)    ! I_MATIN(I+1) - I_MATIN(I) are the number of nonzeros in MATIN row I
      INTEGER(LONG), INTENT(IN)       :: J_MATIN(NTERM_IN)   ! Col numbers for nonzero terms in MATIN
      INTEGER(LONG)                   :: I,J,K               ! DO loop indices or counters
      INTEGER(LONG)                   :: ROW_I_NTERMS        ! No. terms in row I of input matrix MATIN
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SPARSE_CRS_TO_FULL_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: MATIN(NTERM_IN)     ! Real nonzero values in input  matrix MATIN
      REAL(DOUBLE) , INTENT(OUT)      :: MATOUT(NROWS,NCOLS) ! Real nonzero values in output matrix MATOUT

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      DO I=1,NROWS
         DO J=1,NCOLS
            MATOUT(I,J) = ZERO
         ENDDO
      ENDDO

! Calc outputs

      DO I=1,NROWS
         DO J=1,NCOLS
            MATOUT(I,J) = ZERO
         ENDDO 
      ENDDO 

! Create full matrix MATOUT from sparse input matrix MATIN

      K = 0                                                
      DO I=1,NROWS
         ROW_I_NTERMS = I_MATIN(I+1) - I_MATIN(I)
         DO J=1,ROW_I_NTERMS
            K = K + 1
            IF (K > NTERM_IN) CALL ARRAY_SIZE_ERROR_1( SUBR_NAME, NTERM_IN, MATIN_NAME )
            MATOUT(I,J_MATIN(K)) = MATIN(K)
         ENDDO 
      ENDDO

! If input matrix was tagged as symmetric, then lower triang portion was not in MATIN, so set lower triang portion:

      IF (SYM_IN == 'Y') THEN
         DO I=1,NROWS
            DO J=1,I-1
               MATOUT(I,J) = MATOUT(J,I)
            ENDDO 
         ENDDO 
      ENDIF

      IF( DEBUG(205) > 0) CALL DEBUG_CRS_TO_FULL

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE DEBUG_CRS_TO_FULL
 
      IMPLICIT NONE
 
      CHARACTER(14*BYTE)              :: MATOUT_CHAR(NCOLS) ! Character representation of the real data in one row of REAL_VAR

      INTEGER                         :: II,JJ,KK,LL        ! DO loop indices or counters
      INTEGER                         :: NGC                ! If NCOLS is divisible by 6, NGC is that number
      INTEGER                         :: NGR                ! If NROWS is divisible by 6, NGR is that number

! **********************************************************************************************************************************
      WRITE(F06,98720)

      WRITE(F06,9300) MATIN_NAME

      NGC = 0
icols:DO II=1,NROWS
         IF (6*II == NROWS) THEN
            NGC = II
            EXIT icols
         ENDIF
      ENDDO icols

      NGR = 0
irows:DO II=1,NROWS
         IF (6*II == NROWS) THEN
            NGR = II
            EXIT irows
         ENDIF
      ENDDO irows

      IF ((NGC > 0) .AND. (NGR > 0)) THEN

         DO II=1,NGR
            DO KK=1,6
               CALL WRT_REAL_TO_CHAR_VAR ( MATOUT, NROWS, NCOLS, 6*(II-1)+KK, MATOUT_CHAR )
               DO JJ=1,NGC
                  IF (JJ < NGC) THEN
                     WRITE(F06,9206,ADVANCE='NO') (MATOUT_CHAR(6*(JJ-1)+LL),LL=1,6)
                  ELSE
                     WRITE(F06,9207,ADVANCE='YES') (MATOUT_CHAR(6*(JJ-1)+LL),LL=1,6)
                  ENDIF
               ENDDO
            ENDDO
            IF (II < NGR) WRITE(F06,9208,ADVANCE='YES')
         ENDDO

      ELSE

         DO II=1,NROWS
            CALL WRT_REAL_TO_CHAR_VAR ( MATOUT, NROWS, NCOLS, II, MATOUT_CHAR )
            WRITE(F06,9206) (MATOUT_CHAR(JJ),JJ=1,NCOLS)
         ENDDO

      ENDIF

      WRITE(F06,*)

      WRITE(F06,98799)
 
      WRITE(F06,*)

! **********************************************************************************************************************************
 9206 FORMAT(6A14,' |')

 9207 FORMAT(6A14)

 9208 FORMAT('  -----------------------------------------------------------------------------------------------------------------',&
             '-------------------------------------------------------')

 9300 FORMAT(65X, 'Matrix ', A, ' displayed in full format',/,65X,'----------------------------------------',/) 

98720 FORMAT(' __________________________________________________________________________________________________________________',&
             '_________________'                                                                                               ,//,&
             ' ::::::::::::::::::::::::::::::::::::START DEBUG(205) OUTPUT FROM SUBROUTINE SPARSE_CRS_TO_FULL::::::::::::::::::::',&
             ':::::::::::::::::',/)

98799 FORMAT(' ::::::::::::::::::::::::::::::::::::::END DEBUG(205) OUTPUT FROM SUBROUTINE SPARSE_CRS_TO_FULL::::::::::::::::::::',&
             ':::::::::::::::::'                                                                                                ,/,&
             ' __________________________________________________________________________________________________________________',&
             '_________________',/)

! **********************************************************************************************************************************

      END SUBROUTINE DEBUG_CRS_TO_FULL
 
      END SUBROUTINE SPARSE_CRS_TO_FULL
