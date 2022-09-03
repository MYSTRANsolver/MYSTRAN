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

       SUBROUTINE BANDGEN_LAPACK_DPB ( MATIN_NAME, N, KD, NTERM_MATIN, I_MATIN, J_MATIN, MATIN, MATOUT, CALLING_SUBR )

! Puts sparse matrix MATIN into LAPACK DPB banded form in matrix MATOUT. MATIN is symmetric and has KD super diagonals
! (determined in subr BANDSIZ) and can be stored (upper triangle) in array MATOUT with KD+1 rows and N cols

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  SPARSTOR
      USE SUBR_BEGEND_LEVELS, ONLY    :  BANDGEN_BEGEND

      USE BANDGEN_LAPACK_DPB_USE_IFs

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BANDGEN_LAPACK_DPB'
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR         ! Name of subr calling this one
      CHARACTER(LEN=*), INTENT(IN)    :: MATIN_NAME           ! Name of matrix input

      INTEGER(LONG)                   :: ROW_NO,COL_NO        ! Row and col no's of terms in banded matrix MATOUT 

      INTEGER(LONG), INTENT(IN)       :: N                    ! Number of cols (or rows) of symmetric matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: NTERM_MATIN          ! No. of terms in sparse matrix    
      INTEGER(LONG), INTENT(IN)       :: I_MATIN(N+1)         ! Array of row no's for terms in matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: J_MATIN(NTERM_MATIN) ! Array of col no's for terms in matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: KD                   ! Number of sub (or super) diagonals in matrix MATIN.
                                                              ! The total band width of MATIN is 2*KD + 1. However, the upper
                                                              ! or lower triangle can be stored in an array of size KD+1 x N
      INTEGER(LONG)                   :: I,J                  ! DO loop indices
      INTEGER(LONG)                   :: K                    ! Counter
      INTEGER(LONG)                   :: NUM_TERMS_ROW_I      ! Number of terms in MATIN matrix in row I
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BANDGEN_BEGEND
     
      REAL(DOUBLE) , INTENT(IN)       :: MATIN(NTERM_MATIN)   ! Array of terms in sparse matrix MATIN
      REAL(DOUBLE) , INTENT(INOUT)    :: MATOUT(KD+1,N)       ! Array of terms in band matrix MATOUT

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages

      ROW_NO = 0
      K  = 0
      DO I=1,N
         NUM_TERMS_ROW_I = I_MATIN(I+1) - I_MATIN(I)       ! Number of terms in row I
         WRITE(SC1,12345,ADVANCE='NO') I, N, NUM_TERMS_ROW_I, CR13
         DO J=1,NUM_TERMS_ROW_I
            K = K + 1
            IF (K > NTERM_MATIN) THEN
               WRITE(ERR,923) SUBR_NAME, K, NTERM_MATIN
               WRITE(F06,923) SUBR_NAME, K, NTERM_MATIN
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )                      ! Coding error (attempt to exceed allocated array size), so quit
            ENDIF
            COL_NO = J_MATIN(K)
            IF (J_MATIN(K) >= I) THEN                      ! New test that should make this subr work whether MATIN is stored sym
               ROW_NO = (KD + 1) - (J_MATIN(K) - I)        ! (nonzeros on/above diag) or nonsym (all nonzero terms in each row)
               IF ((ROW_NO > 0) .AND. (ROW_NO <= KD+1) .AND. (COL_NO > 0) .AND. (COL_NO <= N)) THEN
                  MATOUT(ROW_NO,COL_NO) = MATIN(K)
               ELSE
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,955) SUBR_NAME, MATIN_NAME, CALLING_SUBR, KD+1, ROW_NO, N, COL_NO
                  WRITE(F06,955) SUBR_NAME, MATIN_NAME, CALLING_SUBR, N   , ROW_NO, N, COL_NO
                  CALL OUTA_HERE ( 'Y' )
               ENDIF
            ENDIF
         ENDDO
      ENDDO
      WRITE(SC1,*) CR13

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  923 FORMAT(' *ERROR   923: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INDEX K = ',I12,' IS GREATER THAN NTERM_MATIN = ',I12)

  955 FORMAT(' *ERROR   955: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INVALID ROW OR COL NUMBER CALCULATED FOR BAND FORM OF MATRIX ',A,' FROM CALLING SUBR ',A              &
                    ,/,14X,' ROW NUMBERS MUST BE > 0 AND <= ',I8,'. ROW NUMBER CALCULATED WAS ',I8                                 &
                    ,/,14X,' COL NUMBERS MUST BE > 0 AND <= ',I8,'. COL NUMBER CALCULATED WAS ',I8)

12345 format(7X,'Row ',I8,' of ',I8,' of orig matrix with ',I8,' term(s)',10X,A)

98788 format('       I       J       K  J_MATIN(K)      KD  ROW_NO  COL_NO    MATIN(K)'                                            &
          ,/,'       -       -       -  ----------      --  ------  ------   --------')

98789 format(3i8,i12,3i8,1es14.6)

! **********************************************************************************************************************************

      END SUBROUTINE BANDGEN_LAPACK_DPB