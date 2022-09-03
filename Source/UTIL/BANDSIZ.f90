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

       SUBROUTINE BANDSIZ ( N, NTERM_MATIN, I_MATIN, J_MATIN, KD )

! Determines the band size of a matrix so that it can be put into the banded form required by the
! LAPACK routines that will be used to decompose it.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BANDSIZ_BEGEND

      USE BANDSIZ_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BANDSIZ'

      INTEGER(LONG), INTENT(IN)       :: N                    ! Col or row size of matrix MATIN (no. of A-set DOF's)
      INTEGER(LONG), INTENT(IN)       :: NTERM_MATIN          ! No. of terms in sparse matrix    
      INTEGER(LONG), INTENT(IN)       :: I_MATIN(N+1)         ! Array of row no's for terms in matrix MATIN
      INTEGER(LONG), INTENT(IN)       :: J_MATIN(NTERM_MATIN) ! Array of col no's for terms in matrix MATIN
      INTEGER(LONG), INTENT(OUT)      :: KD                   ! Number of sub (or super) diagonals in matrix MATIN.
                                                              ! The total band width of MATIN is 2*KD + 1. However, the upper
                                                              ! or lower triangle can be stored in an array of size KD+1 x N
      INTEGER(LONG)                   :: I,J                  ! DO loop index  
      INTEGER(LONG)                   :: K                    ! Counter        
      INTEGER(LONG)                   :: KD_TEMP              ! Temporary value of in calculation of KD
      INTEGER(LONG)                   :: NUM_TERMS_ROW_I      ! Number of terms in MATIN matrix in row I
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BANDSIZ_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      KD = 0

      K  = 0
      DO I=1,N
         NUM_TERMS_ROW_I = I_MATIN(I+1) - I_MATIN(I)  ! Number of terms in row I
         DO J=1,NUM_TERMS_ROW_I
            K = K + 1
            IF (K > NTERM_MATIN) THEN
               WRITE(ERR,927) SUBR_NAME,K,NTERM_MATIN
               WRITE(F06,927) SUBR_NAME,K,NTERM_MATIN
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )
            ENDIF
            KD_TEMP = J_MATIN(K) - I 
            IF (KD_TEMP >  KD) THEN 
               KD = KD_TEMP
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
  927 FORMAT(' *ERROR   927: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INDEX K = ',I12,' IS GREATER THAN NTERM_MATIN = ',I12)




! **********************************************************************************************************************************

      END SUBROUTINE BANDSIZ