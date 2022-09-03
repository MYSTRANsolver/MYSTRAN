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

       SUBROUTINE INVERT_EIGENS ( MLAM, N, W, Z, EIG_NUM )
  
! For MGIV and LANCZOS eigenvalue methods, inverts and reorders eigenvalues/vectors since those algorithms are set up to solve for
! 1/eigenvalue
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NVEC
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  INVERT_EIGENS_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE
      USE MACHINE_PARAMS, ONLY        :  MACH_SFMIN, MACH_LARGE_NUM
      USE MODEL_STUF, ONLY            :  EIG_SIGMA
      USE LAPACK_BLAS_AUX
 
      USE INVERT_EIGENS_USE_IFs

      IMPLICIT NONE
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'INVERT_EIGENS'

      INTEGER(LONG), INTENT(IN)       :: MLAM              ! Number of eigenvalues.
      INTEGER(LONG), INTENT(IN)       :: N                 ! Size of eigenvectors.
      INTEGER(LONG), INTENT(INOUT)    :: EIG_NUM(MLAM)     ! Eigenvector numbers.
      INTEGER(LONG)                   :: I,J               ! DO loop indices.
      INTEGER(LONG)                   :: M1                ! One eigenvector number
      INTEGER(LONG)                   :: PM,QM             ! Indices used in reording the W and Z
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = INVERT_EIGENS_BEGEND

      REAL(DOUBLE) , INTENT(INOUT)    :: W(MLAM)           ! Eigenvalues
      REAL(DOUBLE) , INTENT(INOUT)    :: Z(N,NVEC)         ! Eigenvectors
      REAL(DOUBLE)                    :: W1                ! One eigenvalue
      REAL(DOUBLE)                    :: Z1(N)             ! One eigenvector

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      DO I=1,MLAM                                          ! Invert eigenvalues
         IF (DABS(W(I)) > MACH_SFMIN) THEN
            W(I) = EIG_SIGMA + ONE/W(I)
         ELSE
            IF (W(I) >= 0.D0) THEN
               W(I) =  MACH_LARGE_NUM
            ELSE
               W(I) = -MACH_LARGE_NUM
            ENDIF
         ENDIF
      ENDDO

      PM = MLAM                                            ! Reorder so lowest mgiv eigenvalue (1/w) is first
      QM = 1
      DO J=1,MLAM
         IF (PM > QM) THEN
            W1    = W(QM)
            W(QM) = W(PM)
            W(PM) = W1
            M1    = EIG_NUM(QM)
            EIG_NUM(QM) = EIG_NUM(PM)
            EIG_NUM(PM) = M1
         ENDIF
         PM = PM-1
         QM = QM+1 
      ENDDO

      IF (NVEC > 0) THEN
         PM = NVEC                                         ! Reorder eigenvectors
         QM = 1
         DO J=1,NVEC
            IF (PM > QM) THEN
               DO I=1,N
                 Z1(I)   = Z(I,QM)
               ENDDO
               DO I=1,N
                  Z(I,QM) = Z(I,PM)
               ENDDO
               DO I=1,N
                  Z(I,PM) = Z1(I)
               ENDDO
            ENDIF
            PM = PM-1
            QM = QM+1 
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
 
      END SUBROUTINE INVERT_EIGENS

