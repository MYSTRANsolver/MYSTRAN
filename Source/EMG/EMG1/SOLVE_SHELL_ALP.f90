! ##################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2019 Dr William R Case, Jr (dbcase29@gmail,com)                                              
                                                                                                         
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

      SUBROUTINE SOLVE_SHELL_ALP ( SHELL_ALP_ERR )

! Solves for SHELL_ALP which is an effective CTE matrix for the PSHELL equivalent for a PCOMP (akin to ALPVEC's 1st 3 rows for a
! homogeneous shell element) 

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MEMATC
      USE PARAMS, ONLY                :  EPSIL

      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE MODEL_STUF, ONLY            :  SHELL_ALP, SHELL_A, SHELL_B, SHELL_D, SHELL_T, SHELL_AALP, SHELL_BALP, SHELL_DALP,        &
                                         SHELL_TALP

      USE SOLVE_SHELL_ALP_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'SOLVE_SHELL_ALP'

      INTEGER(LONG), INTENT(OUT)      :: SHELL_ALP_ERR(4)  ! Error indicator if SHELL_ALP was calculated
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: INFO              ! Error indicator from subr INVERT_FF_MAT

      REAL(DOUBLE)                    :: A(3,3)            ! One of the matrices SHELL_A, SHELL_D, SHELL_T, SHELL_B
      REAL(DOUBLE)                    :: AI(3,3)           ! Inverse of A
      REAL(DOUBLE)                    :: B(3)              ! Solution for a col of SHELL_ALP
      REAL(DOUBLE)                    :: C(3)              ! One of the matrices SHELL_AALP, SHELL_DALP, SHELL_TALP, SHELL_BALP
      REAL(DOUBLE)                    :: DETA              ! Determinant of A = SHELL_A
      REAL(DOUBLE)                    :: EPS1              ! Small number with which to comapre zero

! **********************************************************************************************************************************
      EPS1 = EPSIL(1)

      DO I=1,3
         B(I) = ZERO
         DO J=1,MEMATC
            SHELL_ALP(I,J) = ZERO
         ENDDO
      ENDDO

      DO K=1,4                                             ! Solve for cols of SHELL_ALP. Put in array ALP_COL(I) for the time being

         IF      (K == 1) THEN

            DO I=1,3
               C(I) = SHELL_AALP(I)
               DO J=1,3
                  A(I,J) = SHELL_A(I,J)
               ENDDO
            ENDDO

         ELSE IF (K == 2) THEN

            DO I=1,3
               C(I) = SHELL_DALP(I)
               DO J=1,3
                  A(I,J) = SHELL_D(I,J)
               ENDDO
            ENDDO

         ELSE IF (K == 3) THEN

            DO I=1,3                                       ! Make A for SHELL_T into a 3x3 matrix with SHELL_T in the upper 2x2 and
               C(I) = ZERO                                 ! a 1 in the 3x3 slot. In this manner we can call the inversion routine
               DO J=1,3                                    ! in a single loop
                  A(I,J) = ZERO
               ENDDO
            ENDDO
            DO I=1,2
               C(I) = SHELL_TALP(I)
               DO J=1,2
                  A(I,J) = SHELL_T(I,J)
               ENDDO
            ENDDO
            A(3,3) = ONE

         ELSE IF (K == 4) THEN

            DO I=1,3
               C(I) = SHELL_BALP(I)
               DO J=1,3
                  A(I,J) = SHELL_B(I,J)
               ENDDO
            ENDDO

         ENDIF

         SHELL_ALP_ERR(K) = 0

         DETA = (A(1,1)*A(2,2)*A(3,3) + A(2,1)*A(3,2)*A(1,3) + A(3,1)*A(2,3)*A(1,2))                                               &
              - (A(1,3)*A(2,2)*A(3,1) + A(1,2)*A(2,1)*A(3,3) + A(1,1)*A(3,2)*A(2,3))

         IF (DABS(DETA) > EPS1) THEN                       ! Det A = SHELL_A is > 0 so call subr to invert A = SHELL_A

            DO I=1,3
               DO J=1,3
                  AI(I,J) = A(I,J)
               ENDDO
            ENDDO

            CALL INVERT_FF_MAT ( SUBR_NAME, 'NAME(K)', AI, 3, INFO )

            IF (INFO == 0) THEN                            ! Inversion was successful so calc SHELL_ALP

               CALL MATMULT_FFF ( AI, C, 3, 3, 1, B )

               IF      (K == 1) THEN

                  DO I=1,3
                     SHELL_ALP(I,1) = B(I)
                  ENDDO

               ELSE IF (K == 2) THEN

                  DO I=1,3
                     SHELL_ALP(I,2) = B(I)
                  ENDDO

               ELSE IF (K == 3) THEN

                  DO I=1,2
                     SHELL_ALP(I+4,3) = B(I)
                  ENDDO

               ELSE IF (K == 4) THEN

                  DO I=1,3
                     SHELL_ALP(I,4) = B(I)
                  ENDDO

               ENDIF

            ELSE

               SHELL_ALP_ERR(K) = 2                        ! Inversion was unsuccessful so set error flag = 2

            ENDIF

         ELSE                                              ! Det A = 0 so set error flag = 1

            SHELL_ALP_ERR(K) = 1

         ENDIF

      ENDDO


      RETURN

! **********************************************************************************************************************************



! **********************************************************************************************************************************

      END SUBROUTINE SOLVE_SHELL_ALP
