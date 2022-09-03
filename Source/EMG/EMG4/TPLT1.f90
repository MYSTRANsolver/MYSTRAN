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
  
      SUBROUTINE TPLT1 ( OPT, AREA, X2E, X3E, Y3E )
  
! DKT triangular thin (Kirchoff) plate bending element. This element is based on the following work:

! "An Explicit Formulation For An Efficient Triangular Plate-Bending Element", by Jean-Louis Batoz,
! International Journal For Numerical Methods In Engineering, Vol 18 (1982) pp 1655-1677

! Element matrices calculated are:

!  1) PTE       = element thermal load vectors         , if OPT(2) = 'Y'
!  2) SEi, STEi = element stress data recovery matrices, if OPT(3) = 'Y'
!  3) KE        = element linea stiffness matrix       , if OPT(4) = 'Y'
!  4) PPE       = element pressure load matrix         , if OPT(5) = 'Y'
!  5) KED       = element differen stiff matrix calc   , if OPT(6) = 'Y' = 'Y'
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, WRT_LOG, f06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  TPLT1_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE, TWO, THREE, FOUR, SIX, TWELVE
      USE MODEL_STUF, ONLY            :  ALPVEC, BE2, DT, EB, KE, PRESS, PPE, PTE, SHELL_DALP, SHELL_D, SHELL_PROP_ALP, SE2, STE2
 
      USE TPLT1_USE_IFs
 
      IMPLICIT NONE 
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'TPLT1'
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices

      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: I1                ! A computed index into array S
      INTEGER(LONG)                   :: I2                ! Part of a computed index into array S
      INTEGER(LONG)                   :: J1                ! A computed index into array KE
      INTEGER(LONG)                   :: K1                ! A computed index into array KE
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = TPLT1_BEGEND
 
      REAL(DOUBLE) , INTENT(IN)       :: AREA              ! Element area
      REAL(DOUBLE) , INTENT(IN)       :: X2E               ! x coord of elem node 2
      REAL(DOUBLE) , INTENT(IN)       :: X3E               ! x coord of elem node 3
      REAL(DOUBLE) , INTENT(IN)       :: Y3E               ! y coord of elem node 3
      REAL(DOUBLE)                    :: ALP(3)            ! Col of ALPVEC
      REAL(DOUBLE)                    :: ALPHA(3,3,3,3)    ! 9 sets of 3x3 submatrices needed for calc of elem stiff matrix, KE
      REAL(DOUBLE)                    :: AREAF             ! 48*AREA
      REAL(DOUBLE)                    :: C11               ! Intermediate variable used in calc SEi stress recovery matrices
      REAL(DOUBLE)                    :: C12               ! Intermediate variable used in calc SEi stress recovery matrices
      REAL(DOUBLE)                    :: C21               ! Intermediate variable used in calc SEi stress recovery matrices
      REAL(DOUBLE)                    :: C22               ! Intermediate variable used in calc SEi stress recovery matrices
      REAL(DOUBLE)                    :: C33               ! Intermediate variable used in calc SEi stress recovery matrices
      REAL(DOUBLE)                    :: CT0               ! Intermediate variable used in calc PTE stress recovery matrices
      REAL(DOUBLE)                    :: D1(3,3)           ! Output from subr ATRA, called herein (used in calc KE)
      REAL(DOUBLE)                    :: D2(3,3)           ! Output from subr ATRA, called herein (used in calc KE)
      REAL(DOUBLE)                    :: D3(3,3)           ! Output from subr ATRA, called herein (used in calc KE)
      REAL(DOUBLE)                    :: D4(3,3)           ! Output from subr ATRA, called herein (used in calc KE)
      REAL(DOUBLE)                    :: D5(3,3)           ! Output from subr ATRA, called herein (used in calc KE)
      REAL(DOUBLE)                    :: E1                ! Intermediate variable used in calc KE elem stiffness
      REAL(DOUBLE)                    :: E2                ! Intermediate variable used in calc KE elem stiffness
      REAL(DOUBLE)                    :: E4                ! Intermediate variable used in calc KE elem stiffness
      REAL(DOUBLE)                    :: EALP(3)           ! Intermed var used in calc STEi therm stress coeffs
      REAL(DOUBLE)                    :: L12S              ! Intermediate variable used in calc Pi, Ti, Qi, Ri variables
      REAL(DOUBLE)                    :: L23S              ! Intermediate variable used in calc Pi, Ti, Qi, Ri variables
      REAL(DOUBLE)                    :: L31S              ! Intermediate variable used in calc Pi, Ti, Qi, Ri variables
      REAL(DOUBLE)                    :: P4                ! Intermediate variable used in calc array S
      REAL(DOUBLE)                    :: P5                ! Intermediate variable used in calc array S
      REAL(DOUBLE)                    :: P6                ! Intermediate variable used in calc array S
      REAL(DOUBLE)                    :: Q4                ! Intermediate variable used in calc array S
      REAL(DOUBLE)                    :: Q5                ! Intermediate variable used in calc array S
      REAL(DOUBLE)                    :: R4                ! Intermediate variable used in calc array S
      REAL(DOUBLE)                    :: R5                ! Intermediate variable used in calc array S
      REAL(DOUBLE)                    :: S(3,9)            ! Column sums of the ALPHA-ij submatrices of the 4 dimensional array A
      REAL(DOUBLE)                    :: T4                ! Intermediate variable used in calc array S
      REAL(DOUBLE)                    :: T5                ! Intermediate variable used in calc array S
      REAL(DOUBLE)                    :: X12               ! Diff in x coords of elem nodes 1 and 2
      REAL(DOUBLE)                    :: X23               ! Diff in x coords of elem nodes 2 and 3
      REAL(DOUBLE)                    :: X31               ! Diff in x coords of elem nodes 3 and 1
      REAL(DOUBLE)                    :: Y23               ! Diff in y coords of elem nodes 2 and 3
      REAL(DOUBLE)                    :: Y31               ! Diff in y coords of elem nodes 3 and 1
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Generate element parameters
  
      AREAF = FOUR*TWELVE*AREA
      E1 = SHELL_D(1,1)/AREAF
      E2 = SHELL_D(1,2)/AREAF
      E4 = SHELL_D(3,3)/AREAF
 
      X12  = -X2E
      X31  =  X3E
      X23  =  X2E - X3E
      Y31  =  Y3E
      Y23  = -Y3E
      L12S =  X12*X12 
      L31S =  X31*X31 + Y23*Y23
      L23S =  X23*X23 + Y23*Y23
      P4   = -SIX*X23/L23S
      P5   = -SIX*X3E/L31S
      P6   = -SIX*X12/L12S
      T4   = -SIX*Y23/L23S
      T5   = -SIX*Y3E/L31S
      Q4   =  THREE*X23*Y23/L23S
      Q5   =  THREE*X3E*Y3E/L31S
      R4   =  THREE*Y23*Y23/L23S
      R5   =  THREE*Y31*Y31/L31S 
  
! **********************************************************************************************************************************
! Determine element thermal loads. 
 
      IF (OPT(2) == 'Y') THEN

         DO J=1,NTSUB
            CT0 = SHELL_DALP(1)*DT(4,J)/TWO
            PTE(4,J)  = CT0*X23
            PTE(5,J)  = CT0*Y23
            PTE(10,J) = CT0*X31
            PTE(11,J) = CT0*Y31
            PTE(16,J) = CT0*X12
         ENDDO 
      ENDIF
     
! **********************************************************************************************************************************
! Determine element pressure loads. 
 
      IF (OPT(5) == 'Y') THEN
         DO J=1,NSUB
            PPE( 3,J) = AREA*PRESS(3,J)/THREE
            PPE( 9,J) = AREA*PRESS(3,J)/THREE
            PPE(15,J) = AREA*PRESS(3,J)/THREE
         ENDDO 
      ENDIF
     
! **********************************************************************************************************************************
! Calculate column sums of the ALPHA-ij submatrices of the 4 dimensional array A. These are needed for SE2 ,STE2 and
! KE calculation. The sums are calculated explicitly instead of summing the ALPHA(i,j,k,l) terms calculated later since
! several terms cancel in the summation.
 
      IF ((OPT(3) == 'Y') .OR. (OPT(4) == 'Y')) THEN

         S(1,1) = Y3E*P5                                   ! ALPHA-11 Column sums
         S(1,2) =-Y3E*Q5
         S(1,3) =-Y3E*R5

         S(2,1) =-X3E*T5                                   ! ALPHA-21 Column sums 
         S(2,2) = X3E*R5 + THREE*X23
         S(2,3) =-X3E*Q5

         S(3,1) =-X3E*P5 - X2E*P6 + Y3E*T5                 ! ALPHA-31 Column sums
         S(3,2) = X3E*Q5 + Y3E*(THREE - R5)
         S(3,3) = X3E*R5 + Y3E*Q5

         S(1,4) = Y3E*P4                                   ! ALPHA-12 Column sums
         S(1,5) = Y3E*Q4
         S(1,6) = Y3E*R4

         S(2,4) = X23*T4                                   ! ALPHA-22 Column sums
         S(2,5) = X23*R4 + THREE*X3E
         S(2,6) =-X23*Q4

         S(3,4) = X23*P4 + X2E*P6 + Y3E*T4                 ! ALPHA-32 Column sums
         S(3,5) = X23*Q4 + Y3E*(R4 - THREE)
         S(3,6) = X23*R4 - Y3E*Q4

         S(1,7) =-Y3E*(P4 + P5)                            ! ALPHA-13 Column sums
         S(1,8) = Y3E*(Q4 - Q5)
         S(1,9) = Y3E*(R4 - R5)

         S(2,7) =-X23*T4 + X3E*T5                          ! ALPHA-23 Column sums
         S(2,8) = X23*R4 + X3E*R5 - THREE*X2E
         S(2,9) =-X23*Q4 - X3E*Q5

         S(3,7) =-X23*P4 + X3E*P5 - Y3E*(T4 + T5)          ! ALPHA-33 Column sums
         S(3,8) = X23*Q4 + X3E*Q5 + Y3E*(R4 - R5)
         S(3,9) = X23*R4 + X3E*R5 + Y3E*(Q5 - Q4)
  
      ENDIF
  
! **********************************************************************************************************************************
! Calculate SE2, STE2 matrices for stress data recovery.
! Note: stress recovery matrices only make sense for individual plies (or whole elem if only 1 "ply")
 
      IF (OPT(3) == 'Y') THEN
                                                           ! Strain recovery matrix BE2
         C11 = ONE/(SIX*AREA)
         C12 = ONE/(SIX*AREA)
         C21 = ONE/(SIX*AREA)
         C22 = ONE/(SIX*AREA)
         C33 = ONE/(SIX*AREA)
        
         BE2(1, 3,1) = (C11*S(1,1) + C12*S(2,1))
         BE2(1, 4,1) = (C11*S(1,2) + C12*S(2,2))
         BE2(1, 5,1) = (C11*S(1,3) + C12*S(2,3))
         BE2(1, 9,1) = (C11*S(1,4) + C12*S(2,4))
         BE2(1,10,1) = (C11*S(1,5) + C12*S(2,5))
         BE2(1,11,1) = (C11*S(1,6) + C12*S(2,6))
         BE2(1,15,1) = (C11*S(1,7) + C12*S(2,7))
         BE2(1,16,1) = (C11*S(1,8) + C12*S(2,8))
         BE2(1,17,1) = (C11*S(1,9) + C12*S(2,9))

         BE2(2, 3,1) = (C21*S(1,1) + C22*S(2,1))
         BE2(2, 4,1) = (C21*S(1,2) + C22*S(2,2))
         BE2(2, 5,1) = (C21*S(1,3) + C22*S(2,3))
         BE2(2, 9,1) = (C21*S(1,4) + C22*S(2,4))
         BE2(2,10,1) = (C21*S(1,5) + C22*S(2,5))
         BE2(2,11,1) = (C21*S(1,6) + C22*S(2,6))
         BE2(2,15,1) = (C21*S(1,7) + C22*S(2,7))
         BE2(2,16,1) = (C21*S(1,8) + C22*S(2,8))
         BE2(2,17,1) = (C21*S(1,9) + C22*S(2,9))

         BE2(3, 3,1) = C33*S(3,1)
         BE2(3, 4,1) = C33*S(3,2)
         BE2(3, 5,1) = C33*S(3,3)
         BE2(3, 9,1) = C33*S(3,4)
         BE2(3,10,1) = C33*S(3,5)
         BE2(3,11,1) = C33*S(3,6)
         BE2(3,15,1) = C33*S(3,7)
         BE2(3,16,1) = C33*S(3,8)
         BE2(3,17,1) = C33*S(3,9)

! SE2, STE2 generated in elem coords. Then, in LINK9 the stresses, calc'd in elem coords, will be transformed to ply coords

         C11 = EB(1,1)/(SIX*AREA)
         C12 = EB(1,2)/(SIX*AREA)
         C21 = EB(2,1)/(SIX*AREA)
         C22 = EB(2,2)/(SIX*AREA)
         C33 = EB(3,3)/(SIX*AREA)

         SE2(1, 3,1) = (C11*S(1,1) + C12*S(2,1))
         SE2(1, 4,1) = (C11*S(1,2) + C12*S(2,2))
         SE2(1, 5,1) = (C11*S(1,3) + C12*S(2,3))
         SE2(1, 9,1) = (C11*S(1,4) + C12*S(2,4))
         SE2(1,10,1) = (C11*S(1,5) + C12*S(2,5))
         SE2(1,11,1) = (C11*S(1,6) + C12*S(2,6))
         SE2(1,15,1) = (C11*S(1,7) + C12*S(2,7))
         SE2(1,16,1) = (C11*S(1,8) + C12*S(2,8))
         SE2(1,17,1) = (C11*S(1,9) + C12*S(2,9))

         SE2(2, 3,1) = (C21*S(1,1) + C22*S(2,1))
         SE2(2, 4,1) = (C21*S(1,2) + C22*S(2,2))
         SE2(2, 5,1) = (C21*S(1,3) + C22*S(2,3))
         SE2(2, 9,1) = (C21*S(1,4) + C22*S(2,4))
         SE2(2,10,1) = (C21*S(1,5) + C22*S(2,5))
         SE2(2,11,1) = (C21*S(1,6) + C22*S(2,6))
         SE2(2,15,1) = (C21*S(1,7) + C22*S(2,7))
         SE2(2,16,1) = (C21*S(1,8) + C22*S(2,8))
         SE2(2,17,1) = (C21*S(1,9) + C22*S(2,9))

         SE2(3, 3,1) = C33*S(3,1)
         SE2(3, 4,1) = C33*S(3,2)
         SE2(3, 5,1) = C33*S(3,3)
         SE2(3, 9,1) = C33*S(3,4)
         SE2(3,10,1) = C33*S(3,5)
         SE2(3,11,1) = C33*S(3,6)
         SE2(3,15,1) = C33*S(3,7)
         SE2(3,16,1) = C33*S(3,8)
         SE2(3,17,1) = C33*S(3,9)

         ALP(1) = ALPVEC(1,1)
         ALP(2) = ALPVEC(2,1)
         ALP(3) = ALPVEC(3,1)

         CALL MATMULT_FFF ( EB, ALP, 3, 3, 1, EALP )
         DO J=1,NTSUB
            DO I=1,3
               STE2(I,J,1) = EALP(I)*DT(4,J)
            ENDDO 
         ENDDO 
      ENDIF
  
! **********************************************************************************************************************************
! Determine ALPHA-ij matrices needed for KE
  
      IF(OPT(4) == 'Y') THEN

         ALPHA(1,1,1,1) = Y3E*P6                           ! ALPHA-11, Col 1
         ALPHA(2,1,1,1) =-ALPHA(1,1,1,1) 
         ALPHA(3,1,1,1) = Y3E*P5

         ALPHA(1,2,1,1) = ZERO                             !    "      Col 2
         ALPHA(2,2,1,1) = ZERO
         ALPHA(3,2,1,1) =-Y3E*Q5 

         ALPHA(1,3,1,1) =-FOUR*Y3E                         !    "      Col 3
         ALPHA(2,3,1,1) = Y3E + Y3E
         ALPHA(3,3,1,1) = Y3E*(TWO - R5)
  
         ALPHA(1,1,2,1) =-X2E*T5                           ! ALPHA-21, Col 1
         ALPHA(2,1,2,1) = ZERO 
         ALPHA(3,1,2,1) = X23*T5 

         ALPHA(1,2,2,1) = X23 + X2E*R5                     !    "      Col 2
         ALPHA(2,2,2,1) = X23 
         ALPHA(3,2,2,1) = X23*(ONE - R5) 

         ALPHA(1,3,2,1) =-X2E*Q5                           !    "      Col 3
         ALPHA(2,3,2,1) = ZERO 
         ALPHA(3,3,2,1) = X23*Q5  
  
         ALPHA(1,1,3,1) =-X3E*P6 - X2E*P5                  ! ALPHA-31, Col 1  
         ALPHA(2,1,3,1) =-X23*P6 
         ALPHA(3,1,3,1) = X23*P5 + Y3E*T5 

         ALPHA(1,2,3,1) = X2E*Q5 + Y3E                     !    "      Col 2
         ALPHA(2,2,3,1) = Y3E 
         ALPHA(3,2,3,1) = -X23*Q5 + (ONE - R5)*Y3E

         ALPHA(1,3,3,1) = -FOUR*X23 +X2E*R5                !    "      Col 2
         ALPHA(2,3,3,1) = X23 + X23
         ALPHA(3,3,3,1) = (TWO - R5)*X23 + Y3E*Q5
  
         ALPHA(1,1,1,2) =-Y3E*P6                           ! ALPHA-12, Col 1
         ALPHA(2,1,1,2) = Y3E*P6
         ALPHA(3,1,1,2) = Y3E*P4 

         ALPHA(1,2,1,2) = ZERO                             !    "      Col 2 
         ALPHA(2,2,1,2) = ZERO 
         ALPHA(3,2,1,2) = Y3E*Q4 

         ALPHA(1,3,1,2) =-Y3E - Y3E                        !    "      Col 3
         ALPHA(2,3,1,2) = FOUR*Y3E
         ALPHA(3,3,1,2) = Y3E*(R4 - TWO)
 
         ALPHA(1,1,2,2) = ZERO                             ! ALPHA-22, Col 1 
         ALPHA(2,1,2,2) = X2E*T4 
         ALPHA(3,1,2,2) =-X3E*T4 

         ALPHA(1,2,2,2) = X3E                              !    "      Col 2 
         ALPHA(2,2,2,2) = X3E + X2E*R4 
         ALPHA(3,2,2,2) = X3E*(ONE - R4) 

         ALPHA(1,3,2,2) = ZERO                             !    "      Col 3  
         ALPHA(2,3,2,2) =-X2E*Q4
         ALPHA(3,3,2,2) = X3E*Q4 
  
         ALPHA(1,1,3,2) = X3E*P6                           ! ALPHA-32, Col 1  
         ALPHA(2,1,3,2) = X23*P6 + X2E*P4
         ALPHA(3,1,3,2) =-X3E*P4 + Y3E*T4 

         ALPHA(1,2,3,2) =-Y3E                              !    "      Col 2 
         ALPHA(2,2,3,2) = -Y3E + X2E*Q4
         ALPHA(3,2,3,2) = (R4 - ONE)*Y3E - X3E*Q4

         ALPHA(1,3,3,2) = X3E + X3E                        !    "      Col 3 
         ALPHA(2,3,3,2) = -FOUR*X3E + X2E*R4
         ALPHA(3,3,3,2) = (TWO - R4)*X3E - Y3E*Q4
  
         ALPHA(1,1,1,3) = ZERO                             ! ALPHA-13, Col 1
         ALPHA(2,1,1,3) = ZERO 
         ALPHA(3,1,1,3) = -Y3E*(P4 + P5)

         ALPHA(1,2,1,3) = ZERO                             !    "      Col 2 
         ALPHA(2,2,1,3) = ZERO 
         ALPHA(3,2,1,3) = Y3E*(Q4 - Q5)

         ALPHA(1,3,1,3) = ZERO                             !    "      Col 3 
         ALPHA(2,3,1,3) = ZERO 
         ALPHA(3,3,1,3) = Y3E*(R4 - R5)
  
         ALPHA(1,1,2,3) = X2E*T5                           ! ALPHA-23, Col 1
         ALPHA(2,1,2,3) =-X2E*T4
         ALPHA(3,1,2,3) =-X23*T5 + X3E*T4

         ALPHA(1,2,2,3) = X2E*(R5 - ONE)                   !    "      Col 2 
         ALPHA(2,2,2,3) = X2E*(R4 - ONE) 
         ALPHA(3,2,2,3) =-X23*R5 - X3E*R4 - X2E 

         ALPHA(1,3,2,3) =-X2E*Q5                           !    "      Col 3
         ALPHA(2,3,2,3) =-X2E*Q4
         ALPHA(3,3,2,3) = X3E*Q4 + X23*Q5
 
         ALPHA(1,1,3,3) = X2E*P5                           ! ALPHA-33, Col 1
         ALPHA(2,1,3,3) =-X2E*P4 
         ALPHA(3,1,3,3) =-X23*P5 + X3E*P4 - Y3E*(T4 + T5)

         ALPHA(1,2,3,3) = X2E*Q5                           !    "      Col 2 
         ALPHA(2,2,3,3) = X2E*Q4
         ALPHA(3,2,3,3) =-X23*Q5 - X3E*Q4 + Y3E*(R4 - R5)

         ALPHA(1,3,3,3) = X2E*(R5 - TWO)                   !    "      Col 3 
         ALPHA(2,3,3,3) = X2E*(R4 - TWO)
         ALPHA(3,3,3,3) =-X23*R5 - X3E*R4 + FOUR*X2E + Y3E*(Q5 - Q4)
 
! Calculate the 9 - 3x3 partitions of the element stiffness matrix. Since it is symmetric, only 6 of the 3x3's need to
! be calculated. These 3x3's are put into a global size stiffness matrix for this element which has 18 global DOF.
! The only nonzero's are for DOF's 3,4,5. The resulting 18x18 matrix is in elem coords for the 6 DOF's per grid point.
 
! Each of the 6 - 3x3's has 5 terms in it. Each of these 5 terms has a triple matrix product consisting of:
!                     T
!           (ALPHA-mi) R (ALPHA-kj)
  
! where i,j range over the 3 grid points to which the elem connects and m,k are 1,1  2,1  1,2  2,2  3,3 for the 5 terms
! for each i,j pair.
  
         DO I=1,3

            I1 = 3*I - 2
            I2 = 6*I - 4

            DO J=I,3

               J1 = 6*J - 4
                                                           ! Calculate the 5 triple matrix products using subroutine ATRA (explicit)
               CALL ATRA ( ALPHA(1,1,1,I), ALPHA(1,1,1,J), S(1,I1), S(1,I1+1), S(1,I1+2), D1 )
              
               CALL ATRA ( ALPHA(1,1,2,I), ALPHA(1,1,1,J), S(2,I1), S(2,I1+1), S(2,I1+2), D2 )
                
               CALL ATRA ( ALPHA(1,1,2,I), ALPHA(1,1,2,J), S(2,I1), S(2,I1+1), S(2,I1+2), D4 )
                
               CALL ATRA ( ALPHA(1,1,3,I), ALPHA(1,1,3,J), S(3,I1), S(3,I1+1), S(3,I1+2), D5 )

               IF(I == J) THEN                             ! Calculate the 3x3 element stiffness matrix partition for I,J

                  DO K=1,3
                     K1 = K + I2
                     KE(K1,J1+1) = KE(K1,J1+1) + E1*(D1(K,1)+D4(K,1)) + E2*(D2(K,1)+D2(1,K)) + E4*D5(K,1)
                     KE(K1,J1+2) = KE(K1,J1+2) + E1*(D1(K,2)+D4(K,2)) + E2*(D2(K,2)+D2(2,K)) + E4*D5(K,2)
                     KE(K1,J1+3) = KE(K1,J1+3) + E1*(D1(K,3)+D4(K,3)) + E2*(D2(K,3)+D2(3,K)) + E4*D5(K,3)
                  ENDDO 
 
               ELSE

                  CALL ATRA ( ALPHA(1,1,1,I), ALPHA(1,1,2,J), S(1,I1), S(1,I1+1), S(1,I1+2), D3 )
                  DO K=1,3
                     K1 = K + I2
                     KE(K1,J1+1) = KE(K1,J1+1) + E1*(D1(K,1)+D4(K,1)) + E2*(D2(K,1)+D3(K,1)) + E4*D5(K,1)
                     KE(K1,J1+2) = KE(K1,J1+2) + E1*(D1(K,2)+D4(K,2)) + E2*(D2(K,2)+D3(K,2)) + E4*D5(K,2)
                     KE(K1,J1+3) = KE(K1,J1+3) + E1*(D1(K,3)+D4(K,3)) + E2*(D2(K,3)+D3(K,3)) + E4*D5(K,3)
                  ENDDO 
   
               ENDIF

            ENDDO 
   
         ENDDO 

         DO I=2,18                                         ! Calculate sub-diagonal portion by enforcing symmetry 
            DO J=1,I-1
               KE(I,J) = KE(J,I)
            ENDDO 
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

! ######################################################################
 
      CONTAINS
 
! ######################################################################
  
      SUBROUTINE ATRA ( A1, A2, SL1, SL2, SL3, D )
  
! Subroutine to calculate the triple matrix product, below, needed for the DKT elem stiff matrix:

!                       T
!             (ALPHA-mi) R (ALPHA-kj)

! The product is evaluated explicitly since R is a simple form. R is a 3x3 matrix whose diagonals are all 2.0 and all
! other terms are 1.0
  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  TPLT1_BEGEND
  
      IMPLICIT NONE 
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ATRA'

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = TPLT1_BEGEND + 1

      REAL(DOUBLE) , INTENT(IN)       :: A1(3,3)           ! ALPHA-mi matrix
      REAL(DOUBLE) , INTENT(IN)       :: A2(3,3)           ! ALPHA-kj matrix
      REAL(DOUBLE) , INTENT(IN)       :: SL1               ! The 3 column sums of ALPHA-mi (k=1 column)
      REAL(DOUBLE) , INTENT(IN)       :: SL2               ! The 3 column sums of ALPHA-mi (k=2 column)
      REAL(DOUBLE) , INTENT(IN)       :: SL3               ! The 3 column sums of ALPHA-mi (k=3 column)
      REAL(DOUBLE) , INTENT(OUT)      :: D(3,3)            ! Triple matrix product discussed above
      REAL(DOUBLE)                    :: W11               ! Intermediate variable used in calculating array D
      REAL(DOUBLE)                    :: W12               ! Intermediate variable used in calculating array D
      REAL(DOUBLE)                    :: W13               ! Intermediate variable used in calculating array D
      REAL(DOUBLE)                    :: W21               ! Intermediate variable used in calculating array D
      REAL(DOUBLE)                    :: W22               ! Intermediate variable used in calculating array D
      REAL(DOUBLE)                    :: W23               ! Intermediate variable used in calculating array D
      REAL(DOUBLE)                    :: W31               ! Intermediate variable used in calculating array D
      REAL(DOUBLE)                    :: W32               ! Intermediate variable used in calculating array D
      REAL(DOUBLE)                    :: W33               ! Intermediate variable used in calculating array D
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Wij are the values in ALPHA-mi (transpose) times R
  
      W11 = A1(1,1) + SL1
      W12 = A1(2,1) + SL1
      W13 = A1(3,1) + SL1
  
      W21 = A1(1,2) + SL2
      W22 = A1(2,2) + SL2
      W23 = A1(3,2) + SL2
  
      W31 = A1(1,3) + SL3
      W32 = A1(2,3) + SL3
      W33 = A1(3,3) + SL3
  
! D is the triple matrix product ALPHA-mi (transp) R ALPHA-kj
  
      D(1,1) = W11*A2(1,1) + W12*A2(2,1) + W13*A2(3,1)
      D(1,2) = W11*A2(1,2) + W12*A2(2,2) + W13*A2(3,2)
      D(1,3) = W11*A2(1,3) + W12*A2(2,3) + W13*A2(3,3)
  
      D(2,1) = W21*A2(1,1) + W22*A2(2,1) + W23*A2(3,1)
      D(2,2) = W21*A2(1,2) + W22*A2(2,2) + W23*A2(3,2)
      D(2,3) = W21*A2(1,3) + W22*A2(2,3) + W23*A2(3,3)
  
      D(3,1) = W31*A2(1,1) + W32*A2(2,1) + W33*A2(3,1)
      D(3,2) = W31*A2(1,2) + W32*A2(2,2) + W33*A2(3,2)
      D(3,3) = W31*A2(1,3) + W32*A2(2,3) + W33*A2(3,3)
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

      END SUBROUTINE ATRA
 
      END SUBROUTINE TPLT1
