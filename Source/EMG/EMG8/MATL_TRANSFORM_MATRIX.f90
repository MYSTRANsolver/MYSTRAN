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

      SUBROUTINE MATL_TRANSFORM_MATRIX ( T21, TS )

! Formulates 6x6 transformation matrix TS which can do a 2D plane coord transformation for stresses, strains and material matrices

! TS is formed from the coefficients in 3x3 coord transformation matrix T21 where T21 is the coord transformation matrix that
! transforms a unit vector in coord sys 1 (U1) to a unit vector in coord sys 2 (U2):

!                                                  U2 = T21*U1                                                         (1)

! For a plane coord transformation (2D, not 3D)

!                                                       |  cos(THETA)  sin(THETA)  0 |
!                                                 T21 = | -sin(THETA)  cos(THETA)  0 |                                 (2)
!                                                       |      0           0       1 |
 
! The stresses in any coord sys is a tensor (call it S) that is written as:
!                                                    ~
!                                                       | Sxx  Sxy  Sxz |
!                                                   S = | Syx  Syy  Syz |                                              (3)
!                                                   ~   | Szx  Szy  Szz |

! Matrix T21 is used to transform stress tensors from coord sys 1 to coord sys 2 via the equation (NOTE: T21' indicates
! the transpose of matrix T21 which is also T12):


!                                                 S22 = T21'*S11*T21 = T12*S11*T21                                     (4)
!                                                 ~          ~             ~

! The terms in this triple matrix product can be used to rewrite the stress transformation with the 6 independent stresses
! in a vector (instead of as the 3x3 stress tensor). To this end, define the vector:

!                                                       | Sxx | 
!                                                       | Syy |
!                                                   S = | Szz |                                                        (5)
!                                                       | Sxy |
!                                                       | Syz |
!                                                       | Szx |

! Then the relationship between S in coord sys 2 and that in coord sys 1 can be written as:

!                                                --------------
!                                               |              |
!                                               |  S2 = TS*S1  |                                                       (6)
!                                               |              |
!                                                --------------

! where TS is a 6x6 matrix of coefficients from T12 (eqn (2)). The 9 terms in T21 are denoted as Aij and the 36 terms in TS,
! developed from the T21 using eqn (4), are developed in the code below. Again, they are developed by expanding the triple
! matrix product (symbolically) in eqn (4) and then rewriting the 6 independent of the resulting 9 equations in the 6x6
! matrix form of (6).

! Matrix TS can also be used to rotate the material matrices from coord sys 1 to coord sys 2 as follows. 
! The stress-strain relations in coord sys 1 and 2 are:

!                                                  S1 = E1*e1
!                                                                                                                      (7)
!                                                  S2 = E2*e2

! where ei are strains and Ei is the material matrix in coord sys 1 and 2. Eqn 6 shows how to transform stresses. It is
! the same for strains so that eqns (7) can be written:

!                                                  S2 = TS*S1 = TS*E1*e1                                               (8)

! but e1 in terms of e2, using the same logic in eqn(6) is

!                                                  e1 = TS'*e2                                                         (9)

! Substituting 9 into 8 yields:

!                                                  S2 = (Ts*E1*TS')*e2 = E2*e2                                         (10)

! so that we conclude:

!                                                ------------------
!                                               |                  |
!                                               |  E2 = TS*E1*TS'  |                                                   (11)                 
!                                               |                  |
!                                                ------------------
 
! ----------------------------------------------------------------------------------------------------------------------------------

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  MATL_TRANSFORM_MATRIX_BEGEND

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MATL_TRANSFORM_MATRIX'

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MATL_TRANSFORM_MATRIX_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: T21(3,3)          ! 3x3 matrix that transforms a vector in coord sys 1 to coord sys 2
      REAL(DOUBLE), INTENT(OUT)       :: TS(6,6)           ! 6x6 stress transformation matrix
      REAL(DOUBLE)                    :: A11,A12,A13       ! Coefficients from matrix TME
      REAL(DOUBLE)                    :: A21,A22,A23       ! Coefficients from matrix TME
      REAL(DOUBLE)                    :: A31,A32,A33       ! Coefficients from matrix TME

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Formulate 6x6 matrix TS from terms in 3x3 matrix T12

      A11 = T21(1,1)     ;     A12 = T21(1,2)     ;     A13 = T21(1,3)
      A21 = T21(2,1)     ;     A22 = T21(2,2)     ;     A23 = T21(2,3)
      A31 = T21(3,1)     ;     A32 = T21(3,2)     ;     A33 = T21(3,3)

! Matrix TS is formed from the coefficients in matrix T12 (denoted as Aij coefficients)

! Row 1 of TS:

      TS(1,1)= A11*A11           ;   TS(1,2)= A21*A21           ;   TS(1,3)= A31*A31; 
      TS(1,4)= 2*A11*A21         ;   TS(1,5)= 2*A21*A31         ;   TS(1,6)= 2*A11*A31

! Row 2 of TS:

      TS(2,1)= A12*A12           ;   TS(2,2)= A22*A22           ;   TS(2,3)= A32*A32; 
      TS(2,4)= 2*A12*A22         ;   TS(2,5)= 2*A22*A32         ;   TS(2,6)= 2*A12*A32

! Row 3 of TS:

      TS(3,1)= A13*A13           ;   TS(3,2)= A23*A23           ;   TS(3,3)= A33*A33; 
      TS(3,4)= 2*A13*A23         ;   TS(3,5)= 2*A23*A33         ;   TS(3,6)= 2*A13*A33

! Row 4 of TS:

      TS(4,1)= A11*A12           ;   TS(4,2)= A21*A22           ;   TS(4,3)= A31*A32; 
      TS(4,4)= A11*A22+A21*A12   ;   TS(4,5)= A21*A32+A31*A22   ;   TS(4,6)= A11*A32+A31*A12

! Row 5 of TS:

      TS(5,1)= A12*A13           ;   TS(5,2)= A22*A23           ;   TS(5,3)= A32*A33; 
      TS(5,4)= A12*A23+A22*A13   ;   TS(5,5)= A22*A33+A32*A23   ;   TS(5,6)= A12*A33+A32*A13

! Row 6 of TS:

      TS(6,1)= A13*A11           ;   TS(6,2)= A23*A21           ;   TS(6,3)= A33*A31; 
      TS(6,4)= A13*A21+A23*A11   ;   TS(6,5)= A23*A31+A33*A21   ;   TS(6,6)= A13*A31+A33*A11

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
99664 format('  Transformation matrix TS: transforms 6 stress and 6x6 matl matrices from matl to elem axes')

99667 format(3(1es14.6),4x,3(1es14.6))                                                                                            

! **********************************************************************************************************************************
 
      END SUBROUTINE MATL_TRANSFORM_MATRIX

