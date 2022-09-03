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

      SUBROUTINE ROT_COMP_ELEM_AXES ( IPLY, THETA, DIRECTION )

! Rotates axes of a ply material and CTE matrices in ply coords to coords along and perpendicular to element material axes, or
! vice versa, depending on input arg DIRECTION

! Ref 1: "Stress Tensor coord transform.doc" my derivation documented in a WORD file located in \MYSTRAN\Documentation (this would
! be T1P in the code below)

! Ref 2: "Practical Analysis of Composites" by J.N. Reddy and A. Miravete, CRC Press, 1995 section 3.3. Eqn 7 in Ref 2 is matrix T1
! whereas my matrix is T1P (in the code below). Note that Ref 2 stress tensor definition and mine have a different order:

!      -------------------------------------------------------------------------  
!     | Ref 2 stress tensor for matrix T1 | MYSTRAN stress tensor for matrix T1P|
!     |-----------------------------------|-------------------------------------|
!     |         sig-1 = sig-xx            |            sig-1 = sig-xx           |
!     |         sig-2 = sig-yy            |            sig-2 = sig-yy           |
!     |         sig-3 = sig-zz            |            sig-3 = sig-zz           |
!     |         sig-4 = sig-yz            |            sig-4 = sig-xy           |
!     |         sig-5 = sig-zx            |            sig-5 = sig-yz           |
!     |         sig-6 = sig-xy            |            sig-6 = sig-zx           |
!      -------------------------------------------------------------------------

! Thus, rows and cols 4 and 6 are transposed between Ref 2 matrix T1 and the code here for matrix T1P

! Terms from T1 transform the stresses (in vector, not tensor format) and material matrix and terms from T1' transform strains (in
! vector, not tensor format)

! Note that we can get these terms from the T1 matrix in Ref 2 (rather than the procedure outlined in subr ROT_AXES_MATL_TO_LOC)
! since the transformation here involves only one coord transformation, not 2 in subr ROT_AXES_MATL_TO_LOC (which rotates from
! material to local via basic system since we can't go directly from material to local in that case)

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MEMATC 
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  CONV_DEG_RAD, ZERO, HALF, ONE, TWO, FOUR
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE MODEL_STUF, ONLY            :  ALPVEC, EB, EM, ET, EBM, INTL_MID, MTRL_TYPE, STRESS, STRAIN, T1P, T1M, T1T, T2P, T2M, T2T

      USE SUBR_BEGEND_LEVELS, ONLY    :  ROT_COMP_ELEM_AXES_BEGEND

      USE ROT_COMP_ELEM_AXES_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ROT_COMP_ELEM_AXES'
      CHARACTER(LEN=*), INTENT(IN)    :: DIRECTION         ! =1-2, rotate from ply to elem mat'l axes (when gen ABD matrices)
!                                                            =2-1, rotate stress from elem mat'l to ply axes (when recov stresses)

      INTEGER(LONG), INTENT(IN)       :: IPLY              ! Ply number
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ROT_COMP_ELEM_AXES_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: THETA             ! Orient angle of long dir of ply i wrt matl axis for the composite elem
      REAL(DOUBLE)                    :: ALP3(3,MEMATC)    ! The 3 rows of ALPVEC for membrane strains
      REAL(DOUBLE)                    :: C,S,SC            ! COS, SIN, SIN*COS of RADIANS_ROT
      REAL(DOUBLE)                    :: C2,S2             ! C*C, S*S
      REAL(DOUBLE)                    :: RADIANS_ROT       ! Angle (radians) to rotate from elem material axes to ply axes
      REAL(DOUBLE)                    :: DUM1(3)           ! Intermediate matrix
      REAL(DOUBLE)                    :: DUM2(3)           ! Intermediate matrix
      REAL(DOUBLE)                    :: DUM3(3)           ! Intermediate matrix
      REAL(DOUBLE)                    :: DUM33(3,3)        ! Intermediate matrix
      REAL(DOUBLE)                    :: DUM22(2,2)        ! Intermediate matrix
      REAL(DOUBLE)                    :: DUM3M(3,MEMATC)   ! Intermediate matrix
      REAL(DOUBLE)                    :: STRESS1(3)        ! Rows 1-3 of STRESS
      REAL(DOUBLE)                    :: STRESS2(3)        ! Rows 4-6 of STRESS
      REAL(DOUBLE)                    :: STRESS3(3)        ! Rows 7-9 of STRESS
      REAL(DOUBLE)                    :: STRAIN1(3)        ! Rows 1-3 of STRAIN
      REAL(DOUBLE)                    :: STRAIN2(3)        ! Rows 4-6 of STRAIN
      REAL(DOUBLE)                    :: STRAIN3(3)        ! Rows 7-9 of STRAIN
      REAL(DOUBLE)                    :: T1Mt(3,3)         ! Transpose of T1M
      REAL(DOUBLE)                    :: T1Tt(2,2)         ! Transpose of T1T
      REAL(DOUBLE)                    :: T1T3(3,3)         ! T1T expanded to 3x3
      REAL(DOUBLE)                    :: T2T3(3,3)         ! T2T expanded to 3x3
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Calc T1P matrix from eqn 3.3-7 in Ref 1. (with order 1,2,3,4,5,6 changed to 1,2,3,6,4,5 to account for the fact that Ref (1) has
! the 6 position for xy stress whereas it is the 4th position here) 

      RADIANS_ROT = CONV_DEG_RAD*THETA                     ! THETA is angle (deg) from elem matl axis to ply K longitudinal axis
      C  = DCOS(RADIANS_ROT)
      S  = DSIN(RADIANS_ROT)
      SC = S*C
      C2 = C*C
      S2 = S*S

! Transformation matrix T1P is 6x6 for transforming 3D solid material matrices from ply coords to element coords. Notice that this
! is the same as matrix T1 in Ref 2 except that the ordering of rows and cols is different since my definition of the stress tensor
! has a different ordering than Ref 2

      T1P(1,1) =  C2    ;  T1P(1,2) =  S2    ;  T1P(1,3) =  ZERO  ;  T1P(1,4) = -TWO*SC   ;  T1P(1,5) =  ZERO  ;  T1P(1,6) =  ZERO

      T1P(2,1) =  S2    ;  T1P(2,2) =  C2    ;  T1P(2,3) =  ZERO  ;  T1P(2,4) =  TWO*S*C  ;  T1P(2,5) =  ZERO  ;  T1P(2,6) =  ZERO

      T1P(3,1) =  ZERO  ;  T1P(3,2) =  ZERO  ;  T1P(3,3) =  ONE   ;  T1P(3,4) =  ZERO     ;  T1P(3,5) =  ZERO  ;  T1P(3,6) =  ZERO

      T1P(4,1) =  SC    ;  T1P(4,2) = -SC    ;  T1P(4,3) =  ZERO  ;  T1P(4,4) =  C2 - S2  ;  T1P(4,5) =  ZERO  ;  T1P(4,6) =  ZERO

      T1P(5,1) =  ZERO  ;  T1P(5,2) =  ZERO  ;  T1P(5,3) =  ZERO  ;  T1P(5,4) =  ZERO     ;  T1P(5,5) =  C     ;  T1P(5,6) =  S

      T1P(6,1) =  ZERO  ;  T1P(6,2) =  ZERO  ;  T1P(6,3) =  ZERO  ;  T1P(6,4) =  ZERO     ;  T1P(6,5) = -S     ;  T1P(6,6) =  C

! T1M and T1T are portions of T1P for membrane and transverse shear (for 2D shell elements)

      T1M(1,1) = T1P(1,1)   ;   T1M(1,2) = T1P(1,2)   ;   T1M(1,3) = T1P(1,4)
      T1M(2,1) = T1P(2,1)   ;   T1M(2,2) = T1P(2,2)   ;   T1M(2,3) = T1P(2,4)
      T1M(3,1) = T1P(4,1)   ;   T1M(3,2) = T1P(4,2)   ;   T1M(3,3) = T1P(4,4)

      T1T(1,1) = T1P(5,5)   ;   T1T(1,2) = T1P(6,5)
      T1T(2,1) = T1P(5,6)   ;   T1T(2,2) = T1P(6,6)

! Transformation matrix T2P is the inverse of T1P.

      T2P(1,1) =  C2    ;  T2P(1,2) =  S2    ;  T2P(1,3) =  ZERO  ;  T2P(1,4) =  TWO*SC   ;  T2P(1,5) =  ZERO  ;  T2P(1,6) =  ZERO

      T2P(2,1) =  S2    ;  T2P(2,2) =  C2    ;  T2P(2,3) =  ZERO  ;  T2P(2,4) = -TWO*S*C  ;  T2P(2,5) =  ZERO  ;  T2P(2,6) =  ZERO

      T2P(3,1) =  ZERO  ;  T2P(3,2) =  ZERO  ;  T2P(3,3) =  ONE   ;  T2P(3,4) =  ZERO     ;  T2P(3,5) =  ZERO  ;  T2P(3,6) =  ZERO

      T2P(4,1) = -SC    ;  T2P(4,2) =  SC    ;  T2P(4,3) =  ZERO  ;  T2P(4,4) =  C2 - S2  ;  T2P(4,5) =  ZERO  ;  T2P(4,6) =  ZERO

      T2P(5,1) =  ZERO  ;  T2P(5,2) =  ZERO  ;  T2P(5,3) =  ZERO  ;  T2P(5,4) =  ZERO     ;  T2P(5,5) =  C     ;  T2P(5,6) = -S

      T2P(6,1) =  ZERO  ;  T2P(6,2) =  ZERO  ;  T2P(6,3) =  ZERO  ;  T2P(6,4) =  ZERO     ;  T2P(6,5) =  S     ;  T2P(6,6) =  C

!  T2M and T2T are portions of T2P for membrane and transverse shear (for 2D shell elements)

      T2M(1,1) = T2P(1,1)   ;   T2M(1,2) = T2P(1,2)   ;   T2M(1,3) = T2P(1,4)
      T2M(2,1) = T2P(2,1)   ;   T2M(2,2) = T2P(2,2)   ;   T2M(2,3) = T2P(2,4)
      T2M(3,1) = T2P(4,1)   ;   T2M(3,2) = T2P(4,2)   ;   T2M(3,3) = T2P(4,4)

      T2T(1,1) = T2P(5,5)   ;   T2T(1,2) = T2P(5,6)
      T2T(2,1) = T2P(6,5)   ;   T2T(2,2) = T2P(6,6)

! Do transformation

      IF      (DIRECTION == '1-2') THEN                    ! Transform material and CTE matrices from ply to elem material axes
                                                           ! (1) Membrane matl matrix
         DO I=1,3
            DO J=1,3
               T1Mt(I,J) = T1M(J,I)
            ENDDO
         ENDDO

         DO I=1,2
            DO J=1,2
               T1Tt(I,J) = T1T(J,I)
            ENDDO
         ENDDO

         DO I=1,3                                          ! Transform ALPVEC rows for ply membrane from ply to elem axes
            DO J=1,MEMATC
               ALP3(I,J) = ALPVEC(I,J)
            ENDDO
         ENDDO

         CALL MATMULT_FFF ( T1M   , EM  , 3, 3, 3, DUM33 ) ! (1) Transform EM  membrane matl matrix from ply to elem coords
         CALL MATMULT_FFF ( DUM33 , T1Mt, 3, 3, 3, EM)
                                                          
         CALL MATMULT_FFF ( T1M   , EB  , 3, 3, 3, DUM33 ) ! (2) Transform EB  bending matl matrix from ply to elem coords
         CALL MATMULT_FFF ( DUM33 , T1Mt, 3, 3, 3, EB)
                                                          
         CALL MATMULT_FFF ( T1T   , ET  , 2, 2, 2, DUM22 ) ! (3) Transform TM  transverse shear matl matrix from ply to elem coords
         CALL MATMULT_FFF ( DUM22 , T1Tt, 2, 2, 2, ET)
                                                          
         CALL MATMULT_FFF ( T1M   , EBM , 3, 3, 3, DUM33 ) ! (4) Transform EBM bending/membrane matl matrix from ply to elem coords
         CALL MATMULT_FFF ( DUM33 , T1Mt, 3, 3, 3, EBM)

         CALL MATMULT_FFF_T ( T2M, ALP3, 3, 3, MEMATC, DUM3M )

         DO I=1,6
            DO J=1,MEMATC
               ALPVEC(I,J) = ZERO
            ENDDO
         ENDDO
         DO I=1,3
            DO J=1,MEMATC
               ALPVEC(I,J) = DUM3M(I,J)
            ENDDO
         ENDDO


      ELSE IF (DIRECTION == '2-1') THEN                    ! Transform stresses in elem material axes to ply axes

         DO I=1,3                                          ! Initialize T2T3, T2T3
            DO J=1,3
               T1T3(I,J) = ZERO
               T2T3(I,J) = ZERO
            ENDDO
         ENDDO

         DO I=1,2                                          ! Load all 2x2 of T1T into upper 2x2 of T1T3
            DO J=1,2
!zzzz          T1T3  = T1T(I,J)
               T1T3(I,J)  = T1T(I,J)
            ENDDO
         ENDDO
         T1T3(3,3)  = ZERO

         DO I=1,2                                          ! Load all 2x2 of T2T into upper 2x2 of T2T3
            DO J=1,2
!zzzz          T2T3  = T2T(I,J)
               T2T3(I,J)  = T2T(I,J)
            ENDDO
         ENDDO
         T2T3(3,3)  = ZERO


         DO I=1,3                                          ! Transform axes on stress (in vector form) from elem to ply axes
            DUM1(I) = STRESS(I)
            DUM2(I) = STRESS(I+3)
            DUM3(I) = STRESS(I+6)
         ENDDO

         CALL MATMULT_FFF ( T2M , DUM1, 3, 3, 1, STRESS1 )
         CALL MATMULT_FFF ( T2M , DUM2, 3, 3, 1, STRESS2 )
         CALL MATMULT_FFF ( T2T3, DUM3, 3, 3, 1, STRESS3 )

         DO I=1,3
            STRESS(I)   = STRESS1(I)
            STRESS(I+3) = STRESS2(I)
            STRESS(I+6) = STRESS3(I)
         ENDDO

         DO I=1,3                                          ! Transform axes on strain (in vector form) from elem to ply axes
            DUM1(I) = STRAIN(I)
            DUM2(I) = STRAIN(I+3)
            DUM3(I) = STRAIN(I+6)
         ENDDO

         CALL MATMULT_FFF_T ( T1M , DUM1, 3, 3, 1, STRAIN1 )
         CALL MATMULT_FFF_T ( T1M , DUM2, 3, 3, 1, STRAIN2 )
         CALL MATMULT_FFF_T ( T1T3, DUM3, 3, 3, 1, STRAIN3 )

         DO I=1,3
            STRAIN(I)   = STRAIN1(I)
            STRAIN(I+3) = STRAIN2(I)
            STRAIN(I+6) = STRAIN3(I)
         ENDDO


      ELSE

         FATAL_ERR = FATAL_ERR
         WRITE(ERR,1962) SUBR_NAME, DIRECTION
         WRITE(F06,1962) SUBR_NAME, DIRECTION
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
 1962 FORMAT(' *ERROR  1962: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' INPUT ARGUMENT "DIRECTION" MUST BE = "1-2" OR "2-1" BUT VALUE IS ',A)







! **********************************************************************************************************************************

      END SUBROUTINE ROT_COMP_ELEM_AXES

