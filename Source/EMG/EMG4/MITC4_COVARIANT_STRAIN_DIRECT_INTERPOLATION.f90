! #################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
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
      SUBROUTINE MITC4_COVARIANT_STRAIN_DIRECT_INTERPOLATION ( R, S, T, MEMBRANE, BENDING, B )
 
! Reference [1]:
! "A new MITC4+ shell element" by Ko, Lee, Bathe, 2016

 
! Covariant strain-displacement components at point (R,S,T) directly evaluated from the displacement and rotation interpolations.
! Only for in-layer strains.
!
!        Grid point 1        Grid point 2      ...  
!      ux uy uz rx ry rz   ux uy uz rx ry rz
! 11 [ #  #  #  #  #  #  | #  #  #  #  #  #  |     ]
! 22 [ #  #  #  #  #  #  | #  #  #  #  #  #  |     ]
! 33 [ 0  0  0  0  0  0  | 0  0  0  0  0  0  |     ]
! 12 [ #  #  #  #  #  #  | #  #  #  #  #  #  | ... ]
! 23 [ 0  0  0  0  0  0  | 0  0  0  0  0  0  |     ]
! 13 [ 0  0  0  0  0  0  | 0  0  0  0  0  0  |     ]


      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE MODEL_STUF, ONLY            :  ELGP, EPROP, TYPE, XEB
      USE CONSTANTS_1, ONLY           :  ZERO, HALF, ONE, TWO, FOUR, QUARTER
      USE IOUNT1, ONLY                :  ERR, F06
      USE SCONTR, ONLY                :  FATAL_ERR

      USE SHP2DQ_Interface
      USE OUTA_HERE_Interface
      USE MITC_GP_RS_Interface
      USE MITC_DIRECTOR_VECTOR_Interface
      USE CROSS_Interface

      IMPLICIT NONE 
      
      REAL(DOUBLE) , INTENT(IN)       :: R,S,T             ! Isparametric coordinates
      REAL(DOUBLE) , INTENT(OUT)      :: B(6, 6*ELGP)      ! Strain-displacement matrix.
      REAL(DOUBLE)                    :: PSH(ELGP)         ! Shape functions
      REAL(DOUBLE)                    :: DPSHG(2,ELGP)     ! Derivatives of shape functions with respect to R and S.
      REAL(DOUBLE)                    :: GP_RS(2,ELGP)     ! Isoparametric coordinates of the nodes
      REAL(DOUBLE)                    :: X_R(3)            ! Characteristic geometry vector x_r
      REAL(DOUBLE)                    :: X_S(3)            ! Characteristic geometry vector x_s
      REAL(DOUBLE)                    :: X_D(3)            ! Characteristic geometry vector x_d (distortion vector)
      REAL(DOUBLE)                    :: DXMDRS(2,3)       ! Partial derivatives of x_m with respect to r and s
      REAL(DOUBLE)                    :: DXBDRS(2,3)       ! Partial derivatives of x_b with respect to r and s
      REAL(DOUBLE)                    :: DIRECTOR(ELGP,3)  ! Director vector at each grid point. Called Vn^i in ref [1]
      REAL(DOUBLE)                    :: THICKNESS(ELGP)   ! Element thicknesses at grid points
      REAL(DOUBLE)                    :: DUM(2,ELGP)
      REAL(DOUBLE)                    :: V1(ELGP,3)        ! Basis vector orthogonal to the director vector.
      REAL(DOUBLE)                    :: V2(ELGP,3)        ! Basis vector orthogonal to the director vector and V1.

      INTEGER(LONG)                   :: GP                ! Grid point number. 1-4.
      INTEGER(LONG)                   :: I, J              ! Tensor indices.
      INTEGER(LONG)                   :: ROW               ! Row number of B

      LOGICAL      , INTENT(IN)       :: MEMBRANE          ! If true, generate membrane parts of B
      LOGICAL      , INTENT(IN)       :: BENDING           ! If true, generate bending parts of B

! **********************************************************************************************************************************

                                                           ! Thickness is currently treated as uniform.
                                                           ! To allow grid point thicknesses, a different value should be used
                                                           ! for each node, interpolating to midside nodes.
      DO GP=1,ELGP
         THICKNESS(GP) = EPROP(1)
      ENDDO
      
                                                           ! Shape function derivatives at R,S
      IF ((TYPE(1:5) == 'QUAD4') .OR. (TYPE(1:5) == 'QUAD8')) THEN

         CALL SHP2DQ ( 0, 0, ELGP, 'MITC4_COVARIANT_STRAIN_DIRECT_INTERPOLATION', '', 0, R, S, 'N', PSH, DPSHG )

                                                           ! Change node numbering to match Bathe's MITC4+ paper.
         IF (TYPE(1:5) == 'QUAD4') THEN
            DUM = DPSHG
            DPSHG(:,1) = DUM(:,3)
            DPSHG(:,2) = DUM(:,4)
            DPSHG(:,3) = DUM(:,1)
            DPSHG(:,4) = DUM(:,2)
         ENDIF

      ELSE

         WRITE(ERR,*) ' *ERROR: INCORRECT ELEMENT TYPE ', TYPE
         WRITE(F06,*) ' *ERROR: INCORRECT ELEMENT TYPE ', TYPE
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )

      ENDIF
      
      
                                                           ! Initialize B
      B(:,:) = ZERO

                                                           ! Isoparametric coordinates of the nodes.
      GP_RS = MITC_GP_RS()

                                                           ! Characteristic geometry vectors for the element
      X_R(:) = ZERO
      X_S(:) = ZERO
      X_D(:) = ZERO
      DO GP=1,ELGP
         X_R(:) = X_R(:) + QUARTER * GP_RS(1, GP)                * XEB(GP, :)
         X_S(:) = X_S(:) + QUARTER *                GP_RS(2, GP) * XEB(GP, :)
         X_D(:) = X_D(:) + QUARTER * GP_RS(1, GP) * GP_RS(2, GP) * XEB(GP, :)
      ENDDO
                                                           
                                                           ! Eqn (9) of ref [1].
      DXMDRS(1,:) = X_R + S * X_D                          ! ∂x_m/∂r
      DXMDRS(2,:) = X_S + R * X_D                          ! ∂x_m/∂s
      

      IF(BENDING) THEN

         DO GP=1,ELGP                                      ! Director vector at each node
            DIRECTOR(GP,:) = MITC_DIRECTOR_VECTOR( GP_RS(1,GP), GP_RS(2,GP) )
         ENDDO

                                                           ! ∂x_b/∂r
                                                           ! ∂x_b/∂s
                                                           ! From eqns (8a) and (2) of ref [1].
         DXBDRS(:,:) = ZERO
         DO GP=1,ELGP
            DXBDRS(1,:) = DXBDRS(1,:) + HALF * THICKNESS(GP) * DIRECTOR(GP,:) * DPSHG(1,GP)
            DXBDRS(2,:) = DXBDRS(2,:) + HALF * THICKNESS(GP) * DIRECTOR(GP,:) * DPSHG(2,GP)
         ENDDO

                                                           ! Pick a V1 and V2 for each node which form an 
                                                           ! orthogonal right-handed coordinate system V1, V2, Vn
                                                           ! where Vn is the director vector.
         DO GP=1,ELGP
!victor todo for now just use basic x for V1 (and thus y for V2) so it's only valid for elements in the xy plane.
            V1(GP,:) = (/ ONE , ZERO, ZERO /)
            CALL CROSS(DIRECTOR(GP,:), V1(GP,:), V2(GP,:))
         ENDDO
         
      ENDIF

      DO ROW=1,4

                                                           ! Tensor indices for the row
         SELECT CASE (ROW)
            CASE (1); I=1; J=1                             ! In-layer normal strain
            CASE (2); I=2; J=2                             ! In-layer normal strain
            CASE (3); CYCLE                                ! No zz strain
            CASE (4); I=1; J=2                             ! In-layer shear strain
         END SELECT

         IF(MEMBRANE) THEN
                                                           ! Membrane e^m_xx, e^m_yy, e^m_xy terms of eqn (7a)
                                                           ! described in eqn (7b) in ref [1]
            CALL ADD_TERM_MM(ROW, I, J)
            CALL ADD_TERM_MM(ROW, J, I)
         ENDIF

         IF(BENDING) THEN
                                                           ! Bending e^b1_xx, e^b1_yy, e^b1_xy terms of eqn (7a)
                                                           ! described in eqn (7c) in ref [1]
            CALL ADD_TERM_MB(ROW, I, J)
            CALL ADD_TERM_MB(ROW, J, I)
            CALL ADD_TERM_BM(ROW, I, J)
            CALL ADD_TERM_BM(ROW, J, I)

                                                           ! Bending e^b2_xx, e^b2_yy, e^b2_xy terms of eqn (7a)
                                                           ! described in eqn (7d) in ref [1]
            CALL ADD_TERM_BB(ROW, I, J)
            CALL ADD_TERM_BB(ROW, J, I)
         ENDIF
         
      ENDDO


      IF(BENDING) THEN
! At this point, alpha and beta are the columns of B for dofs 4 and 5 for each node.
! We need to transform the dof 4,5,6 columns of each row and each node from V1,V2,Vn coordinates to basic x,y,z
! victor todo do that here.
      ENDIF


      RETURN

! **********************************************************************************************************************************
      CONTAINS

      
      SUBROUTINE ADD_TERM_MM(ROW, IX, IU)

      REAL(DOUBLE)                    :: DUMDRS(2,3)       ! One grid point's term in the sum for the coefficients of the partial 
                                                           ! derivatives of u_m with respect to R and S.

      INTEGER(LONG), INTENT(IN)       :: ROW               ! Row of B to add the result to. 1, 2, or 4.
      INTEGER(LONG), INTENT(IN)       :: IX                ! Index of dr in the x derivative. 1 or 2.
      INTEGER(LONG), INTENT(IN)       :: IU                ! Index of dr in the u derivative. 1 or 2.
      INTEGER(LONG)                   :: K                 ! Column of B before the column for DOF 1 of the current node.

      ! One term of eqn (7b) in ref [1]

      !              1  / ∂x_m      ∂u_m  \
      ! B(ROW,:) +=  - (  ----- dot -----  )
      !              2  \ ∂r_IX     ∂r_IU /
      !
      ! MM in the subroutine name identifies it as using derivatives of x_m and u_m.

          
      DO GP=1,ELGP
         K = (GP-1) * 6
                        
                                                                               ! Eqn (9) of ref [1]. This node's term of:
         DUMDRS(1,:) = ( GP_RS(1,GP) + S * GP_RS(1,GP) * GP_RS(2,GP) ) / FOUR  ! ∂u_m/∂r = u_r + s * u_d
         DUMDRS(2,:) = ( GP_RS(2,GP) + R * GP_RS(1,GP) * GP_RS(2,GP) ) / FOUR  ! ∂u_m/∂s = u_s + r * u_d

         B(ROW, K+1) = B(ROW, K+1) + ONE / TWO * DXMDRS(IX,1) * DUMDRS(IU,1)
         B(ROW, K+2) = B(ROW, K+2) + ONE / TWO * DXMDRS(IX,2) * DUMDRS(IU,2)
         B(ROW, K+3) = B(ROW, K+3) + ONE / TWO * DXMDRS(IX,3) * DUMDRS(IU,3)
      ENDDO
         
      END SUBROUTINE ADD_TERM_MM

! **********************************************************************************************************************************

      SUBROUTINE ADD_TERM_BM(ROW, IX, IU)

      REAL(DOUBLE)                    :: DUMDRS(2,3)       ! One grid point's term in the sum for the coefficients of the partial 
                                                           ! derivatives of u_m with respect to R and S.

      INTEGER(LONG), INTENT(IN)       :: ROW               ! Row of B to add the result to. 1, 2, or 4.
      INTEGER(LONG), INTENT(IN)       :: IX                ! Index of dr in the x derivative. 1 or 2.
      INTEGER(LONG), INTENT(IN)       :: IU                ! Index of dr in the u derivative. 1 or 2.
      INTEGER(LONG)                   :: K                 ! Column of B before the column for DOF 1 of the current node.

      ! One term of eqn (7c) in ref [1]

      !              t  / ∂x_b      ∂u_m  \
      ! B(ROW,:) +=  - (  ----- dot -----  )
      !              2  \ ∂r_IX     ∂r_IU /
      !
      ! BM in the subroutine name identifies it as using derivatives of x_b and u_m.
      ! t is the isparametric coordinate T and the coefficient of e^b1 in eqn (7a) of ref [1].
          
      DO GP=1,ELGP
         K = (GP-1) * 6
                        
                                                                               ! Eqn (9) of ref [1]. This node's term of:
         DUMDRS(1,:) = ( GP_RS(1,GP) + S * GP_RS(1,GP) * GP_RS(2,GP) ) / FOUR  ! ∂u_m/∂r = u_r + s * u_d
         DUMDRS(2,:) = ( GP_RS(2,GP) + R * GP_RS(1,GP) * GP_RS(2,GP) ) / FOUR  ! ∂u_m/∂s = u_s + r * u_d

         B(ROW, K+1) = B(ROW, K+1) + T / TWO * DXBDRS(IX,1) * DUMDRS(IU,1)
         B(ROW, K+2) = B(ROW, K+2) + T / TWO * DXBDRS(IX,2) * DUMDRS(IU,2)
         B(ROW, K+3) = B(ROW, K+3) + T / TWO * DXBDRS(IX,3) * DUMDRS(IU,3)
      ENDDO
               
      END SUBROUTINE ADD_TERM_BM

! **********************************************************************************************************************************

      SUBROUTINE ADD_TERM_MB(ROW, IX, IU)

      REAL(DOUBLE)                    :: DUMa(3)           ! Coefficient of alpha in ∂u_b/∂r_IU for one node.
      REAL(DOUBLE)                    :: DUMb(3)           ! Coefficient of beta  in ∂u_b/∂r_IU for one node.

      INTEGER(LONG), INTENT(IN)       :: ROW               ! Row of B to add the result to. 1, 2, or 4.
      INTEGER(LONG), INTENT(IN)       :: IX                ! Index of dr in the x derivative. 1 or 2.
      INTEGER(LONG), INTENT(IN)       :: IU                ! Index of dr in the u derivative. 1 or 2.
      INTEGER(LONG)                   :: K                 ! Column of B before the column for DOF 1 of the current node.

      ! One term of eqn (7c) in ref [1]

      !              t  / ∂x_m      ∂u_b  \
      ! B(ROW,:) +=  - (  ----- dot -----  )
      !              2  \ ∂r_IX     ∂r_IU /
      !
      ! MB in the subroutine name identifies it as using derivatives of x_m and u_b.
      ! t is the isparametric coordinate T and the coefficient of e^b1 in eqn (7a) of ref [1].
          
      DO GP=1,ELGP
         K = (GP-1) * 6

                                                           ! Put coefficients of alpha in DOF 4.
         DUMa = THICKNESS(GP) / TWO * DPSHG(IU,GP) * (-V2(GP,:))
         B(ROW, K+4) = B(ROW, K+4) + T / TWO * DOT_PRODUCT(DXMDRS(IX,:), DUMa)

                                                           ! Put coefficients of beta in DOF 5.
         DUMb = THICKNESS(GP) / TWO * DPSHG(IU,GP) * ( V1(GP,:))
         B(ROW, K+5) = B(ROW, K+5) + T / TWO * DOT_PRODUCT(DXMDRS(IX,:), DUMb)

      ENDDO
               
      END SUBROUTINE ADD_TERM_MB

! **********************************************************************************************************************************

      SUBROUTINE ADD_TERM_BB(ROW, IX, IU)

      REAL(DOUBLE)                    :: DUMa(3)           ! Coefficient of alpha in ∂u_b/∂r_IU for one node.
      REAL(DOUBLE)                    :: DUMb(3)           ! Coefficient of beta  in ∂u_b/∂r_IU for one node.

      INTEGER(LONG), INTENT(IN)       :: ROW               ! Row of B to add the result to. 1, 2, or 4.
      INTEGER(LONG), INTENT(IN)       :: IX                ! Index of dr in the x derivative. 1 or 2.
      INTEGER(LONG), INTENT(IN)       :: IU                ! Index of dr in the u derivative. 1 or 2.
      INTEGER(LONG)                   :: K                 ! Column of B before the column for DOF 1 of the current node.

      ! One term of eqn (7d) in ref [1]

      !              t^2  / ∂x_b      ∂u_b  \
      ! B(ROW,:) +=  --- (  ----- dot -----  )
      !               2   \ ∂r_IX     ∂r_IU /
      !
      ! BB in the subroutine name identifies it as using derivatives of x_b and u_b.
      ! t is the isparametric coordinate T and t^2 the coefficient of e^b2 in eqn (7a) of ref [1].
          
      DO GP=1,ELGP
         K = (GP-1) * 6

                                                           ! Put coefficients of alpha in DOF 4.
         DUMa = THICKNESS(GP) / TWO * DPSHG(IU,GP) * (-V2(GP,:))
         B(ROW, K+4) = B(ROW, K+4) + T*T / TWO * DOT_PRODUCT(DXBDRS(IX,:), DUMa)

                                                           ! Put coefficients of beta in DOF 5.
         DUMb = THICKNESS(GP) / TWO * DPSHG(IU,GP) * ( V1(GP,:))
         B(ROW, K+5) = B(ROW, K+5) + T*T / TWO * DOT_PRODUCT(DXBDRS(IX,:), DUMb)

      ENDDO
               
      END SUBROUTINE ADD_TERM_BB

! **********************************************************************************************************************************
  
      END SUBROUTINE MITC4_COVARIANT_STRAIN_DIRECT_INTERPOLATION
