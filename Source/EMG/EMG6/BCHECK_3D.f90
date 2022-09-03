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
  
      SUBROUTINE BCHECK_3D ( B, NUM_GRIDS, ID, NROWB, NCOLB, BW )
 
! Checks strain-displacement matrices for rigid body motion and constant strain for 3-D solid elements
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, BUG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BCHECK_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, TWO
      USE MODEL_STUF, ONLY            :  AGRID, TE, XEB, XEL
 
      USE BCHECK_3D_USE_IFs

      IMPLICIT NONE
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BCHECK_3D'
      CHARACTER(41*BYTE)              :: MESSAG(12)        ! Output messages for the 14 modes of deformation (6 RB + 8 const strain)
      CHARACTER( 3*BYTE)              :: NOTE(12)          ! Output message
  
      INTEGER(LONG), INTENT(IN)       :: NCOLB             ! Number of cols in the input B matrix
      INTEGER(LONG), INTENT(IN)       :: NROWB             ! Number of rows in the input B matrix
      INTEGER(LONG), INTENT(IN)       :: NUM_GRIDS         ! Number of grids that this solid element has.
      INTEGER(LONG), INTENT(IN)       :: ID(NCOLB)         ! List of elem DOF's for each of the elem grids (e.g 3,4,5 for each of
!                                                            4 grids for a 4 node plate bending elem 
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: KK                ! A computed index into array W
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BCHECK_BEGEND
  
      REAL(DOUBLE) , INTENT(IN)       :: B(NROWB,NCOLB)    ! Strain-displ matrix
      REAL(DOUBLE) , INTENT(OUT)      :: BW(NROWB,12)      ! Output from subr BCHECK_3D (matrix of NROWB elem strains for various
!                                                            elem rigid body motions/constant strain distortions)
      REAL(DOUBLE)                    :: GRD_COORDS(3)     ! 3 coords from XEB for one node of the element
      REAL(DOUBLE)                    :: REF_COORDS(3)     ! 3 coords from XEB for node 1
      REAL(DOUBLE)                    :: RB_DISP_0(6,6)    ! 6 x 6 RB matrix for one grid for this element in basic coords
      REAL(DOUBLE)                    :: RB_DISP_L(6,6)    ! 6 x 6 RB matrix for one grid for this element in elenent local coords
      REAL(DOUBLE)                    :: TE6(6,6)          ! 6 x 6 transformation matrix with TE as 2 diagonal 3 x 3 matrices
      REAL(DOUBLE)                    :: W(6*NUM_GRIDS,12) ! Displs for the 14 modes of elem deformation (6 RB + 6 constant strain)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      DO I=1,NROWB
         DO J=1,12
            BW(I,J) = ZERO
         ENDDO
      ENDDO

! Form TE6 from 2 diagonal TE matrices

      DO I=1,6
         DO J=1,6
            TE6(I,J) = ZERO
         ENDDO
      ENDDO

      DO I=1,3
         DO J=1,3
            TE6(I,J)     = TE(I,J)
            TE6(I+3,J+3) = TE(I,J)
         ENDDO
      ENDDO

! Init W

      DO I=1,6*NUM_GRIDS
         DO J=1,12
            W(I,J) = ZERO
         ENDDO 
      ENDDO 

      MESSAG( 1) = '      Rigid Body Displacement,  T1  = 1.0:'
      MESSAG( 2) = '      Rigid Body Displacement,  T2  = 1.0:'
      MESSAG( 3) = '      Rigid Body Displacement,  T3  = 1.0:'
      MESSAG( 4) = '      Rigid Body Rotation,      R1  = 1.0:'
      MESSAG( 5) = '      Rigid Body Rotation,      R2  = 1.0:'
      MESSAG( 6) = '      Rigid Body Rotation,      R3  = 1.0:'
      MESSAG( 7) = '      Constant direct x strain, Exx = 1.0:'
      MESSAG( 8) = '      Constant direct y strain, Eyy = 1.0:'
      MESSAG( 9) = '      Constant direct z strain, Ezz = 1.0:'
      MESSAG(10) = '      Constant shear xy strain, Gxy = 1.0:'
      MESSAG(11) = '      Constant shear yz strain, Gxy = 1.0:'
      MESSAG(12) = '      Constant shear zx strain, Gxy = 1.0:'

      NOTE( 1)   = '(a)' 
      NOTE( 2)   = '(a)' 
      NOTE( 3)   = '(a)' 
      NOTE( 4)   = '(a)' 
      NOTE( 5)   = '(a)' 
      NOTE( 6)   = '(a)' 
      NOTE( 7)   = '(b)' 
      NOTE( 8)   = '(c)' 
      NOTE( 9)   = '(d)' 
      NOTE(10)   = '(e)' 
      NOTE(11)   = '(f)' 
      NOTE(12)   = '(g)' 
  
! Calc RB modes (cols 1-6 of W)

      DO I=1,NUM_GRIDS
         DO J=1,3
            GRD_COORDS(J) = XEB(I,J)
            REF_COORDS(J) = XEB(1,J)
         ENDDO
         CALL RIGID_BODY_DISP_MAT ( GRD_COORDS, REF_COORDS, RB_DISP_0 )
         CALL MATMULT_FFF ( TE6, RB_DISP_0, 6, 6, 6, RB_DISP_L )
         DO J=1,6
            DO K=1,6
               W(6*(I-1)+J,K) = RB_DISP_L(J,K)
            ENDDO
         ENDDO
      ENDDO

! Calc constant strain modes (cols 7-14 of W)

      DO K=1,NUM_GRIDS

         KK = 6*(K-1)

         W(KK+1, 7) =  XEL(K,1)                            ! This gives constant direct x strain

         W(KK+2, 8) =  XEL(K,2)                            ! This gives constant direct y strain

         W(KK+3, 9) =  XEL(K,3)                            ! This gives constant direct z strain

         W(KK+1,10) =  XEL(K,2)/TWO                        ! The next 2 give constant shear xy strain
         W(KK+2,10) =  XEL(K,1)/TWO

         W(KK+2,11) =  XEL(K,3)/TWO                        ! The next 2 give constant shear yz strain
         W(KK+3,11) =  XEL(K,2)/TWO

         W(KK+3,12) =  XEL(K,1)/TWO                        ! The next 2 give constant shear zx strain
         W(KK+1,12) =  XEL(K,3)/TWO

      ENDDO 

! Calc BW = B*W (but B has fewer cols since element has no stiffness for 4,5,6 DOF's)

      DO I=1,NROWB
         DO J=1,12
            BW(I,J) = ZERO
            DO K=1,NCOLB
               BW(I,J) = BW(I,J) + B(I,K)*W(ID(K),J)
            ENDDO 
         ENDDO 
      ENDDO 
  
! Write results

      DO J=1,3
         WRITE(BUG,9101) MESSAG(J),(BW(I,J),I=1,NROWB), NOTE(J)
      ENDDO
      WRITE(BUG,*)        

      DO J=4,6
         WRITE(BUG,9101) MESSAG(J),(BW(I,J),I=1,NROWB), NOTE(J)
      ENDDO
      WRITE(BUG,*)        

      DO J=7,9
         WRITE(BUG,9101) MESSAG(J),(BW(I,J),I=1,NROWB), NOTE(J)
      ENDDO
      WRITE(BUG,*)        

      DO J=10,12
         WRITE(BUG,9101) MESSAG(J),(BW(I,J),I=1,NROWB), NOTE(J)
      ENDDO
      WRITE(BUG,*)        

      WRITE(BUG,*)
      WRITE(BUG,9901)

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 9101 FORMAT(1X,A,6(1ES14.6),2X,A)

 9901 FORMAT(' Notes: (a) All strains should be zero',/,                                                                           &
             '        (b) Exx should be 1.0, others 0',/,                                                                          &
             '        (c) Eyy should be 1.0, others 0',/,                                                                          &
             '        (d) Ezz should be 1.0, others 0',/,                                                                          &
             '        (e) Gxy should be 1.0, others 0',/,                                                                          &
             '        (f) Gyz should be 1.0, others 0',/,                                                                          &
             '        (g) Gzx should be 1.0, others 0',/)

! **********************************************************************************************************************************
 
      END SUBROUTINE BCHECK_3D
