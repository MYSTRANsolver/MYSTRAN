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

      SUBROUTINE ELMOFF ( OPT, WRITE_WARN ) 
 
! Processes element mass, stiffness, thermal load, pressure load, stress recovery matrices if there are any offsets of the element
! at any grid points. This is a general routine which can be used by any of the elements as long as the 
! element has no more than 4 grid points.
! ======================================

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MAX_STRESS_POINTS, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELMOFF_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE MODEL_STUF, ONLY            :  CAN_ELEM_TYPE_OFFSET, ELDOF, ELGP, EID, KE, ME, NUM_EMG_FATAL_ERRS,                       &
                                         OFFDIS, OFFSET, PPE, PTE, SE1, SE2, SE3, TYPE
      USE ELMOFF_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ELMOFF'
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN        ! If 'Y" write warning messages, otherwise do not

      INTEGER(LONG)                   :: I,J,K,L,M,N       ! DO loop indices
      INTEGER(LONG)                   :: II,JJ             ! Computed indices 
      INTEGER(LONG)                   :: JBEG              ! Index 
      INTEGER(LONG)                   :: KBEG              ! Index 
      INTEGER(LONG)                   :: ROW               ! A computed row number in the elem stiff matrix
      INTEGER(LONG)                   :: COL               ! A computed col number in the elem stiff matrix
      INTEGER(LONG)                   :: NCOL              ! An input to subr MULT_OFFSET, called herein
      INTEGER(LONG)                   :: METH              ! An input to subr MULT_OFFSET, called herein
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELMOFF_BEGEND
 
      REAL(DOUBLE)                    :: DUM3(3,3)         ! An intermediate result when calculating offset SEi
      REAL(DOUBLE)                    :: DUM4(3,3)         ! An intermediate result when calculating offset SEi
      REAL(DOUBLE)                    :: DUM11(3,3)        ! An intermediate result when calculating offset KE
      REAL(DOUBLE)                    :: DUM12(3,3)        ! An intermediate result when calculating offset KE
      REAL(DOUBLE)                    :: DUM21(3,3)        ! An intermediate result when calculating offset KE
      REAL(DOUBLE)                    :: DUM22(3,3)        ! An intermediate result when calculating offset KE
      REAL(DOUBLE)                    :: DXI               ! An offset distance in direction 1
      REAL(DOUBLE)                    :: DYI               ! An offset distance in direction 2
      REAL(DOUBLE)                    :: DZI               ! An offset distance in direction 3
      REAL(DOUBLE)                    :: DXJ               ! An offset distance in direction 1
      REAL(DOUBLE)                    :: DYJ               ! An offset distance in direction 2
      REAL(DOUBLE)                    :: DZJ               ! An offset distance in direction 3
      REAL(DOUBLE)                    :: PDUM1(3,NSUB)     ! An intermediate result when calculating offset PTE, PPE
      REAL(DOUBLE)                    :: PDUM2(3,NSUB)     ! An intermediate result when calculating offset PTE, PPE

      REAL(DOUBLE)                    :: DUM_KE(6*ELGP,6*ELGP)
      REAL(DOUBLE)                    :: E(6*ELGP,6*ELGP)
      REAL(DOUBLE)                    :: Ei(ELGP,6,6)
      REAL(DOUBLE)                    :: KE1(6*ELGP,6*ELGP)
      
      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC, (OPT(I),I=1,6)
 9001    FORMAT(1X,A,' BEGN ',F10.3, 3X,6A1)
      ENDIF

! **********************************************************************************************************************************
! Make sure we are not here for an element that does not support offsets

      IF (CAN_ELEM_TYPE_OFFSET /= 'Y') THEN
         WRITE(ERR,1955) SUBR_NAME, TYPE, EID
         WRITE(F06,1955) SUBR_NAME, TYPE, EID
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! The processing is done in 1 major loop over the  number of G.P.'s in which PTE, SEi are processed by pre or post multiplying by
! the offset matrix (or it's transpose).

! A minor loop within the major loop takes care of ME and KE which gets post-multiplied by the offset matrix and pre-multiplied by
! it's transpose. The offset matrices for each G.P. are a 6 x 6 matrix which is an identity matrix plus a small 3 x 3 submatrix
! containing only 3 independent terms, and the processing takes advantage of this and simplifies the matrix multiplications.
! The offset matrix is called E for each G.P. but is never written out as a 6 x 6 matrix.

! The general form of E for one grid point is:
 
!                             | 1  0  0 |  0    DZ  -DY |
!                             | 0  1  0 | -DZ   0    DX |
!                             | 0  0  1 |  DY  -DX   0  |
!                         E = |---------|---------------|
!                             | 0  0  0 |  1    0    0  |
!                             | 0  0  0 |  0    1    0  |
!                             | 0  0  0 |  0    0    1  |

! where DX, DY and DZ are the 3 components of the offset of the element at a grid and are in global coords

! With this E matrix, the transformed element matrices are (prime indicates matrix transposition):

!                                MEg = E'* MEe * E

!                                KEg = E'* KEe * E

!                               PTEg = E'* PTEe

!                                SEg = SEe * E

! where MEe, KEe, PTEe and SEe are the mass, stiffness, thermal loads and stress recovery matrices developed in local element
! coordinates at the element nodes and MEg, KEg, PTEg, SEg are the same matrices but in terms of degrees of freedom at the grids and
! in global coordinates.

! Initialize

      DO J=1,6*ELGP
         DO K=1,6*ELGP
            E(J,K)   = ZERO
         ENDDO
      ENDDO

      DO I=1,ELGP
         DO J=1,6
            DO K=1,6
               Ei(I,J,K) = ZERO
            ENDDO
         ENDDO
      ENDDO

      DO I=1,ELGP

         II  = 6*(I-1)
         DXI = ZERO
         DYI = ZERO
         DZI = ZERO
         IF (OFFSET(I) == 'Y') THEN
            DXI = OFFDIS(I,1)
            DYI = OFFDIS(I,2)
            DZI = OFFDIS(I,3)
         ENDIF
         DO J=1,6
            Ei(I,J,J) = ONE
         ENDDO

         Ei(I,1,5) =  DZI
         Ei(I,1,6) = -DYI
         Ei(I,2,4) = -DZI
         Ei(I,2,6) =  DXI
         Ei(I,3,4) =  DYI
         Ei(I,3,5) = -DXI

      ENDDO

! Set E matrix

      DO I=1,ELGP
         JBEG = 6*(I-1)
         DO J=1,6
            KBEG = 6*(I-1)
            DO K=1,6
               E(JBEG+J,KBEG+K) = Ei(I,J,K)
            ENDDO
         ENDDO
      ENDDO

      IF (OPT(4) == 'Y') THEN

         DO J=1,6*ELGP
            DO K=1,6*ELGP
               KE1(J,K) = KE(J,K)
            ENDDO
         ENDDO

! Mult E'*KE*E

         CALL MATMULT_FFF   ( KE1, E     , 6*ELGP, 6*ELGP, 6*ELGP, DUM_KE )
         CALL MATMULT_FFF_T ( E  , DUM_KE, 6*ELGP, 6*ELGP, 6*ELGP, KE1    )

! Set KE = KE1 for 6*ELGP by 6*ELGP terms

         DO J=1,6*ELGP
            DO K=1,6*ELGP
               KE(J,K) = KE1(J,K)
            ENDDO
         ENDDO

      ENDIF

! Process offsets

      DO I=1,ELGP
         II  = 6*(I-1)
         DXI = ZERO
         DYI = ZERO
         DZI = ZERO
         IF (OFFSET(I) == 'Y') THEN
            DXI = OFFDIS(I,1)
            DYI = OFFDIS(I,2)
            DZI = OFFDIS(I,3)
            IF (OPT(2) == 'Y') THEN                        ! Process PTE. Generate E'* PTE
               DO J=1,3
                  DO K=1,NTSUB
                     PDUM1(J,K) = PTE(II+J,K)
                  ENDDO 
               ENDDO 
               NCOL = NTSUB
               METH = 2
               CALL MULT_OFFSET ( PDUM1, DXI, DYI, DZI, NCOL, METH, PDUM2 )
               DO J=1,3
                  DO K=1,NTSUB
                     PTE(II+J+3,K) = PTE(II+J+3,K) + PDUM2(J,K)
                  ENDDO 
               ENDDO
            ENDIF 

            IF (OPT(1) == 'Y') THEN                        ! Process ME. Generate E(transp)*ME*E.
               DO J=I,ELGP
                  JJ = 6*(J-1)
                  IF (OFFSET(J) == 'Y') THEN
                     DXJ = OFFDIS(J,1)
                     DYJ = OFFDIS(J,2)
                     DZJ = OFFDIS(J,3)

                     DO K=1,3                              ! Partition ME for this grid point pair (i,j) into 4-3x3 matrices
                        DO L=1,3
                           DUM11(K,L) = ME(II+K,JJ+L)
                           DUM12(K,L) = ME(II+K,JJ+L+3)
                           DUM21(K,L) = ME(II+K+3,JJ+L)
                           DUM22(K,L) = ME(II+K+3,JJ+L+3)
                        ENDDO 
                     ENDDO   

                     NCOL = 3                              ! Modify upper right 3x3 partition of ME
                     METH = 1
                     CALL MULT_OFFSET ( DUM11, DXJ, DYJ, DZJ, NCOL, METH, DUM3 )
                     DO K=1,3
                        DO L=1,3
                           ME(II+K,JJ+L+3) = DUM12(K,L) + DUM3(K,L)
                        ENDDO 
                     ENDDO   

                     NCOL = 3                              ! Modify lower left 3x3 partition of ME
                     METH = 2
                     CALL MULT_OFFSET ( DUM11, DXI, DYI, DZI, NCOL, METH, DUM3 )
                     DO K=1,3
                        DO L=1,3
                           ME(II+K+3,JJ+L) = DUM21(K,L) + DUM3(K,L)
                        ENDDO 
                     ENDDO   

                     NCOL = 3                              ! Modify lower right 3x3 partition of ME
                     METH = 1
                     CALL MULT_OFFSET ( DUM21, DXJ, DYJ, DZJ, NCOL, METH, DUM3 )
                     DO K=1,3
                        DO L=1,3
                           ME(II+K+3,JJ+L+3) = DUM22(K,L) + DUM3(K,L)
                        ENDDO 
                     ENDDO   
 
                     NCOL = 3
                     METH = 2
                     CALL MULT_OFFSET ( DUM12, DXI, DYI, DZI, NCOL, METH, DUM3 )
                     DO K=1,3
                        DO L=1,3
                           ME(II+K+3,JJ+L+3) = ME(II+K+3,JJ+L+3) +DUM3(K,L)
                        ENDDO 
                     ENDDO   

                     NCOL = 3
                     METH = 1
                     CALL MULT_OFFSET ( DUM11, DXJ, DYJ, DZJ, NCOL, METH, DUM4 )
                     NCOL = 3
                     METH = 2
                     CALL MULT_OFFSET ( DUM4, DXI, DYI, DZI, NCOL, METH, DUM3 )
                     DO K=1,3
                        DO L=1,3
                           ME(II+K+3,JJ+L+3) = ME(II+K+3,JJ+L+3) +DUM3(K,L)
                        ENDDO 
                     ENDDO   

                  ENDIF

               ENDDO   
 
               DO K=2,ELGP                                 ! Generate the remaining Mij using symmetry.
                  DO L=1,K-1
                     DO M=1,6
                        DO N=1,6
                           ROW = 6*(K-1) + M
                           COL = 6*(L-1) + N
                           ME(ROW,COL) = ME(COL,ROW)
                        ENDDO 
                     ENDDO   
                  ENDDO 
               ENDDO

            ENDIF   
 
            IF (OPT(3) == 'Y') THEN                        ! Process SEi. Generate SEi*E

               DO L=1,MAX_STRESS_POINTS+1
                  DO J=1,3
                     DO K=1,3
!xxError                DUM3(J,K) = SE1(L,J,II+K)
                        DUM3(J,K) = SE1(J,II+K,L)
                     ENDDO 
                  ENDDO
               ENDDO 
               NCOL = 3
               METH = 1
               CALL MULT_OFFSET ( DUM3, DXI, DYI, DZI, NCOL, METH, DUM4 )
               DO L=1,MAX_STRESS_POINTS+1
                  DO J=1,3 
                     DO K=1,3
!xxError                SE1(L,J,II+K+3) = SE1(L,J,II+K+3) + DUM4(J,K)
                        SE1(J,II+K+3,L) = SE1(J,II+K+3,L) + DUM4(J,K)
                     ENDDO
                  ENDDO 
               ENDDO 

               DO L=1,MAX_STRESS_POINTS+1
                  DO J=1,3
                     DO K=1,3
!xxError                DUM3(J,K) = SE2(L,J,II+K)
                        DUM3(J,K) = SE2(J,II+K,L)
                     ENDDO 
                  ENDDO
               ENDDO 
               NCOL = 3
               METH = 1
               CALL MULT_OFFSET ( DUM3, DXI, DYI, DZI, NCOL, METH, DUM4 )
               DO L=1,MAX_STRESS_POINTS+1
                  DO J=1,3
                     DO K=1,3
!xxError                SE2(L,J,II+K+3) = SE2(L,J,II+K+3) + DUM4(J,K)
                        SE2(J,II+K+3,L) = SE2(J,II+K+3,L) + DUM4(J,K)
                     ENDDO 
                  ENDDO 
               ENDDO

               DO L=1,MAX_STRESS_POINTS+1
                  DO J=1,3
                     DO K=1,3
!xxError                DUM3(J,K) = SE3(L,J,II+K)
                        DUM3(J,K) = SE3(J,II+K,L)
                     ENDDO 
                  ENDDO
               ENDDO 
               NCOL = 3
               METH = 1
               CALL MULT_OFFSET ( DUM3, DXI, DYI, DZI, NCOL, METH, DUM4 )
               DO L=1,MAX_STRESS_POINTS+1
                  DO J=1,3
                     DO K=1,3
!xxError                SE3(L,J,II+K+3) = SE3(L,J,II+K+3) + DUM4(J,K)
                        SE3(J,II+K+3,L) = SE3(J,II+K+3,L) + DUM4(J,K)
                     ENDDO 
                  ENDDO
               ENDDO 

            ENDIF

            IF (OPT(4) == 'Z') THEN                        ! Process KE. Generate E(transp)*KE*E.
!xx         IF (OPT(4) == 'Y') THEN                        ! Process KE. Generate E(transp)*KE*E.
               DO J=I,ELGP
                  JJ = 6*(J-1)
                  IF (OFFSET(J) == 'Y') THEN
                     DXJ = OFFDIS(J,1)
                     DYJ = OFFDIS(J,2)
                     DZJ = OFFDIS(J,3)

                     DO K=1,3                              ! Partition KE for this grid point pair (i,j) into 4-3x3 matrices
                        DO L=1,3
                           DUM11(K,L) = KE(II+K,JJ+L)
                           DUM12(K,L) = KE(II+K,JJ+L+3)
                           DUM21(K,L) = KE(II+K+3,JJ+L)
                           DUM22(K,L) = KE(II+K+3,JJ+L+3)
                        ENDDO 
                     ENDDO   

                     NCOL = 3                              ! Modify upper right 3x3 partition of KE
                     METH = 1
                     CALL MULT_OFFSET ( DUM11, DXJ, DYJ, DZJ, NCOL, METH, DUM3 )
                     DO K=1,3
                        DO L=1,3
                           KE(II+K,JJ+L+3) = DUM12(K,L) + DUM3(K,L)
                        ENDDO 
                     ENDDO   

                     NCOL = 3                              ! Modify lower left 3x3 partition of KE
                     METH = 2
                     CALL MULT_OFFSET ( DUM11, DXI, DYI, DZI, NCOL, METH, DUM3 )
                     DO K=1,3
                        DO L=1,3
                           KE(II+K+3,JJ+L) = DUM21(K,L) + DUM3(K,L)
                        ENDDO 
                     ENDDO   

                     NCOL = 3                              ! Modify lower right 3x3 partition of KE
                     METH = 1
                     CALL MULT_OFFSET ( DUM21, DXJ, DYJ, DZJ, NCOL, METH, DUM3 )
                     DO K=1,3
                        DO L=1,3
                           KE(II+K+3,JJ+L+3) = DUM22(K,L) + DUM3(K,L)
                        ENDDO 
                     ENDDO   
 
                     NCOL = 3
                     METH = 2
                     CALL MULT_OFFSET ( DUM12, DXI, DYI, DZI, NCOL, METH, DUM3 )
                     DO K=1,3
                        DO L=1,3
                           KE(II+K+3,JJ+L+3) = KE(II+K+3,JJ+L+3) +DUM3(K,L)
                        ENDDO 
                     ENDDO   

                     NCOL = 3
                     METH = 1
                     CALL MULT_OFFSET ( DUM11, DXJ, DYJ, DZJ, NCOL, METH, DUM4 )
                     NCOL = 3
                     METH = 2
                     CALL MULT_OFFSET ( DUM4, DXI, DYI, DZI, NCOL, METH, DUM3 )
                     DO K=1,3
                        DO L=1,3
                           KE(II+K+3,JJ+L+3) = KE(II+K+3,JJ+L+3) +DUM3(K,L)
                        ENDDO 
                     ENDDO   

                  ENDIF

               ENDDO   
 
               DO K=2,ELGP                                 ! Generate the remaining Kij using symmetry.
                  DO L=1,K-1
                     DO M=1,6
                        DO N=1,6
                           ROW = 6*(K-1) + M
                           COL = 6*(L-1) + N
                           KE(ROW,COL) = KE(COL,ROW)
                        ENDDO 
                     ENDDO   
                  ENDDO 
               ENDDO

            ENDIF   
 
            IF (OPT(5) == 'Y') THEN                        ! Process PPE. Generate E(transp.)*PPE
               DO J=1,3
                  DO K=1,NSUB
                     PDUM1(J,K) = PPE(II+J,K)
                  ENDDO 
               ENDDO 
               NCOL = NSUB
               METH = 2
               CALL MULT_OFFSET ( PDUM1, DXI, DYI, DZI, NCOL, METH, PDUM2 )
               DO J=1,3
                  DO K=1,NSUB
                     PPE(II+J+3,K) = PPE(II+J+3,K) + PDUM2(J,K)
                  ENDDO 
               ENDDO 

            ENDIF

         ENDIF

      ENDDO   
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 3006 format(6(1es14.6))

 1955 FORMAT(' *ERROR  1955: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ELEMENT TYPE ',A,' DOES NOT SUPPORT OFFSETS. ERROR OCCURRED FOR ELEMENT NUMBER ',I8)

! ##################################################################################################################################

      CONTAINS

! ##################################################################################################################################
 
      SUBROUTINE MULT_OFFSET ( A, DX, DY, DZ, NCOLA, METH, B )
 
! Perform matrix multiply to get A*E or E(transp)*A for elem offsets. Matrix E, the offset matrix, is a simple form.
! It is an identity  6 x 6 plus a 3 x 3 in the upper right corner containing the 3 offset distances. Due to this
! simplicity, A*E or E(transp)*A is calculated explicitly

      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_ERR
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MEFE
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELMOFF_BEGEND
      USE MODEL_STUF, ONLY            :  EMG_IFE, ERR_SUB_NAM, NUM_EMG_FATAL_ERRS 

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MULT_OFFSET'

      INTEGER(LONG)                   :: IK,JK             ! DO loop indices
      INTEGER(LONG), INTENT(IN)       :: METH              ! = 1 if A*E is to be calculated
                                                           ! = 2 if E(transp)*A is to be calculated
      INTEGER(LONG), INTENT(IN)       :: NCOLA             ! Number of cols in matrix A
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELMOFF_BEGEND + 1
 
      REAL(DOUBLE) , INTENT(IN)       :: A(3,NCOLA)        ! Matrix to either post-multiply E by or pre-multiply E(transp) by
      REAL(DOUBLE) , INTENT(IN)       :: DX                ! Offset distance in direction 1
      REAL(DOUBLE) , INTENT(IN)       :: DY                ! Offset distance in direction 2
      REAL(DOUBLE) , INTENT(IN)       :: DZ                ! Offset distance in direction 3
      REAL(DOUBLE) , INTENT(INOUT)    :: B(3,NCOLA)        ! Result matrix of either A*E or E(transp)*A
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Do not initialize B. It is an output in one call and maybe is input back as A in next call

      IF      (METH == 1) THEN
         DO IK=1,3
            B(IK,1) = -A(IK,2)*DZ + A(IK,3)*DY
            B(IK,2) =  A(IK,1)*DZ - A(IK,3)*DX
            B(IK,3) = -A(IK,1)*DY + A(IK,2)*DX
         ENDDO 
      ELSE IF (METH == 2) THEN
         DO JK=1,NCOLA
            B(1,JK) = -A(2,JK)*DZ + A(3,JK)*DY
            B(2,JK) =  A(1,JK)*DZ - A(3,JK)*DX
            B(3,JK) = -A(1,JK)*DY + A(2,JK)*DX
         ENDDO 
      ELSE  
         NUM_EMG_FATAL_ERRS = NUM_EMG_FATAL_ERRS + 1
         FATAL_ERR = FATAL_ERR + 1
         IF (WRT_ERR /= 0) THEN
            WRITE(ERR,1917) SUBR_NAME, METH
            WRITE(F06,1917) SUBR_NAME, METH
         ELSE
            IF (NUM_EMG_FATAL_ERRS <= MEFE) THEN
               ERR_SUB_NAM(NUM_EMG_FATAL_ERRS) = SUBR_NAME
               EMG_IFE(NUM_EMG_FATAL_ERRS,1) = 1917
               EMG_IFE(NUM_EMG_FATAL_ERRS,2) = METH
            ENDIF
         ENDIF
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
 1917 FORMAT(' *ERROR  1917: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' METHOD INDICATOR MUST BE 1 OR 2. VALUE IS ',I8)

! **********************************************************************************************************************************

      END SUBROUTINE MULT_OFFSET

      END SUBROUTINE ELMOFF

