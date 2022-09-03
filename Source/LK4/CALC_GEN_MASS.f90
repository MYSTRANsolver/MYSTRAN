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

      SUBROUTINE CALC_GEN_MASS

! Generates generalized mass from mass matrix and eigenvectors:

!   The generalized mass matrix is a square matrix of terms:

!         MIJ = EIGEN_VEC(i)'*MLL*EIGEN_VEC(j)   where EIGEN_VEC(i) is the ith eigenvector and MLL is the L-set mass matrix
!                                                The ' indicates a transpose of EIGEN_VEC(i)

!   Array GEN_MASS is a 1-D array of the diagonal terms, MIJ (i = j) from the square generalized mass matrix outlined above.
!   This subr calculates all NDOFL diagonal terms of the generalized mass matrix plus all off diagonal terms below the diagonal.
!   The diagonal terms go into array GEN_MASS. The off diagonal terms are not stored, only the largest one, MAXMIJ is kept, and
!   output later, so that the user will know to what accuracy the eigenvectors were calculated


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NDOFL, NTERM_KLLDn, NTERM_MLLn, NVEC, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  EPSIL
      USE SUBR_BEGEND_LEVELS, ONLY    :  CALC_GEN_MASS_BEGEND
      USE EIGEN_MATRICES_1, ONLY      :  GEN_MASS, EIGEN_VEC
      USE MODEL_STUF, ONLY            :  EIG_CRIT, MAXMIJ, MIJ_COL, MIJ_ROW, NUM_FAIL_CRIT
      USE SPARSE_MATRICES, ONLY       :  I_KLLDn, J_KLLDn, KLLDn, I_MLLn, J_MLLn, MLLn
      USE SPARSE_MATRICES, ONLY       :  SYM_MLLn
      USE LAPACK_BLAS_AUX

      USE CALC_GEN_MASS_USE_IFs

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'CALC_GEN_MASS'

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CALC_GEN_MASS_BEGEND
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices

      REAL(DOUBLE)                    :: DMIJ              ! DABS of MIJ
      REAL(DOUBLE)                    :: MAX               ! Temporary variable used in finding MAXMIJ
      REAL(DOUBLE)                    :: MIJ               ! The i,j-th value from gen. mass matrix. Used to find MAXMIJ
      REAL(DOUBLE)                    :: OUTVECI(NDOFL,1)  ! One eigenvector
      REAL(DOUBLE)                    :: OUTVECJ(NDOFL,1)  ! One eigenvector
      REAL(DOUBLE)                    :: ZVEC(NDOFL,1)     ! Intermediate matrix in the calculation of GEN_MASS

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages

      NUM_FAIL_CRIT = 0
      MIJ_ROW       = 1
      MIJ_COL       = 1
      MAX           = ZERO
      MAXMIJ        = ZERO

      DO I=1,NVEC

         WRITE(SC1,12345,ADVANCE='NO') I, NVEC, CR13

         DO K=1,NDOFL                                      ! Calc diag terms
            OUTVECI(K,1) = EIGEN_VEC(K,I)
         ENDDO

         IF (SOL_NAME(1:8) == 'BUCKLING') THEN
            CALL MATMULT_SFF ( 'KLLDn', NDOFL, NDOFL, NTERM_KLLDn, 'N'     , I_KLLDn, J_KLLDn, KLLDn, 'OUTVECI', NDOFL, 1,         &
                                OUTVECI, 'N', 'ZVEC', ONE, ZVEC )
         ELSE
            CALL MATMULT_SFF ( 'MLLn' , NDOFL, NDOFL, NTERM_MLLn , SYM_MLLn, I_MLLn , J_MLLn , MLLn , 'OUTVECI', NDOFL, 1,         &
                                OUTVECI, 'N', 'ZVEC', ONE, ZVEC )
         ENDIF

         GEN_MASS(I) = DDOT ( NDOFL, OUTVECI, 1, ZVEC, 1 )
         GEN_MASS(I) = ABS(GEN_MASS(I))
         IF (DEBUG(48) == 0) THEN                          ! Calc off-diag terms

            DO J=1,I-1

               WRITE(SC1,22345,ADVANCE='NO') J, NVEC, I, NVEC, CR13

               DO K=1,NDOFL
                  OUTVECJ(K,1) = EIGEN_VEC(K,J)
               ENDDO

               IF (SOL_NAME(1:8) == 'BUCKLING') THEN
                  CALL MATMULT_SFF ( 'KLLDn', NDOFL, NDOFL, NTERM_KLLDn, 'N'     , I_KLLDn, J_KLLDn, KLLDn, 'OUTVECJ', NDOFL, 1,   &
                                      OUTVECJ, 'N', 'ZVEC', ONE, ZVEC )
               ELSE
                  CALL MATMULT_SFF ( 'MLLn' , NDOFL, NDOFL, NTERM_MLLn , SYM_MLLn, I_MLLn , J_MLLn , MLLn , 'OUTVECJ', NDOFL, 1,   &
                                      OUTVECJ,'N', 'ZVEC', ONE, ZVEC )
               ENDIF

               MIJ = DDOT ( NDOFL, OUTVECI, 1, ZVEC, 1 )

               DMIJ = DABS(MIJ)
               IF (DMIJ > MAX) THEN
                  MAXMIJ  = MIJ
                  MAX     = DMIJ
                  MIJ_ROW = I
                  MIJ_COL = J
               ENDIF
               IF (DMIJ > EIG_CRIT) THEN
                  NUM_FAIL_CRIT = NUM_FAIL_CRIT + 1
               ENDIF
               
            ENDDO

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
12345 format(5X,'Diag term for eigenvector ',i5,' of ',i5,'                                  ',A)

22345 format(5X,'Off-diag term ',i5,' of ',i5,' for vector ',i5,' of ',i5,'                    ',A)

! **********************************************************************************************************************************

      END SUBROUTINE CALC_GEN_MASS
