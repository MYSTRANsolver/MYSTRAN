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

      SUBROUTINE REDUCE_KFF_TO_KAA ( PART_VEC_F_AO )
 
! Call routines to reduce the KFF linear stiffness matrix from the F-set to the A, O-sets
 
! NOTE: This subr has code for sparse matrices as well as full matrices, (i.e. Bulk Data PARAM MATSPARS = 'Y' for sparse and 'N'
! for full). The code for full matrices was put in originally in order that the sparse code could be thoroughly checked. That task
! is complete and the remaining full matrix code has not been maintained since around 2005. In addition, new capability added to
! MYSTRAN since that approx time does not have full matrix code.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, L2E, LINK2E, L2E_MSG, SC1, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, KOO_SDIA, NDOFF, NDOFA, NDOFO, NTERM_KFF,       &
                                         NTERM_KAA, NTERM_KAO, NTERM_KOO, NTERM_GOA
      USE PARAMS, ONLY                :  KOORAT, MATSPARS, SOLLIB, SPARSTOR, SPARSE_FLAVOR, RCONDK
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_KFF_TO_KAA_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE 
      USE FULL_MATRICES, ONLY         :  KAA_FULL, KAO_FULL, GOA_FULL, DUM1, DUM2
      USE SPARSE_MATRICES, ONLY       :  I_KFF, J_KFF, KFF, I_KAA, J_KAA, KAA, I_KAO, J_KAO, KAO, I_GOA, J_GOA, GOA,               &
                                         I_KOO, J_KOO, KOO
                                         
      USE SPARSE_MATRICES, ONLY       :  SYM_GOA, SYM_KFF, SYM_KAA, SYM_KAO, SYM_KOO
      USE SCRATCH_MATRICES
 
      USE REDUCE_KFF_TO_KAA_USE_IFs

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_KFF_TO_KAA'
      CHARACTER(  1*BYTE)             :: CLOSE_IT            ! Input to subr READ_MATRIX_i. 'Y'/'N' whether to close a file or not 
      CHARACTER(  8*BYTE)             :: CLOSE_STAT          ! Char constant for the CLOSE status of a file
      CHARACTER(  1*BYTE)             :: EQUED               ! 'Y' if the stiff matrix was equilibrated in subr EQUILIBRATE    
      CHARACTER(  1*BYTE)             :: EQUIL_KOO           ! 'Y'/'N' for whether to equilibrate KOO in subr SYM_MAT_DECOMP_LAPACK
      CHARACTER(  1*BYTE)             :: SYM_CRS2            ! Storage format for matrix CRS2 (either 'Y' for sym storage or
!                                                              'N' for nonsymmetric storage)
 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_F_AO(NDOFF)! Partitioning vector (F set into A and O sets) 
      INTEGER(LONG)                   :: AROW_MAX_TERMS      ! Output from MATMULT_SFS_NTERM and input to MATMULT_SFS

      INTEGER(LONG)                   :: DEB_PRT(2)          ! Debug numbers to say whether to write ABAND and/or its decomp
!                                                              in called subr SYM_MAT_DECOMP_LAPACK

      INTEGER(LONG)                   :: I,J                 ! DO loop indices
      INTEGER(LONG)                   :: INFO        = 0     ! Input value for subr SYM_MAT_DECOMP_LAPACK (quit on sing KRRCB)
      INTEGER(LONG), PARAMETER        :: ITRNSPB     = 0     ! Transpose indicator for matrix multiply routine
      INTEGER(LONG)                   :: KAA_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KAO_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KOO_ROW_MAX_TERMS   ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: NTERM_CRS1          ! Number of terms in matrix CRS1  
      INTEGER(LONG)                   :: NTERM_CRS2          ! Number of terms in matrix CRS2  
      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_KFF_TO_KAA_BEGEND

      REAL(DOUBLE)                    :: ALPHA = ONE         ! Scalar multiplier for matrix
      REAL(DOUBLE)                    :: BETA  = ONE         ! Scalar multiplier for matrix
      REAL(DOUBLE)                    :: K_INORM             ! Inf norm of KOO matrix

      REAL(DOUBLE)                    :: KOO_SCALE_FACS(NDOFO)
                                                             ! KOO equilibration scale factors

      REAL(DOUBLE)                    :: RCOND               ! Recrip of cond no. of the KLL. Det in  subr COND_NUM
      REAL(DOUBLE)                    :: SMALL               ! A number used in filtering out small numbers from a full matrix
 
      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Initialize outputs

      KOO_SDIA   = 0

! Partition KAA from KFF (This is KAA before reduction, or KAA(bar) )

      IF (NDOFA > 0) THEN

         CALL PARTITION_SS_NTERM ( 'KFF', NTERM_KFF, NDOFF, NDOFF, SYM_KFF, I_KFF, J_KFF,      PART_VEC_F_AO, PART_VEC_F_AO,       &
                                    NUM1, NUM1, KAA_ROW_MAX_TERMS, 'KAA', NTERM_KAA, SYM_KAA ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KAA', NDOFA, NTERM_KAA, SUBR_NAME )

         IF (NTERM_KAA > 0) THEN      
            CALL PARTITION_SS ( 'KFF', NTERM_KFF, NDOFF, NDOFF, SYM_KFF, I_KFF, J_KFF, KFF, PART_VEC_F_AO, PART_VEC_F_AO,          &
                                 NUM1, NUM1, KAA_ROW_MAX_TERMS, 'KAA', NTERM_KAA, NDOFA, SYM_KAA, I_KAA, J_KAA, KAA )
         ENDIF

      ENDIF

! Partition KAO from KFF

      IF ((NDOFA > 0) .AND. (NDOFO > 0)) THEN

         CALL PARTITION_SS_NTERM ( 'KFF', NTERM_KFF, NDOFF, NDOFF, SYM_KFF, I_KFF, J_KFF,      PART_VEC_F_AO, PART_VEC_F_AO,       &
                                    NUM1, NUM2, KAO_ROW_MAX_TERMS, 'KAO', NTERM_KAO, SYM_KAO ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KAO', NDOFA, NTERM_KAO, SUBR_NAME )

         IF (NTERM_KAO > 0) THEN
            CALL PARTITION_SS ( 'KFF', NTERM_KFF, NDOFF, NDOFF, SYM_KFF, I_KFF, J_KFF, KFF, PART_VEC_F_AO, PART_VEC_F_AO,          &
                                 NUM1, NUM2, KAO_ROW_MAX_TERMS, 'KAO', NTERM_KAO, NDOFA, SYM_KAO, I_KAO, J_KAO, KAO )
         ENDIF

      ENDIF

! Partition KOO from KFF

      IF (NDOFO > 0) THEN

         CALL PARTITION_SS_NTERM ( 'KFF', NTERM_KFF, NDOFF, NDOFF, SYM_KFF, I_KFF, J_KFF,      PART_VEC_F_AO, PART_VEC_F_AO,       &
                                    NUM2, NUM2, KOO_ROW_MAX_TERMS, 'KOO', NTERM_KOO, SYM_KOO ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KOO', NDOFO, NTERM_KOO, SUBR_NAME )

         IF (NTERM_KOO > 0) THEN
            CALL PARTITION_SS ( 'KFF', NTERM_KFF, NDOFF, NDOFF, SYM_KFF, I_KFF, J_KFF, KFF, PART_VEC_F_AO, PART_VEC_F_AO,          &
                                 NUM2, NUM2, KOO_ROW_MAX_TERMS, 'KOO', NTERM_KOO, NDOFO, SYM_KOO, I_KOO, J_KOO, KOO )
         ENDIF

      ENDIF

! Decompose KOO, if there are any O-set DOF's

      IF (NDOFO > 0) THEN

         DEB_PRT(1) = 28
         DEB_PRT(2) = 29

         IF (SOLLIB == 'BANDED  ') THEN                    ! Use LAPACK

            KOO_SDIA   = 0
            EQUIL_KOO  = 'N'
            INFO = 0
            CALL SYM_MAT_DECOMP_LAPACK ( SUBR_NAME, 'KOO', 'O ', NDOFO, NTERM_KOO, I_KOO, J_KOO, KOO, 'Y', KOORAT, EQUIL_KOO,      &
                                         RCONDK, DEB_PRT, EQUED, KOO_SDIA, K_INORM, RCOND, KOO_SCALE_FACS, INFO )
         ELSE IF (SOLLIB == 'SPARSE  ') THEN
         
            IF (SPARSE_FLAVOR(1:7) == 'SUPERLU') THEN

               INFO = 0
               CALL SYM_MAT_DECOMP_SUPRLU ( SUBR_NAME, 'KOO', NDOFO, NTERM_KOO, I_KOO, J_KOO, KOO, INFO )

            ELSE

               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,9991) SUBR_NAME, 'SPARSE_FLAVOR'
               WRITE(F06,9991) SUBR_NAME, 'SPARSE_FLAVOR'
               CALL OUTA_HERE ( 'Y' )

            ENDIF


         ELSE

            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,9991) SUBR_NAME, 'SOLLIB'
            WRITE(F06,9991) SUBR_NAME, 'SOLLIB'
            CALL OUTA_HERE ( 'Y' )

         ENDIF

      ENDIF

! Solve for GOA = -KOO(-1)*KAO'

      IF (NTERM_KAO > 0) THEN

         CALL SOLVE_GOA                                      ! Solve for GOA matrix
         CLOSE_IT   = 'Y'
         CLOSE_STAT = 'KEEP'
         CALL WRITE_MATRIX_1 ( LINK2E, L2E, CLOSE_IT, CLOSE_STAT, L2E_MSG, 'GOA', NTERM_GOA, NDOFO, I_GOA, J_GOA, GOA )

      ELSE

         NTERM_GOA = 0                                     ! GOA is null
         CALL ALLOCATE_SPARSE_MAT ( 'GOA', NDOFO, NTERM_GOA, SUBR_NAME )

      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
! Reduce KFF to KAA = KAA(bar) + KAO*GOA. Note: (KAO*GOA)' + GOA'*KOO*GOA = 0 by definition of GOA = -KOO(-1)*KAO'
! If PARAM MATSPARS = 'Y', then we use sparse matrix operations (multiply/add/transpose). If not, use full matrix operations

      IF (MATSPARS == 'Y') THEN                            ! Reduce KFF to KAA using sparse matrix operations

         IF (NTERM_KAO > 0) THEN                           ! Calc KAO*GOA & add it orig KAA


                                                           ! CCS1 will be sparse CCS format version of sparse CRS matrix GOA
            CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NDOFA, NTERM_GOA, SUBR_NAME )
            CALL SPARSE_CRS_SPARSE_CCS ( NDOFO, NDOFA, NTERM_GOA, 'GOA', I_GOA, J_GOA, GOA, 'CCS1', J_CCS1, I_CCS1, CCS1, 'Y' )

                                                           ! Sparse multiply to get CRS1 = KAO*GOA. Use CCS1 for GOA CCS
            CALL MATMULT_SSS_NTERM ( 'KAO' , NDOFA, NTERM_KAO, SYM_KAO, I_KAO , J_KAO ,                                            &
                                     'GOA' , NDOFA, NTERM_GOA, SYM_GOA, J_CCS1, I_CCS1, AROW_MAX_TERMS,                            &
                                     'CRS1' ,       NTERM_CRS1 )

            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFA, NTERM_CRS1, SUBR_NAME )

            CALL MATMULT_SSS ( 'KAO' , NDOFA, NTERM_KAO , SYM_KAO, I_KAO , J_KAO , KAO ,                                           &
                               'GOA' , NDOFA, NTERM_GOA , SYM_GOA, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                           &
                               'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )

            CALL DEALLOCATE_SCR_MAT ( 'CCS1' )

                                                           ! CRS1 = KAO*GOA has all nonzero terms in it.
            IF      (SPARSTOR == 'SYM   ') THEN            !      If SPARSTOR == 'SYM   ', rewrite CRS1 as sym in CRS2     

               CALL SPARSE_CRS_TERM_COUNT ( NDOFA, NTERM_CRS1, 'CRS1 = KAO*GOA all nonzeros', I_CRS1, J_CRS1, NTERM_CRS2 )
               CALL ALLOCATE_SCR_CRS_MAT ( 'CRS2', NDOFA, NTERM_CRS2, SUBR_NAME )
               CALL CRS_NONSYM_TO_CRS_SYM ( 'CRS1 = KAO*GOA all nonzeros', NDOFA, NTERM_CRS1, I_CRS1, J_CRS1, CRS1,                &
                                            'CRS2 = KAO*GOA stored sym'  ,        NTERM_CRS2, I_CRS2, J_CRS2, CRS2 )
               SYM_CRS2 = 'Y'

            ELSE IF (SPARSTOR == 'NONSYM') THEN            !      If SPARSTOR == 'NONSYM', rewrite CRS1 in CRS2 with NTERM_CRS2

               NTERM_CRS2 = NTERM_CRS1
               CALL ALLOCATE_SCR_CRS_MAT ( 'CRS2', NDOFA, NTERM_CRS2, SUBR_NAME )
               DO I=1,NDOFA+1
                  I_CRS2(I) = I_CRS1(I)
               ENDDO
               DO I=1,NTERM_CRS2
                  J_CRS2(I) = J_CRS1(I)
                    CRS2(I) =   CRS1(I)
               ENDDO
               SYM_CRS2 = 'N'

            ELSE                                           ! Error - incorrect SPARSTOR

               WRITE(ERR,932) SUBR_NAME, SPARSTOR
               WRITE(F06,932) SUBR_NAME, SPARSTOR
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )

            ENDIF

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! Deallocate CRS1
                                                           ! Sparse add to get CRS1 = KAA-bar + CRS2
            CALL MATADD_SSS_NTERM ( NDOFA, 'KAA-bar', NTERM_KAA, I_KAA , J_KAA , SYM_KAA , 'KOO*GOA', NTERM_CRS2,                  &
                                                                 I_CRS2, J_CRS2, SYM_CRS2, 'CRS1', NTERM_CRS1 )
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFA, NTERM_CRS1, SUBR_NAME )
            CALL MATADD_SSS (NDOFA, 'KAA-bar' , NTERM_KAA , I_KAA , J_KAA , KAA , ONE, 'KOO*GOA', NTERM_CRS2, I_CRS2, J_CRS2, CRS2,&
                            ONE, 'CRS1', NTERM_CRS1, I_CRS1, J_CRS1, CRS1 )

            CALL DEALLOCATE_SCR_MAT ( 'CRS2' )             ! Deallocate CRS2

            NTERM_KAA = NTERM_CRS1                         ! Reallocate KAA to be size of CRS1
            WRITE(SC1, * ) '    Reallocate KAA'
      !xx   WRITE(SC1, * )                                 ! Advance 1 line for screen messages         
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KAA', CR13   
            CALL DEALLOCATE_SPARSE_MAT ( 'KAA' )
            WRITE(SC1,12345,ADVANCE='NO') '       Allocate   KAA', CR13   
            CALL ALLOCATE_SPARSE_MAT ( 'KAA', NDOFA, NTERM_KAA, SUBR_NAME )
                                                           ! Set KAA = CRS1
            DO I=1,NDOFA+1
               I_KAA(I) = I_CRS1(I)
            ENDDO
            DO J=1,NTERM_KAA
               J_KAA(J) = J_CRS1(J)
                 KAA(J) =   CRS1(J)
            ENDDO 

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! Deallocate CRS1

         ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
      ELSE IF (MATSPARS == 'N') THEN
                                                           ! Calc reduced KAA <= KAA + KAO*GOA
         CALL ALLOCATE_FULL_MAT ('KAA_FULL', NDOFA, NDOFA, SUBR_NAME )

         IF (NTERM_KAA > 0) THEN
            CALL SPARSE_CRS_TO_FULL ( 'KAA', NTERM_KAA, NDOFA, NDOFA, SYM_KAA, I_KAA, J_KAA, KAA, KAA_FULL )
         ENDIF

         IF (NTERM_KAO > 0) THEN                           ! Part 1: calc KAO*GOA and add it & it's transpose to KNN_FULL

            CALL ALLOCATE_FULL_MAT ( 'KAO_FULL', NDOFA, NDOFO, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'KAO', NTERM_KAO, NDOFA, NDOFO, SYM_KAO, I_KAO, J_KAO, KAO, KAO_FULL )

            CALL ALLOCATE_FULL_MAT ( 'GOA_FULL', NDOFO, NDOFA, SUBR_NAME )
            CALL SPARSE_CRS_TO_FULL ( 'GOA', NTERM_GOA, NDOFO, NDOFA, SYM_GOA, I_GOA, J_GOA, GOA, GOA_FULL )

            CALL ALLOCATE_FULL_MAT ( 'DUM1', NDOFA, NDOFA, SUBR_NAME )

            CALL MATMULT_FFF (  KAO_FULL, GOA_FULL, NDOFA, NDOFO, NDOFA, DUM1 )
            CALL DEALLOCATE_FULL_MAT ( 'KAO_FULL' )
            CALL DEALLOCATE_FULL_MAT ( 'GOA_FULL' )

            CALL ALLOCATE_FULL_MAT ( 'DUM2', NDOFA, NDOFA, SUBR_NAME )
            CALL MATADD_FFF ( KAA_FULL, DUM1, NDOFA, NDOFA, ALPHA, BETA, ITRNSPB, DUM2 )
            DO I=1,NDOFA
               DO J=1,NDOFA
                  KAA_FULL(I,J) = DUM2(I,J)
               ENDDO
            ENDDO

            CALL DEALLOCATE_FULL_MAT ( 'DUM1' )
            CALL DEALLOCATE_FULL_MAT ( 'DUM2' )

         ENDIF

         CALL CNT_NONZ_IN_FULL_MAT ( 'KAA_FULL  ', KAA_FULL, NDOFA, NDOFA, SYM_KAA, NTERM_KAA, SMALL )

         WRITE(SC1, * ) '    Reallocate KAA'
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KAA', CR13
         CALL DEALLOCATE_SPARSE_MAT ( 'KAA' )
         WRITE(SC1,12345,ADVANCE='NO') '       Allocate   KAA', CR13   
         CALL ALLOCATE_SPARSE_MAT ( 'KAA', NDOFA, NTERM_KAA, SUBR_NAME )

         IF (NTERM_KAA > 0) THEN                           ! Create new sparse arrays from KAA_FULL
!xx         CALL FULL_TO_SPARSE_CRS ( 'KAA_FULL  ', NDOFA, NDOFA, KAA_FULL, NTERM_KAA, SYM_KAA, I_KAA, J_KAA, KAA )
            CALL DEALLOCATE_FULL_MAT ( 'KAA_FULL' )
         ENDIF

      ELSE

         WRITE(ERR,911) SUBR_NAME,MATSPARS
         WRITE(F06,911) SUBR_NAME,MATSPARS
         FATAL_ERR = FATAL_ERR + 1
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
  911 FORMAT(' *ERROR   911: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' PARAMETER MATSPARS MUST BE EITHER ','Y',' OR ','N',' BUT VALUE IS ',A)

  932 FORMAT(' *ERROR   932: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' PARAMETER SPARSTOR MUST BE EITHER "SYM" OR "NONSYM" BUT VALUE IS ',A)

 9991 FORMAT(' *ERROR  9991: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,A, ' = ',A,' NOT PROGRAMMED ',A)

12345 FORMAT(A,10X,A)

! **********************************************************************************************************************************

      END SUBROUTINE REDUCE_KFF_TO_KAA
