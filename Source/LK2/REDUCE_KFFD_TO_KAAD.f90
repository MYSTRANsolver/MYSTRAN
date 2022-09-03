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

      SUBROUTINE REDUCE_KFFD_TO_KAAD ( PART_VEC_F_AO )
 
! Call routines to reduce the KFFD differential stiffness matrix from the F-set to the A, O-sets
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, L2E, LINK2E, L2E_MSG, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FACTORED_MATRIX, FATAL_ERR, NDOFF, NDOFA, NDOFO, NTERM_KFFD, NTERM_KAAD,    &
                                         NTERM_KAOD, NTERM_KOOD, NTERM_KOODs, NTERM_GOA
      USE PARAMS, ONLY                :  EPSIL, KOORAT, SPARSTOR, RCONDK
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  REDUCE_KFFD_TO_KAAD_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE 
      USE SPARSE_MATRICES, ONLY       :  I_KFFD, J_KFFD, KFFD, I_KAAD, J_KAAD, KAAD, I_KAOD, J_KAOD, KAOD, I_GOA, J_GOA, GOA,      &
                                         I_KOOD, I2_KOOD, J_KOOD, KOOD, I_KOODs, I2_KOODs, J_KOODs, KOODs
                                         
      USE SPARSE_MATRICES, ONLY       :  SYM_GOA, SYM_KFFD, SYM_KAAD, SYM_KAOD, SYM_KOOD
      USE SCRATCH_MATRICES
 
      USE REDUCE_KFFD_TO_KAAD_USE_IFs

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'REDUCE_KFFD_TO_KAAD'
      CHARACTER(  1*BYTE)             :: SYM_CRS2            ! Storage format for matrix CRS2 (either 'Y' for sym storage or
!                                                              'N' for nonsymmetric storage)
 
      INTEGER(LONG), INTENT(IN)       :: PART_VEC_F_AO(NDOFF)! Partitioning vector (F set into A and O sets) 
      INTEGER(LONG)                   :: AROW_MAX_TERMS      ! Output from MATMULT_SFS_NTERM and input to MATMULT_SFS

      INTEGER(LONG)                   :: I,J                 ! DO loop indices
      INTEGER(LONG)                   :: KAAD_ROW_MAX_TERMS  ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KAOD_ROW_MAX_TERMS  ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: KOOD_ROW_MAX_TERMS  ! Output from subr PARTITION_SIZE (max terms in any row of matrix)
      INTEGER(LONG)                   :: NTERM_CRS1          ! Number of terms in matrix CRS1  
      INTEGER(LONG)                   :: NTERM_CRS2          ! Number of terms in matrix CRS2  
      INTEGER(LONG), PARAMETER        :: NUM1        = 1     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: NUM2        = 2     ! Used in subr's that partition matrices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = REDUCE_KFFD_TO_KAAD_BEGEND

      INTRINSIC                       :: DABS

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Partition KAAD from KFFD (This is KAAD before reduction, or KAAD(bar) )

      IF (NDOFA > 0) THEN

         CALL PARTITION_SS_NTERM ( 'KFFD', NTERM_KFFD, NDOFF, NDOFF, SYM_KFFD, I_KFFD, J_KFFD,      PART_VEC_F_AO, PART_VEC_F_AO,  &
                                    NUM1, NUM1, KAAD_ROW_MAX_TERMS, 'KAAD', NTERM_KAAD, SYM_KAAD ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KAAD', NDOFA, NTERM_KAAD, SUBR_NAME )

         IF (NTERM_KAAD > 0) THEN      
            CALL PARTITION_SS ( 'KFFD', NTERM_KFFD, NDOFF, NDOFF, SYM_KFFD, I_KFFD, J_KFFD, KFFD, PART_VEC_F_AO, PART_VEC_F_AO,    &
                                 NUM1, NUM1, KAAD_ROW_MAX_TERMS, 'KAAD', NTERM_KAAD, NDOFA, SYM_KAAD, I_KAAD, J_KAAD, KAAD )
         ENDIF

      ENDIF

! Partition KAOD from KFFD

      IF ((NDOFA > 0) .AND. (NDOFO > 0)) THEN

         CALL PARTITION_SS_NTERM ( 'KFFD', NTERM_KFFD, NDOFF, NDOFF, SYM_KFFD, I_KFFD, J_KFFD,      PART_VEC_F_AO, PART_VEC_F_AO,  &
                                    NUM1, NUM2, KAOD_ROW_MAX_TERMS, 'KAOD', NTERM_KAOD, SYM_KAOD ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KAOD', NDOFA, NTERM_KAOD, SUBR_NAME )

         IF (NTERM_KAOD > 0) THEN
            CALL PARTITION_SS ( 'KFFD', NTERM_KFFD, NDOFF, NDOFF, SYM_KFFD, I_KFFD, J_KFFD, KFFD, PART_VEC_F_AO, PART_VEC_F_AO,    &
                                 NUM1, NUM2, KAOD_ROW_MAX_TERMS, 'KAOD', NTERM_KAOD, NDOFA, SYM_KAOD, I_KAOD, J_KAOD, KAOD )
         ENDIF

      ENDIF

! Partition KOOD from KFFD

      IF (NDOFO > 0) THEN

         CALL PARTITION_SS_NTERM ( 'KFFD', NTERM_KFFD, NDOFF, NDOFF, SYM_KFFD, I_KFFD, J_KFFD,      PART_VEC_F_AO, PART_VEC_F_AO,  &
                                    NUM2, NUM2, KOOD_ROW_MAX_TERMS, 'KOOD', NTERM_KOOD, SYM_KOOD ) 

         CALL ALLOCATE_SPARSE_MAT ( 'KOOD', NDOFO, NTERM_KOOD, SUBR_NAME )

         IF (NTERM_KOOD > 0) THEN
            CALL PARTITION_SS ( 'KFFD', NTERM_KFFD, NDOFF, NDOFF, SYM_KFFD, I_KFFD, J_KFFD, KFFD, PART_VEC_F_AO, PART_VEC_F_AO,    &
                                 NUM2, NUM2, KOOD_ROW_MAX_TERMS, 'KOOD', NTERM_KOOD, NDOFO, SYM_KOOD, I_KOOD, J_KOOD, KOOD )
         ENDIF

      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
! Reduce KFFD to KAAD = KAAD(bar) + KAOD*GOA. Note: (KAOD*GOA)' + GOA'*KOOD*GOA = 0 by definition of GOA = -KOOD(-1)*KAOD'
! If PARAM MATSPARS = 'Y', then we use sparse matrix operations (multiply/add/transpose). If not, use full matrix operations

         IF (NTERM_KAOD > 0) THEN                          ! Calc KAOD*GOA & add it orig KAAD


                                                           ! CCS1 will be sparse CCS format version of sparse CRS matrix GOA
            CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NDOFA, NTERM_GOA, SUBR_NAME )
            CALL SPARSE_CRS_SPARSE_CCS ( NDOFO, NDOFA, NTERM_GOA, 'GOA', I_GOA, J_GOA, GOA, 'CCS1', J_CCS1, I_CCS1, CCS1, 'Y' )

                                                           ! Sparse multiply to get CRS1 = KAOD*GOA. Use CCS1 for GOA CCS
            CALL MATMULT_SSS_NTERM ( 'KAOD' , NDOFA, NTERM_KAOD, SYM_KAOD, I_KAOD , J_KAOD ,                                       &
                                     'GOA' , NDOFA, NTERM_GOA, SYM_GOA, J_CCS1, I_CCS1, AROW_MAX_TERMS,                            &
                                     'CRS1' ,       NTERM_CRS1 )

            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFA, NTERM_CRS1, SUBR_NAME )

            CALL MATMULT_SSS ( 'KAOD' , NDOFA, NTERM_KAOD , SYM_KAOD, I_KAOD , J_KAOD , KAOD ,                                     &
                               'GOA' , NDOFA, NTERM_GOA , SYM_GOA, J_CCS1, I_CCS1, CCS1, AROW_MAX_TERMS,                           &
                               'CRS1', ONE  , NTERM_CRS1,          I_CRS1, J_CRS1, CRS1 )

            CALL DEALLOCATE_SCR_MAT ( 'CCS1' )

                                                           ! CRS1 = KAOD*GOA has all nonzero terms in it.
            IF      (SPARSTOR == 'SYM   ') THEN            !      If SPARSTOR == 'SYM   ', rewrite CRS1 as sym in CRS2     

               CALL SPARSE_CRS_TERM_COUNT ( NDOFA, NTERM_CRS1, 'CRS1 = KAOD*GOA all nonzeros', I_CRS1, J_CRS1, NTERM_CRS2 )
               CALL ALLOCATE_SCR_CRS_MAT ( 'CRS2', NDOFA, NTERM_CRS2, SUBR_NAME )
               CALL CRS_NONSYM_TO_CRS_SYM ( 'CRS1 = KAOD*GOA all nonzeros', NDOFA, NTERM_CRS1, I_CRS1, J_CRS1, CRS1,               &
                                            'CRS2 = KAOD*GOA stored sym'  ,        NTERM_CRS2, I_CRS2, J_CRS2, CRS2 )
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
!xx            SYM_CRS2 = 'Y'      ! NO: SHOULD BE 'N' HERE SINCE SPARSTOR = 'NONSYM' IN THIS BRANCH OF THE IF BLOCK
               SYM_CRS2 = 'N'

            ELSE                                           ! Error - incorrect SPARSTOR

               WRITE(ERR,932) SUBR_NAME, SPARSTOR
               WRITE(F06,932) SUBR_NAME, SPARSTOR
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )

            ENDIF

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! Deallocate CRS1
                                                           ! Sparse add to get CRS1 = KAAD-bar + CRS2
            CALL MATADD_SSS_NTERM ( NDOFA, 'KAAD-bar', NTERM_KAAD, I_KAAD , J_KAAD , SYM_KAAD , 'KOOD*GOA', NTERM_CRS2,            &
                                                                 I_CRS2, J_CRS2, SYM_CRS2, 'CRS1', NTERM_CRS1 )
            CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', NDOFA, NTERM_CRS1, SUBR_NAME )
            CALL MATADD_SSS (NDOFA, 'KAAD-bar' , NTERM_KAAD , I_KAAD , J_KAAD , KAAD , ONE, 'KOOD*GOA', NTERM_CRS2,                &
                             I_CRS2, J_CRS2, CRS2, ONE, 'CRS1', NTERM_CRS1, I_CRS1, J_CRS1, CRS1 )

            CALL DEALLOCATE_SCR_MAT ( 'CRS2' )             ! Deallocate CRS2

            NTERM_KAAD = NTERM_CRS1                        ! Reallocate KAAD to be size of CRS1
            WRITE(SC1, * ) '    Reallocate KAAD'
      !xx   WRITE(SC1, * )                                 ! Advance 1 line for screen messages         
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KAAD', CR13
            CALL DEALLOCATE_SPARSE_MAT ( 'KAAD' )
            WRITE(SC1,12345,ADVANCE='NO') '       Allocate   KAAD', CR13   
            CALL ALLOCATE_SPARSE_MAT ( 'KAAD', NDOFA, NTERM_KAAD, SUBR_NAME )
                                                           ! Set KAAD = CRS1
            DO I=1,NDOFA+1
               I_KAAD(I) = I_CRS1(I)
            ENDDO
            DO J=1,NTERM_KAAD
               J_KAAD(J) = J_CRS1(J)
                 KAAD(J) =   CRS1(J)
            ENDDO 

            CALL DEALLOCATE_SCR_MAT ( 'CRS1' )             ! Deallocate CRS1

         ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  932 FORMAT(' *ERROR   932: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' PARAMETER SPARSTOR MUST BE EITHER "SYM" OR "NONSYM" BUT VALUE IS ',A)

 2400 FORMAT(' *ERROR  2400: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' THERE IS AN O-SET BUT GUYAN REDUCTION MATRIX GOA HAS ',I12,' TERMS IN IT. MUST BE > 0')

12345 FORMAT(A,10X,A)

! **********************************************************************************************************************************

      END SUBROUTINE REDUCE_KFFD_TO_KAAD
