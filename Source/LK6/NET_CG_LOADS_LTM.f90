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

      SUBROUTINE NET_CG_LOADS_LTM
 
! Merges matrices to get the net CG Loads Transformation Matrix (LTM):

!            CG_LTM   = MCG(-1)*TR6_CG'*| MRRcb  MRN  0RR |     6x(2R+N)

! For a description of Craig-Bamptom analyses, see Appendix D to the MYSTRAN User's Referance Manual

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFR, NTERM_MRRcbn, NTERM_MRN, NTERM_CG_LTM, NUM_CB_DOFS
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ONE
      USE PARAMS, ONLY                :  PRTCGLTM, WTMASS
      USE RIGID_BODY_DISP_MATS, ONLY  :  TR6_CG, TR6_0
      USE MODEL_STUF, ONLY            :  MCG
      USE OUTPUT4_MATRICES, ONLY      :  RBM0
      USE SPARSE_MATRICES, ONLY       :  SYM_MRN   , SYM_MRRcbn, SYM_CG_LTM  
      USE SPARSE_MATRICES, ONLY       :  I_MRRcbn  , J_MRRcbn  , MRRcbn   ,  I_MRN      , J_MRN      , MRN      ,                  &
                                         I_CG_LTM  , J_CG_LTM  , CG_LTM   

      USE SCRATCH_MATRICES, ONLY      :  I_CRS1, J_CRS1, CRS1, I_CRS2, J_CRS2, CRS2, I_CCS1, J_CCS1, CCS1

      USE SUBR_BEGEND_LEVELS, ONLY    :  NET_CG_LOADS_LTM_BEGEND

      USE NET_CG_LOADS_LTM_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'NET_CG_LOADS_LTM'

      INTEGER(LONG)                   :: AROW_MAX_TERMS    ! Max number of terms in any row of matrix A sent to subr MATMULT_SSS
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: INFO              ! Info designator from subrs DPOTRF and DPOTRI when INVERT_FF_MAT called
      INTEGER(LONG)                   :: NTERM_CCS1        ! Number of nonzero terms in scratch matrix CCS1
      INTEGER(LONG)                   :: NTERM_CRS1        ! Number of nonzero terms in scratch matrix CRS1
      INTEGER(LONG)                   :: NTERM_CRS2        ! Number of nonzero terms in scratch matrix CRS2
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = NET_CG_LOADS_LTM_BEGEND

      REAL(DOUBLE)                    :: DUM1(NDOFR,6)     ! MRRcbn*TR6_CG
      REAL(DOUBLE)                    :: DUM2(6,NDOFR)     ! 
      REAL(DOUBLE)                    :: WCG(6,6)          ! MCG/WTMASS
      REAL(DOUBLE)                    :: WBASIC(6,6)       ! RBM0/WTMASS
      REAL(DOUBLE)                    :: MCGI(6,6)         ! MCG inverse
      REAL(DOUBLE)                    :: SMALL             ! A number used in filtering out small numbers from a full matrix
      REAL(DOUBLE)                    :: TR6_CGt(6,NDOFR)  ! TR6_CG'

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************

! Calc MCG = TR6_CG'*MRRcbn*TR6_CG
                                                           ! 1st, multiply MRRcbn*TR6_CG to get DUM1
      CALL MATMULT_SFF('MRRcbn', NDOFR, NDOFR, NTERM_MRRcbn, SYM_MRRcbn, I_MRRcbn, J_MRRcbn, MRRcbn, 'TR6_CG', NDOFR, 6, TR6_CG,   &
                        'N','DUM1', ONE, DUM1)
      CALL MATMULT_FFF_T ( TR6_CG, DUM1, NDOFR, 6, 6, MCG )

! Calc and print WCG

      DO I=1,6
         DO J=1,6
            WCG(I,J)  = MCG(I,J)/WTMASS
         ENDDO
      ENDDO

      WRITE(F06,101)
      WRITE(F06,103)
      DO I=1,3
         WRITE(F06,109) (WCG(I,J),J=1,6)
      ENDDO
      WRITE(F06,103)
      DO I=4,6
         WRITE(F06,109) (WCG(I,J),J=1,6)
      ENDDO
      WRITE(F06,103)
      WRITE(F06,*)



! Calc RBM0 = TR6_0'*MRRcbn*TR6_0

      CALL MATMULT_SFF('MRRcbn', NDOFR, NDOFR, NTERM_MRRcbn, SYM_MRRcbn, I_MRRcbn, J_MRRcbn, MRRcbn, 'TR6_0', NDOFR, 6, TR6_0,    &
                        'N','DUM1', ONE, DUM1)
      CALL MATMULT_FFF_T ( TR6_0, DUM1, NDOFR, 6, 6, RBM0 )

! Calc and print WBASIC

      DO I=1,6
         DO J=1,6
            WBASIC(I,J)  = RBM0(I,J)/WTMASS
         ENDDO
      ENDDO

      WRITE(F06,102)
      WRITE(F06,103)
      DO I=1,3
         WRITE(F06,109) (WBASIC(I,J),J=1,6)
      ENDDO
      WRITE(F06,103)
      DO I=4,6
         WRITE(F06,109) (WBASIC(I,J),J=1,6)
      ENDDO
      WRITE(F06,103)
      WRITE(F06,*)

! Invert MCG (to MCGI). Note INFO is returned from INVERT_FF_MAT but any necessary action was taken there

      DO I=1,6                                             ! 1st set MCGI = MCG since INVERT will write over the input matrix
         DO J=1,6
            MCGI(I,J) = MCG(I,J)
         ENDDO
      ENDDO

      CALL INVERT_FF_MAT ( SUBR_NAME, 'MCG', MCGI, 6, INFO )

! Calc DUM2 =  MCG(-1)*TR6_CG'. First, transpose TR6_CG to TR6_CGt. Then rewrite DUM2 as a sparse matrix CRS1 so we can use
! MATMULT_SSS to multiply it times the sparse matrix CRS2 below (to get final CG loads LTM)
! If user wants cg LTM calculated such that translational terms are in G's, scale the upper 3 rows of the LTM (but do it on DUM2
! since it is easier here) 

      DO I=1,6
         DO J=1,NDOFR
            TR6_CGt(I,J) = TR6_CG(J,I)
         ENDDO
      ENDDO

      CALL MATMULT_FFF ( MCGI, TR6_CGt, 6, 6, NDOFR, DUM2 )
!     IF (SC_CGLTM == 'Y') THEN
         DO I=1,3
            DO J=1,NDOFR
               DUM2(I,J) = WTMASS*DUM2(I,J)
            ENDDO
         ENDDO
!     ENDIF

      CALL CNT_NONZ_IN_FULL_MAT ( 'MCGI*TR6_CGt', DUM2, 6, NDOFR, 'N', NTERM_CRS1, SMALL )
      CALL ALLOCATE_SCR_CRS_MAT ( 'CRS1', 6, NTERM_CRS1, SUBR_NAME )
      CALL FULL_TO_SPARSE_CRS ( 'MCGI*TR6_CGt', 6, NDOFR, DUM2, NTERM_CRS1, SMALL, SUBR_NAME, 'N', I_CRS1, J_CRS1, CRS1 )


! Merge MRRcbn and MRN (both in nonsym format) into nonsym format temporary scratch matrix CRS2.
! First allocate enough memory for merge of cols of MRRcbn with MRN  .

      NTERM_CRS2 = NTERM_MRRcbn + NTERM_MRN  
      CALL ALLOCATE_SCR_CRS_MAT ( 'CRS2', NDOFR, NTERM_CRS2, SUBR_NAME )
      CALL MERGE_MAT_COLS_SSS ( 'MRRcbn', NTERM_MRRcbn, I_MRRcbn, J_MRRcbn, MRRcbn, SYM_MRRcbn, NDOFR,                             &
                                'MRN  ' , NTERM_MRN   , I_MRN   , J_MRN   , MRN   , SYM_MRN   , NDOFR,                             &
                                'CRS2'  ,               I_CRS2  , J_CRS2  , CRS2  , 'N'        )

! Calc CG loads LTM: Mult DUM2 = MCG(-1)*TR6_CG' times CRS2 = | MRRcbn  MRN 0 |. First, convert CRS2 to sparse col storage in CCS1

      NTERM_CCS1 = NTERM_CRS2                              ! CCS1 will be CCS strage of | MRRcbn  MRN 0 |
      CALL ALLOCATE_SCR_CCS_MAT ( 'CCS1', NUM_CB_DOFS, NTERM_CCS1, SUBR_NAME )
      CALL SPARSE_CRS_SPARSE_CCS ( NDOFR, NUM_CB_DOFS, NTERM_CRS2, 'CRS2', I_CRS2, J_CRS2, CRS2, 'CCS1', J_CCS1, I_CCS1, CCS1, 'Y')
      CALL DEALLOCATE_SCR_MAT ( 'CRS2' )

      CALL MATMULT_SSS_NTERM ( 'MCGI*TR6_CGt'     , 6          , NTERM_CRS1  , 'N', I_CRS1, J_CRS1,                                &
                               '| MRRcbn  MRN 0 |', NUM_CB_DOFS, NTERM_CCS1  , 'N', J_CCS1, I_CCS1, AROW_MAX_TERMS,                &
                               'CG_LTM'           ,              NTERM_CG_LTM   )

      CALL ALLOCATE_SPARSE_MAT ( 'CG_LTM', NDOFR, NTERM_CG_LTM  , SUBR_NAME )

      CALL MATMULT_SSS ( 'MCGI*TR6_CGt'     , 6          , NTERM_CRS1  , 'N', I_CRS1  , J_CRS1  , CRS1  ,                          &
                         '| MRRcbn  MRN 0 |', NUM_CB_DOFS, NTERM_CCS1  , 'N', J_CCS1  , I_CCS1  , CCS1  ,  AROW_MAX_TERMS,         &
                         'CG_LTM'          , ONE         , NTERM_CG_LTM,      I_CG_LTM, J_CG_LTM, CG_LTM   )

      CALL DEALLOCATE_SCR_MAT ( 'CRS1' )
      CALL DEALLOCATE_SCR_MAT ( 'CCS1' )

      IF (PRTCGLTM > 0) THEN
         CALL WRITE_SPARSE_CRS ( 'CG_LTM','  ','  ', NTERM_CG_LTM, NDOFR, I_CG_LTM, J_CG_LTM, CG_LTM   )
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF
 
      RETURN

! **********************************************************************************************************************************
  101 FORMAT(40X,'RIGID BODY WEIGHT MATRIX RELATIVE TO THE CG OF THE MODEL',/,                                                     &
             40X,'                       MATRIX WCG',/)

  102 FORMAT(35X,'RIGID BODY WEIGHT MATRIX RELATIVE TO THE MODEL BASIC SYSTEM ORIGIN',/,                                           &
             40X,'                      MATRIX WBASIC',/)

  103 FORMAT(19X,'-----------------------------------------------------------------------------------------------------')

  109 FORMAT(19X,'|',2(1ES13.6,4X),1ES13.6,'  | ',2(1ES13.6,4X),1ES13.6,' |')

  142 FORMAT(21X,6(1ES15.6))


! **********************************************************************************************************************************

      END SUBROUTINE NET_CG_LOADS_LTM