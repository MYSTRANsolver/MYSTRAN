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

      SUBROUTINE MERGE_LTM  
 
! Merges CG_LTM and IF_LTM into LTM matrix:

!                    | CG_LTM |        6 x NUM_CB_DOFS      1st 6 rows of LTM
!            LTM   = |        |
!                    | IF_LTM |        NDOFR x NUM_CB_DOFS  last NDOFR rows of LTM

 
! For a description of Craig-Bamptom analyses, see Appendix D to the MYSTRAN User's Referance Manual


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFR, NTERM_CG_LTM, NTERM_IF_LTM, NTERM_LTM, NUM_CB_DOFS
      USE TIMDAT, ONLY                :  TSEC
      USE SPARSE_MATRICES, ONLY       :  I_CG_LTM, J_CG_LTM, CG_LTM, I_IF_LTM, J_IF_LTM, IF_LTM, I_LTM, J_LTM, LTM
      USE SUBR_BEGEND_LEVELS, ONLY    :  MERGE_LTM_BEGEND

      USE MERGE_LTM_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MERGE_LTM  '

      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: LTM_MERGE_VEC(6+NDOFR) 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MERGE_LTM_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Merge CG_LTM rows with rows ofIF_LTM into LTM

      NTERM_LTM = NTERM_CG_LTM + NTERM_IF_LTM

      CALL ALLOCATE_SPARSE_MAT ( 'LTM', 6+NDOFR, NTERM_LTM, SUBR_NAME )

      DO I=1,6
         LTM_MERGE_VEC(I) = 1
      ENDDO
      DO I=7,6+NDOFR
         LTM_MERGE_VEC(I) = 2
      ENDDO

      CALL MERGE_MAT_ROWS_SSS ( 'CG_LTM', 6    , NTERM_CG_LTM, I_CG_LTM, J_CG_LTM, CG_LTM, 1,                                      &
                                'IF_LTM', NDOFR, NTERM_IF_LTM, I_IF_LTM, J_IF_LTM, IF_LTM, 2, LTM_MERGE_VEC,                       &
                                'LTM'                        , I_LTM   , J_LTM   , LTM )
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF
 
      RETURN

! **********************************************************************************************************************************
 
      END SUBROUTINE MERGE_LTM
