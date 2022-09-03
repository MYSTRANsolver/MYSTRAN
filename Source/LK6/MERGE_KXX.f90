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

      SUBROUTINE MERGE_KXX  
 
! Merges matrices to get CB stifness matrix:

!                    | KRRcb    0   |        KRRcb = KRR + KLR'*DLR
!            KXX   = |              |
!                    |  0      Kee  |         Kee  = gen stiffnesses for the eigenvecs of the L-set = EIGEN_VAL(i)*GEN_MASS(i)
!                                                    (this is a diagonal matrix)

 
! For a description of Craig-Bamptom analyses, see Appendix D to the MYSTRAN User's Referance Manual


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NDOFR, NTERM_KRRcb, NTERM_KXX  , NVEC
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  PRTKXX
      USE EIGEN_MATRICES_1, ONLY      :  GEN_MASS, EIGEN_VAL
      USE SPARSE_MATRICES , ONLY      :  I_KRRcb, J_KRRcb, KRRcb, I_KXX  , J_KXX  , KXX  
      USE SUBR_BEGEND_LEVELS, ONLY    :  MERGE_KXX_BEGEND

      USE MERGE_KXX_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'MERGE_KXX  '

      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: K                 ! Counter
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MERGE_KXX_BEGEND

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      NTERM_KXX   = NTERM_KRRcb + NVEC
      CALL ALLOCATE_SPARSE_MAT ( 'KXX', NDOFR+NVEC, NTERM_KXX  , SUBR_NAME )
      DO I=1,NDOFR+1
         I_KXX  (I) = I_KRRcb(I)
      ENDDO
      DO I=NDOFR+2,NDOFR+NVEC+1                            ! This is for the diagonal terms in Kee
         I_KXX  (I) = I_KXX  (I-1) + 1
      ENDDO

      DO J=1,NTERM_KRRcb
         J_KXX  (J) = J_KRRcb(J)
           KXX  (J) = KRRcb(J)
      ENDDO
      K = 0
      DO J=NTERM_KRRcb+1,NTERM_KXX  
         K          = K + 1
         J_KXX  (J) = NDOFR + K
           KXX  (J) = EIGEN_VAL(K)*GEN_MASS(K)
      ENDDO


      IF (PRTKXX > 0) THEN
         CALL WRITE_SPARSE_CRS ( 'KXX  ','  ','  ', NTERM_KXX  , NDOFR+NVEC, I_KXX  , J_KXX  , KXX   )
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF
 
      RETURN

! **********************************************************************************************************************************


! **********************************************************************************************************************************

      END SUBROUTINE MERGE_KXX  