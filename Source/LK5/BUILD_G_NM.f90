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

      SUBROUTINE BUILD_G_NM
 
! For one subcase:
    
!   1) Calcs UM displacements: UM = GMN*UN where:
 
!          UN  = Displs calc'd in subr BUILD_N_FS
 
!   2) Merge UM and UN to get UG

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  NDOFG, NDOFM, NDOFN, NTERM_GMN, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BUILD_G_NM_BEGEND
      USE CONSTANTS_1, ONLY           :  ONE
      USE PARAMS, ONLY                :  PRTDISP
      USE SPARSE_MATRICES, ONLY       :  I_GMN, J_GMN, GMN, SYM_GMN
      USE COL_VECS, ONLY              :  UG_COL, UM_COL, UN_COL 
 
      USE BUILD_G_NM_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME   = 'BUILD_G_NM'
 
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: G_SET_COL         ! Col no. in TDOF for G  displ set definition
      INTEGER(LONG)                   :: N_SET_COL         ! Col no. in TDOF for N  displ set definition
      INTEGER(LONG)                   :: M_SET_COL         ! Col no. in TDOF for M  displ set definition
      INTEGER(LONG), PARAMETER        :: NUMCOLS     = 1   ! Variable for number of cols of an array
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BUILD_G_NM_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Recover UM from GMN x UN

      IF (NDOFM > 0) THEN

         CALL MATMULT_SFF ( 'GMN', NDOFM, NDOFN, NTERM_GMN, SYM_GMN, I_GMN, J_GMN, GMN, 'UN', NDOFN, NUMCOLS, UN_COL, 'Y',         &
                            'UM', ONE, UM_COL )
! Merge UN and UM to get UG

         CALL TDOF_COL_NUM ( 'G ', G_SET_COL )
         CALL TDOF_COL_NUM ( 'N ', N_SET_COL )
         CALL TDOF_COL_NUM ( 'M ', M_SET_COL )

         CALL MERGE_COL_VECS ( N_SET_COL, NDOFN, UN_COL, M_SET_COL, NDOFM, UM_COL,G_SET_COL, NDOFG, UG_COL )

      ELSE

         DO I=1,NDOFG
            UG_COL(I) = UN_COL(I)
         ENDDO 

      ENDIF


! Print out displ matrices if PRTDISP says to

      IF (PRTDISP(1) == 1) THEN
         IF (NDOFG  > 0) THEN
            CALL WRITE_VECTOR ( '   G-SET DISPL VECTOR   ', 'DISPL', NDOFG, UG_COL)
         ENDIF
      ENDIF

      IF ((PRTDISP(2) == 1) .OR. (PRTDISP(2) == 3)) THEN
         IF (NDOFN  > 0) THEN
            CALL WRITE_VECTOR ( '   N-SET DISPL VECTOR   ', 'DISPL', NDOFN, UN_COL)
         ENDIF
      ENDIF

      IF ((PRTDISP(2) == 2) .OR. (PRTDISP(2) == 3)) THEN
         IF (NDOFM  > 0) THEN
            CALL WRITE_VECTOR ( '   M-SET DISPL VECTOR   ', 'DISPL', NDOFM, UM_COL)
         ENDIF
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 
      END SUBROUTINE BUILD_G_NM
