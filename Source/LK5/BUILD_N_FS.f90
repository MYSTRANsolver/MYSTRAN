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

      SUBROUTINE BUILD_N_FS
 
! For one subcase:
    
!   1) Merge UF and US to get UN where UF is calc'd in subr BUILD_F_AO

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  NDOFF, NDOFN, NDOFS, NDOFSE, NDOFSZ, BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BUILD_N_FS_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  PRTDISP
      USE COL_VECS, ONLY              :  UF_COL, UN_COL, US_COL, YSe
 
      USE BUILD_N_FS_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME   = 'BUILD_N_FS'
  
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: N_SET_COL         ! Col no. in TDOF for N  displ set definition
      INTEGER(LONG)                   :: F_SET_COL         ! Col no. in TDOF for F  displ set definition
      INTEGER(LONG)                   :: S_SET_COL         ! Col no. in TDOF for S  displ set definition
      INTEGER(LONG)                   :: SZ_SET_COL        ! Col no. in TDOF for SZ displ set definition
      INTEGER(LONG)                   :: SE_SET_COL        ! Col no. in TDOF for SE displ set definition
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BUILD_N_FS_BEGEND

      REAL(DOUBLE)                    :: USZ_COL(NDOFSZ)   ! Array of zero displs for the SZ set 
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Get column numbers for various DOF sets

      IF (NDOFS > 0) THEN

         CALL TDOF_COL_NUM('N ', N_SET_COL)
         CALL TDOF_COL_NUM('F ', F_SET_COL)
         CALL TDOF_COL_NUM('S ', S_SET_COL)
         CALL TDOF_COL_NUM('SZ',SZ_SET_COL)
         CALL TDOF_COL_NUM('SE',SE_SET_COL)

! Merge zeros for USZ ( the SZ-set) with YSe to get US ( the S-set)

         DO I=1,NDOFSZ
            USZ_COL(I) = ZERO
         ENDDO

         IF (NDOFSE > 0) THEN
            CALL MERGE_COL_VECS ( SZ_SET_COL, NDOFSZ, USZ_COL, SE_SET_COL, NDOFSE, YSe, S_SET_COL, NDOFS, US_COL )
         ELSE
            DO I=1,NDOFS
               US_COL(I) = ZERO
            ENDDO 
         ENDIF

! Merge UF and US to get UN 

         CALL MERGE_COL_VECS ( F_SET_COL, NDOFF, UF_COL, S_SET_COL, NDOFS, US_COL, N_SET_COL, NDOFN, UN_COL )

      ELSE
         DO I=1,NDOFN
            UN_COL(I) = UF_COL(I)
         ENDDO
 
      ENDIF


! Print out displ matrices if PRTDISP says to

      IF ((PRTDISP(3) == 1) .OR. (PRTDISP(3) == 3)) THEN
         IF (NDOFF  > 0) THEN
            CALL WRITE_VECTOR ( '   F-SET DISPL VECTOR   ', 'DISPL', NDOFF, UF_COL)
         ENDIF
      ENDIF

      IF ((PRTDISP(3) == 2) .OR. (PRTDISP(3) == 3)) THEN
         IF (NDOFS  > 0) THEN
            CALL WRITE_VECTOR ( '   S-SET DISPL VECTOR   ', 'DISPL', NDOFS, US_COL)
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
 
      END SUBROUTINE BUILD_N_FS
