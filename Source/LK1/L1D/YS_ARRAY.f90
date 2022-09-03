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
 
      SUBROUTINE YS_ARRAY
 
! Process enforced displacement data in file LINK1H and write the enforced displacement array, YSe, for use in subsequent LINK's.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR,     F04,     F06,    L1H
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG,                           LINK1H
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG,                           L1H_MSG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NDOFSE, NGRID
      USE TIMDAT, ONLY                :  STIME, TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  YS_ARRAY_BEGEND
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE MODEL_STUF, ONLY            :  GRID_ID
      USE COL_VECS, ONLY              :  YSe
 
      USE YS_ARRAY_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'YS_ARRAY'
 
      INTEGER(LONG)                   :: COMP              ! Displ component number read from file LINK1H
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM   ! Row num in array GRID_ID of the actual grid that the YS displ is at
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IERR1H            ! Count of errors as file LINK1H is read
      INTEGER(LONG)                   :: IGRID             ! Internal grid ID
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening/reading a file
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to. Input to subr UNFORMATTED_OPEN  
      INTEGER(LONG)                   :: REC_NO            ! Record number when reading a file
      INTEGER(LONG)                   :: ROW_NUM_START     ! Row no. in array TDOF where data begins for GRID_ID(GRID_ID_ROW_NUM)
      INTEGER(LONG)                   :: SE_SET_COL_NUM    ! Col no., in TDOF array, of the SE-set DOF list
      INTEGER(LONG)                   :: TDOF_ROW_NUM      ! Row num in array TDOF for DOF corresponding to GRID_ID_ROW_NUM, COMP
      INTEGER(LONG)                   :: YSDOF             ! SE-set DOF number for the DOF corresponding to GRID_ID_ROW_NUM, COMP
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = YS_ARRAY_BEGEND
 
      REAL(DOUBLE)                    :: YSV               ! Enforced displ value read from file LINK1H
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Make units for writing errors the error file and output file
 
      OUNT(1) = ERR
      OUNT(2) = F06
 
! Get YSV values from file L1H array This subr is only called if NDOFSE > 0 so we can assume there is a SE-set
 
      IERR1H = 0
      DO I = 1,NDOFSE
         READ(L1H,IOSTAT=IOCHK) GRID_ID_ROW_NUM,COMP,YSV
         IF (IOCHK /= 0) THEN
            REC_NO = I
            CALL READERR ( IOCHK, LINK1H, L1H_MSG, REC_NO, OUNT, 'Y' )
            IERR1H = IERR1H + 1
         ELSE
            CALL TDOF_COL_NUM ( 'SE', SE_SET_COL_NUM )
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GRID_ID(GRID_ID_ROW_NUM), IGRID )
            ROW_NUM_START = TDOF_ROW_START(IGRID)
            TDOF_ROW_NUM = ROW_NUM_START + COMP - 1
            YSDOF = TDOF(TDOF_ROW_NUM,SE_SET_COL_NUM)
            YSe(YSDOF) = YSV
         ENDIF
      ENDDO   
 
! If there were any errors based on reading above file, quit.
 
      IF (IERR1H > 0) THEN
         CALL FILERR ( OUNT, 'Y' )
         CALL OUTA_HERE ( 'Y' )                                    ! Errors reading YSe file, so quit
      ENDIF
 
! Rewind L1H and write out ordered YSe array
 
      REWIND (L1H)
      WRITE(L1H) STIME
      DO I = 1,NDOFSE
         WRITE(L1H) YSe(I)
      ENDDO   
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 
      END SUBROUTINE YS_ARRAY
