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
 
      SUBROUTINE TSET_PROC_FOR_RIGELS ( IERRT )
 
! DOF Processor for rigid elements (incl RBE3 and RSPLINE)
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06, L1F, L1F_MSG, LINK1F
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, LIND_GRDS_MPCS, NDOFM, NGRID, NIND_GRDS_MPCS, NRECARD
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  DOF_PROC_BEGEND
      USE DOF_TABLES, ONLY            :  TSET_CHR_LEN, TSET
      USE MODEL_STUF, ONLY            :  GRID, GRID_ID, MPC_IND_GRIDS
 
      USE TSET_PROC_FOR_RIGELS_USE_IFs                     ! Added 2019/07/14

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'TSET_PROC_FOR_RIGELS'
      CHARACTER( 1*BYTE)              :: CDOF(6)           ! An output from subr RDOF
      CHARACTER( 1*BYTE)              :: CDOF1(6)          ! An output from subr RDOF
      CHARACTER( 1*BYTE)              :: CDOF2(6)          ! An output from subr RDOF
      CHARACTER(LEN=LEN(TSET_CHR_LEN)):: DOFSET            ! The name of a DOF set (e.g. 'SB', 'A ', etc)
      CHARACTER( 8*BYTE)              :: RTYPE             ! Rigid elem type (RBAR, RBE1, RBE2)
 
      INTEGER(LONG), INTENT(INOUT)    :: IERRT             ! Sum of all grid and DOF errors
      INTEGER(LONG)                   :: COMPS_D           ! Dep DOF's on RBE1 or RBE2 elem read from file LINK1F
      INTEGER(LONG)                   :: DDOF1             ! Dep DOF's at GID1 on RBAR elem read from file LINK1F
      INTEGER(LONG)                   :: DDOF2             ! Dep DOF's at GID2 on RBAR elem read from file LINK1F
      INTEGER(LONG)                   :: AGRID_D           ! Dep grid for RBE1 or RBE2 elem read from file LINK1F/LINK1S
      INTEGER(LONG)                   :: DOF_ERR    = 0    ! Count of errors that result from setting displ sets in TSET
      INTEGER(LONG)                   :: GID1              ! A grid number
      INTEGER(LONG)                   :: GID2              ! A grid number
      INTEGER(LONG)                   :: GID_ERR    = 0    ! Count of errors that result from undefined grid ID's
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM   ! Row number, in array GRID_ID, where an actual grid ID resides
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM_D ! Row number, in array GRID_ID, where an actual grid ID resides
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM_I ! Row number, in array GRID_ID, where an actual grid ID resides
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: IDUM(6)           ! Integer values read that are not used
      INTEGER(LONG)                   :: AGRIDI_I(6)       ! Up to 6 indep grids for RBE1 elem read from file LINK1F
      INTEGER(LONG)                   :: AGRID_I           ! Indep grid on RBE elem read from file LINK1F
      INTEGER(LONG)                   :: AGRID_I1          ! Indep grid on RBE elem read from file LINK1F
      INTEGER(LONG)                   :: AGRID_I2          ! Indep grid on RBE elem read from file LINK1F
      INTEGER(LONG)                   :: AGRID_I_PREV      ! Previous value of AGRID_I read from file L1F. (each RBE2 element has a
!                                                            separate record in file L1F for each dep  grid, so, if there are 10
!                                                            dep grids on one RBE2 then there will be 10 records.The 10 IGID0's
!                                                            read from these 10 records are all identical and we only want to write
!                                                            the IGID0 once to array MPC_IND_GRIDS.
      INTEGER(LONG)                   :: AGRID_I1_PREV     ! Same general description as for AGRID_I_PREV
      INTEGER(LONG)                   :: AGRID_I2_PREV     ! Same general description as for AGRID_I_PREV
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening or reading a file
      INTEGER(LONG)                   :: IRBE3             ! Number of triplets of grid/comp/weight on L1F for RBE# elems
      INTEGER(LONG)                   :: NUMI              ! Noumber of pairs of grid/indep DOF's read from file LINK1F for RBE1's
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to.   
      INTEGER(LONG)                   :: REC_NO     = 0    ! Record number when reading a file
      INTEGER(LONG)                   :: REFC              ! Dependent components on RBE3
      INTEGER(LONG)                   :: REFGRID           ! Dependent grid on RBE3
      INTEGER(LONG)                   :: REID              ! Rigid elem ID read from file LINK1F
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = DOF_PROC_BEGEND + 1

      REAL(DOUBLE)                    :: RDUM              ! Real value read that is not used

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
 
! **********************************************************************************************************************************
! Process the Rigid Element data from file L1F (data written when RBAR, RBE1, RBE2 Bulk Data cards were read)

      AGRID_I_PREV  = 0
      AGRID_I1_PREV = 0
      AGRID_I2_PREV = 0

      DO I=1,NRECARD

         READ(L1F,IOSTAT=IOCHK) RTYPE      
         REC_NO = REC_NO + 1
         IF (IOCHK /= 0) THEN
            CALL READERR ( IOCHK, LINK1F, L1F_MSG, REC_NO, OUNT, 'Y' )
            CALL OUTA_HERE ( 'Y' )                         ! Error reading rigid elem file. No sense continuing
         ENDIF

! RBAR rigid element

         IF      (RTYPE == 'RBAR    ') THEN

            READ(L1F,IOSTAT=IOCHK) REID,GID1,IDUM(1),DDOF1,GID2,IDUM(2),DDOF2      
            REC_NO = REC_NO + 1
            IF (IOCHK /= 0) THEN
               CALL READERR ( IOCHK, LINK1F, L1F_MSG, REC_NO, OUNT, 'Y' )
               CALL OUTA_HERE ( 'Y' )                      ! Error reading rigid elem file. No sense continuing
            ENDIF
                                                           ! Check for existence of grid pt. GID1
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GID1, GRID_ID_ROW_NUM )
            IF (GRID_ID_ROW_NUM == -1) THEN
               GID_ERR = GID_ERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1822) 'GRID ', GID1, RTYPE, REID
               WRITE(F06,1822) 'GRID ', GID1, RTYPE, REID
            ENDIF
                                                           ! Check for existence of grid pt. GID2 
            IF (DDOF1 > 0) THEN                            ! Convert DDOF1 to CDOF1 char forn for use below
               CALL RDOF ( DDOF1, CDOF1 )
            ELSE
               DO K=1,6
                  CDOF1(K) = '0'
               ENDDO
            ENDIF
 
            DOFSET = 'M '
            IF (GRID_ID_ROW_NUM > 0) THEN                  ! Put CDOF1 data in TSET                    
               DO K = 1,6
                  IF (CDOF1(K) == '1') THEN
                     IF (TSET(GRID_ID_ROW_NUM,K) == '  ') THEN
                        TSET(GRID_ID_ROW_NUM,K) = DOFSET
                        NDOFM = NDOFM + 1
                     ELSE
                        DOF_ERR = DOF_ERR + 1
                        FATAL_ERR = FATAL_ERR + 1
                        IF (TSET(GRID_ID_ROW_NUM,K) == '--') THEN
                           WRITE(ERR,1313) RTYPE, REID, GID1, K, DOFSET
                           WRITE(F06,1313) RTYPE, REID, GID1, K, DOFSET
                        ELSE
                           WRITE(ERR,1330) RTYPE, REID, GID1, K, DOFSET, TSET(GRID_ID_ROW_NUM,K)
                           WRITE(F06,1330) RTYPE, REID, GID1, K, DOFSET, TSET(GRID_ID_ROW_NUM,K)
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF

            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GID2, GRID_ID_ROW_NUM )
            IF (GRID_ID_ROW_NUM == -1) THEN
               GID_ERR = GID_ERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1822) 'GRID ', GID2, RTYPE, REID
               WRITE(F06,1822) 'GRID ', GID2, RTYPE, REID
            ENDIF
 
            IF (DDOF2 > 0) THEN                            ! Convert DDOF2 to CDOF2 char forn for use below
               CALL RDOF ( DDOF2, CDOF2 )           
            ELSE
               DO K=1,6
                  CDOF2(K) = '0'
               ENDDO
            ENDIF
 
            IF (GRID_ID_ROW_NUM > 0) THEN                  ! Put CDOF2 data in TSET                    
               DO K = 1,6
                  IF (CDOF2(K) == '1') THEN
                     IF (TSET(GRID_ID_ROW_NUM,K) == '  ') THEN
                        TSET(GRID_ID_ROW_NUM,K) = DOFSET
                        NDOFM = NDOFM + 1
                     ELSE
                        DOF_ERR = DOF_ERR + 1
                        FATAL_ERR = FATAL_ERR + 1
                        IF (TSET(GRID_ID_ROW_NUM,K) == '--') THEN
                           WRITE(ERR,1313) RTYPE, REID, GID2, K, DOFSET
                           WRITE(F06,1313) RTYPE, REID, GID2, K, DOFSET
                        ELSE
                           WRITE(ERR,1330) RTYPE, REID, GID2, K, DOFSET, TSET(GRID_ID_ROW_NUM,K)
                           WRITE(F06,1330) RTYPE, REID, GID2, K, DOFSET, TSET(GRID_ID_ROW_NUM,K)
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF

! RBE1 rigid element

         ELSE IF (RTYPE == 'RBE1    ') THEN
                                                           ! NUMI was checked to be <= 6 when RBE1 was read in Bulk Data
            READ(L1F,IOSTAT=IOCHK) REID,AGRID_D,COMPS_D,NUMI,(AGRIDI_I(J),IDUM(J),J=1,NUMI)     
            REC_NO = REC_NO + 1
            IF (IOCHK /= 0) THEN
               CALL READERR ( IOCHK, LINK1F, L1F_MSG, REC_NO, OUNT, 'Y' )
               CALL OUTA_HERE ( 'Y' )                      ! Error reading rigid elem file. No sense continuing
            ENDIF

            DO J=1,NUMI                                    ! Check for existence of independent grids, AGRIDI_I(1-6)
               CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRIDI_I(J), GRID_ID_ROW_NUM )
               IF (GRID_ID_ROW_NUM == -1) THEN
                  GID_ERR = GID_ERR + 1
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1822) 'GRID ', AGRIDI_I(J), RTYPE, REID
                  WRITE(F06,1822) 'GRID ', AGRIDI_I(J), RTYPE, REID
               ENDIF
            ENDDO
                                                            ! Check for existence of dependent grid, AGRID_D
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_D, GRID_ID_ROW_NUM )
            IF (GRID_ID_ROW_NUM == -1) THEN
               GID_ERR = GID_ERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1822) 'GRID ', AGRID_D, RTYPE, REID
               WRITE(F06,1822) 'GRID ', AGRID_D, RTYPE, REID
            ENDIF
 
            CALL RDOF ( COMPS_D, CDOF )                    ! Convert COMPS_D to CDOF char forn for use below
 
            DOFSET = 'M '                                  ! Put data in TSET
            IF (GRID_ID_ROW_NUM > 0) THEN
               DO K = 1,6
                  IF (CDOF(K) == '1') THEN
                     IF (TSET(GRID_ID_ROW_NUM,K) == '  ') THEN
                        TSET(GRID_ID_ROW_NUM,K) = DOFSET
                        NDOFM = NDOFM + 1
                     ELSE
                        DOF_ERR = DOF_ERR + 1
                        FATAL_ERR = FATAL_ERR + 1
                        IF (TSET(GRID_ID_ROW_NUM,K) == '--') THEN
                           WRITE(ERR,1313) RTYPE, REID, AGRID_D, K, DOFSET
                           WRITE(F06,1313) RTYPE, REID, AGRID_D, K, DOFSET
                        ELSE
                           WRITE(ERR,1330) RTYPE, REID, AGRID_D, K, DOFSET, TSET(GRID_ID_ROW_NUM,K)
                           WRITE(F06,1330) RTYPE, REID, AGRID_D, K, DOFSET, TSET(GRID_ID_ROW_NUM,K)
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF

! RBE2 rigid element

         ELSE IF (RTYPE == 'RBE2    ') THEN
            READ(L1F,IOSTAT=IOCHK) REID, AGRID_D, COMPS_D, AGRID_I
            IF (AGRID_I /= AGRID_I_PREV) THEN
               NIND_GRDS_MPCS = NIND_GRDS_MPCS + 1         ! Up count on num indep grids & store grid in array MPC_IND_GRIDS
               IF (NIND_GRDS_MPCS > LIND_GRDS_MPCS) CALL ARRAY_SIZE_ERROR_1  ( SUBR_NAME, LIND_GRDS_MPCS, 'MPC_IND_GRIDS' ) 
               MPC_IND_GRIDS(NIND_GRDS_MPCS) = AGRID_I
               AGRID_I_PREV = AGRID_I
            ENDIF   
            REC_NO = REC_NO + 1
            IF (IOCHK /= 0) THEN
               CALL READERR ( IOCHK, LINK1F, L1F_MSG, REC_NO, OUNT, 'Y' )
               CALL OUTA_HERE ( 'Y' )                      ! Error reading rigid elem file. No sense continuing
            ENDIF
                                                           ! Check for existence of dependent grid, AGRID_D
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_D, GRID_ID_ROW_NUM_D )
            IF (GRID_ID_ROW_NUM_D == -1) THEN
               GID_ERR = GID_ERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1822) 'GRID ', AGRID_D, RTYPE, REID
               WRITE(F06,1822) 'GRID ', AGRID_D, RTYPE, REID
            ENDIF
                                                           ! Check for existence of independent grid, AGRID_I 
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_I, GRID_ID_ROW_NUM_I )
            IF (GRID_ID_ROW_NUM_I == -1) THEN
               GID_ERR = GID_ERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1822) 'GRID ', AGRID_I, RTYPE, REID
               WRITE(F06,1822) 'GRID ', AGRID_I, RTYPE, REID
            ENDIF
 
            CALL RDOF ( COMPS_D, CDOF )                    ! Convert COMPS_D to CDOF char forn for use below
 
            DOFSET = 'M '                                  ! Put data in TSET
            IF (GRID_ID_ROW_NUM_D > 0) THEN
               DO K = 1,6
                  IF (CDOF(K) == '1') THEN
                     IF (TSET(GRID_ID_ROW_NUM_D,K) == '  ') THEN
                        TSET(GRID_ID_ROW_NUM_D,K) = DOFSET
                        NDOFM = NDOFM + 1
                     ELSE
                        DOF_ERR = DOF_ERR + 1
                        FATAL_ERR = FATAL_ERR + 1
                        IF (TSET(GRID_ID_ROW_NUM_D,K) == '--') THEN
                           WRITE(ERR,1313) RTYPE, REID, AGRID_D, K, DOFSET
                           WRITE(F06,1313) RTYPE, REID, AGRID_D, K, DOFSET
                        ELSE
                           WRITE(ERR,1330) RTYPE, REID, AGRID_D, K, DOFSET, TSET(GRID_ID_ROW_NUM_D,K)
                           WRITE(F06,1330) RTYPE, REID, AGRID_D, K, DOFSET, TSET(GRID_ID_ROW_NUM_D,K)
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF

! RBE3 rigid element

         ELSE IF (RTYPE == 'RBE3    ') THEN
            READ(L1F,IOSTAT=IOCHK) REID, REFGRID, REFC, IRBE3, RDUM
            REC_NO = REC_NO + 1
            IF (IOCHK /= 0) THEN
               CALL READERR ( IOCHK, LINK1F, L1F_MSG, REC_NO, OUNT, 'Y' )
               CALL OUTA_HERE ( 'Y' )                      ! Error reading rigid elem file. No sense continuing
            ENDIF
                                                           ! Check for existence of dependent grid, REFGRID
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, REFGRID, GRID_ID_ROW_NUM_D )
            IF (GRID_ID_ROW_NUM_D == -1) THEN
               GID_ERR = GID_ERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1822) 'GRID ', REFGRID, RTYPE, REID
               WRITE(F06,1822) 'GRID ', REFGRID, RTYPE, REID
            ENDIF
                                                           ! Check for existence of independent grids
            DO J=1,IRBE3
               READ(L1F) AGRID_I, IDUM(1), RDUM
               REC_NO = REC_NO + 1
               IF (IOCHK /= 0) THEN
                  CALL READERR ( IOCHK, LINK1F, L1F_MSG, REC_NO, OUNT, 'Y' )
                  CALL OUTA_HERE ( 'Y' )                   ! Error reading rigid elem file. No sense continuing
               ENDIF
               CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_I, GRID_ID_ROW_NUM_I )
               IF (GRID_ID_ROW_NUM_I == -1) THEN
                  GID_ERR = GID_ERR + 1
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1822) 'GRID ', AGRID_I, RTYPE, REID
                  WRITE(F06,1822) 'GRID ', AGRID_I, RTYPE, REID
               ENDIF
            ENDDO
 
            CALL RDOF ( REFC, CDOF )                       ! Convert REFC to CDOF char forn for use below
 
            DOFSET = 'M '                                  ! Put data in TSET
            IF (GRID_ID_ROW_NUM_D > 0) THEN
               DO K = 1,6
                  IF (CDOF(K) == '1') THEN
                     IF (TSET(GRID_ID_ROW_NUM_D,K) == '  ') THEN
                        TSET(GRID_ID_ROW_NUM_D,K) = DOFSET
                        NDOFM = NDOFM + 1
                     ELSE
                        DOF_ERR = DOF_ERR + 1
                        FATAL_ERR = FATAL_ERR + 1
                        IF (TSET(GRID_ID_ROW_NUM_D,K) == '--') THEN
                           WRITE(ERR,1313) RTYPE, REID, REFGRID, K, DOFSET
                           WRITE(F06,1313) RTYPE, REID, REFGRID, K, DOFSET
                        ELSE
                           WRITE(ERR,1330) RTYPE, REID, REFGRID, K, DOFSET, TSET(GRID_ID_ROW_NUM_D,K)
                           WRITE(F06,1330) RTYPE, REID, REFGRID, K, DOFSET, TSET(GRID_ID_ROW_NUM_D,K)
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF

! RSPLINE rigid element

         ELSE IF (RTYPE == 'RSPLINE ') THEN

            READ(L1F,IOSTAT=IOCHK) REID, IDUM(1), IDUM(2), AGRID_I1, AGRID_I2, AGRID_D, COMPS_D, RDUM   

            IF (AGRID_I1 /= AGRID_I1_PREV) THEN
               NIND_GRDS_MPCS = NIND_GRDS_MPCS + 1         ! Up count on num indep grids & store grid in array MPC_IND_GRIDS
               IF (NIND_GRDS_MPCS > LIND_GRDS_MPCS) CALL ARRAY_SIZE_ERROR_1  ( SUBR_NAME, LIND_GRDS_MPCS, 'MPC_IND_GRIDS' ) 
               MPC_IND_GRIDS(NIND_GRDS_MPCS) = AGRID_I1
               AGRID_I1_PREV = AGRID_I1
            ENDIF   

            IF (AGRID_I2 /= AGRID_I2_PREV) THEN
               NIND_GRDS_MPCS = NIND_GRDS_MPCS + 1         ! Up count on num indep grids & store grid in array MPC_IND_GRIDS
               IF (NIND_GRDS_MPCS > LIND_GRDS_MPCS) CALL ARRAY_SIZE_ERROR_1  ( SUBR_NAME, LIND_GRDS_MPCS, 'MPC_IND_GRIDS' ) 
               MPC_IND_GRIDS(NIND_GRDS_MPCS) = AGRID_I2
               AGRID_I2_PREV = AGRID_I2
            ENDIF   

            REC_NO = REC_NO + 1
            IF (IOCHK /= 0) THEN
               CALL READERR ( IOCHK, LINK1F, L1F_MSG, REC_NO, OUNT, 'Y' )
               CALL OUTA_HERE ( 'Y' )                      ! Error reading rigid elem file. No sense continuing
            ENDIF
                                                           ! Check for existence of dependent grid, AGRID_D
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_D, GRID_ID_ROW_NUM_D )
            IF (GRID_ID_ROW_NUM_D == -1) THEN
               GID_ERR = GID_ERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1822) 'GRID ', AGRID_D, RTYPE, REID
               WRITE(F06,1822) 'GRID ', AGRID_D, RTYPE, REID
            ENDIF
                                                           ! Check for existence of independent grid, AGRID_I1 
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_I1, GRID_ID_ROW_NUM_I )
            IF (GRID_ID_ROW_NUM_I == -1) THEN
               GID_ERR = GID_ERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1822) 'GRID ', AGRID_I1, RTYPE, REID
               WRITE(F06,1822) 'GRID ', AGRID_I1, RTYPE, REID
            ENDIF
                                                           ! Check for existence of independent grid, AGRID_I2 
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_I2, GRID_ID_ROW_NUM_I )
            IF (GRID_ID_ROW_NUM_I == -1) THEN
               GID_ERR = GID_ERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1822) 'GRID ', AGRID_I2, RTYPE, REID
               WRITE(F06,1822) 'GRID ', AGRID_I2, RTYPE, REID
            ENDIF
 
            CALL RDOF ( COMPS_D, CDOF )                    ! Convert COMPS_D to CDOF char forn for use below
 
            DOFSET = 'M '                                  ! Put data in TSET
            IF (GRID_ID_ROW_NUM_D > 0) THEN
               DO K = 1,6
                  IF (CDOF(K) == '1') THEN
                     IF (TSET(GRID_ID_ROW_NUM_D,K) == '  ') THEN
                        TSET(GRID_ID_ROW_NUM_D,K) = DOFSET
                        NDOFM = NDOFM + 1
                     ELSE
                        DOF_ERR = DOF_ERR + 1
                        FATAL_ERR = FATAL_ERR + 1
                        IF (TSET(GRID_ID_ROW_NUM_D,K) == '--') THEN
                           WRITE(ERR,1313) RTYPE, REID, AGRID_D, K, DOFSET
                           WRITE(F06,1313) RTYPE, REID, AGRID_D, K, DOFSET
                        ELSE
                           WRITE(ERR,1330) RTYPE, REID, AGRID_D, K, DOFSET, TSET(GRID_ID_ROW_NUM_D,K)
                           WRITE(F06,1330) RTYPE, REID, AGRID_D, K, DOFSET, TSET(GRID_ID_ROW_NUM_D,K)
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF

         ELSE

            WRITE(ERR,1329) SUBR_NAME,RTYPE
            WRITE(F06,1329) SUBR_NAME,RTYPE
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )

         ENDIF

      ENDDO
 
      IERRT = IERRT + GID_ERR + DOF_ERR

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1822 FORMAT(' *ERROR  1822: ',A,I8,' ON ',A,I8,' IS UNDEFINED')

 1329 FORMAT(' *ERROR  1329: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' RIGID ELEMENT TYPE MUST BE "RBAR", "RBE1", OR "RBE2" BUT IS "',A8,'"')

 1313 FORMAT(' *ERROR  1330: ',A8,' RIGID ELEMENT NUMBER ',I8,' HAS SPOINT POINT ',I8,' COMPONENT ',I2,' IN THE ',A2,' DISPL SET.',&
                           ' HOWEVER THIS COMPONENT IS UNDEFINED FOR SPOINT''s')

 1330 FORMAT(' *ERROR  1330: ',A8,' RIGID ELEMENT NUMBER ',I8,' HAS GRID POINT ',I8,' COMPONENT ',I2,' IN THE ',A2,' DISPL SET.',  &
                           ' HOWEVER THIS GRID/COMPONENT IS ALREADY IN THE ',A2,' DISPL SET')





! **********************************************************************************************************************************

      END SUBROUTINE TSET_PROC_FOR_RIGELS
