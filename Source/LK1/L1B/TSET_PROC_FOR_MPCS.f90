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
 
      SUBROUTINE TSET_PROC_FOR_MPCS ( IERRT )
 
! DOF Processor for MPC's 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1S, L1S_MSG, LINK1S
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, LIND_GRDS_MPCS, LMPCADDC, NDOFM, NGRID, NIND_GRDS_MPCS, NMPC,    &
                                         NMPCADD, NTERM_RMG, NUM_MPCSIDS 
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  DOF_PROC_BEGEND
      USE DOF_TABLES, ONLY            :  TSET_CHR_LEN, TSET
      USE MODEL_STUF, ONLY            :  GRID_ID, MPC_IND_GRIDS, MPCSET, MPCSIDS
 
      USE TSET_PROC_FOR_MPCS_USE_IFs                       ! Added 2019/07/14

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'TSET_PROC_FOR_MPCS'
      CHARACTER(LEN=LEN(TSET_CHR_LEN)):: DOFSET            ! The name of a DOF set (e.g. 'SB', 'A ', etc)
      CHARACTER( 1*BYTE)              :: MPC_SET_USED      ! 'Y'/'N' indicator if an MPC set in B.D. is used
 
      INTEGER(LONG), INTENT(INOUT)    :: IERRT             ! Sum of all grid and DOF errors
      INTEGER(LONG)                   :: AGRID_D           ! Dep grid for an MPC read from file LINK1S
      INTEGER(LONG)                   :: AGRID_I           ! An independent grid in an MPC equation

      INTEGER(LONG)                   :: AGRID_I_PREV = 0  ! Previous value of AGRID_I read from file L1F. (each RBE2 element has a
!                                                            separate record in file L1F for each dep  grid, so, if there are 10
!                                                            dep grids on one RBE2 then there will be 10 records.The 10 AGRID_I's
!                                                            read from these 10 records are all identical and we only want to write
!                                                            the AGRID_I once to array MPC_IND_GRIDS.

      INTEGER(LONG)                   :: COMPS_D           ! Comp number for the dep grid for an MPC eqn (read from file LINK1S)
      INTEGER(LONG)                   :: COMPS_D_TSET      ! COMPS_D converted (e.g. if COMPS_D read from L1S is 0, change to 1)
      INTEGER(LONG)                   :: DOF_ERR   = 0     ! Count of errors that result from setting displ sets in TSET
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM   ! Row number, in array GRID_ID, where an actual grid ID resides
      INTEGER(LONG)                   :: GID_ERR   = 0     ! Count of errors that result from undefined grid ID's
      INTEGER(LONG)                   :: IJUNK             ! A grid read from file LINK1S that we do not need
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening or reading a file
      INTEGER(LONG)                   :: NUM_TRIPLES       ! Counter on number of pairs of grid/comp/coeff triplets on this MPC
!                                                            card. Must be <= MMPC which was counted in subr BD_MPC0
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to.   
      INTEGER(LONG)                   :: REC_NO    = 0     ! Record number when reading a file
      INTEGER(LONG)                   :: SETID             ! An SPC set ID read from file LINK1O
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = DOF_PROC_BEGEND + 1
 
      REAL(DOUBLE)                    :: RJUNK             ! An MPC coeff value read from file LINK1S that we do not need
 
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
! Process MPC data from file L1S (data written when MPC Bulk Data cards were read)
 
! File LINK1S contains data from the NMPC number of logical MPC cards in the input B.D. deck. For each logical MPC card, LINK1S has:
!     1st record          for 1st MPC: the MPC set ID
!     2nd record          for 1st MPC: the num of triplets of grid/comp/coeff (incl ones for the dependent DOF) on this logical MPC
!     3rd record          for 1st MPC: grid/comp/coeff for the dependent DOF on the this MPC logical card
!     4th record          for 1st MPC: grid/comp/coeff for the 1st independent DOF on the this MPC logical card
!     5th record, and on, for 1st MPC: grid/comp/coeff for the 2nd, and on, independent DOF's (if any) on the this MPC logical card

! The above record structure is repeated for each MPC logical card in the data deck (in the order in which they were read from the 
! B.D. deck). All logical MPC cards are included, not only the ones that may be used in a particular execution of MYSTRAN

      GID_ERR = 0
      DOF_ERR = 0

      REC_NO = 0
i_do3:DO I=1,NMPC                                          ! Process data from file LINK1S (contains all info from the NMPC MPC's)
 
         READ(L1S,IOSTAT=IOCHK) SETID                      ! Read the SETID for the i-th logical MPC    
         REC_NO = REC_NO + 1
         IF (IOCHK /= 0) THEN
            CALL READERR ( IOCHK, LINK1S, L1S_MSG, REC_NO, OUNT, 'Y' )
            CALL OUTA_HERE ( 'Y' )
         ENDIF

         READ(L1S,IOSTAT=IOCHK) NUM_TRIPLES                ! Read the number of triplets of grid/comp/coeff for the i-th logical MPC
         REC_NO = REC_NO + 1
         IF (IOCHK /= 0) THEN
            CALL READERR ( IOCHK, LINK1S, L1S_MSG, REC_NO, OUNT, 'Y' )
            CALL OUTA_HERE ( 'Y' )
         ENDIF


         MPC_SET_USED = 'N'
                           
j_do3:   DO J=1,NUM_MPCSIDS                                ! NUM_MPCSIDS will be 1 if there are any MPC's. It will be > 1 if there
!                                                            are MPCADD entries that match the MPC set requested in Case Control
            IF (SETID == MPCSIDS(J)) THEN

               MPC_SET_USED = 'Y'
               NTERM_RMG = NTERM_RMG + NUM_TRIPLES
                                                           ! Read dependent grid/comp/coeff from for the i-th logical MPC
               READ(L1S,IOSTAT=IOCHK) AGRID_D, COMPS_D, RJUNK

               IF (COMPS_D == 0) THEN                      ! If SPOINT change COMPS_D from 0 to 1
                  COMPS_D_TSET = 1
               ELSE
                  COMPS_D_TSET = COMPS_D
               ENDIF  

               REC_NO = REC_NO + 1
               IF (IOCHK /= 0) THEN
                  CALL READERR ( IOCHK, LINK1S, L1S_MSG, REC_NO, OUNT, 'Y' )
                  CALL OUTA_HERE ( 'Y' )
               ENDIF
                                                           ! Get the row number, in array GRID_ID, for dependent grid, AGRID_D
               CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, AGRID_D, GRID_ID_ROW_NUM )
               IF (GRID_ID_ROW_NUM == -1) THEN
                  GID_ERR = GID_ERR + 1
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1822) 'GRID ', AGRID_D, 'MPC ', MPCSIDS(J)
                  WRITE(F06,1822) 'GRID ', AGRID_D, 'MPC ', MPCSIDS(J)
               ENDIF

               DO K=1,NUM_TRIPLES-1                        ! Read the indep grid/comp/coeff values (only need indep grid ID's)
                  READ(L1S,IOSTAT=IOCHK) AGRID_I, IJUNK, RJUNK
                  REC_NO = REC_NO + 1
                  IF (IOCHK /= 0) THEN
                     CALL READERR ( IOCHK, LINK1S, L1S_MSG, REC_NO, OUNT, 'Y' )
                     CALL OUTA_HERE ( 'Y' )
                  ENDIF
                  IF (AGRID_I /= AGRID_I_PREV) THEN
                     NIND_GRDS_MPCS = NIND_GRDS_MPCS + 1      ! Up count on num indep grids & store grid in array MPC_IND_GRIDS
                     IF (NIND_GRDS_MPCS > LIND_GRDS_MPCS) CALL ARRAY_SIZE_ERROR_1 ( SUBR_NAME, LIND_GRDS_MPCS, 'MPC_IND_GRIDS')
                     MPC_IND_GRIDS(NIND_GRDS_MPCS) = AGRID_I
                     AGRID_I_PREV = AGRID_I
                  ENDIF   
               ENDDO 

               IF (GID_ERR > 0) THEN
                  CYCLE j_do3
               ENDIF

               DOFSET = 'M '
               IF(TSET(GRID_ID_ROW_NUM,COMPS_D_TSET) == '  ') THEN
                  TSET(GRID_ID_ROW_NUM,COMPS_D_TSET) = DOFSET
                  NDOFM = NDOFM + 1
               ELSE
                  DOF_ERR = DOF_ERR + 1
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1333) SETID, AGRID_D, COMPS_D_TSET, DOFSET, TSET(GRID_ID_ROW_NUM, COMPS_D_TSET)
                  WRITE(F06,1333) SETID, AGRID_D, COMPS_D_TSET, DOFSET, TSET(GRID_ID_ROW_NUM, COMPS_D_TSET)
               ENDIF

               EXIT j_do3                                  ! We found and processed an MPC set, so exit this loop

            ELSE

               MPC_SET_USED = 'N'
               CYCLE j_do3

            ENDIF

         ENDDO j_do3 
 
         IF (MPC_SET_USED == 'N') THEN                     ! This MPC set is not to be used, so skip all grid/comp/coeff records
            DO K=1,NUM_TRIPLES
               READ(L1S,IOSTAT=IOCHK) IJUNK, IJUNK, RJUNK
               REC_NO = REC_NO + 1
               IF (IOCHK /= 0) THEN
                  CALL READERR ( IOCHK, LINK1S, L1S_MSG, REC_NO, OUNT, 'Y' )
                  CALL OUTA_HERE ( 'Y' )
               ENDIF
            ENDDO 
         ENDIF

      ENDDO i_do3 
 
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

 1333 FORMAT(' *ERROR  1333: MPC SET ',I8,' HAS GRID POINT ',I8,' COMPONENT ',I2,' IN THE ',A2,' DISPL SET.',                      &
                           ' HOWEVER THIS GRID/COMPONENT IS ALREADY IN THE ',A2,' DISPL SET')

! **********************************************************************************************************************************

      END SUBROUTINE TSET_PROC_FOR_MPCS
