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
 
      SUBROUTINE TSET_PROC_FOR_OMITS ( IERRT )
 
! DOF Processor for MPC's 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1N, L1N_MSG, LINK1N
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NAOCARD, NDOFO, NGRID
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  DOF_PROC_BEGEND
      USE DOF_TABLES, ONLY            :  TSET_CHR_LEN, TSET
      USE MODEL_STUF, ONLY            :  GRID, GRID_ID
 
      USE TSET_PROC_FOR_OMITS_USE_IFs                      ! Added 2019/07/14

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'TSET_PROC_FOR_OMITS'
      CHARACTER( 1*BYTE)              :: ASET_FND          ! 'Y' if there are ASET/ASET1 data in file LINK1N
      CHARACTER( 1*BYTE)              :: CDOF1(6)          ! An output from subr RDOF
      CHARACTER(LEN=LEN(TSET_CHR_LEN)):: DOFSET            ! The name of a DOF set (e.g. 'SB', 'A ', etc)
      CHARACTER( 1*BYTE)              :: OMIT_FND          ! 'Y' if there are OMIT/OMIT1 data in file LINK1N
 
      INTEGER(LONG), INTENT(INOUT)    :: IERRT             ! Sum of all grid and DOF errors
      INTEGER(LONG)                   :: DOF_ERR   = 0     ! Count of errors that result from setting displ sets in TSET
      INTEGER(LONG)                   :: GID1              ! 1st grid on RBAR element read from file LINK1F
      INTEGER(LONG)                   :: GID2              ! 2nd grid on RBAR element read from file LINK1F
      INTEGER(LONG)                   :: GID_ERR   = 0     ! Count of errors that result from undefined grid ID's
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM   ! Row number, in array GRID_ID, where an actual grid ID resides
      INTEGER(LONG)                   :: I,J,K             ! DO loop indices
      INTEGER(LONG)                   :: ICOMP             ! DOF components read from file LINK1O (SPC's) or LINK1N (ASET/OMIT's)
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening or reading a file
      INTEGER(LONG)                   :: NUM_COMPS         ! Number of displ components (1 for SPOINT, 6 for physical grid)
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to.   
      INTEGER(LONG)                   :: REC_NO    = 0     ! Record number when reading a file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = DOF_PROC_BEGEND + 1
 
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
! Process the ASET / OMIT data in file L1N
! If there is only ASET/ASET1 data in file L1N, then all remaining DOF will be O-set 
! If there is only OMIT/OMIT1 data in file L1N, then all remaining DOF will be A-set
! If there is both ASET/ASET1 and OMIT/OMIT1 then they must define all remaining DOF's, or error 
 
      GID_ERR = 0
      DOF_ERR = 0
      REC_NO  = 0

      IF (NAOCARD > 0) THEN

         ASET_FND = 'N'
         OMIT_FND = 'N'

         DO I=1,NAOCARD    
 
            READ(L1N,IOSTAT=IOCHK) ICOMP,GID1,GID2,DOFSET  ! Read a record from L1N
            REC_NO = REC_NO + 1
            IF (IOCHK /= 0) THEN
               CALL READERR ( IOCHK, LINK1N, L1N_MSG, REC_NO, OUNT, 'Y' )
               CALL OUTA_HERE ( 'Y' )                      ! Error reading ASET/OMIT file. No sense continuing
            ENDIF

            IF      (DOFSET == 'A ') THEN                  ! Make sure DOFSET is "A " or "O "
               ASET_FND = 'Y' 
            ELSE IF (DOFSET == 'O ') THEN
               OMIT_FND = 'Y'
            ELSE
               WRITE(ERR,1363) SUBR_NAME,LINK1N,DOFSET
               WRITE(F06,1363) SUBR_NAME,LINK1N,DOFSET
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )                      ! Pgm error (DOFSET is not A-set or O-set), so quit
            ENDIF

            CALL RDOF ( ICOMP, CDOF1 )                     ! Convert ICOMP to CDOF1 char forn for use below
                                                           ! Make sure grid GID1 & GID2 exist
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GID1, GRID_ID_ROW_NUM )
            IF (GRID_ID_ROW_NUM == -1) THEN
               GID_ERR = GID_ERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1365) GID1 
               WRITE(F06,1365) GID1
            ENDIF
            IF (GID2 /= GID1) THEN
               CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GID2, GRID_ID_ROW_NUM )
               IF (GRID_ID_ROW_NUM == -1) THEN
                  GID_ERR = GID_ERR + 1
                  FATAL_ERR = FATAL_ERR + 1
                  WRITE(ERR,1365) GID2 
                  WRITE(F06,1365) GID2
               ENDIF
            ENDIF 
 
            IF ((GID_ERR == 0) .AND. (GID2 >= GID1)) THEN  ! Put CDOF1 data in TSET for GID1 thru GID2
               DO J=GID1,GID2                              ! GID2 > GID1 was checked when ASET/OMIT B.D. cards were read
                  CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, J, GRID_ID_ROW_NUM )
                  IF (GRID_ID_ROW_NUM /= -1) THEN
                     CALL GET_GRID_NUM_COMPS ( GRID(GRID_ID_ROW_NUM,1), NUM_COMPS, SUBR_NAME )
                     DO K = 1,NUM_COMPS                       
                        IF (CDOF1(K) == '1') THEN
                           IF ((TSET(GRID_ID_ROW_NUM,K) == '  ') .OR. (TSET(GRID_ID_ROW_NUM,K) == DOFSET)) THEN
                              TSET(GRID_ID_ROW_NUM,K) = DOFSET
                              IF (DOFSET == 'O ') THEN
                                 NDOFO = NDOFO + 1
                              ENDIF
                           ELSE
                              DOF_ERR = DOF_ERR + 1
                              FATAL_ERR = FATAL_ERR + 1
                              WRITE(ERR,1366) J,K,DOFSET,TSET(GRID_ID_ROW_NUM,K)
                              WRITE(F06,1366) J,K,DOFSET,TSET(GRID_ID_ROW_NUM,K)
                           ENDIF
                        ENDIF
                     ENDDO
                  ENDIF
               ENDDO 
            ENDIF

         ENDDO                                             ! DO loop on I = 1 to NAOCARD

         IF ((ASET_FND == 'Y').AND.(OMIT_FND == 'N')) THEN ! Make all DOF's not as yet set be O-set
            DO I=1,NGRID
               CALL GET_GRID_NUM_COMPS ( GRID(I,1), NUM_COMPS, SUBR_NAME )
               DO K=1,NUM_COMPS
                  IF (TSET(I,K) == '  ') THEN
                     TSET(I,K) = 'O '
                     NDOFO = NDOFO + 1
                  ENDIF
               ENDDO
            ENDDO 
         ENDIF

         IF ((ASET_FND == 'N').AND.(OMIT_FND == 'Y')) THEN ! Make all DOF's not as yet set be A-set
            DO I=1,NGRID
               CALL GET_GRID_NUM_COMPS ( GRID(I,1), NUM_COMPS, SUBR_NAME )
               DO K=1,NUM_COMPS
                  IF (TSET(I,K) == '  ') THEN
                     TSET(I,K) = 'A '
                  ENDIF
               ENDDO
            ENDDO 
         ENDIF

      ENDIF                                                ! IF (NAOCARD > 0)

      IERRT = IERRT + GID_ERR + DOF_ERR

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1363 FORMAT(' *ERROR  1363: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' RECORD READ FROM FILE: '                                                                              &
                    ,/,14X,' HAS INCORRECT SET DEFINITION = ',A2,'. SHOULD BE EITHER "A " OR "O "')

 1365 FORMAT(' *ERROR  1365: GRID ',I8,' SPECIFIED ON ASET/ASET1 OR OMIT/OMIT1 ENTRY DOES NOT EXIST')

 1366 FORMAT(' *ERROR  1366: GRID POINT ',I8,' HAS COMPONENT ',I2,' IN THE ',A2,' DISPL SET',                                      &
                           ' HOWEVER THIS GRID/COMPONENT IS ALREADY IN THE ',A2,' DISPL SET')


! **********************************************************************************************************************************

      END SUBROUTINE TSET_PROC_FOR_OMITS
