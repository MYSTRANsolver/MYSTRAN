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

      SUBROUTINE USET_PROC
 
! USET is a table that specifies which grid/component pairs are ones defined by the user on Bulk Data USET or USET1 entries.
! The table has NGRID rows and 6 columns (1 col for each of the 6 components of displ at a grid). The table can have entries that
! are either 'U1' or 'U2' (i.e. user set U1 or U2). The table is constructed like tha TSET table (see explanation in subr TSET_PROC)
! An example of a USET table written to the F06 file is shown below:


!                        DEGREES OF FREEDOM DEFINED ON USET BULK DATA ENTRIES
!                        ----------------------------------------------------

!                     Grid       T1       T2       T3       R1       R2       R3

!                     1012       U1       U2       U1       --       U1       --
!                     1022       --       U2       --       U2       --       U2
!                     1031       U2       U1       U1       U2       --       --
!                     1032       U1       --       U1       --       U1       --
!                     1033       --       U1       --       U1       --       --
!                     1041       U1       --       U1       U2       U1       U2

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06, L1X, L1X_MSG, LINK1X
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ENFORCED, FATAL_ERR, NGRID, NUM_USET_RECORDS, NUM_USET_U1, NUM_USET_U2
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  USET_PROC_BEGEND
      USE PARAMS, ONLY                :  EPSIL
      USE DOF_TABLES, ONLY            :  TSET_CHR_LEN, USET
      USE MODEL_STUF, ONLY            :  GRID, GRID_ID
 
      USE USET_PROC_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'USET_PROC'
      CHARACTER( 1*BYTE)              :: CDOF(6)           ! An output from subr RDOF
      CHARACTER(LEN=LEN(TSET_CHR_LEN)):: SNAME             ! The name of a set ('U1' or 'U2')
 
      INTEGER(LONG)                   :: USET_ERR   = 0    ! Count of errors that result from setting displ sets in USET
      INTEGER(LONG)                   :: GRID1             ! An actual grid ID
      INTEGER(LONG)                   :: GRID2             ! An actual grid ID
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM   ! Row number, in array GRID_ID, where an actual grid ID resides
      INTEGER(LONG)                   :: GID_ERR   = 0     ! Count of errors that result from undefined grid ID's
      INTEGER(LONG)                   :: I,J,GRID_NUM      ! DO loop indices
      INTEGER(LONG)                   :: ICOMP             ! DOF components read from file LINK1X (SPC's) or LINK1N (ASET/OMIT's)
      INTEGER(LONG)                   :: IERRT             ! Total number of errors found here
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening or reading a file
      INTEGER(LONG)                   :: NUM_COMPS         ! Number of displ components (1 for SPOINT, 6 for physical grid)
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to.   
      INTEGER(LONG)                   :: REC_NO    = 0     ! Record number when reading a file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = USET_PROC_BEGEND
 
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
 
! ----------------------------------------------------------------------------------------------------------------------------------
! Initialize

      GID_ERR     = 0
      IERRT       = 0
      USET_ERR    = 0
      NUM_USET_U1 = 0
      NUM_USET_U2 = 0
      REC_NO      = 0

! Allocate memory to USET and initialize

      CALL ALLOCATE_DOF_TABLES ( 'USET', SUBR_NAME )
      DO I=1,NGRID
         DO J=1,6
            USET(I,J) = '--'
         ENDDO
      ENDDO

! Process USET data from file L1X (data written when USET and USET1 Bulk Data entries were read)
 
      CALL FILE_OPEN ( L1X, LINK1X, OUNT, 'OLD', L1X_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )

i_do6:DO I=1,NUM_USET_RECORDS
 
         READ(L1X,IOSTAT=IOCHK) SNAME, ICOMP, GRID1, GRID2
         REC_NO = REC_NO + 1
         IF (IOCHK /= 0) THEN
            CALL READERR ( IOCHK, LINK1X, L1X_MSG, REC_NO, OUNT, 'Y' )
            CALL OUTA_HERE ( 'Y' )                         ! Error reading SPC file . No sense continuing
         ENDIF

         IF ((SNAME /= 'U1') .AND. (SNAME /= 'U2')) THEN   ! Make sure that SNAME = 'U1' or 'U2'
            WRITE(ERR,1369) SUBR_NAME, LINK1X, SNAME
            WRITE(F06,1369) SUBR_NAME, LINK1X, SNAME
            FATAL_ERR = FATAL_ERR + 1
            CALL OUTA_HERE ( 'Y' )                         ! Pgm error (data in file LINK1X must be for sets 'U1' or 'U2')
         ENDIF
 
! No error, so processes data.
 
         CALL RDOF ( ICOMP, CDOF )                   ! Convert ICOMP to CDOF char form for use below

         CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GRID1, GRID_ID_ROW_NUM )
         IF (GRID_ID_ROW_NUM == -1) THEN
            GID_ERR = GID_ERR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1822) 'GRID ', GRID1, 'USET OR USET1', SNAME
            WRITE(F06,1822) 'GRID ', GRID1, 'USET OR USET1', SNAME
         ENDIF
         IF (GRID2 /= GRID1) THEN
            CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GRID2, GRID_ID_ROW_NUM )
            IF (GRID_ID_ROW_NUM == -1) THEN
               GID_ERR = GID_ERR + 1
               FATAL_ERR = FATAL_ERR + 1
               WRITE(ERR,1822) 'GRID ', GRID1, 'USET OR USET1', SNAME
               WRITE(F06,1822) 'GRID ', GRID1, 'USET OR USET1', SNAME
            ENDIF
         ENDIF 

         IF (GID_ERR == 0) THEN                      ! Put CDOF data in USET for GRID1 thru GRID2
            DO GRID_NUM=GRID1,GRID2                  ! GRID2 >= GRID1 was checked in subr BD_SPC1
               CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GRID_NUM, GRID_ID_ROW_NUM )
               IF (GRID_ID_ROW_NUM /= -1) THEN
                  CALL GET_GRID_NUM_COMPS ( GRID(GRID_ID_ROW_NUM,1), NUM_COMPS, SUBR_NAME )
                  DO J = 1,NUM_COMPS                 ! Put data in USET and write enforced displ to L1H.
                     IF (CDOF(J) == '1') THEN
                        IF      (SNAME == 'U1') THEN
                           IF ((USET(GRID_ID_ROW_NUM,J) == '--') .OR. (USET(GRID_ID_ROW_NUM,J) == 'U1')) THEN
                              IF (USET(GRID_ID_ROW_NUM,J) == '--') THEN
                                 NUM_USET_U1 = NUM_USET_U1 + 1
                              ENDIF
                              USET(GRID_ID_ROW_NUM,J) = 'U1'
                           ELSE
                              USET_ERR = USET_ERR + 1
                              FATAL_ERR = FATAL_ERR + 1
                              WRITE(ERR,1332) SNAME, GRID_NUM, J, SNAME, USET(GRID_ID_ROW_NUM,J)
                              WRITE(F06,1332) SNAME, GRID_NUM, J, SNAME, USET(GRID_ID_ROW_NUM,J)
                           ENDIF
                        ELSE IF (SNAME == 'U2') THEN
                           IF ((USET(GRID_ID_ROW_NUM,J) == '--') .OR. (USET(GRID_ID_ROW_NUM,J) == 'U2')) THEN
                              IF (USET(GRID_ID_ROW_NUM,J) == '--') THEN
                                 NUM_USET_U2 = NUM_USET_U2 + 1
                              ENDIF
                              USET(GRID_ID_ROW_NUM,J) = 'U2'
                           ELSE
                              USET_ERR = USET_ERR + 1
                              FATAL_ERR = FATAL_ERR + 1
                              WRITE(ERR,1332) SNAME, GRID_NUM, J, SNAME, USET(GRID_ID_ROW_NUM,J)
                              WRITE(F06,1332) SNAME, GRID_NUM, J, SNAME, USET(GRID_ID_ROW_NUM,J)
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO 
         ENDIF

      ENDDO i_do6 

      IERRT = GID_ERR + USET_ERR
      IF (IERRT > 0) THEN
         WRITE(ERR,1322) IERRT, SUBR_NAME
         WRITE(F06,1322) IERRT, SUBR_NAME
         CALL OUTA_HERE ( 'Y' )
      ENDIF

      CALL WRITE_USET

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1322 FORMAT(' *ERROR  1322: PROCESSING STOPPED DUE TO THE ABOVE LISTED ',I8,' ERRORS IN SUBR ',A)

 1331 FORMAT(' *ERROR  1331: GRID POINT ',I8,' HAS COMPONENT ',I2,' IN THE ',A2,' DISPL SET. (PERM SPC ON GRID CARD)',             &
                           ' HOWEVER THIS GRID/COMPONENT IS ALREADY IN THE ',A2,' DISPL SET')

 1332 FORMAT(' *ERROR  1332: USET SET ',A,' HAS GRID POINT ',I8,' COMPONENT ',I2,' IN THE ',A2,' USET.',                           &
                           ' HOWEVER THIS GRID/COMPONENT IS ALREADY IN THE ',A2,' USET')

 1369 FORMAT(' *ERROR  1369: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' RECORD READ FROM FILE: ',A                                                                            &
                    ,/,14X,' INDICATES THAT AN SPCd DOF BELONGS TO THE "',A2,'" SET. MUST BE EITHER "SE" OR "SB"')

 1822 FORMAT(' *ERROR  1822: ',A,I8,' ON ',A,2X,A,' IS UNDEFINED')

! **********************************************************************************************************************************

      END SUBROUTINE USET_PROC

