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
 
      SUBROUTINE TSET_PROC_FOR_SUPORTS ( IERRT )
 
! DOF Processor for SUPORT's 
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1T, L1T_MSG, LINK1T
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFR, NGRID, NUM_SUPT_CARDS
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  DOF_PROC_BEGEND
      USE PARAMS, ONLY                :  EPSIL
      USE DOF_TABLES, ONLY            :  TSET_CHR_LEN, TSET
      USE MODEL_STUF, ONLY            :  GRID, GRID_ID
 
      USE TSET_PROC_FOR_SUPORTS_USE_IFs                    ! Added 2019/07/14

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'TSET_PROC_FOR_SUPTS'
      CHARACTER( 1*BYTE)              :: CDOF(6)           ! An output from subr RDOF
      CHARACTER(LEN=LEN(TSET_CHR_LEN)):: DOFSET            ! The name of a DOF set (e.g. 'SB', 'A ', etc)
 
      INTEGER(LONG), INTENT(INOUT)    :: IERRT             ! Sum of all grid and DOF errors
      INTEGER(LONG)                   :: GRID_NUM          ! An actual grid ID
      INTEGER(LONG)                   :: DOF_ERR   = 0     ! Count of errors that result from setting displ sets in TSET
      INTEGER(LONG)                   :: GRID_ID_ROW_NUM   ! Row number, in array GRID_ID, where an actual grid ID resides
      INTEGER(LONG)                   :: GID_ERR   = 0     ! Count of errors that result from undefined grid ID's
      INTEGER(LONG)                   :: I,K               ! DO loop indices
      INTEGER(LONG)                   :: ICOMP             ! DOF components read from file LINK1T (SUPORT's)
      INTEGER(LONG)                   :: IOCHK             ! IOSTAT error number when opening or reading a file
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
! Process SPC data from file L1T (data written when SUPORT Bulk Data cards were read)
 
      GID_ERR = 0
      DOF_ERR = 0
      DOFSET = 'R '
      REC_NO = 0

i_do6:DO I=1,NUM_SUPT_CARDS
 
         READ(L1T,IOSTAT=IOCHK) GRID_NUM, ICOMP
         REC_NO = REC_NO + 1
         IF (IOCHK /= 0) THEN
            CALL READERR ( IOCHK, LINK1T, L1T_MSG, REC_NO, OUNT, 'Y' )
            CALL OUTA_HERE ( 'Y' )                         ! Error reading SUPORT data file . No sense continuing
         ENDIF

         CALL GET_ARRAY_ROW_NUM ( 'GRID_ID', SUBR_NAME, NGRID, GRID_ID, GRID_NUM, GRID_ID_ROW_NUM )
         IF (GRID_ID_ROW_NUM == -1) THEN
            GID_ERR = GID_ERR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1310) 'GRID ', GRID_NUM, 'SUPORT'
            WRITE(F06,1310) 'GRID ', GRID_NUM, 'SUPORT'
         ENDIF
 
         IF (GID_ERR == 0) THEN
            CALL RDOF ( ICOMP, CDOF )                      ! Convert ICOMP to CDOF char form for use below
            DO K = 1,6                                     ! Put data in TSET
               IF (CDOF(K) == '1') THEN
                  IF      (DOFSET == 'R ') THEN
                     IF (TSET(GRID_ID_ROW_NUM,K) == 'A ') THEN
                        TSET(GRID_ID_ROW_NUM,K) = 'R '
                        NDOFR = NDOFR + 1
                     ELSE
                        DOF_ERR = DOF_ERR + 1
                        FATAL_ERR = FATAL_ERR + 1
                        WRITE(ERR,1334) GRID_NUM, K, DOFSET, TSET(GRID_ID_ROW_NUM,K)
                        WRITE(F06,1334) GRID_NUM, K, DOFSET, TSET(GRID_ID_ROW_NUM,K)
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDIF

      ENDDO i_do6 

      IERRT = IERRT + GID_ERR + DOF_ERR

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 1310 FORMAT(' *ERROR  1310: ',A,I8,' ON ',A,' BULK DATA ENTRY IS UNDEFINED')

 1331 FORMAT(' *ERROR  1331: GRID POINT ',I8,' HAS COMPONENT ',I2,' IN THE ',A2,' DISPL SET.',                                     &
                           ' HOWEVER THIS GRID/COMP IS ALREADY IN THE ',A2,' DISPL SET')

 1334 FORMAT(' *ERROR  1334: SUPORT B.D. ENTRY HAS GRID POINT ',I8,' COMPONENT ',I2,' IN THE ',A2,' DISPL SET.',                   &
                           ' HOWEVER THIS GRID/COMP IS ALREADY IN THE ',A2,' DISPL SET')

 1369 FORMAT(' *ERROR  1369: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' RECORD READ FROM FILE: ',A                                                                            &
                    ,/,14X,' INDICATES THAT AN SPCd DOF BELONGS TO THE "',A2,'" SET. MUST BE EITHER "SE" OR "SB"')

! **********************************************************************************************************************************

      END SUBROUTINE TSET_PROC_FOR_SUPORTS
