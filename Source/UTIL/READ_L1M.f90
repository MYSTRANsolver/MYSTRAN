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
 
      SUBROUTINE READ_L1M ( IERROR )
 
! Reads eigenvalue extraction data from file LINK1M. The actual eigenvalue and eigenvector data is not on this file
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
 
      USE SCONTR, ONLY                :  LINKNO, NUM_EIGENS
      USE IOUNT1, ONLY                :  ERR, F06, L1M, L1M_MSG, L1MSTAT, LINK1M, SC1, WRT_ERR, WRT_LOG
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, STIME, TSEC
      USE EIGEN_MATRICES_1 , ONLY     :  EIGEN_VAL, GEN_MASS, MODE_NUM

      USE MODEL_STUF, ONLY            :  EIG_COMP, EIG_CRIT, EIG_FRQ1, EIG_FRQ2, EIG_GRID, EIG_METH, EIG_MSGLVL, EIG_LAP_MAT_TYPE, &
                                         EIG_MODE, EIG_N1, EIG_N2, EIG_NCVFACL, EIG_NORM, EIG_SID, EIG_SIGMA, EIG_VECS, MAXMIJ,    &
                                         MIJ_COL, MIJ_ROW, NUM_FAIL_CRIT

      USE READ_L1M_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(24*BYTE)              :: ENAME(20)         ! Array of names of recirds read from file LINK1M
      CHARACTER(54*BYTE)              :: MODNAM            ! Name to write to screen to describe module being run

      INTEGER(LONG), INTENT(OUT)      :: IERROR            ! Error count
      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IOCHK(20)         ! IOSTAT error number when opening/reading a file
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to
      INTEGER(LONG)                   :: REC_NO            ! Indicator of record number when error encountered reading file
 
! **********************************************************************************************************************************
! Initialize outputs

      IERROR = 0

! Make units for writing errors the screen and output file
 
      OUNT(1) = ERR
      OUNT(2) = F06

      CALL FILE_OPEN ( L1M, LINK1M, OUNT, 'OLD', L1M_MSG, 'READ_STIME', 'UNFORMATTED', 'READ', 'REWIND', 'Y', 'N', 'Y' )

      CALL OURTIM
      MODNAM = 'READ EIGENVALUE DATA FROM PRIOR LINK'
      WRITE(SC1,9092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC

      READ(L1M,IOSTAT=IOCHK( 1)) EIG_SID
      READ(L1M,IOSTAT=IOCHK( 2)) EIG_METH
      READ(L1M,IOSTAT=IOCHK( 3)) EIG_FRQ1
      READ(L1M,IOSTAT=IOCHK( 4)) EIG_FRQ2
      READ(L1M,IOSTAT=IOCHK( 5)) EIG_N1
      READ(L1M,IOSTAT=IOCHK( 6)) EIG_N2
      READ(L1M,IOSTAT=IOCHK( 7)) EIG_VECS
      READ(L1M,IOSTAT=IOCHK( 8)) EIG_CRIT
      READ(L1M,IOSTAT=IOCHK( 9)) EIG_NORM
      READ(L1M,IOSTAT=IOCHK(10)) EIG_GRID
      READ(L1M,IOSTAT=IOCHK(11)) EIG_COMP
      READ(L1M,IOSTAT=IOCHK(12)) EIG_MODE
      READ(L1M,IOSTAT=IOCHK(13)) EIG_SIGMA
      READ(L1M,IOSTAT=IOCHK(14)) EIG_LAP_MAT_TYPE
      READ(L1M,IOSTAT=IOCHK(15)) EIG_MSGLVL
      READ(L1M,IOSTAT=IOCHK(16)) EIG_NCVFACL

      READ(L1M,IOSTAT=IOCHK(17)) NUM_FAIL_CRIT
      READ(L1M,IOSTAT=IOCHK(18)) MAXMIJ
      READ(L1M,IOSTAT=IOCHK(19)) MIJ_ROW
      READ(L1M,IOSTAT=IOCHK(20)) MIJ_COL

      ENAME( 1) = 'EIG_SID'
      ENAME( 2) = 'EIG_METH'
      ENAME( 3) = 'EIG_FRQ1'
      ENAME( 4) = 'EIG_FRQ2'
      ENAME( 5) = 'EIG_N1'
      ENAME( 6) = 'EIG_N2'
      ENAME( 7) = 'EIG_VECS'
      ENAME( 8) = 'EIG_CRIT'
      ENAME( 9) = 'EIG_NORM'
      ENAME(10) = 'EIG_GRID'
      ENAME(11) = 'EIG_COMP'
      ENAME(12) = 'EIG_MODE'
      ENAME(13) = 'EIG_SIGMA'
      ENAME(14) = 'EIG_LAP_MAT_TYPE'
      ENAME(15) = 'EIG_MSGLVL'
      ENAME(16) = 'EIG_NCVFACL'
      ENAME(17) = 'NUM_FAIL_CRIT'
      ENAME(18) = 'MAXMIJ'
      ENAME(19) = 'MIJ_ROW'
      ENAME(20) = 'MIJ_COL'

      REC_NO = 0
      DO I=1,20
         REC_NO = REC_NO + 1
         IF (IOCHK(I) /= 0) THEN
            IERROR = IERROR + 1
            CALL READERR ( IOCHK(I), LINK1M, ENAME(I), REC_NO, OUNT, 'Y' )
         ENDIF
      ENDDO

      DO I=1,NUM_EIGENS
         REC_NO = REC_NO + 1
         READ(L1M,IOSTAT=IOCHK(1)) MODE_NUM(I), EIGEN_VAL(I), GEN_MASS(I)
         IF (IOCHK(1) /= 0) THEN
            IERROR = IERROR + 1
            CALL READERR ( IOCHK(1), LINK1M, L1M_MSG, REC_NO, OUNT, 'Y' )
         ENDIF
      ENDDO

      CALL FILE_CLOSE ( L1M, LINK1M, 'KEEP', 'Y' )

! **********************************************************************************************************************************
 9092 FORMAT(1X,I2,'/',A54,8X,2X,I2,':',I2,':',I2,'.',I3)

! **********************************************************************************************************************************
 
      END SUBROUTINE READ_L1M
