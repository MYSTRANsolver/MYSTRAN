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
 
      SUBROUTINE WRITE_L1M
 
! Writes data from file LINK1M of eigenvalue extraction data (actual eigenvalues/vectors are not in this file)
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
 
      USE SCONTR, ONLY                :  LINKNO, NUM_EIGENS
      USE IOUNT1, ONLY                :  ERR, F06, L1M, L1M_MSG, L1MSTAT, LINK1M, SC1, WRT_ERR, WRT_LOG
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, STIME, TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE EIGEN_MATRICES_1 , ONLY     :  EIGEN_VAL, GEN_MASS, MODE_NUM

      USE MODEL_STUF, ONLY            :  EIG_COMP, EIG_CRIT, EIG_FRQ1, EIG_FRQ2, EIG_GRID, EIG_METH, EIG_MSGLVL, EIG_LAP_MAT_TYPE, &
                                         EIG_MODE, EIG_N1, EIG_N2, EIG_NCVFACL, EIG_NORM, EIG_SID, EIG_SIGMA, EIG_VECS, MAXMIJ,    &
                                         MIJ_COL, MIJ_ROW, NUM_FAIL_CRIT

      USE WRITE_L1M_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(54*BYTE)              :: MODNAM            ! Name to write to screen to describe module being run

      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: OUNT(2)           ! File units to write messages to
 
! **********************************************************************************************************************************
! Make units for writing errors the screen and output file
 
      OUNT(1) = ERR
      OUNT(2) = F06

!xx   STATUS = 'OLD    '
!xx   RW     = 'WRITE'
      CALL FILE_OPEN ( L1M, LINK1M, OUNT, 'REPLACE', L1M_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )

      CALL OURTIM
      MODNAM = 'WRITE EIGENVALUE DATA FROM PRIOR LINK'
      WRITE(SC1,9092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC

      WRITE(L1M) EIG_SID
      WRITE(L1M) EIG_METH
      WRITE(L1M) EIG_FRQ1
      WRITE(L1M) EIG_FRQ2
      WRITE(L1M) EIG_N1
      WRITE(L1M) EIG_N2
      WRITE(L1M) EIG_VECS
      WRITE(L1M) EIG_CRIT
      WRITE(L1M) EIG_NORM
      WRITE(L1M) EIG_GRID
      WRITE(L1M) EIG_COMP
      WRITE(L1M) EIG_MODE
      WRITE(L1M) EIG_SIGMA
      WRITE(L1M) EIG_LAP_MAT_TYPE
      WRITE(L1M) EIG_MSGLVL
      WRITE(L1M) EIG_NCVFACL
      WRITE(L1M) NUM_FAIL_CRIT
      WRITE(L1M) MAXMIJ
      WRITE(L1M) MIJ_ROW
      WRITE(L1M) MIJ_COL

      DO I=1,NUM_EIGENS
         WRITE(L1M) MODE_NUM(I), EIGEN_VAL(I), GEN_MASS(I)
      ENDDO

      CALL FILE_CLOSE ( L1M, LINK1M, 'KEEP', 'Y' )

! **********************************************************************************************************************************
 9092 FORMAT(1X,I2,'/',A54,8X,2X,I2,':',I2,':',I2,'.',I3)

! **********************************************************************************************************************************
 
      END SUBROUTINE WRITE_L1M
