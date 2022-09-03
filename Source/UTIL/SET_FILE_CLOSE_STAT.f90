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
 
      SUBROUTINE SET_FILE_CLOSE_STAT ( CLOSE_STAT )
 
! This subr gets called by subrs LINK0 and READ_INI to set a common close status for all files.
! LINK0 does this if the run is check pointed
! READ_INI does this if the MYSTRAN.INI file sets a common close status (like KEEP) for ALLFILES 

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
 
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERRSTAT, F04STAT, SEQSTAT, SPCSTAT, L1ASTAT,                   &
                                         L1BSTAT, L1CSTAT, L1DSTAT, L1ESTAT, L1FSTAT, L1GSTAT, L1HSTAT, L1ISTAT, L1TSTAT, L1JSTAT, &
                                         L1KSTAT, L1LSTAT, L1MSTAT, L1NSTAT, L1OSTAT, L1PSTAT, L1QSTAT, L1RSTAT, L1SSTAT, L1USTAT, &
                                         L1VSTAT, L1WSTAT, L1XSTAT, L1YSTAT, L1ZSTAT,                                              &
                                         L2ASTAT, L2BSTAT, L2CSTAT, L2DSTAT, L2ESTAT, L2FSTAT, L2GSTAT, L2HSTAT, L2ISTAT, L2JSTAT, &
                                         L2KSTAT, L2LSTAT, L2MSTAT, L2NSTAT, L2OSTAT, L2PSTAT, L2QSTAT, L2RSTAT, L2SSTAT, L2TSTAT, &
                                         L3ASTAT, L4ASTAT, L4BSTAT, L4CSTAT, L4DSTAT, L5ASTAT, L5BSTAT
      USE SET_FILE_CLOSE_STAT_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: CLOSE_STAT        ! Close status of files

! **********************************************************************************************************************************

      L1ASTAT = CLOSE_STAT
      L1BSTAT = CLOSE_STAT
      L1CSTAT = CLOSE_STAT
      L1DSTAT = CLOSE_STAT
      L1ESTAT = CLOSE_STAT
      L1FSTAT = CLOSE_STAT
      L1GSTAT = CLOSE_STAT
      L1HSTAT = CLOSE_STAT
      L1ISTAT = CLOSE_STAT
      L1JSTAT = CLOSE_STAT
      L1KSTAT = CLOSE_STAT
      L1LSTAT = CLOSE_STAT
      L1MSTAT = CLOSE_STAT
      L1NSTAT = CLOSE_STAT
      L1OSTAT = CLOSE_STAT
      L1PSTAT = CLOSE_STAT
      L1QSTAT = CLOSE_STAT
      L1RSTAT = CLOSE_STAT
      L1SSTAT = CLOSE_STAT
      L1TSTAT = CLOSE_STAT
      L1USTAT = CLOSE_STAT
      L1VSTAT = CLOSE_STAT
      L1WSTAT = CLOSE_STAT
      L1XSTAT = CLOSE_STAT
      L1YSTAT = CLOSE_STAT
      L1ZSTAT = CLOSE_STAT
      L2ASTAT = CLOSE_STAT
      L2BSTAT = CLOSE_STAT
      L2CSTAT = CLOSE_STAT
      L2DSTAT = CLOSE_STAT
      L2ESTAT = CLOSE_STAT
      L2FSTAT = CLOSE_STAT
      L2GSTAT = CLOSE_STAT
      L2HSTAT = CLOSE_STAT
      L2ISTAT = CLOSE_STAT
      L2JSTAT = CLOSE_STAT
      L2KSTAT = CLOSE_STAT
      L2LSTAT = CLOSE_STAT
      L2MSTAT = CLOSE_STAT
      L2NSTAT = CLOSE_STAT
      L2OSTAT = CLOSE_STAT
      L2PSTAT = CLOSE_STAT
      L2QSTAT = CLOSE_STAT
      L2RSTAT = CLOSE_STAT
      L2SSTAT = CLOSE_STAT
      L2TSTAT = CLOSE_STAT
      L3ASTAT = CLOSE_STAT
      L3ASTAT = CLOSE_STAT
      L4ASTAT = CLOSE_STAT
      L4BSTAT = CLOSE_STAT
      L4CSTAT = CLOSE_STAT
      L4DSTAT = CLOSE_STAT
      L5ASTAT = CLOSE_STAT
      L5BSTAT = CLOSE_STAT

! **********************************************************************************************************************************
 
      END SUBROUTINE SET_FILE_CLOSE_STAT
