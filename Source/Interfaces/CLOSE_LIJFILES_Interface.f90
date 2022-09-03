! ###############################################################################################################################
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

   MODULE CLOSE_LIJFILES_Interface

   INTERFACE

      SUBROUTINE CLOSE_LIJFILES ( CLOSE_STAT )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, RESTART
      USE IOUNT1, ONLY                :  MOU4,    WRT_ERR, WRT_LOG, ERR, F06,                                                      &
                                         L1B,     L1C,     L1D,     L1E,     L1F,     L1G,     L1H,     L1I,     L1J,     L1K,     &
                                         L1L,     L1M,     L1N,     L1O,     L1P,     L1Q,     L1R,     L1S,     L1T,     L1U,     &
                                         L1V,     L1W,     L1X,     L1Y,     L1Z,                                                  &
                                         L2A,     L2B,     L2C,     L2D,     L2E,     L2F,     L2G,     L2H,     L2I,     L2J,     &
                                         L2K,     L2L,     L2M,     L2N,     L2O,     L2P,     L2Q,     L2R,     L2S,     L2T,     &
                                         L3A,     L4A,     L4B,     L4C,     L4D,     L5A,     L5B,     OU4

      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG,                                                                &
                                         LINK1B,  LINK1C,  LINK1D,  LINK1E,  LINK1F,  LINK1G,  LINK1H,  LINK1I,  LINK1J,  LINK1K,  &
                                         LINK1L,  LINK1M,  LINK1N,  LINK1O,  LINK1P,  LINK1Q,  LINK1R,  LINK1S,  LINK1T,  LINK1U,  &
                                         LINK1V,  LINK1W,  LINK1X,  LINK1Y,  LINK1Z,                                               &
                                         LINK2A,  LINK2B,  LINK2C,  LINK2D,  LINK2E,  LINK2F,  LINK2G,  LINK2H,  LINK2I,  LINK2J,  &
                                         LINK2K,  LINK2L,  LINK2M,  LINK2N,  LINK2O,  LINK2P,  LINK2Q,  LINK2R,  LINK2S,  LINK2T,  &
                                         LINK3A,  LINK4A,  LINK4B,  LINK4C,  LINK4D,  LINK5A,  LINK5B,  OU4FIL

      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG,                                                                &
                                         L1BSTAT, L1CSTAT, L1DSTAT, L1ESTAT, L1FSTAT, L1GSTAT, L1HSTAT, L1ISTAT, L1JSTAT, L1KSTAT, &
                                         L1LSTAT, L1MSTAT, L1NSTAT, L1OSTAT, L1PSTAT, L1QSTAT, L1RSTAT, L1SSTAT, L1TSTAT, L1USTAT, &
                                         L1VSTAT, L1WSTAT, L1XSTAT, L1YSTAT, L1ZSTAT,                                              &
                                         L2ASTAT, L2BSTAT, L2CSTAT, L2DSTAT, L2ESTAT, L2FSTAT, L2GSTAT, L2HSTAT, L2ISTAT, L2JSTAT, &
                                         L2KSTAT, L2LSTAT, L2MSTAT, L2NSTAT, L2OSTAT, L2PSTAT, L2QSTAT, L2RSTAT, L2SSTAT, L2TSTAT, &
                                         L3ASTAT, L4ASTAT, L4BSTAT, L4CSTAT, L4DSTAT, L5ASTAT, L5BSTAT, OU4STAT

      USE TIMDAT, ONLY                :  STIME, TSEC

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: CLOSE_STAT        ! Indicator of what to do with file when it is closed

      END SUBROUTINE CLOSE_LIJFILES

   END INTERFACE

   END MODULE CLOSE_LIJFILES_Interface

