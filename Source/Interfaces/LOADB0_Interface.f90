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

   MODULE LOADB0_Interface

   INTERFACE

      SUBROUTINE LOADB0

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, IN1                
      USE SCONTR, ONLY                :  BD_ENTRY_LEN, BLNK_SUB_NAM, FATAL_ERR, LCMASS, LDOFG, LELE,                               &
                                         LEDAT, LFORCE, LCONM2, LCORD, LGRAV, LGRID, LGUSERIN, LLOADC, LLOADR,                     &
                                         LMATL, LMPC, LMPCADDC, LMPCADDR, LPBAR, LPBEAM, LPBUSH, LPCOMP, LPCOMP_PLIES, LPDAT,      &
                                         LPELAS, LPLOAD, LPMASS, LPROD, LPSHEL, LPSHEAR, LPSOLID, LPUSER1, LPUSERIN, LRFORCE,      &
                                         LRIGEL, LSEQ, LSLOAD, LSPC, LSPC1, LSPCADDC, LSPCADDR, LSUSERIN, LTDAT,                   &
                                         MEDAT_CBAR, MEDAT_CBEAM, MEDAT_CBUSH,                                                     &
                                         MEDAT_CELAS1, MEDAT_CELAS2, MEDAT_CELAS3, MEDAT_CELAS4,                                   &
                                         MEDAT_CQUAD, MEDAT_CROD, MEDAT_CSHEAR, MEDAT_CTRIA, MEDAT_CUSER1, MEDAT0_CUSERIN, MMPC,   &
                                         MPDAT_PLOAD2, MPDAT_PLOAD4, MEDAT_PLOTEL, MRBE3, MRSPLINE, MTDAT_TEMPRB, MTDAT_TEMPP1,    &
                                         NPBARL, NSPOINT, PROG_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  LOADB0_BEGEND
      USE MODEL_STUF, ONLY            :  GRDSET3, GRDSET7, GRDSET8
      USE PARAMS, ONLY                :  GRIDSEQ
 
      IMPLICIT NONE
 
      CHARACTER( 7*BYTE), PARAMETER   :: END_CARD    = 'ENDDATA'

      INTEGER(LONG)                   :: NG_USERIN         ! Number of grids found on USERIN elems (not incl SPOINT's)
      INTEGER(LONG)                   :: NS_USERIN         ! Number of SPOINT's found on USERIN elems
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = LOADB0_BEGEND
 
      END SUBROUTINE LOADB0

   END INTERFACE

   END MODULE LOADB0_Interface

