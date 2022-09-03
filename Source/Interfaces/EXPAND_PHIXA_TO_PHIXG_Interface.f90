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

   MODULE EXPAND_PHIXA_TO_PHIXG_Interface

   INTERFACE

      SUBROUTINE EXPAND_PHIXA_TO_PHIXG


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ONE
      USE IOUNT1, ONLY                :  ERR, F04, F06, L5B, SC1, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, LINKNO, NDOFA, NDOFF, NDOFG, NDOFM, NDOFN, NDOFO, NDOFR, NDOFS, NTERM_PHIXA,&
                                         NTERM_PHIXG, NVEC, SOL_NAME
      USE TIMDAT, ONLY                :  YEAR, MONTH, DAY, HOUR, MINUTE, SEC, SFRAC, STIME, TSEC
      USE COL_VECS, ONLY              :  UA_COL, UG_COL
      USE PARAMS, ONLY                :  EPSIL, TINY
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SPARSE_MATRICES, ONLY       :  I_PHIXA, J_PHIXA, PHIXA, I_PHIXG, J_PHIXG, PHIXG  
      USE SUBR_BEGEND_LEVELS, ONLY    :  EXPAND_PHIXA_TO_PHIXG_BEGEND
      IMPLICIT NONE

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = EXPAND_PHIXA_TO_PHIXG_BEGEND

      REAL(DOUBLE)                    :: SMALL             ! A number used in filtering out small numbers from a full matrix

      END SUBROUTINE EXPAND_PHIXA_TO_PHIXG

   END INTERFACE

   END MODULE EXPAND_PHIXA_TO_PHIXG_Interface

