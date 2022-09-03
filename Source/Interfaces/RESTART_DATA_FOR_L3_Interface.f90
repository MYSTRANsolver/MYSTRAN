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

   MODULE RESTART_DATA_FOR_L3_Interface

   INTERFACE

      SUBROUTINE RESTART_DATA_FOR_L3

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE IOUNT1, ONLY                :  L2G, LINK2G, L2G_MSG, L2GSTAT
      USE IOUNT1, ONLY                :  L2H, LINK2H, L2H_MSG, L2HSTAT
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFL, NTERM_KLL, NTERM_PL
      USE TIMDAT, ONLY                :  TSEC 
      USE SPARSE_MATRICES, ONLY       :  I_KLL , J_KLL , KLL ,I_PL , J_PL , PL
      USE SUBR_BEGEND_LEVELS, ONLY    :  RESTART_DATA_FOR_L3_BEGEND

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RESTART_DATA_FOR_L3_BEGEND
 
      END SUBROUTINE RESTART_DATA_FOR_L3

   END INTERFACE

   END MODULE RESTART_DATA_FOR_L3_Interface

