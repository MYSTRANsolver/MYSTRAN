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

   MODULE WRITE_L1M_Interface

   INTERFACE

      SUBROUTINE WRITE_L1M

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
 
      USE SCONTR, ONLY                :  LINKNO, NUM_EIGENS
      USE IOUNT1, ONLY                :  ERR, F06, L1M, L1M_MSG, L1MSTAT, LINK1M, SC1, WRT_ERR, WRT_LOG
      USE TIMDAT, ONLY                :  HOUR, MINUTE, SEC, SFRAC, STIME, TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE EIGEN_MATRICES_1 , ONLY     :  EIGEN_VAL, GEN_MASS, MODE_NUM

      USE MODEL_STUF, ONLY            :  EIG_COMP, EIG_CRIT, EIG_FRQ1, EIG_FRQ2, EIG_GRID, EIG_METH, EIG_MSGLVL, EIG_LAP_MAT_TYPE, &
                                         EIG_MODE, EIG_N1, EIG_N2, EIG_NCVFACL, EIG_NORM, EIG_SID, EIG_SIGMA, EIG_VECS, MAXMIJ,    &
                                         MIJ_COL, MIJ_ROW, NUM_FAIL_CRIT

      IMPLICIT NONE
 
      END SUBROUTINE WRITE_L1M

   END INTERFACE

   END MODULE WRITE_L1M_Interface

