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

   MODULE MPC_PROC_Interface

   INTERFACE

      SUBROUTINE MPC_PROC

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR,     F04,     F06,     L1J,     L1S, LINK1S, L1S_MSG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, LMPCADDC, NGRID, NMPC, NMPCADD, NUM_MPCSIDS
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  MPC_PROC_BEGEND
      USE MODEL_STUF, ONLY            :  GRID_ID, MPCSET, MPCSIDS
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START

      IMPLICIT NONE
 
      CHARACTER( 1*BYTE)              :: MPC_SET_USED      ! 'Y'/'N' indicator if an MPC set in B.D. is used

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MPC_PROC_BEGEND
 
      END SUBROUTINE MPC_PROC

   END INTERFACE

   END MODULE MPC_PROC_Interface

