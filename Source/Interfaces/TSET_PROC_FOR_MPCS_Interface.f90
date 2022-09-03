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

   MODULE TSET_PROC_FOR_MPCS_Interface

   INTERFACE

      SUBROUTINE TSET_PROC_FOR_MPCS ( IERRT )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1S, L1S_MSG, LINK1S
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, LIND_GRDS_MPCS, LMPCADDC, NDOFM, NGRID, NIND_GRDS_MPCS, NMPC,    &
                                         NMPCADD, NTERM_RMG, NUM_MPCSIDS 
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  DOF_PROC_BEGEND
      USE DOF_TABLES, ONLY            :  TSET_CHR_LEN, TSET
      USE MODEL_STUF, ONLY            :  GRID_ID, MPC_IND_GRIDS, MPCSET, MPCSIDS
 
      IMPLICIT NONE

      CHARACTER( 1*BYTE)              :: MPC_SET_USED      ! 'Y'/'N' indicator if an MPC set in B.D. is used
 
      INTEGER(LONG), INTENT(INOUT)    :: IERRT             ! Sum of all grid and DOF errors

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = DOF_PROC_BEGEND + 1
 
      END SUBROUTINE TSET_PROC_FOR_MPCS

   END INTERFACE

   END MODULE TSET_PROC_FOR_MPCS_Interface

