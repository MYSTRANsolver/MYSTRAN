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

   MODULE KGG_SINGULARITY_PROC_Interface

   INTERFACE

      SUBROUTINE KGG_SINGULARITY_PROC ( AGRID, KGRD, NUM_ASPC_BY_COMP )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, SPC
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFSA, NGRID, NUM_PCHD_SPC1
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE PARAMS, ONLY                :  AUTOSPC, AUTOSPC_INFO, AUTOSPC_RAT, EPSIL, PCHSPC1, SPC1SID, SUPINFO
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  KGG_SINGULARITY_PROC_BEGEND
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START, TDOFI, TSET
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE MODEL_STUF, ONLY            :  GRID_ID

      IMPLICIT NONE

      INTEGER(LONG), INTENT(IN)       :: AGRID              ! Actual grid ID for IGRID
      INTEGER(LONG), INTENT(INOUT)    :: NUM_ASPC_BY_COMP(6)! The number of DOF's AUTOSPC'd for each displ component
      INTEGER(LONG)                   :: EIGENVAL_NUM(6)    ! Array to hold the eigenvalue number used in finding a SINGLR_COMP
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = KGG_SINGULARITY_PROC_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: KGRD(6,6)          ! 6x6 diagonal stiffness matrix for grid point AGRID
      REAL(DOUBLE)                    :: FAC                ! Multipling factor used in an intermediate calc
      REAL(DOUBLE)                    :: EPS1               ! Small value used in comparison to determine a real zero

      END SUBROUTINE KGG_SINGULARITY_PROC

   END INTERFACE

   END MODULE KGG_SINGULARITY_PROC_Interface

