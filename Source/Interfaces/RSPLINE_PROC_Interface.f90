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

   MODULE RSPLINE_PROC_Interface

   INTERFACE

      SUBROUTINE RSPLINE_PROC ( RTYPE, REC_NO, IERR )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1F, L1F_MSG, LINK1F, L1J
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MRSPLINE, NCORD, NGRID, NTERM_RMG
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE SUBR_BEGEND_LEVELS, ONLY    :  RIGID_ELEM_PROC_BEGEND
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE MODEL_STUF, ONLY            :  CORD, GRID, RGRID, GRID_ID, CORD
      USE PARAMS, ONLY                :  EPSIL
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      IMPLICIT NONE
 
      CHARACTER( 8*BYTE), INTENT(IN)  :: RTYPE             ! The type of rigid element being processed (RSPLINE)

      INTEGER(LONG), INTENT(INOUT)    :: IERR              ! Count of errors in RIGID_ELEM_PROC
      INTEGER(LONG), INTENT(INOUT)    :: REC_NO            ! Record number when reading a file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RIGID_ELEM_PROC_BEGEND + 1
 
      END SUBROUTINE RSPLINE_PROC

   END INTERFACE

   END MODULE RSPLINE_PROC_Interface

