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

   MODULE RB_DISP_MATRIX_PROC_Interface

   INTERFACE

      SUBROUTINE RB_DISP_MATRIX_PROC ( REF_PT_TXT, REF_PT )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NCORD, NGRID, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE DOF_TABLES, ONLY            :  TDOF, TDOFI, TDOF_ROW_START
      USE PARAMS, ONLY                :  EQCHK_REF_GRID, SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  RB_DISP_MATRIX_PROC_BEGEND
      USE MODEL_STUF, ONLY            :  CORD, GRID, RGRID, GRID_ID, INV_GRID_SEQ, MODEL_XCG, MODEL_YCG, MODEL_ZCG 
      USE RIGID_BODY_DISP_MATS, ONLY  :  RBGLOBAL_GSET
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: REF_PT_TXT        ! Reference point used in calculating the 6 rigid body displ vectors

      INTEGER(LONG), INTENT(IN)       :: REF_PT            ! An actual grid ID (only used if REF_PT_TXT = 'GRID')

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = RB_DISP_MATRIX_PROC_BEGEND + 1
 
      END SUBROUTINE RB_DISP_MATRIX_PROC

   END INTERFACE

   END MODULE RB_DISP_MATRIX_PROC_Interface

