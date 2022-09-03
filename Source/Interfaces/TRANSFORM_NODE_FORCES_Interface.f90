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

   MODULE TRANSFORM_NODE_FORCES_Interface

   INTERFACE

      SUBROUTINE TRANSFORM_NODE_FORCES ( COORD_SYS )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MELGP, NCORD
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  TRANSFORM_NODE_FORCES_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  CAN_ELEM_TYPE_OFFSET, GRID, CORD, BGRID, ELDOF, ELGP, OFFDIS, OFFSET, PEB, PEG, PEL, TE,  &
                                         TYPE

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: COORD_SYS         ! 'B" for basic, 'G' for global
 
      INTEGER(LONG), PARAMETER        :: NCOLA     = 3     ! An input to subr MATMULT_FFF called herein
      INTEGER(LONG), PARAMETER        :: NCOLB     = 1     ! An input to subr MATMULT_FFF called herein
      INTEGER(LONG), PARAMETER        :: NROWA     = 3     ! An input to subr MATMULT_FFF called herein
      INTEGER(LONG), PARAMETER        :: NROW      = 3     ! An input to subr MATPUT, MATGET called herein
      INTEGER(LONG), PARAMETER        :: NCOL      = 1     ! An input to subr MATPUT, MATGET called herein
      INTEGER(LONG), PARAMETER        :: PCOL      = 1     ! An input to subr MATPUT, MATGET called herein 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = TRANSFORM_NODE_FORCES_BEGEND
 
      REAL(DOUBLE)                    :: THETAD,PHID       ! Returns from subr GEN_T0L (not used here)

      END SUBROUTINE TRANSFORM_NODE_FORCES

   END INTERFACE

   END MODULE TRANSFORM_NODE_FORCES_Interface

