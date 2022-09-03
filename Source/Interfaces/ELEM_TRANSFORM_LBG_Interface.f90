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

   MODULE ELEM_TRANSFORM_LBG_Interface

   INTERFACE

      SUBROUTINE ELEM_TRANSFORM_LBG ( WHICH, ZE, QE )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MELDOF, NCORD, NGRID, NSUB, NTSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELEM_TRANSFORM_LBG_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE MODEL_STUF, ONLY            :  ELDOF, GRID, GRID_ID, CORD, AGRID, TE_IDENT, TYPE
      USE MODEL_STUF, ONLY            :  ELGP

      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: WHICH             ! 'K' for stiffness, 'M' for mass or 'PTE' for thermal load matrix
 
      INTEGER(LONG), PARAMETER        :: NCOLA     = 3     ! An input to subr MATMULT_FFF/MATMULT_FFF_T, called herein
      INTEGER(LONG), PARAMETER        :: NROW_GET  = 3     ! An input to subr MATGET/MATPUT (no. rows to get/put)
      INTEGER(LONG), PARAMETER        :: NROWA     = 3     ! An input to subr MATMULT_FFF/MATMULT_FFF_T, called herein
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELEM_TRANSFORM_LBG_BEGEND
 
      REAL(DOUBLE) , INTENT(INOUT)    :: QE(MELDOF,NSUB)  ! PTE or PPE if WHICH = 'PTE' or 'PPE'
      REAL(DOUBLE) , INTENT(INOUT)    :: ZE(MELDOF,MELDOF) ! Either the mass or stiff matrix of the element
 
      END SUBROUTINE ELEM_TRANSFORM_LBG

   END INTERFACE

   END MODULE ELEM_TRANSFORM_LBG_Interface

