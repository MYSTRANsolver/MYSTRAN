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

   MODULE WRITE_FEMAP_GRID_VECS_Interface

   INTERFACE

      SUBROUTINE WRITE_FEMAP_GRID_VECS ( GRID_VEC, FEMAP_SET_ID, WHAT )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, NEU
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NCORD, NDOFG, NGRID
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  CORD, GRID, GRID_ID, INV_GRID_SEQ
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_FEMAP_GRID_VECS_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: WHAT              ! Indicator if GRID_VEC is DISP, OLOA, SPCF or MPCF

      INTEGER(LONG), INTENT(IN)       :: FEMAP_SET_ID      ! FEMAP set ID to write out
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_FEMAP_GRID_VECS_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: GRID_VEC(NDOFG)   ! G-set Vector to process
 
      END SUBROUTINE WRITE_FEMAP_GRID_VECS

   END INTERFACE

   END MODULE WRITE_FEMAP_GRID_VECS_Interface

