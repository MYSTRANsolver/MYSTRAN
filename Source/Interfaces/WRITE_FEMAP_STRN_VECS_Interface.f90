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

   MODULE WRITE_FEMAP_STRN_VECS_Interface

   INTERFACE

      SUBROUTINE WRITE_FEMAP_STRN_VECS ( ELEM_TYP, IS_PCOMP, NUM_FEMAP_ROWS, FEMAP_SET_ID )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, NEU
      USE PARAMS, ONLY                :  SUPWARN
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NGRID, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CC_OUTPUT_DESCRIBERS, ONLY  :  STRN_OPT
      USE FEMAP_ARRAYS, ONLY          :  FEMAP_EL_NUMS, FEMAP_EL_VECS
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_FEMAP_STRN_VECS_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: ELEM_TYP               ! Element type
      CHARACTER(LEN=*), INTENT(IN)    :: IS_PCOMP               ! 'Y'/'N' for whether elements are PCOMP

      INTEGER(LONG), INTENT(IN)       :: NUM_FEMAP_ROWS         ! Number of rows of FEMAP data to write
      INTEGER(LONG), INTENT(IN)       :: FEMAP_SET_ID           ! FEMAP set ID to write out

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_FEMAP_STRN_VECS_BEGEND

      END SUBROUTINE WRITE_FEMAP_STRN_VECS

   END INTERFACE

   END MODULE WRITE_FEMAP_STRN_VECS_Interface

