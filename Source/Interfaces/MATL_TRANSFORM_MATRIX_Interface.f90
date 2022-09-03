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

   MODULE MATL_TRANSFORM_MATRIX_Interface

   INTERFACE

      SUBROUTINE MATL_TRANSFORM_MATRIX ( T21, TS )


      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  MATL_TRANSFORM_MATRIX_BEGEND

      IMPLICIT NONE

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = MATL_TRANSFORM_MATRIX_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: T21(3,3)          ! 3x3 matrix that transforms a vector in coord sys 1 to coord sys 2
      REAL(DOUBLE), INTENT(OUT)       :: TS(6,6)           ! 6x6 stress transformation matrix

      END SUBROUTINE MATL_TRANSFORM_MATRIX

   END INTERFACE

   END MODULE MATL_TRANSFORM_MATRIX_Interface

