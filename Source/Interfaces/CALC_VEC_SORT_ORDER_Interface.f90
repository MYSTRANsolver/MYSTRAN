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

   MODULE CALC_VEC_SORT_ORDER_Interface

   INTERFACE

      SUBROUTINE CALC_VEC_SORT_ORDER ( VEC, SORT_ORDER, SORT_INDICES )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  CALC_VEC_SORT_ORDER_BEGEND

      IMPLICIT NONE

      CHARACTER( 5*BYTE), INTENT(OUT) :: SORT_ORDER        ! Order in which the VX(i) have been sorted. If none of the tests below
      INTEGER(LONG), INTENT(OUT)      :: SORT_INDICES(3)   ! Indices of VEC in the order from lowest value component to highest
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CALC_VEC_SORT_ORDER_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: VEC(3)            ! A 3 component vector

      END SUBROUTINE CALC_VEC_SORT_ORDER

   END INTERFACE

   END MODULE CALC_VEC_SORT_ORDER_Interface

