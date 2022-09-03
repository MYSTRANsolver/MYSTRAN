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

   MODULE PLANE_COORD_TRANS_21_Interface

   INTERFACE

      SUBROUTINE PLANE_COORD_TRANS_21 ( THETA, T21, CALLING_SUBR )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ONE, ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  PLANE_COORD_TRANS_21_BEGEND
      
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR      ! Subr that called this one

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = PLANE_COORD_TRANS_21_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: THETA             ! Angle from x axis of system 1 to x axis of system 2
      REAL(DOUBLE), INTENT(OUT)       :: T21(3,3)          ! Transformation matrix which will transform a vector, U1, in coord sys
      END SUBROUTINE PLANE_COORD_TRANS_21

   END INTERFACE

   END MODULE PLANE_COORD_TRANS_21_Interface

