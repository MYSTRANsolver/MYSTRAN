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

   MODULE GET_VEC_MIN_MAX_ABS_Interface

   INTERFACE

      SUBROUTINE GET_VEC_MIN_MAX_ABS ( NROWS, ID_LIST, VECTOR, VEC_MIN, VEC_MAX, VEC_ABS, ID_MIN, ID_MAX )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_VEC_MIN_MAX_ABS_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MACHINE_PARAMS, ONLY        :  MACH_LARGE_NUM
  
      IMPLICIT NONE
 
      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in ID_LIST and VECTOR
      INTEGER(LONG), INTENT(IN)       :: ID_LIST(NROWS)    ! The ID (grid or elem) numbers corresponding to rows in VECTOR
      INTEGER(LONG), INTENT(OUT)      :: ID_MAX            ! ID where vector is max
      INTEGER(LONG), INTENT(OUT)      :: ID_MIN            ! ID where vector is min
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_VEC_MIN_MAX_ABS_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: VECTOR(NROWS)     ! Values to scan for MIN, MAX, ABS
      REAL(DOUBLE) , INTENT(OUT)      :: VEC_ABS           ! Abs value in vector
      REAL(DOUBLE) , INTENT(OUT)      :: VEC_MAX           ! Max value in vector
      REAL(DOUBLE) , INTENT(OUT)      :: VEC_MIN           ! Min value in vector

      END SUBROUTINE GET_VEC_MIN_MAX_ABS

   END INTERFACE

   END MODULE GET_VEC_MIN_MAX_ABS_Interface

