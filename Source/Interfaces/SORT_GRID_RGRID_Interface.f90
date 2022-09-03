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

   MODULE SORT_GRID_RGRID_Interface

   INTERFACE

      SUBROUTINE SORT_GRID_RGRID ( CALLING_SUBR, MESSAG, NSIZE, IARRAY, RARRAY )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MGRID, MRGRID
      USE PARAMS, ONLY                :  SORT_MAX
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  SORT_GRID_RGRID_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: CALLING_SUBR        ! Subr that called this subr
      CHARACTER(LEN=*), INTENT(IN)    :: MESSAG              ! Message to be written out if this subr fails to sort

      INTEGER(LONG), INTENT(IN)       :: NSIZE               ! No. rows in arrays IARRAY, RARRAY
      INTEGER(LONG), INTENT(INOUT)    :: IARRAY(NSIZE,MGRID) ! Array GRID
      INTEGER(LONG)                   :: IDUM1               ! Dummy values in IARRAY used when switching IARRAY rows during sort 
      INTEGER(LONG)                   :: JCT                 ! Shell sort parameter returned from subroutine SORTLEN.
      INTEGER(LONG)                   :: SORTPK              ! Intermediate variable used in setting a DO loop range.
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SORT_GRID_RGRID_BEGEND
 
      REAL(DOUBLE),  INTENT(INOUT)    :: RARRAY(NSIZE,MRGRID)! Array RGRID 
      REAL(DOUBLE)                    :: RDUM1               ! Dummy values in RARRAY used when switching RARRAY rows during sort
 
      END SUBROUTINE SORT_GRID_RGRID

   END INTERFACE

   END MODULE SORT_GRID_RGRID_Interface

