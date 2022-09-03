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

   MODULE WRT_REAL_TO_CHAR_VAR_Interface

   INTERFACE

      SUBROUTINE WRT_REAL_TO_CHAR_VAR ( REAL_VAR, NROWS, NCOLS, ROW_NUM, CHAR_VAR )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F06
      USE SCONTR, ONLY                :  FATAL_ERR
      USE CONSTANTS_1, ONLY           :  ZERO

      IMPLICIT NONE

      CHARACTER(14*BYTE), INTENT(OUT) :: CHAR_VAR(NCOLS)      ! Character representation of the real data in one row of REAL_VAR

      INTEGER(LONG), INTENT(IN)       :: NCOLS                ! Number of cols in array REAL_VAR
      INTEGER(LONG), INTENT(IN)       :: NROWS                ! Number of rows in array REAL_VAR
      INTEGER(LONG), INTENT(IN)       :: ROW_NUM              ! Row number in array REAL_VAR to write

      REAL(DOUBLE) , INTENT(IN)       :: REAL_VAR(NROWS,NCOLS)! 

      END SUBROUTINE WRT_REAL_TO_CHAR_VAR

   END INTERFACE

   END MODULE WRT_REAL_TO_CHAR_VAR_Interface

