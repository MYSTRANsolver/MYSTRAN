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

   MODULE GET_I_MAT_FROM_I2_MAT_Interface

   INTERFACE

      SUBROUTINE GET_I_MAT_FROM_I2_MAT ( MAT_NAME, NROWS, NTERMS, I2_MAT, I_MAT ) 


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_I_MAT_FROM_I2_MAT_BEGEND

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: MAT_NAME          ! Matrix name

      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in MAT
      INTEGER(LONG), INTENT(IN)       :: NTERMS            ! Number of matrix terms that should be in MAT
      INTEGER(LONG), INTENT(IN)       :: I2_MAT(NTERMS)    ! Row numbers for terms in matrix MAT
      INTEGER(LONG), INTENT(OUT)      :: I_MAT(NROWS+1)    ! Row numbers for terms in matrix MAT
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_I_MAT_FROM_I2_MAT_BEGEND

      END SUBROUTINE GET_I_MAT_FROM_I2_MAT

   END INTERFACE

   END MODULE GET_I_MAT_FROM_I2_MAT_Interface

