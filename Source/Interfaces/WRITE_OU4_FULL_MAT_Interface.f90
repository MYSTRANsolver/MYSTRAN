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

   MODULE WRITE_OU4_FULL_MAT_Interface

   INTERFACE

      SUBROUTINE WRITE_OU4_FULL_MAT ( MAT_NAME, NROWS, NCOLS, FORM, SYM, MAT, UNT )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, F06, LEN_INPUT_FNAME, OU4, OU4FIL, MOU4, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  PRTOU4
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_OU4_FULL_MAT_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: MAT_NAME          ! Matrix name (only 1st 8 characters will be written)
      CHARACTER(LEN=*), INTENT(IN)    :: SYM               ! 'Y' if input matrix is symmetric

      INTEGER(LONG), INTENT(IN)       :: FORM              ! NASTRAN matrix FORM (not really used in MYSTRAN but needed for OUTPUT4)
      INTEGER(LONG), INTENT(IN)       :: NCOLS             ! Number of cols in MAT
      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in MAT
      INTEGER(LONG), INTENT(IN)       :: UNT               ! Unit number where to write matrix
      INTEGER(LONG), PARAMETER        :: IROW        = 1   ! A term written to UNT for the trailer record (just to be like NASTRAN)
      INTEGER(LONG), PARAMETER        :: PREC        = 2   ! Matrix precision (2 indicates double precision)
      INTEGER(LONG), PARAMETER        :: ROW_BEG     = 1   ! 1st row of matrix output to UNT is row 1
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_OU4_FULL_MAT_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: MAT(NROWS,NCOLS)  ! Array of terms in matrix MAT
 
      END SUBROUTINE WRITE_OU4_FULL_MAT

   END INTERFACE

   END MODULE WRITE_OU4_FULL_MAT_Interface

