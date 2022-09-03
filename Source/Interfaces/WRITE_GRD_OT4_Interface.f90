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

   MODULE WRITE_GRD_OT4_Interface

   INTERFACE

      SUBROUTINE WRITE_GRD_OT4 ( MAT_NAME, NROWS_MAT, NROWS_TXT, NCOLS, TXT, UNT )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  F04, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_GRD_OT4_BEGEND
 
      IMPLICIT NONE
 
      INTEGER(LONG), INTENT(IN)       :: NROWS_TXT         ! Number of rows in TXT

      CHARACTER(LEN=*), INTENT(IN)    :: MAT_NAME          ! Matrix name of the OTM that MAT describes
      CHARACTER(LEN=*), INTENT(IN)    :: TXT(NROWS_TXT)    ! Lines of this array describe the rows of CB OTM of MAT_NAME

      INTEGER(LONG), INTENT(IN)       :: NCOLS             ! Number of cols in MAT
      INTEGER(LONG), INTENT(IN)       :: NROWS_MAT         ! Number of rows in MAT
      INTEGER(LONG), INTENT(IN)       :: UNT               ! Unit number where to write matrix
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_GRD_OT4_BEGEND
 
      END SUBROUTINE WRITE_GRD_OT4

   END INTERFACE

   END MODULE WRITE_GRD_OT4_Interface

