! ###############################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
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

   MODULE GET_SPARSE_CRS_COL_Interface

   INTERFACE

      SUBROUTINE GET_SPARSE_CRS_COL ( MATIN_NAME, COL_NUM, NTERM, NROWS, NCOLS, I_MATIN, J_MATIN, MATIN, BETA, OUT_VEC, NULL_COL )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_SPARSE_CRS_COL_BEGEND
      
      IMPLICIT NONE 
 
      CHARACTER(LEN=*), INTENT(IN )   :: MATIN_NAME        ! Name of input matrix to be partitioned
      CHARACTER(1*BYTE),INTENT(OUT)   :: NULL_COL          ! = 'Y' if OUT_VEC is null

      INTEGER(LONG), INTENT(IN )      :: NROWS             ! No. rows in MATIN
      INTEGER(LONG), INTENT(IN )      :: NTERM             ! No. terms in MATIN
      INTEGER(LONG), INTENT(IN )      :: I_MATIN(NROWS+1)  ! Starting locations in MATIN for each row
      INTEGER(LONG), INTENT(IN )      :: J_MATIN(NTERM)    ! Col numbers for terms in MATIN
      INTEGER(LONG), INTENT(IN )      :: NCOLS             ! No. cols in MATIN
      INTEGER(LONG), INTENT(IN )      :: COL_NUM           ! Col number for the col to get in MATIN
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_SPARSE_CRS_COL_BEGEND
                                 
      REAL(DOUBLE) , INTENT(IN)       :: MATIN(NTERM)      ! Nonzero terms in matrix MATIN
      REAL(DOUBLE) , INTENT(IN)       :: BETA              ! Scalar multiplier for row from MATIN
      REAL(DOUBLE) , INTENT(OUT)      :: OUT_VEC(NROWS)    ! Output vector containing the terms from col COL_NUM of MATIN

      END SUBROUTINE GET_SPARSE_CRS_COL

   END INTERFACE

   END MODULE GET_SPARSE_CRS_COL_Interface

