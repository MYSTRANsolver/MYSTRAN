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

   MODULE WRITE_PARTNd_MAT_HDRS_Interface

   INTERFACE

      SUBROUTINE WRITE_PARTNd_MAT_HDRS ( MAT_NAME, ROW_SET, COL_SET, NROWS, NCOLS )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, MTDOF, NDOFA, NDOFF, NDOFG, NDOFL, NDOFM, NDOFN, NDOFO, NDOFR,   &
                                         NDOFS, NDOFSA, NDOFSB, NDOFSE, NDOFSG, NDOFSZ, NUM_USET_U1, NUM_USET_U2, TSET_CHR_LEN
      USE TIMDAT, ONLY                :  TSEC
      USE DOF_TABLES, ONLY            :  TDOFI
      USE OUTPUT4_MATRICES, ONLY      :  OU4_MAT_COL_GRD_COMP, OU4_MAT_ROW_GRD_COMP
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_PARTNd_MAT_HDRS_BEGEND

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: MAT_NAME            ! Name of the partitioned matrix whose row/col headings will be output
      CHARACTER(LEN=*), INTENT(IN)    :: COL_SET             ! Set name that the cols of MAT_NAME belong to (e.g. 'G ', 'l ', etc)
      CHARACTER(LEN=*), INTENT(IN)    :: ROW_SET             ! Set name that the rows of MAT_NAME belong to (e.g. 'G ', 'l ', etc)

      INTEGER(LONG), INTENT(IN)       :: NCOLS               ! Number of cols in the partitioned matrix MAT_NAME
      INTEGER(LONG), INTENT(IN)       :: NROWS               ! Number of rows in the partitioned matrix MAT_NAME
      INTEGER(LONG)                   :: NUM_LEFT            ! Used when printing a line of 10 values in the set
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_PARTNd_MAT_HDRS_BEGEND

      END SUBROUTINE WRITE_PARTNd_MAT_HDRS

   END INTERFACE

   END MODULE WRITE_PARTNd_MAT_HDRS_Interface

