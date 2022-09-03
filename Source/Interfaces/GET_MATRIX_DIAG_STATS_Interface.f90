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

   MODULE GET_MATRIX_DIAG_STATS_Interface

   INTERFACE

      SUBROUTINE GET_MATRIX_DIAG_STATS ( MAT_NAME, INPUT_SET, NROWS, NTERM, I_KIN, J_KIN, KIN, WRITE_WHAT, KIN_DIAG,               &

                                         MAX_OA_DIAG_TERM )

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NDOFG, NGRID
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  AUTOSPC_RAT, EPSIL
      USE DOF_TABLES, ONLY            :  TDOFI
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_MATRIX_DIAG_STATS_BEGEND

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN )   :: INPUT_SET         ! Char description of MYSTRAN displ set (e.g. 'A ' or 'SG')
      CHARACTER(LEN=*), INTENT(IN )   :: MAT_NAME          ! Name of the input matrix

      INTEGER(LONG), INTENT(IN)       :: NROWS             ! Number of rows in the input matrix
      INTEGER(LONG), INTENT(IN)       :: NTERM             ! Number of nonzero terms in the input matrix
      INTEGER(LONG), INTENT(IN)       :: I_KIN(NROWS+1)    ! Indices that are used to determine where in KIN the next column begins
      INTEGER(LONG), INTENT(IN)       :: J_KIN(NTERM)      ! Col numbers of terms in KIN
      INTEGER(LONG), INTENT(IN)       :: WRITE_WHAT        ! 1 write diagonal, 2 write summary stats, 3 write both
      INTEGER(LONG)                   :: AGRID_OLD         ! Actual grid number (used to add blank line bet grids when write diags)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_MATRIX_DIAG_STATS_BEGEND

      REAL(DOUBLE) , INTENT(IN)       :: KIN(NTERM)        ! Nonzero terms in the stiffness matrix
      REAL(DOUBLE) , INTENT(OUT)      :: KIN_DIAG(NROWS)   ! Diagonal terms from KIN
      REAL(DOUBLE) , INTENT(OUT)      :: MAX_OA_DIAG_TERM  ! Maximum diagonal term in the stiffness matrix for the COMPS_TO_CHECK

      END SUBROUTINE GET_MATRIX_DIAG_STATS

   END INTERFACE

   END MODULE GET_MATRIX_DIAG_STATS_Interface

