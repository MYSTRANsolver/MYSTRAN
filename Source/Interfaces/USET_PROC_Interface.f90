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

   MODULE USET_PROC_Interface

   INTERFACE

      SUBROUTINE USET_PROC

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06, L1X, L1X_MSG, LINK1X
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ENFORCED, FATAL_ERR, NGRID, NUM_USET_RECORDS, NUM_USET_U1, NUM_USET_U2
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  USET_PROC_BEGEND
      USE PARAMS, ONLY                :  EPSIL
      USE DOF_TABLES, ONLY            :  TSET_CHR_LEN, USET
      USE MODEL_STUF, ONLY            :  GRID, GRID_ID
 
      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'USET_PROC'
 
      INTEGER(LONG)                   :: USET_ERR   = 0    ! Count of errors that result from setting displ sets in USET
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = USET_PROC_BEGEND
 
      END SUBROUTINE USET_PROC

   END INTERFACE

   END MODULE USET_PROC_Interface

