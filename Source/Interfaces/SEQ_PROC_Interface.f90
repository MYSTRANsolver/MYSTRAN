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

   MODULE SEQ_PROC_Interface

   INTERFACE

      SUBROUTINE SEQ_PROC

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR,     F04,     F06,     SEQ,     L1B
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, SEQFIL
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, SEQSTAT
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, DATA_NAM_LEN, FATAL_ERR, NGRID, NSEQ, PROG_NAME, WARN_ERR
      USE PARAMS, ONLY                :  EPSIL, GRIDSEQ
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPINFO, SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  SEQ_PROC_BEGEND
      USE MODEL_STUF, ONLY            :  GRID_ID, GRID_SEQ, INV_GRID_SEQ, SEQ1, SEQ2
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      IMPLICIT NONE

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = SEQ_PROC_BEGEND
 
      END SUBROUTINE SEQ_PROC

   END INTERFACE

   END MODULE SEQ_PROC_Interface

