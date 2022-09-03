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

   MODULE ELMGM1_Interface

   INTERFACE

      SUBROUTINE ELMGM1 ( INT_ELEM_ID, WRITE_WARN )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MELGP, MOFFSET, NCORD, FATAL_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELMGM1_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  EPSIL
      USE MODEL_STUF, ONLY            :  BGRID, BUSH_CID, BUSH_OCID, CAN_ELEM_TYPE_OFFSET, CORD, EID, ELEM_LEN_12, ELEM_LEN_AB,    &
                                         ELGP, NUM_EMG_FATAL_ERRS, EOFF, GRID, OFFDIS, OFFDIS_B, RCORD, TE, TE_IDENT, TYPE, XEB, XEL
 
      IMPLICIT NONE
 
      CHARACTER( 1*BYTE)              :: ID(3)              ! Used in deciding whether TE_IDENT = 'Y' or 'N'
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN         ! If 'Y" write warning messages, otherwise do not

      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID        ! Internal element ID for which
      INTEGER(LONG)                   :: I3_IN(3)           ! Integer array used in sorting VX. 

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELMGM1_BEGEND
  
      REAL(DOUBLE)                    :: V13(3)             ! A vector from grid 1 to grid 3 (for BAR, BEAM or USER1 it is V vector)
  
      END SUBROUTINE ELMGM1

   END INTERFACE

   END MODULE ELMGM1_Interface

