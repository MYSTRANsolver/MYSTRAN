! ###############################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2019 Dr William R Case, Jr (dbcase29@gmail,com)                                              
                                                                                                         
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

   MODULE GET_ELEM_AGRID_BGRID_Interface

   INTERFACE

      SUBROUTINE GET_ELEM_AGRID_BGRID ( INT_ELEM_ID, CHECK_AGRID )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, medat0_cuserin, MELGP, NGRID
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_ELEM_AGRID_BGRID_BEGEND
      USE MODEL_STUF, ONLY            :  AGRID, BGRID, EDAT, EID, ELGP, EPNT, ETYPE, GRID, GRID_ID, TYPE

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: CHECK_AGRID       ! If 'Y' perform check on AGRID's to see if appropriate type

      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID       ! Internal element ID for which
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_ELEM_AGRID_BGRID_BEGEND

      END SUBROUTINE GET_ELEM_AGRID_BGRID

   END INTERFACE

   END MODULE GET_ELEM_AGRID_BGRID_Interface

