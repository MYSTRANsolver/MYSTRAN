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

   MODULE BD_CUSERIN_Interface

   INTERFACE

      SUBROUTINE BD_CUSERIN ( CARD, LARGE_FLD_INP, NG, NS )

  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, IERRFL, JCARD_LEN, JF, LGUSERIN, LSUSERIN, MEDAT0_CUSERIN,       &
                                         NCUSERIN, NEDAT, NELE, WARN_ERR 
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_CUSERIN_BEGEND
      USE MODEL_STUF, ONLY            :  EDAT, ETYPE
 
      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'BD_CUSERIN'
      CHARACTER(LEN=*), INTENT(INOUT) :: CARD              ! A Bulk Data card
      CHARACTER(LEN=*), INTENT(IN)    :: LARGE_FLD_INP     ! If 'Y', CARD is large field format
 
      INTEGER(LONG), INTENT(OUT)      :: NG                ! Number of GRID's for the elem as defined on parent card field 5
      INTEGER(LONG), INTENT(OUT)      :: NS                ! Number of SPOINT's for the elem as defined on parent card field 5

                                                           ! Array of grids on the CUSERIN entry (not incl SPOINT's)
      INTEGER(LONG)                   :: USERIN_GRIDS(LGUSERIN)

                                                           ! Array of SPOINT's on the CUSERIN entry
      INTEGER(LONG)                   :: USERIN_SPOINTS(LSUSERIN)

                                                           ! Array of displ components on the CUSERIN entry (for USERIN_GRIDS)
      INTEGER(LONG)                   :: USERIN_COMPS(LGUSERIN)

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_CUSERIN_BEGEND
 
      END SUBROUTINE BD_CUSERIN

   END INTERFACE

   END MODULE BD_CUSERIN_Interface

