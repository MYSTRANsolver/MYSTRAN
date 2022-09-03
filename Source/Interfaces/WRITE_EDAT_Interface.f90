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

   MODULE WRITE_EDAT_Interface

   INTERFACE

      SUBROUTINE WRITE_EDAT


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  ERR, F04, F06, WRT_LOG

      USE SCONTR, ONLY                :  BLNK_SUB_NAM  , LGUSERIN      , LSUSERIN      , NELE          , NCUSERIN      , WARN_ERR, &
                                         MEDAT_CBAR    , MEDAT_CBEAM   , MEDAT_CBUSH   , MEDAT_CELAS1  , MEDAT_CELAS2  ,           &
                                         MEDAT_CELAS3  , MEDAT_CELAS4  , MEDAT_CHEXA8  , MEDAT_CHEXA20 , MEDAT_CPENTA6 ,           &
                                         MEDAT_CPENTA15, MEDAT_PLOTEL  , MEDAT_CQUAD   , MEDAT_CROD    ,                           &
                                         MEDAT_CSHEAR  , MEDAT_CTETRA4 , MEDAT_CTETRA10, MEDAT_CTRIA   ,                           &
                                         MEDAT_CUSER1  , MEDAT0_CUSERIN, METYPE
              
      USE TIMDAT, ONLY                :  TSEC
      USE MODEL_STUF, ONLY            :  EDAT, EPNT, ETYPE
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_EDAT_BEGEND

      IMPLICIT NONE

      CHARACTER(16*BYTE)              :: EDAT_DESCR(0:MAX(2*LGUSERIN+LSUSERIN+6,25),METYPE)

      INTEGER(LONG)                   :: NG                ! Number of grids defined on a CUSERIN entry
      INTEGER(LONG)                   :: NS                ! Number of scalar points defined on a CUSERIN entry
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_EDAT_BEGEND

      END SUBROUTINE WRITE_EDAT

   END INTERFACE

   END MODULE WRITE_EDAT_Interface

