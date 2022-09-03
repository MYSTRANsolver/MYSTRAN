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

   MODULE ELSAVE_Interface

   INTERFACE

      SUBROUTINE ELSAVE

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR,     F04,     F06,     L1G
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, DATA_NAM_LEN, MMATL, MPBAR, MPBEAM, MPBUSH, MPELAS, MPROD, MPSHEL,          &
                                         MPSOLID, MPUSER1,MPUSERIN, MRMATLC, MRPBAR, MRPBEAM, MRPBUSH, MRPELAS, MRPROD, MPSHEAR,   &
                                         MRPSHEAR, MRPSHEL, MRPUSER1, NBAROFF, NBUSHOFF, NEDAT, NELE, NMATANGLE, NMATL, MPCOMP0,   &
                                         MRPCOMP0, MPCOMP_PLIES, MRPCOMP_PLIES, MUSERIN_MAT_NAMES, NPBAR, NPBEAM, NPBUSH, NPCOMP,  &
                                         NPELAS, NPLATEOFF, NPLATETHICK, NPROD, NPSHEAR, NPSHEL, NPSOLID, NPUSER1, NPUSERIN, NVVEC
      USE PARAMS, ONLY                :  CBMIN3, CBMIN4, IORQ1M, IORQ1S, IORQ1B, IORQ2B, IORQ2T 
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELSAVE_BEGEND
      USE MODEL_STUF, ONLY            :  BAROFF, BUSHOFF, EDAT, EOFF, EPNT, ESORT1, ESORT2, ETYPE, MATANGLE, MATL, RMATL,PBAR,     &
                                         RPBAR, PBEAM, RPBEAM, PBUSH, RPBUSH, PCOMP, RPCOMP, PELAS, RPELAS, PROD, RPROD, PSHEAR,   &
                                         RPSHEAR, PSHEL, RPSHEL, PSOLID, PUSER1, RPUSER1, PUSERIN, PLATEOFF, PLATETHICK,           &
                                         USERIN_MAT_NAMES, VVEC
      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELSAVE_BEGEND
 
      END SUBROUTINE ELSAVE

   END INTERFACE

   END MODULE ELSAVE_Interface

