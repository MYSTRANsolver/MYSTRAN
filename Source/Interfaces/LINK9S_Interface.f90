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

   MODULE LINK9S_Interface

   INTERFACE

      SUBROUTINE LINK9S


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE

      USE IOUNT1, ONLY                :  FILE_NAM_MAXLEN, WRT_ERR, WRT_LOG, ERR, F04, F06,                                         &
                                         L1D    , L1G    , L1K    , L1Q    ,                                                       &
                                         LINK1D , LINK1G , LINK1K , LINK1Q ,                                                       &
                                         L1D_MSG, L1G_MSG, L1K_MSG, L1Q_MSG,                                                       &
                                         L1DSTAT, L1GSTAT, L1KSTAT, L1QSTAT

      USE SCONTR, ONLY                :  BLNK_SUB_NAM, DATA_NAM_LEN, MPCOMP0, MRPCOMP0, MPCOMP_PLIES,                              &
                                         MRPCOMP_PLIES, MRMATLC, MPBAR, MRPBAR, MPBEAM, MRPBEAM, MPBUSH, MRPBUSH, MPELAS, MRPELAS, &
                                         MPLOAD4_3D_DATA, MPROD, MRPROD, MPSHEAR, MRPSHEAR, MPSHEL, MRPSHEL, MPUSER1, MRPUSER1,    &
                                         MPUSERIN, MUSERIN_MAT_NAMES, MMATL, MPSOLID, NEDAT, NBAROFF, NBUSHOFF, NELE, NGRID,       &
                                         NMATANGLE, NMATL, NPBAR, NPBEAM, NPBUSH, NPCOMP, NPCARD, NPDAT, NPELAS, NPROD, NPSHEAR,   &
                                         NPSHEL, NPSOLID, NPLATEOFF, NPLATETHICK, NPLOAD4_3D, NPUSER1, NPUSERIN, NSEQ, NSUB,       &
                                         NTCARD, NTDAT, NTSUB, NVVEC, SOL_NAME

      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  CBMIN3, CBMIN4, IORQ1M, IORQ1S, IORQ1B, IORQ2B, IORQ2T
      USE SUBR_BEGEND_LEVELS, ONLY    :  LINK9S_BEGEND

      USE MODEL_STUF, ONLY            :  BAROFF, BUSHOFF, EDAT, EOFF, EPNT, ESORT1, ESORT2, ETYPE, PLATEOFF, PLATETHICK, VVEC
      USE MODEL_STUF, ONLY            :  MATANGLE, MATL, RMATL, PBAR, RPBAR, PBEAM, RPBEAM, PBUSH, RPBUSH, PCOMP, RPCOMP, PELAS,   &
                                         RPELAS, PROD, RPROD, PSHEAR, RPSHEAR, PSHEL, PSOLID, RPSHEL, PUSER1, RPUSER1, PUSERIN,    &
                                         USERIN_MAT_NAMES
      USE MODEL_STUF, ONLY            :  ELDT, ELOUT, GROUT, OELDT, OELOUT, OGROUT, SCNUM, SUBLOD, TITLE, STITLE, LABEL
      USE MODEL_STUF, ONLY            :  GTEMP, TDATA, TPNT
      USE MODEL_STUF, ONLY            :  PDATA, PPNT, PLOAD4_3D_DATA, PTYPE
      USE MODEL_STUF, ONLY            :  ANY_ACCE_OUTPUT, ANY_DISP_OUTPUT, ANY_MPCF_OUTPUT, ANY_SPCF_OUTPUT, ANY_OLOA_OUTPUT,      &
                                         ANY_GPFO_OUTPUT, ANY_ELFE_OUTPUT, ANY_ELFN_OUTPUT, ANY_STRE_OUTPUT
      IMPLICIT NONE

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = LINK9S_BEGEND

      END SUBROUTINE LINK9S

   END INTERFACE

   END MODULE LINK9S_Interface

