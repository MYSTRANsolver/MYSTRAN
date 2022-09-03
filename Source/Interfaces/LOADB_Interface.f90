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

   MODULE LOADB_Interface

   INTERFACE

      SUBROUTINE LOADB

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, IN1
      USE SCONTR, ONLY                :  BD_ENTRY_LEN, BLNK_SUB_NAM, ECHO, FATAL_ERR, IMB_BLANK, JF, LIND_GRDS_MPCS,               &
                                         LSUB, LLOADC, LMPCADDC, LSPCADDC, MDT, MTDAT_TEMPP1, MTDAT_TEMPRB,                        &
                                         MAX_GAUSS_POINTS, MAX_STRESS_POINTS,                                                      &
                                         MELGP, MELDOF, MMPC, MOFFSET, NBAROR, NBEAMOR, NFORCE,NGRAV, NGRDSET, NGRID, NLOAD, NMPC, &
                                         NMPCADD, NPCOMP, NRBAR, NRBE1, NRBE2, NRFORCE, NRSPLINE, NSLOAD, NSPOINT, NSPC, NSPC1,    &
                                         NSPCADD, NPBAR, NPBARL, NPLOAD, NSUB, NUM_MPCSIDS, NUM_PARTVEC_RECORDS, PROG_NAME,        &
                                         SOL_NAME, NCBAR, NCBEAM, NCBUSH, NCHEXA20, NCHEXA8, NCPENTA15, NCPENTA6, NCQUAD4,         &
                                         NCQUAD4K, NCROD, NCSHEAR, NCTETRA10, NCTETRA4, NCTRIA3, NCTRIA3K, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE PARAMS, ONLY                :  GRIDSEQ, IORQ1M, IORQ1S, IORQ1B, IORQ2B, IORQ2T, QUADAXIS, SUPINFO, SUPWARN
      USE OUTPUT4_MATRICES, ONLY      :  NUM_PARTN_REQUESTS
      USE SUBR_BEGEND_LEVELS, ONLY    :  LOADB_BEGEND 
      USE MODEL_STUF, ONLY            :  FORMOM_SIDS, GRAV_SIDS, IOR3D_MAX, LOAD_SIDS,                                             &
                                         MPCSET, MPC_SIDS, MPCSIDS, MPCADD_SIDS, PBAR, RPCOMP, PRESS_SIDS, RFORCE_SIDS,            &
                                         RPBAR, SLOAD_SIDS, SPC_SIDS, SPC1_SIDS, SPCADD_SIDS, SPCSET, CC_EIGR_SID, SCNUM, SUBLOD
 
      IMPLICIT NONE
 
      CHARACTER( 3*BYTE)              :: CONSTR_TYPE = '   '! Used for output error message (='SPC' or 'MPC')
      CHARACTER( 1*BYTE)              :: FOUND       = 'N'  ! Used to indicate if we found something we are looking for

      CHARACTER( 7*BYTE), PARAMETER   :: END_CARD    = 'ENDDATA'
 
      INTEGER(LONG)                   :: NG                 ! Actual num grids on CUSERIN (not incl SPOINT's)
      INTEGER(LONG)                   :: NS                 ! Actual num SPOINT'ss on CUSERIN
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = LOADB_BEGEND
 
      END SUBROUTINE LOADB

   END INTERFACE

   END MODULE LOADB_Interface

