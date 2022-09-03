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

   MODULE OFP2_Interface

   INTERFACE

      SUBROUTINE OFP2 ( JVEC, WHAT, SC_OUT_REQ, ZERO_GEN_STIFF, FEMAP_SET_ID, ITG, OT4_GROW, ITABLE, NEW_RESULT )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, OT4

      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, GROUT_SPCF_BIT, GROUT_MPCF_BIT, GROUT_GPFO_BIT, IBIT, INT_SC_NUM,&
                                         MELGP, MOGEL, NGRID, NDOFF, NDOFG, NDOFM, NDOFN, NDOFS, NDOFSA, NTERM_GMN,                &
                                         NTERM_HMN, NTERM_KFS, NTERM_KFSD, NTERM_LMN, NTERM_MFS, NTERM_QS, SOL_NAME

      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  OFP2_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START, TDOFI
      USE EIGEN_MATRICES_1, ONLY      :  EIGEN_VAL, GEN_MASS, MEFFMASS, MPFACTOR_N6
      USE MODEL_STUF, ONLY            :  ANY_SPCF_OUTPUT, ANY_MPCF_OUTPUT, GRID, GRID_ID, GROUT, MEFFMASS_CALC, MPFACTOR_CALC
      USE PARAMS, ONLY                :  AUTOSPC_SPCF, EPSIL, MEFMCORD, OTMSKIP, POST

      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE SPARSE_MATRICES, ONLY       :  I_GMN  , J_GMN  , GMN    , I_GMNt  , J_GMNt , GMNt   , I_HMN, J_HMN, HMN,                 &
                                         I_KSF  , J_KSF  , KSF    , I_KSFD  , J_KSFD , KSFD   ,                                    &
                                         I_LMN  , J_LMN  , LMN    , I_MSF   , J_MSF  , MSF    ,                                    &
                                         SYM_GMN, SYM_HMN, SYM_KFS, SYM_KFSD, SYM_MFS, SYM_LMN

      USE LINK9_STUFF, ONLY           :  GID_OUT_ARRAY, MAXREQ, OGEL
      USE COL_VECS, ONLY              :  UF_COL, UG_COL, UN_COL, PHIXG_COL, PHIXN_COL, PM_COL, PS_COL,                             &
                                         QGm_COL, QGs_COL, QM_COL, QN_COL, QS_COL, QSYS_COL
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE OUTPUT4_MATRICES, ONLY      :  OTM_MPCF, OTM_SPCF, TXT_MPCF, TXT_SPCF
      USE CC_OUTPUT_DESCRIBERS, ONLY  :  MPCF_OUT, SPCF_OUT

      IMPLICIT NONE

      CHARACTER(LEN=*) , INTENT(IN)   :: WHAT              ! Indicator of whether to process output requests for SPC or MPC forces
      CHARACTER(LEN=*) , INTENT(IN)   :: ZERO_GEN_STIFF    ! Indicator of whether there are zero gen stiffs (can't calc MEFFMASS)
      CHARACTER( 1*BYTE), PARAMETER   :: IHDR      = 'Y'   ! An input to subr WRITE_GRD_PRT_OUTPUTS, called herein
      CHARACTER(1*BYTE)               :: WRITE_OGEL(NGRID) ! 'Y'/'N' as to whether to write OGEL for a grid (used to avoid writing
      INTEGER(LONG), INTENT(IN)       :: FEMAP_SET_ID      ! Set ID for FEMAP output
      INTEGER(LONG), INTENT(IN)       :: ITG               ! Unit number for text files for OTM row descriptors 
      INTEGER(LONG), INTENT(IN)       :: JVEC              ! Solution vector number
      INTEGER(LONG), INTENT(IN)       :: SC_OUT_REQ        ! If > 0, then req1uests for WHAT are to be output
      INTEGER(LONG), INTENT(INOUT)    :: OT4_GROW          ! Row number in OT4 file for grid related OTM descriptors
      INTEGER(LONG), INTENT(INOUT)    :: ITABLE            ! 
      LOGICAL, INTENT(INOUT)          :: NEW_RESULT        ! is this the first result of a table
      INTEGER(LONG)                   :: NREQ              ! Number of user requested outputs of displ/force
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = OFP2_BEGEND

      END SUBROUTINE OFP2

   END INTERFACE

   END MODULE OFP2_Interface

