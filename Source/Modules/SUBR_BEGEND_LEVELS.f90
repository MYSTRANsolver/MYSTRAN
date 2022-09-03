! ##################################################################################################################################
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

      MODULE SUBR_BEGEND_LEVELS

! These variables are used in subrs to decide if the begin/end times of the subr are to be written to the F04 file

      USE PENTIUM_II_KIND, ONLY       :  LONG

      IMPLICIT NONE
 
      SAVE

! Values used in deciding whether subroutine begin/end times are printed in the log file. The file MYSTRAN.INI can have a value for
! variable PRBEGEND. If a subroutine has a value for the xxx_BEGEND, below, that is equal to, or less than, then the
! begin/end times for that subroutine will be printed in the log file. Note that the xxx_BEGEND parameters, below, all are in the
! format of where xxx is the subroutine name.

      INTEGER(LONG), PARAMETER, PRIVATE :: ALLOCATE_BEGEND              = 8
      INTEGER(LONG), PARAMETER, PRIVATE :: DATA_DECK_BEGEND             = 8
      INTEGER(LONG), PARAMETER, PRIVATE :: ELEM_BEGEND                  = 4
      INTEGER(LONG), PARAMETER, PRIVATE :: LINK_BEGEND                  = 1

      INTEGER(LONG), PARAMETER          :: MYSTRAN_FILES_BEGEND         = 0
      INTEGER(LONG), PARAMETER          :: PROCESS_INCLUDE_FILES_BEGEND = 0

      INTEGER(LONG), PARAMETER          :: ELMDAT_BEGEND                = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: ELMGM1_BEGEND                = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: ELMGM2_BEGEND                = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: ELMGM3_BEGEND                = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: EMG_BEGEND                   = ELEM_BEGEND 
      INTEGER(LONG), PARAMETER          :: GET_ELEM_AGRID_BGRID_BEGEND  = ELEM_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: GET_ELEM_ONAME_BEGEND        = ELEM_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: GET_ELGP_BEGEND              = ELEM_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: GET_PCOMP_SECT_PROPS_BEGEND  = ELEM_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: GET_MATANGLE_FROM_CID_BEGEND = ELEM_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: GRID_ELEM_CONN_TABLE_BEGEND  = LINK_BEGEND + 1 
      INTEGER(LONG), PARAMETER          :: ROT_COMP_ELEM_AXES_BEGEND    = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SHELL_ABD_MATRICES_BEGEND    = ELEM_BEGEND + 1

      INTEGER(LONG), PARAMETER          :: ELMOFF_BEGEND                = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: ELMOUT_BEGEND                = ELEM_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: ELMTLB_BEGEND                = ELEM_BEGEND + 2

      INTEGER(LONG), PARAMETER          :: BAR1_BEGEND                  = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: BEAM_BEGEND                  = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: BREL1_BEGEND                 = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: BUSH_BEGEND                  = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: ELAS1_BEGEND                 = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: KUSER1_BEGEND                = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: PINFLG_BEGEND                = ELEM_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: ROD1_BEGEND                  = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: USERIN_BEGEND                = ELEM_BEGEND + 1

      INTEGER(LONG), PARAMETER          :: CALC_PHI_SQ_BEGEND           = ELEM_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: GET_ELEM_NUM_PLIES_BEGEND    = ELEM_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: TMEM1_BEGEND                 = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: TPLT1_BEGEND                 = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: TPLT2_BEGEND                 = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: TREL1_BEGEND                 = ELEM_BEGEND + 1

      INTEGER(LONG), PARAMETER          :: QDEL1_BEGEND                 = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: QMEM1_BEGEND                 = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: QPLT1_BEGEND                 = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: QPLT2_BEGEND                 = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: QPLT3_BEGEND                 = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: QSHEAR_BEGEND                = ELEM_BEGEND + 1

      INTEGER(LONG), PARAMETER          :: HEXA_BEGEND                  = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: PENTA_BEGEND                 = ELEM_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: TETRA_BEGEND                 = ELEM_BEGEND + 1

      INTEGER(LONG), PARAMETER          :: BBDKQ_BEGEND                 = ELEM_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: BBMIN3_BEGEND                = ELEM_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: BBMIN4_BEGEND                = ELEM_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: BCHECK_BEGEND                = ELEM_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: B3D_ISOPARAMETRIC_BEGEND     = ELEM_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: BMQMEM_BEGEND                = ELEM_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: BSMIN3_BEGEND                = ELEM_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: BSMIN4_BEGEND                = ELEM_BEGEND + 3

      INTEGER(LONG), PARAMETER          :: MIN4SH_BEGEND                = ELEM_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: ORDER_BEGEND                 = ELEM_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: SHP_BEGEND                   = ELEM_BEGEND + 3

      INTEGER(LONG), PARAMETER          :: JACOBIAN_BEGEND              = ELEM_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: MATERIAL_PROPS_BEGEND        = ELEM_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: MATGET_BEGEND                = ELEM_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: MATPUT_BEGEND                = ELEM_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: MATL_TRANSFORM_MATRIX_BEGEND = ELEM_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: ROT_AXES_MATL_TO_LOC_BEGEND  = ELEM_BEGEND + 2

      INTEGER(LONG), PARAMETER          :: CSHIFT_BEGEND                = DATA_DECK_BEGEND+1
      INTEGER(LONG), PARAMETER          :: EC_DEBUG_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: EC_IN4FIL_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: EC_OUTPUT4_BEGEND            = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: EC_PARTN_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: ELEPRO_BEGEND                = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: FFIELD_BEGEND                = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: FFIELD2_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: LOADB_BEGEND                 = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: LOADB_RESTART_BEGEND         = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: LOADB0_BEGEND                = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: LOADC_BEGEND                 = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: LOADC0_BEGEND                = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: LOADE_BEGEND                 = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: LOADE0_BEGEND                = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REPLACE_TABS_W_BLANKS_BEGEND = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: READ_INCLUDE_FILNAM_BEGEND   = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: RW_INCLUDE_FILES_BEGEND      = LINK_BEGEND + 8

      INTEGER(LONG), PARAMETER          :: BD_ASET_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_ASET1_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_BAROR0_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_BAROR_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_BEAMOR0_BEGEND            = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_BEAMOR_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CBAR0_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CBAR_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CBUSH0_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CBUSH_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CELAS_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CHEXA0_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CHEXA_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CMASS_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CONM2_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CONROD_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CORD_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CPENTA0_BEGEND            = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CPENTA_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CQUAD0_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CQUAD_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CROD_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CSHEAR_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CTETRA0_BEGEND            = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CTETRA_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CTRIA0_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CTRIA_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CUSER1_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CUSERIN0_BEGEND           = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_CUSERIN_BEGEND            = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_DEBUG_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_EIG_BEGEND                = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_FORMOM_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_GRAV_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_GRDSET_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_GRDSET0_BEGEND            = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_GRID_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_IMBEDDED_BLANK_BEGEND     = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_LOAD_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_LOAD0_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_MATL_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_MPC0_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_MPC_BEGEND                = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_MPCADD_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_MPCADD0_BEGEND            = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_NLPARM_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PARAM_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PARAM0_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PARVEC_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PARVEC1_BEGEND            = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PBAR_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PBARL_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PBEAM_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PBush_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PCOMP_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PCOMP0_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PCOMP1_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PCOMP10_BEGEND            = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PELAS_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PLOAD2_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PLOAD4_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PLOTEL_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PMASS_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PROD_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PSHEAR_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PSHEL_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PSOLID_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PUSER1_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_PUSERIN_BEGEND            = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_RBAR_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_RBE1_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_RBE2_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_RBE30_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_RBE3_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_RSPLINE0_BEGEND           = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_RSPLINE_BEGEND            = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_RFORCE_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_SEQGP_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_SLOAD0_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_SLOAD_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_SPC_BEGEND                = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_SPC1_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_SPCADD0_BEGEND            = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_SPCADD_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_SPOINT0_BEGEND            = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_SPOINT_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_SUPORT_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_TEMP_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_TEMPD_BEGEND              = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_TEMPRP_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_USET_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: BD_USET1_BEGEND              = DATA_DECK_BEGEND

      INTEGER(LONG), PARAMETER          :: CC_ACCE_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_DISP_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_ECHO_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_ELDA_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_ELFO_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_ENFO_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_GPFO_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_LABE_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_LOAD_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_MEFM_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_MPF_BEGEND                = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_METH_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_MPC_BEGEND                = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_MPCF_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_NLPARM_BEGEND             = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_OLOA_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_OUTPUTS_BEGEND            = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_SET_BEGEND                = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_SET0_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_SPC_BEGEND                = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_SPCF_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_STRE_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_STRN_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_SUBC_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_SUBT_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_TEMP_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CC_TITL_BEGEND               = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CHK_CC_CMD_DESCRIBERS_BEGEND = DATA_DECK_BEGEND

      INTEGER(LONG), PARAMETER          :: CORD_PROC_BEGEND             = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: CORD1_PROC_BEGEND            = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: CORD2_PROC_BEGEND            = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: DOF_PROC_BEGEND              = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: GRID_PROC_BEGEND             = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: RDOF_BEGEND                  = LINK_BEGEND + 6
      INTEGER(LONG), PARAMETER          :: OU4_PARTVEC_PROC_BEGEND      = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SEQ_PROC_BEGEND              = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: TDOF_COL_NUM_BEGEND          = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: TSET_PROC_FOR_MPCS_BEGEND    = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: TSET_PROC_FOR_OMITS_BEGEND   = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: TSET_PROC_FOR_RIGELS_BEGEND  = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: TSET_PROC_FOR_SPCS_BEGEND    = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: USET_PROC_BEGEND             = LINK_BEGEND + 1

      INTEGER(LONG), PARAMETER          :: CONM2_PROC_1_BEGEND          = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: CONM2_PROC_2_BEGEND          = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: ELEM_PROP_MATL_IIDS_BEGEND   = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: ELEM_TRANSFORM_LBG_BEGEND    = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: ELESORT_BEGEND               = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: ELSAVE_BEGEND                = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: GPWG_BEGEND                  = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: GPWG_PMOI_BEGEND             = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: GPWG_USERIN_BEGEND           = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: RB_DISP_MATRIX_PROC_BEGEND   = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SUBCASE_PROC_BEGEND          = LINK_BEGEND + 1

      INTEGER(LONG), PARAMETER          :: EPTL_BEGEND                  = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: FORCE_MOM_PROC_BEGEND        = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: GET_GRID_6X6_MASS_BEGEND     = LINK_BEGEND + 5
      INTEGER(LONG), PARAMETER          :: GRAV_PROC_BEGEND             = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: MPC_PROC_BEGEND              = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: PRESSURE_DATA_PROC_BEGEND    = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: RFORCE_PROC_BEGEND           = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: RSPLINE_PROC_BEGEND          = LINK_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: RIGID_ELEM_PROC_BEGEND       = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SLOAD_PROC_BEGEND            = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: TEMPERATURE_DATA_PROC_BEGEND = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: YS_ARRAY_BEGEND              = LINK_BEGEND + 1

      INTEGER(LONG), PARAMETER          :: EMP_BEGEND                   = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: EMP0_BEGEND                  = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: ESP_BEGEND                   = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: ESP0_BEGEND                  = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: ESP0_FINAL_BEGEND            = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: KGG_SINGULARITY_PROC_BEGEND  = LINK_BEGEND + 5
      INTEGER(LONG), PARAMETER          :: MGGC_MASS_MATRIX_BEGEND      = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: MGGS_MASS_MATRIX_BEGEND      = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SPARSE_KGG_BEGEND            = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SPARSE_KGGD_BEGEND           = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SPARSE_MGG_BEGEND            = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SPARSE_PG_BEGEND             = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SPARSE_RMG_BEGEND            = LINK_BEGEND + 1

      INTEGER(LONG), PARAMETER          :: CHAR_FLD_BEGEND              = DATA_DECK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: CHECK_BAR_MOIs_BEGEND        = DATA_DECK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: CRDERR_BEGEND                = DATA_DECK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: GET_ANSID_BEGEND             = DATA_DECK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: GET_SETID_BEGEND             = DATA_DECK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: I4FLD_BEGEND                 = DATA_DECK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: IP6CHK_BEGEND                = DATA_DECK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: LEFT_ADJ_BDFLD_BEGEND        = DATA_DECK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: MKCARD_BEGEND                = DATA_DECK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: MKJCARD_BEGEND               = DATA_DECK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: NEXTC_BEGEND                 = DATA_DECK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: NEXTC0_BEGEND                = DATA_DECK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: NEXTC2_BEGEND                = DATA_DECK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: NEXTC20_BEGEND               = DATA_DECK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: R4FLD_BEGEND                 = DATA_DECK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: R8FLD_BEGEND                 = DATA_DECK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: STOKEN_BEGEND                = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: TOKCHK_BEGEND                = DATA_DECK_BEGEND + 1

      INTEGER(LONG), PARAMETER          :: ALLOCATE_EMS_ARRAYS_BEGEND   = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_L1_MGG_BEGEND       = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_STF_ARRAYS_BEGEND   = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_TEMPLATE_BEGEND     = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_L1_MGG_BEGEND     = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_EMS_ARRAYS_BEGEND = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_STF_ARRAYS_BEGEND = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_TEMPLATE_BEGEND   = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: LINK1_BEGEND                 = LINK_BEGEND
      INTEGER(LONG), PARAMETER          :: LINK1_RESTART_DATA_BEGEND    = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: PRINT_CONSTANTS_1_BEGEND     = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: WRITE_ENF_TO_L1O_BEGEND      = LINK_BEGEND + 1

      INTEGER(LONG), PARAMETER          :: ALLOCATE_L2_GMN_2_BEGEND     = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_L2_GOA_2_BEGEND     = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_L2_GMN_2_BEGEND   = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_L2_GOA_2_BEGEND   = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: LINK2_BEGEND                 = LINK_BEGEND
      INTEGER(LONG), PARAMETER          :: REDUCE_G_NM_BEGEND           = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_N_FS_BEGEND           = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_F_AO_BEGEND           = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_A_LR_BEGEND           = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_KGG_TO_KNN_BEGEND     = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_KNN_TO_KFF_BEGEND     = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_KFF_TO_KAA_BEGEND     = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_KAA_TO_KLL_BEGEND     = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_KGGD_TO_KNND_BEGEND   = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_KNND_TO_KFFD_BEGEND   = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_KFFD_TO_KAAD_BEGEND   = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_KAAD_TO_KLLD_BEGEND   = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_MGG_TO_MNN_BEGEND     = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_MNN_TO_MFF_BEGEND     = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_MFF_TO_MAA_BEGEND     = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_MAA_TO_MLL_BEGEND     = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_PG_TO_PN_BEGEND       = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_PN_TO_PF_BEGEND       = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_PF_TO_PA_BEGEND       = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: REDUCE_PA_TO_PL_BEGEND       = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SOLVE_GMN_BEGEND             = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SOLVE_GOA_BEGEND             = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SOLVE_UO0_BEGEND             = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: STIFF_MAT_EQUIL_CHK_BEGEND   = LINK_BEGEND + 1

      INTEGER(LONG), PARAMETER          :: EPSCALC_BEGEND               = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: LINK3_BEGEND                 = LINK_BEGEND
      INTEGER(LONG), PARAMETER          :: REFINE_SOL_BEGEND            = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: VECINORM_BEGEND              = LINK_BEGEND + 1

      INTEGER(LONG), PARAMETER          :: CALC_GEN_MASS_BEGEND         = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: EIG_GIV_MGIV_BEGEND          = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: EIG_INV_PWR_BEGEND           = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: EIG_LANCZOS_ARPACK_BEGEND    = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: EIG_SUMMARY_BEGEND           = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: INVERT_EIGENS_BEGEND         = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: LINK4_BEGEND                 = LINK_BEGEND
      INTEGER(LONG), PARAMETER          :: RENORM_ON_MASS_BEGEND        = LINK_BEGEND + 1

      INTEGER(LONG), PARAMETER          :: BUILD_A_LR_BEGEND            = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: BUILD_F_AO_BEGEND            = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: BUILD_G_NM_BEGEND            = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: BUILD_N_FS_BEGEND            = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: EXPAND_PHIXA_TO_PHIXG_BEGEND = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: LINK5_BEGEND                 = LINK_BEGEND
      INTEGER(LONG), PARAMETER          :: RENORM_BEGEND                = LINK_BEGEND + 2

      INTEGER(LONG), PARAMETER          :: ALLOCATE_L6_2_BEGEND         = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: CALC_CB_MEFM_MPF_BEGEND      = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: CALC_KRRcb_BEGEND            = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: CALC_MRN_BEGEND              = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: CALC_MRRcb_BEGEND            = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: CALC_PHIZL_BEGEND            = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_L6_2_BEGEND       = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: INTERFACE_FORCE_LTM_BEGEND   = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: LINK6_BEGEND                 = LINK_BEGEND
      INTEGER(LONG), PARAMETER          :: MERGE_KXX_BEGEND             = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: MERGE_LTM_BEGEND             = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: MERGE_MXX_BEGEND             = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: MERGE_PHIXA_BEGEND           = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: NET_CG_LOADS_LTM_BEGEND      = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SOLVE_DLR_BEGEND             = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SOLVE_PHIZL1_BEGEND          = LINK_BEGEND + 1

      INTEGER(LONG), PARAMETER          :: BAR_MARGIN_BEGEND            = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: GET_MAX_MIN_ABS_STR_BEGEND   = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: PRINCIPAL_2D_BEGEND          = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: PRINCIPAL_3D_BEGEND          = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: ROD_MARGIN_BEGEND            = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: WRITE_FEMAP_ELFO_VECS_BEGEND = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: WRITE_FEMAP_GRID_VECS_BEGEND = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: WRITE_FEMAP_STRE_VECS_BEGEND = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: WRITE_FEMAP_STRN_VECS_BEGEND = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: WRITE_MEFFMASS_BEGEND        = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: WRITE_MPFACTOR_BEGEND        = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: WRITE_PLY_STRAINS_BEGEND     = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: WRITE_PLY_STRESSES_BEGEND    = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: WRITE_BAR_BEGEND             = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: WRITE_ELEM_ENGR_FORCE_BEGEND = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: WRITE_ELEM_NODE_FORCE_BEGEND = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: WRITE_ELEM_STRESSES_BEGEND   = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: WRITE_ELEM_STRAINS_BEGEND    = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: WRITE_GRD_PCH_OUTPUTS_BEGEND = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: WRITE_GRD_PRT_OUTPUTS_BEGEND = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: WRITE_ROD_BEGEND             = LINK_BEGEND + ELEM_BEGEND

      INTEGER(LONG), PARAMETER          :: CALC_ELEM_NODE_FORCES_BEGEND = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: CALC_ELEM_STRESSES_BEGEND    = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: CALC_ELEM_STRAINS_BEGEND     = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: ELEM_STRE_STRN_ARRAYS_BEGEND = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: POLYNOM_FIT_STRE_STRN_BEGEND = LINK_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: SHELL_ENGR_FORCE_OGEL_BEGEND = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SHELL_STRAIN_OUTPUTS_BEGEND  = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SHELL_STRESS_OUTPUTS_BEGEND  = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SOLID_STRAIN_OUTPUTS_BEGEND  = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SOLID_STRESS_OUTPUTS_BEGEND  = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: ELMDIS_BEGEND                = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: ELMDIS_PLY_BEGEND            = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: GET_COMP_SHELL_ALLOWS_BEGEND = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: GP_FORCE_BALANCE_PROC_BEGEND = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: INDEP_FAILURE_INDEX_BEGEND   = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: OFP1_BEGEND                  = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: OFP2_BEGEND                  = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: OFP3_BEGEND                  = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: OFP3_ELFN_BEGEND             = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: OFP3_ELFE_1D_BEGEND          = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: OFP3_ELFE_2D_BEGEND          = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: OFP3_STRE_NO_PCOMP_BEGEND    = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: OFP3_STRE_PCOMP_BEGEND       = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: OFP3_STRN_NO_PCOMP_BEGEND    = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: OFP3_STRN_PCOMP_BEGEND       = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: ONE_D_STRAIN_OUTPUTS_BEGEND  = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: ONE_D_STRESS_OUTPUTS_BEGEND  = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: POLY_FAILURE_INDEX_BEGEND    = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SUSER1_BEGEND                = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: STR_TENSOR_TRANSFORM_BEGEND  = LINK_BEGEND + ELEM_BEGEND
      INTEGER(LONG), PARAMETER          :: TRANSFORM_NODE_FORCES_BEGEND = LINK_BEGEND + ELEM_BEGEND

      INTEGER(LONG), PARAMETER          :: ALLOCATE_FEMAP_DATA_BEGEND   = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_LINK9_STUF_BEGEND   = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_FEMAP_DATA_BEGEND = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_LINK9_STUF_BEGEND = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: LINK9_BEGEND                 = LINK_BEGEND
      INTEGER(LONG), PARAMETER          :: LINK9S_BEGEND                = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: MAXREQ_OGEL_BEGEND           = LINK_BEGEND + 1

      INTEGER(LONG), PARAMETER          :: ALLOCATE_CB_ELM_OTM_BEGEND   = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_CB_GRD_OTM_BEGEND   = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_COL_VEC_BEGEND      = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_DOF_TABLES_BEGEND   = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_EIGEN1_MAT_BEGEND   = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_FULL_MAT_BEGEND     = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_IN4_FILES_BEGEND    = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_LAPACK_MAT_BEGEND   = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_MISC_MAT_BEGEND     = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_MODEL_STUF_BEGEND   = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_NL_PARAMS_BEGEND    = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_RBGLOBAL_BEGEND     = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_SCR_CCS_MAT_BEGEND  = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_SCR_CRS_MAT_BEGEND  = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_SPARSE_ALG_BEGEND   = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ALLOCATE_SPARSE_MAT_BEGEND   = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: ARRAY_SIZE_ERROR_1_BEGEND    = LINK_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: BANDGEN_BEGEND               = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: BANDSIZ_BEGEND               = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: CALC_TDOF_ROW_START_BEGEND   = LINK_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: CALC_VEC_SORT_ORDER_BEGEND   = LINK_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: CARD_FLDS_NOT_BLANK_BEGEND   = DATA_DECK_BEGEND
      INTEGER(LONG), PARAMETER          :: CHECK_MAT_INVERSE_BEGEND     = LINK_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: CLOSE_LIJFILES_BEGEND        = LINK_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: CLOSE_OUTFILES_BEGEND        = LINK_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: CNT_NONZ_IN_FULL_MAT_BEGEND  = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: COND_NUM_BEGEND              = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: CONVERT_INT_TO_CHAR_BEGEND   = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: CONVERT_VEC_COORD_SYS_BEGEND = LINK_BEGEND + 4 
      INTEGER(LONG), PARAMETER          :: CROSS_BEGEND                 = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: CRS_NONSYM_TO_CRS_SYM_BEGEND = LINK_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: CRS_SYM_TO_CRS_NONSYM_BEGEND = LINK_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: DATA_SET_NAME_ERROR_BEGEND   = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: DATA_SET_SIZE_ERROR_BEGEND   = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_CB_ELM_OTM_BEGEND = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_CB_GRD_OTM_BEGEND = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_COL_VEC_BEGEND    = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_DOF_TABLES_BEGEND = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_EIGEN1_MAT_BEGEND = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_IN4_FILES_BEGEND  = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_FULL_MAT_BEGEND   = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_LAPACK_MAT_BEGEND = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_MISC_MAT_BEGEND   = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_MODEL_STUF_BEGEND = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_NL_PARAMS_BEGEND  = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_RBGLOBAL_BEGEND   = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_SCR_MAT_BEGEND    = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_SPARSE_ALG_BEGEND = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: DEALLOCATE_SPARSE_MAT_BEGEND = ALLOCATE_BEGEND
      INTEGER(LONG), PARAMETER          :: EQUILIBRATE_BEGEND           = LINK_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: FBS_LAPACK_BEGEND            = LINK_BEGEND
      INTEGER(LONG), PARAMETER          :: FBS_SUPRLU_BEGEND            = LINK_BEGEND
      INTEGER(LONG), PARAMETER          :: FILE_INQUIRE_BEGEND          = LINK_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: FILE_OPEN_BEGEND             = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: FILERR_BEGEND                = LINK_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: FULL_TO_SPARSE_CRS_BEGEND    = LINK_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: GET_COMMAND_LINE_BEGEND      = LINK_BEGEND
      INTEGER(LONG), PARAMETER          :: GEN_T0L_BEGEND               = LINK_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: GET_ARRAY_ROW_NUM_BEGEND     = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: GET_CHAR_STRING_END_BEGEND   = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: GET_FORMATTED_INTEGER_BEGEND = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: GET_GRID_AND_COMP_BEGEND     = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: GET_GRID_NUM_COMPS_BEGEND    = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: GET_I_MAT_FROM_I2_MAT_BEGEND = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: GET_I2_MAT_FROM_I_MAT_BEGEND = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: GET_MACHINE_PARAMS_BEGEND    = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: GET_MATRIX_DIAG_STATS_BEGEND = LINK_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: GET_MAX_NUM_ELM_GRIDS_BEGEND = LINK_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: GET_SPARSE_CRS_COL_BEGEND    = LINK_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: GET_SPARSE_CRS_ROW_BEGEND    = LINK_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: GET_SPARSE_MAT_TERM_BEGEND   = LINK_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: GET_UG_123_IN_GRD_ORD_BEGEND = LINK_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: GET_VEC_MIN_MAX_ABS_BEGEND   = LINK_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: INVERT_FF_MAT_BEGEND         = LINK_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: MATADD_FFF_BEGEND            = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: MATADD_SSS_BEGEND            = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: MATADD_SSS_NTERM_BEGEND      = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: MATMULT_FFF_BEGEND           = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: MATMULT_FFF_T_BEGEND         = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: MATMULT_SFF_BEGEND           = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: MATMULT_SFS_BEGEND           = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: MATMULT_SFS_NTERM_BEGEND     = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: MATMULT_SSS_BEGEND           = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: MATMULT_SSS_NTERM_BEGEND     = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: MATRIX_VECTOR_OP_BEGEND      = LINK_BEGEND + 2
      INTEGER(LONG), PARAMETER          :: MATTRNSP_SS_BEGEND           = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: MAX_MEMORY_AVAILABLE_BEGEND  = LINK_BEGEND
      INTEGER(LONG), PARAMETER          :: MERGE_COL_VECS_BEGEND        = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: MERGE_MAT_COLS_SSS_BEGEND    = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: MERGE_MAT_ROWS_SSS_BEGEND    = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: OPEN_OUTFILES_BEGEND         = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: OPNERR_BEGEND                = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: OURDAT_BEGEND                = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: OURTIM_BEGEND                = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: OUTA_HERE_BEGEND             = LINK_BEGEND
      INTEGER(LONG), PARAMETER          :: OUTPUT4_PROC_BEGEND          = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: PARAM_CORDS_ACT_CORDS_BEGEND = LINK_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: PARSE_CHAR_STRING_BEGEND     = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: PARTITION_FF_BEGEND          = LINK_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: PARTITION_SS_BEGEND          = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: PARTITION_SS_NTERM_BEGEND    = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: PARTITION_VEC_BEGEND         = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: PLANE_COORD_TRANS_21_BEGEND  = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: PROJ_VEC_ONTO_PLANE_BEGEND   = LINK_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: PRT_MATS_ON_RESTART_BEGEND   = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: READ_CL_BEGEND               = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: RESTART_DATA_FOR_L3_BEGEND   = LINK_BEGEND + 1 
      INTEGER(LONG), PARAMETER          :: RESTART_DATA_FOR_L5_BEGEND   = LINK_BEGEND + 1 
      INTEGER(LONG), PARAMETER          :: READ_DOF_TABLES_BEGEND       = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: READ_IN4_FULL_MAT_BEGEND     = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: READ_L1A_BEGEND              = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: READ_L1M_BEGEND              = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: READ_L1Z_BEGEND              = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: READ_MATRIX_1_BEGEND         = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: READ_MATRIX_2_BEGEND         = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: READ_XTIME_BEGEND            = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: READERR_BEGEND               = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: RIGID_BODY_DISP_MAT_BEGEND   = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: ROW_AT_COLJ_BEGEND_BEGEND    = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: SORT_GRID_RGRID_BEGEND       = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: SORT_INT1_BEGEND             = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: SORT_INT1_REAL1_BEGEND       = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: SORT_INT1_REAL3_BEGEND       = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: SORT_INT2_BEGEND             = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: SORT_INT2_REAL1_BEGEND       = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: SORT_INT3_BEGEND             = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: SORT_INT3_CHAR2_BEGEND       = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: SORT_REAL1_INT1_BEGEND       = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: SORT_TDOF_BEGEND             = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: SORTLEN_BEGEND               = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: SPARSE_CRS_SPARSE_CCS_BEGEND = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: SPARSE_CRS_TERM_COUNT_BEGEND = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: SPARSE_CRS_TO_FULL_BEGEND    = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: SPARSE_MAT_DIAG_ZEROS_BEGEND = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: SPARSE_NONSYM_TO_SYM_BEGEND  = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: STMERR_BEGEND                = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: SURFACE_FIT_BEGEND           = LINK_BEGEND + 8
      INTEGER(LONG), PARAMETER          :: SYM_MAT_DECOMP_LAPACK_BEGEND = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: SYM_MAT_DECOMP_SUPRLU_BEGEND = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: TIME_INIT_BEGEND             = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: VECTOR_NORM_BEGEND           = LINK_BEGEND + 3
      INTEGER(LONG), PARAMETER          :: WRITE_ALLOC_MEM_TABLE_BEGEND = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_DOF_TABLES_BEGEND      = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_EDAT_BEGEND            = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_ELM_OT4_BEGEND         = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_FIJFIL_BEGEND          = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_FILNAM_BEGEND          = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_GRD_OT4_BEGEND         = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_GRID_COORDS_BEGEND     = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_INTEGER_VEC_BEGEND     = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_L1A_BEGEND             = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_L1M_BEGEND             = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_L1Z_BEGEND             = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_MATRIX_1_BEGEND        = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_MATRIX_BY_COLS_BEGEND  = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_MATRIX_BY_ROWS_BEGEND  = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_MEM_SUM_TO_F04_BEGEND  = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_OU4_FULL_MAT_BEGEND    = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_OU4_SPARSE_MAT_BEGEND  = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_PARTNd_MAT_HDRS_BEGEND = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_SPARSE_CRS_BEGEND      = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_TDOF_BEGEND            = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_TSET_BEGEND            = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_USERIN_BD_CARDS_BEGEND = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_USET_BEGEND            = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_USETSTR_BEGEND         = LINK_BEGEND + 10
      INTEGER(LONG), PARAMETER          :: WRITE_VECTOR_BEGEND          = LINK_BEGEND + 10

      INTEGER(LONG), PARAMETER          :: ARPACK_BEGEND                = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: BANDIT_BEGEND                = LINK_BEGEND + 1
      INTEGER(LONG), PARAMETER          :: LAPACK_BEGEND                = LINK_BEGEND + 1

      END MODULE SUBR_BEGEND_LEVELS
