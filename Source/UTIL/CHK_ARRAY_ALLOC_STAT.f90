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

      SUBROUTINE CHK_ARRAY_ALLOC_STAT

! Checks allocation status of all allocatable arrays. User has to include a Bulk Data DEBUG entry (see module DEBUG_PARAMS for
! which DEBUG to use)

      USE SCONTR, ONLY                :  LINKNO
      USE IOUNT1, ONLY                :  F06, IN4FIL, IN4FIL_NUM
      USE ARPACK_MATRICES_1
      USE COL_VECS
      USE DOF_TABLES
      USE EIGEN_MATRICES_1
      USE EMS_ARRAYS
      USE FEMAP_ARRAYS
      USE FULL_MATRICES
      USE LAPACK_DPB_MATRICES
      USE LINK9_STUFF
      USE MODEL_STUF
      USE NONLINEAR_PARAMS
      USE INPUTT4_MATRICES
      USE RIGID_BODY_DISP_MATS
      USE SCRATCH_MATRICES
      USE SPARSE_ALG_ARRAYS
      USE SPARSE_MATRICES
      USE STF_ARRAYS
      USE STF_TEMPLATE_ARRAYS
      USE OUTPUT4_MATRICES

      USE CHK_ARRAY_ALLOC_STAT_USE_IFs                     ! Added 2019/07/14

      IMPLICIT NONE

      INTEGER(LONG), PARAMETER        :: NUM_NAMES = 585   ! Number of arrays to check

      CHARACTER(31*BYTE)              :: NAME(NUM_NAMES)   ! Array name

      INTEGER(LONG)                   :: I                 ! DO loop index
      INTEGER(LONG)                   :: IA(NUM_NAMES)     ! Indicator of whether array is allocated

! **********************************************************************************************************************************

      DO I=1,NUM_NAMES
         NAME(I)(1:) = ' '
         IA(I)       = 0
      ENDDO

      IF (ALLOCATED( ABAND                          )) THEN;  NAME(  1) = 'ABAND                          ';  IA(  1) = 1;   ENDIF
      IF (ALLOCATED( AGRID                          )) THEN;  NAME(  2) = 'AGRID                          ';  IA(  2) = 1;   ENDIF
      IF (ALLOCATED( ALG                            )) THEN;  NAME(  3) = 'ALG                            ';  IA(  3) = 1;   ENDIF
      IF (ALLOCATED( ALL_SETS_ARRAY                 )) THEN;  NAME(  4) = 'ALL_SETS_ARRAY                 ';  IA(  4) = 1;   ENDIF
      IF (ALLOCATED( AROW                           )) THEN;  NAME(  5) = 'AROW                           ';  IA(  5) = 1;   ENDIF
      IF (ALLOCATED( BAROFF                         )) THEN;  NAME(  6) = 'BAROFF                         ';  IA(  6) = 1;   ENDIF
      IF (ALLOCATED( BUSHOFF                        )) THEN;  NAME(  7) = 'BUSHOFF                        ';  IA(  7) = 1;   ENDIF
      IF (ALLOCATED( BBAND                          )) THEN;  NAME(  8) = 'BBAND                          ';  IA(  8) = 1;   ENDIF
      IF (ALLOCATED( BE1                            )) THEN;  NAME(  9) = 'BE1                            ';  IA(  9) = 1;   ENDIF
      IF (ALLOCATED( BE2                            )) THEN;  NAME( 10) = 'BE2                            ';  IA( 10) = 1;   ENDIF
      IF (ALLOCATED( BE3                            )) THEN;  NAME( 11) = 'BE3                            ';  IA( 11) = 1;   ENDIF
      IF (ALLOCATED( BGRID                          )) THEN;  NAME( 12) = 'BGRID                          ';  IA( 12) = 1;   ENDIF
      IF (ALLOCATED( CCS1                           )) THEN;  NAME( 13) = 'CCS1                           ';  IA( 13) = 1;   ENDIF
      IF (ALLOCATED( CCS2                           )) THEN;  NAME( 14) = 'CCS2                           ';  IA( 14) = 1;   ENDIF
      IF (ALLOCATED( CCS3                           )) THEN;  NAME( 15) = 'CCS3                           ';  IA( 15) = 1;   ENDIF
      IF (ALLOCATED( CETEMP                         )) THEN;  NAME( 16) = 'CETEMP                         ';  IA( 16) = 1;   ENDIF
      IF (ALLOCATED( CG_LTM                         )) THEN;  NAME( 17) = 'CG_LTM                         ';  IA( 17) = 1;   ENDIF
      IF (ALLOCATED( CGTEMP                         )) THEN;  NAME( 18) = 'CGTEMP                         ';  IA( 18) = 1;   ENDIF
      IF (ALLOCATED( CMASS                          )) THEN;  NAME( 19) = 'CMASS                          ';  IA( 19) = 1;   ENDIF
      IF (ALLOCATED( CONM2                          )) THEN;  NAME( 20) = 'CONM2                          ';  IA( 20) = 1;   ENDIF
      IF (ALLOCATED( CORD                           )) THEN;  NAME( 21) = 'CORD                           ';  IA( 21) = 1;   ENDIF
      IF (ALLOCATED( CROW                           )) THEN;  NAME( 22) = 'CROW                           ';  IA( 22) = 1;   ENDIF
      IF (ALLOCATED( CRS1                           )) THEN;  NAME( 23) = 'CRS1                           ';  IA( 23) = 1;   ENDIF
      IF (ALLOCATED( CRS2                           )) THEN;  NAME( 24) = 'CRS2                           ';  IA( 24) = 1;   ENDIF
      IF (ALLOCATED( CRS3                           )) THEN;  NAME( 25) = 'CRS3                           ';  IA( 25) = 1;   ENDIF
      IF (ALLOCATED( DLR                            )) THEN;  NAME( 26) = 'DLR                            ';  IA( 26) = 1;   ENDIF
      IF (ALLOCATED( DLRt                           )) THEN;  NAME( 27) = 'DLRt                           ';  IA( 27) = 1;   ENDIF
      IF (ALLOCATED( DOFPIN                         )) THEN;  NAME( 28) = 'DOFPIN                         ';  IA( 28) = 1;   ENDIF
      IF (ALLOCATED( DT                             )) THEN;  NAME( 29) = 'DT                             ';  IA( 29) = 1;   ENDIF
      IF (ALLOCATED( DUM1                           )) THEN;  NAME( 30) = 'DUM1                           ';  IA( 30) = 1;   ENDIF
      IF (ALLOCATED( DUM2                           )) THEN;  NAME( 31) = 'DUM2                           ';  IA( 31) = 1;   ENDIF
      IF (ALLOCATED( DUM3                           )) THEN;  NAME( 32) = 'DUM3                           ';  IA( 32) = 1;   ENDIF
      IF (ALLOCATED( EDAT                           )) THEN;  NAME( 33) = 'EDAT                           ';  IA( 33) = 1;   ENDIF
      IF (ALLOCATED( EID_OUT_ARRAY                  )) THEN;  NAME( 34) = 'EID_OUT_ARRAY                  ';  IA( 34) = 1;   ENDIF
      IF (ALLOCATED( EIGEN_VAL                      )) THEN;  NAME( 35) = 'EIGEN_VAL                      ';  IA( 35) = 1;   ENDIF
      IF (ALLOCATED( EIGEN_VEC                      )) THEN;  NAME( 36) = 'EIGEN_VEC                      ';  IA( 36) = 1;   ENDIF
      IF (ALLOCATED( ELDT                           )) THEN;  NAME( 37) = 'ELDT                           ';  IA( 37) = 1;   ENDIF
      IF (ALLOCATED( ELOUT                          )) THEN;  NAME( 38) = 'ELOUT                          ';  IA( 38) = 1;   ENDIF
      IF (ALLOCATED( EMS                            )) THEN;  NAME( 39) = 'EMS                            ';  IA( 39) = 1;   ENDIF
      IF (ALLOCATED( EMSCOL                         )) THEN;  NAME( 40) = 'EMSCOL                         ';  IA( 40) = 1;   ENDIF
      IF (ALLOCATED( EMSKEY                         )) THEN;  NAME( 41) = 'EMSKEY                         ';  IA( 41) = 1;   ENDIF
      IF (ALLOCATED( EMSPNT                         )) THEN;  NAME( 42) = 'EMSPNT                         ';  IA( 42) = 1;   ENDIF
      IF (ALLOCATED( EOFF                           )) THEN;  NAME( 43) = 'EOFF                           ';  IA( 43) = 1;   ENDIF
      IF (ALLOCATED( EPNT                           )) THEN;  NAME( 44) = 'EPNT                           ';  IA( 44) = 1;   ENDIF
      IF (ALLOCATED( ESORT1                         )) THEN;  NAME( 45) = 'ESORT1                         ';  IA( 45) = 1;   ENDIF
      IF (ALLOCATED( ESORT2                         )) THEN;  NAME( 46) = 'ESORT2                         ';  IA( 46) = 1;   ENDIF
      IF (ALLOCATED( ETEMP                          )) THEN;  NAME( 47) = 'ETEMP                          ';  IA( 47) = 1;   ENDIF
      IF (ALLOCATED( ETYPE                          )) THEN;  NAME( 48) = 'ETYPE                          ';  IA( 48) = 1;   ENDIF
      IF (ALLOCATED( FA_COL                         )) THEN;  NAME( 49) = 'FA_COL                         ';  IA( 49) = 1;   ENDIF
      IF (ALLOCATED( FEMAP_EL_NUMS                  )) THEN;  NAME( 50) = 'FEMAP_EL_NUMS                  ';  IA( 50) = 1;   ENDIF
      IF (ALLOCATED( FEMAP_EL_VECS                  )) THEN;  NAME( 51) = 'FEMAP_EL_VECS                  ';  IA( 51) = 1;   ENDIF
      IF (ALLOCATED( FF_COL                         )) THEN;  NAME( 52) = 'FF_COL                         ';  IA( 52) = 1;   ENDIF
      IF (ALLOCATED( FG_COL                         )) THEN;  NAME( 53) = 'FG_COL                         ';  IA( 53) = 1;   ENDIF
      IF (ALLOCATED( FL_COL                         )) THEN;  NAME( 54) = 'FL_COL                         ';  IA( 54) = 1;   ENDIF
      IF (ALLOCATED( FM_COL                         )) THEN;  NAME( 55) = 'FM_COL                         ';  IA( 55) = 1;   ENDIF
      IF (ALLOCATED( FN_COL                         )) THEN;  NAME( 56) = 'FN_COL                         ';  IA( 56) = 1;   ENDIF
      IF (ALLOCATED( FO_COL                         )) THEN;  NAME( 57) = 'FO_COL                         ';  IA( 57) = 1;   ENDIF
      IF (ALLOCATED( FORMOM_SIDS                    )) THEN;  NAME( 58) = 'FORMOM_SIDS                    ';  IA( 58) = 1;   ENDIF
      IF (ALLOCATED( FR_COL                         )) THEN;  NAME( 59) = 'FR_COL                         ';  IA( 59) = 1;   ENDIF
      IF (ALLOCATED( FS_COL                         )) THEN;  NAME( 60) = 'FS_COL                         ';  IA( 60) = 1;   ENDIF
      IF (ALLOCATED( FTNAME                         )) THEN;  NAME( 61) = 'FTNAME                         ';  IA( 61) = 1;   ENDIF
      IF (ALLOCATED( GEN_MASS                       )) THEN;  NAME( 62) = 'GEN_MASS                       ';  IA( 62) = 1;   ENDIF
      IF (ALLOCATED( GID_OUT_ARRAY                  )) THEN;  NAME( 63) = 'GID_OUT_ARRAY                  ';  IA( 63) = 1;   ENDIF
      IF (ALLOCATED( GMN                            )) THEN;  NAME( 64) = 'GMN                            ';  IA( 64) = 1;   ENDIF
      IF (ALLOCATED( GMN_FULL                       )) THEN;  NAME( 65) = 'GMN_FULL                       ';  IA( 65) = 1;   ENDIF
      IF (ALLOCATED( GMNt                           )) THEN;  NAME( 66) = 'GMNt                           ';  IA( 66) = 1;   ENDIF
      IF (ALLOCATED( GMNt_FULL                      )) THEN;  NAME( 67) = 'GMNt_FULL                      ';  IA( 67) = 1;   ENDIF
      IF (ALLOCATED( GOA                            )) THEN;  NAME( 68) = 'GOA                            ';  IA( 68) = 1;   ENDIF
      IF (ALLOCATED( GOA_FULL                       )) THEN;  NAME( 69) = 'GOA_FULL                       ';  IA( 69) = 1;   ENDIF
      IF (ALLOCATED( GOAt                           )) THEN;  NAME( 70) = 'GOAt                           ';  IA( 70) = 1;   ENDIF
      IF (ALLOCATED( GOAt_FULL                      )) THEN;  NAME( 71) = 'GOAt_FULL                      ';  IA( 71) = 1;   ENDIF
      IF (ALLOCATED( GRAV_SIDS                      )) THEN;  NAME( 72) = 'GRAV_SIDS                      ';  IA( 72) = 1;   ENDIF
      IF (ALLOCATED( GRID                           )) THEN;  NAME( 73) = 'GRID                           ';  IA( 73) = 1;   ENDIF
      IF (ALLOCATED( GRID_ELEM_CONN_ARRAY           )) THEN;  NAME( 74) = 'GRID_ELEM_CONN_ARRAY           ';  IA( 74) = 1;   ENDIF
      IF (ALLOCATED( GRID_ID                        )) THEN;  NAME( 75) = 'GRID_ID                        ';  IA( 75) = 1;   ENDIF
      IF (ALLOCATED( GRID_SEQ                       )) THEN;  NAME( 76) = 'GRID_SEQ                       ';  IA( 76) = 1;   ENDIF
      IF (ALLOCATED( GROUT                          )) THEN;  NAME( 77) = 'GROUT                          ';  IA( 77) = 1;   ENDIF
      IF (ALLOCATED( GTEMP                          )) THEN;  NAME( 78) = 'GTEMP                          ';  IA( 78) = 1;   ENDIF
      IF (ALLOCATED( HMN                            )) THEN;  NAME( 79) = 'HMN                            ';  IA( 79) = 1;   ENDIF
      IF (ALLOCATED( HMN_FULL                       )) THEN;  NAME( 80) = 'HMN_FULL                       ';  IA( 80) = 1;   ENDIF
      IF (ALLOCATED( I_CCS1                         )) THEN;  NAME( 81) = 'I_CCS1                         ';  IA( 81) = 1;   ENDIF
      IF (ALLOCATED( I_CCS2                         )) THEN;  NAME( 82) = 'I_CCS2                         ';  IA( 82) = 1;   ENDIF
      IF (ALLOCATED( I_CCS3                         )) THEN;  NAME( 83) = 'I_CCS3                         ';  IA( 83) = 1;   ENDIF
      IF (ALLOCATED( I_CG_LTM                       )) THEN;  NAME( 84) = 'I_CG_LTM                       ';  IA( 84) = 1;   ENDIF
      IF (ALLOCATED( I_CRS1                         )) THEN;  NAME( 85) = 'I_CRS1                         ';  IA( 85) = 1;   ENDIF
      IF (ALLOCATED( I_CRS2                         )) THEN;  NAME( 86) = 'I_CRS2                         ';  IA( 86) = 1;   ENDIF
      IF (ALLOCATED( I_CRS3                         )) THEN;  NAME( 87) = 'I_CRS3                         ';  IA( 87) = 1;   ENDIF
      IF (ALLOCATED( I_DLR                          )) THEN;  NAME( 88) = 'I_DLR                          ';  IA( 88) = 1;   ENDIF
      IF (ALLOCATED( I_DLRt                         )) THEN;  NAME( 89) = 'I_DLRt                         ';  IA( 89) = 1;   ENDIF
      IF (ALLOCATED( I_GMN                          )) THEN;  NAME( 90) = 'I_GMN                          ';  IA( 90) = 1;   ENDIF
      IF (ALLOCATED( I_GMNt                         )) THEN;  NAME( 91) = 'I_GMNt                         ';  IA( 91) = 1;   ENDIF
      IF (ALLOCATED( I_GOA                          )) THEN;  NAME( 92) = 'I_GOA                          ';  IA( 92) = 1;   ENDIF
      IF (ALLOCATED( I_GOAt                         )) THEN;  NAME( 93) = 'I_GOAt                         ';  IA( 93) = 1;   ENDIF
      IF (ALLOCATED( I_HMN                          )) THEN;  NAME( 94) = 'I_HMN                          ';  IA( 94) = 1;   ENDIF
      IF (ALLOCATED( I_IF_LTM                       )) THEN;  NAME( 95) = 'I_IF_LTM                       ';  IA( 95) = 1;   ENDIF
      IF (ALLOCATED( I_IRR                          )) THEN;  NAME( 96) = 'I_IRR                          ';  IA( 96) = 1;   ENDIF
      IF (ALLOCATED( I_KAA                          )) THEN;  NAME( 97) = 'I_KAA                          ';  IA( 97) = 1;   ENDIF
      IF (ALLOCATED( I_KAO                          )) THEN;  NAME( 98) = 'I_KAO                          ';  IA( 98) = 1;   ENDIF
      IF (ALLOCATED( I_KFF                          )) THEN;  NAME( 99) = 'I_KFF                          ';  IA( 99) = 1;   ENDIF
      IF (ALLOCATED( I_KFS                          )) THEN;  NAME(100) = 'I_KFS                          ';  IA(100) = 1;   ENDIF
      IF (ALLOCATED( I_KFSe                         )) THEN;  NAME(101) = 'I_KFSe                         ';  IA(101) = 1;   ENDIF
      IF (ALLOCATED( I_KGG                          )) THEN;  NAME(102) = 'I_KGG                          ';  IA(102) = 1;   ENDIF
      IF (ALLOCATED( I_KLL                          )) THEN;  NAME(103) = 'I_KLL                          ';  IA(103) = 1;   ENDIF
      IF (ALLOCATED( I_KLLs                         )) THEN;  NAME(104) = 'I_KLLs                         ';  IA(104) = 1;   ENDIF
      IF (ALLOCATED( I_KMM                          )) THEN;  NAME(105) = 'I_KMM                          ';  IA(105) = 1;   ENDIF
      IF (ALLOCATED( I_KMN                          )) THEN;  NAME(106) = 'I_KMN                          ';  IA(106) = 1;   ENDIF
      IF (ALLOCATED( I_KMSM                         )) THEN;  NAME(107) = 'I_KMSM                         ';  IA(107) = 1;   ENDIF
      IF (ALLOCATED( I_KMSMn                        )) THEN;  NAME(108) = 'I_KMSMn                        ';  IA(108) = 1;   ENDIF
      IF (ALLOCATED( I_KMSMs                        )) THEN;  NAME(109) = 'I_KMSMs                        ';  IA(109) = 1;   ENDIF
      IF (ALLOCATED( I_KNM                          )) THEN;  NAME(110) = 'I_KNM                          ';  IA(110) = 1;   ENDIF
      IF (ALLOCATED( I_KNN                          )) THEN;  NAME(111) = 'I_KNN                          ';  IA(111) = 1;   ENDIF
      IF (ALLOCATED( I_KOO                          )) THEN;  NAME(112) = 'I_KOO                          ';  IA(112) = 1;   ENDIF
      IF (ALLOCATED( I_KOOs                         )) THEN;  NAME(113) = 'I_KOOs                         ';  IA(113) = 1;   ENDIF
      IF (ALLOCATED( I_KRL                          )) THEN;  NAME(114) = 'I_KRL                          ';  IA(114) = 1;   ENDIF
      IF (ALLOCATED( I_KRR                          )) THEN;  NAME(115) = 'I_KRR                          ';  IA(115) = 1;   ENDIF
      IF (ALLOCATED( I_KRRcb                        )) THEN;  NAME(116) = 'I_KRRcb                        ';  IA(116) = 1;   ENDIF
      IF (ALLOCATED( I_KRRcbn                       )) THEN;  NAME(117) = 'I_KRRcbn                       ';  IA(117) = 1;   ENDIF
      IF (ALLOCATED( I_KRRcbs                       )) THEN;  NAME(118) = 'I_KRRcbs                       ';  IA(118) = 1;   ENDIF
      IF (ALLOCATED( I_KSF                          )) THEN;  NAME(119) = 'I_KSF                          ';  IA(119) = 1;   ENDIF
      IF (ALLOCATED( I_KSFD                         )) THEN;  NAME(120) = 'I_KSFD                         ';  IA(120) = 1;   ENDIF
      IF (ALLOCATED( I_KSS                          )) THEN;  NAME(121) = 'I_KSS                          ';  IA(121) = 1;   ENDIF
      IF (ALLOCATED( I_KSSe                         )) THEN;  NAME(122) = 'I_KSSe                         ';  IA(122) = 1;   ENDIF
      IF (ALLOCATED( I_KXX                          )) THEN;  NAME(123) = 'I_KXX                          ';  IA(123) = 1;   ENDIF
      IF (ALLOCATED( I_LMN                          )) THEN;  NAME(124) = 'I_LMN                          ';  IA(124) = 1;   ENDIF
      IF (ALLOCATED( I_LTM                          )) THEN;  NAME(125) = 'I_LTM                          ';  IA(125) = 1;   ENDIF
      IF (ALLOCATED( I_MAA                          )) THEN;  NAME(126) = 'I_MAA                          ';  IA(126) = 1;   ENDIF
      IF (ALLOCATED( I_MAO                          )) THEN;  NAME(127) = 'I_MAO                          ';  IA(127) = 1;   ENDIF
      IF (ALLOCATED( I_MFF                          )) THEN;  NAME(128) = 'I_MFF                          ';  IA(128) = 1;   ENDIF
      IF (ALLOCATED( I_MFS                          )) THEN;  NAME(129) = 'I_MFS                          ';  IA(129) = 1;   ENDIF
      IF (ALLOCATED( I_MGG                          )) THEN;  NAME(130) = 'I_MGG                          ';  IA(130) = 1;   ENDIF
      IF (ALLOCATED( I_MGGC                         )) THEN;  NAME(131) = 'I_MGGC                         ';  IA(131) = 1;   ENDIF
      IF (ALLOCATED( I_MGGE                         )) THEN;  NAME(132) = 'I_MGGE                         ';  IA(132) = 1;   ENDIF
      IF (ALLOCATED( I_MGGS                         )) THEN;  NAME(133) = 'I_MGGS                         ';  IA(133) = 1;   ENDIF
      IF (ALLOCATED( I_MLL                          )) THEN;  NAME(134) = 'I_MLL                          ';  IA(134) = 1;   ENDIF
      IF (ALLOCATED( I_MLLn                         )) THEN;  NAME(135) = 'I_MLLn                         ';  IA(135) = 1;   ENDIF
      IF (ALLOCATED( I_MLLs                         )) THEN;  NAME(136) = 'I_MLLs                         ';  IA(136) = 1;   ENDIF
      IF (ALLOCATED( I_MLR                          )) THEN;  NAME(137) = 'I_MLR                          ';  IA(137) = 1;   ENDIF
      IF (ALLOCATED( I_MMM                          )) THEN;  NAME(138) = 'I_MMM                          ';  IA(138) = 1;   ENDIF
      IF (ALLOCATED( I_MMN                          )) THEN;  NAME(139) = 'I_MMN                          ';  IA(139) = 1;   ENDIF
      IF (ALLOCATED( I_MNM                          )) THEN;  NAME(140) = 'I_MNM                          ';  IA(140) = 1;   ENDIF
      IF (ALLOCATED( I_MNN                          )) THEN;  NAME(141) = 'I_MNN                          ';  IA(141) = 1;   ENDIF
      IF (ALLOCATED( I_MOO                          )) THEN;  NAME(142) = 'I_MOO                          ';  IA(142) = 1;   ENDIF
      IF (ALLOCATED( I_MPF0                         )) THEN;  NAME(143) = 'I_MPF0                         ';  IA(143) = 1;   ENDIF
      IF (ALLOCATED( I_MRL                          )) THEN;  NAME(144) = 'I_MRL                          ';  IA(144) = 1;   ENDIF
      IF (ALLOCATED( I_MRN                          )) THEN;  NAME(145) = 'I_MRN                          ';  IA(145) = 1;   ENDIF
      IF (ALLOCATED( I_MRR                          )) THEN;  NAME(146) = 'I_MRR                          ';  IA(146) = 1;   ENDIF
      IF (ALLOCATED( I_MRRcb                        )) THEN;  NAME(147) = 'I_MRRcb                        ';  IA(147) = 1;   ENDIF
      IF (ALLOCATED( I_MRRcbn                       )) THEN;  NAME(148) = 'I_MRRcbn                       ';  IA(148) = 1;   ENDIF
      IF (ALLOCATED( I_MSF                          )) THEN;  NAME(149) = 'I_MSF                          ';  IA(149) = 1;   ENDIF
      IF (ALLOCATED( I_MSS                          )) THEN;  NAME(150) = 'I_MSS                          ';  IA(150) = 1;   ENDIF
      IF (ALLOCATED( I_MXX                          )) THEN;  NAME(151) = 'I_MXX                          ';  IA(151) = 1;   ENDIF
      IF (ALLOCATED( I_MXXn                         )) THEN;  NAME(152) = 'I_MXXn                         ';  IA(152) = 1;   ENDIF
      IF (ALLOCATED( I_PA                           )) THEN;  NAME(153) = 'I_PA                           ';  IA(153) = 1;   ENDIF
      IF (ALLOCATED( I_PF                           )) THEN;  NAME(154) = 'I_PF                           ';  IA(154) = 1;   ENDIF
      IF (ALLOCATED( I_PF_TMP                       )) THEN;  NAME(155) = 'I_PF_TMP                       ';  IA(155) = 1;   ENDIF
      IF (ALLOCATED( I_PFYS                         )) THEN;  NAME(156) = 'I_PFYS                         ';  IA(156) = 1;   ENDIF
      IF (ALLOCATED( I_PFYS1                        )) THEN;  NAME(157) = 'I_PFYS1                        ';  IA(157) = 1;   ENDIF
      IF (ALLOCATED( I_PG                           )) THEN;  NAME(158) = 'I_PG                           ';  IA(158) = 1;   ENDIF
      IF (ALLOCATED( I_PHIXA                        )) THEN;  NAME(159) = 'I_PHIXA                        ';  IA(159) = 1;   ENDIF
      IF (ALLOCATED( I_PHIZL                        )) THEN;  NAME(160) = 'I_PHIZL                        ';  IA(160) = 1;   ENDIF
      IF (ALLOCATED( I_PHIZL1                       )) THEN;  NAME(161) = 'I_PHIZL1                       ';  IA(161) = 1;   ENDIF
      IF (ALLOCATED( I_PHIZL1t                      )) THEN;  NAME(162) = 'I_PHIZL1t                      ';  IA(162) = 1;   ENDIF
      IF (ALLOCATED( I_PHIZL2                       )) THEN;  NAME(163) = 'I_PHIZL2                       ';  IA(163) = 1;   ENDIF
      IF (ALLOCATED( I_PL                           )) THEN;  NAME(164) = 'I_PL                           ';  IA(164) = 1;   ENDIF
      IF (ALLOCATED( I_PM                           )) THEN;  NAME(165) = 'I_PM                           ';  IA(165) = 1;   ENDIF
      IF (ALLOCATED( I_PN                           )) THEN;  NAME(166) = 'I_PN                           ';  IA(166) = 1;   ENDIF
      IF (ALLOCATED( I_PO                           )) THEN;  NAME(167) = 'I_PO                           ';  IA(167) = 1;   ENDIF
      IF (ALLOCATED( I_PR                           )) THEN;  NAME(168) = 'I_PR                           ';  IA(168) = 1;   ENDIF
      IF (ALLOCATED( I_PS                           )) THEN;  NAME(169) = 'I_PS                           ';  IA(169) = 1;   ENDIF
      IF (ALLOCATED( I_QSYS                         )) THEN;  NAME(170) = 'I_QSYS                         ';  IA(170) = 1;   ENDIF
      IF (ALLOCATED( I_RMG                          )) THEN;  NAME(171) = 'I_RMG                          ';  IA(171) = 1;   ENDIF
      IF (ALLOCATED( I_RMM                          )) THEN;  NAME(172) = 'I_RMM                          ';  IA(172) = 1;   ENDIF
      IF (ALLOCATED( I_RMN                          )) THEN;  NAME(173) = 'I_RMN                          ';  IA(173) = 1;   ENDIF
      IF (ALLOCATED( I2_DLR                         )) THEN;  NAME(174) = 'I2_DLR                         ';  IA(174) = 1;   ENDIF
      IF (ALLOCATED( I2_DLRt                        )) THEN;  NAME(175) = 'I2_DLRt                        ';  IA(175) = 1;   ENDIF
      IF (ALLOCATED( I2_GMN                         )) THEN;  NAME(176) = 'I2_GMN                         ';  IA(176) = 1;   ENDIF
      IF (ALLOCATED( I2_GOA                         )) THEN;  NAME(177) = 'I2_GOA                         ';  IA(177) = 1;   ENDIF
      IF (ALLOCATED( I2_KLL                         )) THEN;  NAME(178) = 'I2_KLL                         ';  IA(178) = 1;   ENDIF
      IF (ALLOCATED( I2_KLLs                        )) THEN;  NAME(179) = 'I2_KLLs                        ';  IA(179) = 1;   ENDIF
      IF (ALLOCATED( I2_KMSM                        )) THEN;  NAME(180) = 'I2_KMSM                        ';  IA(180) = 1;   ENDIF
      IF (ALLOCATED( I2_KMSMs                       )) THEN;  NAME(181) = 'I2_KMSMs                       ';  IA(181) = 1;   ENDIF
      IF (ALLOCATED( I2_KOO                         )) THEN;  NAME(182) = 'I2_KOO                         ';  IA(182) = 1;   ENDIF
      IF (ALLOCATED( I2_KOOs                        )) THEN;  NAME(183) = 'I2_KOOs                        ';  IA(183) = 1;   ENDIF
      IF (ALLOCATED( I2_MGG                         )) THEN;  NAME(184) = 'I2_MGG                         ';  IA(184) = 1;   ENDIF
      IF (ALLOCATED( I2_MLL                         )) THEN;  NAME(185) = 'I2_MLL                         ';  IA(185) = 1;   ENDIF
      IF (ALLOCATED( I2_MLLs                        )) THEN;  NAME(186) = 'I2_MLLs                        ';  IA(186) = 1;   ENDIF
      IF (ALLOCATED( I2_PHIZL1                      )) THEN;  NAME(187) = 'I2_PHIZL1                      ';  IA(187) = 1;   ENDIF
      IF (ALLOCATED( I2_PHIZL1t                     )) THEN;  NAME(188) = 'I2_PHIZL1t                     ';  IA(188) = 1;   ENDIF
      IF (ALLOCATED( IF_LTM                         )) THEN;  NAME(189) = 'IF_LTM                         ';  IA(189) = 1;   ENDIF
      IF (ALLOCATED( IN4_COL_MAP                    )) THEN;  NAME(190) = 'IN4_COL_MAP                    ';  IA(190) = 1;   ENDIF
      IF (ALLOCATED( IN4_MAT                        )) THEN;  NAME(191) = 'IN4_MAT                        ';  IA(191) = 1;   ENDIF
      IF (ALLOCATED( IN4FIL                         )) THEN;  NAME(192) = 'IN4FIL                         ';  IA(192) = 1;   ENDIF
      IF (ALLOCATED( IN4FIL_NUM                     )) THEN;  NAME(193) = 'IN4FIL_NUM                     ';  IA(193) = 1;   ENDIF
      IF (ALLOCATED( INV_GRID_SEQ                   )) THEN;  NAME(194) = 'INV_GRID_SEQ                   ';  IA(194) = 1;   ENDIF
      IF (ALLOCATED( IRR                            )) THEN;  NAME(195) = 'IRR                            ';  IA(195) = 1;   ENDIF
      IF (ALLOCATED( IWORK                          )) THEN;  NAME(196) = 'IWORK                          ';  IA(196) = 1;   ENDIF
      IF (ALLOCATED( J_AROW                         )) THEN;  NAME(197) = 'J_AROW                         ';  IA(197) = 1;   ENDIF
      IF (ALLOCATED( J_CCS1                         )) THEN;  NAME(198) = 'J_CCS1                         ';  IA(198) = 1;   ENDIF
      IF (ALLOCATED( J_CCS2                         )) THEN;  NAME(199) = 'J_CCS2                         ';  IA(199) = 1;   ENDIF
      IF (ALLOCATED( J_CCS3                         )) THEN;  NAME(200) = 'J_CCS3                         ';  IA(200) = 1;   ENDIF
      IF (ALLOCATED( J_CG_LTM                       )) THEN;  NAME(201) = 'J_CG_LTM                       ';  IA(201) = 1;   ENDIF
      IF (ALLOCATED( J_CRS1                         )) THEN;  NAME(202) = 'J_CRS1                         ';  IA(202) = 1;   ENDIF
      IF (ALLOCATED( J_CRS2                         )) THEN;  NAME(203) = 'J_CRS2                         ';  IA(203) = 1;   ENDIF
      IF (ALLOCATED( J_CRS3                         )) THEN;  NAME(204) = 'J_CRS3                         ';  IA(204) = 1;   ENDIF
      IF (ALLOCATED( J_DLR                          )) THEN;  NAME(205) = 'J_DLR                          ';  IA(205) = 1;   ENDIF
      IF (ALLOCATED( J_DLRt                         )) THEN;  NAME(206) = 'J_DLRt                         ';  IA(206) = 1;   ENDIF
      IF (ALLOCATED( J_GMN                          )) THEN;  NAME(207) = 'J_GMN                          ';  IA(207) = 1;   ENDIF
      IF (ALLOCATED( J_GMNt                         )) THEN;  NAME(208) = 'J_GMNt                         ';  IA(208) = 1;   ENDIF
      IF (ALLOCATED( J_GOA                          )) THEN;  NAME(209) = 'J_GOA                          ';  IA(209) = 1;   ENDIF
      IF (ALLOCATED( J_GOAt                         )) THEN;  NAME(210) = 'J_GOAt                         ';  IA(210) = 1;   ENDIF
      IF (ALLOCATED( J_HMN                          )) THEN;  NAME(211) = 'J_HMN                          ';  IA(211) = 1;   ENDIF
      IF (ALLOCATED( J_IF_LTM                       )) THEN;  NAME(212) = 'J_IF_LTM                       ';  IA(212) = 1;   ENDIF
      IF (ALLOCATED( J_IRR                          )) THEN;  NAME(213) = 'J_IRR                          ';  IA(213) = 1;   ENDIF
      IF (ALLOCATED( J_KAA                          )) THEN;  NAME(214) = 'J_KAA                          ';  IA(214) = 1;   ENDIF
      IF (ALLOCATED( J_KAO                          )) THEN;  NAME(215) = 'J_KAO                          ';  IA(215) = 1;   ENDIF
      IF (ALLOCATED( J_KFF                          )) THEN;  NAME(216) = 'J_KFF                          ';  IA(216) = 1;   ENDIF
      IF (ALLOCATED( J_KFS                          )) THEN;  NAME(217) = 'J_KFS                          ';  IA(217) = 1;   ENDIF
      IF (ALLOCATED( J_KFSe                         )) THEN;  NAME(218) = 'J_KFSe                         ';  IA(218) = 1;   ENDIF
      IF (ALLOCATED( J_KGG                          )) THEN;  NAME(219) = 'J_KGG                          ';  IA(219) = 1;   ENDIF
      IF (ALLOCATED( J_KLL                          )) THEN;  NAME(220) = 'J_KLL                          ';  IA(220) = 1;   ENDIF
      IF (ALLOCATED( J_KLLs                         )) THEN;  NAME(221) = 'J_KLLs                         ';  IA(221) = 1;   ENDIF
      IF (ALLOCATED( J_KMM                          )) THEN;  NAME(222) = 'J_KMM                          ';  IA(222) = 1;   ENDIF
      IF (ALLOCATED( J_KMN                          )) THEN;  NAME(223) = 'J_KMN                          ';  IA(223) = 1;   ENDIF
      IF (ALLOCATED( J_KMSM                         )) THEN;  NAME(224) = 'J_KMSM                         ';  IA(224) = 1;   ENDIF
      IF (ALLOCATED( J_KMSMn                        )) THEN;  NAME(225) = 'J_KMSMn                        ';  IA(225) = 1;   ENDIF
      IF (ALLOCATED( J_KMSMs                        )) THEN;  NAME(226) = 'J_KMSMs                        ';  IA(226) = 1;   ENDIF
      IF (ALLOCATED( J_KNM                          )) THEN;  NAME(227) = 'J_KNM                          ';  IA(227) = 1;   ENDIF
      IF (ALLOCATED( J_KNN                          )) THEN;  NAME(228) = 'J_KNN                          ';  IA(228) = 1;   ENDIF
      IF (ALLOCATED( J_KOO                          )) THEN;  NAME(229) = 'J_KOO                          ';  IA(229) = 1;   ENDIF
      IF (ALLOCATED( J_KOOs                         )) THEN;  NAME(230) = 'J_KOOs                         ';  IA(230) = 1;   ENDIF
      IF (ALLOCATED( J_KRL                          )) THEN;  NAME(231) = 'J_KRL                          ';  IA(231) = 1;   ENDIF
      IF (ALLOCATED( J_KRR                          )) THEN;  NAME(232) = 'J_KRR                          ';  IA(232) = 1;   ENDIF
      IF (ALLOCATED( J_KRRcb                        )) THEN;  NAME(233) = 'J_KRRcb                        ';  IA(233) = 1;   ENDIF
      IF (ALLOCATED( J_KRRcbn                       )) THEN;  NAME(234) = 'J_KRRcbn                       ';  IA(234) = 1;   ENDIF
      IF (ALLOCATED( J_KRRcbs                       )) THEN;  NAME(235) = 'J_KRRcbs                       ';  IA(235) = 1;   ENDIF
      IF (ALLOCATED( J_KSF                          )) THEN;  NAME(236) = 'J_KSF                          ';  IA(236) = 1;   ENDIF
      IF (ALLOCATED( J_KSFD                         )) THEN;  NAME(237) = 'J_KSFD                         ';  IA(237) = 1;   ENDIF
      IF (ALLOCATED( J_KSS                          )) THEN;  NAME(238) = 'J_KSS                          ';  IA(238) = 1;   ENDIF
      IF (ALLOCATED( J_KSSe                         )) THEN;  NAME(239) = 'J_KSSe                         ';  IA(239) = 1;   ENDIF
      IF (ALLOCATED( J_KXX                          )) THEN;  NAME(240) = 'J_KXX                          ';  IA(240) = 1;   ENDIF
      IF (ALLOCATED( J_LMN                          )) THEN;  NAME(241) = 'J_LMN                          ';  IA(241) = 1;   ENDIF
      IF (ALLOCATED( J_LTM                          )) THEN;  NAME(242) = 'J_LTM                          ';  IA(242) = 1;   ENDIF
      IF (ALLOCATED( J_MAA                          )) THEN;  NAME(243) = 'J_MAA                          ';  IA(243) = 1;   ENDIF
      IF (ALLOCATED( J_MAO                          )) THEN;  NAME(244) = 'J_MAO                          ';  IA(244) = 1;   ENDIF
      IF (ALLOCATED( J_MFF                          )) THEN;  NAME(245) = 'J_MFF                          ';  IA(245) = 1;   ENDIF
      IF (ALLOCATED( J_MFS                          )) THEN;  NAME(246) = 'J_MFS                          ';  IA(246) = 1;   ENDIF
      IF (ALLOCATED( J_MGG                          )) THEN;  NAME(247) = 'J_MGG                          ';  IA(247) = 1;   ENDIF
      IF (ALLOCATED( J_MGGC                         )) THEN;  NAME(248) = 'J_MGGC                         ';  IA(248) = 1;   ENDIF
      IF (ALLOCATED( J_MGGE                         )) THEN;  NAME(249) = 'J_MGGE                         ';  IA(249) = 1;   ENDIF
      IF (ALLOCATED( J_MGGS                         )) THEN;  NAME(250) = 'J_MGGS                         ';  IA(250) = 1;   ENDIF
      IF (ALLOCATED( J_MLL                          )) THEN;  NAME(251) = 'J_MLL                          ';  IA(251) = 1;   ENDIF
      IF (ALLOCATED( J_MLLn                         )) THEN;  NAME(252) = 'J_MLLn                         ';  IA(252) = 1;   ENDIF
      IF (ALLOCATED( J_MLLs                         )) THEN;  NAME(253) = 'J_MLLs                         ';  IA(253) = 1;   ENDIF
      IF (ALLOCATED( J_MLR                          )) THEN;  NAME(254) = 'J_MLR                          ';  IA(254) = 1;   ENDIF
      IF (ALLOCATED( J_MMM                          )) THEN;  NAME(255) = 'J_MMM                          ';  IA(255) = 1;   ENDIF
      IF (ALLOCATED( J_MMN                          )) THEN;  NAME(256) = 'J_MMN                          ';  IA(256) = 1;   ENDIF
      IF (ALLOCATED( J_MNM                          )) THEN;  NAME(257) = 'J_MNM                          ';  IA(257) = 1;   ENDIF
      IF (ALLOCATED( J_MNN                          )) THEN;  NAME(258) = 'J_MNN                          ';  IA(258) = 1;   ENDIF
      IF (ALLOCATED( J_MOO                          )) THEN;  NAME(259) = 'J_MOO                          ';  IA(259) = 1;   ENDIF
      IF (ALLOCATED( J_MPF0                         )) THEN;  NAME(260) = 'J_MPF0                         ';  IA(260) = 1;   ENDIF
      IF (ALLOCATED( J_MRL                          )) THEN;  NAME(261) = 'J_MRL                          ';  IA(261) = 1;   ENDIF
      IF (ALLOCATED( J_MRN                          )) THEN;  NAME(262) = 'J_MRN                          ';  IA(262) = 1;   ENDIF
      IF (ALLOCATED( J_MRR                          )) THEN;  NAME(263) = 'J_MRR                          ';  IA(263) = 1;   ENDIF
      IF (ALLOCATED( J_MRRcb                        )) THEN;  NAME(264) = 'J_MRRcb                        ';  IA(264) = 1;   ENDIF
      IF (ALLOCATED( J_MRRcbn                       )) THEN;  NAME(265) = 'J_MRRcbn                       ';  IA(265) = 1;   ENDIF
      IF (ALLOCATED( J_MSF                          )) THEN;  NAME(266) = 'J_MSF                          ';  IA(266) = 1;   ENDIF
      IF (ALLOCATED( J_MSS                          )) THEN;  NAME(267) = 'J_MSS                          ';  IA(267) = 1;   ENDIF
      IF (ALLOCATED( J_MXX                          )) THEN;  NAME(268) = 'J_MXX                          ';  IA(268) = 1;   ENDIF
      IF (ALLOCATED( J_MXXn                         )) THEN;  NAME(269) = 'J_MXXn                         ';  IA(269) = 1;   ENDIF
      IF (ALLOCATED( J_PA                           )) THEN;  NAME(270) = 'J_PA                           ';  IA(270) = 1;   ENDIF
      IF (ALLOCATED( J_PF                           )) THEN;  NAME(271) = 'J_PF                           ';  IA(271) = 1;   ENDIF
      IF (ALLOCATED( J_PF_TMP                       )) THEN;  NAME(272) = 'J_PF_TMP                       ';  IA(272) = 1;   ENDIF
      IF (ALLOCATED( J_PFYS                         )) THEN;  NAME(273) = 'J_PFYS                         ';  IA(273) = 1;   ENDIF
      IF (ALLOCATED( J_PFYS1                        )) THEN;  NAME(274) = 'J_PFYS1                        ';  IA(274) = 1;   ENDIF
      IF (ALLOCATED( J_PG                           )) THEN;  NAME(275) = 'J_PG                           ';  IA(275) = 1;   ENDIF
      IF (ALLOCATED( J_PHIXA                        )) THEN;  NAME(276) = 'J_PHIXA                        ';  IA(276) = 1;   ENDIF
      IF (ALLOCATED( J_PHIZL                        )) THEN;  NAME(277) = 'J_PHIZL                        ';  IA(277) = 1;   ENDIF
      IF (ALLOCATED( J_PHIZL1                       )) THEN;  NAME(278) = 'J_PHIZL1                       ';  IA(278) = 1;   ENDIF
      IF (ALLOCATED( J_PHIZL1t                      )) THEN;  NAME(279) = 'J_PHIZL1t                      ';  IA(279) = 1;   ENDIF
      IF (ALLOCATED( J_PHIZL2                       )) THEN;  NAME(280) = 'J_PHIZL2                       ';  IA(280) = 1;   ENDIF
      IF (ALLOCATED( J_PL                           )) THEN;  NAME(281) = 'J_PL                           ';  IA(281) = 1;   ENDIF
      IF (ALLOCATED( J_PM                           )) THEN;  NAME(282) = 'J_PM                           ';  IA(282) = 1;   ENDIF
      IF (ALLOCATED( J_PN                           )) THEN;  NAME(283) = 'J_PN                           ';  IA(283) = 1;   ENDIF
      IF (ALLOCATED( J_PO                           )) THEN;  NAME(284) = 'J_PO                           ';  IA(284) = 1;   ENDIF
      IF (ALLOCATED( J_PR                           )) THEN;  NAME(285) = 'J_PR                           ';  IA(285) = 1;   ENDIF
      IF (ALLOCATED( J_PS                           )) THEN;  NAME(286) = 'J_PS                           ';  IA(286) = 1;   ENDIF
      IF (ALLOCATED( J_QSYS                         )) THEN;  NAME(287) = 'J_QSYS                         ';  IA(287) = 1;   ENDIF
      IF (ALLOCATED( J_RMG                          )) THEN;  NAME(288) = 'J_RMG                          ';  IA(288) = 1;   ENDIF
      IF (ALLOCATED( J_RMM                          )) THEN;  NAME(289) = 'J_RMM                          ';  IA(289) = 1;   ENDIF
      IF (ALLOCATED( J_RMN                          )) THEN;  NAME(290) = 'J_RMN                          ';  IA(290) = 1;   ENDIF
      IF (ALLOCATED( KAA                            )) THEN;  NAME(291) = 'KAA                            ';  IA(291) = 1;   ENDIF
      IF (ALLOCATED( KAA_FULL                       )) THEN;  NAME(292) = 'KAA_FULL                       ';  IA(292) = 1;   ENDIF
      IF (ALLOCATED( KAO                            )) THEN;  NAME(293) = 'KAO                            ';  IA(293) = 1;   ENDIF
      IF (ALLOCATED( KAO_FULL                       )) THEN;  NAME(294) = 'KAO_FULL                       ';  IA(294) = 1;   ENDIF
      IF (ALLOCATED( KE                             )) THEN;  NAME(295) = 'KE                             ';  IA(295) = 1;   ENDIF
      IF (ALLOCATED( KED                            )) THEN;  NAME(296) = 'KED                            ';  IA(296) = 1;   ENDIF
      IF (ALLOCATED( KEM                            )) THEN;  NAME(297) = 'KEM                            ';  IA(297) = 1;   ENDIF
      IF (ALLOCATED( KFF                            )) THEN;  NAME(298) = 'KFF                            ';  IA(298) = 1;   ENDIF
      IF (ALLOCATED( KFF_FULL                       )) THEN;  NAME(299) = 'KFF_FULL                       ';  IA(299) = 1;   ENDIF
      IF (ALLOCATED( KFS                            )) THEN;  NAME(300) = 'KFS                            ';  IA(300) = 1;   ENDIF
      IF (ALLOCATED( KFS_FULL                       )) THEN;  NAME(301) = 'KFS_FULL                       ';  IA(301) = 1;   ENDIF
      IF (ALLOCATED( KFSe                           )) THEN;  NAME(302) = 'KFSe                           ';  IA(302) = 1;   ENDIF
      IF (ALLOCATED( KFSe_FULL                      )) THEN;  NAME(303) = 'KFSe_FULL                      ';  IA(303) = 1;   ENDIF
      IF (ALLOCATED( KGG                            )) THEN;  NAME(304) = 'KGG                            ';  IA(304) = 1;   ENDIF
      IF (ALLOCATED( KLL                            )) THEN;  NAME(305) = 'KLL                            ';  IA(305) = 1;   ENDIF
      IF (ALLOCATED( KLLs                           )) THEN;  NAME(306) = 'KLLs                           ';  IA(306) = 1;   ENDIF
      IF (ALLOCATED( KMM                            )) THEN;  NAME(307) = 'KMM                            ';  IA(307) = 1;   ENDIF
      IF (ALLOCATED( KMM_FULL                       )) THEN;  NAME(308) = 'KMM_FULL                       ';  IA(308) = 1;   ENDIF
      IF (ALLOCATED( KMN                            )) THEN;  NAME(309) = 'KMN                            ';  IA(309) = 1;   ENDIF
      IF (ALLOCATED( KMSM                           )) THEN;  NAME(310) = 'KMSM                           ';  IA(310) = 1;   ENDIF
      IF (ALLOCATED( KMSMn                          )) THEN;  NAME(311) = 'KMSMn                          ';  IA(311) = 1;   ENDIF
      IF (ALLOCATED( KMSMs                          )) THEN;  NAME(312) = 'KMSMs                          ';  IA(312) = 1;   ENDIF
      IF (ALLOCATED( KNM                            )) THEN;  NAME(313) = 'KNM                            ';  IA(313) = 1;   ENDIF
      IF (ALLOCATED( KNM_FULL                       )) THEN;  NAME(314) = 'KNM_FULL                       ';  IA(314) = 1;   ENDIF
      IF (ALLOCATED( KNN                            )) THEN;  NAME(315) = 'KNN                            ';  IA(315) = 1;   ENDIF
      IF (ALLOCATED( KNN_FULL                       )) THEN;  NAME(316) = 'KNN_FULL                       ';  IA(316) = 1;   ENDIF
      IF (ALLOCATED( KOO                            )) THEN;  NAME(317) = 'KOO                            ';  IA(317) = 1;   ENDIF
      IF (ALLOCATED( KOO_FULL                       )) THEN;  NAME(318) = 'KOO_FULL                       ';  IA(318) = 1;   ENDIF
      IF (ALLOCATED( KOOs                           )) THEN;  NAME(319) = 'KOOs                           ';  IA(319) = 1;   ENDIF
      IF (ALLOCATED( KRL                            )) THEN;  NAME(320) = 'KRL                            ';  IA(320) = 1;   ENDIF
      IF (ALLOCATED( KRR                            )) THEN;  NAME(321) = 'KRR                            ';  IA(321) = 1;   ENDIF
      IF (ALLOCATED( KRRcb                          )) THEN;  NAME(322) = 'KRRcb                          ';  IA(322) = 1;   ENDIF
      IF (ALLOCATED( KRRcbn                         )) THEN;  NAME(323) = 'KRRcbn                         ';  IA(323) = 1;   ENDIF
      IF (ALLOCATED( KRRcbs                         )) THEN;  NAME(324) = 'KRRcbs                         ';  IA(324) = 1;   ENDIF
      IF (ALLOCATED( KSF                            )) THEN;  NAME(325) = 'KSF                            ';  IA(325) = 1;   ENDIF
      IF (ALLOCATED( KSFD                           )) THEN;  NAME(326) = 'KSFD                           ';  IA(326) = 1;   ENDIF
      IF (ALLOCATED( KSS                            )) THEN;  NAME(327) = 'KSS                            ';  IA(327) = 1;   ENDIF
      IF (ALLOCATED( KSS_FULL                       )) THEN;  NAME(328) = 'KSS_FULL                       ';  IA(328) = 1;   ENDIF
      IF (ALLOCATED( KSSe                           )) THEN;  NAME(329) = 'KSSe                           ';  IA(329) = 1;   ENDIF
      IF (ALLOCATED( KSSe_FULL                      )) THEN;  NAME(330) = 'KSSe_FULL                      ';  IA(330) = 1;   ENDIF
      IF (ALLOCATED( KXX                            )) THEN;  NAME(331) = 'KXX                            ';  IA(331) = 1;   ENDIF
      IF (ALLOCATED( LABEL                          )) THEN;  NAME(332) = 'LABEL                          ';  IA(332) = 1;   ENDIF
      IF (ALLOCATED( LAPACK_S                       )) THEN;  NAME(333) = 'LAPACK_S                       ';  IA(333) = 1;   ENDIF
      IF (ALLOCATED( LMN                            )) THEN;  NAME(334) = 'LMN                            ';  IA(334) = 1;   ENDIF
      IF (ALLOCATED( LOAD_FACS                      )) THEN;  NAME(335) = 'LOAD_FACS                      ';  IA(335) = 1;   ENDIF
      IF (ALLOCATED( LOAD_SIDS                      )) THEN;  NAME(336) = 'LOAD_SIDS                      ';  IA(336) = 1;   ENDIF
      IF (ALLOCATED( LOGICAL_VEC                    )) THEN;  NAME(337) = 'LOGICAL_VEC                    ';  IA(337) = 1;   ENDIF
      IF (ALLOCATED( LTM                            )) THEN;  NAME(338) = 'LTM                            ';  IA(338) = 1;   ENDIF
      IF (ALLOCATED( MAA                            )) THEN;  NAME(339) = 'MAA                            ';  IA(339) = 1;   ENDIF
      IF (ALLOCATED( MAA_FULL                       )) THEN;  NAME(340) = 'MAA_FULL                       ';  IA(340) = 1;   ENDIF
      IF (ALLOCATED( MAO                            )) THEN;  NAME(341) = 'MAO                            ';  IA(341) = 1;   ENDIF
      IF (ALLOCATED( MAO_FULL                       )) THEN;  NAME(342) = 'MAO_FULL                       ';  IA(342) = 1;   ENDIF
      IF (ALLOCATED( MATANGLE                       )) THEN;  NAME(343) = 'MATANGLE                       ';  IA(343) = 1;   ENDIF
      IF (ALLOCATED( MATL                           )) THEN;  NAME(344) = 'MATL                           ';  IA(344) = 1;   ENDIF
      IF (ALLOCATED( ME                             )) THEN;  NAME(345) = 'ME                             ';  IA(345) = 1;   ENDIF
      IF (ALLOCATED( MEFFMASS                       )) THEN;  NAME(346) = 'MEFFMASS                       ';  IA(346) = 1;   ENDIF
      IF (ALLOCATED( MFF                            )) THEN;  NAME(347) = 'MFF                            ';  IA(347) = 1;   ENDIF
      IF (ALLOCATED( MFF_FULL                       )) THEN;  NAME(348) = 'MFF_FULL                       ';  IA(348) = 1;   ENDIF
      IF (ALLOCATED( MFS                            )) THEN;  NAME(349) = 'MFS                            ';  IA(349) = 1;   ENDIF
      IF (ALLOCATED( MFS_FULL                       )) THEN;  NAME(350) = 'MFS_FULL                       ';  IA(350) = 1;   ENDIF
      IF (ALLOCATED( MGG                            )) THEN;  NAME(351) = 'MGG                            ';  IA(351) = 1;   ENDIF
      IF (ALLOCATED( MGGC                           )) THEN;  NAME(352) = 'MGGC                           ';  IA(352) = 1;   ENDIF
      IF (ALLOCATED( MGGE                           )) THEN;  NAME(353) = 'MGGE                           ';  IA(353) = 1;   ENDIF
      IF (ALLOCATED( MGGS                           )) THEN;  NAME(354) = 'MGGS                           ';  IA(354) = 1;   ENDIF
      IF (ALLOCATED( MLL                            )) THEN;  NAME(355) = 'MLL                            ';  IA(355) = 1;   ENDIF
      IF (ALLOCATED( MLLn                           )) THEN;  NAME(356) = 'MLLn                           ';  IA(356) = 1;   ENDIF
      IF (ALLOCATED( MLLs                           )) THEN;  NAME(357) = 'MLLs                           ';  IA(357) = 1;   ENDIF
      IF (ALLOCATED( MLR                            )) THEN;  NAME(358) = 'MLR                            ';  IA(358) = 1;   ENDIF
      IF (ALLOCATED( MMM                            )) THEN;  NAME(359) = 'MMM                            ';  IA(359) = 1;   ENDIF
      IF (ALLOCATED( MMM_FULL                       )) THEN;  NAME(360) = 'MMM_FULL                       ';  IA(360) = 1;   ENDIF
      IF (ALLOCATED( MMN                            )) THEN;  NAME(361) = 'MMN                            ';  IA(361) = 1;   ENDIF
      IF (ALLOCATED( MNM                            )) THEN;  NAME(362) = 'MNM                            ';  IA(362) = 1;   ENDIF
      IF (ALLOCATED( MNM_FULL                       )) THEN;  NAME(363) = 'MNM_FULL                       ';  IA(363) = 1;   ENDIF
      IF (ALLOCATED( MNN                            )) THEN;  NAME(364) = 'MNN                            ';  IA(364) = 1;   ENDIF
      IF (ALLOCATED( MNN_FULL                       )) THEN;  NAME(365) = 'MNN_FULL                       ';  IA(365) = 1;   ENDIF
      IF (ALLOCATED( MODE_NUM                       )) THEN;  NAME(366) = 'MODE_NUM                       ';  IA(366) = 1;   ENDIF
      IF (ALLOCATED( MOO                            )) THEN;  NAME(367) = 'MOO                            ';  IA(367) = 1;   ENDIF
      IF (ALLOCATED( MOO_FULL                       )) THEN;  NAME(368) = 'MOO_FULL                       ';  IA(368) = 1;   ENDIF
      IF (ALLOCATED( MPC_IND_GRIDS                  )) THEN;  NAME(369) = 'MPC_IND_GRIDS                  ';  IA(369) = 1;   ENDIF
      IF (ALLOCATED( MPC_SIDS                       )) THEN;  NAME(370) = 'MPC_SIDS                       ';  IA(370) = 1;   ENDIF
      IF (ALLOCATED( MPCADD_SIDS                    )) THEN;  NAME(371) = 'MPCADD_SIDS                    ';  IA(371) = 1;   ENDIF
      IF (ALLOCATED( MPCSIDS                        )) THEN;  NAME(372) = 'MPCSIDS                        ';  IA(372) = 1;   ENDIF
      IF (ALLOCATED( MPCSETS                        )) THEN;  NAME(373) = 'MPCSETS                        ';  IA(373) = 1;   ENDIF
      IF (ALLOCATED( MPF0                           )) THEN;  NAME(374) = 'MPF0                           ';  IA(374) = 1;   ENDIF
      IF (ALLOCATED( MPFACTOR_N6                    )) THEN;  NAME(375) = 'MPFACTOR_N6                    ';  IA(375) = 1;   ENDIF
      IF (ALLOCATED( MPFACTOR_NR                    )) THEN;  NAME(376) = 'MPFACTOR_NR                    ';  IA(376) = 1;   ENDIF
      IF (ALLOCATED( MRL                            )) THEN;  NAME(377) = 'MRL                            ';  IA(377) = 1;   ENDIF
      IF (ALLOCATED( MRN                            )) THEN;  NAME(378) = 'MRN                            ';  IA(378) = 1;   ENDIF
      IF (ALLOCATED( MRR                            )) THEN;  NAME(379) = 'MRR                            ';  IA(379) = 1;   ENDIF
      IF (ALLOCATED( MRRcb                          )) THEN;  NAME(380) = 'MRRcb                          ';  IA(380) = 1;   ENDIF
      IF (ALLOCATED( MRRcbn                         )) THEN;  NAME(381) = 'MRRcbn                         ';  IA(381) = 1;   ENDIF
      IF (ALLOCATED( MSF                            )) THEN;  NAME(382) = 'MSF                            ';  IA(382) = 1;   ENDIF
      IF (ALLOCATED( MSPRNT                         )) THEN;  NAME(383) = 'MSPRNT                         ';  IA(383) = 1;   ENDIF
      IF (ALLOCATED( MSS                            )) THEN;  NAME(384) = 'MSS                            ';  IA(384) = 1;   ENDIF
      IF (ALLOCATED( MSS_FULL                       )) THEN;  NAME(385) = 'MSS_FULL                       ';  IA(385) = 1;   ENDIF
      IF (ALLOCATED( MXX                            )) THEN;  NAME(386) = 'MXX                            ';  IA(386) = 1;   ENDIF
      IF (ALLOCATED( MXXn                           )) THEN;  NAME(387) = 'MXXn                           ';  IA(387) = 1;   ENDIF
      IF (ALLOCATED( NL_SID                         )) THEN;  NAME(388) = 'NL_SID                         ';  IA(388) = 1;   ENDIF
      IF (ALLOCATED( OELOUT                         )) THEN;  NAME(389) = 'OELOUT                         ';  IA(389) = 1;   ENDIF
      IF (ALLOCATED( OFFDIS                         )) THEN;  NAME(390) = 'OFFDIS                         ';  IA(390) = 1;   ENDIF
      IF (ALLOCATED( OFFSET                         )) THEN;  NAME(391) = 'OFFSET                         ';  IA(391) = 1;   ENDIF
      IF (ALLOCATED( OGEL                           )) THEN;  NAME(392) = 'OGEL                           ';  IA(392) = 1;   ENDIF
      IF (ALLOCATED( OGROUT                         )) THEN;  NAME(393) = 'OGROUT                         ';  IA(393) = 1;   ENDIF
      IF (ALLOCATED( ONE_SET_ARRAY                  )) THEN;  NAME(394) = 'ONE_SET_ARRAY                  ';  IA(394) = 1;   ENDIF
      IF (ALLOCATED( OTM_ACCE                       )) THEN;  NAME(395) = 'OTM_ACCE                       ';  IA(395) = 1;   ENDIF
      IF (ALLOCATED( OTM_DISP                       )) THEN;  NAME(396) = 'OTM_DISP                       ';  IA(396) = 1;   ENDIF
      IF (ALLOCATED( OTM_ELFE                       )) THEN;  NAME(397) = 'OTM_ELFE                       ';  IA(397) = 1;   ENDIF
      IF (ALLOCATED( OTM_ELFN                       )) THEN;  NAME(398) = 'OTM_ELFN                       ';  IA(398) = 1;   ENDIF
      IF (ALLOCATED( OTM_MPCF                       )) THEN;  NAME(399) = 'OTM_MPCF                       ';  IA(399) = 1;   ENDIF
      IF (ALLOCATED( OTM_SPCF                       )) THEN;  NAME(400) = 'OTM_SPCF                       ';  IA(400) = 1;   ENDIF
      IF (ALLOCATED( OTM_STRE                       )) THEN;  NAME(401) = 'OTM_STRE                       ';  IA(401) = 1;   ENDIF
      IF (ALLOCATED( OTM_STRN                       )) THEN;  NAME(402) = 'OTM_STRN                       ';  IA(402) = 1;   ENDIF
      IF (ALLOCATED( PA                             )) THEN;  NAME(403) = 'PA                             ';  IA(403) = 1;   ENDIF
      IF (ALLOCATED( OU4_MAT_COL_GRD_COMP           )) THEN;  NAME(404) = 'OU4_MAT_COL_GRD_COMP           ';  IA(404) = 1;   ENDIF
      IF (ALLOCATED( OU4_MAT_ROW_GRD_COMP           )) THEN;  NAME(405) = 'OU4_MAT_ROW_GRD_COMP           ';  IA(405) = 1;   ENDIF
      IF (ALLOCATED( PA_FULL                        )) THEN;  NAME(406) = 'PA_FULL                        ';  IA(406) = 1;   ENDIF
      IF (ALLOCATED( PBAR                           )) THEN;  NAME(407) = 'PBAR                           ';  IA(407) = 1;   ENDIF
      IF (ALLOCATED( PBEAM                          )) THEN;  NAME(408) = 'PBEAM                          ';  IA(408) = 1;   ENDIF
      IF (ALLOCATED( PBUSH                          )) THEN;  NAME(409) = 'PBUSH                          ';  IA(409) = 1;   ENDIF
      IF (ALLOCATED( PCOMP                          )) THEN;  NAME(410) = 'PCOMP                          ';  IA(410) = 1;   ENDIF
      IF (ALLOCATED( PDATA                          )) THEN;  NAME(411) = 'PDATA                          ';  IA(411) = 1;   ENDIF
      IF (ALLOCATED( PEB                            )) THEN;  NAME(412) = 'PEB                            ';  IA(412) = 1;   ENDIF
      IF (ALLOCATED( PEG                            )) THEN;  NAME(413) = 'PEG                            ';  IA(413) = 1;   ENDIF
      IF (ALLOCATED( PEL                            )) THEN;  NAME(414) = 'PEL                            ';  IA(414) = 1;   ENDIF
      IF (ALLOCATED( PELAS                          )) THEN;  NAME(415) = 'PELAS                          ';  IA(415) = 1;   ENDIF
      IF (ALLOCATED( PF                             )) THEN;  NAME(416) = 'PF                             ';  IA(416) = 1;   ENDIF
      IF (ALLOCATED( PF_FULL                        )) THEN;  NAME(417) = 'PF_FULL                        ';  IA(417) = 1;   ENDIF
      IF (ALLOCATED( PF_TMP                         )) THEN;  NAME(418) = 'PF_TMP                         ';  IA(418) = 1;   ENDIF
      IF (ALLOCATED( PFYS                           )) THEN;  NAME(419) = 'PFYS                           ';  IA(419) = 1;   ENDIF
      IF (ALLOCATED( PFYS_FULL                      )) THEN;  NAME(420) = 'PFYS_FULL                      ';  IA(420) = 1;   ENDIF
      IF (ALLOCATED( PFYS1                          )) THEN;  NAME(421) = 'PFYS1                          ';  IA(421) = 1;   ENDIF
      IF (ALLOCATED( PG                             )) THEN;  NAME(422) = 'PG                             ';  IA(422) = 1;   ENDIF
      IF (ALLOCATED( PG_COL                         )) THEN;  NAME(423) = 'PG_COL                         ';  IA(423) = 1;   ENDIF
      IF (ALLOCATED( PHIXA                          )) THEN;  NAME(424) = 'PHIXA                          ';  IA(424) = 1;   ENDIF
      IF (ALLOCATED( PHIXG_COL                      )) THEN;  NAME(425) = 'PHIXG_COL                      ';  IA(425) = 1;   ENDIF
      IF (ALLOCATED( PHIXGP_COL                     )) THEN;  NAME(426) = 'PHIXGP_COL                     ';  IA(426) = 1;   ENDIF
      IF (ALLOCATED( PHIXL_COL                      )) THEN;  NAME(427) = 'PHIXL_COL                      ';  IA(427) = 1;   ENDIF
      IF (ALLOCATED( PHIXLP_COL                     )) THEN;  NAME(428) = 'PHIXLP_COL                     ';  IA(428) = 1;   ENDIF
      IF (ALLOCATED( PHIXN_COL                      )) THEN;  NAME(429) = 'PHIXN_COL                      ';  IA(429) = 1;   ENDIF
      IF (ALLOCATED( PHIXNP_COL                     )) THEN;  NAME(430) = 'PHIXNP_COL                     ';  IA(430) = 1;   ENDIF
      IF (ALLOCATED( PHIZG_FULL                     )) THEN;  NAME(431) = 'PHIZG_FULL                     ';  IA(431) = 1;   ENDIF
      IF (ALLOCATED( PHIZL                          )) THEN;  NAME(432) = 'PHIZL                          ';  IA(432) = 1;   ENDIF
      IF (ALLOCATED( PHIZL1                         )) THEN;  NAME(433) = 'PHIZL1                         ';  IA(433) = 1;   ENDIF
      IF (ALLOCATED( PHIZL1t                        )) THEN;  NAME(434) = 'PHIZL1t                        ';  IA(434) = 1;   ENDIF
      IF (ALLOCATED( PHIZL2                         )) THEN;  NAME(435) = 'PHIZL2                         ';  IA(435) = 1;   ENDIF
      IF (ALLOCATED( PL                             )) THEN;  NAME(436) = 'PL                             ';  IA(436) = 1;   ENDIF
      IF (ALLOCATED( PL_COL                         )) THEN;  NAME(437) = 'PL_COL                         ';  IA(437) = 1;   ENDIF
      IF (ALLOCATED( PLATEOFF                       )) THEN;  NAME(438) = 'PLATEOFF                       ';  IA(438) = 1;   ENDIF
      IF (ALLOCATED( PLATETHICK                     )) THEN;  NAME(439) = 'PLATETHICK                     ';  IA(439) = 1;   ENDIF
      IF (ALLOCATED( PM                             )) THEN;  NAME(440) = 'PM                             ';  IA(440) = 1;   ENDIF
      IF (ALLOCATED( PM_COL                         )) THEN;  NAME(441) = 'PM_COL                         ';  IA(441) = 1;   ENDIF
      IF (ALLOCATED( PM_FULL                        )) THEN;  NAME(442) = 'PM_FULL                        ';  IA(442) = 1;   ENDIF
      IF (ALLOCATED( PMASS                          )) THEN;  NAME(443) = 'PMASS                          ';  IA(443) = 1;   ENDIF
      IF (ALLOCATED( PN                             )) THEN;  NAME(444) = 'PN                             ';  IA(444) = 1;   ENDIF
      IF (ALLOCATED( PN_FULL                        )) THEN;  NAME(445) = 'PN_FULL                        ';  IA(445) = 1;   ENDIF
      IF (ALLOCATED( PO                             )) THEN;  NAME(446) = 'PO                             ';  IA(446) = 1;   ENDIF
      IF (ALLOCATED( PO_FULL                        )) THEN;  NAME(447) = 'PO_FULL                        ';  IA(447) = 1;   ENDIF
      IF (ALLOCATED( POLY_FIT_ERR                   )) THEN;  NAME(448) = 'POLY_FIT_ERR                   ';  IA(448) = 1;   ENDIF
      IF (ALLOCATED( POLY_FIT_ERR_INDEX             )) THEN;  NAME(449) = 'POLY_FIT_ERR_INDEX             ';  IA(449) = 1;   ENDIF
      IF (ALLOCATED( PPE                            )) THEN;  NAME(450) = 'PPE                            ';  IA(450) = 1;   ENDIF
      IF (ALLOCATED( PPNT                           )) THEN;  NAME(451) = 'PPNT                           ';  IA(451) = 1;   ENDIF
      IF (ALLOCATED( PR                             )) THEN;  NAME(452) = 'PR                             ';  IA(452) = 1;   ENDIF
      IF (ALLOCATED( PRESS                          )) THEN;  NAME(453) = 'PRESS                          ';  IA(453) = 1;   ENDIF
      IF (ALLOCATED( PLOAD4_3D_DATA                 )) THEN;  NAME(454) = 'PLOAD4_3D_DATA                 ';  IA(454) = 1;   ENDIF
      IF (ALLOCATED( PRESS_SIDS                     )) THEN;  NAME(455) = 'PRESS_SIDS                     ';  IA(455) = 1;   ENDIF
      IF (ALLOCATED( PROD                           )) THEN;  NAME(456) = 'PROD                           ';  IA(456) = 1;   ENDIF
      IF (ALLOCATED( PS                             )) THEN;  NAME(457) = 'PS                             ';  IA(457) = 1;   ENDIF
      IF (ALLOCATED( PS_COL                         )) THEN;  NAME(458) = 'PS_COL                         ';  IA(458) = 1;   ENDIF
      IF (ALLOCATED( PS_FULL                        )) THEN;  NAME(459) = 'PS_FULL                        ';  IA(459) = 1;   ENDIF
      IF (ALLOCATED( PSHEAR                         )) THEN;  NAME(460) = 'PSHEAR                         ';  IA(460) = 1;   ENDIF
      IF (ALLOCATED( PSHEL                          )) THEN;  NAME(461) = 'PSHEL                          ';  IA(461) = 1;   ENDIF
      IF (ALLOCATED( PSOLID                         )) THEN;  NAME(462) = 'PSOLID                         ';  IA(462) = 1;   ENDIF
      IF (ALLOCATED( PTE                            )) THEN;  NAME(463) = 'PTE                            ';  IA(463) = 1;   ENDIF
      IF (ALLOCATED( PTYPE                          )) THEN;  NAME(464) = 'PTYPE                          ';  IA(464) = 1;   ENDIF
      IF (ALLOCATED( PUSER1                         )) THEN;  NAME(465) = 'PUSER1                         ';  IA(465) = 1;   ENDIF
      IF (ALLOCATED( PUSERIN                        )) THEN;  NAME(466) = 'PUSERIN                        ';  IA(466) = 1;   ENDIF
      IF (ALLOCATED( QGm_COL                        )) THEN;  NAME(467) = 'QGm_COL                        ';  IA(467) = 1;   ENDIF
      IF (ALLOCATED( QGr_COL                        )) THEN;  NAME(468) = 'QGr_COL                        ';  IA(468) = 1;   ENDIF
      IF (ALLOCATED( QGs_COL                        )) THEN;  NAME(469) = 'QGs_COL                        ';  IA(469) = 1;   ENDIF
      IF (ALLOCATED( QM_COL                         )) THEN;  NAME(470) = 'QM_COL                         ';  IA(470) = 1;   ENDIF
      IF (ALLOCATED( QN_COL                         )) THEN;  NAME(471) = 'QN_COL                         ';  IA(471) = 1;   ENDIF
      IF (ALLOCATED( QR_COL                         )) THEN;  NAME(472) = 'QR_COL                         ';  IA(472) = 1;   ENDIF
      IF (ALLOCATED( QS_COL                         )) THEN;  NAME(473) = 'QS_COL                         ';  IA(473) = 1;   ENDIF
      IF (ALLOCATED( QSYS                           )) THEN;  NAME(474) = 'QSYS                           ';  IA(474) = 1;   ENDIF
      IF (ALLOCATED( QSYS_COL                       )) THEN;  NAME(475) = 'QSYS_COL                       ';  IA(475) = 1;   ENDIF
      IF (ALLOCATED( QSYS_FULL                      )) THEN;  NAME(476) = 'QSYS_FULL                      ';  IA(476) = 1;   ENDIF
      IF (ALLOCATED( RBGLOBAL_ASET                  )) THEN;  NAME(477) = 'RBGLOBAL_ASET                  ';  IA(477) = 1;   ENDIF
      IF (ALLOCATED( RBGLOBAL_FSET                  )) THEN;  NAME(478) = 'RBGLOBAL_FSET                  ';  IA(478) = 1;   ENDIF
      IF (ALLOCATED( RBGLOBAL_GSET                  )) THEN;  NAME(479) = 'RBGLOBAL_GSET                  ';  IA(479) = 1;   ENDIF
      IF (ALLOCATED( RBGLOBAL_LSET                  )) THEN;  NAME(480) = 'RBGLOBAL_LSET                  ';  IA(480) = 1;   ENDIF
      IF (ALLOCATED( RBGLOBAL_NSET                  )) THEN;  NAME(481) = 'RBGLOBAL_NSET                  ';  IA(481) = 1;   ENDIF
      IF (ALLOCATED( RCONM2                         )) THEN;  NAME(482) = 'RCONM2                         ';  IA(482) = 1;   ENDIF
      IF (ALLOCATED( RCORD                          )) THEN;  NAME(483) = 'RCORD                          ';  IA(483) = 1;   ENDIF
      IF (ALLOCATED( REAL_VEC                       )) THEN;  NAME(484) = 'REAL_VEC                       ';  IA(484) = 1;   ENDIF
      IF (ALLOCATED( RES                            )) THEN;  NAME(485) = 'RES                            ';  IA(485) = 1;   ENDIF
      IF (ALLOCATED( RESID                          )) THEN;  NAME(486) = 'RESID                          ';  IA(486) = 1;   ENDIF
      IF (ALLOCATED( RFAC                           )) THEN;  NAME(487) = 'RFAC                           ';  IA(487) = 1;   ENDIF
      IF (ALLOCATED( RFORCE_SIDS                    )) THEN;  NAME(488) = 'RFORCE_SIDS                    ';  IA(488) = 1;   ENDIF
      IF (ALLOCATED( RGRID                          )) THEN;  NAME(489) = 'RGRID                          ';  IA(489) = 1;   ENDIF
      IF (ALLOCATED( RIGID_ELEM_IDS                 )) THEN;  NAME(490) = 'RIGID_ELEM_IDS                 ';  IA(490) = 1;   ENDIF
      IF (ALLOCATED( RMATL                          )) THEN;  NAME(491) = 'RMATL                          ';  IA(491) = 1;   ENDIF
      IF (ALLOCATED( RMG                            )) THEN;  NAME(492) = 'RMG                            ';  IA(492) = 1;   ENDIF
      IF (ALLOCATED( RMM                            )) THEN;  NAME(493) = 'RMM                            ';  IA(493) = 1;   ENDIF
      IF (ALLOCATED( RMM_FULL                       )) THEN;  NAME(494) = 'RMM_FULL                       ';  IA(494) = 1;   ENDIF
      IF (ALLOCATED( RMN                            )) THEN;  NAME(495) = 'RMN                            ';  IA(495) = 1;   ENDIF
      IF (ALLOCATED( RPBAR                          )) THEN;  NAME(496) = 'RPBAR                          ';  IA(496) = 1;   ENDIF
      IF (ALLOCATED( RPBEAM                         )) THEN;  NAME(497) = 'RPBEAM                         ';  IA(497) = 1;   ENDIF
      IF (ALLOCATED( RPBUSH                         )) THEN;  NAME(498) = 'RPBUSH                         ';  IA(498) = 1;   ENDIF
      IF (ALLOCATED( RPCOMP                         )) THEN;  NAME(499) = 'RPCOMP                         ';  IA(499) = 1;   ENDIF
      IF (ALLOCATED( RPELAS                         )) THEN;  NAME(500) = 'RPELAS                         ';  IA(500) = 1;   ENDIF
      IF (ALLOCATED( RPMASS                         )) THEN;  NAME(501) = 'RPMASS                         ';  IA(501) = 1;   ENDIF
      IF (ALLOCATED( RPROD                          )) THEN;  NAME(502) = 'RPROD                          ';  IA(502) = 1;   ENDIF
      IF (ALLOCATED( RPSHEAR                        )) THEN;  NAME(503) = 'RPSHEAR                        ';  IA(503) = 1;   ENDIF
      IF (ALLOCATED( RPSHEL                         )) THEN;  NAME(504) = 'RPSHEL                         ';  IA(504) = 1;   ENDIF
      IF (ALLOCATED( RPUSER1                        )) THEN;  NAME(505) = 'RPUSER1                        ';  IA(505) = 1;   ENDIF
      IF (ALLOCATED( SC_ACCE                        )) THEN;  NAME(506) = 'SC_ACCE                        ';  IA(506) = 1;   ENDIF
      IF (ALLOCATED( SC_DISP                        )) THEN;  NAME(507) = 'SC_DISP                        ';  IA(507) = 1;   ENDIF
      IF (ALLOCATED( SC_ELFE                        )) THEN;  NAME(508) = 'SC_ELFE                        ';  IA(508) = 1;   ENDIF
      IF (ALLOCATED( SC_ELFN                        )) THEN;  NAME(509) = 'SC_ELFN                        ';  IA(509) = 1;   ENDIF
      IF (ALLOCATED( SC_GPFO                        )) THEN;  NAME(510) = 'SC_GPFO                        ';  IA(510) = 1;   ENDIF
      IF (ALLOCATED( SC_MPCF                        )) THEN;  NAME(511) = 'SC_MPCF                        ';  IA(511) = 1;   ENDIF
      IF (ALLOCATED( SC_OLOA                        )) THEN;  NAME(512) = 'SC_OLOA                        ';  IA(512) = 1;   ENDIF
      IF (ALLOCATED( SC_SPCF                        )) THEN;  NAME(513) = 'SC_SPCF                        ';  IA(513) = 1;   ENDIF
      IF (ALLOCATED( SC_STRE                        )) THEN;  NAME(514) = 'SC_STRE                        ';  IA(514) = 1;   ENDIF
      IF (ALLOCATED( SC_STRN                        )) THEN;  NAME(515) = 'SC_STRN                        ';  IA(515) = 1;   ENDIF
      IF (ALLOCATED( SCNUM                          )) THEN;  NAME(516) = 'SCNUM                          ';  IA(516) = 1;   ENDIF
      IF (ALLOCATED( SE1                            )) THEN;  NAME(517) = 'SE1                            ';  IA(517) = 1;   ENDIF
      IF (ALLOCATED( SE2                            )) THEN;  NAME(518) = 'SE2                            ';  IA(518) = 1;   ENDIF
      IF (ALLOCATED( SE3                            )) THEN;  NAME(519) = 'SE3                            ';  IA(519) = 1;   ENDIF
      IF (ALLOCATED( SELECT                         )) THEN;  NAME(520) = 'SELECT                         ';  IA(520) = 1;   ENDIF
      IF (ALLOCATED( SEQ1                           )) THEN;  NAME(521) = 'SEQ1                           ';  IA(521) = 1;   ENDIF
      IF (ALLOCATED( SEQ2                           )) THEN;  NAME(522) = 'SEQ2                           ';  IA(522) = 1;   ENDIF
      IF (ALLOCATED( SETS_IDS                       )) THEN;  NAME(523) = 'SETS_IDS                       ';  IA(523) = 1;   ENDIF
      IF (ALLOCATED( SLOAD_SIDS                     )) THEN;  NAME(524) = 'SLOAD_SIDS                     ';  IA(524) = 1;   ENDIF
      IF (ALLOCATED( SPC_SIDS                       )) THEN;  NAME(525) = 'SPC_SIDS                       ';  IA(525) = 1;   ENDIF
      IF (ALLOCATED( SPC1_SIDS                      )) THEN;  NAME(526) = 'SPC1_SIDS                      ';  IA(526) = 1;   ENDIF
      IF (ALLOCATED( SPC1_SIDS                      )) THEN;  NAME(527) = 'SPCSETS                        ';  IA(527) = 1;   ENDIF
      IF (ALLOCATED( SPCADD_SIDS                    )) THEN;  NAME(528) = 'SPCADD_SIDS                    ';  IA(528) = 1;   ENDIF
      IF (ALLOCATED( SPCSIDS                        )) THEN;  NAME(529) = 'SPCSIDS                        ';  IA(529) = 1;   ENDIF
      IF (ALLOCATED( STE1                           )) THEN;  NAME(530) = 'STE1                           ';  IA(530) = 1;   ENDIF
      IF (ALLOCATED( STE2                           )) THEN;  NAME(531) = 'STE2                           ';  IA(531) = 1;   ENDIF
      IF (ALLOCATED( STE3                           )) THEN;  NAME(532) = 'STE3                           ';  IA(532) = 1;   ENDIF
      IF (ALLOCATED( STF                            )) THEN;  NAME(533) = 'STF                            ';  IA(533) = 1;   ENDIF
      IF (ALLOCATED( STFCOL                         )) THEN;  NAME(534) = 'STFCOL                         ';  IA(534) = 1;   ENDIF
      IF (ALLOCATED( STFKEY                         )) THEN;  NAME(535) = 'STFKEY                         ';  IA(535) = 1;   ENDIF
      IF (ALLOCATED( STFPNT                         )) THEN;  NAME(536) = 'STFPNT                         ';  IA(536) = 1;   ENDIF
      IF (ALLOCATED( STF3                           )) THEN;  NAME(537) = 'STF3                           ';  IA(537) = 1;   ENDIF
      IF (ALLOCATED( STITLE                         )) THEN;  NAME(538) = 'STITLE                         ';  IA(538) = 1;   ENDIF
      IF (ALLOCATED( SUBLOD                         )) THEN;  NAME(539) = 'SUBLOD                         ';  IA(539) = 1;   ENDIF
      IF (ALLOCATED( SYS_LOAD                       )) THEN;  NAME(540) = 'SYS_LOAD                       ';  IA(540) = 1;   ENDIF
      IF (ALLOCATED( TDATA                          )) THEN;  NAME(541) = 'TDATA                          ';  IA(541) = 1;   ENDIF
      IF (ALLOCATED( TDOF                           )) THEN;  NAME(542) = 'TDOF                           ';  IA(542) = 1;   ENDIF
      IF (ALLOCATED( TDOF_ROW_START                 )) THEN;  NAME(543) = 'TDOF_ROW_START                 ';  IA(543) = 1;   ENDIF
      IF (ALLOCATED( TDOFI                          )) THEN;  NAME(544) = 'TDOFI                          ';  IA(544) = 1;   ENDIF
      IF (ALLOCATED( TEMPLATE                       )) THEN;  NAME(545) = 'TEMPLATE                       ';  IA(545) = 1;   ENDIF
      IF (ALLOCATED( TITLE                          )) THEN;  NAME(546) = 'TITLE                          ';  IA(546) = 1;   ENDIF
      IF (ALLOCATED( TN                             )) THEN;  NAME(547) = 'TN                             ';  IA(547) = 1;   ENDIF
      IF (ALLOCATED( TPNT                           )) THEN;  NAME(548) = 'TPNT                           ';  IA(548) = 1;   ENDIF
      IF (ALLOCATED( TR6_0                          )) THEN;  NAME(549) = 'TR6_0                          ';  IA(549) = 1;   ENDIF
      IF (ALLOCATED( TR6_CG                         )) THEN;  NAME(550) = 'TR6_CG                         ';  IA(550) = 1;   ENDIF
      IF (ALLOCATED( TR6_MEFM                       )) THEN;  NAME(551) = 'TR6_MEFM                       ';  IA(551) = 1;   ENDIF
      IF (ALLOCATED( TSET                           )) THEN;  NAME(552) = 'TSET                           ';  IA(552) = 1;   ENDIF
      IF (ALLOCATED( TXT_ACCE                       )) THEN;  NAME(553) = 'TXT_ACCE                       ';  IA(553) = 1;   ENDIF
      IF (ALLOCATED( TXT_DISP                       )) THEN;  NAME(554) = 'TXT_DISP                       ';  IA(554) = 1;   ENDIF
      IF (ALLOCATED( TXT_ELFE                       )) THEN;  NAME(555) = 'TXT_ELFE                       ';  IA(555) = 1;   ENDIF
      IF (ALLOCATED( TXT_ELFN                       )) THEN;  NAME(556) = 'TXT_ELFN                       ';  IA(556) = 1;   ENDIF
      IF (ALLOCATED( TXT_MPCF                       )) THEN;  NAME(557) = 'TXT_MPCF                       ';  IA(557) = 1;   ENDIF
      IF (ALLOCATED( TXT_SPCF                       )) THEN;  NAME(558) = 'TXT_SPCF                       ';  IA(558) = 1;   ENDIF
      IF (ALLOCATED( TXT_STRE                       )) THEN;  NAME(559) = 'TXT_STRE                       ';  IA(559) = 1;   ENDIF
      IF (ALLOCATED( TXT_STRN                       )) THEN;  NAME(560) = 'TXT_STRN                       ';  IA(560) = 1;   ENDIF
      IF (ALLOCATED( UA_COL                         )) THEN;  NAME(561) = 'UA_COL                         ';  IA(561) = 1;   ENDIF
      IF (ALLOCATED( UEB                            )) THEN;  NAME(562) = 'UEB                            ';  IA(562) = 1;   ENDIF
      IF (ALLOCATED( UEG                            )) THEN;  NAME(563) = 'UEG                            ';  IA(563) = 1;   ENDIF
      IF (ALLOCATED( UEL                            )) THEN;  NAME(564) = 'UEL                            ';  IA(564) = 1;   ENDIF
      IF (ALLOCATED( UF_COL                         )) THEN;  NAME(565) = 'UF_COL                         ';  IA(565) = 1;   ENDIF
      IF (ALLOCATED( UG_COL                         )) THEN;  NAME(566) = 'UG_COL                         ';  IA(566) = 1;   ENDIF
      IF (ALLOCATED( UGG                            )) THEN;  NAME(567) = 'UGG                            ';  IA(567) = 1;   ENDIF
      IF (ALLOCATED( UL_COL                         )) THEN;  NAME(568) = 'UL_COL                         ';  IA(568) = 1;   ENDIF
      IF (ALLOCATED( UM_COL                         )) THEN;  NAME(569) = 'UM_COL                         ';  IA(569) = 1;   ENDIF
      IF (ALLOCATED( UN_COL                         )) THEN;  NAME(570) = 'UN_COL                         ';  IA(570) = 1;   ENDIF
      IF (ALLOCATED( UO_COL                         )) THEN;  NAME(571) = 'UO_COL                         ';  IA(571) = 1;   ENDIF
      IF (ALLOCATED( UO0_COL                        )) THEN;  NAME(572) = 'UO0_COL                        ';  IA(572) = 1;   ENDIF
      IF (ALLOCATED( UR_COL                         )) THEN;  NAME(573) = 'UR_COL                         ';  IA(573) = 1;   ENDIF
      IF (ALLOCATED( US_COL                         )) THEN;  NAME(574) = 'US_COL                         ';  IA(574) = 1;   ENDIF
      IF (ALLOCATED( USERIN_ACT_GRIDS               )) THEN;  NAME(575) = 'USERIN_ACT_GRIDS               ';  IA(575) = 1;   ENDIF
      IF (ALLOCATED( USERIN_MAT_NAMES               )) THEN;  NAME(576) = 'USERIN_MAT_NAMES               ';  IA(576) = 1;   ENDIF
      IF (ALLOCATED( USET                           )) THEN;  NAME(577) = 'USET                           ';  IA(577) = 1;   ENDIF
      IF (ALLOCATED( VBAS                           )) THEN;  NAME(578) = 'VBAS                           ';  IA(578) = 1;   ENDIF
      IF (ALLOCATED( VVEC                           )) THEN;  NAME(579) = 'VVEC                           ';  IA(579) = 1;   ENDIF
      IF (ALLOCATED( WORKD                          )) THEN;  NAME(580) = 'WORKD                          ';  IA(580) = 1;   ENDIF
      IF (ALLOCATED( WORKL                          )) THEN;  NAME(581) = 'WORKL                          ';  IA(581) = 1;   ENDIF
      IF (ALLOCATED( XEB                            )) THEN;  NAME(582) = 'XEB                            ';  IA(582) = 1;   ENDIF
      IF (ALLOCATED( XEL                            )) THEN;  NAME(583) = 'XEL                            ';  IA(583) = 1;   ENDIF
      IF (ALLOCATED( XGL                            )) THEN;  NAME(584) = 'XGL                            ';  IA(584) = 1;   ENDIF
      IF (ALLOCATED( YSe                            )) THEN;  NAME(585) = 'YSe                            ';  IA(585) = 1;   ENDIF

      DO I=1,NUM_NAMES
         IF (IA(I) > 0) THEN
            WRITE(F06,101) NAME(I), LINKNO
         ENDIF
      ENDDO

      RETURN

! **********************************************************************************************************************************
  101 FORMAT(' *INFORMATION: Array ',A,' is allocated at the end of LINK',I3)

! **********************************************************************************************************************************

      END SUBROUTINE CHK_ARRAY_ALLOC_STAT

