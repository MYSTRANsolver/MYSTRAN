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

      MODULE LINK0_USE_IFs

! USE Interface statements for all subroutines called by SUBROUTINE LINK0

      USE TIME_INIT_Interface
      USE OURDAT_Interface
      USE OURTIM_Interface
      USE LOADC0_Interface
      USE LOADB0_Interface
      USE ALLOCATE_IN4_FILES_Interface
      USE LOADE_Interface
      USE ALLOCATE_NL_PARAMS_Interface
      USE SET_FILE_CLOSE_STAT_Interface
      USE ALLOCATE_MODEL_STUF_Interface
      USE LOADC_Interface
      USE READ_L1Z_Interface
      USE LINK1_RESTART_DATA_Interface
      USE LOADB_RESTART_Interface
      USE SET_SPARSE_MAT_SYM_Interface
      USE PRINT_CONSTANTS_1_Interface
      USE GET_MACHINE_PARAMS_Interface
      USE PRINT_ORDER_Interface
      USE FILE_OPEN_Interface
      USE LOADB_Interface
      USE DEALLOCATE_MODEL_STUF_Interface
      USE WRITE_EDAT_Interface
      USE OUTA_HERE_Interface
      USE WRITE_ENF_TO_L1O_Interface
      USE FILE_CLOSE_Interface
      USE ELESORT_Interface
      USE ELEM_PROP_MATL_IIDS_Interface
      USE GRID_PROC_Interface
      USE GET_ELEM_AGRID_BGRID_Interface
      USE SEQ_PROC_Interface
      USE SUBCASE_PROC_Interface
      USE GRID_ELEM_CONN_TABLE_Interface
      USE CORD_PROC_Interface
      USE WRITE_GRID_COORDS_Interface
      USE ALLOCATE_DOF_TABLES_Interface
      USE READ_DOF_TABLES_Interface
      USE CALC_TDOF_ROW_START_Interface
      USE WRITE_TSET_Interface
      USE WRITE_TDOF_Interface
      USE EMG_Interface
      USE DOF_PROC_Interface
      USE WRITE_USETSTR_Interface
      USE CONM2_PROC_1_Interface
      USE ALLOCATE_RBGLOBAL_Interface
      USE GPWG_USERIN_Interface
      USE GPWG_Interface
      USE ALLOCATE_SPARSE_MAT_Interface
      USE READ_MATRIX_1_Interface
      USE PRT_MATS_ON_RESTART_Interface
      USE DEALLOCATE_SPARSE_MAT_Interface
      USE RB_DISP_MATRIX_PROC_Interface
      USE GET_MATRIX_DIAG_STATS_Interface
      USE STIFF_MAT_EQUIL_CHK_Interface
      USE CONM2_PROC_2_Interface
      USE TEMPERATURE_DATA_PROC_Interface
      USE PRESSURE_DATA_PROC_Interface
      USE TDOF_COL_NUM_Interface
      USE ALLOCATE_COL_VEC_Interface
      USE YS_ARRAY_Interface
      USE DEALLOCATE_COL_VEC_Interface
      USE WRITE_L1Z_Interface
      USE OUTPUT4_MATRIX_MSGS_Interface
      USE WRITE_L1A_Interface
      USE CHK_ARRAY_ALLOC_STAT_Interface
      USE WRITE_ALLOC_MEM_TABLE_Interface
      USE FILE_INQUIRE_Interface
      USE WRITE_FIJFIL_Interface

      END MODULE LINK0_USE_IFs
