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

   MODULE GP_FORCE_BALANCE_PROC_Interface

   INTERFACE

      SUBROUTINE GP_FORCE_BALANCE_PROC ( JVEC, IHDR )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, SHORT, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ANS, ERR, F04, F06, SC1, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, GROUT_GPFO_BIT, IBIT, INT_SC_NUM, JTSUB, NDOFG, NDOFM, MELDOF, NDOFO, NDOFR,&
                                         NELE, NGRID, NUM_CB_DOFS, NVEC, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GP_FORCE_BALANCE_PROC_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE_HUNDRED
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE MODEL_STUF, ONLY            :  AGRID, EID, ELGP, ESORT1, ETYPE, NUM_EMG_FATAL_ERRS, GRID, GRID_ELEM_CONN_ARRAY, GRID_ID, &
                                         GROUT, LABEL, PLY_NUM, PEG, PTE, SCNUM, STITLE, SUBLOD, TITLE, TYPE
      USE LINK9_STUFF, ONLY           :  GID_OUT_ARRAY
      USE COL_VECS, ONLY              :  FG_COL, PG_COL, QGm_COL, QGs_COL, QGr_COL, UG_COL
      USE PARAMS, ONLY                :  EPSIL

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=*) , INTENT(IN)   :: IHDR              ! Indicator of whether to write an output header
      CHARACTER( 1*BYTE), PARAMETER   :: COORD_SYS   = 'G' ! Subr TRANSFORM_NODE_FORCES will calc elem node forces in global coords

      INTEGER(LONG), INTENT(IN)       :: JVEC              ! Solution vector number
      INTEGER(LONG)                   :: IB                ! Intermediate value used in determining NREQ
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GP_FORCE_BALANCE_PROC_BEGEND

      END SUBROUTINE GP_FORCE_BALANCE_PROC

   END INTERFACE

   END MODULE GP_FORCE_BALANCE_PROC_Interface

