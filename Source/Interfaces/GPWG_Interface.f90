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

   MODULE GPWG_Interface

   INTERFACE

      SUBROUTINE GPWG ( WHICH )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_BUG, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_ME_BIT, IBIT, MBUG, NCONM2, NCORD, NELE, NGRID, SOL_NAME, WARN_ERR
      USE PARAMS, ONLY                :  EPSIL, GRDPNT, MEFMGRID, MEFMLOC, SUPWARN, WTMASS
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GPWG_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MODEL_STUF, ONLY            :  AGRID, BGRID, CONM2, CORD, CAN_ELEM_TYPE_OFFSET, ELDT, ELGP, NUM_EMG_FATAL_ERRS,          &
                                         GRID, GRID_ID, MCG, ME, MEFFMASS_CALC, MEFM_RB_MASS,                                      &
                                         MODEL_MASS, MODEL_IXX, MODEL_IYY, MODEL_IZZ, MODEL_XCG, MODEL_YCG, MODEL_ZCG,             &
                                         OFFDIS, OFFSET, PLY_NUM, RCONM2, RGRID, TYPE, USERIN_RBM0
 
      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(12*BYTE), INTENT(IN)  :: WHICH             ! Whether to get mass props for
      INTEGER(LONG)                   :: JDOF              ! Array index used in getting mass terms from the elem mass matrix, ME
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GPWG_BEGEND

      REAL(DOUBLE)                    :: M0                ! An intermediate variable used in calc model mass props
 
      END SUBROUTINE GPWG

   END INTERFACE

   END MODULE GPWG_Interface

