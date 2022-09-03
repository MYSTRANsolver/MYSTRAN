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

   MODULE ESP_Interface

   INTERFACE

      SUBROUTINE ESP

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, F23, F23FIL, F23_MSG, F24, F24FIL, F24_MSG, FILE_NAM_MAXLEN, SC1, SCR,     &
                                         WRT_BUG, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_KE_BIT, ELDT_BUG_SE_BIT,                                           &
                                         ELDT_F23_KE_BIT, ELDT_F24_SE_BIT, ELDT_BUG_BCHK_BIT, ELDT_BUG_BMAT_BIT, ELDT_BUG_SHPJ_BIT,&
                                         FATAL_ERR, IBIT, LINKNO, LTERM_KGG, LTERM_KGGD, MBUG, MELDOF, NDOFG, NELE, NGRID,         &
                                         NTERM_KGG, NTERM_KGGD, NSUB, SOL_NAME
      USE PARAMS, ONLY                :  EPSIL, SPARSTOR
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ZERO
      USE SUBR_BEGEND_LEVELS, ONLY    :  ESP_BEGEND
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE MODEL_STUF, ONLY            :  AGRID, ELDT, ELDOF, ELGP, GRID_ID, NUM_EMG_FATAL_ERRS, PLY_NUM, OELDT, KE, KED, TYPE
      USE STF_ARRAYS, ONLY            :  STFKEY, STF3
      USE STF_TEMPLATE_ARRAYS, ONLY   :  CROW, TEMPLATE
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
 
      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
 
      INTEGER(LONG), PARAMETER        :: DEB_NUM   = 46    ! Debug number for output error message
      INTEGER(LONG)                   :: IDUM              ! Dummy variable used when flipping DOF's
      INTEGER(LONG)                   :: KSTART            ! Used in deciding whether to process all elem stiffness terms or only
      INTEGER(LONG)                   :: MAX_NUM           ! MAX of NTERM_KGG/NDOFG (used for DEBUG printout)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ESP_BEGEND

      END SUBROUTINE ESP

   END INTERFACE

   END MODULE ESP_Interface

