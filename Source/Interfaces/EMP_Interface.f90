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

   MODULE EMP_Interface

   INTERFACE

      SUBROUTINE EMP

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, F22, F22FIL, F22_MSG, SC1, WRT_BUG, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_ME_BIT, ELDT_F22_ME_BIT, FATAL_ERR, IBIT, LINKNO, LTERM_MGGE,   &
                                         MBUG, MELDOF, NDOFG, NELE, NGRID, NTERM_MGGE, NSUB
      USE TIMDAT, ONLY                :  TSEC
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  EPSIL, SPARSTOR
      USE SUBR_BEGEND_LEVELS, ONLY    :  EMP_BEGEND
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE MODEL_STUF, ONLY            :  AGRID, ELDT, ELDOF, ELGP, GRID_ID, NUM_EMG_FATAL_ERRS, ME, OELDT, PLY_NUM, TYPE
      USE EMS_ARRAYS, ONLY            :  EMS, EMSCOL, EMSKEY, EMSPNT
 
      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
 
      INTEGER(LONG)                   :: IDUM              ! Dummy variable used when flipping DOF's
      INTEGER(LONG)                   :: KSTART            ! Used in deciding whether to process all elem mass terms or only
      INTEGER(LONG)                   :: MAX_NUM           ! MAX of NTERM_MGGE/NDOFG (used for DEBUG printout)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = EMP_BEGEND
 
      END SUBROUTINE EMP

   END INTERFACE

   END MODULE EMP_Interface

