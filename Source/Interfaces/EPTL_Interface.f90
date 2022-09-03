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

   MODULE EPTL_Interface

   INTERFACE

      SUBROUTINE EPTL

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  ERR, F04, F06, F21, F21FIL, F21_MSG, SC1, WRT_BUG, WRT_ERR, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_P_T_BIT, ELDT_F21_P_T_BIT, IBIT, LINKNO, MBUG, MELDOF, NCORD,      &
                                         NELE, NGRID, NSUB, NTSUB
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  EPSIL
      USE TIMDAT, ONLY                :  TSEC
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE SUBR_BEGEND_LEVELS, ONLY    :  EPTL_BEGEND
      USE MODEL_STUF, ONLY            :  ELDOF, ELDT, GRID, GRID_ID, CORD, AGRID, ELGP, NUM_EMG_FATAL_ERRS, OELDT, PLY_NUM, PPE,   &
                                         PTE, SYS_LOAD, TYPE, SUBLOD

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT

      INTEGER(LONG)                   :: I1                ! Intermediate variable used in setting WRT_BUG(3) and OUT10
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = EPTL_BEGEND

      END SUBROUTINE EPTL

   END INTERFACE

   END MODULE EPTL_Interface

