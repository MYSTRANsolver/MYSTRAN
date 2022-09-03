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

   MODULE ELMOUT_Interface

   INTERFACE

      SUBROUTINE ELMOUT ( INT_ELEM_ID, DUM_BUG, CASE_NUM, OPT )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, BUG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ELDT_BUG_DAT1_BIT, ELDT_BUG_DAT2_BIT, ELDT_BUG_ME_BIT, ELDT_BUG_P_T_BIT,  &
                                         ELDT_BUG_SE_BIT, ELDT_BUG_KE_BIT, ELDT_BUG_U_P_BIT, MBUG, MDT, MELGP, METYPE,             &
                                         MEMATR, MEMATC, MEPROP, MPRESS, NSUB, NTSUB, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELMOUT_BEGEND
      USE CONSTANTS_1, ONLY           :  CONV_RAD_DEG, ZERO
      USE PARAMS, ONLY                :  CBMIN3, CBMIN4, ELFORCEN, QUADAXIS, QUAD4TYP
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE MODEL_STUF, ONLY            :  AGRID, BGRID, BE1, BE2, BE3, BENSUM, BMEANT, CAN_ELEM_TYPE_OFFSET, DOFPIN, DT, ELAS_COMP, &
                                         EID, EB, EM, ES, ET, ELEM_LEN_AB, ELDOF, ELMTYP, ELGP, EMAT, EPROP, FCONV, HBAR, KE, KED, &
                                         ME, MXWARP, NUM_PLIES, NUM_SEi, OFFDIS, OFFSET, PCOMP_PROPS, PEB, PEG, PEL, PHI_SQ,       &
                                         PPE, PRESS, PSI_HAT, PTE, QUAD_DELTA, QUAD_GAMMA, QUAD_THETA, SE1, SE2, SE3,              &
                                         SHELL_T, SHRSUM, STE1, STE2, STE3, THETAM, TE, TYPE, UEB, UEG, UEL, XEB, XEL, SCNUM,      &
                                         SUBLOD, ULT_STRE, ULT_STRN
      IMPLICIT NONE
 
      CHARACTER(LEN=*), INTENT(IN)    :: OPT(6)              ! Array of EMG option indicators explained above
      CHARACTER( 1*BYTE)              :: FOUND               ! Used in determining if we found something we were looking for
      CHARACTER(60*BYTE)              :: NAME1               ! Text used for output print purposes
      CHARACTER(21*BYTE)              :: NAME2               ! Text used for output print purposes
      CHARACTER(12*BYTE)              :: NAME3               ! Text used for output print purposes
  
      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID         ! Internal element ID for which
      INTEGER(LONG), INTENT(IN)       :: CASE_NUM            ! Can be subcase number (e.g. for UEL, PEL output)
      INTEGER(LONG), INTENT(IN)       :: DUM_BUG(0:MBUG-1)   ! Indicator for output of elem data to BUG file
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELMOUT_BEGEND

      END SUBROUTINE ELMOUT

   END INTERFACE

   END MODULE ELMOUT_Interface

