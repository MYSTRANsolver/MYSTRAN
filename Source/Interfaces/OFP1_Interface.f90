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

   MODULE OFP1_Interface

   INTERFACE

      SUBROUTINE OFP1 ( JVEC, WHAT, SC_OUT_REQ, FEMAP_SET_ID, ITG, OT4_GROW, ITABLE, NEW_RESULT )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, OT4
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, GROUT_ACCE_BIT, GROUT_DISP_BIT, GROUT_OLOA_BIT, IBIT, INT_SC_NUM,&
                                         MELGP, MOGEL, NGRID, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  OFP1_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  OTMSKIP, POST
      USE DOF_TABLES, ONLY            :  TDOF, TDOF_ROW_START
      USE MODEL_STUF, ONLY            :  ANY_ACCE_OUTPUT, ANY_DISP_OUTPUT, ANY_OLOA_OUTPUT, GROUT, GRID, GRID_ID
      USE LINK9_STUFF, ONLY           :  GID_OUT_ARRAY, MAXREQ, OGEL
      USE COL_VECS, ONLY              :  UG_COL, UG0_COL, PG_COL, PHIXG_COL, PHIXN_COL
      USE OUTPUT4_MATRICES, ONLY      :  OTM_ACCE, OTM_DISP, TXT_ACCE, TXT_DISP
      USE CC_OUTPUT_DESCRIBERS, ONLY  :  ACCE_OUT, DISP_OUT, OLOA_OUT

      IMPLICIT NONE

      CHARACTER(LEN=*) , INTENT(IN)   :: WHAT              ! Indicator whether to process displ or force output requests
      CHARACTER( 1*BYTE), PARAMETER   :: IHDR      = 'Y'   ! An input to subr WRITE_GRD_PRT_OUTPUTS, called herein
      CHARACTER( 1*BYTE)              :: WRITE_OGEL(NGRID) ! 'Y'/'N' as to whether to write OGEL for a grid (used to avoid writing
      INTEGER(LONG), INTENT(IN)       :: FEMAP_SET_ID      ! Set ID for FEMAP output
      INTEGER(LONG), INTENT(IN)       :: ITG               ! Unit number for text files for OTM row descriptors 
      INTEGER(LONG), INTENT(IN)       :: JVEC              ! Solution vector number
      INTEGER(LONG), INTENT(IN)       :: SC_OUT_REQ        ! If > 0, then req1uests for WHAT are to be output
      INTEGER(LONG), INTENT(INOUT)    :: OT4_GROW          ! Row number in OT4 file for grid related OTM descriptors
      INTEGER(LONG), INTENT(INOUT)    :: ITABLE            ! 
      LOGICAL, INTENT(INOUT)          :: NEW_RESULT        ! is this the first result of a table
      INTEGER(LONG)                   :: NREQ              ! Number of user requested outputs of displ/force
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = OFP1_BEGEND

      END SUBROUTINE OFP1

   END INTERFACE

   END MODULE OFP1_Interface

