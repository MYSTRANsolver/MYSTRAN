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

   MODULE PRESSURE_DATA_PROC_Interface

   INTERFACE

      SUBROUTINE PRESSURE_DATA_PROC

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR,     F04,     F06,     L1Q
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG,                            LINK1Q
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG,                            L1Q_MSG
      USE SCONTR, ONLY                :  BD_ENTRY_LEN, BLNK_SUB_NAM, DATA_NAM_LEN, FATAL_ERR, JCARD_LEN, LPDAT, LLOADC,            &
                                         MPDAT_PLOAD1, MPDAT_PLOAD2, MPDAT_PLOAD4, MPLOAD4_3D_DATA, NELE, NLOAD, NPCARD,           &
                                         NPLOAD4_3D, NPDAT, NSUB, WARN_ERR
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      USE SUBR_BEGEND_LEVELS, ONLY    :  PRESSURE_DATA_PROC_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE MODEL_STUF, ONLY            :  LOAD_SIDS, LOAD_FACS, SUBLOD, PDATA, PPNT, PLOAD4_3D_DATA, PTYPE
 
      IMPLICIT NONE
 
      CHARACTER( 8*BYTE)              :: TOKTYP            ! Variable to test whether "THRU" option was used on B.D. PLOAD2 card
      CHARACTER( 8*BYTE)              :: THRU              ! ='Y' if THRU option used on TEMPRB, TEMPP1 continuation card
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = PRESSURE_DATA_PROC_BEGEND
 
      END SUBROUTINE PRESSURE_DATA_PROC

   END INTERFACE

   END MODULE PRESSURE_DATA_PROC_Interface

