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

   MODULE LOADE_Interface

   INTERFACE

      SUBROUTINE LOADE

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, IN1
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, EC_ENTRY_LEN, CHKPNT, FATAL_ERR, WARN_ERR, JCARD_LEN, JF,     &
                                         PROG_NAME, SOL_NAME, RESTART
      USE TIMDAT, ONLY                :  TSEC
      USE PARAMS, ONLY                :  SUPWARN
      use debug_parameters, only      :  debug

      USE OUTPUT4_MATRICES, ONLY      :  ACT_OU4_MYSTRAN_NAMES, ACT_OU4_OUTPUT_NAMES, ALLOW_OU4_MYSTRAN_NAMES,                     &
                                         ALLOW_OU4_OUTPUT_NAMES, OU4_PART_MAT_NAMES, OU4_PART_VEC_NAMES, NUM_OU4_VALID_NAMES

      USE SUBR_BEGEND_LEVELS, ONLY    :  LOADE_BEGEND
 
      IMPLICIT NONE
 
      CHARACTER(LEN=JCARD_LEN)        :: CHARFLD           ! Character field used when suvr I4FLD is called
      CHARACTER( 4*BYTE), PARAMETER   :: END_CARD  = 'CEND'

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = LOADE_BEGEND
 
      END SUBROUTINE LOADE

   END INTERFACE

   END MODULE LOADE_Interface

