! ###############################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2022 Dr William R Case, Jr (mystransolver@gmail.com)                                              
                                                                                                         
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

   MODULE BD_SLOAD_Interface

   INTERFACE

      SUBROUTINE BD_SLOAD ( CARD, CC_LOAD_FND )

  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1W
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, ECHO, FATAL_ERR, IERRFL, JCARD_LEN, JF, LFORCE, LSUB, NFORCE, NSLOAD, NSUB
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  BD_SLOAD_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE PARAMS, ONLY                :  EPSIL, SUPWARN
      USE MODEL_STUF, ONLY            :  SLOAD_SIDS, SUBLOD
 
      IMPLICIT NONE
 
      CHARACTER(LEN=*),INTENT(IN)     :: CARD                ! A Bulk Data card
      CHARACTER( 1*BYTE),INTENT(INOUT):: CC_LOAD_FND(LSUB,2) ! 'Y' if B.D SLOAD card w/ same set ID (SID) as C.C. LOAD = SID
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = BD_SLOAD_BEGEND
  
      END SUBROUTINE BD_SLOAD

   END INTERFACE

   END MODULE BD_SLOAD_Interface

