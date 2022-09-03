! ###############################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2019 Dr William R Case, Jr (dbcase29@gmail,com)                                              
                                                                                                         
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

   MODULE GET_MATANGLE_FROM_CID_Interface

   INTERFACE

      SUBROUTINE GET_MATANGLE_FROM_CID ( ACID )

 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NCORD
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  CONV_DEG_RAD, ZERO, ONE
      USE PARAMS, ONLY                :  EPSIL
      USE MODEL_STUF, ONLY            :  CORD, EID, NUM_EMG_FATAL_ERRS, NUM_EMG_FATAL_ERRS, RCORD, TE, THETAM, TYPE
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_MATANGLE_FROM_CID_BEGEND
 
      IMPLICIT NONE
 
      INTEGER(LONG), INTENT(IN)       :: ACID              ! Actual coord system ID for the sys that defines the material axes
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_MATANGLE_FROM_CID_BEGEND
 
      END SUBROUTINE GET_MATANGLE_FROM_CID

   END INTERFACE

   END MODULE GET_MATANGLE_FROM_CID_Interface

