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

   MODULE ARPACK_INFO_MSG_Interface

   INTERFACE

      SUBROUTINE ARPACK_INFO_MSG ( SUBNAME, INFO, IPARAM, LWORKL, NEV, NCV )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE SCONTR, ONLY                :  PROG_NAME, FATAL_ERR, NDOFL, WARN_ERR
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE PARAMS, ONLY                :  DARPACK, SUPWARN
      USE MODEL_STUF, ONLY            :  EIG_N2

      IMPLICIT NONE

      CHARACTER(  6*BYTE), INTENT(IN):: SUBNAME            ! Name of subr w/ msg

      INTEGER, INTENT(IN)            :: INFO               ! Error msg number from ARPACK routine to do eigenvalue/vec calc
      INTEGER, INTENT(IN)            :: IPARAM(11)         ! Integer array from subr DSBAND (in module ARPACK_LANCZOS_1)
      INTEGER, INTENT(IN)            :: LWORKL             ! Length of array WORKL in subr dsband
      INTEGER, INTENT(IN)            :: NEV                !
      INTEGER, INTENT(IN)            :: NCV                !

      END SUBROUTINE ARPACK_INFO_MSG

   END INTERFACE

   END MODULE ARPACK_INFO_MSG_Interface

