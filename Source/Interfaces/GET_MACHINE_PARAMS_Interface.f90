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

   MODULE GET_MACHINE_PARAMS_Interface

   INTERFACE

      SUBROUTINE GET_MACHINE_PARAMS


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, F04
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE CONSTANTS_1, ONLY           :  ONE
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_MACHINE_PARAMS_BEGEND
      USE MACHINE_PARAMS, ONLY        :  MACH_BASE, MACH_EMAX, MACH_EMIN, MACH_EPS, MACH_PREC, MACH_RMAX, MACH_RMIN, MACH_RND,     &
                                         MACH_SFMIN, MACH_T, MACH_LARGE_NUM
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE LAPACK_BLAS_AUX
 
      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_MACHINE_PARAMS_BEGEND

      END SUBROUTINE GET_MACHINE_PARAMS

   END INTERFACE

   END MODULE GET_MACHINE_PARAMS_Interface

