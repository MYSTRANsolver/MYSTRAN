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

   MODULE GET_PCOMP_SECT_PROPS_Interface

   INTERFACE

      SUBROUTINE GET_PCOMP_SECT_PROPS ( PCOMP_TM, PCOMP_IB, PCOMP_TS )

 
      USE PENTIUM_II_KIND, ONLY       :  LONG, DOUBLE
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, MPCOMP_PLIES, MPCOMP0, MRPCOMP_PLIES, MRPCOMP0
      USE IOUNT1, ONLY                :  F04, WRT_LOG
      USE MODEL_STUF, ONLY            :  EPROP, INTL_PID, NUM_PLIES, RPCOMP, TPLY
      USE PARAMS, ONLY                :  PCMPTSTM 
      USE CONSTANTS_1, ONLY           :  ZERO, THIRD
      USE TIMDAT, ONLY                :  TSEC 
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_PCOMP_SECT_PROPS_BEGEND

      IMPLICIT NONE
 
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_PCOMP_SECT_PROPS_BEGEND

      REAL(DOUBLE), INTENT(OUT)       :: PCOMP_TM           ! Membrane thickness of PCOMP for equivalent PSHELL
      REAL(DOUBLE), INTENT(OUT)       :: PCOMP_IB           ! Bending MOI of PCOMP for equivalent PSHELL
      REAL(DOUBLE), INTENT(OUT)       :: PCOMP_TS           ! Transverse shear thickness of PCOMP for equivalent PSHELL
 
      END SUBROUTINE GET_PCOMP_SECT_PROPS

   END INTERFACE

   END MODULE GET_PCOMP_SECT_PROPS_Interface

