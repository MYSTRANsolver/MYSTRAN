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

   MODULE IS_ELEM_PCOMP_PROPS_Interface

   INTERFACE

      SUBROUTINE IS_ELEM_PCOMP_PROPS ( INT_ELEM_ID )

 
      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE SCONTR, ONLY                :  DEDAT_T3_SHELL_KEY, DEDAT_Q4_SHELL_KEY 
      USE MODEL_STUF, ONLY            :  EDAT, EPNT, ETYPE, PCOMP_PROPS, TYPE

      IMPLICIT NONE

      INTEGER(LONG), INTENT(IN)       :: INT_ELEM_ID        ! Internal element ID for which
      END SUBROUTINE IS_ELEM_PCOMP_PROPS

   END INTERFACE

   END MODULE IS_ELEM_PCOMP_PROPS_Interface

