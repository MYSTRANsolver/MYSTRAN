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

   MODULE CONVERT_VEC_COORD_SYS_Interface

   INTERFACE

      SUBROUTINE CONVERT_VEC_COORD_SYS ( MESSAG, INPUT_VEC, OUTPUT_VEC, NCID )


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, NCORD, NDOFG, NGRID
      USE TIMDAT, ONLY                :  TSEC
      USE MODEL_STUF, ONLY            :  CORD, RCORD, GRID, GRID_ID, INV_GRID_SEQ
      USE SUBR_BEGEND_LEVELS, ONLY    :  CONVERT_VEC_COORD_SYS_BEGEND

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN)    :: MESSAG            ! Text description of INPUT_VEC in case of undefined NCID

      INTEGER(LONG), INTENT(IN)       :: NCID              ! Actual coord system number. INPUT_VEC is to be transformed to this sys.
      INTEGER(LONG)                   :: JFLD              ! Used in error message to indicate a coord sys ID undefined
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = CONVERT_VEC_COORD_SYS_BEGEND

      REAL(DOUBLE), INTENT(IN)        :: INPUT_VEC(NDOFG)  ! G-set input vector to be transformed from global to NCID
      REAL(DOUBLE), INTENT(OUT)       :: OUTPUT_VEC(NDOFG) ! Transformed output vector
      REAL(DOUBLE)                    :: PHID              ! Dummy arg for subr GEN_T0L that is not used here
      REAL(DOUBLE)                    :: THETAD            ! Dummy arg for subr GEN_T0L that is not used here

      END SUBROUTINE CONVERT_VEC_COORD_SYS

   END INTERFACE

   END MODULE CONVERT_VEC_COORD_SYS_Interface

