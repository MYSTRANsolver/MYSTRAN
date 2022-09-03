! ##################################################################################################################################
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

      SUBROUTINE GET_COMP_SHELL_ALLOWS ( STRE_ALLOWABLES, STRN_ALLOWABLES )

! Gets allowable stresses and strains for a composite element ply. Arrays ULT_STRE, ULT_STRN were formulated from user supplied
! data on the MATi Bulk Data entries in material processing subrs called by subr EMG.

      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, F04
      USE TIMDAT, ONLY                :  TSEC
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE MACHINE_PARAMS, ONLY        :  MACH_LARGE_NUM
      USE MODEL_STUF, ONLY            :  ULT_STRE, ULT_STRN
      USE SUBR_BEGEND_LEVELS, ONLY    :  GET_COMP_SHELL_ALLOWS_BEGEND

      USE GET_COMP_SHELL_ALLOWS_USE_IFs

      IMPLICIT NONE

      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GET_COMP_SHELL_ALLOWS'

      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GET_COMP_SHELL_ALLOWS_BEGEND

      REAL(DOUBLE), INTENT(OUT)       :: STRE_ALLOWABLES(9)! Stress allowables for the material
      REAL(DOUBLE), INTENT(OUT)       :: STRN_ALLOWABLES(9)! Strain allowables for the material

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
      STRE_ALLOWABLES(1) = ULT_STRE(1,1)                !   Axis   1 tension     stress allowable
      STRE_ALLOWABLES(2) = ULT_STRE(2,1)                !   Axis   1 compression stress allowable
      STRE_ALLOWABLES(3) = ULT_STRE(1,1)                !   Axis   2 tension     stress allowable
      STRE_ALLOWABLES(4) = ULT_STRE(2,1)                !   Axis   2 compression stress allowable
      STRE_ALLOWABLES(5) = MACH_LARGE_NUM               !   Axis   3 tension     stress allowable
      STRE_ALLOWABLES(6) = MACH_LARGE_NUM               !   Axis   3 compression stress allowable
      STRE_ALLOWABLES(7) = ULT_STRE(9,3)                !   Plane 23 shear       stress allowable (from transv shear matl props)
      STRE_ALLOWABLES(8) = ULT_STRE(8,3)                !   Plane 13 shear       stress allowable (from transv shear matl props)
      STRE_ALLOWABLES(9) = ULT_STRE(7,1)                !   Plane 12 shear       stress allowable

      STRN_ALLOWABLES(1) = ULT_STRN(1,1)                !   Axis   1 tension     strain allowable
      STRN_ALLOWABLES(2) = ULT_STRN(2,1)                !   Axis   1 compression strain allowable
      STRN_ALLOWABLES(3) = ULT_STRN(1,1)                !   Axis   2 tension     strain allowable
      STRN_ALLOWABLES(4) = ULT_STRN(2,1)                !   Axis   2 compression strain allowable
      STRN_ALLOWABLES(5) = MACH_LARGE_NUM               !   Axis   3 tension     strain allowable
      STRN_ALLOWABLES(6) = MACH_LARGE_NUM               !   Axis   3 compression strain allowable
      STRN_ALLOWABLES(7) = ULT_STRN(9,3)                !   Plane 23 shear       strain allowable (from transv shear matl props)
      STRN_ALLOWABLES(8) = ULT_STRN(8,3)                !   Plane 13 shear       strain allowable (from transv shear matl props)
      STRN_ALLOWABLES(9) = ULT_STRN(7,1)                !   Plane 12 shear       strain allowable

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************

      END SUBROUTINE GET_COMP_SHELL_ALLOWS

