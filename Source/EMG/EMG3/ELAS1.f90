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
 
      SUBROUTINE ELAS1 ( OPT, WRITE_WARN )
  
! Calculates 1-D scalar spring matrices in global coordinate system

!  1) SEi  = Element stress recovery matrices if OPT(3) = 'Y'
!  2) KE   = Element stiffness matrix in element coord's if OPT(4) = 'Y'
  
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  ELAS1_BEGEND
      USE MODEL_STUF, ONLY            :  AGRID, ELAS_COMP, EID, EPROP, FCONV, KE, SE1, TYPE
 
      USE ELAS1_USE_IFs

      IMPLICIT NONE 
  
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'ELAS1'
      CHARACTER(1*BYTE), INTENT(IN)   :: OPT(6)            ! 'Y'/'N' flags for whether to calc certain elem matrices
      CHARACTER(LEN=*), INTENT(IN)    :: WRITE_WARN        ! If 'Y" write warning messages, otherwise do not

      INTEGER(LONG)                   :: I1                ! The component no (1-6) at end A that this elem is connected to
      INTEGER(LONG)                   :: I2                ! The component no (1-6) at end B that this elem is connected to
      INTEGER(LONG)                   :: NUM_COMPS_GRID_1  ! No. displ components (1 for SPOINT, 6 for actual grid) for 1st grid
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = ELAS1_BEGEND
 
      REAL(DOUBLE)                    :: K                 ! Spring stiffness
      REAL(DOUBLE)                    :: GE                ! Material damping coeff
  
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
! Set element property and material constants
 
      K        = EPROP(1)
      GE       = EPROP(2)
      IF ((TYPE(1:5) == 'ELAS1') .OR. (TYPE(1:5) == 'ELAS3')) THEN
         FCONV(1) = EPROP(3)
      ELSE
         FCONV(1) = 1.D0 
      ENDIF
      I1       = ELAS_COMP(1)
      CALL GET_GRID_NUM_COMPS ( AGRID(1), NUM_COMPS_GRID_1, SUBR_NAME )
      I2       = NUM_COMPS_GRID_1 + ELAS_COMP(2)

! **********************************************************************************************************************************
! Calculate the element stiffness matrix in global coordinates.
 
      IF (OPT(4) == 'Y') THEN
         KE(I1,I1) =  K
         KE(I1,I2) = -KE(I1,I1)
         KE(I2,I1) = -KE(I1,I1)
         KE(I2,I2) =  KE(I1,I1)
      ENDIF

! **********************************************************************************************************************************
! Calculate SE1 matrix for stress recovery.
 
      IF (OPT(3) == 'Y') THEN
         SE1(1,I2,1) =  K*FCONV(1)
         SE1(1,I1,1) = -SE1(1,I2,1)
      ENDIF
 
! *********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! *********************************************************************************************************************************
 
      END SUBROUTINE ELAS1