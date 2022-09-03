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
 
      SUBROUTINE TSET_PROC
 
! Generate TSET table. The displ set definitions are:

!   G---------->N + M (M = DOF's identified as dependent on MPC's or rigid elements)
!               |           
!               |           
!               |           
!               |           
!               |---------->F + S (S is SZ (SPC'd to 0 displ) or SE (enforced displ))
!                           |           SZ can be either: SB (defined on SPC/SPC1 cards) or
!                           |                             SG (defined on GRID cards) or
!                           |                             SA (based on AUTOSPC)
!                           |
!                           |---------->A + O (O = DOF's omitted via Guyan reduction)
!                                       |
!                                       |           
!                                       |           
!                                       |---------->L + R (R = DOF on SUPORT B.D. entries))

! Every DOF is in either: M (MPC'd via MPC eqns or rigid elements), or
!                         S (SPC'd),or
!                         O (Omitted via Guyan Reduction),or
!                         R (SUPORT'd), or
!                         L (Left over)

! TSET is a table that defines which displ set each grid/component pair belongs to. It has: R, A, O, (SG, SB, SE) or M for each DOF.
! The table has NGRID rows and 6 columns (1 col for each of the 6 components of displ at a grid). An example of a TSET table written
! to the F06 file is shown below:
 
!                                      DEGREE OF FREEDOM SET TABLE (TSET)

!             GRID   GRID_SEQ   INV_GRID_SEQ*      T1       T2       T3       R1       R2       R3

!             101           4              7       L        L        L        SG       SG       SG
!             102           5              5       M        M        M        L        L        L 
!             103           3              3       L        L        L        L        L        L 
!             104           6              1       L        L        L        L        L        L 
!             105           2              2       L        L        L        L        L        L 
!             106           7              4       L        L        L        O        O        O 
!             107           1              6       SE       SG       SG       SG       SG       SG

!              * INV_GRID_SEQ = J meams that the J-th entry under GRID is sequenced GRID_SEQ(J)


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG
      USE IOUNT1, ONLY                :  ERR, F04, F06, SC1, WRT_LOG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, NGRID, NAOCARD, NUM_SUPT_CARDS,                                  &
                                         NDOFL, NDOFM, NDOFO, NDOFR, NDOFS, NDOFSA, NDOFSG, NDOFSB, NDOFSE, NDOFSZ
      USE PARAMS, ONLY                :  PRTTSET
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  DOF_PROC_BEGEND
      USE DOF_TABLES, ONLY            :  TSET
      USE MODEL_STUF, ONLY            :  GRID
 
      USE TSET_PROC_USE_IFs

      IMPLICIT NONE

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'TSET_PROC'
 
      INTEGER(LONG)                   :: I,K               ! DO loop indices
      INTEGER(LONG)                   :: IERRT     = 0     ! Sum of all grid and DOF errors
      INTEGER(LONG)                   :: NUM_COMPS         ! Number of displ components (1 for SPOINT, 6 for physical grid)
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = DOF_PROC_BEGEND
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages         

! ----------------------------------------------------------------------------------------------------------------------------------
! TSET for cols 2-6 are not defined for SPOINT's, so set = '--'

      WRITE(SC1,12345,ADVANCE='NO') '       Initializing           ', CR13
      DO I=1,NGRID
         CALL GET_GRID_NUM_COMPS ( GRID(I,1), NUM_COMPS, SUBR_NAME )
         IF (NUM_COMPS == 1) THEN
            DO K=2,6
               TSET(I,K) = '--'
            ENDDO
         ENDIF
      ENDDO

! ----------------------------------------------------------------------------------------------------------------------------------
! Process M-set (MPC's and rigid elements)

      NDOFM = 0
      WRITE(SC1,12345,ADVANCE='NO') '       Process MPCs           ', CR13
      CALL TSET_PROC_FOR_MPCS ( IERRT )                    ! Process MPC data from file L1S

      WRITE(SC1,12345,ADVANCE='NO') '       Process Rigid Elements', CR13
      CALL TSET_PROC_FOR_RIGELS ( IERRT )                  ! Process the Rigid Element data from file L1F

! ----------------------------------------------------------------------------------------------------------------------------------
! Process S-set (SPC's)

      NDOFSG  = 0
      NDOFSB  = 0
      NDOFSE  = 0
      WRITE(SC1,12345,ADVANCE='NO') '       Process SPCs           ', CR13
      CALL TSET_PROC_FOR_SPCS ( IERRT )                    ! Process SPC data from file L1O
      NDOFSZ = NDOFSA + NDOFSB + NDOFSG
      NDOFS  = NDOFSZ + NDOFSE 

! ----------------------------------------------------------------------------------------------------------------------------------
! Process O-set (OMIT's/ASET's)

      NDOFO = 0
      IF (NAOCARD == 0) THEN                               ! If no ASET,1/OMIT,1 cards, then, for time being, set all remaining DOF
         DO I = 1,NGRID                                    ! to A-set  (if there are SUPORT's some will get changed to R set below)
            CALL GET_GRID_NUM_COMPS ( GRID(I,1), NUM_COMPS, SUBR_NAME )
            DO K = 1,NUM_COMPS
               IF (TSET(I,K) == '  ') THEN
                  TSET(I,K) = 'A '   
               ENDIF
            ENDDO
         ENDDO
      ELSE
         WRITE(SC1,12345,ADVANCE='NO') '       Process OMITs          ', CR13
         CALL TSET_PROC_FOR_OMITS ( IERRT )
      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
! Check that all grid/components have been set

      DO I=1,NGRID
         DO K=1,6
            IF (TSET(I,K) == '  ') THEN
               WRITE(ERR,1368) SUBR_NAME,GRID(I,1),K
               WRITE(F06,1368) SUBR_NAME,GRID(I,1),K
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )
            ENDIF
         ENDDO 
      ENDDO 

! ----------------------------------------------------------------------------------------------------------------------------------
! Process R-set (SUPORT's)

      NDOFR = 0
      IF (NUM_SUPT_CARDS > 0) THEN
         WRITE(SC1,12345,ADVANCE='NO') '       Process SUPORTs        ', CR13
         CALL TSET_PROC_FOR_SUPORTS ( IERRT )
      ENDIF

! ----------------------------------------------------------------------------------------------------------------------------------
! All remaining A set are also L set

      NDOFL = 0
      DO I = 1,NGRID
         DO K = 1,6
            IF (TSET(I,K) == 'A ') THEN
               TSET(I,K) = 'L '
               NDOFL = NDOFL + 1   
            ENDIF
         ENDDO
      ENDDO

! ----------------------------------------------------------------------------------------------------------------------------------
! Print the TSET table, if requested

      IF (PRTTSET == 1) THEN
         CALL WRITE_TSET
      ENDIF
 
! ----------------------------------------------------------------------------------------------------------------------------------
      WRITE(SC1,*) CR13 

! Quit if there were errors

      IF (IERRT > 0) THEN
         WRITE(ERR,9996) SUBR_NAME,IERRT
         WRITE(F06,9996) SUBR_NAME,IERRT
         CALL OUTA_HERE ( 'Y' )
      ENDIF         

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! *********************************************************************************************************************************
 1368 FORMAT(' *ERROR  1368: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' GRID POINT ',I8,' COMPONENT ',I2,' IS NOT DEFINED IN ANY DISPL SET')

 9996 FORMAT(/,' PROCESSING ABORTED IN SUBROUTINE ',A,' DUE TO ABOVE ',I8,' ERRORS')

12345 FORMAT(A, A)

! *********************************************************************************************************************************

      END SUBROUTINE TSET_PROC
