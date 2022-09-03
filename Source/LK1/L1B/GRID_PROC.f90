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
 
      SUBROUTINE GRID_PROC
  
! Performs 7 functions:
!   1) Generate GRID_ID and GRID_SEQ and sort them so that GRID_ID is in numerical order and then GRID_SEQ(I)
!      is the position, in the stack of GRID's in Bulk Data, where GRID_ID(I) exists. GRID_SEQ may change order
!      in subroutine SEQ_PROC depending on the grid point sequencing scheme the user asks for. 
!   2) Sort arrays GRID and RGRID so that they are in grid point numerical order and check for duplicate grid ID's.
!   3) Reset coord sys and perm SPC data on GRID cards based on values read from a GRDSET card (if in the data deck)
!   4) Call CORD_PROC.FOR to calc the transformations to basic from each of the coord systems (their principal axes)
!   5) Transform the grid coordinates to the basic coord system
!   6) Write grid data to filename.L1B
!   7) Write some grid data to output file if requested
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE CONSTANTS_1, ONLY           :  CONV_DEG_RAD
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ERR, F04, F06, L1B, OP2, SC1
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, DATA_NAM_LEN, FATAL_ERR, MCORD, MRCORD, MGRID, MRGRID, NCORD, NGRID
      USE PARAMS, ONLY                :  PRTBASIC
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  GRID_PROC_BEGEND
      USE MODEL_STUF, ONLY            :  GRID, RGRID, GRID_ID, GRID_SEQ, CORD, RCORD, TN
 
      USE GRID_PROC_USE_IFs

      IMPLICIT NONE
 
      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'GRID_PROC'
      CHARACTER(LEN=DATA_NAM_LEN)     :: DATA_SET_NAME     ! A data set name for output purposes
 
      INTEGER(LONG)                   :: CP                ! The actual coord sys that a grid is located in.
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG)                   :: IERROR            ! Error count
      INTEGER(LONG)                   :: JCORD             ! Internal coord sys ID
      INTEGER(LONG)                   :: JFLD              ! Used in error message to indicate a coord sys ID undefined
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = GRID_PROC_BEGEND
 
      REAL(DOUBLE)                    :: ANG1              ! An angle in a cyl or sph coord sys from the RGRID array
      REAL(DOUBLE)                    :: ANG2              ! An angle in a cyl or sph coord sys from the RGRID array
      REAL(DOUBLE)                    :: RADIUS            ! A radius coord of a grid point in a cyl or sph coord sys (CP)
      REAL(DOUBLE)                    :: RGRIDI_CP(3)      ! Coords of one grid point in the principal rectangular axes of coord
!                                                            sys CP rel to the origin of coord sys CP
      REAL(DOUBLE)                    :: RGRIDI_0(3)       ! Coords of one grid point in basic coords rel to origin of coord sys CP
!                                                            sys CP rel to the origin of coord sys CP
 
      INTRINSIC                       :: DCOS, DSIN
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages

! Part 1:
! -------
 
! Generate GRID_ID and GRID_SEQ data.

      WRITE(SC1,12345,ADVANCE='NO') '    Initialize arrays GRID and GRID_SEQ                                     ', CR13
      DO I=1,NGRID
         GRID_ID(I)  = GRID(I,1)
         GRID_SEQ(I) = I                                   ! This is the initial GRID_SEQ array (order of GRID input)
      ENDDO                                                ! It may change in subr SEQ_PROC
 
! Sort these so that the actual G.P. numbers in GRID_ID are in numerical order. ! At this point GRID_SEQ(I) is seq num for internal
! grid I but this will change (in subr SEQ_PROC) unless the final sequence order is to be the order of the grids as read in from the
! input data deck.

      WRITE(SC1,12345,ADVANCE='NO') '    Sort arrays GRID and GRID_SEQ so GRID is in numerical order', CR13
      IF (NGRID > 1) THEN
         CALL SORT_INT2 ( SUBR_NAME, 'GRID_ID, GRID_SEQ', NGRID, GRID_ID, GRID_SEQ )
      ENDIF                                                
 
! **********************************************************************************************************************************
! Part 2:
! -------
 
! Sort arrays GRID and RGRID so that they are in grid point numerical order.
 
      WRITE(SC1,12345,ADVANCE='NO') '    Sort arrays GRID and RGRID so GRID is in numerical order                ', CR13
      CALL SORT_GRID_RGRID ( SUBR_NAME, 'GRID, RGRID', NGRID, GRID, RGRID )
 
! Check for duplicate GRID ID's and quit if there are any
 
      WRITE(SC1,12345,ADVANCE='NO') '    Check for duplicate GRID IDs                                            ', CR13 
      IERROR = 0
      DO I=1,NGRID-1
         IF (GRID(I+1,1) == GRID(I,1)) THEN
            IERROR = IERROR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1344) GRID(I+1,1)
            WRITE(F06,1344) GRID(I+1,1)
         ENDIF
      ENDDO
 
! Check: If GRID(I,1) /= GRID_ID(I), then coding error

      WRITE(SC1,12345,ADVANCE='NO') '    Check GRID_ID array                                                     ', CR13 
      DO I=1,NGRID
         IF (GRID(I,1) == GRID_ID(I)) THEN
            CYCLE
         ELSE
            IERROR = IERROR + 1
            FATAL_ERR = FATAL_ERR + 1
            WRITE(ERR,1346) SUBR_NAME,GRID(I,1),I,GRID_ID(I)
            WRITE(F06,1346) SUBR_NAME,GRID(I,1),I,GRID_ID(I)
         ENDIF
      ENDDO
 
      IF (IERROR > 0) THEN
         WRITE(ERR,1343) IERROR
         WRITE(F06,1343) IERROR
         CALL OUTA_HERE ( 'Y' )
      ENDIF
 
! **********************************************************************************************************************************
! Part 3: 
! -------
 
! Check to make sure that all coord systems referenced in field 3 (input coord sys) of the grid card except the 0 system exist.
 
      WRITE(SC1,12345,ADVANCE='NO') '    Check that coord systems in field 3 referenced do exist                 ', CR13 
      IERROR = 0
i_do1:DO I=1,NGRID
         CP = GRID(I,2)
         IF (CP /= 0) THEN
            DO J=1,NCORD
               IF (CORD(J,2) == CP) THEN
                  CYCLE i_do1
               ENDIF
            ENDDO
            IERROR = IERROR + 1
            FATAL_ERR = FATAL_ERR + 1
            JFLD   = 3
            WRITE(ERR,910) GRID(I,2),JFLD,GRID(I,1),JFLD
            WRITE(F06,910) GRID(I,2),JFLD,GRID(I,1),JFLD
         ENDIF
      ENDDO i_do1
  
! Also check that any coord sys in field 7 (global) are defined. They won't be used at this time, but we want to know they exist.
 
      WRITE(SC1,12345,ADVANCE='NO') '    Check that coord systems in field 7 referenced do exist                 ', CR13 
i_do2:DO I=1,NGRID
         CP = GRID(I,3)
         IF (CP /= 0) THEN
            DO J=1,NCORD
               IF (CORD(J,2) == CP) THEN
                  CYCLE i_do2
               ENDIF
            ENDDO
            IERROR = IERROR + 1
            FATAL_ERR = FATAL_ERR + 1
            JFLD   = 7
            WRITE(ERR,910) GRID(I,3),JFLD,GRID(I,1),JFLD
            WRITE(F06,910) GRID(I,3),JFLD,GRID(I,1),JFLD
         ENDIF
      ENDDO i_do2
      IF (IERROR > 0) THEN
         WRITE(ERR,1343) IERROR 
         WRITE(F06,1343) IERROR
         CALL OUTA_HERE ( 'Y' )
      ENDIF
 
! **********************************************************************************************************************************
! Part 4:
! -------
 
! Calculate coordinate system transformation matrices if there are any CORD cards
 
      IF (NCORD /= 0) THEN
 
         WRITE(SC1,12345,ADVANCE='NO') '    Calc coord sys transformation matrices                                  ', CR13 
         CALL CORD_PROC 
 
! **********************************************************************************************************************************
! Part 5:
! -------
 
! Transform grid coordinates to basic system if the coords were not input in basic (i.e. if GRID(I,2) not 0).
  
         WRITE(SC1,12345,ADVANCE='NO') '    Transform grid coords to basic system                                   ', CR13 
         JCORD = 0
grid_do: DO I=1,NGRID
            CP = GRID(I,2)
            IF (CP == 0) CYCLE grid_do
            DO J = 1,NCORD
               IF (CORD(J,2) == CP) THEN
                  JCORD = J
                  EXIT
               ENDIF
            ENDDO
                                                           ! JCORD=0, means pgm error 
            IF (JCORD == 0) THEN                           ! (we should not have gotten here if a coord sys is undefined)
               WRITE(ERR,1345) SUBR_NAME
               WRITE(F06,1345) SUBR_NAME
               FATAL_ERR = FATAL_ERR + 1
               CALL OUTA_HERE ( 'Y' )
            ENDIF
                                                           ! Cylindrical.  The Z in RGRID(I,3) is OK as is.
            IF      ((CORD(JCORD,1) == 12) .OR. (CORD(JCORD,1) == 22)) THEN
               RADIUS     = RGRID(I,1)
               ANG1       = RGRID(I,2)*CONV_DEG_RAD
               RGRID(I,1) = RADIUS*DCOS(ANG1)
               RGRID(I,2) = RADIUS*DSIN(ANG1)
                                                           ! Spherical
            ELSE IF ((CORD(JCORD,1) == 13) .OR. (CORD(JCORD,1) == 23)) THEN
	            RADIUS     = RGRID(I,1)
               ANG1       = RGRID(I,2)*CONV_DEG_RAD
               ANG2       = RGRID(I,3)*CONV_DEG_RAD
               RGRID(I,1) = RADIUS*DSIN(ANG1)*DCOS(ANG2) 
               RGRID(I,2) = RADIUS*DSIN(ANG1)*DSIN(ANG2)
               RGRID(I,3) = RADIUS*DCOS(ANG1)
            ENDIF
 
      	   DO J=1,3                                       ! RGRID(I,1-3) has rectangular coords, in the princ axes of JCORD,
               RGRIDI_CP(J) = RGRID(I,J)                   ! relative to the origin of that coord system
            ENDDO   
 
! The basic coords of G.P. I are now obtained by multiplying the coord system transformation matrix TN (see CORD_PROC.FOR)
! times the rectangular coords in RGRIDI_CP (to get RGRID_0) and adding the basic coords of the origin of coord system
! JCORD (otherwise the calculated basic coords of G.P. I would only be relative to the JCORD origin).  The coords in
! basic of JCORD are in RCORD array (calculated in CORDP.FOR).
  
            CALL MATMULT_FFF ( TN(1,1,JCORD), RGRIDI_CP, 3, 3, 1, RGRIDI_0 )

            DO J=1,3                                       ! Change RGRID(I,1-3) to have the basic coords of G.P. I
               RGRID(I,J) = RGRIDI_0(J) + RCORD(JCORD,J)
            ENDDO
   
            GRID(I,2) = CORD(JCORD,3)                      ! CORD(JCORD,3) was set = 0 in subr CORD_PROC after all transformations
!                                                            so set GRID(I,2) to 0 (basic) also  
         ENDDO grid_do

      ENDIF   
 
! **********************************************************************************************************************************
! Part 6:
 
! Store the grid point data in 'L1B'
  
      WRITE(SC1,12345,ADVANCE='NO') '    Write grid data to file                                                 ', CR13 
      DATA_SET_NAME = 'GRID, RGRID'
      WRITE(L1B) DATA_SET_NAME
!      WRITE(OP2) DATA_SET_NAME
      WRITE(L1B) NGRID
!      WRITE(OP2) NGRID
      DO I=1,NGRID
         DO J=1,MGRID
            WRITE(L1B) GRID(I,J)
!            WRITE(OP2) GRID(I,J)
         ENDDO   
         DO J=1,MRGRID
            WRITE(L1B) RGRID(I,J)
!            WRITE(OP2) RGRID(I,J)
         ENDDO   
      ENDDO   
 
! Write coord data to L1B. Need coord type, CID, 12 words of RCORD. The RID for all systems is now 0
 
      WRITE(SC1,12345,ADVANCE='NO') '    Write coord data to file                                                ', CR13 
      DATA_SET_NAME = 'COORDINATE SYSTEM DATA'
      WRITE(L1B) DATA_SET_NAME
      WRITE(L1B) NCORD
      DO I=1,NCORD
         DO J=1,MCORD
            WRITE(L1B) CORD(I,J)
         ENDDO 
         DO J=1,MRCORD
            WRITE(L1B) RCORD(I,J)
         ENDDO
      ENDDO
 
      WRITE(SC1,*) CR13 

! **********************************************************************************************************************************
! Part 7:
 
! Print out grid data in basic coords
 
      IF (PRTBASIC == 1) THEN
         CALL WRITE_GRID_COORDS
      ENDIF
 
! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
  910 FORMAT(' *ERROR   910: COORD SYSTEM',I8,' IN FIELD ',I2,' ON GRID ',I8,' (OR FROM GRDSET ENTRY FIELD ',I2,') IS UNDEFINED')

 1343 FORMAT(' PROCESSING TERMINATED DUE TO ',I8,' ERRORS')

 1344 FORMAT(' *ERROR  1344: GRID NUMBER ',I8,' IS A DUPLICATE GRID NUMBER')

 1345 FORMAT(' *ERROR  1345: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' JCORD MUST NOT BE ZERO')

 1346 FORMAT(' *ERROR  1346: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ARRAYS GRID AND GRID_ID SHOULD HAVE THE SAME GRID IDs, BUT THEY DO NOT.'                              &
                    ,/,14X,' GRID NO. ',I8,' AT ROW ',I8,' IS NOT THE SAME AS GRID NO ',I8,' AT THE SAME ROW ')

12345 FORMAT(A, A)

! **********************************************************************************************************************************
 
      END SUBROUTINE GRID_PROC
