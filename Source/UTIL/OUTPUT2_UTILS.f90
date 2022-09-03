!===================================================================================================================================
      SUBROUTINE WRITE_OP2_HEADER(POST)
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  OP2

      integer, intent(in)  :: POST
      character(len=28) :: TAPE_CODE
      character(len=8)  :: NASTRAN_VERSION
      IF (POST == -1) THEN
        !_write_markers(op2, op2_ascii, [3, 0, 7])
        WRITE(OP2) 3
        WRITE(OP2) 3, 24, 2021-2000  ! date
        !WRITE(OP2) 0
        WRITE(OP2) 7
        TAPE_CODE = 'NASTRAN FORT TAPE ID CODE - '
!       write(OP2) 1
!       write(OP2) 7
        WRITE(OP2) TAPE_CODE
!       fop2.write(pack(endian + b'7i 28s i', *[4, 1, 4,
!                                               4, 7, 4,
!                                               28, tape_code, 28]))

!       nastran_version = b'NX8.5   ' if obj.is_nx else b'XXXXXXXX'
        NASTRAN_VERSION = 'XXXXXXXX'
        WRITE(OP2) 2
        WRITE(OP2) NASTRAN_VERSION
!       write(OP2) 'XXXXXXXX'
!       fop2.write(pack(endian + b'4i 8s i', *[4, 2, 4,
!                                              #4, 2, 4,
!                                              #4, 1, 4,
!                                              #4, 8, 4,
!                                              8, nastran_version, 8]))
        WRITE(OP2) -1
        WRITE(OP2) 0
!       fop2.write(pack(endian + b'6i', *[4, -1, 4,
!                                         4, 0, 4,]))
      ELSE IF(POST == -2) THEN
!       write_markers(fop2_ascii, [2, 4])
        WRITE(OP2) 2
        WRITE(OP2) 4
      ENDIF
      END SUBROUTINE WRITE_OP2_HEADER

!===================================================================================================================================
      SUBROUTINE  END_OP2_TABLE(ITABLE)
      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE IOUNT1, ONLY                :  ERR, OP2
      IMPLICIT NONE
      INTEGER(LONG) :: ITABLE
      WRITE(ERR,9114) ITABLE

 9114 FORMAT(" *DEBUG:       END_OP2_TABLE; ITABLE=", I8)
      WRITE(OP2) ITABLE
      WRITE(OP2) 1
      WRITE(OP2) 0
      WRITE(OP2) 0
      END SUBROUTINE END_OP2_TABLE

!===================================================================================================================================
      SUBROUTINE  END_OP2_TABLES()
      USE IOUNT1, ONLY                :  ERR, OP2
      IMPLICIT NONE
 9115 FORMAT(" *DEBUG:       END_OP2_TABLES", A)
      WRITE(ERR,9115) " "
      WRITE(OP2) 0
      END SUBROUTINE END_OP2_TABLES

!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM()
      USE PENTIUM_II_KIND, ONLY       :  LONG, BYTE
      USE IOUNT1, ONLY                :  ERR, OP2
      
      ! GEOM1 - GRID/COORDs
      USE SCONTR, ONLY                :  NGRID
      USE MODEL_STUF, ONLY            :  GRID_ID, GRID
      
      ! GEOM2 - elements
      !USE MODEL_STUF, ONLY : ELAS1, ELAS2, ELAS3, ELAS4, ROD, TETRA4, TETRA10
      USE SCONTR, ONLY : NCTETRA4, NCTETRA10, NCPENTA6, NCPENTA15, NCHEXA8, NCHEXA20
      USE SCONTR, ONLY : NCQUAD4, NCQUAD4K, NCSHEAR, NCTRIA3, NCTRIA3K
      USE SCONTR, ONLY : NCROD, NCBAR, NCBEAM, NCBUSH, NCELAS1, NCELAS2, NCELAS3, NCELAS4
      USE SCONTR, ONLY : NCMASS, NCONM2

      USE SCONTR, ONLY     : NELE, NEDAT
      USE MODEL_STUF, ONLY : ETYPE, EOFF, EPNT

      IMPLICIT NONE
      INTEGER(LONG)                    :: ITABLE                        ! the subtable counter
      INTEGER(LONG)                    :: I,J                           ! counter
      CHARACTER(LEN=8*BYTE)            :: TABLE_NAME                    ! the current op2 table name
      INTEGER(LONG)                    :: NGRID_ACTUAL = 0              ! number of GRID;    corrects for oversizing
      INTEGER(LONG)                    :: NSPOINT_ACTUAL = 0            ! number of SPOINTs; corrects for oversizing
      INTEGER(LONG), DIMENSION(NGRID)  :: GRID_INDEX, SPOINT_INDEX      ! oversized, but mehhh
      LOGICAL                          :: IS_GEOM1 = .FALSE.            ! do we need to write the table
      LOGICAL                          :: IS_GEOM3 = .FALSE.            ! do we need to write the table
      LOGICAL                          :: IS_GEOM4 = .FALSE.            ! do we need to write the table

      !---------------------------------------------------------------------------------------------
      IF(NGRID > 0) THEN
        IS_GEOM1 = .TRUE.
      ENDIF

! Each row of GRID is for one grid point and contains:
!              (1) Grid point number    in col 1
!              (2) Input coord system   in col 2
!              (3) Global coord system  in col 3
!              (4) Permanent SPC's      in col 4
!              (5) Line break indicator in col 5 (put this many line breaks in F06 after this grid number)
!              (6) Num of comps         in col 6 (1 for SPOINT, 6 for actual grid)
!
!            The array is sorted in the following order:
!              (1) After the B.D. deck is read it is in GRID input order
!              (2) After subr GRID_PROC it is in grid point numerical order
!
! Each row of RGRID is for one grid point and contains:
!          After Bulk Data has been read: the 3 coords of the grid in the coord sys defined in col 2 of array GRID for this G.P.
!          After subr GRID_PROC has run : the 3 coords of the grid in the basic (0) coord sys of the model
 1    FORMAT("****DEBUG:   WRITE_OP2 GEOM ngrid",i4)
      IF (IS_GEOM1) THEN
        WRITE(ERR,1) NGRID
      
        TABLE_NAME = "GEOM1"
        CALL WRITE_OP2_GEOM_HEADER(TABLE_NAME, ITABLE)

        ! identify the GRIDs vs. the SPOINTs
        DO I=1,NGRID
          IF (GRID(I,6) == 1) THEN
            NSPOINT_ACTUAL = NSPOINT_ACTUAL + 1
            SPOINT_INDEX(NSPOINT_ACTUAL) = I
          ELSE
            NGRID_ACTUAL = NGRID_ACTUAL + 1
            GRID_INDEX(NGRID_ACTUAL) = I
          ENDIF
        ENDDO

        CALL WRITE_OP2_GEOM1_GRID(ITABLE, NGRID_ACTUAL, GRID_INDEX)
        CALL WRITE_OP2_GEOM1_CORD(ITABLE)

        !DO I=1,NGRID
          ! is GRID_ID(I) the same as GRID(I,1)???
          !
          !(nid, cp, x1, x2, x3, cd, ps, seid)
          !                        nid        cp         x                   y                   z                  
        !  WRITE(ERR,2) GRID_ID(I), GRID(I,1), GRID(I,2), REAL(RGRID(I,1),4), REAL(RGRID(I,2),4), REAL(RGRID(I,3),4), &
          !            cd         ps         ndof
        !               GRID(I,3), GRID(I,4), GRID(I,6)
        !ENDDO
        CALL END_OP2_GEOM_TABLE(ITABLE)
      ENDIF

      CALL WRITE_OP2_GEOM2()
      CALL WRITE_OP2_GEOM_EPT()
      !CALL WRITE_OP2_GEOM_MPT()

      END SUBROUTINE WRITE_OP2_GEOM

!===================================================================================================================================
      SUBROUTINE GET_PROD_INDEX(PID, IPROD)
      USE PENTIUM_II_KIND, ONLY   :  LONG
      USE SCONTR, ONLY            :  NPROD
      USE MODEL_STUF, ONLY        :  PROD
      USE IOUNT1, ONLY            :  ERR
 
      INTEGER(LONG), INTENT(IN)    :: PID
      INTEGER(LONG), INTENT(INOUT) :: IPROD
      INTEGER(LONG)                :: I
      WRITE(ERR,1) PID,IPROD
      DO I=1,NPROD
        IF (PROD(I,1) == PID) THEN
          IPROD = I
          WRITE(ERR,2) PID
          EXIT  ! break statement
        ENDIF
      ENDDO
      WRITE(ERR,3) PID,IPROD,PROD(IPROD,1)
 1    FORMAT("GET_PROD_INDEX START:  PID=",i4,"; IPROD=",i4)
 2    FORMAT("GET_PROD_INDEX MID:  *********FOUND PID=",i4)
 3    FORMAT("GET_PROD_INDEX END:  PID=",i4,"; IPROD=",i4,"; PROD(I,1)=",i4)
      END SUBROUTINE GET_PROD_INDEX
     
!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM_EPT()
      ! writes the element property table (EPT)
      USE PENTIUM_II_KIND, ONLY       :  LONG, BYTE
      USE IOUNT1, ONLY                :  ERR, OP2
      USE SCONTR, ONLY : NPELAS, NPROD, NPBUSH, NPCOMP, NPSHEAR, NPSOLID ! NPSHELL, 
      USE MODEL_STUF, ONLY : PELAS, PROD, PBAR, PBEAM, PSHEAR, PCOMP, PSOLID ! PSHELL, 
      USE MODEL_STUF, ONLY : RPELAS, RPROD, RPROD, RPBAR, RPBEAM, RPSHEAR, RPCOMP ! RPSHELL, RPSOLID

      IMPLICIT NONE
      !INTEGER(LONG), INTENT(IN)   :: NCTETRA ! number of elements
      INTEGER(LONG)                :: I, J                        ! loop counter
      INTEGER(LONG)                :: ITABLE                      ! the subtable id
      INTEGER(LONG)                :: NUM_WIDE                    ! the width of a single card
      INTEGER(LONG)                :: NVALUES                     ! the number of words in the block
      CHARACTER(8*BYTE), PARAMETER :: TABLE_NAME = "EPT"          ! the table_name we're writing
      INTEGER(LONG)                :: PID, NPROD_ACTUAL = 0
      INTEGER(LONG),DIMENSION(NPROD) :: PROD_INDEX

      LOGICAL :: IS_EPT


      !  PELAS  = Array of integer data from PELAS Bulk Data entries. Each row is for one PELAS entry read in B.D. and contains:
      !             ( 1) Col  1: Property ID
      !
      !  RPELAS = Array of real data from PELAS Bulk Data entries. Each row is for one PELAS entry read in B.D. and contains:
      !             ( 1) Col  1: Spring rate, K
      !             ( 2) Col  2: Damping coefficient, GE
      !             ( 3) Col  3: Stress recovery coefficient, S


      !  PROD   = Array of integer data from PROD  Bulk Data entries. Each row is for one PROD  entry read in B.D. and contains:
      !             ( 1) Col  1: Property ID
      !             ( 2) Col  2: Material ID
      !
      !  RPROD  = Array of real data from PROD  Bulk Data entries. Each row is for one PROD  entry read in B.D. and contains:
      !             ( 1) Col  1: Cross sectional area, A
      !             ( 2) Col  2: Torsion constant, J
      !             ( 3) Col  3: Torsion stress recov. coeff, C
      !             ( 4) Col  4: Non structural mass, NSM

      IS_EPT = ( (NPROD > 0) )

      IF (IS_EPT) THEN
        CALL WRITE_OP2_GEOM_HEADER(TABLE_NAME, ITABLE)
        
        IF (NPROD > 0) THEN
          NUM_WIDE = 6
          ! (pid, mid, a, j, c, nsm)
          !  PROD= Array of integer data from PROD  Bulk Data entries. Each row is for one PROD  entry read in B.D. and contains:
          !    (1) Col  1: Property ID
          !    (2) Col  2: Material ID
          !  RPROD = Array of real data from PROD  Bulk Data entries. Each row is for one PROD  entry read in B.D. and contains:
          !    (1) Col  1: Cross sectional area, A
          !    (2) Col  2: Torsion constant, J
          !    (3) Col  3: Torsion stress recov. coeff, C
          !    (4) Col  4: Non structural mass, NSM
          DO I=1,NPROD
            PID = PROD(I,1)
            IF (PID /= 0) THEN
              NPROD_ACTUAL = NPROD_ACTUAL + 1
              PROD_INDEX(I) = NPROD_ACTUAL
            ENDIF
          ENDDO
          NVALUES = NUM_WIDE * NPROD
          WRITE(OP2) NVALUES + 3
          WRITE(OP2) 902, 9, 29, (PROD(PROD_INDEX(I),1),PROD(PROD_INDEX(I),2),               &
                                 (REAL(RPROD(PROD_INDEX(I),J),4), J=1,4), I=1,NPROD_ACTUAL)
          CALL WRITE_OP2_SUBTABLE_INCREMENT(ITABLE)
        ENDIF
        CALL END_OP2_GEOM_TABLE(ITABLE)
      ENDIF
      END SUBROUTINE WRITE_OP2_GEOM_EPT

!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM_MPT()
      ! writes the material property table (MPT)
      USE PENTIUM_II_KIND, ONLY       :  LONG, BYTE, DOUBLE
      USE IOUNT1, ONLY                :  ERR, OP2
      USE SCONTR, ONLY : NMATL
      USE MODEL_STUF, ONLY : MATL, RMATL
      IMPLICIT NONE
      INTEGER(LONG), DIMENSION(NMATL) :: MAT1_INDEX, MAT2_INDEX, MAT8_INDEX, MAT9_INDEX
      REAL(DOUBLE), DIMENSION(NMATL, 10) :: RMAT1, RMAT2, RMAT8, RMAT9

!  MATL   = Array of integer data from Material Bulk Data entries. Each row is for one Material entry read in B.D. and contains:
!             ( 1) Col  1: Material ID
!             ( 2) Col  2: Material type: 1, in MATL array for MAT1 entry (isotropic)
!                                         2, in MATL array for MAT2 entry (anisotropic, 2D elements)
!                                         8, in MATL array for MAT8 entry (orthotropic)
!                                         9, in MATL array for MAT9 entry (anisotropic, 3D elements)

!  RMATL  = Array of real data from material Bulk Data entries. Each row is for one material entry read in B.D. and contains:
!
!           MAT1 (isotropic):
!           ----------------
!                ( 1) Col  1: E     Young's modulus, E
!                ( 2) Col  2: G     Shear modulus, G
!                ( 3) Col  3: NU    Poisson's ratio, NU
!                ( 4) Col  4: RHO   Mass density, RHO
!                ( 5) Col  5: ALPHA Thermal expansion coeff, ALPHA
!                ( 6) Col  6: TREF  Reference temperature, TREF
!                ( 7) Col  7: GE    Damping coefficient, GE
!                ( 8) Col  8: ST    Tension limit, ST
!                ( 9) Col  9: SC    Compression limit, SC
!                (10) Col 10: SS    Shear limit, SS

!           MAT2 (2D anisotropic):
!           ---------------------
!                ( 1) Col  1: G11   11 term in stress/strain matrix
!                ( 2) Col  2: G12   12 term in stress/strain matrix
!                ( 3) Col  3: G13   13 term in stress/strain matrix
!                ( 4) Col  4: G22   22 term in stress/strain matrix
!                ( 5) Col  5: G23   23 term in stress/strain matrix
!                ( 6) Col  6: G33   33 term in stress/strain matrix
!                ( 7) Col  7: RHO   Mass density
!                ( 8) Col  8: A1    Thermal expansion coeff in material x direction
!                ( 9) Col  9: A2    Thermal expansion coeff in material y direction
!                (10) Col 10: A3    Thermal expansion coeff in shear
!                (11) Col 11: TREF  Reference temperature for thermal expansion
!                (12) Col 12: GE    Damping coeff
!                (13) Col 13: ST    Tension limit, ST
!                (14) Col 14: SC    Compression limit, SC
!                (15) Col 15: SS    Shear limit, SS

!           MAT8 (orthotropic):
!           ------------------
!                ( 1) Col  1: E1    Modulus of elasticity in the longitudinal direction
!                ( 2) Col  2: E2    Modulus of elasticity in the lateral direction
!                ( 3) Col  3: NU12  Poisson's ratio
!                ( 4) Col  4: G12   In-plane shear modulus
!                ( 5) Col  5: G1Z   Transverse shear modulus in 1-Z plane
!                ( 6) Col  6: G2Z   Transverse shear modulus in 2-Z plane
!                ( 7) Col  7: RHO   Mass density
!                ( 8) Col  8: A1    Thermal expansion coeff in the longitudinal direction
!                ( 9) Col  9: A2    Thermal expansion coeff in the lateral direction
!                (10) Col 10: TREF  Reference temperature for thermal expansion
!                (11) Col 11: Xt    Allowable stress or strain in tension in the longitudinal directio
!                (12) Col 12: Xc    Allowable stress or strain in compression in the longitudinal direction
!                (13) Col 13: Yt    Allowable stress or strain in tension in the lateral direction
!                (14) Col 14: Yc    Allowable stress or strain in compression in the lateral direction
!                (15) Col 15: S     Allowable stress or strain for in-plane shear
!                (16) Col 16: GE    Damping coeff
!                (17) Col 17: F12   Interaction term in the tensor polynomial theory of failure
!                (18) Col 18: STRN  For max strain failure theory only. Indicates whether allowables are stress or strain allowables

      INTEGER(LONG)                :: I, J                        ! loop counter
      INTEGER(LONG)                :: ITABLE                      ! the subtable id
      INTEGER(LONG)                :: NUM_WIDE                    ! the width of a single card
      INTEGER(LONG)                :: NVALUES                     ! the number of words in the block
      CHARACTER(8*BYTE), PARAMETER :: TABLE_NAME = "MPT"          ! the table_name we're writing
      INTEGER(LONG)                :: IMAT1 = 0, IMAT2 = 0, IMAT8 = 0, IMAT9 = 0

      LOGICAL :: IS_MPT

      IS_MPT = ( (NMATL > 0) )
      IS_MPT = .FALSE.
      IF (IS_MPT) THEN
        DO I=1,NMATL
          IF      (MATL(I,2) == 1) THEN
            IMAT1 = IMAT1 + 1
            MAT1_INDEX(IMAT1) = I
          ELSE IF (MATL(I,2) == 2) THEN
            IMAT2 = IMAT2 + 1
            MAT2_INDEX(IMAT2) = I
          ELSE IF (MATL(I,2) == 8) THEN
            IMAT8 = IMAT8 + 1
            MAT8_INDEX(IMAT8) = I
          ELSE IF (MATL(I,2) == 9) THEN
            IMAT9 = IMAT9 + 1
            MAT9_INDEX(IMAT9) = I
          ENDIF
        ENDDO
        !  MATL = Array of integer data from Material Bulk Data entries. Each row is for one Material entry read in B.D. and contains:
        ! (1) Col  1: Material ID
        ! (2) Col  2: Material type: 1, in MATL array for MAT1 entry (isotropic)
        !                            2, in MATL array for MAT2 entry (anisotropic, 2D elements)
        !                            8, in MATL array for MAT8 entry (orthotropic)
        !                            9, in MATL array for MAT9 entry (anisotropic, 3D elements)

        ! MAT1_INDEX, MAT2_INDEX, MAT8_INDEX, MAT9_INDEX
        CALL WRITE_OP2_GEOM_HEADER(TABLE_NAME, ITABLE)
        CALL END_OP2_GEOM_TABLE(ITABLE)
      ENDIF
      END SUBROUTINE WRITE_OP2_GEOM_MPT

!===================================================================================================================================
      SUBROUTINE WRITE_OP2_GEOM_HEADER(TABLE_NAME, ITABLE)
      USE PENTIUM_II_KIND, ONLY         :  LONG, BYTE
      USE IOUNT1, ONLY                  :  ERR,OP2
      IMPLICIT NONE
      CHARACTER(LEN=8*BYTE),INTENT(IN)  :: TABLE_NAME
      INTEGER(LONG),INTENT(INOUT)       :: ITABLE
 1    FORMAT("writing table_name ",A)
      WRITE(ERR,1) TABLE_NAME

      CALL WRITE_TABLE_HEADER(TABLE_NAME)
      !WRITE(OP2) 7
      !WRITE(OP2) 1, 2, 3, 4, 5, 6, 7

      !WRITE(OP2) -2
      !WRITE(OP2) 1
      !WRITE(OP2) 0

      !WRITE(OP2) 2
      !WRITE(OP2) 1, 2

      WRITE(OP2) -3
      WRITE(OP2) 1
      WRITE(OP2) 0
      ITABLE = -3
      END SUBROUTINE WRITE_OP2_GEOM_HEADER

!===================================================================================================================================
      SUBROUTINE WRITE_OP2_SUBTABLE_INCREMENT( ITABLE )
      USE PENTIUM_II_KIND, ONLY         :  LONG
      USE IOUNT1, ONLY                  :  OP2
      IMPLICIT NONE
      INTEGER(LONG),INTENT(INOUT)       :: ITABLE

      ITABLE = ITABLE - 1
      WRITE(OP2) ITABLE
      WRITE(OP2) 1
      WRITE(OP2) 0
      END SUBROUTINE WRITE_OP2_SUBTABLE_INCREMENT

!===================================================================================================================================
      SUBROUTINE END_OP2_GEOM_TABLE( ITABLE )
      USE PENTIUM_II_KIND, ONLY         :  LONG
      USE IOUNT1, ONLY                  :  OP2
      IMPLICIT NONE
      INTEGER(LONG),INTENT(INOUT)       :: ITABLE

      !WRITE(OP2) 3
      !WRITE(OP2) 1, 2, 3
      !ITABLE = ITABLE - 1
      !
      !WRITE(OP2) ITABLE
      !WRITE(OP2) 1
      !WRITE(OP2) 0
      !
      !WRITE(OP2) 0
      !ITABLE = ITABLE - 1

      WRITE(OP2) 0
      END SUBROUTINE END_OP2_GEOM_TABLE
