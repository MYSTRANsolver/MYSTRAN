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

      SUBROUTINE LINK4

! Calculates system eigenvalues, eigenvectors. There are 4 eigenvalue extraction methods in MYSTRAN, none of which seem suited to
! very large eigenvalue problems for one reason or another:

!   (1) LANCZOS method:

!          calculates some eigenvalues and some eigenvectors of KLL, MLL (or KLL, KLLD for BUCKLING). This is the most widely used
!          eigenvalue extraction method for large problems. The LANCZOS method in MYSTRAN uses 1 of 2 algorithms:
!             (a) LAPACK: requires KLL, MLL (or KLL, KLLD for buckling) to be in band storage - sparse storage can NOT be used

!          Tis Lanczos algorithms is not practical for very large eigenvalue problems since LAPACK/ARPACK will require
!          large amounts of memory to store the banded KLL, MLL matrices. 

!   (2) GIV (Givens) method:
!          calculates all eigenvalues and some eigenvactors of KLL, MLL (or KLL, KLLD for buckling). This method is only practical
!          for relatively small problems. It requires MLL (or KLLD for buckling) to be a positive definite matrix. The algorithm
!          performs a Cholesky decomp of matrix MLL (or KLLD for buckling) which can be time consuming for large problems

!   (3) MGIV (modified Givens) method: 
!          calculates all eigenvalues and some eigenvactors of KLL, MLL (or KLL, KLLD for buckling). This method is only practical
!          for relatively small problems. It requires KLL to be a positive definite matrix. The algorithm performs a Cholesky
!          decomp of matrix KLL which can be time consuming for large problems

!   (4) INV (Inverse Power) method:
!          calculates only the lowest eigenvalue and its eigenvector of KLL, MLL (or KLL, KLLD for buckling).


      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_BUG, WRT_ERR, WRT_LOG, ERR, ERRSTAT, F04, F06, L1M, L3A, SC1
      USE IOUNT1, ONLY                :  LINK1M,  LINK2I,  LINK3A, L1M_MSG, L3A_MSG
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, COMM, FATAL_ERR, LINKNO, MBUG, NDOFL,                                       &
                                         NTERM_KLL, NTERM_KLLD, NTERM_KLLDn,                                                       &
                                         NTERM_MLL, NTERM_MLLn,                                                                    &
                                         NVEC, NUM_EIGENS, NUM_KLLD_DIAG_ZEROS, NUM_MLL_DIAG_ZEROS, SOL_NAME, WARN_ERR
      USE TIMDAT, ONLY                :  YEAR, MONTH, DAY, HOUR, MINUTE, SEC, SFRAC, STIME, TSEC
      USE CONSTANTS_1, ONLY           :  ZERO, ONE
      USE PARAMS, ONLY                :  EPSIL, LANCMETH, SPARSTOR, SUPINFO
      USE MODEL_STUF, ONLY            :  EIG_COMP, EIG_CRIT, EIG_FRQ1, EIG_FRQ2, EIG_GRID, EIG_METH, EIG_MSGLVL, EIG_LAP_MAT_TYPE, &
                                         EIG_MODE, EIG_N1, EIG_N2, EIG_NCVFACL, EIG_NORM, EIG_SID, EIG_SIGMA, EIG_VECS, MAXMIJ,    &
                                         MIJ_COL, MIJ_ROW, NUM_FAIL_CRIT

      USE SPARSE_MATRICES, ONLY       :  I_KLL, J_KLL, KLL, I_KLLD, J_KLLD, KLLD, I_KLLDn, J_KLLDn, KLLDn,                         &
                                         I_MLL, J_MLL, MLL, I_MLLn, J_MLLn, MLLn
      USE OUTPUT4_MATRICES, ONLY      :  NUM_OU4_REQUESTS
      USE EIGEN_MATRICES_1, ONLY      :  GEN_MASS, MODE_NUM, EIGEN_VAL, EIGEN_VEC
      USE LAPACK_DPB_MATRICES, ONLY   :  ABAND, BBAND
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG

      USE LINK4_USE_IFs

      IMPLICIT NONE 

      CHARACTER, PARAMETER            :: CR13 = CHAR(13)   ! This causes a carriage return simulating the "+" action in a FORMAT
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'LINK4'
      CHARACTER( 44*BYTE)             :: MODNAM              ! Name to write to screen to describe module being run.

      INTEGER(LONG)                   :: I,J                 ! DO loop indices or counters.
      INTEGER(LONG)                   :: IERROR              ! Error count when reading records from a file.
      INTEGER(LONG)                   :: OUNT(2)             ! File units to write messages to. Input to subr UNFORMATTED_OPEN.
      INTEGER(LONG), PARAMETER        :: P_LINKNO = 2        ! Prior LINK no's that should have run before this LINK can execute.

      REAL(DOUBLE)                    :: EPS1                ! Small number to compare variables against zero.
      REAL(DOUBLE)                    :: EIGEN_VEC_COL(NDOFL)! One eigenvector put into a 1-D array.

! **********************************************************************************************************************************
      LINKNO = 4

      EPS1   = EPSIL(1)

! Set time initializing parameters

      CALL TIME_INIT

! Initialize WRT_BUG

      DO I=0,MBUG-1
         WRT_BUG(I) = 0
      ENDDO

! Get date and time, write to screen

      CALL OURDAT
      CALL OURTIM
      WRITE(SC1,152) LINKNO

! Make units for writing errors the screen until we open output files

      OUNT(1) = SC1
      OUNT(2) = SC1

! Make units for writing errors the error file and output file

      OUNT(1) = ERR
      OUNT(2) = F06

! Write info to text files
  
      WRITE(F06,150) LINKNO
      IF (WRT_LOG > 0) THEN
         WRITE(F04,150) LINKNO
      ENDIF
      WRITE(ERR,150) LINKNO

! Read LINK1A file

      CALL READ_L1A ( 'KEEP', 'Y' )
! Check COMM for successful completion of prior LINKs

      IF (COMM(P_LINKNO) /= 'C') THEN
         WRITE(ERR,9998) P_LINKNO,P_LINKNO,LINKNO
         WRITE(F06,9998) P_LINKNO,P_LINKNO,LINKNO
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )                            ! Prior LINK's didn't complete, so quit
      ENDIF

! Make sure we have correct SOL

      IF ((SOL_NAME(1:5) /= 'MODES') .AND. (SOL_NAME(1:12) /= 'GEN CB MODEL') .AND. (SOL_NAME(1:8) /= 'BUCKLING')) THEN
         WRITE(ERR,999) 'MODES or BUCKLING or GEN CB MODEL', SOL_NAME
         WRITE(F06,999) 'MODES or BUCKLING or GEN CB MODEL', SOL_NAME
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! **********************************************************************************************************************************
! Read data from file LINK1M

      CALL READ_L1M ( IERROR )

      IF (DEBUG(184) > 0) THEN
         WRITE(F06,*   ) ' Data written to file L1M'
         WRITE(F06,9102) '   EIG_SID         ', EIG_SID
         WRITE(F06,9101) '   EIG_METH        ', EIG_METH
         WRITE(F06,9103) '   EIG_FRQ1        ', EIG_FRQ1
         WRITE(F06,9103) '   EIG_FRQ2        ', EIG_FRQ2
         WRITE(F06,9102) '   EIG_N1          ', EIG_N1
         WRITE(F06,9102) '   EIG_N2          ', EIG_N2
         WRITE(F06,9101) '   EIG_VECS        ', EIG_VECS
         WRITE(F06,9103) '   EIG_CRIT        ', EIG_CRIT
         WRITE(F06,9101) '   EIG_NORM        ', EIG_NORM
         WRITE(F06,9102) '   EIG_GRID        ', EIG_GRID
         WRITE(F06,9102) '   EIG_COMP        ', EIG_COMP
         WRITE(F06,9102) '   EIG_MODE        ', EIG_MODE
         WRITE(F06,9103) '   EIG_SIGMA       ', EIG_SIGMA
         WRITE(F06,9101) '   EIG_LAP_MAT_TYPE', EIG_LAP_MAT_TYPE
         WRITE(F06,9102) '   EIG_MSGLVL      ', EIG_MSGLVL
         WRITE(F06,9102) '   EIG_NCVFACL     ', EIG_NCVFACL
         WRITE(F06,9102) '   NUM_FAIL_CRIT   ', NUM_FAIL_CRIT
         WRITE(F06,9103) '   MAXMIJ          ', MAXMIJ
         WRITE(F06,9102) '   MIJ_ROW         ', MIJ_ROW
         WRITE(F06,9102) '   MIJ_COL         ', MIJ_COL
         WRITE(F06,*)
      ENDIF

      IF (IERROR > 0) THEN
         CALL OUTA_HERE ( 'Y' )
      ENDIF 

! NUM_MLL_DIAG_ZEROS will be used for a message written when the eigen summary is printed in subr EIG_SUMMARY (if more than this
! number of eigens are requested)

      IF (SOL_NAME(1:8) == 'BUCKLING') THEN
         CONTINUE
      ELSE
         CALL SPARSE_MAT_DIAG_ZEROS ( 'MLL', NDOFL, NTERM_MLL, I_MLL, J_MLL, NUM_MLL_DIAG_ZEROS )
      ENDIF

! Generate nonsymmetric storage form for KLLD (if BUCKLING soln) or MLL. This is done since subr MATMULT_SFF, used when subr DSBAND
! is called herein, will run faster. MATMULT_SFF is called in each "Reverse commumication loop" in DSBAND for the LANCZOS method.

      IF      (SPARSTOR == 'SYM   ') THEN

         IF (SOL_NAME(1:8) == 'BUCKLING') THEN

            CALL SPARSE_MAT_DIAG_ZEROS ( 'KLLD', NDOFL, NTERM_KLLD, I_KLLD, J_KLLD, NUM_KLLD_DIAG_ZEROS )
            NTERM_KLLDn = 2*NTERM_KLLD  - (NDOFL - NUM_KLLD_DIAG_ZEROS)

            CALL OURTIM
            MODNAM = 'ALLOCATE SPARSE KLLDn ARRAYS'
            WRITE(SC1,4092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL ALLOCATE_SPARSE_MAT ( 'KLLDn', NDOFL, NTERM_KLLDn, SUBR_NAME )

            CALL OURTIM
            MODNAM = 'CONVERT SYM CRS KLLD TO NONSYM CRS KLLDn'
            WRITE(SC1,4092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL CRS_SYM_TO_CRS_NONSYM ( 'KLLD', NDOFL, NTERM_KLLD, I_KLLD, J_KLLD, KLLD, 'KLLDn', NTERM_KLLDn,                    &
                                         I_KLLDn, J_KLLDn, KLLDn, 'Y' )

         ELSE

            CALL SPARSE_MAT_DIAG_ZEROS ( 'MLL', NDOFL, NTERM_MLL, I_MLL, J_MLL, NUM_MLL_DIAG_ZEROS )
            NTERM_MLLn = 2*NTERM_MLL  - (NDOFL - NUM_MLL_DIAG_ZEROS)

            CALL OURTIM
            MODNAM = 'ALLOCATE SPARSE MLLn ARRAYS'
            WRITE(SC1,4092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL ALLOCATE_SPARSE_MAT ( 'MLLn', NDOFL, NTERM_MLLn, SUBR_NAME )

            CALL OURTIM
            MODNAM = 'CONVERT SYM CRS MLL TO NONSYM CRS MLLn'
            WRITE(SC1,4092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL CRS_SYM_TO_CRS_NONSYM ( 'MLL', NDOFL, NTERM_MLL, I_MLL, J_MLL, MLL, 'MLLn', NTERM_MLLn, I_MLLn, J_MLLn, MLLn, 'Y' )

         ENDIF

      ELSE IF (SPARSTOR == 'NONSYM') THEN

         IF (SOL_NAME(1:8) == 'BUCKLING') THEN

            CALL OURTIM

            MODNAM = 'ALLOCATE ARRAYS FOR NONSYM STORAGE OF KLLD'
            WRITE(SC1,4092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            NTERM_KLLDn = NTERM_KLLD
            CALL ALLOCATE_SPARSE_MAT ( 'KLLDn', NDOFL, NTERM_KLLDn, SUBR_NAME )

            CALL OURTIM
            MODNAM = 'GET VALUES FOR NONSYM FORM OF KLLD'
            WRITE(SC1,4092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            DO I=1,NDOFL+1
               I_KLLDn(I) = I_KLLD(I)
            ENDDO
            DO J=1,NTERM_KLLDn
               J_KLLDn(J) = J_KLLD(J)
                 KLLDn(J) =   KLLD(J)
            ENDDO

         ELSE

            CALL OURTIM
            MODNAM = 'ALLOCATE ARRAYS FOR NONSYM STORAGE OF MLL'
            WRITE(SC1,4092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            NTERM_MLLn = NTERM_MLL
            CALL ALLOCATE_SPARSE_MAT ( 'MLLn', NDOFL, NTERM_MLLn, SUBR_NAME )

            CALL OURTIM
            MODNAM = 'GET VALUES FOR NONSYM FORM OF MLL'
            WRITE(SC1,4092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            DO I=1,NDOFL+1
               I_MLLn(I) = I_MLL(I)
            ENDDO
            DO J=1,NTERM_MLLn
               J_MLLn(J) = J_MLL(J)
                 MLLn(J) =   MLL(J)
            ENDDO

         ENDIF

      ELSE                                           !      Error - incorrect SPARSTOR

         WRITE(ERR,932) SUBR_NAME, SPARSTOR
         WRITE(F06,932) SUBR_NAME, SPARSTOR
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )

      ENDIF

      IF (DEBUG(42) == 2) THEN
         CALL WRITE_SPARSE_CRS ( ' MLLn', 'A ', 'A ', NTERM_MLLn, NDOFL, I_MLLn, J_MLLn, MLLn )
      ENDIF


! **********************************************************************************************************************************
! Solve eigenvalue problem

      IF ((EIG_METH(1:3) == 'GIV') .OR. (EIG_METH(1:4) == 'MGIV')) THEN

         CALL EIG_GIV_MGIV

      ELSE IF (EIG_METH(1:3) == 'INV') THEN

         CALL EIG_INV_PWR

      ELSE IF (EIG_METH(1:7) == 'LANCZOS') THEN

         CALL EIG_LANCZOS_ARPACK

      ELSE

         WRITE(ERR,4005) SUBR_NAME, EIG_METH 
         WRITE(F06,4005) SUBR_NAME, EIG_METH 
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )                            ! Coding error, so quit

      ENDIF

      IF (SOL_NAME(1:12) /= 'GEN CB MODEL') THEN
   !xx   WRITE(SC1, * )                                    ! Advance 1 line for screen messages         
         IF (SOL_NAME(1:8) == 'BUCKLING') THEN
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KLLD', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KLLD' )
         ELSE
            WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MLL ', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'MLL' )
         ENDIF
      ENDIF

! Calc generalized masses and renorm eigenvectors to mass (users renorm is done in LINK5) 

      NUM_FAIL_CRIT = 0
      MAXMIJ        = 0
      MIJ_ROW       = 0
      MIJ_COL       = 0

      CALL ALLOCATE_EIGEN1_MAT ( 'GEN_MASS', NUM_EIGENS, 1, SUBR_NAME )

      IF (NVEC > 0) THEN 

         CALL OURTIM                                       ! Calc gen mass
         MODNAM = 'CALCULATE GENERALIZED MASS'
         WRITE(SC1,4092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL CALC_GEN_MASS

         IF (EIG_NORM == 'MASS') THEN
            CALL OURTIM                                    ! Renorm vecs to mass if user asked for 'MASS'.
            MODNAM = 'RENORMALIZE EIGENVECTORS TO UNIT GEN MASS'
            WRITE(SC1,4092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
            CALL RENORM_ON_MASS ( NVEC, EPS1 )
         ENDIF

      ELSE

         DO I=1,NUM_EIGENS
            GEN_MASS(I) = ZERO
         ENDDO

      ENDIF

!xx   WRITE(SC1, * )                                       ! Advance 1 line for screen messages         
      IF (SOL_NAME(1:8) == 'BUCKLING') THEN
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate KLLDn', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'KLLDn' )
      ELSE
         WRITE(SC1,12345,ADVANCE='NO') '       Deallocate MLLn ', CR13   ;   CALL DEALLOCATE_SPARSE_MAT ( 'MLLn' )
      ENDIF

! Write data to L1M

      CALL WRITE_L1M

! Write eigenvalue analysis summary to output file, if DEBUG requested them or if renormalization is on 'MASS' or 'NONE'

      MODNAM = 'WRITE EIGENVALUE SUMMARY TO OUTFIL'
      IF ((EIG_NORM == 'MASS    ') .OR. (EIG_NORM == 'NONE')) THEN
         CALL OURTIM
         WRITE(SC1,4092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         CALL EIG_SUMMARY
      ENDIF

! Open and set up file L3A (used to hold eigenvectors)

      CALL FILE_OPEN ( L3A, LINK3A, OUNT, 'REPLACE', L3A_MSG, 'WRITE_STIME', 'UNFORMATTED', 'WRITE', 'REWIND', 'Y', 'N', 'Y' )

! Write out computed eigenvectors to L3A

      CALL OURTIM
      MODNAM = 'WRITE EIGENVECTORS TO DISK FILE'
      WRITE(SC1,4092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
      DO J=1,NVEC
         DO I=1,NDOFL
           WRITE(L3A) EIGEN_VEC(I,J)
         ENDDO
      ENDDO 
      CALL FILE_CLOSE ( L3A, LINK3A, 'KEEP', 'Y' )  

! Optional eigenvector debug output

      IF (DEBUG(43) == 1) THEN
         DO J=1,NVEC
            DO I=1,NDOFL
               EIGEN_VEC_COL(I) = EIGEN_VEC(I,J)
            ENDDO 
            WRITE(F06,'(//,1X,''EIGENVECTOR'',I8/)') J
            CALL WRITE_VECTOR ('    A-SET EIGENVECTOR   ','DISPL',NDOFL, EIGEN_VEC_COL )
         ENDDO 
      ENDIF

! Call OUTPUT4 processor to process output requests for OUTPUT4 matrices generated in this link

      IF (NUM_OU4_REQUESTS > 0) THEN
         CALL OURTIM
         MODNAM = 'WRITE OUTPUT4 NATRICES      '
         WRITE(SC1,4092) LINKNO,MODNAM,HOUR,MINUTE,SEC,SFRAC
         WRITE(F06,*)
         CALL OUTPUT4_PROC ( SUBR_NAME )
      ENDIF

! Deallocate arrays (leave EIGEN_VAL until LINK9 since it may be needed there

      CALL DEALLOCATE_LAPACK_MAT ( 'RFAC' )

!xx   CALL DEALLOCATE_EIGEN1_MAT ( 'EIGEN_VAL' )  
      CALL DEALLOCATE_EIGEN1_MAT ( 'GEN_MASS' )
      CALL DEALLOCATE_EIGEN1_MAT ( 'EIGEN_VEC' )  
      CALL DEALLOCATE_EIGEN1_MAT ( 'MODE_NUM' )  

      CALL DEALLOCATE_LAPACK_MAT ( 'ABAND' )
      CALL DEALLOCATE_LAPACK_MAT ( 'BBAND' )

! Process is now complete so set COMM(LINKNO)

      COMM(LINKNO) = 'C'

! Write data to L1A

      CALL WRITE_L1A ( 'KEEP', 'Y', 'Y' )

! Check allocation status of allocatable arrays, if requested

      IF (DEBUG(100) > 0) THEN
         CALL CHK_ARRAY_ALLOC_STAT
         IF (DEBUG(100) > 1) THEN
            CALL WRITE_ALLOC_MEM_TABLE ( 'at the end of '//SUBR_NAME )
         ENDIF
      ENDIF

! Write LINK4 end to F04, F06

      CALL OURTIM
      IF (WRT_LOG > 0) THEN
         WRITE(F04,151) LINKNO
      ENDIF
      WRITE(F06,151) LINKNO

! Close files

      IF (( DEBUG(193) == 4) .OR. (DEBUG(193) == 999)) THEN
         CALL FILE_INQUIRE ( 'near end of LINK4' )
      ENDIF

! Write LINK4 end to screen
      WRITE(SC1,153) LINKNO

      RETURN

! **********************************************************************************************************************************
  150 FORMAT(/,' >> LINK',I3,' BEGIN',/)

  151 FORMAT(/,' >> LINK',I3,' END',/)

  152 FORMAT(/,' >> LINK',I3,' BEGIN')

  153 FORMAT(  ' >> LINK',I3,' END')

  932 FORMAT(' *ERROR   932: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' PARAMETER SPARSTOR MUST BE EITHER "SYM" OR "NONSYM" BUT VALUE IS ',A)

  999 FORMAT(' *ERROR   999: INCORRECT SOLUTION IN EXEC CONTROL. SHOULD BE "',A,'", BUT IS "',A,'"')

 4005 FORMAT(' *ERROR  4005: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' CODE ONLY WRITTEN FOR METHOD = GIV, MGIV, OR LANCZOS BUT METHOD IS = ',A8)

 4092 FORMAT(1X,I2,'/',A44,18X,2X,I2,':',I2,':',I2,'.',I3)

 9101 FORMAT(1X,A,' =  ','"',A,'"')

 9102 FORMAT(1X,A,' =  ',I13)

 9103 FORMAT(1X,A,' =  ',1ES13.6)

 9998 FORMAT(' *ERROR  9998: COMM ',I3,' INDICATES UNSUCCESSFUL LINK ',I2,' COMPLETION.'                                           &
                    ,/,14X,' FATAL ERROR - CANNOT START LINK ',I2)

12345 FORMAT(A,10X,A)

99001 FORMAT(1X,6(1ES14.6))
 
! **********************************************************************************************************************************

      END SUBROUTINE LINK4
