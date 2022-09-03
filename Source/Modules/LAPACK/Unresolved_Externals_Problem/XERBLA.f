! 068 LAPACK_BLAS_AUX ##############################################################################################################

      SUBROUTINE XERBLA( SRNAME, arg_num )

      USE PENTIUM_II_KIND, ONLY         :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                  :  ERR, F04, F06, SC1, WRT_LOG
      USE SCONTR, ONLY                  :  BLNK_SUB_NAM, FATAL_ERR
      USE TIMDAT, ONLY                  :  HOUR, MINUTE, SEC,
     &                                     SFRAC, TSEC
      USE SUBR_BEGEND_LEVELS, ONLY      :  LAPACK_BEGEND

      USE OUTA_HERE_Interface

*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER*6        SRNAME
      INTEGER            arg_num
*     ..
*
*  Purpose
*  =======
*
*  XERBLA  is an error handler for the LAPACK routines.
*  It is called by an LAPACK routine if an input parameter has an
*  invalid value.  A message is printed and execution stops.
*
*  Installers may consider modifying the STOP statement in order to
*  call system-specific exception-handling facilities.
*
*  Arguments
*  =========
*
*  SRNAME  (input) CHARACTER*6
*          The name of the routine which called XERBLA.
*
*  INFO    (input) INTEGER
*          The position of the invalid parameter in the parameter list
*          of the calling routine.
*
* =====================================================================
*
*     .. Executable Statements ..
*
      WRITE(ERR,800) SRNAME, arg_num
      WRITE(F06,800) SRNAME, arg_num
      FATAL_ERR = FATAL_ERR + 1
      CALL OUTA_HERE ( 'Y' )
*
  800 FORMAT(' *ERROR   800: PROGRAMMING ERROR IN SUBROUTINE ',A 
     $ ,/,14X,' PARAMETER NUMBER ',I2,' HAD AN ILLEGAL VALUE')  
*
*     End of XERBLA
*
      END SUBROUTINE XERBLA

