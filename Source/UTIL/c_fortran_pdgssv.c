/*
 * -- SuperLU MT routine (version 3.0) --
 * Univ. of California Berkeley, Xerox Palo Alto Research Center,
 * and Lawrence Berkeley National Lab.
 * Modified from c_fortran_dgssv.c to use SuperLU_MT
 *
 */

#include <slu_mt_ddefs.h>

#define HANDLE_SIZE 8

/* kind of integer to hold a pointer.  Use 64-bit. */
typedef long long int fptr;

typedef struct
{
  SuperMatrix *L;
  SuperMatrix *U;
  int_t *perm_c;
  int_t *perm_r;
} factors_t;

/*!
 * This routine can be called from Fortran.
 *
 * iopt (input) int
 *      Specifies the operation:
 *      = 1, performs LU decomposition for the first time
 *      = 2, performs triangular solve
 *      = 3, free all the storage in the end
 *
 * f_factors (input/output) fptr*
 *      If iopt == 1, it is an output and contains the pointer pointing to
 *                    the structure of the factored matrices.
 *      Otherwise, it it an input.
 *
 * nprocs (input) int
 *      Number of threads to use for parallel factorization.
 */
void c_fortran_pdgssv_(int *iopt, int *n, int_t *nnz, int *nrhs, int *nprocs,
                       double *values, int_t *rowind, int_t *colptr,
                       double *b, int *ldb,
                       fptr *f_factors, /* a handle containing the address
                                           pointing to the factored matrices */
                       int_t *info)
{
  SuperMatrix A, AC, B;
  SuperMatrix *L, *U;
  int_t *perm_r; /* row permutations from partial pivoting */
  int_t *perm_c; /* column permutation vector */
  int_t *etree;  /* column elimination tree */
  SCPformat *Lstore;
  NCPformat *Ustore;
  int i, panel_size, permc_spec, relax;
  trans_t trans;
  superlu_memusage_t mem_usage;
  superlumt_options_t options;
  Gstat_t stat;
  factors_t *LUfactors;
  int_t *rowind0; /* counter 1-based indexing from Fortran arrays. */
  int_t *colptr0;
  yes_no_t refact, usepr;
  double diag_pivot_thresh, drop_tol;
  void *work;
  int_t lwork;

  trans = NOTRANS;

  if (*iopt == 1)
  { /* LU decomposition */

    /* Set the default input options matching SuperLU_MT pdgssv example */
    fact_t fact = EQUILIBRATE;
    refact = NO;
    panel_size = sp_ienv(1);
    relax = sp_ienv(2);
    diag_pivot_thresh = 1.0;
    usepr = NO;
    drop_tol = 0.0;
    work = NULL;
    lwork = 0;

    /* Initialize the statistics variables. */
    StatAlloc(*n, *nprocs, panel_size, relax, &stat);
    StatInit(*n, *nprocs, &stat);

    /* Adjust to 0-based indexing */
    if (!(rowind0 = intMalloc(*nnz)))
      SUPERLU_ABORT("Malloc fails for rowind0[].");
    if (!(colptr0 = intMalloc(*n + 1)))
      SUPERLU_ABORT("Malloc fails for colptr0[].");
    for (i = 0; i < *nnz; ++i)
      rowind0[i] = rowind[i] - 1;
    for (i = 0; i <= *n; ++i)
      colptr0[i] = colptr[i] - 1;

    dCreate_CompCol_Matrix(&A, *n, *n, *nnz, values, rowind0, colptr0,
                           SLU_NC, SLU_D, SLU_GE);
    L = (SuperMatrix *)SUPERLU_MALLOC(sizeof(SuperMatrix));
    U = (SuperMatrix *)SUPERLU_MALLOC(sizeof(SuperMatrix));
    if (!(perm_r = intMalloc(*n)))
      SUPERLU_ABORT("Malloc fails for perm_r[].");
    if (!(perm_c = intMalloc(*n)))
      SUPERLU_ABORT("Malloc fails for perm_c[].");
    if (!(etree = intMalloc(*n)))
      SUPERLU_ABORT("Malloc fails for etree[].");

    /*
     * Get column permutation vector perm_c[], according to permc_spec:
     *   permc_spec = 0: natural ordering
     *   permc_spec = 1: minimum degree on structure of A'*A
     *   permc_spec = 2: minimum degree on structure of A'+A
     *   permc_spec = 3: approximate minimum degree for unsymmetric matrices (COLAMD)
     */
    permc_spec = COLAMD;
    get_perm_c(permc_spec, &A, perm_c);

    /* Initialize the option structure and apply column permutation */
    pdgstrf_init(*nprocs, fact, trans, refact, panel_size, relax,
                 diag_pivot_thresh, usepr, drop_tol, perm_c, perm_r,
                 work, lwork, &A, &AC, &options, &stat);

    /* Perform parallel LU factorization */
    pdgstrf(&options, &AC, perm_r, L, U, &stat, info);

    if (*info == 0)
    {
      Lstore = (SCPformat *)L->Store;
      Ustore = (NCPformat *)U->Store;
      printf("No of nonzeros in factor L = %lld\n", (long long)Lstore->nnz);
      printf("No of nonzeros in factor U = %lld\n", (long long)Ustore->nnz);
      printf("No of nonzeros in L+U = %lld\n", (long long)Lstore->nnz + Ustore->nnz);
      superlu_dQuerySpace(*nprocs, L, U, panel_size, &mem_usage);
      printf("L\\U MB %.3f\ttotal MB needed %.3f\n",
             mem_usage.for_lu / 1e6, mem_usage.total_needed / 1e6);
    }
    else
    {
      printf("pdgstrf() error returns INFO= %lld\n", (long long)*info);
      if (*info <= *n)
      { /* factorization completes */
        superlu_dQuerySpace(*nprocs, L, U, panel_size, &mem_usage);
        printf("L\\U MB %.3f\ttotal MB needed %.3f\n",
               mem_usage.for_lu / 1e6, mem_usage.total_needed / 1e6);
      }
    }

    /* Save the LU factors in the factors handle */
    LUfactors = (factors_t *)SUPERLU_MALLOC(sizeof(factors_t));
    LUfactors->L = L;
    LUfactors->U = U;
    LUfactors->perm_c = perm_c;
    LUfactors->perm_r = perm_r;
    *f_factors = (fptr)LUfactors;

    /* Free un-wanted storage */
    SUPERLU_FREE(etree);
    Destroy_SuperMatrix_Store(&A);
    pxgstrf_finalize(&options, &AC);
    SUPERLU_FREE(rowind0);
    SUPERLU_FREE(colptr0);
    StatFree(&stat);
  }
  else if (*iopt == 2)
  { /* Triangular solve */
    int_t iinfo;

    /* Initialize the statistics variables. */
    panel_size = sp_ienv(1);
    relax = sp_ienv(2);
    StatAlloc(*n, *nprocs, panel_size, relax, &stat);
    StatInit(*n, *nprocs, &stat);

    /* Extract the LU factors in the factors handle */
    LUfactors = (factors_t *)*f_factors;
    L = LUfactors->L;
    U = LUfactors->U;
    perm_c = LUfactors->perm_c;
    perm_r = LUfactors->perm_r;

    dCreate_Dense_Matrix(&B, *n, *nrhs, b, *ldb, SLU_DN, SLU_D, SLU_GE);

    /* Solve the system A*X=B, overwriting B with X. */
    /* NOTE: SuperLU_MT dgstrs has perm_r, perm_c order (opposite of SuperLU) */
    dgstrs(trans, L, U, perm_r, perm_c, &B, &stat, &iinfo);
    *info = iinfo;

    Destroy_SuperMatrix_Store(&B);
    StatFree(&stat);
  }
  else if (*iopt == 3)
  { /* Free storage */
    /* Free the LU factors in the factors handle */
    LUfactors = (factors_t *)*f_factors;
    SUPERLU_FREE(LUfactors->perm_r);
    SUPERLU_FREE(LUfactors->perm_c);
    Destroy_SuperNode_Matrix(LUfactors->L);
    Destroy_CompCol_Matrix(LUfactors->U);
    SUPERLU_FREE(LUfactors->L);
    SUPERLU_FREE(LUfactors->U);
    SUPERLU_FREE(LUfactors);
  }
  else
  {
    fprintf(stderr, "Invalid iopt=%d passed to c_fortran_pdgssv()\n", *iopt);
    exit(-1);
  }
}

void c_fortran_dgssv_(int *iopt, int *n, int_t *nnz, int *nrhs,
                      double *values, int_t *rowind, int_t *colptr,
                      double *b, int *ldb,
                      fptr *f_factors, /* a handle containing the address
                                          pointing to the factored matrices */
                      int_t *info)
{
  int nprocs = 12;
  c_fortran_pdgssv_(
      iopt, n, nnz, nrhs, &nprocs, values, rowind, colptr, b, ldb, f_factors, info);
}
