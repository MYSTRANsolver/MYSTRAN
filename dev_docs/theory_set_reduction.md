# Theory

## 8.1 Introduction

As discussed in Section 3.6, MYSTRAN builds the original stiffness and mass matrices based on the
G-set, which has 6 degrees of freedom per grid specified in the Bulk Data deck. The stiffness matrix
is by definition singular as, at this point, there have been no constraints imposed. There are two type
of constraints MYSTRAN allows; single point constraints and multi-point constraints as discussed
earlier in this manual. In order to apply boundary conditions that restrain the model from rigid body
motion, single point constraints must be used. Multi-point constraints (using rigid elements or Bulk
Data MPC entries) are used to express some degrees of freedom (DOF’s) of the model as being
rigidly restrained to some other DOF's. Thus, MYSTRAN must reduce the G-set stiffness, mass, and
loads to the independent A-set DOF's.

The discussion below shows the process that MYSTRAN uses to solve for the displacements and
constraint forces by going through a systematic reduction of the G-set to the N-set then to the F-set
and finally to the L-set which represent the independent DOF’s. These equations can then be solved
for the L-set DOF’s. The other DOF displacements, as well as constraint forces, can then be
recovered. Element forces and stresses are obtained from the displacements as discussed in
Appendix C. The process in this appendix uses the displacement set notation developed in Section
3.6 which should be reviewed prior to this section. In general, the matrix notation used in this
development is such that the matrix subscripts describe the matrix size. Thus, KGG is a matrix which
has G rows and columns, $R_{CG}$ is a matrix that has C rows and G columns and $R_{CG}^T$ is the transpose
of $R_{CG}$ and has G rows and C columns. If a matrix has only one column, it would exhibit only one
subscript, as in $Y_S$ which is an S x 1 matrix of single point constraint values

## 8.2 Reduction of the G-set to the N-set

In terms of this G-set, the equations of motion for the structure can be written as:

asdf

$$ M_{GG} \ddot U_G + K_{GG} U_G = P_G + R_{CG}^T q_C $$

$$ R_{CG} = U_G Y_C $$

In the first of equations 8.1 $M_{GG}$ is the G-set mass matrix, KGG is the G-set stiffness matrix, UG are the
G-set displacements, $P_G$ are the applied loads on the G-set DOF’s and qC are the independent,
generalized, constraint forces (due to single and multi-point constraints). The second of 8.1
expresses the constraints (both single and multi-point constraints) wherein C is the number of
constraint equations, RCG is a constraint coefficient matrix and YC is a vector of constraint values. For
example, if all of the constraints were single point constraints, then all of the coefficients in any one
row of RCG would be zero except for one unity value. In addition, if all of these single point constraints
were for DOF’s that are grounded, then all of the $Y_C$ values would be zero and these single point
constraints would all have the form of $u_i = 0$.

The unknowns in 8.1 are the UG displacements and the $q_C$ generalized constraint forces and there
are G+C equations to solve for these unknowns. As will be explained later, direct solution of the $q_C$
constraint forces will not be made.

The qC generalized forces of constraint do not necessarily have any physical meaning. Rather, the
G-set nodal forces of constraint are of interest and are expressed in terms of the $q_C$ as:

$$ Q_G = R_{CG}^T q_C $$

In order to reduce 8.1 the G-set is partitioned into the N and M-sets, where the M DOF's are to be
eliminated using the multi-point constraints (from rigid elements as well as MPC Bulk Data entries
defined by the user in the input data deck). The UN are the remainder of the DOF's in the G-set.
Thus, write $U_G$ as:

$$ U_G = \begin{Bmatrix} U_N \\  
                         U_M \end{Bmatrix} $$

The number of constraints is C which is equal to M+S (where S is the number of DOF's in the S set).
Thus, partition $q_C$ and $Y_C$ as:

$$ q_C = \begin{Bmatrix} q_S \\
                         q_M \end{Bmatrix} $$

 $$ Y_C = \begin{Bmatrix} Y_S \\
                          0_M \end{Bmatrix} $$

$0_M$ is a column vector of M zeros. That is, only the S-set can have nonzero constraint values.
With the second of 8.4 in mind, partition the second of equations 8.1 using 8.3 as:

$$ \begin{bmatrix} R_{SN} & 0_{SM} \\
                   R_{MN} & R_{MM} \end{bmatrix} 
                   \begin{Bmatrix}    U_N \\ 
                                      U_M \end{Bmatrix} =
                   \begin{Bmatrix} Y_S \\ 
                                   0_M \end{Bmatrix} $$

The $0_{SM}$ partition is an S x M matrix of zeros. This is required by the form of the single point
constraint equations which are all of the form $u_i = Y_i$ where $Y_i$ is a constant (zero or some enforced
displacement value).

Using 8.3, partition the first of equations 8.1 as:

$$ \begin{bmatrix} M_{SN} & M_{SM} \\
                   M_{MN} & M_{MM} \end{bmatrix} 
                   \begin{Bmatrix}    \ddot U_N \\ 
                                      \ddot U_M \end{Bmatrix} +
   \begin{bmatrix} K_{SN} & K_{SM} \\
                   K_{MN} & K_{MM} \end{bmatrix} 
                   \begin{Bmatrix}    U_N \\ 
                                      U_M \end{Bmatrix} =
                   \begin{Bmatrix}    P_N \\ 
                                      P_M \end{Bmatrix} +
 \begin{bmatrix} R_{SN}^T & 0_{MN}^T \\
                 0_{SM}^T & R_{MM}^T \end{bmatrix} 
                   \begin{Bmatrix}    q_S \\ 
                                      q_M \end{Bmatrix} $$

$$ \textsf{\color{red} start} $$

$$ R_{MN} U_N + R_{MM} U_M $$

$$ R_{MM} U_M = -R_{MN} U_N $$

$$ U_M =   \underbrace{ -R_{MM}^{-1} R_{MN} }_\text{GMN} U_N $$

$$ U_M =  -R_{MM}^{-1} R_{MN} U_N $$

or:

$$ U_M = G_{MN} U_N $$

where:

$$ G_{MN} = -R_{MM}^{-1} R_{MN} $$

$$ \textsf{\color{red} end} $$

The bars over the N-set mass, stiffness and loads matrices are used for convenience to distinguish
these terms from those that will result from the reduction of the G-set to the N-set. From the second
of the constraint equations in 8.5 solve for UM in terms of UN:

$$ U_M = G_{MN} U_N $$

where:

$$ G_{MN} = -R_{MM}^{-1} R_{MN} $$

Using 8.7, equation 8.3 can be written as:

$$ U_G = \begin{Bmatrix}    U_N \\ 
                            U_M \end{Bmatrix} = 
         \begin{Bmatrix}    I_{NN} \\ 
                            G_{MN} \end{Bmatrix} U_N $$

where $I_{NN}$ is an identity matrix of size N.

Substitute 8.9 into 8.6 and premultiply the result by the transpose of the coefficient matrix in 8.9. The
result can be written as:

$$ M_{NN} \ddot U_N + K_{NN} U_N = P_N + 
  \begin{bmatrix} R_{SN}^T & (R_{MN}^T + G_{MN}^T R_{MM}^T) \end{bmatrix}
  \begin{Bmatrix} q_S \\
                  q_M \end{Bmatrix} $$


where:

$$ \xoverline K_{NN} = K_{NN} + K_{NM} G_{MN} + (K_{NM} G_{MN})^T + G_{MN}^T K_{MM} G_{MN} $$

$$ \xoverline M_{NN} = M_{NN} + M_{NM} G_{MN} + (M_{NM} G_{MN})^T + G_{MN}^T M_{MM} G_{MN} $$

$$ P_N = \xoverline P_N + G_{MN}^T P_M $$

$M_{NN}$, $K_{NN}$ and $P_N$ are the reduced N-set mass stiffness and loads. Note that $P_N$ is not the set of
applied loads on the N-set if there are applied loads on the M-set as expressed by the second of
equations 8.11 ($P_N$ are the applied loads on the N set).

In addition, the second term in the square brackets in 8.10 is zero by the definition of $G_{MN}$ in 8.8 so
that 8.10 and 8.5 can be written as:

$$ M_{NN} \ddot U_N + K_{NN} U_N = P_N + R_{SN}^T q_S $$

## 8.3 Reduction of the N-set to the F-set

The N-set can now be partitioned into the F and S-sets where the S DOF's are to be eliminated using
the single point constraints identified by the user in the input data deck. The F-set are the remainder
of the DOF’s in the N-set and are known as the "free" DOF's (i.e. those that have no constraints
imposed on them). Thus, partition UN into UF and US:

$$ U_N = \begin{Bmatrix}    U_F \\ 
                            U_S \end{Bmatrix} $$

Rewrite equation 8.5 in terms of the F, S and M-sets with the restriction that the single point
constraints are of the form $u_i = Y_i$ where $Y_i$ is a constant (zero or some enforced displacement value),
using:

asdf

where $0_{SF}$ is an S x F matrix of zeros and ISS is an S size identity matrix. Equation 8.5 can be written
as:

asdf

Substitute 8.13 and the first of 8.14 into 8.12 and partition the mass, stiffness and load matrices into
the F and S-sets to get:

asdf

Note that $0_{SF}$ is the transpose of 0FS and is an S x F matrix of zero’s. From the first of 8.15 it is seen
that the single point constraints are of the form:

asdf

where $Y_S$ is a column matrix of known constant displacement values (either zero or some enforced
displacement). This agrees with the single point constraint form discussed above; that is, single point
constraints express one DOF as being equal to a constant.

Substituting 8.17 into the first of 8.16 results in the equations for the F-set displacements:

asdf

At this point the F-set equations in 8.18 can be solved for since there are F unknowns and F
equations with which to solve for them. However, MYSTRAN also allows for a Guyan reduction
which, although not generally used in static analysis, may be relevant for eigenvalue analysis. In
eigenvalue analyses by the GIV method (see EIGR Bulk Data entry), the mass matrix must be
nonsingular. In a situation where the model has no mass for the rotational DOF’s, the mass matrix
would be singular. Guyan reduction to statically condense massless DOF’s will result in a
nonsingular mass matrix. Thus, if the user identifies an O set, there is a further reduction; that from
the F-set to the A-set.


## 8.4 Reduction of the F-set to the A-set

The F-set is partitioned into the A and O-sets where the O DOF's are to be eliminated using Guyan
reduction identified by the user either through the use of ASET/ASET1 or OMIT/OMIT1 entries in the
input data deck. The A-set are the remainder of the DOF’s in the F-set and are known as the
"analysis" DOF's. Thus, partition $U_F$ into $U_A$ and $U_O$:

Substitute 8.20 into 8.18 and partition the stiffness and load matrices into the A and O-sets to get:
asdf

Guyan reduction is only exact, in general, for a statics problem. In a dynamic problem it is only exact
if there is no mass on the O-set. In order to explain the Guyan reduction, consider equation 8.21 for a
statics problem:

In a static analysis ($\ddot U=0$) the second of 8.21 can be used to get:

asdf

From the 2nd of 8.22 we can solve for $U_O$ in terms of $U_A$. We can then write:


The first part of the first equation in 8.23 suggests the possibility of using:

adsf

Using 8.24 in 8.22 and premutiplying by the transpose of the coefficient matrix in 8.24 yields:

Which is exactly what would have been found if 8.23 had been substituted into 8.22 for UO .
Equation 8.24 to can be used as a way to eliminate the O-set degrees of freedom for the dynamic
system of equations in 8.21. This would be an approximation unless there was no mass associated
with the O-set degrees of freedom and is the classic Guyan reduction approximation made in
dynamic analyses in which the O-set is eliminated by static condensation (i.e. using the GOA in
equation 8.23). Using 8.24 in 8.21 yields:

adsf

where:

asdf

Now, equation 8.27 can be solved for the A-set DOF displacements. The process of recovering the
displacements of the O, S and M-set displacements is accomplished by reversing the process we just
went through in the reduction. First, the O set displacements are recovered using 8.23. The
combination of the A and O-sets yields the F-set. The S-set is given by 8.17. The combination of the
F and S-sets yields the N-set. The M-set is recovered from the N-set by 8.7 and the combination of
the N and M-sets yield the complete model displacements in the G-set.

## 8.5 Reduction of the A-set to the L-set

The A-set is partitioned into the L and R-sets where the R DOF’s are boundary DOF’s where one
substructure attaches to another in Craig-Bampton (CB) analyses. The modal properties of the
substructure in CB analysis are fixed boundary modes so that, for the modal portion of CB, the R-set
are constrained to zero. The development of the subsequent CB equations of motion in terms of the
modal and boundary DOF’s will not be presented here. See Appendix D and reference 11 for a
complete discussion of CB analyses. For other analyses there is no R-set so that the L set is the
same as the A set for solution of the independent degrees of freedom

asdf

## 8.6 Solution for constraint forces

The constraint forces are recovered as follows. Rewrite 8.2 by partitioning QG into QF, QN and QM
and partitioning qC into qS and qM. Using the coefficient matrix in 8.15 for RCG we get, for QG:

asdf

As discussed earlier, the distinction between the q and Q is that the former are generalized forces of
constraint and the later are physical constraint forces on the DOF’s of the model. It is the Q
constraint forces that are of interest.

Rewrite 8.28 as:

asdf

The first term in 8.30 represents the forces of single point constraint and the second the forces of
multi-point constraint. Comparing 8.29 and 8.30:

asdf

From the first of 8.31 it is seen that the grid point SPC constraint forces are equal to the generalized
qS forces. Using 8.17 and the second of 8.16 (keeping in mind that the derivatives of the S-set
degrees of freedom are zero due to 8.17) the qS, or QS is:

asdf

Thus, there are SPC forces only on the S-set DOF’s:

asdf

From the second of 8.31 and using 8.14 it is seen that the MPC forces can be written as:

adsf

Substituting 8.34 into 8.33 yields:


Using 8.8 this becomes:

asdf

This can also be written as:

asdf

There are MPC forces on the N-set (which includes the F and S-sets) as well as on the M-set.
Equations 8.32 and 8.36 (or 8.37) are used to determine the constraint forces once the UG are found.
This completes the derivation of the solution for the G-set displacements and the constraint forces.
However, it is of interest to demonstrate that the constraint forces satisfy the principal of virtual work
(that is, constraint forces do no virtual work).

Let WC be the work done by the constraint forces and $\delta W_C$ the virtual work done by the constraint
forces. Write $\delta W_C$ as:

asdf

The virtual work of the constraint forces is equal to the constraint forces moving through a virtual
displacement, $\delta U$. Thus:

asdf

...
