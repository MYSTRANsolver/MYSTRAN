# Developer Docs

Pardon our dust.  We gotta figure out what should be documented first.  Some topics include:

- General Coding:
   - Building the code
   - Fortran for Dummies
   - Coding Practices
   - Using Git & GitHub so your changes get accepted
   - Terminology

- Basic FEA topics
   - Terminology
   - Solution Procedure
     - Statics
     - Modes
   - Coordinate Transforms

- Element Formulations (i.e., mass/stiffness matrix, stress/strain/force recovery):
   - CELAS1 (spring)
   - CROD (rod)
   - CBAR (bar)
   - CTRIA3 / CQUAD4 (shell)
   - CTETRA / CPENTA / CHEXA (solid)

- Process
   - Adding a new:
      - PARAM card
      - BDF card
      - Element:
        - Adding a mass matrix
        - Adding a stiffness matrix
        - Adding element force recovery
        - Adding element strain recovery
        - Adding element stress recovery

## Code

### Coding Practices:
 - Use "IMPLICIT NONE"
   - Why:  It helps to avoid "magic" behavior in the code and incorrect types (e.g., a variable that should be an integer is cast to a float).
   
 TODO: example...
 
 - Initialize Variables
   - Why: Behavoir of the code can be different under different compilers/compiler versions/platforms/processor.
          Maybe the default is 0 for gfortran under Windows, but 1 for Linux.
   - How: Just initalize your variables.
 
 TODO: example
 
 - How to (unintentionally?) do the wrong thing: 
   - Default variables may be overwritten externally by a parent function.
     Variable "state" may be preserved across different runs, though the intended behavior was for it to reset.
 
 TODO: explicit example
 
