MYSTRAN
=======

MYSTRAN is an acronym for “My Structural Analysis” (https://www.mystran.com)


---

[Build Instructions](#Build-Instructions) |
[Introduction](#Introduction) |
[Features](#Features) |
[Get EXE or Make Binary](#Get-EXE-or-Make-Binary) |
[Documentation](#Documentation) |
[Five Repositories](#Five-Repositories) |
[Developmental Goals](#Developmental-Goals) |
[Ways You Can Help](#ways-you-can-help) |
[Community](#community)

---

# Build Instructions

See [BUILD.md](BUILD.md) for both Windows and Linux build (compiling) instructions.

# Introduction

MYSTRAN is a general purpose finite element analysis computer program for
structures that can be modeled as linear (i.e. displacements, forces and
stresses proportional to applied load). MYSTRAN is an acronym for
“My Structural Analysis”, to indicate its usefulness in solving a wide variety
of finite element analysis problems.

For anyone familiar with the popular NASTRAN computer program developed by NASA
(National Aeronautics and Space Administration) in the 1970’s and popularized
in several commercial versions since, the input to MYSTRAN will look quite
familiar. Many structural analyses modeled for execution in NASTRAN will
execute in MYSTRAN with little, or no, modification. MYSTRAN, however, is not
NASTRAN. It is an independent program written in modern Fortran 95.

# Features

- Nastran compatibility
- Linear Static Analysis
- Modal analysis
- Linear Elastic Buckling Analysis
- Full Suite of 1D, 2D, and 3D elements
- Support for Classical Laminated Plate Theory
- OP2 Support

# Get EXE or Make Binary

Windows EXE (executable) for can be found in the "Releases" section of this page (right hand pane).
Alternatively, there is a repository that contains older releases: [MYSTRAN Releases](https://github.com/MYSTRANsolver/MYSTRAN_Releases)

Static Linux binaries have been built, but releases are in work.
For now, it is better to build it yourself -- it's really
straightforward.

# Documentation

The end user documentation is located the [MYSTRAN_Documentation](https://github.com/MYSTRANsolver/MYSTRAN_Documentation) repository.
This includes a Quick Setup Guide, User Manual, and Theory Manual.

# Five Repositories

The MYSTRAN project consits of five repositoires.

1 - This repository contains the source code and build instructions.

2 - The [MYSTRAN_Documentation](https://github.com/MYSTRANsolver/MYSTRAN_Documentation) repository contains various document related to the MYSTRAN program.

3 - The [MYSTRAN_Resources](https://github.com/MYSTRANsolver/MYSTRAN_Resources) repository consists of files for MYSTRAN developers.
It also contains information and files related to pre- and post-processors relavant to MYSTRAN.

4 - The [MYSTRAN_Releases](https://github.com/MYSTRANsolver/MYSTRAN_Releases) repository contains older releases.

5 - The [MYSTRAN_Benchmark](https://github.com/MYSTRANsolver/MYSTRAN_Benchmark) repository contains the test cases and utilities used to verify that a new build produces results consistent with prior builds and models that have been verified.


# Developmental Goals

- Continue the implementation of the MITC shell elements and shell element buckling capability
- Validation effort (hundreds/thousands of test cases)
- Discover and resolve bugs
- Improve performance


# Ways You Can Help

- Join the MYSTRAN Discord Channel and/or Forum (links below)
- Report bugs and inconsistencies
- Report issues with Documentation
- A large validaiton effort is underway. This will require the assistance of the community. Any help would be greatly appreciated.

# Community

- [Join our Discord Channel](https://discord.gg/9k76SkHpHM) - Very active.
- [Join our Forums](https://mystran.com/forums) - Little activity. Mostly for archive purposes.
