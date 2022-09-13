MYSTRAN
=======

MYSTRAN is an acronym for “My Structural Analysis” (https://www.mystran.com)


---

[Introduction](#Introduction) |
[Features](#Features) |
[Installation Instructions](#Installation-Instructions) |
[Build Instructions](#Build-Instructions) |
[Developmental Goals](#Developmental-Goals) |
[Ways You Can Help](#ways-you-can-help) |
[Community](#community)

---


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

As a main overview, see all four of the [MYSTRAN Repositories](https://github.com/MYSTRANsolver)

# Features

- NASTRAN compatibility
- Modal analysis
- Linear Static Analysis
- Linear Elastic Buckling Analysis (All But Shell Elements)
- Support for True Classical Laminate Plate Theory
- Documentation can be found in the 
[MYSTRAN Documentation Repository](https://github.com/MYSTRANsolver/MYSTRAN_Documentation)

# Installation Instructions

Windows binaries for can be found in the 
[MYSTRAN Releases Repository](https://github.com/MYSTRANsolver/MYSTRAN_Releases)

Static Linux binaries have been built, but we're still working on releases.
For now, you'll have better luck building it yourself -- it's really
straightforward.

# Build Instructions

Check out [BUILDING.md](BUILDING.md) for Windows and Linux build (compiling)
instructions.

# Developmental Goals

- Implement the MITC shell elements and shell element buckling capability
- Implement OP2 output format
- Creating easier ways to acquire MYSTRAN would be nice. This would include, but
  is not limited to, entry into the Arch Linux User Repository (AUR), the
  Debian Advanced Package Manager (apt), the snapcraft store (snap), the
  chocolatey package manager for Windows, an AppImage, or flatpak.
- As a longer term goal, geometric nonlinear support is desirable.

# Ways You Can Help

- Join the MYSTRAN forum and/or Discord Channel
- Contribute your MYSTRAN runs to the list of demonstration problems
- Report bugs and inconsistencies

# Community

- [Join our Forums](https://mystran.com/forums)
- [Join our Discord Channel](https://discord.gg/BAdT6rJyFF)
