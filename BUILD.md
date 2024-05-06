# Building MYSTRAN from source

###### Last updated 2023-12-18.

## Setting up a build environment

In order to build (compile) MYSTRAN using CMake, you first have to set up a
proper build environment (i.e. toolchain and required programs/libraries).

You can skip this part if you've done it already (or if you really know what
you're doing).

### Steps for Windows (x86_64)

First, download and install MSYS2 from the
[official site](https://www.msys2.org/).

Open the MSYS2 terminal and run the following commands:

  1. **`pacman -Syu`**
This updates repository information and installed packages, and might require
you close and reopen MSYS2 terminals.
  1. **`pacman -S mingw-w64-x86_64-gcc-fortran mingw-w64-x86_64-cmake mingw-w64-x86_64-make git`**
This installs the required compilers (the GNU C and Fortran compilers), CMake
itself, and `git`.
  1. **`export PATH="/mingw64/bin:$PATH"`**
This makes the MinGW toolchain programs (such as `make` and the compilers)
visible so CMake can find them more easily. Note that this command's effects
are lost when you reopen the terminal, so you might want to append it to your
`~/.bashrc` to save time.

### Steps for Linux (any)

Follow your distribution's steps to install the following programs/libraries:
  - **`gcc`**
  - **`g++`**
  - **`gfortran`**
  - **`make`**
  - **`cmake`**
  - **`git`**

All of those are fairly common, so get in touch in the MYSTRAN Forums or
MYSTRAN Discord if you have trouble installing any of them. Also, note that
most distros have a "base" package group for developers (e.g. Arch's
`base-devel` or Ubuntu's `build-essential`) that includes necessary tooling
such as `gcc` and `make`. If that's the case, install it!

If your distribution doesn't ship CMake 3.18+ yet, check if your distro has a
some sort of testing/unstable channel before attempting to
[install it manually](https://cmake.org/install/).

For Ubuntu
```
sudo apt update
sudo apt upgrade
apt install gcc g++ gfortran make cmake git
```

---

## Building MYSTRAN

If your build environment is already set up, building MYSTRAN is quite
straightforward.

### Steps for Windows (any)

  1. Open the MSYS2 shell.
  2. Re-run step #3 of the previous section if needed.
  3. Fetch the source code if you haven't already. If you're using Git, you can
  clone the repo with
  **`git clone https://github.com/MYSTRANsolver/MYSTRAN.git`**.
  4. Move the terminal to the MYSTRAN folder. If you've just run `git clone`,
     just do a **`cd MYSTRAN`**.
  5. Generate the build scripts by running **`cmake -G "MinGW Makefiles" .`**.
  6. Compile with **`mingw32-make`**. If you have an N-core processor, running
  **`mingw32-make -Oline -jN`** will probably be much faster. A good choice of N is
  printed in the previous step, right before the end. The `-Oline` argument prevents garbled output when `N` > 1.
  7. The executable will reside at **`Binaries/mystran.exe`**.

### Steps for Linux (any)

  1. Open a terminal.
  2. Fetch the source code if you haven't already. If you're using Git, you can
  clone the repo with
  **`git clone https://github.com/MYSTRANsolver/MYSTRAN.git`**.
  3. Move the terminal to the MYSTRAN folder. If you've just run `git clone`,
  just do a **`cd MYSTRAN`**.
  1. Generate the build scripts by running **`cmake .`**.
  2. Compile with **`make`**. If you have an N-core processor, running
  **`make -jN`** will probably me much faster. A good choice of N is printed in
  the previous step, right before the end. You can also find the number of
  cores/threads with the `nproc` command (not all distros ship it
  out-of-the-box though).
  1. The executable will reside at **`Binaries/mystran`**.

---

## Troubleshooting

While this process is meant to be straightforward, here is a list of some of
the more common issues that can arise. Other issues users find might be added
here if they're not too specific.

If your issue isn't here, you can always ask for help at the
[MYSTRAN forums](https://www.mystran.com/forums/) or the
[Discord server](https://discord.gg/9k76SkHpHM)

---

### "I'm getting "file not found" errors when running the step #2 setup command!"

Run a **`pacman -Syyu`** (note the two 'y's) and try again.

---

### "CMake is complaining about not being able to find the toolchain or the Fortran compiler or the "make" command!"

Try running the commands `make`/`mingw32-make`, `gcc`, and `gfortran`. If any
of these comes up as a "command not found", make sure they've been installed.
If you're **sure** they are, they might not be in the PATH.

Windows users, have a look at step #3 of the setup. Linux users, check out your
distro documentation, because whatever's happening should not be happening at
all.

---

### "CMake complains about `ARCHIVE_EXTRACT`!"

Check out the output of `cmake --version`. You must have version 3.18 or newer.
If you don't, first ensure it's up to date -- perform a system-wide update.
Windows users should not find this issue relevant -- MSYS2 ships CMake 3.27.1
as of this writing. Linux users should use their own package manager.

If your system is up to date and you still run into this issue, that means your
distro ships CMake 3.17 or older. Bad luck there. Here's what you can do:

  1. Enable a testing/unstable package channel (not all distros have one)
  2. Install the latest CMake [manually](https://cmake.org/install/)
  (might piss off your package manager)
  1. Download and extract `libf2c.zip` yourself, and comment out the
  `ARCHIVE_EXTRACT` stuff in `CMakeLists.txt`.

---

### "I'm getting random SuperLU build errors!"

SuperLU is included as a submodule. A recent update to the submodule might
require a clean build. Run `make clean` and delete the `superlu` subdirectory
and run the appropriate `cmake` command again.

---

### "I'm getting cryptic linker errors related to BLAS!"

SuperLU requires BLAS. Its build script can look for and link against your
system's installed BLAS implementation (we recommend OpenBLAS). However, your
install might be lacking the appropriate static (`.a`) library files.

If you don't know how to fix that and just want to build, you can use the
integrated BLAS subroutines bundled with the SuperLU source. To do that, run
the appropriate `cmake` command with the extra option
`-Denable_internal_blaslib=YES` *before* the `.` argument.

Please be aware that the bundled CBLAS might be slow when compared to a proper
BLAS install. That might have an impact on the time it takes to run larger
models.

---

### "I want to build offline, but the CMake script attempts to download stuff!"

Download the `superlu` submodule and `libf2c.zip` beforehand, and you should be
fine.

---

### "The terminal output is garbled during compilation!"

Multiple threads are printing to standard output simultaneously. That issue can
sometimes happen as a result of running `make` instead of `mingw32-make` on
Windows, but it can affect both. It's annoying, but harmless.

However, if you *really* need compiler output to be readable, ensure `make`
only runs with one thread by passing the option `-j1`. This will make
compilation slower, but at least you'll be able to read the output.

And if it's errors you're looking for, you can build fast with `-j[number]`,
and then `-j1` just to see the error again.

---

If your issue isn't here, you can always ask for help at the
[MYSTRAN forums](https://www.mystran.com/forums/) or the
[Discord server](https://discord.gg/9k76SkHpHM)
