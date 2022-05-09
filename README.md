# ebstatmax
Simulation-based computation of power and type-I error of statistical testing procedures for the EBStatMax project. Currently, `nparLD` and various `GPC` methods are implemented.

## Contents

The main part of this project is the `diacerin_sim_cli.R` script. It provides users with a command line interface to perform simulations. This script uses various utility functions and global `CONFIG` object from the supplied `R`-package `simUtils`.

## Install and Run

### Requirements
In order to run the script, you need to install `R` (the project was developed with version `3.6.3`). Additionally, the following `R` packages are required:
  - `optparse` (`1.7.1`)
  - `data.table` (`1.12.8`)
  - `dplyr` (`1.0.7`)
  - `nparLD` (`2.1`)
  - `jsonlite` (`1.7.2`)
  - `devtools`

The version number is, again, the one used to develop this project.

### Execute
Perform the following steps:
  - clone this repo
  - type `cd ebstatmax`
  - On Linux, type `./diacerin_sim_cli.R --help`. This requires that you have an `Rscript` executable located in `/usr/bin/`.
  - On Windows (and on Linux as well), type `Rscript diacerin_sim_cli.R --help`. Make sure the `Rscript` command is detectable via the `PATH` environment variable.
