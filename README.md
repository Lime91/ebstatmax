# ebstatmax
Simulation-based computation of power and type-I error of statistical testing procedures for the EBStatMax project. Currently, `nparLD` and various `GPC` methods are implemented.

## Contents

The main part of this project is the `diacerein.R` command line tool that provides various options to perform simulations. Its name is an allusion to the clinical trial conducted by [Vally et al. 2018](https://www.sciencedirect.com/science/article/pii/S0190962218301300), in which a drug called *Diacerein* was applied to treat Epidermolysis Bullosa. The provided tool is purely implemented in `R`. Internally, `diacerein.R` uses various utility functions and global `CONFIG` object from the supplied `simUtils/` package.

The original study data is provided in the `data/` subdirectory. It is used to compute rejection rates of the implemented statistical hypotheses tests. Various command line arguments of `diacerein.R` determine how this data is used and manipulated during simulations.

## Install and Run

### Requirements
In order to run the script, you need to install `R` (the project was developed with version `3.6.3`). Additionally, the following `R` packages are required:
  - `optparse` (`1.7.1`)
  - `data.table` (`1.12.8`)
  - `dplyr` (`1.0.7`)
  - `nparLD` (`2.1`)
  - `jsonlite` (`1.7.2`)
  - `devtools` (`2.2.2`)

The version number is the one used to develop this project.

### Execute
Perform the following steps:
  - Clone this repo
  - Type `cd ebstatmax`
  - On Linux, type `./diacerein.R --help` to familiarize yourself with the available program options. This requires an `Rscript` executable located in `/usr/bin/`. If you're using Windows (or if `Rscript` is located elsewhere), type `Rscript diacerein.R --help`. Make sure the `Rscript` command is detectable via the `PATH` environment variable.
  - Each program invocation simulates either type-I error or power in a specific scenario. For instance, if you want to simulate the power of `nparLD` when normally-distributed effects are added (at post-treatment time) to the *Pruritus* variable in the placebo group, then invoke the program with arguments `-m nparLD -e norm -t Pruritus`. The strenght of the normally-distributed effects is determined in the config file `simUtils/R/config.R`.
