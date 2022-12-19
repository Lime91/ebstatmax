# ebstatmax
Simulation-based computation of power and type-I error of statistical testing procedures for the EBStatMax project. Currently, `nparLD` and various `GPC` methods are implemented.

## Contents

The main part of this project is the `diacerein.R` command line tool that provides various options to perform simulations. Its name alludes to the clinical trial by [Vally et al. 2018](https://www.sciencedirect.com/science/article/pii/S0190962218301300), in which a drug called *Diacerein* was applied to treat Epidermolysis Bullosa. The provided tool is purely implemented in `R`. Internally, `diacerein.R` uses various functions and a global `CONFIG` object from the provided `simUtils` package.

The original study data is provided in the `data/` subdirectory. It is used to compute rejection rates of the implemented statistical hypotheses tests. Various command line arguments of `diacerein.R` determine how this data is used and manipulated during simulations.

## Install and Run

### Requirements
In order to run the script, install `R` (the project was developed with version `4.2.1`). Additionally, the following `R` packages are required:
  - `optparse` (`1.7.3`)
  - `data.table` (`1.14.6`)
  - `dplyr` (`1.0.10`)
  - `nparLD` (`2.2`)
  - `jsonlite` (`1.8.3`)
  - `devtools` (`2.4.5`)

The version number is the one used to develop this project.

### Execute
Perform the following steps:
  - Clone this repo and navigate into the repo root
  - On Linux, type `./diacerein.R --help` to familiarize yourself with the available program options. This requires an `Rscript` executable located in `/usr/bin/`. If you're using Windows (or if `Rscript` is located elsewhere), type `Rscript diacerein.R --help`. Make sure the `Rscript` command is detectable via the `PATH` environment variable.
  - Each program invocation simulates either type-I error or power in a specific scenario. For instance, if you want to simulate the power of `nparLD` when normally-distributed effects are added at post-treatment time only to the *Pruritus* variable in the placebo group, then invoke the program with arguments `-m nparld -e norm -t Pruritus`. Thereby, the size of the random effects is determined in the config file `simUtils/R/config.R`.

### Test
There are unittests available for the `simUtils` package. Execute all unittests with `Rscript -e "devtools::test('./simUtils')"`.

## Support and Copyright

For general questions contact the main developer [Konstantin Emil Thiel](mailto:konstantin.thiel@pmu.ac.at).
For questions regarding `GPC` and its implementation contact co-developer [Johan Verbeeck](mailto:johan.verbeeck@uhasselt.be).


Copyright (C) 2022  Konstantin Emil Thiel

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>. 
