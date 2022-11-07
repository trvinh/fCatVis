# fCatVis
UI for visualising fCAT results and comparing the completeness assessment between different data sets

## Installation and usage
From an R terminal, PhyloRBF can be installed using *devtools*:

```r
if (!requireNamespace("devtools"))
    install.packages("devtools")
devtools::install_github("trvinh/fCatVis", INSTALL_opts = c('--no-lock'), dependencies = TRUE)
```

Then, to run it, enter:

```r
library(fCatVis)
runFcatVis()
```

## Input

There are 2 kinds of inputs can be uploaded into fCatVis:

- A single summary report from [fCAT](https://github.com/BIONF/fCAT)
- A folder containing several summary reports. Each file has to be named using this format `<species_name>.report_summary.txt` (e.g.: HUMAN.report_summary.txt; CIONA.report_summary.txt; ...)

## Screenshot
<img width="1292" alt="image" src="https://user-images.githubusercontent.com/19269760/183078583-a973d96b-4a62-49d7-8a79-408521c7add3.png">

## Contact

For any bug reports or questions, please [open an issue on GitHub](https://github.com/trvinh/fCatVis/issues/new) or be in touch via email tran@bio.uni-frankfurt.de
