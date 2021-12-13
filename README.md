# ProteinBase

<!-- badges: start -->

<!-- badges: end -->

**ProteinBase** is built to make some function for proteinomics analysis

## Installation

You can install the development version of ProteinBase from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Moonerss/ProteinBase")
```

## Main

Some useful operations are include:

-   read files: `read_all_file` `read_all_sheets` `decompress`
-   tumor vs normal: `tumor_vs_normal`  
-   calculate NA: `na_ratio` `na_ratio_row` `na_ratio_col`  
-   test: `T_test` `Wilcox_test`
-   group test: `group_diff`  
-   normalization: `normalize_data`  
-   plot: `plot_pca` `plot_venn` `plot_density` `plot_density_by_sample` `qc_boxplot`  
-   useful color: `get_color_palette`
