specleanr package for outlier detection
================

<style>
body {
    text-align: justify;
    font-family: Calibri;
    font-size: 17px;
}
.p {
 font-size:17px;
 font-family: Consolas;
}
</style>
<!-- README.md is generated from README.Rmd. Please edit that file -->

## specleanr

<!-- badges: start -->
<!-- badges: end -->

The package aims to improve the reliability and acceptability of
biogeographical models, including species distribution models,
ecological niche models, and bioclimatic envelope models, by detecting
environmental outliers in the environmental predictors. In the package,
we collate 21 outlier detection methods, which a user can collectively
apply (ensemble outlier detection) and determine whether the species
records are in a suitable environmental space. The package complements
other packages that address geographical, taxonomic, and temporal
checks.

### Process of identifying environmental outliers.

The process of identifying environmental outliers is generally
classified into **four steps** as detailed below;

<!--(![This is the caption!](img/rmarkdown_hex.png))-->

1.  **Arranging of species records and environmental data**.

This includes collecting species data from either online sources or
locally stored data. The user can check for species records for
geographical, taxonomic, or temporal inconsistencies such as missing
coordinates, interchanged coordinates, species name spelling mistakes,
and wrong event dates. Environmental data, mainly in the raster format,
is based chiefly on user needs, but numerous sources include WORLDCLIM
(Fick & Hijmans, 2017) and CHELSA (Karger et al., 2017) for bioclimatic
variables; Hydrography90m for stream or river-based hydromorphological
parameters such as stream order, flow accumulation, stream power index,
and stream transportation index (Amatulli et al., 2022); and Copernicus
for land use changes <https://www.copernicus.eu/en>.

2.  **Extracting the environmental variables**.

In this step the environmental predictors are extracted from where the
species was recorded or absent. The extracted dataset forms the species
**reference dataset** for environmental outlier checks. In the package
we included **`pred_extract()`** to extract the environmental
predictors.

3.  **Ensemble multiple methods for outlier detection**.

Multiple outlier detection methods are used; each method flags outliers
in the same dataset. These outliers are then compared among methods to
determine records, which are flagged by several methods called
**absolute outliers** or **true outliers**. The total number of methods
that a user can ensemble is user-based; however, at least 3 are expected
to be set. The methods should be also at least from different
categories. There are three main categories of outlier detection
methods, namely **1) univariate methods**, **2) multivariate methods**,
and **3) ecological ranges**. All the methods are set in
**`multdetect()`** function and not individually to allow seamless
comparison.

**Univariate methods**

These methods only consider one environmental predictor. It is advisable
that an environmental predictor which directly affects the species
should be used, for example, minimum temperature of the coldest month
(IUCN 2012; Logez et al., 2012).

| Function           | Method implemented                     | Userword in **`multdetect()`** |
|:-------------------|:---------------------------------------|-------------------------------:|
| `zscore()`         | Z-score                                |                         zscore |
| `semiIQR()`        | Semi interquartile range               |                         semiqr |
| `adjustboxplots()` | Adjusted boxplot-robust boxplot method |                         adjbox |
| `interquartile()`  | Interquartile range (IQR)              |                            iqr |
| `medianrule()`     | Median rule method                     |                     medianrule |
| `logboxplot()`     | Logarithmic boxplot                    |                     logboxplot |
| `seqfences()`      | Sequential fences                      |                      seqfences |
| `mixediqr()`       | Mixed semi and interquartile range     |                       mixediqr |
| `distboxplot()`    | distribution-based boxplots            |                    distboxplot |
| `rjknife()`        | Reverse jackknifing                    |                         jknife |
| `hampel()`         | The Hampel filter method               |                         hampel |

**Multivariate methods**

They consider multiple environmental predictors in detecting outliers in
the environmental data. In the package, we ensure that the user can
exclude particular columns, such as the coordinates (latitude and
longitude), so they are not included in the computation.

| Function      | Method used to fit and detect outliers       | Userword in **`multdetect()`** |
|:--------------|:---------------------------------------------|-------------------------------:|
| `isoforest()` | Isolation forest                             |                        iforest |
| `onesvm()`    | One-class support vector machine             |                        onvesvm |
| `xglosh()`    | Global-Local Outlier Score from Hierarchies. |                          glosh |
| `xknn()`      | k-nearest neighbor                           |                            knn |
| `xlof()`      | Local outlier factor                         |                            lof |
| `xkmeans()`   | k-means clustering                           |                         kmeans |
| `xkmedoids()` | Partitioning around the kmedoids             |                        kmedoid |
| `xkmedian()`  | k-medians clustering                         |                        kmedian |
| `mahal()`     | Mahalanobis distances both robust and simple |                          mahal |

**Ecological ranges**

The user collates the species optimal ecological ranges to identify the
values outside the known optimal ranges. In the package, for a single
species, the optimal ranges (mininmu, maximum, or mean values) are
indicated manually, and the user sets the environmental variables to be
used for flagging the outliers. A dataset with the minimum and maximum
values is allowed for multiple species. If the taxa is fish, we include
the **`thermal_range()`** and **`geo_range()`** functions, which a user
can set to flag records exceeding the FishBase collated temperature and
latitudinal/longitudinal ranges. The user word **optimal** is used in
**`multdetect()`** function for seamless comparisons with other methods.

4.  **Extract species environmental without outliers**

The **reference dataset** in **Step 2** and lists or outliers flagged by
each method in **Step 3** are then used to retain the **clean dataset**.
Under the hood, two approaches are implemented **1) absolute method**
where absolute outliers are removed at a particular threshold or **2)
suitable method** where a method with highest proportion of absolute
outliers and has highest similarity with other methods (in terms of the
outliers flagged) can be used. The **threshold** parameter is a measure
of proportion of the methods that have flagged the record as an outlier
to the total number of methods used.

- `extract_clean_data()` to extract clean data using the reference data
  and outliers for single species.
- `mult_extract_data()` to extract data for multiple species.

5.  **Post-environmental outlier removal**

After environmental outlier removal, the user can examine the
improvement in the model performance before and after environmental
outlier removal. The following function can be used.

- `sdm_fit()` fit Generalized Linear Model (GLM) or Random Forest (RF).
- `model_comp` to compare model performance before and outlier removal
  using threshold-dependent and independent metrics such as Area Under
  the Curve, true skill statistics, specificity, sensitivity, kappa, and
  accuracy.
- `extract_performance` to obtain the dataset with the performance
  values for both scenarios.

### Package installation

\*\*Development version on GitHub:

``` r
# install.packages("remotes")
remotes::install_github("AnthonyBasooma/specleanr")
```

\*\*CRAN version

``` r
###cran not yet
```

### Package citation

Basooma et al.. Under review

### References

1.  Amatulli, G., Garcia Marquez, J., Sethi, T., Kiesel, J.,
    Grigoropoulou, A., Üblacker, M. M., Shen, L. Q., & Domisch, S.
    (2022). Hydrography90m: A new high-resolution global hydrographic
    dataset. Earth System Science Data, 14(10), 4525–4550.
    <https://doi.org/10.5194/essd-14-4525-2022>

2.  Fick, S. E., & Hijmans, R. J. (2017). WorldClim 2: new 1-km spatial
    resolution climate surfaces for global land areas. International
    Journal of Climatology, 37(12), 4302–4315.
    <https://doi.org/10.1002/joc.5086>

3.  Karger, D. N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H.,
    Soria-Auza, R. W., Zimmermann, N. E., Linder, H. P., & Kessler, M.
    (2017). Climatologies at high resolution for the earth’s land
    surface areas. Scientific Data, 4.
    <https://doi.org/10.1038/sdata.2017.122>

4.  Logez, M., Belliard, J., Melcher, A., Kremser, H., Pletterbauer, F.,
    Schmutz, S., Gorges, G., Delaigue, O., & Pont, D. (2012).
    Deliverable D5.1-3: BQEs sensitivity to global/climate change in
    European rivers: implications for reference conditions and
    pressure-impact-recovery chains.

5.  IUCN Standards and Petitions Committee. (2022). THE IUCN RED LIST OF
    THREATENED SPECIESTM Guidelines for Using the IUCN Red List
    Categories and Criteria Prepared by the Standards and Petitions
    Committee of the IUCN Species Survival Commission.
    <https://www.iucnredlist.org/documents/RedListGuidelines.pdf>.
