#' @title Standard date formats
#'
#' @description A \code{tibble} Dates standard dataframe. Extensible by the package author
#'
#' @docType data
#'
#' @details Standard date formats for cleaning the provided dates from users
#'
#' @usage data(dateformats)
#'
#' @keywords datasets
#'
#' @format A \code{tibble} with 2 columns and 52 rows of date formats.
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' data("dateformats")
#' dateformats
#' }
#'
#'
#'
"dateformats"


#' @title Standard species ecological ranges
#'
#' @description A \code{tibble} Extensible from Freshwater Information Platform  (Schmidt-Kloiber et al., 2019)
#'
#' @docType data
#'
#' @details Species ecological parameters such as ecological ranges both native and alien
#'
#' @usage data(ndata)
#'
#' @keywords freshwater information platform datasets
#'
#' @format A \code{tibble} 637 rows and 129 columns.
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' data("ndata")
#' ndata
#' }
#'
#'
#'
#'
#' @references Schmidt-Kloiber, A., Bremerich, V., De Wever, A. et al. The Freshwater Information Platform:
#' a global online network providing data, tools and resources for science and policy support. Hydrobiologia 838, 1–11 (2019).
#'  https://doi.org/10.1007/s10750-019-03985-5
#'
"ndata"

#' @title Efiplus data used to develop ecological sensitivity parameters for riverine species in European streams and rivers.
#'
#' @description A \code{tibble}
#'
#' @docType data
#'
#' @details BQEs sensitivity to global/climate change in European rivers:
#' implications for reference conditions and pressure-impact-recovery chains (Logez et al. 2012). An extract has been made for
#' usage in this package but for more information write to ihg@boku.ac.at
#'
#' @usage data(efidata)
#'
#' @keywords European wide dataset
#'
#' @format A \code{tibble} 99 rows and 23 columns.
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' data("efidata")
#' efidata
#' }
#'
#'
#'
#'
#' @references Logez M, Belliard J, Melcher A, Kremser H, Pletterbauer F, Schmutz S, Gorges G, Delaigue O, Pont D. 2012.
#' Deliverable D5.1-3: BQEs sensitivity to global/climate change in European rivers: implications for reference conditions
#' and pressure-impact-recovery chains.
#'
"efidata"



#' @title Joint Danube Survey Data
#'
#' @description A \code{tibble} Data on a five year periodic data collection within the Danube River Basin.
#' For more information, please visit https://www.danubesurvey.org/jds4/about
#'
#' @docType data
#'
#' @details Species ecological parameters such as ecological ranges both native and alien
#'
#' @usage data(jdsdata)
#'
#' @keywords freshwater information platform datasets
#'
#' @format A \code{tibble} 98 rows and 24 columns.
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' data("jdsdata")
#' jdsdata
#' }
#'
#'
#'
#'
#' @references https://www.danubesurvey.org/jds4/about
#'
"jdsdata"


#' @title Sequental fences constants
#'
#' @description A \code{tibble} data with k constants for sequential fences method.
#'
#' @docType data
#'
#' @details k constants fro flagging outliers with several chnages in the fences.
#'
#' @usage data(kdat)
#'
#' @keywords constants sequential fences
#'
#' @format A \code{tibble} 101 rows and 2 columns.
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' data("kdat")
#' kdat
#' }
#'
#'
#'
#'
#' @references Schwertman NC, de Silva R. 2007. Identifying outliers with sequential fences.
#' Computational Statistics and Data Analysis 51:3800–3810.
#'
"kdat"


#' @title mth datasets with constant at each confidence interval levels.
#'
#' @description A \code{tibble} The data consist the
#'
#' @docType data
#'
#' @details The data is extracted from \code{(Schwertman & de Silva 2007)}.
#'
#' @usage data(mth)
#'
#' @keywords Standard dataset with constants used to compute outlier at a particular thresholds.
#'
#' @format A \code{tibble} 7 rows and 9 columns.
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' data("mth")
#' mth
#' }
#'
#'
#'
#' @references Schwertman NC, de Silva R. 2007. Identifying outliers with sequential fences.
#' Computational Statistics and Data Analysis 51:3800–3810.
#'
"mth"


#' @title Dataset with species NATURA2000 categories.
#'
#' @description A \code{tibble} The data consist of 388303 rows and 19 columns which represents the
#'        different species on the list. The list was downloaded from the EEA website and manually
#'        archived in the package for use. For more information and citation
#'
#' @docType data
#'
#' @details The data is extracted from \code{(https://www.eea.europa.eu/themes/biodiversity/natura-2000)}.
#'
#' @usage data(naturalist)
#'
#' @keywords NATURA2000 NATURA2000 fish.
#'
#' @format A \code{tibble} 388303 rows and i9 columns.
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' data("naturalist")
#' naturalist
#' }
#
#'
#' @references https://www.eea.europa.eu/themes/biodiversity/natura-2000.
#'
"naturalist"








