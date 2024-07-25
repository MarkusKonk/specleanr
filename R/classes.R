#' @title Data online class
#'
#' @slot output List of datasets from each database.
#'
#' @export
#'
setClass(Class = 'dataonline',
         representation = list(output='list')
)

#Online df summary

#' @title Method for viewing details of the downloaded data.

#' @docType methods
#' @param object The data online model for displaying data.
#'
#' @importFrom methods show
#'
#' @export
#'
setMethod(f='show', signature = 'dataonline', definition = function(object){
            cat(" ======================================",'\n',
                '      Data Online Summary','\n',
                "======================================",'\n',
                'Number of species        :',   length(object@output),'\n',
                'Number of databases      :',   length(object@output[[1]]),'\n',
                'Names of databases       :',   sapply(names(object@output[[1]]),
                                                       paste0, collapse=' '),'\n',
                "======================================")
          }
)

#' @title Outlier detection class for multiple methods
#'
#' @slot result List of data sets with outliers detected.
#' @slot mode Either ´TRUE´ for multiple species and FALSE for one species.
#' @slot varused The variable used for outlier detection, useful for univariate outlier detection methods.
#' @slot out Either outliers or clean dataset outputted.
#' @slot methodsused The methods used in outlier detection.
#' @slot dfname the dataframe name to aid tracking it during clean data extraction.
#' @slot excluded whether some columns were excluded during outlier detection. useful for multivariate methods where coordinates are removed from the data.
#'
#' @export
#'


setClass(Class = 'datacleaner',
         representation = list(result='list',
                               mode = 'logical',
                               varused = 'character',
                               out ='character',
                               methodsused='vector',
                               dfname='character',
                               excluded ='vector')
)


#' @title set method for displaying output details after outlier detection.
#'
#' @param object The data model for outlier detection.
#'
#'
#' @export
#'
setMethod(f='show', signature = 'datacleaner', definition = function(object){

  if(object@mode==FALSE){
    cat("======================================",'\n',
        ' Data cleaning summary','\n',
        "======================================",'\n',
        'Number of species        :',   1,'\n',
        'Number of methods        :',   length(object@result),'\n',
        'Methods used             :',   paste(object@methodsused, collapse = ','), '\n',
        'Multiple                 :',   object@mode,'\n',
        'Variable                 :',   noquote(object@varused),'\n',
        'Output                   :',   noquote(object@out),'\n',
        'Dataset Name             :',   noquote(object@dfname),'\n',
        'Excluded columns         :',   paste(object@excluded, collapse = ','), '\n',
        "======================================")
  }else{
    cat(" ======================================",'\n',
        ' Data cleaning summary','\n',
        "======================================",'\n',
        'Number of species        :',   length(object@result),'\n',
        'Number of methods        :',   length(object@result[[1]]),'\n',
        'Methods used             :',   paste(object@methodsused, collapse = ','), '\n',
        'Multiple                 :',   object@mode,'\n',
        'Variable                 :',   noquote(object@varused),'\n',
        'Output                   :',   noquote(object@out),'\n',
        'Dataset Name             :',   noquote(object@dfname),'\n',
        'Excluded columns         :',   paste(object@excluded, collapse = ','), '\n',
        "======================================")
  }
}
)
