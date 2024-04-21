setClass(Class = 'dataonline',
         representation = list(output='list')
)

#Online df summary

setMethod(f='show', signature = 'dataonline',
          definition = function(object){
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

setClass(Class = 'datacleaner',
         representation = list(result='list',
                               mode = 'logical',
                               varused = 'character',
                               out ='character',
                               methodsused='vector')
)


setMethod(f='show', signature = 'datacleaner', definition = function(object){

  if(object@mode==FALSE){
    cat(" ======================================",'\n',
        ' Data cleaning summary','\n',
        "======================================",'\n',
        'Number of species        :',   1,'\n',
        'Number of methods        :',   length(object@result),'\n',
        'Methods used             :',   paste(object@methodsused, collapse = ','), '\n',
        'Mode                     :',   object@mode,'\n',
        'Variable                 :',   noquote(object@varused),'\n',
        'Output                   :',   noquote(object@out),'\n',
        "======================================")
  }else{
    cat(" ======================================",'\n',
        ' Data cleaning summary','\n',
        "======================================",'\n',
        'Number of species        :',   length(object@result),'\n',
        'Number of methods        :',   length(object@result[[1]]),'\n',
        'Methods used             :',   paste(object@methodsused, collapse = ','), '\n',
        'Mode                     :',   object@mode,'\n',
        'Variable                 :',   noquote(object@varused),'\n',
        'Output                   :',   noquote(object@out),'\n',
        "======================================")
  }
}
)
#standardize a class
# if(!isGeneric('explore')){
#   setGeneric('explore', function(x, detailed)
#     standardGeneric('explore'))
# }
#
# setMethod(f = 'explore',signature(x = 'datacleaner'),
#           definition = function(x, detailed){
#
#             print(length(x@result))
#
#           }
# )

