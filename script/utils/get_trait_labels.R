#get trait labels

get_trait_labels <- function(traits_list){
  sapply(traits_list, function(X){
    X$trait_label       
    })
  
}
