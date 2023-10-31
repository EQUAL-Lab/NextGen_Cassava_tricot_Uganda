

set_trait_labels <- function(trial_data, pattern){
  names(trial_data)
  
  # get the an organized list of traits
  traits <- getTraitList(trial_data, pattern)
  
  #to see the trait labels, run the code below
  trait_labels <- lapply(traits, function(x) {
    x$trait_label
  })
  
  trait_labels <- unlist(trait_labels)
  
  format_label_sufix <- function(sufix, label){
    if(grepl(pattern = sufix, x = label))
      paste(label, " - ", toupper(sufix))
  }
 
  
  # fix trait labels to get better names 
  trait_labels  <- ifelse(grepl("9map", trait_labels), 
                         paste0(trait_labels, " - 9 MAP"),
                         ifelse(grepl("6map", trait_labels), 
                                paste0(trait_labels, " - 6 MAP"),
                                ifelse(grepl("3map", trait_labels), 
                                       paste0(trait_labels, " - 3 MAP"),
                                       ifelse(grepl("12map_boiledroots", trait_labels), 
                                              paste0(trait_labels, " - 12 MAP Boiled Roots"),
                                              ifelse(grepl("12map_flour_based_meal", trait_labels),
                                                     paste0(trait_labels, " - 12 MAP Flour Meal"),
                                                     ifelse(grepl("basedmealdataat12map", trait_labels),#**
                                                            paste0(trait_labels, " - 12 MAP Flour Meal"),
                                                            ifelse(grepl("12map_harvest_field", trait_labels), 
                                                                   paste0(trait_labels, " - 12 MAP Harvest"),
                                                                   
                                                                   ifelse(grepl("d1map", trait_labels), 
                                                                          paste0(trait_labels, " - 1 MAP"),
                                                                          ifelse(grepl("12map", trait_labels), #added for cycle 2 dataset
                                                                                 paste0(trait_labels, " - 12 MAP"),
                                                                                 trait_labels)))))))))
  
  
  
  
  trait_labels <- gsub("d9map_", "", trait_labels)
  trait_labels <- gsub("d6map_", "", trait_labels)
  trait_labels <- gsub("d3map_", "", trait_labels)
  trait_labels <- gsub("d1map_", "", trait_labels)
  trait_labels <- gsub("X12map_harvest_field_", "", trait_labels)
  trait_labels <- gsub("X12map_flour_based_meal_", "", trait_labels)
  trait_labels <- gsub("X12map_boiledroots_", "", trait_labels)
  trait_labels <- gsub("9map", "", trait_labels)
  trait_labels <- gsub("6map", "", trait_labels)
  trait_labels <- gsub("about", "", trait_labels)
  
  #added for processing cycle 2 dataset
  trait_labels <- gsub("tricot2021mid.altitudedatacollection.", "", trait_labels)
  trait_labels <- gsub("tricot2021mid.altitudedata._", "", trait_labels)
  trait_labels <- gsub("tricot2021_2022flour.basedmealdataat12map_qst_", "", trait_labels)
  trait_labels <- gsub("tricot2021_2022fieldharvestdataat12map_qst_", "", trait_labels)
  trait_labels <- gsub("tricot2021_2022boiledmealdataat12map_qst_", "", trait_labels)
  trait_labels <- gsub("d22_", "", trait_labels)
  
  # now the trait names
  trait_labels <- gsub("generalappearanc", "General Appearance", trait_labels)
  trait_labels <- gsub("plantheightabove", "Plant Height", trait_labels)
  trait_labels <- gsub("suitabilitysoils", "Soil Suitability", trait_labels)
  trait_labels <- gsub("branchhabit", "Branching Habit", trait_labels)
  trait_labels <- gsub("stemappear", "Stem Appearance", trait_labels)
  trait_labels <- gsub("pestresis", "Pests Resistance", trait_labels)
  trait_labels <- gsub("diseaseres|diseaseresistanc", "Diseases Resistance", trait_labels)
  trait_labels <- gsub("vigor", "Vigor", trait_labels)
  trait_labels <- gsub("germination", "Germination", trait_labels)
  trait_labels <- gsub("stemquality", "Stem Quality", trait_labels)
  trait_labels <- gsub("rootshapes", "Root Shape", trait_labels)
  trait_labels <- gsub("rootofsize", "Root Size", trait_labels)
  trait_labels <- gsub("rootcotcolor", "Root Colour", trait_labels)
  trait_labels <- gsub("rootyieldcass", "Root Yield", trait_labels)
  trait_labels <- gsub("easedrying", "Easy Drying", trait_labels)
  trait_labels <- gsub("easemingling", "Easy Mingling", trait_labels)
  trait_labels <- gsub("easemingleflour", "Easy Mingling", trait_labels)#**
  
  trait_labels <- gsub("stickinesspaste", "Paste Stickiness", trait_labels)
  trait_labels <- gsub("tastecasspaste", "Paste Taste", trait_labels)
  trait_labels <- gsub("texturepaste", "Paste Texture", trait_labels)
  trait_labels <- gsub("colorcasspaste", "Paste Colour", trait_labels)
  trait_labels <- gsub("overalpastelike", "Paste Like", trait_labels)
  trait_labels <- gsub("easepeeling", "Easy Peeling", trait_labels)
  
  trait_labels <- gsub("choicreplantflou", "Grow Again", trait_labels)
  trait_labels <- gsub("cookingtime", "Cooking Time", trait_labels)
  
  #added for processing cycle 2 dataset
  trait_labels <- gsub("mealinessroots", "Root Mealiness", trait_labels) #added here to avoid being override by next line
  
  #for cycle 1
  trait_labels <- gsub("mealiness", "Root Mealiness", trait_labels)
  trait_labels <- gsub("cookedtaste", "Root Taste", trait_labels)
  trait_labels <- gsub("softness", "Root Softness", trait_labels)
  trait_labels <- gsub("fibrousness", "Root Fibrousness", trait_labels)
  trait_labels <- gsub("choicreplantboil", "Grow Again", trait_labels)
  trait_labels <- gsub("overalboiledroot", "Boiled Like", trait_labels)
  
  trait_labels <- gsub("overallperf", "Overall Performance", trait_labels)
  trait_labels <- gsub("overallbest", "Overall Performance", trait_labels)
  trait_labels <- gsub("overalperfoflour", "Overall Performance Flour", trait_labels)
  trait_labels <- gsub("overalperfharvst", "Overall Performance Harvest", trait_labels)
  trait_labels <- gsub("overallrootlike", "Root Like", trait_labels)#Perhaps should be change to "Overall Root"
  
  #added for processing cycle 2 dataset
  trait_labels <- gsub("dryingtimechips", "Drying Time Chips", trait_labels)#check if it is the same as Easy drying
  trait_labels <- gsub("sticknesspaste", "Paste Stickiness", trait_labels)#seems the same as stickinesspaste
  trait_labels <- gsub("tastecassavapaste", "Paste Taste", trait_labels)#seems the same as tastecasspaste
  trait_labels <- gsub("qualitystems", "Stem Quality", trait_labels)# stemquality
  trait_labels <- gsub("freshrootyield", "Root Yield", trait_labels)
 
  return(trait_labels)
 
}