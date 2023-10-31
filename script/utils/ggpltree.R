library(partykit)
library(ggparty)

library(gosset)
# x <- plt_1
# qve = T
# log = F
# ordered_items <- ordered_varieties


# 
# 
# 
# levels(coeffs$items) == "NAROCASS 1"
# 
# unique_items <- coeffs[coeffs$id == 2, "items"]


ggpltree <- function(x, 
                     log = TRUE,
                     ref = NULL, 
                     ci.level = 0.95, 
                     multcomp = FALSE,
                     qve = FALSE,
                     ordered_items = NULL,
                     color_palette = NULL,
                     group_1 = NULL){
  
  # Extract ids from terminal nodes
  node_id <- partykit::nodeids(x, terminal = TRUE)
  
  # get number of observations in each inner node
  nobs <- vector(mode = "numeric", length = length(node_id))
  
  for (i in seq_along(node_id)) {
    nobs[i] = as.integer(x[[ node_id[i] ]]$node$info$nobs)
  }
  
  # get PL models from each node
  nodes <- vector(mode = "list", length = length(node_id))
  
  for (i in seq_along(node_id)) {
    nodes[[i]] = x[[ node_id[i] ]]$node$info$object
  }
  
  # get item names
  items <- names(coef(nodes[[1]]))
  
  if(isTRUE(qve)){
    # get item parameters from model
    coeffs <-lapply(nodes, function(y) {
      z = psychotools::itempar(y, log = log, ref = ref)
      # get estimates from item parameters using qvcalc
      z = qvcalc::qvcalc(z, ref = ref)$qvframe
      })
    
    # if(isTRUE("try-error" %in% class(coeffs))) {
    #
    # is likely that the error id due to missing items in one the nodes
    # so we apply the function pseudo_ranking() to add these missing items
    # extract the original rankings, add pseudo_ranking and refit the model
    #   coeffs = lapply(x, function(y){
    #     r = y$rankings
    #     r = pseudo_rank(r)
    #     stats::update(y, rankings = r)
    #   })
    # 
    #   coeffs = try(lapply(coeffs, function(y) {
    #   z = psychotools::itempar(y, log = log, ref = ref)
    #   get estimates from item parameters using qvcalc
    #   z = qvcalc::qvcalc(z)$qvframe
    #   }), silent = TRUE)
    # }
    
    # if the error persists then return an error
    # if (isTRUE("try-error" %in% class(coeffs))) {
    #   stop("Unable to compute worth estimates. Check for errors/warnings in ",
    #        "your modelparty object. \n You can try log = FALSE \n")
    # }
    
    # Add limits in error bars and item names
    coeffs <- lapply(coeffs, function(X){
      X = within(X, {
        bmax = X$estimate + stats::qnorm(1-(1-ci.level)/2) * X$quasiSE
        bmin = X$estimate - stats::qnorm(1-(1-ci.level)/2) * X$quasiSE
        items <- items
        })
      
      return(X)
      })
    
    # Add node information and number of observations
    # and if required add multicomp letters
    for (i in seq_along(node_id)){
      coeffs[[i]] <- within(coeffs[[i]], {
        #number of observations
        nobs <- nobs[i]
        #node id
        id <- node_id[i]})
      
      if(isTRUE(multcomp)){
        mc = multcompPL(x[[i]])
        coeffs[[i]] = merge(coeffs[[i]], mc[,c("items", "group")], by = "items")
        }else{
          coeffs[[i]]$group = ""
        }
      }
    
    coeffs <- do.call("rbind", coeffs)
    
    if (isFALSE(log)){
      coeffs$bmin = ifelse(coeffs$bmin < 0, 0, coeffs$bmin)
      coeffs$bmax = ifelse(coeffs$bmax > 1, 1, coeffs$bmax)
    }
  } # End of qve TRUE
  
  if(isFALSE(qve)){
    message("qve FAlSE")
    add.letters <- FALSE
    coeffs <- itempar(x, vcov = FALSE, log = log, ref = ref)
    X <- list()
    #remove ties if present
    if(0 < sum(grepl(pattern = "tie", x = items))){
      message("Removing ties for plotting")
      items <- items[grep(pattern = "tie", x = items, invert = TRUE)]
    }
    
    for (i in seq_len(dim(coeffs)[[1]])){
      xi <- data.frame(estimate = coeffs[i, ],
                       quasiSE = 0,
                       items = items)
      X[[i]] <- xi
    }
    
    coeffs <- X
    
    for (i in seq_along(node_id)){
      coeffs[[i]] <- within(coeffs[[i]], {
        #number of observations
        nobs <- nobs[i]
        #node id
        id <- node_id[i]})
      
      coeffs <- do.call("rbind", coeffs)
    }
  }
  
  #ggparty needs that the node id column be named "id"
  #coeffs$id <- coeffs$node
  
  #perhaps not necesary 
  # coeffs$id = paste0(coeffs$node, "_", coeffs$items)
  # 
  #groups = ""

  # node_lev = unique(paste0("Node ", coeffs$node, " (n=", coeffs$nobs, ")"))

  # coeffs$id = coeffs$node

  # coeffs$node = factor(paste0("Node ", coeffs$node, " (n=", coeffs$nobs, ")"),
  #                      levels = node_lev)

  
  if(is.null(ordered_items)){
    coeffs$items = factor(coeffs$items, levels = rev(sort(items)))  
  }else{
    coeffs$items = factor(coeffs$items, levels = rev(ordered_items), ordered = T)  
  }
  
  # Added here for this instance, but color palette shoud be passed as parameter
  color_palette <- ifelse(levels(coeffs$items) == "NAROCASS 1", "#ca0020",
                          ifelse(levels(coeffs$items) %in% group_1, "#1a9850", "#104e8b"))
  
  # if(!is.null(color_palette)){
  #   color_palette <- color_palette[order(ordered_items)]
  # }
  
  #//////////////
  
  # Get max and min values for the x axis in the plot
  xmax = round(max(coeffs$bmax, na.rm = TRUE) + 0.01, digits = 4)
  
  if (isFALSE(log)) {
    xmin = 0
    xinter = 1/length(items)
    xbreaks = round(c(mean(c(0, xmax)), xmax), 2)
    xbreaks = c(0, xbreaks)
  }
  
  if (isTRUE(log)) {
    xinter = 0
    xmin = min(coeffs$bmin, na.rm = TRUE)
    xbreaks = round(c(mean(c(xmin, xmax)), xmax), 2)
    xbreaks = c(xmin, xbreaks)
  }
  
  #xlabs = as.character(round(xbreaks, 2))
  xmin = xmin + (xmin * 0.15)
  xmax = xmax + (xmax * 0.15)
  
  
  
  
 # END new 
  
  # ggparty plot with node and tree info
  ggparty(x) +
    geom_edge() +
    geom_edge_label() +
    #geom_node_splitvar() +
    ggparty::geom_node_label(line_list =
                               list(ggplot2::aes(label = splitvar),
                                    ggplot2::aes(label = ifelse(p.value < 0.01, "p < 0.01 ", paste("p =", round(p.value, 4)))),
                                    ggplot2::aes(label = ""),
                                    ggplot2::aes(label = id)),
                             line_gpar = list(list(size = 12),
                                              list(size = 10),
                                              list(size = 10),
                                              list(size = 10,
                                                   col = "black",
                                                   fontface = "bold",
                                                   alignment = "center")),
                             ids = "inner") +
    geom_node_label(aes(label = paste0("Node ", id, ", n = ", nodesize)),
                    ids = "terminal",
                    nudge_y = 0.01,
                    label.col = NA) +
    geom_node_plot(gglist = list(geom_vline(xintercept = xinter, 
                                            colour = "darkgray", size = 0.75),
                                 geom_point(aes(x = estimate, 
                                                y = items),
                                            data = coeffs),
                                 geom_errorbarh(aes(xmin = bmin,
                                                    xmax = bmax,
                                                    y = items),
                                                height = .1,
                                                data = coeffs,
                                                colour = "black"),
                                 theme_bw(),
                                 theme(axis.title.x = element_blank(),
                                       axis.title.y = element_blank(),
                                       axis.text.y = element_text(color = color_palette,
                                                                  face = "bold"))
                                 ), shared_axis_labels = T)
  }




