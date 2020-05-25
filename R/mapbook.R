# Author: Roozbeh Valavi
# contact: valavi.r@gmail.com
# Date : December 2019
# Version 0.1
# Licence GPL v3

# use "col_blind" for colour blind theme
# other available styles are: "white", "gray", "natural", 
# "cobalt", "col_blind", "albatross", "beaver", "bw", "watercolor" 

spMapBook <- function(region, output_pdf, map_theme = "classic", verbose = TRUE){
  regions <- unique(toupper(unlist(list(region))))
  if(regions[1] == 'ALL'){
    regions <- c('AWT', 'CAN', 'NSW', 'NZ', 'SA', 'SWI')
  }
  grDevices::pdf(output_pdf, width = 8.5, height = 10) # pdf setting
  tmap::tmap_style(map_theme)
  for(r in regions){
    if(! r %in% c('AWT', 'CAN', 'NSW', 'NZ', 'SA', 'SWI')){
      warning('unknown region: ', r)
    }
    # reading the spatial polygon
    poly <- disBorder(r)
    base_plot <- tmap::tm_shape(poly) + 
      tmap::tm_polygons() + 
      tmap::tm_grid(lwd = 0.2, labels.inside.frame = FALSE, alpha = 0.4, projection = "longlat",
              labels.format = list(big.mark = ",", fun = function(x){paste(x, "º")}))
    legend_pos <- switch(r == "SWI", c("left", "top"), NULL)
    naposition <- ifelse(r %in% c("AWT", "CAN", "SA"), "right", "left")
    # reading presence-only file
    path <- system.file("extdata", package="disdat")
    x <- paste0(path, '/', r, 'train_po.rds')
    presences <- readRDS(x)
    # extract all species
    presences$spid <- as.character(presences$spid)
    species <- unique(presences$spid)
    if(verbose){cat(paste(toupper(r), "has", length(species), "species\n"))}
    # reading evaluation files
    eval_files <- list.files(path)[grep(paste0(r, 'test_pa'), list.files(path))]
    i <- 0
    for(s in species){
      # find the evaluation file - for some regions this means identifying the taxonomic group
      if(length(eval_files) > 1){
        sp_grp <- presences[presences$spid == s, "group"][1]
        for(f in eval_files){
          file_grp <- unlist(strsplit(f, "_"))[3]
          if(file_grp == paste0(sp_grp, '.rds')){
            evaluation <- readRDS(file.path(path, f))
          }
        }
      } else{
        f <- eval_files
        evaluation <- readRDS(file.path(path, f))
      }
      sp_presence <- sf::st_as_sf(presences[presences$spid == s, ], coords = c("x", "y"), crs = sf::st_crs(poly))
      sp_eval <- sf::st_as_sf(evaluation, coords = c("x", "y"), crs = sf::st_crs(poly))
      pacount <- table(evaluation[,s])
      sp_eval[,s] <- as.factor(evaluation[,s])
      i <- i + 1
      if(i %% 2 != 0){
        poplot1 <- base_plot + 
          tmap::tm_shape(sp_presence) + 
          tmap::tm_dots(size = 0.1, col = "blue", alpha = 0.6, legend.show = FALSE) + 
          tmap::tm_compass(type = "8star", position = c(naposition, "top")) + 
          tmap::tm_layout(main.title = paste("Species:", s, "\nPresences:", nrow(sp_presence)), 
                    main.title.size = 0.8, , main.title.position = "center")
        paplot1 <- base_plot +
          tmap::tm_shape(sp_eval) +
          tmap::tm_dots(s, size = 0.07, palette = c("red", "blue"), title = "Occurrence", alpha = 0.4) +
          tmap::tm_legend(legend.position = legend_pos) +
          tmap::tm_layout(main.title = paste("Species:", s, "\nPresences:", pacount[2], " Absences:", pacount[1]), 
                    main.title.size = 0.8, , main.title.position = "center")
      } else{
        poplot2 <- base_plot + 
          tmap::tm_shape(sp_presence) + 
          tmap::tm_dots(size = 0.1, col = "blue", alpha = 0.6, legend.show = FALSE) + 
          tmap::tm_compass(type = "8star", position = c(naposition, "top")) + 
          tmap::tm_layout(main.title = paste("Species:", s, "\nPresences:", nrow(sp_presence)), 
                    main.title.size = 0.8, , main.title.position = "center")
        paplot2 <- base_plot +
          tmap::tm_shape(sp_eval) +
          tmap::tm_dots(s, size = 0.07, palette = c("red", "blue"), title = "Occurrence", alpha = 0.4) +
          tmap::tm_legend(legend.position = legend_pos) +
          tmap::tm_layout(main.title = paste("Species:", s, "\nPresences:", pacount[2], " Absences:", pacount[1]), 
                    main.title.size = 0.8, , main.title.position = "center")
      }
      if(i < length(species) && (i %% 2 == 0) || i == length(species)){
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(layout=grid::grid.layout(2,2)))
        print(poplot1, vp=grid::viewport(layout.pos.col = 1, layout.pos.row = 1))
        print(paplot1, vp=grid::viewport(layout.pos.col = 2, layout.pos.row = 1))
        print(poplot2, vp=grid::viewport(layout.pos.col = 1, layout.pos.row = 2))
        print(paplot2, vp=grid::viewport(layout.pos.col = 2, layout.pos.row = 2))
        poplot1 <- paplot1 <- poplot2 <- paplot2 <- NULL
      }
      if(verbose) cat(i, " ")
    }
    if(verbose) cat("\n")
  }
  grDevices::dev.off()
}
