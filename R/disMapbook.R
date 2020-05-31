# Author: Roozbeh Valavi
# contact: valavi.r@gmail.com
# Date : June 2020
# Version 0.2
# Licence GPL v3

disMapBook <- function(region, output_pdf, verbose = TRUE){

  # # check for availability of sf
  # pkg <- c("sf")
  # pkgna <- names(which(sapply(sapply(pkg, find.package, quiet = TRUE), length) == 0))
  # if(length(pkgna) > 0){
  #   message("This function requires ", pkg, " package for plotting.", "\nWould you like to install it now?\n1: yes\n2: no")
  #   user <- readline(prompt = paste0("Selection: "))
  #   if(tolower(user) %in% c("1", "yes", "y")){
  #     utils::install.packages(pkgna)
  #   } else{
  #     stop("Please install sf package.")
  #   }
  # }

  regions <- unique(toupper(unlist(region)))
  if(regions[1] == 'ALL'){
    regions <- c('AWT', 'CAN', 'NSW', 'NZ', 'SA', 'SWI')
  }
  regions <- lapply(regions, .checkRegion)
  grDevices::pdf(output_pdf, width = 8.5, height = 10) # pdf setting
  for(r in regions){
    poly <- disBorder(r)
    presences <- disPo(r)
    # extract all species
    species <- unique(presences$spid)
    if(verbose){cat(paste(toupper(r), "has", length(species), "species\n"))}
    # reading evaluation files
    i <- 0
    graphics::par(mfrow=c(2,2))
    for(s in species){
      if (r %in% c("AWT", "NSW")) {
        grp <- presences[presences$spid == s, "group"][1]
        evaluation <- disPa(r, grp)
      } else {
        evaluation <- disPa(r)
      }
      i <- i + 1
      npres <- nrow(presences[presences$spid == s,])
      plot(sf::st_geometry(poly))
      graphics::points(presences[presences$spid == s, c("x", "y")],
                       pch = 16,
                       col = grDevices::rgb(0, 0, 255, maxColorValue = 255, alpha = 125))
      graphics::mtext(sprintf("Species: %s\nPresences: %s", s, npres))

      pres <- which(evaluation[,s] == 1)
      abse <- which(evaluation[,s] == 0)
      plot(sf::st_geometry(poly))
      graphics::points(evaluation[abse, c("x", "y")],
                       pch = 16,
                       col = grDevices::rgb(255, 0, 0, maxColorValue = 255, alpha = 50))
      graphics::points(evaluation[pres, c("x", "y")],
                       pch = 16,
                       col = grDevices::rgb(0, 0, 255, maxColorValue = 255, alpha = 50))
      graphics::mtext(sprintf("Species: %s\nPresences: %s Absences: %s", s, length(pres), length(abse)))

      if(verbose) cat(i, " ")
    }
    if(verbose) cat("\n")
  }
  grDevices::dev.off()
}
