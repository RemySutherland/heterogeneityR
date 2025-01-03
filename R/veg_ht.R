#' Calculate vegetation height from segmented images
#'
#' @param input_path File path for binary input images
#' @param board_height Numeric value of actual height of profile board
#' @param veg_type Character string to denote vegetation type
#'
#' @return A dataframe with mean, maximum, and standard deviation of vegetation height for input images
#' @export
#'
#' @examples
#' sample_imgs <- paste(find.package(package="heterogeneityR"),"/inst/vo_img",sep ="", collapse="")
#' sample_heights_df <- veg_ht(input_path = ,
#'                             board_height = 116)
veg_ht <- function(input_path,board_height,veg_type) {

  paths_veg_ht <- list.files(path=input_path,full.names=TRUE)
  names_veg_ht <- list.files(path=input_path,full.names=FALSE)
  veg_ht.stats<- data.frame()

  for (i in 1:length(paths_veg_ht)){
    ht.image=jpeg::readJPEG(paths_veg_ht[i])
    img_width <- dim(ht.image)[2]
    img_height <- dim(ht.image)[1]
    veg.heights <- integer(length = img_height)
    for (col in 1:img_width) {
      max.veg <- which(ht.image[,col] > 0)
      if (length(max.veg) > 0) {
        veg.heights[col] <- img_height - min(max.veg)
      } else {
        veg.heights[col] <- 0
      }
    }

    real.heights <- veg.heights/img_width*board_height
    max_height_tot <- max(real.heights)
    min_height_tot <- min(real.heights)
    average_height <- mean(real.heights)
    sd_height <- sd(real.heights)

    write.stats<- data.frame(img.ID=        stringr::str_sub(names_veg_ht[i]),
                             avg =          average_height,
                             max =          max_height_tot,
                             min =          min_height_tot,
                             sd =           sd_height
    )

  veg_ht.stats<- rbind(veg_ht.stats, write.stats)
  }
  names(veg_ht.stats)[names(veg_ht.stats) == "avg"] <- paste("avg.height.", veg_type, sep = "")
  names(veg_ht.stats)[names(veg_ht.stats) == "max"] <- paste("max.height.", veg_type, sep = "")
  names(veg_ht.stats)[names(veg_ht.stats) == "min"] <- paste("min.height.", veg_type, sep = "")
  names(veg_ht.stats)[names(veg_ht.stats) == "sd"] <- paste("sd.height.", veg_type, sep = "")
  return(veg_ht.stats)
}
