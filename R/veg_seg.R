#' Segment vegetation from background and calculate visual obstruction
#'
#' @param input_path File path for input images
#' @param image_output = Logical operator to enable visual output, Default = FALSE
#' @param output_path File path for binary output images when image_output = TRUE, Default = NULL
#' @param training Training data file for Random Forest Model
#' @param thresh Numeric value of threshold value for Random Forest prediction
#' @param ntree Number of trees to grow for Random Forest Model
#' @param mtry Number of variables to randomly sample as candidates at each split
#'
#' @return A dataframe with estimated vegetation visual obstruction and binary images if selected
#' @export
#'

veg_seg <- function(input_path,image_output = FALSE, output_path = NULL,training,thresh,ntree,mtry) {

  if(image_output != TRUE & image_output != FALSE) warning("value of image_output must be TRUE or FALSE")

  palette_selection_veg<- training
  palette_selection_veg <- dplyr::group_by(palette_selection_veg,mix)
  veg.rfm<- suppressWarnings(randomForest::randomForest(veg_class~red+green+blue,data=palette_selection_veg, ntree=ntree,mtry = mtry,importance=TRUE))

  paths_veg <- list.files(path=input_path,full.names=TRUE)
  names_veg <- list.files(path=input_path,full.names=FALSE)
  veg.stats<- data.frame()

  for (i in 1:length(paths_veg)) {
    img.01<- jpeg::readJPEG(paths_veg[i])
    coor<- as.data.frame(as.table(img.01[,,1]))[1:2]
    red<- 255*as.data.frame(as.table(img.01[,,1]))[3]
    green<- 255*as.data.frame(as.table(img.01[,,2]))[3]
    blue<- 255*as.data.frame(as.table(img.01[,,3]))[3]
    img.dat.01<- cbind(coor, red, green, blue)
    colnames(img.dat.01)<- c("y","x","red","green","blue")
    img.dat.01$classify<- predict(veg.rfm, img.dat.01)
    img.dat.01$thresh<- ifelse(img.dat.01$classify>thresh, 1,0)
    img.02<- matrix(img.dat.01$thresh, nrow=nrow(img.01), ncol=ncol(img.01))
    if(image_output == TRUE){jpeg::writeJPEG(img.02, paste(output_path, "/", names_veg[i], sep = ""), quality = 1)}

    write.stats<- data.frame(img.ID=          stringr::str_sub(names_veg[i]),
                             sum.img=         length(img.dat.01$thresh),
                             sum.veg=         sum(img.dat.01$thresh),
                             perc.veg=        sum(img.dat.01$thresh)/length(img.dat.01$thresh))

    veg.stats<-rbind(veg.stats, write.stats)
  }
  return(veg.stats)
}
