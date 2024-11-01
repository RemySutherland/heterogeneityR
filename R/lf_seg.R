#' Segment and calculate percent visual obstruction of live fuel in vegetation
#'
#' @param input_path File path for input images
#' @param image_output Logical operator to enable visual output, Default = FALSE
#' @param output_path File path for binary output images when image_output = TRUE, Default = NULL
#' @param veg_vo List of values for associated vegetation visual obstruction values to calculate proportion live fuel
#' @param training Training data file for Random Forest Model
#' @param thresh Numeric value of threshold value for Random Forest prediction
#' @param ntree Number of trees to grow for Random Forest Model
#' @param mtry Number of variables to randomly sample as candidates at each split
#'
#' @return a data frame with estimated live fuel visual obstruction (percentage of overall vegetation) and binary images if selected
#' @export
#'
#' @examples
#' sample_imgs <- paste(find.package(package="heterogeneityR"),"/inst/img",sep ="", collapse="")
#' training_file <- system.file("stillwater.training.rda")
#' sample_lf_vo_df <- lf_seg(input_path = sample_imgs,
#'                             image_output = FALSE,
#'                             training = training_file,
#'                             thresh = 0.80,
#'                             ntree = 100,
#'                             mtry = 1)
lf_seg <- function(input_path,image_output = FALSE, output_path = NULL,veg_vo,training,thresh,ntree,mtry) {

  if(image_output != TRUE & image_output != FALSE) warning("value of image_output must be TRUE or FALSE")

  palette_selection_lf <- training
  palette_selection_lf <- dplyr::group_by(palette_selection_lf,mix)
  rfm.lf<- suppressWarnings(randomForest::randomForest(lf_class~(red+green+blue),data=palette_selection_lf, ntree=ntree,mtry = mtry,importance=TRUE))

  paths_lf <- list.files(path=input_path,full.names=TRUE)
  names_lf <- list.files(path=input_path,full.names=FALSE)
  lf.stats<- data.frame()

  for (i in 1:length(paths_lf)) {

  img.04<- jpeg::readJPEG(paths_lf[i])
  coor<- as.data.frame(as.table(img.04[,,1]))[1:2]
  red<- 255*as.data.frame(as.table(img.04[,,1]))[3]
  green<- 255*as.data.frame(as.table(img.04[,,2]))[3]
  blue<- 255*as.data.frame(as.table(img.04[,,3]))[3]
  img.dat.02<- cbind(coor, red, green, blue)
  colnames(img.dat.02)<- c("y","x","red","green","blue")
  img.dat.02$classify<- predict(rfm.lf, img.dat.02)
  img.dat.02$thresh<- ifelse(img.dat.02$classify>thresh, 1,0)
  img.05<- matrix(img.dat.02$thresh, nrow=nrow(img.04), ncol=ncol(img.04))
  if(image_output == TRUE){jpeg::writeJPEG(img.05, paste(output_path, "/", names_lf[i] ,sep=""), quality= 1)}

  write.stats<- data.frame(img.ID=         stringr::str_sub(names_lf[i]),
                           sum.img=        length(img.dat.02$thresh),
                           sum.lf=         sum(img.dat.02$thresh),
                           sum.veg=        vegVO[i],
                           perc.lf=             ((sum(img.dat.02$thresh)/vegVO[i])*100))

  lf.stats<-rbind(lf.stats, write.stats)
  }
  return(lf.stats)
}
