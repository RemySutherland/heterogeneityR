#' Segment and calculate percent visual obstruction of live and dead fuel
#'
#' @param input_path File path for input images
#' @param lf_output Logical operator to enable live fuel visual output, Default = FALSE
#' @param lf_path File path for binary output images for live fuel when lf_output = TRUE, Default = NULL
#' @param df_output Logical operator to enable dead fuel visual output, Default = FALSE
#' @param df_path File path for binary output images for dead fuel when df_output = TRUE, Default = NULL
#' @param veg_vo Option to provide overall vegetation obstruction values (list) otherwise function will calculate, Default = NULL
#' @param training Training data file for Random Forest Model
#' @param veg_thresh Numeric value of threshold value for overall vegetation Random Forest prediction
#' @param fuel_thresh Numeric value of threshold value for fuel type Random Forest prediction
#' @param ntree Number of trees to grow for Random Forest Model
#' @param mtry Number of variables to randomly sample as candidates at each split
#'
#' @return a data frame with estimated live and dead fuel visual obstruction (percentage of overall vegetation) and binary images if selected
#' @export
#'

fuel_seg <- function(input_path,lf_output = FALSE, lf_path = NULL, df_output = FALSE,
                     df_path = NULL, veg_vo = NULL,training,veg_thresh,fuel_thresh,ntree,mtry) {

  if(lf_output != TRUE & lf_output != FALSE) warning("value of df_output must be TRUE or FALSE")
  if(df_output != TRUE & df_output != FALSE) warning("value of df_output must be TRUE or FALSE")

  palette_selection_fuel <- training
  palette_selection_fuel <- dplyr::group_by(palette_selection_fuel,mix)
  rfm.lf<- suppressWarnings(randomForest::randomForest(lf_class~(red+green+blue),data=palette_selection_fuel, ntree=ntree,mtry = mtry,importance=TRUE))
  rfm.df<- suppressWarnings(randomForest::randomForest(df_class~(red+green+blue),data=palette_selection_fuel, ntree=ntree,mtry = mtry,importance=TRUE))


  if(is.null(veg_vo) == TRUE)
  {
  palette_selection_veg<- training
  palette_selection_veg <- dplyr::group_by(palette_selection_veg,mix)
  veg.rfm<- suppressWarnings(randomForest::randomForest(veg_class~red+green+blue,data=palette_selection_veg, ntree=ntree,mtry = mtry,importance=TRUE))}

  paths_fuel <- list.files(path=input_path,full.names=TRUE)
  names_fuel <- list.files(path=input_path,full.names=FALSE)
  fuel.stats<- data.frame()

  for (i in 1:length(paths_fuel)) {

  if(is.null(veg_vo) == TRUE)
  img.01<- jpeg::readJPEG(paths_fuel[i])
  coor<- as.data.frame(as.table(img.01[,,1]))[1:2]
  red<- 255*as.data.frame(as.table(img.01[,,1]))[3]
  green<- 255*as.data.frame(as.table(img.01[,,2]))[3]
  blue<- 255*as.data.frame(as.table(img.01[,,3]))[3]
  img.dat.01<- cbind(coor, red, green, blue)
  colnames(img.dat.01)<- c("y","x","red","green","blue")
  img.dat.01$classify<- predict(veg.rfm, img.dat.01)
  img.dat.01$thresh<- ifelse(img.dat.01$classify>veg_thresh, 1,0)
  img.02<- matrix(img.dat.01$thresh, nrow=nrow(img.01), ncol=ncol(img.01))

  img.04<- jpeg::readJPEG(paths_fuel[i])
  coor<- as.data.frame(as.table(img.04[,,1]))[1:2]
  red<- 255*as.data.frame(as.table(img.04[,,1]))[3]
  green<- 255*as.data.frame(as.table(img.04[,,2]))[3]
  blue<- 255*as.data.frame(as.table(img.04[,,3]))[3]
  img.dat.02<- cbind(coor, red, green, blue)
  colnames(img.dat.02)<- c("y","x","red","green","blue")
  img.dat.02$lf_classify<- predict(rfm.lf, img.dat.02)
  img.dat.02$veg_thresh <- img.dat.01$thresh
  img.dat.02$lf_thresh<- ifelse(img.dat.01$classify>veg_thresh,ifelse(img.dat.02$lf_classify>fuel_thresh, 1,0),0)
  img.dat.02$df_classify<- predict(rfm.df, img.dat.02)
  img.dat.02$df_thresh<- ifelse(img.dat.01$classify>veg_thresh,ifelse(img.dat.02$df_classify>fuel_thresh, 1,0),0)
  img.05<- matrix(img.dat.02$lf_thresh, nrow=nrow(img.04), ncol=ncol(img.04))
  img.06<- matrix(img.dat.02$df_thresh, nrow=nrow(img.04), ncol=ncol(img.04))
  if(lf_output == TRUE){jpeg::writeJPEG(img.05, paste(lf_path, "/", names_fuel[i] ,sep=""), quality= 1)}
  if(df_output == TRUE){jpeg::writeJPEG(img.06, paste(df_path, "/", names_fuel[i] ,sep=""), quality= 1)}



  if(is.null(veg_vo) == TRUE){perc.veg = (sum(img.dat.01$thresh)/length(img.dat.01$thresh))} else {perc.veg = veg_vo[i]}

  write.stats<- data.frame(img.ID=         stringr::str_sub(names_fuel[i]),
                           sum.img=        length(img.dat.02$lf_thresh),
                           sum.lf=         sum(img.dat.02$lf_thresh),
                           sum.df=         sum(img.dat.02$df_thresh),
                           perc.veg=       perc.veg,
                           perc.lf=        ((sum(img.dat.02$lf_thresh)/length(img.dat.02$lf_thresh))),
                           perc.df=        ((sum(img.dat.02$df_thresh)/length(img.dat.02$df_thresh))),
                           perc.lf.veg=    ((sum(img.dat.02$lf_thresh)/length(img.dat.02$lf_thresh)))/perc.veg,
                           perc.df.veg=    ((sum(img.dat.02$df_thresh)/length(img.dat.02$df_thresh)))/perc.veg)

  fuel.stats<-rbind(fuel.stats, write.stats)
  }
  return(fuel.stats)
}
