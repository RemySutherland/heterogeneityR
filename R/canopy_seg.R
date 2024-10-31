#' Calculate visual obstruction of specific vertical canopy sections
#'
#' @param input_path File path for binary input images
#' @param image_output Logical operator to enable visual output, Default = FALSE
#' @param low_top Numeric value of upper limit of lowest canopy section, Default = 0.22
#' @param low_bot Numeric value of lower limit of lowest canopy section, Default = 0.00
#' @param mid_top Numeric value of upper limit of middle canopy section, Default = 0.43
#' @param mid_bot Numeric value of lower limit of middle canopy section, Default = 0.22
#' @param upper_top Numeric value of upper limit of upper canopy section, Default = 1.00
#' @param upper_bot Numeric value of lower limit of upper canopy section, Default = 0.43
#' @param low_output File path for lower canopy binary output images when image_output = TRUE, Default = NULL
#' @param mid_output File path for middle canopy binary output images when image_output = TRUE, Default = NULL
#' @param upper_output File path for upper canopy binary output images when image_output = TRUE, Default = NULL
#'
#' @return A dataframe with estimated vegetation visual obstruction for specified canopy sections and binary images if selected
#' @export
#'
#' @examples
#' sample_imgs <- paste(find.package(package="heterogeneityR"),"/inst/vo_img",sep ="", collapse="")
#' sample_canopy_df <- canopy_seg(input_path = sample_imgs)
canopy_seg <- function(input_path,image_output = FALSE,
                       low_top=0.22,low_bot=0.00,mid_top=0.43,mid_bot=0.22,upper_top=1,upper_bot=0.43,
                       low_output=NULL,mid_output=NULL,upper_output=NULL) {


if(image_output != TRUE & image_output != FALSE) warning("value of image_output must be TRUE or FALSE")

paths_canopy_img<- list.files(path=input_path,full.names = TRUE)
names_canopy_img<- list.files(path=input_path,full.names = FALSE)
Topl<- 1-low_top
Botl<- low_bot
Topm<- 1-mid_top
Botm <-mid_bot
Toph<- 1-upper_top
Both <-upper_bot
Lef.Rig<- 0.00
canopy.stats<- data.frame()

for (i in 1:length(paths_canopy_img)){
  ## Low Canopy
  low=jpeg::readJPEG(paths_canopy_img[i])
  low1=low[1:dim(low)[1], 1:dim(low)[2]]
  lowdim<- round(dim(low1))
  top<- round(lowdim[1] * Topl)
  bottom<- lowdim[1] - (lowdim[1] * Botl)
  left<- round(lowdim[2] * Lef.Rig)
  right<- round(lowdim[2] - (lowdim[2] * Lef.Rig))
  low2<-low1[top:bottom, left:right]
  if(image_output == TRUE){jpeg::writeJPEG(low2, paste(low_output, "/", names_canopy_img[i], sep = ""), quality = 1)}

  ## Mid Canopy
  mid=jpeg::readJPEG(paths_canopy_img[i])
  mid1=mid[1:dim(mid)[1], 1:dim(mid)[2]]
  middim<- round(dim(mid1))
  top<- round(middim[1] * Topm)
  bottom<- middim[1] - (middim[1] * Botm)
  left<- round(middim[2] * Lef.Rig)
  right<- round(middim[2] - (middim[2] * Lef.Rig))
  mid2<-mid1[top:bottom, left:right]
  if(image_output == TRUE){jpeg::writeJPEG(mid2, paste(mid_output, "/", names_canopy_img[i], sep = ""), quality = 1)}

  ## Upper Canopy
  hi=jpeg::readJPEG(paths_canopy_img[i])
  hi1=hi[1:dim(hi)[1], 1:dim(hi)[2]]
  hidim<- round(dim(hi1))
  top<- round(hidim[1] * Toph)
  bottom<- hidim[1] - (hidim[1] * Both)
  left<- round(hidim[2] * Lef.Rig)
  right<- round(hidim[2] - (hidim[2] * Lef.Rig))
  hi2<-hi1[top:bottom, left:right]
  if(image_output == TRUE){jpeg::writeJPEG(hi2, paste(upper_output, "/", names_canopy_img[i], sep = ""), quality = 1)}

  write.stats<- data.frame(img.ID=              stringr::str_sub(names_canopy_img[i]),
                           sum.low.img=        length(low2),
                           sum.low.veg=     sum(low2 == 1),
                           perc.low.veg=    sum(low2 ==1)/length(low2)*100,
                           sum.mid.img=         length(mid2),
                           sum.mid.veg=     sum(mid2 == 1),
                           perc.mid.veg=    sum(mid2 ==1)/length(mid2)*100,
                           sum.upper.img=         length(hi2),
                           sum.upper.veg=     sum(hi2 == 1),
                           perc.upper.veg=    sum(hi2 ==1)/length(hi2)*100)

  canopy.stats<-rbind(canopy.stats, write.stats)

}
return(canopy.stats)
}
