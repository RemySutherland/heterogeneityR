#' Calculate horizontal variance of vegetation visual obstruction within profile board images
#'
#' @param input_path File path for binary input images
#' @param image_output Logical operator to enable visual output, Default = FALSE
#' @param left_output File path for left section binary output images when image_output = TRUE, Default = NULL
#' @param center_output File path for center section binary output images when image_output = TRUE, Default = NULL
#' @param right_output File path for right section binary output images when image_output = TRUE, Default = NULL
#'
#' @return A dataframe with estimated vegetation visual obstruction for specified horizontal sections, variance, and standard deviation and binary images if selected
#' @export
#'

veg_var <- function(input_path,image_output = FALSE,
                    left_output=NULL,center_output=NULL,right_output=NULL) {

  if(image_output != TRUE & image_output != FALSE) warning("value of image_output must be TRUE or FALSE")

paths_var_img<- list.files(path=input_path,full.names = TRUE)
names_var_img<- list.files(path=input_path,full.names = FALSE)
Lefl<- 0.00
Rigl<-0.67
Lefm<- 0.33
Rigm <-0.33
Lefr<- 0.67
Rigr <-0.00
Top.Bot<- 0.00

veg.variance<- data.frame()

for (i in 1:length(paths_var_img)){
  ## Left Section
  lef=jpeg::readJPEG(paths_var_img[i])
  lef1=lef[1:dim(lef)[1], 1:dim(lef)[2]]
  lefdim<- round(dim(lef1))
  top<- round(lefdim[1] * Top.Bot)
  bottom<- lefdim[1] - (lefdim[1] * Top.Bot)
  left<- round(lefdim[2] * Lefl)
  right<- round(lefdim[2] - (lefdim[2] * Rigl))
  lef2<-lef1[top:bottom, left:right]
  if(image_output == TRUE){jpeg::writeJPEG(lef2, paste(left_output, "/", names_var_img[i], sep = ""), quality = 1)}

  ## Center Section
  mid=jpeg::readJPEG(paths_var_img[i])
  mid1=mid[1:dim(mid)[1], 1:dim(mid)[2]]
  middim<- round(dim(mid1))
  top<- round(middim[1] * Top.Bot)
  bottom<- middim[1] - (middim[1] * Top.Bot)
  left<- round(middim[2] * Lefm)
  right<- round(middim[2] - (middim[2] * Rigm))
  mid2<-mid1[top:bottom, left:right]
  if(image_output == TRUE){jpeg::writeJPEG(mid2, paste(center_output, "/", names_var_img[i], sep = ""), quality = 1)}

  ## Right Section
  rig=jpeg::readJPEG(paths_var_img[i])
  rig1=rig[1:dim(rig)[1], 1:dim(rig)[2]]
  rigdim<- round(dim(rig1))
  top<- round(rigdim[1] * Top.Bot)
  bottom<- rigdim[1] - (rigdim[1] * Top.Bot)
  left<- round(rigdim[2] * Lefr)
  right<- round(rigdim[2] - (rigdim[2] * Rigr))
  rig2<-rig1[top:bottom, left:right]
  if(image_output == TRUE){jpeg::writeJPEG(rig2, paste(right_output, "/", names_var_img[i], sep = ""), quality = 1)}

  write.stats<- data.frame(img.ID=              stringr::str_sub(names_var_img[i]),
                           sum.left.img=        length(lef2),
                           sum.left.veg=     sum(lef2 == 1),
                           perc.left.veg=    sum(lef2 ==1)/length(lef2),
                           sum.center.img=         length(mid2),
                           sum.center.veg=     sum(mid2 == 1),
                           perc.center.veg=    sum(mid2 ==1)/length(mid2),
                           sum.right.img=         length(rig2),
                           sum.right.veg=     sum(rig2 == 1),
                           perc.right.veg=    sum(rig2 ==1)/length(rig2),
                           horizontal.sd.veg=              sd(x = c(sum(lef2 ==1)/length(lef2),sum(mid2 ==1)/length(mid2),sum(rig2 ==1)/length(rig2))),
                           horizontal.var.veg=             (sd(x = c(sum(lef2 ==1)/length(lef2),sum(mid2 ==1)/length(mid2),sum(rig2 ==1)/length(rig2))))^2
  )

  veg.variance<-rbind(veg.variance, write.stats)

}
return(veg.variance)
}
