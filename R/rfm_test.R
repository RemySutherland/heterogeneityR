#' Returns error and convergence estimates for Random Forest Model based on training data
#'
#' @param training Training data file for Random Forest Model
#' @param focal_column Column within training data file denoting focal vegetation
#' @param thresh Numeric value of threshold value for Random Forest prediction
#' @param ntree Number of trees to grow for Random Forest Model
#' @param mtry Number of variables to randomly sample as candidates at each split
#'
#' @return A print summary of estimated model error and plot of error as a function number of trees
#' @export

rfm_test <- function(training,focal_column,thresh,ntree,mtry){
palette_selection_veg<- training
focal_column <- deparse(substitute(focal_column))
palette_selection_veg <- dplyr::group_by(palette_selection_veg,mix)
palette_selection_veg<- dplyr::ungroup(palette_selection_veg)

formula<- reformulate(termlabels = c("red", "green", "blue"), response = focal_column)

class.rfm<- suppressWarnings(randomForest::randomForest(formula,data=palette_selection_veg, ntree=ntree,mtry = mtry,importance=TRUE))
print(class.rfm)
plot(class.rfm)
}
