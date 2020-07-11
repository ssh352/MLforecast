get_var_imp <- function(object, horizone = 1, plot = T){

  
if(plot){
plot(caret::varImp(object$model$model[[paste("horizon_", horizone, sep = "")]]$window_1$model))
}else{
  
  caret::varImp(object$model$model[[paste("horizon_", horizone, sep = "")]]$window_1$model)
}

}
