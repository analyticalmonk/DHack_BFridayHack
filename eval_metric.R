eval_metric <- function(original_y, pred_y) {
    rmse <- sqrt((sum((original_y - pred_y)^2)) / length(original_y))
    rmse
}