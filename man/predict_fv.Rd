% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fcts_admin_models.R
\name{predict_fv}
\alias{predict_fv}
\title{Predict FV Positions for Thrower and Receiver}
\usage{
predict_fv(model_path, preprocessing_info_path, throws_data)
}
\arguments{
\item{model_path}{Path to the XGBoost model file.}

\item{preprocessing_info_path}{Path to the preprocessing information file containing scaling params and features.}

\item{throws_data}{Data frame with thrower and receiver features.}
}
\value{
Data frame with columns: `throwID`, `gameID`, `fv_thrower_position`, `fv_receiver_position`.
}
\description{
Predicts feature values for thrower and receiver positions using a pre-trained XGBoost model.
}
\examples{
model_path <- "./inst/app/www/fv_xgb.model"
preprocessing_info_path <- "./inst/app/www/preprocessing_info.rds"
throws_data <- some_throws_data
output_df <- predict_fv(model_path, preprocessing_info_path, throws_data)

}
