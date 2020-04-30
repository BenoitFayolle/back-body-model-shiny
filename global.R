library(dplyr);library(image.ContourDetector)
library(OpenImageR)
library(reticulate)
library(purrr);library(tidyr)
library(ggplot2)
library(shiny)

if (Sys.info()$user == 'ben')
   use_condaenv(condaenv="r-reticulate")
else if (Sys.info()$user == 'ubuntu')
   use_virtualenv('/home/ubuntu/miniconda3/envs/r-reticulate')

source_python("gabor_convolve.py")
source_python("gabor_filter_bank_fun.py")

source("contour_detect_helpers.R")
source("helpers.R")

## input params
target_width = 299
target_aspect_ratio = 1
target_height = target_width * target_aspect_ratio
crop_heights = c("high","mid","low")
height_percent_crop = c(25,50,75)
horizontal_correction_coef = 0.96
min_width = 300.0
width_percent = 20.0

sigma_x =4
sigma_y = 4
pixel_quant_step =  0.05
# color_grad_n = 2
color_change_treshold = 5
n_window = 10
min_rsquare = 0.87
min_number_of_slopes = 2 # will crash below 2

model_path = "xgboost_two_piece_model_v2.rData"

filter_list = get_gabor_filters()
