

get_curve_meta <- function(df,n_window){
  df= df %>% 
    add_count(curve) %>% filter(n>(n_window+2))
  if(nrow(df)==0) return(df)
  df %>% group_by(curve) %>%
    summarise(min_x=min(x),max_x=max(x),
              # mean_slope=get_mean_slope(x,y,n_window),
              center_x=(max_x+min_x)/2,fit_lm=list(lm(y~x,model = F)),n_points=n()) %>%
    mutate(fit_slope = map_dbl(fit_lm,~.$coefficients[2]),
           fit_rsquare = map_dbl(fit_lm,~summary(.)$r.squared),
           fit_intercep = map_dbl(fit_lm,~.$coefficients[1])) %>%
    rename(curve_id=curve) %>% select(-fit_lm)
}
# 
# source('~/Documents/Deep Learning/tarisio NN/helpers_contours.R', echo=F)
# total_signed_curv <- function(x,y){
#   a=atan(diff(x)/diff(y)) * 180 / pi
#   # out = apply(cbind(pi-diff(a),diff(a)),1,min) * 180 /pi
#   # idx = which(out<)
# }
# 
# get_mean_slope<- function(x,y,n_window){
#   angle_raw = total_signed_curv(x,y)
#   matrix(c(angle_raw,
#            map(1:n_window,function(i) lag(angle_raw,n = i)) %>% unlist),
#          nrow=n_window+1,byrow = T) %>% colMeans() %>% na.omit() %>% mean()
# }
# #angle_raw = c(0.3,0.3,0.4,0.35,0.43,0.41,0.38)
# 
# smooth_curve <- function(x,y,n_window){
#   y_out = matrix(c(y,
#                    map(1:n_window,function(i) lag(y,n = i)) %>% unlist),
#                  nrow=n_window+1,byrow = T) %>% colMeans() %>% na.omit()
#   tibble(x=x[1:(length(x)-n_window)],y=y_out)
# }


break_curves_double_valued <- function(df){
  df %>% as_tibble %>% mutate(dx=sign(x-lag(x))) %>%
    filter(dx!=0) %>%
    mutate(sign_change = if_else(dx-lag(dx)!=0 & !is.na(dx-lag(dx)),1,0)) %>%
    mutate(curve_bis = cumsum(sign_change)+1) %>%
    dplyr::select(-dx,-sign_change)
}
break_curves <- function(df){
  lm_init = lm(data=df,y~x)
  seg = segmented(obj = lm_init,seg.Z=~x)
  if(is.null(seg$psi[,"Est."])) 
    return(df %>% mutate(curve_idx2=NA_integer_))
  x_break = which.min(abs(df$x-seg$psi[,"Est."]))
  browser()
  df %>% mutate(curve_idx2 = if_else(x<=seg$psi[,"Est."],1,2))
}
