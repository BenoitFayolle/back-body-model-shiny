get_gabor_filters <- function(){
  # freq_vec = seq(0.1,0.25,by=0.05)
  freq_vec = 0.1
  # bandwidth_vec = seq(0.9,0.3,by=-0.1)
  # bandwidth_vec = c(0.8,0.8,0.7,0.6,0.5,0.4,0.3)
  # bandwidth_vec = rev(freq_vec)
  theta_vec = c(80,90,100) * pi / 180
  
  idx=1
  filter_list = vector(mode = "list",length = length(freq_vec)*length(theta_vec))
  for (i in 1:length(freq_vec)){
    for (j in 1:length(theta_vec)){
      # for (k in 1:length(bandwidth_vec)){
      filter_list[[idx]] = gabor_filter_bank_fun(frequency=freq_vec[i], theta=theta_vec[j], 
                                                 # bandwidth=bandwidth_vec[i],
                                                 sigma_x = sigma_x,sigma_y=sigma_y
      )
      idx=idx+1
      # }
    }
  }
  filter_list
}




get_color_gradients <- function(img_array){
  
  ## color gradient metrics
  img_color = img_array
  L_out = dim(img_color)[2]
  x_grad_out = vector(mode="double",length = L_out-1)
  img_color=img_color/max(img_color)
  for (i in 2:L_out){
    x_grad_out[i] = sum((img_color[,i,]-img_color[,i-1,])>(color_change_treshold/target_height))
  }
  x_grad_out = (x_grad_out - mean(x_grad_out))/sd(x_grad_out)
  x_curve = c(rep(0,4),diff(x_grad_out)[-c(1:3,((L_out-4):(L_out-1)))],rep(0,4))
  x_curve_max = which.max(x_curve)
  standard_curve_max = abs(max(x_curve))
  c(x_curve_max,standard_curve_max)
}

gabor_convolve_wrap <-function(img_array,filter_list){
  im_convolved_list = list()
  dim(img_array) <- c(1,dim(img_array))
  for (j in 1:length(filter_list)){
    filt_temp = filter_list[[j]]
    dim(filt_temp) <- c(dim(filt_temp),1,1)
    im_convolved <- gabor_convolve(img_array,filt_temp,strides_var=list(2,2))
    im_convolved = im_convolved / max(im_convolved)
    im_convolved = im_convolved[1,,,1]
    im_convolved = matrix(im_convolved,nrow=dim(im_convolved)[1])
    im_convolved_list[[j]]=im_convolved
  }
  return(im_convolved_list)
}

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
           fit_intercept = map_dbl(fit_lm,~.$coefficients[1])) %>%
    rename(curve_id=curve) %>% select(-fit_lm)
}


get_contours <- function(filter_list,im_convolved_list,pixel_quant_step,n_window){
  # Extract contours
  ## Extract 
  im_contours_list = curve_meta = list()
  for (j_filter in 1:length(filter_list)){
    res <- try(img_contours <- image_contour_detector(im_convolved_list[[j_filter]],
                                                      pixel_quant_step))
    if(class(res) == "try-error"){
      im_contours_list[[j_filter]] <- tibble(x=integer(),y=integer(),curve=integer())
      curve_meta[[j_filter]] <-tibble(curve_id=integer(),
                                      min_x=integer(),max_x=integer(),center_x=integer(),
                                      fit_slope=double(),fit_intercept=double(),
                                      n_points=integer(),fit_rsquare=double())
    } else {
      im_contours_list[[j_filter]] <- res$data
      ## Break non bijective curves
      im_contours_list[[j_filter]] = map(split(im_contours_list[[j_filter]],
                                               im_contours_list[[j_filter]]$curve),
                                         break_curves_double_valued) %>%
        bind_rows() %>%
        group_by(curve,curve_bis) %>% mutate(curve_3= group_indices()) %>% 
        ungroup %>% mutate(curve=curve_3) %>% dplyr::select(-curve_bis,-curve_3)
      
      ## Fit linear model on all curves
      curve_meta[[j_filter]] = get_curve_meta(im_contours_list[[j_filter]],n_window=n_window)
    }
  }
  curve_meta
}

get_kmean_clusters <- function(curve_meta,filter_list,target_width,
                               min_rsquare,min_number_of_slopes){
  ##  kmeans clustering}

  tibble(curve_meta,filter_id = 1:length(filter_list)) %>% 
    unnest(curve_meta,keep_empty=T) %>%
    mutate_at("center_x",function(x){
      x[is.na(x)] <- target_width/2
      x
    }) %>%
    mutate_at(c("fit_slope","min_x","n_points","curve_id"),function(x){
      x[is.na(x)] <- 0
      x
    }) %>%
    mutate_at(c("max_x"),function(x){
      x[is.na(x)] <- target_width
      x
    }) %>%
    mutate_at(c("fit_rsquare"),function(x){
      x[is.na(x)] <- target_width
      1
    }) %>%
    filter(fit_rsquare >= min_rsquare) %>%
    select(center_x,fit_slope,min_x,max_x,fit_intercept) %>%
    nest(slopes=c(center_x,fit_slope,fit_intercept),min_max_x=c(min_x,max_x)) %>%
    mutate(slope_count = map_int(slopes,nrow)) %>%
    # filter(slope_count > min_number_of_slopes) %>%
    mutate(slopes_standardized = map(slopes,function(x) { x %>% 
        mutate(center_x = center_x / max(center_x))
      # mutate_all(function(x){x=x-min(x);x/max(x)
    })) %>%
    mutate(kmean_clustering = map2(slopes_standardized,slope_count,
                                   function(x,nn){
                                     if(nn<=min_number_of_slopes*3)
                                       return(list(centers=matrix(c(0,0,0,0),nrow=2),size=c(0,0)))
                                     kmeans(x %>% select(center_x,fit_slope),centers=2)
                                   })) %>%
    mutate(h_clustering = map2(slopes_standardized,slope_count,
                               function(x,nn){
                                 if(nn<=min_number_of_slopes*3)
                                   return(list(height=c(0,0.5)))
                                 distances = dist(x %>% select(center_x,fit_slope), method ="euclidean")
                                 clusters = hclust(distances, method="ward.D")
                               }))
}


get_meta <- function(df_reload){
  df_meta = df_reload %>%
    mutate(center_left = map_dbl(kmean_clustering,~min(.$centers[,1]))) %>%
    mutate(center_right = map_dbl(kmean_clustering,~max(.$centers[,1]))) %>%
    mutate(slope_left =  map_dbl(kmean_clustering,~ .$centers[which.min(.$centers[,1]),2])) %>%
    mutate(slope_right =  map_dbl(kmean_clustering,~ .$centers[which.max(.$centers[,1]),2])) %>%
    # mutate(left_cluster_n =  map_dbl(kmean_clustering,~ .$size[which.min(.$centers[,1])])) %>%
    # mutate(right_cluster_n =  map_dbl(kmean_clustering,~ .$size[which.max(.$centers[,1])])) %>%
    mutate(cluster_height = map_dbl(h_clustering,~tail(.$height,1))) %>%
    mutate(cluster_height_last = map_dbl(h_clustering,function(x){h=x$height;l=length(h);(h[l]-h[l-1])/h[l]})) %>%
    mutate(slope_diff = abs(slope_left-slope_right)) %>%
    mutate(mean_min_x = map_dbl(min_max_x,~min(.$min_x))) %>%
    mutate(mean_max_x = map_dbl(min_max_x,~max(.$min_x))) %>%
    mutate(median_min_x = map_dbl(min_max_x,~median(.$min_x))) %>%
    mutate(median_max_x = map_dbl(min_max_x,~median(.$max_x)))
}

reconstruct_slope <- function(min_x,max_x,fit_slope,fit_intercept){

   y_left = fit_intercept + fit_slope * min_x
   y_right = fit_intercept + fit_slope * max_x
  tibble(y_left = y_left,
         y_right = y_right)
}
