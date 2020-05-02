
shinyServer(function(input, output) {
    
    transform_input_image <- reactive({
        validate(
            need(input$input_pw == 'gagliano_tufnell',label='password'),
            need(nrow(input$jpg_name)!=0, 'choose a jpg',label='file')
        )
        full_jpg_name = input$jpg_name$datapath
        jpg_name = input$jpg_name$name
        img_full = readImage(full_jpg_name)
        
        if(dim(img_full)[3] == 4)
            img_full = img_full[,,1:3]
        
        width = dim(img_full)[2]
        validate(need(width >= min_width,label='min width'))
        height = dim(img_full)[1]
        quarter_width = width * width_percent/100.0
        x_mid = round(width/2) * horizontal_correction_coef
        xleft = round(x_mid - quarter_width)
        xright = round(x_mid + quarter_width)
        
        df_meta = tibble()
        im_conv = tibble()
        im_cont = tibble()
        im_arr = tibble()
        
        for (i in seq_along(height_percent_crop)){
            
            ytop = round(height_percent_crop[i]/100 * height - quarter_width)
            ybot = round(height_percent_crop[i]/100 * height + quarter_width)
            
            img_array = img_full[ytop:ybot,xleft:xright,] %>%
                rgb_2gray(.) %>%
                resizeImage(.,height = target_height,width = target_width)
            dim(img_array) <- c(dim(img_array),1)
            
            
            res = get_color_gradients(img_array)
            x_curve_max= res[1]
            standard_curve_max = res[2]
            
            im_convolved_list = gabor_convolve_wrap(img_array,filter_list)
            
            res = get_contours(filter_list,im_convolved_list,pixel_quant_step,n_window)
            curve_meta = res$curve_meta
            im_contours_list = res$im_contours_list
            df_cluster <- get_kmean_clusters(curve_meta,filter_list,target_width,
                                             min_rsquare,min_number_of_slopes)
            
            meta_df <- get_meta(df_cluster %>% mutate(x_curve_max=x_curve_max,
                                                      standard_curve_max=standard_curve_max,
                                                      jpg_name=jpg_name))
            
            im_arr = im_arr %>% bind_rows(tibble(img_array=list(img_array)) %>% 
                                              mutate(crop_height = factor(crop_heights[i],
                                                                          levels=crop_heights,labels=crop_heights)))
            im_conv = im_conv %>% bind_rows(tibble(img_conv = im_convolved_list) %>% 
                                                mutate(crop_height = factor(crop_heights[i],
                                                                            levels=crop_heights,labels=crop_heights),
                                                       filter_no = 1:n()))
            im_cont = im_cont %>% bind_rows(tibble(img_cont = im_contours_list,curve_meta=curve_meta) %>%
                                                mutate(crop_height = factor(crop_heights[i],
                                                                            levels=crop_heights,labels=crop_heights),
                                                       filter_no = 1:n()))
            df_meta = df_meta %>% bind_rows(meta_df %>% mutate(crop_height=factor(crop_heights[i],
                                                                                  levels=crop_heights,labels=crop_heights)))
        }
        
        load(model_path)
        print(names(df_meta))
        df_meta_wide = df_meta %>% 
            pivot_wider(names_from=crop_height,values_from = -c(jpg_name,crop_height)) 
        pred = predict(fit_nl,df_meta_wide)
        
        left_slope_signs = sign(df_meta$slope_left)
        right_slope_signs = sign(df_meta$slope_right)
        if (pred=="two_piece"){
            flame_orientation = case_when(
                all(left_slope_signs == 1) && all(right_slope_signs==-1) ~ "descending from center seam",
                all(left_slope_signs == -1) && all(right_slope_signs==1) ~ "ascending from center seam",
                all(left_slope_signs == 1) && all(right_slope_signs==1) ~ "ascending from treble to bass",
                all(left_slope_signs == -1) && all(right_slope_signs==-1) ~ "descending from treble to bass",
            )
        } else
            flame_orientation = NA
        
        list(df_meta=df_meta,df_meta_wide=df_meta_wide,
             im_cont=im_cont,
             flame_orientation=flame_orientation,pred=pred,
             im_conv=im_conv,im_arr=im_arr,
             height=height,width=width)
        
    })
    
    output$original_image <- renderImage({
        res <- transform_input_image()
        ar = res$height / res$width
        if (res$height > 800){
            height_out = 800
            width_out = round(height_out / ar)
        } else {
            height_out = res$height
            width_out = res$width
        }
        # browser()
        list(src = input$jpg_name$datapath,
             contentType = 'image/png',
             width = width_out,
             height = height_out,
             alt = "")
        
    }, deleteFile = is_delete_image)
    
    output$prediction <- renderText({
        res <- transform_input_image()
        paste("Model prediction :",res$pred,
              "\n",ifelse(is.na(res$flame_orientation),"",res$flame_orientation))
    })
    
    output$raw_patches <-renderPlot({
        res <- transform_input_image()
        res$im_arr %>% 
            mutate(img_raster = map(img_array,function(x){ 
                x %>% as_tibble %>% mutate(x=1:n()) %>% 
                    pivot_longer(cols=c(1:target_width)) %>%
                    mutate(y=as.numeric(stringr::str_sub(name,start = 2,end=4))) %>%
                    mutate(x = max(x)-x)
            })) %>%
            unnest(img_raster) %>% 
            ggplot(aes(x=y,y=x,fill=value)) + geom_raster() + facet_wrap(~crop_height,nrow=3) +
            scale_fill_gradientn(colors=grey.colors(200), guide = FALSE) + coord_fixed()
    })
    
    output$convolution_plot <- renderPlot({
        res <- transform_input_image()
        raw_patched  <- res$im_arr %>% 
            mutate(img_raster = map(img_array,function(x){ 
                x %>% as_tibble %>% mutate(x=1:n()) %>% 
                    pivot_longer(cols=c(1:target_width)) %>%
                    mutate(y=as.numeric(stringr::str_sub(name,start = 2,end=4))) %>%
                    mutate(x = max(x)-x)
            })) %>% select(-img_array)
        conv_patches <- res$im_conv %>%
            mutate(img_raster = map(img_conv,function(x){ 
                x %>% as_tibble %>% mutate(x=1:n()) %>% 
                    pivot_longer(cols=c(1:138)) %>%
                    mutate(y=as.numeric(stringr::str_sub(name,start = 2,end=4))) %>%
                    mutate(x = max(x)-x)
            })) %>% select(-img_conv)
        raw_patched  %>% mutate(filter_no=0) %>%
            bind_rows(conv_patches) %>%
            unnest(img_raster) %>%
            ggplot(aes(x=y,y=x,fill=value)) + geom_raster() + 
            facet_wrap(~filter_no + crop_height,nrow=3,scales="free",dir="v") +
            scale_fill_gradientn(colors=grey.colors(200), guide = FALSE)
        
    })
    

# line segments -----------------------------------------------------------

    output$line_plot <- renderPlot({
        res <- transform_input_image()
        df_meta=res$df_meta
        curve_meta = res$im_cont %>% select(-img_cont) %>% unnest(curve_meta)
        im_cont = res$im_cont %>% select(-curve_meta) %>% unnest(img_cont)
        meta_plot <- df_meta %>% 
            mutate(max_center_x = map_dbl(slopes,~max(.$center_x))) %>%
            mutate(cluster_ids = map(kmean_clustering,~.$cluster)) %>%
            select(crop_height,slopes,cluster_ids,max_center_x,min_max_x) %>% 
            unnest(c(slopes,cluster_ids,min_max_x)) %>%
            left_join(curve_meta,by=c("fit_slope"="fit_slope",
                                      'fit_intercept'="fit_intercept",
                                      "center_x"="center_x",'min_x'='min_x','max_x'='max_x',
                                      'crop_height'='crop_height')) %>%
            mutate_at("cluster_ids",factor) %>%
            mutate(slopes_reconstructed = pmap(list(min_x,max_x,fit_slope,fit_intercept),
                                              reconstruct_slope)) %>%
            unnest(slopes_reconstructed)  %>% 
            mutate(curve_id = 1:n()) %>%
            select(crop_height,curve_id,cluster_ids,min_x,max_x,y_left,y_right,fit_rsquare) %>%
            pivot_longer(c(y_left,y_right)) %>%
            filter(fit_rsquare >= input$fit_rsquare) %>%
            mutate(x=if_else(name=="y_left",min_x,max_x))


        meta_plot %>%
            ggplot(aes(x=x,y=value)) + 
            geom_line(data=im_cont,inherit.aes = F,aes(x=x,y=y,group=factor(paste(curve,filter_no))),alpha=0.3) + 
            geom_line(aes(col=cluster_ids,group=curve_id)) + 
            facet_wrap(~crop_height,ncol=3) + coord_fixed()
    })

# line meta ---------------------------------------------------------------

    
    output$processing_plot <- renderPlot({
        
        res <- transform_input_image()
        df_meta=res$df_meta
        center_meta = bind_rows(tibble(crop_height = df_meta$crop_height,
                                       center_x = df_meta$center_left,
                                       fit_slope = df_meta$slope_left,
                                       class_of_point = 'cluster center'),
                                tibble(crop_height = df_meta$crop_height,
                                       center_x = df_meta$center_right,
                                       fit_slope = df_meta$slope_right,
                                       class_of_point = 'cluster center'))
        
        meta_plot <- df_meta %>% 
            mutate(max_center_x = map_dbl(slopes,~max(.$center_x))) %>%
            mutate(cluster_ids = map(kmean_clustering,~.$cluster)) %>%
            select(crop_height,slopes,cluster_ids,max_center_x) %>% 
            unnest(c(slopes,cluster_ids)) %>%
            mutate(class_of_point = paste("slope center",cluster_ids)) %>%
            select(-cluster_ids) %>%
            mutate(center_x=center_x/max_center_x)
        
        meta_plot %>%
            bind_rows(center_meta) %>%
            ggplot(aes(x=center_x,y=fit_slope)) + 
            geom_point(aes(col=class_of_point),size=5,shape=1) + 
            geom_hline(yintercept = 0, color = 'purple') + 
            facet_wrap(~crop_height,ncol=1)
    })
})
