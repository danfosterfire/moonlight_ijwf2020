validate_model = 
  function(X_i, y_i, yhat_i, ep_i){
    
    
    plot_data = 
      X_i %>%
      mutate(residual = ep_i, 
             fitted = yhat_i,
             scale_fitted = order(fitted)/n(),
             observed = y_i,
             scale_observed = order(y_i)/n())
    
    
    obs_pred = 
      ggplot(data = plot_data,
             aes(x = fitted, y = observed))+
      geom_point(alpha = 0.5, size = 0)+
      geom_abline(intercept = 0, slope = 1, col = 'red', lty = 3)+
      theme_minimal()
    
    hist_resids = 
      ggplot(data = plot_data,
             aes(x = residual))+
      geom_histogram()+
      theme_minimal()
    
    resid_pred = 
      ggplot(data = plot_data,
             aes(x = fitted, y = residual))+
      geom_point(alpha = 0.5, size = 0)+
      geom_smooth()+
      theme_minimal()
    
    resid2_pred = 
      ggplot(data = plot_data,
             aes(x = fitted, y = residual**2))+
      geom_point(alpha = 0.5, size = 0)+
      geom_smooth(method = 'lm')+
      geom_abline(intercept = 0, slope = 0, lty = 'dashed', color = 'red')+
      theme_minimal()
    
    resid_time = 
      ggplot(data = plot_data,
             aes(x = time, y = residual))+
      geom_boxplot()+
      theme_minimal()
    
    resid_planting = 
      ggplot(data = plot_data,
             aes(x = planting, y = residual))+
      geom_boxplot()+
      theme_minimal()
    
    resid_followup = 
      ggplot(data = plot_data,
             aes(x = followup, y = residual))+
      geom_boxplot()+
      theme_minimal()
    
    resid_timeplanting = 
      ggplot(data = plot_data,
             aes(x = paste0(time,':',planting), y = residual))+
      geom_boxplot()+
      theme_minimal()
    
    resid_timefollowup = 
      ggplot(data = plot_data,
             aes(x = paste0(time,':',followup), y = residual))+
      geom_boxplot()+
      theme_minimal()
    
    resid_timeplantingfollowup = 
      ggplot(data = plot_data,
             aes(x = paste0(time,':',planting,':',followup), y = residual))+
      geom_boxplot()+
      theme_minimal()
    
    
    gridExtra::grid.arrange(obs_pred, hist_resids,
                            resid_pred, resid2_pred, 
                            resid_time, resid_planting,
                            resid_followup, 
                            resid_timeplanting, resid_timefollowup,
                            resid_timeplantingfollowup,
                            ncol = 2)
    
  }
