#Basic example : pyramid
data_funnel <- data.frame(description = c("Website visits", "Downloads", 
                                          "Requested price list", 
                                          "Contaced for more info",
                                          "Purchased", "Contacted for support",
                                          "Purchased additional products"), 
                          value = c(300, 123, 98, 72, 80, 15, 8),
                          stringsAsFactors = FALSE)
                          
amFunnel(data = data_funnel, inverse = TRUE)
             
#Change the orientation and legend side              
amFunnel(data = data_funnel, inverse = FALSE,
               label_side = "left", margin_right = 15, margin_left = 160)

#Basic example : Funnel chart
amFunnel(data = data_funnel, neck_height = 30, neck_width = 40)
               
#3D pyramid
amFunnel(data = data_funnel, depth = 50, inverse = TRUE)
