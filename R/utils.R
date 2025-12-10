# Utility functions (not exported)



#1 Get colors

      get.colors <- function(k) {
                  if (k==1) return('dodgerblue')
          
          if (k == 2) {
            return(c("red4", "dodgerblue"))
          } else if (k == 3) {
            return(c("red4", "dodgerblue", "green4"))
          } else if (k == 4) {
            return(c("orange1", "orange3", "red2", "red4"))
          } else {
            # For 5+ groups, use a combination of colors that cycle
            # Start with the 4-group palette and add more colors
            base_colors <- c(  "orange1", "orange3", "red2", "red4",
                             "dodgerblue", "dodgerblue3", "blue1", "blue4",
                             "green", "darkgreen",  "darkorchid", "purple4","gray88", "gray11")
            return(base_colors[1:k])
          }
      }

      
      
#2 Nice exit
  exit <- function(msg, col='red4',font=2) {
    message.col(msg,col=col,font=font)
    invokeRestart("abort") 
  }

