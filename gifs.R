#gifs of 4.5 and 8.5 for BRT
library("magick")
gif1<-list.files(path="E:/BrokenHardDrive/postdoc/analysis_files/png_files/gif",
                 pattern="45", full.names = T)
gif1<-image_read(gif1)
gif2<-image_read("E:/BrokenHardDrive/postdoc/analysis_files/png_files/gif/current_Dreissena bugensis.png")
gif<-c(gif1,gif2)
map_gif<-image_animate(gif, fps=.25, dispose="previous")
image_write(map_gif, path="E:/BrokenHardDrive/postdoc/analysis_files/png_files/gif_BRT/Dreissena bugensis_45.gif",
            format="gif")

gif1<-list.files(path="E:/BrokenHardDrive/postdoc/analysis_files/png_files/gif",
                 pattern="85", full.names = T)
gif1<-image_read(gif1)
gif<-c(gif1,gif2)
map_gif<-image_animate(gif, fps=.25, dispose="previous")
image_write(map_gif, path="E:/BrokenHardDrive/postdoc/analysis_files/png_files/gif_BRT/Dreissena bugensis_85.gif",
            format="gif")

#gifs for 4.5 and 8.5 for RAMP
gif1<-list.files(path="C:/Users/vprescott/Desktop/RAMP2/Species/Dreissena_bugensis",
                 pattern="4_results.jpg", full.names = T)
gif1<-image_read(gif1)
gif2<-image_read("C:/Users/vprescott/Desktop/RAMP2/Species/Dreissena_bugensis/Dreissena_bugensis_Great Lakes Basin_Current_results.jpg")
gif<-c(gif1,gif2)
map_gif<-image_animate(gif, fps=.25, dispose="previous")
image_write(map_gif, path="E:/BrokenHardDrive/postdoc/analysis_files/png_files/gif_RAMP/Dreissena_bugensis_45.gif",
            format="gif")

gif1<-list.files(path="C:/Users/vprescott/Desktop/RAMP2/Species/Dreissena_bugensis",
                 pattern="8_results.jpg", full.names = T)
gif1<-image_read(gif1)
gif<-c(gif1,gif2)
map_gif<-image_animate(gif, fps=.25, dispose="previous")
image_write(map_gif, path="E:/BrokenHardDrive/postdoc/analysis_files/png_files/gif_RAMP/Dreissena_bugensis_85.gif",
            format="gif")
