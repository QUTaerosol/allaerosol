## CIMS FAMILY PLOTTER
# install.packages(ggplot2)
library(ggplot2)

# Here's a simple dataset to simulate your CIMS data 
sample_data <- data.frame(mass=c(1,1.1,1.2,2,2.1,2.2,3,3.1,3.2,4,4.1,4.2), 
                          defect=c(-1,0,1,0,1,2,-2,-1,0,-1,0,1), 
                          pk_area=c(1,7,3,4,2,9,7,3,11,5,8,6),
                          family=c("F1","F1","F2","F2","F3","F1","F2","F3","F2","F1","F3","F3"))

# The following graph scales the point size based on the peak area, and colours them based on their family
ggplot(sample_data) +
  geom_point(aes(x=mass,y=defect,group=family,colour=family,size=pk_area)) +
  theme_bw()

# The following graph does the same but also assigns different point shapes for each family
# (to create a colourblind-friendly plot)
ggplot(sample_data) +
  geom_point(aes(x=mass,y=defect,group=family,colour=family,size=pk_area,shape=family)) +
  theme_bw()
