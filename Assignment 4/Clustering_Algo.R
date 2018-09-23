MyData <- read.csv("C:\\Users\\Rishabh_7\\Desktop\\Data Mining\\Assignment 4\\iris_cfs_canopy.csv",head=TRUE)
print(MyData)

km = kmeans(MyData,3)

print(km)