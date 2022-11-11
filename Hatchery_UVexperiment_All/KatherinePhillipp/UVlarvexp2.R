#microbe abundance data
cd1a = c(3042550.804, 2691487.249,	3276593.173,	3627656.727,	2340423.695)
cd1b = c(3393614.358, 2340423.695,	3861699.097,	3744677.912,	5265953.314)
cd1c = c(1872338.956,	2925529.619,	3744677.912,	4563826.205,	5265953.314)
uvd1a = c(1287233.032,	1462764.809,	2106381.326,	1813828.364, 2223402.51)
uvd1b = c(1228722.44,	2632976.657,	585105.9238,	1345743.625, 994680.0704)
uvd1c = c(468084.739,	643616.5161,	409574.1466,	702127.1085, 848403.5895)
cd3a = c(4446805.021,	4329783.836,	5148932.129,	3159571.988, 6202122.792)
cd3b = c(4914889.76,	5265953.314,	1404254.217,	4563826.205, 2574466.065)
cd3c = c(1872338.956,	4329783.836,	3159571.988,	3159571.988, 5968080.422)
uvd3a = c(1872338.956,	1755317.771,	2164891.918,	1813828.364, 2106381.326)
uvd3b = c(1989360.141,	1404254.217,	1989360.141,	1755317.771, 1930849.548)
uvd3c = c(975176.5396,	663120.0469,	1365247.155,	546098.8622, 1560282.463)
IQR(cd1a)
IQR(cd1b)
IQR(cd1c)
IQR(uvd1a)
IQR(uvd1b)
IQR(uvd1c)
IQR(cd3a)
IQR(cd3b)
IQR(cd3c)
IQR(uvd3a)
IQR(uvd3b)
IQR(uvd3c)
median(cd1a)
median(cd1b)
median(cd1c)
median(uvd1a)
median(uvd1b)
median(uvd1c)
median(cd3a)
median(cd3b)
median(cd3c)
median(uvd3a)
median(uvd3b)
median(uvd3c)
boxplot(cd1a, cd1b, cd1c, cd3a, cd3b, cd3c, uvd1a, uvd1b, uvd1c, uvd3a, uvd3b, uvd3c)
boxplot(cd1a, cd1b, cd1c, uvd1a, uvd1b, uvd1c, cd3a, cd3b, cd3c, uvd3a, uvd3b, uvd3c,
        ylab = "Microbes per mL", 
        xlab = "Day 1                                                         Day 3",
        col = c('gray','gray','gray', 'blue', 'blue', 'blue', 'gray', 'gray', 'gray',
                'blue', 'blue', 'blue'))
legend("topright", inset = 0.02, legend = c("UV", "Control"), 
       pch = c(20, 20), col = c("blue", "gray"))
cd1 = c(3042550.804, 2691487.249,	3276593.173,	3627656.727,	2340423.695, 
        3393614.358, 2340423.695,	3861699.097,	3744677.912,	5265953.314,
        1872338.956,	2925529.619,	3744677.912,	4563826.205,	5265953.314)
uvd1 = c(1287233.032,	1462764.809,	2106381.326,	1813828.364, 2223402.51,
         1228722.44,	2632976.657,	585105.9238,	1345743.625, 994680.0704,
         468084.739,	643616.5161,	409574.1466,	702127.1085, 848403.5895)
cd3 = c(4446805.021,	4329783.836,	5148932.129,	3159571.988, 6202122.792,
        4914889.76,	5265953.314,	1404254.217,	4563826.205, 2574466.065, 
        1872338.956,	4329783.836,	3159571.988,	3159571.988, 5968080.422)
uvd3 = c(1872338.956,	1755317.771,	2164891.918,	1813828.364, 2106381.326,
         1989360.141,	1404254.217,	1989360.141,	1755317.771, 1930849.548,
         975176.5396,	663120.0469,	1365247.155,	546098.8622, 1560282.463)
mean(cd1)
mean(uvd1) 
mean(cd3)
mean(uvd3)
sd(cd1)
sd(uvd1) 
sd(cd3)
sd(uvd3)
median(cd1)
median(uvd1) 
median(cd3)
median(uvd3)
t.test(uvd1,cd1)
t.test(uvd3,cd3)

#larval suvival data
Control = c(1610000, 1490000, 1530000)
UV = c(1656000, 1770000, 1830000)
boxplot(Control, UV, ylab = "Number of Larvae present on Day 3", xlab = "Treatment Group",
        title = "Larval survival in different treatment groups", col = c("gray", "blue"))
legend("bottomright", inset = 0.16, legend = c("UV", "Control"), 
       pch = c(20, 20), col = c("blue", "gray"))
mean(UV)
mean(Control)
sd(UV)
median(UV)
sd(Control)
IQR(UV)
IQR(Control)
t.test(UV,Control)
