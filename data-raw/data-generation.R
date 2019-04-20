#
ajk_Longini_table1 <- cbind('1'=c(NA,NA,NA,NA),
                    '2'=c(NA,NA,NA,NA),
                    '3'=c(29,9,2,2))
ajk_Longini_table1 <- data.frame(ajk_Longini_table1)
rownames(ajk_Longini_table1) <- c(0:3)
colnames(ajk_Longini_table1) <- paste("hhSize",c(1:3),sep="_")
save(ajk_Longini_table1,file="./data/ajk_Longini_table1.RData")
#
ajk_Longini_table2Tecumeseh <- cbind('1'=c(65,13,NA,NA,NA),
                            '2'=c(88,14,4,NA,NA),
                            '3'=c(27,15,4,4,NA),
                            '4'=c(22,9,9,3,1))
ajk_Longini_table2Tecumeseh <- data.frame(ajk_Longini_table2Tecumeseh)
rownames(ajk_Longini_table2Tecumeseh) <- c(0:4)
colnames(ajk_Longini_table2Tecumeseh) <- paste("hhSize",c(1:4),sep="_")
save(ajk_Longini_table2Tecumeseh,file="./data/ajk_Longini_table2Tecumeseh.RData")
#
ajk_Longini_table2Seatle <- cbind('1'=c(29,12,NA,NA,NA),
                                     '2'=c(19,14,9,NA,NA),
                                     '3'=c(23,11,11,4,NA),
                                     '4'=c(9,9,2,3,4))
ajk_Longini_table2Seatle <- data.frame(ajk_Longini_table2Seatle)
rownames(ajk_Longini_table2Seatle) <- c(0:4)
colnames(ajk_Longini_table2Seatle) <- paste("hhSize",c(1:4),sep="_")
save(ajk_Longini_table2Seatle,file="./data/ajk_Longini_table2Seatle.RData")

ajk_Longini_table3uncrowded <- cbind('1'=c(NA,NA,NA,NA,NA,NA),
                                  '2'=c(NA,NA,NA,NA,NA,NA),
                                  '3'=c(NA,NA,NA,NA,NA,NA),
                                  '4'=c(NA,NA,NA,NA,NA,NA),
                                  '5'=c(0,156,55,19,10,2))
ajk_Longini_table3uncrowded <- data.frame(ajk_Longini_table3uncrowded)
rownames(ajk_Longini_table3uncrowded) <- c(0:5)
colnames(ajk_Longini_table3uncrowded) <- paste("hhSize",c(1:5),sep="_")
save(ajk_Longini_table3uncrowded,file="./data/ajk_Longini_table3uncrowded.RData")
#
ajk_Longini_table3crowded <- cbind('1'=c(NA,NA,NA,NA,NA,NA),
                                     '2'=c(NA,NA,NA,NA,NA,NA),
                                     '3'=c(NA,NA,NA,NA,NA,NA),
                                     '4'=c(NA,NA,NA,NA,NA,NA),
                                     '5'=c(0,155,41,24,15,6))
ajk_Longini_table3crowded <- data.frame(ajk_Longini_table3crowded)
rownames(ajk_Longini_table3crowded) <- c(0:5)
colnames(ajk_Longini_table3crowded) <- paste("hhSize",c(1:5),sep="_")
save(ajk_Longini_table3crowded,file="./data/ajk_Longini_table3crowded.RData")
#
ajk_Longini_table3overcrowded <- cbind('1'=c(NA,NA,NA,NA,NA,NA),
                                     '2'=c(NA,NA,NA,NA,NA,NA),
                                     '3'=c(NA,NA,NA,NA,NA,NA),
                                     '4'=c(NA,NA,NA,NA,NA,NA),
                                     '5'=c(0,112,35,17,11,6))
ajk_Longini_table3overcrowded <- data.frame(ajk_Longini_table3overcrowded)
rownames(ajk_Longini_table3overcrowded) <- c(0:5)
colnames(ajk_Longini_table3overcrowded) <- paste("hhSize",c(1:5),sep="_")
save(ajk_Longini_table3overcrowded,file="./data/ajk_Longini_table3overcrowded.RData")
#
ajk_Haber_table1 <- cbind('1'=c(65,13,NA,NA,NA),
                          '2'=c(88,14,4,NA,NA),
                          '3'=c(27,15,4,4,NA),
                          '4'=c(22,9,9,3,1))
ajk_Haber_table1 <- data.frame(ajk_Haber_table1)
rownames(ajk_Haber_table1) <- c(0:4)
colnames(ajk_Haber_table1) <- paste("hhSize",c(1:4),sep="_")
save(ajk_Haber_table1,file="./data/ajk_Haber_table1.RData")
#
ajk_Haber_table3 <- cbind('1'=c(40,4,NA,NA,NA,NA),
                          '2'=c(63,12,2,NA,NA,NA),
                          '3'=c(18,10,3,2,NA,NA),
                          '4'=c(17,5,3,3,1,NA),
                          '5'=c(3,4,2,2,1,0))
ajk_Haber_table3 <- data.frame(ajk_Haber_table3)
rownames(ajk_Haber_table3) <- c(0:5)
colnames(ajk_Haber_table3) <- paste("hhSize",c(1:5),sep="_")
save(ajk_Haber_table3,file="./data/ajk_Haber_table3.RData")
#
# Data
a1c1 <- a2c1 <- a1c2 <- a2c2 <- a1c3 <- a2c3 <- matrix(NA,3,4)
a1c1[1+0,1+1] <- 1
a1c1[1+1,1+1] <- 1
a2c1[1+0,1+1] <- 6
a2c1[1+1,1+1] <- 2
a2c1[1+2,1+1] <- 0
a1c2[1+0,1+1] <- 0
a1c2[1+0,1+2] <- 1
a1c2[1+1,1+1] <- 0
a1c2[1+1,1+2] <- 4
a2c2[1+0,1+1] <- 6
a2c2[1+0,1+2] <- 5
a2c2[1+1,1+1] <- 4
a2c2[1+1,1+2] <- 2
a2c2[1+2,1+1] <- 1
a2c2[1+2,1+2] <- 1
a1c3[1+0,1+1] <- 1
a1c3[1+0,1+2] <- 0
a1c3[1+0,1+3] <- 0
a1c3[1+1,1+1] <- 0
a1c3[1+1,1+2] <- 0
a1c3[1+1,1+3] <- 0
a2c3[1+0,1+1] <- 1
a2c3[1+0,1+2] <- 1
a2c3[1+0,1+3] <- 0
a2c3[1+1,1+1] <- 1
a2c3[1+1,1+2] <- 1
a2c3[1+1,1+3] <- 1
a2c3[1+2,1+1] <- 0
a2c3[1+2,1+2] <- 0
a2c3[1+2,1+3] <- 0
#
ajk_Longini_table3 <- list(a1c1,a2c1,a1c2,a2c2,a1c3,a2c3)
ajk_Longini_table3 <- data.frame(ajk_Longini_table3)
save(ajk_Longini_table3,file="./data/ajk_Longini_table3.RData")
#devtools::use_data(ajkmn1)
#
