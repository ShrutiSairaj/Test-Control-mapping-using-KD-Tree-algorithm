
library(RANN)
list.files()

#C:\Users\shruti.sairaj\Documents\OG\25.FY20_forecast\Delivery_fulflment_type


setwd("C:/Users/shibendu.mukherjee/Documents/OG/31_Store_Closure")

list.files()

base0<- read.table("tc_map_base_data.csv"  ,sep = ",", header = T, stringsAsFactors = F )
base <- subset(base0 , tenure_OG > 8  )
colnames(base)
base$TC_Flag <- ifelse(base$TC_Flag == 1 ,"T" , "C" )

# Control time period launch before 201824 (tenure >8)

table(base$TC_Flag)
str(base)

base_test <- base[base$TC_Flag == "T",]

base_control <- base[base$TC_Flag == "C",]

colnames(base)


base_master_scale <- cbind(base[,c(1,12)],scale(base[,c(2:11)]))

test_scale <- base_master_scale[base_master_scale$TC_Flag == "T",]
control_scale <- base_master_scale[base_master_scale$TC_Flag == "C",]

nrow(test_scale)     

nrow(control_scale)  

test_final <- cbind(test_scale,1:nrow(test_scale))
control_final <- cbind(control_scale,1:nrow(control_scale))

colnames(test_final)[13] <- "rownum"
colnames(control_final)[13] <- "rownum"

#Adding Weightages
test_final[7] <- test_final[7]*2
control_final[7] <- control_final[7]*2
##Filtering the columns for which the mapping is to be done 

test_final_mapping <- test_final[,c(4,5,7,9,10,12)]
control_final_mapping <- control_final[,c(4,5,7,9,10,12)]

## Mapping using KD-Tree algorithm 

final_map <- nn2(control_final_mapping, query <- test_final_mapping)
#head(final_map,1)
final_map_v1 <- cbind(test_final$rownum, data.frame(final_map$nn.idx[,1]), data.frame(final_map$nn.dists[,1]))

#final_map_v2 <- cbind(test_final$rownum, data.frame(final_map$nn.idx[,c(2,3)]), data.frame(final_map$nn.dists[,c(2,3)]))

colnames(final_map_v1) <- c("test_index", "control_index", "distance")

## Extracting the test and control household ids

final_metrics_test = merge(final_map_v1 , test_final, by.x="test_index", by.y="rownum")
colnames(final_metrics_test)[4]="test_store"

final_metrics_test_control = merge(final_metrics_test,control_final, by.x="control_index", by.y="rownum")

colnames(final_metrics_test_control)[16]="control_store"

final_metrics_test_control_v1 <- final_metrics_test_control[,c(2,4,1,16)]

test_metrics_mapped <- merge(final_metrics_test_control_v1, base_test , by.x = "test_store" , by.y = "delivery_store" )

test_control_metrics_mapped <- merge(test_metrics_mapped, base_control, by.x = "control_store", by.y = "delivery_store")

#write.csv(test_control_metrics_mapped, "mapped_markets_1_to_1.csv",row.names = F)

test_control_metrics_mapped_1to1 <- test_control_metrics_mapped

#############################
# 1:2 Mapping 
############################


## Mapping using KD-Tree algorithm 

#final_map <- nn2(control_final_mapping, query <- test_final_mapping)

final_map_v1 <- cbind(test_final$rownum, data.frame(final_map$nn.idx[,2]), data.frame(final_map$nn.dists[,2]))

#final_map_v2 <- cbind(test_final$rownum, data.frame(final_map$nn.idx[,c(2,3)]), data.frame(final_map$nn.dists[,c(2,3)]))

colnames(final_map_v1) <- c("test_index", "control_index", "distance")

## Extracting the test and control household ids

final_metrics_test = merge(final_map_v1 , test_final, by.x="test_index", by.y="rownum")
colnames(final_metrics_test)[4]="test_store"

final_metrics_test_control = merge(final_metrics_test,control_final, by.x="control_index", by.y="rownum")

colnames(final_metrics_test_control)[16]="control_store"

final_metrics_test_control_v1 <- final_metrics_test_control[,c(2,4,1,16)]

test_metrics_mapped <- merge(final_metrics_test_control_v1, base_test , by.x = "test_store" , by.y = "delivery_store" )

test_control_metrics_mapped <- merge(test_metrics_mapped, base_control, by.x = "control_store", by.y = "delivery_store")


test_control_metrics_mapped_1to2<- test_control_metrics_mapped

#############################
# 1:3 Mapping 
############################


## Mapping using KD-Tree algorithm 

#final_map <- nn2(control_final_mapping, query <- test_final_mapping)

final_map_v1 <- cbind(test_final$rownum, data.frame(final_map$nn.idx[,3]), data.frame(final_map$nn.dists[,3]))

#final_map_v2 <- cbind(test_final$rownum, data.frame(final_map$nn.idx[,c(2,3)]), data.frame(final_map$nn.dists[,c(2,3)]))

colnames(final_map_v1) <- c("test_index", "control_index", "distance")

## Extracting the test and control household ids

final_metrics_test = merge(final_map_v1 , test_final, by.x="test_index", by.y="rownum")
colnames(final_metrics_test)[4]="test_store"

final_metrics_test_control = merge(final_metrics_test,control_final, by.x="control_index", by.y="rownum")

colnames(final_metrics_test_control)[16]="control_store"

final_metrics_test_control_v1 <- final_metrics_test_control[,c(2,4,1,16)]

test_metrics_mapped <- merge(final_metrics_test_control_v1, base_test , by.x = "test_store" , by.y = "delivery_store" )

test_control_metrics_mapped <- merge(test_metrics_mapped, base_control, by.x = "control_store", by.y = "delivery_store")


#write.csv(test_control_metrics_mapped, "mapped_markets_1_to_3.csv",row.names = F)
test_control_metrics_mapped_1to3<- test_control_metrics_mapped




#############################
# 1:4 Mapping 
############################


## Mapping using KD-Tree algorithm 

#final_map <- nn2(control_final_mapping, query <- test_final_mapping)

final_map_v1 <- cbind(test_final$rownum, data.frame(final_map$nn.idx[,4]), data.frame(final_map$nn.dists[,4]))

#final_map_v2 <- cbind(test_final$rownum, data.frame(final_map$nn.idx[,c(2,3)]), data.frame(final_map$nn.dists[,c(2,3)]))

colnames(final_map_v1) <- c("test_index", "control_index", "distance")

## Extracting the test and control household ids

final_metrics_test = merge(final_map_v1 , test_final, by.x="test_index", by.y="rownum")
colnames(final_metrics_test)[4]="test_store"

final_metrics_test_control = merge(final_metrics_test,control_final, by.x="control_index", by.y="rownum")

colnames(final_metrics_test_control)[16]="control_store"

final_metrics_test_control_v1 <- final_metrics_test_control[,c(2,4,1,16)]

test_metrics_mapped <- merge(final_metrics_test_control_v1, base_test , by.x = "test_store" , by.y = "delivery_store" )

test_control_metrics_mapped <- merge(test_metrics_mapped, base_control, by.x = "control_store", by.y = "delivery_store")


#write.csv(test_control_metrics_mapped, "mapped_markets_1_to_3.csv",row.names = F)
test_control_metrics_mapped_1to4<- test_control_metrics_mapped



#############################
# 1:5 Mapping 
############################


## Mapping using KD-Tree algorithm 

#final_map <- nn2(control_final_mapping, query <- test_final_mapping)

final_map_v1 <- cbind(test_final$rownum, data.frame(final_map$nn.idx[,5]), data.frame(final_map$nn.dists[,5]))

#final_map_v2 <- cbind(test_final$rownum, data.frame(final_map$nn.idx[,c(2,3)]), data.frame(final_map$nn.dists[,c(2,3)]))

colnames(final_map_v1) <- c("test_index", "control_index", "distance")

## Extracting the test and control household ids

final_metrics_test = merge(final_map_v1 , test_final, by.x="test_index", by.y="rownum")
colnames(final_metrics_test)[4]="test_store"

final_metrics_test_control = merge(final_metrics_test,control_final, by.x="control_index", by.y="rownum")

colnames(final_metrics_test_control)[16]="control_store"

final_metrics_test_control_v1 <- final_metrics_test_control[,c(2,4,1,16)]

test_metrics_mapped <- merge(final_metrics_test_control_v1, base_test , by.x = "test_store" , by.y = "delivery_store" )

test_control_metrics_mapped <- merge(test_metrics_mapped, base_control, by.x = "control_store", by.y = "delivery_store")


#write.csv(test_control_metrics_mapped, "mapped_markets_1_to_3.csv",row.names = F)
test_control_metrics_mapped_1to5<- test_control_metrics_mapped


############################
# Merging All test to the control
###########################

test_control_metrics_mapped_1to1$control_num <- 1
test_control_metrics_mapped_1to2$control_num <- 2
test_control_metrics_mapped_1to3$control_num <- 3
test_control_metrics_mapped_1to4$control_num <- 4
test_control_metrics_mapped_1to5$control_num <- 5


test_control_metrics_mapped_1to5_2<- rbind(test_control_metrics_mapped_1to1,
test_control_metrics_mapped_1to2,test_control_metrics_mapped_1to3,test_control_metrics_mapped_1to4,test_control_metrics_mapped_1to5)


colnames(test_control_metrics_mapped_1to5_2)

tc_mapped_complete_list <- test_control_metrics_mapped_1to5_2[,c(2,1,27,5:25)]
colnames(tc_mapped_complete_list)


write.csv(tc_mapped_complete_list,"tc_mapped_store.csv")


####################################################################

final_map_v1 <- cbind(test_final$rownum, data.frame(final_map$nn.idx[,c(1)]), data.frame(final_map$nn.dists[,c(1)]))


colnames(final_map_v1) <- c("test_index", "control_index", "distance")

final_map_v2 <- cbind(test_final$rownum, data.frame(final_map$nn.idx[,c(2)]), data.frame(final_map$nn.dists[,c(2)]))
colnames(final_map_v2) <- c("test_index", "control_index", "distance")

final_map_v3 <- cbind(test_final$rownum, data.frame(final_map$nn.idx[,c(3)]), data.frame(final_map$nn.dists[,c(3)]))
colnames(final_map_v3) <- c("test_index", "control_index", "distance")

final_map_v4 <- cbind(test_final$rownum, data.frame(final_map$nn.idx[,c(4)]), data.frame(final_map$nn.dists[,c(4)]))
colnames(final_map_v4) <- c("test_index", "control_index", "distance")

final_map_v5 <- cbind(test_final$rownum, data.frame(final_map$nn.idx[,c(5)]), data.frame(final_map$nn.dists[,c(5)]))
colnames(final_map_v5) <- c("test_index", "control_index", "distance")


final_map_v <- rbind(final_map_v1, final_map_v2, final_map_v3,final_map_v4, final_map_v5)

write.csv(final_map_v,"final_map_dist_911.csv")

colnames(final_map_v)


library(sqldf)

test_control_metrics_mapped_1to5_3 <- sqldf("select t1.* , t2.distance 
                                            from  test_control_metrics_mapped_1to5_2 t1 
                                            left join final_map_v t2 on t1.control_index = t2.control_index and t1.test_index = t2.test_index")


colnames(test_control_metrics_mapped_1to5_3)

tc_mapped_complete_list <- test_control_metrics_mapped_1to5_3[,c(2,1,28,27,5:25)]
colnames(tc_mapped_complete_list)


write.csv(tc_mapped_complete_list,"tc_mapped_store_2.csv")
