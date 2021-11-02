cat("\n\n################################################################################################")
cat("\n# START EXECUTE ORACLE CLUS - JOIN VALIDATION WITH TRAIN                                         #")
cat("\n##################################################################################################\n\n") 

##################################################################################################
# ORACLE PARTITIONS                                                                              #
# Copyright (C) 2021                                                                             #
# WITH CLUS -- JOIN TRAIN WITH VALIDATION                                                        #
#                                                                                                #
# This code is free software: you can redistribute it and/or modify it under the terms of the    #
# GNU General Public License as published by the Free Software Foundation, either version 3 of   #  
# the License, or (at your option) any later version. This code is distributed in the hope       #
# that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for    #
# more details.                                                                                  #     
#                                                                                                #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
#                                                                                                #
##################################################################################################


##################################################################################################
# Script 7 - Execute on Cluster/Server                                                           #
##################################################################################################


##################################################################################################
# Options Configuration                                                                          #
##################################################################################################
options(java.parameters = "-Xmx32g")
options(show.error.messages = TRUE)
options(scipen=30)


##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
sistema = c(Sys.info())
FolderRoot = ""
if (sistema[1] == "Linux"){
  FolderRoot = paste("/home/", sistema[7], "/Oracle-Clus-TVT", sep="")
} else {
  FolderRoot = paste("C:/Users/", sistema[7], "/Oracle-Clus-TVT", sep="")
}
FolderScripts = paste(FolderRoot, "/scripts/", sep="")



##################################################################################################
# Read the dataset file with the information for each dataset                                    #
##################################################################################################
setwd(FolderRoot)
datasets <- data.frame(read.csv("datasets.csv"))


##################################################################################################
# ARGS COMMAND LINE                                                                              #
##################################################################################################
args <- commandArgs(TRUE)


##################################################################################################
# Get dataset information                                                                        #
##################################################################################################
number_dataset = as.numeric(args[1])


##################################################################################################
# Get dataset information                                                                        #
##################################################################################################
ds <- datasets[as.numeric(args[1]),]
cat("\nOracle: DS \t ", as.numeric(args[1]))


##################################################################################################
# Get the number of cores                                                                        #
##################################################################################################
number_cores <- as.numeric(args[2])
cat("\nOracle: cores \t ", number_cores)


##################################################################################################
# Get the number of folds                                                                        #
##################################################################################################
number_folds <- as.numeric(args[3])
cat("\nOracle: folds \t ", number_folds)


##################################################################################################
# Get the number of folds                                                                        #
##################################################################################################
folderResults  <- toString(args[4])
cat("\nOracle: folder \t ", folderResults)


##################################################################################################
# Get dataset name                                                                               #
##################################################################################################
dataset_name  <- toString(ds$Name) 
cat("\nOracle: nome \t ", dataset_name)


##################################################################################################
# DON'T RUN -- it's only for test the code
# ds <- datasets[29,]#
# dataset_name = ds$Name
# number_dataset = ds$Id
# number_cores = 10
# number_folds = 10
# folderResults = "/dev/shm/res"
##################################################################################################


##################################################################################################
# cat("\n\nCopy FROM google drive \n")
# destino = paste(FolderRoot, "/datasets/", dataset_name, sep="")
# origem = paste("cloud:Datasets/CrossValidation_WithValidation/", dataset_name, sep="")
# comando = paste("rclone -v copy ", origem, " ", destino, sep="") 
# cat("\n", comando, "\n") 
# a = print(system(comando))
# a = as.numeric(a)
# if(a != 0) {
# stop("Erro RCLONE")
# quit("yes")
# }


##################################################################################################
# CONFIG THE FOLDER RESULTS                                                                      #
##################################################################################################
if(dir.exists(folderResults)==FALSE){
  dir.create(folderResults)
}
cat("\n", folderResults)


##################################################################################################
# LOAD RUN.R                                                                                     #
##################################################################################################
setwd(FolderScripts)
source("run.R") 


##################################################################################################
# GET THE DIRECTORIES                                                                            #
##################################################################################################
cat("\nGet directories\n")
diretorios <- directories(dataset_name, folderResults)


##################################################################################################
# cat("\n Copy partitions from google drive")
# destino = paste(diretorios$folderBellPart, "/", dataset_name, sep="")
# origem = paste("cloud:[2021]ResultadosExperimentos/BellPartitions/", dataset_name, sep="")
# comando1 = paste("rclone -v copy ", origem, " ", destino, sep="")
# cat("\n", comando1, "\n") 
# a = print(system(comando1))
# a = as.numeric(a)
# if(a != 0) {
# stop("Erro RCLONE")
# quit("yes")
# }



##################################################################################################
# Get the number of bell partitions                                                              # 
##################################################################################################
Folder_ = paste(FolderRoot, "/BellPartitions/", dataset_name, sep="")
setwd(Folder_)
str_ = paste(dataset_name, "-groupsPerPartitions.csv", sep="")
bell = data.frame(read.csv(str_))
n = nrow(bell)


##################################################################################################
apagar = c(0)
result = data.frame(apagar)
namesMeasures = c("")


##################################################################################################
# EXECUTE                                                                                        # 
##################################################################################################
count = 2
id_part = 2
while(id_part<=n){
  
  diretorios <- directories(dataset_name, folderResults)
  
  cat("\nPARTITION: ", id_part, "\n")
  
 ########################################################################################################################
 # execute oracle experiment
 timeEP = system.time(res <- oraclePartitions(number_dataset, number_cores, number_folds, id_part, folderResults))
 print(timeEP)
 
 ########################################################################################################################
 # create folder to save results
 Folder <- paste(diretorios$folderResultDataset, "/Partition-",id_part, sep="")
 setwd(Folder)
  
 ########################################################################################################################
 # save results in RDATA format
 str1a <- paste(dataset_name, "-Partition-", id_part, "-RunTimeFinal.rds", sep="")
 print(str1a)
 cat("\n Save RDATA")
 save(res, file = str1a)
  
 ########################################################################################################################
 # save resultsw in RDS format
 str2a = paste(dataset_name, "-Partition-", id_part, "-Results.rds", sep="")
 print(str2a)
 cat("\n Save RDS")
 save(res, file = str2a)
  
 ########################################################################################################################
 # compress files
 cat("\n Compress folders and files")
 str3a <- paste("tar -zcvf ", diretorios$folderResultDataset, "/", dataset_name, "-Partition-", id_part, "-results.tar.gz " , diretorios$folderResults, sep="")
 print(str3a)
 system(str3a)
  
########################################################################################################################
# cat("\n Copy to google drive")
# origem = paste(diretorios$folderResultDataset, "/", dataset_name, "-Partition-", id_part, "-results.tar.gz", sep="")
# destino = paste("cloud:elaine/[2021]ResultadosExperimentos/Oracle-Clus/Train-Val-Test/", dataset_name, sep="")
# comando = paste("rclone -v copy ", origem, " ", destino, sep="")
# cat("\n", comando, "\n") 
# a = print(system(comando))
# a = as.numeric(a)
# if(a != 0) {
# stop("Erro RCLONE")
# quit("yes")
# }
  
 ########################################################################################################################
 cat("\n Copy to Folder Reports")
 setwd(diretorios$folderReportsDataset)
 origem = paste(diretorios$folderResultDataset, "/", dataset_name, "-Partition-", id_part, "-results.tar.gz", sep="")
 comando1 = paste("cp -r ", origem, " ", diretorios$folderReportsDataset, sep="")
 print(system(comando1))
  
 #######################################################################################################################
 cat("\n delete files")
 comando2 = paste("rm -r ", Folder)
 system(comando2)
   
 ########################################################################################################################
 # count
 id_part = id_part + 1
 count = count + 1
  
 ########################################################################################################################
 cat("\n")
 gc()
 
}

cat("\n##################################################################################################")
cat("\nCompute Best Measures Partitions per Fold\n")
timeBMP = system.time(bestMeasuresPartitions(dataset_name, number_folds, folderResults))
cat("\n##################################################################################################")

cat("\n##################################################################################################")
cat("\nCompute Best and Worst partition\n")
timeBWP = system.time(bwPartition())
cat("\n##################################################################################################")

cat("\n##################################################################################################")
runtime = rbind(timeBMP, timeBWP)
setwd(diretorios$folderReportsDataset)
write.csv(runtime, "Other-Runtime.csv")
cat("\n##################################################################################################")

# cat("\n##################################################################################################")
# cat("\n Copy to google drive")
# origem = paste(diretorios$folderReportsDataset)
# destino = paste("cloud:elaine/[2021]ResultadosExperimentos/Oracle-Clus/Train-Val-Test/", dataset_name, "/Reports/", sep="")
# comando = paste("rclone -v copy ", origem, " ", destino, sep="")
# cat("\n", comando, "\n") 
# a = print(system(comando))
# a = as.numeric(a)
# if(a != 0) {
# stop("Erro RCLONE")
# quit("yes")
# }
# cat("\n##################################################################################################")

cat("\n##################################################################################################")
cat("\nDelelete folders\n")
system(paste("rm -r ", diretorios$folderResultDataset, sep=""))
system(paste("rm -r ", diretorios$folderReportsDataset, sep=""))
system(paste("rm -r ", diretorios$folderBellPart, sep=""))
system(paste("rm -r ", diretorios$folderDatasetFolds, sep=""))
cat("\n##################################################################################################")

gc()
cat("\n##################################################################################################")
cat("\n# END OF ORACLE PARTITIONS. Thanks God!                                                          #") 
cat("\n##################################################################################################")
cat("\n\n\n\n") 


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
