###############################################################################
# Oracle Partitions with CLUS                                                 #
# Copyright (C) 2022                                                          #
#                                                                             #
# This code is free software: you can redistribute it and/or modify it under  #
# the terms of the GNU General Public License as published by the Free        #
# Software Foundation, either version 3 of the License, or (at your option)   #
# any later version. This code is distributed in the hope that it will be     #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of      #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General    #
# Public License for more details.                                            #
#                                                                             #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin  #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) |        #
# Campus Sao Carlos | Computer Department (DC: https://site.dc.ufscar.br/)    #
# Program of Post Graduation in Computer Science                              #
# (PPG-CC: http://ppgcc.dc.ufscar.br/) | Bioinformatics and Machine Learning  #
# Group (BIOMAL: http://www.biomal.ufscar.br/)                                #
###############################################################################



###############################################################################
# SET WORKSAPCE                                                               #
###############################################################################
FolderRoot = "~/Oracle-Clus"
FolderScripts = paste(FolderRoot, "/R", sep="")


############################################################
#                                                                               
############################################################
partition <- function(id_part,
                      ds,
                      dataset_name,
                      number_dataset,
                      number_cores,
                      number_folds,
                      folderResults,
                      namesLabels){
  
  diretorios = directories(dataset_name, folderResults)
  info <- infoPartitions(id_part, dataset_name, folderResults)
  # criando a pasta específica da partição
  FolderPartition = paste(diretorios$folderTest, "/Partition-",
                          id_part, sep="")
  if(dir.exists(FolderPartition)==FALSE){dir.create(FolderPartition)}
  
  
  f = 1
  buildBellPartitions <- foreach(f = 1:number_folds) %dopar% {
    
    cat("\n\nFold: ", f)
    
    #####################################################################
    FolderRoot = "~/Oracle-Clus"
    FolderScripts = paste(FolderRoot, "/R", sep="")
    
    #####################################################################
    # LOAD LIBRARIES
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    #####################################################################
    diretorios = directories(dataset_name, folderResults)
    
    #####################################################################
    FolderSplit = paste(FolderPartition, "/Split-", f, sep="")
    if(dir.exists(FolderSplit)==FALSE){dir.create(FolderSplit)}
    
    # get bell partition information
    info = infoPartitions(id_part, dataset_name, folderResults)
    
    #################################################################
    converteArff <- function(arg1, arg2, arg3, FolderUtils){  
      str = paste("java -jar ", diretorios$folderUtils, 
                  "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", 
                  arg3, sep="")
      print(system(str))
      cat("\n\n")  
    }
    
    # get bell partition information
    info = infoPartitions(id_part, dataset_name, folderResults)
    
    g = 1
    while(g<=info$numberGroupsOfPartition){
  
      #############################################################
      nome_particao = paste("partition_", info$numberOfPartition, sep="")
      nome_grupo = paste("group_", g, sep="")
      cat("\n\tPartition: ", nome_particao)
      cat("\n\tGroup: ", nome_grupo)

      
      ##################################################################
      nomeTR = paste(dataset_name, "-Split-Tr-", f, ".csv", sep="")
      nomeTS = paste(dataset_name, "-Split-Ts-", f, ".csv", sep="")
      nomeVL = paste(dataset_name, "-Split-Vl-", f, ".csv", sep="")

      
      #################################################################
      nome_grupo_2 = paste("Group-", g, sep="")
      FolderGroup = paste(FolderSplit , "/", nome_grupo_2, sep="")
      if(dir.exists(FolderGroup)==FALSE){
        dir.create(FolderGroup)  
      } 

      
      ################################################################
      # get the labels of this group
      specificPartition = info$specificPartition
      specificGroup = specificPartition %>% filter(., specificPartition$group == g)

      
      ######################################################################
      # total de rótulos neste grupo
      totalLabelsThisGr = nrow(specificGroup)

      
      ######################################################################
      cat("\n\t\tTRAIN: Creating File\n")
      setwd(diretorios$folderCVTR)
      nomeTr2 = paste(diretorios$folderCVTR, "/", nomeTR, sep="")
      arquivoTR = data.frame(read.csv(nomeTr2))
      atributosTR = arquivoTR[ds$AttStart:ds$AttEnd]
      classesTR = select(arquivoTR, specificGroup$labels)
      thisGroupTR = cbind(atributosTR, classesTR)      

      
      #######################################################################
      cat("\n\t\tVALIDATION: Creating File\n")
      setwd(diretorios$folderCVVL)
      nomeVl2 = paste(diretorios$folderCVVL, "/", nomeVL, sep="")
      arquivoVL = data.frame(read.csv(nomeVl2))
      atributosVL = arquivoVL[ds$AttStart:ds$AttEnd]
      classesVL = select(arquivoVL, specificGroup$labels)
      thisGroupVL = cbind(atributosVL, classesVL)         
      

      #####################################################################
      cat("\n\t\tJOIN VALIDATION WITH TRAIN\n")
      treino2 = rbind(thisGroupTR, thisGroupVL)
      nrow(treino2)

      
      ######################################################################
      # TRAIN: Save CSV
      nomeCsTr = paste(FolderGroup, "/grupo_Tr_", g, ".csv", sep="")
      nomeArTr = paste(FolderGroup, "/grupo_Tr_", g, ".arff", sep="")
      setwd(FolderGroup)
      write.csv(treino2, nomeCsTr, row.names = FALSE)

      
      #######################################################################
      # Targets
      inicio = ds$LabelStart
      fim = ncol(treino2)
      ifr = data.frame(inicio, fim)
      setwd(FolderGroup)
      write.csv(ifr, "inicioFimRotulos.csv", row.names = FALSE)

      
      ########################################################################
      # TRAIN: Convert CSV to ARFF
      setwd(FolderGroup)
      arg1Tr = nomeCsTr
      arg2Tr = nomeArTr
      arg3Tr = paste(inicio, "-", fim, sep="")
      converteArff(arg1Tr, arg2Tr, arg3Tr, diretorios$folderUtils)

      
      #########################################################################
      str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", nomeArTr, sep="")
      print(system(str0))
      
      
      #########################################################################
      cat("\n\t\tTEST: Creating File\n")
      setwd(diretorios$folderCVTS)
      nomeTs2 = paste(diretorios$folderCVTS, "/", nomeTS, sep="")
      arquivoTS = data.frame(read.csv(nomeTs2))
      atributosTS = arquivoTS[ds$AttStart:ds$AttEnd]
      classesTS = select(arquivoTS, specificGroup$labels)
      thisGroupTS = cbind(atributosTS, classesTS)

      
      ########################################################################
      #TEST: Save CSV
      nomeCsTs = paste(FolderGroup, "/grupo_Ts_", g, ".csv", sep="")
      nomeArTs = paste(FolderGroup, "/grupo_Ts_", g, ".arff", sep="")
      setwd(FolderGroup)
      write.csv(thisGroupTS, nomeCsTs, row.names = FALSE)

      
      #######################################################################
      # TEST: Convert CSV to ARFF
      setwd(FolderGroup)
      arg1Ts = nomeCsTs
      arg2Ts = nomeArTs
      arg3Ts = paste(inicio, "-", fim, sep="")
      converteArff(arg1Ts, arg2Ts, arg3Ts, diretorios$folderUtils)

      
      ########################################################################
      str1 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", nomeArTs, sep="")
      print(system(str1))

      
      ########################################################################
      # Creating .s file for Clus
      setwd(FolderGroup)
      
      nome_config = paste("grupo_", g, ".s", sep="")
      sink(nome_config, type = "output")
      
      cat("[General]")          
      cat("\nCompatibility = MLJ08")
      
      cat("\n")
      cat("\n[Data]")
      
      nome_arquivo_2 = paste("grupo_Tr_", g, ".arff", sep="")
      cat(paste("\nFile = ", nome_arquivo_2, sep=""))
      
      nome_arquivo_3 = paste("grupo_Ts_", g, ".arff", sep="")
      cat(paste("\nTestSet = ", nome_arquivo_3, sep=""))
      
      cat("\n")
      cat("\n[Attributes]")
      cat("\nReduceMemoryNominalAttrs = yes")
      
      cat("\n")
      cat("\n[Attributes]")
      cat(paste("\nTarget = ", inicio, "-", fim, sep=""))
      cat("\nWeights = 1")
      
      cat("\n")
      cat("\n[Tree]")
      cat("\nHeuristic = VarianceReduction")
      cat("\nFTest = [0.001,0.005,0.01,0.05,0.1,0.125]")
      
      cat("\n")
      cat("\n[Model]")
      cat("\nMinimalWeight = 5.0")
      
      cat("\n")
      cat("\n[Output]")
      cat("\nWritePredictions = {Test}")
      cat("\n")
      sink()

      
      ##########################################################
      # Execute CLUS
      nome_config2 = paste(FolderGroup, "/", nome_config, sep="")
      setwd(FolderGroup)      
      str = paste("java -jar ", diretorios$folderUtils, "/Clus.jar ", nome_config2, sep="")
      print(system(str))

      
      ############################################################
      # Open inicioFimRotulos.csv
      targets = data.frame(read.csv("inicioFimRotulos.csv"))

      
      ##########################################################
      # Open predictions
      setwd(FolderGroup)
      namae2 = paste(FolderGroup, "/grupo_", g, ".test.pred.arff", sep="")
      predicoes = data.frame(foreign::read.arff(namae2))

      
      ##########################################################
      # Split Y True and Y Predicts
      
      if(targets$inicio == targets$fim){
        
        library("foreign")
        cat("\n\t\t\tOnly one label in this group\n")
        
        ##########################################################
        # Save Y_True
        setwd(FolderGroup)
        classes = data.frame(predicoes[,1])
        names(classes) = colnames(predicoes)[1]
        write.csv(classes, "y_true.csv", row.names = FALSE)
        
        ########################################################
        # Save Y_Predict
        rot = paste("Pruned.p.", colnames(predicoes)[1], sep="")
        pred = data.frame(predicoes[,rot])
        names(pred) = colnames(predicoes)[1]
        setwd(FolderGroup)
        write.csv(pred, "y_predict.csv", row.names = FALSE)
        
        ########################################################
        rotulos = c(colnames(classes))
        n_r = length(rotulos)
        
        gc()
        
      } else {
        
        library("foreign")
        
        ##########################################################
        # More than one label in this group
        comeco = 1+(targets$fim - targets$inicio)
        
        ##########################################################
        # Save Y_true
        classes = data.frame(predicoes[,1:comeco])
        setwd(FolderGroup)
        write.csv(classes, "y_true.csv", row.names = FALSE)
        
        ##########################################################
        # Save Y_Predict
        rotulos = c(colnames(classes))
        n_r = length(rotulos)
        nomeColuna = c()
        t = 1 
        while(t <= n_r){
          nomeColuna[t] = paste("Pruned.p.", rotulos[t], sep="")
          t = t + 1
          gc()
        }
        pred = data.frame(predicoes[nomeColuna])
        names(pred) = rotulos
        setwd(FolderGroup)
        write.csv(pred, "y_predict.csv", row.names = FALSE)
        
        gc()
      } # END ELSE
      
      ########################################################
      nome1 = paste("grupo_Tr_", g, ".arff", sep="")
      nome2 = paste("grupo_Ts_", g, ".arff", sep="")
      nome3 = paste("grupo_Tr_", g, ".csv", sep="")
      nome4 = paste("grupo_Ts_", g, ".csv", sep="")
      nome5 = paste("grupo_", g, ".model", sep="")
      nome6 = paste("grupo_", g, ".s", sep="")
      
      setwd(FolderGroup)
      unlink(nome1, recursive = TRUE)
      unlink(nome2, recursive = TRUE)
      unlink(nome3, recursive = TRUE)
      unlink(nome4, recursive = TRUE)
      unlink(nome5, recursive = TRUE)
      unlink(nome6, recursive = TRUE)
      
      #####################################################
      # count
      g = g + 1
      
      gc()
    } # END GROUP
    
    gc()
  }
  
  gc()
  cat("\n#######################################################")
  cat("\n# Build and Test Partitions: END                      #") 
  cat("\n#######################################################")
  cat("\n\n\n\n")
}


#################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com   
# Thank you very much!                                           
#################################################################