# Oracle Partitions Version 2
This code is part of my doctoral research at PPG-CC/DC/UFSCar. It's oracle experimentation of Bell Partitions. All the bell partitions generated are tested using the CLUS framework, there is no validation of the partitions. The best partition is chosen after the process of all partitions. **IMPORTANT**: In this version we join the validation and train sets to build the model.
In this version we do not use the validation set together with the train set.

# Type of Oracle Partitions
<img src="https://github.com/cissagatto/Oracle-Clus/blob/main/images/tipos_particoes_oracle_transparente.png" width="300">

## Scripts
This source code consists of an R project for R Studio and the following R scripts:

1. libraries
2. utils
3. buildAndTestPartitions
4. BestWorstPartitions
5. evaluation
6. run
7. oracle

## Bell Partitions
<img src="https://github.com/cissagatto/Oracle-Clus/blob/main/images/Bell-Partitions-branco.png" width="300">


## FlowChart
<img src="https://github.com/cissagatto/Oracle-Clus/blob/main/images/Oracle-Mi-Branco.png" width="300">
<img src="https://github.com/cissagatto/Oracle-Clus/blob/main/images/Oracle-Ma-Branco.png" width="300">

## Folder Structure
<img src="https://github.com/cissagatto/Oracle-Clus/blob/main/images/Folder-Structure-Oracle.png" width="300">

## Preparing your experiment

### Step-1
This code is executed in X-fold cross-validation (mandatory!). First, you have to obtain the X-fold cross-validation files using this code: https://github.com/cissagatto/CrossValidationMultiLabel (all the instructions to use the code are in the Github). After that, put the results generated in the *folds* folder in this project. The folder structure generated by the code CrossValidation is used here.

<img src="https://github.com/cissagatto/Oracle-Clus/blob/main/images/10-Folds-Cross-Validation.png" width="300">

### Step-2
After obtained the X-Fold Cross-Validation and put in the correct folder, you need the Bell Partitions. The code to generated the Bell Partitions is available here https://github.com/cissagatto/BellPartitionsMultiLabel. Please, follow the instructions in GitHub. After generated the bell partitions, put them in the "BellPartitions" folder. The folder structure is maintained.

<img src="https://github.com/cissagatto/Oracle-Clus/blob/main/images/Bell-Partitions-Oracle.png" width="300">

### Step-3
Confirms if the folder UTILS contains the following files: Clus.jar, R_csv_2_arff.jar, and weka.jar. Without these jars, the code not runs. Also, confirms if the folder _libs_ is present with the jars: Clus.jar, commons-math-1.0.jar, jgap.jar and weka.jar.

### Step-4
Place a copy of this code in _"C:/Users/[username]/Oracle-Clus"_ or _"/home/[username]/Oracle-Clus"_. Our files are configured to obtain the paths of the folders from this path. You can change this in the code if you want.

### Step-5
A file called *datasets.csv* must be in the root project folder. This file is used to read information about the datasets and they are used in the code. All 74 datasets available in cometa (https://cometa.ujaen.es/) are in this file. If you want to use another dataset, please, add the following information about the dataset in the file:

*Id, Name, Domain, Labels, Instances, Attributes, Inputs, Labelsets, Single, Max freq, Card, Dens, MeanIR, Scumble, TCS, AttStart, AttEnd, LabelStart, LabelEnd, xn, yn, gridn*

The _"Id"_ of the dataset is a mandatory parameter in the command line to run all code. The fields are used in a lot of internal functions. Please, make sure that this information is available before running the code. *xn* and *yn* correspond to a dimension of the quadrangular map for kohonen, and *gridn* is *xn* * *yn*. Example: xn = 4, yn = 4, gridn = 16.

### Step-4
Confirms if the folder UTILS contains the following files: Clus.jar, R_csv_2_arff.jar, and weka.jar. Without these jars, the code not runs. Also, confirms if the folder _libs_ is present with the jars: Clus.jar, commons-math-1.0.jar, jgap.jar and weka.jar.

## Software Requirements
This code was develop in *RStudio Version 1.4.1106 © 2009-2021 RStudio, PBC "Tiger Daylily" (2389bc24, 2021-02-11) for Ubuntu Bionic Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36*. The R Language version was: *R version 4.1.0 (2021-05-18) -- "Camp Pontanezen" Copyright (C) 2021 The R Foundation for Statistical Computing Platform: x86_64-pc-linux-gnu (64-bit)*. 

**Please, make sure all the dependencies are installed (verify libraries.R). This code does not provide any installation of the packages.**

## Hardware Requirements
This code may or may not be executed in parallel, however, it is highly recommended that you run it in parallel. The number of cores can be configured via the command line (_number_cores_). If *number_cores = 1* the code will run sequentially. In our experiments, we used ten cores. For reproducibility, we recommend that you also use ten cores.

This code was tested with the birds dataset in the following machine:

System:

  Host: bionote | Kernel: 5.8.0-53-generic | x86_64 bits: 64 | Desktop: Gnome 3.36.7 | Distro: Ubuntu 20.04.2 LTS (Focal Fossa) 
  
CPU:

  Topology: 6-Core | model: Intel Core i7-10750H | bits: 64 | type: MT MCP | L2 cache: 12.0 MiB | Speed: 800 MHz | min/max: 800/5000 MHz 
  Core speeds (MHz): | 1: 800 | 2: 800 | 3: 800 | 4: 800 | 5: 800 | 6: 800 | 7: 800 | 8: 800 | 9: 800 | 10: 800 | 11: 800 | 12: 800 |
  
_Then the experiment was executed in a cluster at UFSCar._

# Run

To run the code, open the terminal, enter */home/[username]/Oracle-Clus/scripts/* folder, and type:

```
Rscript oracle.R [number_dataset] [number_cores] [number_folds] [name_folder_results]
```

_number_dataset_: dataset number according to file "datasets.csv" in the folder root

_number_cores_: cores to parallel

_number_folds_: number of folds to cross-validation

_name_folder_results_: name of folder to store the results


Example:

```
Rscript oracle.R 24 10 10 "/dev/shm/res"
```

This will compute all partitions for dataset Flags, using 10 cores and 10-folds for cross-validation. If you want, you can modify the "oracle.R" to execute only for one partition. You also can modify the code to upload the results into your google drive using rclone. 

## Results
The results are stored in the folder _REPORTS_. 

<img src="https://github.com/cissagatto/Oracle-Clus/blob/main/images/Reports-oracle-1.png" width="300">

## Acknowledgment
This study is financed in part by the Coordenação de Aperfeiçoamento de Pessoal de Nível Superior - Brasil (CAPES) - Finance Code 001

## Links

[Post-Graduate Program in Computer Science](http://ppgcc.dc.ufscar.br/pt-br)

[Biomal](http://www.biomal.ufscar.br/)

[Computer Department](https://site.dc.ufscar.br/)

[CAPES](https://www.gov.br/capes/pt-br)

[Embarcados](https://www.embarcados.com.br/author/cissa/)

[Linkedin](https://www.linkedin.com/in/elainececiliagatto/)

[Linkedin](https://www.linkedin.com/company/27241216)

[Instagram](https://www.instagram.com/professoracissa/)

[Facebook](https://www.facebook.com/ProfessoraCissa/)

[Twitter](https://twitter.com/professoracissa)

# Thanks
