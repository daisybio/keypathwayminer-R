
KeyPathwayMineR - R package 
============
Given a **biological network** and a **set of case-control studies**, KeyPathwayMiner(KPM) efficiently extracts all             **maximal connected sub-networks**. These sub-networks contain the genes that are **mainly** dysregulated, e.g.,           differentially expressed, in **most** cases studied:

* The exact quantities for “mainly” and “most” are modeled with two easy-to-interpret parameters **(K, L)** that allows      the user to control the number of outliers (not dysregulated genes/cases) in the solutions.
     
Two different approaches for extracting subnetworks that are enriched for active/deregulated genes have been               implemented:

  * **INES:**  Extract all maximal sub-networks containing nodes with no more than L inactive cases (0's) besides of K exceptions.
      
  * **GLONE:** Extracts maximal sub-networks where the total sum of **not**-active/diff. exp. cases is at most L. 
      
For more information please visit our website [(Key Pathway Miner website)](https://exbio.wzw.tum.de/keypathwayminer/).

Overview
=================
<!--ts-->
   * [Purpose](#purpose)
   * [Prerequisites](#prerequisites)
      * [Common problems](#common-problems)
   * [Get started](#get-started)
      * [Users](#user)
      * [Developers](#developer)
   

<!--te-->

Purpose
=================
The following package provides an easy way for using KPM and visualize the extracted subnetworks via R. The user can decide whether he wants to call KPM on his computer (via standalone) or with the Web API. With the kpm_options() object the arguments for the execution of the program can be easily processed and viewed.

Prerequisites
=================
KeyPathwayMineR uses the rJava library in order to execute the standalone local version of KPM. The rJava package allows to access and execute required functions from the jar file. In order to use this functionality it must be ensured that the user is using version **1.8** of Java. 

When the package is loaded, it is checked which java version the user is currently using. If the version is not correct a warning message is displayed.

A helpful guide for getting R to use the correct Java version can be found [here](https://github.com/Utah-Data-Science/Home_repo/wiki/Getting-R-to-use-the-correct-Java-version).

Common problems
-----
* Java is not installed. Browse the web for instructions specific for your operating system to install Java.

* rJava is not installed. Fix by install.packages("rJava")

* Java is not properly configured. Under linux this can often be fixed by sudo update-alternatives --config java. Another option is to set the environment variable JAVA_HOME correctly.

* rJava is not properly configured. This can often be fixed by sudo R CMD javareconf

Get started
=================
Once your R client has been successfully configured with Java 8 you can start.

Users
-----
KeyPathwayMiner is currently available on github and can be installed through the devtools R package:

      # Install devtools package   
      install.packages("devtools")

      # Load and attach devtools package
      library(devtools)

      # Install KeyPathwayMineR from github and build vignettes
      install_github("baumbachlab/keypathwayminer-R", build_vignettes = TRUE)

      # Load and attach KeyPathwayMineR 
      library(KeyPathwayMineR)

If the initialization was successful you will get following output:
      
      #> Loading required package: rJava
      #> Standalone jar added to class path:  TRUE
      #> Utils: The Java virtual machine is available and has the correct version.
      #> KeyPathwayMineR ready for local and remote execution.
      
When everything is set up you can get started with the following vignettes:

      vignette("KeyPathwayMineR")
      vignette("input_files_format")

Developers
-----
If you want to play around or work on the code you can clone the repo:

      git clone https://github.com/baumbachlab/keypathwayminer-R.git

Opent the *KeyPathwayMineR.Rproj* file to open the package. To get started install the following libraries:

      install.packages(c("devtools", "roxygen2", "testthat", "knitr"))

