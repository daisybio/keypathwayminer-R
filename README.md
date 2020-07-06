
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
   * [Setup](#setup-jvm)
      * [Linux](#linux)
      * [macOS](#macOs)
      * [Windows](#windows)
   * [Get started](#get-started)
      * [Users](#user)
      * [Developers](#developer)
   

<!--te-->

Purpose
=================
The following package provides an easy way for using KPM and visualize the extracted subnetworks via R. The user can decide whether he wants to call KPM on his computer (via standalone) or with the Web API. With the kpm_options() object the arguments for the execution of the program can be easily processed and viewed.

Get started
=================
When your R client has been successfully configured with Java 8 you can start.

User
-----
KeyPathwayMiner is currently available on github and can be conveniently installed through the devtools R package:
         
      library(devtools)
      install_github("baumbachlab/RJAMI")






Developer
-----
