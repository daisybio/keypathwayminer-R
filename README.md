
Rkpm - R package of KeyPathwayMiner
============
The following package provides an easy way for using KeyPathwayMiner and visualize its results via R.
The user can decide whether he wants to call KeyPathwayMiner on his computer (via standalone) or with the Web api. 
With the kpm_options object the arguments for the execution of the program can be easily set and viewed.

Given a biological network and a set of case-control studies, KeyPathwayMiner efficiently extracts all maximal connected sub-networks.
These sub-networks contain the genes that are mainly dysregulated, e.g., differentially expressed, in most cases studied.

For more information please visit our website [(Key Pathway Miner website)](https://exbio.wzw.tum.de/keypathwayminer/).

Overview
=================
<!--ts-->
   * [Purpose](#purpose)
   * [For users](#for-users)
   * [Setup JVM](#setup-jvm)
      * [Linux](#linux)
      * [macOS](#macOs)
      * [Windows](#windows)
   * [Quickstart](#quickstart)
   * [Want to code on the package ?](#want-to-code-on-the-package)
<!--te-->

Purpose
=================
Our goal is to simplify the user's data analysis work process. Users can perform their analysis directly in R without having to worry about how to access Jar files or set up HTTP requests. At the same time, R provides an efficient way to handle and further process data. 

