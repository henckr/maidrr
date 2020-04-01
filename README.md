# The `maidrr` package <img src="inst/figures/maidrr_hex.png" alt="maidrr hex_logo" style="float:right;height:232.25px" align="right" height="232.25">

This is the source code for the  `maidrr` package, which is currently under development.

## Overview
The goal of `maidrr` is to aid you in the development of a Model-Agnostic Interpretable Data-driven suRRogate model for your black box algorithm of choice. In short, these are the steps in the procedure: 
1. Partial dependencies (PDs) are used to obtain model insights from the black box.
2. Those are used to group feature levels in an optimal data-driven way, with built-in feature selection.
3. A GLM surrogate is fit to the segmented features and meaningful interactions.
