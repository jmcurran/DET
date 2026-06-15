# DET 3.0.3

* New files 
  - show.R (the current does not work)
  - measures.R (we have added objects in the output of minDcf function)
  - plot.R (we have corrected some things about the graphs of the curves with CI, marks of axises, and improved the zoom options for both DET and ROC curve)
  - detc.R (we have corrected the names of the positive and negative labels)
  
* Small code modernisation and polish

# DET 3.0.2 

* Minor changes to package to get DET back on CRAN.

# DET 3.0.0

* Adding class objects to model the DET curves

* Override standard `plot()` function when called with a *DET* or *DETs* object

* Renaming function `printDET()` to `show()`. 

* New functions to compute performance measures: `EER()` and `mindDcf()`.

* New functions to cplot the correspondent ROC curve from a DET curve: `plotROC` and `plotROCs`.


# DET 2.0.1

* `printDET()` shows the names of the classifiers to access de DET object.

* `detc()` and `detc.CI()` do not plot the EER. How to to that is described at the example

* `speaker` and `ovarianCancer` databases are added for complementing real examples of using DET curves.



# DET 2.0.0

* New function `printDET()` for describing the DET returned parameters.

* `det()` and `det.CI()` are renamed as `detc()` and `detc.CI()` to avoid overriding.

* New `detc()` and `detc.CI()` functions now provide an estimation for the Equal Error Rate (new field at the returned dataframe)


# DET 1.0.1

* New aspect of ROC Curve plot.

* `det()` and `det.CI()` now inlcude *xlim* and *ylim* arguments to select the axes limit of the plot.


