# Spatial Analysis: An Applied Introduction

This site maintains replication materials for the book, _Spatial Analysis: An Applied Introduction_ (under contract, Cambridge University Press; full manuscript submitted Sep. 1, 2026).

The materials are organized according to the empirical work being replicated, and will be maintained and updated on an ongoing basis.

## Example 1

The empirical example running throughout the book is from Brass et al. (2020). The study region is Ghana, the outcome of interest is the number of solar panels, and explanatory factors include a wide range of socio-economic, political, demographic, and other variables. The units of analysis are districts in Ghana. The article is a great example to use with audiences interested in development and democracy, green energy, or West Africa, as well as research methods. This example is excellent for learning spatial analysis because the data lend themselves to testing all spatial effects covered in the book. Further, from a practical perspective, the number of observations (N=170) is in a kind of ``sweet spot'' for learning spatial analysis. With an N like this, the different spatial effects can be examined in depth, and there are no long computational delays while running diagnostics or estimated models. For instance, every technique can be implemented in a classroom or workshop environment without extended delays for computationally-intensive steps. 

Among the practical examples included here, this example has the most complete set of materials reflecting every aspect of materials covered in the book. 

### Virtual Containers: Links to Binder

This binder uses conda to install R packages. This is done via the environment.yml file. Some packages may not be available that might otherwise be available using other methods, e.g., install.R and runtime.txt files.

Click on one of the images below to open binder online:

Jupyter + R:  [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/mattcingram/spatial-analysis/HEAD)

Jupyter + RStudio:  [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/mattcingram/spatial-analysis/HEAD?urlpath=rstudio)


## Additional Examples

Additional empirical examples include studies from Kehane (2023) and Ingram and Marchesini da Costa (2019).

The replication of Kehane takes the original, non-spatial study and shows what different kinds of spatial analyses would look like. The outcome of interest is mask-wearing behavior in the U.S., a wide range of socio-economic, political, and demographic variables are included, and the units of analysis are U.S. counties. There is one county with missing data, so the exercise also illustrates techniques for imputing missing data in spatial work. The example should appeal to audiences in public health, political science, and research methods. There are 3,100 observations, so some steps are more computationally intensive and take more time.

The replication of Ingram and Marchesini da Costa takes the original article, which included a series of OLS and GWR models, and adds other spatial models and MGWR specifications. The outcome of interest is lethal violence (homicide rates), a wide range of socio-economic, political, and demographic variables are included, and the units of analysis are municipalities in Brazil. There are 5,564 observations, so there is a further increase in delays due to computational demands. Some steps with this study can take several hours even on a large-memory machine.
