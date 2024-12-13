---
output:
  html_document: default
  pdf_document: default
---
# exhaustiveRasch 0.3.6
This release adds verbose argument to exhaustive_tests() and fixes some documentation and code issues.

### Bug fixes
fixed some documentation and code issues not related to bugs or functionality

### New features
**new argument in main function exhaustive_tests(): verbose**. set verbose=TRUE to suppress output to the console during the analysis. Note, there will also be no progress bar, if verbose=TRUE. The default value for verbose is verbose=FALSE.

# exhaustiveRasch 0.3.5
This release fixes some minor errors in the documentation.

# exhaustiveRasch 0.3.4
This release fixes some minor errors in the documentation.

# exhaustiveRasch 0.3.3
This release fixes several bugs and adds some more documentation.

### Bug fixes
fixed bug that ppar.psy could not be used outside of the package with standard
objects created by psychotools (because of the missing "thresholds" list entry)

# exhaustiveRasch 0.3.2
This release fixes several bugs and changes minor parts of the handling.

### changed features and usability
the parameters sum0 and se were removed from estimation_control(). From this version on, item parameters are always estimated summed to zero, and standard errors are always computed.
The parameters resp.cat and use.thurst for pairwise estimation have moved from pairwise_control() to estimation_control(). The function pairwise_control() was removed.

### Bug fixes
fixed error messages with RSM-models estimated with eRm (issue in context of calculating hessians). Affected item combinations are considered as not passing the test.
When estimating with psychotools, no models passed test_personsItems. This bug was fixed. The bug, that waldtests with PCM models resulted in 0 passed models was also fixed.

### Contributors
as code from the packages pairwise and eRm was used (cited in the respective functions), the authors and contributors of these packages were added as contributors in the DESCRIPTION file.

# exhaustiveRasch 0.3.1
This release adds some new (changed) functionality.

### New features
This version implements parameter calculation (not estimation) using the pairwise approach and the respective model tests provided by the pairwise package.
Parameters resp.cat and use.thurst for pairwise estimation can be set using the new function pairwise_control().
Person parameters are no longer calculated for each test, but are passed from test to test and are part (as an additional slot of the S4-class) of the passed_exRa object, if a test relying on person parameters is part of the test circuit.
When using psychotools for estimation, the results are no longer converted to an eRm class, and functions of eRm are no longer used in this case. There are new internal functions for model tests based on psychotools estimations since this version.

# exhaustiveRasch 0.2.2
This release fixes two bugs.

### Bug fixes
Accidently, the add_ICs() function was ignored when compiling the package, so it was not available. This bug was fixed.
The C- and FORTRAN- code from eRm (originally from the sna package) is now integrated into the package. Previously it was imported from eRm. This was changed to to CRAN's restrictions that (of course!) does not allow importing compiled code from other packages.

# exhaustiveRasch 0.2.1
This release adds some new functionality.

### New features
- **new argument in main function: estimation_param**. The est argument, that was introduced in version 0.2.0, has now moved into this new parameter. This list type argument also offers arguments for the calculation of standard errors for the item parameters (argument se**) and, if opting for parameter estimation using eRm function, the sum0 argument known from eRm. Default values for estimation_param can be set by using the function call estimation_control() and can be overriden by providing one or more of its arguments. As with the arguments of itemfit_control(), the arguments of estimation_control() can alternatively be given directly as optional arguments of exhaustive_tests().
- **new test: test_PSI** a new test, that can be used in the tests- argument of exhaustive_tests. The tests checks, if the Person Seperation Index (also known als Person Seperation Reliability) is at least equal than the value provided in the argument PSI.

### improved performance
If using "psychotools" as the value for the est argument, this will now also affect calculation times for test_mloef, test_waldtest and test_LR. You can expect shorter runtimes by at least the factor 2-3.

### new dataset
an additional dataset "cognition" was added. This datasets provides polytomous data of the FACT-cog subscale "perceived cognitive impairment" with 5 item categories. See the package description for information on item labels and answer categories.

### Bug fixes
several bugs that lead to error messages were fixed.


# exhaustiveRasch 0.2.0
This release adds some new functionality.

### New feature
- **new argument in main function: est**. Setting the argument to "psychotools" will speed up model estimation by using the estimation functions of the package of the same name. Use est=eRm" for the old behaviour."


# exhaustiveRasch 0.1.8
This release adds some new (changed) functionality.

### New features
- **new function: add_ICs()**. Passing an object of class *passed_exRa* will add information criteria in the @IC slot for all models in the @passed_models slot.

### Bug fixes
- in some cases unordered thresholds were not correctly recognized when using *threshold_order* as one of the tests in *exhaustive_tests()*. This bug was fixed.
- several minor bug fixes

### Other changes
- in the *remove_subsets()* function, the default value for the *keep_longest* argument was changed to FALSE.
- to reduce runtimes, the calculation of information criteria for the remaining models in *exhaustive_tests()* is no longer done by default. A new argument *ICs* with FALSE as the default value was added to the function. Set this argument TRUE for calculating the information criteria. If this argument is set to FALSE, the data.frame in the @IC sot of the *passed_exRa* object will be empty. You can add the information criteria later with a call to the new function *add_ICs()*. The calculation of ICs was accelerated, as the new function *add_ICs()* now uses the *parallels* package.
- in *exhaustive_tests()* (and also in the new function *add_ICs*) the defalut value of the argument *ignoreCores* was changed from 0 to 1.



***


# exhaustiveRasch 0.1.7
This release adds some new functionality.

### New features
- **new test available: test_respca**. This test runs a principal component analysis (PCA) on the residuals of the
  rasch model. The test will be passed, if the maximum eigenwert of a contrast in the pca is below the *max_contrast* argument. You can call the test from the *tests* argument of the *exhaustive_tests()* function. Provide the *max_contrast argument* to *exhaustive_tests()* (if you do not want to use the default value of 1.5)
- **new test available: no_test**. This is not really a test, as no test will be conducted.  But rasch models (RM, PCM, RSM - depending on the modelType argument) will be fit. You can use this "test" in the *tests* argument of the *exhaustive_tests()* function, if you have a a set of item combinations and want to run several tests with varying test criteria. In this case you could first run one call to *exhaustive_tests()* to fit rasch models for all item combinations and then use the returned object of class *passed_exRa* for all tests to follow. This will speed up the following analyses, as the models has already been fit.

### Unit tests added
- unit tests were added in the testthat folder below the tests folder.

### Bug fixes
- the bug that led to no item combination passing the *all_rawscores* test when using it with polytomous data (*modelType* PCM or RSM) has been fixed
- several minor bug fixes

### Other changes
- all code was re-formatted to not extend 80 characters in width.
