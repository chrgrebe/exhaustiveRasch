# exhaustiveRasch 0.2.0
This release adds some new functionality.

### New feature
- **new parameter in main function: est**. Setting the parameter to "psychotools" will speed up model estimation by using the estimation functions of the package of the same name. Use est=eRm" for the old behaviour."


# exhaustiveRasch 0.1.8
This release adds some new (changed) functionality.

### New features
- **new function: add_ICs()**. Passing an object of class *passed_exRa* will add information criteria in the @IC slot for all models in the @passed_models slot.

### Bug fixes
- in some cases unordered thresholds were not correctly recognized when using *threshold_order* as one of the tests in *exhaustive_tests()*. This bug was fixed.
- several minor bug fixes

### Other changes
- in the *remove_subsets()* function, the default value for the *keep_longest* parameter was changed to FALSE.
- to reduce runtimes, the calculation of information criteria for the remaining models in *exhaustive_tests()* is no longer done by default. A new parameter *ICs* with FALSE as the default value was added to the function. Set this parameter TRUE for calculating the information criteria. If this parameter is set to FALSE, the data.frame in the @IC sot of the *passed_exRa* object will be empty. You can add the information criteria later with a call to the new function *add_ICs()*. The calculation of ICs was accelerated, as the new function *add_ICs()* now uses the *parallels* package.
- in *exhaustive_tests()* (and also in the new function *add_ICs*) the defalut value of the parameter *ignoreCores* was changed from 0 to 1.



***


# exhaustiveRasch 0.1.7
This release adds some new functionality.

### New features
- **new test available: test_respca**. This test runs a principal component analysis (PCA) on the residuals of the
  rasch model. The test will be passed, if the maximum eigenwert of a contrast in the pca is below the *max_contrast* parameter. You can call the test from the *tests* parameter of the *exhaustive_tests()* function. Provide the *max_contrast parameter* to *exhaustive_tests()* (if you do not want to use the default value of 1.5)
- **new test available: no_test**. This is not really a test, as no test will be conducted.  But rasch models (RM, PCM, RSM - depending on the modelType parameter) will be fit. You can use this "test" in the *tests* parameter of the *exhaustive_tests()* function, if you have a a set of item combinations and want to run several tests with varying test criteria. In this case you could first run one call to *exhaustive_tests()* to fit rasch models for all item combinations and then use the returned object of class *passed_exRa* for all tests to follow. This will speed up the following analyses, as the models has already been fit.

### Unit tests added
- unit tests were added in the testthat folder below the tests folder.

### Bug fixes
- the bug that led to no item combination passing the *all_rawscores* test when using it with polytomous data (*modelType* PCM or RSM) has been fixed
- several minor bug fixes

### Other changes
- all code was re-formatted to not extend 80 characters in width.
