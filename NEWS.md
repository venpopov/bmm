# bmm 0.2.1

### Bug Fixes



# bmm 0.2.0

### New features

* New model available - The Signal Discrimination Model by Oberauer (2023) for visual working memory continuous reproduction tasks. See ?sdmSimple. The current version does not take into account non-target activation
* Add ability to extract information about the default priors in `bmm` models with `get_model_prior()` (#53)
* Add ability to generate stan code and stan data for each model with `get_model_stancode()` and `get_model_standata()` (#81)
* BREAKING CHANGE: Add distribution functions for likelihood (e.g. `dimm()`) and random variate generation `rimm()`) for all models in the package. Remove deprecated `gen_3p_data()` and `gen_imm_data()` functions (#69)
* Two new datasets available: `ZhangLuck_2008` and `OberauerLin_2017` (#22)

### Documentation

* Website for the development version of the package is now available at https://venpopov.github.io/bmm/dev/ (#18)
* Add vignettes for each model to the website at https://venpopov.github.io/bmm/dev/articles/
* Add a detailed developer's guide to the website at https://venpopov.github.io/bmm/dev/dev-notes (#21)
* Improve README with more detailed information about the package's goals and its models (#21)

### Other changes

* Save `bmm` package version in the `brmsfit` object for reproducibility - e.g. `fit$version$bmm` (#88)


# bmm 0.1.1

### New features

* BREAKING CHANGE: Improve user interface to fit_model() ensures package stability and future development. Model specific arguments are now passed to the model functions as named arguments (e.g. `mixture3p(non_targets, setsize)`). This allows for a more flexible and intuitive way to specify model arguments. Passing model specific arguments directly to the `fit_model()` function is now deprecated (#43).
* Add information about each model such as domain, task, name, version, citation, requirements and parameters (#42)
* Add ability to generate a template file for adding new models to the package with `use_model_template()` (for developers) (#39)

### Other changes

* Improve documentation of model functions. You can now get help on each model by typing `?model_name` into your console. For example, calling the information on the full version of the Interference Measurement Model would look like this: `?IMMfull`


# bmm 0.1.0

A major restructuring of the package to support stable and generalizable development of future models (#41). 

### New Features

* Refactor the `fit_model()` function to be generic and independent of the model being fit (#20)
* Transform models to be S3 objects. (#41). 
* View currently supported models with new function `supported_models()`. Currently supported models are: `mixture2p()`, `mixture3p()`, `IMMabc()`, `IMMbsc()`, `IMMfull()` 
* Add S3 methods for checking the data, formula, model and priors (#41)
* Add distribution functions for the Signal Discrimination Model. See `?SDM` for usage (#27)
* Add softmax and invsoftmax functions 

### Bug Fixes

* Change default prior on log(kappa) to Normal(2,1) for the `mixture3p()` model (#15)

### Other changes

* BREAKING CHANGE: deprecate `model_type` argument in `fit_model()`. Models must now be specified with S3 functions passed to argument `model` rather than model names as strings passed to argument `model_type` (#41)
* Add extensive unit testing


# bmm 0.0.1

* Initial release version
