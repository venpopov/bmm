# bmm 0.3.0+

### New features

* add a check for the sdmSimple model if the data is sorted by predictors. This leads to much faster sampling. The user can control the default behavior with the `sort_data` argument (#72)
* the mixture3p and imm models now require that when set size is used as a predictor, the intercept must be suppressed. This is because set size 1 otherwise causes problems - there can be no contribution of non_target responses when there is set size 1, and it is not meaningful to estimate an intercept for parameters that involve non_target responses (#96).
* add postprocessing methods for sdmSimple to allow for pp_check(), conditional_effects and bridgesampling usage with the model (#30)
* add informed default priors for all models. You can always use the `get_model_prior()` function to see the default priors for a model
* add a new function `set_default_prior` for developers, which allows them to more easily set default priors on new models regardless of the user-specified formula
* you can now specify variables for models via regular expressions rather than character vectors [#102]

### Bug fixes
* fix a bug in the mixture3p and IMM models which caused an error when intercept was not supressed and set size was used as predictor
* fix a bug in the sort_data check which caused an error when using grouped covariance structure in random effects across different parameters



# bmm 0.3.0

### New features

* BREAKING CHANGE: The `fit_model` function now requires a `bmmformula` to be passed. The  syntax of the `bmmformula` or its short form `bmf` is equal to specifying a `brmsformula`. However, as of this version the `bmmformula` only specifies how parameters of a `bmmmodel` change across experimental conditions or continuous predictors. The response variables that the model is fit to now have to be specified when the model is defined using `model = bmmmodel()`. (#79)
* BREAKING CHANGE: The `non_target` and `spaPos` variables for the `mixture3p` and `IMM` models were relabled to `nt_features` and `nt_distances` for consistency. This is also to communicate that distance is not limited to spatial distance but distances on any feature dimensions of the retrieval cues. Currently, still only a single generalization gradient for the cue features is possible. 
* This release includes reference fits for all implemented models to ensure that future changes to the package do not compromise the included models and change the results that their implementations produce.
* The `check_formula` methods have been adapted to match the new `bmmformula` syntax. It now evaluates if formulas have been specified using the `bmmformula` function, if formulas for all parameters of a `bmmmodel` have been specified and warns the user that only a fixed intercept will be estimated if no formula for one of the parameters was provided. Additionally, `check_formula` throws an error should formulas be provided that do not match a parameter of the called `bmmmodel` unless they are part of a non-linear transformation.
* You can now specify formulas for internally fixed parameters such as `mu` in all visual working memory models. This allows you to predict if there is response bias in the data. If a formula is not provided for `mu`, the model will assume that the mean of the response distribution is fixed to zero.
* there is now an option `bmm.silent` that allows to suppress messages
* the baseline activation `b` was removed from the `IMM` models, as this is internally fixed 
to zero for scaling and as of now cannot be predicted by independent variables because the model would be unidentifiable.
* the arguments used to fit the bmm model are now accessible in the `bmmfit` object via the `fit$bmm_fit_args` list.
* add class('bmmfit') to the object returned from fit_model() allowing for more flexible postprocessing of the underlying `brmsfit` object. The object is now of class('bmmfit', 'brmsfit')
* changes to column names of datasets `ZhangLuck_2008` and `OberauerLin_2017` to make them more consistent

### Bug Fixes
* an error with the treatment of distances in the `IMMfull` and the `IMMbsc` has been corrected. This versions ensures that only positive distances can be passed to any of the two models.
* removed a warning regarding the scaling of the distances in the `IMMfull` and the `IMMbsc` that was specific only for circular distances.

### Documentation
* All vignettes have been update to the new `bmmformula` syntax.


# bmm 0.2.2

### Bug Fixes
* fixed a bug where passing a character vector or negative values to setsize argument of visual working memory models caused an error or incorrect behavior (#97)

# bmm 0.2.1

### Bug Fixes
* Minor change to sdmSimple stan helper functions to avoid a harmless warning message in the stan output


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
