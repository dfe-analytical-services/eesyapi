# Contributing to eesyapi

Try and make use of the [usethis](https://usethis.r-lib.org/) package wherever possible.

When you initially clone the package, the first thing you'll need to do is install 
[devtools](https://devtools.r-lib.org/):

```
install.packages("devtools")
```

Then to load in the package in its current form:

```
devtools::load_all()
```

## Documenting

Please always document as you develop. Always add a description to functions, alongside working
examples (see [the section on `example_id()`](#using-example_id()-with-examples-and-tests) for 
generating functional IDs and codes). For extended processes / workflows, we recommend starting
a vignette explaining the workflow as you're creating the functions underpinning it.

## Adding a package/dependency

`usethis::use_package(<package_name>)`

Note that when adding a function from another package into one of the eesyapi functions you will need to 
explicitly state the package in the function call, e.g.:

```package::function()```

Alternatively, if there's a lot of uses of a single function within one of our R scripts, you can call that 
function once at the top of the R script, e.g:

```
@' importFrom package function
```

For more information see the [roxygen2 documentation on declaring dependencies](https://roxygen2.r-lib.org/articles/namespace.html).

## Creating a new function script

`usethis::use_r(name = <script_name>)`

This will create a new script within the package [R/](R/) folder.

## Creating a new function test script

`usethis::use_test(name = <script_name>)`

This will create a new blank test script within the package [testthat/](testthat/) folder.

## Using `example_id()` with examples and tests

As the functionality of eesyapi is heavily tied to the EES API content and a variety of associated IDs and codes,
we have created the function `example_id()` to create a go to reference point for example IDs and codes that 
work with the API. `example_id()` should be used to generate IDs and codes whenever writing function tests or 
examples that require a connection to the API. For example in `api_url()`, we give the functioning example:

`api_url("get-data-catalogue", publication_id = eesyapi::example_id("publication"))`

which connects to an actual publication on EES and retrieves a list of data sets within that publication.

By using `example_id()`, we can easily update all tests and examples if a given ID or code becomes unusable
in the context of the package for any reason.

## Using test data in the tests and seeding / refreshing

Any data used by the package test suite should be stored in rds format in the 
**tests/testthat/testdata/** folder. The code required to obtain the test data should always be
added to the **tests/testthat/seed_tests.R** script:

- Add an individual function for creating an individual test data file
- Add that function as a call in `seed_tests()` to make sure it is refreshed as part of any
bulk refresh

Note that the `example_id()` function is especially useful in creating test data and should be
used for creating test data wherever possible.

To refresh all test data run `seed_tests()`. Or to refresh individual test data sets, find the
relevant see function and run that.

## Updating the package version

Once changes have been completed, reviewed and are ready for use in the wild, you
can increment the package version using:

`usethis::use_version()`

Once you've incremented the version number, it'll add a new heading to news.md.

Add a summary under news.md and then accept it's offer to commit on your behalf.

Once pushed and on the main branch, create a new release in GitHub itself.

## Running tests

You should run the following lines to test the package locally:
``` 
# To check functionality
devtools::check() # Ctrl-Shft-E

# For code styling
styler::style_pkg()
lintr::lint_package()
```

If you get a lot of lintr errors, particularly around things not being defined, make sure to load the package first using Ctrl-Shft-L or `devtools::load_all(".")`, then run again. There's a known issue with lintr not picking up on bindings until packages are loaded
