# eesyapi 0.3.1

* Added parsing of SQIDs in retrieved data to provide human readable content
* Created function, `preview_dataset()`, to connect to csv endpoint for downloading data set csv file
* Added first draft of example workflow for querying a data set

# eesyapi 0.3.0

* Created capacity to query data using POST:
  - `query_dataset()`: Now defaults to using POST instead of GET
  - `post_dataset()`: Sends a query of a data set, either using a json file, json string or 
  parameters
* Updated how `example_id()` works to allow more complex examples

# eesyapi 0.2.1

* Created initial `query_dataset()` function that queries a data set using `get_dataset()`
* Created `get_dataset()` function that queries a data set using GET and URL parameters
* Updated `get_meta()` to work with new API meta output (addition of id alongside col_name and 
label)
* Removed redundant function: `parse_meta_filter_columns()`
* Hex logo added for documentation

# eesyapi 0.2.0

* Creating publication querying functions:
  - `get_publication_catalogue()`: Retrieve the list of available publications
  - `get_publication_datasets()`: Retrieve the list of available data sets in a given publication
  - `example_id()`: Provide example publication and data set IDs (largely for example code and testing)
  - `api_url_pages()`: Render string to set paging on API query results
  - `eesapi_url()` -> `api_url()`: Name change to function and updated to allow publication and data set URLs
  - `warning_max_pages()`: Check for the query page number exceeding the total query pages available
  - Added some test data and a process for maintaining that data
  - Added the `validate_` family of validation helpers

# eesyapi 0.1.0

* Creating meta data retrieval and parsing functions:
  - `get_meta()`: primary function for retrieving parsed R-friendly meta data
  - `get_meta_response()`: underlying function for retrieving meta data without full parsing
  - `parse_meta_location_ids()`: convert location data from initial meta response to simple data frame
  - `parse_meta_filter_columns()`: create data frame of filter col_name and label from initial meta response
  - `parse_meta_filter_item_ids()`: convert filter item data from initial meta response to simple data frame
  - `parse_meta_indicator_columns()`: create data frame of indicator col_name and label from initial meta response
  - `http_request_error()`: render the API url for a give endpoint / data set combination
