# eesyapi 0.2.0

* Creating publication querying functions:
  - `get_publication_catalogue()`: Retrieve the list of available publications
  - `get_publication_datasets()`: Retrieve the list of available data sets in a given publication
  - `example_id()`: Provide example publication and data set IDs (largely for example code and testing)
  - `api_url_pages()`: Render string to set paging on API query results
  - `eesapi_url()` -> `api_url()`: Name change to function and updated to allow publication and data set URLs
  - `warning_max_pages()`: Check for the query page number exceeding the total query pages available
  - Added some test data and a process for maintaining that data

# eesyapi 0.1.0

* Creating meta data retrieval and parsing functions:
  - `get_meta()`: primary function for retrieving parsed R-friendly meta data
  - `get_meta_response()`: underlying function for retrieving meta data without full parsing
  - `parse_meta_location_ids()`: convert location data from initial meta response to simple data frame
  - `parse_meta_filter_columns()`: create data frame of filter col_name and label from initial meta response
  - `parse_meta_filter_item_ids()`: convert filter item data from initial meta response to simple data frame
  - `parse_meta_indicator_columns()`: create data frame of indicator col_name and label from initial meta response
  - `http_request_error()`: render the API url for a give endpoint / data set combination
