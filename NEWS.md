# eesyapi 0.1.0

* Creating meta data retrieval and parsing functions:
  - `get_meta()`: primary function for retrieving parsed R-friendly meta data
  - `get_meta_response()`: underlying function for retrieving meta data without full parsing
  - `parse_meta_location_ids()`: convert lcoation data from initial meta response to simple data frame
  - `parse_meta_filter_columns()`: create data frame of filter col_name and label from initial meta response
  - `parse_meta_filter_item_ids()`: convert filter item data from initial meta response to simple data frame
  - `parse_meta_indicator_columns()`: create data frame of indicator col_name and label from initial meta response
  - `http_request_error()`: render the API url for a give endpoint / data set combination
