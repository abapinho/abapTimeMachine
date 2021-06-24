INTERFACE zif_timem_asset
  PUBLIC .

  "! Return the filename
  METHODS get_url
    RETURNING
      VALUE(result) TYPE w3url .

  "! Return the asset type (html or css)
  METHODS get_subtype
    RETURNING
      VALUE(result) TYPE char20.

  "! Return the actual content
  METHODS get_content
    RETURNING
      VALUE(result) TYPE string .
ENDINTERFACE.
