INTERFACE zif_blame_asset
  PUBLIC .

  "! Return the filename
  METHODS get_url
    RETURNING
      VALUE(r_url) TYPE w3url .

  "! Return the asset type (html or css)
  METHODS get_subtype
    RETURNING
      VALUE(r_subtype) TYPE char20.

  "! Return the actual content
  METHODS get_content
    RETURNING
      VALUE(r_content) TYPE string .
ENDINTERFACE.
