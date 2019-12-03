INTERFACE zif_blame_asset
  PUBLIC .


  METHODS get_url
    RETURNING
      VALUE(r_url) TYPE w3url .

  METHODS get_subtype
    RETURNING
      VALUE(r_subtype) TYPE char20.

  METHODS get_content
    RETURNING
      VALUE(r_content) TYPE string .
ENDINTERFACE.
