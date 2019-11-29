interface ZIF_BLAME_OBJECT
  public .


  methods GET_INCLUDES
    returning
      value(RT_INCLUDE) type SEOINCL_T .
  methods GET_NAME
    returning
      value(R_NAME) type STRING .
endinterface.
