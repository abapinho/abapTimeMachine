CLASS zcl_blame_parts DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA type TYPE zblame_object_type READ-ONLY.
    DATA name TYPE sobj_name READ-ONLY.

    METHODS constructor
      IMPORTING
        !i_object_type TYPE zblame_object_type
        !i_object_name TYPE sobj_name.

    METHODS compute_blame
      RETURNING VALUE(rt_blame) TYPE zblame_line_t.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA  gt_part TYPE STANDARD TABLE OF REF TO zcl_blame_part.

    METHODS load_objects.
ENDCLASS.



CLASS zcl_blame_parts IMPLEMENTATION.
  METHOD constructor.
    me->type = i_object_type.
    me->name = i_object_name.
    load_objects( ).
  ENDMETHOD.


  METHOD load_objects.
    DATA(o_object) = NEW zcl_blame_object_factory( )->get_instance( i_object_type = me->type
                                                                     i_object_name = me->name ).
    gt_part = VALUE #( FOR include IN o_object->get_includes( )
                       ( NEW zcl_blame_part( include ) ) ).
  ENDMETHOD.


  METHOD compute_blame.
    LOOP AT gt_part INTO DATA(o_part).
      INSERT LINES OF o_part->compute_blame( ) INTO TABLE rt_blame.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
