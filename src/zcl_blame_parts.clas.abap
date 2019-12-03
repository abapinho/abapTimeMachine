CLASS zcl_blame_parts DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !i_object_type TYPE zblame_object_type
        !i_object_name TYPE sobj_name
      RAISING
        zcx_blame .

    METHODS get_data
      RETURNING VALUE(rs_data) TYPE zblame_parts.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA g_type TYPE zblame_object_type.
    DATA g_name TYPE sobj_name.
    DATA gt_part TYPE zblame_part_ref_t.

    METHODS load_objects
      RAISING zcx_blame.
ENDCLASS.



CLASS zcl_blame_parts IMPLEMENTATION.


  METHOD constructor.
    me->g_type = i_object_type.
    me->g_name = i_object_name.
    load_objects( ).
  ENDMETHOD.


  METHOD load_objects.
    DATA(o_object) = NEW zcl_blame_object_factory( )->get_instance( i_object_type = me->g_type
                                                                    i_object_name = me->g_name ).
    me->gt_part = o_object->get_part_list( ).
  ENDMETHOD.


  method get_data.
    rs_data = value #( name = g_name
                       type = g_type
                       t_part = value #( for o_part in gt_part
                                         ( name = o_part->name
                                           type = o_part->vrsd_type
                                           object_name = o_part->vrsd_name
                                           t_blame = o_part->t_blame ) ) ).
  ENDMETHOD.
ENDCLASS.
