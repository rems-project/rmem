/*===============================================================================*/
/*                                                                               */
/*                rmem executable model                                          */
/*                =====================                                          */
/*                                                                               */
/*  This file is:                                                                */
/*                                                                               */
/*  Copyright Shaked Flur, University of Cambridge 2017                          */
/*  Copyright Jon French, University of Cambridge  2017                          */
/*                                                                               */
/*  All rights reserved.                                                         */
/*                                                                               */
/*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   */
/*  LICENCE.txt.                                                                 */
/*                                                                               */
/*===============================================================================*/

//Provides: caml_input_value_from_reader mutable
//Requires: caml_failwith
//Requires: caml_float_of_bytes, caml_int64_of_bytes
//Requires: create_nat, set_digit_nat

function caml_input_value_from_reader(reader, ofs) {
  var _magic = reader.read32u ()
  var _block_len = reader.read32u ();
  var num_objects = reader.read32u ();
  var _size_32 = reader.read32u ();
  var _size_64 = reader.read32u ();
  var stack = [];
  var intern_obj_table = (num_objects > 0)?[]:null;
  var obj_counter = 0;
  function intern_rec () {
    var code = reader.read8u ();
    if (code >= 0x40 /*cst.PREFIX_SMALL_INT*/) {
      if (code >= 0x80 /*cst.PREFIX_SMALL_BLOCK*/) {
        var tag = code & 0xF;
        var size = (code >> 4) & 0x7;
        var v = [tag];
        if (size == 0) return v;
        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
        stack.push(v, size);
        return v;
      } else
        return (code & 0x3F);
    } else {
      if (code >= 0x20/*cst.PREFIX_SMALL_STRING */) {
        var len = code & 0x1F;
        var v = reader.readstr (len);
        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
        return v;
      } else {
        switch(code) {
        case 0x00: //cst.CODE_INT8:
          return reader.read8s ();
        case 0x01: //cst.CODE_INT16:
          return reader.read16s ();
        case 0x02: //cst.CODE_INT32:
          return reader.read32s ();
        case 0x03: //cst.CODE_INT64:
          caml_failwith("input_value: integer too large");
          break;
        case 0x04: //cst.CODE_SHARED8:
          var offset = reader.read8u ();
          return intern_obj_table[obj_counter - offset];
        case 0x05: //cst.CODE_SHARED16:
          var offset = reader.read16u ();
          return intern_obj_table[obj_counter - offset];
        case 0x06: //cst.CODE_SHARED32:
          var offset = reader.read32u ();
          return intern_obj_table[obj_counter - offset];
        case 0x08: //cst.CODE_BLOCK32:
          var header = reader.read32u ();
          var tag = header & 0xFF;
          var size = header >> 10;
          var v = [tag];
          if (size == 0) return v;
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          stack.push(v, size);
          return v;
        case 0x13: //cst.CODE_BLOCK64:
          caml_failwith ("input_value: data block too large");
          break;
        case 0x09: //cst.CODE_STRING8:
          var len = reader.read8u();
          var v = reader.readstr (len);
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          return v;
        case 0x0A: //cst.CODE_STRING32:
          var len = reader.read32u();
          var v = reader.readstr (len);
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          return v;
        case 0x0C: //cst.CODE_DOUBLE_LITTLE:
          var t = new Array(8);;
          for (var i = 0;i < 8;i++) t[7 - i] = reader.read8u ();
          var v = caml_float_of_bytes (t);
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          return v;
        case 0x0B: //cst.CODE_DOUBLE_BIG:
          var t = new Array(8);;
          for (var i = 0;i < 8;i++) t[i] = reader.read8u ();
          var v = caml_float_of_bytes (t);
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          return v;
        case 0x0E: //cst.CODE_DOUBLE_ARRAY8_LITTLE:
          var len = reader.read8u();
          var v = new Array(len+1);
          v[0] = 254;
          var t = new Array(8);;
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          for (var i = 1;i <= len;i++) {
            for (var j = 0;j < 8;j++) t[7 - j] = reader.read8u();
            v[i] = caml_float_of_bytes (t);
          }
          return v;
        case 0x0D: //cst.CODE_DOUBLE_ARRAY8_BIG:
          var len = reader.read8u();
          var v = new Array(len+1);
          v[0] = 254;
          var t = new Array(8);;
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          for (var i = 1;i <= len;i++) {
            for (var j = 0;j < 8;j++) t[j] = reader.read8u();
            v [i] = caml_float_of_bytes (t);
          }
          return v;
        case 0x07: //cst.CODE_DOUBLE_ARRAY32_LITTLE:
          var len = reader.read32u();
          var v = new Array(len+1);
          v[0] = 254;
          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
          var t = new Array(8);;
          for (var i = 1;i <= len;i++) {
            for (var j = 0;j < 8;j++) t[7 - j] = reader.read8u();
            v[i] = caml_float_of_bytes (t);
          }
          return v;
        case 0x0F: //cst.CODE_DOUBLE_ARRAY32_BIG:
          var len = reader.read32u();
          var v = new Array(len+1);
          v[0] = 254;
          var t = new Array(8);;
          for (var i = 1;i <= len;i++) {
            for (var j = 0;j < 8;j++) t[j] = reader.read8u();
            v [i] = caml_float_of_bytes (t);
          }
          return v;
        case 0x10: //cst.CODE_CODEPOINTER:
        case 0x11: //cst.CODE_INFIXPOINTER:
          caml_failwith ("input_value: code pointer");
          break;
        case 0x12: //cst.CODE_CUSTOM:
          var c, s = "";
          while ((c = reader.read8u ()) != 0) s += String.fromCharCode (c);
          switch(s) {
          case "_j":
            // Int64
            var t = new Array(8);;
            for (var j = 0;j < 8;j++) t[j] = reader.read8u();
            var v = caml_int64_of_bytes (t);
            if (intern_obj_table) intern_obj_table[obj_counter++] = v;
            return v;
          case "_i":
            // Int32
            var v = reader.read32s ();
            if (intern_obj_table) intern_obj_table[obj_counter++] = v;
            return v;
          case "_n":
            // Nativeint
            switch (reader.read8u ()) {
            case 1:
              var v = reader.read32s ();
              if (intern_obj_table) intern_obj_table[obj_counter++] = v;
              return v;
            case 2:
              caml_failwith("input_value: native integer value too large");
            default:
              caml_failwith("input_value: ill-formed native integer");
            }
            break;
          case "_nat":
              // Nat
              var size = reader.read32u();
              var v = create_nat(size);
              for (var j = 0; j < size; j++) {
                  set_digit_nat(v, j, reader.read32u());
              }
              if (intern_obj_table) intern_obj_table[obj_counter++] = v;
              return v;
          default:
              caml_failwith("input_value: unknown custom block identifier " + s);
          }
        default:
          caml_failwith ("input_value: ill-formed message");
        }
      }
    }
  }
  var res = intern_rec ();
  while (stack.length > 0) {
    var size = stack.pop();
    var v = stack.pop();
    var d = v.length;
    if (d < size) stack.push(v, size);
    v[d] = intern_rec ();
  }
  if (typeof ofs!="number") ofs[0] = reader.i;
  return res;
}
