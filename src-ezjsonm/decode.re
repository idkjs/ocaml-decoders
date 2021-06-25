open Decoders;

module Ezjsonm_decodeable: Decode.Decodeable with type value = Ezjsonm.value = {
  type value = Ezjsonm.value;

  let pp_t = (fmt, t) =>
    switch (t) {
    | `Null => Format.fprintf(fmt, "@[null@]")
    | `Bool(bool) =>
      Format.fprintf(fmt, "@[%a@]", Format.pp_print_bool, bool)
    | `Float(float) =>
      Format.fprintf(fmt, "@[%a@]", Format.pp_print_float, float)
    | `String(string) => Format.fprintf(fmt, "@[%S@]", string)
    | (`A(_) | `O(_)) as t =>
      Format.fprintf(fmt, "@[%s@]", Ezjsonm.(to_string(t)))
    };

  let pp = (fmt, t) => Format.fprintf(fmt, "@[%a@]", pp_t, t);

  let of_string = (input: string): result(value, string) =>
    try(Ok(Ezjsonm.from_string(input))) {
    | [@implicit_arity] Ezjsonm.Parse_error(_json, msg) => Error(msg)
    };

  let of_file = (file: string): result(value, string) =>
    try(Ok(Decoders_util.with_file_in(file, Ezjsonm.from_channel))) {
    | e => Error(Printexc.to_string(e))
    };

  let get_string =
    fun
    | `String(str) => Some(str)
    | _ => None;

  let get_int =
    fun
    | `Float(float) =>
      /* TODO: fail if not an int? */
      Some(int_of_float(float))
    | _ => None;

  let get_float =
    fun
    | `Float(float) => Some(float)
    | _ => None;

  let get_null =
    fun
    | `Null => Some()
    | _ => None;

  let get_bool =
    fun
    | `Bool(bool) => Some(bool)
    | _ => None;

  let get_list =
    fun
    | `A(a) => Some(a)
    | _ => None;

  let get_key_value_pairs =
    fun
    | `O(assoc) =>
      Some(List.map(((key, value)) => (`String(key), value), assoc))
    | _ => None;

  let to_list = values => `A(values);
};

include Decode.Make(Ezjsonm_decodeable);
