open Decoders;
open Sexplib0;

module Sexplib_decodeable: Decode.Decodeable with type value = Sexp.t = {
  type value = Sexp.t;

  let pp = (fmt, value) => Format.fprintf(fmt, "@[%a@]", Sexp.pp_hum, value);

  let of_string = (input: string): result(value, string) =>
    try(Ok(Sexplib.Sexp.of_string(input))) {
    | Failure(msg) => Error(msg)
    };

  let of_file = (file: string): result(value, string) =>
    try(Ok(Sexplib.Sexp.load_sexp(file))) {
    | e => Error(Printexc.to_string(e))
    };

  let try_get = (f, value) =>
    try(Some(f(value))) {
    | Sexp_conv.Of_sexp_error(_) => None
    };

  let get_string = try_get(Sexp_conv.string_of_sexp);

  let get_int = try_get(Sexp_conv.int_of_sexp);

  let get_float = try_get(Sexp_conv.float_of_sexp);

  let get_null = try_get(Sexp_conv.unit_of_sexp);

  let get_bool = try_get(Sexp_conv.bool_of_sexp);

  let get_list =
    fun
    | Sexp.List(lst) => Some(lst)
    | _ => None;

  let get_key_value_pairs =
    fun
    | Sexp.List(lst) =>
      lst
      |> Decoders_util.My_list.map(
           fun
           | Sexp.List([key, value]) => Some((key, value))
           | Sexp.List([key, ...values]) => Some((key, Sexp.List(values)))
           | _ => None,
         )
      |> Decoders_util.My_list.all_some
    | _ => None;

  let to_list = values => Sexp.List(values);
};

include Decode.Make(Sexplib_decodeable);
