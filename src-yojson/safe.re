/** {2 Yojson.Safe implementation} */;

open Decoders;

module Json_decodeable: Decode.Decodeable with type value = Yojson.Safe.json = {
  type value = Yojson.Safe.json;

  let pp = (fmt, json) =>
    Format.fprintf(fmt, "@[%s@]", Yojson.Safe.pretty_to_string(json));

  let of_string: string => result(value, string) = (
    string =>
      try(Ok(Yojson.Safe.from_string(string))) {
      | Yojson.Json_error(msg) => Error(msg)
      }:
      string => result(value, string)
  );

  let of_file = file =>
    try(Ok(Yojson.Safe.from_file(file))) {
    | e => Error(Printexc.to_string(e))
    };

  let get_string =
    fun
    | `String(value) => Some(value)
    | _ => None;

  let get_int =
    fun
    | `Int(value) => Some(value)
    | _ => None;

  let get_float =
    fun
    | `Float(value) => Some(value)
    | `Int(value) => Some(float_of_int(value))
    | _ => None;

  let get_bool =
    fun
    | `Bool(value) => Some(value)
    | _ => None;

  let get_null =
    fun
    | `Null => Some()
    | _ => None;

  let get_list: value => option(list(value)) = (
    fun
    | `List(l) => Some(l)
    | _ => None:
      value => option(list(value))
  );

  let get_key_value_pairs: value => option(list((value, value))) = (
    fun
    | `Assoc(assoc) =>
      Some(List.map(((key, value)) => (`String(key), value), assoc))
    | _ => None:
      value => option(list((value, value)))
  );

  let to_list = values => `List(values);
};

module Decode = Decode.Make(Json_decodeable);

module Json_encodeable = {
  type value = Yojson.Safe.json;

  let to_string = json => Yojson.Safe.to_string(json);

  let of_string = x => `String(x);

  let of_int = x => `Int(x);

  let of_float = x => `Float(x);

  let of_bool = x => `Bool(x);

  let null = `Null;

  let of_list = xs => `List(xs);

  let of_key_value_pairs = xs =>
    `Assoc(
      xs
      |> Decoders_util.My_list.filter_map(((k, v)) =>
           switch (k) {
           | `String(k) => Some((k, v))
           | _ => None
           }
         ),
    );
};

module Encode = Encode.Make(Json_encodeable);
