/** {2 Yojson implementation} */
open Decoders;

module Json_decodeable: Decode.Decodeable with type value = Yojson.Raw.t = {
  type value = Yojson.Raw.t;

  let pp = (fmt, json) =>
    Format.fprintf(fmt, "@[%s@]", Yojson.Raw.pretty_to_string(json));

  let of_string: string => result(value, string) = (
    string =>
      try(Ok(Yojson.Raw.from_string(string))) {
      | Yojson.Json_error(msg) => Error(msg)
      }:
      string => result(value, string)
  );

  let of_file = file =>
    try(Ok(Yojson.Raw.from_file(file))) {
    | e => Error(Printexc.to_string(e))
    };

  let get_string =
    fun
    | `Stringlit(s) =>
      /* Stringlits are wrapped in double-quotes. */
      Some(String.sub(s, 1, String.length(s) - 2))
    | _ => None;

  let get_int =
    fun
    | `Intlit(value) => Some(int_of_string(value))
    | _ => None;

  let get_float =
    fun
    | `Floatlit(value) => Some(float_of_string(value))
    | `Intlit(value) => Some(float_of_string(value))
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
      Some(
        List.map(
          ((key, value)) =>
            (`Stringlit(Printf.sprintf("%S", key)), value),
          assoc,
        ),
      )
    | _ => None:
      value => option(list((value, value)))
  );

  let to_list = values => `List(values);
};

module Decode = {
  include Decode.Make(Json_decodeable);

  /* Yojson.Raw specific decoders */

  let stringlit: decoder(string) = (
    {
      run:
        fun
        | `Stringlit(value) => Ok(value)
        | json => fail("Expected a string").run(json),
    }:
      decoder(string)
  );

  let intlit: decoder(string) = (
    {
      run:
        fun
        | `Intlit(value) => Ok(value)
        | json => fail("Expected an int").run(json),
    }:
      decoder(string)
  );

  let floatlit: decoder(string) = (
    {
      run:
        fun
        | `Floatlit(value) => Ok(value)
        | `Intlit(value) => Ok(value)
        | json => fail("Expected a float").run(json),
    }:
      decoder(string)
  );
};

module Json_encodeable = {
  type value = Yojson.Raw.t;

  let to_string = json => Yojson.Raw.to_string(json);

  let of_string = x => `Stringlit(Printf.sprintf("%S", x));

  let of_int = x => `Intlit(string_of_int(x));

  let of_float = x => `Floatlit(string_of_float(x));

  let of_bool = x => `Bool(x);

  let null = `Null;

  let of_list = xs => `List(xs);

  let of_key_value_pairs = xs =>
    `Assoc(
      xs
      |> Decoders_util.My_list.filter_map(((k, v)) =>
           Json_decodeable.get_string(k)
           |> Decoders_util.My_opt.map(k => (k, v))
         ),
    );
};

module Encode = {
  include Encode.Make(Json_encodeable);

  let stringlit = x => `Stringlit(x);

  let intlit = x => `Intlit(x);

  let floatlit = x => `Floatlit(x);
};
