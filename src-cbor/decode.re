open Decoders;

module Cbor_decodeable: Decode.Decodeable with type value = CBOR.Simple.t = {
  type value = CBOR.Simple.t;

  let pp = (fmt, t) =>
    Format.fprintf(fmt, "@[%s@]", CBOR.Simple.to_diagnostic(t));

  let of_string = (input: string): result(value, string) =>
    try(Ok(CBOR.Simple.decode(input))) {
    | CBOR.Error(msg) => Error(msg)
    };

  let of_file = (file: string): result(value, string) =>
    try(
      Ok(
        Decoders_util.with_file_in(file, chan =>
          Decoders_util.read_all(chan) |> CBOR.Simple.decode
        ),
      )
    ) {
    | e => Error(Printexc.to_string(e))
    };

  let get_string =
    fun
    | `Text(str) => Some(str)
    | _ => None;

  let get_int =
    fun
    | `Int(int) => Some(int)
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
    | `Array(a) => Some(a)
    | _ => None;

  let get_key_value_pairs =
    fun
    | `Map(assoc) => Some(assoc)
    | _ => None;

  let to_list = vs => `Array(vs);
};

include Decode.Make(Cbor_decodeable);

/* CBOR-specific decoders */

let undefined: decoder(unit) = (
  {
    run:
      fun
      | `Undefined => Ok()
      | json => fail("Expected Undefined").run(json),
  }:
    decoder(unit)
);

let simple: decoder(int) = (
  {
    run:
      fun
      | `Simple(i) => Ok(i)
      | json => fail("Expected Simple").run(json),
  }:
    decoder(int)
);

let bytes: decoder(string) = (
  {
    run:
      fun
      | `Bytes(b) => Ok(b)
      | json => fail("Expected bytes").run(json),
  }:
    decoder(string)
);
