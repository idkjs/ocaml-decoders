open Decoders;

module Bencode_decodeable: Decode.Decodeable with type value = Bencode.t = {
  type value = Bencode.t;

  let pp = (fmt, t) =>
    Format.fprintf(fmt, "@[%s@]", Bencode.pretty_print(t));

  let of_string = (input: string): result(value, string) =>
    try(Ok(Bencode.decode(`String(input)))) {
    | _ => Error("invalid bencode")
    };

  let of_file = (file: string): result(value, string) =>
    try({
      let v =
        Decoders_util.with_file_in(file, ic => Bencode.decode(`Channel(ic)));

      Ok(v);
    }) {
    | e => Error(Printexc.to_string(e))
    };

  let get_string =
    fun
    | Bencode.String(str) => Some(str)
    | _ => None;

  let get_int =
    fun
    | Bencode.Integer(int) => Some(Int64.to_int(int))
    | Bencode.String(s) =>
      try(Some(int_of_string(s))) {
      | _ => None
      }
    | _ => None;

  let get_float =
    fun
    | Bencode.String(s) =>
      try(Some(float_of_string(s))) {
      | _ => None
      }
    | _ => None;

  let get_null =
    fun
    | Bencode.Integer(0L)
    | Bencode.List([]) => Some()
    | _ => None;

  let get_bool =
    fun
    | Bencode.Integer(1L)
    | Bencode.String("true") => Some(true)
    | Bencode.Integer(0L)
    | Bencode.String("false") => Some(false)
    | _ => None;

  let get_list =
    fun
    | Bencode.List(a) => Some(a)
    | _ => None;

  let get_key_value_pairs =
    fun
    | Bencode.Dict(assoc) =>
      Some(List.rev_map(((s, v)) => (Bencode.String(s), v), assoc))
    | _ => None;

  let to_list = vs => Bencode.List(vs);
};

include Decode.Make(Bencode_decodeable);

let int64: decoder(int64) = (
  {
    run: t =>
      switch (t) {
      | Bencode.Integer(value) => Ok(value)
      | _ => fail("Expected an int64").run(t)
      },
  }:
    decoder(int64)
);
