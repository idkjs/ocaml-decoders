open Decoders;
module M = Msgpck;

module Msgpck_decodeable: Decode.Decodeable with type value = Msgpck.t = {
  type value = Msgpck.t;

  let pp = (fmt, t) => Format.fprintf(fmt, "@[%a@]", Msgpck.pp, t);

  let of_string = (input: string): result(value, string) =>
    try(Ok(snd @@ M.StringBuf.read(input))) {
    | Invalid_argument(s) => Error(s)
    };

  let of_file = (file: string): result(value, string) =>
    try(
      Ok(
        Decoders_util.with_file_in(file, chan =>
          Decoders_util.read_all(chan) |> M.StringBuf.read |> snd
        ),
      )
    ) {
    | e => Error(Printexc.to_string(e))
    };

  let get_string =
    fun
    | M.String(str)
    | M.Bytes(str) => Some(str)
    | _ => None;

  /* note: the other int constructors are only used for values that do
     not fit in [int]. */
  let get_int =
    fun
    | M.Int(int) => Some(int)
    | _ => None;

  let get_float =
    fun
    | M.Float(float) => Some(float)
    | M.Float32(f) => Some(Int32.float_of_bits(f))
    | _ => None;

  let get_null =
    fun
    | M.Nil => Some()
    | _ => None;

  let get_bool =
    fun
    | M.Bool(bool) => Some(bool)
    | _ => None;

  let get_list =
    fun
    | M.List(a) => Some(a)
    | _ => None;

  let get_key_value_pairs =
    fun
    | M.Map(assoc) => Some(assoc)
    | _ => None;

  let to_list = vs => M.List(vs);
};

include Decode.Make(Msgpck_decodeable);

let string_strict: decoder(string) = (
  {
    run:
      fun
      | M.String(b) => Ok(b)
      | m => fail("Expected string (strict)").run(m),
  }:
    decoder(string)
);

let bytes: decoder(string) = (
  {
    run:
      fun
      | M.Bytes(b) => Ok(b)
      | m => fail("Expected bytes").run(m),
  }:
    decoder(string)
);

let int32: decoder(_) = (
  {
    run:
      fun
      | M.Int32(i) => Ok(i)
      | m => fail("Expected int32").run(m),
  }:
    decoder(_)
);

let int64: decoder(_) = (
  {
    run:
      fun
      | M.Int64(i) => Ok(i)
      | m => fail("Expected int64").run(m),
  }:
    decoder(_)
);

let uint32: decoder(_) = (
  {
    run:
      fun
      | M.Uint32(i) => Ok(i)
      | m => fail("Expected uint32").run(m),
  }:
    decoder(_)
);

let uint64: decoder(_) = (
  {
    run:
      fun
      | M.Uint64(i) => Ok(i)
      | m => fail("Expected uint64").run(m),
  }:
    decoder(_)
);

let ext: decoder((int, string)) = (
  {
    run:
      fun
      | [@implicit_arity] M.Ext(i, s) => [@implicit_arity] Ok(i, s)
      | m => fail("Expected extension").run(m),
  }:
    decoder((int, string))
);
