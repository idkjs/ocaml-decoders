open Decoders_util;

module type S = {
  type value;

  type encoder('a) = 'a => value;

  let string: encoder(string);

  let int: encoder(int);

  let float: encoder(float);

  let bool: encoder(bool);

  let null: value;

  let nullable: encoder('a) => encoder(option('a));

  [@ocaml.deprecated "Use nullable instead."]
  let option: encoder('a) => encoder(option('a));

  let list: encoder('a) => encoder(list('a));

  let array: encoder('a) => encoder(array('a));

  let obj: encoder(list((string, value)));

  let obj': encoder(list((value, value)));

  let value: encoder(value);

  let of_to_string: ('a => string) => encoder('a);

  let encode_value: (encoder('a), 'a) => value;

  let encode_string: (encoder('a), 'a) => string;
};

module type Encodeable = {
  type value;

  let to_string: value => string;

  let of_string: string => value;

  let of_int: int => value;

  let of_float: float => value;

  let of_bool: bool => value;

  let null: value;

  let of_list: list(value) => value;

  let of_key_value_pairs: list((value, value)) => value;
};

module Make = (E: Encodeable) : (S with type value = E.value) => {
  type value = E.value;

  type encoder('a) = 'a => value;

  let string = x => E.of_string(x);

  let int = x => E.of_int(x);

  let float = x => E.of_float(x);

  let bool = x => E.of_bool(x);

  let null = E.null;

  let nullable = encoder =>
    fun
    | None => E.null
    | Some(x) => encoder(x);

  let option = nullable;

  let list = (encoder, xs) =>
    xs |> My_list.map(x => encoder(x)) |> E.of_list;

  let array = (encoder, xs) =>
    xs |> Array.to_list |> My_list.map(x => encoder(x)) |> E.of_list;

  let obj' = xs => E.of_key_value_pairs(xs);

  let obj = xs =>
    xs |> List.map(((k, v)) => (E.of_string(k), v)) |> E.of_key_value_pairs;

  let value = x => x;

  let of_to_string = (to_string, x) => string(to_string(x));

  let encode_value = (encoder, x) => encoder(x);

  let encode_string = (encoder, x) => encoder(x) |> E.to_string;
};
