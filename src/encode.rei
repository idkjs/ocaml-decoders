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

module Make: (E: Encodeable) => S with type value = E.value;
