open Decoders;

module Bencode_encodeable = {
  type value = Bencode.t;

  let to_string = value => Bencode.encode_to_string(value);

  let of_string = x => Bencode.String(x);

  let of_int = x => Bencode.Integer(Int64.of_int(x));

  let of_float = x => Bencode.String(string_of_float(x));

  let of_bool = x => Bencode.Integer(if (x) {1L} else {0L});

  let null = Bencode.Integer(0L);

  let of_list = xs => Bencode.List(xs);

  let of_key_value_pairs = xs => {
    let xs =
      Decoders_util.My_list.filter_map(
        fun
        | (Bencode.String(s), v) => Some((s, v))
        | _ => None,
        xs,
      );

    Bencode.Dict(xs);
  };
};

include Decoders.Encode.Make(Bencode_encodeable);

let int64 = i => Bencode.Integer(i);
