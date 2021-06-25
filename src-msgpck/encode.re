module M = Msgpck;

module Msgpck_encodeable = {
  type value = M.t;

  let to_string = value => {
    let buf = M.StringBuf.to_string(value);
    Buffer.contents(buf);
  };

  let of_string = x => M.String(x);

  let of_int = x => M.Int(x);

  let of_float = x => M.Float(x);

  let of_bool = x => M.Bool(x);

  let null = M.Nil;

  let of_list = xs => M.List(xs);

  let of_key_value_pairs = xs => M.Map(xs);
};

include Decoders.Encode.Make(Msgpck_encodeable);

let ext = ((i, s)) => [@implicit_arity] M.Ext(i, s);

let int32 = i => M.Int32(i);

let int64 = i => M.Int64(i);

let uint32 = i => M.Uint32(i);

let uint64 = i => M.Uint64(i);

let bytes: encoder(string) = (b => M.Bytes(b): encoder(string));
