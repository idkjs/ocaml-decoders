type env = {
  encoder: Jsonm.encoder,
  on_partial: unit => unit,
};

let make_env =
    (~encoder, ~on_partial=() => failwith("Not expecting `Partial"), ()) => {
  encoder,
  on_partial,
};

let make_encoder' = (x, {encoder, on_partial}) => {
  let rec await = () => {
    on_partial();
    switch (Jsonm.encode(encoder, `Await)) {
    | `Ok => ()
    | `Partial => await()
    };
  };

  switch (Jsonm.encode(encoder, x)) {
  | `Ok => ()
  | `Partial => await()
  };
};

let make_encoder = (l, env) => make_encoder'(`Lexeme(l), env);

type v = env => unit;

let (>>) = (v1: v, v2: v): v =>
  env => {
    v1(env);
    v2(env);
  };

let iter = (encode, xs): v => env => xs |> List.iter(x => encode(x, env));

let object_start = make_encoder(`Os);

let name = x => make_encoder(`Name(x));

let object_end = make_encoder(`Oe);

let array_start = make_encoder(`As);

let array_end = make_encoder(`Ae);

let end_ = make_encoder'(`End);

module Jsonm_encodeable = {
  type value = v;

  let to_string = (_v: value): string => failwith("Not implemented");

  let of_string = (x): value => make_encoder(`String(x));

  let of_int = (x): value => make_encoder(`Float(float_of_int(x)));

  let of_float = (x): value => make_encoder(`Float(x));

  let of_bool = (x): value => make_encoder(`Bool(x));

  let null: value = (make_encoder(`Null): value);

  let of_list = (xs: list(value)): value =>
    array_start >> iter(x => x, xs) >> array_end;

  let of_key_value_pairs = (xs: list((value, value))): value =>
    object_start >> iter(((k, v)) => k >> v, xs) >> object_end;
};

include Decoders.Encode.Make(Jsonm_encodeable);

/* Override with more efficient implementations */

let list = (encode, xs) => array_start >> iter(encode, xs) >> array_end;

let obj = (xs: list((string, value))): value =>
  object_start >> iter(((k, v)) => name(k) >> v, xs) >> object_end;

let encode_value = (encoder, x) => encoder(x) >> end_;

let encode_string = (encoder, x) => {
  let b = Buffer.create(16);
  let env = make_env(~encoder=Jsonm.encoder(~minify=true, `Buffer(b)), ());
  let () = encode_value(encoder, x, env);
  Buffer.contents(b);
};
