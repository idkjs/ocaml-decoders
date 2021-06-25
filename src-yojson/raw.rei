/** Turn JSON values into Ocaml values. */

module Decode: {
  include Decoders.Decode.S with type value = Yojson.Raw.t;

  /** {2 Yojson.Raw-specific decoders}*/

  let stringlit: decoder(string);

  let intlit: decoder(string);

  let floatlit: decoder(string);
};

module Encode: {
  include Decoders.Encode.S with type value = Yojson.Raw.t;

  /** {2 Yojson.Raw-specific encoders}*/

  let stringlit: encoder(string);

  let intlit: encoder(string);

  let floatlit: encoder(string);
};
