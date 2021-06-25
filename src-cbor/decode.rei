include Decoders.Decode.S with type value = CBOR.Simple.t;

let undefined: decoder(unit);

let simple: decoder(int);

let bytes: decoder(string);
