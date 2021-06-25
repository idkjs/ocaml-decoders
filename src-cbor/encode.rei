include Decoders.Encode.S with type value = CBOR.Simple.t;

let undefined: encoder(unit);

let simple: encoder(int);

let bytes: encoder(string);
