include Decoders.Encode.S with type value = Msgpck.t;

let ext: encoder((int, string));

let int32: encoder(int32);

let int64: encoder(int64);

let uint32: encoder(int32);

let uint64: encoder(int64);

let bytes: encoder(string);
