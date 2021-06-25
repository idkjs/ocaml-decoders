include Decoders.Decode.S with type value = Msgpck.t;

/** Only accepts [String], not [Bytes]. The string should be valid UTF8
    per the spec. */

let string_strict: decoder(string);

/** Raw data only. */

let bytes: decoder(string);

let int32: decoder(int32);

let int64: decoder(int64);

let uint32: decoder(int32);

let uint64: decoder(int64);

let ext: decoder((int, string));
