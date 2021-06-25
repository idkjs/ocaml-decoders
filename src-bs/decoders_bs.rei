/** Turn JSON values into Ocaml values. */

module Decode: {include Decoders.Decode.S with type value = Js.Json.t;};

module Encode: {include Decoders.Encode.S with type value = Js.Json.t;};
