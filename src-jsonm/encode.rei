/** Streaming encoding using [Jsonm].

    Example usage:

    {[
      module E = Decoders_jsonm.Encode

      let run_encoder out_channel (encode : t E.encoder) (x : t) =
        let dst = `Channel out_channel in
        let encoder = Jsonm.encoder ~minify:true dst in
        let env = E.make_env ~encoder () in
        E.encode_value encode x env
    ]}
*/;

type env;

let make_env:
  (~encoder: Jsonm.encoder, ~on_partial: unit => unit=?, unit) => env;

include Decoders.Encode.S with type value = env => unit;

/** {2 Low-level combinators}

    Assuming we have:

    {[
      type member
      val member : member encoder
    ]}

    And a type [x]:

    {[
      type x =
        { id : string
        ; members : member list
        }
    ]}

    An encoder for [x] might look like this:

    {[
      let x_encoder x =
        object_start >>

        name "id" >>
        string x.id >>

        name "members" >>
        array_start >>
        iter member x.members >>
        array_end >>

        object_end
    ]}
*/;

let (>>): (value, value) => value;

let iter: (encoder('a), list('a)) => value;

let object_start: value;

let name: string => value;

let object_end: value;

let array_start: value;

let array_end: value;

let end_: value;
