type exposed_error('value) =
  | Decoder_error(string, option('value))
  | Decoder_errors(list(exposed_error('value)))
  | Decoder_tag(string, exposed_error('value));

type result('good, 'bad) =
  Decoders_util.My_result.t('good, 'bad) = | Ok('good) | Error('bad);

type exposed_decoder('value, 'a) = {
  run: 'value => result('a, exposed_error('value)),
};

/** User-facing Decoder interface. */

module type S = {
  /** The type of values to be decoded (e.g. JSON or Yaml). */

  type value;

  type error = exposed_error(value);

  let pp_error: (Format.formatter, error) => unit;

  let string_of_error: error => string;

  let of_string: string => result(value, error);

  let of_file: string => result(value, error);

  /** The type of decoders.

      Use the functions below to construct decoders for your data types.

      To run a decoder, pass it to {!val:decode_value}.
  */

  type decoder('a);

  /** {2 Primitives} */;

  /** Decode a [string]. */

  let string: decoder(string);

  /** Decode an [int]. */

  let int: decoder(int);

  /** Decode a [float]. */

  let float: decoder(float);

  /** Decode a [bool]. */

  let bool: decoder(bool);

  /** Decode a [null]. */

  let null: decoder(unit);

  /** Decode a literal [value]. */

  let value: decoder(value);

  /** {2 Lists} */;

  /** Decode a collection into an OCaml list. */

  let list: decoder('a) => decoder(list('a));

  /** Decode a collection into an OCaml list, skipping elements for which the
      decoder returns None.
  */

  let list_filter: decoder(option('a)) => decoder(list('a));

  /** Decode a collection with an accumulator.

      If we consider that an ['a decoder] is basically a type alias for
      [json -> ('a, error) result], the signature of this function is comparable
      to that of [List.fold_left]:

      {[
      val List.fold_left : ('a ->   'b ->                 'a) -> 'a -> 'b list ->                 'a
      val list_fold_left : ('a -> json -> ('a, error) result) -> 'a ->    json -> ('a, error) result
      val list_fold_left : ('a ->                 'a decoder) -> 'a ->                    'a decoder
      ]}
  */

  let list_fold_left: ('a => decoder('a), 'a) => decoder('a);

  /** Decode a collection into an OCaml array. */

  let array: decoder('a) => decoder(array('a));

  /** Decode a collection, requiring a particular index. */

  let index: (int, decoder('a)) => decoder('a);

  /** [fst |> uncons rest] decodes the first element of a list using [fst], then
      decodes the remainder of the list using [rest].

      For example, to decode this s-expression:

      {[
          (library
            (name decoders))
      ]}

      we can use this decoder:

      {[
          string |> uncons (function
            | "library" -> field "name" string
            | _ -> fail "Expected a library stanza")
      ]}

      As another example, say you have a JSON array that starts with a string,
      then a bool, then a list of integers:

      {[
          ["hello", true, 1, 2, 3, 4]
      ]}

      We could decode it like this:

      {[
          let (>>=::) fst rest = uncons rest fst

          let decoder : (string * bool * int list) decoder =
            string >>=:: fun the_string ->
            bool >>=:: fun the_bool ->
            list int >>= fun the_ints ->
            succeed (the_string, the_bool, the_ints)
      ]}

      (If you squint, the uncons operator [>>=::] kind of looks like the cons
      operator [::].)
  */

  let uncons: ('a => decoder('b), decoder('a)) => decoder('b);

  /** {1 Object primitives} */;

  /** Decode an object, requiring a particular field. */

  let field: (string, decoder('a)) => decoder('a);

  /** Decode an object, where a particular field may or may not be present.

      For example, [(field_opt "hello" int)]:

      - when run on [{"hello": 123}], will succeed with [Some 123]
      - when run on [{"hello": null}], will fail
      - when run on [{"world": 123}], will succeed with [None]
      - when run on [["a", "list", "of", "strings"]], will fail
  */

  let field_opt: (string, decoder('a)) => decoder(option('a));

  /** Decode an object, requiring exactly one field. */

  let single_field: (string => decoder('a)) => decoder('a);

  /** Decode a nested object, requiring certain fields. */

  let at: (list(string), decoder('a)) => decoder('a);

  /** {2 Inconsistent structure} */;

  /** [maybe d] is a decoder that always succeeds. If [d] succeeds with [x],
      then [maybe d] succeeds with [Some x], otherwise if [d] fails, then [maybe d]
      succeeds with [None].

      For example, [maybe (field "hello" int)]:

      - when run on [{"hello": 123}], will succeed with [Some 123]
      - when run on [{"hello": null}], will succeed with [None]
      - when run on [{"world": 123}], will succeed with [None]
      - when run on [["a", "list", "of", "strings"]], will succeed with [None]

  */

  let maybe: decoder('a) => decoder(option('a));

  /** [nullable d] will succeed with [None] if the JSON value is [null]. If the
      JSON value is non-[null], it wraps the result of running [x] in a [Some].

      For example, [field "hello" (nullable int)]:

      - when run on [{"hello": 123}], will succeed with [Some 123]
      - when run on [{"hello": null}], will succeed with [None]
      - when run on [{"world": 123}], will fail
      - when run on [["a", "list", "of", "strings"]], will fail

  */

  let nullable: decoder('a) => decoder(option('a));

  /** Try a sequence of different decoders. */

  let one_of: list((string, decoder('a))) => decoder('a);

  /** {2 Mapping} */;

  /** Map over the result of a decoder. */

  let map: ('a => 'b, decoder('a)) => decoder('b);

  /** Try two decoders and then combine the result. We can use this to decode
      objects with many fields (but it's preferable to use [Infix.(>>=)] - see the README).
  */

  let apply: (decoder('a => 'b), decoder('a)) => decoder('b);

  /** {2 Working with object keys} */;

  /** Decode all of the keys of an object to a list of strings. */

  let keys: decoder(list(string));

  /** Decode an object into a list of key-value pairs. */

  let key_value_pairs: decoder('v) => decoder(list((string, 'v)));

  /** Decode an object into a list of values, where the value
      decoder depends on the key. */

  let key_value_pairs_seq: (string => decoder('v)) => decoder(list('v));

  /** [keys'] is for when your keys might not be strings - probably only likely for Yaml. */

  let keys': decoder('k) => decoder(list('k));

  let key_value_pairs':
    (decoder('k), decoder('v)) => decoder(list(('k, 'v)));

  let key_value_pairs_seq':
    (decoder('k), 'k => decoder('v)) => decoder(list('v));

  /** {2 Fancy decoding} */;

  /** A decoder that always succeeds with the argument, ignoring the input. */

  let succeed: 'a => decoder('a);

  /** A decoder that always fails with the given message, ignoring the input. */

  let fail: string => decoder('a);

  let fail_with: error => decoder('a);

  let from_result: result('a, error) => decoder('a);

  /** Create decoders that depend on previous results. */

  let and_then: ('a => decoder('b), decoder('a)) => decoder('b);

  /** Recursive decoders.

      [let my_decoder = fix (fun my_decoder -> ...)] allows you to define
      [my_decoder] in terms of itself.
  */

  let fix: (decoder('a) => decoder('a)) => decoder('a);

  /** Create a decoder from a function [of_string : string -> 'a option] */

  let of_of_string: (~msg: string, string => option('a)) => decoder('a);

  module Infix: {
    let (>|=): (decoder('a), 'a => 'b) => decoder('b);

    let (>>=): (decoder('a), 'a => decoder('b)) => decoder('b);

    let (<*>): (decoder('a => 'b), decoder('a)) => decoder('b);

    let (<$>): ('a => 'b, decoder('a)) => decoder('b);

    include Shims_let_ops_.S with type t_let('a) := decoder('a);
  };

  include (module type of Infix);

  /** {2 Running decoders} */;

  /** Run a decoder on some input. */

  let decode_value: (decoder('a), value) => result('a, error);

  /** Run a decoder on a string. */

  let decode_string: (decoder('a), string) => result('a, error);

  /** Run a decoder on a file. */

  let decode_file: (decoder('a), string) => result('a, error);

  /** {2 Pipeline Decoders} */

  module Pipeline: {
    /**
        Pipeline decoders present an alternative to the [mapN] style. They read
        more naturally, but can lead to harder-to-understand type errors.
      {[
        let person_decoder : person decoder =
          decode as_person
          |> required "name" string
          |> required "age" int
      ]}
    */;

    let decode: 'a => decoder('a);

    let required: (string, decoder('a), decoder('a => 'b)) => decoder('b);

    let required_at:
      (list(string), decoder('a), decoder('a => 'b)) => decoder('b);

    let optional:
      (string, decoder('a), 'a, decoder('a => 'b)) => decoder('b);

    let optional_at:
      (list(string), decoder('a), 'a, decoder('a => 'b)) => decoder('b);

    let custom: (decoder('a), decoder('a => 'b)) => decoder('b);
  };
};

/** {2 Creating a Decoder implementation}

    The following is useful only if you are creating a new Decoder implementation.
*/;

/** Signature of things that can be decoded. */

module type Decodeable = {
  type value;

  let pp: (Format.formatter, value) => unit;

  let of_string: string => result(value, string);

  let of_file: string => result(value, string);

  let get_string: value => option(string);

  let get_int: value => option(int);

  let get_float: value => option(float);

  let get_bool: value => option(bool);

  let get_null: value => option(unit);

  let get_list: value => option(list(value));

  let get_key_value_pairs: value => option(list((value, value)));

  let to_list: list(value) => value;
};

/** Derive decoders for a [Decodeable.value]. */

module Make:
  (M: Decodeable) =>

    S with
      type value = M.value and
      type decoder('a) = exposed_decoder(M.value, 'a);
