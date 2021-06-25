/** {2 Bucklescript Js.Json implementation} */
open Decoders;

type result('good, 'bad) =
  Decode.result('good, 'bad) = | Ok('good) | Error('bad);

module Json_decodeable: Decode.Decodeable with type value = Js.Json.t = {
  type value = Js.Json.t;

  let pp = (fmt, json) =>
    Format.fprintf(fmt, "@[%s@]", Js.Json.stringifyWithSpace(json, 2));

  let of_string: string => result(value, string) = (
    string =>
      try(Ok(Js.Json.parseExn(string))) {
      | Js.Exn.Error(e) =>
        Error(Js.Exn.message(e)->(Belt.Option.getWithDefault("unknown")))
      }:
      string => result(value, string)
  );

  let of_file = _file => failwith("Not implemented");

  let get_string = Js.Json.decodeString;

  let get_int = json =>
    Js.Json.decodeNumber(json)->(Belt.Option.map(int_of_float));

  let get_float = Js.Json.decodeNumber;

  let get_bool = Js.Json.decodeBoolean;

  let get_null = value =>
    Js.Json.decodeNull(value)->(Belt.Option.map(_ => ()));

  let get_list = (value: value): option(list(value)) =>
    Js.Json.decodeArray(value)->(Belt.Option.map(Array.to_list));

  let get_key_value_pairs = (value: value): option(list((value, value))) =>
    Js.Json.decodeObject(value)
    ->(
        Belt.Option.map(dict =>
          Js.Dict.entries(dict)->Array.to_list
          |> List.map(((key, value)) => (Js.Json.string(key), value))
        )
      );

  let to_list = values => Js.Json.array(Array.of_list(values));
};

module Decode = {
  module D = Decode.Make(Json_decodeable);
  include D;

  let tag_error = (msg: string, error: error): error =>
    [@implicit_arity] Decoder_tag(msg, error);

  let tag_errors = (msg: string, errors: list(error)): error =>
    [@implicit_arity] Decoder_tag(msg, Decoder_errors(errors));

  let array: decoder('a) => decoder(array('a)) = (
    decoder => {
      run: t =>
        switch (Js.Json.decodeArray(t)) {
        | None => fail("Expected an array").run(t)
        | Some(arr) =>
          let (oks, errs) =
            arr
            |> Js.Array.reducei(
                 ((oks, errs), x, i) =>
                   switch (decoder.run(x)) {
                   | Ok(a) =>
                     let _ = Js.Array.push(a, oks);
                     (oks, errs);
                   | Error(e) =>
                     let _ =
                       Js.Array.push(
                         tag_error("element " ++ Js.Int.toString(i), e),
                         errs,
                       );
                     (oks, errs);
                   },
                 ([||], [||]),
               );

          if (Js.Array.length(errs) > 0) {
            Error(
              tag_errors("while decoding an array", errs |> Array.to_list),
            );
          } else {
            Ok(oks);
          };
        },
    }:
      decoder('a) => decoder(array('a))
  );
};

module Json_encodeable = {
  type value = Js.Json.t;

  let to_string = json => Js.Json.stringify(json);

  let of_string = x => Js.Json.string(x);

  let of_int = x => Js.Json.number(float_of_int(x));

  let of_float = x => Js.Json.number(x);

  let of_bool = x => Js.Json.boolean(x);

  let null = Js.Json.null;

  let of_list = xs => Js.Json.array(Array.of_list(xs));

  let of_key_value_pairs = xs =>
    Js.Json.object_(
      xs
      ->(
          Belt.List.keepMap(((k, v)) =>
            Js.Json.decodeString(k)->(Belt.Option.map(k => (k, v)))
          )
        )
      ->Js.Dict.fromList,
    );
};

module Encode = {
  include Encode.Make(Json_encodeable);

  let array = (encoder, xs) =>
    xs |> Js.Array.map(x => encoder(x)) |> Js.Json.array;
};
