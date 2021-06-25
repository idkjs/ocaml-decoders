/** Functors for creating Decoders. */;

open Decoders_util;

type exposed_error('value) =
  | Decoder_error(string, option('value))
  | Decoder_errors(list(exposed_error('value)))
  | Decoder_tag(string, exposed_error('value));

type result('good, 'bad) =
  My_result.t('good, 'bad) = | Ok('good) | Error('bad);

type exposed_decoder('value, 'a) = {
  run: 'value => result('a, exposed_error('value)),
};

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

/** User-facing Decoder interface. */
module type S = {
  type value;

  type error = exposed_error(value);

  let pp_error: (Format.formatter, error) => unit;

  let string_of_error: error => string;

  let of_string: string => result(value, error);

  let of_file: string => result(value, error);

  type decoder('a);

  let string: decoder(string);

  let int: decoder(int);

  let float: decoder(float);

  let bool: decoder(bool);

  let null: decoder(unit);

  let value: decoder(value);

  let list: decoder('a) => decoder(list('a));

  let list_filter: decoder(option('a)) => decoder(list('a));

  let list_fold_left: ('a => decoder('a), 'a) => decoder('a);

  let array: decoder('a) => decoder(array('a));

  let index: (int, decoder('a)) => decoder('a);

  let uncons: ('a => decoder('b), decoder('a)) => decoder('b);

  let field: (string, decoder('a)) => decoder('a);

  let field_opt: (string, decoder('a)) => decoder(option('a));

  let single_field: (string => decoder('a)) => decoder('a);

  let at: (list(string), decoder('a)) => decoder('a);

  let maybe: decoder('a) => decoder(option('a));

  let nullable: decoder('a) => decoder(option('a));

  let one_of: list((string, decoder('a))) => decoder('a);

  let map: ('a => 'b, decoder('a)) => decoder('b);

  let apply: (decoder('a => 'b), decoder('a)) => decoder('b);

  let keys: decoder(list(string));

  let key_value_pairs: decoder('v) => decoder(list((string, 'v)));

  let key_value_pairs_seq: (string => decoder('v)) => decoder(list('v));

  let keys': decoder('k) => decoder(list('k));

  let key_value_pairs':
    (decoder('k), decoder('v)) => decoder(list(('k, 'v)));

  let key_value_pairs_seq':
    (decoder('k), 'k => decoder('v)) => decoder(list('v));

  let succeed: 'a => decoder('a);

  let fail: string => decoder('a);

  let fail_with: error => decoder('a);

  let from_result: result('a, error) => decoder('a);

  let and_then: ('a => decoder('b), decoder('a)) => decoder('b);

  let fix: (decoder('a) => decoder('a)) => decoder('a);

  let of_of_string: (~msg: string, string => option('a)) => decoder('a);

  module Infix: {
    let (>|=): (decoder('a), 'a => 'b) => decoder('b);

    let (>>=): (decoder('a), 'a => decoder('b)) => decoder('b);

    let (<*>): (decoder('a => 'b), decoder('a)) => decoder('b);

    let (<$>): ('a => 'b, decoder('a)) => decoder('b);

    include Shims_let_ops_.S with type t_let('a) := decoder('a);
  };

  include (module type of Infix);

  let decode_value: (decoder('a), value) => result('a, error);

  let decode_string: (decoder('a), string) => result('a, error);

  let decode_file: (decoder('a), string) => result('a, error);

  module Pipeline: {
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

module Make =
       (Decodeable: Decodeable)

         : (
           S with
             type value = Decodeable.value and
             type decoder('a) = exposed_decoder(Decodeable.value, 'a)
       ) => {
  type value = Decodeable.value;

  let pp = Decodeable.pp;

  type error = exposed_error(value);

  let rec pp_error = fmt =>
    fun
    | [@implicit_arity] Decoder_error(msg, Some(t)) =>
      Format.fprintf(fmt, "@[%s, but got@ @[%a@]@]", msg, pp, t)
    | [@implicit_arity] Decoder_error(msg, None) =>
      Format.fprintf(fmt, "@[%s@]", msg)
    | Decoder_errors(errors) => {
        let errors_trunc = My_list.take(5, errors);
        let not_shown = List.length(errors) - 5;
        Format.fprintf(
          fmt,
          "@[%a@ %s@]",
          Format.pp_print_list(~pp_sep=Format.pp_print_space, pp_error),
          errors_trunc,
          if (not_shown > 0) {
            Printf.sprintf("(...%d errors not shown...)", not_shown);
          } else {
            "";
          },
        );
      }
    | [@implicit_arity] Decoder_tag(msg, error) =>
      Format.fprintf(fmt, "@[<2>%s:@ @[%a@]@]", msg, pp_error, error);

  let string_of_error = (e): string =>
    Format.asprintf("@[<2>%a@?@]", pp_error, e);

  let tag_error = (msg: string, error: error): error =>
    [@implicit_arity] Decoder_tag(msg, error);

  let tag_errors = (msg: string, errors: list(error)): error =>
    [@implicit_arity] Decoder_tag(msg, Decoder_errors(errors));

  let merge_errors = (e1, e2) =>
    switch (e1, e2) {
    | (Decoder_errors(e1s), Decoder_errors(e2s)) => Decoder_errors(e1s @ e2s)
    | (Decoder_errors(e1s), _) => Decoder_errors(e1s @ [e2])
    | (_, Decoder_errors(e2s)) => Decoder_errors([e1] @ e2s)
    | _ => Decoder_errors([e1, e2])
    };

  let combine_errors =
      (results: list(result('a, error))): result(list('a), list(error)) => {
    let rec aux = combined =>
      fun
      | [] =>
        switch (combined) {
        | Ok(xs) => Ok(List.rev(xs))
        | Error(es) => Error(List.rev(es))
        }
      | [result, ...rest] => {
          let combined =
            switch (result, combined) {
            | (Ok(x), Ok(xs)) => Ok([x, ...xs])
            | (Error(e), Error(es)) => Error([e, ...es])
            | (Error(e), Ok(_)) => Error([e])
            | (Ok(_), Error(es)) => Error(es)
            };

          aux(combined, rest);
        };

    aux(Ok([]), results);
  };

  let of_string: string => result(value, error) = (
    string =>
      Decodeable.of_string(string)
      |> My_result.map_err(msg =>
           [@implicit_arity]
           Decoder_tag(
             "Json parse error",
             [@implicit_arity] Decoder_error(msg, None),
           )
         ):
      string => result(value, error)
  );

  let of_file: string => result(value, error) = (
    file =>
      Decodeable.of_file(file)
      |> My_result.map_err(msg =>
           [@implicit_arity]
           Decoder_tag(
             Printf.sprintf("While reading %s", file),
             [@implicit_arity] Decoder_error(msg, None),
           )
         ):
      string => result(value, error)
  );

  type decoder('a) = exposed_decoder(value, 'a);

  let succeed = x => {run: _ => Ok(x)};

  let fail = msg => {
    run: input => Error([@implicit_arity] Decoder_error(msg, Some(input))),
  };

  let fail_with = error => {run: _ => Error(error)};

  let from_result =
    fun
    | Ok(ok) => succeed(ok)
    | Error(error) => fail_with(error);

  let value = {run: input => Ok(input)};

  let map = (f, decoder) => {
    run: input => My_result.Infix.(decoder.run(input) >|= f),
  };

  let apply: (decoder('a => 'b), decoder('a)) => decoder('b) = (
    (f, decoder) => {
      run: input =>
        switch (f.run(input), decoder.run(input)) {
        | (Error(e1), Error(e2)) => Error(merge_errors(e1, e2))
        | (Error(e), _) => Error(e)
        | (_, Error(e)) => Error(e)
        | (Ok(g), Ok(x)) => Ok(g(x))
        },
    }:
      (decoder('a => 'b), decoder('a)) => decoder('b)
  );

  let and_then = (f: 'a => decoder('b), decoder: decoder('a)): decoder('b) => {
    run: input =>
      My_result.Infix.(
        decoder.run(input) >>= (result => f(result).run(input))
      ),
  };

  let fix = (f: decoder('a) => decoder('a)): decoder('a) => {
    let rec p = lazy(f(r))
    and r = {run: value => Lazy.force(p).run(value)};
    r;
  };

  module Infix = {
    [@inline]
    let (>|=) = (x, f) => map(f, x);

    [@inline]
    let (>>=) = (x, f) => and_then(f, x);

    [@inline]
    let (<*>) = (f, x) => apply(f, x);

    let (<$>) = map;

    include Shims_let_ops_.Make({
      type t('a) = decoder('a);

      let (>>=) = (>>=);

      let (>|=) = (>|=);

      [@inline]
      let monoid_product = (a, b) => map((x, y) => (x, y), a) <*> b;
    });
  };

  let maybe = (decoder: decoder('a)): decoder(option('a)) => {
    run: input =>
      switch (decoder.run(input)) {
      | Ok(result) => Ok(Some(result))
      | Error(_) => Ok(None)
      },
  };

  let nullable = (decoder: decoder('a)): decoder(option('a)) => {
    run: input =>
      switch (Decodeable.get_null(input)) {
      | Some () => Ok(None)
      | None =>
        decoder.run(input)
        |> My_result.map(My_opt.return)
        |> My_result.map_err(tag_error("Expected null or"))
      },
  };

  let one_of: list((string, decoder('a))) => decoder('a) = (
    decoders => {
      let run = input => {
        let rec go = errors =>
          fun
          | [(name, decoder), ...rest] =>
            switch (decoder.run(input)) {
            | Ok(result) => Ok(result)
            | Error(error) =>
              go(
                [
                  tag_errors(Printf.sprintf("%S decoder", name), [error]),
                  ...errors,
                ],
                rest,
              )
            }
          | [] =>
            Error(
              tag_errors(
                "I tried the following decoders but they all failed",
                errors,
              ),
            );

        go([], decoders);
      };

      {run: run};
    }:
      list((string, decoder('a))) => decoder('a)
  );

  let primitive_decoder =
      (get_value: value => option('a), message: string): decoder('a) => {
    run: t =>
      switch (get_value(t)) {
      | Some(value) => Ok(value)
      | _ => fail(message).run(t)
      },
  };

  let string: decoder(string) = (
    primitive_decoder(Decodeable.get_string, "Expected a string"):
      decoder(string)
  );

  let int: decoder(int) = (
    primitive_decoder(Decodeable.get_int, "Expected an int"): decoder(int)
  );

  let float: decoder(float) = (
    primitive_decoder(Decodeable.get_float, "Expected a float"):
      decoder(float)
  );

  let bool: decoder(bool) = (
    primitive_decoder(Decodeable.get_bool, "Expected a bool"): decoder(bool)
  );

  let null: decoder(unit) = (
    primitive_decoder(Decodeable.get_null, "Expected a null"): decoder(unit)
  );

  let list: decoder('a) => decoder(list('a)) = (
    decoder => {
      run: t =>
        switch (Decodeable.get_list(t)) {
        | None => fail("Expected a list").run(t)
        | Some(values) =>
          values
          |> My_list.mapi((i, x) =>
               decoder.run(x)
               |> My_result.map_err(
                    tag_error(Printf.sprintf("element %i", i)),
                  )
             )
          |> combine_errors
          |> My_result.map_err(tag_errors("while decoding a list"))
        },
    }:
      decoder('a) => decoder(list('a))
  );

  let list_filter: decoder(option('a)) => decoder(list('a)) = (
    decoder => {
      let rec go = i =>
        fun
        | [] => Ok([])
        | [v, ...vs] =>
          My_result.Infix.(
            decoder.run(v)
            |> My_result.map_err(
                 tag_error(Printf.sprintf("element %i", i)),
               )
            >>= (
              fun
              | Some(x) =>
                go(i + 1, vs) >>= (xs => My_result.return([x, ...xs]))
              | None => go(i + 1, vs)
            )
          );

      {
        run: t =>
          switch (Decodeable.get_list(t)) {
          | None => fail("Expected a list").run(t)
          | Some(values) =>
            go(0, values)
            |> My_result.map_err(tag_error("while decoding a list"))
          },
      };
    }:
      decoder(option('a)) => decoder(list('a))
  );

  let list_fold_left: ('a => decoder('a), 'a) => decoder('a) = (
    (decoder_func, init) => {
      run: t =>
        switch (Decodeable.get_list(t)) {
        | None => fail("Expected a list").run(t)
        | Some(values) =>
          values
          |> My_result.Infix.(
               My_list.fold_left(
                 ((acc, i), el) =>
                   (
                     acc
                     >>= (
                       acc =>
                         (acc |> decoder_func).run(el)
                         |> My_result.map_err(
                              tag_error(Printf.sprintf("element %i", i)),
                            )
                     ),
                     i + 1,
                   ),
                 (Ok(init), 0),
               )
             )
          |> fst
          |> My_result.map_err(tag_error("while decoding a list"))
        },
    }:
      ('a => decoder('a), 'a) => decoder('a)
  );

  let array: decoder('a) => decoder(array('a)) = (
    decoder => {
      run: t => {
        let res = list(decoder).run(t);
        switch (res) {
        | Ok(x) => Ok(Array.of_list(x))
        | Error([@implicit_arity] Decoder_tag("while decoding a list", e)) =>
          Error([@implicit_arity] Decoder_tag("while decoding an array", e))
        | Error(e) => Error(e)
        };
      },
    }:
      decoder('a) => decoder(array('a))
  );

  let field: (string, decoder('a)) => decoder('a) = (
    (key, value_decoder) => {
      run: t => {
        let value =
          Decodeable.get_key_value_pairs(t)
          |> My_opt.flat_map(
               My_list.find_map(((k, v)) =>
                 switch (Decodeable.get_string(k)) {
                 | Some(s) when s == key => Some(v)
                 | _ => None
                 }
               ),
             );

        switch (value) {
        | Some(value) =>
          value_decoder.run(value)
          |> My_result.map_err(
               tag_error(Printf.sprintf("in field %S", key)),
             )
        | None =>
          fail(
            Printf.sprintf("Expected an object with an attribute %S", key),
          ).
            run(
            t,
          )
        };
      },
    }:
      (string, decoder('a)) => decoder('a)
  );

  let field_opt: (string, decoder('a)) => decoder(option('a)) = (
    (key, value_decoder) => {
      run: t => {
        let value =
          Decodeable.get_key_value_pairs(t)
          |> My_opt.flat_map(
               My_list.find_map(((k, v)) =>
                 switch (Decodeable.get_string(k)) {
                 | Some(s) when s == key => Some(v)
                 | _ => None
                 }
               ),
             );

        switch (value) {
        | Some(value) =>
          value_decoder.run(value)
          |> My_result.map(v => Some(v))
          |> My_result.map_err(
               tag_error(Printf.sprintf("in field %S", key)),
             )
        | None => Ok(None)
        };
      },
    }:
      (string, decoder('a)) => decoder(option('a))
  );

  let single_field: (string => decoder('a)) => decoder('a) = (
    value_decoder => {
      run: t =>
        switch (Decodeable.get_key_value_pairs(t)) {
        | Some([(key, value)]) =>
          switch (Decodeable.get_string(key)) {
          | Some(key) =>
            value_decoder(key).run(value)
            |> My_result.map_err(
                 tag_error(Printf.sprintf("in field %S", key)),
               )
          | None => fail("Expected an object with a string key").run(t)
          }
        | _ => fail("Expected an object with a single attribute").run(t)
        },
    }:
      (string => decoder('a)) => decoder('a)
  );

  let index: (int, decoder('a)) => decoder('a) = (
    (i, decoder) => {
      run: t =>
        switch (Decodeable.get_list(t)) {
        | Some(l) =>
          let item =
            try(Some(List.nth(l, i))) {
            | Failure(_) => None
            | Invalid_argument(_) => None
            };

          switch (item) {
          | None =>
            fail(
              "expected a list with at least "
              ++ string_of_int(i)
              ++ " elements",
            ).
              run(
              t,
            )
          | Some(item) => decoder.run(item)
          };
        | None => fail("Expected a list").run(t)
        },
    }:
      (int, decoder('a)) => decoder('a)
  );

  let uncons = (tail: 'a => decoder('b), head: decoder('a)): decoder('b) => {
    run: value =>
      switch (Decodeable.get_list(value)) {
      | Some([x, ...rest]) =>
        My_result.Infix.(
          head.run(x)
          |> My_result.map_err(tag_error("while consuming a list element"))
          >>= (
            x =>
              tail(x).run(Decodeable.to_list(rest))
              |> My_result.map_err(
                   tag_error("after consuming a list element"),
                 )
          )
        )
      | Some([]) => fail("Expected a non-empty list").run(value)
      | None => fail("Expected a list").run(value)
      },
  };

  let rec at: (list(string), decoder('a)) => decoder('a) = (
    (path, decoder) =>
      switch (path) {
      | [key] => field(key, decoder)
      | [key, ...rest] => field(key, at(rest, decoder))
      | [] => fail("Must provide at least one key to 'at'")
      }:
      (list(string), decoder('a)) => decoder('a)
  );

  let keys': decoder('k) => decoder(list('k)) = (
    key_decoder => {
      run: value =>
        switch (Decodeable.get_key_value_pairs(value)) {
        | Some(assoc) =>
          assoc
          |> List.map(((key, _)) => key_decoder.run(key))
          |> combine_errors
          |> My_result.map_err(
               tag_errors("Failed while decoding the keys of an object"),
             )
        | None => fail("Expected an object").run(value)
        },
    }:
      decoder('k) => decoder(list('k))
  );

  let keys = keys'(string);

  let key_value_pairs':
    (decoder('k), decoder('v)) => decoder(list(('k, 'v))) = (
    (key_decoder, value_decoder) => {
      run: value =>
        switch (Decodeable.get_key_value_pairs(value)) {
        | Some(assoc) =>
          assoc
          |> List.map(
               My_result.Infix.(
                 ((key_val, value_val)) =>
                   key_decoder.run(key_val)
                   >>= (
                     key =>
                       value_decoder.run(value_val)
                       >|= (value => (key, value))
                   )
               ),
             )
          |> combine_errors
          |> My_result.map_err(
               tag_errors("Failed while decoding key-value pairs"),
             )
        | None => fail("Expected an object").run(value)
        },
    }:
      (decoder('k), decoder('v)) => decoder(list(('k, 'v)))
  );

  let key_value_pairs = value_decoder =>
    key_value_pairs'(string, value_decoder);

  let key_value_pairs_seq':
    (decoder('k), 'k => decoder('v)) => decoder(list('v)) = (
    (key_decoder, value_decoder) => {
      run: value =>
        switch (Decodeable.get_key_value_pairs(value)) {
        | Some(assoc) =>
          assoc
          |> List.map(
               My_result.Infix.(
                 ((key_val, value_val)) =>
                   key_decoder.run(key_val)
                   >>= (key => value_decoder(key).run(value_val))
               ),
             )
          |> combine_errors
          |> My_result.map_err(
               tag_errors("Failed while decoding key-value pairs"),
             )
        | None => fail("Expected an object").run(value)
        },
    }:
      (decoder('k), 'k => decoder('v)) => decoder(list('v))
  );

  let key_value_pairs_seq = value_decoder =>
    key_value_pairs_seq'(string, value_decoder);

  let decode_value = (decoder: decoder('a), input: value): result('a, error) =>
    decoder.run(input);

  let of_of_string = (~msg, of_string) =>
    Infix.(
      string
      >|= of_string
      >>= (
        fun
        | Some(x) => succeed(x)
        | None => fail("Expected " ++ msg)
      )
    );

  let decode_string: (decoder('a), string) => result('a, error) = (
    (decoder, string) =>
      My_result.Infix.(of_string(string) >>= decode_value(decoder)):
      (decoder('a), string) => result('a, error)
  );

  let decode_file: (decoder('a), string) => result('a, error) = (
    (decoder, file) =>
      My_result.Infix.(of_file(file) >>= decode_value(decoder)):
      (decoder('a), string) => result('a, error)
  );

  module Pipeline = {
    let decode = succeed;

    let custom: (decoder('a), decoder('a => 'b)) => decoder('b) = (
      (customDecoder, next) => apply(next, customDecoder):
        (decoder('a), decoder('a => 'b)) => decoder('b)
    );

    let required: (string, decoder('a), decoder('a => 'b)) => decoder('b) = (
      (key, decoder, next) => custom(field(key, decoder), next):
        (string, decoder('a), decoder('a => 'b)) => decoder('b)
    );

    let required_at:
      (list(string), decoder('a), decoder('a => 'b)) => decoder('b) = (
      (path, decoder, next) => custom(at(path, decoder), next):
        (list(string), decoder('a), decoder('a => 'b)) => decoder('b)
    );

    let optional_decoder: (decoder(value), decoder('a), 'a) => decoder('a) = (
      (path_decoder, val_decoder, default) => {
        let null_or = decoder =>
          one_of([
            ("non-null", decoder),
            ("null", null |> map(() => default)),
          ]);

        let handle_result: value => decoder('a) = (
          input =>
            switch (decode_value(path_decoder, input)) {
            | Ok(rawValue) =>
              decode_value(null_or(val_decoder), rawValue) |> from_result
            | Error(_) => succeed(default)
            }:
            value => decoder('a)
        );

        value |> and_then(handle_result);
      }:
        (decoder(value), decoder('a), 'a) => decoder('a)
      /* The field was present. */
      /* The field was not present. */
    );

    let optional:
      (string, decoder('a), 'a, decoder('a => 'b)) => decoder('b) = (
      (key, val_decoder, default, next) =>
        custom(
          optional_decoder(field(key, value), val_decoder, default),
          next,
        ):
        (string, decoder('a), 'a, decoder('a => 'b)) => decoder('b)
    );

    let optional_at:
      (list(string), decoder('a), 'a, decoder('a => 'b)) => decoder('b) = (
      (path, val_decoder, default, next) =>
        custom(
          optional_decoder(at(path, value), val_decoder, default),
          next,
        ):
        (list(string), decoder('a), 'a, decoder('a => 'b)) => decoder('b)
    );
  };

  include Infix;
};
