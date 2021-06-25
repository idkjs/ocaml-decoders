open OUnit2;

type tree =
  | Leaf(int)
  | Node(tree, tree);

let yojson_basic_suite = {
  open Decoders_yojson.Basic.Decode;
  let decoder_test = (~decoder, ~input, ~expected, ~printer=?, _test_ctxt) =>
    switch (decode_string(decoder, input)) {
    | Ok(value) => assert_equal(value, expected, ~printer?)
    | Error(error) => assert_string(Format.asprintf("%a", pp_error, error))
    };

  let list_string_test =
    "list string"
    >:: decoder_test(
          ~decoder=list(string),
          ~input="[\"Hello world\"]",
          ~expected=["Hello world"],
        );

  let array_string_test =
    "array string"
    >:: decoder_test(
          ~decoder=array(string),
          ~input="[\"Hello world\"]",
          ~expected=[|"Hello world"|],
        );

  let fix_one_of_test =
    "fix one_of"
    >:: (
      _ => {
        let tree_decoder =
          fix(tree_decoder =>
            let leaf_decoder = int |> map(i => Leaf(i));
            let node_decoder =
              Pipeline.(
                decode((left, right) => [@implicit_arity] Node(left, right))
                |> required("left", tree_decoder)
                |> required("right", tree_decoder)
              );

            one_of([("leaf", leaf_decoder), ("node", node_decoder)]);
          );

        decoder_test(
          ~decoder=tree_decoder,
          ~input="{\"left\":1, \"right\":{\"left\":2,\"right\":3}}",
          ~expected=
            [@implicit_arity]
            Node(Leaf(1), [@implicit_arity] Node(Leaf(2), Leaf(3))),
          (),
        );
      }
    );

  let mut_rec_test =
    "mutual recursion"
    >:: (
      _ => {
        module M = {
          type t1 =
            | T1_end
            | T1_more(t2)

          and t2 =
            | T2_end
            | T2_more(t1);

          let rec t1_to_string =
            fun
            | T1_end => "T1_end"
            | T1_more(t2) => Printf.sprintf("(T1_more %s)", t2_to_string(t2))

          and t2_to_string =
            fun
            | T2_end => "T2_end"
            | T2_more(t1) =>
              Printf.sprintf("(T2_more %s)", t1_to_string(t1));
        };
        open M;
        let t1_decoder =
          fix(t1_decoder =>
            let t2 =
              nullable(field("t1", t1_decoder))
              |> map(
                   fun
                   | None => T2_end
                   | Some(t1) => T2_more(t1),
                 );

            let t1 =
              nullable(field("t2", t2))
              |> map(
                   fun
                   | None => T1_end
                   | Some(t2) => T1_more(t2),
                 );

            t1;
          );

        decoder_test(
          (),
          ~decoder=t1_decoder,
          ~input={|
          { "t2": { "t1": { "t2": null } } }
         |},
          ~expected=T1_more(T2_more(T1_more(T2_end))),
          ~printer=t1_to_string,
        );
      }
    );

  let string_or_floatlit_test =
    "string or floatlit"
    >:: (
      _ => {
        let empty_string =
          string
          |> and_then(
               fun
               | "" => succeed()
               | _ => fail("Expected an empty string"),
             );

        decoder_test(
          ~decoder=one_of([("empty", empty_string |> map(() => None))]),
          ~input="\"\"",
          ~expected=None,
          (),
        );
      }
    );

  let grouping_errors_test =
    "grouping errors"
    >:: (
      _test_ctxt => {
        let decoder =
          Pipeline.(
            decode((x, y, z) => (x, y, z))
            |> required(
                 "records",
                 list(
                   decode((x, y, z) => (x, y, z))
                   |> required("x", list(string))
                   |> required("y", int)
                   |> required("z", bool),
                 ),
               )
            |> required("hello", int)
            |> required("another", int)
          );

        let input = {|
        {"records": [true, {"x": [1, "c", 3], "y": "hello"}], "hello": "world", "another": "error"}
      |};

        let expected_error =
          Decoders.Decode.(
            Decoder_errors([
              [@implicit_arity]
              Decoder_tag(
                {|in field "records"|},
                [@implicit_arity]
                Decoder_tag(
                  "while decoding a list",
                  Decoder_errors([
                    [@implicit_arity]
                    Decoder_tag(
                      "element 0",
                      Decoder_errors([
                        [@implicit_arity]
                        Decoder_error(
                          {|Expected an object with an attribute "x"|},
                          Some(`Bool(true)),
                        ),
                        [@implicit_arity]
                        Decoder_error(
                          {|Expected an object with an attribute "y"|},
                          Some(`Bool(true)),
                        ),
                        [@implicit_arity]
                        Decoder_error(
                          {|Expected an object with an attribute "z"|},
                          Some(`Bool(true)),
                        ),
                      ]),
                    ),
                    [@implicit_arity]
                    Decoder_tag(
                      "element 1",
                      Decoder_errors([
                        [@implicit_arity]
                        Decoder_tag(
                          {|in field "x"|},
                          [@implicit_arity]
                          Decoder_tag(
                            "while decoding a list",
                            Decoder_errors([
                              [@implicit_arity]
                              Decoder_tag(
                                "element 0",
                                [@implicit_arity]
                                Decoder_error(
                                  "Expected a string",
                                  Some(`Int(1)),
                                ),
                              ),
                              [@implicit_arity]
                              Decoder_tag(
                                "element 2",
                                [@implicit_arity]
                                Decoder_error(
                                  "Expected a string",
                                  Some(`Int(3)),
                                ),
                              ),
                            ]),
                          ),
                        ),
                        [@implicit_arity]
                        Decoder_tag(
                          {|in field "y"|},
                          [@implicit_arity]
                          Decoder_error(
                            "Expected an int",
                            Some(`String("hello")),
                          ),
                        ),
                        [@implicit_arity]
                        Decoder_error(
                          {|Expected an object with an attribute "z"|},
                          Some(
                            `Assoc([
                              (
                                "x",
                                `List([`Int(1), `String("c"), `Int(3)]),
                              ),
                              ("y", `String("hello")),
                            ]),
                          ),
                        ),
                      ]),
                    ),
                  ]),
                ),
              ),
              [@implicit_arity]
              Decoder_tag(
                {|in field "hello"|},
                [@implicit_arity]
                Decoder_error("Expected an int", Some(`String("world"))),
              ),
              [@implicit_arity]
              Decoder_tag(
                {|in field "another"|},
                [@implicit_arity]
                Decoder_error("Expected an int", Some(`String("error"))),
              ),
            ])
          );

        switch (decode_string(decoder, input)) {
        | Ok(_) => assert_string("Expected an error")
        | Error(error) =>
          assert_equal(expected_error, error, ~printer=e =>
            Format.asprintf("@,@[%a@]", pp_error, e)
          )
        };
      }
    );

  "Yojson.Basic"
  >::: [
    list_string_test,
    array_string_test,
    fix_one_of_test,
    mut_rec_test,
    string_or_floatlit_test,
    grouping_errors_test,
  ];
};

let yojson_raw_suite = {
  open Decoders_yojson.Raw.Decode;
  let decoder_test = (~decoder, ~input, ~expected, _test_ctxt) =>
    switch (decode_string(decoder, input)) {
    | Ok(value) => assert_equal(value, expected)
    | Error(error) => assert_string(Format.asprintf("%a", pp_error, error))
    };

  "Yojson.Raw"
  >::: [
    "list string"
    >:: decoder_test(
          ~decoder=list(string),
          ~input="[\"Hello world\"]",
          ~expected=["Hello world"],
        ),
    "fix one_of"
    >:: (
      _ => {
        let tree_decoder =
          fix(tree_decoder =>
            let leaf_decoder = int |> map(i => Leaf(i));
            let node_decoder =
              Pipeline.(
                decode((left, right) => [@implicit_arity] Node(left, right))
                |> required("left", tree_decoder)
                |> required("right", tree_decoder)
              );

            one_of([("leaf", leaf_decoder), ("node", node_decoder)]);
          );

        decoder_test(
          ~decoder=tree_decoder,
          ~input="{\"left\":1, \"right\":{\"left\":2,\"right\":3}}",
          ~expected=
            [@implicit_arity]
            Node(Leaf(1), [@implicit_arity] Node(Leaf(2), Leaf(3))),
          (),
        );
      }
    ),
    "string or floatlit"
    >:: {
      let empty_string =
        string
        |> and_then(
             fun
             | "" => succeed(None)
             | _ => fail("Expected an empty string"),
           );

      decoder_test(
        ~decoder=
          list(
            one_of([
              ("empty", empty_string),
              ("floatlit", floatlit |> map(x => Some(x))),
            ]),
          ),
        ~input={|["", 123, 123.45]|},
        ~expected=[None, Some("123"), Some("123.45")],
      );
    },
  ];
};

let () =
  "decoders" >::: [yojson_basic_suite, yojson_raw_suite] |> run_test_tt_main;
