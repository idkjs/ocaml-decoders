open OUnit2;

type tree =
  | Leaf(int)
  | Node(tree, tree);

let ezjsonm_suite = {
  open Decoders_ezjsonm.Decode;
  let decoder_test = (~decoder, ~input, ~expected, _test_ctxt) =>
    switch (decode_string(decoder, input)) {
    | Ok(value) => assert_equal(value, expected)
    | Error(error) => assert_string(Format.asprintf("%a", pp_error, error))
    };

  "Ezjsonm"
  >::: [
    "list string"
    >:: decoder_test(
          ~decoder=list(string),
          ~input={|["hello", "world"]|},
          ~expected=["hello", "world"],
        ),
    "field_opt present"
    >:: decoder_test(
          ~decoder=field_opt("optional", string),
          ~input={|{"optional": "hello"}|},
          ~expected=Some("hello"),
        ),
    "field_opt missing"
    >:: decoder_test(
          ~decoder=field_opt("optional", string),
          ~input={|{"missing": "hello"}|},
          ~expected=None,
        ),
    "field_opt decode error"
    >:: (
      _ =>
        switch (
          decode_string(field_opt("optional", string), {|{"optional": 123}|})
        ) {
        | Ok(_) => assert_string("expected decode error")
        | Error(e) =>
          assert_equal(
            ~printer=CCFun.id,
            {|in field "optional": Expected a string, but got 123.|},
            Format.asprintf("%a", pp_error, e),
          )
        }
    ),
  ];
};

let ezjsonm_encoders_suite =
  Decoders_ezjsonm.Encode.(
    "Ezjsonm encoders"
    >::: [
      "list string"
      >:: (
        _ctxt =>
          assert_equal(
            ~printer=CCFun.id,
            {|["hello","world"]|},
            encode_string(list(string), ["hello", "world"]),
          )
      ),
      "string"
      >:: (
        _ctxt =>
          assert_equal(
            ~printer=CCFun.id,
            {|"hello"|},
            encode_string(string, "hello"),
          )
      ),
    ]
  );

let () =
  "decoders" >::: [ezjsonm_suite, ezjsonm_encoders_suite] |> run_test_tt_main;
