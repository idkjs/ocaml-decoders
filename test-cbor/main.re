open OUnit2;

let decoders_suite = {
  open Decoders_cbor.Decode;
  let decoder_test = (~decoder, ~input, ~expected, _test_ctxt) =>
    switch (decode_string(decoder, input)) {
    | Ok(value) => assert_equal(value, expected)
    | Error(error) => assert_string(Format.asprintf("%a", pp_error, error))
    };

  "decoders"
  >::: [
    "list string"
    >:: decoder_test(
          ~decoder=list(string),
          ~input=
            CBOR.Simple.encode(`Array([`Text("hello"), `Text("world")])),
          ~expected=["hello", "world"],
        ),
    "field_opt present"
    >:: decoder_test(
          ~decoder=field_opt("optional", string),
          ~input=
            CBOR.Simple.encode(
              `Map([(`Text("optional"), `Text("hello"))]),
            ),
          ~expected=Some("hello"),
        ),
    "field_opt missing"
    >:: decoder_test(
          ~decoder=field_opt("optional", string),
          ~input=
            CBOR.Simple.encode(
              `Map([(`Text("missing"), `Text("hello"))]),
            ),
          ~expected=None,
        ),
    "field_opt decode error"
    >:: (
      _ =>
        switch (
          decode_string(
            field_opt("optional", string),
            CBOR.Simple.encode(`Map([(`Text("optional"), `Int(123))])),
          )
        ) {
        | Ok(_) => assert_string("expected decode error")
        | Error(e) =>
          assert_equal(
            ~printer=CCFun.id,
            {|in field "optional": Expected a string, but got 123|},
            Format.asprintf("%a", pp_error, e),
          )
        }
    ),
  ];
};

let encoders_suite =
  Decoders_cbor.Encode.(
    "encoders"
    >::: [
      "list string"
      >:: (
        _ctxt =>
          assert_equal(
            ~printer=CCFun.id,
            CBOR.Simple.encode(`Array([`Text("hello"), `Text("world")])),
            encode_string(list(string), ["hello", "world"]),
          )
      ),
      "string"
      >:: (
        _ctxt =>
          assert_equal(
            ~printer=CCFun.id,
            CBOR.Simple.encode(`Text("hello")),
            encode_string(string, "hello"),
          )
      ),
    ]
  );

let () = "CBOR" >::: [decoders_suite, encoders_suite] |> run_test_tt_main;
