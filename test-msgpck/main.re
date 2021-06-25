open OUnit2;
module M = Msgpck;

let m_to_str = m => M.StringBuf.to_string(m) |> Buffer.contents;

let decoders_suite = {
  open Decoders_msgpck.Decode;
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
          ~input=m_to_str(M.List([M.String("hello"), M.String("world")])),
          ~expected=["hello", "world"],
        ),
    "field_opt present"
    >:: decoder_test(
          ~decoder=field_opt("optional", string),
          ~input=
            m_to_str(M.Map([(M.String("optional"), M.String("hello"))])),
          ~expected=Some("hello"),
        ),
    "field_opt missing"
    >:: decoder_test(
          ~decoder=field_opt("optional", string),
          ~input=
            m_to_str(M.Map([(M.String("missing"), M.String("hello"))])),
          ~expected=None,
        ),
    "field_opt decode error"
    >:: (
      _ =>
        switch (
          decode_string(
            field_opt("optional", string),
            m_to_str(M.Map([(M.String("optional"), M.Int(123))])),
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
    "int32 conversion"
    >:: decoder_test(
          ~decoder=int,
          ~input=m_to_str(M.Int32(32l)),
          ~expected=32,
        ),
    "int64 conversion"
    >:: decoder_test(
          ~decoder=int,
          ~input=m_to_str(M.Int64(525252L)),
          ~expected=525252,
        ),
    "int64 helper"
    >:: decoder_test(
          ~decoder=int64,
          ~input=m_to_str(M.Int64(Int64.max_int)),
          ~expected=Int64.max_int,
        ),
    "int64 overflow"
    >:: (
      _ =>
        switch (decode_string(int, m_to_str(M.Int64(Int64.max_int)))) {
        | Ok(v) =>
          assert_string(Printf.sprintf("expected decode error, got %d", v))
        | Error(_e) => ()
        }
    ),
  ];
};

let encoders_suite =
  Decoders_msgpck.Encode.(
    "encoders"
    >::: [
      "list string"
      >:: (
        _ctxt =>
          assert_equal(
            ~printer=CCFun.id,
            m_to_str(M.List([M.String("hello"), M.String("world")])),
            encode_string(list(string), ["hello", "world"]),
          )
      ),
      "string"
      >:: (
        _ctxt =>
          assert_equal(
            ~printer=CCFun.id,
            m_to_str(M.String("hello")),
            encode_string(string, "hello"),
          )
      ),
      "int64"
      >:: (
        _ctxt =>
          assert_equal(
            ~printer=CCFun.id,
            m_to_str(M.Int64(Int64.max_int)),
            encode_string(int64, Int64.max_int),
          )
      ),
    ]
  );

let () = "msgpck" >::: [decoders_suite, encoders_suite] |> run_test_tt_main;
