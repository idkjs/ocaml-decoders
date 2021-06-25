open OUnit2;

let sexplib_suite = {
  open Decoders_sexplib.Decode;
  let decoder_test = (~decoder, ~input, ~expected, _test_ctxt) =>
    switch (decode_string(decoder, input)) {
    | Ok(value) => assert_equal(value, expected)
    | Error(error) => assert_string(Format.asprintf("%a", pp_error, error))
    };

  "Sexplib"
  >::: [
    "list string"
    >:: decoder_test(
          ~decoder=list(string),
          ~input="(hello world)",
          ~expected=["hello", "world"],
        ),
    "list string (one element)"
    >:: decoder_test(
          ~decoder=list(string),
          ~input="(hello)",
          ~expected=["hello"],
        ),
    "field_opt present"
    >:: decoder_test(
          ~decoder=field_opt("optional", string),
          ~input="((optional hello))",
          ~expected=Some("hello"),
        ),
    "field_opt missing"
    >:: decoder_test(
          ~decoder=field_opt("optional", string),
          ~input="()",
          ~expected=None,
        ),
    "uncons"
    >:: {
      let (>>=::) = (head, tail) => uncons(tail, head);
      decoder_test(
        ~input=
          "(library (name decoders-sexplib) (libraries decoders sexplib0))",
        ~decoder=
          string
          >>=:: (
            fun
            | "library" =>
              field("name", string)
              >>= (
                name =>
                  field(
                    "libraries",
                    one_of([
                      ("list", list(string)),
                      ("string", string >|= (s => [s])),
                    ]),
                  )
                  >>= (libs => succeed((name, libs)))
              )
            | _ => fail("Expected 'library'")
          ),
        ~expected=("decoders-sexplib", ["decoders", "sexplib0"]),
      );
    },
  ];
};

let () = "decoders" >::: [sexplib_suite] |> run_test_tt_main;
