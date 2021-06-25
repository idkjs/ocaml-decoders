open OUnit2;

let jsonm_suite = {
  open Decoders_jsonm.Encode;
  let test = _test_ctxt => {
    let encoder = () =>
      array_start >> int(0) >> int(1) >> string("hello") >> array_end;

    let s = encode_string(encoder, ());
    assert_equal(~printer=CCFun.id, {|[0,1,"hello"]|}, s);
  };

  "Jsonm" >::: ["encoding a list" >:: test];
};

let () = "encoders" >::: [jsonm_suite] |> run_test_tt_main;
