open Jest;
open Decoders_bs;

let () =
  describe(
    "decoders-bs decode",
    Expect.(
      () =>
        test(
          "string",
          Decode.(
            () => {
              let json_str = {|"Hello world"|};
              let decoded = decode_string(string, json_str);
              expect(decoded) |> toEqual(Belt.Result.Ok("Hello world"));
            }
          ),
        )
    ),
  );

let () =
  describe(
    "decoders-bs decode array",
    Expect.(
      () =>
        test(
          "array",
          Decode.(
            () => {
              let json_str = {|["a", "b", "c"]|};
              let decoded = decode_string(array(string), json_str);
              expect(decoded) |> toEqual(Belt.Result.Ok([|"a", "b", "c"|]));
            }
          ),
        )
    ),
  );

let () =
  describe(
    "decoders-bs decode error",
    Expect.(
      () =>
        test(
          "array",
          Decode.(
            () => {
              let json_str = {|["a", 1, "c"]|};
              let decoded = decode_string(array(string), json_str);
              expect(decoded)
              |> toEqual(
                   Belt.Result.Error(
                     Decoders.Decode.(
                       [@implicit_arity]
                       Decoder_tag(
                         "while decoding an array",
                         Decoder_errors([
                           [@implicit_arity]
                           Decoder_tag(
                             "element 1",
                             [@implicit_arity]
                             Decoder_error(
                               "Expected a string",
                               Some(Js.Json.number(1.)),
                             ),
                           ),
                         ]),
                       )
                     ),
                   ),
                 );
            }
          ),
        )
    ),
  );

let () =
  describe(
    "decoders-bs encode",
    Expect.(
      () =>
        test(
          "string",
          Encode.(
            () => {
              let str = "Hello world";
              let encoded = encode_string(string, str);
              expect(encoded) |> toEqual({|"Hello world"|});
            }
          ),
        )
    ),
  );

let () =
  describe(
    "decoders-bs encode array",
    Expect.(
      () =>
        test(
          "string",
          Encode.(
            () => {
              let x = [|"a", "b", "c"|];
              let encoded = encode_string(array(string), x);
              expect(encoded) |> toEqual({|["a","b","c"]|});
            }
          ),
        )
    ),
  );
